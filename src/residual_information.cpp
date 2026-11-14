#include "core/information.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace persephone {
ResidualInformation residual_information(const ResidualFunction& function,
    const std::vector<double>& x, const InformationControl& c, const Interrupt& interrupt) {
    c.validate();
    if(!function || x.size()>64) throw std::invalid_argument("residual information requires a function and at most 64 coefficients");
    for(const auto value:x) if(!std::isfinite(value)) throw std::invalid_argument("information coefficients must be finite");
    ResidualInformation result; auto& out=result.diagnostics;
    const auto k=x.size(); bool budget=false;
    const auto evaluate=[&](const std::vector<double>& point) {
        if(interrupt) interrupt();
        if(out.evaluations>=c.max_evaluations) { budget=true; return ResidualSample{"evaluation_limit",{},0,{}}; }
        ++out.evaluations;
        auto value=function(point);
        if(value.status=="available") {
            if(value.residuals.empty() || !value.effective_count) value.status="invalid_residual_domain";
            for(const auto v:value.residuals) if(!std::isfinite(v)) value.status="nonfinite_residuals";
        }
        if(value.status!="available") ++out.rejected[value.status];
        return value;
    };
    result.center=evaluate(x);
    if(result.center.status!="available") { out.status="center_unavailable"; return result; }
    const auto m=result.center.residuals.size(); const auto& z=result.center.residuals;
    long double q=0; for(const auto v:z) q+=static_cast<long double>(v)*v;
    if(!(q>0) || !std::isfinite(q)) { out.status="invalid_residual_scale"; return result; }
    if(!k) {
        out.status=out.derivative_status=out.curvature_status="no_free_parameters";
        out.stationary=true; return result;
    }
    const auto factor=static_cast<long double>(result.center.effective_count)/q;
    const auto epsilon=std::numeric_limits<double>::epsilon();
    std::vector<double> h(k);
    for(std::size_t j=0;j<k;++j) h[j]=c.difference_step*std::max(1.,std::abs(x[j]));
    for(int refinement=0;refinement<=c.difference_refinements;++refinement) {
        out.refinements=refinement;
        std::array<DenseMatrix,3> estimates{{DenseMatrix(m,k),DenseMatrix(m,k),DenseMatrix(m,k)}};
        DenseMatrix roundoff(m,k); bool feasible=true;
        for(std::size_t level=0;level<3 && feasible;++level) for(std::size_t j=0;j<k && feasible;++j) {
            const auto step=std::ldexp(h[j],-static_cast<int>(level));
            auto plus=x,minus=x; plus[j]+=step; minus[j]-=step;
            if(plus[j]==x[j] || minus[j]==x[j]) { feasible=false; break; }
            const auto fp=evaluate(plus),fm=evaluate(minus);
            if(fp.status!="available" || fm.status!="available") { feasible=false; break; }
            if(fp.residuals.size()!=m || fm.residuals.size()!=m ||
                fp.effective_count!=result.center.effective_count || fm.effective_count!=result.center.effective_count) {
                throw std::logic_error("residual information cannot differentiate changing domains");
            }
            for(std::size_t i=0;i<m;++i) {
                estimates[level](i,j)=(fp.residuals[i]-fm.residuals[i])/(2*step);
                roundoff(i,j)=std::max(roundoff(i,j),64*epsilon*
                    std::max({std::abs(z[i]),std::abs(fp.residuals[i]),std::abs(fm.residuals[i])})/step);
            }
        }
        if(budget) { out.status=out.derivative_status="evaluation_limit"; return result; }
        if(feasible) {
            DenseMatrix J(m,k),E(m,k),H(k,k),HE(k,k);
            bool finite=true;
            for(std::size_t j=0;j<k;++j) for(std::size_t i=0;i<m;++i) {
                J(i,j)=(4*estimates[2](i,j)-estimates[1](i,j))/3;
                const auto coarse=(4*estimates[1](i,j)-estimates[0](i,j))/3;
                E(i,j)=std::abs(J(i,j)-coarse)+roundoff(i,j);
                finite=finite && std::isfinite(J(i,j)) && std::isfinite(E(i,j));
            }
            out.gradient.assign(k,0); out.gradient_error.assign(k,0); out.steps=h;
            double hnorm=0,enorm=0,gnorm=0,gerror=0;
            for(std::size_t j=0;j<k;++j) {
                long double gradient=0,error=0;
                for(std::size_t t=0;t<m;++t) {
                    gradient+=static_cast<long double>(J(t,j))*z[t];
                    error+=static_cast<long double>(E(t,j))*std::abs(z[t]);
                }
                out.gradient[j]=static_cast<double>(factor*gradient);
                out.gradient_error[j]=static_cast<double>(factor*error)+64*epsilon*std::abs(out.gradient[j]);
                finite=finite && std::isfinite(out.gradient[j]) && std::isfinite(out.gradient_error[j]);
                gnorm=std::max(gnorm,std::abs(out.gradient[j])+out.gradient_error[j]);
                gerror=std::max(gerror,out.gradient_error[j]);
                for(std::size_t i=0;i<=j;++i) {
                    long double value=0,bound=0;
                    for(std::size_t t=0;t<m;++t) {
                        value+=static_cast<long double>(J(t,i))*J(t,j);
                        bound+=static_cast<long double>(std::abs(J(t,i)))*E(t,j)+
                            static_cast<long double>(std::abs(J(t,j)))*E(t,i)+static_cast<long double>(E(t,i))*E(t,j);
                    }
                    H(i,j)=H(j,i)=static_cast<double>(factor*value);
                    HE(i,j)=HE(j,i)=static_cast<double>(factor*bound)+64*epsilon*std::abs(H(i,j));
                    finite=finite && std::isfinite(H(i,j)) && std::isfinite(HE(i,j));
                }
            }
            for(std::size_t j=0;j<k;++j) {
                double a=0,b=0; for(std::size_t i=0;i<k;++i) { a+=std::abs(H(i,j)); b+=HE(i,j); }
                hnorm=std::max(hnorm,a); enorm=std::max(enorm,b);
            }
            if(finite) { result.jacobian=J; result.jacobian_error=E; out.hessian=H; out.hessian_error=HE; }
            if(finite && std::isfinite(hnorm) && std::isfinite(enorm) &&
                enorm<=c.absolute_tolerance+c.relative_tolerance*hnorm && gerror<=c.gradient_tolerance/8) {
                out.derivative_status="available"; out.stationary=gnorm<=c.gradient_tolerance;
                qualify_information(out,c,interrupt); return result;
            }
        }
        for(auto& step:h) step*=.5;
    }
    out.status=out.derivative_status="derivatives_unresolved";
    return result;
}
} // namespace persephone
