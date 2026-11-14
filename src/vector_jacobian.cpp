#include "core/information.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace persephone {
VectorJacobian vector_jacobian(const VectorFunction& function,
    const std::vector<double>& x, const InformationControl& c, const Interrupt& interrupt) {
    c.validate();
    if(!function || x.empty() || x.size()>64) throw std::invalid_argument("vector Jacobian requires a function and 1 to 64 coefficients");
    for(const auto value:x) if(!std::isfinite(value)) throw std::invalid_argument("Jacobian coefficients must be finite");
    VectorJacobian out; bool budget=false;
    const auto evaluate=[&](const std::vector<double>& point) {
        if(interrupt) interrupt();
        if(out.evaluations>=c.max_evaluations) { budget=true; return VectorSample{"evaluation_limit",{}}; }
        ++out.evaluations;
        auto result=function(point);
        if(result.status=="available") {
            if(result.values.empty()) result.status="empty_vector";
            for(const auto value:result.values) if(!std::isfinite(value)) result.status="nonfinite_vector";
        }
        if(result.status!="available") ++out.rejected[result.status];
        return result;
    };
    out.center=evaluate(x);
    if(out.center.status!="available") { out.status="center_unavailable"; return out; }
    const auto m=out.center.values.size(), k=x.size();
    const auto epsilon=std::numeric_limits<double>::epsilon();
    std::vector<double> h(k);
    for(std::size_t j=0;j<k;++j) h[j]=c.difference_step*std::max(1.,std::abs(x[j]));
    for(int refinement=0;refinement<=c.difference_refinements;++refinement) {
        out.refinements=refinement;out.steps=h;
        std::array<DenseMatrix,3> estimates{{DenseMatrix(m,k),DenseMatrix(m,k),DenseMatrix(m,k)}};
        DenseMatrix roundoff(m,k);bool feasible=true;
        for(std::size_t level=0;level<3 && feasible;++level) for(std::size_t j=0;j<k && feasible;++j) {
            const auto step=std::ldexp(h[j],-static_cast<int>(level));
            auto plus=x,minus=x;plus[j]+=step;minus[j]-=step;
            if(plus[j]==x[j] || minus[j]==x[j]) { feasible=false;break; }
            const auto fp=evaluate(plus),fm=evaluate(minus);
            if(fp.status!="available" || fm.status!="available") { feasible=false;break; }
            if(fp.values.size()!=m || fm.values.size()!=m) throw std::logic_error("vector Jacobian cannot differentiate a changing domain");
            for(std::size_t i=0;i<m;++i) {
                estimates[level](i,j)=(fp.values[i]-fm.values[i])/(2*step);
                roundoff(i,j)=std::max(roundoff(i,j),64*epsilon*
                    std::max({std::abs(out.center.values[i]),std::abs(fp.values[i]),std::abs(fm.values[i])})/step);
            }
        }
        if(budget) { out.status="evaluation_limit";return out; }
        if(feasible) {
            DenseMatrix J(m,k),E(m,k);bool accurate=true;
            for(std::size_t j=0;j<k;++j) for(std::size_t i=0;i<m;++i) {
                J(i,j)=(4*estimates[2](i,j)-estimates[1](i,j))/3;
                const auto coarse=(4*estimates[1](i,j)-estimates[0](i,j))/3;
                E(i,j)=std::abs(J(i,j)-coarse)+roundoff(i,j);
                accurate=accurate && std::isfinite(J(i,j)) && std::isfinite(E(i,j)) &&
                    E(i,j)<=c.absolute_tolerance+c.relative_tolerance*std::abs(J(i,j));
            }
            // Unqualified derivatives are diagnostics only; callers must check
            // status before using them to construct inferential quantities.
            out.jacobian=std::move(J);out.error=std::move(E);
            if(accurate) { out.status="available";return out; }
        }
        for(auto& step:h) step*=.5;
    }
    out.status="derivatives_unresolved";return out;
}
} // namespace persephone
