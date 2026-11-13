#include "core/information.hpp"

#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace persephone {
void InformationControl::validate() const {
    const auto positive = [](double x) { return std::isfinite(x) && x > 0; };
    if (!positive(difference_step) || difference_step > .1 || difference_refinements < 0 ||
        difference_refinements > 16 || max_evaluations < 1 || max_evaluations > 1000000 ||
        !positive(relative_tolerance) || relative_tolerance >= 1 || !positive(absolute_tolerance) ||
        !positive(gradient_tolerance) || !positive(rank_tolerance) || rank_tolerance >= 1 ||
        !positive(weak_tolerance) || weak_tolerance <= rank_tolerance || weak_tolerance >= 1) {
        throw std::invalid_argument("information controls are outside their supported ranges");
    }
}

ObservedInformation observed_information(const ObjectiveFunction& objective,
    const std::vector<double>& x, const InformationControl& c, const Interrupt& interrupt) {
    c.validate();
    if (!objective || x.size() > 64) throw std::invalid_argument("information requires an objective and at most 64 coefficients");
    for (const auto v : x) if (!std::isfinite(v)) throw std::invalid_argument("information coefficients must be finite");
    ObservedInformation out;
    const auto n = x.size();
    bool budget = false;
    const auto evaluate = [&](const std::vector<double>& point) {
        if (interrupt) interrupt();
        if (out.evaluations >= c.max_evaluations) {
            budget = true;
            return ObjectiveSample{std::numeric_limits<double>::quiet_NaN(), "evaluation_limit", {}};
        }
        ++out.evaluations;
        auto result = objective(point);
        if (result.status == "available" && !std::isfinite(result.value)) result.status = "nonfinite_objective";
        if (result.status != "available") ++out.rejected[result.status];
        return result;
    };
    out.center = evaluate(x);
    if (out.center.status != "available") { out.status = "center_unavailable"; return out; }
    if (!n) {
        out.status = "no_free_parameters"; out.derivative_status = "no_free_parameters";
        out.curvature_status = "no_free_parameters"; out.stationary = true; return out;
    }
    const double epsilon = std::numeric_limits<double>::epsilon();
    std::vector<double> h(n);
    for (std::size_t j = 0; j < n; ++j) h[j] = c.difference_step*std::max(1.,std::abs(x[j]));
    for (int refinement = 0; refinement <= c.difference_refinements; ++refinement) {
        out.refinements = refinement;
        std::array<std::vector<double>,3> gradients;
        std::array<DenseMatrix,3> hessians{{DenseMatrix(n,n),DenseMatrix(n,n),DenseMatrix(n,n)}};
        bool feasible = true;
        double magnitude = std::abs(out.center.value);
        for (std::size_t level = 0; level < 3 && feasible; ++level) {
            gradients[level].resize(n);
            const auto factor = std::ldexp(1.,-static_cast<int>(level));
            for (std::size_t j = 0; j < n && feasible; ++j) {
                const auto hj = h[j]*factor;
                auto plus = x, minus = x;
                plus[j] += hj; minus[j] -= hj;
                if (plus[j] == x[j] || minus[j] == x[j]) { feasible = false; break; }
                const auto fp = evaluate(plus), fm = evaluate(minus);
                if (fp.status != "available" || fm.status != "available") { feasible = false; break; }
                magnitude = std::max({magnitude,std::abs(fp.value),std::abs(fm.value)});
                gradients[level][j] = (fp.value-fm.value)/(2*hj);
                hessians[level](j,j) = ((fp.value-out.center.value)+(fm.value-out.center.value))/(hj*hj);
                for (std::size_t i = 0; i < j && feasible; ++i) {
                    const auto hi = h[i]*factor;
                    auto pp=plus,pm=plus,mp=minus,mm=minus;
                    pp[i]+=hi; pm[i]-=hi; mp[i]+=hi; mm[i]-=hi;
                    const auto fpp=evaluate(pp),fpm=evaluate(pm),fmp=evaluate(mp),fmm=evaluate(mm);
                    if (fpp.status!="available" || fpm.status!="available" || fmp.status!="available" || fmm.status!="available") { feasible=false; break; }
                    magnitude=std::max({magnitude,std::abs(fpp.value),std::abs(fpm.value),std::abs(fmp.value),std::abs(fmm.value)});
                    hessians[level](i,j)=hessians[level](j,i)=((fpp.value-fpm.value)-(fmp.value-fmm.value))/(4*hi*hj);
                }
            }
        }
        if (budget) { out.status="evaluation_limit"; out.derivative_status=out.status; return out; }
        if (feasible) {
            DenseMatrix H(n,n),E(n,n);
            out.gradient.resize(n); out.gradient_error.resize(n); out.steps=h;
            double hnorm=0,enorm=0,gnorm=0,gerror=0;
            bool finite=true;
            for (std::size_t j=0;j<n;++j) {
                const auto coarse=(4*gradients[1][j]-gradients[0][j])/3;
                out.gradient[j]=(4*gradients[2][j]-gradients[1][j])/3;
                out.gradient_error[j]=std::abs(out.gradient[j]-coarse)+64*epsilon*magnitude/(h[j]/4);
                finite=finite && std::isfinite(out.gradient[j]) && std::isfinite(out.gradient_error[j]);
                gnorm=std::max(gnorm,std::abs(out.gradient[j])+out.gradient_error[j]);
                gerror=std::max(gerror,out.gradient_error[j]);
                double row_h=0,row_e=0;
                for (std::size_t i=0;i<n;++i) {
                    H(i,j)=(4*hessians[2](i,j)-hessians[1](i,j))/3;
                    const auto old=(4*hessians[1](i,j)-hessians[0](i,j))/3;
                    E(i,j)=std::abs(H(i,j)-old)+128*epsilon*magnitude/(h[i]*h[j]/16);
                    finite=finite && std::isfinite(H(i,j)) && std::isfinite(E(i,j));
                    row_h+=std::abs(H(i,j)); row_e+=E(i,j);
                }
                hnorm=std::max(hnorm,row_h); enorm=std::max(enorm,row_e);
            }
            if(finite) { out.hessian=H; out.hessian_error=E; }
            if (finite && std::isfinite(hnorm) && std::isfinite(enorm) && std::isfinite(gnorm) &&
                enorm <= c.absolute_tolerance+c.relative_tolerance*hnorm && gerror <= c.gradient_tolerance/8) {
                out.derivative_status="available"; out.stationary=gnorm<=c.gradient_tolerance;
                // Positive diagonal scaling exposes correlation-related weakness.
                // Nonpositive diagonals use a common norm for signed diagnostics.
                std::vector<double> scale(n);
                bool positive=true;
                for(std::size_t j=0;j<n;++j) positive=positive && H(j,j)>0;
                for(std::size_t j=0;j<n;++j) scale[j]=std::sqrt(positive ? H(j,j) : std::max(hnorm,1.));
                DenseMatrix normalized(n,n);
                out.normalized_error=0;
                for(std::size_t j=0;j<n;++j) {
                    double row=0;
                    for(std::size_t i=0;i<n;++i) {
                        normalized(i,j)=H(i,j)/scale[i]/scale[j];
                        row+=E(i,j)/scale[i]/scale[j];
                    }
                    out.normalized_error=std::max(out.normalized_error,row);
                }
                out.normalized_eigenvalues=symmetric_eigenvalues(normalized,interrupt);
                const auto smallest=out.normalized_eigenvalues.front(),largest=out.normalized_eigenvalues.back();
                const auto cutoff=c.rank_tolerance*std::max(std::abs(largest),std::abs(smallest));
                out.rank=0;
                for(const auto v:out.normalized_eigenvalues) if(v>cutoff) ++out.rank;
                out.eigenvalue_ratio=largest>0 ? smallest/largest : 0;
                if(smallest < -out.normalized_error) out.curvature_status="indefinite_information";
                else if(out.rank<static_cast<int>(n)) out.curvature_status="rank_deficient_information";
                else if(smallest<=out.normalized_error) out.curvature_status="unresolved_curvature";
                else if(out.eigenvalue_ratio<=c.weak_tolerance) out.curvature_status="weak_information";
                else out.curvature_status="positive_definite";
                out.status=out.curvature_status;
                if(out.curvature_status=="positive_definite") {
                    if(!out.stationary) out.status="not_stationary";
                    else {
                        // Invert the scaled information, then restore physical units.
                        DenseMatrix identity(n,n);
                        for(std::size_t j=0;j<n;++j) identity(j,j)=1;
                        auto covariance=cholesky_solve(cholesky_factor(normalized,interrupt),identity,interrupt);
                        for(std::size_t j=0;j<n;++j) for(std::size_t i=0;i<=j;++i) {
                            const auto value=(covariance(i,j)+covariance(j,i))*.5/scale[i]/scale[j];
                            covariance(i,j)=covariance(j,i)=value;
                        }
                        covariance.validate(interrupt);
                        out.covariance=std::move(covariance); out.status="available";
                    }
                }
                return out;
            }
        }
        for(auto& step:h) step*=.5;
    }
    out.status="derivatives_unresolved"; out.derivative_status=out.status;
    return out;
}
} // namespace persephone
