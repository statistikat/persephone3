#include "core/arma_information.hpp"
#include <algorithm>
#include <cmath>
#include <limits>

namespace persephone {
JointRegressionInformation joint_regression_information(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    const std::vector<double>& coordinates, int period, int d, int seasonal_d,
    double mean, double rank_tolerance, double log_jacobian,
    const InformationControl& control, const Interrupt& interrupt) {
    JointRegressionInformation out;
    out.ml_variance=std::numeric_limits<double>::quiet_NaN();
    out.arma=arma_information(series,design,map,coordinates,period,d,seasonal_d,
        mean,rank_tolerance,log_jacobian,control,interrupt);
    out.evaluations=out.arma.information.evaluations;
    const auto center=out.arma.estimation_center;
    if(!center || !center->likelihood) return out;
    const auto& fit=*center->likelihood;
    out.free_columns=fit.free_columns;out.effective_count=fit.finite_count;
    for(const auto j:out.free_columns) out.coefficients.push_back(fit.coefficients.at(j));
    const auto r=out.free_columns.size(),k=out.arma.coefficients.size(),q=r+k;
    out.residual_df=out.effective_count>q ? out.effective_count-q : 0;
    if(out.arma.information.status!="available" && out.arma.information.status!="no_free_parameters") return out;
    out.ml_variance=fit.profiled_variance;
    if(!(out.ml_variance>0) || !std::isfinite(out.ml_variance) || fit.rank!=r || fit.ml_covariance.size()!=r*r) {
        out.status="regression_scale_or_rank_unavailable";return out;
    }
    if(!q) { out.status="no_free_parameters";out.derivatives.status="not_required";return out; }
    if(r) out.conditional_ml_covariance=DenseMatrix(r,r,fit.ml_covariance);
    std::vector<double> scale(q);
    for(std::size_t i=0;i<q;++i) {
        const auto v=i<r ? fit.ml_covariance[i+r*i] : (*out.arma.information.covariance)(i-r,i-r);
        if(!(v>0) || !std::isfinite(v)) { out.status="joint_covariance_range";return out; }
        scale[i]=std::sqrt(v);
    }
    bool range=true;
    const auto convert=[&](long double value) {
        const auto converted=static_cast<double>(value);
        range=range && std::isfinite(converted) && (value==0 || converted!=0);
        return converted;
    };
    if(r && k) {
        const auto remaining=control.max_evaluations-out.evaluations;
        if(remaining<=0) { out.status=out.derivatives.status="evaluation_limit";return out; }
        auto differences=control;differences.max_evaluations=remaining;
        auto correction_design=design;
        std::fill(correction_design.coefficients.begin(),correction_design.coefficients.end(),0);
        const auto parameters=map.decode(coordinates);
        const VectorFunction corrections=[&](const std::vector<double>& values) {
            auto coefficients=parameters.coefficients;std::size_t at=0;
            for(std::size_t b=0;b<4;++b) for(std::size_t j=0;j<map.blocks()[b].fixed.size();++j) {
                if(!map.blocks()[b].fixed[j]) coefficients[b][j]=values.at(at++);
            }
            // Affine GLS identity: fitting y-X*beta_final gives the coefficient
            // correction without subtracting two large nearly equal estimates.
            // Known fixed effects have already been removed; the fixed mean
            // and original missing/calendar domain are preserved.
            const auto candidate=evaluate_arma_physical_objective(fit.residual_series,correction_design,map,
                coefficients,period,d,seasonal_d,mean,rank_tolerance,0,interrupt);
            if(candidate.status!="available") return VectorSample{candidate.status,{}};
            const auto& next=*candidate.likelihood;
            if(next.finite_count!=fit.finite_count || next.free_columns!=fit.free_columns) {
                return VectorSample{"regression_domain_changed",{}};
            }
            std::vector<double> value(r);
            for(std::size_t i=0;i<r;++i) value[i]=next.coefficients[out.free_columns[i]]/scale[i];
            return VectorSample{"available",std::move(value)};
        };
        out.derivatives=vector_jacobian(corrections,out.arma.coefficients,differences,interrupt);
        out.evaluations+=out.derivatives.evaluations;
        if(out.derivatives.status!="available") { out.status=out.derivatives.status;return out; }
        DenseMatrix B(r,k),E(r,k);
        for(std::size_t j=0;j<k;++j) for(std::size_t i=0;i<r;++i) {
            B(i,j)=convert(static_cast<long double>((*out.derivatives.jacobian)(i,j))*scale[i]);
            E(i,j)=convert(static_cast<long double>((*out.derivatives.error)(i,j))*scale[i]);
        }
        if(!range) { out.status="regression_sensitivity_range";return out; }
        out.sensitivity=std::move(B);out.sensitivity_error=std::move(E);
    } else out.derivatives.status="not_required";

    // Work in units of conditional-ML beta SD and profile theta SD. If
    // B=d beta_hat/d theta and Vtheta=inverse(Hprofile), block inversion gives
    // Vbeta=Q/n*inverse(Xw'Xw)+B*Vtheta*B', C=B*Vtheta. There is no df multiplier.
    DenseMatrix normalized(q,q);
    for(std::size_t j=0;j<r;++j) for(std::size_t i=0;i<r;++i) {
        normalized(i,j)=convert(static_cast<long double>(fit.ml_covariance[i+r*j])/scale[i]/scale[j]);
    }
    for(std::size_t j=0;j<k;++j) for(std::size_t i=0;i<k;++i) {
        normalized(r+i,r+j)=convert(static_cast<long double>((*out.arma.information.covariance)(i,j))/scale[r+i]/scale[r+j]);
    }
    if(r && k) {
        DenseMatrix T(r,k);
        for(std::size_t j=0;j<k;++j) for(std::size_t i=0;i<r;++i) {
            T(i,j)=convert(static_cast<long double>((*out.derivatives.jacobian)(i,j))*scale[r+j]);
        }
        for(std::size_t j=0;j<k;++j) for(std::size_t i=0;i<r;++i) {
            long double value=0;
            for(std::size_t a=0;a<k;++a) value+=static_cast<long double>(T(i,a))*normalized(r+a,r+j);
            normalized(i,r+j)=normalized(r+j,i)=convert(value);
        }
        for(std::size_t j=0;j<r;++j) {
            if(interrupt) interrupt();
            for(std::size_t i=0;i<=j;++i) {
                long double value=normalized(i,j);
                for(std::size_t a=0;a<k;++a) value+=static_cast<long double>(normalized(i,r+a))*T(j,a);
                normalized(i,j)=normalized(j,i)=convert(value);
            }
        }
    }
    if(!range) { out.status="joint_covariance_range";return out; }
    // Correlation eigenvalues detect an unresolved/weak joint direction without
    // depending on regression measurement units. This is covariance conditioning,
    // not an observed-information or optimizer convergence diagnostic.
    DenseMatrix correlation(q,q);
    for(std::size_t j=0;j<q;++j) {
        if(!(normalized(j,j)>0)) { out.status="nonpositive_joint_covariance";return out; }
        for(std::size_t i=0;i<q;++i) {
            correlation(i,j)=normalized(i,j)/std::sqrt(normalized(i,i))/std::sqrt(normalized(j,j));
        }
    }
    out.normalized_eigenvalues=symmetric_eigenvalues(correlation,interrupt);
    out.eigenvalue_ratio=out.normalized_eigenvalues.front()/out.normalized_eigenvalues.back();
    if(out.eigenvalue_ratio<=control.rank_tolerance) { out.status="rank_deficient_joint_covariance";return out; }
    if(out.eigenvalue_ratio<=control.weak_tolerance) { out.status="weak_joint_covariance";return out; }
    DenseMatrix joint(q,q);
    for(std::size_t j=0;j<q;++j) {
        if(interrupt) interrupt();
        for(std::size_t i=0;i<=j;++i) joint(i,j)=joint(j,i)=convert(static_cast<long double>(normalized(i,j))*scale[i]*scale[j]);
    }
    if(!range) { out.status="joint_covariance_range";return out; }
    if(r) {
        DenseMatrix beta(r,r);
        for(std::size_t j=0;j<r;++j) for(std::size_t i=0;i<r;++i) beta(i,j)=joint(i,j);
        out.covariance=std::move(beta);
    }
    if(r && k) {
        DenseMatrix cross(r,k);
        for(std::size_t j=0;j<k;++j) for(std::size_t i=0;i<r;++i) cross(i,j)=joint(i,r+j);
        out.cross_covariance=std::move(cross);
    }
    out.joint_covariance=std::move(joint);out.status="available";
    return out;
}
} // namespace persephone
