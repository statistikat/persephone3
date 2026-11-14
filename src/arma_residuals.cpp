#include "core/arma_residuals.hpp"
#include "core/parallel.hpp"
#include <algorithm>
#include <cmath>
#include <exception>
#include <limits>
#include <stdexcept>
#include <utility>

namespace persephone {
namespace {
std::vector<double> standardized(const std::vector<double>& values, double sd,
    const Interrupt& interrupt) {
    std::vector<double> result(values.size());
    parallel_for(values.size(),1,interrupt,[&](std::size_t i) {
        const auto value=values[i]/sd;
        if(!std::isfinite(value) || (values[i]!=0 && value==0)) {
            throw std::range_error("residual standardization exceeds the finite numeric range");
        }
        result[i]=value;
    });
    return result;
}
double legacy_checked(long double value) {
    const auto result=static_cast<double>(value);
    if(!std::isfinite(result) || (value!=0 && result==0)) throw std::range_error("legacy residual reporting exceeds the finite numeric range");
    return result;
}
double rounded_product(double left,double right) {
    // Explicit binary64 rounding prevents contraction into an FMA. Tiny
    // predictor tails can choose a different Householder orientation at a
    // mathematically zero pivot, even though both likelihoods are identical.
    volatile double value=left*right;
    return value;
}
std::vector<double> ar1_reporting_design(const CompleteResiduals& projection,
    double phi,int d,int seasonal_d,int period,const Interrupt& interrupt) {
    const auto polynomial=difference_polynomial(d,seasonal_d,period,interrupt);
    const auto n=projection.rows.size(),m=projection.missing_rows.size();
    std::vector<double> result(n*m);
    // Independent scalar fast-innovation covariance update. In exact
    // arithmetic F1=1 and C1=phi. Preserve the actual rounded update here only
    // for legacy reporting, rather than simplifying those identities.
    const auto F0=1/(1-rounded_product(phi,phi));
    const auto C0=rounded_product(phi,F0),ratio=C0/F0;
    const auto F1=F0-rounded_product(ratio,C0);
    const auto C1=C0-rounded_product(ratio,rounded_product(phi,C0));
    if(!(F0>0) || !(F1>0) || !std::isfinite(F0) || !std::isfinite(F1))
        throw std::range_error("legacy AR(1) innovation variance is unavailable");
    // After the first observed value an AR(1) has innovation variance exactly
    // one. Retain the rounded covariance-update ratio for its prediction gain,
    // while normalizing the startup innovation and gain separately.
    const auto sd0=std::sqrt(F0),gain0=C0/sd0,gain1=C1/F1;
    for(std::size_t j=0;j<m;++j) {
        double state=0;
        for(std::size_t i=0;i<n;++i) {
            if((i&255U)==0 && interrupt)interrupt();
            double value=0;
            if(projection.rows[i]>=projection.missing_rows[j]) {
                const auto lag=projection.rows[i]-projection.missing_rows[j];
                if(lag<polynomial.size())value=polynomial[lag];
            }
            const auto innovation=value-state;
            result[i+n*j]=innovation/(i ? 1 : sd0);
            // Normalized innovation and normalized prediction gain are
            // multiplied separately, preserving the fast reporting convention.
            state=rounded_product(phi,state)+rounded_product(i ? gain1 : gain0,result[i+n*j]);
        }
    }
    return result;
}
// Unpivoted Householder elimination with chronological missing columns. The
// column sign is supplied explicitly by the differencing convention. A zero
// leading entry takes the nonnegative branch, including a signed negative zero.
// This reporting basis is not used for fitting, covariance or observed rows.
std::vector<double> compressed_residuals(std::vector<double> residuals,
    std::vector<double> design, std::size_t missing, int sign, const Interrupt& interrupt) {
    const auto n=residuals.size();
    if(missing>=n || design.size()!=n*missing) throw std::invalid_argument("legacy missing basis dimensions do not agree");
    for(auto& value:design)value*=sign;
    std::vector<double> reflector(n);
    for(std::size_t j=0;j<missing;++j) {
        if(interrupt)interrupt();
        double norm_squared=0;
        for(auto i=j;i<n;++i) {
            if((i&255U)==0 && interrupt)interrupt();
            norm_squared+=rounded_product(design[i+n*j],design[i+n*j]);
        }
        const auto length=std::sqrt(norm_squared);
        if(!(length>0) || !std::isfinite(length)) throw std::range_error("legacy missing basis is numerically rank deficient");
        for(auto i=j;i<n;++i) reflector[i]=design[i+n*j]/length;
        reflector[j]+=design[j+n*j]>=0 ? 1 : -1;
        double squared=0;
        for(auto i=j;i<n;++i)squared+=rounded_product(reflector[i],reflector[i]);
        const auto tau=2/squared;
        const auto apply=[&](double* values) {
            double dot=0;
            for(auto i=j;i<n;++i)dot+=rounded_product(reflector[i],values[i]);
            dot*=tau;
            for(auto i=j;i<n;++i) {
                if((i&255U)==0 && interrupt)interrupt();
                values[i]=legacy_checked(values[i]-rounded_product(dot,reflector[i]));
            }
        };
        apply(residuals.data());
        for(auto column=j+1;column<missing;++column)apply(design.data()+n*column);
    }
    return std::vector<double>(residuals.begin()+static_cast<std::ptrdiff_t>(missing),residuals.end());
}
} // namespace
ArmaResiduals arma_residuals(const MaskedSeries& series, const RegressionDesign& design,
    const ArmaParameterMap& map, const std::vector<double>& coordinates,
    int period, int d, int seasonal_d, double mean, double rank_tolerance,
    double log_jacobian, ResidualScale scale, const Interrupt& interrupt) {
    ArmaResiduals out;out.variance=std::numeric_limits<double>::quiet_NaN();
    out.free_arma=map.initial_coordinates().size();
    std::exception_ptr interrupted;
    const Interrupt checked_interrupt=[&] {
        if(!interrupt) return;
        try { interrupt(); }
        catch(...) { interrupted=std::current_exception();throw; }
    };
    out.center=std::make_shared<ArmaObjective>(evaluate_arma_objective(series,design,map,coordinates,
        period,d,seasonal_d,mean,rank_tolerance,log_jacobian,checked_interrupt));
    for(const auto fixed:design.fixed) if(!fixed) ++out.free_regression;
    if(out.center->likelihood) out.effective_count=out.center->likelihood->finite_count;
    const auto estimated=out.free_arma+out.free_regression;
    out.residual_df=out.effective_count>estimated ? out.effective_count-estimated : 0;
    if(out.center->status!="available") return out;
    const auto& fit=*out.center->likelihood;
    out.rows=fit.finite_rows;out.unit_innovations=fit.whitened_residuals;
    const auto denominator=scale==ResidualScale::ML ? out.effective_count : out.residual_df;
    if(!denominator) { out.status="nonpositive_residual_df";return out; }
    out.variance=fit.sum_squared_standardized/static_cast<double>(denominator);
    if(!(out.variance>0) || !std::isfinite(out.variance)) { out.status="residual_scale_range";return out; }
    const auto sd=std::sqrt(out.variance);
    try { out.standardized_innovations=standardized(out.unit_innovations,sd,checked_interrupt); }
    catch(const std::runtime_error&) {
        if(interrupted) std::rethrow_exception(interrupted);
        out.status="standardization_range";return out;
    }
    out.status="available";
    const auto& p=out.center->parameters.coefficients;
    try {
        out.complete=sarima_complete_residuals(series,design,p[0],p[1],p[2],p[3],period,d,seasonal_d,
            mean,rank_tolerance,checked_interrupt);
        if(out.complete.status=="available") {
            out.scaled_complete_residuals=standardized(out.complete.residuals,sd,checked_interrupt);
        }
    } catch(const std::runtime_error& error) {
        if(interrupted) std::rethrow_exception(interrupted);
        out.complete.status="numerical_failure";out.complete_message=error.what();
        out.scaled_complete_residuals.clear();
    }
    return out;
}

LegacyArmaResiduals legacy_arma_residuals(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaResiduals& accurate, int period,
    int d, int seasonal_d, double mean, double rank_tolerance,
    Transform transformation, const Interrupt& interrupt) {
    LegacyArmaResiduals out;
    if(accurate.status!="available" || accurate.complete.status!="available")return out;
    std::exception_ptr interrupted;
    const Interrupt checked_interrupt=[&] {
        if(!interrupt)return;
        try {interrupt();}catch(...) {interrupted=std::current_exception();throw;}
    };
    out.compressed=accurate.free_regression==0;
    out.basis=out.compressed ? "jdplus_3.9.0_missing_projected_householder" : "jdplus_3.9.0_complete_reported_series";
    out.correction_sign=(d+seasonal_d)%2 ? -1 : 1;
    try {
        const auto& missing=accurate.complete.missing_rows;
        out.preliminary.resize(missing.size());out.interpolated=accurate.complete.interpolated;
        for(std::size_t j=0;j<missing.size();) {
            checked_interrupt();
            auto end=j+1;
            while(end<missing.size() && missing[end]==missing[end-1]+1)++end;
            const auto a=series.values[missing[j]-1],b=series.values[missing[end-1]+1];
            const double midpoint=transformation==Transform::logarithm ?
                std::max(a,b)+std::log1p(std::exp(-std::abs(a-b)))-std::log(2.) : a/2+b/2;
            for(auto k=j;k<end;++k) {
                out.preliminary[k]=legacy_checked(midpoint);
                if(out.correction_sign<0) out.interpolated[k]=legacy_checked(
                    2*static_cast<long double>(midpoint)-accurate.complete.interpolated[k]);
            }
            j=end;
        }
        const auto& p=accurate.center->parameters.coefficients;
        auto projection=sarima_complete_residuals(series,design,p[0],p[1],p[2],p[3],period,d,seasonal_d,
            mean,rank_tolerance,checked_interrupt,{true,out.compressed});
        if(projection.status!="available") {out.status="projection_"+projection.status;return out;}
        if(projection.rows!=accurate.complete.rows || projection.missing_rows!=missing) {
            throw std::logic_error("legacy and accurate residual projection domains differ");
        }
        if(out.compressed) {
            if(!missing.empty() && p[0].size()==1 && p[1].empty() && p[2].empty() && p[3].empty()) {
                projection.missing_whitened_design=ar1_reporting_design(projection,p[0][0],d,seasonal_d,period,checked_interrupt);
            }
            out.residuals=missing.empty() ? projection.residuals : compressed_residuals(
                projection.residuals,std::move(projection.missing_whitened_design),missing.size(),out.correction_sign,checked_interrupt);
            out.calendar_rows.assign(projection.rows.end()-static_cast<std::ptrdiff_t>(out.residuals.size()),projection.rows.end());
        } else {
            out.residuals=accurate.complete.residuals;out.calendar_rows=projection.rows;
            const auto n=out.residuals.size();
            parallel_for(n,missing.size(),checked_interrupt,[&](std::size_t i) {
                long double value=out.residuals[i];
                for(std::size_t j=0;j<missing.size();++j) value+=static_cast<long double>(projection.missing_whitened_design[i+n*j])*
                    (static_cast<long double>(out.interpolated[j])-accurate.complete.interpolated[j]);
                out.residuals[i]=legacy_checked(value);
            });
        }
        long double squares=0;
        for(std::size_t i=0;i<out.residuals.size();++i) {
            if((i&255U)==0)checked_interrupt();
            squares+=static_cast<long double>(out.residuals[i])*out.residuals[i];
        }
        out.sum_squares=legacy_checked(squares);
        out.scaled_residuals=standardized(out.residuals,std::sqrt(accurate.variance),checked_interrupt);
        out.status="available";
    } catch(const std::runtime_error& error) {
        if(interrupted)std::rethrow_exception(interrupted);
        out.status="numerical_failure";out.message=error.what();
        out.residuals.clear();out.scaled_residuals.clear();out.calendar_rows.clear();
    }
    return out;
}
} // namespace persephone
