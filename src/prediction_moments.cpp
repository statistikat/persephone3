#include "core/prediction_moments.hpp"
#include "core/parallel.hpp"
#include <cmath>
#include <exception>
#include <limits>
#include <stdexcept>
#include <utility>

namespace persephone {
namespace {
double exponent(long double logarithm) {
    const auto result=static_cast<double>(std::exp(logarithm));
    if(!std::isfinite(result) || !(result>0))
        throw std::range_error("original prediction moment overflows or underflows");
    return result;
}
}
PredictionMoments prediction_moments(const std::vector<double>& location,
    const DenseMatrix& covariance,Transform transform,const Interrupt& interrupt) {
    const auto n=location.size();
    if(!n || covariance.rows()!=n || covariance.columns()!=n)
        throw std::invalid_argument("prediction moment dimensions disagree");
    if(transform!=Transform::none && transform!=Transform::logarithm)
        throw std::invalid_argument("unsupported prediction transformation");
    std::vector<double> sd(n);
    for(std::size_t i=0;i<n;++i) {
        if(interrupt)interrupt();
        if(!std::isfinite(location[i]) || !std::isfinite(covariance(i,i)) || !(covariance(i,i)>0))
            throw std::invalid_argument("prediction locations and positive variances must be finite");
        sd[i]=std::sqrt(covariance(i,i));
    }
    DenseMatrix correlation(n,n);
    parallel_for(n,n,interrupt,[&](std::size_t j) {
        for(std::size_t i=0;i<n;++i) {
            const auto entry=covariance(i,j);
            if(!std::isfinite(entry) || entry!=covariance(j,i))
                throw std::invalid_argument("prediction covariance must be finite and symmetric");
            correlation(i,j)=entry/sd[i]/sd[j];
            if(!std::isfinite(correlation(i,j)) || (entry!=0 && correlation(i,j)==0))
                throw std::range_error("prediction correlation exceeds the finite range");
        }
    });
    (void)cholesky_factor(correlation,interrupt);
    PredictionMoments out;
    const auto unavailable=std::numeric_limits<double>::quiet_NaN();
    out.medians.assign(n,unavailable);out.means=out.medians;out.standard_errors=out.medians;
    if(transform==Transform::none) {
        out.medians=location;out.means=location;out.standard_errors=std::move(sd);out.covariance=covariance;
        out.median_status=out.mean_status=out.covariance_status=out.standard_error_status="available";return out;
    }
    std::exception_ptr interrupted;
    const Interrupt checked_interrupt=[&] {
        if(!interrupt)return;
        try {interrupt();}catch(...) {interrupted=std::current_exception();throw;}
    };
    try {
        std::vector<double> medians(n);
        parallel_for(n,1,checked_interrupt,[&](std::size_t i){medians[i]=exponent(location[i]);});
        out.medians=std::move(medians);out.median_status="available";
    } catch(const std::runtime_error& error) {
        if(interrupted)std::rethrow_exception(interrupted);
        out.median_status="original_median_range";out.median_message=error.what();
    }
    std::vector<long double> log_mean(n);
    for(std::size_t i=0;i<n;++i) {
        if((i&255U)==0)checked_interrupt();
        log_mean[i]=static_cast<long double>(location[i])+.5L*covariance(i,i);
    }
    try {
        std::vector<double> means(n);
        parallel_for(n,1,checked_interrupt,[&](std::size_t i){means[i]=exponent(log_mean[i]);});
        out.means=std::move(means);out.mean_status="available";
    } catch(const std::runtime_error& error) {
        if(interrupted)std::rethrow_exception(interrupted);
        out.mean_status="original_mean_range";out.mean_message=error.what();
    }
    // A standard deviation can remain representable when its variance does
    // not. Qualify it independently in log units, without squaring it first.
    try {
        std::vector<double> se(n);
        parallel_for(n,1,checked_interrupt,[&](std::size_t i) {
            const auto v=static_cast<long double>(covariance(i,i));
            const auto logarithm=v>std::log(2.L) ?
                (log_mean[i]+.5L*v)+.5L*std::log1p(-std::exp(-v)) :
                log_mean[i]+.5L*std::log(std::expm1(v));
            se[i]=exponent(logarithm);
        });
        out.standard_errors=std::move(se);out.standard_error_status="available";
    } catch(const std::runtime_error& error) {
        if(interrupted)std::rethrow_exception(interrupted);
        out.standard_error_status="original_standard_error_range";out.standard_error_message=error.what();
    }
    try {
        DenseMatrix original(n,n);
        parallel_for(n,n,checked_interrupt,[&](std::size_t j) {
            for(std::size_t i=0;i<=j;++i) {
                const auto c=static_cast<long double>(covariance(i,j));
                double value=0;
                if(c!=0) {
                    long double logarithm;
                    if(c>std::log(2.L)) {
                        // Split the large positive cross variance between the
                        // two log means before adding; exp(c) is never formed.
                        logarithm=(log_mean[i]+.5L*c)+(log_mean[j]+.5L*c)+std::log1p(-std::exp(-c));
                    } else logarithm=log_mean[i]+log_mean[j]+std::log(std::abs(std::expm1(c)));
                    value=exponent(logarithm)*(c>0 ? 1 : -1);
                }
                original(i,j)=original(j,i)=value;
            }
        });
        out.covariance=std::move(original);out.covariance_status="available";
    } catch(const std::runtime_error& error) {
        if(interrupted)std::rethrow_exception(interrupted);
        out.covariance_status="original_covariance_range";out.covariance_message=error.what();
    }
    return out;
}
} // namespace persephone
