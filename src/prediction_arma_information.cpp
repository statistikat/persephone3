#include "core/prediction_arma_information.hpp"
#include "core/parallel.hpp"
#include <cmath>
#include <stdexcept>
#include <utility>
namespace persephone {
namespace {
double finite_prediction(long double value) {
    const auto result=static_cast<double>(value);
    if(!std::isfinite(result) || (value!=0 && result==0))
        throw std::range_error("ARMA prediction uncertainty exceeds the finite numeric range");
    return result;
}
}
void assemble_prediction_delta(PredictionArmaInformation& out,
    const DenseMatrix& conditional,const std::vector<double>& conditional_standard_errors,
    const Interrupt& checked_interrupt) {
    const auto m=conditional.rows();
    if(conditional.columns()!=m || conditional_standard_errors.size()!=m ||
        !out.arma || out.arma->information.status!="available" || !out.arma->information.covariance ||
        out.derivatives.status!="available" || !out.derivatives.jacobian || !out.derivatives.error)
        throw std::logic_error("prediction delta requires qualified covariance and derivatives");
    const auto k=out.derivatives.jacobian->columns();
    if(out.derivatives.jacobian->rows()!=m || out.derivatives.error->rows()!=m ||
        out.derivatives.error->columns()!=k || out.arma->information.covariance->rows()!=k)
        throw std::logic_error("prediction delta dimensions disagree");
    DenseMatrix sensitivity(m,k),error(m,k);
    parallel_for(m*k,1,checked_interrupt,[&](std::size_t at) {
        const auto i=at%m,j=at/m;
        sensitivity(i,j)=finite_prediction(static_cast<long double>((*out.derivatives.jacobian)(i,j))*conditional_standard_errors[i]);
        error(i,j)=finite_prediction(static_cast<long double>((*out.derivatives.error)(i,j))*conditional_standard_errors[i]);
    });
    out.sensitivity=std::move(sensitivity);out.sensitivity_error=std::move(error);
    const auto factor=cholesky_factor(*out.arma->information.covariance,checked_interrupt);
    DenseMatrix loading(m,k);
    parallel_for(m*k,k,checked_interrupt,[&](std::size_t at) {
        const auto i=at%m,a=at/m;long double value=0;
        for(auto j=a;j<k;++j)value+=static_cast<long double>((*out.derivatives.jacobian)(i,j))*factor.upper(a,j);
        loading(i,a)=finite_prediction(value);
    });
    DenseMatrix increment(m,m),covariance(m,m);
    parallel_for(m,m*k,checked_interrupt,[&](std::size_t j) {
        for(std::size_t i=0;i<=j;++i) {
            long double value=0;bool nonzero=false;
            for(std::size_t a=0;a<k;++a) {
                value+=static_cast<long double>(loading(i,a))*loading(j,a);
                nonzero=nonzero || loading(i,a)!=0;
            }
            value*=static_cast<long double>(conditional_standard_errors[i])*conditional_standard_errors[j];
            const auto addition=finite_prediction(value);
            if(i==j && nonzero && addition==0)throw std::underflow_error("ARMA prediction variance underflows");
            increment(i,j)=increment(j,i)=addition;
            covariance(i,j)=covariance(j,i)=finite_prediction(
                static_cast<long double>(conditional(i,j))+addition);
        }
    });
    std::vector<double> se(m);
    parallel_for(m,1,checked_interrupt,[&](std::size_t i) {
        if(!(covariance(i,i)>0))throw std::range_error("ARMA prediction covariance has an invalid variance");
        se[i]=std::sqrt(covariance(i,i));
    });
    out.covariance=std::move(covariance);out.increment=std::move(increment);
    out.standard_errors=std::move(se);out.status="available";
}
} // namespace persephone
