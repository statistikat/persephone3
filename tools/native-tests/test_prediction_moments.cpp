#include "core/prediction_moments.hpp"
#include "core/parallel.hpp"
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include <thread>

namespace {
void threads(const char* value) {
#ifdef _WIN32
    assert(_putenv_s("PERSEPHONE_NUM_THREADS",value)==0);
#else
    assert(setenv("PERSEPHONE_NUM_THREADS",value,1)==0);
#endif
}
}
int main() {
    using namespace persephone;
    DenseMatrix v(2,2);v(0,0)=.1;v(1,1)=.2;v(0,1)=v(1,0)=-.05;
    const auto x=prediction_moments({1,2},v,Transform::logarithm);
    assert(x.mean_status=="available" && x.covariance_status=="available");
    for(std::size_t i=0;i<2;++i) {
        assert(std::abs(x.means[i]/std::exp(static_cast<double>(i+1)+v(i,i)/2)-1)<1e-13);
        for(std::size_t j=0;j<2;++j)
            assert(std::abs((*x.covariance)(i,j)/(x.means[i]*x.means[j]*std::expm1(v(i,j)))-1)<1e-13);
    }
    DenseMatrix extreme(2,2);extreme(0,0)=extreme(1,1)=1000;extreme(0,1)=extreme(1,0)=500;
    const auto stable=prediction_moments({-1000,-1000},extreme,Transform::logarithm);
    assert(stable.median_status=="original_median_range" && stable.mean_status=="available" && stable.covariance_status=="available");
    assert(std::abs((*stable.covariance)(0,0)-1)<1e-13);
    assert(std::abs((*stable.covariance)(0,1)/std::exp(-500.)-1)<1e-13);
    DenseMatrix one(1,1);one(0,0)=1;
    for(const double m:{-500.,500.}) {
        const auto range=prediction_moments({m},one,Transform::logarithm);
        assert(range.mean_status=="available" && range.covariance_status=="original_covariance_range");
        assert(!range.covariance && std::isfinite(range.standard_errors[0]) && range.standard_error_status=="available");
        assert(std::abs(range.standard_errors[0]/std::exp(m+.5)/std::sqrt(std::expm1(1.))-1)<1e-13);
    }
    const auto thread=std::this_thread::get_id();int calls=0;
    prediction_moments({1,2},v,Transform::logarithm,[&] {assert(std::this_thread::get_id()==thread);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {prediction_moments({1,2},v,Transform::logarithm,[&] {if(++at==stop)throw std::runtime_error("cancel moments");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel moments";}
        assert(caught);
    }
    assert(prediction_moments({1,2},v,Transform::logarithm).covariance->values()==x.covariance->values());
    DenseMatrix large(260,260);std::vector<double> means(260);
    for(std::size_t i=0;i<260;++i) {
        means[i]=std::sin(static_cast<double>(i+1));
        for(std::size_t j=0;j<260;++j)large(i,j)=i==j ? .11 : .01;
    }
    threads("1");const auto serial=prediction_moments(means,large,Transform::logarithm);
    threads("2");const auto parallel=prediction_moments(means,large,Transform::logarithm);
    assert(parallel.covariance_status=="available" && serial.covariance->values()==parallel.covariance->values());
    assert(serial.means==parallel.means && serial.standard_errors==parallel.standard_errors);
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    std::cout<<"PASS: signed lognormal moments, extreme exponent/range cases, 260-point serial/OpenMP and "<<calls<<" cancellation boundaries\n";
}
