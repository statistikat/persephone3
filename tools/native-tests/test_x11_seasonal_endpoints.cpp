#include "core/x11_seasonal_endpoints.hpp"
#include "core/parallel.hpp"
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <limits>
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
template<class F> void invalid(F run) {
    bool caught=false;try{run();}catch(const std::invalid_argument&){caught=true;}assert(caught);
}
}
int main() {
    using namespace persephone;
    for(const int frequency:{4,12})for(const int N:{1,3,5,9,15}) {
        for(int q=0;q<(N+1)/2;++q) {
            const auto left=x11_seasonal_endpoint_definition(frequency,N,q,X11SeasonalSide::left);
            const auto right=x11_seasonal_endpoint_definition(frequency,N,q,X11SeasonalSide::right);
            double sum=0;
            for(std::size_t j=0;j<left.weights.size();++j) {
                const auto reverse=left.weights.size()-1-j;
                assert(left.weights[j]>0 && left.weights[j]==right.weights[reverse]);
                assert(left.offsets[j]==-right.offsets[reverse]);sum+=left.weights[j];
            }
            assert(std::abs(sum-1)<1e-14);
        }
        const int annual_minimum=N==15 ? 20 : std::max(5,N+2);
        assert(x11_seasonal_minimum_phase_length(N)==annual_minimum);
        const auto minimum=static_cast<std::size_t>(frequency*annual_minimum);
        MaskedSeries short_series{std::vector<double>(minimum-1,0),std::vector<std::uint8_t>(minimum-1,0)};
        invalid([&]{x11_seasonal_endpoints(short_series,frequency,N);});
        MaskedSeries y{std::vector<double>(minimum+3),std::vector<std::uint8_t>(minimum+3,0)};
        for(std::size_t i=0;i<y.values.size();++i)y.values[i]=static_cast<double>(i%static_cast<std::size_t>(frequency)+1);
        const auto periodic=x11_seasonal_endpoints(y,frequency,N);
        for(std::size_t i=0;i<y.values.size();++i)assert(std::abs(periodic.values[i]-y.values[i])<1e-14 && !periodic.missing[i]);
        for(std::size_t i=0;i<y.values.size();i+=static_cast<std::size_t>(frequency))y.missing[i]=1;
        const auto masked=x11_seasonal_endpoints(y,frequency,N);
        for(std::size_t i=0;i<y.values.size();++i) {
            assert(masked.missing[i]==(i%static_cast<std::size_t>(frequency)==0));
            if(!masked.missing[i])assert(masked.values[i]==periodic.values[i]);
        }
    }
    const auto rational=x11_seasonal_endpoint_definition(12,5,2,X11SeasonalSide::left);
    assert(rational.weights==std::vector<double>({9./60,13./60,13./60,13./60,8./60,4./60}));
    const auto decimal=x11_seasonal_endpoint_definition(12,15,0,X11SeasonalSide::left);
    assert(decimal.weights==std::vector<double>({.16,.16,.16,.16,.16,.06667,.06667,.04444,.02222}));
    assert(x11_seasonal_endpoint_definition(12,15,4,X11SeasonalSide::left).weights.back()==.02219);
    MaskedSeries y{std::vector<double>(40003),std::vector<std::uint8_t>(40003,0)};
    for(std::size_t i=0;i<y.values.size();++i)y.values[i]=2+std::sin(static_cast<double>(i)*.7)+static_cast<double>(i)/31;
    y.missing[1]=y.missing[19001]=1;y.values[1]=y.values[19001]=0;
    const auto calling=std::this_thread::get_id();int total_calls=0;
    for(const int N:{9,15}) {
        threads("1");const auto serial=x11_seasonal_endpoints(y,12,N);
        threads("2");const auto parallel=x11_seasonal_endpoints(y,12,N);
        assert(serial.values==parallel.values && serial.missing==parallel.missing);
        if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
        int calls=0;
        x11_seasonal_endpoints(y,12,N,[&]{assert(std::this_thread::get_id()==calling);++calls;});
        for(int stop=1;stop<=calls;++stop) {
            int at=0;bool caught=false;
            try{x11_seasonal_endpoints(y,12,N,[&]{if(++at==stop)throw std::runtime_error("cancel seasonal endpoints");});}
            catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel seasonal endpoints";}
            assert(caught);
        }
        assert(x11_seasonal_endpoints(y,12,N).values==serial.values);total_calls+=calls;
        std::cout<<"3x"<<N<<": "<<calls<<" cancellation boundaries\n";
    }
    invalid([&]{x11_seasonal_endpoints(y,12,25);});
    invalid([]{x11_seasonal_minimum_phase_length(std::numeric_limits<int>::max());});
    invalid([&]{x11_seasonal_endpoints(y,52,3);});
    invalid([]{x11_seasonal_endpoint_definition(12,3,2,X11SeasonalSide::left);});
    invalid([]{x11_seasonal_endpoint_definition(12,3,-1,X11SeasonalSide::left);});
    invalid([]{x11_seasonal_endpoint_definition(12,3,std::numeric_limits<int>::max(),X11SeasonalSide::left);});
    invalid([]{x11_seasonal_endpoint_definition(12,3,std::numeric_limits<int>::min(),X11SeasonalSide::left);});
    invalid([]{x11_seasonal_endpoint_definition(12,3,0,static_cast<X11SeasonalSide>(9));});
    y.values[0]=std::numeric_limits<double>::infinity();
    invalid([&]{x11_seasonal_endpoints(y,12,9);});
    y.missing[0]=1;invalid([&]{x11_seasonal_endpoints(y,12,9);});
    y.values[0]=0;assert(x11_seasonal_endpoints(y,12,9).missing[0]);
    std::cout<<"PASS: seasonal rational/decimal/mirror/phase/minimum/mask/range checks, actual OpenMP and "<<total_calls<<" cancellation boundaries\n";
}
