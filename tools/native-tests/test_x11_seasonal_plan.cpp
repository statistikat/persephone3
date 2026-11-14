#include "core/x11_seasonal_plan.hpp"
#include <cassert>
#include <iostream>
#include <limits>
#include <stdexcept>
namespace {
template<class F>void invalid(F run) {
    bool caught=false;try{run();}catch(const std::invalid_argument&){caught=true;}assert(caught);
}
}
int main() {
    using namespace persephone;using Request=X11SeasonalRequest;using Stage=X11SeasonalStage;using Reason=X11SeasonalResolution;
    for(const int f:{4,12}) {
        const auto size=static_cast<std::size_t>(f);std::vector<Request> requests(size,Request::msr);
        const auto pending=x11_seasonal_plan(f,requests,Stage::d_final);
        assert(pending.global_msr && pending.requires_msr);
        assert(pending.base_filters==std::vector<int>(size,5));
        assert(pending.resolutions==std::vector<Reason>(size,Reason::requires_msr));
        for(const auto stage:{Stage::b_initial,Stage::c_initial,Stage::d_initial}) {
            const auto initial=x11_seasonal_plan(f,requests,stage);
            assert(initial.global_msr && !initial.requires_msr && initial.base_filters==std::vector<int>(size,3));
        }
        for(const auto stage:{Stage::b_final,Stage::c_final}) {
            const auto intermediate=x11_seasonal_plan(f,requests,stage);
            assert(intermediate.global_msr && !intermediate.requires_msr && intermediate.base_filters==std::vector<int>(size,5));
        }
        for(std::size_t p=0;p<size;++p) {
            requests.assign(size,Request::msr);requests[p]=Request::stable;
            const auto mixed=x11_seasonal_plan(f,requests,Stage::d_final);
            assert(!mixed.global_msr && !mixed.requires_msr && mixed.base_filters[p]==0);
            assert(mixed.resolutions[p]==Reason::fixed && mixed.resolutions[(p+1)%size]==Reason::msr_mixed);
        }
        requests.assign(size,Request::x11default);
        assert(!x11_seasonal_plan(f,requests,Stage::d_final).global_msr);
        assert(x11_seasonal_plan(f,requests,Stage::b_initial).base_filters==std::vector<int>(size,3));
        assert(x11_seasonal_plan(f,requests,Stage::d_final).base_filters==std::vector<int>(size,5));
        for(const auto fixed:{Request::stable,Request::s3x1,Request::s3x3,Request::s3x5,Request::s3x9,Request::s3x15}) {
            requests.assign(size,fixed);
            for(const auto stage:{Stage::b_initial,Stage::b_final,Stage::c_initial,Stage::c_final,Stage::d_initial,Stage::d_final})
                assert(x11_seasonal_plan(f,requests,stage).base_filters==std::vector<int>(size,static_cast<int>(fixed)));
        }
        requests[0]=static_cast<Request>(std::numeric_limits<int>::max());invalid([&]{x11_seasonal_plan(f,requests,Stage::d_final);});
    }
    invalid([]{x11_seasonal_plan(52,{},Stage::d_final);});
    invalid([]{x11_seasonal_plan(4,{Request::msr},Stage::d_final);});
    invalid([]{x11_seasonal_plan(4,std::vector<Request>(4,Request::msr),static_cast<Stage>(6));});
    std::cout<<"PASS: fixed and staged choices, mixed/all MSR, unresolved final state and invalid requests\n";
}
