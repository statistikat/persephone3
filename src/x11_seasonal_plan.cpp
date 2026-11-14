#include "core/x11_seasonal_plan.hpp"
#include <algorithm>
#include <stdexcept>
namespace persephone {
X11SeasonalPlan x11_seasonal_plan(int frequency,const std::vector<X11SeasonalRequest>& requests,
    X11SeasonalStage stage) {
    if(frequency!=4 && frequency!=12)throw std::invalid_argument("frequency must be 4 or 12");
    if(requests.size()!=static_cast<std::size_t>(frequency))throw std::invalid_argument("provide one request for every calendar period");
    bool initial=false,final=false;
    switch(stage) {
    case X11SeasonalStage::b_initial:case X11SeasonalStage::c_initial:case X11SeasonalStage::d_initial:initial=true;break;
    case X11SeasonalStage::b_final:case X11SeasonalStage::c_final:break;
    case X11SeasonalStage::d_final:final=true;break;
    default:throw std::invalid_argument("unsupported seasonal stage");
    }
    X11SeasonalPlan out;
    out.global_msr=std::all_of(requests.begin(),requests.end(),[](X11SeasonalRequest request){return request==X11SeasonalRequest::msr;});
    out.requires_msr=out.global_msr && final;
    for(const auto request:requests) {
        auto choice=static_cast<int>(request);auto resolution=X11SeasonalResolution::fixed;
        switch(request) {
        case X11SeasonalRequest::stable:case X11SeasonalRequest::s3x1:case X11SeasonalRequest::s3x3:
        case X11SeasonalRequest::s3x5:case X11SeasonalRequest::s3x9:case X11SeasonalRequest::s3x15:break;
        case X11SeasonalRequest::x11default:
            choice=initial ? 3 : 5;
            resolution=initial ? X11SeasonalResolution::x11default_initial : X11SeasonalResolution::x11default_final;break;
        case X11SeasonalRequest::msr:
            choice=initial ? 3 : 5;
            resolution=!out.global_msr ? X11SeasonalResolution::msr_mixed :
                (initial ? X11SeasonalResolution::msr_initial :
                (final ? X11SeasonalResolution::requires_msr : X11SeasonalResolution::msr_preliminary));break;
        default:throw std::invalid_argument("unsupported seasonal request");
        }
        out.base_filters.push_back(choice);out.resolutions.push_back(resolution);
    }
    return out;
}
} // namespace persephone
