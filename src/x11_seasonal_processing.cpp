#include "core/x11_seasonal_processing.hpp"
#include "core/parallel.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>
namespace persephone {
namespace {
struct PhasePlan {
    std::size_t first=0,count=0,radius=0;
    X11FilterDefinition central;
    std::vector<X11FilterDefinition> left,right;
    double mean=0,scale=0,sum=0;
    bool mean_missing=false;
};
void phase_means(const MaskedSeries& series,std::size_t frequency,std::vector<PhasePlan>& plans,
    const std::vector<std::uint8_t>& needed,const Interrupt& interrupt) {
    std::size_t maximum=0;
    for(std::size_t period=0;period<frequency;++period)if(needed[period])maximum=std::max(maximum,plans[period].count);
    // Independent phases own their scale and running sum. Successive bounded
    // annual chunks preserve each phase's exact serial addition order; only
    // the caller polls between joined chunks, including on very long inputs.
    for(int pass=0;pass<2;++pass)for(std::size_t first=0;first<maximum;) {
        const auto width=std::min(std::size_t{8192},maximum-first);
        parallel_for(frequency,2*width,interrupt,[&](std::size_t period) {
            auto& phase=plans[period];
            if(!needed[period] || phase.mean_missing || first>=phase.count || (pass==1 && phase.scale==0))return;
            const auto end=first+std::min(width,phase.count-first);
            for(auto j=first;j<end;++j) {
                const auto row=phase.first+j*frequency;
                if(pass==0) {
                    if(series.missing[row]) {phase.mean_missing=true;break;}
                    phase.scale=std::max(phase.scale,std::abs(series.values[row]));
                } else phase.sum+=series.values[row]/phase.scale;
            }
        });
        first+=width;
    }
    for(std::size_t period=0;period<frequency;++period) {
        auto& phase=plans[period];
        if(!needed[period] || phase.mean_missing || phase.scale==0)continue;
        phase.mean=(phase.sum/static_cast<double>(phase.count))*phase.scale;
        if(!std::isfinite(phase.mean))throw std::overflow_error("non-finite seasonal phase mean");
    }
}
void apply(const MaskedSeries& input,MaskedSeries& output,std::size_t row,
    const X11FilterDefinition& filter) {
    // The selected phase plan guarantees in-range support. Resolve all active
    // missing taps before arithmetic, exactly as in the finite-filter primitive.
    for(std::size_t j=0;j<filter.weights.size();++j) {
        if(filter.weights[j]==0)continue;
        const auto at=static_cast<std::size_t>(static_cast<std::int64_t>(row)+filter.offsets[j]);
        if(input.missing[at]) {output.missing[row]=1;return;}
    }
    double value=0;
    for(std::size_t j=0;j<filter.weights.size();++j) {
        if(filter.weights[j]==0)continue;
        const auto at=static_cast<std::size_t>(static_cast<std::int64_t>(row)+filter.offsets[j]);
        value=value+filter.weights[j]*input.values[at];
        if(!std::isfinite(value))throw std::overflow_error("non-finite result in seasonal processing arithmetic");
    }
    output.values[row]=value;
}
}
X11SeasonalProcessing x11_seasonal_process(const MaskedSeries& series,int frequency,
    int start_period,const std::vector<int>& filters,X11ShortSeriesPolicy policy,
    const Interrupt& interrupt) {
    if(frequency!=4 && frequency!=12)throw std::invalid_argument("frequency must be 4 or 12");
    if(start_period<1 || start_period>frequency)throw std::invalid_argument("start period must be between 1 and frequency");
    const auto f=static_cast<std::size_t>(frequency),n=series.values.size();
    if(filters.size()!=f)throw std::invalid_argument("provide one seasonal filter for every calendar period");
    if(policy!=X11ShortSeriesPolicy::reject && policy!=X11ShortSeriesPolicy::x11)
        throw std::invalid_argument("unsupported short-series policy");
    for(const auto N:filters)if(N!=0)x11_seasonal_minimum_phase_length(N);
    series.validate();
    if(n<f)throw std::invalid_argument("seasonal processing requires at least one observation in every phase");
    X11SeasonalProcessing result;
    result.values={std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)};
    result.selected_filters=filters;result.phase_lengths.resize(f);
    result.reasons.resize(f,X11SeasonalSelectionReason::requested_filter);
    result.row_methods.resize(n);result.available.assign(n,-1);
    std::vector<PhasePlan> plans(f);std::vector<std::uint8_t> mean_needed(f,0);std::size_t work=1;
    for(std::size_t period=0;period<f;++period) {
        if(interrupt)interrupt();
        auto& plan=plans[period];
        plan.first=(period+f-static_cast<std::size_t>(start_period-1))%f;
        plan.count=1+(n-1-plan.first)/f;result.phase_lengths[period]=plan.count;
        auto& N=result.selected_filters[period];auto& reason=result.reasons[period];
        if(N==0)reason=X11SeasonalSelectionReason::requested_stable;
        else if(policy==X11ShortSeriesPolicy::reject) {
            if(plan.count<static_cast<std::size_t>(x11_seasonal_minimum_phase_length(N)))
                throw std::invalid_argument("requested seasonal filter lacks explicit minimum phase support");
        } else if(n/f<5) {N=0;reason=X11SeasonalSelectionReason::short_span;}
        else if(N==15 && n/f+(n%f!=0)<20) {N=0;reason=X11SeasonalSelectionReason::short_3x15_span;}
        if(N!=0) {
            plan.radius=static_cast<std::size_t>((N+1)/2);
            if(plan.count<=plan.radius) {
                N=0;reason=X11SeasonalSelectionReason::insufficient_endpoint_support;
            } else {
                plan.central=x11_filter_definition(X11FilterKind::seasonal,frequency,N);
                const auto endpoints=std::min(plan.radius,plan.count-plan.radius);
                for(std::size_t q=0;q<endpoints;++q) {
                    plan.left.push_back(x11_seasonal_endpoint_definition(frequency,N,static_cast<int>(q),X11SeasonalSide::left));
                    plan.right.push_back(x11_seasonal_endpoint_definition(frequency,N,static_cast<int>(q),X11SeasonalSide::right));
                }
                work=std::max(work,2*plan.central.weights.size());
            }
        }
        mean_needed[period]=N==0 || plan.count<2*plan.radius;
    }
    phase_means(series,f,plans,mean_needed,interrupt);
    // Each worker owns one full-calendar row, including its method metadata.
    // Definitions/phase means are immutable; callbacks stay on the caller.
    parallel_for(n,work,interrupt,[&](std::size_t row) {
        const auto period=(row%f+static_cast<std::size_t>(start_period-1))%f;
        const auto& plan=plans[period];const auto j=(row-plan.first)/f;
        auto& method=result.row_methods[row];const X11FilterDefinition* filter=nullptr;
        if(result.selected_filters[period]==0)method=X11SeasonalRowMethod::stable;
        else if(j>=plan.radius && plan.count-1-j>=plan.radius) {
            method=X11SeasonalRowMethod::symmetric;filter=&plan.central;
        } else if(j<plan.left.size()) {
            method=X11SeasonalRowMethod::left;filter=&plan.left[j];result.available[row]=static_cast<int>(j);
        } else if(plan.count-1-j<plan.right.size()) {
            const auto q=plan.count-1-j;method=X11SeasonalRowMethod::right;
            filter=&plan.right[q];result.available[row]=static_cast<int>(q);
        } else method=X11SeasonalRowMethod::short_mean;
        if(filter)apply(series,result.values,row,*filter);
        else {result.values.values[row]=plan.mean;result.values.missing[row]=plan.mean_missing;}
    });
    return result;
}
} // namespace persephone
