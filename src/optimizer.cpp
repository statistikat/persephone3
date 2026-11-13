#include "core/optimizer.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace persephone {
namespace {
const double missing = std::numeric_limits<double>::quiet_NaN();
double norm_inf(const std::vector<double>& v) {
    double n = 0;
    for (const auto x : v) { if (!std::isfinite(x)) return missing; n = std::max(n, std::abs(x)); }
    return n;
}
long double dot(const std::vector<double>& a, const std::vector<double>& b) {
    long double v = 0;
    for (std::size_t i = 0; i < a.size(); ++i) v += static_cast<long double>(a[i]) * b[i];
    return v;
}
std::vector<double> identity(std::size_t n) {
    std::vector<double> h(n*n, 0);
    for (std::size_t i = 0; i < n; ++i) h[i+n*i] = 1;
    return h;
}
struct Evaluator {
    const ObjectiveFunction& function;
    const OptimizerControl& control;
    const Interrupt& interrupt;
    OptimizationResult& result;
    bool exhausted = false;
    ObjectiveSample operator()(const std::vector<double>& x) {
        if (interrupt) interrupt();
        if (result.evaluations >= control.max_evaluations) {
            exhausted = true; return {missing, "evaluation_limit", {}};
        }
        ++result.evaluations;
        // Range loss in a proposal is a rejected evaluation, never an infinite
        // coordinate passed to a caller that expects ordinary finite inputs.
        const bool finite=std::all_of(x.begin(),x.end(),[](double value){return std::isfinite(value);});
        auto sample = finite ? function(x) : ObjectiveSample{missing,"nonfinite_parameters",{}};
        if (sample.status == "available") {
            if (!std::isfinite(sample.value)) throw std::logic_error("available optimizer objective must be finite");
        } else ++result.rejected[sample.status];
        return sample;
    }
};

// Estimate a complete gradient at the current accepted point. Differences at h
// and h/2 must agree to the declared absolute/relative accuracy budget. The
// cancellation floor is conservative floating arithmetic, not a statistical SE.
bool gradient(Evaluator& evaluate, OptimizationResult& result) {
    const auto n = result.coordinates.size();
    ++result.gradient_evaluations;
    result.gradient.assign(n, missing); result.gradient_error.assign(n, missing);
    result.difference_steps.assign(n, missing); result.difference_schemes.assign(n, "unavailable");
    const auto& control = evaluate.control;
    const auto f0 = result.final.value;
    for (std::size_t j = 0; j < n; ++j) {
        double h = control.difference_step * std::max(1.0, std::abs(result.coordinates[j]));
        bool found = false;
        for (int attempt = 0; attempt <= control.difference_refinements; ++attempt, h *= .5) {
            const auto point = [&](double delta) {
                auto x = result.coordinates;
                x[j] += delta;
                if (!std::isfinite(x[j]) || x[j] == result.coordinates[j]) return ObjectiveSample{missing,"step_resolution_limit",{}};
                return evaluate(x);
            };
            const auto plus = point(h), minus = point(-h);
            if (evaluate.exhausted) return false;
            const auto half_plus = point(h/2), half_minus = point(-h/2);
            if (evaluate.exhausted) return false;
            double coarse = missing, fine = missing, scale = std::max(1.0,std::abs(f0));
            std::string scheme;
            const auto add_scale = [&](const ObjectiveSample& sample) {
                if (sample.status == "available") scale = std::max(scale, std::abs(sample.value));
            };
            add_scale(plus); add_scale(minus); add_scale(half_plus); add_scale(half_minus);
            if (plus.status == "available" && minus.status == "available" &&
                half_plus.status == "available" && half_minus.status == "available") {
                coarse = static_cast<double>((static_cast<long double>(plus.value)-minus.value)/(2*h));
                fine = static_cast<double>((static_cast<long double>(half_plus.value)-half_minus.value)/h);
                scheme = "central";
            } else {
                // Prefer the positive side deterministically when both work.
                for (const int sign : {1,-1}) {
                    const auto& one = sign > 0 ? plus : minus;
                    const auto& half = sign > 0 ? half_plus : half_minus;
                    if (one.status != "available" || half.status != "available") continue;
                    const auto two = point(sign*2*h);
                    if (evaluate.exhausted) return false;
                    if (two.status != "available") continue;
                    add_scale(two);
                    const long double a = static_cast<long double>(one.value)-f0;
                    coarse = static_cast<double>(sign*(4*a-(static_cast<long double>(two.value)-f0))/(2*h));
                    fine = static_cast<double>(sign*(4*(static_cast<long double>(half.value)-f0)-a)/h);
                    scheme = sign > 0 ? "forward" : "backward";
                    break;
                }
            }
            if (!std::isfinite(coarse) || !std::isfinite(fine)) continue;
            const auto error = std::abs(fine-coarse)/3 + 16*std::numeric_limits<double>::epsilon()*scale/h;
            if (!std::isfinite(error) || error > control.gradient_tolerance/8 + 1e-3*std::abs(fine)) continue;
            result.gradient[j] = fine; result.gradient_error[j] = error;
            result.difference_steps[j] = h/2; result.difference_schemes[j] = scheme;
            found = true; break;
        }
        if (!found) return false;
    }
    return true;
}
bool stationary(const OptimizationResult& r, double tolerance) {
    for (std::size_t j=0; j<r.gradient.size(); ++j) {
        if (!std::isfinite(r.gradient[j]) || !std::isfinite(r.gradient_error[j]) ||
            std::abs(r.gradient[j])+r.gradient_error[j] > tolerance) return false;
    }
    return true;
}
void trace(OptimizationResult& r, double step=0, double change=0, double alpha=0) {
    double bound=0;
    for(std::size_t j=0;j<r.gradient.size();++j) {
        if(!std::isfinite(r.gradient[j]) || !std::isfinite(r.gradient_error[j])) { bound=missing; break; }
        bound=std::max(bound,std::abs(r.gradient[j])+r.gradient_error[j]);
    }
    r.trace.push_back({r.iterations,r.evaluations,r.final.value,norm_inf(r.gradient),
        norm_inf(r.gradient_error),step,change,alpha,bound});
}
bool update_inverse(std::vector<double>& h, const std::vector<double>& s,
                    const std::vector<double>& y) {
    const auto n = s.size();
    const long double ys=dot(y,s), ss=dot(s,s), yy=dot(y,y);
    if (!std::isfinite(ys) || !std::isfinite(ss) || !std::isfinite(yy) ||
        ys <= 1e-10L*std::sqrt(ss)*std::sqrt(yy) || ys<=0) return false;
    std::vector<double> hy(n,0);
    for (std::size_t i=0; i<n; ++i) {
        long double total=0;
        for (std::size_t j=0; j<n; ++j) total+=static_cast<long double>(h[i+n*j])*y[j];
        hy[i]=static_cast<double>(total);
        if(!std::isfinite(hy[i])) return false;
    }
    const long double coefficient=(ys+dot(y,hy))/(ys*ys);
    if(!std::isfinite(coefficient)) return false;
    auto next=h;
    for (std::size_t i=0; i<n; ++i) for(std::size_t j=i; j<n; ++j) {
        const long double value=h[i+n*j]+coefficient*s[i]*s[j] -
            (static_cast<long double>(hy[i])*s[j]+static_cast<long double>(s[i])*hy[j])/ys;
        const auto entry=static_cast<double>(value);
        if(!std::isfinite(entry) || (i==j && entry<=0)) return false;
        next[i+n*j]=next[j+n*i]=entry;
    }
    h=std::move(next); return true;
}
} // namespace

void OptimizerControl::validate() const {
    if (max_iterations<0 || max_iterations>10000 || max_evaluations<1 || max_evaluations>1000000 ||
        max_line_search<1 || max_line_search>100 || difference_refinements<0 || difference_refinements>20) {
        throw std::invalid_argument("optimizer iteration/evaluation controls exceed their supported bounds");
    }
    for (const auto value : {gradient_tolerance,step_tolerance,difference_step,armijo,max_step}) {
        if(!std::isfinite(value) || value<=0) throw std::invalid_argument("optimizer tolerances and step controls must be finite and positive");
    }
    if(!std::isfinite(function_tolerance) || function_tolerance<0 || difference_step>.1 || armijo>=.5) {
        throw std::invalid_argument("invalid optimizer function tolerance, difference step or Armijo constant");
    }
}

OptimizationResult minimize_bfgs(const ObjectiveFunction& objective,
    const std::vector<double>& start, const OptimizerControl& control, const Interrupt& interrupt) {
    control.validate();
    if(!objective || start.size()>64) throw std::invalid_argument("optimizer needs an objective and at most 64 coordinates");
    for(const auto value:start) if(!std::isfinite(value)) throw std::invalid_argument("optimizer start coordinates must be finite");
    OptimizationResult result{};
    result.initial_coordinates=result.coordinates=start;
    result.gradient.assign(start.size(),missing); result.gradient_error.assign(start.size(),missing);
    result.difference_steps.assign(start.size(),missing); result.difference_schemes.assign(start.size(),"unavailable");
    Evaluator evaluate{objective,control,interrupt,result};
    result.initial=result.final=evaluate(start);
    if(result.final.status!="available") { result.status="initial_unavailable"; return result; }
    if(start.empty()) { result.status="no_free_parameters"; trace(result); return result; }
    bool valid_gradient=gradient(evaluate,result);
    trace(result);
    if(!valid_gradient) { result.status=evaluate.exhausted?"evaluation_limit":"gradient_unavailable"; return result; }
    auto h=identity(start.size());
    while(true) {
        if(interrupt) interrupt();
        if(stationary(result,control.gradient_tolerance)) { result.status="converged_gradient"; result.converged=true; return result; }
        if(result.iterations>=control.max_iterations) { result.status="iteration_limit"; return result; }
        const auto n=start.size();
        std::vector<double> direction(n,0);
        for(std::size_t i=0;i<n;++i) {
            long double total=0;
            for(std::size_t j=0;j<n;++j) total-=static_cast<long double>(h[i+n*j])*result.gradient[j];
            direction[i]=static_cast<double>(total);
        }
        if(!std::isfinite(norm_inf(direction)) || dot(direction,result.gradient)>=0) {
            h=identity(n); ++result.bfgs_resets;
            for(std::size_t j=0;j<n;++j) direction[j]=-result.gradient[j];
        }
        const auto length=norm_inf(direction);
        if(length>control.max_step) for(auto& value:direction) value=(value/length)*control.max_step;
        const auto slope=dot(direction,result.gradient);
        if(!std::isfinite(slope) || slope>=0) { result.status="direction_failure"; return result; }
        double alpha=1;
        bool accepted=false;
        std::vector<double> proposal(n);
        ObjectiveSample trial{missing,"unavailable",{}};
        for(int line=0;line<control.max_line_search;++line,alpha*=.5) {
            bool moved=false;
            for(std::size_t j=0;j<n;++j) { proposal[j]=result.coordinates[j]+alpha*direction[j]; moved=moved || proposal[j]!=result.coordinates[j]; }
            if(!moved) { result.status="step_resolution_limit"; return result; }
            trial=evaluate(proposal);
            if(evaluate.exhausted) { result.status="evaluation_limit"; return result; }
            if(trial.status=="available" && static_cast<long double>(trial.value)<=
               static_cast<long double>(result.final.value)+control.armijo*alpha*slope) { accepted=true; break; }
        }
        if(!accepted) { result.status="line_search_failed"; return result; }
        const auto old=result.coordinates, old_gradient=result.gradient, old_error=result.gradient_error;
        const auto change=result.final.value-trial.value;
        result.coordinates=proposal; result.final=std::move(trial); ++result.iterations;
        std::vector<double> s(n),y(n);
        for(std::size_t j=0;j<n;++j) s[j]=result.coordinates[j]-old[j];
        const auto step=norm_inf(s);
        valid_gradient=gradient(evaluate,result);
        trace(result,step,change,alpha);
        if(!valid_gradient) { result.status=evaluate.exhausted?"evaluation_limit":"gradient_unavailable"; return result; }
        if(stationary(result,control.gradient_tolerance)) { result.status="converged_gradient"; result.converged=true; return result; }
        if(step<=control.step_tolerance*(1+norm_inf(result.coordinates)) &&
           change<=control.function_tolerance*(1+std::abs(result.final.value))) {
            result.status="stalled"; return result;
        }
        for(std::size_t j=0;j<n;++j) y[j]=result.gradient[j]-old_gradient[j];
        long double curvature_error=0;
        for(std::size_t j=0;j<n;++j) curvature_error+=std::abs(static_cast<long double>(s[j]))*(old_error[j]+result.gradient_error[j]);
        if(dot(s,y)<=curvature_error || !update_inverse(h,s,y)) { h=identity(n); ++result.bfgs_resets; }
    }
}

} // namespace persephone
