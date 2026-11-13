#include "core/optimizer.hpp"

#include <cassert>
#include <cmath>
#include <iostream>
#include <limits>
#include <stdexcept>

namespace {
using namespace persephone;
ObjectiveSample sample(double value) { return {value,"available",{}}; }
ObjectiveSample rejected() { return {std::numeric_limits<double>::quiet_NaN(),"outside_domain",{}}; }
bool close(double a,double b,double tol=1e-6) { return std::abs(a-b)<tol*(1+std::abs(b)); }
void valid(const OptimizationResult& r,const OptimizerControl& c) {
    assert(r.evaluations<=c.max_evaluations && r.iterations<=c.max_iterations);
    for(std::size_t i=1;i<r.trace.size();++i) assert(r.trace[i].value<=r.trace[i-1].value);
    if(r.converged) {
        assert(r.status=="converged_gradient");
        for(std::size_t j=0;j<r.gradient.size();++j) assert(std::abs(r.gradient[j])+r.gradient_error[j]<=c.gradient_tolerance);
        assert(r.trace.back().gradient_bound<=c.gradient_tolerance);
    }
}
}

int main() {
    using namespace persephone;
    OptimizerControl c;
    const ObjectiveFunction quadratic=[](const auto& x) { return sample((x[0]-2)*(x[0]-2)+3*(x[1]+1)*(x[1]+1)); };
    const auto q=minimize_bfgs(quadratic,{-4,3},c);
    std::cout<<"quadratic "<<q.status<<" "<<q.iterations<<" "<<q.evaluations<<std::endl;
    valid(q,c); assert(q.converged && close(q.coordinates[0],2) && close(q.coordinates[1],-1));
    const auto repeated=minimize_bfgs(quadratic,{-4,3},c);
    assert(q.coordinates==repeated.coordinates && q.gradient==repeated.gradient && q.evaluations==repeated.evaluations);
    const ObjectiveFunction rosenbrock=[](const auto& x) { return sample(100*std::pow(x[1]-x[0]*x[0],2)+std::pow(1-x[0],2)); };
    const auto r=minimize_bfgs(rosenbrock,{-1.2,1},c);
    std::cout<<"rosenbrock "<<r.status<<" "<<r.iterations<<" "<<r.evaluations<<std::endl;
    valid(r,c); assert(r.converged && close(r.coordinates[0],1,2e-5) && close(r.coordinates[1],1,2e-5));
    const ObjectiveFunction restricted=[](const auto& x) { return x[0]<=0 || x[0]>=1 ? rejected() : sample(std::pow(x[0]-.4,2)); };
    const auto feasible=minimize_bfgs(restricted,{1e-8},c);
    valid(feasible,c); assert(feasible.converged && close(feasible.coordinates[0],.4));
    assert(feasible.rejected.at("outside_domain")>0);
    // At the narrow-domain starting point, only a qualified one-sided stencil
    // is possible. Zero steps still report that gradient, without convergence.
    c.max_iterations=0;
    const auto one=minimize_bfgs(restricted,{1e-8},c);
    assert(one.status=="iteration_limit" && one.difference_schemes[0]=="forward");
    assert(close(one.gradient[0],2*(1e-8-.4)) && one.gradient_error[0]<1e-6);
    c=OptimizerControl{};
    const auto nofree=minimize_bfgs([](const auto&){return sample(7);},{},c);
    assert(nofree.status=="no_free_parameters" && !nofree.converged && nofree.evaluations==1);
    const auto initial=minimize_bfgs(restricted,{-1},c);
    assert(initial.status=="initial_unavailable" && initial.evaluations==1 && std::isnan(initial.gradient[0]));
    // Exact budget enforcement through initial derivatives and later trials.
    for(int budget=1;budget<=20;++budget) {
        c.max_evaluations=budget;
        const auto limited=minimize_bfgs(quadratic,{-4,3},c);
        valid(limited,c); assert(limited.status=="evaluation_limit" && limited.evaluations==budget);
    }
    c=OptimizerControl{}; c.max_line_search=1;
    const auto no_step=minimize_bfgs([](const auto& x){return sample(1e4*x[0]*x[0]);},{.1},c);
    assert(no_step.status=="line_search_failed" && no_step.coordinates[0]==.1 && !no_step.converged);
    c=OptimizerControl{}; c.step_tolerance=10; c.function_tolerance=10;
    const auto stalled=minimize_bfgs(quadratic,{-4,3},c);
    assert(stalled.status=="stalled" && !stalled.converged && stalled.final.value<stalled.initial.value);
    c=OptimizerControl{}; c.gradient_tolerance=1e-16;
    const auto precision=minimize_bfgs([](const auto& x){return sample(1e8+x[0]*x[0]);},{0},c);
    assert(precision.status=="gradient_unavailable" && !precision.converged);
    c=OptimizerControl{};
    // All callback exceptions propagate, including after a candidate/gradient
    // was evaluated. Reuse must reproduce the untouched result.
    int calls=0;
    minimize_bfgs(restricted,{.1},c,[&]{++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int count=0; bool interrupted=false;
        try { minimize_bfgs(restricted,{.1},c,[&]{if(++count==stop) throw std::runtime_error("cancel");}); }
        catch(const std::runtime_error& e){interrupted=std::string(e.what())=="cancel";}
        assert(interrupted);
    }
    assert(minimize_bfgs(quadratic,{-4,3},c).coordinates==q.coordinates);
    bool bad=false;
    try { c.max_evaluations=0; minimize_bfgs(quadratic,{1,1},c); } catch(const std::invalid_argument&) { bad=true; }
    assert(bad);
    std::cout<<"PASS: analytic objectives, feasible differences, budgets, failures and "<<calls<<" cancellation boundaries\n";
}
