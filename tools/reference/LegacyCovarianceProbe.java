// Public-API observer only. No optimizer or numerical implementation is copied.
import jdplus.toolkit.base.api.data.DoubleSeq;
import jdplus.toolkit.base.core.arima.estimation.IArimaMapping;
import jdplus.toolkit.base.core.math.functions.IParametersDomain;
import jdplus.toolkit.base.core.math.functions.ssq.*;
import jdplus.toolkit.base.core.math.functions.levmar.LevenbergMarquardtMinimizer;
import jdplus.toolkit.base.core.math.matrices.FastMatrix;
import jdplus.toolkit.base.core.regarima.RegArimaModel;
import jdplus.toolkit.base.core.regsarima.RegSarimaComputer;
import jdplus.toolkit.base.core.sarima.SarimaModel;
import java.util.ArrayList;
public final class LegacyCovarianceProbe {
 static final class Trace {
  double[] point,steps; FastMatrix jacobian;
 }
 static final class Function implements ISsqFunction {
  final ISsqFunction original; final Trace trace;
  Function(ISsqFunction f,Trace t){original=f;trace=t;}
  public IParametersDomain getDomain(){return original.getDomain();}
  public ISsqFunctionPoint ssqEvaluate(DoubleSeq p){return new Point(original.ssqEvaluate(p),this);}
 }
 static final class Point implements ISsqFunctionPoint {
  final ISsqFunctionPoint original; final Function function;
  Point(ISsqFunctionPoint p,Function f){original=p;function=f;}
  public ISsqFunction getSsqFunction(){return function;}
  public DoubleSeq getE(){return original.getE();}
  // Delegation preserves the original summation's floating-point behavior.
  public double getSsqE(){return original.getSsqE();}
  public DoubleSeq getParameters(){return original.getParameters();}
  public ISsqFunctionDerivatives ssqDerivatives(){
   var p=original.getParameters();var t=function.trace;t.point=p.toArray();
   t.steps=new double[p.length()];
   for(int i=0;i<p.length();i++)t.steps[i]=function.getDomain().epsilon(p,i);
   t.jacobian=FastMatrix.make(getE().length(),p.length());
   var d=original.ssqDerivatives();d.jacobian(t.jacobian);return d;
  }
 }
 static final class Observer implements SsqFunctionMinimizer {
  final LevenbergMarquardtMinimizer original; final Trace trace=new Trace(); FastMatrix retained; Trace retainedTrace;
  Observer(LevenbergMarquardtMinimizer x){original=x;}
  public FastMatrix curvatureAtMinimum(){var h=original.curvatureAtMinimum();retained=h.deepClone();retainedTrace=new Trace();retainedTrace.point=trace.point.clone();retainedTrace.steps=trace.steps.clone();retainedTrace.jacobian=trace.jacobian.deepClone();return h;}
  public ISsqFunctionPoint getResult(){return ((Point)original.getResult()).original;}
  public DoubleSeq gradientAtMinimum(){return original.gradientAtMinimum();}
  public double getObjective(){return original.getObjective();}
  public int getIterationsCount(){return original.getIterationsCount();}
  public boolean minimize(ISsqFunctionPoint p){return original.minimize(new Point(p,new Function(p.getSsqFunction(),trace)));}
 }
 static final class Builder implements SsqFunctionMinimizer.Builder {
  final LevenbergMarquardtMinimizer.LmBuilder original=LevenbergMarquardtMinimizer.builder();
  Observer last;
  public Builder functionPrecision(double v){original.functionPrecision(v);return this;}
  public Builder maxIter(int n){original.maxIter(n);return this;}
  public Observer build(){last=new Observer(original.build());return last;}
 }
 // Output: k,Q,retained damping,isotropy error, final[k],working[k],steps[k],cov[k*k].
 public static double[] capture(RegArimaModel<SarimaModel> model,IArimaMapping<SarimaModel> mapping,int[] missing) {
  model=model.toBuilder().missing(missing).build();
  var builder=new Builder();
  var computer=RegSarimaComputer.builder().precision(1e-12).computeExactFinalDerivatives(false).minimizer(builder).build();
  var result=computer.process(model,mapping);var observer=builder.last;var trace=observer.retainedTrace;
  var point=result.getMax().getParameters();int k=point.length();
  var curvature=observer.retained; double mu=0,error=0;
  var extra=FastMatrix.square(k);
  for(int j=0;j<k;j++)for(int i=0;i<k;i++) {
   double gram=0;for(int t=0;t<trace.jacobian.getRowsCount();t++)gram+=trace.jacobian.get(t,i)*trace.jacobian.get(t,j);
   extra.set(i,j,curvature.get(i,j)/2-gram);
  }
  for(int j=0;j<k;j++)mu+=extra.get(j,j)/k;
  for(int j=0;j<k;j++)for(int i=0;i<k;i++)error=Math.max(error,Math.abs(extra.get(i,j)-(i==j?mu:0)));
  if(mu<0){error=Math.max(error,-mu);mu=0;}
  var cov=result.getMax().asymptoticCovariance();var out=new double[4+3*k+k*k];int at=0;
  out[at++]=k;out[at++]=observer.getResult().getSsqE();out[at++]=mu;out[at++]=error;
  for(int j=0;j<k;j++)out[at++]=point.get(j);
  for(int j=0;j<k;j++)out[at++]=trace.point[j];
  for(int j=0;j<k;j++)out[at++]=trace.steps[j];
  for(int j=0;j<k;j++)for(int i=0;i<k;i++)out[at++]=cov.get(i,j);
  return out;
 }
}
