# Frozen/offline checks, optional pinned public replay and warmed comparisons.
args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'x11-trend-selection-reference.R'));source(file.path(tooling,'fit-timing.R'))
root<-reference_root();base<-reference_fixture_dir(root);args<-commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--live','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-x11-trend-selection.R [--source] [--live] [--timing] [--output=<empty directory>]')
if('--timing' %in% args && !'--live' %in% args)stop('Timings require --live')
output<-args[startsWith(args,'--output=')];output<-if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
fixture_dir<-file.path(root,'inst/tinytest/fixtures/native/x11-trend-selection')
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
    startsWith(output,normalizePath(base)) || startsWith(output,normalizePath(fixture_dir))))stop('Output must be empty and outside frozen corpora')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
for(file in c('x11-symmetric-cases.R','x11-musgrave-cases.R','x11-trend-selection-cases.R'))source(file.path(root,'inst/tinytest/fixtures/native',file))
select<-getFromNamespace('native_x11_trend_select','persephone3');policy<-getFromNamespace('native_x11_trend_policy','persephone3')
endpoint<-getFromNamespace('native_x11_musgrave_spec','persephone3')
checks<-timed_calls<-0L;rows<-timings<-list()
assert<-function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
manifest<-read.csv(file.path(fixture_dir,'SHA256SUMS.csv'));before<-extended_parent_hashes(base)
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'frozen artifacts')
metadata<-dget(file.path(fixture_dir,'metadata.R'))
assert(identical(metadata$oracle_manifest_sha256,reference_sha256(file.path(base,'oracle-manifest.R'))),'pinned provenance')
frozen<-read.csv(file.path(fixture_dir,'ratios.csv'),stringsAsFactors=FALSE)
policies<-read.csv(file.path(fixture_dir,'policies.csv'),stringsAsFactors=FALSE)
defaults<-read.csv(file.path(fixture_dir,'defaults.csv'),stringsAsFactors=FALSE)
weights<-read.csv(file.path(fixture_dir,'weights.csv'),stringsAsFactors=FALSE)
pin<-if('--live' %in% args)extended_assert_oracle(base)else NULL
error<-function(a,b)if(is.finite(a) && is.finite(b))abs(a-b)/(1+abs(b))else if(identical(a,b))0 else Inf
for(ca in x11_trend_cases()) {
  y<-x11_trend_input(ca);native<-select(y,ca$mode,ca$backcasts,ca$forecasts)
  expected<-if(is.null(ca$expected_status))'available'else ca$expected_status
  assert(identical(native$status,expected),paste(ca$id,'status'))
  target<-frozen[frozen$case==ca$id,];frozen_error<-independent_error<-live_error<-NA_real_
  if(expected=='available') {
    oracle<-x11_trend_oracle(y,ca$frequency,ca$mode %in% c('multiplicative','pseudo_additive'),ca$backcasts,ca$forecasts)
    frozen_error<-error(native$comparison_ic_ratio,target$comparison_ic_ratio)
    independent_error<-error(native$comparison_ic_ratio,oracle$comparison_ic_ratio)
    assert(frozen_error<=2e-12,paste(ca$id,'frozen ratio'))
    assert(independent_error<=2e-12,paste(ca$id,'independent ratio'))
    assert(identical(native$selected_length,target$selected_length),paste(ca$id,'selected length'))
    for(field in c('trend_mean_change','irregular_mean_change','raw_ic_ratio'))
      assert(error(native[[field]],oracle[[field]])<=2e-12,paste(ca$id,field))
  } else assert(is.na(native$selected_length) && is.na(native$comparison_ic_ratio),paste(ca$id,'unavailable selection'))
  if('--live' %in% args) {
    live<-reference_x11_trend_ratio(y,ca$frequency,ca$mode,ca$backcasts,ca$forecasts)
    assert((is.na(live) && is.na(target$comparison_ic_ratio)) || error(live,target$comparison_ic_ratio)<=2e-12,paste(ca$id,'unchanged public output'))
    if(expected=='available') {
      live_error<-error(native$comparison_ic_ratio,live)
      assert(live_error<=2e-12,paste(ca$id,'live ratio'))
    }
  }
  rows[[ca$id]]<-data.frame(case=ca$id,frequency=ca$frequency,n=ca$n,mode=ca$mode,status=native$status,
    comparison_ic_ratio=native$comparison_ic_ratio,public_ic_ratio=target$comparison_ic_ratio,
    selected_length=native$selected_length,public_selected_length=target$selected_length,
    endpoint_ic_ratio=native$endpoint_ic_ratio,frozen_scaled_error=frozen_error,independent_scaled_error=independent_error,live_scaled_error=live_error)
}
prospective<-x11_trend_policy_cases()
for(j in seq_len(nrow(prospective))) {
  ca<-prospective[j,];result<-policy(ca$frequency,ic_ratio=ca$ratio)
  assert(identical(result$selected_length,policies$selected_length[match(ca$id,policies$id)]),paste(ca$id,'threshold'))
  if('--live' %in% args)assert(identical(result$selected_length,reference_x11_trend_length(ca$ratio,ca$frequency)),paste(ca$id,'live threshold'))
}
weight_errors<-independent_weight_errors<-c()
for(j in seq_len(nrow(defaults))) {
  ca<-defaults[j,];result<-policy(ca$frequency,length=ca$length)
  assert(identical(result$endpoint_ic_ratio,ca$endpoint_ic_ratio),paste(ca$case,'endpoint default'))
  if('--live' %in% args)assert(identical(result$endpoint_ic_ratio,reference_x11_trend_default(ca$length,ca$frequency)),paste(ca$case,'live default'))
  if(ca$length<=3L)next
  w<-weights[weights$case==ca$case,];full<-henderson_minimum_roughness(ca$length)
  bank<-if('--live' %in% args)reference_x11_trend_bank(ca$length,ca$frequency)else NULL
  for(q in unique(w$available)) {
    native<-unname(endpoint(ca$frequency,ca$length,q,result$endpoint_ic_ratio)$weights)
    err<-max(abs(native-w$weight[w$available==q]));weight_errors<-c(weight_errors,err)
    assert(err<=1e-14,paste(ca$case,q,'default endpoint weights'))
    err<-max(abs(native-x11_musgrave_minimum(full,q,result$endpoint_ic_ratio)));independent_weight_errors<-c(independent_weight_errors,err)
    assert(err<=1e-10,paste(ca$case,q,'independent default weights'))
    if(!is.null(bank))assert(max(abs(native-rJava::.jcall(bank[[q+1L]],'[D','weightsToArray')))<=1e-14,paste(ca$case,q,'live default weights'))
  }
}
if('--timing' %in% args) {
  workloads<-Filter(function(ca)ca$mode %in% c('additive','multiplicative') &&
    ca$group %in% c('regular','threshold') && ca$backcasts==0L && ca$forecasts==0L,x11_trend_cases())
  assert(length(workloads)==52L,'predeclared short timing matrix')
  for(f in c(4L,12L))for(mode in c('additive','multiplicative')) {
    id<-paste(f,mode,'long',sep='-');workloads[[id]]<-list(id=id,frequency=f,n=100003L,type='rough',mode=mode,
      backcasts=0L,forecasts=0L,start=c(-10L,3L))
  }
  for(ca in workloads) {
    y<-x11_trend_input(ca);functions<-list(native=function()select(y,ca$mode)$comparison_ic_ratio,
      jd_x11=function()reference_x11_trend_ratio(y,ca$frequency,ca$mode))
    target<-functions$jd_x11();assert(error(functions$native(),target)<=2e-12,paste(ca$id,'timing input'))
    sizes<-vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
      max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
    for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
      iterations<-sizes[[engine]];invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      while(sample$seconds<.05 && iterations<1000L) {
        iterations<-min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
        invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      }
      assert(sample$seconds>0 && error(sample$value,target)<=2e-12,paste(ca$id,'timed result'))
      sizes[[engine]]<-iterations
      timings[[length(timings)+1L]]<-data.frame(case=ca$id,n=ca$n,input_group=if(ca$n==100003L)'long'else'short',frequency=ca$frequency,
        mode=ca$mode,engine=engine,round=round,iterations=iterations,elapsed_seconds=sample$seconds,seconds_per_selection=sample$seconds/iterations)
    }
    cat(ca$id,'trend-selection timing PASS\n')
  }
}
assert(identical(before,extended_parent_hashes(base)),'original fixtures unchanged')
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'trend fixtures unchanged')
if(!'--live' %in% args)assert(!any(c('rJava','rjd3toolkit','rjd3x13','rjd3tramoseats') %in% loadedNamespaces()),'offline without Java')
files<-c(file.path(tooling,c('verify-x11-trend-selection.R','x11-trend-selection-reference.R','fit-timing.R')),
  file.path(root,c('src/x11_trend_selection.cpp','src/core/x11_trend_selection.hpp','src/rcpp_x11_trend_selection.cpp',
    'R/native_x11_trend_selection.R','src/x11_filters.cpp','src/core/x11_filters.hpp','src/x11_musgrave.cpp','src/core/x11_musgrave.hpp',
    'src/numerics.cpp','src/core/parallel.hpp','src/RcppExports.cpp','R/RcppExports.R',
    'inst/tinytest/fixtures/native/x11-symmetric-cases.R','inst/tinytest/fixtures/native/x11-musgrave-cases.R','inst/tinytest/fixtures/native/x11-trend-selection-cases.R')))
report<-do.call(rbind,rows)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  fixture_manifest_sha256=reference_sha256(file.path(fixture_dir,'SHA256SUMS.csv')),R=R.version.string,java=if(!is.null(pin))pin$java else NULL,
  checks=checks,ratio_cases=nrow(report),available_cases=sum(report$status=='available'),policy_cases=nrow(policies),default_cases=nrow(defaults),
  coefficient_max_error=max(weight_errors),independent_coefficient_max_error=max(independent_weight_errors),
  ratio_scaled_budget=2e-12,coefficient_absolute_budget=1e-14,independent_coefficient_absolute_budget=1e-10,
  timed_calls_including_warmup_and_calibration=timed_calls,
  timing_scope='52 short and 4 long 100003-row pilot-ratio/selection calls, not full fits. Native includes domain/count/status/choice metadata; public includes context construction, DoubleSeq creation and scalar ratio serialization.',
  timing_conventions='Three warmed alternating rounds; >=50ms calibrated batches when possible; loading/startup and explicit pre-batch GC excluded; natural in-batch GC included',
  parallel=getFromNamespace('.native_parallel_info','persephone3')())
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',nrow(report),'ratios /',nrow(policies),'policies /',nrow(defaults),'defaults\n')
print(vapply(report[c('frozen_scaled_error','independent_scaled_error','live_scaled_error')],function(x)if(all(is.na(x)))NA_real_ else max(x,na.rm=TRUE),numeric(1)))
cat('Default coefficient maxima:',max(weight_errors),max(independent_weight_errors),'\n')
if(length(timings)) {
  totals<-aggregate(seconds_per_selection~input_group+engine+round,do.call(rbind,timings),sum)
  print(aggregate(seconds_per_selection~input_group+engine,totals,median),row.names=FALSE)
}
