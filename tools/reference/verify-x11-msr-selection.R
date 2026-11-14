args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script))
for(file in c('common.R','extended-fixtures.R','x11-filter-reference.R','x11-mean-reference.R',
  'x11-msr-reference.R','x11-msr-selection-reference.R','fit-timing.R'))source(file.path(tooling,file))
root<-reference_root();base<-reference_fixture_dir(root);args<-commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--live','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-x11-msr-selection.R [--source] [--live] [--timing] [--output=<empty directory>]')
if('--timing' %in% args && !'--live' %in% args)stop('Timings require --live')
output<-args[startsWith(args,'--output=')];output<-if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
fixture_dir<-file.path(root,'inst/tinytest/fixtures/native/x11-msr-selection')
if(!is.null(output) && (length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)) ||
    startsWith(output,normalizePath(fixture_dir))))stop('Output must be empty and outside frozen corpora')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
for(file in c('x11-msr-cases.R','x11-msr-selection-cases.R'))source(file.path(root,'inst/tinytest/fixtures/native',file))
select<-getFromNamespace('native_x11_msr_select','persephone3')
policy<-getFromNamespace('native_x11_msr_policy','persephone3')
checks<-timed_calls<-0L;rows<-timings<-list()
assert<-function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
error<-function(a,b) {
  a<-as.numeric(a);b<-as.numeric(b)
  if(!identical(is.na(a),is.na(b)) || !identical(is.infinite(a),is.infinite(b)))return(Inf)
  at<-is.finite(a)&is.finite(b)
  if(any(is.infinite(a)&sign(a)!=sign(b),na.rm=TRUE))return(Inf)
  if(!any(at))return(0)
  max(abs(a[at]-b[at])/pmax(1,abs(b[at])))
}
manifest<-read.csv(file.path(fixture_dir,'SHA256SUMS.csv'));before<-extended_parent_hashes(base)
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'frozen artifacts')
meta<-dget(file.path(fixture_dir,'metadata.R'))
assert(identical(meta$oracle_manifest_sha256,reference_sha256(file.path(base,'oracle-manifest.R'))),'pinned provenance')
for(name in names(meta$source_sha256)) {
  path<-if(grepl('-cases[.]R$',name))file.path(root,'inst/tinytest/fixtures/native',name)else file.path(tooling,name)
  assert(identical(unname(meta$source_sha256[name]),reference_sha256(path)),paste(name,'capture source'))
}
read_split<-function(name) {x<-read.csv(file.path(fixture_dir,paste0(name,'.csv')));split(x,x$case)}
summaries<-read.csv(file.path(fixture_dir,'summaries.csv'));tables<-read_split('tables')
pilots<-read_split('pilots');attempts<-read_split('attempts');cases<-x11_msr_selection_cases()
assert(identical(sort(summaries$case),sort(names(cases))),'complete case matrix')
pin<-if('--live' %in% args)extended_assert_oracle(base)else NULL
for(ca in cases) {
  y<-x11_msr_selection_input(ca);p<-summaries[summaries$case==ca$id,];live_error<-0
  if('--live' %in% args) {
    live<-reference_x11_msr_select(y,ca$mode,ca$backcasts,ca$forecasts,ca$exclude_forecast)
    assert(identical(tolower(live$choice),p$choice) && identical(live$iterations,p$iterations) && identical(live$using_default,p$using_default),paste(ca$id,'live policy'))
    live_error<-error(live$global,p$global)
    if(!is.null(live$table))for(field in c('seasonal','irregular'))live_error<-max(live_error,error(live$table[[field]],tables[[ca$id]][[field]]))
    assert(live_error<2e-12,paste(ca$id,'live table'))
  }
  for(method in c('jdplus','x11_five_year_cap')) {
    a<-select(y,ca$mode,method,ca$backcasts,ca$forecasts)
    o<-x11_msr_selection_oracle(y,ca$mode,method,ca$backcasts,ca$forecasts)
    for(field in c('status','reason','selected_filter','using_default'))assert(identical(a[[field]],o[[field]]),paste(ca$id,method,field))
    assert(a$iterations==length(o$attempts),paste(ca$id,method,'iterations'))
    independent_error<-error(a$attempts$global_msr,vapply(o$attempts,function(z)z$global_msr,numeric(1)))
    public_error<-0
    if(!is.null(o$pilot)) {
      for(field in c('seasonal','irregular'))independent_error<-max(independent_error,
        error(a$pilot[[field]],o$pilot[[field]]),error(a$table$phases[[paste0(field,'_mean')]],o$table$phases[[field]]))
      independent_error<-max(independent_error,error(a$global_msr,o$table$global))
    }
    assert(independent_error<2e-12,paste(ca$id,method,'independent formulas'))
    if(a$status=='available') {
      if(a$iterations)public_error<-error(a$attempts$global_msr,head(attempts[[ca$id]]$global,a$iterations))
      if(method=='jdplus') {
        assert(identical(a$selected_filter,p$choice) && identical(a$using_default,p$using_default) && a$iterations==p$iterations,paste(ca$id,'public policy'))
        if(a$iterations)for(field in c('seasonal','irregular'))public_error<-max(public_error,
          error(a$pilot[[field]],pilots[[ca$id]][[field]]),error(a$table$phases[[paste0(field,'_mean')]],tables[[ca$id]][[field]]))
      }
      assert(public_error<2e-12,paste(ca$id,method,'public numerical targets'))
    }
    rows[[length(rows)+1L]]<-data.frame(case=ca$id,frequency=ca$frequency,n=ca$n,start_period=ca$start[2],mode=ca$mode,
      shape=ca$shape,policy=method,status=a$status,reason=a$reason,choice=a$selected_filter,using_default=a$using_default,
      iterations=a$iterations,global_native=a$global_msr,public_choice=p$choice,public_iterations=p$iterations,
      public_scaled_error=public_error,independent_scaled_error=independent_error,live_scaled_error=live_error)
  }
}
thresholds<-read.csv(file.path(fixture_dir,'thresholds.csv'))
for(j in seq_len(nrow(thresholds))) {
  t<-thresholds[j,];a<-policy(t$ratio)
  assert(identical(a$band,c('lower_gap','s3x5','upper_gap','s3x9')[j]),paste(t$ratio,'scalar boundary'))
  if('--live' %in% args) {
    year<-seq_len(5L);y<-ts(rep(year+as.numeric(t$amplitude_hex)*(-1)^year+t$level,each=4),frequency=4)
    live<-reference_x11_msr_select(y,'additive')
    assert(identical(live$global,t$ratio) && identical(tolower(live$choice),t$choice) && identical(live$using_default,t$using_default) &&
      live$iterations==t$iterations,paste(t$ratio,'exact public boundary'))
  }
}
if('--timing' %in% args) {
  workloads<-Filter(function(ca)(ca$shape %in% c('smooth','rough') && ca$start[2]==1L && ca$mode %in% c('additive','multiplicative') &&
    ca$n %in% c(5L*ca$frequency,10L*ca$frequency+1L,20L*ca$frequency) && startsWith(ca$id,'regular')) ||
      startsWith(ca$id,'policy-difference'),cases)
  for(f in c(4L,12L))for(mode in c('additive','multiplicative'))for(shape in c('smooth','rough')) {
    id<-paste('long',f,mode,shape,sep='-');workloads[[id]]<-list(id=id,frequency=f,n=100003L,start=c(2000L,f),mode=mode,
      shape=shape,backcasts=0L,forecasts=0L,exclude_forecast=TRUE)
  }
  assert(length(workloads)==34L,'predeclared timing matrix')
  for(ca in workloads) {
    y<-x11_msr_selection_input(ca);target<-reference_x11_msr_select(y,ca$mode)
    native_target<-select(y,ca$mode)
    assert(native_target$status=='available' && native_target$selected_filter==tolower(target$choice) && native_target$iterations==target$iterations &&
      error(native_target$global_msr,target$global)<2e-12,paste(ca$id,'timing semantics'))
    functions<-list(native=function()select(y,ca$mode),jd_x11=function()reference_x11_msr_select(y,ca$mode))
    expected<-list(native=native_target,jd_x11=target)
    if(startsWith(ca$id,'policy-difference')) {
      functions$native_capped<-function()select(y,ca$mode,'x11_five_year_cap')
      expected$native_capped<-select(y,ca$mode,'x11_five_year_cap')
    }
    sizes<-vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
      max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
    for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
      iterations<-sizes[[engine]];invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      while(sample$seconds<.05 && iterations<1000L) {
        iterations<-min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
        invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      }
      assert(sample$seconds>0 && identical(sample$value,expected[[engine]]),paste(ca$id,engine,'timed output'));sizes[[engine]]<-iterations
      timings[[length(timings)+1L]]<-data.frame(case=ca$id,group=if(engine=='native_capped')'capped-policy'else if(ca$n>100000)'long'else'short',
        frequency=ca$frequency,n=ca$n,mode=ca$mode,policy=if(engine=='native_capped')'x11_five_year_cap'else'jdplus',
        engine=engine,round=round,iterations=iterations,elapsed_seconds=sample$seconds,seconds_per_call=sample$seconds/iterations)
    }
    cat(ca$id,'MSR selection timing PASS\n')
  }
}
assert(identical(before,extended_parent_hashes(base)),'original fixtures unchanged')
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'selection fixtures unchanged')
if(!'--live' %in% args)assert(!any(c('rJava','rjd3toolkit','rjd3x13','rjd3tramoseats') %in% loadedNamespaces()),'offline without Java')
files<-c(file.path(tooling,c('verify-x11-msr-selection.R','x11-filter-reference.R','x11-mean-reference.R','x11-msr-reference.R',
  'x11-msr-selection-reference.R','fit-timing.R')),file.path(root,c('src/x11_msr_selection.cpp','src/core/x11_msr_selection.hpp',
  'src/x11_msr.cpp','src/core/x11_msr.hpp','src/numerics.cpp','src/core/numerics.hpp','src/core/parallel.hpp',
  'src/rcpp_x11_msr_selection.cpp','src/rcpp_x11_msr.cpp','src/rcpp_x11_msr_support.hpp',
  'R/native_x11_msr_selection.R','R/native_x11_msr.R','R/native_x11_seasonal_plan.R','R/native_inputs.R',
  'src/RcppExports.cpp','R/RcppExports.R','inst/tinytest/fixtures/native/x11-msr-cases.R','inst/tinytest/fixtures/native/x11-msr-selection-cases.R')))
report<-do.call(rbind,rows)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  fixture_manifest_sha256=reference_sha256(file.path(fixture_dir,'SHA256SUMS.csv')),R=R.version.string,java=if(!is.null(pin))pin$java else NULL,
  checks=checks,cases=length(cases),policy_evaluations=nrow(report),exact_thresholds=nrow(thresholds),
  timed_calls_including_warmup_and_calibration=timed_calls,parallel=getFromNamespace('.native_parallel_info','persephone3')(),
  timing_scope='26 short and 8 long default-JD-policy selector calls; includes trimming, seven-term pilots, corrected phase tables and iteration. Native additionally returns final pilot vectors, domains, statuses and per-attempt trace. Public includes validated context creation and final table serialization. Two capped-policy native calls are separate: JD+ has no matching capped selector option. No decomposition or fit.',
  timing_conventions='Three warmed alternating rounds; >=50ms calibrated batches when possible, maximum 1000 calls/batch; loading/startup and explicit pre-batch GC excluded; natural in-batch GC included. Owned OpenMP for independent pilot rows/changes; deterministic caller reductions.')
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',length(cases),'cases /',nrow(report),'policy evaluations\n')
print(vapply(report[c('public_scaled_error','independent_scaled_error','live_scaled_error')],max,numeric(1)))
if(length(timings)) {
  totals<-aggregate(seconds_per_call~group+engine+round,do.call(rbind,timings),sum)
  print(aggregate(seconds_per_call~group+engine,totals,median),row.names=FALSE)
}
