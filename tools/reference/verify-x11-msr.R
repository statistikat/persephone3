args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'x11-msr-reference.R'));source(file.path(tooling,'fit-timing.R'))
root<-reference_root();base<-reference_fixture_dir(root);args<-commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--live','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-x11-msr.R [--source] [--live] [--timing] [--output=<empty directory>]')
if('--timing' %in% args && !'--live' %in% args)stop('Timings require --live')
output<-args[startsWith(args,'--output=')];output<-if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
fixture_dir<-file.path(root,'inst/tinytest/fixtures/native/x11-msr')
if(!is.null(output) && (length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)) ||
    startsWith(output,normalizePath(fixture_dir))))stop('Output must be empty and outside frozen corpora')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
source(file.path(root,'inst/tinytest/fixtures/native/x11-msr-cases.R'))
table<-getFromNamespace('native_x11_msr_table','persephone3');checks<-timed_calls<-0L;rows<-timings<-list()
assert<-function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
error<-function(a,b) {
  if(!identical(is.na(a),is.na(b)) || !identical(is.infinite(a),is.infinite(b)))return(Inf)
  at<-is.finite(a)&is.finite(b)
  if(any(is.infinite(a) & sign(a)!=sign(b),na.rm=TRUE))return(Inf)
  if(!any(at))return(0)
  max(abs(a[at]-b[at])/pmax(1,abs(b[at])))
}
manifest<-read.csv(file.path(fixture_dir,'SHA256SUMS.csv'));before<-extended_parent_hashes(base)
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'frozen artifacts')
meta<-dget(file.path(fixture_dir,'metadata.R'))
assert(identical(meta$oracle_manifest_sha256,reference_sha256(file.path(base,'oracle-manifest.R'))),'pinned provenance')
frozen<-read.csv(file.path(fixture_dir,'tables.csv'),stringsAsFactors=FALSE);targets<-split(frozen,frozen$case)
assert(identical(sort(names(targets)),sort(names(x11_msr_cases()))),'complete case matrix')
pin<-if('--live' %in% args)extended_assert_oracle(base)else NULL
for(ca in x11_msr_cases()) {
  y<-x11_msr_input(ca);actual<-table(y$seasonal,y$irregular,ca$mode);p<-actual$phases
  target<-targets[[ca$id]];oracle<-x11_msr_oracle(y,ca$mode);public_error<-independent_error<-live_error<-0
  for(field in c('seasonal','irregular')) {
    at<-p[[paste0(field,'_status')]]=='available'
    e<-error(p[[paste0(field,'_mean')]][at],target[[field]][at]);assert(e<2e-12,paste(ca$id,field,'public means'));public_error<-max(e,public_error)
    e<-error(p[[paste0(field,'_mean')]],oracle$phases[[field]]);assert(e<2e-12,paste(ca$id,field,'independent means'));independent_error<-max(e,independent_error)
  }
  e<-error(p$msr,oracle$phases$msr);assert(e<2e-12,paste(ca$id,'independent phases'));independent_error<-max(e,independent_error)
  e<-error(actual$global_msr,oracle$global);assert(e<2e-12,paste(ca$id,'independent global'));independent_error<-max(e,independent_error)
  if(actual$status %in% c('available','undefined_msr')) {
    e<-error(actual$global_msr,target$global[1]);assert(e<2e-12,paste(ca$id,'public global'));public_error<-max(e,public_error)
  }
  if('--live' %in% args) {
    live<-reference_x11_msr_table(y$seasonal,y$irregular,ca$mode)
    e<-max(error(live$seasonal,target$seasonal),error(live$irregular,target$irregular),error(live$msr,target$msr),error(live$global,target$global[1]))
    assert(e<2e-12,paste(ca$id,'live target'));live_error<-e
  }
  rows[[length(rows)+1L]]<-data.frame(case=ca$id,frequency=ca$frequency,n=ca$n,start_period=ca$start[2],mode=ca$mode,shape=ca$shape,
    status=actual$status,global_native=actual$global_msr,global_jd=target$global[1],unavailable_phases=sum(p$status!='available'),
    public_scaled_error=public_error,independent_scaled_error=independent_error,live_scaled_error=live_error)
}
mode_targets<-read.csv(file.path(fixture_dir,'modes.csv'));mode_error<-0
for(ca in x11_msr_mode_cases()) {
  target<-mode_targets[mode_targets$case==ca$id,];y<-x11_msr_mode_components(ca,target$iterations[1])
  actual<-table(y$seasonal,y$irregular,ca$mode)
  e<-max(error(actual$phases$seasonal_mean,target$seasonal),error(actual$phases$irregular_mean,target$irregular),error(actual$global_msr,target$global[1]))
  assert(e<2e-12,paste(ca$id,'public mode semantics'));mode_error<-max(mode_error,e)
  if('--live' %in% args) {
    live<-reference_x11_msr_mode(ca$y,ca$mode)
    assert(identical(live$iterations,target$iterations[1]) && identical(live$choice,target$choice[1]) &&
      max(error(live$seasonal,target$seasonal),error(live$irregular,target$irregular),error(live$global,target$global[1]))<2e-12,paste(ca$id,'live mode table'))
  }
}
if('--timing' %in% args) {
  workloads<-Filter(function(ca)ca$shape %in% c('smooth','rough') && ca$start[2]==1L && ca$mode %in% c('additive','multiplicative') &&
    ca$n %in% c(4L*ca$frequency,7L*ca$frequency+1L,20L*ca$frequency+1L),x11_msr_cases())
  for(f in c(4L,12L))for(mode in c('additive','multiplicative'))for(shape in c('smooth','rough')) {
    id<-paste('long',f,mode,shape,sep='-');workloads[[id]]<-list(id=id,frequency=f,n=100003L,start=c(2000L,f),mode=mode,shape=shape)
  }
  assert(length(workloads)==32L,'predeclared timing matrix')
  for(ca in workloads) {
    y<-x11_msr_input(ca);target<-reference_x11_msr_table(y$seasonal,y$irregular,ca$mode)
    functions<-list(native=function()table(y$seasonal,y$irregular,ca$mode),jd_x11=function()reference_x11_msr_table(y$seasonal,y$irregular,ca$mode))
    valid<-function(value,engine)if(engine=='native')value$status=='available' && max(error(value$phases$seasonal_mean,target$seasonal),
      error(value$phases$irregular_mean,target$irregular),error(value$phases$msr,target$msr),error(value$global_msr,target$global))<2e-12 else identical(value,target)
    sizes<-vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
      max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
    for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
      iterations<-sizes[[engine]];invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      while(sample$seconds<.05 && iterations<1000L) {
        iterations<-min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
        invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      }
      assert(sample$seconds>0 && valid(sample$value,engine),paste(ca$id,engine,'timed output'));sizes[[engine]]<-iterations
      timings[[length(timings)+1L]]<-data.frame(case=ca$id,group=if(ca$n>100000)'long'else'short',frequency=ca$frequency,n=ca$n,
        mode=ca$mode,engine=engine,round=round,iterations=iterations,elapsed_seconds=sample$seconds,seconds_per_call=sample$seconds/iterations)
    }
    cat(ca$id,'MSR table timing PASS\n')
  }
}
assert(identical(before,extended_parent_hashes(base)),'original fixtures unchanged')
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'MSR fixtures unchanged')
if(!'--live' %in% args)assert(!any(c('rJava','rjd3toolkit','rjd3x13','rjd3tramoseats') %in% loadedNamespaces()),'offline without Java')
files<-c(file.path(tooling,c('verify-x11-msr.R','x11-msr-reference.R','fit-timing.R')),file.path(root,c('src/x11_msr.cpp','src/core/x11_msr.hpp',
  'src/numerics.cpp','src/core/numerics.hpp','src/core/parallel.hpp','src/rcpp_x11_msr.cpp','R/native_x11_msr.R','R/native_inputs.R',
  'src/RcppExports.cpp','R/RcppExports.R','inst/tinytest/fixtures/native/x11-msr-cases.R')))
report<-do.call(rbind,rows)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  fixture_manifest_sha256=reference_sha256(file.path(fixture_dir,'SHA256SUMS.csv')),R=R.version.string,java=if(!is.null(pin))pin$java else NULL,
  checks=checks,cases=length(targets),mode_cases=length(x11_msr_mode_cases()),mode_scaled_error=mode_error,timed_calls_including_warmup_and_calibration=timed_calls,
  parallel=getFromNamespace('.native_parallel_info','persephone3')(),
  timing_scope='24 short and 8 long component-table calls; native includes input validation, per-phase statuses/counts/raw means/corrections and serialization. Public includes DoubleSeq creation and table serialization. No pilot, trimming, selection, decomposition or fit.',
  timing_conventions='Three warmed alternating rounds; >=50ms calibrated batches when possible; loading/startup and explicit pre-batch GC excluded; natural in-batch GC included. Owned OpenMP for independent changes; deterministic caller reductions.')
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',length(targets),'MSR cases\n')
cat('Mode qualification:',length(x11_msr_mode_cases()),'cases / maximum scaled error',mode_error,'\n')
print(vapply(report[c('public_scaled_error','independent_scaled_error','live_scaled_error')],max,numeric(1)))
if(length(timings)) {
  totals<-aggregate(seconds_per_call~group+engine+round,do.call(rbind,timings),sum)
  print(aggregate(seconds_per_call~group+engine,totals,median),row.names=FALSE)
}
