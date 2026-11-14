args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'x11-seasonal-plan-reference.R'));source(file.path(tooling,'fit-timing.R'))
root<-reference_root();base<-reference_fixture_dir(root);args<-commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--live','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-x11-seasonal-plan.R [--source] [--live] [--timing] [--output=<empty directory>]')
if('--timing' %in% args && !'--live' %in% args)stop('Timings require --live')
output<-args[startsWith(args,'--output=')];output<-if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
fixture_dir<-file.path(root,'inst/tinytest/fixtures/native/x11-seasonal-plan')
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
    startsWith(output,normalizePath(base)) || startsWith(output,normalizePath(fixture_dir))))stop('Output must be empty and outside frozen corpora')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
source(file.path(root,'inst/tinytest/fixtures/native/x11-seasonal-plan-cases.R'))
plan<-getFromNamespace('native_x11_seasonal_plan','persephone3');stages<-x11_seasonal_stages()
checks<-timed_calls<-0L;rows<-timings<-list()
assert<-function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
manifest<-read.csv(file.path(fixture_dir,'SHA256SUMS.csv'));before<-extended_parent_hashes(base)
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'frozen artifacts')
meta<-dget(file.path(fixture_dir,'metadata.R'))
assert(identical(meta$oracle_manifest_sha256,reference_sha256(file.path(base,'oracle-manifest.R'))),'pinned provenance')
frozen<-read.csv(file.path(fixture_dir,'plans.csv'),stringsAsFactors=FALSE);targets<-split(frozen,frozen$case)
assert(identical(sort(names(targets)),sort(names(x11_seasonal_plan_cases()))),'complete case matrix')
pin<-if('--live' %in% args)extended_assert_oracle(base)else NULL
for(ca in x11_seasonal_plan_cases()) {
  target<-targets[[ca$id]]
  for(stage in stages) {
    native<-plan(ca$frequency,ca$filters,stage)
    expected<-if(endsWith(stage,'initial'))target$initial_filter else target$final_filter
    pending<-stage=='d_final' && target$global_msr[1]
    assert(identical(unname(native$base_filters),expected),paste(ca$id,stage,'public mapping'))
    assert(identical(native$global_msr,target$global_msr[1]),paste(ca$id,stage,'global eligibility'))
    assert(identical(native$requires_msr,pending),paste(ca$id,stage,'pending state'))
    assert(if(pending)all(is.na(native$resolved_filters))else identical(unname(native$resolved_filters),expected),paste(ca$id,stage,'resolved mapping'))
    rows[[length(rows)+1L]]<-data.frame(case=ca$id,frequency=ca$frequency,n=ca$n,start_period=ca$start[2L],mode=ca$mode,stage=stage,
      requested_filters=paste(native$requested_filters,collapse=';'),base_filters=paste(native$base_filters,collapse=';'),
      resolved_filters=paste(native$resolved_filters,collapse=';'),global_msr=native$global_msr,status=native$status)
  }
  if('--live' %in% args) {
    live<-reference_x11_seasonal_plan(x11_seasonal_plan_input(ca),ca$filters,ca$mode)
    assert(identical(live$initial_filters,target$initial_filter) && identical(live$final_filters,target$final_filter) &&
      identical(live$global_msr,target$global_msr[1]),paste(ca$id,'live context'))
  }
}
if('--timing' %in% args) {
  workloads<-Filter(function(ca)ca$n==20L*ca$frequency+1L && ca$start[2L]==1L,x11_seasonal_plan_cases())
  assert(length(workloads)==42L,'predeclared timing matrix')
  for(ca in workloads) {
    y<-x11_seasonal_plan_input(ca);target<-reference_x11_seasonal_plan(y,ca$filters,ca$mode)
    functions<-list(native=function()lapply(stages,function(stage)plan(ca$frequency,ca$filters,stage)),
      jd_x11=function()reference_x11_seasonal_plan(y,ca$filters,ca$mode))
    valid<-function(value,engine) {
      if(engine=='jd_x11')return(identical(value,target))
      all(vapply(seq_along(stages),function(j)identical(unname(value[[j]]$base_filters),
        if(endsWith(stages[j],'initial'))target$initial_filters else target$final_filters) &&
        identical(value[[j]]$global_msr,target$global_msr),logical(1)))
    }
    sizes<-vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
      max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
    for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
      iterations<-sizes[[engine]];invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      while(sample$seconds<.05 && iterations<1000L) {
        iterations<-min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
        invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      }
      assert(sample$seconds>0 && valid(sample$value,engine),paste(ca$id,engine,'timed output'))
      sizes[[engine]]<-iterations
      timings[[length(timings)+1L]]<-data.frame(case=ca$id,frequency=ca$frequency,request=ca$request,engine=engine,
        round=round,iterations=iterations,elapsed_seconds=sample$seconds,seconds_per_bundle=sample$seconds/iterations)
    }
    cat(ca$id,'seasonal-stage timing PASS\n')
  }
}
assert(identical(before,extended_parent_hashes(base)),'original fixtures unchanged')
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'plan fixtures unchanged')
if(!'--live' %in% args)assert(!any(c('rJava','rjd3toolkit','rjd3x13','rjd3tramoseats') %in% loadedNamespaces()),'offline without Java')
files<-c(file.path(tooling,c('verify-x11-seasonal-plan.R','x11-seasonal-plan-reference.R','fit-timing.R')),
  file.path(root,c('src/x11_seasonal_plan.cpp','src/core/x11_seasonal_plan.hpp','src/rcpp_x11_seasonal_plan.cpp',
    'R/native_x11_seasonal_plan.R','src/RcppExports.cpp','R/RcppExports.R','inst/tinytest/fixtures/native/x11-seasonal-plan-cases.R')))
report<-do.call(rbind,rows)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  fixture_manifest_sha256=reference_sha256(file.path(fixture_dir,'SHA256SUMS.csv')),R=R.version.string,java=if(!is.null(pin))pin$java else NULL,
  checks=checks,context_cases=length(targets),stage_cases=nrow(report),timed_calls_including_warmup_and_calibration=timed_calls,
  timing_scope='42 request bundles; six explicit native stage-plan calls vs one public validated context construction and serialization of shared initial/final mappings and global MSR flag. Native includes per-stage status, names and reasons. Policy-only, no numeric filtering or MSR estimation.',
  timing_conventions='Three warmed alternating rounds; >=50ms calibrated batches when possible; loading/startup and explicit pre-batch GC excluded; natural in-batch GC included. Bounded scalar policy uses no OpenMP work.')
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',length(targets),'contexts /',nrow(report),'stage cases\n')
if(length(timings)) {
  totals<-aggregate(seconds_per_bundle~engine+round,do.call(rbind,timings),sum)
  print(aggregate(seconds_per_bundle~engine,totals,median),row.names=FALSE)
}
