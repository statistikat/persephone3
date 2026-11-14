args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script))
for(file in c('common.R','extended-fixtures.R','x11-initial-reference.R','fit-timing.R'))source(file.path(tooling,file))
root<-reference_root();base<-reference_fixture_dir(root);args<-commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--live','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-x11-initial.R [--source] [--live] [--timing] [--output=<empty directory>]')
if('--timing' %in% args && !'--live' %in% args)stop('Timings require --live')
output<-args[startsWith(args,'--output=')];output<-if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
fixture_dir<-file.path(root,'inst/tinytest/fixtures/native/x11-initial')
if(!is.null(output) && (length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)) ||
    startsWith(output,normalizePath(fixture_dir))))stop('Output must be empty and outside frozen corpora')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
helpers<-c('x11-seasonal-cases.R','x11-seasonal15-cases.R','x11-seasonal-processing-cases.R','x11-normalization-cases.R','x11-initial-cases.R')
for(file in helpers)source(file.path(root,'inst/tinytest/fixtures/native',file))
initial<-getFromNamespace('native_x11_initial_additive','persephone3')
checks<-timed_calls<-0L;rows<-timings<-list()
assert<-function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
error<-function(a,b) {
  a<-as.numeric(a);b<-as.numeric(b)
  if(!identical(is.na(a),is.na(b)) || !identical(is.infinite(a),is.infinite(b)))return(Inf)
  at<-is.finite(a)&is.finite(b);if(!any(at))return(0)
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
points<-read.csv(file.path(fixture_dir,'points.csv'));targets<-split(points,points$case)
points<-read.csv(file.path(fixture_dir,'bstep_points.csv'));bstep<-split(points,points$case)
statuses<-read.csv(file.path(fixture_dir,'statuses.csv'));cases<-x11_initial_cases()
assert(identical(sort(names(bstep)),sort(statuses$case[statuses$bstep_available])),'B-step status audit')
assert(identical(sort(names(targets)),sort(names(cases))),'complete case matrix')
pin<-if('--live' %in% args)extended_assert_oracle(base)else NULL
for(ca in cases) {
  y<-x11_initial_input(ca);a<-initial(y,ca$filters);o<-x11_initial_oracle(y,ca$filters)
  live<-if('--live' %in% args)reference_x11_initial_bstep(y,ca$filters)else NULL
  for(name in names(a$tables)) {
    target<-targets[[ca$id]]$value[targets[[ca$id]]$table==name]
    btarget<-bstep[[ca$id]]$value[bstep[[ca$id]]$table==name]
    mismatch<-sum(is.na(a$tables[[name]])!=is.na(target))
    audit<-ca$shape=='gap' && ca$choice %in% c('stable','mixed_stable','s3x15') && name %in% c('B4a','B4anorm','B4d')
    assert(if(audit)mismatch>0L else mismatch==0L,paste(ca$id,name,'missingness audit'))
    keep<-if(audit)!is.na(a$tables[[name]])else rep(TRUE,length(target))
    public_error<-error(a$tables[[name]][keep],target[keep]);independent_error<-error(a$tables[[name]],o[[name]])
    bstep_error<-error(target,btarget);live_error<-if(is.null(live))0 else error(live[[name]],btarget)
    assert(public_error<2e-12,paste(ca$id,name,'public available values'))
    assert(independent_error<2e-12,paste(ca$id,name,'independent composition'))
    assert(bstep_error<2e-12,paste(ca$id,name,'actual B-step values'))
    if(!is.null(live))assert(live_error<2e-12,paste(ca$id,name,'live getters'))
    basis<-if(name=='B1')seq_along(y)else ca$frequency%/%2L+seq_len(ca$n-ca$frequency)
    assert(identical(a$row_basis[[name]],basis),paste(ca$id,name,'original row basis'))
    rows[[length(rows)+1L]]<-data.frame(case=ca$id,table=name,frequency=ca$frequency,n=ca$n,start_period=ca$start[2],choice=ca$choice,
      shape=ca$shape,status=a$status,available=length(a$available_rows[[name]]),public_missing_mismatches=mismatch,
      comparison=if(audit)'missing_policy_audit'else'full_table',public_scaled_error=public_error,
      independent_scaled_error=independent_error,bstep_scaled_error=bstep_error,live_scaled_error=live_error)
  }
}
if('--timing' %in% args) {
  workloads<-Filter(function(ca)ca$shape=='smooth' && ca$start[2]==1L && ca$n %in% c(3L*ca$frequency,6L*ca$frequency,20L*ca$frequency+1L) &&
    ca$choice %in% c('s3x3','stable','mixed_stable','mixed15'),cases)
  for(f in c(4L,12L))for(choice in c('s3x3','stable','mixed_stable','mixed15')) {
    id<-paste('long',f,choice,sep='-')
    filters<-switch(choice,s3x3='s3x3',stable='stable',mixed_stable=c('stable',rep('s3x3',f-1L)),mixed15=c('s3x15',rep('s3x9',f-1L)))
    workloads[[id]]<-list(id=id,frequency=f,n=100003L,start=c(2000L,f),shape='smooth',choice=choice,filters=filters)
  }
  assert(length(workloads)==32L,'predeclared timing matrix')
  for(ca in workloads) {
    y<-x11_initial_input(ca);target<-reference_x11_initial_primitives(y,ca$filters)
    native_target<-initial(y,ca$filters)
    assert(native_target$status=='available' && all(vapply(names(native_target$tables),function(name)
      error(native_target$tables[[name]],target[[name]])<2e-12,logical(1))),paste(ca$id,'timing semantics'))
    functions<-list(native=function()initial(y,ca$filters),jd_x11=function()reference_x11_initial_primitives(y,ca$filters))
    expected<-list(native=native_target,jd_x11=target)
    sizes<-vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
      max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
    for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
      iterations<-sizes[[engine]];invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      while(sample$seconds<.05 && iterations<1000L) {
        iterations<-min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
        invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      }
      assert(sample$seconds>0 && identical(sample$value,expected[[engine]]),paste(ca$id,engine,'timed output'));sizes[[engine]]<-iterations
      timings[[length(timings)+1L]]<-data.frame(case=ca$id,group=if(ca$n>100000)'long'else'short',frequency=ca$frequency,n=ca$n,
        choice=ca$choice,engine=engine,round=round,iterations=iterations,
        elapsed_seconds=sample$seconds,seconds_per_call=sample$seconds/iterations)
    }
    cat(ca$id,'initial-table timing PASS\n')
  }
}
assert(identical(before,extended_parent_hashes(base)),'original fixtures unchanged')
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'initial-table fixtures unchanged')
if(!'--live' %in% args)assert(!any(c('rJava','rjd3toolkit','rjd3x13','rjd3tramoseats') %in% loadedNamespaces()),'offline without Java')
files<-c(file.path(tooling,c('verify-x11-initial.R','x11-initial-reference.R','fit-timing.R')),
  file.path(root,'inst/tinytest/fixtures/native',helpers),file.path(root,c(
  'src/x11_initial.cpp','src/core/x11_initial.hpp','src/rcpp_x11_initial.cpp','R/native_x11_initial.R',
  'src/x11_normalization.cpp','src/core/x11_normalization.hpp','src/rcpp_x11_table_support.cpp','src/rcpp_x11_table_support.hpp',
  'src/x11_filters.cpp','src/core/x11_filters.hpp','src/x11_seasonal_endpoints.cpp','src/core/x11_seasonal_endpoints.hpp',
  'src/x11_seasonal_processing.cpp','src/core/x11_seasonal_processing.hpp',
  'src/numerics.cpp','src/core/numerics.hpp','src/core/parallel.hpp','R/native_x11_normalization.R','R/native_x11_seasonal_processing.R',
  'R/native_inputs.R','src/RcppExports.cpp','R/RcppExports.R')))
report<-do.call(rbind,rows)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  fixture_manifest_sha256=reference_sha256(file.path(fixture_dir,'SHA256SUMS.csv')),R=R.version.string,java=if(!is.null(pin))pin$java else NULL,
  checks=checks,cases=length(cases),timed_calls_including_warmup_and_calibration=timed_calls,
  parallel=getFromNamespace('.native_parallel_info','persephone3')(),
  missing_policy_audits=sum(report$comparison=='missing_policy_audit'),
  timing_scope='24 short and 8 long fixed additive initial B1/B2/B3/B4a/B4anorm/B4d calls. Matched public primitive chain verified against actual B-step getters; the timed public call does not run the rest of B. Native additionally returns nested seasonal/normalization metadata, domains, masks and row maps. Public includes validated context creation and serialization of all six tables. No extreme correction, full B/C/D pass or fitting.',
  timing_conventions='Three warmed alternating rounds; >=50ms calibrated batches when possible, maximum 1000 calls/batch; loading/startup and explicit pre-batch GC excluded; natural in-batch GC included. Owned OpenMP for independent outputs; fixed per-row arithmetic order.')
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',length(cases),'initial-table cases\n')
print(vapply(report[c('public_scaled_error','independent_scaled_error','bstep_scaled_error','live_scaled_error')],max,numeric(1)))
if(length(timings)) {
  totals<-aggregate(seconds_per_call~group+engine+round,do.call(rbind,timings),sum)
  print(aggregate(seconds_per_call~group+engine,totals,median),row.names=FALSE)
}
