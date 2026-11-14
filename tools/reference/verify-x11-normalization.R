args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script))
for(file in c('common.R','extended-fixtures.R','x11-normalization-reference.R','fit-timing.R'))source(file.path(tooling,file))
root<-reference_root();base<-reference_fixture_dir(root);args<-commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--live','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-x11-normalization.R [--source] [--live] [--timing] [--output=<empty directory>]')
if('--timing' %in% args && !'--live' %in% args)stop('Timings require --live')
output<-args[startsWith(args,'--output=')];output<-if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
fixture_dir<-file.path(root,'inst/tinytest/fixtures/native/x11-normalization')
if(!is.null(output) && (length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)) ||
    startsWith(output,normalizePath(fixture_dir))))stop('Output must be empty and outside frozen corpora')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
source(file.path(root,'inst/tinytest/fixtures/native/x11-normalization-cases.R'))
normalize<-getFromNamespace('native_x11_seasonal_normalize','persephone3')
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
points<-read.csv(file.path(fixture_dir,'points.csv'));targets<-split(points,points$case);cases<-x11_normalization_cases()
assert(identical(sort(names(targets)),sort(names(cases))),'complete case matrix')
pin<-if('--live' %in% args)extended_assert_oracle(base)else NULL
for(ca in cases) {
  y<-x11_normalization_input(ca);a<-normalize(y,ca$filters,ca$extend);o<-x11_normalization_oracle(y,ca$filters,ca$extend)
  public_error<-error(a$values,targets[[ca$id]]$value)
  independent_error<-max(error(a$values,o$values),error(a$normalization,o$normalization));live_error<-0
  assert(public_error<2e-12,paste(ca$id,'public values'))
  assert(independent_error<2e-12,paste(ca$id,'independent operators'))
  assert(identical(a$source_rows,o$source_rows) && identical(a$normalization_source_rows,o$normalization_source_rows),paste(ca$id,'source rows'))
  assert(identical(unname(a$endpoint_reasons),o$reasons),paste(ca$id,'endpoint reasons'))
  if('--live' %in% args) {
    live<-reference_x11_normalization(y,ca$filters,ca$extend)
    live_error<-error(live,targets[[ca$id]]$value);assert(live_error<2e-12,paste(ca$id,'live targets'))
  }
  rows[[length(rows)+1L]]<-data.frame(case=ca$id,frequency=ca$frequency,n=ca$n,start_period=ca$start[2],choice=ca$choice,
    extend=ca$extend,shape=ca$shape,status=a$status,available=length(a$available_rows),
    public_scaled_error=public_error,independent_scaled_error=independent_error,live_scaled_error=live_error)
}
if('--timing' %in% args) {
  workloads<-Filter(function(ca)ca$shape=='smooth' && ca$start[2]==1L && ca$n %in% c(5L*ca$frequency,19L*ca$frequency+1L) &&
    ca$choice %in% c('s3x3','stable','mixed_stable'),cases)
  for(f in c(4L,12L))for(choice in c('s3x3','mixed_stable'))for(extend in c(0L,f%/%2L)) {
    id<-paste('long',f,choice,extend,sep='-')
    workloads[[id]]<-list(id=id,frequency=f,n=100003L,start=c(2000L,f),extend=extend,shape='smooth',choice=choice,
      filters=if(choice=='s3x3')'s3x3'else c('stable',rep('s3x3',f-1L)))
  }
  assert(length(workloads)==32L,'predeclared timing matrix')
  for(ca in workloads) {
    y<-x11_normalization_input(ca);target<-reference_x11_normalization(y,ca$filters,ca$extend)
    native_target<-normalize(y,ca$filters,ca$extend)
    assert(native_target$status=='available' && error(native_target$values,target)<2e-12,paste(ca$id,'timing semantics'))
    functions<-list(native=function()normalize(y,ca$filters,ca$extend),jd_x11=function()reference_x11_normalization(y,ca$filters,ca$extend))
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
        choice=ca$choice,extend=ca$extend,engine=engine,round=round,iterations=iterations,
        elapsed_seconds=sample$seconds,seconds_per_call=sample$seconds/iterations)
    }
    cat(ca$id,'normalization timing PASS\n')
  }
}
assert(identical(before,extended_parent_hashes(base)),'original fixtures unchanged')
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'normalization fixtures unchanged')
if(!'--live' %in% args)assert(!any(c('rJava','rjd3toolkit','rjd3x13','rjd3tramoseats') %in% loadedNamespaces()),'offline without Java')
files<-c(file.path(tooling,c('verify-x11-normalization.R','x11-normalization-reference.R','fit-timing.R')),file.path(root,c(
  'src/x11_normalization.cpp','src/core/x11_normalization.hpp','src/x11_filters.cpp','src/core/x11_filters.hpp',
  'src/rcpp_x11_table_support.cpp','src/rcpp_x11_table_support.hpp','src/numerics.cpp','src/core/numerics.hpp','src/core/parallel.hpp','src/rcpp_x11_normalization.cpp','R/native_x11_normalization.R',
  'R/native_inputs.R','src/RcppExports.cpp','R/RcppExports.R','inst/tinytest/fixtures/native/x11-normalization-cases.R')))
report<-do.call(rbind,rows)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  fixture_manifest_sha256=reference_sha256(file.path(fixture_dir,'SHA256SUMS.csv')),R=R.version.string,java=if(!is.null(pin))pin$java else NULL,
  checks=checks,cases=length(cases),timed_calls_including_warmup_and_calibration=timed_calls,
  parallel=getFromNamespace('.native_parallel_info','persephone3')(),
  timing_scope='24 short and 8 long additive normalization calls, with fixed/stable/mixed requests and optional half-year periodic restoration. Native additionally returns normalizing means, both source-row maps, domains, masks and phase policy metadata. Public includes validated context creation and output serialization. No seasonal estimation, B/C/D pass or fit.',
  timing_conventions='Three warmed alternating rounds; >=50ms calibrated batches when possible, maximum 1000 calls/batch; loading/startup and explicit pre-batch GC excluded; natural in-batch GC included. Owned OpenMP for central means, independent normalization rows and restoration; fixed per-row arithmetic order.')
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',length(cases),'normalization cases\n')
print(vapply(report[c('public_scaled_error','independent_scaled_error','live_scaled_error')],max,numeric(1)))
if(length(timings)) {
  totals<-aggregate(seconds_per_call~group+engine+round,do.call(rbind,timings),sum)
  print(aggregate(seconds_per_call~group+engine,totals,median),row.names=FALSE)
}
