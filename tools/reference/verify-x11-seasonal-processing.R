# Frozen fixed-choice/stable/short processing; optional public replay and timing.
args<-commandArgs(trailingOnly=FALSE)
script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'))
source(file.path(tooling,'extended-fixtures.R'));source(file.path(tooling,'x11-seasonal-processing-reference.R'))
source(file.path(tooling,'fit-timing.R'))
root<-reference_root();base<-reference_fixture_dir(root);args<-commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--live','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-x11-seasonal-processing.R [--source] [--live] [--timing] [--output=<existing empty directory>]')
if('--timing' %in% args && !'--live' %in% args)stop('Timings require --live')
output<-args[startsWith(args,'--output=')]
output<-if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
fixture_dir<-file.path(root,'inst/tinytest/fixtures/native/x11-seasonal-processing')
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
    startsWith(output,normalizePath(base)) || startsWith(output,normalizePath(fixture_dir))))stop('Output must be empty and outside frozen corpora')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
for(file in c('x11-symmetric-cases.R','x11-seasonal-cases.R','x11-seasonal15-cases.R','x11-seasonal-processing-cases.R'))
  source(file.path(root,'inst/tinytest/fixtures/native',file))
process<-getFromNamespace('native_x11_seasonal_process','persephone3')
checks<-timed_calls<-0L;rows<-timings<-list()
assert<-function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
manifest<-read.csv(file.path(fixture_dir,'SHA256SUMS.csv'),stringsAsFactors=FALSE)
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'frozen processing artifacts')
before<-extended_parent_hashes(base);frozen<-dget(file.path(fixture_dir,'metadata.R'))
assert(identical(frozen$oracle_manifest_sha256,reference_sha256(file.path(base,'oracle-manifest.R'))),'pinned fixture provenance')
w<-read.csv(file.path(fixture_dir,'weights.csv'),stringsAsFactors=FALSE)
p<-read.csv(file.path(fixture_dir,'points.csv'),stringsAsFactors=FALSE)
weights<-split(w,w$case);points<-split(p,p$case)
assert(identical(sort(names(points)),sort(names(x11_processing_cases()))),'complete point matrix')
assert(identical(sort(names(weights)),sort(names(x11_processing_cases()))),'complete operator matrix')
pin<-if('--live' %in% args)extended_assert_oracle(base)else NULL
scaled<-function(a,b)if(length(a)!=length(b) || !identical(is.na(a),is.na(b)))Inf else
  if(any(!is.na(b)))max(abs(a-b)/(1+abs(b)),na.rm=TRUE)else 0
for(ca in x11_processing_cases()) {
  y<-x11_processing_input(ca);native<-process(y,ca$filters)
  oracle<-x11_processing_oracle(as.numeric(y),ca$frequency,ca$filters,ca$start[2])
  w<-weights[[ca$id]];p<-points[[ca$id]];phase<-seq.int(1L,ca$n,ca$frequency)
  A<-oracle$operators[[ca$start[2]]];target<-matrix(0,length(phase),length(phase))
  captured_rows<-(w$row-1L)%/%ca$frequency+1L;captured_columns<-(w$column-1L)%/%ca$frequency+1L
  target[cbind(captured_rows,captured_columns)]<-w$weight
  native_A<-matrix(vapply(phase,function(j)as.numeric(process(ts(as.numeric(seq_len(ca$n)==j),
    start=ca$start,frequency=ca$frequency),ca$filters)$values[phase]),numeric(length(phase))),nrow=length(phase))
  selected<-unique(captured_rows)
  operator_error<-max(abs(native_A[selected,,drop=FALSE]-target[selected,,drop=FALSE]))
  independent_operator_error<-max(abs(native_A-A))
  point_error<-scaled(as.numeric(native$values[p$row]),p$value)
  independent_error<-scaled(as.numeric(native$values),oracle$values)
  assert(operator_error<=1e-14,paste(ca$id,'frozen public operators'))
  assert(independent_operator_error<=1e-14,paste(ca$id,'independent full operators'))
  assert(point_error<=2e-12,paste(ca$id,'frozen public points'))
  assert(independent_error<=2e-12,paste(ca$id,'independent points'))
  assert(max(abs(rowSums(native_A)-1))<=1e-14,paste(ca$id,'constant reproduction'))
  assert(identical(native$available_rows,seq_len(ca$n)) && identical(tsp(native$values),tsp(y)),paste(ca$id,'full calendar'))
  assert(identical(unname(native$requested_filters),ca$filters),paste(ca$id,'requested choices'))
  assert(identical(native$row_methods$period,as.integer(cycle(y))),paste(ca$id,'period metadata'))
  live_error<-NA_real_
  if('--live' %in% args) {
    public<-reference_x11_seasonal_processing(ca$frequency,ca$filters,y,ca$start[2])
    live_error<-scaled(as.numeric(native$values),public$values)
    assert(live_error<=2e-12,paste(ca$id,'public processor application'))
  }
  rows[[ca$id]]<-data.frame(case=ca$id,frequency=ca$frequency,n=ca$n,start_period=ca$start[2],input_group=ca$input_group,
    requested_filters=paste(native$requested_filters,collapse=';'),selected_filters=paste(native$selected_filters,collapse=';'),
    selection_reasons=paste(native$selection_reasons,collapse=';'),
    operator_max_error=operator_error,independent_operator_max_error=independent_operator_error,
    frozen_point_scaled_error=point_error,independent_point_scaled_error=independent_error,live_point_scaled_error=live_error)
}
timing_workloads<-c(short=0L,long=0L)
if('--timing' %in% args) {
  # Predeclared subset removes duplicate calendar-start timings while covering
  # stable, short-middle, partial-year, long-filter and mixed-choice work.
  workloads<-Filter(function(ca)ca$start[2]==3L &&
    ((ca$input_group=='uniform' && ca$n %in% c(5L*ca$frequency-1L,5L*ca$frequency+1L,19L*ca$frequency+1L)) ||
     (ca$input_group=='mixed' && ca$n %in% c(5L*ca$frequency+1L,20L*ca$frequency+1L))),x11_processing_cases())
  timing_workloads['short']<-length(workloads)
  for(f in c(4L,12L))for(type in c('stable','s3x5','s3x15','mixed')) {
    id<-paste(f,type,100003L,sep='-')
    filters<-if(type=='mixed')rep(c('s3x1','s3x3','s3x5','s3x9','s3x15','stable'),length.out=f)else rep(type,f)
    workloads[[id]]<-list(id=id,frequency=f,n=100003L,start=c(-2L,3L),filters=filters)
    timing_workloads['long']<-timing_workloads['long']+1L
  }
  for(ca in workloads) {
    y<-x11_processing_input(ca)
    functions<-list(native=function()process(y,ca$filters),
      jd_x11=function()reference_x11_seasonal_processing(ca$frequency,ca$filters,y,ca$start[2]))
    target<-functions$jd_x11();assert(scaled(as.numeric(functions$native()$values),target$values)<=2e-12,paste(ca$id,'timing workload'))
    sizes<-vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
      max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
    for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
      iterations<-sizes[[engine]];invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      while(sample$seconds<.05 && iterations<1000L) {
        iterations<-min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
        invisible(gc());sample<-fit_timing_once(functions[[engine]],iterations);timed_calls<-timed_calls+iterations
      }
      assert(sample$seconds>0 && scaled(as.numeric(sample$value$values),target$values)<=2e-12,paste(ca$id,'timed output'))
      sizes[[engine]]<-iterations
      timings[[length(timings)+1L]]<-data.frame(case=ca$id,n=ca$n,input_group=if(ca$n==100003L)'long'else'short',
        frequency=ca$frequency,filters=paste(ca$filters,collapse=';'),engine=engine,round=round,iterations=iterations,
        elapsed_seconds=sample$seconds,seconds_per_filter=sample$seconds/iterations)
    }
    cat(ca$id,'seasonal-processing timing PASS\n')
  }
}
assert(identical(before,extended_parent_hashes(base)),'original corpus unchanged')
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'processing corpus unchanged')
if(!'--live' %in% args)assert(!any(c('rJava','rjd3toolkit','rjd3x13','rjd3tramoseats') %in% loadedNamespaces()),'offline without Java namespaces')
files<-c(file.path(tooling,c('verify-x11-seasonal-processing.R','x11-seasonal-processing-reference.R','fit-timing.R')),
  file.path(root,c('src/x11_seasonal_processing.cpp','src/core/x11_seasonal_processing.hpp','src/rcpp_x11_seasonal_processing.cpp',
    'R/native_x11_seasonal_processing.R','src/x11_seasonal_endpoints.cpp','src/core/x11_seasonal_endpoints.hpp',
    'src/x11_filters.cpp','src/core/x11_filters.hpp','src/numerics.cpp','src/core/parallel.hpp','R/RcppExports.R','src/RcppExports.cpp',
    'inst/tinytest/fixtures/native/x11-symmetric-cases.R','inst/tinytest/fixtures/native/x11-seasonal-cases.R',
    'inst/tinytest/fixtures/native/x11-seasonal15-cases.R','inst/tinytest/fixtures/native/x11-seasonal-processing-cases.R')))
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  fixture_manifest_sha256=reference_sha256(file.path(fixture_dir,'SHA256SUMS.csv')),R=R.version.string,java=if(!is.null(pin))pin$java else NULL,
  checks=checks,cases=length(rows),operator_absolute_budget=1e-14,point_scaled_budget=2e-12,timing_workloads=timing_workloads,
  timing_calls_including_warmup_and_calibration_retries=timed_calls,
  timing_scope='Fixed uniform/per-period seasonal processing with stable and explicit X-11 short-series policy: native includes full R calendar, selection and row-method metadata; public includes enum/factory/processor and vector serialization. Not data-driven MSR, full X-11 or fitted models',
  timing_conventions='48 predeclared short cases plus 8 long 100003-row cases; 3 warmed alternating rounds; >=50ms calibrated batches when possible; startup/loading and explicit pre-batch GC excluded; natural in-batch GC included',
  parallel=getFromNamespace('.native_parallel_info','persephone3')())
report<-do.call(rbind,rows)
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',length(rows),'seasonal-processing cases\n')
print(vapply(report[c('operator_max_error','independent_operator_max_error','frozen_point_scaled_error','independent_point_scaled_error','live_point_scaled_error')],function(x)if(all(is.na(x)))NA_real_ else max(x,na.rm=TRUE),numeric(1)))
if(length(timings)) {
  totals<-aggregate(seconds_per_filter~input_group+engine+round,do.call(rbind,timings),sum)
  cat('Median summed filter-only time by input group (seconds):\n')
  print(aggregate(seconds_per_filter~input_group+engine,totals,median),row.names=FALSE)
}
