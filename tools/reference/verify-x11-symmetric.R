# Offline frozen-fixture checks; optional public replay and filter-only timings.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script));source(file.path(tooling,'common.R'))
source(file.path(tooling,'extended-fixtures.R'));source(file.path(tooling,'x11-filter-reference.R'))
source(file.path(tooling,'fit-timing.R'));root <- reference_root();base <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--live','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-x11-symmetric.R [--source] [--live] [--timing] [--output=<existing empty directory>]')
if('--timing' %in% args && !'--live' %in% args)stop('Timings require --live')
output <- args[startsWith(args,'--output=')]
output <- if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
fixture_dir <- file.path(root,'inst/tinytest/fixtures/native/x11-symmetric')
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
    startsWith(output,normalizePath(base)) || startsWith(output,normalizePath(fixture_dir))))stop('Output must be empty and outside frozen corpora')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
source(file.path(root,'inst/tinytest/fixtures/native/x11-symmetric-cases.R'))
filter <- getFromNamespace('native_x11_filter','persephone3')
checks <- timed_calls <- 0L;rows <- timings <- list()
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
manifest <- read.csv(file.path(fixture_dir,'SHA256SUMS.csv'),stringsAsFactors=FALSE)
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'frozen X-11 artifact hashes')
before <- extended_parent_hashes(base)
frozen <- dget(file.path(fixture_dir,'metadata.R'))
assert(identical(frozen$oracle_manifest_sha256,reference_sha256(file.path(base,'oracle-manifest.R'))),'pinned fixture provenance')
weights <- read.csv(file.path(fixture_dir,'weights.csv'),stringsAsFactors=FALSE)
points <- read.csv(file.path(fixture_dir,'points.csv'),stringsAsFactors=FALSE)
assert(identical(sort(unique(weights$case)),sort(names(x11_symmetric_cases()))),'complete coefficient matrix')
assert(identical(sort(unique(points$case)),sort(names(x11_symmetric_cases()))),'complete point matrix')
pin <- if('--live' %in% args)extended_assert_oracle(base)else NULL
scaled <- function(a,b)if(length(a)!=length(b) || !identical(is.na(a),is.na(b)))Inf else
  if(any(!is.na(b)))max(abs(a-b)/(1+abs(b)),na.rm=TRUE)else 0
run_reference <- function(ca,y) {
  f <- reference_x11_filter(ca);spacing <- if(startsWith(ca$type,'s3x'))ca$frequency else 1L
  list(weights=rJava::.jcall(f,'[D','weightsToArray'),
    offsets=seq.int(rJava::.jcall(f,'I','getLowerBound'),rJava::.jcall(f,'I','getUpperBound'))*spacing,
    values=reference_x11_apply(f,y,spacing))
}
cases <- x11_symmetric_cases()
for(ca in cases) {
  y <- x11_symmetric_input(ca$frequency);native <- filter(y,ca$type,ca$length)
  w <- weights[weights$case==ca$id,];p <- points[points$case==ca$id,]
  coefficient_error <- max(abs(unname(native$weights)-w$weight))
  point_error <- scaled(as.numeric(native$values[p$row]),p$value)
  assert(identical(native$offsets,as.integer(w$offset)),paste(ca$id,'coefficient support'))
  assert(coefficient_error<=1e-14,paste(ca$id,'public coefficients'))
  assert(point_error<=2e-12,paste(ca$id,'frozen public points'))
  assert(identical(range(native$available_rows),as.integer(c(p$first_available[1],p$last_available[1]))),paste(ca$id,'public central domain'))
  oracle <- x11_filter_sum(y,unname(native$weights),native$offsets)
  independent_error <- scaled(as.numeric(native$values),oracle)
  assert(independent_error<=2e-12,paste(ca$id,'independent tap summation'))
  assert(abs(sum(native$weights)-1)<=1e-14,paste(ca$id,'constant reproduction'))
  if(ca$type=='henderson') {
    j <- native$offsets/max(1,native$radius)
    assert(abs(sum(native$weights*j^2))<=1e-14,paste(ca$id,'cubic reproduction'))
    if(ca$length<=23L)assert(max(abs(unname(native$weights)-henderson_minimum_roughness(ca$length)))<=1e-9,paste(ca$id,'independent constrained minimum'))
  }
  live_error <- NA_real_
  if('--live' %in% args) {
    reference <- run_reference(ca,y)
    assert(identical(reference$offsets,as.integer(w$offset)) && max(abs(reference$weights-w$weight))<=1e-14,paste(ca$id,'live frozen coefficients'))
    live_error <- scaled(as.numeric(native$values),reference$values)
    assert(live_error<=2e-12,paste(ca$id,'live full public application'))
  }
  rows[[ca$id]] <- data.frame(case=ca$id,frequency=ca$frequency,type=ca$type,length_parameter=native$length_parameter,
    terms=native$terms,radius=native$radius,coefficient_max_error=coefficient_error,
    frozen_point_scaled_error=point_error,independent_point_scaled_error=independent_error,live_point_scaled_error=live_error)
}
# Long-series timing cases exercise actual native workers as well as the small
# coefficient/application matrix. They use the same independent input formula.
if('--timing' %in% args) {
  workloads <- lapply(cases,function(ca){ca$n<-257L;ca})
  for(period in c(4L,12L))for(type in c('trend','s3x1','s3x3','s3x5','s3x9','s3x15','henderson')) {
    id <- paste(c(period,type,if(type=='henderson')13L),collapse='-')
    ca <- cases[[id]];ca$id <- paste0(id,'-long');ca$n <- 40000L;workloads[[ca$id]] <- ca
  }
  for(ca in workloads) {
    y <- x11_symmetric_input(ca$frequency,ca$n)
    functions <- list(native=function()filter(y,ca$type,ca$length),jd_x11=function()run_reference(ca,y))
    target <- functions$jd_x11();assert(scaled(as.numeric(functions$native()$values),target$values)<=2e-12,paste(ca$id,'timing-workload qualification'))
    sizes <- vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
      max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
    for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
      iterations <- sizes[[engine]];invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
      while(sample$seconds<.05 && iterations<1000L) {
        iterations <- min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
        invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
      }
      assert(sample$seconds>0 && scaled(as.numeric(sample$value$values),target$values)<=2e-12,paste(ca$id,'timed output'))
      sizes[[engine]] <- iterations
      timings[[length(timings)+1L]] <- data.frame(case=ca$id,n=ca$n,engine=engine,round=round,iterations=iterations,
        elapsed_seconds=sample$seconds,seconds_per_filter=sample$seconds/iterations)
    }
    cat(ca$id,'filter timing PASS\n')
  }
}
assert(identical(before,extended_parent_hashes(base)),'original corpus unchanged')
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'X-11 corpus unchanged')
if(!'--live' %in% args)assert(!any(c('rJava','rjd3toolkit','rjd3x13','rjd3tramoseats') %in% loadedNamespaces()),'offline execution without Java namespaces')
files <- c(file.path(tooling,c('verify-x11-symmetric.R','x11-filter-reference.R','fit-timing.R')),
  file.path(root,c('src/x11_filters.cpp','src/core/x11_filters.hpp','src/numerics.cpp','src/rcpp_x11_filters.cpp',
    'R/native_x11_filters.R','inst/tinytest/fixtures/native/x11-symmetric-cases.R')))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  fixture_manifest_sha256=reference_sha256(file.path(fixture_dir,'SHA256SUMS.csv')),R=R.version.string,java=if(!is.null(pin))pin$java else NULL,
  checks=checks,cases=length(cases),coefficient_absolute_budget=1e-14,point_scaled_budget=2e-12,
  independent_roughness_absolute_budget=1e-9,timing_calls_including_warmup_and_calibration_retries=timed_calls,
  timing_scope='Filter-only construction/application: native includes R calendar metadata; public includes factory, coefficient access, DataBlock filtering and R-vector serialization. Not complete X-11 or fitted-model timings',
  timing_conventions='3 warmed alternating rounds; >=50ms calibrated batches when possible; setup/loading/startup and explicit pre-batch GC excluded',
  parallel=getFromNamespace('.native_parallel_info','persephone3')())
report <- do.call(rbind,rows)
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',length(cases),'symmetric filter cases\n')
print(vapply(report[c('coefficient_max_error','frozen_point_scaled_error','independent_point_scaled_error','live_point_scaled_error')],function(x)if(all(is.na(x)))NA_real_ else max(x,na.rm=TRUE),numeric(1)))
if(length(timings)) {
  totals <- aggregate(seconds_per_filter~n+engine+round,do.call(rbind,timings),sum)
  cat('Median summed filter-only time by input length (seconds):\n')
  print(aggregate(seconds_per_filter~n+engine,totals,median),row.names=FALSE)
}
