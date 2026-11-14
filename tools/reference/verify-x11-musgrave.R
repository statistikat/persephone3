# Frozen independent qualification; optional bulk public application and timing.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script));source(file.path(tooling,'common.R'))
source(file.path(tooling,'extended-fixtures.R'));source(file.path(tooling,'x11-filter-reference.R'))
source(file.path(tooling,'x11-musgrave-reference.R'));source(file.path(tooling,'x11-musgrave-processor.R'))
source(file.path(tooling,'fit-timing.R'));root <- reference_root();base <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--live','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-x11-musgrave.R [--source] [--live] [--timing] [--output=<existing empty directory>]')
if('--timing' %in% args && !'--live' %in% args)stop('Timings require --live')
output <- args[startsWith(args,'--output=')]
output <- if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
fixture_dir <- file.path(root,'inst/tinytest/fixtures/native/x11-musgrave')
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
    startsWith(output,normalizePath(base)) || startsWith(output,normalizePath(fixture_dir))))stop('Output must be empty and outside frozen corpora')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
for(file in c('x11-symmetric-cases.R','x11-musgrave-cases.R'))source(file.path(root,'inst/tinytest/fixtures/native',file))
filter <- getFromNamespace('native_x11_filter','persephone3')
spec <- getFromNamespace('native_x11_musgrave_spec','persephone3')
checks <- timed_calls <- live_calls <- 0L;rows <- timings <- list()
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
manifest <- read.csv(file.path(fixture_dir,'SHA256SUMS.csv'),stringsAsFactors=FALSE)
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'frozen endpoint artifact hashes')
before <- extended_parent_hashes(base);frozen <- dget(file.path(fixture_dir,'metadata.R'))
assert(identical(frozen$oracle_manifest_sha256,reference_sha256(file.path(base,'oracle-manifest.R'))),'pinned fixture provenance')
weights <- read.csv(file.path(fixture_dir,'weights.csv'),stringsAsFactors=FALSE)
points <- read.csv(file.path(fixture_dir,'points.csv'),stringsAsFactors=FALSE)
assert(identical(sort(unique(weights$case)),sort(names(x11_musgrave_cases()))),'complete endpoint coefficients')
assert(identical(sort(unique(points$case)),sort(names(x11_musgrave_cases()))),'complete endpoint points')
pin <- if('--live' %in% args)extended_assert_oracle(base)else NULL
scaled <- function(a,b)if(length(a)!=length(b) || !identical(is.na(a),is.na(b)))Inf else
  if(any(!is.na(b)))max(abs(a-b)/(1+abs(b)),na.rm=TRUE)else 0
cache <- new.env(parent=emptyenv())
for(ca in x11_musgrave_cases()) {
  definition <- spec(ca$frequency,ca$length,ca$available,ca$ic_ratio)
  y <- x11_symmetric_input(ca$frequency);native <- filter(y,'henderson',ca$length,'musgrave',ca$ic_ratio)
  w <- weights[weights$case==ca$id,];p <- points[points$case==ca$id,]
  coefficient_error <- max(abs(unname(definition$weights)-w$weight))
  point_error <- scaled(as.numeric(native$values[p$row]),p$value)
  assert(identical(definition$offsets,as.integer(w$offset)),paste(ca$id,'support'))
  assert(coefficient_error<=1e-14,paste(ca$id,'public coefficients'))
  assert(point_error<=2e-12,paste(ca$id,'public frozen endpoints'))
  assert(abs(sum(definition$weights)-1)<=1e-14,paste(ca$id,'constant reproduction'))
  oracle <- x11_musgrave_minimum(unname(native$weights),ca$available,ca$ic_ratio)
  independent_error <- max(abs(unname(definition$weights)-oracle))
  assert(independent_error<=2e-9,paste(ca$id,'independent constrained minimum'))
  live_error <- NA_real_
  if('--live' %in% args) {
    key <- paste(ca$frequency,ca$length,ca$ic_ratio,sep='-')
    if(!exists(key,cache,inherits=FALSE)) {
      public <- reference_musgrave_process(ca$frequency,ca$length,ca$ic_ratio,y)
      live_calls <- live_calls+1L;assign(key,public,cache)
    } else public <- get(key,cache,inherits=FALSE)
    live_error <- scaled(as.numeric(native$values),public$values)
    assert(live_error<=2e-12,paste(ca$id,'public bulk full-calendar application'))
  }
  rows[[ca$id]] <- data.frame(case=ca$id,frequency=ca$frequency,length_parameter=ca$length,
    available=ca$available,ic_ratio=ca$ic_ratio,coefficient_max_error=coefficient_error,
    frozen_point_scaled_error=point_error,independent_coefficient_error=independent_error,live_point_scaled_error=live_error)
}
# The native zero-ratio limit is a separate mathematical control, not a public
# target. It reproduces a line, with the H5 rational coefficient check retained.
zero <- spec(12L,5L,0L,0)
assert(max(abs(unname(zero$weights)-c(-105/572,105/286,467/572)))<1e-14,'closed zero-ratio minimum')
assert(max(abs(filter(stats::ts(1:101,frequency=12),'henderson',101L,'musgrave',0)$values-1:101))<2e-12,'zero-ratio line reproduction')
if('--timing' %in% args) {
  workloads <- expand.grid(frequency=c(4L,12L),length=c(5L,13L,23L,101L),ic_ratio=c(.5,3.5),KEEP.OUT.ATTRS=FALSE)
  workloads$n <- 257L
  long <- expand.grid(frequency=c(4L,12L),length=c(13L,101L),ic_ratio=3.5,KEEP.OUT.ATTRS=FALSE);long$n<-40000L
  workloads <- rbind(workloads,long)
  for(i in seq_len(nrow(workloads))) {
    ca <- workloads[i,];id <- paste(ca$frequency,ca$length,ca$ic_ratio,ca$n,sep='-')
    y <- x11_symmetric_input(ca$frequency,ca$n)
    functions <- list(native=function()filter(y,'henderson',ca$length,'musgrave',ca$ic_ratio),
      jd_x11=function()reference_musgrave_process(ca$frequency,ca$length,ca$ic_ratio,y))
    target <- functions$jd_x11();assert(scaled(as.numeric(functions$native()$values),target$values)<=2e-12,paste(id,'timing workload'))
    sizes <- vapply(functions,function(f){x<-fit_timing_once(f);timed_calls<<-timed_calls+1L
      max(1L,min(1000L,as.integer(ceiling(.05/max(.001,x$seconds)))))},integer(1))
    for(round in 1:3)for(engine in if(round%%2)names(functions)else rev(names(functions))) {
      iterations <- sizes[[engine]];invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
      while(sample$seconds<.05 && iterations<1000L) {
        iterations <- min(1000L,max(iterations+1L,ceiling(iterations*.05/max(.001,sample$seconds))))
        invisible(gc());sample <- fit_timing_once(functions[[engine]],iterations);timed_calls <- timed_calls+iterations
      }
      assert(sample$seconds>0 && scaled(as.numeric(sample$value$values),target$values)<=2e-12,paste(id,'timed output'))
      sizes[[engine]] <- iterations
      timings[[length(timings)+1L]] <- data.frame(case=id,n=ca$n,frequency=ca$frequency,length_parameter=ca$length,
        ic_ratio=ca$ic_ratio,engine=engine,round=round,iterations=iterations,elapsed_seconds=sample$seconds,seconds_per_filter=sample$seconds/iterations)
    }
    cat(id,'endpoint timing PASS\n')
  }
}
assert(identical(before,extended_parent_hashes(base)),'original corpus unchanged')
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'endpoint corpus unchanged')
if(!'--live' %in% args)assert(!any(c('rJava','rjd3toolkit','rjd3x13','rjd3tramoseats') %in% loadedNamespaces()),'offline without Java namespaces')
files <- c(file.path(tooling,c('verify-x11-musgrave.R','x11-filter-reference.R','x11-musgrave-reference.R','x11-musgrave-processor.R','fit-timing.R')),
  file.path(root,c('src/x11_musgrave.cpp','src/core/x11_musgrave.hpp','src/x11_filters.cpp','src/core/x11_filters.hpp',
    'src/numerics.cpp','src/rcpp_x11_musgrave.cpp','src/rcpp_x11_filters.cpp','R/native_x11_filters.R',
    'inst/tinytest/fixtures/native/x11-symmetric-cases.R','inst/tinytest/fixtures/native/x11-musgrave-cases.R')))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  fixture_manifest_sha256=reference_sha256(file.path(fixture_dir,'SHA256SUMS.csv')),R=R.version.string,java=if(!is.null(pin))pin$java else NULL,
  checks=checks,cases=length(rows),public_complete_filter_calls=live_calls,coefficient_absolute_budget=1e-14,point_scaled_budget=2e-12,
  independent_minimum_absolute_budget=2e-9,timing_calls_including_warmup_and_calibration_retries=timed_calls,
  timing_scope='Henderson filter plus Musgrave endpoints: native includes R calendar/boundary metadata; public uses factory, bulk symmetric DataBlock filtering and AsymmetricEndPoints processor, coefficients and R-vector serialization. Not full X-11 or fitted-model timings',
  timing_conventions='3 warmed alternating rounds; >=50ms calibrated batches when possible; setup/loading/startup and explicit pre-batch GC excluded',
  parallel=getFromNamespace('.native_parallel_info','persephone3')())
report <- do.call(rbind,rows)
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',length(rows),'endpoint cases /',live_calls,'public full filters\n')
print(vapply(report[c('coefficient_max_error','frozen_point_scaled_error','independent_coefficient_error','live_point_scaled_error')],function(x)if(all(is.na(x)))NA_real_ else max(x,na.rm=TRUE),numeric(1)))
if(length(timings)) {
  totals <- aggregate(seconds_per_filter~n+engine+round,do.call(rbind,timings),sum)
  cat('Median summed filter-only time by input length (seconds):\n')
  print(aggregate(seconds_per_filter~n+engine,totals,median),row.names=FALSE)
}
