# Frozen explicit seasonal endpoints; optional public replay and filter timings.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script));source(file.path(tooling,'common.R'))
source(file.path(tooling,'extended-fixtures.R'));source(file.path(tooling,'x11-seasonal-reference.R'))
source(file.path(tooling,'fit-timing.R'))
root <- reference_root();base <- reference_fixture_dir(root);args <- commandArgs(trailingOnly=TRUE)
if(anyDuplicated(sub('=.*$','',args)) || any(!args %in% c('--source','--live','--timing') & !startsWith(args,'--output=')))
  stop('Usage: verify-x11-seasonal.R [--source] [--live] [--timing] [--output=<existing empty directory>]')
if('--timing' %in% args && !'--live' %in% args)stop('Timings require --live')
output <- args[startsWith(args,'--output=')]
output <- if(length(output))normalizePath(sub('^--output=','',output),mustWork=TRUE)else NULL
fixture_dir <- file.path(root,'inst/tinytest/fixtures/native/x11-seasonal')
if(!is.null(output) && (!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) ||
    startsWith(output,normalizePath(base)) || startsWith(output,normalizePath(fixture_dir))))stop('Output must be empty and outside frozen corpora')
if('--source' %in% args)pkgload::load_all(root,quiet=TRUE,compile=FALSE)else
  if(!requireNamespace('persephone3',quietly=TRUE))stop('Install persephone3 first')
for(file in c('x11-symmetric-cases.R','x11-seasonal-cases.R'))source(file.path(root,'inst/tinytest/fixtures/native',file))
filter <- getFromNamespace('native_x11_filter','persephone3')
spec <- getFromNamespace('native_x11_seasonal_spec','persephone3')
checks <- timed_calls <- 0L;rows <- timings <- list()
assert <- function(value,label){checks<<-checks+1L;if(!isTRUE(value))stop('Qualification failed: ',label)}
manifest <- read.csv(file.path(fixture_dir,'SHA256SUMS.csv'),stringsAsFactors=FALSE)
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'frozen seasonal artifacts')
before <- extended_parent_hashes(base);frozen <- dget(file.path(fixture_dir,'metadata.R'))
assert(identical(frozen$oracle_manifest_sha256,reference_sha256(file.path(base,'oracle-manifest.R'))),'pinned fixture provenance')
weights <- read.csv(file.path(fixture_dir,'weights.csv'),stringsAsFactors=FALSE)
points <- read.csv(file.path(fixture_dir,'points.csv'),stringsAsFactors=FALSE)
assert(identical(sort(unique(points$case)),sort(names(x11_seasonal_cases()))),'complete point matrix')
assert(identical(sort(unique(weights$case)),sort(names(x11_seasonal_cases()))),'complete operator matrix')
pin <- if('--live' %in% args)extended_assert_oracle(base)else NULL
scaled <- function(a,b)if(length(a)!=length(b) || !identical(is.na(a),is.na(b)))Inf else
  if(any(!is.na(b)))max(abs(a-b)/(1+abs(b)),na.rm=TRUE)else 0
for(ca in x11_seasonal_cases()) {
  y <- x11_seasonal_input(ca);native <- filter(y,ca$type,endpoints='seasonal')
  w <- weights[weights$case==ca$id,];p <- points[points$case==ca$id,]
  coefficient_error <- 0
  for(row in unique(w$row)) {
    side <- if(row<=native$radius)'left'else'right'
    q <- if(side=='left')(row-1L)%/%ca$frequency else(ca$n-row)%/%ca$frequency
    definition <- spec(ca$type,ca$frequency,q,side);target <- w[w$row==row,]
    assert(identical(row+definition$offsets,as.integer(target$column)),paste(ca$id,row,'operator support'))
    coefficient_error <- max(coefficient_error,abs(unname(definition$weights)-target$weight))
    assert(abs(sum(definition$weights)-1)<=1e-14 && all(definition$weights>0),paste(ca$id,row,'constant reproduction/positive weights'))
  }
  point_error <- scaled(as.numeric(native$values[p$row]),p$value)
  assert(coefficient_error<=1e-14,paste(ca$id,'frozen public operators'))
  assert(point_error<=2e-12,paste(ca$id,'frozen public points'))
  independent_error <- NA_real_
  if(ca$N %in% c(3L,5L)) {
    independent_error <- scaled(as.numeric(native$values),x11_seasonal_virtual_phases(as.numeric(y),ca$frequency,ca$N))
    assert(independent_error<=2e-12,paste(ca$id,'independent virtual extension/convolution'))
  }
  assert(identical(native$available_rows,seq_len(ca$n)) && identical(tsp(native$values),tsp(y)),paste(ca$id,'full calendar'))
  center <- filter(y,ca$type)
  assert(identical(native$values[native$central_rows],center$values[center$central_rows]),paste(ca$id,'unchanged central arithmetic'))
  live_error <- NA_real_
  if('--live' %in% args) {
    public <- reference_x11_seasonal(ca$frequency,ca$type,y,ca$start[2])
    live_error <- scaled(as.numeric(native$values),public$values)
    assert(live_error<=2e-12,paste(ca$id,'public processor application'))
  }
  rows[[ca$id]] <- data.frame(case=ca$id,frequency=ca$frequency,type=ca$type,n=ca$n,start_period=ca$start[2],
    operator_max_error=coefficient_error,frozen_point_scaled_error=point_error,
    independent_point_scaled_error=independent_error,live_point_scaled_error=live_error)
}
controls <- read.csv(file.path(fixture_dir,'selection-controls.csv'),stringsAsFactors=FALSE)
for(id in unique(controls$case)) {
  control <- controls[controls$case==id,];f <- control$frequency[1]
  y <- x11_symmetric_input(f,nrow(control));expected <- ave(as.numeric(y),rep(seq_len(f),length.out=length(y)),FUN=mean)
  assert(scaled(control$value,expected)<=2e-12,paste(id,'public short S3X1 stable control'))
  assert(inherits(tryCatch(filter(y,'s3x1',endpoints='seasonal'),error=identity),'error'),paste(id,'explicit unsupported short selection'))
  if('--live' %in% args)assert(scaled(reference_x11_seasonal(f,'s3x1',y,3L)$values,expected)<=2e-12,paste(id,'live short selection control'))
}
if('--timing' %in% args) {
  workloads <- x11_seasonal_cases()
  for(frequency in c(4L,12L))for(N in c(1L,3L,5L,9L)) {
    id<-paste(frequency,N,40003L,sep='-')
    workloads[[id]]<-list(id=id,frequency=frequency,N=N,type=paste0('s3x',N),n=40003L,start=c(-2L,3L))
  }
  for(ca in workloads) {
    y <- x11_seasonal_input(ca)
    functions <- list(native=function()filter(y,ca$type,endpoints='seasonal'),
      jd_x11=function()reference_x11_seasonal(ca$frequency,ca$type,y,ca$start[2]))
    target <- functions$jd_x11();assert(scaled(as.numeric(functions$native()$values),target$values)<=2e-12,paste(ca$id,'timing workload'))
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
      timings[[length(timings)+1L]] <- data.frame(case=ca$id,n=ca$n,input_group=if(ca$n==40003L)'long'else'short',
        frequency=ca$frequency,type=ca$type,engine=engine,round=round,iterations=iterations,
        elapsed_seconds=sample$seconds,seconds_per_filter=sample$seconds/iterations)
    }
    cat(ca$id,'seasonal-endpoint timing PASS\n')
  }
}
assert(identical(before,extended_parent_hashes(base)),'original corpus unchanged')
assert(identical(reference_sha256(file.path(fixture_dir,manifest$file)),manifest$sha256),'seasonal endpoint corpus unchanged')
if(!'--live' %in% args)assert(!any(c('rJava','rjd3toolkit','rjd3x13','rjd3tramoseats') %in% loadedNamespaces()),'offline without Java namespaces')
files <- c(file.path(tooling,c('verify-x11-seasonal.R','x11-seasonal-reference.R','fit-timing.R')),
  file.path(root,c('src/x11_seasonal_endpoints.cpp','src/core/x11_seasonal_endpoints.hpp','src/x11_filters.cpp','src/core/x11_filters.hpp',
    'src/numerics.cpp','src/rcpp_x11_seasonal_endpoints.cpp','src/rcpp_x11_filters.cpp','R/native_x11_filters.R',
    'inst/tinytest/fixtures/native/x11-symmetric-cases.R','inst/tinytest/fixtures/native/x11-seasonal-cases.R')))
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  parent_source_commit=system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE),source_sha256=setNames(reference_sha256(files),basename(files)),
  fixture_manifest_sha256=reference_sha256(file.path(fixture_dir,'SHA256SUMS.csv')),R=R.version.string,java=if(!is.null(pin))pin$java else NULL,
  checks=checks,cases=length(rows),selection_controls=length(unique(controls$case)),operator_absolute_budget=1e-14,point_scaled_budget=2e-12,
  timing_calls_including_warmup_and_calibration_retries=timed_calls,
  timing_scope='Uniform explicit seasonal 3x1/3x3/3x5/3x9 filters with endpoints: native includes R calendar/phase/boundary metadata; public includes enum/factory/processor and vector serialization. Not full X-11 or fitted-model timings',
  timing_conventions='3 warmed alternating rounds; >=50ms calibrated batches when possible; setup/loading/startup and explicit pre-batch GC excluded; natural in-batch GC included',
  parallel=getFromNamespace('.native_parallel_info','persephone3')())
report <- do.call(rbind,rows)
if(!is.null(output)) {
  write.csv(report,file.path(output,'matrix.csv'),row.names=FALSE)
  writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
  if(length(timings))write.csv(do.call(rbind,timings),file.path(output,'timings.csv'),row.names=FALSE)
}
cat('PASS:',checks,'checks /',length(rows),'seasonal-endpoint cases\n')
print(vapply(report[c('operator_max_error','frozen_point_scaled_error','independent_point_scaled_error','live_point_scaled_error')],function(x)if(all(is.na(x)))NA_real_ else max(x,na.rm=TRUE),numeric(1)))
if(length(timings)) {
  totals <- aggregate(seconds_per_filter~input_group+engine+round,do.call(rbind,timings),sum)
  cat('Median summed filter-only time by input group (seconds):\n')
  print(aggregate(seconds_per_filter~input_group+engine,totals,median),row.names=FALSE)
}
