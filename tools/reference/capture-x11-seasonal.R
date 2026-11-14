# Public seasonal endpoint operator/point capture; no native execution.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script));source(file.path(tooling,'common.R'))
source(file.path(tooling,'extended-fixtures.R'));source(file.path(tooling,'x11-seasonal-reference.R'))
root <- reference_root();base <- reference_fixture_dir(root);args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-seasonal.R --output=<existing empty directory>')
output <- normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside the original corpus')
before <- extended_parent_hashes(base);pin <- extended_assert_oracle(base)
fixtures <- file.path(root,'inst/tinytest/fixtures/native',c('x11-symmetric-cases.R','x11-seasonal-cases.R'))
for(fixture in fixtures)source(fixture)
weights <- points <- controls <- list()
for(ca in x11_seasonal_cases()) {
  y <- x11_seasonal_input(ca)
  values <- reference_x11_seasonal(ca$frequency,ca$type,y,ca$start[2])$values
  points[[ca$id]] <- data.frame(case=ca$id,row=seq_len(ca$n),value=values)
  phase <- seq.int(1L,ca$n,ca$frequency);h <- (ca$N+1L)%/%2L
  rows <- phase[unique(c(seq_len(h),length(phase)-h+seq_len(h)))]
  A <- vapply(phase,function(j)reference_x11_seasonal(ca$frequency,ca$type,
    as.numeric(seq_len(ca$n)==j),ca$start[2])$values[rows],numeric(length(rows)))
  indices <- which(A!=0,arr.ind=TRUE)
  weights[[ca$id]] <- data.frame(case=ca$id,row=rows[indices[,1]],column=phase[indices[,2]],weight=A[indices])
}
# Below five annual observations S3X1's public processor selects the stable
# phase mean. This is a separate selection control, outside the first native
# explicit-endpoint scope; it is not silently substituted in production.
for(frequency in c(4L,12L))for(years in c(3L,4L)) {
  y <- x11_symmetric_input(frequency,frequency*years)
  values <- reference_x11_seasonal(frequency,'s3x1',y,3L)$values
  id <- paste(frequency,years,sep='-')
  controls[[id]] <- data.frame(case=id,frequency=frequency,years=years,row=seq_along(y),value=values)
}
stopifnot(identical(before,extended_parent_hashes(base)))
write.csv(do.call(rbind,weights),file.path(output,'weights.csv'),row.names=FALSE)
write.csv(do.call(rbind,points),file.path(output,'points.csv'),row.names=FALSE)
write.csv(do.call(rbind,controls),file.path(output,'selection-controls.csv'),row.names=FALSE)
files <- c(normalizePath(script),file.path(tooling,'x11-seasonal-reference.R'),fixtures)
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),source_sha256=setNames(reference_sha256(files),basename(files)),
  java=pin$java,R=R.version.string,cases=length(points),selection_controls=length(controls),
  public_scope='Public seasonal enum/factory/processor; uniform 3x1/3x3/3x5/3x9 selections, full point and boundary basis probes. Separately captured S3X1 stable selection below five annual observations; no complete decomposition claim')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files <- file.path(output,c('weights.csv','points.csv','selection-controls.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(points),'seasonal endpoint cases and',length(controls),'short-series selection controls\n')
