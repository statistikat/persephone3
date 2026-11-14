# Public 3x15 endpoint capture; no native execution.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script));source(file.path(tooling,'common.R'))
source(file.path(tooling,'extended-fixtures.R'));source(file.path(tooling,'x11-seasonal-reference.R'))
root <- reference_root();base <- reference_fixture_dir(root);args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-seasonal15.R --output=<existing empty directory>')
output <- normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside the original corpus')
before <- extended_parent_hashes(base);pin <- extended_assert_oracle(base)
fixtures <- file.path(root,'inst/tinytest/fixtures/native',c('x11-symmetric-cases.R','x11-seasonal15-cases.R'))
for(fixture in fixtures)source(fixture)
weights <- points <- controls <- list()
for(ca in x11_seasonal15_cases()) {
  y <- x11_seasonal15_input(ca)
  values <- reference_x11_seasonal(ca$frequency,ca$type,y,ca$start[2])$values
  points[[ca$id]] <- data.frame(case=ca$id,row=seq_len(ca$n),value=values)
  phase <- seq.int(1L,ca$n,ca$frequency);rows <- phase[c(1:8,length(phase)-7:0)]
  A <- vapply(phase,function(j)reference_x11_seasonal(ca$frequency,ca$type,
    as.numeric(seq_len(ca$n)==j),ca$start[2])$values[rows],numeric(length(rows)))
  indices <- which(A!=0,arr.ind=TRUE)
  weights[[ca$id]] <- data.frame(case=ca$id,row=rows[indices[,1]],column=phase[indices[,2]],weight=A[indices])
}
# These characterize short-series policy separately; they do not determine
# native acceptance or change the explicit minimum of twenty per phase.
for(frequency in c(4L,12L))for(n in c(17L*frequency,19L*frequency,19L*frequency+1L,20L*frequency-1L))for(period in c(1L,3L,frequency)) {
  y <- x11_symmetric_input(frequency,n)
  values <- reference_x11_seasonal(frequency,'s3x15',y,period)$values
  id <- paste(frequency,n,period,sep='-')
  controls[[id]] <- data.frame(case=id,frequency=frequency,n=n,start_period=period,row=seq_len(n),value=values)
}
stopifnot(identical(before,extended_parent_hashes(base)))
write.csv(do.call(rbind,weights),file.path(output,'weights.csv'),row.names=FALSE)
write.csv(do.call(rbind,points),file.path(output,'points.csv'),row.names=FALSE)
write.csv(do.call(rbind,controls),file.path(output,'selection-controls.csv'),row.names=FALSE)
files <- c(normalizePath(script),file.path(tooling,'x11-seasonal-reference.R'),fixtures)
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),source_sha256=setNames(reference_sha256(files),basename(files)),
  java=pin$java,R=R.version.string,cases=length(points),selection_controls=length(controls),
  public_scope='Public seasonal enum/factory/processor; uniform S3X15, full points and boundary basis probes. Separate short-series controls do not authorize automatic selection or a complete decomposition claim')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files <- file.path(output,c('weights.csv','points.csv','selection-controls.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(points),'S3X15 endpoint cases and',length(controls),'short-series controls\n')
