# Additive capture from public mean endpoint APIs. Native code is never loaded.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script));source(file.path(tooling,'common.R'))
source(file.path(tooling,'extended-fixtures.R'));source(file.path(tooling,'x11-filter-reference.R'))
source(file.path(tooling,'x11-mean-reference.R'));root <- reference_root();base <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-mean.R --output=<existing empty directory>')
output <- normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside the original corpus')
before <- extended_parent_hashes(base);pin <- extended_assert_oracle(base)
fixtures <- file.path(root,'inst/tinytest/fixtures/native',c('x11-symmetric-cases.R','x11-mean-cases.R'))
for(fixture in fixtures)source(fixture)
weights <- points <- list()
for(ca in x11_mean_cases()) {
  y <- x11_symmetric_input(ca$frequency,ca$n)
  values <- reference_x11_mean(ca$frequency,y)$values
  points[[ca$id]] <- data.frame(case=ca$id,row=seq_len(ca$n),value=values)
  h <- ca$frequency%/%2L
  rows <- if(ca$n<=17L)seq_len(ca$n)else unique(c(seq_len(h),ca$n-h+seq_len(h),ca$n%/%2L))
  A <- vapply(seq_len(ca$n),function(j)reference_x11_mean(ca$frequency,as.numeric(seq_len(ca$n)==j))$values[rows],numeric(length(rows)))
  indices <- which(A!=0,arr.ind=TRUE)
  weights[[ca$id]] <- data.frame(case=ca$id,row=rows[indices[,1]],column=indices[,2],weight=A[indices])
}
stopifnot(identical(before,extended_parent_hashes(base)))
write.csv(do.call(rbind,weights),file.path(output,'weights.csv'),row.names=FALSE)
write.csv(do.call(rbind,points),file.path(output,'points.csv'),row.names=FALSE)
files <- c(normalizePath(script),file.path(tooling,c('x11-filter-reference.R','x11-mean-reference.R')),fixtures)
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),source_sha256=setNames(reference_sha256(files),basename(files)),
  java=pin$java,R=R.version.string,cases=length(points),
  public_scope='Public FilteredMeanEndPoints with centered 2 x frequency trend filters; complete finite inputs and basis-vector operator probes; no automatic policy or decomposition claim')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files <- file.path(output,c('weights.csv','points.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(points),'mean endpoint cases\n')
