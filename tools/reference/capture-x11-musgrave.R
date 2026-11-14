# Additive public-API capture: neither old corpus nor production output is input.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script));source(file.path(tooling,'common.R'))
source(file.path(tooling,'extended-fixtures.R'));source(file.path(tooling,'x11-filter-reference.R'))
source(file.path(tooling,'x11-musgrave-reference.R'))
root <- reference_root();base <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-musgrave.R --output=<existing empty directory>')
output <- normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))
  stop('Output must be empty and outside the original corpus')
before <- extended_parent_hashes(base);pin <- extended_assert_oracle(base)
fixtures <- file.path(root,'inst/tinytest/fixtures/native',c('x11-symmetric-cases.R','x11-musgrave-cases.R'))
for(fixture in fixtures)source(fixture)
weights <- points <- list()
for(ca in x11_musgrave_cases()) {
  bank <- reference_musgrave_bank(ca$frequency,ca$length,ca$ic_ratio)
  f <- bank$right[[ca$available+1L]];w <- rJava::.jcall(f,'[D','weightsToArray')
  offsets <- seq.int(rJava::.jcall(f,'I','getLowerBound'),rJava::.jcall(f,'I','getUpperBound'))
  stopifnot(length(w)==length(offsets),all(is.finite(w)))
  weights[[ca$id]] <- data.frame(case=ca$id,offset=offsets,weight=w)
  y <- x11_symmetric_input(ca$frequency)
  points[[ca$id]] <- data.frame(case=ca$id,side=c('left','right'),row=c(ca$available+1L,length(y)-ca$available),
    value=c(reference_musgrave_point(f,y,'left'),reference_musgrave_point(f,y)))
}
stopifnot(identical(before,extended_parent_hashes(base)))
write.csv(do.call(rbind,weights),file.path(output,'weights.csv'),row.names=FALSE)
write.csv(do.call(rbind,points),file.path(output,'points.csv'),row.names=FALSE)
files <- c(normalizePath(script),file.path(tooling,c('x11-filter-reference.R','x11-musgrave-reference.R')),fixtures)
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),
  source_sha256=setNames(reference_sha256(files),basename(files)),java=pin$java,R=R.version.string,
  cases=length(weights),public_scope='Public Musgrave makeFilters on explicit Henderson lengths and I/C ratios, coefficient bounds and DataBlock applications; left mirrors right; no automatic choice or decomposition claim',
  zero_ratio='Excluded from public fixtures: public factory returns NaN at zero. Native zero is independently qualified minimum-distance slope-reproduction limit')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files <- file.path(output,c('weights.csv','points.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(weights),'public Musgrave endpoint cases\n')
