# Capture an additive public-API fixture without touching existing corpora.
args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling <- dirname(normalizePath(script));source(file.path(tooling,'common.R'))
source(file.path(tooling,'extended-fixtures.R'));source(file.path(tooling,'x11-filter-reference.R'))
root <- reference_root();base <- reference_fixture_dir(root)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-symmetric.R --output=<existing empty directory>')
output <- normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))
  stop('Output must be empty and outside the original corpus')
before <- extended_parent_hashes(base);pin <- extended_assert_oracle(base)
fixture <- file.path(root,'inst/tinytest/fixtures/native/x11-symmetric-cases.R');source(fixture)
weights <- points <- list()
for(ca in x11_symmetric_cases()) {
  f <- reference_x11_filter(ca)
  w <- rJava::.jcall(f,'[D','weightsToArray');lower <- rJava::.jcall(f,'I','getLowerBound');upper <- rJava::.jcall(f,'I','getUpperBound')
  spacing <- if(startsWith(ca$type,'s3x'))ca$frequency else 1L
  offsets <- seq.int(lower,upper)*spacing
  weights[[ca$id]] <- data.frame(case=ca$id,offset=offsets,weight=w)
  y <- x11_symmetric_input(ca$frequency);value <- reference_x11_apply(f,y,spacing)
  rows <- which(!is.na(value));selected <- rows[unique(round(seq(1,length(rows),length.out=8)))]
  points[[ca$id]] <- data.frame(case=ca$id,row=selected,value=value[selected],
    first_available=min(rows),last_available=max(rows))
}
stopifnot(identical(before,extended_parent_hashes(base)))
write.csv(do.call(rbind,weights),file.path(output,'weights.csv'),row.names=FALSE)
write.csv(do.call(rbind,points),file.path(output,'points.csv'),row.names=FALSE)
files <- c(normalizePath(script),file.path(tooling,'x11-filter-reference.R'),fixture)
metadata <- list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),
  source_sha256=setNames(reference_sha256(files),basename(files)),java=pin$java,R=R.version.string,
  cases=length(weights),public_scope='Only public X-11 filter factories, coefficients and finite-input DataBlock application; no decomposition, endpoint or automatic-selection claim')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files <- file.path(output,c('weights.csv','points.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(weights),'public symmetric filter cases\n')
