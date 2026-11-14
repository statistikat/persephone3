args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'x11-normalization-reference.R'));root<-reference_root();base<-reference_fixture_dir(root)
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-normalization.R --output=<empty directory>')
output<-normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside original fixtures')
pin<-extended_assert_oracle(base);before<-extended_parent_hashes(base)
helper<-file.path(root,'inst/tinytest/fixtures/native/x11-normalization-cases.R');source(helper)
cases<-x11_normalization_cases();points<-list()
for(ca in cases) {
  y<-x11_normalization_input(ca);value<-reference_x11_normalization(y,ca$filters,ca$extend)
  points[[ca$id]]<-data.frame(case=ca$id,row=seq_along(value),value=value)
}
stopifnot(identical(before,extended_parent_hashes(base)),!'persephone3' %in% loadedNamespaces())
write.csv(do.call(rbind,points),file.path(output,'points.csv'),row.names=FALSE)
files<-c(normalizePath(script),file.path(tooling,'x11-normalization-reference.R'),helper)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),
  source_sha256=setNames(reference_sha256(files),basename(files)),java=pin$java,R=R.version.string,cases=length(cases),
  scope='Public additive normalization for fixed/stable/mixed requests, native supported span >=2 annual cycles, explicit periodic restoration; no full X-11 pass or fit. Independent R operators are separate from capture.')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files<-file.path(output,c('points.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(cases),'public normalization cases\n')
