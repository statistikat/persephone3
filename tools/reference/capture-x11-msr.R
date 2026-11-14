args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'x11-msr-reference.R'));root<-reference_root();base<-reference_fixture_dir(root)
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-msr.R --output=<empty directory>')
output<-normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside original fixtures')
before<-extended_parent_hashes(base);pin<-extended_assert_oracle(base)
fixture<-file.path(root,'inst/tinytest/fixtures/native/x11-msr-cases.R');source(fixture)
rows<-lapply(x11_msr_cases(),function(ca) {
  y<-x11_msr_input(ca);value<-reference_x11_msr_table(y$seasonal,y$irregular,ca$mode)
  data.frame(case=ca$id,period=seq_len(ca$frequency),seasonal=value$seasonal,irregular=value$irregular,msr=value$msr,global=value$global)
})
stopifnot(identical(before,extended_parent_hashes(base)),!'persephone3' %in% loadedNamespaces())
write.csv(do.call(rbind,rows),file.path(output,'tables.csv'),row.names=FALSE)
mode_rows<-lapply(x11_msr_mode_cases(),function(ca) {
  value<-reference_x11_msr_mode(ca$y,ca$mode)
  data.frame(case=ca$id,period=seq_len(frequency(ca$y)),iterations=value$iterations,choice=value$choice,
    seasonal=value$seasonal,irregular=value$irregular,global=value$global)
})
write.csv(do.call(rbind,mode_rows),file.path(output,'modes.csv'),row.names=FALSE)
stopifnot(identical(before,extended_parent_hashes(base)),!'persephone3' %in% loadedNamespaces())
files<-c(normalizePath(script),file.path(tooling,'x11-msr-reference.R'),fixture)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),source_sha256=setNames(reference_sha256(files),basename(files)),
  java=pin$java,R=R.version.string,cases=length(rows),mode_cases=length(mode_rows),
  scope='640 public MsrTable component cases plus 16 public-selector mode audits. Native qualification covers component tables and mode semantics only; public iteration counts identify mode-audit spans, without claiming native pilot/trimming/selection.')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files<-file.path(output,c('tables.csv','modes.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(rows),'MSR component cases\n')
