args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'x11-seasonal-plan-reference.R'));root<-reference_root();base<-reference_fixture_dir(root)
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-seasonal-plan.R --output=<empty directory>')
output<-normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside original fixtures')
before<-extended_parent_hashes(base);pin<-extended_assert_oracle(base)
fixture<-file.path(root,'inst/tinytest/fixtures/native/x11-seasonal-plan-cases.R');source(fixture)
rows<-lapply(x11_seasonal_plan_cases(),function(ca) {
  result<-reference_x11_seasonal_plan(x11_seasonal_plan_input(ca),ca$filters,ca$mode)
  stopifnot(length(result$initial_filters)==ca$frequency,length(result$final_filters)==ca$frequency)
  data.frame(case=ca$id,period=seq_len(ca$frequency),initial_filter=result$initial_filters,
    final_filter=result$final_filters,global_msr=result$global_msr)
})
stopifnot(identical(before,extended_parent_hashes(base)),!'persephone3' %in% loadedNamespaces())
write.csv(do.call(rbind,rows),file.path(output,'plans.csv'),row.names=FALSE)
files<-c(normalizePath(script),file.path(tooling,'x11-seasonal-plan-reference.R'),fixture)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),source_sha256=setNames(reference_sha256(files),basename(files)),
  java=pin$java,R=R.version.string,cases=length(rows),
  scope='Validated public X11Spec/Context initial/final filter mapping and global MSR flag. Fixed and scalar/per-period special requests, spans and calendars. No numerical passes or data-driven final MSR choice')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files<-file.path(output,c('plans.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(rows),'seasonal context cases\n')
