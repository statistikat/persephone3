# Freeze prospective public ratios, choices, endpoint defaults and weights.
args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'x11-trend-selection-reference.R'));root<-reference_root();base<-reference_fixture_dir(root)
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1 || !startsWith(args,'--output='))stop('Usage: capture-x11-trend-selection.R --output=<empty directory>')
output<-normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside original fixtures')
before<-extended_parent_hashes(base);pin<-extended_assert_oracle(base)
fixtures<-file.path(root,'inst/tinytest/fixtures/native',c('x11-symmetric-cases.R','x11-trend-selection-cases.R'))
for(file in fixtures)source(file)
ratios<-lapply(x11_trend_cases(),function(ca) {
  y<-x11_trend_input(ca);ratio<-reference_x11_trend_ratio(y,ca$frequency,ca$mode,ca$backcasts,ca$forecasts)
  # These context fields do not change the low-level pilot on this matrix.
  alternate<-reference_x11_trend_ratio(y,ca$frequency,ca$mode,ca$backcasts,ca$forecasts,23L,TRUE)
  stopifnot(identical(ratio,alternate))
  data.frame(case=ca$id,comparison_ic_ratio=ratio,selected_length=reference_x11_trend_length(ratio,ca$frequency))
})
policies<-x11_trend_policy_cases();policies$selected_length<-mapply(reference_x11_trend_length,policies$ratio,policies$frequency)
defaults<-weights<-list()
for(f in c(4L,12L))for(length in seq.int(1L,101L,2L)) {
  id<-paste(f,length,sep='-');defaults[[id]]<-data.frame(case=id,frequency=f,length=length,endpoint_ic_ratio=reference_x11_trend_default(length,f))
  if(length<=3L)next
  bank<-reference_x11_trend_bank(length,f);h<-length%/%2L
  stopifnot(length(bank)==h)
  for(q in unique(c(0L,h%/%2L,h-1L))) {
    value<-rJava::.jcall(bank[[q+1L]],'[D','weightsToArray')
    weights[[paste(id,q,sep='-')]]<-data.frame(case=id,available=q,tap=seq_along(value),weight=value)
  }
}
stopifnot(identical(before,extended_parent_hashes(base)),!'persephone3' %in% loadedNamespaces())
write.csv(do.call(rbind,ratios),file.path(output,'ratios.csv'),row.names=FALSE)
write.csv(policies,file.path(output,'policies.csv'),row.names=FALSE)
write.csv(do.call(rbind,defaults),file.path(output,'defaults.csv'),row.names=FALSE)
write.csv(do.call(rbind,weights),file.path(output,'weights.csv'),row.names=FALSE)
files<-c(normalizePath(script),file.path(tooling,'x11-trend-selection-reference.R'),fixtures)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),source_sha256=setNames(reference_sha256(files),basename(files)),
  java=pin$java,R=R.version.string,ratio_cases=length(ratios),policy_cases=nrow(policies),default_cases=length(defaults),
  scope='Public AutomaticHenderson ratio/selection and Musgrave default ratio/bank; context-control calls assert pilot independence of explicit trend length and excludefcast. No B/C/D integration')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files<-file.path(output,c('ratios.csv','policies.csv','defaults.csv','weights.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(ratios),'ratios,',nrow(policies),'policies and',length(defaults),'defaults\n')
