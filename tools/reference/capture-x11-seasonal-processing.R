# Public fixed-choice/stable/short-series processing; no native execution.
args<-commandArgs(trailingOnly=FALSE)
script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'))
source(file.path(tooling,'extended-fixtures.R'));source(file.path(tooling,'x11-seasonal-processing-reference.R'))
root<-reference_root();base<-reference_fixture_dir(root);args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-seasonal-processing.R --output=<existing empty directory>')
output<-normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside the original corpus')
before<-extended_parent_hashes(base);pin<-extended_assert_oracle(base)
fixtures<-file.path(root,'inst/tinytest/fixtures/native',c('x11-symmetric-cases.R','x11-seasonal-cases.R',
  'x11-seasonal15-cases.R','x11-seasonal-processing-cases.R'))
for(fixture in fixtures)source(fixture)
weights<-points<-list()
for(ca in x11_processing_cases()) {
  y<-x11_processing_input(ca)
  values<-reference_x11_seasonal_processing(ca$frequency,ca$filters,y,ca$start[2])$values
  points[[ca$id]]<-data.frame(case=ca$id,row=seq_len(ca$n),value=values)
  phase<-seq.int(1L,ca$n,ca$frequency);indices<-seq_along(phase)
  rows<-phase[sort(unique(c(head(indices,8L),tail(indices,8L),as.integer(length(indices)%/%2L+1L))))]
  A<-matrix(vapply(phase,function(j)reference_x11_seasonal_processing(ca$frequency,ca$filters,
    as.numeric(seq_len(ca$n)==j),ca$start[2])$values[rows],numeric(length(rows))),nrow=length(rows),ncol=length(phase))
  nonzero<-which(A!=0,arr.ind=TRUE)
  weights[[ca$id]]<-data.frame(case=ca$id,row=rows[nonzero[,1]],column=phase[nonzero[,2]],weight=A[nonzero])
}
stopifnot(identical(before,extended_parent_hashes(base)),!'persephone3' %in% loadedNamespaces())
write.csv(do.call(rbind,weights),file.path(output,'weights.csv'),row.names=FALSE)
write.csv(do.call(rbind,points),file.path(output,'points.csv'),row.names=FALSE)
files<-c(normalizePath(script),file.path(tooling,'x11-seasonal-processing-reference.R'),fixtures)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),source_sha256=setNames(reference_sha256(files),basename(files)),
  java=pin$java,R=R.version.string,cases=length(points),
  public_scope='Public seasonal enum/factory/processor; fixed uniform/per-calendar-period choices including Stable, complete/incomplete years and short-series gates. Full points plus first-phase boundary/middle basis probes. Not data-driven MSR or a full decomposition')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files<-file.path(output,c('weights.csv','points.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(points),'seasonal processing cases\n')
