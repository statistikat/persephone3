args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'x11-initial-reference.R'));root<-reference_root();base<-reference_fixture_dir(root)
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-initial.R --output=<empty directory>')
output<-normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside original fixtures')
pin<-extended_assert_oracle(base);before<-extended_parent_hashes(base)
helper<-file.path(root,'inst/tinytest/fixtures/native/x11-initial-cases.R');source(helper)
cases<-x11_initial_cases();points<-bstep_points<-statuses<-list()
as_points<-function(id,tables)do.call(rbind,lapply(names(tables),function(name)
  data.frame(case=id,table=name,row=seq_along(tables[[name]]),value=as.numeric(tables[[name]]))))
for(ca in cases) {
  y<-x11_initial_input(ca);primitive<-reference_x11_initial_primitives(y,ca$filters)
  bstep<-tryCatch(reference_x11_initial_bstep(y,ca$filters),error=function(e)e)
  points[[ca$id]]<-as_points(ca$id,primitive[setdiff(names(primitive),'drop')])
  success<-!inherits(bstep,'error')
  statuses[[ca$id]]<-data.frame(case=ca$id,drop=primitive$drop,bstep_available=success,
    bstep_error=if(success)''else conditionMessage(bstep))
  if(success) {
    stopifnot(identical(bstep$drop,primitive$drop))
    bstep_points[[ca$id]]<-as_points(ca$id,bstep[setdiff(names(bstep),'drop')])
  }
}
stopifnot(identical(before,extended_parent_hashes(base)),!'persephone3' %in% loadedNamespaces())
for(name in c('points','bstep_points','statuses'))write.csv(do.call(rbind,get(name)),file.path(output,paste0(name,'.csv')),row.names=FALSE)
files<-c(normalizePath(script),file.path(tooling,'x11-initial-reference.R'),helper)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),
  source_sha256=setNames(reference_sha256(files),basename(files)),java=pin$java,R=R.version.string,cases=length(cases),
  bstep_available=sum(vapply(statuses,function(x)x$bstep_available,logical(1))),
  scope='Fixed additive initial B1/B2/B3/B4a/B4anorm/B4d public primitive chain and actual public B-step getters. No native runtime is loaded; independent R operators are separate. B-step failures are recorded explicitly because its process continues beyond the initial tables.')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files<-file.path(output,c('points.csv','bstep_points.csv','statuses.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(cases),'public initial-table cases;',metadata$bstep_available,'actual B-step successes\n')
