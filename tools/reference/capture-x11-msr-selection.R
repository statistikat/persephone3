args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
helpers<-c('x11-filter-reference.R','x11-mean-reference.R','x11-msr-reference.R','x11-msr-selection-reference.R')
for(file in helpers)source(file.path(tooling,file))
root<-reference_root();base<-reference_fixture_dir(root);args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: capture-x11-msr-selection.R --output=<empty directory>')
output<-normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside original fixtures')
before<-extended_parent_hashes(base);pin<-extended_assert_oracle(base)
fixtures<-file.path(root,'inst/tinytest/fixtures/native',c('x11-msr-cases.R','x11-msr-selection-cases.R'))
for(file in fixtures)source(file)
cases<-x11_msr_selection_cases();summaries<-tables<-attempts<-pilots<-list()
for(ca in cases) {
  y<-x11_msr_selection_input(ca)
  value<-reference_x11_msr_select(y,ca$mode,ca$backcasts,ca$forecasts,ca$exclude_forecast)
  summaries[[ca$id]]<-data.frame(case=ca$id,choice=tolower(value$choice),iterations=value$iterations,
    using_default=value$using_default,global=value$global,has_table=!is.null(value$table))
  if(!is.null(value$table))tables[[ca$id]]<-data.frame(case=ca$id,period=seq_len(ca$frequency),seasonal=value$table$seasonal,
    irregular=value$table$irregular,global=value$table$global)
  rows<-x11_msr_selection_rows(y,ca$backcasts,ca$forecasts)$calendar
  for(j in seq_len(value$iterations)) {
    if(j>1L)rows<-head(rows,-ca$frequency)
    stopifnot(length(rows)>=5L*ca$frequency)
    cropped<-ts(as.numeric(y)[rows],start=as.numeric(time(y)[rows[1L]]),frequency=ca$frequency)
    pilot<-reference_x11_msr_pilot(cropped,ca$mode)
    table<-reference_x11_msr_table(pilot$seasonal,pilot$irregular,ca$mode)
    attempts[[length(attempts)+1L]]<-data.frame(case=ca$id,iteration=j,observations=length(rows),years_removed=j-1L,
      last_row=tail(rows,1),global=table$global)
    if(j==value$iterations)pilots[[ca$id]]<-data.frame(case=ca$id,row=rows,seasonal=as.numeric(pilot$seasonal),irregular=as.numeric(pilot$irregular))
  }
}
# Find exact public scalar thresholds on five-year controls, where the returned
# MSR is always the first table's ratio. Store input coefficients in hexadecimal
# so a decimal serialization cannot move a control to the adjacent double.
thresholds<-list()
make<-function(amplitude,level)ts(rep(seq_len(5)+amplitude*(-1)^seq_len(5)+level,each=4),frequency=4)
for(target in c(2.5,3.5,5.5,6.5)) {
  hit<-NULL
  for(level in c(0,1,10,100,-10)) {
    value<-function(a)reference_x11_msr_select(make(a,level),'additive')$global
    amplitude<-uniroot(function(a)value(a)-target,c(0,30),tol=1e-14)$root
    for(k in -100:100) {
      a<-amplitude+k*.Machine$double.eps*abs(amplitude)
      public<-reference_x11_msr_select(make(a,level),'additive')
      if(identical(public$global,target)) {
        hit<-data.frame(ratio=target,amplitude_hex=sprintf('%a',a),level=level,choice=tolower(public$choice),
          using_default=public$using_default,iterations=public$iterations);break
      }
    }
    if(!is.null(hit))break
  }
  if(is.null(hit))stop('No exact public threshold control found: ',target)
  thresholds[[as.character(target)]]<-hit
}
stopifnot(identical(before,extended_parent_hashes(base)),!'persephone3' %in% loadedNamespaces())
for(name in c('summaries','tables','attempts','pilots','thresholds'))write.csv(do.call(rbind,get(name)),file.path(output,paste0(name,'.csv')),row.names=FALSE)
files<-c(normalizePath(script),file.path(tooling,helpers),fixtures)
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),source_sha256=setNames(reference_sha256(files),basename(files)),
  java=pin$java,R=R.version.string,cases=length(cases),attempts=length(attempts),exact_thresholds=length(thresholds),
  scope='Public JD+ MSR selector choices/counters/final tables, with separately reconstructed public seven-term pilot/prefix tables. Exact threshold controls qualify scalar policy; whole-input near-boundary cases are offset by 1e-5. Both native policies remain independently qualified; the published cap is not a public JD+ selector option.')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
files<-file.path(output,c('summaries.csv','tables.csv','attempts.csv','pilots.csv','thresholds.csv','metadata.R'))
write.csv(data.frame(file=basename(files),sha256=reference_sha256(files)),file.path(output,'SHA256SUMS.csv'),row.names=FALSE)
cat('Captured',length(cases),'selector cases,',length(attempts),'public prefix tables and four exact scalar thresholds\n')
