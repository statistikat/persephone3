# Reviewable method-policy evidence, not native selector qualification.
args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[startsWith(args,'--file=')][[1L]])
tooling<-dirname(normalizePath(script));source(file.path(tooling,'common.R'));source(file.path(tooling,'extended-fixtures.R'))
source(file.path(tooling,'x11-msr-reference.R'));root<-reference_root();base<-reference_fixture_dir(root)
source(file.path(root,'inst/tinytest/fixtures/native/x11-msr-cases.R'))
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !startsWith(args,'--output='))stop('Usage: probe-x11-msr-iterations.R --output=<empty directory>')
output<-normalizePath(sub('^--output=','',args),mustWork=TRUE)
if(!dir.exists(output) || length(list.files(output,all.files=TRUE,no..=TRUE)) || startsWith(output,normalizePath(base)))stop('Output must be empty and outside original fixtures')
before<-extended_parent_hashes(base);pin<-extended_assert_oracle(base)
make<-function(amplitude)list(mode='additive',y=ts(rep(seq_len(20)+amplitude*sin(seq_len(20)*1.37),each=4),start=c(2000,1),frequency=4))
ratio<-function(ca,iteration)x11_msr_oracle(x11_msr_mode_components(ca,iteration),'additive')$global
# Derive the input from an independent full-span target, without consulting JD+.
amplitude<-uniroot(function(a)ratio(make(a),1)-2.6,c(0,30),tol=1e-13)$root
ca<-make(amplitude);public<-reference_x11_msr_mode(ca$y,'additive')
trace<-data.frame(years_removed=0:15,years_remaining=20:5,
  independent_msr=vapply(1:16,function(j)ratio(ca,j),numeric(1)))
# These controls stay away from exact boundaries; their equality convention is
# deliberately not inferred or qualified by this probe.
trace$decisive_filter<-vapply(trace$independent_msr,function(x)
  if(x<2.5)'S3X3'else if(x>3.5 && x<5.5)'S3X5'else if(x>6.5)'S3X9'else NA_character_,character(1))
published<-trace$decisive_filter[seq_len(6)];published<-published[!is.na(published)]
published_choice<-if(length(published))published[1]else'S3X5'
asserted<-x11_msr_oracle(x11_msr_mode_components(ca,public$iterations),'additive')
error<-max(abs(asserted$phases$seasonal-public$seasonal),abs(asserted$phases$irregular-public$irregular),abs(asserted$global-public$global))
stopifnot(error<2e-12,public$iterations==16L,public$choice=='S3X9',!public$using_default,published_choice=='S3X5',
  identical(before,extended_parent_hashes(base)),!'persephone3' %in% loadedNamespaces())
write.csv(trace,file.path(output,'trace.csv'),row.names=FALSE)
write.csv(data.frame(value=as.numeric(ca$y)),file.path(output,'input.csv'),row.names=FALSE)
files<-c(normalizePath(script),file.path(tooling,'x11-msr-reference.R'),file.path(root,'inst/tinytest/fixtures/native/x11-msr-cases.R'))
metadata<-list(schema_version=1L,captured_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  oracle_manifest_sha256=reference_sha256(file.path(base,'oracle-manifest.R')),source_sha256=setNames(reference_sha256(files),basename(files)),
  java=pin$java,amplitude=amplitude,frequency=4L,start=c(2000L,1L),mode='additive',public=public,
  independent_public_maximum_error=error,published_maximum_years_removed=5L,published_choice=published_choice,
  published_source='Ladiray and Quenneville (2000), section 4.3.10, page 152: maximum five years removed, then default 3x5.',
  source_url='https://www.census.gov/content/dam/Census/library/working-papers/2000/adrm/x11-french.pdf',
  scope='One independent 20-year method-policy control. Public behavior and published cap select different filters. Does not establish statistical superiority, threshold equalities or native selector qualification.')
writeLines(sub('[[:blank:]]+$','',capture.output(dput(metadata))),file.path(output,'metadata.R'))
cat('PASS: independent final-table error',error,'; JDemetra',public$choice,'after',public$iterations-1L,'years removed; published cap',published_choice,'\n')
