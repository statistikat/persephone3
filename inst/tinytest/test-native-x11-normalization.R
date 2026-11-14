local({
  normalize<-getFromNamespace('native_x11_seasonal_normalize','persephone3')
  raw<-getFromNamespace('.native_x11_seasonal_normalize','persephone3')
  source('fixtures/native/x11-normalization-cases.R',local=TRUE)
  points<-read.csv('fixtures/native/x11-normalization/points.csv');targets<-split(points,points$case)
  close<-function(a,b,label) {
    a<-as.numeric(a);b<-as.numeric(b)
    expect_identical(is.na(a),is.na(b),info=label)
    at<-is.finite(a)&is.finite(b)
    expect_true(all(abs(a[at]-b[at])/pmax(1,abs(b[at]))<2e-12),info=label)
  }
  cases<-x11_normalization_cases();expect_identical(sort(names(cases)),sort(names(targets)))
  for(ca in cases) {
    y<-x11_normalization_input(ca);before<-serialize(y,NULL);a<-normalize(y,ca$filters,ca$extend)
    o<-x11_normalization_oracle(y,ca$filters,ca$extend)
    close(a$values,targets[[ca$id]]$value,paste(ca$id,'public'))
    close(a$values,o$values,paste(ca$id,'independent'))
    close(a$normalization,o$normalization,paste(ca$id,'normalizing operator'))
    expect_identical(a$source_rows,o$source_rows)
    expect_identical(a$normalization_source_rows,o$normalization_source_rows)
    expect_identical(unname(a$endpoint_reasons),o$reasons)
    expect_identical(unname(a$endpoint_policies),ifelse(o$reasons=='nearest_central_mean','nearest_central_mean','annual_copy'))
    expect_identical(unname(a$requested_filters),rep(ca$filters,length.out=ca$frequency))
    expect_identical(names(a$endpoint_reasons),as.character(seq_len(ca$frequency)))
    expect_identical(a$original_rows,ca$extend+seq_len(ca$n))
    expect_identical(a$available_rows,which(!is.na(a$values)))
    expect_identical(tsp(a$normalization),tsp(y))
    close(tsp(a$values),c(tsp(y)[1]-ca$extend/ca$frequency,tsp(y)[2]+ca$extend/ca$frequency,ca$frequency),'restored calendar')
    expect_identical(a$status,if(ca$shape=='all_missing')'no_available_outputs'else'available')
    expect_identical(serialize(y,NULL),before);expect_identical(unserialize(serialize(a,NULL)),a)
    expect_identical(as.numeric(a$values)[a$original_rows][a$source_rows],as.numeric(a$values))
    if(ca$extend) {
      b<-normalize(y,ca$filters)
      expect_identical(as.numeric(a$values)[a$original_rows],as.numeric(b$values))
      expect_identical(a$normalization,b$normalization)
    }
  }
  # Entire operators, not only coefficients read from the native implementation.
  for(f in c(4L,12L))for(n in c(2L*f,5L*f-1L,5L*f,19L*f,19L*f+1L)) {
    filters<-c('stable','s3x15',rep('s3x3',f-2L));extend<-f%/%2L
    y<-ts(rep(0,n),start=c(2000,2),frequency=f);o<-x11_normalization_operator(y,filters,extend)
    actual<-matrix(0,n+2L*extend,n);normalizer<-matrix(0,n,n)
    for(j in seq_len(n)) {
      y[]<-0;y[j]<-1;a<-normalize(y,filters,extend)
      actual[,j]<-a$values;normalizer[,j]<-a$normalization
    }
    close(actual,o$values,'complete normalization operator')
    close(normalizer,o$normalization,'complete annual-mean operator')
    close(rowSums(normalizer),rep(1,n),'preserves constant means')
    close(rowSums(actual),rep(0,n+2L*extend),'removes constant level')
    y[]<-3;a<-normalize(y,filters,extend);close(a$values,rep(0,length(a$values)),'constant control')
    # Each missing input propagates through precisely its nonzero operator taps.
    for(j in seq_len(n)) {
      y[]<-3;y[j]<-NA_real_;a<-normalize(y,filters,extend)
      expect_identical(as.vector(is.na(a$values)),o$values[,j]!=0)
      expect_identical(as.vector(is.na(a$normalization)),o$normalization[,j]!=0)
    }
  }
  for(f in c(4L,12L)) {
    y<-ts(10+sin(seq_len(10L*f+3L)*.37),start=c(2000,f),frequency=f)
    filters<-c('stable','s3x15',rep('s3x5',f-2L));a<-normalize(y,filters,f+3L)
    for(scale in c(-1,1e150,1e-150))close(normalize(y*scale,filters,f+3L)$values/scale,a$values,'additive units')
    close(normalize(y+128,filters,f+3L)$values,a$values,'level shift')
    named<-setNames(filters,as.character(seq_len(f)));expect_identical(normalize(y,rev(named),f+3L),a)
    expect_error(normalize(y,setNames(filters,rep('1',f))),'every calendar period')
    expect_error(normalize(y,setNames(filters,seq_len(f)+1L)),'every calendar period')
    # Normalization follows the requested fixed filter and its own span rule;
    # a short endpoint fallback in the separate processor is not substituted.
    short<-ts(seq_len(5*f),frequency=f)
    b<-normalize(short,'s3x9');expect_true(all(b$endpoint_policies=='nearest_central_mean'))
    expect_true(all(getFromNamespace('native_x11_seasonal_process','persephone3')(short,'s3x9')$selected_filters=='stable'))
    for(n in c(0L,1L,2L*f-1L))expect_error(raw(seq_len(n),f,1,'s3x3',0),'two annual cycles')
    # All missing inputs have no available output, including restored rows.
    expect_identical(normalize(y*NA_real_,filters,3L*f)$status,'no_available_outputs')
    for(extend in c(1L,f+1L,3L*f+2L)) {
      b<-normalize(y,filters,extend);o<-x11_normalization_oracle(y,filters,extend)
      close(b$values,o$values,'multi-year restoration');expect_identical(b$source_rows,o$source_rows)
    }
    big<-ts(rep(.Machine$double.xmax,10L*f),frequency=f);big[1]<--.Machine$double.xmax
    expect_error(normalize(big,'s3x3'),'non-finite')
  }
  prior<-Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  on.exit(if(is.na(prior))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=prior))
  y<-ts(10+sin(seq_len(100003L)*.37),start=c(2000,11),frequency=12)
  for(filters in list('s3x3',c('stable',rep('s3x3',11)))) {
    Sys.setenv(PERSEPHONE_NUM_THREADS=1);a<-normalize(y,filters,15000L)
    Sys.setenv(PERSEPHONE_NUM_THREADS=2);b<-normalize(y,filters,15000L)
    expect_identical(a,b)
  }
  for(x in list(NULL,TRUE,'1',factor(1),matrix(1),1+1i,Inf,-Inf))expect_error(raw(x,4,1,'s3x3',0))
  for(f in list(NULL,NA,NaN,Inf,-Inf,0,1,52,4.5,TRUE,'4',factor(4),c(4,12),matrix(4)))expect_error(raw(1:80,f,1,'s3x3',0))
  for(first in list(0,5,1.5,NA,Inf,TRUE,'1',matrix(1)))expect_error(raw(1:80,4,first,'s3x3',0))
  for(e in list(NULL,-1,1.5,NA,Inf,TRUE,'1',matrix(1),2^31-1,2^32))expect_error(raw(1:80,4,1,'s3x3',e))
  for(filters in list(NULL,character(),NA_character_,1,TRUE,matrix('stable'),'msr','x11default','S3X3',c('stable','s3x3'),rep('stable',5)))
    expect_error(raw(1:80,4,1,filters,0))
  for(mode in list(NULL,NA,'multiplicative','log_additive','pseudo_additive',1,c('additive','additive')))
    expect_error(normalize(ts(1:80,frequency=4),'s3x3',mode=mode),'additive mode only')
  expect_error(normalize(1:80,'s3x3'),'numeric ts')
  expect_true('x11_additive_normalization' %in% backend_capabilities('native')$primitives)
  expect_true(!backend_capabilities('native')$fit)
})
