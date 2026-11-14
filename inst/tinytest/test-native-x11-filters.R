local({
  spec <- getFromNamespace('native_x11_filter_spec','persephone3')
  filter <- getFromNamespace('native_x11_filter','persephone3')
  raw <- getFromNamespace('.native_x11_filter','persephone3')
  source('fixtures/native/x11-symmetric-cases.R',local=TRUE)
  weights <- read.csv('fixtures/native/x11-symmetric/weights.csv',stringsAsFactors=FALSE)
  points <- read.csv('fixtures/native/x11-symmetric/points.csv',stringsAsFactors=FALSE)
  for(ca in x11_symmetric_cases()) {
    definition <- spec(ca$type,ca$frequency,ca$length)
    actual <- filter(x11_symmetric_input(ca$frequency),ca$type,ca$length)
    w <- weights[weights$case==ca$id,];p <- points[points$case==ca$id,]
    expect_equal(unname(definition$weights),w$weight,tolerance=1e-14,info=ca$id)
    expect_identical(definition$offsets,as.integer(w$offset))
    expect_identical(definition$weights,rev(setNames(unname(definition$weights),rev(names(definition$weights)))))
    expect_equal(sum(definition$weights),1,tolerance=1e-14)
    expect_equal(sum(definition$weights*definition$offsets),0,tolerance=1e-12)
    expect_identical(names(definition$weights),as.character(definition$offsets))
    expect_identical(actual$weights,definition$weights)
    expect_identical(tsp(actual$values),tsp(x11_symmetric_input(ca$frequency)))
    expect_identical(actual$status,'available')
    expect_identical(actual$available_rows,actual$central_rows)
    expect_identical(range(actual$available_rows),as.integer(c(p$first_available[1],p$last_available[1])))
    expect_equal(as.numeric(actual$values[p$row]),p$value,tolerance=2e-12,info=ca$id)
    independent <- x11_filter_sum(x11_symmetric_input(ca$frequency),unname(definition$weights),definition$offsets)
    expect_equal(as.numeric(actual$values),independent,tolerance=2e-12)
    expect_identical(actual$missing,is.na(independent))
    expect_identical(actual$boundary_rows,which(actual$missing))
    expect_identical(unserialize(serialize(actual,NULL)),actual)
    if(ca$type=='henderson') {
      j <- definition$offsets/max(1,definition$radius)
      expect_true(abs(sum(definition$weights*j^2))<1e-14)
      expect_true(abs(sum(definition$weights*j^3))<1e-14)
      if(ca$length<=23L)expect_equal(unname(definition$weights),henderson_minimum_roughness(ca$length),tolerance=1e-9)
      # Filter a whole cubic, not only the coefficient moment identities.
      t <- seq(-1,1,length.out=257);y <- ts(2+3*t-4*t^2+.2*t^3,start=c(-1,3),frequency=ca$frequency)
      cubic <- filter(y,ca$type,ca$length)
      expect_equal(as.numeric(cubic$values[cubic$central_rows]),as.numeric(y[cubic$central_rows]),tolerance=2e-12)
    } else if(startsWith(ca$type,'s3x')) {
      N <- as.integer(sub('s3x','',ca$type))
      expect_equal(unname(definition$weights),as.numeric(stats::convolve(rep(1/3,3),rep(1/N,N),type='open')),tolerance=1e-14)
      periodic <- ts(rep(seq_len(ca$frequency),length.out=257),frequency=ca$frequency)
      kept <- filter(periodic,ca$type)
      expect_equal(as.numeric(kept$values[kept$central_rows]),as.numeric(periodic[kept$central_rows]),tolerance=1e-12)
    } else {
      expect_equal(unname(definition$weights),c(.5,rep(1,ca$frequency-1L),.5)/ca$frequency,tolerance=1e-14)
      for(k in seq_len(ca$frequency%/%2L)) {
        cycle <- ts(cos(2*pi*k*seq_len(257)/ca$frequency),frequency=ca$frequency)
        removed <- filter(cycle,'trend')
        expect_true(max(abs(removed$values[removed$central_rows]))<1e-12)
      }
    }
  }
  expect_identical(spec('henderson',4L,3L)$weights,spec('henderson',4L,1L)$weights)
  y <- ts(seq_len(257),start=c(2000,3),frequency=12)
  for(type in c('trend','s3x5','henderson')) {
    length <- if(type=='henderson')13L else NULL
    incomplete <- y;incomplete[c(1,13,60,105,257)] <- c(NA,NaN,NA,NA,NA)
    before <- serialize(incomplete,NULL);actual <- filter(incomplete,type,length)
    target <- x11_filter_sum(incomplete,unname(actual$weights),actual$offsets)
    expect_equal(as.numeric(actual$values),target,tolerance=1e-12)
    expect_identical(actual$missing,is.na(target))
    expect_identical(serialize(incomplete,NULL),before)
    expect_true(all(actual$available_rows %in% actual$central_rows))
  }
  zero <- filter(y,'henderson',13L);y[105] <- NA_real_
  expect_equal(filter(y,'henderson',13L)$values[101],zero$values[101],tolerance=0)
  expect_true(is.na(filter(y,'henderson',13L)$values[105]))
  short <- filter(ts(1:4,frequency=12),'s3x15')
  expect_identical(short$status,'no_available_outputs')
  expect_true(all(short$missing) && is.null(short$central_domain))
  expect_identical(short$central_rows,integer())
  empty <- raw(numeric(),'trend',4L)
  expect_identical(empty$values,numeric());expect_identical(empty$missing,logical())
  for(type in list(NULL,NA_character_,c('trend','s3x3'),matrix('trend'),1,'stable','automatic','S3X5'))expect_error(spec(type,12L))
  for(frequency in list(NULL,NA_real_,c(4,12),matrix(4),4.5,Inf,-4,52,'4',factor(4)))expect_error(spec('trend',frequency))
  for(length in list(NULL,0,2,102,-3,NA_real_,Inf,c(5,7),matrix(5),'5',factor(5)))expect_error(spec('henderson',12L,length))
  expect_error(spec('s3x5',12L,5L));expect_error(spec('trend',12L,12L))
  for(series in list(1:12,matrix(1:12),ts(matrix(1:24,12,2),frequency=12),ts(1:12,frequency=1),ts(c(1,Inf),frequency=4)))
    expect_error(filter(series,'trend'))
  for(values in list(matrix(1:12),letters,factor(1:12),1+1i,TRUE,c(1,Inf)))expect_error(raw(values,'trend',4L))
  # Independent active-tap missingness is checked before overflow arithmetic.
  expect_true(all(is.na(raw(c(.Machine$double.xmax,NA_real_),'trend',4L)$values)))
  weights <- unname(spec('henderson',12L,13L)$weights)
  expect_error(raw(sign(weights)*.Machine$double.xmax,'henderson',12L,13L),'non-finite')
  expect_equal(as.numeric(filter(ts(1:20,frequency=4),'henderson',3L)$values),1:20,tolerance=0)
  # Actual worker path, with complete and missing input; deterministic arithmetic.
  old <- Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  on.exit(if(is.na(old))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=old),add=TRUE)
  y <- x11_symmetric_input(12L,40000L);y[c(103,19001)] <- NA_real_
  for(type in c('henderson','s3x15')) {
    length <- if(type=='henderson')101L else NULL
    Sys.setenv(PERSEPHONE_NUM_THREADS=1);serial <- filter(y,type,length)
    Sys.setenv(PERSEPHONE_NUM_THREADS=2);parallel <- filter(y,type,length)
    expect_identical(serial,parallel)
    for(multiplier in c(1e-100,-2,1e100)) {
      changed <- filter(y*multiplier,type,length)
      expect_equal(as.numeric(changed$values)/multiplier,as.numeric(serial$values),tolerance=2e-12)
    }
  }
})
