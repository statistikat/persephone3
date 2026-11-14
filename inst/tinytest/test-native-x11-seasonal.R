local({
  spec <- getFromNamespace('native_x11_seasonal_spec','persephone3')
  filter <- getFromNamespace('native_x11_filter','persephone3')
  raw <- getFromNamespace('.native_x11_seasonal_filter','persephone3')
  source('fixtures/native/x11-symmetric-cases.R',local=TRUE)
  source('fixtures/native/x11-seasonal-cases.R',local=TRUE)
  weights <- read.csv('fixtures/native/x11-seasonal/weights.csv',stringsAsFactors=FALSE)
  points <- read.csv('fixtures/native/x11-seasonal/points.csv',stringsAsFactors=FALSE)
  for(ca in x11_seasonal_cases()) {
    y <- x11_seasonal_input(ca);actual <- filter(y,ca$type,endpoints='seasonal')
    w <- weights[weights$case==ca$id,];p <- points[points$case==ca$id,]
    expect_true(max(abs(as.numeric(actual$values[p$row])-p$value)/(1+abs(p$value)))<2e-12,info=ca$id)
    expect_identical(tsp(actual$values),tsp(y))
    expect_identical(actual$available_rows,seq_len(ca$n))
    expect_identical(actual$endpoint_policy,'seasonal_x11')
    expect_identical(actual$minimum_phase_length,max(5L,ca$N+2L))
    expect_identical(actual$endpoint_rows$row,actual$boundary_rows)
    expect_identical(actual$endpoint_rows$period,as.integer(cycle(y))[actual$boundary_rows])
    expect_identical(actual$phase_lengths,stats::setNames(tabulate(as.integer(cycle(y)),nbins=ca$frequency),as.character(seq_len(ca$frequency))))
    expect_true(all(actual$phase_lengths>=actual$minimum_phase_length))
    center <- filter(y,ca$type)
    expect_identical(actual$values[actual$central_rows],center$values[center$central_rows])
    expect_identical(actual$central_domain,center$central_domain)
    expect_identical(unserialize(serialize(actual,NULL)),actual)
    for(row in unique(w$row)) {
      side <- if(row<=actual$radius)'left'else'right'
      available <- if(side=='left')(row-1L)%/%ca$frequency else(ca$n-row)%/%ca$frequency
      definition <- spec(ca$type,ca$frequency,available,side)
      captured <- w[w$row==row,]
      expect_true(max(abs(unname(definition$weights)-captured$weight))<1e-14,info=paste(ca$id,row))
      expect_identical(row+definition$offsets,as.integer(captured$column))
      expect_identical(names(definition$weights),as.character(definition$offsets))
      expect_true(abs(sum(definition$weights)-1)<1e-14)
      expect_true(all(definition$weights>0))
      expect_identical(definition$available,available)
      expect_identical(definition$annual_radius,(ca$N+1L)%/%2L)
      reverse <- spec(ca$type,ca$frequency,available,if(side=='left')'right'else'left')
      expect_identical(unname(reverse$weights),rev(unname(definition$weights)))
      expect_identical(reverse$offsets,-rev(definition$offsets))
    }
    if(ca$N %in% c(3L,5L))expect_equal(as.numeric(actual$values),
      x11_seasonal_virtual_phases(as.numeric(y),ca$frequency,ca$N),tolerance=2e-12)
    periodic <- ts(rep(seq_len(ca$frequency),length.out=ca$n),start=ca$start,frequency=ca$frequency)
    expect_equal(as.numeric(filter(periodic,ca$type,endpoints='seasonal')$values),as.numeric(periodic),tolerance=1e-14)
    # Gaps in one calendar phase cannot influence another phase, including
    # incomplete years and both endpoint directions.
    rows <- seq.int(1L,ca$n,ca$frequency);missing <- y;missing[rows] <- NA_real_
    before <- serialize(missing,NULL);masked <- filter(missing,ca$type,endpoints='seasonal')
    expect_true(all(is.na(masked$values[rows])))
    expect_identical(masked$values[-rows],actual$values[-rows])
    expect_identical(serialize(missing,NULL),before)
  }
  # Published/derived rational controls independent of the captured fixtures.
  expect_equal(unname(spec('s3x1',12L,0L,'left')$weights),c(61,39)/100,tolerance=0)
  expect_equal(unname(spec('s3x3',12L,0L,'left')$weights),c(11,11,5)/27,tolerance=0)
  expect_equal(unname(spec('s3x5',12L,2L,'left')$weights),c(9,13,13,13,8,4)/60,tolerance=0)
  for(frequency in c(4L,12L))for(N in c(3L,5L)) {
    n <- frequency*(N+2L)+1L;type <- paste0('s3x',N)
    for(column in seq.int(1L,n,frequency)) {
      y <- ts(as.numeric(seq_len(n)==column),frequency=frequency)
      expect_equal(as.numeric(filter(y,type,endpoints='seasonal')$values),
        x11_seasonal_virtual_phases(as.numeric(y),frequency,N),tolerance=1e-14)
    }
    y <- ts(3+sin(seq_len(n)*.31),frequency=frequency);y[c(1,2,n-1L)] <- c(NA,NaN,NA)
    expect_equal(as.numeric(filter(y,type,endpoints='seasonal')$values),
      x11_seasonal_virtual_phases(as.numeric(y),frequency,N),tolerance=2e-12)
  }
  # Captured short S3X1 results are phase averages chosen by the public
  # processor. Native explicit endpoints reject this unimplemented selection.
  controls <- read.csv('fixtures/native/x11-seasonal/selection-controls.csv',stringsAsFactors=FALSE)
  for(id in unique(controls$case)) {
    control <- controls[controls$case==id,];f <- control$frequency[1]
    y <- x11_symmetric_input(f,nrow(control))
    expected <- ave(as.numeric(y),rep(seq_len(f),length.out=length(y)),FUN=mean)
    expect_true(max(abs(control$value-expected)/(1+abs(expected)))<2e-12)
    expect_error(filter(y,'s3x1',endpoints='seasonal'),'observations in every phase')
  }
  for(frequency in c(4L,12L))for(N in c(1L,3L,5L,9L)) {
    type <- paste0('s3x',N);minimum <- frequency*max(5L,N+2L)
    for(n in c(0L,minimum-1L))expect_error(raw(seq_len(n),type,frequency),'observations in every phase')
    minimum_result <- raw(seq_len(minimum),type,frequency)
    expect_true(!any(minimum_result$missing))
    expect_error(spec(type,frequency,(N+1L)%/%2L),'annual filter radius')
    expect_error(spec(type,frequency,-1L))
  }
  y <- x11_symmetric_input(12L)
  for(type in list('trend','henderson','s3x15','stable','automatic',NA_character_,matrix('s3x3'),c('s3x3','s3x5'),NULL,1))expect_error(spec(type,12L,0L))
  expect_error(filter(y,'s3x15',endpoints='seasonal'),'currently support')
  expect_error(filter(y,'trend',endpoints='seasonal'),'currently support')
  expect_error(filter(y,'henderson',13L,endpoints='seasonal'),'currently support')
  expect_error(filter(y,'s3x3',length=3L,endpoints='seasonal'),'only supplied for Henderson')
  expect_error(filter(y,'s3x3',endpoints='seasonal',ic_ratio=1),'requires Musgrave')
  for(side in list('both','r',NA_character_,1,matrix('right'),c('left','right')))expect_error(spec('s3x3',12L,0L,side))
  for(frequency in list(1,4.5,52,NA_real_,Inf,TRUE,'12',matrix(12)))expect_error(spec('s3x3',frequency,0L))
  for(q in list(1.5,NA_real_,Inf,TRUE,'1',matrix(1),c(0,1)))expect_error(spec('s3x3',12L,q))
  for(values in list(matrix(1:132),letters,TRUE,1+1i,factor(1:132),c(1:131,Inf)))expect_error(raw(values,'s3x9',12L))
  # Large owned output paths use actual workers; endpoint loops stay on caller.
  old <- Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  on.exit(if(is.na(old))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=old),add=TRUE)
  for(type in c('s3x1','s3x3','s3x5','s3x9')) {
    y <- x11_symmetric_input(12L,40003L);y[c(103,19001)] <- NA_real_
    Sys.setenv(PERSEPHONE_NUM_THREADS=1);serial <- filter(y,type,endpoints='seasonal')
    Sys.setenv(PERSEPHONE_NUM_THREADS=2);parallel <- filter(y,type,endpoints='seasonal')
    expect_identical(serial,parallel)
    for(multiplier in c(1e-100,-2,1e100))expect_equal(
      as.numeric(filter(y*multiplier,type,endpoints='seasonal')$values)/multiplier,
      as.numeric(serial$values),tolerance=2e-12)
    expect_equal(as.numeric(filter(ts(rev(y),frequency=12),type,endpoints='seasonal')$values),rev(as.numeric(serial$values)),tolerance=2e-12)
  }
})
