local({
  spec <- getFromNamespace('native_x11_seasonal_spec','persephone3')
  filter <- getFromNamespace('native_x11_filter','persephone3')
  raw <- getFromNamespace('.native_x11_seasonal_filter','persephone3')
  source('fixtures/native/x11-symmetric-cases.R',local=TRUE)
  source('fixtures/native/x11-seasonal15-cases.R',local=TRUE)
  weights <- read.csv('fixtures/native/x11-seasonal15/weights.csv',stringsAsFactors=FALSE)
  points <- read.csv('fixtures/native/x11-seasonal15/points.csv',stringsAsFactors=FALSE)
  for(ca in x11_seasonal15_cases()) {
    y <- x11_seasonal15_input(ca);actual <- filter(y,'s3x15',endpoints='seasonal')
    w <- weights[weights$case==ca$id,];p <- points[points$case==ca$id,]
    expect_true(max(abs(as.numeric(actual$values[p$row])-p$value)/(1+abs(p$value)))<2e-12,info=ca$id)
    expect_equal(as.numeric(actual$values),x11_seasonal15_phases(as.numeric(y),ca$frequency),tolerance=2e-12)
    expect_identical(tsp(actual$values),tsp(y))
    expect_identical(actual$available_rows,seq_len(ca$n))
    expect_identical(actual$minimum_phase_length,20L)
    expect_identical(actual$coefficient_convention,'x11_decimal_5')
    expect_identical(actual$endpoint_rows$row,actual$boundary_rows)
    expect_identical(actual$endpoint_rows$period,as.integer(cycle(y))[actual$boundary_rows])
    expect_identical(actual$phase_lengths,stats::setNames(tabulate(as.integer(cycle(y)),nbins=ca$frequency),as.character(seq_len(ca$frequency))))
    expect_true(all(actual$phase_lengths>=20L))
    center <- filter(y,'s3x15')
    expect_identical(actual$values[actual$central_rows],center$values[center$central_rows])
    expect_identical(actual$central_domain,center$central_domain)
    expect_identical(unserialize(serialize(actual,NULL)),actual)
    A <- x11_seasonal15_operator(length(seq.int(1L,ca$n,ca$frequency)))
    for(row in unique(w$row)) {
      side <- if(row<=actual$radius)'left'else'right'
      q <- if(side=='left')(row-1L)%/%ca$frequency else(ca$n-row)%/%ca$frequency
      definition <- spec('s3x15',ca$frequency,q,side);captured <- w[w$row==row,]
      expect_true(max(abs(unname(definition$weights)-captured$weight))<1e-14,info=paste(ca$id,row))
      expect_identical(row+definition$offsets,as.integer(captured$column))
      expect_identical(definition$coefficient_convention,'x11_decimal_5')
      expect_identical(definition$minimum_phase_length,20L)
      expect_identical(definition$available,q)
      expect_identical(definition$annual_radius,8L)
      expect_true(abs(sum(definition$weights)-1)<1e-14)
      expect_true(all(definition$weights>0))
      expect_true(max(abs(1e5*definition$weights-round(1e5*definition$weights)))<5e-12)
      annual_row <- (row-1L)%/%ca$frequency+1L
      # Coefficient qualification uses an absolute budget, including the small
      # decimal-closure tap; all.equal's relative rescaling is inappropriate.
      expect_true(max(abs(unname(definition$weights)-A[annual_row,A[annual_row,]!=0]))<1e-14)
      reverse <- spec('s3x15',ca$frequency,q,if(side=='left')'right'else'left')
      expect_identical(unname(reverse$weights),rev(unname(definition$weights)))
      expect_identical(reverse$offsets,-rev(definition$offsets))
    }
    periodic <- ts(rep(seq_len(ca$frequency),length.out=ca$n),start=ca$start,frequency=ca$frequency)
    expect_equal(as.numeric(filter(periodic,'s3x15',endpoints='seasonal')$values),as.numeric(periodic),tolerance=1e-14)
    phase <- seq.int(1L,ca$n,ca$frequency);missing <- y;missing[phase] <- NA_real_
    before <- serialize(missing,NULL);masked <- filter(missing,'s3x15',endpoints='seasonal')
    expect_true(all(is.na(masked$values[phase])))
    expect_identical(masked$values[-phase],actual$values[-phase])
    expect_identical(serialize(missing,NULL),before)
  }
  expect_equal(unname(spec('s3x15',12L,0L,'left')$weights),c(rep(16000,5),6667,6667,4444,2222)/1e5,tolerance=0)
  expect_identical(tail(unname(spec('s3x15',12L,4L,'left')$weights),1),2219/1e5)
  # Decimal closure is a declared method convention, distinct from applying
  # the unquantized virtual-extension operator. Do not silently substitute it.
  for(frequency in c(4L,12L)) {
    n<-20L*frequency+1L
    for(column in seq.int(1L,n,frequency)) {
      y <- ts(as.numeric(seq_len(n)==column),frequency=frequency)
      expect_equal(as.numeric(filter(y,'s3x15',endpoints='seasonal')$values),
        x11_seasonal15_phases(as.numeric(y),frequency),tolerance=1e-14)
    }
    y <- x11_symmetric_input(frequency,n)
    quantized <- x11_seasonal15_phases(as.numeric(y),frequency)
    unquantized <- x11_seasonal15_phases(as.numeric(y),frequency,FALSE)
    expect_true(max(abs(quantized-unquantized))>1e-6)
    y[c(1,frequency+2L,n-1L)] <- c(NA,NaN,NA)
    expect_equal(as.numeric(filter(y,'s3x15',endpoints='seasonal')$values),
      x11_seasonal15_phases(as.numeric(y),frequency),tolerance=2e-12)
    for(nshort in c(0L,17L*frequency,20L*frequency-1L))
      expect_error(raw(seq_len(nshort),'s3x15',frequency),'at least 20 observations in every phase')
    expect_true(!any(raw(seq_len(20L*frequency),'s3x15',frequency)$missing))
    expect_error(spec('s3x15',frequency,8L),'annual filter radius')
  }
  controls <- read.csv('fixtures/native/x11-seasonal15/selection-controls.csv',stringsAsFactors=FALSE)
  for(id in unique(controls$case)) {
    control <- controls[controls$case==id,];f <- control$frequency[1];n <- nrow(control)
    y <- x11_symmetric_input(f,n)
    expected <- if(ceiling(n/f)<20L)ave(as.numeric(y),rep(seq_len(f),length.out=n),FUN=mean)else
      x11_seasonal15_phases(as.numeric(y),f)
    expect_true(max(abs(control$value-expected)/(1+abs(expected)))<2e-12,info=id)
    expect_error(filter(y,'s3x15',endpoints='seasonal'),'at least 20 observations in every phase')
  }
  # Convention metadata makes the existing rational/published choices explicit;
  # their unchanged frozen values remain covered by the earlier seasonal suite.
  for(type in c('s3x1','s3x3','s3x5','s3x9')) {
    expected <- if(type %in% c('s3x3','s3x5'))'virtual_values_rational'else'published_fixed_weights'
    expect_identical(spec(type,12L,0L)$coefficient_convention,expected)
    expect_identical(filter(x11_symmetric_input(12L),type,endpoints='seasonal')$coefficient_convention,expected)
  }
  old <- Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  on.exit(if(is.na(old))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=old),add=TRUE)
  y <- x11_symmetric_input(12L,40003L);y[c(103,19001)] <- NA_real_
  Sys.setenv(PERSEPHONE_NUM_THREADS=1);serial <- filter(y,'s3x15',endpoints='seasonal')
  Sys.setenv(PERSEPHONE_NUM_THREADS=2);parallel <- filter(y,'s3x15',endpoints='seasonal')
  expect_identical(serial,parallel)
  for(multiplier in c(1e-100,-2,1e100))expect_equal(
    as.numeric(filter(y*multiplier,'s3x15',endpoints='seasonal')$values)/multiplier,
    as.numeric(serial$values),tolerance=2e-12)
  expect_equal(as.numeric(filter(ts(rev(y),frequency=12),'s3x15',endpoints='seasonal')$values),rev(as.numeric(serial$values)),tolerance=2e-12)
})
