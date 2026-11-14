local({
  filter <- getFromNamespace('native_x11_filter','persephone3')
  raw <- getFromNamespace('.native_x11_mean_filter','persephone3')
  source('fixtures/native/x11-symmetric-cases.R',local=TRUE)
  source('fixtures/native/x11-mean-cases.R',local=TRUE)
  weights <- read.csv('fixtures/native/x11-mean/weights.csv',stringsAsFactors=FALSE)
  points <- read.csv('fixtures/native/x11-mean/points.csv',stringsAsFactors=FALSE)
  for(ca in x11_mean_cases()) {
    y <- x11_symmetric_input(ca$frequency,ca$n);h <- ca$frequency%/%2L
    A <- x11_mean_operator(ca$frequency,ca$n)
    result <- filter(y,'trend',endpoints='mean')
    p <- points[points$case==ca$id,];w <- weights[weights$case==ca$id,]
    expect_true(max(abs(as.numeric(result$values[p$row])-p$value)/(1+abs(p$value)))<2e-12,info=ca$id)
    expect_true(max(abs(A[cbind(w$row,w$column)]-w$weight))<1e-14,info=ca$id)
    # Include exact-zero coefficients when comparing each frozen operator row.
    frozen <- matrix(0,ca$n,ca$n);frozen[cbind(w$row,w$column)] <- w$weight
    expect_equal(A[unique(w$row),,drop=FALSE],frozen[unique(w$row),,drop=FALSE],tolerance=1e-14)
    expect_equal(as.numeric(result$values),as.numeric(A%*%y),tolerance=2e-12)
    expect_equal(rowSums(A),rep(1,ca$n),tolerance=1e-14)
    expect_equal(A,A[rev(seq_len(ca$n)),rev(seq_len(ca$n))],tolerance=1e-14)
    expect_equal(result$padding_means,c(left=mean(head(y,h)),right=mean(tail(y,h))),tolerance=1e-14)
    expect_identical(result$available_rows,seq_len(ca$n))
    expect_identical(result$endpoint_rows$row,result$boundary_rows)
    expect_identical(result$endpoint_rows$left_padding,pmax(0L,h+1L-result$boundary_rows))
    expect_identical(result$endpoint_rows$right_padding,pmax(0L,result$boundary_rows+h-ca$n))
    expect_identical(tsp(result$values),tsp(y))
    expect_identical(result$endpoint_policy,'mean')
    expect_identical(result$status,'available')
    expect_identical(unserialize(serialize(result,NULL)),result)
    symmetric <- filter(y,'trend')
    expect_identical(result$values[result$central_rows],symmetric$values[result$central_rows])
    expect_identical(result$central_domain,symmetric$central_domain)
    # All basis vectors qualify the native linear operator, including short
    # overlapping support that has no central rows at all.
    for(column in seq_len(ca$n)) {
      basis <- ts(as.numeric(seq_len(ca$n)==column),start=c(-10,ca$frequency),frequency=ca$frequency)
      expect_equal(as.numeric(filter(basis,'trend',endpoints='mean')$values),A[,column],tolerance=1e-14)
    }
    for(gaps in list(1L,ca$n,seq_len(h),unique(c(1L,ca$n%/%2L,ca$n)))) {
      incomplete <- y;incomplete[gaps] <- NA_real_;incomplete[gaps[1]] <- NaN
      before <- serialize(incomplete,NULL);actual <- filter(incomplete,'trend',endpoints='mean')
      target <- x11_mean_independent(as.numeric(incomplete),ca$frequency)
      expect_equal(as.numeric(actual$values),target,tolerance=2e-12)
      expect_identical(actual$missing,is.na(target))
      expect_identical(serialize(incomplete,NULL),before)
    }
    expect_equal(as.numeric(filter(y*0+3.5,'trend',endpoints='mean')$values),rep(3.5,ca$n),tolerance=1e-14)
  }
  closed <- filter(ts(1:5,frequency=4),'trend',endpoints='mean')
  expect_equal(as.numeric(closed$values),c(27/16,35/16,3,61/16,69/16),tolerance=1e-14)
  expect_equal(unname(closed$padding_means),c(1.5,4.5),tolerance=0)
  for(frequency in c(4L,12L)) {
    h <- frequency%/%2L
    for(n in seq_len(h)-1L)expect_error(raw(seq_len(n),frequency),'half-width')
    short <- filter(ts(seq_len(h),start=c(-100,3),frequency=frequency),'trend',endpoints='mean')
    expect_equal(as.numeric(short$values),rep((h+1)/2,h),tolerance=1e-14)
    expect_true(is.null(short$central_domain));expect_identical(short$central_rows,integer())
    expect_true(all(short$endpoint_rows$left_padding>0 & short$endpoint_rows$right_padding>0))
    expect_identical(short$boundary_rows,seq_len(h))
    # Scaled means avoid overflow of the unscaled sum, with cancelling extremes.
    balanced <- rep(c(.Machine$double.xmax,-.Machine$double.xmax),h/2L)
    stable <- raw(balanced,frequency)
    expect_equal(as.numeric(stable$padding_means),c(0,0),tolerance=0)
    # Fused multiply-add can leave a roundoff residual when cancelling values
    # near DBL_MAX. Bound it relative to input scale, not to the exact zero.
    expect_true(max(abs(stable$values)/.Machine$double.xmax)<1e-15)
    missing <- raw(rep(NA_real_,h),frequency)
    expect_true(all(missing$missing) && all(is.na(missing$padding_means)))
  }
  y <- x11_symmetric_input(12L)
  expect_error(filter(y,'henderson',13L,'mean'),'centered trend')
  expect_error(filter(y,'s3x5',endpoints='mean'),'centered trend')
  expect_error(filter(y,'trend',endpoints='mean',ic_ratio=1),'requires Musgrave')
  expect_error(filter(y,'trend',length=12L,endpoints='mean'),'only supplied for Henderson')
  for(values in list(matrix(1:13),letters,TRUE,1+1i,factor(1:13),c(1:12,Inf)))expect_error(raw(values,12L))
  for(frequency in list(1,4.5,52,NA_real_,NaN,Inf,TRUE,'12',matrix(12),c(4,12)))expect_error(raw(1:13,frequency))
  # Both modes must retain exactly the same central arithmetic with actual
  # workers, regardless of the additional virtual support and copy boundaries.
  old <- Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  on.exit(if(is.na(old))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=old),add=TRUE)
  for(frequency in c(4L,12L)) {
    y <- x11_symmetric_input(frequency,40000L);y[c(103,19001)] <- NA_real_
    Sys.setenv(PERSEPHONE_NUM_THREADS=1);serial <- filter(y,'trend',endpoints='mean')
    Sys.setenv(PERSEPHONE_NUM_THREADS=2);parallel <- filter(y,'trend',endpoints='mean')
    expect_identical(serial,parallel)
    symmetric <- filter(y,'trend')
    expect_identical(serial$values[serial$central_rows],symmetric$values[serial$central_rows])
    for(multiplier in c(1e-100,-2,1e100))expect_equal(
      as.numeric(filter(y*multiplier,'trend',endpoints='mean')$values)/multiplier,
      as.numeric(serial$values),tolerance=2e-12)
    expect_equal(as.numeric(filter(ts(rev(y),frequency=frequency),'trend',endpoints='mean')$values),rev(as.numeric(serial$values)),tolerance=2e-12)
  }
})
