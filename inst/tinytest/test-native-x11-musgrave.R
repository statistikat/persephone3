local({
  spec <- getFromNamespace('native_x11_musgrave_spec','persephone3')
  symmetric <- getFromNamespace('native_x11_filter_spec','persephone3')
  filter <- getFromNamespace('native_x11_filter','persephone3')
  raw <- getFromNamespace('.native_x11_musgrave_filter','persephone3')
  raw_spec <- getFromNamespace('.native_x11_musgrave_spec','persephone3')
  source('fixtures/native/x11-symmetric-cases.R',local=TRUE)
  source('fixtures/native/x11-musgrave-cases.R',local=TRUE)
  weights <- read.csv('fixtures/native/x11-musgrave/weights.csv',stringsAsFactors=FALSE)
  points <- read.csv('fixtures/native/x11-musgrave/points.csv',stringsAsFactors=FALSE)
  for(ca in x11_musgrave_cases()) {
    right <- spec(ca$frequency,ca$length,ca$available,ca$ic_ratio)
    left <- spec(ca$frequency,ca$length,ca$available,ca$ic_ratio,'left')
    w <- weights[weights$case==ca$id,];p <- points[points$case==ca$id,]
    expect_true(max(abs(unname(right$weights)-w$weight))<1e-14,info=ca$id)
    expect_identical(right$offsets,as.integer(w$offset))
    expect_identical(left$offsets,-rev(right$offsets))
    expect_identical(unname(left$weights),rev(unname(right$weights)))
    expect_true(abs(sum(right$weights)-1)<1e-14)
    expect_identical(names(right$weights),as.character(right$offsets))
    full <- symmetric('henderson',ca$frequency,ca$length)
    independent <- x11_musgrave_minimum(unname(full$weights),ca$available,ca$ic_ratio)
    expect_true(max(abs(independent-unname(right$weights)))<2e-9,info=paste(ca$id,'constrained minimum'))
    y <- x11_symmetric_input(ca$frequency)
    result <- filter(y,'henderson',ca$length,endpoints='musgrave',ic_ratio=ca$ic_ratio)
    expect_true(max(abs(as.numeric(result$values[p$row])-p$value)/(1+abs(p$value)))<2e-12,info=ca$id)
    expect_identical(tsp(result$values),tsp(y))
    expect_identical(result$available_rows,seq_along(y))
    expect_identical(result$ic_ratio,ca$ic_ratio)
    expect_identical(result$endpoint_policy,'musgrave')
    expect_identical(result$weights,full$weights)
    expect_identical(result$endpoint_rows$row,result$boundary_rows)
    expect_identical(result$endpoint_rows$available,c(seq_len(full$radius)-1L,rev(seq_len(full$radius)-1L)))
    center <- filter(y,'henderson',ca$length)
    expect_identical(result$values[result$central_rows],center$values[result$central_rows])
  }
  # Endpoint application uses a direct weighted dot product independent of the
  # native mask pass. Include gaps in each boundary and in central support.
  for(length in c(5L,13L,101L))for(ratio in c(0,.001,2.7,.Machine$double.xmax)) {
    full <- symmetric('henderson',12L,length);h <- full$radius
    bank <- lapply(seq_len(h)-1L,function(q)unname(spec(12L,length,q,ratio)$weights))
    y <- x11_symmetric_input(12L);y[c(1,5,64,143,256)] <- c(NA,NaN,NA,NaN,NA)
    before <- serialize(y,NULL)
    actual <- filter(y,'henderson',length,'musgrave',ratio)
    target <- x11_musgrave_direct(as.numeric(y),unname(full$weights),bank)
    expect_equal(as.numeric(actual$values),target,tolerance=2e-12)
    expect_identical(actual$missing,is.na(target))
    expect_identical(serialize(y,NULL),before)
    expect_identical(unserialize(serialize(actual,NULL)),actual)
    expect_identical(actual$central_domain,filter(y,'henderson',length)$central_domain)
  }
  # At I/C=0 the lexicographic limit reproduces a line and minimizes weight
  # distance among slope-exact filters. The public zero-ratio NaNs are not used.
  expect_equal(unname(spec(12L,5L,0L,0)$weights),c(-105/572,105/286,467/572),tolerance=1e-14)
  for(length in seq.int(5L,101L,2L)) {
    h <- length%/%2L;full <- symmetric('henderson',4L,length)
    for(q in unique(c(0L,h%/%2L,h-1L))) {
      zero <- spec(4L,length,q,0)
      expect_true(abs(sum(zero$weights*zero$offsets))<2e-13)
      expect_equal(unname(zero$weights),x11_musgrave_minimum(unname(full$weights),q,0),tolerance=2e-13)
      expect_identical(spec(4L,length,q,1e-200)$weights,zero$weights)
      limit <- spec(4L,length,q,.Machine$double.xmax)
      m <- length(limit$weights)
      expect_equal(unname(limit$weights),unname(full$weights[seq_len(m)])+sum(full$weights[-seq_len(m)])/m,tolerance=1e-14)
    }
    y <- ts(seq_len(length),start=c(-5,3),frequency=4)
    applied <- filter(y,'henderson',length,'musgrave',0)
    expect_equal(as.numeric(applied$values),as.numeric(y),tolerance=2e-12)
    expect_identical(tsp(applied$values),tsp(y))
    expect_identical(applied$central_rows,h+1L)
    constant <- filter(y*0+4,'henderson',length,'musgrave',7)
    expect_equal(as.numeric(constant$values),rep(4,length),tolerance=2e-12)
    expect_error(filter(head(y,-1),'henderson',length,'musgrave',1),'effective Henderson length')
  }
  y <- x11_symmetric_input(12L,41L)
  for(length in c(1L,3L)) {
    result <- filter(y,'henderson',length,'musgrave',0)
    expect_identical(as.numeric(result$values),as.numeric(y))
    expect_identical(nrow(result$endpoint_rows),0L)
    expect_error(spec(12L,length,0L,1),'effective Henderson radius')
  }
  # Polynomial curvature is not reproduced by an endpoint filter; changing the
  # ratio changes endpoint weights but never the central Henderson filter.
  a <- filter(y,'henderson',13L,'musgrave',.5)
  b <- filter(y,'henderson',13L,'musgrave',8)
  expect_true(max(abs(a$values[a$boundary_rows]-b$values[b$boundary_rows]))>1e-4)
  expect_identical(a$values[a$central_rows],b$values[b$central_rows])
  expect_equal(as.numeric(filter(ts(rev(y),frequency=12),'henderson',13L,'musgrave',.5)$values),rev(as.numeric(a$values)),tolerance=2e-12)
  # All invalid selections fail at the boundary, without modifying the input.
  for(ratio in list(NULL,NA_real_,Inf,-1,'1',TRUE,1+1i,factor(1),matrix(1),c(1,2))) {
    expect_error(spec(12L,13L,0L,ratio));expect_error(filter(y,'henderson',13L,'musgrave',ratio))
  }
  for(q in list(-1,6,1.5,Inf,NA_real_,NULL,'1',TRUE,matrix(1),c(1,2)))expect_error(spec(12L,13L,q,1))
  for(side in list('both','r',NULL,NA_character_,matrix('right'),c('left','right'),1))expect_error(spec(12L,13L,0L,1,side))
  for(endpoint in list('auto','m',NULL,NA_character_,TRUE,matrix('musgrave'),c('none','musgrave')))expect_error(filter(y,'henderson',13L,endpoint,1))
  expect_error(filter(y,'trend',endpoints='musgrave',ic_ratio=1),'require Henderson')
  expect_error(filter(y,'s3x5',endpoints='musgrave',ic_ratio=1),'require Henderson')
  expect_error(filter(y,'henderson',13L,ic_ratio=1),'requires Musgrave')
  for(frequency in list(1,4.5,52,NA_real_,TRUE,'12',matrix(12)))expect_error(raw_spec(frequency,13L,0L,1,'right'))
  for(length in list(0,2,103,NA_real_,TRUE,'13',matrix(13)))expect_error(raw_spec(12L,length,0L,1,'right'))
  for(values in list(numeric(),matrix(1:13),letters,TRUE,1+1i,factor(1:13),c(1:12,Inf)))expect_error(raw(values,12L,13L,1))
  # Missingness takes precedence over arithmetic. A constant response near the
  # range limit can still overflow an intermediate sum and fails explicitly.
  expect_true(all(is.na(raw(rep(NA_real_,13L),12L,13L,1)$values)))
  # Every active endpoint window here contains a missing observation.
  expect_true(all(is.na(raw(c(NA_real_,rep(.Machine$double.xmax,3),NA_real_),12L,5L,1)$values)))
  expect_error(raw(rep(.Machine$double.xmax,13L),12L,13L,1),'non-finite')
  # Actual workers, units, missing masks and full-calendar metadata.
  old <- Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  on.exit(if(is.na(old))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=old),add=TRUE)
  y <- x11_symmetric_input(12L,40000L);y[c(100,19001)] <- NA_real_
  Sys.setenv(PERSEPHONE_NUM_THREADS=1);serial <- filter(y,'henderson',101L,'musgrave',3.5)
  Sys.setenv(PERSEPHONE_NUM_THREADS=2);parallel <- filter(y,'henderson',101L,'musgrave',3.5)
  expect_identical(serial,parallel)
  for(multiplier in c(1e-100,-2,1e100))expect_equal(
    as.numeric(filter(y*multiplier,'henderson',101L,'musgrave',3.5)$values)/multiplier,
    as.numeric(serial$values),tolerance=2e-12)
})
