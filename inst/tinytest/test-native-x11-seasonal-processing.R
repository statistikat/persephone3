local({
  process<-getFromNamespace('native_x11_seasonal_process','persephone3')
  raw<-getFromNamespace('.native_x11_seasonal_process','persephone3')
  explicit<-getFromNamespace('native_x11_filter','persephone3')
  for(name in c('x11-symmetric-cases.R','x11-seasonal-cases.R','x11-seasonal15-cases.R','x11-seasonal-processing-cases.R'))
    source(file.path('fixtures/native',name),local=TRUE)
  w<-read.csv('fixtures/native/x11-seasonal-processing/weights.csv',stringsAsFactors=FALSE)
  p<-read.csv('fixtures/native/x11-seasonal-processing/points.csv',stringsAsFactors=FALSE)
  weights<-split(w,w$case);points<-split(p,p$case)
  for(ca in x11_processing_cases()) {
    y<-x11_processing_input(ca);actual<-process(y,ca$filters)
    oracle<-x11_processing_oracle(as.numeric(y),ca$frequency,ca$filters,ca$start[2])
    point<-points[[ca$id]];weight<-weights[[ca$id]]
    expect_true(max(abs(as.numeric(actual$values[point$row])-point$value)/(1+abs(point$value)))<2e-12,info=ca$id)
    expect_equal(as.numeric(actual$values),oracle$values,tolerance=2e-12,info=ca$id)
    expect_identical(tsp(actual$values),tsp(y))
    expect_identical(actual$available_rows,seq_len(ca$n))
    expect_identical(actual$requested_filters,stats::setNames(ca$filters,as.character(seq_len(ca$frequency))))
    expect_identical(actual$phase_lengths,stats::setNames(tabulate(as.integer(cycle(y)),nbins=ca$frequency),as.character(seq_len(ca$frequency))))
    expect_identical(actual$row_methods$row,seq_len(ca$n))
    expect_identical(actual$row_methods$period,as.integer(cycle(y)))
    expect_identical(unserialize(serialize(actual,NULL)),actual)
    expect_true(all(actual$selected_filters %in% c(ca$filters,'stable')))
    asymmetric<-actual$row_methods$method %in% c('left_endpoint','right_endpoint')
    expect_identical(!is.na(actual$row_methods$available),asymmetric)
    expect_true(all(actual$row_methods$available[asymmetric]>=0L))
    phase<-seq.int(1L,ca$n,ca$frequency);A<-oracle$operators[[ca$start[2L]]]
    target<-matrix(0,length(phase),length(phase))
    rows<-(weight$row-1L)%/%ca$frequency+1L;columns<-(weight$column-1L)%/%ca$frequency+1L
    target[cbind(rows,columns)]<-weight$weight
    expect_true(max(abs(A[unique(rows),,drop=FALSE]-target[unique(rows),,drop=FALSE]))<1e-14,info=ca$id)
    expect_true(max(abs(rowSums(A)-1))<1e-14)
    # Full basis application qualifies native support and zero cross-phase taps,
    # including overlap rows that use a complete phase mean.
    for(j in seq_along(phase)) {
      impulse<-ts(as.numeric(seq_len(ca$n)==phase[j]),start=ca$start,frequency=ca$frequency)
      response<-process(impulse,ca$filters)$values
      expect_true(max(abs(as.numeric(response[phase])-A[,j]))<1e-14)
      expect_true(all(response[-phase]==0))
    }
    # The mask must follow the selected support, not an imputation or omission.
    missing<-y;missing[phase[c(1L,length(phase))]]<-NA_real_
    before<-serialize(missing,NULL);masked<-process(missing,ca$filters)
    expected<-x11_processing_oracle(as.numeric(missing),ca$frequency,ca$filters,ca$start[2])$values
    expect_identical(is.na(as.numeric(masked$values)),is.na(expected))
    expect_equal(as.numeric(masked$values),expected,tolerance=2e-12)
    expect_identical(masked$values[-phase],actual$values[-phase])
    expect_identical(serialize(missing,NULL),before)
    if(ca$input_group=='uniform' && ca$filters[1]!='stable') {
      N<-as.integer(sub('s3x','',ca$filters[1],fixed=TRUE));minimum<-if(N==15L)20L else max(5L,N+2L)
      if(ca$n%/%ca$frequency>=minimum) {
        old<-explicit(y,ca$filters[1],endpoints='seasonal')
        expect_identical(actual$values,old$values)
        expect_identical(actual$missing,old$missing)
        expect_identical(process(y,ca$filters,short_series='reject')$values,old$values)
      } else expect_error(process(y,ca$filters,short_series='reject'),'minimum phase support')
    }
  }
  # Closed selector/middle-row controls; full calendar periods index choices.
  for(f in c(4L,12L)) {
    y<-ts(seq_len(5L*f),frequency=f);short<-process(y,'s3x5')
    expect_identical(which(short$row_methods$method=='short_phase_mean'),2L*f+seq_len(f))
    expect_equal(as.numeric(short$values[2L*f+seq_len(f)]),as.numeric(2L*f+seq_len(f)),tolerance=1e-14)
    ninth<-process(y,'s3x9')
    expect_true(all(ninth$selected_filters=='stable'))
    expect_true(all(ninth$selection_reasons=='insufficient_endpoint_support'))
    sixth<-process(ts(seq_len(5L*f+1L),frequency=f),'s3x9')
    expect_identical(unname(sixth$selected_filters),c('s3x9',rep('stable',f-1L)))
    expect_identical(which(sixth$row_methods$method %in% c('left_endpoint','right_endpoint')),c(1L,5L*f+1L))
    fourth<-process(ts(seq_len(5L*f-1L),frequency=f),'s3x1')
    expect_true(all(fourth$selection_reasons=='fewer_than_five_complete_years'))
    before20<-process(ts(seq_len(19L*f),frequency=f),'s3x15')
    expect_true(all(before20$selection_reasons=='s3x15_span_below_twenty_years'))
    partial20<-process(ts(seq_len(19L*f+1L),frequency=f),'s3x15')
    expect_true(all(partial20$selected_filters=='s3x15'))
    expect_true(all(partial20$coefficient_conventions=='x11_decimal_5'))
    expect_error(process(ts(seq_len(19L*f+1L),frequency=f),'s3x15','reject'),'minimum phase support')
    periodic<-ts(rep(seq_len(f),23),start=c(-2L,3L),frequency=f)
    choices<-rep(c('stable','s3x1','s3x5','s3x15'),length.out=f)
    expect_equal(as.numeric(process(periodic,choices)$values),as.numeric(periodic),tolerance=1e-14)
    named<-stats::setNames(choices,as.character(seq_len(f)))
    expect_identical(process(periodic,named[rev(seq_len(f))]),process(periodic,choices))
    stable<-process(ts(seq_len(f),start=c(-2L,3L),frequency=f),'stable','reject')
    expect_identical(as.numeric(stable$values),as.numeric(seq_len(f)))
    expect_true(all(stable$selection_reasons=='requested_stable'))
    # Normalized means remain finite near DBL_MAX, including exact cancellation.
    huge<-ts(rep(c(rep(.Machine$double.xmax,f),rep(-.Machine$double.xmax,f)),2),frequency=f)
    expect_true(all(process(huge,'stable')$values==0))
    expect_true(all(process(abs(huge),'stable')$values==.Machine$double.xmax))
    expect_error(raw(seq_len(f-1L),f,1L,'stable','x11'),'at least one observation')
  }
  y<-x11_symmetric_input(12L)
  for(filters in list(NULL,character(),NA_character_,'msr','x11default','s3x25',TRUE,1,factor('stable'),
      rep('stable',2),rep('stable',13),matrix(rep('stable',12)),setNames('stable','1'),
      setNames(rep('stable',12),rep('1',12)),setNames(rep('stable',12),c(NA,2:12))))expect_error(process(y,filters))
  for(policy in list('automatic',NULL,NA_character_,1,TRUE,matrix('x11'),c('x11','reject')))expect_error(process(y,'stable',policy))
  for(values in list(matrix(1:60),letters,TRUE,1+1i,factor(1:60),c(1:59,Inf)))expect_error(raw(values,12,1,'stable','x11'))
  for(f in list(1,5,52,NA_real_,Inf,TRUE,'12',matrix(12)))expect_error(raw(1:60,f,1,'stable','x11'))
  for(start in list(0,5,NA_real_,Inf,TRUE,'1',matrix(1)))expect_error(raw(1:60,4,start,'stable','x11'))
  missing<-process(ts(rep(NA_real_,60),frequency=12),'s3x5')
  expect_identical(missing$status,'no_available_outputs');expect_identical(missing$available_rows,integer())
  old<-Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  on.exit(if(is.na(old))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=old),add=TRUE)
  for(choices in list(rep('stable',12),rep(c('s3x1','s3x3','s3x5','s3x9','s3x15','stable'),2))) {
    y<-ts(as.numeric(x11_symmetric_input(12L,100003L)),start=c(-2L,3L),frequency=12)
    y[c(103,19001)]<-NA_real_
    Sys.setenv(PERSEPHONE_NUM_THREADS=1);serial<-process(y,choices)
    Sys.setenv(PERSEPHONE_NUM_THREADS=2);parallel<-process(y,choices)
    expect_identical(serial,parallel)
    for(multiplier in c(1e-100,-2,1e100))expect_equal(as.numeric(process(y*multiplier,choices)$values)/multiplier,
      as.numeric(serial$values),tolerance=2e-12)
    # Reversing observations also reverses the mapping to the old periods.
    reversed_choices<-choices[(length(y)-seq_len(12L)+2L)%%12L+1L]
    expect_equal(as.numeric(process(ts(rev(y),frequency=12),reversed_choices)$values),rev(as.numeric(serial$values)),tolerance=2e-12)
  }
})
