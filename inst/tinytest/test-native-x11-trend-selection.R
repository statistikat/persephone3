local({
  select<-getFromNamespace('native_x11_trend_select','persephone3')
  policy<-getFromNamespace('native_x11_trend_policy','persephone3')
  raw<-getFromNamespace('.native_x11_trend_select','persephone3')
  endpoint<-getFromNamespace('native_x11_musgrave_spec','persephone3')
  source('fixtures/native/x11-symmetric-cases.R',local=TRUE)
  source('fixtures/native/x11-musgrave-cases.R',local=TRUE)
  source('fixtures/native/x11-trend-selection-cases.R',local=TRUE)
  frozen<-read.csv('fixtures/native/x11-trend-selection/ratios.csv',stringsAsFactors=FALSE)
  policies<-read.csv('fixtures/native/x11-trend-selection/policies.csv',stringsAsFactors=FALSE)
  defaults<-read.csv('fixtures/native/x11-trend-selection/defaults.csv',stringsAsFactors=FALSE)
  weights<-read.csv('fixtures/native/x11-trend-selection/weights.csv',stringsAsFactors=FALSE)
  for(ca in x11_trend_cases()) {
    y<-x11_trend_input(ca);before<-serialize(y,NULL)
    result<-select(y,ca$mode,ca$backcasts,ca$forecasts)
    expected<-if(is.null(ca$expected_status))'available'else ca$expected_status
    expect_identical(result$status,expected,info=ca$id)
    expect_identical(serialize(y,NULL),before)
    expect_identical(unserialize(serialize(result,NULL)),result)
    expect_identical(result$input_domain$start,as.numeric(ca$start))
    expect_identical(result$mode,ca$mode)
    expect_identical(result$pilot_length,ca$frequency+1L)
    n<-ca$n-ca$backcasts-ca$forecasts;h<-ca$frequency%/%2L
    expect_identical(result$change_count,as.integer(max(0,n-2*h-1L)))
    expect_identical(result$estimation_rows,c(first=ca$backcasts+1L,last=ca$n-ca$forecasts))
    if(expected=='available') {
      target<-frozen[frozen$case==ca$id,];relative<-ca$mode %in% c('multiplicative','pseudo_additive')
      oracle<-x11_trend_oracle(y,ca$frequency,relative,ca$backcasts,ca$forecasts)
      expect_true(abs(result$comparison_ic_ratio-target$comparison_ic_ratio)/(1+abs(target$comparison_ic_ratio))<=2e-12,info=ca$id)
      expect_identical(result$selected_length,target$selected_length)
      for(field in c('trend_mean_change','irregular_mean_change','raw_ic_ratio','comparison_ic_ratio'))
        expect_true(abs(result[[field]]-oracle[[field]])/(1+abs(oracle[[field]]))<=2e-12,info=paste(ca$id,field))
      expect_identical(result$comparison_ic_ratio,result$raw_ic_ratio*result$frequency_factor)
      expect_identical(result$endpoint_ic_ratio,policy(ca$frequency,length=result$selected_length)$endpoint_ic_ratio)
      expect_identical(result$pilot_rows,c(first=ca$backcasts+h+1L,last=ca$n-ca$forecasts-h))
      expect_identical(result$change_rows,c(first=ca$backcasts+h+2L,last=ca$n-ca$forecasts-h))
      trimmed<-ts(as.numeric(y)[seq.int(ca$backcasts+1L,ca$n-ca$forecasts)],frequency=ca$frequency)
      expect_identical(result$comparison_ic_ratio,select(trimmed,ca$mode)$comparison_ic_ratio)
      if(ca$group=='threshold')expect_true(abs(result$comparison_ic_ratio-ca$target)<1e-10)
    } else {
      expect_true(is.na(result$selected_length))
      expect_true(is.na(result$comparison_ic_ratio))
      expect_true(is.na(result$endpoint_ic_ratio))
      # Preserve the public undefined-ratio/default-longest behavior as evidence,
      # while never treating it as an available native estimate.
      if(ca$type %in% c('constant','zero','missing','short'))expect_true(is.na(frozen$comparison_ic_ratio[frozen$case==ca$id]))
    }
  }
  prospective<-x11_trend_policy_cases()
  for(j in seq_len(nrow(prospective))) {
    ca<-prospective[j,];result<-policy(ca$frequency,ic_ratio=ca$ratio)
    expect_identical(result$selected_length,policies$selected_length[match(ca$id,policies$id)])
    expect_identical(result$comparison_ic_ratio,ca$ratio)
    expect_identical(result$selection,'automatic_ic')
  }
  for(j in seq_len(nrow(defaults))) {
    ca<-defaults[j,];result<-policy(ca$frequency,length=ca$length)
    expect_identical(result$endpoint_ic_ratio,ca$endpoint_ic_ratio)
    expect_identical(result$selected_length,ca$length)
    expect_identical(result$selection,'fixed_length')
    if(ca$length<=3L)next
    w<-weights[weights$case==ca$case,];full<-henderson_minimum_roughness(ca$length)
    for(q in unique(w$available)) {
      native<-unname(endpoint(ca$frequency,ca$length,q,result$endpoint_ic_ratio)$weights)
      expect_true(max(abs(native-w$weight[w$available==q]))<1e-14)
      independent<-x11_musgrave_minimum(full,q,result$endpoint_ic_ratio)
      expect_true(max(abs(native-independent))<1e-10)
    }
  }
  # Closed alternating components, polynomial reproduction, trimming and units.
  for(f in c(4L,12L)) {
    y<-ts(10+(-1)^seq_len(121L),start=c(-10,3),frequency=f)
    ratio<-select(y)$comparison_ic_ratio
    expect_equal(ratio,if(f==4L)504/25 else 4232/33,tolerance=2e-12)
    expect_true(select(ts(seq_len(121L)^3,frequency=f))$comparison_ic_ratio<1e-12)
    for(scale in c(1e-100,-2,1e100))expect_equal(select(scale*y)$comparison_ic_ratio,ratio,tolerance=2e-12)
    expect_equal(select(ts(rev(as.numeric(y)),frequency=f))$comparison_ic_ratio,ratio,tolerance=2e-12)
    y<-ts(100+seq_len(100003L)/1000+sin(seq_len(100003L)*.7),frequency=f)
    old<-Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
    Sys.setenv(PERSEPHONE_NUM_THREADS=1);serial<-select(y)
    Sys.setenv(PERSEPHONE_NUM_THREADS=2);parallel<-select(y)
    expect_identical(serial,parallel)
    Sys.setenv(PERSEPHONE_NUM_THREADS=1);serial_relative<-select(y,'multiplicative')
    Sys.setenv(PERSEPHONE_NUM_THREADS=2);expect_identical(serial_relative,select(y,'multiplicative'))
    if(is.na(old))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=old)
    for(scale in c(1e-100,1e100))expect_equal(select(scale*y,'multiplicative')$comparison_ic_ratio,serial_relative$comparison_ic_ratio,tolerance=2e-11)
    gap<-y;gap[1]<-NA_real_
    expect_identical(select(gap)$status,'missing_observations')
    expect_identical(select(gap,backcasts=1L)$comparison_ic_ratio,select(ts(as.numeric(y[-1]),frequency=f))$comparison_ic_ratio)
  }
  for(value in list(NA_real_,NaN,-1,-Inf,'1',TRUE,numeric(),c(1,2),factor('1'),matrix(1)))expect_error(policy(12,ic_ratio=value))
  for(value in list(NA_real_,Inf,-1,0,2,102,1.5,.Machine$integer.max,'5',TRUE))expect_error(policy(12,length=value))
  for(value in list(0,1,52,NA_real_,4.1,TRUE,'4',c(4,12)))expect_error(policy(value,ic_ratio=1))
  expect_error(policy(12),'exactly one');expect_error(policy(12,ic_ratio=1,length=13),'exactly one')
  for(value in list(-1,NA_real_,Inf,.5,TRUE,'1',101,.Machine$integer.max))expect_error(raw(1:100,12,'additive',value,0))
  for(value in list('unknown',NA_character_,c('additive','multiplicative'),1,matrix('additive')))expect_error(raw(1:100,12,value,0,0))
  for(value in list(Inf,c(1,-Inf),matrix(1:20),factor(1:20),'1'))expect_error(raw(value,12,'additive',0,0))
  expect_identical(raw(numeric(),12,'additive',0,0)$status,'insufficient_observations')
  extreme<-rep(.Machine$double.xmax,20);extreme[10]<-.Machine$double.xmin
  expect_identical(raw(extreme,12,'additive',0,0)$status,'numerical_range')
  expect_identical(raw(rep(.Machine$double.xmax,20),12,'additive',0,0)$status,'undefined_ic_ratio')
  expect_identical(raw(rep(c(-.Machine$double.xmax,.Machine$double.xmax),10),12,'additive',0,0)$status,'numerical_range')
  expect_identical(raw(rep(c(0,.Machine$double.xmin*.Machine$double.eps),10),12,'additive',0,0)$status,'numerical_range')
})
