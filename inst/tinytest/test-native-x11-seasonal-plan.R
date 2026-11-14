local({
  plan<-getFromNamespace('native_x11_seasonal_plan','persephone3')
  raw<-getFromNamespace('.native_x11_seasonal_plan','persephone3')
  process<-getFromNamespace('native_x11_seasonal_process','persephone3')
  source('fixtures/native/x11-seasonal-plan-cases.R',local=TRUE)
  frozen<-read.csv('fixtures/native/x11-seasonal-plan/plans.csv',stringsAsFactors=FALSE)
  targets<-split(frozen,frozen$case)
  for(ca in x11_seasonal_plan_cases()) {
    target<-targets[[ca$id]];y<-x11_seasonal_plan_input(ca)
    for(stage in x11_seasonal_stages()) {
      before<-serialize(ca$filters,NULL);actual<-plan(ca$frequency,ca$filters,stage)
      expected<-if(endsWith(stage,'initial'))target$initial_filter else target$final_filter
      pending<-stage=='d_final' && target$global_msr[1]
      expect_identical(unname(actual$base_filters),expected,info=paste(ca$id,stage))
      expect_identical(actual$global_msr,target$global_msr[1])
      expect_identical(actual$requires_msr,pending)
      expect_identical(actual$status,if(pending)'requires_msr'else'available')
      expect_identical(names(actual$base_filters),as.character(seq_len(ca$frequency)))
      expect_identical(unname(actual$requested_filters),rep(ca$filters,length.out=ca$frequency))
      expect_identical(actual$stage,stage)
      expect_identical(unserialize(serialize(actual,NULL)),actual)
      expect_identical(serialize(ca$filters,NULL),before)
      if(pending) {
        expect_true(all(is.na(actual$resolved_filters)))
        expect_true(all(actual$resolution_reasons=='requires_msr'))
        expect_error(process(y,actual$resolved_filters),'missing')
      } else {
        expect_identical(unname(actual$resolved_filters),expected)
        # The existing processor retains the separate length-based policy.
        # Start/calendar differences are exercised by the prospective context
        # spans, and public final mappings stay independent of these fallbacks.
        applied<-process(y,actual$resolved_filters)
        expect_identical(unname(applied$requested_filters),expected)
        expect_identical(tsp(applied$values),tsp(y))
      }
    }
  }
  for(f in c(4L,12L))for(stage in x11_seasonal_stages()) {
    for(request in c('stable','s3x1','s3x3','s3x5','s3x9','s3x15','x11default','msr'))
      expect_identical(plan(f,request,stage),plan(f,rep(request,f),stage))
    choices<-rep(c('msr','x11default','s3x3','stable'),length.out=f)
    named<-setNames(choices,as.character(seq_len(f)))
    expect_identical(plan(f,rev(named),stage),plan(f,choices,stage))
    mixed<-plan(f,choices,stage)
    expect_true(!mixed$global_msr && !mixed$requires_msr)
    expect_identical(unname(mixed$resolution_reasons[1]),'msr_mixed_request')
  }
  # All named requests must be complete calendar vectors, not implicit recycling.
  for(filters in list(setNames('msr','1'),setNames(rep('msr',4),c('1','2','3','3')),
    setNames(rep('msr',4),c('1','2','3',NA_character_)),setNames(rep('msr',4),c('1','2','3','Q4'))))
    expect_error(plan(4,filters,'d_final'),'every calendar period')
  for(filters in list(NULL,character(),c('msr','s3x3'),1,TRUE,factor('msr'),NA_character_,
    'MSR','s3x25','crossvalidation',matrix('msr')))expect_error(raw(4,filters,'d_final'))
  for(stage in list(NULL,character(),c('b_initial','d_final'),NA_character_,1,TRUE,'initial','D_final',matrix('d_final')))
    expect_error(raw(4,'msr',stage))
  for(f in list(NULL,NA_real_,NaN,Inf,-Inf,0,1,52,4.5,TRUE,'4',factor(4),c(4,12),matrix(4))) {
    expect_error(raw(f,'msr','d_final'))
    expect_error(plan(f,'msr','d_final'))
  }
  expect_error(process(ts(1:40,frequency=4),'msr'),'supports')
  expect_error(process(ts(1:40,frequency=4),'x11default'),'supports')
  expect_identical(plan(4,'msr','d_final')$scope,'seasonal_stage_policy_only')
  expect_true(!backend_capabilities('native')$fit)
})
