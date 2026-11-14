local({
  table<-getFromNamespace('native_x11_msr_table','persephone3');raw<-getFromNamespace('.native_x11_msr_table','persephone3')
  source('fixtures/native/x11-msr-cases.R',local=TRUE)
  targets<-split(read.csv('fixtures/native/x11-msr/tables.csv'),read.csv('fixtures/native/x11-msr/tables.csv')$case)
  close<-function(a,b,label) {
    expect_identical(is.na(a),is.na(b),info=label)
    expect_identical(is.infinite(a),is.infinite(b),info=label)
    at<-is.finite(a)&is.finite(b)
    expect_true(all(abs(a[at]-b[at])/pmax(1,abs(b[at]))<2e-12),info=label)
  }
  for(ca in x11_msr_cases()) {
    y<-x11_msr_input(ca);before<-serialize(y,NULL);a<-table(y$seasonal,y$irregular,ca$mode);p<-a$phases
    o<-x11_msr_oracle(y,ca$mode);target<-targets[[ca$id]]
    for(field in c('seasonal','irregular')) {
      close(p[[paste0(field,'_mean')]],o$phases[[field]],paste(ca$id,field,'formula'))
      close(p[[paste0(field,'_raw_mean')]],o$phases[[paste0(field,'_raw')]],paste(ca$id,field,'raw'))
      close(p[[paste0(field,'_correction')]],o$phases[[paste0(field,'_correction')]],paste(ca$id,field,'factor'))
      # Public signed/zero denominators are frozen controls, outside positive
      # relative-component support. Valid components/phases remain comparable.
      at<-p[[paste0(field,'_status')]]=='available'
      close(p[[paste0(field,'_mean')]][at],target[[field]][at],paste(ca$id,field,'public'))
    }
    close(p$msr,o$phases$msr,paste(ca$id,'phase ratio'))
    close(a$global_msr,o$global,paste(ca$id,'global formula'))
    if(a$status %in% c('available','undefined_msr'))close(a$global_msr,target$global[1],paste(ca$id,'global public'))
    expect_equal(p$observations,o$phases$observations)
    expect_equal(p$changes,o$phases$changes)
    expect_identical(p$period,seq_len(ca$frequency))
    expect_identical(serialize(y,NULL),before)
    expect_identical(unserialize(serialize(a,NULL)),a)
    expect_identical(a$global_weight,'adjacent_annual_changes')
    expect_identical(a$correction_convention,'x11_decimal_change_count')
  }
  modes<-read.csv('fixtures/native/x11-msr/modes.csv')
  for(ca in x11_msr_mode_cases()) {
    target<-modes[modes$case==ca$id,];y<-x11_msr_mode_components(ca,target$iterations[1])
    a<-table(y$seasonal,y$irregular,ca$mode)
    close(a$phases$seasonal_mean,target$seasonal,paste(ca$id,'selector seasonal'))
    close(a$phases$irregular_mean,target$irregular,paste(ca$id,'selector irregular'))
    close(a$global_msr,target$global[1],paste(ca$id,'selector global'))
    expect_identical(a$change_measure,if(ca$mode=='multiplicative')'relative_absolute_change'else'absolute_change')
  }
  for(f in c(4L,12L)) {
    y<-ts(seq_len(10*f+1),start=c(2000,f),frequency=f);i<-10+sin(y)
    base<-table(y,i);scaled<-table(y*1e150,i*1e150)
    close(scaled$global_msr,base$global_msr,'absolute common units')
    close(scaled$phases$seasonal_mean/1e150,base$phases$seasonal_mean,'scaled means')
    close(table(y*1e150,i*1e-150,'multiplicative')$global_msr,table(y,i,'multiplicative')$global_msr,'relative independent units')
    close(table(-y,-i)$global_msr,base$global_msr,'absolute signs')
    close(table(y+128,i+32)$global_msr,base$global_msr,'absolute levels')
    a<-table(y*0+1,i*0+1);expect_identical(a$status,'undefined_msr');expect_true(all(a$phases$status=='undefined_msr'))
    expect_identical(table(y*0+1,i)$global_msr,Inf)
    expect_identical(table(y,i*0+1)$global_msr,0)
    a<-table(y[1]+ts(rep(0,2*f-1),frequency=f),ts(seq_len(2*f-1),frequency=f))
    expect_identical(a$status,'incomplete_phases');expect_identical(sum(a$phases$status=='insufficient_observations'),1L)
    for(j in c(1L,f+2L,length(y))) {
      gap<-y;gap[j]<-NA;a<-table(gap,i);period<-as.integer(cycle(y)[j]);other<-seq_len(f)!=period
      expect_identical(a$status,'incomplete_phases');expect_identical(a$phases$seasonal_status[period],'missing_observations')
      expect_identical(a$phases$irregular_mean,base$phases$irregular_mean)
      expect_identical(a$phases$seasonal_mean[other],base$phases$seasonal_mean[other])
    }
    shifted<-ts(as.numeric(y),start=c(2000,1),frequency=f)
    expect_error(table(shifted,i),'identical time domains')
    big<-ts(rep(c(0,.Machine$double.xmax,0),each=f)[seq_len(3*f-1)],frequency=f)
    expect_identical(table(big,big)$status,'available')
    expect_identical(table(big,big)$global_msr,1)
  }
  # Long deterministic worker equality, including uneven phase lengths.
  ca<-list(n=100003L,frequency=12L,start=c(2000L,11L),shape='rough');y<-x11_msr_input(ca)
  prior<-Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  on.exit(if(is.na(prior))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=prior))
  for(mode in c('additive','multiplicative')) {
    Sys.setenv(PERSEPHONE_NUM_THREADS=1);a<-table(y$seasonal,y$irregular,mode)
    Sys.setenv(PERSEPHONE_NUM_THREADS=2);b<-table(y$seasonal,y$irregular,mode)
    expect_identical(a,b)
  }
  # Explicit range failures, without NaN being used as a filter decision.
  y<-ts(rep(c(.Machine$double.xmax,-.Machine$double.xmax),each=4),frequency=4)
  expect_true(all(table(y,y)$phases$status=='numerical_range'))
  y<-ts(rep(c(.Machine$double.xmin,.Machine$double.xmax),each=4),frequency=4)
  expect_true(all(table(y,y,'multiplicative')$phases$status=='numerical_range'))
  tiny<-.Machine$double.xmin*.Machine$double.eps
  y<-ts(c(rep(0,8),rep(tiny,4)),frequency=4)
  expect_identical(table(y,y)$status,'incomplete_phases')
  expect_true(all(table(y,y)$phases$status=='numerical_range'))
  a<-raw(numeric(),numeric(),4,1,'additive')
  expect_identical(a$status,'incomplete_phases');expect_true(all(a$phases$observations==0L))
  for(value in list(NULL,TRUE,'1',factor(1),matrix(1),1+1i,Inf,-Inf))expect_error(raw(value,1:20,4,1,'additive'))
  for(f in list(NULL,NA,NaN,Inf,-Inf,0,1,52,4.5,TRUE,'4',factor(4),c(4,12),matrix(4)))expect_error(raw(1:20,1:20,f,1,'additive'))
  for(first in list(0,5,1.5,NA,Inf,TRUE,'1',matrix(1)))expect_error(raw(1:20,1:20,4,first,'additive'))
  for(mode in list(NULL,character(),NA_character_,1,TRUE,matrix('additive'),'log','Multiplicative',c('additive','multiplicative')))
    expect_error(raw(1:20,1:20,4,1,mode))
  expect_error(raw(1:20,1:19,4,1,'additive'),'identical lengths')
  expect_error(table(1:20,1:20),'numeric ts')
  expect_identical(table(ts(1:20,frequency=4),ts(1:20,frequency=4))$scope,'msr_change_table_only')
  expect_true('x11_msr_tables' %in% backend_capabilities('native')$primitives)
  expect_true(!backend_capabilities('native')$fit)
})
