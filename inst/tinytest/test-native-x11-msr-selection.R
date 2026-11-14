local({
  select<-getFromNamespace('native_x11_msr_select','persephone3')
  pilot<-getFromNamespace('native_x11_msr_pilot','persephone3')
  policy<-getFromNamespace('native_x11_msr_policy','persephone3')
  resolve<-getFromNamespace('native_x11_msr_resolve','persephone3')
  plan<-getFromNamespace('native_x11_seasonal_plan','persephone3')
  process<-getFromNamespace('native_x11_seasonal_process','persephone3')
  table<-getFromNamespace('native_x11_msr_table','persephone3')
  raw<-getFromNamespace('.native_x11_msr_select','persephone3')
  source('fixtures/native/x11-msr-cases.R',local=TRUE)
  source('fixtures/native/x11-msr-selection-cases.R',local=TRUE)
  base<-'fixtures/native/x11-msr-selection'
  summaries<-read.csv(file.path(base,'summaries.csv'))
  read_split<-function(name) {x<-read.csv(file.path(base,paste0(name,'.csv')));split(x,x$case)}
  tables<-read_split('tables');pilots<-read_split('pilots');attempts<-read_split('attempts')
  close<-function(a,b,label) {
    a<-as.numeric(a);b<-as.numeric(b)
    expect_identical(is.na(a),is.na(b),info=label)
    expect_identical(is.infinite(a),is.infinite(b),info=label)
    at<-is.finite(a)&is.finite(b)
    expect_true(all(abs(a[at]-b[at])/pmax(1,abs(b[at]))<2e-12),info=label)
  }
  bounds<-function(rows)if(length(rows))setNames(range(rows),c('first','last'))else integer()
  cases<-x11_msr_selection_cases()
  expect_identical(sort(names(cases)),sort(summaries$case))
  for(ca in cases)for(method in c('jdplus','x11_five_year_cap')) {
    y<-x11_msr_selection_input(ca);before<-serialize(y,NULL)
    a<-select(y,ca$mode,method,ca$backcasts,ca$forecasts)
    o<-x11_msr_selection_oracle(y,ca$mode,method,ca$backcasts,ca$forecasts)
    for(field in c('status','reason','selected_filter','using_default'))
      expect_identical(a[[field]],o[[field]],info=paste(ca$id,method,field))
    for(field in c('estimation_rows','calendar_rows','selection_rows'))expect_identical(a[[field]],bounds(o[[field]]))
    expect_identical(a$iterations,length(o$attempts))
    expect_identical(a$partial_year_trim,length(o$estimation_rows)-length(o$calendar_rows))
    expect_identical(a$final_count,length(o$selection_rows))
    expect_identical(a$iteration_policy,method)
    expect_identical(a$attempts$iteration,seq_len(a$iterations))
    expect_identical(a$attempts$years_removed,seq_len(a$iterations)-1L)
    expect_identical(a$attempts$observations,vapply(o$attempts,function(z)as.integer(z$count),integer(1)))
    expect_identical(a$attempts$last_row,ca$backcasts+a$attempts$observations)
    expect_identical(a$attempts$band,vapply(o$attempts,function(z)z$band,character(1)))
    expect_identical(a$attempts$ratio_status,vapply(o$attempts,function(z)z$status,character(1)))
    close(a$attempts$global_msr,vapply(o$attempts,function(z)z$global_msr,numeric(1)),paste(ca$id,'trace formula'))
    if(!is.null(o$pilot)) {
      for(field in c('seasonal','irregular'))close(a$pilot[[field]],o$pilot[[field]],paste(ca$id,field,'pilot operator'))
      close(tsp(a$pilot$seasonal),tsp(o$pilot$seasonal),paste(ca$id,'pilot calendar'))
      for(field in c('seasonal','irregular'))close(a$table$phases[[paste0(field,'_mean')]],o$table$phases[[field]],paste(ca$id,'final phase formula'))
      close(a$global_msr,o$table$global,paste(ca$id,'final global formula'))
      expect_identical(a$pilot$component_rule,if(ca$mode=='multiplicative')'ratio'else'centered_difference')
    } else {
      expect_true(is.null(a$table))
      if(a$iterations)expect_true(is.null(a$pilot$seasonal) && is.null(a$pilot$irregular))else expect_true(is.null(a$pilot))
      expect_true(is.na(a$global_msr))
    }
    if(a$status=='available') {
      if(a$iterations)close(a$attempts$global_msr,head(attempts[[ca$id]]$global,a$iterations),paste(ca$id,'public prefixes'))
      if(method=='jdplus') {
        p<-summaries[summaries$case==ca$id,]
        expect_identical(a$selected_filter,p$choice);expect_identical(a$using_default,p$using_default)
        expect_identical(a$iterations,p$iterations)
        if(a$iterations) {
          close(a$global_msr,p$global,paste(ca$id,'public global'))
          for(field in c('seasonal','irregular')) {
            close(a$table$phases[[paste0(field,'_mean')]],tables[[ca$id]][[field]],paste(ca$id,'public phase'))
            close(a$pilot[[field]],pilots[[ca$id]][[field]],paste(ca$id,'public pilot'))
          }
        }
      }
    }
    expect_identical(serialize(y,NULL),before)
    expect_identical(unserialize(serialize(a,NULL)),a)
    p<-plan(ca$frequency,'msr','d_final');resolved<-resolve(p,a)
    expect_identical(resolved$msr_selection,a)
    expect_identical(resolved$requires_msr,a$status!='available')
    expect_identical(unname(resolved$resolved_filters),rep(a$selected_filter,ca$frequency))
    expect_true(p$requires_msr && all(is.na(p$resolved_filters)))
  }
  # Complete linear operators, including overlapping mean-extension support.
  for(f in c(4L,12L))for(n in c(3L*f,4L*f-1L,5L*f,7L*f+1L,10L*f+3L,20L*f+1L)) {
    expected<-matrix(0,n,n)
    for(p in seq_len(f)) {at<-seq.int(p,n,by=f);expected[at,at]<-x11_msr_pilot_operator(length(at))}
    actual<-remainder<-matrix(0,n,n)
    for(j in seq_len(n)) {
      y<-ts(as.numeric(seq_len(n)==j),start=c(2000,f),frequency=f);a<-pilot(y)
      actual[,j]<-a$seasonal;remainder[,j]<-a$irregular
      expect_identical(tsp(a$seasonal),tsp(y));expect_identical(a$status,'available')
    }
    close(actual,expected,'complete pilot operator');close(remainder,diag(n)-expected,'complete remainder operator')
    close(rowSums(actual),rep(1,n),'pilot constants')
  }
  # Equality evidence is a scalar-policy qualification. Whole-input controls in
  # the corpus stay 1e-5 away from cutoffs to avoid roundoff-dependent decisions.
  thresholds<-read.csv(file.path(base,'thresholds.csv'))
  expect_identical(thresholds$ratio,c(2.5,3.5,5.5,6.5))
  for(j in seq_len(nrow(thresholds))) {
    t<-thresholds[j,];a<-policy(t$ratio)
    expect_identical(a$band,c('lower_gap','s3x5','upper_gap','s3x9')[j])
    expect_identical(a$status,if(t$using_default)'ambiguous'else'available')
    if(!t$using_default)expect_identical(a$selected_filter,t$choice)else expect_true(is.na(a$selected_filter))
    delta<-.Machine$double.eps*t$ratio
    expect_identical(policy(t$ratio-delta)$band,c('s3x3','lower_gap','s3x5','upper_gap')[j])
    expect_identical(policy(t$ratio+delta)$band,a$band)
  }
  expect_identical(policy(0)$selected_filter,'s3x3');expect_identical(policy(Inf)$selected_filter,'s3x9')
  for(f in c(4L,12L)) {
    ca<-cases[[paste0('policy-difference-',f)]];y<-x11_msr_selection_input(ca)
    jd<-select(y);cap<-select(y,iteration_policy='x11_five_year_cap')
    expect_identical(jd$iterations,16L);expect_identical(jd$selected_filter,'s3x9')
    expect_identical(cap$iterations,6L);expect_identical(cap$reason,'five_year_cap')
    expect_identical(cap$selected_filter,'s3x5');expect_true(cap$using_default)
    for(method in c('jdplus','x11_five_year_cap'))for(budget in c(1L,2L,5L,6L,15L,16L,1000L)) {
      a<-select(y,iteration_policy=method,max_iterations=budget)
      o<-x11_msr_selection_oracle(y,iteration_policy=method,max_iterations=budget)
      for(field in c('status','reason','selected_filter','using_default'))expect_identical(a[[field]],o[[field]])
      expect_identical(a$iterations,length(o$attempts))
      close(a$global_msr,o$table$global,'budget retains last evaluated table')
      expect_identical(length(a$pilot$seasonal),a$final_count)
      if(a$status=='budget_exhausted') {
        p<-resolve(plan(f,'msr','d_final'),a)
        expect_true(p$requires_msr && all(is.na(p$resolved_filters)))
        expect_error(process(y,p$resolved_filters))
      }
    }
    a<-select(ts(rep(NA_real_,4*f),frequency=f),max_iterations=1L)
    expect_identical(a$reason,'insufficient_initial_history');expect_true(a$using_default && is.null(a$table))
    resolved<-resolve(plan(f,'msr','d_final'),a)
    expect_identical(unname(resolved$resolution_reasons),rep('msr_default',f))
    actual<-process(ts(seq_len(4*f),frequency=f),resolved$resolved_filters)
    expect_true(all(actual$selected_filters=='stable'))
    expect_identical(select(ts(rep(2,10*f),frequency=f))$status,'undefined_msr')
    expect_identical(pilot(ts(seq_len(3*f-1),frequency=f))$status,'insufficient_observations')
    z<-ts(10+sin(seq_len(10*f)*.7),start=c(2000,2),frequency=f);a<-pilot(z)
    for(multiplier in c(-1,1e150,1e-150)) {
      b<-pilot(z*multiplier);close(b$seasonal/multiplier,a$seasonal,'pilot units')
      close(b$irregular/multiplier,a$irregular,'remainder units')
    }
    close(pilot(z+128)$irregular,a$irregular,'absolute level')
    close(pilot(z*1e150,'multiplicative')$irregular,pilot(z,'multiplicative')$irregular,'relative units')
    # Direct finite differences retain represented small changes; overflow in
    # an individual difference may still have a representable phase average.
    tiny<-ts(rep(1+2^-40*c(0,1,-1,2,0,3),each=f),frequency=f)
    for(mode in c('additive','multiplicative')) {
      a<-table(tiny,tiny,mode);o<-x11_msr_oracle(list(seasonal=tiny,irregular=tiny),mode)
      expect_true(max(abs(a$phases$seasonal_mean-o$phases$seasonal))<1e-23)
    }
    big<-ts(rep(c(-.Machine$double.xmax,.Machine$double.xmax,.Machine$double.xmax),each=f),frequency=f)
    a<-table(big,big);expect_identical(a$status,'available');expect_identical(a$global_msr,1)
    for(mode in c('additive','multiplicative')) {
      z<-ts(rep(rep(c(.Machine$double.xmin,.Machine$double.xmax),each=f),10),frequency=f)
      expect_identical(pilot(z,mode)$status,'numerical_range')
      expect_identical(select(z,mode)$status,'numerical_range')
    }
  }
  prior<-Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  on.exit(if(is.na(prior))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=prior))
  year<-(seq_len(100003L)-1L)%/%12L+1L
  for(mode in c('additive','multiplicative')) {
    y<-ts(if(mode=='additive')year+1.3*(-1)^year else 10+.1*sin(year*.73),start=c(2000,11),frequency=12)
    Sys.setenv(PERSEPHONE_NUM_THREADS=1);a<-select(y,mode,max_iterations=2L)
    Sys.setenv(PERSEPHONE_NUM_THREADS=2);b<-select(y,mode,max_iterations=2L)
    expect_identical(a,b)
  }
  for(value in list(NULL,TRUE,'1',factor(1),matrix(1),1+1i,Inf,-Inf))expect_error(raw(value,4,1,'additive','jdplus',0,0,10))
  for(f in list(NULL,NA,NaN,Inf,-Inf,0,1,52,4.5,TRUE,'4',factor(4),c(4,12),matrix(4)))expect_error(raw(1:80,f,1,'additive','jdplus',0,0,10))
  for(first in list(0,5,1.5,NA,Inf,TRUE,'1',matrix(1)))expect_error(raw(1:80,4,first,'additive','jdplus',0,0,10))
  for(mode in list(NULL,NA_character_,1,TRUE,matrix('additive'),'log','Multiplicative',c('additive','multiplicative')))
    expect_error(raw(1:80,4,1,mode,'jdplus',0,0,10))
  for(method in list(NULL,character(),NA_character_,1,TRUE,matrix('jdplus'),'X11','five_year_cap',c('jdplus','x11_five_year_cap')))
    expect_error(raw(1:80,4,1,'additive',method,0,0,10))
  for(v in list(-1,1.5,NA,Inf,TRUE,'1',matrix(1),2^32))for(field in c('backcasts','forecasts','max_iterations')) {
    args<-list(values=1:80,frequency=4,first_period=1,mode='additive',iteration_policy='jdplus',backcasts=0,forecasts=0,max_iterations=10)
    args[[field]]<-v;expect_error(do.call(raw,args))
  }
  expect_error(raw(1:80,4,1,'additive','jdplus',0,0,0),'positive')
  expect_error(raw(1:80,4,1,'additive','jdplus',41,40,10),'extension lengths')
  expect_identical(raw(numeric(),4,1,'additive','jdplus',0,0,10)$reason,'insufficient_initial_history')
  for(r in list(NULL,NA,NaN,-1,-Inf,TRUE,'1',factor(1),matrix(1),c(1,2),1+1i))expect_error(policy(r))
  expect_error(select(1:80),'numeric ts');expect_error(pilot(1:80),'numeric ts')
  p<-plan(4,'msr','d_final');a<-select(ts(1:80,frequency=4))
  for(field in c('schema_version','backend','scope','frequency','status','iteration_policy','using_default','selected_filter')) {
    b<-a;b[[field]]<-NULL;expect_error(resolve(p,b))
  }
  invalid<-list(status='budget_exhausted',selected_filter=NA_character_,using_default=TRUE)
  for(field in names(invalid)) {b<-a;b[[field]]<-invalid[[field]];expect_error(resolve(p,b))}
  expect_error(resolve(plan(4,'msr','d_initial'),a),'pending')
  expect_error(resolve(resolve(p,a),a),'pending')
  expect_true(all(c('x11_msr_pilot','x11_msr_selection') %in% backend_capabilities('native')$primitives))
  expect_true(!backend_capabilities('native')$fit)
})
