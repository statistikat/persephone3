local({
  initial<-getFromNamespace('native_x11_initial_additive','persephone3')
  raw<-getFromNamespace('.native_x11_initial_additive','persephone3')
  process<-getFromNamespace('native_x11_seasonal_process','persephone3')
  normalize<-getFromNamespace('native_x11_seasonal_normalize','persephone3')
  for(file in c('x11-seasonal-cases.R','x11-seasonal15-cases.R','x11-seasonal-processing-cases.R',
    'x11-normalization-cases.R','x11-initial-cases.R'))source(file.path('fixtures/native',file),local=TRUE)
  points<-read.csv('fixtures/native/x11-initial/points.csv');targets<-split(points,points$case)
  points<-read.csv('fixtures/native/x11-initial/bstep_points.csv');bstep<-split(points,points$case)
  statuses<-read.csv('fixtures/native/x11-initial/statuses.csv')
  close<-function(a,b,label) {
    a<-as.numeric(a);b<-as.numeric(b)
    expect_identical(length(a),length(b),info=label);expect_identical(is.na(a),is.na(b),info=label)
    expect_identical(is.infinite(a),is.infinite(b),info=label)
    at<-is.finite(a)&is.finite(b);expect_true(all(abs(a[at]-b[at])/pmax(1,abs(b[at]))<2e-12),info=label)
  }
  cases<-x11_initial_cases();expect_identical(sort(names(cases)),sort(names(targets)))
  expect_identical(sort(names(bstep)),sort(statuses$case[statuses$bstep_available]))
  for(ca in cases) {
    y<-x11_initial_input(ca);before<-serialize(y,NULL);a<-initial(y,ca$filters);o<-x11_initial_oracle(y,ca$filters)
    expect_identical(names(a$tables),c('B1','B2','B3','B4a','B4anorm','B4d'))
    rows<-ca$frequency%/%2L+seq_len(ca$n-ca$frequency)
    for(name in names(a$tables)) {
      public<-targets[[ca$id]]$value[targets[[ca$id]]$table==name]
      actual_bstep<-if(!is.null(bstep[[ca$id]]))bstep[[ca$id]]$value[bstep[[ca$id]]$table==name]else NULL
      missing_audit<-ca$shape=='gap' && ca$choice %in% c('stable','mixed_stable','s3x15') && name %in% c('B4a','B4anorm','B4d')
      if(missing_audit) {
        # The existing native contract preserves missing operands. Public stable
        # means omit them. Keep the disagreement visible, never widen tolerance
        # or replace a native output using the public target.
        expect_true(any(is.na(a$tables[[name]])!=is.na(public)))
        expect_true(all(is.na(a$tables[[name]])[is.na(public)]))
        keep<-!is.na(a$tables[[name]])
        close(a$tables[[name]][keep],public[keep],paste(ca$id,name,'available public rows'))
        close(public,actual_bstep,paste(ca$id,name,'public chain/getter agreement'))
        if(name=='B4a')for(p in which(a$seasonal$selected_filters=='stable')) {
          at<-which(cycle(a$tables$B3)==p)
          close(public[at],rep(mean(a$tables$B3[at],na.rm=TRUE),length(at)),'public omitted-missing phase mean')
        }
      } else {
        close(a$tables[[name]],public,paste(ca$id,name,'public chain'))
        if(!is.null(actual_bstep))close(a$tables[[name]],actual_bstep,paste(ca$id,name,'B-step getter'))
      }
      close(a$tables[[name]],o[[name]],paste(ca$id,name,'independent'))
      basis<-if(name=='B1')seq_along(y)else rows
      expect_identical(a$row_basis[[name]],basis)
      expect_identical(a$available_rows[[name]],basis[!is.na(a$tables[[name]])])
      close(as.numeric(time(a$tables[[name]])),as.numeric(time(y))[basis],'table calendar')
    }
    keep<-!is.na(a$tables$B2)&!is.na(a$tables$B3)
    close((a$tables$B2+a$tables$B3)[keep],as.numeric(y)[rows][keep],'B1=T+SI on available rows')
    keep<-!is.na(a$tables$B4anorm)&!is.na(a$tables$B4d)
    close((a$tables$B4anorm+a$tables$B4d)[keep],a$tables$B3[keep],'SI=S+I on available rows')
    expect_identical(a$seasonal,process(a$tables$B3,ca$filters))
    expect_identical(a$normalization,normalize(a$tables$B4a,ca$filters))
    expect_identical(a$drop,ca$frequency%/%2L)
    expect_identical(a$first_period,as.integer(cycle(a$tables$B2)[1]))
    expect_identical(a$complete,all(vapply(a$tables,function(x)!anyNA(x),logical(1))))
    expect_identical(a$status,if(any(!is.na(a$tables$B4d)))'available'else'no_available_outputs')
    expect_identical(a$scope,'additive_initial_tables_only');expect_false(a$complete_b_pass)
    expect_identical(serialize(y,NULL),before);expect_identical(unserialize(serialize(a,NULL)),a)
  }
  # Verify the complete linear operators using independent impulses. Verify
  # missing support separately, retaining the staged dependency graph.
  for(f in c(4L,12L))for(n in c(3L*f,6L*f-1L,6L*f,20L*f+1L)) {
    filters<-c('stable','s3x15',rep('s3x3',f-2L));y<-ts(rep(0,n),start=c(-2,2),frequency=f)
    o<-x11_initial_operator(y,filters);actual<-lapply(o$tables,function(x)x*0)
    for(j in seq_len(n)) {
      y[]<-0;y[j]<-1;a<-initial(y,filters)
      for(name in names(actual))actual[[name]][,j]<-as.numeric(a$tables[[name]])
    }
    for(name in names(actual))close(actual[[name]],o$tables[[name]],paste(name,'complete operator'))
    for(j in seq_len(n)) {
      y[]<-2;y[j]<-NA_real_;a<-initial(y,filters)
      for(name in names(actual))expect_identical(as.vector(is.na(a$tables[[name]])),o$support[[name]][,j])
    }
  }
  for(f in c(4L,12L)) {
    y<-ts(100+.125*seq_len(11L*f+1L)+(seq_len(11L*f+1L)%%f==0)-1/f,start=c(-2,f),frequency=f)
    for(filter in c('stable','s3x1','s3x3','s3x5','s3x9','s3x15')) {
      a<-initial(y,filter);rows<-a$row_basis$B2
      close(a$tables$B2,100+.125*rows,'closed linear trend')
      close(a$tables$B4d,rep(0,length(rows)),'fixed annual pattern leaves no irregular')
    }
    filters<-c('stable','s3x15',rep('s3x5',f-2L));a<-initial(y,filters)
    for(scale in c(-1,1e150,1e-150))for(name in names(a$tables))
      close(initial(y*scale,filters)$tables[[name]]/scale,a$tables[[name]],'additive units')
    named<-setNames(filters,as.character(seq_len(f)));expect_identical(initial(y,rev(named)),a)
    expect_error(initial(y,setNames(filters,rep('1',f))),'every calendar period')
    expect_error(initial(y,setNames(filters,seq_len(f)+1L)),'every calendar period')
    # Cropped history governs both processors; normalization still follows the
    # requested filter even when a short phase falls back to a stable mean.
    short<-initial(ts(seq_len(6L*f),frequency=f),'s3x9')
    expect_true(all(short$seasonal$selected_filters=='stable'))
    expect_true(all(short$normalization$endpoint_policies=='nearest_central_mean'))
    for(n in c(0L,1L,3L*f-1L))expect_error(raw(seq_len(n),f,1,'s3x3'),'three annual cycles')
    expect_identical(initial(y*NA_real_,filters)$status,'no_available_outputs')
    big<-ts(rep(.Machine$double.xmax,10L*f),frequency=f);big[f+1L]<--.Machine$double.xmax
    expect_error(initial(big,'s3x3'),'non-finite')
  }
  prior<-Sys.getenv('PERSEPHONE_NUM_THREADS',unset=NA_character_)
  # Symmetric unweighted pairs can overflow although their annual mean is
  # representable. The guarded weighted-pair path preserves this constant.
  huge_constant<-initial(ts(rep(.Machine$double.xmax,80),frequency=4),'stable')
  expect_identical(as.numeric(huge_constant$tables$B2),rep(.Machine$double.xmax,76))
  expect_identical(as.numeric(huge_constant$tables$B4d),rep(0,76))
  on.exit(if(is.na(prior))Sys.unsetenv('PERSEPHONE_NUM_THREADS')else Sys.setenv(PERSEPHONE_NUM_THREADS=prior))
  y<-ts(10+sin(seq_len(100003L)*.37),start=c(2000,11),frequency=12)
  for(filters in list('s3x3',c('stable','s3x15',rep('s3x3',10)))) {
    Sys.setenv(PERSEPHONE_NUM_THREADS=1);a<-initial(y,filters)
    Sys.setenv(PERSEPHONE_NUM_THREADS=2);b<-initial(y,filters);expect_identical(a,b)
  }
  for(x in list(NULL,TRUE,'1',factor(1),matrix(1),1+1i,Inf,-Inf))expect_error(raw(x,4,1,'s3x3'))
  for(f in list(NULL,NA,NaN,Inf,-Inf,0,1,52,4.5,TRUE,'4',factor(4),c(4,12),matrix(4)))expect_error(raw(1:80,f,1,'s3x3'))
  for(first in list(0,5,1.5,NA,Inf,TRUE,'1',matrix(1)))expect_error(raw(1:80,4,first,'s3x3'))
  for(filters in list(NULL,character(),NA_character_,1,TRUE,matrix('stable'),'msr','x11default','S3X3',c('stable','s3x3'),rep('stable',5)))
    expect_error(raw(1:80,4,1,filters))
  for(mode in list(NULL,NA,'multiplicative','log_additive','pseudo_additive',1,c('additive','additive')))
    expect_error(initial(ts(1:80,frequency=4),'s3x3',mode=mode),'additive mode only')
  expect_error(initial(1:80,'s3x3'),'numeric ts')
  expect_true('x11_additive_initial_tables' %in% backend_capabilities('native')$primitives)
  expect_true(!backend_capabilities('native')$fit)
})
