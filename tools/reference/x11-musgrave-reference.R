# Execute public filter factories and DataBlock applications only.
reference_musgrave_bank <- function(frequency,length,ratio) {
  full <- reference_x11_filter(list(type='henderson',frequency=frequency,length=length))
  filters <- rJava::.jcall('jdplus/x13/base/core/x11/filter/MusgraveFilterFactory',
    '[Ljdplus/toolkit/base/core/math/linearfilters/FiniteFilter;','makeFilters',
    rJava::.jcast(full,'jdplus/toolkit/base/core/math/linearfilters/IFiniteFilter'),as.double(ratio))
  list(symmetric=full,right=rev(filters))
}
reference_musgrave_point <- function(filter,values,side='right') {
  # A right-side public filter on reversed observations defines the mirrored
  # left-side application without reading or translating an implementation.
  if(side=='left')values <- rev(values)
  m <- length(rJava::.jcall(filter,'[D','weightsToArray'))
  values <- tail(values,m)
  applied <- reference_x11_apply(filter,values,1L)
  stopifnot(sum(!is.na(applied))==1L)
  unname(applied[!is.na(applied)])
}
reference_musgrave_apply <- function(bank,values) {
  result <- reference_x11_apply(bank$symmetric,values,1L);n <- length(values)
  for(i in seq_along(bank$right)) {
    result[n-i+1L] <- reference_musgrave_point(bank$right[[i]],values)
    result[i] <- reference_musgrave_point(bank$right[[i]],values,'left')
  }
  result
}
