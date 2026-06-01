createDashboard <- function(hts_object) {

  obj_name <- deparse(substitute(hts_object))
  print(obj_name)
  rmarkdown::run(
    file = system.file(
      "rmarkdown",
      "PersephoneDashboard.Rmd",
      package = "persephone3"
    ),
    render_args = list(
      params = list(
        hts_object = hts_object,
        hts_name = obj_name
      )
    ),
    shiny_args = list(launch.browser = TRUE)
  )

}
