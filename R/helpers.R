#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

ignore_unused_imports <- function() {
  rjd3tramoseats::tramoseats
  rjd3x13::x13
}

.onLoad <- function(...) {
  # avoid issues with missing rprotobuf descriptor which looks like this during
  # devtools::check()
  #> perX13(AirPassengers)$run()
  #> Error in h(simpleError(msg, call)) :
  #>   error in evaluating the argument 'descriptor' in selecting a method for
  #>   function 'read': object 'x13.X13Output' not found
  #> Calls: <Anonymous> ... .x13_output -> <Anonymous> -> .handleSimpleError -> h
  if (!requireNamespace("rjd3toolkit", quietly = T)) {
    stop("Loading rjd3toolkit failed")
  }
}

# Null-Coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

rescale <- function(x, min, max) {
  stopifnot(
    is.numeric(x),
    is.numeric(min),
    is.numeric(max),
    length(min) == 1,
    length(max) == 1
  )
  rg <- range(x)
  min + (x - rg[1]) / (rg[2] - rg[1]) * (max - min)
}

# interne Template-Mengen je Methode (einzig wahre Quelle)
sa_templates <- function(method = c("x13", "tramoseats")) {
  switch(
    match.arg(method),
    x13 = c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c"),
    tramoseats = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
  )
}

# expliziter Exakt-Check (match.arg wuerde partiell matchen: "rsa5" -> "rsa5c")
check_template <- function(template, method) {
  allowed <- sa_templates(method)
  if (length(template) != 1L || !(template %in% allowed)) {
    stop(sprintf(
      "Invalid template '%s' for method '%s'. Allowed values: %s",
      template,
      method,
      paste(allowed, collapse = ", ")
    ))
  }
  template
}

# Methode ist die R6-Klasse und laesst sich an bestehenden Objekten nicht
# aendern; iterate = TRUE kann sie daher nicht umschreiben.
check_component_method <- function(components, method, iterate = FALSE) {
  comp_method <- vapply(
    components,
    function(x) {
      if (is.persephone(x)) x$method else NA_character_
    },
    character(1)
  )
  conflict <- names(comp_method)[!is.na(comp_method) & comp_method != method]
  if (length(conflict) == 0) {
    return(invisible(NULL))
  }
  methods <- paste(unique(comp_method[conflict]), collapse = "/")
  msg <- sprintf(
    paste0(
      "Components %s are already '%s' objects and cannot be switched to ",
      "method = \"%s\"; they stay '%s'."
    ),
    paste(sQuote(conflict), collapse = ", "),
    methods,
    method,
    methods
  )
  if (isTRUE(iterate)) {
    msg <- paste0(
      msg,
      paste0(
        " iterate = TRUE updates their spec parameters but not their method; ",
        "pass them as plain ts/mts objects to have them created with '",
        method,
        "'."
      )
    )
  }
  warning(msg, call. = FALSE)
  invisible(NULL)
}

# spec-blocks and their corresponding functions
specfun_list <- list(
  arima = rjd3toolkit::set_arima,
  automdl = rjd3toolkit::set_automodel,
  basic = rjd3toolkit::set_basic,
  easter = rjd3toolkit::set_easter,
  estimate = rjd3toolkit::set_estimate,
  setout = rjd3toolkit::set_outlier,
  td = rjd3toolkit::set_tradingdays,
  transform = rjd3toolkit::set_transform,
  addout = rjd3toolkit::add_outlier,
  remout = rjd3toolkit::remove_outlier,
  addramp = rjd3toolkit::add_ramp,
  remramp = rjd3toolkit::remove_ramp,
  usrdefvar = rjd3toolkit::add_usrdefvar,
  x11 = rjd3x13::set_x11, # only for x13
  benchmarking = rjd3toolkit::set_benchmarking
)

# persephone3 internal spec names
p3_spec_names <- c("outliers")

# the blue of the plots: a dark blue (dodgerblue3), used for the original
# series, the confidence band and everything else that is plotted in blue
p3_blue <- "#1874CD"

# the same blue as the components of an rgba() colour (with the spaces that
# the rgba() colours of the dygraph plots carry)
p3_blue_rgb <- "24, 116, 205"

# default colours for the series of the time series plots; the first five
# entries are the default colours of Highcharts, except that the bright
# Highcharts blue is replaced by the darker p3_blue; named by the series
# labels of the plots
p3_series_colors <- c(
  Original = p3_blue,
  Trend = "#434348",
  `Seasonally Adjusted` = "#90ed7d",
  Forecasts = "#808080",
  `Confidence Band` = p3_blue,
  `Direct SA` = "#434348",
  `Indirect SA` = "#90ed7d",
  `Seasonal Component` = p3_blue,
  `Irregular Component` = "#434348",
  `Calendar Effects` = "#90ed7d"
)

# colours of the spectral plots, one per series type in the order of the
# tsType argument (the first entry is the blue of the other plots)
p3_spec_colors <- c(p3_blue, "#434348", "#8055A4", "#00643C")

# default colours of the outlier types in the time series plots
p3_outlier_colors <- list(AO = "red", LS = "orange", TC = "#8055A4")

# the same colours for the discreet dashed vertical lines that mark the
# outliers below the dots in the time series plots (faded so that they do
# not dominate the plot)
p3_outlier_line_colors <- list(
  AO = "rgba(255, 0, 0, 0.35)",
  LS = "rgba(255, 165, 0, 0.35)",
  TC = "rgba(128, 85, 164, 0.35)"
)

# JS plotter for the confidence band of the forecasts in the dygraph plots:
# fills the polygon between the "lowerci" column and the column of the
# series it is attached to ("upperci", its own column is opts.seriesIndex +
# 1 because the series label renames the column of getLabels()), like the
# band of the forecasts in the highcharter plots. The two columns are drawn
# with strokeWidth 0, so only the band between them is visible.
p3_band_plotter <- htmlwidgets::JS(paste0(
  "function(opts) {",
  "  var g = opts.dygraph, ctx = opts.drawingContext, area = opts.plotArea;",
  "  var labels = g.getLabels();",
  "  var loCol = labels.indexOf('lowerci');",
  "  var hiCol = opts.seriesIndex + 1;",
  "  var rows = [], i, j, lo, hi;",
  "  if (loCol >= 0) {",
  "    for (i = 0; i < g.numRows(); i++) {",
  "      lo = g.getValue(i, loCol); hi = g.getValue(i, hiCol);",
  "      if (lo === null || hi === null || isNaN(lo) || isNaN(hi)) continue;",
  "      rows.push([g.toDomXCoord(g.getValue(i, 0)),",
  "                 g.toDomYCoord(hi), g.toDomYCoord(lo)]);",
  "    }",
  "  }",
  "  if (rows.length > 1) {",
  "    ctx.save();",
  "    ctx.beginPath(); ctx.rect(area.x, area.y, area.w, area.h); ctx.clip();",
  "    ctx.fillStyle = 'rgba(", p3_blue_rgb, ", 0.25)';",
  "    ctx.beginPath();",
  "    ctx.moveTo(rows[0][0], rows[0][1]);",
  "    for (j = 1; j < rows.length; j++) ctx.lineTo(rows[j][0], rows[j][1]);",
  "    for (j = rows.length - 1; j >= 0; j--) ctx.lineTo(rows[j][0], rows[j][2]);",
  "    ctx.closePath(); ctx.fill(); ctx.restore();",
  "  }",
  "}"
))

# CSS that hides the legend entries of the two confidence limit columns
# (their colour is the transparent band colour); the forecast itself has its
# own entry in the legend
p3_band_css <- paste0(
  ".dygraph-legend span[style*='", p3_blue_rgb, "'] {\n",
  "  display: none !important;\n",
  "}\n"
)

# the two invisible series of the confidence band, to be added to a dygraph
# after the series of the central forecast ("ppm_y_f")
p3_band_series <- function(graphObj) {
  band_color <- paste0("rgba(", p3_blue_rgb, ", 0.01)")
  graphObj <- graphObj %>%
    dySeries(
      "lowerci",
      label = "lowerci",
      color = band_color,
      strokeWidth = 0
    ) %>%
    dySeries(
      "upperci",
      label = "Confidence Band",
      color = band_color,
      strokeWidth = 0,
      plotter = p3_band_plotter
    )
  graphObj %>% dyCSS(textConnection(p3_band_css))
}

# the dygraph widgets: the highlight point at the position of the mouse is
# switched off (like in the plotly plots the cursor line alone marks the
# position, and the dygraphs version used here has no option to turn the point
# off, so its size is set to zero), they get a fluid width (without it the
# htmlwidgets sizing policy keeps the fixed 960px default width of dygraphs, so
# in small windows the right part of the plot is cut off) and, shown on their
# own, the height of the window (p3_fit_js())
# the hovered series is drawn bold (highlightSeriesOpts) while the other
# series fade into a whitish background (highlightSeriesBackgroundAlpha), as
# in the plotly plots (p3_hover_highlight()); the colour of the invisible
# series of the confidence band already carries its alpha, so they stay
# invisible when they are drawn bold.
p3_fluid <- function(graphObj) {
  graphObj$x$options$highlightCircleSize <- 0
  graphObj$x$options$highlightSeriesOpts <- list(strokeWidth = 3)
  graphObj$x$options$highlightSeriesBackgroundAlpha <- 0.15
  size <- p3_plot_size()
  width <- size$width %||% "100%"
  graphObj$width <- width
  graphObj$height <- size$height
  if (!is.null(graphObj$height)) return(graphObj)
  graphObj$x$sizingPolicy <- htmlwidgets::sizingPolicy(
    defaultWidth = width, defaultHeight = "auto",
    viewer.defaultWidth = width, viewer.defaultHeight = "auto",
    browser.defaultWidth = width, browser.defaultHeight = "auto",
    knitr.defaultWidth = width, knitr.defaultHeight = "auto",
    padding = 0, browser.fill = TRUE, viewer.fill = TRUE,
    knitr.figure = FALSE
  )
  if (p3_plot_fill()) graphObj <- p3_add_hook(graphObj, "render", p3_fit_js())
  graphObj
}

# is a plot shown on its own, i.e. in the viewer/plot pane of RStudio or
# Positron? In a knitr document (report, vignette) and in a running shiny app
# (the dashboards) the plot is one element among others on a page and has to
# keep its own size. The option "persephone3.plotFill" (default TRUE) switches
# the filling off.
p3_plot_fill <- function() {
  isTRUE(getOption("persephone3.plotFill", TRUE)) &&
    !isTRUE(getOption("knitr.in.progress", FALSE)) &&
    !isTRUE(tryCatch(shiny::isRunning(), error = function(e) FALSE))
}

# size of a plot as set by the options "persephone3.plotHeight" and
# "persephone3.plotWidth" (pixels or any css length). A plot with a size of its
# own is never fitted to the window: this is the way to get a plot of a fixed,
# always visible size in a host that gives the widget no height of its own.
p3_plot_size <- function() {
  one <- function(x) {
    if (length(x) == 1 && !is.null(x) && !is.na(x)) x else NULL
  }
  list(
    height = one(getOption("persephone3.plotHeight", NULL)),
    width = one(getOption("persephone3.plotWidth", NULL))
  )
}

# fluid sizing for the plotly widgets (as p3_fluid() for the dygraph widgets):
# plotly's own sizing fixes the widget at 400px height, so in a small plot pane
# only a part of the plot is visible and the rest has to be scrolled. Shown on
# its own the widget gets the height of the window (p3_fit_js()), and with
# config(responsive = TRUE) plotly redraws the plot whenever the window is
# resized. grid is the c(rows, cols) panel grid of the figure (c(1, 1) for a
# single plot): a figure of several panels only gets the fraction rows/cols of
# the height that keeps the height:width ratio of a single panel (p3_fit_js).
p3_fluid_plotly <- function(p, grid = c(1, 1)) {
  factor <- grid[1] / grid[2]
  p <- plotly::config(p, responsive = TRUE, displaylogo = FALSE)
  size <- p3_plot_size()
  if (!is.null(size$width)) p$width <- size$width
  if (!is.null(size$height)) {
    h <- size$height
    num <- suppressWarnings(as.numeric(h))
    if (factor != 1 && !is.na(num)) {
      h <- num * factor
    }
    p$height <- h
    return(p)
  }
  if (p3_plot_fill()) {
    p$height <- "100%"
    p$sizingPolicy$padding <- 0
    p$sizingPolicy$viewer$padding <- 0
    p$sizingPolicy$viewer$fill <- TRUE
    p <- p3_plotly_css(p, "p3-plotly-hover.css")
    p <- p3_add_hook(p, "render", p3_style_js("p3-plotly-hover.css"))
    p <- p3_add_hook(p, "render", p3_fit_js(factor = factor))
  }
  p
}

# code that htmlwidgets runs when the widget has been drawn, as the "render"
# hook of the widget. The hooks are part of the json of the widget, so they
# arrive wherever the widget arrives - unlike the stylesheets of the package,
# which are separate files that a host can leave out.
p3_add_hook <- function(w, hook, code) {
  w$jsHooks[[hook]] <- c(
    w$jsHooks[[hook]],
    list(list(code = htmlwidgets::JS(code)))
  )
  w
}

# the height of a plot that is shown on its own: the space between the top of
# the widget and the bottom of the window, so that the plot fills the plot pane
# of RStudio or Positron and a small pane gives a small plot. The panes do not
# honour the htmlwidgets sizing policy (Positron rebuilds the widget in a page
# of its own, in which the htmlwidgets container that the policy applies to
# does not exist), so a height of 100% has nothing to be 100% of and the plot
# keeps its own size, which has to be scrolled then. The height is written in
# pixels by the widget itself, and the resize event lets the binding of the
# widget draw the chart again at that size (htmlwidgets runs its own handler
# for it, the resizeHandler in www/htmlwidgets.js of the htmlwidgets package).
# At first the whole space is used; then the height is corrected by the space
# the host page still shows under the widget (its margins and paddings), so
# that nothing has to be scrolled - but by a limited amount only, so that a
# page with other content below the plot does not make the plot tiny. The
# smallest height a plot is given is minHeight: it is only reached in a window
# that is smaller than that, and it keeps the plot from collapsing completely.
# For a figure of several equally sized panels (see plotSpectrum) the space
# is multiplied by the factor rows/cols of the panel grid, so that every
# panel keeps the height:width ratio a plot of a single panel would have.
p3_fit_js <- function(minHeight = 60, factor = 1) {
  sprintf(paste(
    "function (el, x) {",
    "  var minHeight = %s;",
    "  var factor = %s;",
    "  var de = document.documentElement;",
    "  var busy = false;",
    "  var apply = function (h) {",
    "    el.setAttribute('data-p3-height', String(h));",
    "    el.style.height = h + 'px';",
    "    window.dispatchEvent(new Event('resize'));",
    "  };",
    "  var fit = function () {",
    "    if (busy) { return; }",
    "    var vh = de.clientHeight || window.innerHeight || 0;",
    "    if (!vh) { return; }",
    "    var top = 0, node = el;",
    "    while (node) { top += node.offsetTop; node = node.offsetParent; }",
    "    var h = Math.floor((vh - top) * factor);",
    "    if (!isFinite(h) || h <= 0) { return; }",
    "    busy = true;",
    "    h = Math.max(minHeight, h);",
    "    apply(h);",
    "    setTimeout(function () {",
    "      var max = Math.max(24, Math.floor(vh * 0.08));",
    "      var bottom = el.getBoundingClientRect().bottom +",
    "        (window.scrollY || de.scrollTop || 0);",
    "      var rest = Math.min(max, de.scrollHeight - bottom);",
    "      var next = Math.max(minHeight, h - Math.max(0, Math.ceil(rest)));",
    "      if (next < h) { apply(next); }",
    "      busy = false;",
    "    }, 30);",
    "  };",
    "  fit();",
    "  setTimeout(fit, 60);",
    "  setTimeout(fit, 250);",
    "  window.addEventListener('resize', fit);",
    "}",
    sep = "\n"
  ), as.numeric(minHeight), max(0.01, as.numeric(factor)))
}

# a stylesheet of inst/css as a hook: the rule of p3_hover() for the markers at
# the hovered points is a css rule, and css that comes with the widget as a
# separate file does not always reach the pane. Only the rules of the package
# are added this way, and only once to a page.
p3_style_js <- function(files) {
  css <- paste(vapply(files, function(f) {
    x <- paste(readLines(system.file("css", f, package = "persephone3"),
                        warn = FALSE), collapse = " ")
    gsub("/\\*.*?\\*/", " ", x, perl = TRUE)
  }, ""), collapse = " ")
  paste(
    "function (el, x) {",
    "  if (document.getElementById('p3-plot-css')) { return; }",
    "  var s = document.createElement('style');",
    "  s.id = 'p3-plot-css';",
    paste0("  s.textContent = ", p3_js_string(css), ";"),
    "  document.head.appendChild(s);",
    "}",
    sep = "\n"
  )
}

# a string as a javascript string literal
p3_js_string <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("'", "\\'", x, fixed = TRUE)
  x <- gsub("[\r\n\t]+", " ", x)
  paste0("'", x, "'")
}

# add stylesheets from inst/css to a plotly widget: the html of the widget is
# written by plotly, so extra styles have to come as a html dependency of the
# widget. Several calls add to the same dependency.
p3_plotly_css <- function(p, files) {
  name <- "persephone3-plotly-css"
  old <- Filter(function(d) identical(d$name, name), p$dependencies)
  p$dependencies <- Filter(function(d) !identical(d$name, name), p$dependencies)
  if (length(old) > 0) {
    files <- unique(c(unlist(lapply(old, function(d) d$stylesheet)), files))
  }
  dep <- htmltools::htmlDependency(
    name = name, version = "1.0",
    src = list(file = system.file("css", package = "persephone3")),
    stylesheet = files, all_files = FALSE
  )
  p$dependencies <- c(p$dependencies, list(dep))
  p
}

# the layout of a plotly widget as plotly_build() sees it: plotly collects the
# updates of the layout() calls in x$layoutAttrs and merges them into the
# layout, an axis given twice therefore keeps the keys that are not mentioned.
# modifyList() would replace a whole axis list, so the entries are merged
# recursively here.
p3_layout <- function(p) {
  merge_list <- function(a, b) {
    for (nm in names(b)) {
      a[[nm]] <- if (is.list(a[[nm]]) && is.list(b[[nm]])) {
        merge_list(a[[nm]], b[[nm]])
      } else {
        b[[nm]]
      }
    }
    a
  }
  attrs <- p$x$layoutAttrs[vapply(p$x$layoutAttrs, is.list, TRUE)]
  lay <- if (is.list(p$x$layout)) list(p$x$layout) else list()
  Reduce(merge_list, c(lay, attrs))
}

# the vertical line at the position of the mouse, as the crosshair of the
# highcharter plots and the cursor line of the dygraphs plots: plotly draws it
# when the x axis has spikes on and, with spikesnap = "cursor", the line
# follows the mouse instead of snapping to the nearest observation. The markers
# plotly draws at the hovered point of every series are hidden by the stylesheet
# p3-plotly-hover.css (plotly has no option for this), so the mouse position is
# marked by the line alone. All x axes of the widget get the spikes, so that
# also the panels of a subplot figure are marked.
p3_hover <- function(p, color = "grey", width = 1) {
  lay <- p3_layout(p)
  axes <- grep("^xaxis(\\d*)$", names(lay), value = TRUE)
  # subplot() gives the traces their x axis without a layout entry
  tr <- vapply(p$x$data, function(t) t$xaxis %||% "x", "")
  tr <- ifelse(tr == "x", "xaxis", sub("^x", "xaxis", tr))
  axes <- union(axes, tr[grepl("^xaxis(\\d*)$", tr)])
  updates <- lapply(axes, function(ax) {
    list(
      showspikes = TRUE, spikemode = "across", spikesnap = "cursor",
      spikedash = "solid", spikethickness = width, spikecolor = color
    )
  })
  names(updates) <- axes
  p <- do.call(plotly::layout, c(list(p), updates))
  p3_plotly_css(p, "p3-plotly-hover.css")
}

# highlight the series the cursor is on, as in the default plot of plotly:
# the hovered trace is drawn bold (a line.width on top of its own) and all
# other traces fade to the given opacity; when the cursor leaves the plot,
# everything is restored or the pinned series is restored (see below). plotly
# has no layout option for this, so a hook registers plotly's own hover
# events: with hovermode "x unified" the hover event lists the points of all
# series at the hovered x, and the series to keep is the one whose hovered
# point is closest to the cursor (compared in the pixels of the plot area,
# via the axis' l2p()); for a single point (native "closest" hover) that
# point's trace is kept without any measuring. A click on a series pins its
# highlighting (it stays bold when the cursor moves away); clicking on
# another series pins that one, and clicking on empty space of the plot area
# lifts the pin and shows all series with equal strength again. The
# highlighting requires that the cursor touches the line: a series is only
# kept when its hovered point is at most maxdist pixels from the cursor, so
# that the unified hover (whose vertical line is drawn everywhere in the
# plot) does not highlight anything where no series crosses the cursor. The
# hook is attached once per plotly widget (gd.__p3hl) and travels with the
# widget like the styles of p3_style_js().
p3_hover_highlight <- function(p, fade = 0.15, bold = 0.9, maxdist = 10) {
  p3_add_hook(p, "render", sprintf(p3_highlight_js, fade, bold, maxdist))
}

# the code of p3_hover_highlight() with the fade opacity, the extra line
# width of the bold line and the touch distance as the three %s
p3_highlight_js <- paste(
  "function (el, x) {",
  # the graph div is the widget container itself (plotly's htmlwidget
  # factory renders into the container), with a nested div in some hosts;
  # when the hook runs before plotly has initialised the div, it is retried
  # for a few seconds
  "  var getGd = function () {",
  "    if (el.classList && el.classList.contains('js-plotly-plot')) {",
  "      return el;",
  "    }",
  "    return el.querySelector('.js-plotly-plot');",
  "  };",
  "  var tries = 0;",
  "  var init = function () {",
  "    var gd = getGd();",
  "    if (!gd || typeof gd.on !== 'function') {",
  "      if (tries++ < 80) { setTimeout(init, 50); }",
  "      return;",
  "    }",
  "    if (gd.__p3hl === true) { return; }",
  "    gd.__p3hl = true;",
  "    var fade = %s, bold = %s, maxd = %s, keep = null, pinned = null;",
  # the line widths of the traces as they were built (restyle() changes
  # gd.data, so the bold width is always computed from this base)
  "    var base = gd.data.map(function (t) {",
  "      return (t.line && typeof t.line.width === 'number')",
  "        ? t.line.width : null;",
  "    });",
  "    var restyleKeep = function (k) {",
  "      var op = gd.data.map(function (t, i) {",
  "        return (k === null || i === k) ? 1 : fade;",
  "      });",
  "      window.Plotly.restyle(gd, { opacity: op });",
  "      var idx = [], wid = [];",
  "      base.forEach(function (lw, i) {",
  "        if (lw === null) { return; }",
  "        idx.push(i);",
  "        wid.push(i === k ? Math.min(5, lw + bold) : lw);",
  "      });",
  "      if (idx.length > 0) {",
  "        window.Plotly.restyle(gd, { 'line.width': wid }, idx);",
  "      }",
  "    };",
  "    var setKeep = function (k) {",
  "      if (k === keep) { return; }",
  "      keep = k;",
  "      restyleKeep(k);",
  "    };",
  "    var nearest = function (ev) {",
  "      var pts = (ev && ev.points) || [];",
  "      if (pts.length === 0) { return null; }",
  "      var wanted = pts[0].curveNumber;",
  "      if (pts.length > 1 && ev.event) {",
  "        var cy = ev.event.clientY - gd.getBoundingClientRect().top;",
  "        var best = Infinity;",
  "        for (var j = 0; j < pts.length; j++) {",
  "          try {",
  "            var pt = pts[j];",
  "            var ax = gd._fullLayout[String(pt.data.yaxis || 'y')",
  "              .replace(/^y/, 'yaxis')];",
  "            var d = Math.abs(ax._offset + ax.l2p(pt.y) - cy);",
  "            if (isFinite(d) && d < best) {",
  "              best = d; wanted = pt.curveNumber;",
  "            }",
  "          } catch (e) { /* keep the first point of the event */ }",
  "        }",
  "        // only a series the cursor touches is highlighted",
  "        if (best > maxd) { return null; }",
  "      }",
  "      return wanted;",
  "    };",
  "    gd.on('plotly_hover', function (ev) { setKeep(nearest(ev)); });",
  "    gd.on('plotly_unhover', function () { setKeep(pinned); });",
  "    gd.on('plotly_click', function (ev) {",
  "      var k = nearest(ev);",
  "      if (k === null) { return; }",
  "      pinned = k;",
  "      gd.__p3clicked = true;",
  "      setKeep(k);",
  "    });",
  "    // a click on the empty space of the plot area (not on a series, not",
  "    // on the legend, not after a drag like a zoom) lifts the pin",
  "    el.addEventListener('click', function (e) {",
  "      if (gd.__p3clicked) { gd.__p3clicked = false; return; }",
  "      var t = e.target;",
  "      while (t && t !== el) {",
  "        if (t.classList && t.classList.contains('legend')) { return; }",
  "        t = t.parentNode;",
  "      }",
  "      var s = gd.__p3down;",
  "      if (s && (Math.abs(e.clientX - s.x) > 4 ||",
  "        Math.abs(e.clientY - s.y) > 4)) {",
  "        return;",
  "      }",
  "      if (pinned !== null) { pinned = null; setKeep(null); }",
  "    });",
  "    el.addEventListener('mousedown', function (e) {",
  "      gd.__p3down = { x: e.clientX, y: e.clientY };",
  "    });",
  "  };",
  "  init();",
  "}",
  sep = "\n"
)

# a clear frame around every panel of a plotly plot: the axis lines at the
# bottom and left plus their mirrors at the top and right (the frame that the
# ggplot2 theme_bw() panels have, which the ggplotly conversion drops)
p3_frame <- function(p, color = "black", width = 1) {
  lay <- p3_layout(p)
  axes <- grep("^[xy]axis(\\d*)$", names(lay), value = TRUE)
  updates <- lapply(axes, function(ax) {
    list(showline = TRUE, mirror = TRUE, linewidth = width,
         linecolor = color)
  })
  names(updates) <- axes
  do.call(plotly::layout, c(list(p), updates))
}

# interpolation of the observations on a finer time grid; two methods:
# "monotone" (default) is a monotone piecewise cubic Hermite interpolation
# (Fritsch-Carlson, as MATLAB's pchip): smooth, but it cannot overshoot the
# data or oscillate between observations, so the line does not wobble when
# the plot is enlarged. "spline" is the not-a-knot cubic spline (the same
# family of curves as the smoothed lines of Highcharts with
# curveType = "spline"), which can slightly overshoot the sharpest peaks and
# produce small ripples
dySmooth <- function(x, y, xout, method = "monotone") {
  if (identical(method, "spline")) {
    return(stats::spline(x, y, xout = xout)$y)
  }
  n <- length(x)
  if (n < 3) {
    return(stats::approx(x, y, xout = xout)$y)
  }
  h <- diff(x)
  delta <- diff(y) / h
  # slopes at the nodes: weighted harmonic mean of the slopes of the two
  # neighbouring intervals (Fritsch-Carlson), zero at local extrema and
  # flat spots, one-sided at the ends
  d <- numeric(n)
  int <- 2:(n - 1)
  dm1 <- delta[int - 1]
  dm2 <- delta[int]
  ok <- dm1 * dm2 > 0
  w1 <- 2 * h[int] + h[int - 1]
  w2 <- h[int] + 2 * h[int - 1]
  d[int] <- ifelse(ok, (w1 + w2) / (w1 / dm1 + w2 / dm2), 0)
  d[int][!is.finite(d[int])] <- 0
  d[1] <- delta[1]
  d[n] <- delta[n - 1]
  # evaluate the piecewise cubic Hermite polynomial
  j <- pmin(pmax(findInterval(xout, x), 1), n - 1)
  s <- pmin(pmax((xout - x[j]) / h[j], 0), 1)
  h00 <- (1 + 2 * s) * (1 - s)^2
  h10 <- s * (1 - s)^2
  h01 <- s^2 * (3 - 2 * s)
  h11 <- s^2 * (s - 1)
  h00 * y[j] + h10 * h[j] * d[j] + h01 * y[j + 1] + h11 * h[j] * d[j + 1]
}

# normalize the smooth argument of the plot functions: TRUE (monotone,
# default), FALSE (no smoothing) or "spline" (the overshooting not-a-knot
# spline)
smoothMethod <- function(smooth) {
  if (is.logical(smooth)) {
    if (isTRUE(smooth)) "monotone" else FALSE
  } else if (identical(smooth, "spline")) {
    "spline"
  } else if (identical(smooth, "monotone")) {
    "monotone"
  } else if (isFALSE(smooth)) {
    FALSE
  } else {
    stop('smooth must be TRUE, FALSE or "spline"', call. = FALSE)
  }
}

# is the smoothed line drawn at all (for the smooth argument of the plot
# functions, see smoothMethod)
isSmoothed <- function(smooth) {
  !isFALSE(smoothMethod(smooth))
}

# give the panels of a plotly::subplot() figure y-axis labels (subplot()
# drops the per-panel axis titles, so the labels are set on the composed
# figure after the axes have been renumbered); the panels are ordered like the
# labels: rows from top to bottom, inside a row from left to right
panelYlabs <- function(p, labs, standoff = 8, font = NULL) {
  lay <- p3_layout(p)
  ys <- grep("^yaxis(\\d*)$", names(lay), value = TRUE)
  # name of the layout entry of the x axis a y axis is anchored to
  xAxisKey <- function(ay) {
    an <- lay[[ay]]$anchor %||% "x"
    if (identical(an, "")) "xaxis" else sub("^x", "xaxis", an)
  }
  ord <- order(
    -vapply(ys, function(a) lay[[a]]$domain[1], 0),
    vapply(ys, function(a) lay[[xAxisKey(a)]]$domain[1], 0)
  )
  ys <- ys[ord]
  if (length(ys) < length(labs)) {
    stop("fewer panels than labels", call. = FALSE)
  }
  updates <- lapply(labs, function(lb) {
    list(title = list(text = lb, standoff = standoff, font = font))
  })
  names(updates) <- ys
  do.call(plotly::layout, c(list(p), updates))
}

# give the panels of a plotly::subplot() figure titles above their panels
# (subplot() drops the titles of the single figures, and its own title
# argument is broken in the plotly versions we use, so the titles are added
# as annotations of the composed figure at the centre of each panel's x
# axis domain and above the top of the y axis domain of its row). subplot
# numbers the axes of the panels in the order of the figures, row by row
# from top to bottom, so the geometry is read from the layout of the
# composed widget (which holds the domains of every panel, while the per
# panel layouts in layoutAttrs would only carry the plain "xaxis"/"yaxis"
# keys and a deep merge of them would mix the panels up).
panelTitles <- function(p, titles, size = 12) {
  lay <- p$x$layout
  ann <- lapply(seq_along(titles), function(i) {
    s <- if (i > 1L) as.character(i) else ""
    xdm <- lay[[paste0("xaxis", s)]]$domain
    ydm <- lay[[paste0("yaxis", s)]]$domain %||% lay$yaxis$domain
    list(
      text = titles[i], x = mean(xdm), xref = "paper",
      y = ydm[2] + 0.012, yref = "paper", xanchor = "center",
      yanchor = "bottom", showarrow = FALSE, font = list(size = size)
    )
  })
  p %>% plotly::layout(annotations = ann)
}

# decimal time (as in time(ts)) to Date: the observation times map to the
# first day of the respective month/quarter/half year, points in between are
# interpolated linearly inside the period; needed because smoothed series are
# evaluated on a finer time grid. The period index is determined by flooring
# (except for grid points equal to a period start, where the decimals of the
# time index are not exact): rounding would send the second half of every
# period to the start of the next one, which makes the resulting dates
# non-monotonic (repeated dates at every period boundary) and produces
# vertical lines in the dygraphs/plotly plots when zooming in.
timesToDates <- function(tt, freq) {
  stopifnot(all(freq %in% c(1L, 2L, 4L, 12L)))
  months_per_period <- as.integer(12 / freq)
  v <- as.numeric(tt) * freq
  vr <- round(v)
  q <- ifelse(abs(v - vr) < 1e-6, vr, floor(v)) # index of the current period
  r <- as.numeric(tt) - q / freq    # offset inside the period
  y <- floor(q / freq)
  m <- q %% freq # 0-based period within the year
  mon <- m * months_per_period + 1L
  d0 <- as.Date(sprintf("%04d-%02d-01", y, mon))
  mon1 <- mon + months_per_period
  y1 <- y + as.integer(mon1 > 12L)
  mon1 <- ifelse(mon1 > 12L, mon1 - 12L, mon1)
  d1 <- as.Date(sprintf("%04d-%02d-01", y1, mon1))
  as.Date(d0 + pmin(pmax(r * freq, 0), 1 - 1e-9) * as.numeric(d1 - d0))
}

# Prepare input for dygraphs::dygraph() from a ts/mts object: optionally
# cubic spline interpolation of the columns in smooth_cols on a finer time
# grid (smoothed lines between the observations), and conversion of the time
# index to dates. Columns that are not smoothed (outlier markers, forecast
# bands) keep their original values, placed on the corresponding points of
# the finer grid.
# Each smoothed column is evaluated only inside its own non-missing time
# window: the interpolation is not extrapolated beyond the first and last
# observation of that column. Otherwise, columns that are shorter than the
# common time index (as the estimated series next to the forecasts, which
# extend the index by the forecast horizon) would blow up at the ends of the
# plot and determine the scaling of the y-axis.
dyData <- function(m, smooth = TRUE, smooth_cols = NULL, n = 8) {
  method <- smoothMethod(smooth)
  smooth <- !isFALSE(method)
  ti <- as.numeric(stats::time(m))
  freq <- stats::frequency(m)
  if (!stats::is.ts(m)) {
    stop("dyData() expects a ts/mts object")
  }
  if (!stats::is.mts(m)) {
    nm <- colnames(m)[1]
    if (is.null(nm) || is.na(nm)) {
      nm <- "V1"
    }
    m <- matrix(as.numeric(m), ncol = 1, dimnames = list(NULL, nm))
  }
  if (is.null(colnames(m))) {
    colnames(m) <- paste0("V", seq_len(ncol(m)))
  }
  if (smooth && length(ti) > 4) {
    tnew <- seq(ti[1], ti[length(ti)], length.out = (length(ti) - 1) * n + 1)
    grid <- 1 + n * (seq_along(ti) - 1) # index of the original points in tnew
    out <- matrix(NA_real_, nrow = length(tnew), ncol = ncol(m),
                  dimnames = list(NULL, colnames(m)))
    for (j in seq_len(ncol(m))) {
      yj <- as.numeric(m[, j])
      if (colnames(m)[j] %in% smooth_cols && sum(!is.na(yj)) > 3) {
        tj <- ti[!is.na(yj)]
        yjv <- yj[!is.na(yj)]
        # no extrapolation: keep the interpolation within the non-missing range
        inw <- tnew >= min(tj) - 1e-9 & tnew <= max(tj) + 1e-9
        out[inw, j] <- dySmooth(tj, yjv, tnew[inw], method = method)
      } else {
        out[grid, j] <- yj
      }
    }
    ti <- tnew
    m <- out
  }
  cbind(
    data.frame(Date = timesToDates(ti, freq)),
    as.data.frame(m, check.names = FALSE)
  )
}
