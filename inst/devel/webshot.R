# Installiere webshot2 falls nötig
if (!requireNamespace("webshot2", quietly = TRUE)) {
  install.packages("webshot2")
}

# Erstelle einen temporären HTML-Plot und mache Screenshot
p <- obj$plot()

# Speichere als HTML und mache Screenshot
htmlwidgets::saveWidget(p, file = "temp_plot.html", selfcontained = TRUE)

# Mache Screenshot
webshot2::webshot(
  "temp_plot.html",
  "man/figures/README-unnamed-chunk-1-1.png",
  selector = "div.dygraphs"
)

# Temporäre Datei löschen
unlink("temp_plot.html")
