
library(DiagrammeR)

grViz("
digraph persephone3 {

  node [shape = box]

  persephone
  persephoneSingle
  multipleTimeSeries
  hierarchicalTimeSeries
  x13Single
  tramoseatsSingle

  # inheritance
  persephoneSingle -> persephone
  multipleTimeSeries -> persephone
  hierarchicalTimeSeries -> multipleTimeSeries
  x13Single -> persephoneSingle
  tramoseatsSingle -> persephoneSingle

  # composition
  multipleTimeSeries -> persephone [label='components', style=dashed]
  hierarchicalTimeSeries -> persephone [label='components', style=dashed]
}
")
