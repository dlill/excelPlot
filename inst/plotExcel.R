#### HEADER ================================================================
#
# plotExcel.R
#
# [PURPOSE]
# Purpose
#
# [AUTHOR]
# Daniel Lill (daniel.lill@intiquan.com)
#
rm(list = ls())
devtools::load_all()
library(data.table)
library(ggplot2)

try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

# -------------------------------------------------------------------------#
# Set up a table ----
# -------------------------------------------------------------------------#
list.files("exampleData/", full.names = TRUE)

d0 <- data.table(tibble::tribble(
  ~desc, ~bam, ~asdlfkj,
  "Iris Blue::3",  "exampleData//02-Iris-Brewer.pdf::crop x0,80 y0,100"     , "exampleData//02-Iris-Brewer.pdf"     ,
  "Iris stuff::4",  "exampleData//04-IrisMulti.pdf::page 2::crop x0,80 y0,100"       , "exampleData//04-IrisMulti.pdf::page 1"
))

FLAGaddBorders <- TRUE
headerRowStyle <- "center"
filename <- "plots.xlsx"

# [ ] >>>> Continue here <<<<<<<<<<< ----
# [ ] turn "crop x0,100 y 0,100" into "xmin 0::xmax 100" etc
# [ ] Add pdf export as option, e.g. FLAGpdf
# [ ] Add diff option

d <- data.table::copy(d0)

plotExcel(d, filename, headerRowStyle = "center", FLAGaddBorders = TRUE, FLAGpdf = TRUE, textColWidth = 3)

# Exit ----
