# #### HEADER ================================================================
# #
# # plotExcel.R
# #
# # [PURPOSE]
# # Purpose
# #
# # [AUTHOR]
# # Daniel Lill (daniel.lill@intiquan.com)
# #
# library(data.table)
# library(ggplot2)
#
#
# # -------------------------------------------------------------------------#
# # Set up a table ----
# # -------------------------------------------------------------------------#
# list.files("inst/exampleData/", full.names = TRUE)
# d0 <- data.table(tibble::tribble(
#   ~desc, ~bam, ~asdlfkj,
#   "Iris Blue::rotate 255",  "inst/exampleData//02-Iris-Brewer.pdf::crop x0,75 y0,80"     , "inst/exampleData//02-Iris-Brewer.pdf"     ,
#   "Iris Red::face plain",  "inst/exampleData//03-Iris-scale2.pdf"     , "inst/exampleData//03-Iris-scale2.pdf::crop x0,90 y0,100"     ,
#   "Iris stuff::size 10",  "inst/exampleData//04-IrisMulti.pdf::page 2"       , "inst/exampleData//04-IrisMulti.pdf::page 1"
# ))
#
#
# # -------------------------------------------------------------------------#
# # Crunch ----
# # -------------------------------------------------------------------------#
# d <- copy(d0)
#
# d[,`:=`(ROWID = (1:.N) + 1)]
# d <- data.table::melt(d,id.vars = "ROWID", variable.name = "VARIABLE", variable.factor = FALSE, value.name = "VALUE")
# d[,`:=`(COLID = as.numeric(factor(VARIABLE, unique(VARIABLE))))]
#
# d <- rbindlist(list(
#   d[,unique(.SD),.SDcols = c("COLID", "VARIABLE")][,`:=`(ROWID = 1, VALUE = paste0(VARIABLE,"::face bold::size 16"))],
#   d
# ), use.names = TRUE)
# d[,`:=`(ID = 1:.N)]
#
# parseSpec <- function(text) {
#   if (file.exists(gsub("::.*","", text))) {
#     parsePlotSpec(text)
#   } else {
#     parseTextSpec(text)
#   }
# }
#
# d[,`:=`(ISPLOT = file.exists(gsub("::.*","", VALUE)))]
#
# # -------------------------------------------------------------------------#
# # Handle plots ----
# # -------------------------------------------------------------------------#
#
# dplots <- d[ISPLOT == TRUE]
#
# dplots[,`:=`(SPEC = lapply(dplots$VALUE, function(x) {
#   parsePlotSpec(x)
# }))]
#
#
# # d[ISPLOT == TRUE,names(formals(plotSpec)) := (parsePlotSpec(VALUE)), by = ID]
# # d[ISPLOT == FALSE,names(formals(textSpec)) := (parseTextSpec(VALUE)), by = ID]
#
#
#
#
#
# # Exit ----
