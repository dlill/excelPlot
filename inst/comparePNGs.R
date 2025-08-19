#### HEADER ================================================================
#
# comparePNGs.R
#
# [PURPOSE]
# Develop comparison functionality
#
# [AUTHOR]
# Daniel Lill (daniel.lill@intiquan.com)
#
# [CLEANING]
rm(list = grep("^(\\.input|\\.output)", ls(all.names = TRUE), value = TRUE))
rm(list = ls())
#
# [INPUT]
.input <- "../"
#
# [OUTPUT]
.outputFolder <- "../Output/comparePNGs"
#
# [OTHER]
#
## Preliminaries ====
# Set working directory to script folder
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)), silent = TRUE)
stopifnot(basename(dirname(getwd())) == "excelPlot")

# Default packages (do not load other packages, use "::" instead)
library(dplyr)
library(ggplot2)



# -------------------------------------------------------------------------#
#  ----
# -------------------------------------------------------------------------#

l <- paste0(system.file("exampleData/04-IrisMulti.pdf", package = "excelPlot"), "::page 1")
r <- paste0(system.file("exampleData/04-IrisMulti.pdf", package = "excelPlot"), "::page 2")



# -------------------------------------------------------------------------#
# Exit ----
# -------------------------------------------------------------------------#
