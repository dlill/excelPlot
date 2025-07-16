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
  "Iris Blue::3",  "exampleData//02-Iris-Brewer.pdf::crop x0,75 y0,80"     , "exampleData//02-Iris-Brewer.pdf"     ,
  "Iris stuff::4",  "exampleData//04-IrisMulti.pdf::page 2"       , "exampleData//04-IrisMulti.pdf::page 1"
))

FLAGaddBorders <- TRUE
headerRowStyle <- "center"

# [ ] >>>> Continue here <<<<<<<<<<< ----
# [ ] turn "crop x0,100 y 0,100" into "xmin 0::xmax 100" etc
# [ ] The sections below are already quite good to turn them into functions.
# [ ] Add pdf export as option, e.g. FLAGpdf
# [ ] Add diff option

# -------------------------------------------------------------------------#
# Crunch ----
# -------------------------------------------------------------------------#
d <- copy(d0)

# Add row and col indices
d[,`:=`(ROWID = (1:.N) + 1)]
d <- data.table::melt(d,id.vars = "ROWID", variable.name = "VARIABLE", variable.factor = FALSE, value.name = "VALUE")
d[,`:=`(COLID = as.numeric(factor(VARIABLE, unique(VARIABLE))))]

# Use colnames as first row
d <- rbindlist(list(
  d[,unique(.SD),.SDcols = c("COLID", "VARIABLE")][,`:=`(ROWID = 1, VALUE = paste0(VARIABLE,"::", headerRowStyle))],
  d
), use.names = TRUE)
d[,`:=`(ID = 1:.N)]

# Parse specs into lists and get certain variables into the main table
d[,`:=`(ISPLOT = file.exists(gsub("::.*","", VALUE)))]
d[ISPLOT == FALSE,`:=`(SPEC = lapply(VALUE, function(x) {parseTextSpec(x)}))]
d[ISPLOT == TRUE,`:=`(SPEC = lapply(VALUE, function(x) {parsePlotSpec(x)}))]
d[ISPLOT == TRUE,`:=`(PATHS = lapply(SPEC, function(x) do.call(epFiles, x)))]
d[,`:=`(FILE = sapply(PATHS, function(x) x$tmpPathCommitPageCrop))]

# -------------------------------------------------------------------------#
# Handle plots ----
# -------------------------------------------------------------------------#
# Apply extraction and cropping pipeline
lapply(d[ISPLOT == TRUE, SPEC], applyPngPipelineOnePage)

# Get width and height in cm.
# It is a weird bug of gs that it does not include the dpi in the metadata, and somehow imagemagick can't overwrite the metadata...
d[ISPLOT == TRUE,c("WIDTHCM", "HEIGHTCM"):=(as.data.table(t(sapply(seq_along(FILE), function(i) {
  pxInfo <- system(paste0('identify -format "%w\n%h" ', FILE[[i]]), intern = TRUE) # x pixels, y pixels
  pxInfo <- as.numeric(pxInfo)
  pxInfo / SPEC[[i]]$resolution * 2.54
}))))]

dwidths <- d[,list(WIDTHCM = max(WIDTHCM, na.rm = TRUE)), by = "COLID"]
dwidths[!is.finite(WIDTHCM),`:=`(WIDTHCM = 10)]
dheights <- d[,list(HEIGHTCM = max(HEIGHTCM, na.rm = TRUE)), by = "ROWID"]
dheights[!is.finite(HEIGHTCM),`:=`(HEIGHTCM = 2)]

# -------------------------------------------------------------------------#
# Populate the Excel ----
# -------------------------------------------------------------------------#
wb <- openxlsx::createWorkbook()

sheetName <- "Plots"
openxlsx::addWorksheet(wb = wb, sheetName = sheetName)

# Not vectorized, but the tables aren't large anyway.
i <- (seq_len(nrow(d)))[[1]]
for (i in seq_len(nrow(d))) {
  content <- d[i]

  if (content$ISPLOT) {
    openxlsx::insertImage(wb = wb,
                sheet = sheetName,
                file = content$FILE[[1]],
                startRow = content$ROWID,
                startCol = content$COLID,
                width = content$WIDTHCM,
                height = content$HEIGHTCM,
                units = "cm",
                dpi = content$SPEC[[1]]$resolution)
  } else {
    openxlsx::writeData(wb = wb,
              sheet = sheetName,
              x = content$SPEC[[1]]$text,
              startRow = content$ROWID,
              startCol = content$COLID,
              colNames = FALSE)
    openxlsx::addStyle(wb = wb,
             sheet = sheetName,
             style = styleList[[content$SPEC[[1]]$style]],
             rows = content$ROWID,
             cols = content$COLID)
  }

}

# Finalize layout
openxlsx::pageSetup(wb = wb, sheet = sheetName, fitToWidth = TRUE, fitToHeight = TRUE) # So pdf export on is done on a single page
openxlsx::freezePane(   wb, sheet = sheetName, firstRow = TRUE, firstCol = TRUE)
openxlsx::setColWidths( wb, sheet = sheetName, cols = dwidths$COLID, widths = dwidths$WIDTHCM * 5.3)
openxlsx::setRowHeights(wb, sheet = sheetName, rows = dheights$ROWID, heights = dheights$HEIGHTCM / 2.54 * 72 * 1.05)

if (FLAGaddBorders) openxlsx::addStyle(wb = wb, sheet = sheetName, style = openxlsx::createStyle(border = "TopBottomLeftRight"), rows = unique(d$ROWID), cols = unique(d$COLID), gridExpand = TRUE, stack = TRUE)


# .. Export -----
filename <- "plots.xlsx"
if (!dir.exists(dirname(filename))) {dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)}
filename <- updateLockedFilename(filename)
openxlsx::saveWorkbook(wb = wb, file = filename, overwrite = TRUE)
message("Excel sheet was saved at ", filename)

invisible(filename)


system(paste0('LD_LIBRARY_PATH="/usr/lib/libreoffice/program:/usr/lib/x86_64-linux-gnu/:$LD_LIBRARY_PATH" && ',
              'export LD_LIBRARY_PATH && ', "bash -lic 'libreoffice --headless --convert-to pdf ",normalizePath(filename), " --outdir ", tempdir(), " ", normalizePath(filename), "'"), wait = TRUE)
system(paste0("evince ", file.path(tempdir(), paste0(tools::file_path_sans_ext(basename(filename)), ".pdf"))), wait = FALSE)


# Exit ----
