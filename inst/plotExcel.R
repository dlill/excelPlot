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
library(openxlsx)
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

# -------------------------------------------------------------------------#
# Set up a table ----
# -------------------------------------------------------------------------#
list.files("inst/exampleData/", full.names = TRUE)
d0 <- data.table(tibble::tribble(
  ~desc, ~bam, ~asdlfkj,
  "Iris Blue::3",  "exampleData//02-Iris-Brewer.pdf::crop x0,75 y0,80"     , "exampleData//02-Iris-Brewer.pdf"     ,
  "Iris stuff::4",  "exampleData//04-IrisMulti.pdf::page 2"       , "exampleData//04-IrisMulti.pdf::page 1"
))


# -------------------------------------------------------------------------#
# Crunch ----
# -------------------------------------------------------------------------#
d <- copy(d0)

d[,`:=`(ROWID = (1:.N) + 1)]
d <- data.table::melt(d,id.vars = "ROWID", variable.name = "VARIABLE", variable.factor = FALSE, value.name = "VALUE")
d[,`:=`(COLID = as.numeric(factor(VARIABLE, unique(VARIABLE))))]

d <- rbindlist(list(
  d[,unique(.SD),.SDcols = c("COLID", "VARIABLE")][,`:=`(ROWID = 1, VALUE = paste0(VARIABLE,"::3"))],
  d
), use.names = TRUE)
d[,`:=`(ID = 1:.N)]

parseSpec <- function(text) {
  if (file.exists(gsub("::.*","", text))) {
    parsePlotSpec(text)
  } else {
    parseTextSpec(text)
  }
}

d[,`:=`(ISPLOT = file.exists(gsub("::.*","", VALUE)))]

d[ISPLOT == TRUE,`:=`(SPEC = lapply(VALUE, function(x) {
  parsePlotSpec(x)
}))]

d[ISPLOT == FALSE,`:=`(SPEC = lapply(VALUE, function(x) {
  parseTextSpec(x)
}))]

d[ISPLOT == TRUE,`:=`(PATHS = lapply(SPEC, function(x) do.call(epFiles, x)))]
d[,`:=`(FILE = sapply(PATHS, function(x) x$tmpPathCommitPageCrop))]

# -------------------------------------------------------------------------#
# Handle plots ----
# -------------------------------------------------------------------------#
# Apply extraction pipeline
lapply(d[ISPLOT == TRUE, SPEC], applyPngPipelineOnePage)

# Get width and height in cm.
# It is a weird bug of gs that it does not include the dpi in the metadata, and somehow imagemagick can't overwrite the metadata...
d[ISPLOT == TRUE,c("WIDTHCM", "HEIGHTCM"):=(as.data.table(t(sapply(seq_along(FILE), function(i) {
  pxInfo <- system(paste0('identify -format "%w\n%h" ', FILE[[i]]), intern = TRUE) # x pixels, y pixels
  pxInfo <- as.numeric(pxInfo)
  pxInfo / SPEC[[i]]$resolution * 2.54
}))))]


# -------------------------------------------------------------------------#
# Handle widths ----
# -------------------------------------------------------------------------#
dwidths <- d[,list(WIDTHCM = max(WIDTHCM, na.rm = TRUE)), by = "COLID"]
dwidths[!is.finite(WIDTHCM),`:=`(WIDTHCM = 10)]
dheights <- d[,list(HEIGHTCM = max(HEIGHTCM, na.rm = TRUE)), by = "ROWID"]
dheights[!is.finite(HEIGHTCM),`:=`(HEIGHTCM = 2)]

# -------------------------------------------------------------------------#
# Populate the Excel ----
# -------------------------------------------------------------------------#
wb <- createWorkbook()

styleList <- list(
  bold          = createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = NULL, valign = NULL),
  boldRot       = createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = NULL, valign = NULL, textRotation = 255),
  boldCenter    = createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = "center", valign = "center"),
  boldCenterRot = createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = "center", valign = "center", textRotation = 255),
  neg  = createStyle(fontColour = rgb(155,0,0,maxColorValue = 255)  , fgFill = rgb(255,199,206,maxColorValue = 255), border = "TopBottomLeftRight", borderColour = rgb(230,230,230,maxColorValue = 255), borderStyle = "thin"),
  pos  = createStyle(fontColour = rgb(0,93,0,maxColorValue = 255)   , fgFill = rgb(198,239,206,maxColorValue = 255), border = "TopBottomLeftRight", borderColour = rgb(230,230,230,maxColorValue = 255), borderStyle = "thin"),
  neu  = createStyle(fontColour = rgb(152,73,29,maxColorValue = 255), fgFill = rgb(255,235,156,maxColorValue = 255), border = "TopBottomLeftRight", borderColour = rgb(230,230,230,maxColorValue = 255), borderStyle = "thin"),
  bas  = createStyle(fontColour = rgb(90,90,90,maxColorValue = 255) , fgFill = rgb(250,250,250,maxColorValue = 255), border = "TopBottomLeftRight", borderColour = rgb(230,230,230,maxColorValue = 255), borderStyle = "thin", textDecoration = "Italic"),
  wrap = createStyle(wrapText = TRUE)
)

sheetName <- "Plots"
addWorksheet(wb = wb, sheetName = sheetName)

# Not vectorized, but the tables aren't large anyway.
i <- (seq_len(nrow(d)))[[1]]
for (i in seq_len(nrow(d))) {
  content <- d[i]

  if (content$ISPLOT) {
    insertImage(wb = wb,
                sheet = sheetName,
                file = content$FILE[[1]],
                startRow = content$ROWID,
                startCol = content$COLID,
                width = content$WIDTHCM,
                height = content$HEIGHTCM,
                units = "cm",
                dpi = content$SPEC[[1]]$resolution)
  } else {
    writeData(wb = wb,
              sheet = sheetName,
              x = content$SPEC[[1]]$text,
              startRow = content$ROWID,
              startCol = content$COLID,
              colNames = FALSE)
    addStyle(wb = wb,
             sheet = sheetName,
             style = styleList[[content$SPEC[[1]]$style]],
             rows = content$ROWID,
             cols = content$COLID)
  }

}

# Layout
freezePane(   wb, sheet = sheetName, firstRow = TRUE, firstCol = TRUE)
setColWidths( wb, sheet = sheetName, cols = dwidths$COLID, widths = dwidths$WIDTHCM * 5.3)
setRowHeights(wb, sheet = sheetName, rows = dheights$ROWID, heights = dheights$HEIGHTCM / 2.54 * 72 * 1.05)

# .. Export -----
filename <- "plots.xlsx"
if (!dir.exists(dirname(filename))) {dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)}
filename <- updateLockedFilename(filename)
saveWorkbook(wb = wb, file = filename, overwrite = TRUE)
message("Excel sheet was saved at ", filename)

invisible(filename)




system(paste0('LD_LIBRARY_PATH="/usr/lib/libreoffice/program:/usr/lib/x86_64-linux-gnu/:$LD_LIBRARY_PATH" && ',
'export LD_LIBRARY_PATH && ', "bash -lic 'libreoffice --headless --convert-to pdf ",normalizePath(filename), " --outdir ", tempdir(), " ", normalizePath(filename), "'"), wait = TRUE)
system(paste0("evince ", file.path(tempdir(), paste0(tools::file_path_sans_ext(basename(filename)), ".pdf"))), wait = FALSE)

#
#

# Exit ----
