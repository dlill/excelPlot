# -------------------------------------------------------------------------#
# Excel export pipeline ----
# -------------------------------------------------------------------------#

#' Title
#'
#' @param d
#' @param headerRowStyle
#'
#' @returns
#' @export
#'
#' @examples
parseTable <- function(d, headerRowStyle) {

  d <- data.table::copy(d)
  if (anyDuplicated(names(d))) {
    stop("d has duplicated names. This is not supported. Duplicated names: ",
         "\n", paste0("  - ", unique(names(d)[duplicated(names(d))]), collapse = "\n"))
  }

  # Add row and col indices
  d[,`:=`(ROWID = (1:.N) + 1)]
  d <- data.table::melt(d,id.vars = "ROWID", variable.name = "VARIABLE", variable.factor = FALSE, value.name = "VALUE")
  d[,`:=`(COLID = as.numeric(factor(VARIABLE, unique(VARIABLE))))]

  # Use colnames as first row
  d <- data.table::rbindlist(list(
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

  d
}

#' Title
#'
#' @param dParsed
#' @param dwidths
#' @param dheights
#' @param FLAGaddBorders
#'
#' @returns
#' @export
#'
#' @examples
populateExcel <- function(dParsed, dwidths, dheights, FLAGaddBorders) {
  wb <- openxlsx::createWorkbook()

  sheet <- "Sheet1"
  openxlsx::addWorksheet(wb = wb, sheetName = sheet)

  # Not vectorized, but the tables aren't large anyway.
  i <- (seq_len(nrow(dParsed)))[[1]]
  for (i in seq_len(nrow(dParsed))) {
    content <- dParsed[i]

    if (content$ISPLOT) {
      openxlsx::insertImage(wb = wb,
                            sheet = sheet,
                            file = content$FILE[[1]],
                            startRow = content$ROWID,
                            startCol = content$COLID,
                            width = content$WIDTHCM,
                            height = content$HEIGHTCM,
                            units = "cm",
                            dpi = content$SPEC[[1]]$resolution)
    } else {
      openxlsx::writeData(wb = wb,
                          sheet = sheet,
                          x = content$SPEC[[1]]$text,
                          startRow = content$ROWID,
                          startCol = content$COLID,
                          colNames = FALSE)

      # styleList is defined within this package.
      openxlsx::addStyle(wb = wb,
                         sheet = sheet,
                         style = styleList[[content$SPEC[[1]]$style]],
                         rows = content$ROWID,
                         cols = content$COLID)
    }

  }

  # Finalize layout
  openxlsx::pageSetup(wb = wb, sheet = sheet, fitToWidth = TRUE, fitToHeight = TRUE) # So pdf export on is done on a single page
  openxlsx::freezePane(   wb, sheet = sheet, firstRow = TRUE, firstCol = TRUE)
  openxlsx::setColWidths( wb, sheet = sheet, cols = dwidths$COLID, widths = dwidths$WIDTHCM * 5.3)
  openxlsx::setRowHeights(wb, sheet = sheet, rows = dheights$ROWID, heights = dheights$HEIGHTCM / 2.54 * 72 * 1.05)

  if (FLAGaddBorders) {openxlsx::addStyle(wb = wb, sheet = sheet, style = openxlsx::createStyle(border = "TopBottomLeftRight"),
                                          rows = unique(dParsed$ROWID), cols = unique(dParsed$COLID), gridExpand = TRUE, stack = TRUE)}

  wb
}

#' Title
#'
#' @param d
#' @param filename
#' @param headerRowStyle
#' @param FLAGaddBorders
#' @param FLAGpdf
#'
#' @returns
#' @export
#'
#' @examples
plotExcel <- function(d, filename, headerRowStyle = "center", FLAGaddBorders = FALSE, FLAGpdf = FALSE, textColWidth = 5) {

  # -------------------------------------------------------------------------#
  # Crunch ----
  # -------------------------------------------------------------------------#
  dParsed <- parseTable(d, headerRowStyle)

  # -------------------------------------------------------------------------#
  # Handle plots ----
  # -------------------------------------------------------------------------#
  # Apply extraction and cropping pipeline
  lapply(dParsed[ISPLOT == TRUE, SPEC], applyPngPipelineOnePage)

  # Get width and height in cm.
  # It is a weird bug of gs that it does not include the dpi in the metadata, and somehow imagemagick can't overwrite the metadata...
  dParsed[ISPLOT == TRUE,c("WIDTHCM", "HEIGHTCM"):=(as.data.table(t(sapply(seq_along(FILE), function(i) {
    pxInfo <- system(paste0('identify -format "%w\n%h" ', FILE[[i]]), intern = TRUE) # x pixels, y pixels
    pxInfo <- as.numeric(pxInfo)
    pxInfo / SPEC[[i]]$resolution * 2.54
  }))))]

  dwidths <- dParsed[,list(WIDTHCM = max(WIDTHCM, na.rm = TRUE)), by = "COLID"]
  dwidths[!is.finite(WIDTHCM),`:=`(WIDTHCM = textColWidth)]
  dheights <- dParsed[,list(HEIGHTCM = max(HEIGHTCM, na.rm = TRUE)), by = "ROWID"]
  dheights[!is.finite(HEIGHTCM),`:=`(HEIGHTCM = 2)]

  # -------------------------------------------------------------------------#
  # Populate the Excel ----
  # -------------------------------------------------------------------------#
  wb <- populateExcel(dParsed = dParsed,
                      dwidths = dwidths,
                      dheights = dheights,
                      FLAGaddBorders = FLAGaddBorders)

  # -------------------------------------------------------------------------#
  # Export ----
  # -------------------------------------------------------------------------#
  if (!dir.exists(dirname(filename))) {dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)}
  filename <- updateLockedFilename(filename)
  t0 <- Sys.time()
  openxlsx::saveWorkbook(wb = wb, file = filename, overwrite = TRUE)
  message("Excel sheet was saved at ", filename, " (", signif(Sys.time() - t0, 2), " s)")

  if (FLAGpdf) {
    system(paste0('LD_LIBRARY_PATH="/usr/lib/libreoffice/program:/usr/lib/x86_64-linux-gnu/:$LD_LIBRARY_PATH" && ',
                  'export LD_LIBRARY_PATH && ', "bash -lic 'libreoffice --headless --convert-to pdf ",normalizePath(filename), " --outdir ", tempdir(), " ", normalizePath(filename), "'"), wait = TRUE)
    system(paste0("evince ", file.path(tempdir(), paste0(tools::file_path_sans_ext(basename(filename)), ".pdf"))), wait = FALSE)
  }

  invisible(filename)

}


# -------------------------------------------------------------------------#
# Excel cell styles ----
# -------------------------------------------------------------------------#

styleList <- list(
  left       = openxlsx::createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = NULL, valign = NULL),
  center     = openxlsx::createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = "center"),
  rotateUp   = openxlsx::createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = "right", valign = "center", textRotation = 90),
  rotateDown = openxlsx::createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = "left" , valign = "center", textRotation = -90))


#' Print available styles
#'
#' @returns
#' @export
#' @md
#' @family
#'
#' @examples
#' availableStyles()
availableStyles <- function() {
  cat("Available styles:\n", paste0(seq_along(styleList), ": ", names(styleList) , collapse = "\n"), sep = "")
}



