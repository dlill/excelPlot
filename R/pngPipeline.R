
#' Title
#'
#' @param plotSpec
#'
#' @return
#' @export
#'
#' @examples
applyPngPipelineOnePage <- function(plotSpec) {
  t0 <- Sys.time()
  verifyArg(plotSpec$page, expectedLength = 1)
  cat("File ", plotSpec$path, " page ", plotSpec$page, ": ")
  pngPipelineCheckoutToTemp(plotSpec)
  cat(".")
  pngPipelineExtractPage(plotSpec)
  cat(".")
  out <- pngPipelineCrop(plotSpec)
  cat(". ", paste0(signif(Sys.time() - t0,2), " s"),"\n")
  invisible(out)
}



#' Title
#'
#' @param plotSpec
#'
#' @return
#' @export
#'
#' @examples
pngPipelineCheckoutToTemp <- function(plotSpec) {
  files <- do.call(epFiles, plotSpec)
  fileIn = files$path
  fileOut = files$tmpPathCommit

  # Case 1: Nothing to do
  if (idempotencyNoActionRequired(fileIn = fileIn, fileOut = fileOut, commit = plotSpec$commit)) {
    return(fileOut)
  }

  dir.create(dirname(fileOut), showWarnings = FALSE, recursive = TRUE)

  # Case 2: Just copy HEAD to tmp
  if (plotSpec$commit == "HEAD") {
    file.copy(from = fileIn, to = fileOut, overwrite = TRUE)
    return(fileOut)
  }

  # Case 3: Fetch from commit
  system(paste0("cd ", dirname(fileIn), " && git show ", plotSpec$commit, ":./", basename(fileIn), " > ", fileOut))

  fileOut

}


#' Title
#'
#' @param plotSpec
#'
#' @return
#' @export
#'
#' @examples
pngPipelineExtractPage <- function(plotSpec) {

  files <- do.call(epFiles, plotSpec)
  fileIn = files$tmpPathCommit
  fileOut = files$tmpPathCommitPage

  # Case 1: Nothing to do
  if (idempotencyNoActionRequired(fileIn = fileIn, fileOut = fileOut, commit = plotSpec$commit)) {
    return(fileOut)
  }

  dir.create(dirname(fileOut), showWarnings = FALSE, recursive = TRUE)

  # Case 2: File is png already
  if (tools::file_ext(fileIn) == "png") {
    file.copy(from = fileIn, to = fileOut, overwrite = TRUE)
    return(fileOut)
  }

  # Case 3: Extract page
  cmd <- paste(
    "gs",
    "-dNOPAUSE -dBATCH -dSAFER",
    "-sDEVICE=pngalpha",
    paste0("-dFirstPage=", plotSpec$page, " -dLastPage=", plotSpec$page),
    paste0("-r", plotSpec$resolution),
    "-dPngUsePhysicalDimensions=true",
    paste0("-sOutputFile=", shQuote(fileOut)),
    shQuote(fileIn)
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  # Now fix the missing dpi metadata which gs does not write
  # cmd <- paste("mogrify -units PixelsPerInch -density ", plotSpec$resolution, shQuote(fileOut))
  # system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  fileOut
}

#' Title
#'
#' @param plotSpec
#'
#' @return
#' @export
#'
#' @examples
pngPipelineCrop <- function(plotSpec) {
  files <- do.call(epFiles, plotSpec)
  fileIn = files$tmpPathCommitPage
  fileOut = files$tmpPathCommitPageCrop

  # Case 1: Nothing to do
  if (idempotencyNoActionRequired(fileIn = fileIn, fileOut = fileOut, commit = plotSpec$commit)) {
    return(fileOut)
  }

  # Case 2: Crop
  dir.create(dirname(fileOut), showWarnings = FALSE, recursive = TRUE)
  xmin <- plotSpec$xmin
  xmax <- plotSpec$xmax
  ymin <- plotSpec$ymin
  ymax <- plotSpec$ymax

  cmd <- paste(
    "convert",
    shQuote(fileIn),
    "-crop", paste0(xmax-xmin,"%x",ymax-ymin,"%+",xmin,"%+%", ymin,"%"),
    "+repage",
    shQuote(fileOut)
  )

  hasFailed <- system(cmd)
  if(hasFailed != 0) {warning("Cropping failed for ", plotSpec$path, ", page", page)}
  fileOut

}




#' Title
#'
#' @param path
#' @param commit
#' @param page
#' @param xmin
#' @param xmax
#' @param ymin
#' @param ymax
#' @param resolution In dpi
#'
#' @return
#' @export
#'
#' @examples
#' path <- system.file("exampleData/01-Iris.pdf", package = "excelPlot")
#' plotSpec <- plotSpec(path, commit = getCommit(path), page = allpage(path), xmin = 10, ymax = 90)
plotSpec <- function(path, commit = "HEAD", page = 1, xmin = 0, xmax = 100, ymin = 0, ymax = 100, resolution = 100) {

  verifyArg(path, expectedClass = "character", expectedLength = 1)
  verifyArg(commit, expectedClass = "character", expectedLength = 1)
  verifyArg(page, expectedClass = "numeric", expectedLength = 1)
  verifyArg(resolution, expectedClass = "numeric", expectedLength = 1)

  xmin <- round(xmin)
  xmax <- round(xmax)
  ymin <- round(ymin)
  ymax <- round(ymax)

  verifyArg(xmin, expectedTestFun = function(x) data.table::between(x, 0, 100, incbounds = TRUE))
  verifyArg(xmax, expectedTestFun = function(x) data.table::between(x, 0, 100, incbounds = TRUE))
  verifyArg(ymin, expectedTestFun = function(x) data.table::between(x, 0, 100, incbounds = TRUE))
  verifyArg(ymax, expectedTestFun = function(x) data.table::between(x, 0, 100, incbounds = TRUE))
  if (xmin >= xmax) stop("xmin >= xmax")
  if (ymin >= ymax) stop("ymin >= ymax")

  fx <- formals()
  l <- lapply(setNames(nm = names(fx)), function(x) eval(parse(text = x)))
  l
}

#' Title
#'
#' @param text
#'
#' @return
#' @export
#'
#' @examples
#' path <- system.file("exampleData/01-Iris.pdf", package = "excelPlot")
#' text <- paste0(path, "::commit HEAD::page 20::crop x0,50 y10,80")
#' parsePlotSpec(text)
#' text <- paste0(path, "::crop x0,50 y10,80")
#' parsePlotSpec(text)
parsePlotSpec <- function(text) {
  text <- strsplit(x = text, split = "::")
  text <- text[[1]]

  path <- text[1]

  commit <- grep("^commit", text, value = TRUE)
  if (length(commit)) {commit <- gsub("commit ", "", commit)} else {commit <- "HEAD"}

  page <- grep("^page", text, value = TRUE)
  if (length(page)) {page <- as.numeric(gsub("page ", "", page))} else {page <- 1}

  cropSpec <- grep("^crop", text, value = TRUE)
  if (length(cropSpec)) {
    xmin <- as.numeric(gsub("crop x(\\d+),(\\d+) y(\\d+),(\\d+)", "\\1", cropSpec))
    xmax <- as.numeric(gsub("crop x(\\d+),(\\d+) y(\\d+),(\\d+)", "\\2", cropSpec))
    ymin <- as.numeric(gsub("crop x(\\d+),(\\d+) y(\\d+),(\\d+)", "\\3", cropSpec))
    ymax <- as.numeric(gsub("crop x(\\d+),(\\d+) y(\\d+),(\\d+)", "\\4", cropSpec))
  } else {
    xmin <- 0
    xmax <- 100
    ymin <- 0
    ymax <- 100
  }

  plotSpec(path = path,
           commit = commit,
           page = page,
           xmin = xmin,
           xmax = xmax,
           ymin = ymin,
           ymax = ymax
  )

}

#' Title
#'
#' @param fileIn
#' @param fileOut
#'
#' @return
#' @export
#'
#' @examples
idempotencyNoActionRequired <- function(fileIn, fileOut, commit) {
  # If an existing output file is older than the input file, or we retrieved it from a commit, we want to do nothing,
  file.exists(fileOut) && (commit == "HEAD" || {
    # Wrap this guy into an expression, so we really only try to access fileOut when fileOut exists
    changeDateIn <- as.numeric(system(paste0("stat -c %Z ", fileIn), intern = TRUE))
    changeDateOut <- as.numeric(system(paste0("stat -c %Z ", fileOut), intern = TRUE))
    fileOutIsOlderThanFileIn <- changeDateOut < changeDateIn
    fileOutIsOlderThanFileIn})
}




#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' path <- system.file("exampleData/01-Iris.pdf", package = "excelPlot")
#' path <- system.file("exampleData/04-IrisMulti.pdf", package = "excelPlot")
#' allpage(path)
allpage <- function(path) {
  if (tools::file_ext(path) == "pdf") {
    1:pdftools::pdf_length(path)
  } else if (tools::file_ext(path) == "png") {
    1
  } else {
    stop("Unsupported file type (has to be pdf or png): ", tools::file_ext(path))
  }
}

