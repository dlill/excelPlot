
#' Apply the plot preprocessing pipeline
#'
#' Currently, the pipeline has the following steps
#'
#' * Check out from git
#' * Extract page from pdf as png
#' * Crop png to specified part
#'
#' @param plotSpec A [plotSpec()]
#'
#' @return file.path to checked out, extracted and cropped png
#' @md
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



#' Check out a file, potentially from git to a temporary file.
#'
#' @param plotSpec A [plotSpec()]
#'
#' @return file.path to checked out file. The file name is /tmp/$ABSOLUTEPATH.
#' @md
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


#' Apply the extraction step of the plot preprocessing pipeline
#'
#' @param plotSpec A [plotSpec()]
#'
#' @return file.path to extracted page as png
#' @md
#' @importFrom tools file_ext
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

#' Apply the cropping step of the plot preprocessing pipeline
#'
#' @param plotSpec A [plotSpec()]
#'
#' @return file.path to cropped png
#' @md
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
  if(hasFailed != 0) {warning("Cropping failed for ", plotSpec$path, ", page", plotSpec$page)}
  fileOut

}




#' Collect all options for plot preprocessing in a list
#'
#' This function also does error checking.
#'
#' @param path Path to plot file (pdf or png)
#' @param commit Default: HEAD, else a commit hash
#' @param page Default 1, page numer to extract from the pdf
#' @param xmin,xmax,ymin,ymax Crop the image from xmin to xmax and ymin to ymax. Allowed values: 0-100, units in percent.
#' @param resolution Resolution of the extracted png image, in dpi.
#'
#' @return List of the input arguments
#' @export
#' @md
#' @family UI
#' @importFrom data.table between
#' @importFrom stats setNames
#'
#' @examples
#' path <- system.file("exampleData/01-Iris.pdf", package = "excelPlot")
#' plotSpec <- plotSpec(path, commit = "asdf234", page = 1, xmax = 85)
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
  l <- lapply(stats::setNames(nm = names(fx)), function(x) eval(parse(text = x)))
  l
}

#' Parse a decorated string into a plotSpec
#'
#' @param text Text which follows the format "path/to/file::arg1 value::...", where arg is an argument of [plotSpec()], e.g. "commit asdf234"
#'
#' @return A [plotSpec()]
#' @export
#' @md
#' @family UI
#' @importFrom stats setNames
#'
#' @examples
#' path <- system.file("exampleData/01-Iris.pdf", package = "excelPlot")
#' text <- paste0(path, "::commit HEAD::page 20::xmax 50::ymin 20")
#' parsePlotSpec(text)
parsePlotSpec <- function(text) {
  verifyArg(text, expectedClass = "character", expectedLength = 1)

  text <- paste0("path ", text) # To remove the special case that path is not explicitly called "path path/to/file" in the string.
  text <- strsplit(x = text, split = "::")
  text <- text[[1]]

  idxText <- numeric() # For useful error messages down below
  # Loop over all argument names
  plotSpecArgNames <- names(formals(plotSpec))
  plotSpecArgs <- lapply(stats::setNames(nm = plotSpecArgNames), function(nm) {
    idxText <<- c(idxText, grep(paste0("^", nm), text))
    x <- grep(paste0("^", nm), text, value = TRUE)
    x <- if (length(x)) {x <- gsub(paste0(nm, " "), "", x)} else {NULL}
    x
  })
  # Remove the ones which were not found
  plotSpecArgs <- plotSpecArgs[sapply(plotSpecArgs, function(x) !is.null(x))]
  # Try converting args to numeric, and if it works use the numeric version
  plotSpecArgs <- lapply(plotSpecArgs,  function(x) {
    xNumeric <- suppressWarnings(as.numeric(x))
    if (!is.na(xNumeric)) {x <- xNumeric}
    x
  })

  plotSpec <- do.call(plotSpec, plotSpecArgs)
  plotSpec
}

#' Check if anything needs to be done on a plot file
#'
#' @param fileIn Input file of the function where this function is called.
#' @param fileOut Output file of the function where this function is called.
#' @param commit Commit hash
#'
#' @return TRUE: A sound input output relationship is guaranteed even if we don't redo the step
#' @md
idempotencyNoActionRequired <- function(fileIn, fileOut, commit) {

  if (!file.exists(fileOut)) {
    # Trivial case: File does not exist, we need to execute
    return(FALSE)
  }

  if (commit != "HEAD") {
    # If commit is anything other than head, it means that the content of the file is determined - hence we don't need to do anything.
    return(TRUE)
  }

  # Finally we are at file.exists and commit==HEAD. If the input file is older than the output file, we don't need to redo the step.
  changeDateIn <- as.numeric(system(paste0("stat -c %Z ", fileIn), intern = TRUE))
  changeDateOut <- as.numeric(system(paste0("stat -c %Z ", fileOut), intern = TRUE))
  changeDateIn <= changeDateOut
}



# #' List all pages of a plot file
# #'
# #' @param path Path to plot file
# #'
# #' @return Vector with 1:npage
# #' @export
# #' @md
# #' @importFrom tools file_ext
# #' @importFrom pdftools pdf_length
# #'
# #' @examples
# #' path <- system.file("exampleData/01-Iris.pdf", package = "excelPlot")
# #' path <- system.file("exampleData/04-IrisMulti.pdf", package = "excelPlot")
# #' allpages(path)
# all_pages <- function(path) {
#   if (tools::file_ext(path) == "pdf") {
#     1:pdftools::pdf_length(path)
#   } else if (tools::file_ext(path) == "png") {
#     1
#   } else {
#     stop("Unsupported file type (has to be pdf or png): ", tools::file_ext(path))
#   }
# }

