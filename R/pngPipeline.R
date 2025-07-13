

applyPngPipelineAllPages <- function(plotSpec) {

  lapply(plotSpec$pages, function(page) {
    plotSpecOnePage <- plotSpec
    plotSpecOnePage$page <- page
    applyPngPipelineOnePage(plotSpecOnePage)
  })
}


applyPngPipelineOnePage <- function(plotSpec) {
  if (length(plotSpec$pages) > 1) {
    stop("plotSpec$pages can only have length one inside applyPngPipelineOnePage (is ", length(plotSpec$pages))
  }

  pngPipelineCheckoutToTemp(plotSpec)
  pngPipelineExtractPage(plotSpec)
  pngPipelineCrop(plotSpec)

}



pngPipelineCheckoutToTemp <- function(plotSpec) {
  files <- do.call(epFiles, plotSpec)
  fileIn = files$path
  fileOut = files$tmpPathCommit

  # Case 1: Nothing to do
  if (outputFileIsNewer(fileIn = fileIn, fileOut = fileOut)) {
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


pngPipelineExtractPage <- function(plotSpec) {

  files <- do.call(epFiles, plotSpec)
  fileIn = files$tmpPathCommit
  fileOut = files$tmpPathCommitPage

  # Case 1: Nothing to do
  if (outputFileIsNewer(fileIn = fileIn, fileOut = fileOut)) {
    return(fileOut)
  }

  # Case 2: extract page
  # [ ] >>>> Continue here <<<<<<<<<<< ----
  # [ ] >>>> Continue here <<<<<<<<<<< ----
  # [ ] >>>> Continue here <<<<<<<<<<< ----
  # [ ] >>>> Continue here <<<<<<<<<<< ----


}

pngPipelineCrop <- function(plotSpec) {

}




#' Title
#'
#' @param path
#' @param commit
#' @param pages
#' @param cropSpec
#'
#' @return
#' @export
#'
#' @examples
#' path <- system.file("exampleData/01-Iris.pdf", package = "excelPlot")
#' cropSpec <- cropSpec(0,1,0.2,0.9)
#' plotSpec <- plotSpec(path, commit = getCommit(path), pages = allPages(path), cropSpec = cropSpec())
plotSpec <- function(path, commit = "HEAD", pages = allPages(path), cropSpec = cropSpec()) {
  fx <- formals()
  l <- lapply(setNames(nm = names(fx)), function(x) eval(parse(text = x)))
}

outputFileIsNewer <- function(fileIn, fileOut) {
  if (!file.exists(fileOut)) {
    return(FALSE)
  }
  changeDateIn <- as.numeric(system(paste0("stat -c %Z ", files$path), intern = TRUE))
  changeDateOut <- as.numeric(system(paste0("stat -c %Z ", files$tmpPathCommit), intern = TRUE))
  changeDateOut > changeDateIn
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
#' allPages(path)
allPages <- function(path) {
  if (tools::file_ext(path) == "pdf") {
    1:pdftools::pdf_length(path)
  } else if (tools::file_ext(path) == "png") {
    1
  } else {
    stop("Unsupported file type (has to be pdf or png): ", tools::file_ext(path))
  }
}

# „Path/to/file.pdf:::pages c(1,3:5):::crop x0,0.9 y0,1:::commit sd782h3m55“ = plotspec(filename,…) = .p(filename)
#
# Options:
#
#   * Path
# * Pages
# * Crop
# * Commit
#
# Details
#
# * cm to inch conversion in getIMGWiddthHeight
# * Fix the row height bug
