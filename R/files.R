
#' Title
#'
#' @param path
#' @param commit
#' @param page
#' @param crop
#'
#' @return
#' @export
#'
#' @examples
#' path <- system.file("exampleData/01-Iris.pdf", package = "excelPlot")
#' cropSpec <- cropSpec(0,1,0.2,0.9)
epFiles <- function(path, commit = "HEAD", pages = 1, cropSpec = cropSpec()) {
  path <- normalizePath(path, mustWork = TRUE)

  list(
    path                  = path,
    tmpPathCommit         = gsub(normalizePath("~"), "/tmp", path),
    tmpPathCommitPage     = paste0(tools::file_path_sans_ext(gsub(normalizePath("~"), "/tmp", path)), sprintf("-page-%02d.png", page)),
    tmpPathCommitPageCrop = paste0(tools::file_path_sans_ext(gsub(normalizePath("~"), "/tmp", path)), sprintf("-page-%02d-crop-%2.2f-%2.2f-%2.2f-%2.2f.png", page, cropSpec$xmin, cropSpec$xmax, cropSpec$ymin, cropSpec$ymax))
  )

}


#' Get current commit hash
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
getCommit <- function(path) {
  substr(system(paste0("cd ", dirname(path), " && git log"), wait = FALSE, intern = TRUE)[1], 8,20)
}


#' Spec for cropping
#'
#' @param xmin
#' @param xmax
#' @param ymin
#' @param ymax
#'
#' @return
#' @export
#'
#' @examples
cropSpec <- function(xmin = 0, xmax = 1, ymin = 0, ymax = 1) {
  fx <- formals()
  l <- lapply(setNames(nm = names(fx)), function(x) eval(parse(text = x)))
  l
}


