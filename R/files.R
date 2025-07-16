
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
#' epFiles(path, commit = "HEAD", page = 1)
epFiles <- function(path, commit = "HEAD", page = 1, xmin = 0, xmax = 100, ymin = 0, ymax = 100, ...) {
  path <- normalizePath(path, mustWork = TRUE)

  path                  = path
  tmpPathCommit         = paste0(tools::file_path_sans_ext(gsub(normalizePath("~"), "/tmp", path)), "-commit-", commit, ".", tools::file_ext(path))
  tmpPathCommitPage     = paste0(tools::file_path_sans_ext(tmpPathCommit), sprintf("-page-%02d.png", page))
  tmpPathCommitPageCrop = paste0(tools::file_path_sans_ext(tmpPathCommitPage), sprintf("-crop-%03d-%03d-%03d-%03d.png", xmin, xmax, ymin, ymax))

  list(
    path                  = path,
    tmpPathCommit         = tmpPathCommit,
    tmpPathCommitPage     = tmpPathCommitPage,
    tmpPathCommitPageCrop = tmpPathCommitPageCrop

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




