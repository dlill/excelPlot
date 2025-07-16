
#' Title
#'
#' @param text
#' @param style
#'
#' @returns
#' @export
#'
#' @examples
textSpec <- function(text,
                     style
                     ) {

  verifyArg(text, expectedMode = "character", expectedLength = 1)
  verifyArg(style, expectedLength = 1)

  if (is.character(style)) {
    if (length(setdiff(style, names(styleList)))) {
      stop("style \"", style, "\" is not available. Available options are:\n",
      "\n", paste0("  ", seq_along(styleList), ": ", names(styleList), collapse = "\n")
      )
    }
  } else if (!is.numeric(style)) {
    stop("style must be either character or numeric. It is: ", mode(style))
  }

  fx <- formals()
  l <- lapply(setNames(nm = names(fx)), function(x) eval(parse(text = x)))
  l
}

#' Title
#'
#' @param text
#'
#' @returns
#' @export
#'
#' @examples
#' text <- "Iris Blue::2"
#' text <- "Iris Blue::rot"
#' parseTextSpec(text)
parseTextSpec <- function(text) {

  text <- strsplit(x = text, split = "::")
  text <- text[[1]]

  content <- text[1]
  style <- if (length(text) > 1) {text[[2]]} else {1}
  style <- if (!is.na(as.numeric(style))) {as.numeric(style)} else {style}

  textSpec(text = content,
           style = style)

}

