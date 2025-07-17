
#' Collect all text specification in a list
#'
#' @param text The text you want to insert in the cell
#' @param style Numeric or character, specifying the index or the name of a text style. Execute [availableStyles()] for available styles.
#'
#' @returns List of its input arguments
#' @export
#' @md
#' @family UI
#'
#' @examples
#' textSpec(text = "Text 1", style = "rotateUp")
textSpec <- function(text, style) {

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

#' Parse a decorated text into a textSpec
#'
#' @param text Character with format "arbitrary text::style"
#'
#' @returns A [textSpec()]
#' @export
#' @md
#' @family UI
#'
#' @examples
#' text <- "Iris Blue::2"
#' text <- "Iris Blue::rotateUp"
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

