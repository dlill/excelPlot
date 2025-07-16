
#' Title
#'
#' @param text
#' @param size
#' @param face
#' @param rotation
#' @param halign
#' @param valign
#'
#' @returns
#' @export
#'
#' @examples
textSpec <- function(text,
                     style
                     # , size = 16, face = "bold", rotation = 0, halign = "left", valign = "center"
                     ) {

  verifyArg(text, expectedMode = "character", expectedLength = 1)
  verifyArg(style, expectedMode = "numeric", expectedLength = 1)
  # verifyArg(size, expectedMode = "numeric", expectedLength = 1)
  # verifyArg(face, expectedMode = "character", expectedLength = 1)
  # verifyArg(rotation, expectedMode = "numeric", expectedLength = 1)

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
#' parseTextSpec(text)
parseTextSpec <- function(text) {

  text <- strsplit(x = text, split = "::")
  text <- text[[1]]

  content <- text[1]
  style <- if (length(text) > 1) as.numeric(text[[2]]) else 1

  # rotation <- grep("^rot(ation)?", text, value = TRUE)
  # if (length(rotation)) {rotation <- as.numeric(gsub("rot(ation)? ", "", rotation))} else {rotation <- 0}
  #
  # size <- grep("^size", text, value = TRUE)
  # if (length(size)) {size <- as.numeric(gsub("size ", "", size))} else {size <- 16}
  #
  # face <- grep("^face", text, value = TRUE)
  # if (length(face)) {face <- gsub("face ", "", face)} else {face <- "bold"}
  #
  # halign <- grep("^halign", text, value = TRUE)
  # if (length(halign)) {halign <- gsub("halign ", "", halign)} else {halign <- "left"}
  #
  # valign <- grep("^valign", text, value = TRUE)
  # if (length(valign)) {valign <- gsub("valign ", "", valign)} else {valign <- "center"}

  textSpec(text = content,
           # rotation = rotation,
           # size = size,
           # face = face,
           # halign = halign,
           # valign = valign,
           style = style)

}

