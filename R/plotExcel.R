
styleList <- list(
  left       = openxlsx::createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = NULL, valign = NULL),
  center     = openxlsx::createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = "center"),
  rotateUp   = openxlsx::createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = "right", valign = "center", textRotation = 90),
  rotateDown = openxlsx::createStyle(fontSize = 18, wrapText = TRUE, textDecoration = "bold", halign = "left" , valign = "center", textRotation = -90))

availableStyles <- function() {
  cat("Available styles:\n", paste0(seq_along(styleList), ": ", names(styleList) , collapse = "\n"), sep = "")
}
