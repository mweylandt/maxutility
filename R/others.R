#' Add wordcount and character count to dataframe
#' @export
#' @param highlight a regex string to be highlighted
#' @param highlight_style Interpret regex directly or, if "stem" is specified,
#'        highlight whole words beginning with the stem specified
#' @examples
#' testdf <- data.frame(author=c("me", "you"), text = c("democracy is so nice pero la pobreza no es bueno", "democratic movements happen en la realidad"))
#' add_textcounts(testdf, text=text)


add_textcounts <- function (x, text_col, word = TRUE, char = TRUE) {
  require(dplyr)
  require(stringr)
  column_name = enquo(text_col)

  if (word == TRUE) {
  x <-  x %>%
  mutate(wordcount = str_count(string= !!column_name, pattern = "\\W+"))
  } else {}

  if (char == TRUE) {
  x <-  x %>%
      mutate(charcount = str_count(string= !!column_name, pattern = ""))
  } else {}

  return(x)
}



