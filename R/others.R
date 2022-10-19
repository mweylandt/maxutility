#' Add wordcount and character count to dataframe
#' @export
#' @name add_textcounts
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


#' Reverse codings of items (such as in a survey)
#' @export
#' @name reverse
#' @examples
#' ideology <-  c(1, 4, 4, 5, 2, 2, 2, 4, 4, 6, 2, 4, 5, 7, 4, 1, 2, 3, 2, 3, 8)
#' reverse(ideology)
#' library(dplyr)
#' df <- data.frame(id = seq(1:length(ideology)),ideology )
#' df %>% mutate(ideology_r = reverse(ideology))

reverse <- function (x) {
  for (i in 1:length(x)) {
    x[i] <- abs((max(x)+1) - x[i])
  }
  return(x)
}


















