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
#' test <- c(1,NA,3)
#' reverse(test, min=1, max=3)
#'
#' test2 <- c(1,2,3,4)
#' reverse(test2, min=1, max=4)
#'
#' test3 <- c(0,1,2)
#' reverse(test3, min=0, max=2)
#'
#' test4 <- c(-1,0,1,2,3)
#' reverse(test4, min=-1, max=3)
#' ideology <-  c(1, 4, 4, 5, 2, 2, 2, 4, 4, 6, 2, 4, 5, 7, 4, 1, 2, 3, 2, 3, 8)
#' reverse(ideology)
#' library(dplyr)
#' ideology <-  c(1, 4, 4, 5, 2, 2, 2, 4, 4, 6, 2, 4, 5, 7, 4, 1, 2, 3, 2, 3)
#' df <- data.frame(id = seq(1:length(ideology)),ideology )
#' df %>% mutate(ideology_r = reverse(ideology, min=1, max=7))

reverse <- function (x, min = NULL, max = NULL) {

  # create named vector with old values as names
  # and new values as values
  # then, loop through all the values and read the
  # correct new value that corresponds to the old
  # value (i.e vector name)

  newvalues <- rev(seq(from=min, to=max))
  names(newvalues) <- seq(from=min, to=max) # names with OG labels

  x_r <- rep(NA, length(x))

  for (i in seq(1,length(x))) {

    if(is.na(x[i])){ # have to handle NA manually
      x_r[i] <- NA

    } else {

    index <- which(names(newvalues)==x[i])
    x_r[i] <- newvalues[[index]]
    }
  }

  return(x_r)

}














