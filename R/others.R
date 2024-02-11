#' Add wordcount and character count to dataframe
#' @export
#' @name add_textcounts
#' @param text_col which column contains text to be counted
#' @param word add wordcount
#' @param char add character count
#' @examples
#' testdf <- data.frame(author=c("me", "you"), text = c("democracy is so nice", "democratic movements happen en la realidad"))
#' add_textcounts(testdf, text=text)

text_col= "a"

x <- df

add_textcounts <- function (x, text_col, word = TRUE, char = TRUE) {
  require(dplyr)
  require(stringr)

  string_vector <- unlist(as.vector(x[text_col]))

  if (word == TRUE) {
  x$wordcount = get_wordcount(string_vector)
  } else {}

  if (char == TRUE) {

    x$charcount = str_count(string = string_vector, pattern = "")

  } else {}

  return(x)
}


#' Returns a vector of wordcounts
#' @export
#' @name get_wordcount
#' @param text_col text column to be counted
#' @examples
#' testdf <- data.frame(author=c("me", "you"), text = c("democracy is so nice", "democratic movements happen from time to time"))
#' get_wordcount(testdf, text=text)


get_wordcount <- function(x) {
  require(dplyr)
  require(quanteda)

  if (!is.vector(x)) {
    x <- as.vector(x)
    warning("input not a vector. Trying to coerce")
  }

  toks <-  quanteda::tokens(x, remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_numbers = TRUE)
  ntoken(toks) #10




}


#' Returns a vector of character counts
#' @export
#' @name get_charcount
#' @param text_col text column to be counted
#' @examples
#' testdf <- data.frame(author=c("me", "you"), text = c("democracy is so nice", "democratic movements happen en la realidad"))
#' get_wordcount(testdf, text=text)

get_charcount <- function(x, text_col) {
  require(dplyr)
  require(stringr)
  column_name = enquo(text_col)


    charcount <- x %>%
      summarize(character_count =   str_count(string= !!column_name, pattern = ""))%>%
      pull(character_count)


  return(character_count)
}


# Reverse -----------------------------------------------------------------



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














