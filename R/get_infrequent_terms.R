
#' this is a slight modification fo the function remove_infrequent_terms()
#' from the preText package
#' @export
#' @param x input: takes corpus, vector of characters, or dfm objects
#' @param proportion_threshold which proportion of documents should a word appear
#' in at minimum to prevent being removed (listed here)
#' @examples
#' testdf <- data.frame(author=c("me", "you"), text = c("democracy is so nice", "democratic movements happen en la realidad"))
#' get_infrequent_terms(testdf$text, proportion_threshold = 0.01)


get_infrequent_terms <- function (x, proportion_threshold = 0.01) {
  require(quanteda)

  if (is.dfm(x)) {
    dfm_object <- x
  } else {
    dfm_object <- dfm(x)
  }

  threshold <- ceiling(proportion_threshold * nrow(dfm_object))
  temp_dfm <- dfm_object
  temp_dfm@x <- rep(1, length(temp_dfm@x))
  doc_counts <- quanteda::colSums(temp_dfm)
  rare <- as.numeric(which(doc_counts < threshold))

  cat(length(rare), "of", ncol(dfm_object),
      "total terms  appear in less than", threshold,
      "documents.\n")

  terms <- as.data.frame(doc_counts[rare]) %>% rownames_to_column(var = "term")
  names(terms) <- c("term", "count")
  terms <- terms %>% arrange(-count, term)

  return(terms)
}
