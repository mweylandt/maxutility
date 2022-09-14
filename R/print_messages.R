#' Print a big, text-heavy dataframe to an html with optional organization
#' and higlighting
#' @export
#' @param att1 first attribute to be displayed in the left sidebar
#' @param att2 second attribute to be displayed in the left sidebar
#' @param text which column of the dataframe contains the text to print.
#'        Do not put this in quotes!
#' @param keywords string or vector of strings that are used to split the
#'        file into sections
#' @param highlight a regex string to be highlighted
#' @param highlight_style Interpret regex directly or, if "stem" is specified,
#'        highlight whole words beginning with the stem specified
#' print_data()


# newest function as of September 14, 2022

print_messages <- function(x,att1=NULL, att2 = NULL, 
                           text=NULL, keywords=NULL, 
                           highlight=NULL, 
                           highlight_style = "stem", 
                           filename="out.html") {
  
  #  print("---------------------------TEST HERE")
  #  print(x %>% dplyr::select({{text}}))
  #  print("----------------------------TEST ENDS")
  
  # Different Topics --------------------------------------------------------
  require(stringr)
  require(glue)
  require(dplyr)
  
  # Validate inputs
  if(is.null(att1)) {
    att1_glue <- ""
  } else {
    att1_glue <- paste0("{", att1,"}")
  }
  
  if(is.null(att2)) {
    att2_glue <- ""
  } else {
    att2_glue <- paste0("{",att2,"}")
  }
  
  
  # highlight the text
  
  
  if (is.null(highlight)) {
    
  } else {
    
    
    #replace the  words
    if (highlight_style == "stem") {
      pattern <- paste0("((",highlight,")\\w+)")
      
    } else if (highlight_style == "regex") {
      pattern <- paste0( "(",highlight, ")") # put in parentheses to create a capture group that can then be referenced using backreference \\1
    }
    x$text <- stringr::str_replace_all(x$text, 
                                       pattern=regex(pattern), 
                                       replacement = "<mark>\\1 </mark>")
    
  }
  
  
  
  
  
  head_html <- "<!DOCTYPE html>
<html>
<head>
<link rel=\"stylesheet\"href=\"print.css\" type=\"text/css\">
<title>title</title>
<body>"
  
  foot_html <- "</body></html>"
  
  
  pattern <-   paste("<div class=\"row\">

   <div class=\"column left\">
  <ul>
   <li><b>",att1_glue,"</b></li>
	 <li>",att2_glue,"</li>
   	</ul>
  </div>
    <div class=\"column right\">
    	<div class=\"description-content\">
			
 {", deparse(substitute(text)),"}
</div>
	</div>
</div>
")        
  
  
  
  if(is.null(keywords)) {
    # print all messages
    messages_html <- glue_data(x, pattern)
    
  } 
  
  # if keywords are passed, filter dataset and change layout to 
  # highlight
  
  
  else {
    messages_html = ""
    keywords = c(keywords)
    
    for (i in 1:length(keywords) ) {
      message_text<- x %>% dplyr::filter(str_detect({{ text }}, 
                                                    fixed(keywords[i], 
                                                          ignore_case=T))) %>%
        glue_data(pattern) 
      messages_html <- paste0(messages_html, "<h1>", keywords[i], "</h1>",
                              paste(message_text, sep=" ", collapse="") ,
                              "<hr>"
      )
    }
  }
  
  filename <- filename
  
  page <- paste0(head_html, messages_html, foot_html)
  #write(page, outfile)
  write(page, filename)
  browseURL(filename)
}
