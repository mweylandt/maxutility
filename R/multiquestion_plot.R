#' Build a multi-graph plot that combines plots for each of the questions
#' you want to compare
#' @export
#' @param vars a vector of column names
#' @param extract_labels if the columns have labels, you can extract them and
#' make them the title of each plot
#' @param labels manually specify labels
#' @param fill_var Variable to be used as fill aesthetic in ggplot
#' @param title_text overall graph title
#' multiquestion_plot()



multiquestion_plot <- function(x, vars = NULL,
                               extract_labels=TRUE,
                               labels = NULL,
                               fill_var = NULL,
                               title_text = NULL) {

  var_names <- vars

  # get labels

  if(extract_labels == TRUE) {
    var_labels <- x %>% select(all_of(var_names))%>%
      lapply( attr, "label") %>% unlist() %>% unname()

  } else {
    var_labels <- labels
  }

  # get min and max
  x_min <- x %>% select(all_of(var_names)) %>% min(na.rm=TRUE)
  x_max <- x %>% select(all_of(var_names)) %>% max(na.rm=TRUE)



  graphs <- list()

  for (i in 1:length(var_names)) {
    graphs[[i]] <- ggplot(x) +
      aes_string(x=var_names[i], fill = fill_var) +
      geom_bar(aes(y = ..prop..),  stat="count",
               position=position_dodge2(preserve = "single"))+
      labs(title=var_labels[i])+
      scale_x_continuous(breaks = seq(x_min, x_max, by = 1),
                         limits = c(x_min-1,x_max+1))+

      #theme(legend.position="none")+
      theme_bw()+
      theme(axis.title.x = element_blank(),
            #axis.text.x = element_blank()
            text = element_text(size=10)
      )

  }

  wrap_plots(graphs, ncol = 2) +
    plot_layout(guides = 'collect')+
    plot_annotation(title=title_text)


}
