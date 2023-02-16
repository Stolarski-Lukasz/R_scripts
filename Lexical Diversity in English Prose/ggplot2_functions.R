# multiplotting in ggplot2
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# graph customization
border_color <- "darkgrey"
fill_color <- "#4f5458"
boxplot_border_color <- "grey15"
boxplot_fill_color <- "grey60"

grey_theme <- theme(
  panel.background = element_rect(fill = '#F5F5F5'),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "#C8C8C8"),
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "#C8C8C8"))