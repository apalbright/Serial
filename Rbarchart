#make stacked bar charts for total, lawyers, and nonlaywers
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(plyr)

setwd("raw_data")
total <- read.csv('total1.csv')
total <- ddply(total, .(Episode), transform, pos = cumsum(Percent) - (0.5 * Percent))

total$Opinion <- factor(total$Opinion, levels = c("Guilty", "Not Guilty", "Undecided"))

pal1 <- c("#cb181dDD", "#f0f0f0DD", "#737373DD")
	
a<-ggplot(data = total, aes(x = Episode, y= Percent, fill = Opinion)) + 
	scale_fill_manual(values = pal1) +
	geom_bar(stat="identity", colour="black")+
	guides(fill = guide_legend(override.aes = list(colour = NULL)))+
	theme(legend.key = element_rect(colour = "black"))+
	labs(title = "Public Opinion on Adnan's Guilt/Innocence Following Episodes 6-12") +
	scale_x_continuous(breaks=c(6,7,8,9,10,11,12))+
	theme(panel.background = element_rect(fill = "black"))+
	theme(axis.text.x=element_text(colour="black"))+
	theme(axis.text.y=element_text(colour="black"))+
	geom_text(aes(label = round(Percent, digits=0), y = pos), size = 3)

#lawyers
l <- read.csv('lawyers1.csv')
l <- ddply(l, .(Episode), transform, pos = cumsum(Percent) - (0.5 * Percent))

b<-ggplot(data = l, aes(x = Episode, y= Percent, fill = Opinion)) + 
	geom_bar(stat="identity", colour="black")+ scale_fill_manual(values = pal1, guide=FALSE) +
	labs(title = "Public Opinion on Adnan's Guilt/Innocence Following Episodes 6-12 \n Among Those With Legal Training") +
	scale_x_continuous( breaks=c(6,7,8,9,10,11,12))+
	theme(panel.background = element_rect(fill = "black"))+
	theme(axis.text.x=element_text(colour="black"))+
	theme(axis.text.y=element_text(colour="black"))+
	geom_text(aes(label = round(Percent, digits=0), y = pos), size = 3)

#non-lawyers
nl <- read.csv('non-lawyers1.csv')
nl <- ddply(nl, .(Episode), transform, pos = cumsum(Percent) - (0.5 * Percent))

library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)

c<-ggplot(data = nl, aes(x = Episode, y= Percent, fill = Opinion)) + 
	geom_bar(stat="identity", colour="black")+ scale_fill_manual(values = pal1, guide=FALSE) +
	labs(title = "Public Opinion on Adnan's Guilt/Innocence Following Episodes 6-12 \n Among Those Without Legal Training") +
	scale_x_continuous( breaks=c(6,7,8,9,10,11,12))+
	theme(panel.background = element_rect(fill = "black"))+ 
	theme(axis.text.x=element_text(colour="black"))+
	theme(axis.text.y=element_text(colour="black"))+
	geom_text(aes(label = round(Percent, digits=0), y = pos), size = 3)

	

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

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

multiplot(a,b,c)
