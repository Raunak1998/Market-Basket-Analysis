library(tidyverse)
library(data.table)
library(arules)
library(arulesViz)
library(datasets)
system("ls ./input")
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
list.files("../input")
train <- read_csv("./input/train.csv")
set.seed(1234) 
train <- sample_frac(as.tibble(fread('./input/train.csv')),0.2)
stores <- read_csv("./input/stores.csv")
items <- read_csv("./input/items.csv")
transactions <- read_csv("./input/transactions.csv")
oil <- read_csv("./input/oil.csv")
holidays_events <- read_csv("./input/holidays_events.csv")
summary(train)
glimpse(train)
summary(stores)
summary(items)
summary(transactions)
summary(oil)
summary(holidays_events)
train$date <- as.Date(train$date)
head(train)
head(stores)
head(items)
head(transactions)
head(oil)
head(holidays_events)
transactions %>%
  group_by(date) %>%
  summarise(sum_trans = sum(transactions)) %>%
  ggplot(aes(date, sum_trans)) +
  geom_line(color = "black") +
  geom_smooth(method = "loess", color = "red", span = 1/5) +
  ylab("Total Transactions") +
  ggtitle("Plot of total transactions over training dataset time frame")
transactions %>%
  ggplot(aes(date, transactions, color = factor(store_nbr))) +
  geom_smooth(method = "loess", span = 1/2, se = FALSE) +
  ggtitle("Plot of total sales per store over training dataset")
data(Groceries)
itemFrequencyPlot(Groceries,topN=20,type="absolute")
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
options(digits=2)
inspect(rules[1:5])
rules<-sort(rules, by="confidence", decreasing=TRUE)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
plot(rules,method="graph",engine='interactive',shading=NA)
