#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(data.table)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin="purple",
                    dashboardHeader(title = "Basic Dashboard",
                                    dropdownMenu(type = "messages",
                                                 messageItem(
                                                     from = "Sales Dept",
                                                     message = "Sales are steady this month."
                                                 ),
                                                 messageItem(
                                                     from = "New User",
                                                     message = "How do I register?",
                                                     icon = icon("question"),
                                                     time = "13:45"
                                                 ),
                                                 messageItem(
                                                     from = "Support",
                                                     message = "The new server is ready.",
                                                     icon = icon("life-ring"),
                                                     time = "2014-12-01"
                                                 )
                                    ),
                                    dropdownMenu(type = "notifications",
                                                 notificationItem(
                                                     text = "5 new views today",
                                                     icon("users")
                                                 ),
                                                 notificationItem(
                                                     text = "12 items delivered",
                                                     icon("truck"),
                                                     status = "success"
                                                 ),
                                                 notificationItem(
                                                     text = "Server load at 86%",
                                                     icon = icon("exclamation-triangle"),
                                                     status = "warning"
                                                 )
                                    ),
                                    dropdownMenu(type = "tasks", badgeStatus = "success",
                                                 taskItem(value = 90, color = "green",
                                                          "Documentation"
                                                 ),
                                                 taskItem(value = 17, color = "aqua",
                                                          "Project X"
                                                 ),
                                                 taskItem(value = 75, color = "yellow",
                                                          "Server deployment"
                                                 ),
                                                 taskItem(value = 80, color = "red",
                                                          "Overall project"
                                                 )
                                    )
                    ),
                    dashboardSidebar(sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Exploratory Data Analysis Diagrams", tabName = "edag", icon = icon("th")),
                        menuItem("Grocery Sections", tabName = "gsec", icon = icon("th")),
                        menuItem("Source code", icon = icon("file-code-o"), 
                                 href = "https://github.com/rstudio/shinydashboard/")
                    )),
                    dashboardBody(
                        tabItems(
                            # First tab content
                            tabItem(tabName = "dashboard",
                                    fluidRow(
                                        box(plotOutput("plota1", height = 250), solidHeader = TRUE),
                                        
                                        box(
                                            plotOutput("plota2", height = 250), solidHeader = TRUE)
                                    ),
                                    
                                    fluidRow(
                                        box(plotOutput("plota3", height = 250), solidHeader = TRUE),
                                        
                                        box(
                                            plotOutput("plota4", height = 250), solidHeader = TRUE)
                                    ),
                                    fluidRow(
                                        box(plotOutput("plota5", height = 250), solidHeader = TRUE)
                                        
                                    )),
                            
                            
                            
                            
                            
                            
                            
                            
                            #second tab content
                            tabItem(tabName = "edag",
                                    fluidRow(
                                        column(12, box(plotOutput("plota6", height = 300,width=600), solidHeader = TRUE))),
                                    fluidRow(
                                        column(12,  box(plotOutput("plota7", height = 300,width=600,), solidHeader = TRUE))),
                                    fluidRow(
                                        column(12, box(plotOutput("plota8", height = 300,width=600), solidHeader = TRUE)))
                            ),
                            
                            # third tab content
                            tabItem(tabName = "gsec",
                                    fluidRow(valueBox(4, "Hardware", icon = icon("cog")),
                                             
                                             valueBox(20,"Automotive" ,icon=icon("bus"),color="yellow"),
                                             valueBox(98 ,"Beauty & Care",icon=icon("address-card"),color="red")
                                             
                                    ),
                                    fluidRow(valueBox(446,"Cleaning",color="purple",icon=icon("user")),
                                             
                                             
                                             (valueBox(1348,"Grocery",color="green",icon=icon("apple"))
                                             ),
                                             (valueBox(134,"Bakery",color="yellow",icon=icon("camera-retro")))),
                                    fluidRow(valueBox(613,"Beverages",color="teal",icon=icon("user")),
                                             valueBox(22,"Books",color="red",icon=icon("book"))),
                                    fluidRow( box(plotOutput("plota9", height = 500,width=650), solidHeader = TRUE)))
                            
                            
                            
                            
                        )
                    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
            # Make the panel
            numPlots = length(plots)
            
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
    stores <- read_csv("dataset/stores.csv")
    items <- read_csv("dataset/items.csv")
    transactions <- read_csv("dataset/transactions.csv")
    train<-read_csv("dataset/train.csv")
    holidays_events <- read_csv("dataset/holidays_events.csv")
    train$date <- as.Date(train$date)
    train$family <- with(items, family[match(train$item_nbr, item_nbr)])
    
    output$plota1 <- renderPlot({
        
        p1 <- train %>%
            filter(family == "GROCERY I") %>%
            group_by(date) %>%
            summarise(sum_sales = sum(unit_sales)) %>%
            ggplot(aes(date, sum_sales)) +
            geom_line(color = "black") +
            geom_smooth(method = "loess", color = "red", span = 1/5) +
            ylab("Total Item Sales") +
            ggtitle("Plot of total item sales - GROCERY I") 
        multiplot(p1, cols=2)
        
        
    })
    output$plota2<- renderPlot({
        
        p2 <- train %>%
            filter(family == "BEVERAGES") %>%
            group_by(date) %>%
            summarise(sum_sales = sum(unit_sales)) %>%
            ggplot(aes(date, sum_sales)) +
            geom_line(color = "black") +
            geom_smooth(method = "loess", color = "red", span = 1/5) +
            ylab("Total Item Sales") +
            ggtitle("Plot of total item sales - BEVERAGES") 
        multiplot(p2,cols=2)
        
        
        
    })
    output$plota3 <- renderPlot({
        p3 <- train %>%
            filter(family == "CLEANING") %>%
            group_by(date) %>%
            summarise(sum_sales = sum(unit_sales)) %>%
            ggplot(aes(date, sum_sales)) +
            geom_line(color = "black") +
            geom_smooth(method = "loess", color = "red", span = 1/5) +
            ylab("Total Item Sales") +
            ggtitle("Plot of total item sales - CLEANING")
        multiplot(p3,cols=2)
    })
    output$plota4 <-renderPlot({
        p4 <- train %>%
        filter(family == "DAIRY") %>%
        group_by(date) %>%
        summarise(sum_sales = sum(unit_sales)) %>%
        ggplot(aes(date, sum_sales)) +
        geom_line(color = "black") +
        geom_smooth(method = "loess", color = "red", span = 1/5) +
        ylab("Total Item Sales") +
        ggtitle("Plot of total item sales - DAIRY") 
    multiplot(p4,cols=2)
    })
    output$plota5 <-renderPlot({
        p5 <- train %>%
        filter(family == "PERSONAL CARE") %>%
        group_by(date) %>%
        summarise(sum_sales = sum(unit_sales)) %>%
        ggplot(aes(date, sum_sales)) +
        geom_line(color = "black") +
        geom_smooth(method = "loess", color = "red", span = 1/5) +
        ylab("Total Item Sales") +
        ggtitle("Plot of total item sales - PERSONAL CARE") 
    multiplot(p5,cols=2)
    })
    output$plota6 <- renderPlot({
        transactionstotal <- transactions %>%
            group_by(date) %>%
            summarise(sum_trans = sum(transactions))
        transactionstotal$day <- weekdays(as.Date(transactionstotal$date))
        transactionstotal$month <- lubridate::month(as.Date(transactionstotal$date), label = TRUE, abbr = FALSE)
        
        # Create a dataframe with the sum of the transactions based on day of week and month
        transactionsinfo <- transactionstotal %>%
            group_by(day, month) %>%
            summarise(sum_trans = sum(sum_trans))
        
        # Plot a heatmap based on the day of the week and the month
        ggplot(data = transactionsinfo, aes(day, month)) +
            geom_tile(aes(fill = sum_trans),colour = "white") + scale_fill_gradient(low = "yellow", high = "red") +
            ggtitle("Heatmap of Transactions by Day of the Week and Month")
        
    })
    output$plota7 <- renderPlot({
        
        itemsalestotal <- train %>%
            group_by(date) %>%
            summarise(sum_items = sum(unit_sales))
        
        itemsalestotal$day <- weekdays(as.Date(itemsalestotal$date))
        itemsalestotal$month <- lubridate::month(as.Date(itemsalestotal$date), label = TRUE, abbr = FALSE)
        
        # Create a dataframe with the sum of the transactions based on day of week and month
        itemsalesinfo <- itemsalestotal %>%
            group_by(day, month) %>%
            summarise(sum_items = sum(sum_items))
        
        # Plot a heatmap based on the day of the week and the month
        ggplot(data = itemsalesinfo, aes(day, month)) +
            geom_tile(aes(fill = sum_items),colour = "white") + scale_fill_gradient(low = "yellow", high = "red") +
            ggtitle("Heatmap of Item Sales by Day of the Week and Month")
        
    })
    
    
    output$plota8 <- renderPlot({
        transactionstotal <- transactions %>%
            group_by(date) %>%
            summarise(sum_trans = sum(transactions))
        itemsalestotal <- train %>%
            group_by(date) %>%
            summarise(sum_items = sum(unit_sales))
        transactionstotal$day <- weekdays(as.Date(transactionstotal$date))
        transactionstotal$month <- lubridate::month(as.Date(transactionstotal$date), label = TRUE, abbr = FALSE)
        transactionsinfo <- transactionstotal %>%
            group_by(day, month) %>%
            summarise(sum_trans = sum(sum_trans))
        itemsalestotal$day <- weekdays(as.Date(itemsalestotal$date))
        itemsalestotal$month <- lubridate::month(as.Date(itemsalestotal$date), label = TRUE, abbr = FALSE)
        
        itemsalestotal$dayOfMonth <- format(itemsalestotal$date, "%d")
        
        # Create a dataframe with the sum of the transactions based on day of week and month
        itemsalesinfo2 <- itemsalestotal %>%
            group_by(dayOfMonth, day) %>%
            summarise(sum_items = sum(sum_items))
        
        # Plot a heatmap based on the day of the week and the month
        ggplot(data = itemsalesinfo2, aes(dayOfMonth, day)) +
            geom_tile(aes(fill = sum_items),colour = "white") + scale_fill_gradient(low = "yellow", high = "red") +
            ggtitle("Heatmap of Item Sales by Day of the Week and Day of Month")
        
        
        
        
    })
    
    output$plota9 <- renderPlot({
        
        
        items %>%
            group_by(family) %>%
            ggplot(aes(x=factor(family), fill = perishable)) +
            geom_bar() +
            ggtitle("Plot of different 'Family' types and item counts") +
            ylab("Unique Item Numbers") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
}

shinyApp(ui = ui, server = server)
