#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel("Investment Options"),
  
  fluidRow(
    column(4, 
  sliderInput(inputId = "n",
              "Initial Amount", value = 1000, min = 0, max = 100000, step = 500),
  sliderInput(inputId = "q", "Annual Contribution", min = 0, max = 50000, step = 500, value = 2000)
  ),
  column(4, 
  sliderInput(inputId = "r", "Return Rate(in %)", min = 0, max = 20, step = .1, value = 5),
  sliderInput(inputId = "growth", "Growth Rate(in %)", min = 0, max = 20, step = .1, value = 2)
  ),
  column(4, 
  sliderInput(inputId = "y", "Years", value = 20, min = 0, max = 50),
  selectInput("f", "Facet?", choices = c("No", "Yes"))
  )),

      mainPanel(
         h4("Timeline"),
         plotOutput("gg"),
         h4("Balances"),
         verbatimTextOutput("text")
        # dataTableOutput("table")
      )
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
   df <- reactive({
    # Future Value Function
     #' @title Future Value Function
     #' @description calculates the value of an investment in a given number of years and given rate
     #' @param amount value of investment input
     #' @param rate number rate of growth year on year
     #' @param years number of years for which the value of investment will be returned
     #' @return value in dollar amounts
     
     future_value <- function(amount, rate, years = 1){
       x <- amount*((1 + rate)^years)
       return(x)
     }
     

    # Future Value of Annuity
     

     #' @title Future Value of Annuity
     #' @description given contribution rate and annual rate of return we get a final value of a continual investment arrangement
     #' @param contrib how much is deposited at the end of each year
     #' @param rate annual rate of return
     #' @param years number of years
     
     annuity <- function(contrib, rate, years = 1){
       a <- contrib*((((1 + rate)^years)- 1)/rate)
       return(a)
     }
     

     
     #Future Value of Growing Annuity
     
     #' @title growing annuity
     #' @description annuity with growing contributions
     #' @param contrib how much is deposited at the end of each year
     #' @param rate annual rate of return
     #' @param growth annual growth rate of investment
     #' @param years number of years
     
     growing_annuity <- function(contrib, rate, growth, years = 1){
       if(rate == growth) {
       c <- contrib*years
       } else {
       c <- contrib*(((((1 + rate)^years)- 1)- (((1 + growth)^years)- 1))/(rate - growth))
       }
       return(c)
     }
     
     no_contrib <- c()
     fixed_contrib <- c()
     growing_contrib <- c()
     
     for(i in 0:input$y){
       no_contrib <- c(no_contrib, future_value(amount = input$n, rate = input$r/100, years = i))

       fixed_contrib <- c(fixed_contrib, future_value(amount = input$n, rate = input$r/100, years = i) + annuity(contrib = input$q, rate = input$r/100, years = i))
 
       growing_contrib <- c(growing_contrib, future_value(amount = input$n, rate = input$r/100, years = i) + growing_annuity(contrib = input$q, rate = input$r/100, growth = as.double(input$growth)/100, years = i))
       i = i +1
     }
     
     dat <- data.frame(
       year = 0:input$y,
       no_contrib = no_contrib,
       fixed_contrib = fixed_contrib,
       growing_contrib = growing_contrib
     )
     
     dat
   })
   output$gg <- renderPlot({
    gg1 <- ggplot(df()) +
       geom_line(aes(y = no_contrib, x = year, col = "red")) + 
      geom_point(aes(y = no_contrib, x = year, col = "red"))+
       geom_line(aes(y = fixed_contrib, x = year, col = 'blue')) +
      geom_point(aes(y = fixed_contrib, x = year, col = 'blue'))+
       geom_line(aes(y = growing_contrib, x = year, col = "green")) +
       geom_point(aes(y = growing_contrib, x = year, col = "green"))+
       ylab("Total Value") +
       xlab("Time in Years") +
       scale_color_discrete(name = "Investment", labels = c("constant annuity", "growing annuity", "fixed investment"))
   
    if(input$f == "Yes") {
      dat0 <- df()
    dat1 <- gather(df(), "investment", "total value", 2:4)
      gg2 <- ggplot(dat1, aes(y = `total value`, x = `year`, fill = `investment`)) +
        geom_area() +
        facet_wrap(.~`investment`) +
        scale_fill_manual(values = alpha(c("red", 'green',"blue"), .1)) +
        geom_point(aes(col = `investment`))+
        geom_line(aes(col = `investment`))+
        ylab("Total Value") +
        xlab("Time in Years") 
        #scale_color_discrete(name = "Investment Type", labels = c("constant annuity", "growing annuity", "fixed investment"))
      
      return(gg2)
    } else {
      return(gg1)
    }

   })
   #output$table <- renderDataTable(df())
   output$text <- renderPrint({
     df()
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

