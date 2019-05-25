#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Author: Daniel Villela - dvillela@gmail.com

library(shiny)
library(ggplot2)
library(quantreg)
library(dplyr)

expit <- function (X) {
  return(exp(X)/(1+exp(X)))
}

mylogit <- function (X) {
  return(log(X/(1-X)))
}

load("./dem.RData")
options(bitmapType = "cairo")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  #verticalLayout(
  # Application title
  # titlePanel("Democracy and Human Development"),
   tags$h3("Democracy and Human Development"),
   # Sidebar with a slider input for number of bins 
      # Show a plot of the generated distribution
   #wellPanel(
   tags$hr(),
   fluidRow(
      column(5, tags$b("Pick Democracy score for a generic country:"), align="right"),
      column(7, 
             sliderInput("dem",
                 #"Pick Democracy score for a generic country:",
                 label=NULL,
                 animate = animationOptions(interval=3000),
                 step = .05,
                 min = .1,
                 max = .95,
                 value = .7, 
                 width="600px"
                 ))
      ),#,  style="font-size:50px")
   #),
   #wellPanel(
    
   #),
  # wellPanel(
  fluidRow(
    column(6,tags$b("Where this country is expected to be in the recent landscape of a composite index obtained from HDI and Democracy scores"), align="right"),
    column(6,
           tags$b("What to expect for a country's HDI given the selected democracy score (95% interval in blue)"), align="right")
  ),
    fluidRow(
    column(6,
  plotOutput("distPlot", height="340px")),
  #tags$hr(),
  column(6, plotOutput("histPlot", height="300px"))
  ),
  fluidRow(
    column(2, tags$h6("Data sources"), align="right"),
    column(4,
           #tags$h6("Data sources", align="right"),
           tags$h6("HDI: Human Development Data - UNDP ", align="left"),
                   tags$h6("Democracy score: Economist Intelligence Unit (gapminder.org)",
                   align="left")
           ),
    column(6,
           tags$h6("Analytical data: Daniel Villela", align="right"))
  ),
  
  tags$hr(),
  tags$div(
  tags$h6("Created by Daniel Villela - dvillela@gmail.com"), align="right"
  #)
  # )
   
     )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$histPlot <- renderPlot({
     
     s <- xw[1,]
     s$dem.lit <- mylogit(input$dem)
     xreg <- predict(mreg, s)
     
     lr <- length(xreg[1,])
     sdev <- (xreg[1,lr] - xreg[1,1])/4
     m <- xreg[1,2]
     #mmean = x1
     mmean = (xreg[1,lr] + xreg[1,1])/2.0
     
     resp = data.frame(x=xreg[1,],
                       y= mseq, 
                       mean= mmean, 
                       sd= sdev)

     resp = data.frame(x=(-300:300)/100,
                       y= dnorm((-300:300)/100,
                       mean= mmean, 
                       sd= sdev))
     
     ss <- expit(xreg[1,])
     ls <- length(ss)
     resp %>% mutate(x.expit = expit(x)) %>% 
       mutate(ymin = if_else((x.expit>=ss[1]) & (x.expit<=ss[ls]), 0, as.numeric(NA))) %>% 
       mutate(ymax = if_else((x.expit>=ss[1]) & (x.expit<=ss[ls]), y, as.numeric(NA))) -> resp
     #     require(ggplot2)
     
     ggplot(aes(x=x.expit, y=y, ymin=ymin, 
                ymax=ymax), data=resp) + geom_line() + 
       geom_ribbon(fill="blue", alpha=.6) + 
       #ylim(c(0,1)) +
       xlim(c(0,1)) +
       xlab("HDI index values") +
       ylab("Probability") + 
       #labs(caption = "Analysis: Daniel Villela") +
       #ggtitle(" What to expect for a country's HDI for the selected democracy score (95% interval in blue)") +
       theme(plot.title = element_text(face="bold"),
             axis.title =  element_text(size = 12),
             axis.text =  element_text(size = 12))

     #ggplot(aes(x=x.expit, y=y.diff, ymin=ymin, ymax=ymax), data=resp) + geom_density() + xlim(c(0,1))  
     
     #ggplot(aes(x=x.expit, y=y.diff, ymin=ymin, ymax=ymax), data=resp) + geom_line() + 
       #geom_ribbon() #+ xlim(c(0,1))
     
   }, height="auto")
   
   output$distPlot <- renderPlot({

     s <- xw[1,]
     s$dem.lit <- mylogit(input$dem)
     xreg <- predict(mreg, s)
     
     sdev <- (xreg[1,3] - xreg[1,1])/4
     m <- xreg[1,2]
     #mmean = x1
     mmean = (xreg[1,3] + xreg[1,1])/2.0
     
     resp = data.frame(x=xreg[1,],
                       y= mseq, 
                       mean= mmean, 
                       sd= sdev)
     
     ss <- expit(xreg[1,])
     ls <- length(ss)

     cat <- floor(input$dem * 100/(100/40))
     
     sscat <- floor(ss*100/(100/40))
     #sscat[1]
     #sscat[ls]
     xw[xw$Year==2017,] %>% 
       mutate(level_chosen= 
                if_else(as.numeric(as.character(hdi_cat)) > sscat[1] & 
                          as.numeric(as.character(hdi_cat)) <=sscat[ls],1, 0)) -> xzl
     
#     ggplot(aes(x=combined, fill=as.factor(level_chosen)), data=xzl) + 
#       geom_histogram(aes(weight=Pop), position="stack", bins=20) + theme(legend.position = "bottom") 

     xw[xw$Year==2017,] %>% 
       mutate(level_chosen= 
                if_else(as.numeric(as.character(democracy_cat)) == cat,
                    "  Countries with selected Democracy Score", "  All other countries    ")) -> xzl

       COUNTRY=FALSE
       if (COUNTRY) {
         ggplot(aes(x=combined, fill=as.factor(level_chosen)), data=xzl) + 
         geom_histogram(aes(weight=Pop), position="stack", bins=20) + theme(legend.position = "bottom") 
       } else {
         ggplot(aes(x=combined, fill=as.factor(level_chosen)), data=xzl) + 
         geom_histogram(position="stack", bins=20, alpha=0.6) + 
           scale_fill_manual(values=c("red", "blue")) +
          # xlim(c(0,1)) +
           xlab("Composite index (HDI and Democracy score)") + 
           ylab("Number of countries") +
    #       labs(caption = "Data source HDI: Human Development Data - UNDP / Data source democracy score: Economist Intelligence Unit (gapminder.org)") +
        #   ggtitle(" Where this country is expected to be in the recent landscape of a combining HDI Democracy index") +
         #  facet_grid(region ~ .) + 
           theme(legend.position = "bottom", 
                 legend.title = element_blank(), 
                 legend.text = element_text(size = 12),
                 axis.title =  element_text(size = 12),
                 axis.text =  element_text(size = 12),
              #   legend.key = element_text(size = 12),
                 plot.title = element_text(face="bold"))
       }
     
#     xzl %>% add_count(combined, democracy_cat, Year, weight=Pop) -> xz
#     ggplot(aes(x=combined, y=n, fill=as.factor(level_chosen)), data=xz) + 
#       geom_line(position="stack", bins=20) + theme(legend.position = "bottom") 
     
   }, height="auto")
   
}

# Run the application 
shinyApp(ui = ui, server = server)

