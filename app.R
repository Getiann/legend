#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
# Define UI for application that draws a histogram
ui <- fluidPage(

    
    titlePanel("Customize Your Heart"),

    
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "into the heart",
                        min = 2,
                        max = 10,
                        value = 2),
            sliderInput("size",
                        "change heart size",
                        min = 0,
                        max = 5,
                        value = 2),
            textInput("title",
                      "write a title",
                      "MYheart"),
            radioGroupButtons(
              inputId = "col",
              label = "choose colour",
              choices = c("red","blue","green","purple","pink","black","gold"),
              checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon")),
              )
        ),
       
       
      mainPanel(
           plotOutput("distPlot"),
           textOutput("write")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  
  output$distPlot <- renderPlot({
        
      t <- seq(0,input$bins*pi,0.01)
      x <- 16*sin(t)^3
      y <- 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
      par(pin=c(input$size,input$size))  

        
    
      plot(x,y,type = "h",ann = F,bty="n",xaxt="n",yaxt="n",col = "white")
      polygon(x,y,col = input$col,border = input$col)
     
    })
  output$write <- renderText({input$title})
    
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
