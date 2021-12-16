library(tidyverse)
library(shiny)
library(shinythemes)
library(lubridate)


setwd("~/Documents/R/f1/tracks_ex")
if(file.exists("~/Documents/R/f1/tracks_ex/data.csv")){
  data <- read_csv2("~/Documents/R/f1/tracks_ex/data.csv")
}

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  includeCSS("css/style.css"),
  
  titlePanel("F1Authentics Auctions"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("text", h3("Search"), 
                value = ""),
      
      sliderInput(inputId = "price",
                  label = "Price:",
                  min = min(data$CurrentPrice),max = max(data$CurrentPrice),
                  value = c(min(data$CurrentPrice),max(data$CurrentPrice))),
      
    ),
    mainPanel(
      uiOutput("Case")
      
    )
  )
)

server <- function(input, output) {
  output$Case <- renderUI({
    #invalidateLater(1000)
    #print(input$price[2])
      data <- data %>% filter(grepl(tolower(input$text), tolower(Title))) %>% filter(CurrentPrice %in% (input$price[1]:input$price[2]))
      if(nrow(data)>0){
        case <- lapply(1:nrow(data), function(file){
          tags$div(
            tags$a(data,
              class="case",
              #tags$button("k")
              tags$img(src=data$ProductImg[file], width="150px", height="150px"),
              tags$h4(data$Title[file]),
              tags$h5(data$End[file]),
              tags$h5((round(as.period(as.POSIXct(data$End[file],tz="Europe/London")-Sys.time())))),
            ),
              tags$div(
                id="price",
                tags$h4(paste("£",data$CurrentPrice[file])),
                tags$h5(paste("n.of.bids-",data$NBids[file]))
              ),
              tags$hr(),
            tag_name=rownames(data))
        }) 
      do.call(tagList, case)
    }else{
      tags$div(
        tags$h3("Nothing to be shown.")
      )
    }
  })
  
}

shinyApp(ui = ui, server = server)