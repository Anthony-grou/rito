library(shiny)
source("1 - Riot API - fonctions.R")


ui = fluidPage(
  titlePanel("Riot API"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("sum_name", label="Nom d'invocateur :"),
      submitButton("Recherche"),
      br(),
      p("Nom crypto :"),
      fluidRow(column(12, verbatimTextOutput("crypto", )))
    ),
    
    mainPanel(
      DT::dataTableOutput("table"),
      plotlyOutput("pie"),
      plotlyOutput("ratio")
    )
  )
)
