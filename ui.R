

library(plotly)
library(shiny)
library(shinythemes)

color = "color:#cc6600"
shinyUI(fluidPage(
    # Set theme
    theme = shinytheme("spacelab"),
    # title
    h3("Tycho Project data explorer"),
    tags$hr(),
    
###############################################################
    # L1 summary
    h4("L1 data summary", align = "left"),
    fixedRow(
        column(2,
            br(),
            br(),
            verbatimTextOutput("summary_L1")
        ),
    
        column(9, align= "center",
            plotlyOutput("plot_L1", height = "400px")
        )
    ), 


    tags$hr(),
    
###############################################################    
    # L2 summary 
    h4("L2 data summary", align = "left"),
    fixedRow(
        column(2,
            br(),
            br(),
            verbatimTextOutput("summary_L2")
        ),
        
        column(9, align= "center",
            plotlyOutput("plot_L2", height = "600px"),
            strong("click bar to explore a disease", style = color)
        )
        
    ), 
 
       
    tags$hr(),
    
    # disease 
    #verbatimTextOutput("L2_click"), 
    
###############################################################
    # selected disease 
    fixedRow(
        column(2,
            br(),
            br(),
            verbatimTextOutput("summary_L2_disease")
        ),
        
        column(9, align= "center",
            plotlyOutput("plot_L2_disease", height = "400px"), 
            strong("click bar to explore a year", style = color)
        )
    ), 
  
    tags$hr(),
    
    # year 
    # verbatimTextOutput("L2_click2"),
    
###############################################################
 # selected disease, year 
    fixedRow(
        column(6,
            br(),
            verbatimTextOutput("summary_L2_disease_year")
        )
    ), 
  
    fixedRow(
        column(6, 
            plotlyOutput("state_cases", height = "500px")
        ),
        column(6,
            plotlyOutput("state_deaths", height = "500px")
        )
    ),
    
    fixedRow(
        column(2
        ),
        column(2, 
            plotlyOutput("terr_cases"), height = "250px", inline = FALSE
        ),
        column(4
        ),
        column(2,
            plotlyOutput("terr_deaths"), height = "250px", inline = FALSE
        )
    ),
    
    fixedRow(align = "center",
            strong("click map/bar to explore state/territory", style = color)
        ),
        
    tags$hr(),
    
    # state 
    # verbatimTextOutput("L2_click3"), 
    
##############################################################################
    # disease, year, state 
    fixedRow(
        column(6,
            br(),
            verbatimTextOutput("summary_state")
        )
    ), 
    
    fixedRow(
        column(6, align = "center",
            plotlyOutput("plot_state", height = "500px"),
            textOutput("text_state")
        ),
        column(6, align = "center",
             plotlyOutput("plot_loc", height = "500px"),
            textOutput("text_loc")
        )
       
    ),
    
    # city
    # verbatimTextOutput("L2_click4"), 
    # week 
    # verbatimTextOutput("L2_click5"), 
    
    # table for selected week
    fixedRow(
        column(9, align = "center",
            dataTableOutput('table')
        )
    )
##############################################################################
    )
  )
