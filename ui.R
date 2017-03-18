

library(plotly)
library(shiny)
library(shinythemes)

color = "color:#cc6600"
shinyUI(fluidPage(

    # Set theme
    theme = shinytheme("spacelab"),
    
    # Some help text
    h3("Tycho Project L1 L2 data explorer"),
    tags$hr(),
  
    fixedRow(
        column(2,
            br(),
            br(),
            htmlOutput("ava")),
        
        column(9, align= "center",
            plotlyOutput("plot0", height = "600px"),
            strong("click bar to select disease", style = color)
            )
    
    ), 
    

    
    tags$hr(),

     #verbatimTextOutput("click0"),
    
    fixedRow(
        column(2,
            br(),
            br(),
            htmlOutput("ava_disease")),

        column(9, align = "center",
            plotlyOutput("plot1", height = "400px"), 
            strong("click bar to select year", style = color)
            )
    ),
    
  

    tags$hr(),

    #verbatimTextOutput("click1"),

    fixedRow(
        column(2,
            br(),
            br(),
            htmlOutput("text1")),

        column(5, 
            plotlyOutput("plot21", height = "500px")),

        column(5,
            plotlyOutput("plot22", height = "500px"))
    ),

    fixedRow( 
        column(2), 
        column(9, align = "center",
            strong("click map to select state", style = color))
    ), 
    
    fixedRow(
        column(2),
        column(5, align = "center", 
            plotlyOutput("plot23", height = "300px")
        
            )
    ), 
    
    fixedRow( 
        column(2), 
        column(9, align = "center",
            strong("click bar to select territory", style = color))
    ), 
    
    



    tags$hr(),
    #verbatimTextOutput("click2"), 


    fixedRow(
        column(2),
        column(5, plotlyOutput("plot3",height = "400px")),
        column(5,  htmlOutput("table4"), 
            plotlyOutput("plot4",height = "400px"))

    ),
    
    
    fixedRow(
        column(2),
        column(5, align ="center",  textOutput("text3")),
        column(5, align = "center", textOutput("text4"))
        
    ),
    
    


    #verbatimTextOutput("click3"), 
    #verbatimTextOutput("click4"), 
    
    
    fixedRow(
        column(2),
        column(5, align = "center",
            plotlyOutput("plot5",height = "400px"), 
            textOutput("text5")), 
        column(5,  htmlOutput("table5"))
        
    )
    
    #verbatimTextOutput("click5")


    # # tags$hr(),
    # # fixedRow(
    # #     column(2,
    # #         br(),
    # #         br(),
    # #         strong("click bar to select city", style = color)),
    # #     column(6, plotlyOutput("plot4",height = "200px"))
    # # ),
    # # 
    # # tags$hr(),
    # # verbatimTextOutput("click4")
    # 
    # 
    # # fixedRow(
    # #     column(2),
    # #     column(6, plotlyOutput("plot5",height = "200px"))
    # # )

     
    )
  )
