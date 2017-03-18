rm(list = ls())
library(shiny)
source("helpers.R")



df = readRDS("L1.rds")
df2 = readRDS("L2.rds")
df_state = readRDS("state.rds")
df2_summary = readRDS("L2_summary.rds")




shinyServer(function(input, output) {
    
    output$ava <- renderUI({
        HTML(paste(
            "number of diseases:", 
            "50", 
            '<br/>',
            "location availability:", 
            "51 states, 573 cities, 6 territories",
            '<br/>',
            "year covered:",
            "1887-2014", sep = '<br/>'))
        
    })
    
    
    output$plot0 <- renderPlotly({
        # plot number of deaths and cases by disease 
        plot_ly(df2_summary, x = disease, y = number, group = event, type ="bar", source = "s0")%>%
            layout(yaxis = list(type = "log", title = "number (in log scale)"), 
                xaxis = list(tickangle = 45),
                dragmode = "click", 
                title = "total US deaths and cases by disease",
                margin = list(b = 250))
        
    })
    
    # output$click0 <- renderPrint({
    #     d0 = event_data("plotly_click",source = "s0")
    #     if (is.null(d0)) "Hover events appear here (unhover to clear)" else d0$x[1]
    # })
    
    
    
    
    output$ava_disease <- renderUI({
        
        d0 = event_data("plotly_click",source = "s0")
        if(is.null(d0) == T) return(NULL)
        ava = text0(df2,d0$x[1])
        HTML(paste("disease selected:", d0$x[1],
            '<br/>',
            "location availability:", 
            ava[1], 
            '<br/>',
            "year covered:",
            ava[2], sep = '<br/>'))
        
    })
    
    
   
    
  

  
 
    output$plot1 <- renderPlotly({
        d0 = event_data("plotly_click",source = "s0")
        if(is.null(d0) == T) return(NULL)
        plot1(df2,d0$x[1])
    })

    # output$click1 <- renderPrint({
    #     d1 = event_data("plotly_click",source = "s1")
    #     if (is.null(d1)) "Hover events appear here (unhover to clear)" else d1
    # })

    
    output$text1 <- renderUI({
        
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        if(is.null(d1) == T) return(NULL)
        disease_select = d0$x[1]
        year_select = d1$x[1]
        ava = text1(df2,disease_select,year_select )
        HTML(paste("disease selected:", disease_select,
            '<br/>',
            "year selected:", year_select, 
            '<br/>',
            "location availability:", 
            ava, sep = '<br/>'))
        
    })   
    
    
    
    
    output$plot21 <- renderPlotly({
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        # If NULL dont do anything
        if(is.null(d1) == T) return(NULL)
        disease_select = d0$x[1]
        year_select = d1$x[1]
        df2_sub = subset(df2, disease == disease_select)
        df2_sub$year = as.integer(substr(df2_sub$epi_week, 1, 4))
        df2_sub= subset(df2_sub, year == year_select)
        df2_sub= aggregate(x = list(number= df2_sub$number), 
            by = list( event = df2_sub$event, state = df2_sub$state, 
                loc_type = df2_sub$loc_type ), 
            FUN = sum, na.omit = TRUE)

        df2_sub_state_c = droplevels(subset(df2_sub, loc_type !="TERR" & event == "CASES" ))
        if(nrow(df2_sub_state_c) == 0){return(NULL)}
        
        plot21(df2_sub_state_c, df_state, disease_select, year_select)

    })
    
    output$plot22 <- renderPlotly({
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        # If NULL dont do anything
        if(is.null(d1) == T) return(NULL)
        disease_select = d0$x[1]
        year_select = d1$x[1]
        df2_sub = subset(df2, disease == disease_select)
        df2_sub$year = as.integer(substr(df2_sub$epi_week, 1, 4))
        df2_sub= subset(df2_sub, year == year_select)
        df2_sub= aggregate(x = list(number= df2_sub$number), 
            by = list( event = df2_sub$event, state = df2_sub$state, 
                loc_type = df2_sub$loc_type ), 
            FUN = sum, na.omit = TRUE)
        
        df2_sub_state_d = droplevels(subset(df2_sub, loc_type !="TERR" & event == "DEATHS" ))
        if(nrow(df2_sub_state_d) == 0){return(NULL)}
        
        plot22(df2_sub_state_d, df_state, disease_select, year_select)
        
    })
    
    
    output$plot23 <- renderPlotly({
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        # If NULL dont do anything
        if(is.null(d1) == T) return(NULL)
        disease_select = d0$x[1]
        year_select = d1$x[1]
        df2_sub = subset(df2, disease == disease_select)
        df2_sub$year = as.integer(substr(df2_sub$epi_week, 1, 4))
        df2_sub= subset(df2_sub, year == year_select)
        df2_sub= aggregate(x = list(number= df2_sub$number), 
            by = list( event = df2_sub$event, state = df2_sub$state, 
                loc_type = df2_sub$loc_type ), 
            FUN = sum, na.omit = TRUE)
        
        df2_sub_terr = subset(df2_sub, loc_type =="TERR" )
        if(nrow(df2_sub_terr) == 0){return(NULL)}
        
        plot23(df2_sub_terr,  year_select)
        
    })
    
    
    # output$click2 <- renderPrint({
    #     d2 = event_data("plotly_click",source = "s2")
    #     if (is.null(d2)){
    #         "Hover events appear here (unhover to clear)"
    #         } else{
    #     
    #             if("x" %in% colnames(d2)){
    #                 d2$x[1]
    #             }else{
    #                 d2$key[1]
    #             }
    #             
    #             }
    #     
    # })

    output$plot3 <- renderPlotly({
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        d2 = event_data("plotly_click",source = "s2")
        if(is.null(d2) == T) return(NULL)
        if("z" %in% colnames(d2)){
            if(is.na(d2$z)==T)return(NULL)
            if(d2$z==0)return(NULL)
            state_select = d2$key
        }else{
            state_select = d2$x[1]
        }
        disease_select = d0$x[1]
        year_select = d1$x[1]
        
        plot3(df2, disease_select, year_select , state_select)
    })
    
    
 
    
    
  
    
    
  
    output$text3 <- renderText({
        
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        d2 = event_data("plotly_click",source = "s2")
        if(is.null(d2) == T) return(NULL)
        if("z" %in% colnames(d2)){
            if(is.na(d2$z)==T)return(NULL)
            if(d2$z==0)return(NULL)
            state_select = d2$key
        }else{
            state_select = d2$x[1]
        }
        disease_select = d0$x[1]
        year_select = d1$x[1]
        
        
        df2_sub = subset(df2, disease == disease_select & 
                year == year_select & state == state_select 
            &loc_type =="CITY")
        
        if(nrow(df2_sub)>0) return(NULL)
        
        "click bar to select week"
        
        
    })   
    
    
   
    
    


    output$plot4 <- renderPlotly({
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        d2 = event_data("plotly_click",source = "s2")
        if(is.null(d2) == T) return(NULL)
        if("z" %in% colnames(d2)){
            if(is.na(d2$z)==T)return(NULL)
            if(d2$z==0)return(NULL)
            state_select = d2$key
        }else{
            state_select = d2$x[1]
        }
        disease_select = d0$x[1]
        year_select = d1$x[1]
        
        
        df2_sub = subset(df2, disease == disease_select & 
                year == year_select & state == state_select 
            &loc_type =="CITY")
        
        if(nrow(df2_sub)==0) return(NULL)

         plot4(df2,  disease_select, year_select, state_select)


    })
    
    output$text4 <- renderText({
        
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        d2 = event_data("plotly_click",source = "s2")
        if(is.null(d2) == T) return(NULL)
        if("z" %in% colnames(d2)){
            if(is.na(d2$z)==T)return(NULL)
            if(d2$z==0)return(NULL)
            state_select = d2$key
        }else{
            state_select = d2$x[1]
        }
        disease_select = d0$x[1]
        year_select = d1$x[1]
        
        
        df2_sub = subset(df2, disease == disease_select & 
                year == year_select & state == state_select 
            &loc_type =="CITY")
        
        if(nrow(df2_sub) == 0) return(NULL)
        
        "click bar to select city"
        
        
    })   
    
    
  
    
   
    
   
    
    
    output$table4 <- renderUI({
        
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        d2 = event_data("plotly_click",source = "s2")
        d3 = event_data("plotly_click",source = "s3")
        if(is.null(d3) == T) return(NULL)
        if("z" %in% colnames(d2)){
            if(is.na(d2$z)==T)return(NULL)
            if(d2$z==0)return(NULL)
            state_select = d2$key
        }else{
            state_select = d2$x[1]
        }
        disease_select = d0$x[1]
        year_select = d1$x[1]
        week_select = d3$x[1]
        
        
        df2_sub = subset(df2, disease == disease_select & 
                year == year_select & state == state_select 
            &loc_type =="CITY")
        
        if(nrow(df2_sub)>0) return(NULL)
        
        
        df2_sub = subset(df2, disease == disease_select & 
                year == year_select & state == state_select)

        df2_sub = subset(df2_sub, week==week_select, select = 
                c(event, url))
        
        
        # data for a specific city 
        str = paste("week =", week_select)
        for(row in 1: nrow(df2_sub)){
            str = paste(str, '<br/>','<br/>','<br/>', df2_sub$event[row], "url: ", df2_sub$url[row]) 
        }
        
        HTML(str)
        
        
        
    })   
    
    
    
    
    
    
    
    
    # output$click3 <- renderPrint({
    #     d3 = event_data("plotly_click",source = "s3")
    #     if (is.null(d3)){
    #         "Hover events appear here (unhover to clear)"
    #     } else{d3}
    #     
    #     
    # })
    
    # output$click4 <- renderPrint({
    #     d4 = event_data("plotly_click",source = "s4")
    #     if (is.null(d4)){
    #         "Hover events appear here (unhover to clear)"
    #     } else{d4}
    #     
    # })
    # 
    
    output$plot5 <- renderPlotly({
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        d2 = event_data("plotly_click",source = "s2")
        d4 = event_data("plotly_click",source = "s4")
        if(is.null(d4) == T) return(NULL)
        if("z" %in% colnames(d2)){
            if(is.na(d2$z)==T)return(NULL)
            if(d2$z==0)return(NULL)
            state_select = d2$key
        }else{
            state_select = d2$x[1]
        }
        disease_select = d0$x[1]
        year_select = d1$x[1]
        city_select = d4$x[1]
        
        
        df2_sub = subset(df2, disease == disease_select & 
                year == year_select & loc == city_select)
        if(nrow(df2_sub) == 0){return(NULL)}
        plot5(df2_sub,  year_select, city_select)
        
        
        
    })

    # output$click5 <- renderPrint({
    #     d5 = event_data("plotly_click",source = "s5")
    #     if (is.null(d5)) "Hover events appear here (unhover to clear)" else d5
    # })
    
    
    
    output$table5 <- renderUI({
        
        d0 = event_data("plotly_click",source = "s0")
        d1 = event_data("plotly_click", source = "s1" )
        d2 = event_data("plotly_click",source = "s2")
        d4 = event_data("plotly_click",source = "s4")
        d5 = event_data("plotly_click",source = "s5")
        if(is.null(d5) == T) return(NULL)
        if("z" %in% colnames(d2)){
            if(is.na(d2$z)==T)return(NULL)
            if(d2$z==0)return(NULL)
            state_select = d2$key
        }else{
            state_select = d2$x[1]
        }
        disease_select = d0$x[1]
        year_select = d1$x[1]
        week_select = d5$x[1]
        city_select = d4$x[1]
        

        df2_sub = subset(df2, disease == disease_select & 
                week == week_select & loc == city_select 
            &loc_type =="CITY")
        
        if(nrow(df2_sub)==0) return(NULL)
        
        
        
        # data for a specific city 
        str = paste("week =", week_select)
        for(row in 1: nrow(df2_sub)){
            str = paste(str, '<br/>','<br/>','<br/>', df2_sub$event[row], "url: ", df2_sub$url[row]) 
        }
        
        HTML(str)
        
        
        
    })   
    
    
    output$text5 <- renderText({
        
        d4 = event_data("plotly_click",source = "s4")
        if(is.null(d4) == T) return(NULL)
    
        
        "click bar to select week"
        
        
    })   
    
    
    
    
    # 
    # output$plot5 <- renderPlotly({
    #     d0 = event_data("plotly_click",source = "s0")
    #     d1 = event_data("plotly_click", source = "s1" )
    #     d2 = event_data("plotly_click",source = "s2")
    #     d4 = event_data("plotly_click",source = "s4")
    #     if(is.null(d2) == T) return(NULL)
    #     if(is.null(d4) == T) return(NULL)
    #     if(is.na(d2$z)==T)return(NULL)
    #     if(d2$z==0)return(NULL)
    #     plot5(df, df_state,  d0$x, d1$x, d4$x)
    #     
    # })
    
    
    
})



# output$text1 <- renderText({
#     d0 = event_data("plotly_click",source = "s0")
#     if(is.null(d0) == T) return(NULL)
#     return(d0$x)
# })
# 
# output$text2 <- renderText({
#     d0 = event_data("plotly_click",source = "s0")
#     if(is.null(d0) == T) return(NULL)
#     ava = text0(df,d0$x)
#     return(paste( ava[1], "states,",ava[2], "cities"))
# })
# 
# output$text3<- renderText({
#     d0 = event_data("plotly_click",source = "s0")
#     if(is.null(d0) == T) return(NULL)
#     ava = text0(df,d0$x)
#     return(ava[3])
# })

