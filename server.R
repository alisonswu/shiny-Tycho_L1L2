rm(list = ls())

# install packages if missing
# list.of.packages <- c("shiny","plotly","shinythemes","data.table")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
library(plotly)
library(shiny)
library(data.table)

# read data frames
df= readRDS("L1.rds")
df2 = readRDS("L2.rds")
state_list = readRDS("state_list.rds")

# convert to data.table format 
L1 = data.table(df)
L2 = data.table(df2)
setkey(L2, disease, year, state)

# color for plots 
colors = c('#ff7f0e', '#1f77b4')

shinyServer(function(input, output) {
    
    # L1 summary text 
    output$summary_L1 <- renderText({
        disease_num = length(levels(L1$disease))
        state_num = length(levels(droplevels(L1[loc_type == "STATE"])$state))
        city_num = length(levels(droplevels(L1[loc_type == "CITY"])$loc))
        terr_num = length(levels(droplevels(L1[loc_type == "TERR"])$loc))
        year_range = range(L1$year)
        paste(
            "diseases:", 
            disease_num, 
            '\n',
            "location:", 
            paste(state_num, "states"),
            paste(city_num, "cities"),
            paste(terr_num, "territories"), 
            '\n',
            "year:",
            paste(year_range[1], "-", year_range[2]), 
            sep = '\n')
    })
    
    # L1 summary plot 
    output$plot_L1 <- renderPlotly({
        L1_summary <- L1[, .(cases = sum(number)), by = disease ][order(-cases)]
        plot_ly(L1_summary, x = ~disease, y = ~cases, type ="bar", 
                color = I(colors[2]), source = "L1")%>%
        layout(yaxis = list(title = "cases"), 
                dragmode = "click",
                title = "cases by disease")
    })
    
####################################################################
    # L2 summary text 
    output$summary_L2 <- renderText({
        disease_num = length(levels(L2$disease))
        state_num = length(levels(droplevels(L2[loc_type != "TERR"])$state))
        city_num = length(levels(droplevels(L2[loc_type == "CITY"])$loc))
        terr_num = length(levels(droplevels(L2[loc_type == "TERR"])$loc))
        year_range = range(L2$year)
        paste(
            "diseases:", 
            disease_num, 
            '\n',
            "location:", 
            paste(state_num, "states"),
            paste(city_num, "cities"),
            paste(terr_num, "territories"), 
            '\n',
            "year:",
            paste(year_range[1], "-", year_range[2]), 
            sep = '\n')
    })
    
    # L2 summary plot 
    output$plot_L2 <- renderPlotly({
        L2_summary <- L2[, .(number = sum(number)), 
            by = .(disease, event) ][order(-event,-number)]
       
        L2_summary %>%
        plot_ly(x = ~disease, y = ~number, type ="bar", source = "L2", 
                color = ~event, colors = colors)%>%
        layout(yaxis = list(type = "log", 
                title = "number (in log scale)"), 
                xaxis = list(tickangle = 45),
                dragmode = "click",  barmode = 'group', 
                title = "cases and deaths by disease",
                margin = list(b = 250))
    })
        
####################################################################    
    # clicked disease 
    output$L2_click <- renderPrint({
         L2_click = event_data("plotly_click",source = "L2")
         if (is.null(L2_click)) "Hover events appear here (unhover to clear)" 
         else L2_click
     })
    
    # summary text for clicked disease
    output$summary_L2_disease <- renderText({
        L2_click = event_data("plotly_click",source = "L2")
        if(is.null(L2_click) == T) return(NULL)
        disease_select =L2_click$x[1]
        L2_sub<- droplevels(L2[disease_select])
        disease_num = length(levels(L2_sub$disease))
        state_num = length(levels(droplevels(L2_sub[loc_type != "TERR"])$state))
        city_num = length(levels(droplevels(L2_sub[loc_type == "CITY"])$loc))
        terr_num = length(levels(droplevels(L2_sub[loc_type == "TERR"])$loc))
        year_range = range(L2_sub$year)
        paste(
            paste( disease_select), 
            '\n', 
            "location:", 
            paste(state_num, "states"),
            paste(city_num, "cities"),
            paste(terr_num, "territories"), 
            '\n',
            "year:",
            paste(year_range[1], "-", year_range[2]), 
            sep = '\n')
    })

    
    # summary plot for clicked disease 
    output$plot_L2_disease <- renderPlotly({
        L2_click = event_data("plotly_click",source = "L2")
        if(is.null(L2_click) == T) return(NULL)
        disease_select =L2_click$x[1]
        L2_sub<- L2[disease == disease_select, 
            .(number = sum(number)), by = .(year, event)] 
        
        plot_ly(L2_sub, x = ~year, y = ~number, color= ~event,
                type = 'bar', 
                colors = colors, source = "L2_disease")%>%
        layout(dragmode = "click", 
                title = paste("number of", disease_select, "by year")
              )
    })
    
######################################################################
    # clicked year 
    output$L2_click2 <- renderPrint({
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        if (is.null(L2_click2)) "Hover events appear here (unhover to clear)" 
        else L2_click2
    })
    
    # summary text for disease, year 
    output$summary_L2_disease_year <- renderText({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        if(is.null(L2_click2) == T) return(NULL)
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        
        L2_sub = L2[ .(disease_select, year_select), nomatch =0]
        L2_sub <- droplevels(L2_sub)
        disease_num = length(levels(L2_sub$disease))
        state_num = length(levels(droplevels(L2_sub[loc_type != "TERR"])$state))
        city_num = length(levels(droplevels(L2_sub[loc_type == "CITY"])$loc))
        terr_num = length(levels(droplevels(L2_sub[loc_type == "TERR"])$loc))
        year_range = range(L2_sub$year)
        paste(
            paste(disease_select,",",year_select), 
            '\n',
            "location:", 
            paste(state_num, "states"),
            paste(city_num, "cities"),
            paste(terr_num, "territories"), 
            sep = '\n')
        
    })
    
    # plot cases by state 
    output$state_cases <- renderPlotly({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        if(is.null(L2_click2) == T) return(NULL)
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        
        disease_select = "TUBERCULOSIS [PHTHISIS PULMONALIS]"
        year_select = 1922

        L2_sub = L2[ .(disease_select, year_select), nomatch =0]
        if(L2_sub[,.N] == 0) return(NULL)
        L2_sub = L2_sub[loc_type != "TERR" & event == "CASES", , nomatch =0]
        if(L2_sub[,.N] == 0) return(NULL)
        L2_sub = droplevels(L2_sub[, .(cases = sum(number)), by = state])
        L2_sub$loc1 = sapply(L2_sub$state, function(x) 
                            state_list[[as.character(x)]])
        
        L2_sub$hover <- with(L2_sub, paste("cases", '<br>',"in",loc1))
        l <- list(color = toRGB("grey"), width = 2)
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = FALSE,
            lakecolor = toRGB('blue')
        )
        
        plot_ly(L2_sub, z = ~cases, text = ~hover, key = ~state, 
                locations = ~state,  type = "choropleth",
                locationmode = 'USA-states',color = ~cases, colors = 'Blues',
                marker = list(line=l), colorbar = list(title = "cases"), 
                source = "state"
        )%>%
        layout( title = paste("cases by state"),
                geo = g,
                dragmode = "click"
        )
        })
        
    # plot deaths by state 
    output$state_deaths <- renderPlotly({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        if(is.null(L2_click2) == T) return(NULL)
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        
        L2_sub = L2[ .(disease_select, year_select), nomatch =0]
        if(L2_sub[,.N] == 0) return(NULL)
        L2_sub = L2_sub[loc_type != "TERR" & event == "DEATHS", , nomatch =0]
        if(L2_sub[,.N] == 0) return(NULL)
        L2_sub = droplevels(L2_sub[, .(deaths = sum(number)), by = state])
        L2_sub$loc1 = sapply(L2_sub$state, function(x) 
            state_list[[as.character(x)]])
        
        L2_sub$hover <- with(L2_sub, paste("deaths", '<br>',"in",loc1))
        l <- list(color = toRGB("grey"), width = 2)
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = FALSE,
            lakecolor = toRGB('blue')
        )
        
        plot_ly(L2_sub, z = ~deaths, text = ~hover, key = ~state, 
            locations = ~state,  type = "choropleth",
            locationmode = 'USA-states',color = ~deaths, colors = 'Oranges',
            marker = list(line=l), colorbar = list(title = "deaths"), 
            source = "state"
        )%>%
            layout( title = paste("deaths by state"),
                geo = g,
                dragmode = "click"
            )
    })
    
    # plot cases by territory
    output$terr_cases <- renderPlotly({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        if(is.null(L2_click2) == T) return(NULL)
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        L2_sub = L2[ .(disease_select, year_select), nomatch =0]
        if(L2_sub[,.N] == 0) return(NULL)
        L2_sub = L2_sub[loc_type == "TERR" & event == "CASES", , nomatch =0]
        if(L2_sub[,.N] == 0) return(NULL)
        L2_sub =droplevels(L2_sub[, .(number = sum(number)), by = state])
        levels(L2_sub$state) =  c("AS","GU","MP","PR","PT","VI")
        
        plot_ly(L2_sub, x =~state, y= ~number, key = ~state, type ="bar", 
                source = "state"
        ) %>%
        layout(title = paste("cases by territory"), 
                xaxis = list(title = ''),
                yaxis = list(title = 'cases'),
                dragmode = "click"
        )
    })
    
    # plot deaths by territory  
    output$terr_deaths <- renderPlotly({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        if(is.null(L2_click2) == T) return(NULL)
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        L2_sub = L2[ .(disease_select, year_select), nomatch = 0]
        if(L2_sub[,.N] == 0) return(NULL)
        L2_sub = L2_sub[loc_type == "TERR" & event == "DEATHS", nomatch =0]
        if(L2_sub[,.N] == 0) return(NULL)
        L2_sub =droplevels(L2_sub[, .(number = sum(number)), by = state])
        levels(L2_sub$state) =  c("AS","GU","MP","PR","PT","VI")
        
        plot_ly(L2_sub, x =~state, y=~number, key = ~state, type ="bar", 
                source = "state"
        ) %>%
        layout(title = paste("deaths by territory"), 
                xaxis = list(title = ''),
                yaxis = list(title = 'deaths'),
                dragmode = "click"
        )
    })
    
#######################################################################
    # clicked state 
    output$L2_click3 <- renderPrint({
        L2_click3 = event_data("plotly_click",source = "state")
        if (is.null(L2_click3)) "Hover events appear here (unhover to clear)" else L2_click3
    })
    
    # summary for disease, year, state 
    output$summary_state <- renderText({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        L2_click3 = event_data("plotly_click",source = "state")
        if(is.null(L2_click3) == T) return(NULL)
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        state_select = L2_click3$key
        L2_sub = droplevels(L2[ .(disease_select, year_select, state_select)
                                 , nomatch =0])
        cases_num = L2_sub[event=="CASES",sum(number)]
        deaths_num= L2_sub[event=="DEATHS",sum(number)]
        location_num = length(unique(L2_sub$loc))
        paste(
            paste(disease_select,",",year_select,",", state_select), 
            '\n',
            paste(location_num, "locations"), 
            paste(cases_num, "cases", ",", deaths_num, "deaths"),
            sep = '\n')
    })
    
    
    # if there is only one location, 
    # plot weekly counts for disease, year, location
    
    # if there is more than one location, 
    # plot counts by location
    
    output$plot_state <- renderPlotly({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        L2_click3 = event_data("plotly_click",source = "state")
        if(is.null(L2_click3) == T) return(NULL)
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        state_select = L2_click3$key
        L2_sub = droplevels(L2[ .(disease_select, year_select, state_select)
            , nomatch =0])
        if(nrow(L2_sub)==0) return(NULL)
        location_num = length(unique(L2_sub$loc))
        
        if(location_num >1){
            L2_sub %>%
                plot_ly(x = ~loc, y = ~number, type ="bar", source = "city", 
                    color = ~event, colors = colors)%>%
                layout(yaxis = list(type = "log", 
                    title = "number"), 
                    xaxis = list(tickangle = 45),
                    dragmode = "click",  barmode = 'group', 
                    title = paste(year_select,
                        "cases and deaths by locations in",state_select),
                    margin = list(b = 200))
            
        }else{
            d1 = as.Date(paste("1", 52, year_select-1, sep = "-"), 
                format = "%w-%W-%Y")
            d2 = as.Date(paste("1", 1, year_select+1, sep = "-"), 
                format = "%w-%W-%Y")
            
            data1 = data.table(week = c(d1,d1,d2,d2), 
                               event = c("CASES","DEATHS","CASES","DEATHS"),
                               number = c(0,0,0,0))
                
            L2_sub = L2_sub[,.(week, event, number)]
            L2_sub = rbind(L2_sub,data1)
            L2_sub %>%
                plot_ly(x = ~week, y = ~number, type ="bar", source = "week", 
                    color = ~event, colors = colors)%>%
                layout(yaxis = list(
                    title = "number"), 
                    dragmode = "click",  barmode = 'group', 
                    title = paste(year_select,
                        "weekly cases and deaths in",state_select), 
                    margin = list(b = 200)
                )
            
        }
      
    })
    
    output$text_state <- renderText({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        L2_click3 = event_data("plotly_click",source = "state")
        if(is.null(L2_click3) == T) return(NULL)
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        state_select = L2_click3$key
        L2_sub = droplevels(L2[ .(disease_select, year_select, state_select)
            , nomatch =0])
        if(nrow(L2_sub)==0) return(NULL)
        location_num = length(unique(L2_sub$loc))
        
        if(location_num >1){
          paste("click bar to select location", "\n")
        }else{
          paste("click bar to select week", "\n")
        }
    })
    
    
    
    
    
  ###########################################################################  
    # click location
    output$L2_click4 <- renderPrint({
        L2_click4 = event_data("plotly_click",source = "city")
        if (is.null(L2_click4)) "Hover events appear here (unhover to clear)" else L2_click4
    })

    # plot weekly counts for location 
    
    output$plot_loc <- renderPlotly({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        L2_click3 = event_data("plotly_click",source = "state")
        L2_click5 = event_data("plotly_click",source = "city")
        if(is.null(L2_click5) == T) return(NULL)
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        state_select = L2_click3$key
        loc_select = L2_click5$x[1]
        L2_sub = droplevels(L2[ .(disease_select, year_select, state_select)
            , nomatch =0])
        if(nrow(L2_sub)==0) return(NULL)
        L2_sub = L2_sub[loc ==loc_select]
            d1 = as.Date(paste("1", 52, year_select-1, sep = "-"), 
                format = "%w-%W-%Y")
            d2 = as.Date(paste("1", 1, year_select+1, sep = "-"), 
                format = "%w-%W-%Y")
            
            data1 = data.table(week = c(d1,d1,d2,d2), 
                event = c("CASES","DEATHS","CASES","DEATHS"),
                number = c(0,0,0,0))
            
            L2_sub = L2_sub[,.(week, event, number)]
            L2_sub = rbind(L2_sub,data1)
            L2_sub %>%
                plot_ly(x = ~week, y = ~number, type ="bar", source = "week", 
                    color = ~event, colors = colors)%>%
                layout(yaxis = list(
                    title = "number"), 
                    dragmode = "click",  barmode = 'group', 
                    title = paste(year_select,
                        "weekly cases and deaths in",loc_select), 
                    margin = list(b = 200)
                )
        
    })
    
    
    output$text_loc <- renderText({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        L2_click3 = event_data("plotly_click",source = "state")
        L2_click5 = event_data("plotly_click",source = "city")
        if(is.null(L2_click5) == T) return(NULL)
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        state_select = L2_click3$key
        loc_select = L2_click5$x[1]
        L2_sub = droplevels(L2[ .(disease_select, year_select, state_select)
            , nomatch =0])
        if(nrow(L2_sub)==0) return(NULL)
        paste("click bar to select week", "\n")
    })
    
    
 
    
    # click week 
    output$L2_click5 <- renderPrint({
        L2_click5 = event_data("plotly_click",source = "week")
        if (is.null(L2_click5)) "Hover events appear here (unhover to clear)" else L2_click5
    })
    
    # output info for selected week 
    
    output$table <- renderDataTable({
        L2_click = event_data("plotly_click",source = "L2")
        L2_click2 = event_data("plotly_click",source = "L2_disease")
        L2_click3 = event_data("plotly_click",source = "state")
        L2_click4 = event_data("plotly_click",source = "city")
        L2_click5 = event_data("plotly_click",source = "week")
        if(is.null(L2_click5) == T) return(NULL)
        if(is.null(L2_click4) == F){
            city = L2_click4$x[1]
            L2_sub = L2[loc ==city]
        }
        disease_select = L2_click$x[1]
        year_select = L2_click2$x[1]
        state_select = L2_click3$key
        week_select = L2_click5$x[1]
        L2_sub = droplevels(L2_sub[ .(disease_select, year_select, state_select)
            , nomatch =0])
        
        if(nrow(L2_sub)==0) return(NULL)
        L2_sub = L2_sub[week ==week_select]
        L2_sub$URL = paste0("<a href='",L2_sub$url,"'>",L2_sub$url,"</a>")
        
        return(L2_sub[,.(loc, event,
                number,from_date,to_date,URL)])
        
    }, escape = FALSE)
    
    
    


    
    
    
#############################################################################

})    
    
 