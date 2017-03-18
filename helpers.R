
# install packages if missing
list.of.packages <- c("shiny","plotly","shinythemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

remove(list = ls())
library(plotly)



# data availability at state and city level 
text0<- function(df2,disease_select){
    df2_sub = subset(df2, disease == disease_select)
    df2_sub_non_terr = droplevels(subset(df2_sub,  loc_type != "TERR"))
    df2_sub_terr = droplevels(subset(df2_sub, loc_type =="TERR"))
    df2_sub_city = droplevels(subset(df2_sub, loc_type =="CITY"))
    num_terr = length(levels(df2_sub_terr$loc))
    num_state = length(levels(df2_sub_non_terr$state))
    num_city = length(levels(df2_sub_city$loc))
    str1 = paste(num_state,"states," , num_city, "cities,", num_terr, "territories")
    year_range = range(df2_sub$year)
    str2 = paste(year_range[1], "-", year_range[2])
    return(c(str1, str2))
}

# plot bar chart of yearly count for a given disease 
plot1 <- function(df2, disease_select){
    df2_sub = subset(df2, disease == disease_select)
    df2_sub$year = as.integer(substr(df2_sub$epi_week, 1, 4))
    df2_sub_disease = aggregate(x = list(number= df2_sub$number), 
        by = list(year = df2_sub$year, event = df2_sub$event), 
        FUN = sum, na.omit = TRUE)
    
    # bar plot 
    plot_ly(df2_sub_disease, x = year, y = number, group = event, type ="bar", 
        source = "s1")%>%
        layout(title = paste("counts of ", disease_select, "by year"), 
            dragmode = "click",
            margin = list(b = 100))
    
}



# data availability at state and city level 
text1 <- function(df2,disease_select, year_select){
    df2_sub = subset(df2, disease == disease_select & year == year_select)
    df2_sub_non_terr = droplevels(subset(df2_sub,  loc_type != "TERR"))
    df2_sub_terr = droplevels(subset(df2_sub, loc_type =="TERR"))
    df2_sub_city = droplevels(subset(df2_sub, loc_type =="CITY"))
    num_terr = length(levels(df2_sub_terr$loc))
    num_state = length(levels(df2_sub_non_terr$state))
    num_city = length(levels(df2_sub_city$loc))
    str1 = paste(num_state,"states," , num_city, "cities,", num_terr, "territories")

    return(str1)
}


# plot US map for given year and disease 

plot21 <-function(df2_sub_state_c, df_state, disease_select, year_select){
    if(nrow(df2_sub_state_c)>0){
        df_state1 = df_state
        for(i in 1:nrow(df2_sub_state_c)){
            state = as.character(df2_sub_state_c$state[i])
            df_state1$cases[which(df_state1$state == state)] = df2_sub_state_c$number[i]
        }
        df_state1$state = as.character(df_state1$state)
        df_state1$loc = as.character(df_state1$loc)
        
        df_state1$hover <- with(df_state1, paste("cases", '<br>',"in",loc))
        l <- list(color = toRGB("grey"), width = 2)
        
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = FALSE,
            lakecolor = toRGB('blue')
        )
        # 
        key = df_state1$state
        plot_ly(df_state1, z = cases, text = hover, key = key, locations = state,  type = "choropleth",
            locationmode = 'USA-states',color = cases, colors = 'Blues',
            marker = list(line=l), colorbar = list(title = "cases"), source = "s2"
        )%>%
            layout(
                title = paste( "year = ", year_select,", cases by state"  ),
                geo = g,
                dragmode = "click"
            )
    } 
    
}



#plot22
# plot US map for given year and disease 

plot22 <-function(df2_sub_state_d, df_state, disease_select, year_select){
    if(nrow(df2_sub_state_d)>0){
        df_state1 = df_state
        for(i in 1:nrow(df2_sub_state_d)){
            state = as.character(df2_sub_state_d$state[i])
            df_state1$cases[which(df_state1$state == state)] = df2_sub_state_d$number[i]
        }
        df_state1$deaths = df_state1$cases
        df_state1$state = as.character(df_state1$state)
        df_state1$loc = as.character(df_state1$loc)
        
        df_state1$hover <- with(df_state1, paste("deaths", '<br>',"in",loc))
        l <- list(color = toRGB("grey"), width = 2)
        
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = FALSE,
            lakecolor = toRGB('blue')
        )
        # 
        key = df_state1$state
        plot_ly(df_state1, z = deaths, text = hover, key = key, locations = state,  type = "choropleth",
            locationmode = 'USA-states',color = deaths, colors = 'Oranges',
            marker = list(line=l), colorbar = list(title = "deaths"), source = "s2"
        )%>%
            layout(
                title = paste( "year = ", year_select,", deaths by state"  ),
                geo = g,
                dragmode = "click"
            )
        
    } 
    
}





#plot23

plot23 <-function(df2_sub_terr,  year_select){
    if(nrow(df2_sub_terr)>0){

        plot_ly(df2_sub_terr, x =state, y= number, key = state, group = event, type ="bar", source = "s2") %>%
            layout(title = paste( "year = ", year_select,", cases by territory"  ), 
                yaxis = list(title = 'number'),
                dragmode = "click")
        
    }
    
}





#plot24





plot2 <- function(df, df_state, disease_select, year_select){
    df_disease_year = subset(df, disease == disease_select&year ==year_select)
    df_disease_year = aggregate(x = list(cases = df_disease_year$cases), 
        by = list(state = df_disease_year$state), FUN = sum, na.rm = TRUE)
    df_state1 = df_state
    for(i in 1:nrow(df_disease_year)){
        state = as.character(df_disease_year$state[i])
        df_state1$cases[which(df_state1$state == state)] = df_disease_year$cases[i]
    }
    df_state1$state = as.character(df_state1$state)
    df_state1$loc = as.character(df_state1$loc)
    # create a plot
    # hover over information 
    df_state1$hover <- with(df_state1, paste("cases", '<br>',"in",loc))
    l <- list(color = toRGB("grey"), width = 2)
    
    # specify some map projection/options
    g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = FALSE,
        lakecolor = toRGB('blue')
    )
    # 
    key = df_state1$state
    plot_ly(df_state1, z = cases, text = hover, key = key, locations = state,  type = "choropleth",
        locationmode = 'USA-states',color = cases, colors = 'Purples',
        marker = list(line=l), colorbar = list(title = "cases"), source = "s2"
    )%>%
        layout(
            title = paste( "year = ", year_select,", cases by state"  ),
            geo = g,
            dragmode = "click"
        )
    
}    




plot3 <- function(df2,  disease_select, year_select, state_select){
    
    df2_sub = subset(df2, disease == disease_select &
            year == year_select & state == state_select)
    
    
    
    plot_ly(df2_sub, x = week, y= number, group = event, type ="bar", source = "s3") %>%
        layout(
            title = paste("year = ", year_select,", state = ", state_select,  ", weekly counts"),
            yaxis = list(title = 'number'),
            margin = list(b = 150))

    
}


# plot by city 
plot4 <- function(df2,  disease_select, year_select, state_select){
    df2_sub = subset(df2, disease == disease_select & 
            year == year_select & state == state_select 
        &loc_type =="CITY")
    
    if(nrow(df2_sub)>0){
        df2_sub = aggregate(x = list(number = df2_sub$number), 
            by = list(city = df2_sub$loc, event = df2_sub$event), 
            FUN = sum, na.rm = TRUE)
        
        plot_ly(df2_sub , x = city, y= number, group =event, type ="bar", source = "s4") %>%
            layout(
                title = paste("year = ", year_select,", state = ", state_select,  ", counts by city"),
                yaxis = list(title = 'number'), 
                xaxis = list(tickangle = 45),
                dragmode = "click", 
                margin = list(b = 150))
    }
        
    
}


plot5 <-function(df2_sub,   year_select, city_select){
    
    # histograms of cases by year
    plot_ly(df2_sub , x = week, y= number, group = event, type ="bar", source = "s5") %>%
        layout(
            title = paste("year = ", year_select,", city = ", city_select,  ", weekly counts"),
            yaxis = list(title = 'cases'))
    
}
    



