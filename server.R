library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(shinydashboard)
library(ggdendro)
theme_set(theme_bw())

switch_df = function(col,df1,df2){
    return(ifelse(col %in% colnames(df1),return(df1),return(df2)))
}

shinyServer(function(input, output, session) {
    
    observe({
        updateDateRangeInput(session, 'date_range2', 'Date Range', start=get_start_dt(data.X,input$name2), end=get_end_dt(data.X,input$name2))
    })
    
    strategy.style <- reactive(unique(as.vector(data.X.description %>%
                             filter(.,asset.class == input$name2.1) %>% .$style)))
    
    observeEvent(
        input$name2.1,
                updateSelectInput(session,'name2.2','Investment Style',choices =  strategy.style())
    )
    
    strategy.id.name <- reactive( data.X.description %>% 
        filter(.,asset.class == input$name2.1, style == input$name2.2))
    observeEvent(
        input$name2.2,
        updateSelectInput(session,'name2','Select Investment Strategy',
                          choices = setNames(as.vector(strategy.id.name()$id),as.vector(strategy.id.name()$name)))    
    )
    
    
    output$dateRange <- renderUI({
        dateRangeInput('date_range3', 'Date Range', 
                       start=get_start_dt(data.X,input$name2), end=get_end_dt(data.X,input$name2))
    })
    
    strategy.graph2 <- reactive({
        
        data.X %>%
            select(.,dates,input$name2) %>%
            filter(.,!is.na(!!sym(input$name2)),dates>=input$date_range2[1],dates<=input$date_range2[2])
    })
    
    output$graph2 <- renderPlot(
        strategy.graph2() %>%
          ggplot(aes_string(x="dates",y=input$name2)) +
            geom_line() + 
            scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
            ylab(names(input$name2)) + 
            ggtitle("Historical Performance")
    )
    
    
    output$heatmap2 <- renderPlot(
        
        heatmap(scale(mtcars),scale = "row")
        
    )
    
    output$table2 <- renderTable(
    
        {calc_perf_table(data.X,input$name2,input$date_range2[1],input$date_range2[2])},rownames=TRUE
        
    )
    
    output$graph3 <- renderPlot(
        switch_df(input$name3,data.Y.daily,data.Y.monthly) %>%
            select(.,dates,input$name3) %>%
            filter(.,!is.na(!!sym(input$name3)),dates>=input$date_range3[1],dates<=input$date_range3[2]) %>%
            ggplot(aes_string(x="dates",y=input$name3)) +
            geom_line() + 
            scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
            ylab(names(input$name3)) +
            ggtitle("Historical Performance")
    )
    
    output$heatmap3 <- renderPlot(
        
        heatmap(scale(mtcars),scale = "row")
        
    )
    
    output$table4 <- renderTable(
        
        {calc_perf_table(data.Y.daily,input$name4,input$date_range4[1],input$date_range4[2])},rownames=TRUE
        
    )
    
    output$graph4 <- renderPlot(
        
        switch_df(input$name4,data.Y.daily,data.Y.monthly) %>%
            select(.,dates,input$name4) %>%
            filter(.,!is.na(!!sym(input$name4)),dates>=input$date_range4[1],dates<=input$date_range4[2]) %>%
            ggplot(aes_string(x="dates",y=input$name4)) +
            geom_line() + 
            geom_line(data=
            in_sample_wrapper(data.X,switch_df(input$name4,data.Y.daily,data.Y.monthly),
                              input$name4,input$date_range4[1],input$date_range4[2])[['prices']],
                      aes(x=dates,y=RF_index),color="blue") +
            scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
            ylab(names(input$name4)) + 
            ggtitle("Historical Performance")
    )
    
    output$piechart4 <- renderPlot(
        
        ggplot(data = in_sample_wrapper(data.X,switch_df(input$name4,data.Y.daily,data.Y.monthly),
                    input$name4,input$date_range4[1],input$date_range4[2])[['var.decomposition']], 
               aes(x = "", y = prop, fill = factors)) +
            geom_bar(width = 1, stat = "identity", color = "white") +
            coord_polar("y", start = 0)+
            geom_text(aes(y = lab.ypos, label = percent(prop,0)), color = "white")+
            ggtitle("Variance Decomposition")+
            theme_void()
    )
    
    output$barchart4 <- renderPlot(
        ggplot() + geom_col(data = 
    in_sample_wrapper(data.X,switch_df(input$name4,data.Y.daily,data.Y.monthly),
                      input$name4,input$date_range4[1],input$date_range4[2])[['betas']], 
                   aes(x = coef.name, y = coef.value),fill="grey") + coord_flip()
    )
    

})
