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
    
    output$table2 <- renderTable(
        
        {calc_perf_table(data.X,
                         input$name2,input$date_range2[1],input$date_range2[2])},rownames=TRUE)
    #output$heatmap2 <- renderPlot(
    #    
    #    heatmap(scale(mtcars),scale = "row")
    #    
    #)
    
    output$table3 <- renderTable(
    
        {calc_perf_table(switch_df(input$name3,data.Y.daily,data.Y.monthly),
                         input$name3,input$date_range3[1],input$date_range3[2])},rownames=TRUE)
    
    strategy.style3 <- reactive({unique(as.vector(data.Y.description %>%
                                                    filter(.,type == input$name3.1) %>% .$style))})
    
    observeEvent(
        input$name3.1,
        updateSelectInput(session,'name3.2','Style',choices =  strategy.style3())
    )
    
    strategy.id.name3 <- reactive( data.Y.description %>% 
                                      filter(.,type == input$name3.1, style == input$name3.2))
    observeEvent(
        input$name3.2,
        updateSelectInput(session,'name3','Select Hedge Fund Index',
                          choices = setNames(as.vector(strategy.id.name3()$id),as.vector(strategy.id.name3()$name)))    
    )
    
    strategy.graph3 <- reactive({
        
        switch_df(input$name3,data.Y.daily,data.Y.monthly) %>%
            select(.,dates,input$name3) %>%
            filter(.,!is.na(!!sym(input$name3)),dates>=input$date_range3[1],dates<=input$date_range3[2])
    })
    
    output$graph3 <- renderPlot(
            strategy.graph3() %>%
            ggplot(aes_string(x="dates",y=input$name3)) +
            geom_line() + 
            scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
            ylab(names(input$name3)) +
            ggtitle("Historical Performance")
    )
    
    output$table4 <- renderTable(
        
        {calc_perf_table(switch_df(input$name4,data.Y.daily,data.Y.monthly), 
                         input$name4,input$date_range4[1],input$date_range4[2])},rownames=TRUE
        
    )
    
    output$table1 <- renderTable(
        {get_rf_table(RF.table.description)},
        striped = TRUE,
        bordered = TRUE,
        spacing = 'l',
        align = 'c',
        include.rownames=TRUE
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
    
    betas <-reactive({
        id2name(
            in_sample_wrapper(data.X,switch_df(input$name4,data.Y.daily,data.Y.monthly),
            input$name4,input$date_range4[1],input$date_range4[2])[['betas']],
            data.X.description) %>%
            mutate(.,name.value =
                paste(as.character(coef.name),"=",as.character(percent(coef.value,0)),sep=" "))
    })
    
    var.diagram <-reactive({
        id2name(
            in_sample_wrapper(data.X,switch_df(input$name4,data.Y.daily,data.Y.monthly),
            input$name4,input$date_range4[1],input$date_range4[2])[['var.decomposition']],
            data.X.description) %>%
            mutate(.,name.value =
                       paste(as.character(coef.name),"=",as.character(percent(coef.value,0)),sep=" "))
    })
    
    output$barchart4 <- renderPlot(
        betas() %>% arrange(.,desc(abs(coef.value))) %>% 
            ggplot(.,aes(x = coef.name, y = coef.value)) + 
            geom_col(fill="grey",alpha=0.5) + coord_flip() + 
        ylab('Portfolio Exposures') + xlab('Investment Strategy') +
            expand_limits(y = -0.1) +
            geom_text(aes(x=coef.name,y=coef.value,
            label= name.value),position = position_stack(vjust = 0.5)) +
            theme(axis.text.y = element_blank())
    )
    
    output$barchart5 <- renderPlot(
        var.diagram() %>% arrange(.,desc(abs(coef.value))) %>% 
            ggplot(.,aes(x = coef.name, y = coef.value)) + 
            geom_col(fill="grey",alpha=0.5) + coord_flip() + 
            ylab('Variance Contribution (%)') + xlab('Investment Strategy') + 
            expand_limits(y = c(-0.5,1)) +
            geom_text(aes(x=coef.name,y=coef.value,
                          label= name.value),position = position_stack(vjust = 0.5)) +
            theme(axis.text.y = element_blank())
            
    )

})
