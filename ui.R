# ui.R ####

library(shinydashboard)

shinyUI(dashboardPage(
    # Header ####
    dashboardHeader(title = "Shiny Demo"),

    # Sidebar ####
    dashboardSidebar(
        sidebarUserPanel("Peter Tsyurmasto","NYC"),
        sidebarMenu(
            menuItem("Home", tabName = "Home", icon = icon("home")),
            menuItem("Data", tabName = "Data",icon=icon('database'),
                menuSubItem("About Data",tabName = "RF_Table"),
                menuSubItem("Investment Strategies", tabName = "Investment_Strategies"),
                menuSubItem("Hedge Funds", tabName = "Hedge_Funds")
            ),
            menuItem("Portfolio Decomposition", tabName = "Portfolio_Decomposition", icon = icon("chart-bar")),
            menuItem("The Author", tabName = "About")
        )
    ),

    # Body ####
    dashboardBody(
        tabItems(
        
            # Home tab ####        
            tabItem(tabName = "Home",
                    
                    fluidRow(column(1, div(img(src="image.png", height=400)))),
                    
                    fluidRow(column(6,h3("Investment Strategies and Hedge Fund Decomposition"))),
                    
                    fluidRow(column(6,p("The objective of this application is two-fold: 1) give a basic introduction into
                                        traditional and alternative investments; 2) help the user get a better 
                                        understanding of hedge funds."),
                                        p(" Data tab breaks down investment 
                                        strategies into asset classes (equities, fixed income, FX, commodities and 
                                        volatility) and investment styles (traditional, carry, value, momentum,
                                        liquidity and emerging markets). Traditional strategies are defined as 
                                        long-only investments (such as S&P500, US 10-year sovereign bond, etc.) and 
                                        alternative are defined as long/short investment strategies. Application 
                                        calculates historical performance of a selected strategy 
                                        for a data range specified by user."),
                                        p(" Similar to ingredients in a recipe, hedge funds can also be broken down into its building blocks - 
                    underlying investment strategies. Portfolio Decomposition tab determines traditional and alternative 
                      investment strategies the portfolio is composed of and percentage allocations to them. 
                      Variance decomposition chart illustrates percentages of portfolio risk explained by each strategy and the remaining (idiosyncratic) 
                      component that can not be attributed to considered factors. User can specify hedge fund index and 
                      date range for analysis.")))
                    ),
            
            # Data tab ####        
            tabItem(tabName = "RF_Table",
                    fluidRow(column(12,tableOutput("table1"))) 
            ),
            tabItem(tabName = "Investment_Strategies",
                fluidRow(
                    box(column(dateRangeInput(inputId = "date_range2", label = "Date Range",
                                              start = as.Date('2001/12/31'), end = as.Date('2019/09/01')),width = 2),
                        column(selectizeInput(inputId = "name2.1",label = "Asset Class",
                        choices = c('Equities','Fixed Income','FX','Commodities','Volatility')),width=2),
                        column(selectizeInput(inputId = "name2.2",label = "Investment Style",
                                              choices = c('Traditional','Carry','Value','Momentum','Liquidity','Emerging Markets')),width=2),
                        column(selectizeInput(inputId = "name2",label = 'Select Investment Strategy',
                        choices = setNames(as.vector(data.X.description$id),as.vector(data.X.description$name))),width=2),width=12)
                ),
                
                fluidPage(
                    plotOutput("graph2"),
                    tableOutput("table2")
                )
            ),
            
            # Hedge Funds tab ####
            tabItem(tabName = "Hedge_Funds",
                fluidRow(
                    box(column(dateRangeInput(inputId = "date_range3", label = "Date Range",
                                                  start = as.Date('2001/12/31'), end = as.Date('2019/09/01')),width = 2),
                        column(selectizeInput(inputId = "name3.1",label = "Type",
                                              choices = c('Liquid Alternatives','Hedge Funds')),width=2),
                        column(selectizeInput(inputId = "name3.2",label = "Style",
                                              choices = c('Composite Multi-Strategy','Equity Hedge','Event Driven','Global Macro','Relative Value')),width=2),
                        column(selectizeInput(inputId = "name3",label = 'Select Hedge Fund Index',
                                              choices = setNames(as.vector(data.Y.description$id),as.vector(data.Y.description$name))),width=2),width = 12)
                
                ),
                
                fluidPage(
                    plotOutput("graph3"),
                    tableOutput("table3")
                )
            ),
                    
            # Portfolio Decomposition tab ####        
            tabItem(tabName = "Portfolio_Decomposition",
                    fluidRow(
                        box(column(dateRangeInput("date_range4", label = ("Date range"), 
                            start = as.Date('2006/12/31'), end = as.Date('2019/01/09')),width = 2),
                        column(selectizeInput(inputId = "name4",
                                              label = "Select Hedge Fund Index",
                                              choices = setNames(as.vector(data.Y.description$id),as.vector(data.Y.description$name))),width=2),width=12)
                    ),
                    fluidPage(
                            
                        box(plotOutput("graph4"),
                        tableOutput("table4"),width=8),
                        box(tabsetPanel(
                            tabPanel('Percentage Allocations',plotOutput("barchart4")),
                            tabPanel('Variance Decomposition',plotOutput("barchart5"))),
                            width = 4)
                    ) 
                    
            ),
            # About tab ####        
            tabItem(tabName = "About", 
                    fluidRow(column(3,h3("Bio Sketch"))),
                    fluidRow(column(1, div(img(src="photo.png", height=150)))),
                    fluidRow(column(1,br())),
                    fluidRow(column(6, 
                           div(
                               p("Dr. Peter Tsyurmasto has over 5 years of financial industry experience."),
                               p("He started his career at Deutsche Bank as a quantitative researcher, where 
                               he applied statistical and machine learning models to solve complex
                               financial problems.
                               Peter worked for a systematic hedge fund with $1b in assets, where he led research 
                                 into systematic long/short strategies in futures and currencies. He successfully 
                                 developed and deployed a portfolio of high-capacity strategies with a Sharpe ratio of 1.5+ and
                                 annualized dollar volatility of $50m."),
                               p("Peter earned his Master degree in Machine Learning 
                               from Moscow Institute Of Physics And Technology 
                               and a Ph.D. in Operations Research from University of Florida. 
                                 Currently, Peter is a Data Science Fellow at NYC Data Science Academy.")
                           )))    
            )
    )) 
))
