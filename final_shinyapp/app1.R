library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(RColorBrewer)
library(wesanderson)
library(ggplot2)
library(ggmap)
library(readr)
library(dplyr)
library(leaflet)
library(rjson)
library(rcartocolor)
library(maps)

source("www/functions/EDA.R")
source("www/functions/charts1.R")

ui <- navbarPage('A Team',theme=shinytheme('flatly'),
                 tabPanel('Geospatial Analysis',
                          dashboardPage(
                            dashboardHeader(disable = T),
                            dashboardSidebar(
                              pickerInput(
                                inputId = "State1",
                                label = "State",
                                choices = c(ALL_State = '',unique(df$State)),
                                options = list(`live-search` = TRUE)),
                              uiOutput("City1"),
                              textInput(
                                inputId = "Zip1", 
                                label = "Zip Code",
                                value = ""),
                              awesomeCheckboxGroup(
                                inputId = "Category1",
                                label = "Category", 
                                choices = c('Flu Shot', 'Flu Shot (Egg free)', 
                                            'Flu Shot (65+, high-dose or adjuvanted)',
                                            'Flu Nasal Spray'),
                                selected = ''),
                              
                              
                              
                              actionButton(
                                inputId = "reset",
                                label = "Reset Data",
                                icon = icon("refresh"),
                                width = "87%"
                              )),
                            dashboardBody(
                              fluidRow(
                                box(width=7,title = "Vaccination Sites in U.S.",height = 550,
                                    uiOutput("map1")),
                                box(width=5,title = "Vaccination Sites Number",height = 550,
                                    numericInput("num1",
                                                 "Top n cities & States",
                                                 value = 5,width = "40%"),
                                    plotOutput("bar1"))
                              ),
                              fluidRow(
                                box(width=7,title = "Pharmacy Information",
                                    DT::dataTableOutput("table1")),
                                box(width=5,title = "Vaccination Types Proportion",
                                    plotOutput("pie1"))
                              )))),
                 
                 tabPanel('Recommendation System',
                          dashboardPage(
                            dashboardHeader(disable=T),
                            dashboardSidebar(
                              useShinyjs(),
                              width = 250,
                              pickerInput(
                                "State2",
                                "State Select",
                                choices = df$State %>% unique(),
                                selected = NULL,
                                options = list('live-search'=TRUE),
                                multiple=T),
                              
                              uiOutput('City2'),
                              
                              searchInput(
                                "Zipcode2",
                                label = "Zipcode", 
                                placeholder = "Zipcode",
                                btnReset = icon("remove"),
                                width = "100%"),
                              
                              awesomeCheckboxGroup("Flu_Vaccine_Category2", "Flu Category",
                                                   choices = df$searchable_name%>% unique()),
                              pickerInput("weekday2", 
                                          "Weekday",
                                          choices =c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                                          selected = NULL,
                                          multiple = T),
                              
                              #reset button
                              actionButton('reset2','Reset Data',icon = icon('refresh'),
                                           width = '87%')
                              
                            ),
                            dashboardBody(
                              fluidRow(valueBoxOutput("Pharmacy_Number2",width=6)),
                              #Output:pharmacy information of having available flu vaccine
                              fluidRow(
                                box(title='Pharmacy Recommendation Information', solidHeader= T,
                                    width=12,collapsible = T,
                                    div( DT::dataTableOutput('table2'))))
                              
                            ))),
                 
                 tabPanel('Stock Management System',
                          dashboardPage(
                            dashboardHeader(disable = T),
                            dashboardSidebar(
                              # state
                              pickerInput(
                                inputId = "State3",
                                label = "State Select",
                                choices = c(choose='',df$State %>% unique()),
                                options = list(`live-search` = TRUE)
                              ),
                              
                              # city
                              uiOutput("City3"),
                              
                              # zip code
                              textInput(
                                inputId = "Zip3", 
                                label = "Zip Code",
                                value = ""
                              ),
                              
                              # category
                              pickerInput(
                                inputId = "Category3",
                                label = "Category Select",
                                choices = c(choose='',df$searchable_name %>% unique()),
                                options = list(
                                  `live-search` = TRUE)
                              ),
                              
                              #slider
                              sliderInput(inputId = "days",
                                          label = "Vaccine supply days",
                                          value = 0,
                                          min = 0,
                                          max = 3),
                              
                              
                              # action button
                              actionButton(
                                inputId = "reset3",
                                label = "Reset Data",icon = icon('refresh'),width = '87%'
                              )
                            ),
                            dashboardBody(
                              fluidPage( 
                                
                                tabsetPanel(type = "tabs",
                                            tabPanel("In-Stock Pharmacy Information", DT::dataTableOutput('table3')),
                                            tabPanel("Availability of Future Stocks", plotOutput("plot3"))
                                            
                                )
                              )
                            ))),
                 
)

server <- function(input, output) {
  
  #page1
  topn <- reactive({
    input$num1
  })
  
  values <- reactiveValues(
    Cities = NULL,
    State = NULL,
    tbl1 = NULL
  )
  
  observeEvent(input$State1,{
    values$State <- input$State1
    values$Cities <- df %>% filter(State==input$State1) %>% select(City) %>% unique()
    
    output$City1 <- renderUI({
      if (!NA %in% match(values$State,df$State %>% unique())){
        pickerInput("City1",
                    "City Select",
                    choices = values$Cities,
                    options = list(
                      `live-search` = TRUE)
        )
      } else{}
    })
    
    output$map1 <- renderUI({
      plot.new()
      div(
        if((values$State=='')){
          usMap <- map_data("state")
          usMap$State <- state.abb[match(toupper(usMap$region), toupper(state.name))]
          usMap$State[usMap$region=='district of columbia'] <- "DC"
          plotlyOutput("map11")
          output$map11 <- renderPlotly({
            US_distribution_chart(df,usMap)
          })
        } else{
          usMap <- rjson::fromJSON(file=url)
          plotOutput("map12")
          output$map12 <- renderPlotly({
            state_chart(input$State1,df,usMap)
          })
        }
      )
    })
    
    output$bar1 <- renderPlot({
      plot.new()
      chart_bar(df,values$State,topn())
    })
    
    if(values$State!=""){
      output$pie1 <- renderPlot({
        plot.new()
        pie_chart(df,values$State,input$City1) 
      }
      )}
  })
  
  tableselected <- reactive({list(input$State1,input$City1,input$Zip1,input$Category1)}) 
  
  observeEvent(tableselected(), { 
    
    # Save the selected column/s to values$tbl
    
    values$tbl1 <- subset(df, select = default_col1)
    
    if (!is.null(input$State1) && input$State1 != "") {
      values$tbl1 <- values$tbl1 %>% filter(State == input$State1)
    } 
    if (!is.null(input$City1) && input$City1 != "") {
      values$tbl1 <- values$tbl1 %>% filter(City == input$City1)
    } 
    if (!is.null(input$Zip1) && input$Zip1 != "") {
      values$tbl1 <- values$tbl1 %>% filter(zip == input$Zip1)
    } 
    if (!is.null(input$Category1)) {
      values$tbl1 <- values$tbl1 %>% filter(searchable_name %in% input$Category1)
    } 
    
    names(values$tbl1) <- c('Name','Street','City','State',
                            'zip','Flu Name')
    
    # Render output data table ---- 
    output$table1 <- DT::renderDataTable(DT::datatable({values$tbl1}))
  })
  
  # Widget RESET ----
  # hint: set widget to NULL, then widget will disappear ----
  observeEvent(input$reset, {
    values$Cities <- NULL
    values$State <- ''
    values$tbl1 <- subset(df, select = c('Name','Street','City','State',
                                         'zip','searchable_name')) %>% rename('Flu Name'=searchable_name)
    output$pie1 <- NULL
  })
  
  
  
  #page2
  values2 <- reactiveValues(tbl2 = NULL,
                            tbl2.df = NULL)
  
  #observe any update from State2
  observeEvent(input$State2, {
    Cities2 <- df %>% filter(State %in% input$State2) %>% select(City) %>% unique()
    # Function IF will prevent to show error/warning messages on the blank screen ----
    if (!NA %in% match(input$State2, df$State %>% unique())) {
      
      # UI output for City
      output$City2 <- renderUI({
        pickerInput(
          inputId = "City2",
          label = "City Select",
          choices = Cities2,
          selected = NULL,
          options=list('live-search'=T),
          multiple=T)})
    }}
  )
  
  
  
  
  # Observe the updates of the selected columns ----
  tolisten <- reactive({list(input$State2,input$City2,input$Flu_Vaccine_Category2,input$weekday2)})
  observeEvent(tolisten(), {
    
    # Save the selected column/s to values$tbl.df ----
    values2$tbl2 <- subset(df,in_stock=='TRUE') %>% filter(State %in% input$State2) %>% filter(City %in% input$City2) %>% filter(searchable_name %in% input$Flu_Vaccine_Category2)
    col2 <- append(default_col2,input$weekday2,after=5)
    values2$tbl2.df <-as.data.frame(subset(values2$tbl2, select=col2))
    #values2$kpinum2 <- length(unique(values2$tbl2))
    # Render output data table ----
    output$table2 <- DT::renderDataTable(DT::datatable({
      values2$tbl2.df  }))
    
  })
  
  #kpicard
  observeEvent(tolisten(),{
    kpi_num <- nrow(subset(values2$tbl2,select='Name') %>% unique())
    output$Pharmacy_Number2 <- renderValueBox({
      valueBox(paste0("Pharmacy Number: ",kpi_num),
               "Flu Vaccine",
               icon = icon("fire"),
               color = "yellow")
    })
  })
  
  
  #set reset widget
  observeEvent(input$reset2, {reset("Flu_Vaccine_Category2")
    reset('State2')
    reset('City2')
    reset('weekday2')})
  
  
  
  #page3
  values3 <- reactiveValues(
    Cities = NULL,
    tbl3 = NULL
  )
  
  #select state and city-------
  observeEvent(input$State3,{
    Cities <- df %>% filter(State==input$State3) %>% select(City) %>% unique()
    if(!NA %in% match(input$State3,df$State %>% unique())){
      output$City3 <- renderUI({
        pickerInput("City3",
                    "City Select",
                    choices = c(choose='',df$City %>% unique()),
                    options = list(
                      `live-search` = TRUE))})}})
  
  # table3 display -------
  tolist3 <- reactive({list(input$State3,input$City3,input$Zip3,input$Category3)}) 
  
  observeEvent(tolist3(), { 
    # change the name of the columns
    df1 <- df %>%
      mutate(supply_level = case_when(
        supply_level == -1 ~ "no report",
        supply_level == 0 ~ "no supply",
        supply_level == 1 ~ "<24 hour supply",
        supply_level == 3 ~ "24-48 Hour Supply",
        supply_level == 4 ~ ">48 Hour Supply",
        TRUE ~ as.character(supply_level)
      ))%>%
      mutate(insurance_accepted = ifelse(insurance_accepted, "Yes", "No"))%>%
      mutate(walkins_accepted = ifelse(walkins_accepted, "Yes", "No"))
    
    
    
    # Save the selected column/s to values$tbl.df ---- 
    values3$tbl3 <- subset(df1,in_stock=='TRUE')
    
    if (!is.null(input$State3) && input$State3 != "") {
      values3$tbl3 <- values3$tbl3 %>% filter(State == input$State3)
    } 
    if (!is.null(input$City3) && input$City3 != "") {
      values3$tbl3 <- values3$tbl3 %>% filter(City == input$City3)
    } 
    if (!is.null(input$Zip3) && input$Zip3 != "") {
      values3$tbl3 <- values3$tbl3 %>% filter(zip == input$Zip3)
    } 
    if (!is.null(input$Category3) && input$Category3 != "") {
      values3$tbl3 <- values3$tbl3 %>% filter(searchable_name == input$Category3)
    } 
    
    #values3$tbl3 <- subset(values3$tbl3,select=default_col3)
    
    values3$tbl3 <- values3$tbl3 %>% select(default_col3)
    names(values3$tbl3) <- new_col_names <- c('Name', 'State', 'City', 'Street', 'Phone','Zip',
                                              'Insurance Acceptability', 'Walkin Acceptability', 
                                              'Flu Name', 'Supply Level')
    # Render output data table ---- 
    output$table3 <- DT::renderDataTable(DT::datatable({values3$tbl3}))
    
  })
  
  # plot3 display -------
  tolist4 <- reactive({list(input$State3,input$City3,input$Category3,input$Zip3,input$days)})
  
  df2 <- df %>%
    mutate(supply_level = case_when(
      supply_level == -1 ~ "0",
      supply_level == 0 ~ "0",
      supply_level == 1 ~ "1",
      supply_level == 3 ~ "2",
      supply_level == 4 ~ "3",
      TRUE ~ as.character(supply_level)  
    ))
  
  observeEvent(tolist4(),{
    
    df2 <- df %>%
      mutate(supply_level = case_when(
        supply_level == -1 ~ "0",
        supply_level == 0 ~ "0",
        supply_level == 1 ~ "1",
        supply_level == 3 ~ "2",
        supply_level == 4 ~ "3",
        TRUE ~ as.character(supply_level)  
      ))
    
    values3$plot3 <- subset(df2,in_stock=='TRUE')
    
    if (!is.null(input$State3) && input$State3 != "") {
      values3$plot3 <- values3$plot3 %>% filter(State == input$State3)
    } 
    if (!is.null(input$City3) && input$City3 != "") {
      values3$plot3 <- values3$plot3 %>% filter(City == input$City3)
    } 
    if (!is.null(input$Zip3) && input$Zip3 != "") {
      values3$plot3 <- values3$plot3 %>% filter(zip == input$Zip3)
    } 
    if (!is.null(input$Category3) && input$Category3 != "") {
      values3$plot3 <- values3$plot3 %>% filter(searchable_name == input$Category3)
    } 
    values3$plot3 <- values3$plot3 %>% select(default_col3)
    
    output$plot3 <- renderPlot({
      plot.new()
      g <- ggplot(values3$plot3 %>% filter(supply_level %in% input$days),
                  aes(x = searchable_name, fill = searchable_name)) +
        geom_bar(width = 0.5)+
        labs(x ="Flu shot category",
             y ="Number of pharmacy"
             ) +
        theme(axis.text.x = element_text(angle = 0,hjust = 0.5),
              plot.title = element_text(size = 12, hjust = 0.5))+
        theme_classic()+ 
        scale_fill_brewer(palette = 'Set2', name = 'Flu Category')
      g
    })})
  
  # Widget RESET ----
  # hint: set widget to NULL, then widget will disappear ----
  observeEvent(input$reset3, {
    values3$Cities <- NULL
    values3$tbl3 <- subset(df, select = c('Name','Street','City','State','Phone',
                                          'zip','searchable_name')) %>% rename('Flu Name'=searchable_name)
  })
  
  
  
}

shinyApp(ui = ui, server = server)