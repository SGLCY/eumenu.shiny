

library(shiny)
library(dplyr)
library(ggplot2)

options(shiny.maxRequestSize = 10 * 1024^2)

ggplot2::theme_set(ggplot2::theme_light(16))

Sys.setlocale(locale = "greek")

side_width <- 3
main_width <- 12- side_width
table_digits <- 2



## UI ####

ui <- fluidPage(
  
  navbarPage("Exploring EU MENU consumption Data",
             tabPanel("Data"),
             
             
             tabPanel("Explore", 
                      
                      
                      titlePanel("Upload your EU MENU data"),
                      
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          fileInput("file", "An excel file please", buttonLabel = "Upload...", accept = ".xlsx")
                          
                          ,h3("Select columns to show")
                          
                          # Other inputs
                          ,uiOutput("show_vars")
                          
                          , width = side_width
                        ),
                        
                        mainPanel("Exploring Consumption habits",
                          
                          tabsetPanel(id = "data_tabset",
                                      
                                      tabPanel(title = "FULL DATASET", 
                                               h1("Here is the FULL table of the observations"), 
                                               h3("This table is not affected by your selections"),
                                               DT::dataTableOutput("consumption")
                                      )
                                      
                          )
                          
                          , width = main_width
                          
                        )
                      )
                      
                      )
  ),
  
  titlePanel("Testing EU MENU shiny app"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file", "Upload a file", buttonLabel = "Upload..", accept = ".xlsx")
      
      ,h3("Filters")
     
     # FILTERS
      ,uiOutput("fdx1_slct")
     ,uiOutput("fdx2_slct")
     ,uiOutput("ENFOODNAME")
     ,uiOutput("ORFOODNAME")
     ,uiOutput("ENRECIPEDESC")
     
     # Other inputs
     ,uiOutput("show_vars")
     
     , width = side_width
    ),
    
    mainPanel(
      "Exploring Consumption habits",
      
      tabsetPanel(id = "inTabset",
                  
                  tabPanel(title = "Mean Consumption"
                           
                           ,h2("Descriptives per Age Group")
                           
                           ,tableOutput("cons_mean")
                           
                           ,h2("Individual mean consumption")
                           
                           ,DT::dataTableOutput("cons_individual")
                           
                           ),
                  
                  tabPanel(title = "GRAPHS",
                           fluidRow(
                             
                             column(5, plotOutput("plot_mean"))
                             ,column(1, "")
                             ,column(5, plotOutput("plot_dist"))
                             
                           )
                           
                        
                           
                           ),
                  
                  tabPanel(title = "FULL DATASET", 
                           h1("Here is the FULL table of the observations"), 
                           h3("This table is not affected by your selections"),
                           DT::dataTableOutput("consumption")
                           )
        
      )
      
      , width = main_width
      
      )
    )
  
  
  )

# SERVER ####
server <- function(input, output) {
  

  # function to filter the consumption by the food group
  filter_fdgrp <- function(.data, food_group){
    
    .data %>% 
      filter(fdx1_name %in% food_group |
               fdx2_name  %in% food_group | 
               ENFOODNAME %in% food_group |
               ENRECIPEDESC %in% food_group |
               ORFOODNAME %in% food_group) %>%
      group_by(AgeGroup, ORSUBCODE) %>% 
      summarise(total_consumption = sum(AMOUNTFRAW)) %>% 
      left_join(number_of_days(), by = "ORSUBCODE") %>% 
      left_join(subject_weight(), by = "ORSUBCODE") %>%
      left_join(n_age_group(), by = "AgeGroup") %>% 
      mutate(gr_day =  total_consumption/ n_days,
             gr_kbw_day  = total_consumption/ n_days/ Weight) %>% 
      ungroup() 
    
  }
  
  aggr_age_group <- function(.data){
    
    .data %>% 
      group_by(AgeGroup, "participants"= n) %>% 
      summarise(consumers = n(),
                total_cons  = sum(gr_day),
                med_con = median(gr_day),
                avg_con = mean(gr_day),
                med_con_bw = median(gr_kbw_day),
                avg_con_bw = mean(gr_kbw_day),
                SD_con = sd(gr_day)
      ) %>% 
      mutate(prop = consumers/participants) %>% 
      select(AgeGroup, participants, consumers, prop, everything())
    
  }
  
  
  # Reactive Eleemnts ####
  dataset <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$filename)
    
    file.rename(input$file$datapath,
                paste(input$file$datapath, ext, sep="."))
    
    # validate(need(ext == "xlsx", "Please upload an xlsx file"))
    
    # vroom::vroom(input$file$datapath, delim = ",")
    readxl::read_xlsx(paste(input$file$datapath, ext, sep="."), 1) %>% 
      select(SURVEY:AgeGroup,Age:BMI)
    
  })
  
  # helper datasets
  participants   <- reactive(dataset() %>% distinct(ORSUBCODE, AgeGroup))
  number_of_days <- reactive(dataset() %>% group_by(ORSUBCODE) %>% summarise(n_days = max(DAY))) 
  subject_weight <- reactive(dataset() %>% distinct(ORSUBCODE, Weight))
  n_age_group    <- reactive(dataset() %>% distinct(ORSUBCODE, .keep_all = TRUE) %>%  count(AgeGroup)) 
  
  filters <- reactive(c(input$fdx1, input$fdx2, input$ENFOODNAME, input$ENRECIPEDESC, input$ORFOODNAME))

  cons_individual <- reactive({
    
    filter_fdgrp(dataset(), filters()) %>% 
      mutate_if(is.numeric, ~ round(., 3))
  })
  
  cons_mean <- reactive({
    
    if(!is.null(filters())){
      dataset() %>% 
        filter_fdgrp(filters()) %>% 
        aggr_age_group()
    } else return("No Data")
    
  })
  
    # Create UI's. ####
    # Perhaps its better to create a function to create UI on 
    # variables I pre-determie, for cleaner code
    output$fdx1_slct <- renderUI({
      fdx1_names <- unique(dataset()$fdx1_name)
      selectInput("fdx1","FoodEx1 name(s)", choices = fdx1_names, multiple = TRUE)
    })
    
    output$fdx2_slct <- renderUI({
      fdx2_names <- unique(dataset()$fdx2_name)
      selectInput("fdx2","FoodEx2 name(s)", choices = fdx2_names, multiple = TRUE)
    })
    
    output$ENFOODNAME <- renderUI({
      ENFOOD_names <- unique(dataset()$ENFOODNAME)
      selectInput("ENFOODNAME","English Food name(s)", choices = ENFOOD_names, multiple = TRUE)
    })
    
    output$ORFOODNAME <- renderUI({
      ORFOODNAME_names <- unique(dataset()$ORFOODNAME)
      selectInput("ORFOODNAME","Greek Food name(s)", choices = ORFOODNAME_names, multiple = TRUE)
      
    })
    
    output$ENRECIPEDESC <- renderUI({
      ENRECEP_names <- unique(dataset()$ENRECIPEDESC)
      selectInput("ENRECIPEDESC","Recipe name(s)", choices = ENRECEP_names, multiple = TRUE)
      
    })
    
    # PLOTS ####
    output$plot_mean <- renderPlot({

      if(is.data.frame(cons_mean())) {
          cons_mean() %>%
            ggplot(aes(x = AgeGroup, y = avg_con))+
            geom_col(fill = "#2ca25f")+
            coord_flip()+
            geom_text(aes(label = round(avg_con,0)),
                      hjust = -0.25, size = 6)+
            labs(y = "g/day",
                 x = "")+
            labs(title = "Mean consumption (grams) per day"
                 , subtitle = "Consumers only")
      } else NULL
    }, width = 650, height = 500)
    
    output$plot_dist <- renderPlot({
      
      if(is.data.frame(cons_mean())) {
        cons_individual() %>%
          ggplot(aes(total_consumption, fill = AgeGroup))+
          geom_density()+
          labs(y = "Density",
               x = "g/day")+
          labs(title = "Distribution by Age Group(grams) per day"
               , subtitle = "Consumers only")
      } else NULL
    }, width = 650, height = 500)
    

    
    
    # SHOW variables ####
    output$show_vars <- renderUI({
      
    vars_to_show <- c("fdx2_name", "fdx1_name", "ORFOODNAME", "ENFOODNAME", "COMMENTSFOOD", "ENRECIPEDESC") 
    
    checkboxGroupInput("show_vars", "Columns in the database to show:",
                          names(dataset()), selected = vars_to_show)
    })
    
    
    # TABLES ####
    output$consumption <- DT::renderDataTable({
      data = dataset()
      
      if (!is.null(data)){
        data[, input$show_vars, drop=FALSE]
      } else NULL
      
      }, filter = "top")
    
    
    output$cons_mean <- renderTable({cons_mean()}, digits = table_digits)
    
    output$cons_individual <- DT::renderDataTable({ cons_individual()})
    
    
}

# RUN ####

shinyApp(ui=ui,server=server)