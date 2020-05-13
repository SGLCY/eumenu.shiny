

library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)
library(janitor)

options(shiny.maxRequestSize = 12 * 1024^2)

ggplot2::theme_set(ggplot2::theme_light(16))

side_width   <- 3
main_width   <- 12- side_width
table_digits <- 2

age_levels <- c(
    "Infants: 0-11 MONTHS"
  , "Toddlers: 12 -35 months" 
  , "Other Children: (36 mo to <10 y)"
  , "Adolescents: 10-17"
  , "Adults: 18-64"
  , "Elderly: 65-74"
  , "Pregnant Women"
) 

gender_levels <- c("Female", "Male")


foodex1 <- 
  #readxl::read_excel("data/FoodEx1.xlsx") %>% 
  readRDS("data/FoodEx1.rds") %>% 
  select(FOODEX_L4_CODE, FOODEX_L4_DESC, FOODEX_L3_CODE, FOODEX_L3_DESC,
         FOODEX_L2_CODE, FOODEX_L2_DESC, FOODEX_L1_CODE, FOODEX_L1_DESC)

data_description <- 
  readRDS("data/data_description.rds")

## UI ####

ui <- fluidPage(
  
  navbarPage("Exploring EU MENU consumption Data",
             
             # UPLOAD DATA PAGE ####
             tabPanel("Data", 
                      
                      
                      titlePanel("Upload your EU MENU data"),
                      
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          fileInput("file", "An excel file please", buttonLabel = "Upload...", accept = ".xlsx")
                          
                          #,uiOutput("Gender")
                          #,uiOutput("Area")
                          
                          # Other inputs
                          ,uiOutput("show_vars")
                          
                          , width = side_width
                        ),
                        
                        mainPanel("--------",
                                  
                                  tabsetPanel(id = "data_tabset",
                                              
                                              tabPanel(title = "FULL DATASET", 
                                                       h2("Here is the FULL table of the observations"), 
                                                       DT::dataTableOutput("consumption")
                                              ),
                                              tabPanel(title = "Participants", 
                                                       h2("The table of participants in the food survey"), 
                                                       DT::dataTableOutput("participants")
                                              ),
                                              
                                              tabPanel(title = "FoodEx1",
                                                       h3("The FoodEx1 food classification system"),
                                                       DT::dataTableOutput("foodex1")
                                                       ),
                                              
                                              tabPanel(title = "Survey Samples",
                                                       h3("The FoodSurvey sample sizes"),
                                                       p("The table shows the sample size [% (N)] of participants"),
                                                       tableOutput("freq_gender_age"),
                                                       p(" "),
                                                       tableOutput("freq_district_urban")
                                                       
                                              ),
                                              tabPanel(title = "Data Description",
                                                       h3("A description of the columns in the dataset"),
                                                       #p("The table shows what each column in the data represents"),
                                                       DT::dataTableOutput("data_description")
                                              )
                                  )
                                  
                                  , width = main_width
                                  
                        )
                      )
                      
             ),
             
             # EXPLORE PAGE #### 
             tabPanel("Explore",
                      
                      titlePanel("Combined Food Groups/ Recipes"),
                      
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          
                          h3("SELECTIONS")
                          ,h4('Your selections will "ADD UP"' )
                          , p('Use this section to COMBINE food items, eg.select "Cow milk" and "Goat milk"
                          to get the total milk consumption considering both of these items')
                          
                          # FILTERS
                          ,uiOutput("fdx1_slct")
                          ,uiOutput("fdx2_slct")
                          ,uiOutput("ENFOODNAME")
                          ,uiOutput("ORFOODNAME")
                          ,uiOutput("ENRECIPEDESC")
                          ,uiOutput("FOODEX_L3_DESC")
                          ,uiOutput("FOODEX_L2_DESC")
                          ,uiOutput("FOODEX_L1_DESC")
                          
                          # Other inputs
                          , width = side_width
                        ),
                        
                        mainPanel(
                          
                          h3("Combined food groups/names/recipes"),
                          
                          tabsetPanel(id = "inTabset",
                                      
                                      tabPanel(title = "Descriptives"
                                               
                                               ,h2("Descriptives per Age Group")
                                               
                                               ,tableOutput("cons_mean")
                                               
                                               ,h2("Descriptives per AgeGroup and Gender")
                                               
                                               ,tableOutput("cons_mean_gender")
                                               
                                               ,h2("Individual mean consumption")
                                               
                                               ,DT::dataTableOutput("cons_individual")
                                               
                                               
                                      ),
                                      
                                      # tabPanel(title = "GRAPHS"
                                      #          
                                      #          ,plotOutput("plot_box_age")
                                      #          
                                      #          # fluidRow(
                                      #          #   
                                      #          #   column(5, plotOutput("plot_mean"))
                                      #          #   ,column(1, "")
                                      #          #   ,column(5, plotOutput("plot_dist_age"))
                                      #          #   
                                      #          # ),
                                      #          # 
                                      #          # fluidRow(
                                      #          #   column(12, h3("ddd"))
                                      #          # ),
                                      #          # fluidRow(
                                      #          #   
                                      #          #   column(5, plotOutput("plot_dist_total"))
                                      #          # )
                                      #          # 
                                      #          
                                      #          
                                      # ),
                                      tabPanel(title = "Plots"
                                               
                                               ,plotOutput("plot_combined")
                                               )
                                      
                          )
                          
                          , width = main_width
                          
                        )
                      )
                      
                      
                      
                      
                      )
             

             
             # END OF PAGES ####
             )
  )

# SERVER ####
server <- function(input, output) {
  
   check_filter_combined <-  function(){
     
     validate(
       need(!is.null(filters()), "Make at least one food item/group selection")
     )
   }

   check_data_input <-  function(){
     
     validate(
       need(input$file, "Upload a dataset"),
       need(dataset, "Dataset in the wrong format")
     )
   }
   
   # VAriable names I need in the dataset
   var_names <- c("SURVEY", "RECORDIDENTIFIER", "ORSUBCODE", 
                  "FOODEXCODE", "DAY", "foodexOldCode", 
                  "fdx2_name", "fdx1_name", 
                  "ORFOODCODE", "ORFOODNAME", 
                  "ENFOODNAME", "prodTreat", 
                  "ENRECIPEDESC", "AMOUNTRECIPE", 
                  "AMOUNTFRAW", "AMOUNTFCOOKED", 
                  "ID", "URBAN", "area_gr", "area_en", 
                  "district_gr", "district_en", 
                  "Gender", "AgeGroup", "Age", "Weight", "BMI")
   file_input_error <- 
     c(      "Your data does not have the correct column names!\nPlease check the following:\n
           a) Your data are in a sheet called `Sheet1`,\n
           b) The column names are the ones shown in the Data Description tab")
   
   
  # FUNCTIONS ####
  # function to filter the consumption by the food group
  filter_fdgrp <- function(.data, food_group){
    
    .data %>% 
      filter(fdx1_name %in% food_group |
               fdx2_name  %in% food_group | 
               ENFOODNAME %in% food_group |
               ENRECIPEDESC %in% food_group |
               ORFOODNAME %in% food_group |
               FOODEX_L3_DESC %in% food_group |
               FOODEX_L2_DESC %in% food_group |
               FOODEX_L1_DESC %in% food_group) %>%
      group_by(AgeGroup, Gender, ORSUBCODE) %>% 
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
      group_by(AgeGroup, "participants"= n, add = TRUE) %>% 
      summarise(consumers  = n(),
                total_cons = sum(gr_day),
                med_con    = median(gr_day),
                avg_con    = mean(gr_day),
                med_con_bw = median(gr_kbw_day),
                avg_con_bw = mean(gr_kbw_day),
                SD_con     = sd(gr_day)
                ) %>% 
      mutate(prop = consumers/participants) %>% 
      select(AgeGroup, participants, consumers, prop, everything())
    
  }
  
  # Reactive Elements ####
  dataset <- reactive({
    
    req(input$file)
    
    ext <- tools::file_ext(input$file$filename)
    
    file.rename(input$file$datapath,
                paste(input$file$datapath, ext, sep="."))
    
    #validate(need(ext == "xlsx", "Please upload an xlsx file"))
    
    
    # if the readxl has some warinings it all gets stuck!!!!
    suppressWarnings(
    
      temp <- readxl::read_xlsx(paste(input$file$datapath, ext, sep=".")
                                , sheet = "Sheet1"
                                ) 
    )
    
    validate(
      need(all(names(temp) %in% var_names),file_input_error)
    )
    
    if(all(names(temp) %in% var_names)){
      
      temp %>% 
        select(all_of(var_names)) %>% 
        left_join(
          foodex1
          , by = c("foodexOldCode" = "FOODEX_L4_CODE") ) %>% 
        mutate(AgeGroup = factor(AgeGroup, levels = age_levels)
               , Gender = factor(Gender, levels = gender_levels, labels = gender_levels)
               )
    } else {
      NULL
    }

  })
  
  # helper datasets
  number_of_days <- reactive(dataset() %>% group_by(ORSUBCODE) %>% summarise(n_days = max(DAY))) 
  subject_weight <- reactive(dataset() %>% distinct(ORSUBCODE, Weight))
  n_age_group    <- reactive(dataset() %>% distinct(ORSUBCODE, .keep_all = TRUE) %>%  count(AgeGroup)) 
  
  filters <- reactive({c(input$fdx1
                        , input$fdx2
                        , input$ENFOODNAME
                        , input$ENRECIPEDESC
                        , input$ORFOODNAME
                        , input$FOODEX_L3_DESC
                        , input$FOODEX_L2_DESC
                        , input$FOODEX_L1_DESC
                        )
                      })
  
  participants   <- reactive({
    dataset() %>% 
    distinct(ORSUBCODE, .keep_all = TRUE) %>%
    select(ORSUBCODE, Gender, AgeGroup, Age, Weight, BMI, URBAN, area_en, district_en, SURVEY)
  })

  cons_individual <- reactive({
    
    filter_fdgrp(dataset(), filters()) %>% 
      mutate_if(is.numeric, ~ round(., 3))
  })
  
  cons_mean <- reactive({
    
    if(!is.null(filters())){
      dataset() %>% 
        # {if(input$Gender != "") filter(., Gender == input$Gender) else .} %>% 
        # {if(input$Area != "") filter(., URBAN == input$Area) else .} %>% 
        filter_fdgrp(filters()) %>% 
        aggr_age_group()
    } else return("No Data")
    
  })
  
  cons_mean_gender <- reactive({
    
    if(!is.null(filters())){
      dataset() %>% 
        filter_fdgrp(filters()) %>% 
        group_by(Gender) %>% 
        aggr_age_group() %>% 
        arrange(AgeGroup, Gender)
    } else return("No Data")
    
  })
  
  freq_gender_age <- reactive({
    
    dataset() %>% 
      distinct(ORSUBCODE, AgeGroup, Gender) %>%
      janitor::tabyl(Gender, AgeGroup, show_missing_levels = FALSE) %>% 
      janitor::adorn_totals(c("row", "col")) %>% 
      janitor::adorn_percentages() %>% 
      janitor::adorn_pct_formatting() %>% 
      janitor::adorn_ns() %>% 
      janitor::untabyl() 
  })
  
  freq_district_urban <- reactive({
    
    dataset() %>% 
      distinct(ORSUBCODE, district_en, URBAN) %>%
      janitor::tabyl(district_en, URBAN, show_missing_levels = FALSE) %>% 
      janitor::adorn_totals(c("row", "col")) %>% 
      janitor::adorn_percentages() %>% 
      janitor::adorn_pct_formatting() %>% 
      janitor::adorn_ns() %>% 
      janitor::untabyl() 
  })
  
    ## Create UI's. ####
    # Perhaps its better to create a function to create UI on 
    # variables I pre-determine, for cleaner code..
    output$fdx1_slct      <- renderUI({
      fdx1_names <- unique(dataset()$fdx1_name)
      selectInput("fdx1","FoodEx1 (L4) name(s)", choices = fdx1_names, multiple = TRUE)
    })
    output$fdx2_slct      <- renderUI({
      
      fdx2_names <- unique(dataset()$fdx2_name)
      selectInput("fdx2","FoodEx2 name(s)", choices = fdx2_names, multiple = TRUE)
      
    })
    output$ENFOODNAME     <- renderUI({
      
      ENFOOD_names <- unique(dataset()$ENFOODNAME)
      selectInput("ENFOODNAME","English Food name(s)", choices = ENFOOD_names, multiple = TRUE)
      
    })
    output$ORFOODNAME     <- renderUI({
      
      ORFOODNAME_names <- unique(dataset()$ORFOODNAME)
      selectInput("ORFOODNAME","Greek Food name(s)", choices = ORFOODNAME_names, multiple = TRUE)
      
    })
    output$ENRECIPEDESC   <- renderUI({
      ENRECEP_names <- unique(dataset()$ENRECIPEDESC)
      selectInput("ENRECIPEDESC","Recipe name(s)", choices = ENRECEP_names, multiple = TRUE)
      
    })
    output$FOODEX_L3_DESC <- renderUI({
      FOODEX_L3_names <- unique(dataset()$FOODEX_L3_DESC)
      selectInput("FOODEX_L3_DESC","Description @ FoodEx1 L3", choices = FOODEX_L3_names, multiple = TRUE)
      
    })
    output$FOODEX_L2_DESC <- renderUI({
      FOODEX_L2_names <- unique(dataset()$FOODEX_L2_DESC)
      selectInput("FOODEX_L2_DESC","Description @ FoodEx1 L2", choices = FOODEX_L2_names, multiple = TRUE)
      
    })
    output$FOODEX_L1_DESC <- renderUI({
      FOODEX_L1_names <- unique(dataset()$FOODEX_L1_DESC)
      selectInput("FOODEX_L1_DESC","Description @ FoodEx1 L1", choices = FOODEX_L1_names, multiple = TRUE)
      
    })
    
    output$show_vars      <- renderUI({
      
      vars_to_show <- c("fdx2_name", "fdx1_name", "ORFOODNAME", "ENFOODNAME"
                        #, "COMMENTSFOOD" similar to foodex2 name
                        , "ENRECIPEDESC") 
      
      list(
        h3("Select columns to show"),
        checkboxGroupInput("show_vars", "Columns in the database",
                           names(dataset()), selected = vars_to_show)
      
      )
    })
    
    output$Gender        <- renderUI({
      gender_choices <- levels(dataset()$Gender)
      list(
        h3("Filter your data"),
        p("Use these to filter down your data by GENDER and/or AREA, eg. MALES, LIMASSOL"),
        selectInput("Gender", "Gender", choices = c("ALL" = "", gender_choices))
      )
    })
    
    output$Area        <- renderUI({
      urban_choices <- unique(dataset()$URBAN)
      selectInput("Area", "Area", choices = c("ALL" = "", urban_choices))
    })
    
    # PLOTS ####
    plot_mean <- reactive({
          
          if(is.data.frame(cons_mean())) {
          cons_mean() %>%
            ggplot(aes(x = AgeGroup, y = avg_con))+
            geom_col(fill = "#2ca25f", width = 0.40)+
            coord_flip()+
            geom_text(aes(label = round(avg_con,0)),
                      hjust = 1.3, size = 6)+
            labs(y = "gr/day",
                 x = "")+
            labs(title = "Mean consumption (grams) per day"
                 , subtitle = "Consumers only")
          } else NULL
          
    })
    
    plot_dist_age <- reactive({
      
      if(is.data.frame(cons_mean())) {
        
        cons_individual() %>%
          ggplot(aes(total_consumption, fill = AgeGroup))+
          geom_density(alpha =0.4)+
          #scale_fill_brewer(type = "qual")+
          labs(y = "Density",
               x = "gr/day")+
          labs(title = "Distribution by Age Group(grams) per day"
               , subtitle = "Consumers only")+
          theme(legend.position = "bottom")+
          guides(fill=guide_legend(nrow=2,byrow=TRUE))
      } else NULL
      
    })
      
    plot_dist_total <- reactive({
        
        req(input$file)
        
        dataset() %>% 
          filter_fdgrp(filters()) %>% 
          ggplot(aes(total_consumption))+
          #geom_histogram(bins = 20, binwidth = 10, colour = "white", fill = "#2ca25f")+
          geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)),
                         colour = "white", fill = "#2ca25f"
                         )+
          geom_vline(aes(xintercept = mean(.data$total_consumption)),colour = "red", linetype = "dashed")+
          labs(y = "Number of consumers"
               , x = "Total Consumption in food survey (gr)"
               , title = "Distribution of total consumption")
        
      })
    
    plot_box_age<- reactive({
      
      dataset() %>%
        filter_fdgrp(filters()) %>%
        ggplot(aes(x = AgeGroup, y = total_consumption))+
        geom_boxplot()+
        labs(y = "Total Consumption (gr)"
             , x = "Age Group"
             , title = "Distribution of total consumption by Age Group")
      
    })
    
    output$plot_combined <- renderPlot({
      
      check_filter_combined()
      
      plot_mean() + plot_dist_age()
      }, width = 1100, height = 500)
      
    output$plot_box_age <- renderPlot({
      
     # req(!is.null(filters()))
     check_filter_combined()
        
     plot_box_age 
      
      })

    
    
    # TABLES ####
    output$consumption <- DT::renderDataTable({
      
      
      check_data_input()

      #dataset()[, input$show_vars, drop=FALSE]
      
      dataset() %>% select_at(input$show_vars)
      
      }, filter = "top")
    
    output$cons_mean <- renderTable({
      
      check_filter_combined()
      
      cons_mean() %>% 
        select(
          "Age Group" = AgeGroup
          ,"Participants" = participants
          , "Consumers" = consumers
          , "Proportion" = prop
          , "Total Consumption" = total_cons
          , "Mean Daily Consumption" = avg_con
          , "Median Daily Consumption" = med_con
          , "Mean Daily (kg_bw)" = avg_con_bw
        ) 
      
      }, digits = table_digits, caption = "NOTE:Consumption in grams/ml")
    
    output$cons_mean_gender <- renderTable({
      
      check_filter_combined()
      
      cons_mean_gender() %>% 
        select(
          "Age Group" = AgeGroup
          , Gender
          ,"Participants" = participants
          , "Consumers" = consumers
          , "Proportion" = prop
          , "Total Consumption" = total_cons
          , "Mean Daily Consumption" = avg_con
          , "Median Daily Consumption" = med_con
          , "Mean Daily (kg_bw)" = avg_con_bw
        ) 
      
    }, digits = table_digits, caption = "NOTE:Consumption in grams/ml")
    
    output$cons_individual <- DT::renderDataTable({ 
      
      check_filter_combined()
      
      cons_individual()
      })
    
    output$foodex1 <- DT::renderDataTable({ foodex1}, filter = "top")
    
    output$participants <- DT::renderDataTable({
      
      check_data_input()
      
      participants() 
      
      # not worjing below
      # %>% 
      #   DT::formatRound ( c("Weight"), 1) %>% 
      #   DT::formatRound ( c("gr_day", "gr_kbw_day"), 2) 
      
      }, filter = "top")
    
    output$freq_gender_age <- renderTable({
      
      check_data_input()
      
      freq_gender_age()}, caption = "" )
    
    output$freq_district_urban <- renderTable({
      
      check_data_input()
      
      freq_district_urban()
      
    }, caption = "")
    
    output$data_description <- DT::renderDataTable({ data_description}, filter = "top"
                                                   , options = list(pageLength = 30)
                                                   )
}

# RUN ####

shinyApp(ui=ui,server=server)