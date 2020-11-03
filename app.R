

library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)
library(janitor)

options(shiny.maxRequestSize = 30 * 1024^2,
        dplyr.summarise.inform = FALSE
)

ggplot2::theme_set(ggplot2::theme_light(16))

Sys.setlocale(locale = "greek")

side_width   <- 3
main_width   <- 12 - side_width
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
  readRDS("data/FoodEx1.rds") %>% 
  select(FOODEX_L4_CODE, FOODEX_L4_DESC, FOODEX_L3_CODE, FOODEX_L3_DESC,
         FOODEX_L2_CODE, FOODEX_L2_DESC, FOODEX_L1_CODE, FOODEX_L1_DESC)

data_description <- 
  readRDS("data/data_description.rds")


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
                        
                        mainPanel(
                          
                          tabsetPanel(id = "data_tabset",
                                      
                                      tabPanel(title = "FULL DATASET", 
                                               h2("Here is the FULL table of the observations"), 
                                               DT::dataTableOutput("consumption")
                                      ),
                                      tabPanel(title = "Participants", 
                                               h2("The table of participants in the food survey"), 
                                               DT::dataTableOutput("participants"),
                                               div(id = "down_participants")
                                      ),
                                      
                                      tabPanel(title = "FoodEx1",
                                               h3("The FoodEx1 food classification system"),
                                               DT::dataTableOutput("foodex1")
                                      ),
                                      
                                      tabPanel(title = "Survey Samples",
                                               h3("The FoodSurvey sample sizes"),
                                               p("The table shows the sample size [% (N)] of participants"),
                                               tableOutput("freq_gender_age"),
                                               div(id="freq1"),
                                               p(" "),
                                               tableOutput("freq_district_urban"),
                                               div(id="freq2")
                                               
                                      ),
                                      tabPanel(title = "Data Description",
                                               h3("A description of the columns in the dataset"),
                                               #p("The table shows what each column in the data represents"),
                                               mod_downloadTable_ui("tbl_data_description"),
                                               DT::dataTableOutput("tbl_data_description")
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
                          
                          ,uiOutput("filters_UI")
                          # Other inputs
                          , width = side_width
                        ),
                        
                        mainPanel(
                          
                          h3("Combined food groups/names/recipes"),
                          
                          tabsetPanel(id = "inTabset",
                                      
                                      tabPanel(title = "Descriptives"
                                               
                                               ,h2("Descriptives per Age Group (g/day) - Consumers only")
                                               
                                               ,tableOutput("cons_mean")
                                               
                                               ,h2("Descriptives per AgeGroup and Gender (g/day) - Consumers only")
                                               
                                               ,tableOutput("cons_mean_gender")
                                               
                                               ,h2("Individual mean consumption")
                                               
                                               ,DT::dataTableOutput("cons_individual")
                                               
                                               
                                      ),
                                      
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
  
  callModule(mod_downloadTable_server, "cons_mean",
             table_name = "Consumption_byAgeGroup",
             the_table = cons_mean_cleaned)
  
  callModule(mod_downloadTable_server, "cons_mean_gender",
             table_name = "Consumption_byAgeGroup-Gender",
             the_table = cons_mean_gender_cleaned)
  
  callModule(mod_downloadTable_server, "cons_individual",
             table_name = "Consumption_bySubjectid",
             the_table = cons_individual_cleaned)
  
  callModule(mod_downloadTable_server, "participants",
             table_name = "Participants",
             the_table = participants)
  
  callModule(mod_downloadTable_server, "freq_gender_age",
             table_name = "Sample_byGender-Age",
             the_table = freq_gender_age)
  
  callModule(mod_downloadTable_server, "freq_district_urban",
             table_name = "Sample_byDistrict-Urban",
             the_table = freq_district_urban)
  
  callModule(mod_downloadTable_server, "tbl_data_description",
             table_name = "Data description",
             the_table = tbl_data_description)
  
  
  #  Insert Download buttons only if the user selected something from the filters
  observe({
    if(!is.null(filters())){
      
      insertUI(
        selector = "#cons_mean",
        ui = mod_downloadTable_ui("cons_mean")
      )
      
      insertUI(
        selector = "#cons_mean_gender",
        ui = mod_downloadTable_ui("cons_mean_gender")
      )
      
      insertUI(
        selector = "#cons_individual",
        ui = mod_downloadTable_ui("cons_individual")
        
      )
    }
  })
  
  # Insert when the dataset is up
  observeEvent(participants(),{
    
    # Something weird happens here..Buttons load before the table renderings
    # and are lost..
    
    # I use custme selectors via div(id =..), and not the tableOutput id
    # Problem solved, but why?
    
    insertUI(
      selector = "#down_participants",
      ui = mod_downloadTable_ui("participants")
    )
    
    insertUI(
      selector = "#freq1",
      ui = mod_downloadTable_ui("freq_gender_age")
    )
    
    insertUI(
      selector = "#freq2",
      ui = mod_downloadTable_ui("freq_district_urban")
    )
    
  })
  
  
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
  
  
  file_input_error <- 
    c(      "Your data does not have the correct column names!\nPlease check the following:\n
           a) Your data are in a sheet called `Sheet1`,\n
           b) The column names are the ones shown in the Data Description tab")
  
  
  # FUNCTIONS ####
  
  # Function to filter the consumption by the food group
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
      group_by(AgeGroup, "participants"= n, .add = TRUE) %>% 
      summarise(consumers  = n(),
                total_cons = sum(total_consumption),
                med_con    = median(gr_day),
                avg_con    = mean(gr_day),
                max_con    = max(gr_day),
                perc_95    = quantile(gr_day, probs  = 0.95),
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
    
    ext <- tools::file_ext(input$file$name)
    
    file.rename(input$file$datapath,
                paste(input$file$datapath, ext, sep=".")
    )
    
    
    validate(need(ext == "xlsx", "Please upload an xlsx file"))
    
    file_name <- paste(input$file$datapath, ext, sep=".")
    
    
    # if the readxl has some warinings it all gets stuck!!!!
    suppressWarnings(
      file_sheets <- readxl::excel_sheets(file_name)
    )
    
    validate(
      need("Sheet1" %in% file_sheets, "There is no 'Sheet1' worksheet in the excel file"
      )
    )
    
    suppressWarnings(
      temp <- readxl::read_xlsx(file_name, sheet = "Sheet1") 
    )
    
    validate(
      need(all(names(temp) %in% var_names),file_input_error)
    )
    
    temp %>% 
      select(all_of(var_names)) %>% 
      left_join(
        foodex1
        , by = c("foodexOldCode" = "FOODEX_L4_CODE") 
      ) %>% 
      mutate(
        AgeGroup = factor(AgeGroup, levels = age_levels),
        Gender = factor(Gender, levels = gender_levels, labels = gender_levels)
      )
    
  })
  
  # helper datasets
  number_of_days <- reactive(dataset() %>% group_by(ORSUBCODE) %>% summarise(n_days = max(DAY))) 
  subject_weight <- reactive(dataset() %>% distinct(ORSUBCODE, Weight))
  n_age_group    <- reactive(dataset() %>% distinct(ORSUBCODE, .keep_all = TRUE) %>%  count(AgeGroup)) 
  
  filters <- reactive({
    
    c(
      input$fdx1
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
  
  output$participants <- DT::renderDataTable({
    
    check_data_input()
    
    participants() %>% 
      DT::datatable(
        filter = "top"
      ) %>% 
      DT::formatRound ( c("Weight", "Age"), 1) 
    #DT::formatRound ( c("gr_day", "gr_kbw_day"), 2) 
    
  })
  
  cons_mean <- reactive({
    
    if(!is.null(filters())){
      dataset() %>% 
        filter_fdgrp(filters()) %>% 
        aggr_age_group()  
    } else return("No Data")
    
  })
  
  cons_mean_cleaned <- reactive({
    # Select and clean names for Viewing and Downloading
    cons_mean() %>% 
      select(
        "Age Group" = AgeGroup
        ,"Participants" = participants
        , "Number of Consumers" = consumers
        , "% of Consumers" = prop
        #, "Total Consumption" = total_cons
        , "Mean" = avg_con
        , "Median" = med_con
        , "95th percentile" = perc_95
        , "Mean-2 (gr/day/kg_bw)" = avg_con_bw
      ) 
    
  })
  
  output$cons_mean <- renderTable({
    
    check_filter_combined()
    
    cons_mean_cleaned()
    
    
  }, digits = table_digits#, caption = "NOTE:Consumption in grams/day"
  )
  
  
  cons_mean_gender <- reactive({
    
    dataset() %>% 
      filter_fdgrp(filters()) %>% 
      group_by(Gender) %>% 
      aggr_age_group() %>% 
      arrange(AgeGroup, Gender)
    
  })
  
  cons_mean_gender_cleaned <- reactive({
    # Select and clen for Viewing and Downloading.
    # I leave the originalreactive to create plots using
    # short and syntactily correct names
    cons_mean_gender() %>% 
      select(
        "Age Group" = AgeGroup
        , Gender
        ,"Participants" = participants
        , "Number of Consumers" = consumers
        , "% of Consumers" = prop
        #, "Overall Consumption (grams)" = total_cons
        , "Mean" = avg_con
        , "Median" = med_con
        , "95th percentile" = perc_95
        , "Mean-2 (gr/day/kg_bw)" = avg_con_bw
      )
    
  })
  
  
  
  output$cons_mean_gender <- renderTable({
    
    check_filter_combined()
    
    cons_mean_gender_cleaned() 
    
  }, digits = table_digits #, caption = "NOTE:Consumption in grams/day"
  )
  
  cons_individual <- reactive({
    
    filter_fdgrp(dataset(), filters()) %>% 
      mutate_if(is.numeric, ~ round(., 3))
  })
  
  cons_individual_cleaned <- reactive({
    
    cons_individual() %>% 
      select(
        ORSUBCODE,
        "Age Group" = AgeGroup,
        Gender,
        "Total Consumption" = total_consumption,
        "Total days"  = n_days,
        Weight,
        "Food occassions"  = n,
        "Mean daily consumption (grams)" = gr_day ,
        "Mean daily consumption (gr/kg.b.w/day)" =  gr_kbw_day
      )
  })
  
  output$cons_individual <- DT::renderDataTable({ 
    
    check_filter_combined()
    
    cons_individual_cleaned()
  })
  
  freq_gender_age <- reactive({
    
    dataset() %>% 
      distinct(ORSUBCODE, AgeGroup, Gender) %>%
      tabyl(Gender, AgeGroup, show_missing_levels = TRUE) %>% 
      adorn_totals(c("row", "col")) %>% 
      adorn_percentages() %>% 
      adorn_pct_formatting() %>% 
      adorn_ns() %>% 
      untabyl() 
  })
  
  freq_district_urban <- reactive({
    dataset() %>% 
      distinct(ORSUBCODE, district_en, URBAN) %>%
      tabyl(district_en, URBAN, show_missing_levels = TRUE) %>% 
      adorn_totals(c("row", "col")) %>% 
      adorn_percentages() %>% 
      adorn_pct_formatting() %>% 
      adorn_ns() %>% 
      untabyl() 
  })
  
  ## Create UI's. ####
  # Perhaps its better to create a function to create UI on 
  # variables I pre-determine, for cleaner code..
  
  
  output$filters_UI <- renderUI({
    
    fdx1_names <- unique(dataset()$fdx1_name)
    fdx2_names <- unique(dataset()$fdx2_name)
    ENFOOD_names <- unique(dataset()$ENFOODNAME)
    ORFOODNAME_names <- unique(dataset()$ORFOODNAME)
    ENRECEP_names <- unique(dataset()$ENRECIPEDESC)
    FOODEX_L3_names <- unique(dataset()$FOODEX_L3_DESC)
    FOODEX_L2_names <- unique(dataset()$FOODEX_L2_DESC)
    FOODEX_L1_names <- unique(dataset()$FOODEX_L1_DESC)
    
    tagList(
      selectInput("fdx1","FoodEx1 (L4) name(s)", choices = fdx1_names, multiple = TRUE),
      selectInput("fdx2","FoodEx2 name(s)", choices = fdx2_names, multiple = TRUE),
      selectInput("ENFOODNAME","English Food name(s)", choices = ENFOOD_names, multiple = TRUE),
      selectInput("ORFOODNAME","Greek Food name(s)", choices = ORFOODNAME_names, multiple = TRUE),
      selectInput("ENRECIPEDESC","Recipe name(s)", choices = ENRECEP_names, multiple = TRUE),
      selectInput("FOODEX_L3_DESC","Description @ FoodEx1 L3", choices = FOODEX_L3_names, multiple = TRUE),
      selectInput("FOODEX_L2_DESC","Description @ FoodEx1 L2", choices = FOODEX_L2_names, multiple = TRUE),
      selectInput("FOODEX_L1_DESC","Description @ FoodEx1 L1", choices = FOODEX_L1_names, multiple = TRUE)
      
    )
    
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
  
  
  # PLOTS ####
  plot_mean <- reactive({
    
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
  })
  
  plot_dist_age <- reactive({
    
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
  
  plot_box_age <- reactive({
    
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
    
    dataset() %>% 
      mutate_at(vars(DAY, ORSUBCODE), factor) %>% 
      select_at(input$show_vars)
    
  }, filter = "top")
  
  
  output$foodex1 <- DT::renderDataTable({ foodex1}, filter = "top")
  
  
  output$freq_gender_age <- renderTable({
    
    check_data_input()
    
    freq_gender_age()}, caption = "" 
  )
  
  
  output$freq_district_urban <- renderTable({
    
    check_data_input()
    
    freq_district_urban()
    
  }, caption = ""
  )
  
  
  # Data desctription. As a reactive to enable downloading via the module
  tbl_data_description <- reactive({
    
    data_description 
  })
  
  
  output$tbl_data_description <- DT::renderDataTable({ 
    
    tbl_data_description()
    
  }
  , filter = "top"
  , options = list(pageLength = 30)
  )
}


# output$Gender        <- renderUI({
#   gender_choices <- levels(dataset()$Gender)
#   list(
#     h3("Filter your data"),
#     p("Use these to filter down your data by GENDER and/or AREA, eg. MALES, LIMASSOL"),
#     selectInput("Gender", "Gender", choices = c("ALL" = "", gender_choices))
#   )
# })
# 
# output$Area        <- renderUI({
#   urban_choices <- unique(dataset()$URBAN)
#   selectInput("Area", "Area", choices = c("ALL" = "", urban_choices))
# })
# RUN ####

shinyApp(ui=ui,server=server)