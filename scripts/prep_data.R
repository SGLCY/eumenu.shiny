

library(tidyverse)
library(readxl)


dataset <- read_xlsx("Consumption_EUMENU_mapped_fdx2_fdx1 Lot2.xlsx")

dataset <- dataset %>% filter(AgeGroup != "Pregnant Women")

foodex1 <- read_excel("data/FoodEx1.xlsx")

write_rds(foodex1,"data/FoodEx1.rds" )

age_levels <- c(
  "Infants: 0-11 MONTHS"
  , "Toddlers: 12 -35 months" 
  , "Other Children: (36 mo to <10 y)"
  , "Adolescents: 10-17"
  , "Adults: 18-64"
  , "Elderly: 65-74"
  , "Pregnant Women"
) 

# What is better read_xlsx or readRDS?
dat <- data.frame(x = runif(100, 1, 1000), y=runif(10, 1, 1000))
bench::mark(
  min_time = .1,
  
  dat[dat$x > 500, ],
  dat[which(dat$x > 500), ],
  subset(dat, x > 500)
  )


dataset %>% 
  left_join(
    foodex1
    , by = c("foodexOldCode" = "FOODEX_L4_CODE") ) 

# a sample of the datset

dataset_small <- 
  dataset %>% 
  #select(1:5) %>% 
  sample_frac(0.10) 

dataset_small %>% 
    write_csv("sample_lot1.csv")

dataset_small %>% 
  writexl::write_xlsx("sample_lot1.xlsx")



dataset %>% 
  select(SURVEY:AgeGroup,Age:BMI) %>% 
  writexl::write_xlsx("LOT1.xlsx")


participants = dataset %>% distinct(ORSUBCODE, AgeGroup)

number_of_days = 
  dataset %>% 
  group_by(ORSUBCODE) %>% 
  summarise(n_days = max(DAY))

subject_weight = dataset %>% distinct(ORSUBCODE, Weight)

n_age_group = dataset %>% distinct(ORSUBCODE, .keep_all = T) %>%  count(AgeGroup) 

dataset %>% 
  filter(fdx1_name == "Breakfast cereals") %>% 
  group_by(AgeGroup) %>% 
  summarise(quantity = sum(AMOUNTFRAW))

food_group = "Breakfast cereals"

foodex1_foods <- unique(dataset$fdx1_name)
foodex2_foods <- unique(dataset$fdx2_name)


# function to filter the consumption by the food group
filter_fdgrp <- function(.data, food_group){
  
  .data %>% 
    filter(fdx1_name %in% food_group) %>%
    group_by(AgeGroup, ORSUBCODE) %>% 
    summarise(total_consumption = sum(AMOUNTFRAW)) %>% 
    left_join(number_of_days, by = "ORSUBCODE") %>% 
    left_join(subject_weight, by = "ORSUBCODE") %>%
    left_join(n_age_group, by = "AgeGroup") %>% 
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


dataset %>% 
filter_fdgrp(food_group) %>% 
  aggr_age_group()


# Distribution of consunption ####

dataset %>% 
  filter_fdgrp(food_group) %>% 
  ggplot(aes(total_consumption))+
  geom_histogram(bins = 20, binwidth = 10, colour = "white", fill = "lightgreen")+
  geom_density()+
  geom_vline(aes(xintercept = mean(.data$total_consumption)),colour = "red", linetype = "dashed")+
  labs(y = "Number of consumers"
       , x = "Total Consumption (gr)"
       , title = "Distribution of total consumption")
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))

  # MEAN CONSUMPTIOM TABLES ####
  
  dataset <- read_xlsx("Consumption_EUMENU_mapped_fdx2_fdx1 Lot2.xlsx")
  
  dataset <- dataset %>% filter(AgeGroup != "Pregnant Women") %>% 
    mutate(AgeGroup = factor(AgeGroup, levels = age_levels))
  
  foodex1 <- read_excel("data/FoodEx1.xlsx")
  
  
  dataset <- 
  dataset %>% 
    left_join(
      foodex1
      , by = c("foodexOldCode" = "FOODEX_L4_CODE") ) 

  participants = dataset %>% distinct(ORSUBCODE, AgeGroup)
  
  number_of_days = 
    dataset %>% 
    group_by(ORSUBCODE) %>% 
    summarise(n_days = max(DAY))
  
  subject_weight = dataset %>% distinct(ORSUBCODE, Weight)
  
  n_age_group = dataset %>% distinct(ORSUBCODE, .keep_all = T) %>%  count(AgeGroup) 
  
  
  food_group <- sym("FOODEX_L1_DESC")
  
  total_by_subject <- 
    dataset %>% 
      group_by(ORSUBCODE, AgeGroup , !!food_group) %>% 
      summarise(total = sum(AMOUNTFRAW)) %>% 
      left_join(number_of_days, by = "ORSUBCODE") %>% 
      left_join(subject_weight, by = "ORSUBCODE") %>%
      left_join(n_age_group, by = "AgeGroup") %>%
      mutate(gr_day = total/n_days,
             gr_kbw_day  = total/ n_days/ Weight
             ) %>% 
    ungroup() 
  
  daily_consumption <- 
    total_by_subject %>% 
      group_by(!!food_group) %>% 
      summarise(consumers   = n_distinct(ORSUBCODE),
                gr_day_sub   = mean(gr_day),
                sd_gr_day    = sd(gr_day),
                gr_day_subKW = mean(gr_kbw_day)
                ) 
  
  daily_consumption %>% 
    arrange(desc(gr_day_sub)) %>% 
    mutate_at(vars(!!food_group), ~ fct_inorder(.)) %>% 
    ggplot(aes(x = !!food_group, y = gr_day_sub))+
    geom_bar(width = 0.5, stat = "identity")+
    coord_flip()+
    scale_x_discrete(labels = scales::wrap_format(40))
    
    