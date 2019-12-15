


dataset %>% 
  distinct(ORSUBCODE, .keep_all = TRUE) %>% 
  select(ORSUBCODE, Gender, AgeGroup, Age, Weight, BMI, URBAN, area_en, district_en, SURVEY)




mtcars %>% 
  group_by(vs) %>% 
  summarise(avg = mean(mpg)) %>% 
  ggplot(aes(vs, avg))+
  coord_flip()+
  geom_col()+
  geom_text(aes(label = round(avg, 2)), hjust = 1)
  

boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Sepal.Width ~ fct_reorder(Species, Sepal.Width), data = iris)


dataset %>% 
  mutate(AgeGroup = factor(AgeGroup, levels = age_levels)) %>% count(AgeGroup)






test1 <- read_xlsx("Consumption_EUMENU_mapped_fdx2_fdx1 Lot1.xlsx")


 a = unique(test1$AgeGroup)

 b= unique(dataset$AgeGroup)

 c(a,b) 

 
 factor_age <- c(
   "Infants: 0-11 MONTHS"
   ,"Toddlers: 12 -35 months" 
   ,"Other Children: (36 mo to <10 y)"
   , "Adolescents: 10-17"
   , "Adults: 18-64"
   , "Elderly: 65-74"
   , "Pregnant women"
 ) 
 
 dataset %>% count(AgeGroup)
 
 
 
 f1 <- factor(c("a", "a", NA, NA, "a", "b", NA, "c", "a", "c", "b"))
 table(f1)
 
 f2 <- fct_explicit_na(f1)
 table(f2)
 
 dataset %>% distinct(ORSUBCODE, AgeGroup)
 
 
 number_of_days <- reactive(dataset() %>% group_by(ORSUBCODE) %>% summarise(n_days = max(DAY))) 
 subject_weight <- reactive(dataset() %>% distinct(ORSUBCODE, Weight))
 n_age_group    <- reactive(dataset() %>% distinct(ORSUBCODE, .keep_all = TRUE) %>%  count(AgeGroup)) 
 
 
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
     group_by(AgeGroup, ORSUBCODE) %>% 
     summarise(total_consumption = sum(AMOUNTFRAW)) %>% 
     left_join(number_of_days(), by = "ORSUBCODE") %>% 
     left_join(subject_weight(), by = "ORSUBCODE") %>%
     left_join(n_age_group(), by = "AgeGroup") %>% 
     mutate(gr_day =  total_consumption/ n_days,
            gr_kbw_day  = total_consumption/ n_days/ Weight) %>% 
     ungroup() 
   
 }
 
 dataset %>% 
   mutate(AgeGroup = factor(AgeGroup, levels = age_levels)) %>% 
   filter_fdgrp(food_group) %>% 
   ggplot(aes(x = AgeGroup, y = total_consumption))+
   geom_boxplot()+
   labs(y = "Total Consumption (gr)"
        , x = "Age Group"
        , title = "Distribution of total consumption by Age Group")
 
 
 number_of_days <- dataset %>% group_by(ORSUBCODE) %>% summarise(n_days = max(DAY)) 
 subject_weight <- reactive(dataset() %>% distinct(ORSUBCODE, Weight))
   
   dataset %>% 
     mutate(AgeGroup = factor(AgeGroup, levels = age_levels)) %>% 
     ggplot(aes(AgeGroup, BMI))
     
     distinct(ORSUBCODE, .keep_all = TRUE) %>%  count(AgeGroup)

     
     
     dataset %>%
       filter_fdgrp(food_group) %>% 
       aggr_age_group()

     mtcars2 <- as_tibble(mtcars)
     mtcars2[, "mpg", drop = FALSE] 
     