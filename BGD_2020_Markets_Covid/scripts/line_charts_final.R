#new round data should be added first before running it.

rm(list = ls())

# library -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(Hmisc)

items<- c("food_item","non_food_item")[1]

# path --------------------------------------------------------------------

outputfolder_box <-"BGD_2020_Markets_Covid/outputs/datamerge/graphs/"

# data_preparation ---------------------------------------------------------------

round_1 <- read.csv("BGD_2020_Markets_Covid/inputs/clean_data/2020_04_23_reach_bgd_market_assessment_cleaned_r1.csv", stringsAsFactors = FALSE,
                    na.strings = c("", " ", NA))

round_2 <- read.csv("BGD_2020_Markets_Covid/inputs/clean_data/2020_05_12_reach_bgd_markets_assessment_cleaned_r2.csv", stringsAsFactors = FALSE,
                    na.strings = c("", " ", NA))

old_cols_names <-c("cheapest_price_for_1kg__of_fish","cheapest_price_for_12_of_chicken")

new_cols_names <-c("dry_fish_sale_in_past_week","cheapest_price_for_4mx5m_of_chicken")

round_1 <-round_1 %>%  rename_at(vars(old_cols_names),funs(str_replace(.,old_cols_names,new_cols_names)))


cols_for_line_graph <- c("X_uuid","price_of_1kg","cheapest_price_for_cooking_oil", "cheapest_price_for_1kg_of_lentils",
                         "cheapest_price_for_0.5kg_of_leafy_greens", "cheapest_price_for_1kg_of_bananas",
                         "cheapest_price_for_12__of_eggs", "dry_fish_sale_in_past_week",
                         "cheapest_price_for_4mx5m_of_chicken","cheapest_price_for_100g_soap_bar_of_soap",
                         "cheapest_price_for_0_5l_of_bleachwashing_powder"
                         )
#"cheapest_price_for_12_of_paracetamol",,"cheapest_price_for_4mx5m_of_tarpaulin",

round_1_clean <-round_1[cols_for_line_graph]
round_2_clean <- round_2[cols_for_line_graph]

cleaned_df <- rbind(round_1_clean,round_2_clean)


date_log <- read.csv("outputs/01_data_logger/date_log.csv", stringsAsFactors = FALSE,
                     na.strings = c("", " ", NA)) %>% select(-"reported_date")

data_with_round<- cleaned_df %>%  left_join(date_log,"X_uuid") #add_round

data_with_round$round <- capitalize(data_with_round$round)


# food_item -------------------------------------------------------------

if (items  =="food_item"){

palette <-  c("#58585a", "#58585a", "#58585a", "#ee5859", "#ee5859", "#ee5859",
                "#d2cbb8", "#d2cbb8" )

  #ee5859 (red), #58585a (grey), #d2cbb8 (beige), #939999 (othr gray) #023858 (blue)

line_typ <- c("dashed","solid","twodash","solid","dashed","twodash","dashed","solid"
                )

cols_needed <- c("price_of_1kg","cheapest_price_for_cooking_oil", "cheapest_price_for_1kg_of_lentils",
                   "cheapest_price_for_0.5kg_of_leafy_greens", "cheapest_price_for_1kg_of_bananas",
                   "cheapest_price_for_12__of_eggs", "dry_fish_sale_in_past_week",
                   "cheapest_price_for_4mx5m_of_chicken","round")

data_with_cols <- data_with_round[cols_needed]

final <- gather(data_with_cols,c(1:(ncol(data_with_cols)-1)),key = "key",value = "value")

final_group_gather <- final %>% group_by(key,round) %>% summarise(
    value= median(value,na.rm = T),
  )

final_data_for_chart <- final_group_gather %>% dplyr::mutate(
    name = if_else(grepl("price_of_1kg",key),"Rice",
                   if_else(grepl("cooking_oil",key),"Cooking oil",
                           if_else(grepl("lentils",key),"Lentils",
                                   if_else(grepl("leafy_greens",key),"Leafy greens",
                                           if_else(grepl("eggs",key),"Eggs",
                                                   if_else(grepl("bananas",key),"Bananas",
                                                           if_else(grepl("fish",key),"Dry fish",
                                                                   if_else(grepl("chicken",key),"Chicken","error",NULL
                                                                   )))))))))


final_data_for_chart$name <-factor(final_data_for_chart$name,unique(final_data_for_chart$name))

ymax <- max(final_data_for_chart$value,na.rm = T)+25

  ggplot(final_data_for_chart, aes(x = round, y = value,group =name)) +
    ylim (0,ymax)+
    geom_path(aes(color=name,linetype= name),size=1)+
    theme(axis.title.x = element_blank(),
          axis.text = element_text(size = 14),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(size = 0.5, linetype = "dashed",
                                            colour = "#c1c1c1"),
          legend.title=element_blank(),
          legend.text = element_text(size = 14,color="#58585A"),
          legend.position = "bottom",
          legend.justification = 0,
          legend.key.width =  unit(1,"cm"),
          legend.spacing.x = unit(1, "cm"),
          legend.spacing.y = unit(.5, "cm"),
          legend.key.size = unit(1, 'lines'),
          legend.key = element_rect(fill = NA),
          legend.text.align = 0)+ ylab("Price (BDT)")+
    scale_color_manual(values = palette) +
    scale_linetype_manual(values = line_typ)


  ggsave(path = outputfolder_box,filename ="line_food_item.jpg" ,width=13,height=7,units="cm",scale = 1.8,dpi = 400)
}
# Non food item -----------------------------------------------------------
if (items  =="non_food_item"){

palette <-  c("#58585a", "#ee5859")

  #ee5859 (red), #58585a (grey), #d2cbb8 (beige), #939999 (othr gray) #023858 (blue)

line_typ <- c("solid","solid")

cols_needed <- c("cheapest_price_for_100g_soap_bar_of_soap", "cheapest_price_for_0_5l_of_bleachwashing_powder",
                  "round")
#"cheapest_price_for_12_of_paracetamol","cheapest_price_for_4mx5m_of_tarpaulin",

data_with_cols <- data_with_round[cols_needed]

final <- gather(data_with_cols,c(1:(ncol(data_with_cols)-1)),key = "key",value = "value")

final_group_gather <- final %>% group_by(key,round) %>% summarise(
    value= median(value,na.rm = T),
  )

final_data_for_chart <- final_group_gather %>% dplyr::mutate(
    name = if_else(grepl("soap_bar",key),"Soap",
                   if_else(grepl("bleachwashing",key),"Washing powder",
                           if_else(grepl("paracetamol",key),"Paracetamol",
                                   if_else(grepl("tarpaulin",key),"Tarpaulin","error",NULL)))))


final_data_for_chart$name <-factor(final_data_for_chart$name,unique(final_data_for_chart$name))

ymax <- max(final_data_for_chart$value,na.rm = T)+10

ggplot(final_data_for_chart, aes(x = round, y = value,group =name)) +
    ylim (0,ymax)+
    geom_path(aes(color=name,linetype= name),size=1)+
    theme(axis.title.x = element_blank(),
          axis.text = element_text(size = 14),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(size = 0.5, linetype = "dashed",
                                            colour = "#c1c1c1"),
          legend.title=element_blank(),
          legend.text = element_text(size = 14,color="#58585A"),
          legend.position = "bottom",
          legend.justification = .5,
          legend.key.width =  unit(1,"cm"),
          legend.spacing.x = unit(.5, "cm"),
          legend.spacing.y = unit(.5, "cm"),
          legend.key.size = unit(1, 'lines'),
          legend.key = element_rect(fill = NA),
          legend.text.align = 0)+ ylab("Price (BDT)")+
    scale_color_manual(values = palette)+
    scale_linetype_manual(values = line_typ)


  ggsave(path = outputfolder_box,filename ="line_non_food_item.jpg" ,width=13,height=7,units="cm",scale = 1.8,dpi = 400)

}


