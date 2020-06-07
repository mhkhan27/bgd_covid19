#this script can run with Crlt + A,  No change is required

rm(list = ls())

# library -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(Hmisc)
library(lubridate)

items<- c("food_item","non_food_item")[1]

# path --------------------------------------------------------------------

outputfolder_box <-"BGD_2020_Markets_Covid/outputs/datamerge/graphs/"

# data_preparation ---------------------------------------------------------------

clean_data_file_paths<-list.files("BGD_2020_Markets_Covid/inputs/clean_data",full.names = T) %>% sort()

round<- c()

for (i in 1:length(clean_data_file_paths)){
  assign(paste0("round_",i), read.csv(clean_data_file_paths[i]))
  round <- c(round,paste0("round_",i))
}

old_cols_names_r1 <-c("cheapest_price_for_1kg__of_fish","cheapest_price_for_12_of_chicken",
                      "cheapest_price_for_12__of_eggs","price_of_1kg")
new_cols_names_r1 <-c("cheapest_price_for_1kg_of_dry_fish","cheapest_price_for_1kg_of_chicken",
                      "cheapest_price_for_12_of_eggs","cheapest_price_for_1kg_rice")


old_cols_names_r2 <-c("dry_fish_sale_in_past_week" ,
                      "cheapest_price_for_4mx5m_of_chicken",
                      "cheapest_price_for_12__of_eggs",
                      "price_of_1kg")
new_cols_names_r2 <-c("cheapest_price_for_1kg_of_dry_fish",
                      "cheapest_price_for_1kg_of_chicken",
                      "cheapest_price_for_12_of_eggs",
                      "cheapest_price_for_1kg_rice")

round_1 <-round_1 %>%  rename_at(vars(old_cols_names_r1),funs(str_replace(.,old_cols_names_r1,new_cols_names_r1)))

round_2 <-round_2 %>%  rename_at(vars(old_cols_names_r2),funs(str_replace(.,old_cols_names_r2,new_cols_names_r2)))

round_2$cheapest_price_for_12_of_paracetamol <- NA
round_2$cheapest_price_for_4mx5m_of_tarpaulin <- NA

cols_for_line_graph <- c("X_uuid","cheapest_price_for_1kg_rice","cheapest_price_for_cooking_oil", "cheapest_price_for_1kg_of_lentils",
                         "cheapest_price_for_0.5kg_of_leafy_greens", "cheapest_price_for_1kg_of_bananas",
                         "cheapest_price_for_12_of_eggs", "cheapest_price_for_1kg_of_dry_fish",
                         "cheapest_price_for_1kg_of_chicken","cheapest_price_for_100g_soap_bar_of_soap",
                         "cheapest_price_for_0_5l_of_bleachwashing_powder",
                         "cheapest_price_for_12_of_paracetamol","cheapest_price_for_4mx5m_of_tarpaulin")

round_clean <- c()
for (i in round){
  assign(paste0(i,"_clean"), get(i)[cols_for_line_graph])
  round_clean <- c(round_clean,paste0(i,"_clean"))
}

cleaned_df <- data.frame()

for (i in round_clean) {
  cleaned_df<- rbind(cleaned_df,get(i))
}


# cleaned_df <- rbind(round_1_clean,round_2_clean,round_3_clean)


date_log <- read.csv("outputs/01_data_logger/date_log.csv", stringsAsFactors = FALSE,
                     na.strings = c("", " ", NA)) %>% select(-"reported_date")

data_with_round<- cleaned_df %>%  left_join(date_log,"X_uuid") #add_round

data_with_round$round <- capitalize(data_with_round$round)



# food_item -------------------------------------------------------------

if (items  =="food_item"){

# palette <-  c("#58585a", "#58585a", "#58585a", "#ee5859", "#ee5859", "#ee5859",
#                 "#d2cbb8", "#d2cbb8" )

  #ee5859 (red), #58585a (grey), #d2cbb8 (beige), #939999 (othr gray) #023858 (blue)
#
# line_typ <- c("dashed","solid","twodash","solid","dashed","twodash","dashed","solid"
#                 )

palette <-  tmaptools::get_brewer_pal("Dark2",n=8)
line_typ <- c("solid","solid","solid","solid","solid","solid","solid","solid"
)

cols_needed <- c("cheapest_price_for_1kg_rice","cheapest_price_for_cooking_oil", "cheapest_price_for_1kg_of_lentils",
                   "cheapest_price_for_0.5kg_of_leafy_greens", "cheapest_price_for_1kg_of_bananas",
                   "cheapest_price_for_12_of_eggs", "cheapest_price_for_1kg_of_dry_fish",
                   "cheapest_price_for_1kg_of_chicken","round")

data_with_cols <- data_with_round[cols_needed]

final <- gather(data_with_cols,c(1:(ncol(data_with_cols)-1)),key = "key",value = "value")

final_group_gather <- final %>% group_by(key,round) %>% summarise(
    value= median(value,na.rm = T),
  )
# final_group_gather <- final_group_gather %>% separate(round,c("month","week"),sep = "-")

# final_group_gather$round <- paste0(final_group_gather$month,"-",final_group_gather$week)

final_data_for_chart <- final_group_gather %>% dplyr::mutate(
    name = if_else(grepl("price_for_1kg_rice",key),"Rice",
                   if_else(grepl("cooking_oil",key),"Cooking oil",
                           if_else(grepl("lentils",key),"Lentils",
                                   if_else(grepl("leafy_greens",key),"Leafy greens",
                                           if_else(grepl("eggs",key),"Eggs",
                                                   if_else(grepl("bananas",key),"Bananas",
                                                           if_else(grepl("fish",key),"Dry fish",
                                                                   if_else(grepl("chicken",key),"Chicken","error",NULL
                                                                   )))))))),
    order = if_else(grepl("price_for_1kg_rice",key),8,
                   if_else(grepl("cooking_oil",key),6,
                           if_else(grepl("lentils",key),4,
                                   if_else(grepl("leafy_greens",key),1,
                                           if_else(grepl("eggs",key),2,
                                                   if_else(grepl("bananas",key),3,
                                                           if_else(grepl("fish",key),7,
                                                                   if_else(grepl("chicken",key),5,0,NULL
                                                                   )))))))))

final_data_for_chart <-final_data_for_chart %>% arrange(order)

final_data_for_chart$name <-factor(final_data_for_chart$name,unique(final_data_for_chart$name))

ymax <- max(final_data_for_chart$value,na.rm = T)+25

final_data_for_chart<- final_data_for_chart %>%
  mutate(
    date=case_when(
      round=="April - Week 3" ~"2020-04-20",
      round=="May - Week 1"~ "2020-05-04",
      round=="May - Week 3"~ "2020-05-18",
      TRUE~"2020-06-07"
    ) %>% lubridate::ymd(),
    date_f=floor_date(date,"week"),
    date_c=ceiling_date(date,"week"),
    cut_Date = cut(as.Date(date), "week") %>% as.Date(),
    xmin=floor_date(date,"month")-2,
    xmax=ceiling_date(date,"month")+2,
    year= year(date_f),
    week= week(date_f)
  )

final_data_for_chart<- final_data_for_chart %>% arrange(final_data_for_chart$cut_Date)

ggplot(final_data_for_chart, aes(x = cut_Date, y = value,group =name)) +
  geom_path(aes(color=name,linetype= name),size=1)+
  geom_point(aes(color = name),size = 2.2)+
  scale_x_date(date_breaks = "1 month",
               # date_labels = every_nth("%B",3),
               date_labels = ("%B"),
               date_minor_breaks = "1 week",
               limits=c(as_date("2020-03-30"),as_date("2020-07-06")))+
  scale_y_continuous(limits=c(0,ymax), expand = c(0, 0))+
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.minor.x= element_line(size = 0.5, linetype = "dashed",
                                         colour = "#c1c1c1"),
        panel.grid.minor.y= element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = "dashed",
                                          colour = "#c1c1c1"),
        panel.border = element_rect(colour = "#58585a", fill=NA, size=1),
        panel.spacing =  unit(0,"cm"),
        legend.title=element_blank(),
        legend.text = element_text(size = 14,color="#58585A"),
        legend.position = "bottom",
        legend.justification = 0,
        legend.key.width =  unit(1,"cm"),
        legend.spacing.x = unit(.9, "cm"),
        legend.spacing.y = unit(.9, "cm"),
        legend.key.size = unit(1, 'lines'),
        legend.key = element_rect(fill = NA),
        plot.margin = unit(c(0, .1, 0, 0), "cm"),
        legend.text.align = 0)+ ylab("Price (BDT)")+
  scale_color_manual(values = palette)+
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
                                   if_else(grepl("tarpaulin",key),"Tarpaulin","error",NULL)))),
    order = if_else(grepl("soap_bar",key),2,
                   if_else(grepl("bleachwashing",key),1,
                           if_else(grepl("paracetamol",key),3,
                                   if_else(grepl("tarpaulin",key),4,0,NULL)))))


final_data_for_chart <-final_data_for_chart %>% arrange(order)
final_data_for_chart$name <-factor(final_data_for_chart$name,unique(final_data_for_chart$name))

ymax <- max(final_data_for_chart$value,na.rm = T)+5

final_data_for_chart<- final_data_for_chart %>%
  mutate(
    date=case_when(
      round=="April - Week 3" ~"2020-04-20",
      round=="May - Week 1"~ "2020-05-04",
      round=="May - Week 3"~ "2020-05-18",
      TRUE~"2020-06-07"
    ) %>% lubridate::ymd(),
    date_f=floor_date(date,"week"),
    date_c=ceiling_date(date,"week"),
    cut_Date = cut(as.Date(date), "week") %>% as.Date(),
    xmin=floor_date(date,"month")-2,
    xmax=ceiling_date(date,"month")+2,
    year= year(date_f),
    week= week(date_f)
  )

final_data_for_chart<- final_data_for_chart %>% arrange(final_data_for_chart$cut_Date)

ggplot(final_data_for_chart, aes(x = cut_Date, y = value,group =name)) +
  geom_path(aes(color=name,linetype= name),size=1)+
  geom_point(aes(color = name),size = 2.2)+
  scale_x_date(date_breaks = "1 month",
               # date_labels = every_nth("%B",3),
               date_labels = ("%B"),
               date_minor_breaks = "1 week",
               limits=c(as_date("2020-03-30"),as_date("2020-07-08")))+
  scale_y_continuous(limits=c(0,ymax), expand = c(0, 0))+
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.minor.x= element_line(size = 0.5, linetype = "dashed",
                                         colour = "#c1c1c1"),
        panel.grid.minor.y= element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = "dashed",
                                          colour = "#c1c1c1"),
        panel.border = element_rect(colour = "#58585a", fill=NA, size=1),
        panel.spacing = unit(0,"cm"),
        legend.title=element_blank(),
        legend.text = element_text(size = 14,color="#58585A"),
        legend.position = "bottom",
        legend.justification = .5,
        legend.key.width =  unit(1,"cm"),
        legend.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(.9, "cm"),
        legend.key.size = unit(1, 'lines'),
        legend.key = element_rect(fill = NA),
        plot.margin = unit(c(0, .1, 0, 0), "cm"),
        legend.text.align = 0)+ ylab("Price (BDT)")+
  scale_color_manual(values = palette)+
  scale_linetype_manual(values = line_typ)

  ggsave(path = outputfolder_box,filename ="line_non_food_item.jpg" ,width=13,height=7,units="cm",scale = 1.8,dpi = 400)

}


