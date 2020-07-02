
#data_merge_multiround_comparisons.R & basic_analysis.R should run first
#this script can run with Crlt + A, No change is required

rm(list = ls())

# library -----------------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)

# read_data ---------------------------------------------------------------

basic_analysis<- read.csv("BGD_2020_Markets_Covid/outputs/butter_analysis/basic_analysis_overall.csv",stringsAsFactors = FALSE,
                          na.strings = c("", " ", NA))

# for pie chart -----------------------------------------------------------

old_start_with_cols <- c("fresh_food_items.","non_fresh_food_items.",
                     "non_fresh_food_items_1.","non_fresh_food_items_2.",
                     "vendors_colsed.","customer_visits_change.",
                     "income_changed_to_4_weeks.","stockpiling_behaviors.")

df_pie <- basic_analysis %>% select(c(matches(paste(old_start_with_cols, collapse="|"))), -"i.vendors_colsed_median" )


 colnames(df_pie) <- gsub("non_fresh_food_items_1.","hygenic_nfi.",colnames(df_pie))
 colnames(df_pie) <- gsub("non_fresh_food_items_2.","other_nfi.",colnames(df_pie))
 colnames(df_pie) <- gsub("vendors_colsed.","vendor_dynamic_1.",colnames(df_pie))
 colnames(df_pie) <- gsub("customer_visits_change.","vendor_dynamic_2.",colnames(df_pie))
 colnames(df_pie) <- gsub("income_changed_to_4_weeks.","vendor_dynamic_3.",colnames(df_pie))
 colnames(df_pie) <- gsub("stockpiling_behaviors.","vendor_dynamic_4.",colnames(df_pie))
 # colnames(df_pie) <- gsub("from_wholesaler","market_within_camp",colnames(df_pie))

 df_pie %>% colnames()


# top5 --------------------------------------------------------------------

basic_analysis_top5 <- basic_analysis

basic_analysis_top5[is.na(basic_analysis_top5)] <- 0 #covert NA to 0

## difficulties_faced_in_replenishing-top 5 --------------------------------

 data_for_dfcults_fcd_in_repl <- basic_analysis_top5 %>% dplyr::select(starts_with("difficulties_faced_in_replenishing."))
# data_for_dfcults_fcd_in_repl <-

 #remove cols, which value is 0
 data_for_dfcults_fcd_in_repl<- data_for_dfcults_fcd_in_repl %>% select_if(~sum(.) != 0)

 sort_for_dfcults_fcd_in_repl <- sort(data_for_dfcults_fcd_in_repl[1,], decreasing = TRUE)


 if( ncol(sort_for_dfcults_fcd_in_repl) > 5 ){
         top5_dfcults_fcd_in_repl<- sort_for_dfcults_fcd_in_repl[1:5]
 }

 if( ncol(sort_for_dfcults_fcd_in_repl) < 6 ){
         top5_dfcults_fcd_in_repl<- sort_for_dfcults_fcd_in_repl
 }


 #### assistance_items_if_yes-top 5  ------------------------------------------

 data_for_assistance_items_if_yes <- basic_analysis_top5 %>% dplyr::select(starts_with("assistance_items_if_yes."))

 #remove cols, which value is 0
 data_for_assistance_items_if_yes<- data_for_assistance_items_if_yes %>%  select_if(~sum(.) != 0)

 if( ncol(data_for_assistance_items_if_yes) > 1 ){
 sort_assistance_items_if_yes <- sort(data_for_assistance_items_if_yes[1,], decreasing = TRUE)}

 if( ncol(data_for_assistance_items_if_yes) == 1 ){
         sort_assistance_items_if_yes <- data_for_assistance_items_if_yes}


 if( ncol(sort_assistance_items_if_yes) > 5 ){
         top5_assistance_items_if_yes<- sort_assistance_items_if_yes[1:5]
 }

 if( ncol(sort_assistance_items_if_yes) < 6 ){
         top5_assistance_items_if_yes<- sort_assistance_items_if_yes
 }


 top5_data_merge <- cbind(top5_dfcults_fcd_in_repl,top5_assistance_items_if_yes)

 colnames(top5_data_merge) <- paste("top5", colnames(top5_data_merge), sep = "_")

 data_merge <- cbind(df_pie,top5_data_merge)

data_merge_pie <- data_merge %>%  mutate_all(.,function(x){x<-x*100})
data_merge_pie <- data_merge_pie %>%  mutate_all(.,function(x){x<-round(x,0)})

remove_cols <- c("fresh_food_items.from_wholesaler",
                 "fresh_food_items.market_outside_of_camp",
                 "non_fresh_food_items.from_wholesaler",
                 "non_fresh_food_items.market_outside_of_camp",
                 "hygenic_nfi.from_wholesaler",
                 "hygenic_nfi.market_outside_of_camp")

dm_pie_2 <- data_merge_pie %>% mutate(
        fresh_food_items.inside_the_camp = fresh_food_items.from_wholesaler,
        fresh_food_items.outside_the_camp = fresh_food_items.market_outside_of_camp,

        non_fresh_food_items.inside_the_camp = non_fresh_food_items.from_wholesaler ,
        non_fresh_food_items.outside_the_camp = non_fresh_food_items.market_outside_of_camp,

        hygenic_nfi.inside_the_camp = hygenic_nfi.from_wholesaler,
        hygenic_nfi.outside_the_camp = hygenic_nfi.market_outside_of_camp
) %>% select(-remove_cols)

write.csv(dm_pie_2,paste0("BGD_2020_Markets_Covid/outputs/datamerge/disaggregated_records/","pie_top5_data_merge.csv"))
write.csv(dm_pie_2,paste0("BGD_2020_Markets_Covid/outputs/datamerge/disaggregated_records/",str_replace_all(Sys.Date(),"-","_"),"_pie_top5_data_merge.csv"))


# stock_data --------------------------------------------------------------

stoke_restoke_cols <- basic_analysis %>% select(c(starts_with("i.days_of_stock_of_"),
                                                  starts_with("i.restocking_time_of_"),
                                                  "assistance_items.yes","faced_any_difficulties.yes")) %>%
        select(-ends_with("median"))

stoke_restoke <- stoke_restoke_cols %>%  mutate_all(.,function(x){x<-x*100})
stoke_restoke <- stoke_restoke %>%  mutate_all(.,function(x){x<-round(x,0)})

# read_data_for_multiround_comparisons and graphs -------------------------------------

data_merge_multiround_comparisons <- read.csv("BGD_2020_Markets_Covid/outputs/datamerge/disaggregated_records/datamerge_placeholders.csv",
                                   stringsAsFactors = FALSE,na.strings = c("", " ", NA))


data_merge_graphs <- read.csv("BGD_2020_Markets_Covid/outputs/datamerge/disaggregated_records/data_merge_for_graphs.csv",
                              stringsAsFactors = FALSE,na.strings = c("", " ", NA))



# overall_perctage_change -------------------------------------------------

xlsform_paths<-list.files("BGD_2020_Markets_Covid/inputs/kobo_tool/",full.names = T) %>% sort()
clean_data_file_paths<-list.files("BGD_2020_Markets_Covid/inputs/clean_data",full.names = T) %>% sort()
round_number<-length(clean_data_file_paths)

current_round_opc <- read.csv(clean_data_file_paths[round_number], stringsAsFactors = FALSE,
                              na.strings = c("", " ", NA))
prev_round_opc <- read.csv(clean_data_file_paths[round_number-1], stringsAsFactors = FALSE,
                           na.strings = c("", " ", NA))

old_cols_names_opc <-c("cheapest_price_for_1kg_of_dry_fish",
                       "cheapest_price_for_1kg_of_chicken",
                       "cheapest_price_for_12_of_eggs",
                       "cheapest_price_for_1kg_rice")
new_cols_names_opc <-c("cheapest_price_for_1kg_of_dry_fish",
                       "cheapest_price_for_1kg_of_chicken",
                       "cheapest_price_for_12_of_eggs",
                       "cheapest_price_for_1kg_rice")

prev_round_opc <-prev_round_opc %>%  rename_at(vars(old_cols_names_opc),funs(str_replace(.,old_cols_names_opc,new_cols_names_opc)))

cols_for_line_graph_opc <- c("X_uuid","cheapest_price_for_1kg_rice","cheapest_price_for_cooking_oil", "cheapest_price_for_1kg_of_lentils",
                             "cheapest_price_for_0.5kg_of_leafy_greens", "cheapest_price_for_1kg_of_bananas",
                             "cheapest_price_for_12_of_eggs", "cheapest_price_for_1kg_of_dry_fish",
                             "cheapest_price_for_1kg_of_chicken","cheapest_price_for_100g_soap_bar_of_soap",
                             "cheapest_price_for_0_5l_of_bleachwashing_powder")
#"cheapest_price_for_12_of_paracetamol",,"cheapest_price_for_4mx5m_of_tarpaulin",

prev_round_clean_opc <-prev_round_opc[cols_for_line_graph_opc]
current_round_clean_opc <- current_round_opc[cols_for_line_graph_opc]

cleaned_df_opc <- rbind(prev_round_clean_opc,current_round_clean_opc)

date_log <- read.csv("outputs/01_data_logger/date_log.csv", stringsAsFactors = FALSE,
                     na.strings = c("", " ", NA)) %>% select(-"reported_date")

data_with_round_opc<- cleaned_df_opc %>%  left_join(date_log,"X_uuid") #add_round

# data_with_round_opc$round <- capitalize(data_with_round_opc$round)

data_with_round_opc <- data_with_round_opc %>% select(-"X_uuid")

final <- gather(data_with_round_opc,c(1:(ncol(data_with_round_opc)-1)),key = "key",value = "value")

final_summary <- final %>% group_by(round,key) %>% summarise(
        media_value = median(value,na.rm = T))

final_summary2 <-final_summary %>% spread("round","media_value")

difference <- sum(final_summary2$`June - Week 1` )-sum(final_summary2$`May - Week 3` )

over_all_price_change <-data.frame( over_all_price_change = difference/sum(final_summary2$`May - Week 3` )*100)


# combind -----------------------------------------------------------------

final_data_merge_to_be_shared <- cbind(data_merge_multiround_comparisons,dm_pie_2,data_merge_graphs,stoke_restoke,over_all_price_change)

write.csv(final_data_merge_to_be_shared,paste0("BGD_2020_Markets_Covid/outputs/datamerge/",str_replace_all(Sys.Date(),"-","_"),"_final_data_merge.csv"))

