
#data_merge_multiround_comparisons.R & basic_analysis.R should be run first
#this script can run with Crlt + A, no changing required

rm(list = ls())

# library -----------------------------------------------------------------
library(dplyr)
library(stringr)

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

 #remove cols, which value is 0
 data_for_dfcults_fcd_in_repl<- data_for_dfcults_fcd_in_repl[,-(which(colSums(data_for_dfcults_fcd_in_repl)==0))]

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
 data_for_assistance_items_if_yes<- data_for_assistance_items_if_yes[,-(which(colSums(data_for_assistance_items_if_yes)==0))]


 sort_assistance_items_if_yes <- sort(data_for_assistance_items_if_yes[1,], decreasing = TRUE)

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

remove_cols <- c("fresh_food_items.From.wholesaler...market.outside.of.camp",
                 "fresh_food_items.From.wholesaler...market.within.camp", "fresh_food_items.from_wholesaler",
                 "fresh_food_items.market_outside_of_camp", "non_fresh_food_items.From.wholesaler...market.outside.of.camp",
                 "non_fresh_food_items.From.wholesaler...market.within.camp",
                 "non_fresh_food_items.from_wholesaler", "non_fresh_food_items.market_outside_of_camp",
                 "hygenic_nfi.From.wholesaler...market.outside.of.camp", "hygenic_nfi.From.wholesaler...market.within.camp",
                 "hygenic_nfi.from_wholesaler", "hygenic_nfi.market_outside_of_camp")

dm_pie_2 <- data_merge_pie %>% mutate(
        fresh_food_items.inside_the_camp = fresh_food_items.from_wholesaler + fresh_food_items.From.wholesaler...market.within.camp,
        fresh_food_items.outside_the_camp = fresh_food_items.market_outside_of_camp + fresh_food_items.From.wholesaler...market.outside.of.camp,

        non_fresh_food_items.inside_the_camp = non_fresh_food_items.from_wholesaler + non_fresh_food_items.From.wholesaler...market.within.camp,
        non_fresh_food_items.outside_the_camp = non_fresh_food_items.market_outside_of_camp + non_fresh_food_items.From.wholesaler...market.outside.of.camp,

        hygenic_nfi.inside_the_camp = hygenic_nfi.from_wholesaler + hygenic_nfi.From.wholesaler...market.within.camp,
        hygenic_nfi.outside_the_camp = hygenic_nfi.market_outside_of_camp + hygenic_nfi.From.wholesaler...market.outside.of.camp
) %>% select(-remove_cols)

write.csv(dm_pie_2,paste0("BGD_2020_Markets_Covid/outputs/datamerge/disaggregated_records/","pie_top5_data_merge.csv"))
write.csv(dm_pie_2,paste0("BGD_2020_Markets_Covid/outputs/datamerge/disaggregated_records/",str_replace_all(Sys.Date(),"-","_"),"_pie_top5_data_merge.csv"))


# read_data_for_multiround_comparisons and graphs -------------------------------------

data_merge_multiround_comparisons <- read.csv("BGD_2020_Markets_Covid/outputs/datamerge/disaggregated_records/datamerge_placeholders.csv",
                                   stringsAsFactors = FALSE,na.strings = c("", " ", NA))


data_merge_graphs <- read.csv("BGD_2020_Markets_Covid/outputs/datamerge/disaggregated_records/data_merge_for_graphs.csv",
                              stringsAsFactors = FALSE,na.strings = c("", " ", NA))

# combind -----------------------------------------------------------------

final_data_merge_to_be_shared <- cbind(data_merge_multiround_comparisons,dm_pie_2,data_merge_graphs)

write.csv(final_data_merge_to_be_shared,paste0("BGD_2020_Markets_Covid/outputs/datamerge/",str_replace_all(Sys.Date(),"-","_"),"_final_data_merge.csv"))

