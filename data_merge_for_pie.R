rm(list = ls())


# library -----------------------------------------------------------------
library(dplyr)
library(stringr)

# read_data ---------------------------------------------------------------

basic_analysis<- read.csv("outputs/02_butter_analysis/basic_analysis_overall.csv",stringsAsFactors = FALSE,
                          na.strings = c("", " ", NA))

old_start_with_cols <- c("fresh_food_items.","non_fresh_food_items.",
                     "non_fresh_food_items_1.","non_fresh_food_items_2.",
                     "vendors_colsed.","customer_visits_change.",
                     "income_changed_to_4_weeks.","stockpiling_behaviors.")

df <- basic_analysis %>% select(c(matches(paste(old_start_with_cols, collapse="|"))), -"i.vendors_colsed_median" )


new_start_with_cols <- c("fresh_food_items.","non_fresh_food_items.",
                     "hygenic_nfi.","other_NFI.",
                     "vendor_dynamic_1.","vendor_dynamic_2.",
                     "vendor_dynamic_3.","vendor_dynamic_4.")

 colnames(df) <- gsub("non_fresh_food_items_1.","hygenic_nfi.",colnames(df))
 colnames(df) <- gsub("non_fresh_food_items_2.","other_nfi.",colnames(df))
 colnames(df) <- gsub("vendors_colsed.","vendor_dynamic_1.",colnames(df))
 colnames(df) <- gsub("customer_visits_change.","vendor_dynamic_2.",colnames(df))
 colnames(df) <- gsub("income_changed_to_4_weeks.","vendor_dynamic_3.",colnames(df))
 colnames(df) <- gsub("stockpiling_behaviors.","vendor_dynamic_4.",colnames(df))
 colnames(df) <- gsub("from_wholesaler"," market within camp",colnames(df))

df %>% colnames()

write.csv(df,paste0("BGD_2020_Markets_Covid/outputs/datamerge/", str_replace_all(Sys.Date(),"-","_"), "_pie_chart_data_merge.csv"))

# non_fresh_food_items_1 == hygenic_nfi
# non_fresh_food_items_2 == other_NFI
# vendors_colsed == vendor_dynamic_1
# customer_visits_change == vendor_dynamic_2
# income_changed_to_4_weeks == vendor_dynamic_3
# stockpiling_behaviors ==vendor_dynamic_4


# Main sources of assessed commodities in the 2 weeks prior to dat --------


#  fresh_food_item --------------------------------------------------------

data_for_ffi <- basic_analysis %>% select(starts_with("fresh_food_items."))


# non_fresh_food_items ----------------------------------------------------

data_for_ffi <- basic_analysis %>% select(starts_with("non_fresh_food_items."))


# hygiene NFI ||| non_fresh_food_items_1----------------------------------------------------------

data_for_ffi <- basic_analysis %>% select(starts_with("non_fresh_food_items_1."))


non_fresh_food_items_2 #other NFI (e.g. paracetamol, tarpaulin)?



# How has the number of vendors in your market area changed in the --------

col <- vendors_colsed



# How has the number of daily customer visits change compared to 2 --------

col <- customer_visits_change



# % of vendors reporting a change in income in the 2 weeks prior t --------

col <- income_changed_to_4_weeks



# % of vendors reporting witnessing stockpiling behaviour by custo --------

cols <- stockpiling_behaviors



