#round should be changed in each round for naming purpose.
# no change in read_data

rm(list=ls())

# Library -----------------------------------------------------------------

library(dplyr)
library(butteR)
library(rgdal)
library(sf)
library(sp)
library(readr)
library(stringr)
library(AMR)
library(naniar)

round <- "r5"

# data_read ---------------------------------------------------------------

clean_data_file_paths<-list.files("BGD_2020_Markets_Covid/inputs/clean_data",full.names = T) %>% sort()


round_number<-length(clean_data_file_paths)


current_round <- read.csv(clean_data_file_paths[round_number], stringsAsFactors = FALSE,
                          na.strings = c("", " ", NA))

main_df <- current_round


# recoding ----------------------------------------------------------------

colnams <- c("days_of_stock_of_chicken","restocking_time_of_chicken",
             "days_of_stock_of_rice", "restocking_time_of_rice", "days_of_stock_of_cooking_oil",
             "restocking_time_of_cooking_oil","days_of_stock_of_lentils", "restocking_time_of_lentils",
             "days_of_stock_of_leafy_greens","restocking_time_of_leafy_greens","days_of_stock_of_bananas",
             "restocking_time_of_bananas","days_of_stock_of_eggs","restocking_time_of_eggs",
             "days_of_stock_of_dry_fish", "restocking_time_of_dry_fish", "days_of_stock_of_soap",
             "restocking_time_of_soap", "days_of_stock_of_washing_powder", "restocking_time_of_washing_powder"
             )


for(i in colnams){
    print(i)
    col <- paste0("i.",i)
    main_df[[col]] <- if_else(main_df[[i]] %in% 0:3,"0_3_days",
                              if_else(main_df[[i]] %in% 4:7, "4_7_days",
                                      if_else(main_df[[i]] > 7,"7_and_more","error",NULL)))
  }

# colnames_proportion <- c("rice_sale_in_past_week","oil_sale_in_past_week","chicken_sale_in_past_week",
#                          "lentils_sale_in_past_week","leafy_greens_sale_in_past_week","bananas_sale_in_past_week",
#                          "eggs_sale_in_past_week","dry_fish_sale_in_past_week","soap_sale_in_past_week",
#                          "washing_powder_sale_in_past_week","paracetamol_sale_in_past_week",
#                          "tarpaulin_sale_in_past_week")
#
# for(i in colnames_proportion){
#   print(i)
#   col2 <- paste0("i.",i)
#   main_df[[col2]] <- main_df[[i]]/main_df$vendors_operational
# }


# write_csv ---------------------------------------------------------------

write.csv(main_df,paste0("BGD_2020_Markets_Covid\\inputs\\recoded_data/",str_replace_all(Sys.Date(),"-","_"),"_reach_bgd_market_assessment_cleaned_",round,".csv"))
write.csv(main_df,paste0("BGD_2020_Markets_Covid\\inputs\\recoded_data/reach_bgd_market_assessment_cleaned.csv"))

