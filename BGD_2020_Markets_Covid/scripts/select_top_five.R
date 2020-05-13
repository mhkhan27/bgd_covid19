#this script is archive. we are not using it anymore. Just keeping for future reference

rm(list = ls())

library(dplyr)


# read_data ---------------------------------------------------------------

basic_analysis<- read.csv("outputs/02_butter_analysis/basic_analysis_overall.csv",stringsAsFactors = FALSE,
                          na.strings = c("", " ", NA))

basic_analysis[is.na(basic_analysis)] <- 0 #covert NA to 0

previous_round <- read.csv("inputs/01_daily_data/BGD_covid_19_market_monitoring_roiund1.csv", stringsAsFactors = FALSE,
                    na.strings = c("", " ", NA))

current_round <- read.csv("inputs/01_daily_data/BGD_covid_19_market_monitoring_roiund2.csv", stringsAsFactors = FALSE,
                    na.strings = c("", " ", NA))


# difficulties_faced_in_replenishing-top 5 --------------------------------

data_for_dfcults_fcd_in_repl <- basic_analysis %>% dplyr::select(starts_with("difficulties_faced_in_replenishing."))

#remove cols, which value is 0
data_for_dfcults_fcd_in_repl<- data_for_dfcults_fcd_in_repl[,-(which(colSums(data_for_dfcults_fcd_in_repl)==0))]

sort_for_dfcults_fcd_in_repl <- sort(data_for_dfcults_fcd_in_repl[1,], decreasing = TRUE)


if( ncol(sort_for_dfcults_fcd_in_repl) > 5 ){
  top5_dfcults_fcd_in_repl<- sort_for_dfcults_fcd_in_repl[1:5]
}

if( ncol(sort_for_dfcults_fcd_in_repl) < 6 ){
  top5_dfcults_fcd_in_repl<- sort_for_dfcults_fcd_in_repl
}


# assistance_items_if_yes-top 5  ------------------------------------------

data_for_assistance_items_if_yes <- basic_analysis %>% dplyr::select(starts_with("assistance_items_if_yes."))

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





# change is number of vendor ----------------------------------------------


Change_in_number_of_vendor <- sum(current_round$vendors_operational,na.rm =T)-sum(previous_round$vendors_operational,na.rm =T)


altogether <- list(top5_dfcults_fcd_in_repl = top5_dfcults_fcd_in_repl,
          top5_assistance_items_if_yes = top5_assistance_items_if_yes,
          Change_in_number_of_vendor=Change_in_number_of_vendor)


top5 <- data.frame(lapply(altogether, "length<-", max(lengths(altogether))))

write.csv(top5,"BGD_2020_Markets_Covid/outputs/datamerge/top5_vendor_num_change.csv",na = "")



# number of_vendor_selling each_item --------------------------------------

cols_to_sell_item<- current_round %>% select(starts_with("sell")) %>% colnames()

df <- current_round[cols_to_sell_item]

df2 <- df %>% gather() %>% dplyr::filter(!is.na(value))

df3 <- df2 %>% dplyr::group_by(key) %>% summarise(
  no_of_vendors_selling_yes = sum(value == "yes"),
  no_of_vendors_selling_no = sum(value == "no")
)

write.csv(df3,"BGD_2020_Markets_Covid/outputs/datamerge/vendors selling each item.csv",na = "")
