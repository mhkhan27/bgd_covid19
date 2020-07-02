
rm(list = ls())

# library -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(Hmisc)
library(lubridate)

# data_preparation ---------------------------------------------------------------

clean_data_file_paths<-list.files("BGD_2020_Markets_Covid/inputs/clean_data/",full.names = T) %>% sort()

round<- c()

for (i in 1:length(clean_data_file_paths)){
  assign(paste0("round_",i), read.csv(clean_data_file_paths[i]) %>% mutate(round = paste0("round_",i)))
  round <- c(round,paste0("round_",i))
}

colnames <- round_2 %>% select(contains("soap"),"round") %>% select(-ends_with(".tri"))%>% colnames() %>% dput

round_clean <- c()
for (i in round){
  assign(paste0(i,"_clean"), get(i)[colnames])
  round_clean <- c(round_clean,paste0(i,"_clean"))
}

cleaned_df <- data.frame()

for (i in round_clean) {
  cleaned_df<- rbind(cleaned_df,get(i))
}

write.csv(cleaned_df, "soap_data.csv")
