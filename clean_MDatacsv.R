# Written by Hiromichi Ueda '21 in April 2021
# Merge the ModuleCounts csv files

library(tidyverse)
library(data.table)

get_subject <- function(course_shortname, division_lookup) {
  if (tolower(substr(course_shortname, start=1, stop=4)) %in% division_lookup$subject) {
    subj_str <- tolower(substr(course_shortname, start=1, stop=4))
  } else if (tolower(substr(course_shortname, start=1, stop=2)) %in% c('cs', 'pe')) {
    subj_str <- tolower(substr(course_shortname, start=1, stop=2))
  } else if (tolower(substr(course_shortname, start=1, stop=3)) %in% c('grk', 'gsc')){
    subj_str <- tolower(substr(course_shortname, start=1, stop=3))
  } else if (tolower(substr(course_shortname, start=1, stop=5)) == 'jalli'){
    subj_str <- tolower(substr(course_shortname, start=1, stop=5))
  } else {
    subj_str <- 'other'
  }
  return(subj_str)
}

terms_str <- c('2014Fall', '2015Winter','2015Spring', '2015Fall', '2015Winter', 
              '2016Spring', '2019Fall', '2020Winter', '2020Spring', '2020Fall')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path ))
df_list <- vector(mode='list', length=length(terms_str))
for (i in 1:length(terms_str)) {
  term_str <- terms_str[i]
  puf_path <- paste0("../ModuleCounts/Mdata_", term_str, "_ModuleCounts_PagesUrlsFiles.csv")
  rest_path <- paste0("../ModuleCounts/Mdata_", term_str, "_ModuleCounts_rest.csv")
  
  df <- read_csv(puf_path) %>%
    select(-category) %>% 
    left_join(read_csv(rest_path), by="shortname") %>%
    select(-category)
  colnames(df) <- c(c('shortname'), str_sub(colnames(df)[-1], start = 1, end=-7))
  df_list[[i]] <- cbind(term = term_str, df)
}
df_all <- rbindlist(df_list)
df_division <- read_csv('./sb21_division_lookup.csv', )
df_all <- df_all %>% 
  mutate(subject = sapply(shortname, get_subject, division_lookup=df_division)) %>%
  left_join(df_division, by='subject')

write.csv(df_all[,c(c(1,2,23,24,25), 3:22)], file='../ModuleCounts/Mdata_all.csv', row.names = FALSE)
