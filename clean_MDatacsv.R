# Written by Hiromichi Ueda '21 (DataSquad) in April 2021
# Requirement: R version 4.0, with installation of packages tidyverse and data.table
# Last executed in April 2021

library(tidyverse)
library(data.table)

# get the subject abbreviation e.g.) 'cs', 'arbc'
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
  # drop '_count' from the column name
  colnames(df) <- c(c('shortname'), str_sub(colnames(df)[-1], start = 1, end=-7))
  df_list[[i]] <- cbind(term = term_str, df) # add the term as a new column
}
df_all <- rbindlist(df_list)
df_division <- read_csv('./sb21_division_lookup.csv', )
# add subject ('arbc'), fullname ('Arabic') and division ('Arts & Literature')
df_all <- df_all %>% 
  mutate(subject = sapply(shortname, get_subject, division_lookup=df_division)) %>%
  left_join(df_division, by='subject')
# bring columns term, shortname, subject, fullname and division to the left
# reorder the module columns alphabetically
write.csv(df_all[,c(c(1,2,23,24,25), 6:10, 4, 11:17, 3, 18:20, 5, 21:22)], file='../ModuleCounts/Mdata_all.csv', row.names = FALSE)
