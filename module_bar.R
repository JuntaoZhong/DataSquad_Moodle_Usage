# Written by Hiromichi Ueda '21 (DataSquad) in March 2021
# Requirement: R version 4.0, with installation of packages tidyverse and ggplot2
# Last executed in March 2021

library(tidyverse)
library(ggplot2)

# given a course shortname, returns abbreviation of subject
# e.g.) given 'arbc103-01-s20', returns arbc
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

# returns title of the bar chart
get_title <- function(term_str, division, subject) {
  yr_str <- str_sub(term_str, 1, 4)
  season_str <- str_sub(term_str, start=5)
  if (subject != 'All') {
    return(paste0(season_str, ' ', yr_str, ', Subject: ', subject))
  } else if (division != 'All') {
    return(paste0(season_str, ' ', yr_str, ', Division: ', division))
  } else {
    return(paste0(season_str, ' ', yr_str))
  }
}

# set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path ))


# term must be between F14-S16 or F19-F20
plot_counts <- function(term, 
                        div = 'All',
                        subj_full = 'All',
                        division_path = './sb21_division_lookup.csv', 
                        interactive_lookup = c("assign", "chat", "database", "feedback", "forum", 
                                               "glossary", "languagelesson", "quiz", "survey", "workshop")) {
  # load files from the specified term
  puf_path <- paste0("./ModuleCounts/Mdata_", term, "_ModuleCounts_PagesUrlsFiles.csv")
  rest_path <- paste0("./ModuleCounts/Mdata_", term, "_ModuleCounts_rest.csv")
  
  # import the ModuleCounts csv files
  df <- read_csv(puf_path) %>%
    select(-category) %>% 
    left_join(read_csv(rest_path), by="shortname") %>%
    select(-category)
  
  # add columns subject (arbc), fullname (Arabic) and division (Arts & Literature)
  df_division <- read_csv(division_path)
  df <- df %>% 
    mutate(subject = sapply(shortname, get_subject, division_lookup=df_division)) %>%
    left_join(df_division, by='subject')
  
  if (subj_full != 'All') {
    df <- filter(df, fullname==subj_full)
  }
  if (div != 'All') {
    df <- filter(df, division==div)
  }
  
  # df has 'shortname', 21 columns about '_count', then 'subject', 'fullname', 'division'
  # e.g.) arbc103-01-s20, <all the counts>, arbc, Arabic, Arts & Literature
  df_count <- data.frame(
    module = substr(colnames(df[,c(-1,-22,-23,-24)]), start=1, stop=nchar(colnames(df[,c(-1,-22,-23,-24)]))-6),
    count = colSums(Filter(is.numeric, df))
  ) %>% 
    mutate(is_interactive = module %in% interactive_lookup)
  
  ggplot(df_count) + 
    geom_bar(aes(x = module, y=count, fill=is_interactive), stat='identity') + 
    ggtitle(get_title(term, div, subj_full))
}

division_type <- c("Arts & Literature", "Humanities", "Interdisciplinary Studies", 
                   "Natural Sciences & Mathematics", "Other", "PEAR", "Social Sciences" )
plot_counts(term = '2020Spring', div = 'All', subj_full = 'Mathematics')
