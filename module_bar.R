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
  if (!'All' %in% subject) {
    return(paste0(season_str, ' ', yr_str, ', Subject: ', paste(subj_full, collapse = ', ')))
  } else if (!'All' %in% division) {
    return(paste0(season_str, ' ', yr_str, ', Division: ', paste(division, collapse = ', ')))
  } else {
    return(paste0(season_str, ' ', yr_str))
  }
}

# set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path ))


# term must be between F14-S16 or F19-F20
plot_counts <- function(term, 
                        div = c('All'),
                        subj_full = c('All'),
                        division_path = './sb21_division_lookup.csv', 
                        interactive_lookup = c("assign", "chat", "database", "feedback", "forum", 
                                               "glossary", "languagelesson", "quiz", "survey", "workshop")) {
  # load files from the specified term
  puf_path <- paste0("../ModuleCounts/Mdata_", term, "_ModuleCounts_PagesUrlsFiles.csv")
  rest_path <- paste0("../ModuleCounts/Mdata_", term, "_ModuleCounts_rest.csv")
  
  # import the ModuleCounts csv files
  df <- read_csv(puf_path) %>%
    select(-category) %>% 
    left_join(read_csv(rest_path), by="shortname") %>%
    select(-category)
  colnames(df) <- c(c('shortname'), str_sub(colnames(df)[-1], start = 1, end=-7)) # remove _count from column names
  
  # add columns subject (arbc), fullname (Arabic) and division (Arts & Literature)
  df_division <- read_csv(division_path)
  df <- df %>% 
    mutate(subject = sapply(shortname, get_subject, division_lookup=df_division)) %>%
    left_join(df_division, by='subject')
  
  if (!'All' %in% subj_full) {
    df <- filter(df, fullname %in% subj_full)
  }
  if (!'All' %in% div) {
    df <- filter(df, division==div)
  }
  
  # df has 'shortname', 21 columns about '_count', then 'subject', 'fullname', 'division'
  # e.g.) arbc103-01-s20, <all the counts>, arbc, Arabic, Arts & Literature
  df_count <- data.frame(
    module = colnames(Filter(is.numeric, df)),
    avg_count = df %>% summarise_if(is.numeric, mean) %>% t()
  ) %>%
    remove_rownames() %>%
    mutate(is_interactive = module %in% interactive_lookup)
  
  ggplot(df_count) + 
    geom_bar(aes(x = module, y=avg_count, fill=is_interactive), stat='identity') + 
    ggtitle(get_title(term, div, subj_full))
}

division_type <- c("Arts & Literature", "Humanities", "Interdisciplinary Studies", 
                   "Natural Sciences & Mathematics", "Other", "PEAR", "Social Sciences" )
plot_counts(term = '2020Fall', div = c('All'), subj_full = c('All'))#= c('Mathematics', 'Statistics'))
