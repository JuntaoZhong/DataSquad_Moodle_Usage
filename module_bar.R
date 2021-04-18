# Written by Hiromichi Ueda '21 (DataSquad) in March 2021
# Requirement: R version 4.0, with installation of packages tidyverse and ggplot2
# Last executed in March 2021

library(tidyverse)
library(ggplot2)

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
plot_counts <- function(term_str, 
                        div = c('All'),
                        subj_full = c('All'),
                        division_path = './sb21_division_lookup.csv', 
                        interactive_lookup = c("assign", "chat", "database", "feedback", "forum", 
                                               "glossary", "languagelesson", "quiz", "survey", "workshop")) {
  # load the file with data for all terms
  df <- read_csv("../ModuleCounts/Mdata_all.csv") %>%
    filter(term == term_str)
  
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
    ggtitle(get_title(term_str, div, subj_full))
}

division_type <- c("Arts & Literature", "Humanities", "Interdisciplinary Studies", 
                   "Natural Sciences & Mathematics", "Other", "PEAR", "Social Sciences" )
plot_counts(term_str = '2020Fall', div = c('All'), subj_full = c('All'))#= c('Mathematics', 'Statistics'))
