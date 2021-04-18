# Written by Hiromichi Ueda '21 (DataSquad) in April 2021
# Requirement: R version 4.0, with installation of the following packages:
# tidyverse, ggplot2, plotly, Dict
# Last executed in April 2021

library(tidyverse)
library(ggplot2)
library(plotly)
library(Dict)

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

# sort academic terms in chronological order (WI < SP < FA)
sort_terms <- function(all_terms) {
  term_month_dict <- Dict$new(
    Fall = '09',
    Winter = '01',
    Spring = '04'
  ) # starting month
  all_terms_num <- rep_len(0, length(all_terms))
  for (i in 1:length(all_terms)) {
    all_terms_num[i] <- paste0(str_sub(all_terms[i], 1, 4), term_month_dict[str_sub(all_terms[i], 5)])
  }
  terms_order_idx <- order(all_terms_num) # index of sorted elements
  terms_sorted <- rep_len('term', length(all_terms))
  for (i in 1:length(all_terms)) {
    terms_sorted[i] <- all_terms[terms_order_idx[i]]
  }
  return(terms_sorted)
}

plot_counts_oneterm <- function(df, term_str, div, subj_full, interactive_lookup) {
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
  
plot_counts_crossterm <- function(df, terms_str, div, subj_full, interactive_lookup) {
  p <- aggregate(. ~ term, df[-c(2:5)], mean) %>% # average counts; drop admin info about course
    gather(module, count, assign:workshop, factor_key = TRUE) %>% # convert to long format
    mutate(term = factor(term, levels = sort_terms(unique(df$term)))) %>% # sort terms (WI < SP < FA)
    ggplot(aes(x = term, y = count, group=module, color=module)) +
    geom_point(size=1) +
    geom_line()
  ggplotly(p, tooltip = c('x', 'y', 'group'))
}

# set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path ))


# term must be between F14-S16 or F19-F20
plot_counts <- function(terms_str, 
                        div = c('All'),
                        subj_full = c('All'),
                        division_path = './sb21_division_lookup.csv', 
                        interactive_lookup = c("assign", "chat", "database", "feedback", "forum", 
                                               "glossary", "languagelesson", "quiz", "survey", "workshop")) {
  df <- read_csv("../ModuleCounts/Mdata_all.csv")
  if (!'All' %in% terms_str) {
    df <- filter(df, term %in% terms_str)
  }
  if (!'All' %in% subj_full) {
    df <- filter(df, fullname %in% subj_full)
  }
  if (!'All' %in% div) {
    df <- filter(df, division==div)
  }
  if ((!'All' %in% terms_str) & (length(terms_str) == 1)) {
    plot_counts_oneterm(df, terms_str[1], div, subj_full, interactive_lookup)
  } else {
    plot_counts_crossterm(df, terms_str, div, subj_full, interactive_lookup)
  }
}

division_type <- c("Arts & Literature", "Humanities", "Interdisciplinary Studies", 
                   "Natural Sciences & Mathematics", "Other", "PEAR", "Social Sciences" )
plot_counts(terms_str = c('All'), div = c('All'), subj_full = c('All'))
