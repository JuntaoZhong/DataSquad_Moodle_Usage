library(tidyverse)
library(ggplot2)

get_subject_str <- function(course_shortname) {
  if (tolower(substr(course_shortname, start=1, stop=2)) %in% c('cs', 'pe')) {
    subj_str <- tolower(substr(course_shortname, start=1, stop=2))
  } else if (tolower(substr(course_shortname, start=1, stop=3)) %in% c('grk', 'gsc')){
    subj_str <- tolower(substr(course_shortname, start=1, stop=3))
  } else if (tolower(substr(course_shortname, start=1, stop=5)) == 'jalli'){
    subj_str <- tolower(substr(course_shortname, start=1, stop=5))
  } else {
    subj_str <- tolower(substr(course_shortname, start=1, stop=4))
  }
  return(subj_str)
}

get_discipline <- function(course_shortname, division_lookup) {
  subj_str <- get_subject_str(course_shortname)
  if (subj_str %in% division_lookup$subject) {
    return(division_lookup[division_lookup$subject==subj_str, ]$division)
  } else {
    return('Other')
  }
}

get_subject <- function(course_shortname, division_lookup) {
  subj_str <- get_subject_str(course_shortname)
  if (subj_str %in% division_lookup$subject) {
    return(division_lookup[division_lookup$subject==subj_str, ]$fullname)
  } else {
    return('Other')
  }
}

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

# term must be between F14-S16 or F19-F20
plot_counts <- function(term, 
                        div = 'All',
                        subj = 'All',
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
  
  # add columns subject (Arabic) and discipline (Arts & Literature)
  df_division <- read_csv(division_path)
  df$subject <- lapply(df$shortname, function(x) sapply(x, get_subject, division_lookup=df_division))
  df$discipline <- lapply(df$shortname, function(x) sapply(x, get_discipline, division_lookup=df_division))
  
  # if specific discipline/division is selected, then we should limit the choice for subject
  # e.g. if Humanities is selected, then math cannot be chosen as the subject
  if (subj != 'All') {
    df <- filter(df, subject==subj)
  }
  if (div != 'All') {
    df <- filter(df, discipline==div)
  }
  
  df_count <- data.frame(
    module = substr(colnames(df[,c(-1,-22,-23)]), start=1, stop=nchar(colnames(df[,c(-1,-22,-23)]))-6),
    count = colSums(Filter(is.numeric, df))
  ) %>% 
    mutate(is_interactive = module %in% interactive_lookup)
  
  ggplot(df_count) + 
    geom_bar(aes(x = module, y=count, fill=is_interactive), stat='identity') + 
    ggtitle(get_title(term, div, subj))
}

division_type <- c("Arts & Literature", "Humanities", "Interdisciplinary Studies", 
                   "Natural Sciences & Mathematics", "Other", "PEAR", "Social Sciences" )
plot_counts(term = '2015Fall', div = 'All', subj = 'Mathematics')
