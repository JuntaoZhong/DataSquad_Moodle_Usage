library(tidyverse)
library(ggplot2)

# constants
term_str <- "2020Spring"
puf_path <- paste0("../MoodleData_PostPandemic/Mdata_", term_str, "_ModCounts_PagesUrlsFiles.csv")
rest_path <- paste0("../MoodleData_PostPandemic/Mdata_", term_str, "_ModCounts_rest.csv")
interactive_module <- c("assign", "chat", "database", "feedback", "forum", "glossary", 
                      "languagelesson", "quiz", "survey", "workshop")
division_type <- c("Arts & Literature", "Humanities", "Interdisciplinary Studies", 
                   "Natural Sciences & Mathematics", "Other", "PEAR", "Social Sciences" )
# a csv file to look up division of a subject 
division_path <- "./sb21_division_lookup.csv"

df_division <- read_csv(division_path)

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

get_discipline <- function(course_shortname, division_lookup) {
  subj_str <- get_subject_str(course_shortname)
  if (subj_str %in% division_lookup$subject) {
    return(division_lookup[division_lookup$subject==subj_str, ]$division)
  } else {
    return('Other')
  }
}

# data is similar to the above df, having column for count of each module, 
# with its shortname (arbc101), subject (Arabic) and discipline (Arts & Literature)
plot_counts <- function(data, interactive_lookup) {
  df_count <- data.frame(
    module = substr(colnames(data[,c(-1,-22,-23)]), start=1, stop=nchar(colnames(data[,c(-1,-22,-23)]))-6),
    count = colSums(Filter(is.numeric, data))
  ) %>% 
    mutate(is_interactive = module %in% interactive_lookup)
  
  ggplot(df_count) + 
    geom_bar(aes(x = module, y=count, fill=is_interactive), stat='identity')
}

# import the MData csv files
df <- read_csv(puf_path) %>% 
  select(-category) %>% 
  left_join(read_csv(rest_path), by="shortname") %>%
  select(-category) 
df$subject <- lapply(df$shortname, function(x) sapply(x, get_subject, division_lookup=df_division))
df$discipline <- lapply(df$shortname, function(x) sapply(x, get_discipline, division_lookup=df_division))
df %>% filter(discipline==division_type[4]) %>% plot_counts(interactive_lookup = interactive_module)
