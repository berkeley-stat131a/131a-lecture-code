############# Imports

options(tidyverse.quiet = TRUE)

if (! require("tidyverse")) {
  install.packages("tidyverse")
}
library(tidyverse)

if (! require("glue")) {
  install.packages("glue")
}
library(glue)

if (! require("broom")) {
  install.packages("broom")
}
library(broom)

if (! require("fs")) {
  install.packages("fs")
}
library(fs)

if (! require("ROCR")) {
  install.packages("ROCR")
}
library(ROCR)

if (! require("readxl")) {
  install.packages("readxl")
}
library(readxl)

if (! require("patchwork")) {
  install.packages("patchwork")
}
library(patchwork)

# Other useful libraries
# library(furrr)
# library(future)
# library(stargazer)
# library(xtable)
# library(httpgd)

count = dplyr::count
filter = dplyr::filter
select = dplyr::select
between = dplyr::between

############# Options

options(scipen = 999)
options(digits=3)
options(signif = 4)
options(max.print=1000)
options(dplyr.summarise.inform = FALSE)

theme_set(theme_bw())

# Parallel processing options
# MAX_SIZE_IN_MB <- 50000
# options(future.globals.maxSize= MAX_SIZE_IN_MB*1024^2)
# plan(multisession(workers = availableCores() %/% 3 + 1))

############# Constants

# CBPALETTE <- c("#999999", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
# CBPALETTE <- c("#E69F00", "#009E73", "#999999", "#0072B2", "#D55E00", "#000000")
# CBPALETTE <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
CBPALETTE = c("#56B4E9", "#E69F00", "#009E73", "#D55E00", "#0072B2",  "#CC79A7", "#999999")

############# General functions

# clear httgd cache in VSCode
clear_plots = function() { httpgd::hgd_clear(which = dev.cur()) }

# Won't return -Inf for NA-only vectors
safe_na_max <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  return(max(x, na.rm=TRUE))
}

# Won't return Inf for NA-only vectors
safe_na_min <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  return(min(x, na.rm=TRUE))
}

# Simple heuristic to convert camelcase col names to underscores
camel_to_underscore <- function(s) {
  
  s %>% 
    str_replace_all("([a-z0-9])([A-Z])", 
                    "\\1_\\2") %>% 
    str_to_lower()
}

# Get the proportion of each column with NAs
summarize_missing_cols = function(df) {
  df %>% 
    summarize(across(everything(), ~ mean(is.na(.x)))) %>% 
    glimpse
}

# Get proportion of each column that is NA as a dataframe
get_prop_na_col_values = function(df) {
  df %>% 
    summarize(across(everything(), ~ mean(is.na(.x)))) %>% 
    pivot_longer(
      everything(),
      names_to='col_name',
      values_to='prop_na'
    )
}

# Common typing mistake
VIew = View

get_auc = function(pred, true) {
  pred_ROCR <- prediction(pred, true)
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  auc_ROCR
}

inv_logit = function(x) {
  exp(x)/(1+exp(x))
}

# Tables should include NA by default
table = function(...) { base::table(..., useNA='always') }

make_pretty_percent = function(x) { 
  case_when(
    x > 1 ~ format(round(x), big.mark=','),
    x >= 0.01 ~ glue('{round(x*100)}%'),
    TRUE ~ glue('{round(x*100, 1)}%'),
  )
}

make_pretty_proportion = function(x) { 
  case_when(
    x > 1 ~ format(round(x), big.mark=','),
    x > 0.005 ~ format(round(x, digits=2), nsmall = 2),
    TRUE ~ format(round(x, digits=3), nsmall = 3) 
  )
}

# Can modify as needed for specific project
print_summary_table = function(summary_df) {
  print(xtable(summary_df,
               # rownames are first col even though they are removed
               align=c('|c', '|r', rep('|c', ncol(summary_df)-2), '|c|'),
               caption='\\emph{TKTK}'),
        table.placement='',
        tabular.environment='longtable',
        include.rownames=FALSE,
        floating=FALSE,
        hline.before=1
        # hline.after=ifelse(hlines, 1:nrow(summary_df), NA)
  )
  cat('\n')
  cat('\\FloatBarrier')
  cat('\n\n')
  cat('\\newpage')
  cat('\n\n')
}

# For printing dataframes to be copied elsewhere
# https://gist.github.com/hrbrmstr/8c7862092681d06e6535ab38d03074bf
as_tribble <- function(x) {
  
  out <- capture.output(write.csv(x, quote=TRUE, row.names=FALSE))
  out[1] <- gsub('"', '`', out[1])
  out[1] <- sub('^`', "  ~`", out[1])
  out[1] <- gsub(',`', " , ~`", out[1])
  out <- paste0(out, collapse=",\n  ")
  cat(sprintf("tribble(\n%s\n)", out, ")\n"))
  
}

# unnest faster using data.table
# cols: vector of list-columns to unnest 
unnest_dt <- function(tbl, cols) {
  
  tbl <- as.data.table(tbl)
  
  clnms <- setdiff(colnames(tbl), cols)
  
  tbl <- tbl[, lapply(.SD, unlist), by = clnms]
  
  colnames(tbl) <- c(clnms, cols)
  
  tbl = tibble(tbl)
  
  tbl
}

# Determine if any elements of character vector v are detected in string s
str_detect_any = function(s, v) {
  any(map_lgl(v, ~ str_detect(s, .x)))
}

# select matching columns of df based on character vector of regexes
select_from_chr_vector <- function(data, chr_vec) {
  
  cols = colnames(data)
  
  matched_cols = chr_vec %>% 
    map(function(s) keep(cols, ~ str_detect(.x, s))) %>% 
    unlist() %>% 
    unique()
  
  # even though this is simpler, we want to select columns in the order
  # they appear
  # matched_cols = unique(keep(cols, ~ any(str_detect(.x, chr_vec))))
  
  data[,matched_cols]
}

# sparse model matrices don't like weird characters in col names
make_cols_safe_for_sparse_matrix = function(df) {
  
  colnames(df) = colnames(df) %>% 
    str_replace_all(" ", "_") %>% 
    str_remove_all("[^A-Za-z0-9_]") %>% 
    # e.g., 100 --> V100
    str_replace_all("^([0-9]*)$", "V\\1")
  
  df
}

# Determine whether a binary column has very few 1s
is_low_variance_column = function(v, min_prevalence) {
  
  if (uniqLen(v) == 1) {
    return(TRUE)
  }
  
  
  # only care about low variance sparse cols
  # computationally intensive to check nonzero values other than 1 for
  # long vectors --> max(table(v))/length(v) < (1-min_prevalence)
  zero_indices = (v==0)
  
  # nonzero_indices = (v!=0)
  # all_zero_one = all(zero_indices | nonzero_indices)
  # 
  # if (! all_zero_one) {
  #   return(FALSE)
  # }
  
  # e.g., super sparse col with one 1 and rest 0s. mean(zero_indices) will
  # be super close to 1.
  if (mean(zero_indices) > (1-min_prevalence)) {
    return(TRUE)
  }
  
  one_indices = (v==1)
  
  # e.g., super sparse col with one 0 and rest 1s. mean(one_indices) will
  # be super close to 1.
  if (mean(one_indices) > (1-min_prevalence)) {
    return(TRUE)
  }
  
  # # make sure sufficient obs in each binary category
  # is_low_variance = 
  #   min(mean(zero_indices), mean(nonzero_indices)) < min_prevalence
  # 
  # is_low_variance
  
  return(FALSE)
}

# Safe standardizing as (x-u)/sd 
standardize_col = function(col) {
  
  if (length(col) == 1) {
    return(0)
  }
  
  sd_col = sd(col, na.rm=TRUE)
  # avoid infinite standardized col if only one unique count
  sd_col = if_else(sd_col==0, 1, sd_col)
  mean_col = mean(col, na.rm=TRUE)
  standardized_col = (col - mean_col)/sd_col
  standardized_col
}

# gets unique col vals and calculates prevalence of each val
# Useful for initial EDA 
summarize_unique_col_values = function(df, min_prevalence=0.001, max_rows=100) {
  
  count_df_list = map(
    df, 
    ~ .x %>% 
      as_tibble() %>% 
      count(value) %>% 
      mutate(p=n/sum(n)) %>%
      select(value, p) %>% 
      arrange(desc(p)) %>% 
      slice(1:max_rows)
  )
  
  filtered_df_list = map_if(
    count_df_list,
    # ignore cols with too many unique non NA values
    ~ max(filter(.x, ! is.na(value))[['p']]) <= min_prevalence,
    # if removing unique values, retain info about missingness
    ~ .x %>% filter(is.na(value))
  )
  
  max_n_rows = max(map_dbl(filtered_df_list, nrow))
  
  named_df = map2(
    filtered_df_list, 
    names(filtered_df_list),
    ~ .x %>% 
      add_row(p = rep(NA, max_n_rows - nrow(.))) %>% 
      select('{.y}' := value, 'p_{.y}' := p)
  ) %>% 
    bind_cols()
  
  return(named_df)
}

############# Project-specific functions
