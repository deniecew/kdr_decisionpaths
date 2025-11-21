library(tidyverse)
library(dplyr)
library(tidyr)


load("G:/Press Ganey II/Reports/Ad Hoc/DEEP DIVE/Key Driver Reports/data/alldata_fy25.Rdata")
load("G:/Press Ganey II/Reports/Ad Hoc/DEEP DIVE/Key Driver Reports/data/questions.Rdata")

cutoff_date <- as.Date("2024-12-31")

questions <- questions %>%
  filter (service == "ON") 


data<-alldata %>%
  filter(type == "Physician") %>%
  filter(service == "ON") %>%
  filter(date > cutoff_date) 

npi_filter <- 1679869507
outvar<-"O7"
outvar_all<-c("MED7","O2","O3","O7","I69", "SS54")

provider_data <- data %>%
  filter(npi == npi_filter)

name_filter<-data %>%
  filter(npi == npi_filter) %>%
  distinct(name) %>%
  slice_tail(n = 1)

### Compute correlations
tmp1 <- provider_data %>%
  select(survey_id, varname, response) %>%
  pivot_wider(names_from = varname, values_from = response) %>%
  mutate(survey_id = as.numeric(survey_id)) %>%
  select(-survey_id) %>%
  cor(use = "pairwise.complete.obs") %>%
  { tibble(corr = .[, outvar], varname = rownames(.)) } %>%
  filter(!is.na(corr))


### Calculate top box ratios

tmp2 <- provider_data %>%
  select(survey_id, varname, top_box) %>%
  pivot_wider(names_from = varname, values_from = top_box) %>%
  select(-survey_id) %>%
  { 
    tbwide <- .
    tibble(
      varname = colnames(tbwide),
      ratio = sapply(seq_along(tbwide), function(i) {
        x <- addmargins(xtabs(~ tbwide[[i]] + tbwide[[outvar]], tbwide))
        ((x[2, 2] + 0.5) / (x[2, 3] + 1)) / ((x[1, 2] + 0.5) / (x[1, 3] + 1))
      })
    )
  }


### Calculate Percentile Rank

tmp3 <- data %>%
  select(survey_id, npi, type, varname, top_box) %>%
  group_by(npi, varname) %>%
  summarise(tbscore = mean(top_box) * 100, n = n(), .groups = "drop") %>%  # use mean() instead of sum()/n()
  filter(n > 29) %>%
  group_by(varname) %>%
  mutate(percent_rank = 100 * rank(tbscore) / n()) %>%
  filter(npi == npi_filter)

### Calculate Priority Indexes and Filter Out the Top 3 Key Drivers

tmp4 <- tmp3 %>%
  merge(tmp1, by = "varname") %>%
  merge(tmp2, by = "varname") %>%
  filter(!varname %in% outvar_all) %>%
  mutate(
    rescaled_corr = (corr - min(corr)) / (max(corr) - min(corr)),
    rescaled_tbratio = (ratio - min(ratio)) / (max(ratio) - min(ratio)),
    driver_index = 100 * ((rescaled_corr + rescaled_tbratio) / 2),
    priority_index = driver_index - (0.5 * percent_rank)
  ) %>%
  arrange(desc(priority_index)) %>%
  top_n(3,priority_index) %>%
  pull(varname)  #use pull when you want to store as a vector instead of a column in a table

tmp5<-tmp4 %>%
  append('O7')


###Create Decision Paths for the Top 3 Key Drivers
#path value is number of rows where KD and LTR are top box divided by number of rows where KD is topbox

topbox_counts<- provider_data %>%
  select(survey_id, varname, top_box) %>%
  pivot_wider(names_from = varname, values_from = top_box) %>%
  select(-survey_id) %>%
  filter(if_all(all_of(tmp5), ~ !is.na(.))) %>%
  count(across(all_of(tmp5))) %>%  #Use across() with all_of() for dynamic columns
  mutate(proportion = n / sum(n))

df1 <- topbox_counts %>%
  group_by(across(all_of(tmp4[1])))%>%
  summarise(
    x_count = sum(n[O7 == 1]),
    n_count = sum(n),
    LTR = x_count / n_count
  )

df2 <- topbox_counts %>%
  group_by(across(all_of(tmp4[1:2])))%>%
  summarise(
    x_count = sum(n[O7 == 1]),
    n_count = sum(n),
    LTR = x_count / n_count
  )

df3 <- topbox_counts %>%
  group_by(across(all_of(tmp4)))%>%
  summarise(
    x_count = sum(n[O7 == 1]),
    n_count = sum(n),
    LTR = x_count / n_count
  )

final_path <-bind_rows(df1,df2,df3)

final_path <- final_path %>%
  select(all_of(tmp4),x_count, n_count, LTR)
