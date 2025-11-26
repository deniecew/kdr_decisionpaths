library(tidyverse)
library(dplyr)
library(tidyr)
library(flowchart) 


load("G:/Press Ganey II/Reports/Ad Hoc/DEEP DIVE/Key Driver Reports/data/alldata_fy25.Rdata")
load("G:/Press Ganey II/Reports/Ad Hoc/DEEP DIVE/Key Driver Reports/data/questions.Rdata")

# cutoff_date <- as.Date("2024-12-31")

questions <- questions %>%
  filter (service == "ON") 


data<-alldata %>%
  filter(type == "Physician") %>%
  filter(service == "ON")
  # filter(date > cutoff_date) 

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

# Step 1: Prepare topbox_counts
topbox_counts <- data %>%
  select(survey_id, varname, top_box) %>%
  pivot_wider(names_from = varname, values_from = top_box) %>%
  select(-survey_id) %>%
  filter(if_all(all_of(tmp5), ~ !is.na(.))) %>%
  count(across(all_of(tmp5))) %>%
  mutate(proportion = (100*n / sum(n)))

# Step 2: Create a helper function for summarization
summarize_LTR <- function(group_vars) {
  topbox_counts %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      x_count = sum(n[O7 == 1]),
      n_count = sum(n),
      LTR = round(100 * x_count / n_count,1),
      .groups = "drop"
    )
}

# Step 3: Apply the function for different levels
df_list <- lapply(list(tmp4[1], tmp4[1:2], tmp4), summarize_LTR)

# Step 4: Combine and select final columns
final_path <- bind_rows(df_list) %>%
  select(all_of(tmp4), x_count, n_count, LTR)

##Create network path
final_path <-final_path %>%
  rename_with(~c("kdr1", "kdr2", "kdr3"), .cols = 1:3)  %>%
  mutate(label = case_when(
    kdr1 == 0 & is.na(kdr2) & is.na(kdr3)  ~ "A",
    kdr1 == 1 & is.na(kdr2) & is.na(kdr3)  ~ "B",
    kdr1 == 0 & kdr2 == 0 & is.na(kdr3) ~ "C", 
    kdr1 == 0 & kdr2 == 1 & is.na(kdr3) ~ "D",    
    kdr1 == 1 & kdr2 == 0 & is.na(kdr3) ~ "E", 
    kdr1 == 1 & kdr2 == 1 & is.na(kdr3) ~ "F", 
    kdr1 == 0 & kdr2 == 0 & kdr3 == 0 ~ "G", 
    kdr1 == 0 & kdr2 == 0 & kdr3 == 1 ~ "H", 
    kdr1 == 0 & kdr2 == 1 & kdr3 == 0 ~ "I", 
    kdr1 == 0 & kdr2 == 1 & kdr3 == 1 ~ "J", 
    kdr1 == 1 & kdr2 == 0 & kdr3 == 0 ~ "K", 
    kdr1 == 1 & kdr2 == 0 & kdr3 == 1 ~ "L", 
    kdr1 == 1 & kdr2 == 1 & kdr3 == 0 ~ "M", 
    kdr1 == 1 & kdr2 == 1 & kdr3 == 1 ~ "N", 
  )) 

#Convert variable names to survey questions.

tmp6<-data.frame(varname= c(tmp4))

kdrs <- tmp6 %>%
  left_join(questions, by = "varname") %>%
  pull(question)
  # mutate(rank = c("kdr1", "kdr2", "kdr3"))


# Sample data
dataviz <- data.frame(
  x = c(1,1,2,2,2,2,3,3,3,3,3,3,3,3),
  y = c(7.5,25.5,3,12,21,30,1,5,10,14,19,23,28,32),
  label = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N")
)

dataviz <- dataviz %>%
  left_join(final_path, by = "label")

dataviz <- dataviz %>%
  rename_with(~c(kdrs), .cols = 4:6)


library(stringr)
#str_wrap allows for dynamic code

ggplot(dataviz, aes(x = x, y = y)) +
  geom_point(aes(color = ifelse(label %in% c("A", "C", "E", "G", "I", "K", "M"),
                               "Not Top-Box", "Top-Box")),shape = 21, size = 10) +
  scale_color_manual(values = c("Top-Box" = "blue", "Not Top-Box" = "maroon")) +
  geom_text(aes(label = LTR), size = 3) +
  annotate("text", x = 1, y = 40, label = str_wrap(colnames(dataviz)[4], width = 20), size = 3, fontface = "bold") +
  annotate("text", x = 2, y = 40, label = str_wrap(colnames(dataviz)[5], width = 20), size = 3, fontface = "bold") +
  annotate("text", x = 3, y = 40, label = str_wrap(colnames(dataviz)[6], width = 20), size = 3, fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA),   # White outer background
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.title = element_blank(),        # Remove axis titles
    axis.text = element_blank(),         # Remove axis text
    axis.ticks = element_blank()         # Remove axis ticks
  ) +
  theme(legend.position = "none") +
  xlim(0, 4) + ylim(0, 45)

ggsave("decisionpath.png", width = 8, height = 6, dpi = 300 )