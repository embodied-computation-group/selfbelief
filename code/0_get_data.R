library(tidyverse)


# import the data
# load metacognition data - from Lund et al: 
# https://github.com/embodied-computation-group/dg-metacognition/blob/main/data_summary/mle_fit_data_wide.csv
## NOTE - THESE ARE THE SAME PARTICIPANTS AS IN LUND ET AL AND HOOGERVOORST ET AL - DO NOT DUPLICATE! 

metacog_data <- read_delim("data/mle_fit_data_wide.csv", ",", escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::select(subject,matches("^(avg_conf_|mratio_|da_)"))

# read in raw self belief data. includes additional surveys (MIAI and MDESQ, not analyzed here)

survey_data_pre <- read_delim("data/self_belief_pre_labels.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::select(Sid, age, gender, years_edu, matches("^self_"))


survey_data_post <- read_delim("data/self_belief_post_labels.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::select(Sid, matches("^self_"))

# read in the accuracy data from Lund et al. 

acc_data <- read_csv("data/taskAcc.csv") %>% 
  dplyr::select(subj, matches("^acc_"), matches("^rt_"))



# Rename the ID columns to subjID
survey_data_pre <- survey_data_pre %>% rename(subjID = Sid)
survey_data_post <- survey_data_post %>% rename(subjID = Sid)
metacog_data <- metacog_data %>% rename(subjID = subject)
acc_data <- acc_data %>% rename(subjID = subj)

# Ensure subjID is a factor (categorical variable)
survey_data_pre$subjID <- as.factor(survey_data_pre$subjID)
survey_data_post$subjID <- as.factor(survey_data_post$subjID)
metacog_data$subjID <- as.factor(metacog_data$subjID)
acc_data$subjID <- as.factor(acc_data$subjID)

# Join the data frames
# Start with the pre survey data and then join each subsequent dataset
full_data <- survey_data_pre %>%
  left_join(survey_data_post, by = "subjID") %>%
  left_join(metacog_data, by = "subjID") %>%
  left_join(acc_data, by = "subjID")

# View the first few rows of the combined dataset
head(full_data)



# Filter out subject "996", was a practice run i.e. not real data
filtered_data <- full_data %>%
  filter(subjID != "996")

# Calculate new variables for the difference between accuracy and self-belief measures
# Assuming accuracy variables are acc_mem (for memory), acc_vis (for visual), etc.
# and self-belief variables are named self_memory_pre, self_visual_pre, etc.
result_data <- filtered_data %>%
  mutate(
    diff_acc_vis_pre = acc_vis*100 - self_visual_pre,
    diff_acc_vis_post = acc_vis*100 - self_visual_post,
    diff_acc_mem_pre = acc_mem*100 - self_memory_pre,
    diff_acc_mem_post = acc_mem*100 - self_memory_post,
    diff_acc_cal_pre = acc_cal*100 - self_calories_pre,
    diff_acc_cal_post = acc_cal*100 - self_calories_post,
    diff_acc_gdp_pre = acc_gdp*100 - self_gdp_pre,
    diff_acc_gdp_post = acc_gdp*100 - self_gdp_post,
    VisionSBD = self_visual_post- self_visual_pre,
    MemorySBD = self_memory_post- self_memory_pre,
    CalorieSBD = self_calories_post- self_calories_pre,
    GDPSBD = self_gdp_post- self_gdp_pre,
    VisSBScoreDiff = diff_acc_vis_post - diff_acc_vis_pre,
    MemSBScoreDiff = diff_acc_mem_post - diff_acc_mem_pre,
    CalSBScoreDiff = diff_acc_cal_post - diff_acc_cal_pre,
    GDPSBScoreDiff = diff_acc_gdp_post - diff_acc_gdp_pre,
  )

write.csv(result_data, file = "data/SB_data.csv")


## fit partial correlation matrix
# for partial correlation
library(ppcor)
library(corrplot)

# for linear model
library(lme4)
library(sjPlot) # table functions
library(sjmisc) # sample data




selected_vars <- result_data[,grepl("self_.*_pre|self_.*_post|avg_conf_.*", names(result_data))]

selected_vars_complete_cases <- na.omit(selected_vars)

pcor_matrix <- pcor(selected_vars_complete_cases)$estimate


png("figures/partial_correlation_matrix.png", width = 1200, height = 1200)

# Plotting the partial correlation matrix
corrplot(pcor_matrix, method = "color", type = "full",
         addCoef.col = "black", tl.col = "black", tl.srt = 45, 
         cl.lim = c(-1, 1), diag = FALSE)
dev.off()


## lmer of demographic variables


self_data <- result_data %>% 
  dplyr::select(subjID, age, gender, years_edu, self_memory_pre:self_gdp_pre, self_memory_post:self_gdp_post) %>% 
  mutate(subjID = as.factor(subjID),
         gender = as.factor(gender)) %>% 
  filter(gender != "non-binary")

# Assuming your data frame is named self_data
long_self_data <- self_data %>% 
  pivot_longer(
    cols = starts_with("self_"), # Selects all columns that start with 'self_'
    names_to = c("modality", "time"), # Defines new variables for the extracted parts
    names_pattern = "self_(.+)_(.+)", # Correct pattern to match modality and time
    values_to = "value" # Name of the column to store the values
  ) %>%
  mutate(
    time = recode(time, 'pre' = 'Before', 'post' = 'After'), # Optionally, recode the 'time' values for clarity
    modality = recode(modality, 'memory' = 'Memory', 'visual' = 'Visual', 'calories' = 'Calories', 'gdp' = 'GDP') # Optionally, recode the 'modality' values for clarity
  )


# Fit the linear mixed-effects model
model <- lmer(value ~ time * modality + age + gender + years_edu + (1 | subjID), data = long_self_data)

# save results to table file in word

tab_model(model, file = "docs/controlmodel1.doc")






