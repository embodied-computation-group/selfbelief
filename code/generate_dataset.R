# create subject level dataset from self belief and confidence data
# clear environment
rm(list = ls())

library(tidyr)
library(dplyr)
library(rstatix)
library(tibble)

# load data ---------------------------------------------------------------
data_meta <- read.csv('./data/metacognition_TrialData_master.csv')
sb_pre <- read.table('./data/self_belief_pre_labels.csv', sep = ";", header = T) 
sb_post <- read.table('./data/self_belief_post_labels.csv', sep = ";", header = T)  
data_sub <- read.table('./data/pre_test_final.csv', sep = ";", header = T)  


# check duplicates in data
sum(duplicated(data_meta))
sum(duplicated(sb_pre))
sum(duplicated(sb_post))

# check duplicates in subject number
sum(table(data_meta$subject)!=600)
sum(duplicated(sb_pre$Sid))
sum(duplicated(sb_post$Sid))
# both self belief scores have duplicates, delete them
sb_pre <- sb_pre[-which(duplicated(sb_pre$Sid), arr.ind = FALSE, useNames = TRUE), ]
sb_post <- sb_post[-which(duplicated(sb_post$Sid), arr.ind = FALSE, useNames = TRUE),]


# Exclude trials less than 50 ms or MAD -----------------------------------
# too fast RT
data_meta <- filter(data_meta, rt>0.05)

# remove outliers
source('./code/remove_outliers.R')
data_meta$rt <- remove_outliers(data_meta$rt)
data_meta$rt_conf <- remove_outliers(data_meta$rt_conf)

# remove rows with NA
data_meta <- na.omit(data_meta)

# For T1 data -------------------------------------------------------------
# data wrangling
t1data <- data_meta[,c(1, 2, 6,8)]
colnames(t1data) <- c("subj", "mod", "acc", "rt")

# average the confidence scores over similar trials
t1l <- t1data %>%
  group_by(subj, mod)%>%
  summarise(mean(acc, na.rm = T), mean(rt, na.rm=T))
colnames(t1l) <- c("subj", "mod", "acc", "rt")

# put data in wide format
t1w <- t1l %>%
  pivot_wider(id_cols = subj, names_from = c(mod), values_from = c(acc, rt))
colnames(t1w) <- c("subj", "acc_cal", "acc_gdp", "acc_mem", "acc_vis", "rt_cal", "rt_gdp", "rt_mem", "rt_vis")
t1w <- t1w[,c(1, 4, 5, 3, 2, 8, 9, 7, 6)]

# calculate and add averages
t1w$avg_acc <- rowMeans(subset(t1w, select = c("acc_cal", "acc_gdp", "acc_mem", "acc_vis")))
t1w$avg_rt <- rowMeans(subset(t1w, select = c("rt_cal", "rt_gdp", "rt_mem", "rt_vis")))


# For self belief data ----------------------------------------------------
# select relevant data

# pre test data
pref <- as.data.frame(cbind(sb_pre$Sid, sb_pre$age, sb_pre$gender, sb_pre$self_memory_pre, sb_pre$self_visual_pre, sb_pre$self_gdp_pre, sb_pre$self_calories_pre))
colnames(pref) <- c("subj","age", "gender", "sb_pre_mem", "sb_pre_vis", "sb_pre_gdp", "sb_pre_cal")
pref[, c(1,2,4:7)] <- as.numeric(unlist(pref[, c(1,2,4:7)])) # because numbers were characters

# post test data
postf <- as.data.frame(cbind(sb_post$Sid, sb_post$self_memory_post, sb_post$self_visual_post, sb_post$self_gdp_post, sb_post$self_calories_post))
colnames(postf) <- c("subj", "sb_post_mem", "sb_post_vis", "sb_post_gdp", "sb_post_cal")
postf[, c(1:5)] <- as.numeric(unlist(postf[, c(1:5)])) # because numbers were integers

# merge the two sets
selfbel <- inner_join(pref, postf, by="subj")

# calculate and add additional measures
# average per modality
selfbel$mem_sb <- rowMeans(subset(selfbel, select = c("sb_pre_mem", "sb_post_mem")))
selfbel$vis_sb <- rowMeans(subset(selfbel, select = c("sb_pre_vis", "sb_post_vis")))
selfbel$gdp_sb <- rowMeans(subset(selfbel, select = c("sb_pre_gdp", "sb_post_gdp")))
selfbel$cal_sb <- rowMeans(subset(selfbel, select = c("sb_pre_cal", "sb_post_cal")))

# average per time
selfbel$sb_pre_avg <- rowMeans(subset(selfbel, select = c("sb_pre_mem", "sb_pre_vis", "sb_pre_gdp", "sb_pre_cal")))
selfbel$sb_post_avg <- rowMeans(subset(selfbel, select = c("sb_post_mem", "sb_post_vis", "sb_post_gdp", "sb_post_cal")))
selfbel$sb_diff_mem <- selfbel$sb_post_mem - selfbel$sb_pre_mem
selfbel$sb_diff_vis <- selfbel$sb_post_vis - selfbel$sb_pre_vis
selfbel$sb_diff_gdp <- selfbel$sb_post_gdp - selfbel$sb_pre_gdp
selfbel$sb_diff_cal <- selfbel$sb_post_cal - selfbel$sb_pre_cal
selfbel$sb_diff <- selfbel$sb_post_avg - selfbel$sb_pre_avg
selfbel$sb_avg <- rowMeans(subset(selfbel, select = c("sb_pre_avg", "sb_post_avg")))


# For trial confidence data -----------------------------------------------------------
# select relevant data
confdata <- data_meta[,c(1, 2, 6, 7)]
colnames(confdata) <- c("subj", "mod", "acc", "conf")

# average the confidence scores over similar trials
confw <- confdata %>%
  group_by(subj, mod, acc)%>%
  summarise(mean(conf, na.rm = T))
colnames(confw) <- c("subj", "mod", "acc", "conf")

# from long to wide format
conffinal <- confw %>%
  pivot_wider(id_cols = subj, names_from = c(mod, acc), values_from = conf)
colnames(conffinal) <- cbind("subj", "cal_incorrect", "cal_correct", "gdp_incorrect", "gdp_correct", "mem_incorrect", "mem_correct", "vis_incorrect", "vis_correct")
conffinal <- conffinal[,c(1, 7, 6, 9, 8, 5, 4, 3, 2)]

# add modality and accuracy averages --------------------------------------
# modality
conffinal$conf_mem <- rowMeans(subset(conffinal, select = c("mem_incorrect", "mem_correct")))
conffinal$conf_vis <- rowMeans(subset(conffinal, select = c("vis_incorrect", "vis_correct")))
conffinal$conf_gdp <- rowMeans(subset(conffinal, select = c("gdp_incorrect", "gdp_correct")))
conffinal$conf_cal <- rowMeans(subset(conffinal, select = c("cal_incorrect", "cal_correct")))

# hit minus miss
conffinal$conf_diff_mem <- conffinal$mem_correct - conffinal$mem_incorrect
conffinal$conf_diff_vis <- conffinal$vis_correct - conffinal$vis_incorrect
conffinal$conf_diff_gdp <- conffinal$gdp_correct - conffinal$gdp_incorrect
conffinal$conf_diff_cal <- conffinal$cal_correct - conffinal$cal_incorrect

# accuracy
conffinal$avg_incorrect <- rowMeans(subset(conffinal, select = c("mem_incorrect", "vis_incorrect", "gdp_incorrect", "cal_incorrect")))
conffinal$avg_correct <- rowMeans(subset(conffinal, select = c("mem_correct", "vis_correct", "gdp_correct", "cal_correct")))

# grand average
conffinal$avg_conf <- rowMeans(subset(conffinal[,10:13]))


# Combine the three sets ----------------------------------------------------
all_data <- inner_join(selfbel, t1w, by = "subj")
all_data <- inner_join(all_data, conffinal, by = "subj")

# add years education
yearsedu <- cbind(data_sub$sID, data_sub$YearsEdu.)
yearsedu <- as.data.frame(yearsedu)
colnames(yearsedu) <- c("subj", "years_edu")
# get rid of duplicate
n_occur <- data.frame(table(all_data$subj))
n_occur[n_occur$Freq > 1,]
yearsedu <- yearsedu[-64,]

# merge
all_data <- left_join(all_data, yearsedu, by = 'subj')
all_data <- all_data %>% relocate(years_edu, .after = gender)
  

# delete na's
all_data <- na.omit(all_data)
# delete outlier person
all_data <- filter(all_data, subj!=214)
# check duplicates
sum(duplicated(all_data$subj))
# no duplicates

# additional value calculations -------------------------------------------
result_data <- all_data %>%
  mutate(
    diff_acc_vis_pre = acc_vis*100 - sb_pre_vis,
    diff_acc_vis_post = acc_vis*100 - sb_post_vis,
    diff_acc_mem_pre = acc_mem*100 - sb_pre_mem,
    diff_acc_mem_post = acc_mem*100 - sb_post_mem,
    diff_acc_cal_pre = acc_cal*100 - sb_pre_cal,
    diff_acc_cal_post = acc_cal*100 - sb_post_cal,
    diff_acc_gdp_pre = acc_gdp*100 - sb_pre_gdp,
    diff_acc_gdp_post = acc_gdp*100 - sb_post_gdp,
    VisSBScoreDiff = diff_acc_vis_post - diff_acc_vis_pre,
    MemSBScoreDiff = diff_acc_mem_post - diff_acc_mem_pre,
    CalSBScoreDiff = diff_acc_cal_post - diff_acc_cal_pre,
    GDPSBScoreDiff = diff_acc_gdp_post - diff_acc_gdp_pre,
  )
colnames(result_data)[1] <- "subj"


# merge with Mratio values ------------------------------------------------
data_mratio <- read.csv('./data/mle_fit_data_wide.csv')
filter_mratio <- data_mratio[,c(2, 11:14, 23:26)]
colnames(filter_mratio)[1] <- "subj"

# join to other data
final_data <- left_join(result_data, filter_mratio, by = 'subj')

# final sanity checks
# delete na's
final_data <- na.omit(final_data)
# check duplicates
sum(duplicated(final_data$subj))
# no duplicates

# Save dataset -------------------------------------------------------------

# save dataset as csv
write.csv(final_data, file = "./data/SB_data.csv")



## fit partial correlation matrix


selected_vars <- final_data[,grepl("sb_pre_.*|sb_pre_.*|avg_conf_.*", names(final_data))]

selected_vars_complete_cases <- na.omit(selected_vars)

pcor_matrix <- pcor(selected_vars_complete_cases)$estimate


png("figures/partial_correlation_matrix.png", width = 1200, height = 1200)

# Plotting the partial correlation matrix
corrplot(pcor_matrix, method = "color", type = "full",
         addCoef.col = "black", tl.col = "black", tl.srt = 45, 
         cl.lim = c(-1, 1), diag = FALSE)
dev.off()


## lmer of demographic variables


self_data <- final_data %>% 
  dplyr::select(subj, age, gender, years_edu, sb_pre_mem:sb_pre_gdp, sb_post_mem:sb_post_gdp) %>% 
  filter(gender == c("Masculin","Feminin"))

# Assuming your data frame is named self_data
long_self_data <- self_data %>% 
  pivot_longer(
    cols = starts_with("sb_"), # Selects all columns that start with 'sb'
    names_to = c("time", "domain"), # Defines new variables for the extracted parts
    names_pattern = "sb_(.+)_(.+)", # Correct pattern to match modality and time
    values_to = "value" # Name of the column to store the values
  ) %>%
  mutate(
    time = recode(time, 'pre' = 'Before', 'post' = 'After'), # Optionally, recode the 'time' values for clarity
    domain = recode(domain, 'mem' = 'Memory', 'vis' = 'Visual', 'cal' = 'Calories', 'gdp' = 'GDP') # Optionally, recode the 'modality' values for clarity
  )


# Fit the linear mixed-effects model
model <- lmer(value ~ time * domain + age + gender + years_edu + (1 | subj), data = long_self_data)

# save results to table file in word

tab_model(model, file = "docs/controlmodel1.doc")
