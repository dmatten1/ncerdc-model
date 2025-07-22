library(TraMineR)
library(TraMineRextras)
library(cluster)
library(WeightedCluster)
library(FactoMineR)
library(ade4)
library(RColorBrewer)
library(questionr)
library(descriptio)
library(dplyr)
library(purrr)
library(ggplot2)
library(seqhandbook)
library(tidyverse)

transcripts <- read.csv("../data/transcripts_master.csv")
gifted <- read.csv("../data/masterbuild_master.csv")


# First, create a list of non-gifted mastids from the `gifted` dataframe
non_gifted_ids <- gifted %>% #now gifted
  filter(aig == "N") %>%
  pull(mastid)

# Then filter your transcripts to include only those students
pivoted <- transcripts %>%
  filter(mastid %in% non_gifted_ids) %>%
  filter(!is.na(academic_level_desc), academic_level_desc != "") %>%
  group_by(mastid) %>%
  summarise(levels = list(academic_level_desc)) %>%
  ungroup() %>%
  filter(lengths(levels) >= 20)  # Keep only students with at least 20 classes








#pivoted_filtered <- pivoted |>
 # filter(mastid %in% gifted$mastid)

############traminer
set.seed(123)  # for reproducibility
sampled <- pivoted %>% sample_n(1000)


# Convert list-column to wide with one column per sequence position
seq_data <- sampled %>%
  mutate(id = row_number()) %>%
  tidyr::unnest_wider(levels, names_sep = "_") %>%
  select(-id)

seq_data_clean <- seq_data %>%
  mutate(across(everything(), ~ gsub("-", "_", .)))

seq_data_clean <- seq_data_clean |>
  select(-mastid)

states <- c(
  "Advanced Placement",
  "Co-op Education",
  "Honors/Advanced/Academically Gifted",
  "International Baccalaureate",
  "Non-Classroom Activity",
  "Standard Version",
  "Modified Curriculum",
  "Abridged/Adapted (Remedial)"
)

# 8 corresponding colors
colors <- c("red", "blue", "green", "orange", "purple", "yellow", "brown", "pink")

color_map <- setNames(colors, states)
transcripts$academic_level_color <- color_map[transcripts$academic_level_desc]

seq_obj_temp <- seqdef(seq_data_clean)
# Get true state order in sequence object
state_order <- attr(seq_obj_temp, "alphabet")  # same as seqstatd(seq_obj)$State

# Reorder colors accordingly
colors_ordered <- color_map[state_order]

# Convert to sequence object
seq_obj <- seqdef(seq_data_clean, states = state_order, cpal = colors_ordered)

# Example: frequency plot of states
seqIplot(seq_obj)

# Or compute distances, clusters, etc.



#CLUSTERING

costmatrix <- seqsubm(seq_obj, 
                      method = "TRATE", 
                      with.missing = TRUE, 
                      miss.cost = 0, 
                      transition = "both")
dist_omspell <- seqdist(seq_obj, method = "OMspell", sm = costmatrix, with.missing = TRUE)
#trying omspell above
clusterward1 <- agnes(dist_omspell, diss = TRUE, method = "ward")
plot(clusterward1, which.plot = 2)
#i chose 3
cl3 <- cutree(clusterward1, k = 3) 

#turning cut points into a factor variable and labeling them
cl3fac <- factor(cl3, labels = paste("Type", 1:3)) 

#plot
seqplot(seq_obj, group = cl3fac, type="I", sortv = "from.start",with.legend = TRUE, border = NA)

###########################################

sampled$cl3 <- cl3

gifted_sample <- sampled %>%
  select(mastid, cl3) %>%
  left_join(gifted, by = "mastid")  # brings in 'eds', 'sex', etc.


# Define the Big Five traits
traits <- c("eds")

gifted_sample <- gifted_sample %>%
  mutate(
    eds = ifelse(eds == "Y", 1,
                 ifelse(eds == "N", 0, NA))  # NA if value is not Y or N
  )
gifted_sample <- gifted_sample %>%
  mutate(sex = factor(sex, levels = c("M", "F")))

# Loop through traits and run ANOVA and TukeyHSD
for (trait in traits) {
  cat("\n============================\n")
  cat("Trait:", trait, "\n")
  cat("============================\n")
  
  # Build formula dynamically
  formula <- as.formula(paste(trait, "~ factor(cl3)"))
  
  # Run ANOVA
  aov_result <- aov(formula, data = gifted_sample)
  print(summary(aov_result))
  
  # Post-hoc test
  print(TukeyHSD(aov_result))
}

# Chi-squared test: association between sex and cluster
chisq_result <- chisq.test(table(gifted_sample$sex, gifted_sample$cl3))

# View the result
chisq_result
table(gifted_sample$sex, gifted_sample$cl3)
#Save

install.packages(c("gridExtra", "ggplot2", "knitr"))
library(gridExtra)
library(ggplot2)
library(knitr)
library(grid)

pdf("cluster_stats_summary.pdf", width = 11, height = 8.5)

# ----- 1. ANOVA Table -----
anova_table <- data.frame(
  Source = c("Cluster (cl3)", "Residuals"),
  Df = c(3, 9892),
  Sum_Sq = c(66.7, 1561.0),
  Mean_Sq = c(22.230, 0.158),
  F_value = c(140.9, NA),
  Pr_gt_F = c("< 2e-16 ***", "")
)

grid.text("ANOVA: EDS by Cluster", y = unit(0.95, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.table(anova_table)

# New page
grid.newpage()

# ----- 2. Tukey HSD Table -----
tukey_table <- data.frame(
  Comparison = c("2-1", "3-1", "4-1", "3-2", "4-2", "4-3"),
  Diff = c(-0.090, 0.029, 0.199, 0.119, 0.289, 0.170),
  LWR = c(-0.116, 0.002, 0.161, 0.093, 0.252, 0.131),
  UPR = c(-0.064, 0.057, 0.238, 0.146, 0.327, 0.209),
  p_adj = c("<.001", "0.031", "<.001", "<.001", "<.001", "<.001")
)

grid.text("Tukey HSD: EDS by Cluster", y = unit(0.95, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.table(tukey_table)

# New page
grid.newpage()

# ----- 3. Contingency Table: Sex by Cluster -----
contingency_matrix <- matrix(
  c(1260, 1486, 1457, 644,
    1613, 1898, 1250, 288),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(Sex = c("M", "F"), Cluster = c("1", "2", "3", "4"))
)

grid.text("Contingency Table: Sex by Cluster", y = unit(0.95, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.table(contingency_matrix)

# New page
grid.newpage()

# ----- 4. Chi-squared Result Summary -----
chi_sq_result <- data.frame(
  Statistic = 241.32,
  df = 3,
  p_value = "< 2.2e-16"
)

grid.text("Chi-squared Test: Sex vs. Cluster", y = unit(0.95, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.table(chi_sq_result)

dev.off()


seqdplot(seq_obj, with.legend=F) 
### cluster validation

sil <- silhouette(cl3, dist_omspell)
plot(sil)
mean(sil[, 3])  # Average silhouette width
