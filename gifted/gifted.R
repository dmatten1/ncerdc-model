#traminer gifted

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

transcripts <- read.csv("Desktop/ncerdc_ml/ml-project-structure-demo/data/processed/transcripts_master.csv")
gifted <- read.csv("Desktop/ncerdc_ml/ml-project-structure-demo/data/processed/masterbuild_master.csv")


gifted_ids <- gifted %>% 
  filter(aig != "N") %>%
  pull(mastid)

transcripts <- transcripts %>%
  arrange(mastid, grade)  # Replace 'course_term' with your actual time variable

pivoted_gifted <- transcripts %>%
  filter(mastid %in% gifted_ids) %>%
  filter(!is.na(academic_level_desc), academic_level_desc != "") %>%
  group_by(mastid) %>%
  summarise(levels = list(academic_level_desc)) %>%
  ungroup() %>%
  filter(lengths(levels) >= 20)  # Keep only students with at least 20 classes

set.seed(123)  # for reproducibility
sampled_gifted <- pivoted_gifted %>% sample_n(1000)


# Convert list-column to wide with one column per sequence position
seq_data_gifted <- sampled_gifted %>%
  mutate(id = row_number()) %>%
  tidyr::unnest_wider(levels, names_sep = "_") %>%
  select(-id)

seq_data_clean_gifted <- seq_data_gifted %>%
  mutate(across(everything(), ~ gsub("-", "_", .)))

seq_data_clean_gifted <- seq_data_clean_gifted |>
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

seq_obj_temp <- seqdef(seq_data_clean_gifted)
# Get true state order in sequence object
state_order <- attr(seq_obj_temp, "alphabet")  # same as seqstatd(seq_obj)$State

# Reorder colors accordingly
colors_ordered <- color_map[state_order]

# Convert to sequence object
seq_obj_gifted <- seqdef(seq_data_clean_gifted, states = state_order, cpal = colors_ordered)

# Example: frequency plot of states
seqIplot(seq_obj_gifted)



costmatrix <- seqsubm(seq_obj_gifted, 
                      method = "TRATE", 
                      with.missing = TRUE, 
                      miss.cost = 0, 
                      transition = "both")
state_names <- attr(seq_obj_gifted, "alphabet")

# Define the two states
state_ap <- "Advanced Placement"
state_ib <- "International Baccalaureate"


# Update cost matrix symmetrically
costmatrix[state_ap, state_ib] <- 0
costmatrix[state_ib, state_ap] <- 0


dist_om <- seqdist(seq_obj_gifted, method = "OM", sm = costmatrix, with.missing = TRUE)
#trying omspell above
clusterward1 <- agnes(dist_om, diss = TRUE, method = "ward")
plot(clusterward1, which.plot = 2)





# Store results
validation_results <- list()

for (k in 2:6) {
  cat("\n============================\n")
  cat("Clustering Validation for k =", k, "\n")
  cat("============================\n")
  
  # --- 1. Hierarchical clustering ---
  clusterward_k <- agnes(dist_om, diss = TRUE, method = "ward")
  cl_k <- cutree(clusterward_k, k = k)
  cl_k_fac <- factor(cl_k, labels = paste("Type", 1:k))
  seqplot(seq_obj_gifted, group = cl_k_fac, type = "I", sortv = "from.start",
          with.legend = TRUE, border = NA)
  
  # --- 2. Silhouette ---
  sil <- silhouette(cl_k, dist_om)
  avg_sil <- mean(sil[, 3])
  cat("Average Silhouette Width:", round(avg_sil, 3), "\n")
  
  # --- 3. Stability (using OMspell for demonstration) ---
  stab <- clusterboot(dist_om,
                      B = 100,
                      clustermethod = disthclustCBI,
                      method = "ward.D2",
                      k = k)
  jaccard <- stab$bootmean
  cat("Jaccard Bootstrap Mean Stability:\n")
  print(round(jaccard, 3))
  
  # --- 4. MDS Visualization ---
  mds_coords <- cmdscale(dist_om, k = 2)
  mds_df <- data.frame(Dim1 = mds_coords[, 1],
                       Dim2 = mds_coords[, 2],
                       Cluster = cl_k_fac)
  
  p <- ggplot(mds_df, aes(x = Dim1, y = Dim2, color = Cluster)) +
    geom_point(alpha = 0.6, size = 2.5) +
    theme_minimal() +
    labs(title = paste("MDS Plot (k =", k, ")"), color = "Cluster")
  
  print(p)
  
  # --- 5. Store for summary ---
  validation_results[[paste0("k_", k)]] <- list(
    clusters = cl_k_fac,
    silhouette = avg_sil,
    jaccard = jaccard,
    mds_plot = p
  )
}

