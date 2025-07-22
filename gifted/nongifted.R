#traminer nongifted

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
non_gifted_ids <- gifted %>% 
  filter(aig == "N") %>%
  pull(mastid)


transcripts <- transcripts %>%
  arrange(mastid, grade)  # Replace 'course_term' with your actual time variable


# Then filter your transcripts to include only those students
pivoted_nongifted <- transcripts %>%
  filter(mastid %in% non_gifted_ids) %>%
  filter(!is.na(academic_level_desc), academic_level_desc != "") %>%
  group_by(mastid) %>%
  summarise(levels = list(academic_level_desc)) %>%
  ungroup() %>%
  filter(lengths(levels) >= 20)  # Keep only students with at least 20 classes

set.seed(123)  # for reproducibility
sampled_nongifted <- pivoted_nongifted |> #try and do all?
  sample_n(10000)

#write_csv(pivoted_nongifted, "Desktop/ncerdc_ml/ml-project-structure-demo/data/processed/nongifted_classes.csv")
#pivoted_nongifted <- read_csv("Desktop/ncerdc_ml/ml-project-structure-demo/data/processed/nongifted_classes.csv")

# Convert list-column to wide with one column per sequence position
seq_data_nongifted <- sampled_nongifted %>%
  mutate(id = row_number()) %>%
  tidyr::unnest_wider(levels, names_sep = "_") %>%
  select(-id)

seq_data_clean_nongifted <- seq_data_nongifted %>%
  mutate(across(everything(), ~ gsub("-", "_", .)))

seq_data_clean_nongifted <- seq_data_clean_nongifted |>
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

seq_obj_temp <- seqdef(seq_data_clean_nongifted)
# Get true state order in sequence object
state_order <- attr(seq_obj_temp, "alphabet")  # same as seqstatd(seq_obj)$State

# Reorder colors accordingly
colors_ordered <- color_map[state_order]

# Convert to sequence object
seq_obj_nongifted <- seqdef(seq_data_clean_nongifted, states = state_order, cpal = colors_ordered)



costmatrix <- seqsubm(seq_obj_nongifted, 
                      method = "TRATE", 
                      with.missing = TRUE, 
                      miss.cost = 0, 
                      transition = "both")
dist_om <- seqdist(seq_obj_nongifted, method = "OM", sm = costmatrix, with.missing = TRUE)
#trying omspell above
clusterward1 <- agnes(dist_om, diss = TRUE, method = "ward")
plot(clusterward1, which.plot = 2)


# === SET NUMBER OF CLUSTERS ===
k <- 2  # Change this to any value you want, e.g., 2, 4, 5...

# === 1. Clustering ===
cl_k <- cutree(clusterward1, k = k)
cl_k_fac <- factor(cl_k, labels = paste("Type", 1:k))

# === 2. Sequence Plot ===
seqplot(seq_obj_nongifted, group = cl_k_fac, type = "I",
        sortv = "from.start", with.legend = TRUE,
        border = NA, main = paste("Sequence Plot -", k, "Clusters"))

# === 3. Silhouette Analysis ===
sil_k <- silhouette(cl_k, dist_om)
plot(sil_k, main = paste("Silhouette Plot for", k, "Clusters"))
cat("Average Silhouette Width (k =", k, "):", round(mean(sil_k[, 3]), 3), "\n")


# === 5. MDS Visualization ===
mds_coords <- cmdscale(dist_om, k = 2)
mds_df <- data.frame(Dim1 = mds_coords[, 1],
                     Dim2 = mds_coords[, 2],
                     Cluster = cl_k_fac)

ggplot(mds_df, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 2.5) +
  theme_minimal() +
  labs(title = paste("MDS Plot of Sequence Distances (k =", k, ")"),
       color = "Cluster")

sampled_nongifted_with_clusters <- sampled_nongifted %>%
  mutate(cluster = cl_k_fac)

# Check output
head(sampled_nongifted_with_clusters)
