pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)

# read new variables to be added ----
df0_add <- read.fst(here(path_processed_data, "7.1-dhs-4-5-BR-new-vars.fst"))

# Load the combined dataset from Anna ----
df1 <- read.fst(here(path_processed_data, "7.2_combined_df_anna_short.fst"), as.data.table = T)

# Change birth_id, clust, hhl_id, line_id to numeric
df1 <- df1[, birth_id := as.numeric(birth_id)][
        , clust := as.numeric(clust)][
        , hhl_id := as.numeric(hhl_id)][
        , line_id := as.numeric(line_id)]

nrow(df1)
colnames(df1)
summary(df0_add$line_id)

# Merge the datasets using data.table
df_merge <- df1[df0_add, on = c("birth_id", "clust", "hhl_id", "line_id")]


# Save the merged dataset
write.fst(df_merge, here(path_processed_data, "7.3-dhs-5-IR-merged.fst"))

