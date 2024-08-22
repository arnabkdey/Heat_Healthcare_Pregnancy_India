# title: "Merge climate zones into IR dataset"
# Load packages ----- 
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here, googledrive)
# rm(list = ls())
source("paths-mac.R")

# Read datasets ----- 
### IR vars created datasets -----
df_IR_full <- read_fst(here(path_project, "processed-data", "1.1-dhs-IR-vars-created-full.fst"), as.data.table = TRUE)

## Climate zones of India with districts ----
df_zones <- read_fst(here(path_project, "processed-data", "1.3-india-dist-climate-zones.fst"), as.data.table = TRUE)

# Merge climate zones into IR dataset  ----
## Convert all variable labels to lower case in df_zones and match state/dist names
df_zones <- df_zones |>
  dplyr::mutate(state_name = tolower(OTHREGNA),
    dist_name = tolower(REGNAME)) |>
  dplyr::mutate(state_name = ifelse(state_name == "dadra & nagar haveli & daman & diu", 
    "dadra & nagar haveli and daman & diu", state_name)) |>
  dplyr::mutate(dist_name = case_when(
    dist_name == "maharajganj" ~ "mahrajganj",
    dist_name == "buxer" ~ "buxar",
    dist_name == "north & middle andaman" ~ "north  & middle andaman",
    dist_name == "janjgir-champa" ~ "janjgir - champa",
    dist_name == "leh" ~ "leh(ladakh)",
    dist_name == "north district" ~ "north  district",
    dist_name == "sant ravidas nagar" ~ "sant ravidas nagar (bhadohi)",
    TRUE ~ dist_name))

## Merge df_zones with df_IR_full -----
df_IR_full_w_zones <- merge(df_IR_full, df_zones, 
                    by.x = c("meta_state_name", "meta_dist_name"), 
                    by.y = c("state_name", "dist_name"), 
                    all.x = TRUE)

colnames(df_IR_full_w_zones)
# sum(is.na(df_IR_full_w_zones$climate_zone))

# Save the dataset as an RDS ----
saveRDS(df_IR_full_w_zones, file = here(path_project, "processed-data", "1.4-processed-IR-data.rds"))
print("saving complete")

