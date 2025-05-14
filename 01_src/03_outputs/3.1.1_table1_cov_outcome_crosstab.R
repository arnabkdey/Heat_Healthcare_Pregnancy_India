# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script generates survey-weighted descriptive statistics tables for the study population.
# @date: May, 2024

# Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, here, survey, gt)

# set paths ----
source(here("paths.R"))

# Load datasets ----
df_merged <- readRDS(here(path_processed, "1.3.3_cumulative_bins_all_datasets_5mo.rds"))
df_paper_final <- df_merged$tmax_wbgt # any of the datasets will work

# Load function for weighted tables ----
source(here("01_src", "03_outputs", "utils", "function_wtd_comparegroups.R"))

# Create list of variables
varlist_ses <- c("mat_edu_level", "mat_age_at_int_cat", "mat_parity_bi",
                 "mat_media_exp_any", "ses_caste_2", "ses_religion_2_hindu",
                 "ses_access_issue_distance", "hh_wealth_quintile_ru_og", 
                 "meta_rural")

# Create variable labels ----
## Variable names
var_names <- c(
  "mat_edu_level" = "Maternal education level",
  "mat_age_at_int_cat" = "Maternal age at interview",
  "mat_parity_bi" = "Maternal parity",
  "mat_media_exp_any" = "Maternal media exposure",
  "ses_caste_2" = "Caste",
  "ses_religion_2_hindu" = "Religion",
  "ses_access_issue_distance" = "Distance as a barrier to access healthcare",
  "hh_wealth_quintile_ru_og" = "Household wealth",
  "meta_rural" = "Urban/Rural residence"
)

## Variable level labels
var_labels <- list(
  mat_edu_level = c(
    "no education" = "No formal education",
    "primary" = "Primary education",
    "secondary" = "Secondary education",
    "higher" = "Higher education"
  ),
  mat_age_at_int_cat = c(
    "15-24" = "15-24 years",
    "25-34" = "25-34 years",
    "35-49" = "35-49 years"
  ),
  mat_parity_bi = c(
    "Primiparous" = "First birth",
    "Multiparous" = "Second or higher birth"
  ),
  mat_media_exp_any = c(
    "no" = "No media exposure",
    "yes" = "Media exposure"
  ),
  ses_caste_2 = c(
    "SC/ST/OBC" = "Scheduled Caste/Tribe/Other Backward Class",
    "General" = "General/Other castes"
  ),
  ses_religion_2_hindu = c(
    "Hindu" = "Hindu",
    "Not-Hindu" = "Other religions"
  ),
  ses_access_issue_distance = c(
    "big-problem" = "Distance is a big problem",
    "not-a-big-prob" = "Distance is not a big problem"
  ),
  hh_wealth_quintile_ru_og = c(
    "poorest" = "Poorest",
    "poorer" = "Poorer",
    "middle" = "Middle",
    "richer" = "Richer",
    "richest" = "Richest"
  ),
  meta_rural = c(
    "urban" = "Urban residence",
    "rural" = "Rural residence"
  )
)

# Convert list of variables to factor -----
df_paper_final <- df_paper_final |> 
  mutate(across(all_of(varlist_ses), as.factor)) 

# Create survey object
svy_object <- svydesign(ids = ~1,
                data = df_paper_final,
                weights = df_paper_final$wt_final)

# Column percentages for total FLW visits ----
df_paper_final$dv_no_contact_3mo <- as.factor(df_paper_final$dv_no_contact_3mo)

table_flw_tot_col <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_no_contact_3mo",
                            varlist = c(varlist_ses),
                            survey_object = svy_object,
                            output_type = "full", 
                            n_digits = 1,
                            percentage_type = "column")

# Create a lookup table for levels to variable names
level_to_var <- tibble::tibble(
  Levels_or_Mean_SD = c(
    "no education", "primary", "secondary", "higher",
    "15-24", "25-34", "35-49",
    "Primiparous", "Multiparous",
    "no", "yes",
    "SC/ST/OBC", "General",
    "Hindu", "Not-Hindu",
    "big-problem", "not-a-big-prob",
    "poorest", "poorer", "middle", "richer", "richest",
    "urban", "rural"
  ),
  Variable = c(
    rep("mat_edu_level", 4),
    rep("mat_age_at_int_cat", 3),
    rep("mat_parity_bi", 2),
    rep("mat_media_exp_any", 2),
    rep("ses_caste_2", 2),
    rep("ses_religion_2_hindu", 2),
    rep("ses_access_issue_distance", 2),
    rep("hh_wealth_quintile_ru_og", 5),
    rep("meta_rural", 2)
  )
)

# Join to assign Variable
table_flw_tot_col <- table_flw_tot_col %>%
  left_join(level_to_var, by = "Levels_or_Mean_SD") %>%
  mutate(Variable = Variable.y) %>%
  select(-Variable.x, -Variable.y)

# Create GT table
gt_table <- table_flw_tot_col |>
  gt::gt(
    groupname_col = "Variable"
  ) |>
  # Add title and subtitle
  # gt::tab_header(
  #   title = "",
  #   subtitle = ""
  # ) |>
  # Format column headers
  gt::cols_label(
    Levels_or_Mean_SD = "Characteristics",
    `Overall_Prop_or_Mean` = gt::html("Overall proportion<br>[N = 10606]"),
    `0` = gt::html("No healthcare utilization<br>[N = 1119]"),
    `1` = gt::html("Some healthcare utilization<br>[N = 9487]")
  ) |>
  # Hide columns we don't want to show
  gt::cols_hide(columns = c(N, `p-value`, chisq_or_Fstat)) |>
  # Rename the group labels (variable names)
  gt::text_transform(
    locations = gt::cells_row_groups(),
    fn = function(x) {
      # Vectorized mapping: replace all group names with their labels if available
      out <- var_names[as.character(x)]
      out[is.na(out)] <- x[is.na(out)]
      out
    }
  ) |>
  # Transform level labels
  gt::text_transform(
    locations = gt::cells_body(columns = Levels_or_Mean_SD),
    fn = function(x) {
      var_vec <- table_flw_tot_col$Variable[match(x, table_flw_tot_col$Levels_or_Mean_SD)]
      mapply(function(val, var) {
        if (!is.null(var_labels[[var]]) && val %in% names(var_labels[[var]])) {
          var_labels[[var]][[val]]
        } else {
          val
        }
      }, x, var_vec, USE.NAMES = FALSE)
    }
  ) |>
  # Add source notes
  gt::tab_source_note(
    source_note = "Note: All values represent survey-weighted percentages"
  ) |>
  # Style the table
  gt::tab_style(
    style = list(
      gt::cell_text(weight = "bold")
    ),
    locations = list(
      gt::cells_column_labels(),
      gt::cells_title(),
      gt::cells_row_groups()
    )
  ) |>
  # Style alternating rows
  gt::opt_row_striping() |>
  # Format the table with clean theme
  gt::opt_table_font(
    font = list(
      gt::google_font("Source Sans Pro")
    )
  ) |>
  # Add borders
  gt::tab_style(
    style = list(
      gt::cell_borders(
        sides = "bottom",
        color = "gray85",
        weight = gt::px(1)
      )
    ),
    locations = list(
      gt::cells_body()
    )
  ) |>
  # Style group rows
  gt::tab_style(
    style = list(
      gt::cell_fill(color = "gray95"),
      gt::cell_text(weight = "bold")
    ),
    locations = list(
      gt::cells_row_groups()
    )
  ) |>
  # Customize column alignment
  gt::cols_align(
    align = "left",
    columns = c(Levels_or_Mean_SD)
  ) |>
  gt::cols_align(
    align = "center",
    columns = c(`0`, `1`)
  ) |>
  # Add separators between variables
  gt::tab_style(
    style = list(
      gt::cell_borders(
        sides = c("bottom"),
        color = "black",
        weight = gt::px(2)
      )
    ),
    locations = list(
      gt::cells_body(
        rows = c(4, 7, 9, 11, 13, 15, 17, 22, 24)  # Add separators after each variable group
      )
    )
  )

# Save the table
gt::gtsave(gt_table, here(path_outputs, "tables", "table1_healthcare_utilization.html"))

# Get mean age of mothers
svyvar(~mat_age, design = svy_object, na.rm = T) |> as.data.frame() |> dplyr::mutate(sd = sqrt(mat_age))
svyby(~mat_age, ~dv_no_contact_3mo, 
        design = svy_object, 
        svymean, 
        na.rm = T, 
        vartype = "var") |>
    dplyr::mutate(sd = sqrt(var)) |>
    as.data.frame()

# Get weighted percentages for the outcome variables
svytable(~dv_no_contact_3mo, design = svy_object) |> prop.table() |> round(4) * 100
tabyl(df_paper_final, dv_no_contact_3mo)
nrow(df_paper_final)

unique(table_flw_tot_col$Levels_or_Mean_SD)
