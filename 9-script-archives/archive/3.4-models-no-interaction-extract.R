pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here, googledrive)
source(here(".Rprofile"))

# Read all models ----
list_models <- readRDS(file = here(path_project, "processed-data", "3.3-list-of-models.RDS"))

View(broom.mixed::tidy(list_models[[4]], effects = "fixed", conf.int = TRUE) |> 
        filter(str_detect(term, "^exp_")) |> 
        select(term, estimate, p.value, conf.low, conf.high))

# Function to extract coefficients ----
extract_coefficients <- function(model) {
    model %>% 
        broom.mixed::tidy(effects = "fixed", conf.int = TRUE) %>% 
        filter(str_detect(term, "^exp_")) %>% 
        select(term, estimate, p.value, conf.low, conf.high)
}

# Extract coefficients for each model in list_models ----
extract_fixed_effects(list_models[["m_anc90_bin"]])
extract_fixed_effects("m_anc90_bin")
View(broom.mixed::tidy(m_anc90_bin, exp = TRUE) |> dplyr::filter(str_detect(term, "exp_")) |> dplyr::select(term, estimate, std.error, statistic, p.value))
summary(m_anc90_bin)


ls()
df_anc90 <- df_anc90 |> mutate(year = year(dob))
tabyl(df_anc90, year)
