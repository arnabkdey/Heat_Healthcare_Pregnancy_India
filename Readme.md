# Impact of Extreme Heat on postnatal care checkups for newborns and mothers in India

This repository contains the scripts used to study the effect of extreme heat on postnatal care checkups in India. The following sections describe the data sources used in the study and the scripts needed to replicate the analysis. 

# Data Sources
Raw data for Wet Bulb Globe Temperature (WBGT) and Precipitation can be obtained directly using the links below. Data on place of delivery, date of birth, and other individual level socio-economic and demographic variables can be obtained from the Demographic and Health Survey (DHS) website by creating an account and submitting an application to use the data.

* WBGT-Max: https://zenodo.org/records/8021197
* DHS dataset (https://www.dhsprogram.com/data/available-datasets.cfm)

# Scripts used in the study
All scripts to process and analyze data for this project are contained in the '1-scripts' folder. The following sections provide a brief description of the scripts:

## 1. Scripts to read and process data
### [1-scripts/1.1-process-DHS-IR-create-vars](1-scripts/1.1-process-DHS-IR-create-vars.R)
This script performs several steps to prepare and process the DHS data for analysis, focusing on creating variables for the Individual Recode dataset. It first identifies and selects relevant variables from different domains, such as meta, birth history, institutional delivery, socioeconomic status, and access-related variables. It then reads the raw data file and converts it into a data table format. It filters the data to include only usual residents, women who had a birth in the last five years, and those with caste information. The data is reshaped from wide to long format to get all births, and various new variables are created, including unique identifiers, birth details, institutional delivery indicators, religion, caste classifications, and maternal age-related variables. The final steps include converting specific variables to factors and saving the processed data to a file for future use. 

## [1-scripts/1.2-process-DHS-spatial-read-datasets](1-scripts/1.2-process-DHS-spatial-read-datasets.R)
This script processes spatial datasets by first reading geo-coded PSU data from a DHS India shapefile, filtering out invalid geocodes, and selecting relevant variables such as PSU, district name, latitude, and longitude. Next, it retrieves the administrative boundaries for India, adds a 50 km buffer around the boundary, and ensures necessary directories exist for saving output files. Finally, the processed geo-coded PSU data and buffered India boundary are saved as RDS files for further use.

## [1-scripts/1.3-process-clim-extract-wbgt-precip-data-to-psus](1-scripts/1.3-process-clim-extract-wbgt-precip-data-to-psus.R)
This script links daily temperature data with the geocoded PSU data from the DHS. It reads geocoded PSU data and extracts daily gridded climate data, creating a large dataset where each PSU has one row per day for the number of years of climate data available. The script first points to folders containing raw climate data and loads necessary datasets, including geocoded PSU data and India's administrative boundaries. It then sources a custom function to extract climate data for each PSU and applies this function to obtain maximum temperature and wet-bulb temperature, as well as precipitation data. These datasets are saved and then merged, resulting in a combined dataset that includes daily maximum temperature, wet-bulb temperature, and precipitation for each PSU. Finally, missing precipitation values are removed, and the cleaned dataset is saved for further analysis.

## [1-scripts/1.4-process-clim-create-vars-wbgt-long-term](1-scripts/1.4-process-clim-create-vars-wbgt-long-term.R)
This script, titled "Create temperature related variables specific to PSU and week/month," processes a large climate dataset to generate temperature-related variables based on long-term trends. The script begins by loading the necessary functions and datasets, including daily temperature and precipitation data for each PSU. It retains only complete cases and prepares the data by creating basic date-related variables.

Using harmonic quantile regression, the script creates cutoff variables for the 90th, 95th, and 97th percentiles of maximum wet-bulb temperature. Additionally, it calculates these cutoffs based on the day of the year. Long-term means and medians for maximum wet-bulb temperature are also computed by PSU and day of the year.

The processed data is saved to a file, capturing temperature extremes specific to both space and time, facilitating further analysis of climate impacts at the local level.

## [1-scripts/1.5-process-clim-wbgt-vars-for-paper](1-scripts/1.5-process-clim-wbgt-vars-for-paper.R)
This script processes climate data to generate temperature-related variables specific to PSUs (Primary Sampling Units) and time (week/month). The script begins by loading necessary packages and reading in a processed dataset of daily climate variables. It filters the data to include only cases from 2014 onwards. It then creates tercile categories based on long-term mean and median temperatures.

Next, the script identifies extreme heat days using absolute temperature cutoffs (30, 31, and 32 degrees Celsius) and calculates the number of consecutive extreme heat days. It creates heatwave variables for 2, 3, and 5 consecutive days of extreme heat for each temperature cutoff.

The script further identifies extreme heat days using percentile-based cutoffs (90th, 95th, and 97th percentiles) based on the day of the year and harmonic quantile regression approaches. It similarly calculates consecutive extreme heat days and creates heatwave variables for these percentile cutoffs.

Finally, the script saves the processed dataset, which includes all the newly created temperature-related variables, for further analysis.

## [1-scripts/1.6-process-merge-IR-clim-datasets](1-scripts/1.6-process-merge-DHS-clim-datasets.R)
This script combines temperature-related variables with IR (Individual Recode) data. The script begins by loading necessary datasets, including the IR data and the climate data with temperature variables. It removes the `week_of_year` variable from the IR data as it has already been created in a previous step.

The script then merges the IR and temperature datasets based on PSU and date of birth. It assigns descriptive labels to the variables related to absolute temperature cutoffs, day-of-year percentiles, and harmonic percentiles. After centering the mean precipitation variable, the script saves the final processed dataset as an RDS file.

# 2. Scripts used to run models

## [1-scripts/2.1-models-run-no-interaction](1-scripts/2.1-models-run-no-interaction.R)
This script processes a large dataset to run multiple generalized linear mixed models (GLMMs) in parallel. It specifies a combination of potential confounders and various exposure variables related to weather conditions. The models aim to assess the impact of these exposures on two binary outcomes, `dv_no_pnc_b` and `dv_no_pnc_m`.

The script begins by loading necessary packages and datasets. It then defines lists of covariates and exposure variables. The GLMMs are run in parallel, using the `foreach` package to iterate over each exposure variable.

Helper functions are defined to construct and run the models. The script registers a parallel backend to utilize multiple cores, subtracting four cores to ensure system stability. Each exposure variable is modeled with the specified covariates and a random intercept for `psu_fac`.

The outputs of all models are saved as an RDS object for further analysis. The entire process is designed to be efficient, leveraging parallel processing to handle the computational load.

## [1-scripts/2.2-output-extract-coefs-no-interaction](1-scripts/2.2-output-extract-coefs-no-interaction.R)
This script processes model outputs to generate and save tidy summaries, consolidate coefficients, and extract performance metrics such as ICC and R-squared values.
The script begins by creating a directory for the output if it doesn't already exist and then loads the previously saved model outputs. It extracts tidy outputs for each model using broom.mixed::tidy(), exponentiates the coefficients, and calculates confidence intervals. Next, the tidy outputs are saved to separate sheets in an Excel workbook. The script consolidates the primary exposure coefficients from each model into a single CSV file. Finally, the script calculates various performance metrics, including adjusted and unadjusted ICC, conditional and marginal R-squared values, AIC, and BIC, for each model. These metrics are compiled into a data frame and saved as a CSV file.

## [1-scripts/2.3-models-run-all-interactions](1-scripts/2.3-models-run-all-interactions.R)
This script processes the merged dataset generated in `script 1.6` to run multiple generalized linear mixed models (GLMMs) with interaction terms. It begins by loading necessary libraries and the final dataset. 

The script specifies lists of potential covariates, interaction terms, and exposure variables related to weather conditions. It defines the outcome variable and generates a list of formulas for each combination of exposure variable and interaction term, incorporating the specified covariates and a random intercept for `psu_fac`.

Using parallel processing, the script runs the models in parallel across multiple cores. Each model is stored in a list with appropriately shortened names. Finally, the models are saved as an RDS object for further analysis, ensuring efficient handling of the computational load through parallelization.

## [1-scripts/2.4-models-extract-coefs-all-interactions](1-scripts/2.4-models-extract-coefs-all-interactions.R)
This script processes interaction models to extract and save contrast estimates. It begins by loading necessary libraries and the previously saved interaction models. A directory for output files is created if it doesn't already exist. The script defines interaction terms and identifies the corresponding models. For each interaction term (rural, caste, religion, wealth, long-term maximum temperature mean, and access to health), the script constructs contrasts and uses the `glht` function from the `multcomp` package to generate generalized linear hypotheses.
For each interaction term, the script loops through the relevant models, extracts the contrast estimates, and saves the results into separate Excel sheets. This process includes creating specific contrasts for each interaction term, calculating the confidence intervals, exponentiating the estimates, and saving the tidy outputs. The results are organized into Excel files, each containing the contrast estimates for the respective interaction terms.

# 3. Scripts used to generate outputs 
## [1-scripts/3.1-output-table1-descr](1-scripts/3.1-output-table1-descr.R)
This code generates the descriptive tables used in the paper. It first loads necessary libraries, datasets, and custom functions for data analysis. It sets up directories for output files and creates lists of variables for socio-economic status and various exposure metrics. The dataset is then processed to convert specified variables into factors. A survey design object is created to handle weighted data. The code generates descriptive tables, including a main table (table1) comparing socio-economic variables, and additional tables (table2a, table2b, table2c) based on different exposure metrics, interleaving blank rows for clarity. Finally, it saves these tables as Excel files and calculates denominators for specified variables.

## [1-scripts/3.2-output-fig1-descr](1-scripts/3.2-output-fig1-descr.R)
This R script creates two different visualizations of data related to postnatal checkups for newborns and mothers. It produces a grouped bar chart showing column percentages across different subgroups, including overall sample data, and a set of two vertical bar charts (one for newborns, one for mothers) displaying row percentages for various categories. The script uses the tidyverse and patchwork libraries to manipulate data and create plots. It first prepares the data by reshaping it from wide to long format. For the row percentage plots, it defines a custom plotting function to create consistent charts for both newborns and mothers. The charts are then combined vertically and saved as high-resolution JPEG files. The visualizations include features like color-coded bars, percentage labels, and faceted layouts to clearly present the data across different demographic categories. The code also includes various theme customizations to improve the aesthetics and readability of the plots.

## [1-scripts/3.3-outputs-fig-full-models](1-scripts/3.3-output-fig-full-models.R)
This R script processes and visualizes data related to postnatal checkups and heat exposure. It begins by setting up the environment, loading necessary libraries, and importing data from a CSV file. The script then prepares labels for heat duration and temperature thresholds, adding these to the dataset. It subsets the data for two dependent variables: no postnatal checkup for babies and mothers. The core of the script is a loop that generates and saves plots for each dependent variable, creating separate visualizations for absolute temperature and percentile-based thresholds. These plots are created using a custom function (func_plot_full_model) and are saved as SVG files. The script includes error handling to manage potential issues during file saving. Overall, it automates the process of creating multiple visualizations from a complex dataset, focusing on the relationship between heat exposure and postnatal care.

## [1-scripts/3.4-outputs-fig-interactions](1-scripts/3.4-output-fig-interactions.R)
This script processes interaction data to create and save visual plots of contrast estimates. It starts by loading necessary libraries and setting up output directories. A custom function reads multiple sheets from Excel files, combines them, and adds appropriate labels for effect modifiers. The script reads and processes interaction data for various effect modifiers such as caste, religion, residence, wealth, and long-term mean temperature and access to healthcare. It filters and transforms the data, creating new variables for thresholds, types, and durations of temperature exposure. Labels for exposure variables are generated to facilitate understanding. The script combines all processed data and orders the levels of contrast variables. It then creates separate datasets for absolute and relative temperature thresholds, each based on the day of the year or harmonic analysis. The script uses these datasets to generate plots of the effect modifiers. Finally, the script saves the generated plots to a specified directory using a custom plotting function. The plots are saved in SVG format, ensuring high-quality visuals for further analysis and presentation.

# 6. Scripts containing functions used in the analysis
## [1-scripts/6.1-function-to-extract-climate-data-for-psus](1-scripts/6.1-function-to-extract-climate-data-for-psus.R)
The `merge_dhs_climate` function integrates climate data with DHS PSU geographic data by processing NetCDF files and extracting relevant climate information for each PSU location. It begins by loading necessary libraries (`terra`, `data.table`, `sf`) and retrieving a list of NetCDF files from the specified path. For each file, the function reads the raster, crops, and masks it to fit the specified region boundary. Climate data is then extracted for each PSU location, combined with PSU data (latitude, longitude, district name), and reshaped from wide to long format. The function also extracts start and end dates from the NetCDF file and generates a sequence of dates based on the specified time units, adding these dates to the data frame. Each processed data frame is added to a list. After processing all files, the list of data frames is combined into a single data frame using `rbindlist`. The final combined data frame, which includes climate data for each PSU location along with corresponding dates, is returned.

## [1-scripts/6.2-function-wtd-comparegroups](1-scripts/6.2-function-wtd-comparegroups.R)
The `compareGroups_wtd` function generates weighted comparisons between a dependent variable and a list of independent variables in a given dataset. It first checks if the dependent variable is numeric, stopping execution if it is. The function initializes lists and data frames for storing results and determines the number of levels in the dependent variable. It loops through each independent variable, constructing univariate and bivariate tables, and calculating both unweighted and weighted frequencies.

For categorical independent variables, the function calculates chi-square statistics and p-values, while for numeric variables, it computes means, standard deviations, and p-values from a t-test. The results, including variable labels, frequencies, proportions, means, standard deviations, and statistical tests, are combined into a comprehensive data frame. This data frame is either returned in full or truncated to only include univariate results, based on the specified output type.

## [1-scripts/6.3-function-to-plot-effect-modifiers](1-scripts/6.3-function-to-plot-effect-modifiers.R)
The `plot_effect_modifier` function generates a plot that visualizes the effect of an effect modifier on various outcomes. It takes a data frame with columns `exp_label`, `OR`, `CILow`, `CIHigh`, `contrast`, and `effect_modifier`, along with the column name and value of the effect modifier to filter the data. The function filters the data based on the provided effect modifier value, calculates the odds ratios (OR) and confidence intervals (CI), and determines the range for the CI values. It then creates a ggplot with facets for each `exp_label`, displaying the OR and CI for each `contrast`. The plot is styled using the Calibri font and classic theme, with adjustments for readability. The `plot_wrapper` function calls `plot_effect_modifier` for different effect modifiers (Caste, Religion, Residence, Wealth, and Long-term mean temperature tertiles) and arranges the resulting plots into a single composite plot using `ggarrange`, aligning them vertically with labels. This composite plot is then returned for further use or display.

## [1-scripts/6.4-function-for-lycday-and-harmonics](1-scripts/6.4-function-for-lycday-and-harmonics.R)
The `lycday` function calculates a modified day of the year that accounts for leap years. For dates in leap years, it returns the usual day of the year. For dates in non-leap years, it adjusts the day of the year by adding a cumulative sum based on the sequence of leap and non-leap years.

The `append_harmonics` function adds harmonic sine and cosine components to a data frame based on the modified day of the year calculated by the `lycday` function. It generates these harmonics for a specified number of harmonics (`n_harmonics`) and appends them as new columns to the data frame. These harmonics are useful for capturing seasonal variations in time series data. The sine and cosine components are calculated using the period of the year (365.25 days) and are named sequentially as `harmonic_s1`, `harmonic_s2`, etc., for sine components, and `harmonic_c1`, `harmonic_c2`, etc., for cosine components.

## [1-scripts/6.5-function-for-parallel-proc-quantile-reg](1-scripts/6.5-function-for-parallel-proc-quantile-reg.R)
The `parallel_quantile_regression` function performs quantile regression in parallel across multiple processors for each unique Primary Sampling Unit (PSU) in a data frame. It begins by ensuring the data frame is a data.table and setting up a parallel backend using multiple cores. The function then iterates over each unique PSU, subsets the data for the current PSU, and fits a quantile regression model using the specified formula and quantile.

The fitted values from the quantile regression are assigned back to the subset as a new column, named based on the provided variable name and quantile. Each modified subset is returned and combined into a single data frame. After completing the parallel processing, the function stops the parallel cluster and merges the results back into the main data frame. The final data frame, now containing the fitted values for the quantile regression across all PSUs, is returned.

# Results
## Effect Modification Analysis for WBGT_max >= 30 C
![Effect Modification Analysis for WBGT_max >= 30 C](3-outputs/figures/plot_df_plot_abs_30.svg)

## Effect Modification Analysis for WBGT_max >= 90th percentile
![Effect Modification Analysis for WBGT_max >= 90th percentile](3-outputs/figures/plot_df_plot_rel_doy_90.svg)
