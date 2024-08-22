#' Compare groups using survey-weighted data
#' @param data A data frame
#' @param dep_var A character string specifying the dependent variable
#' @param varlist A character vector specifying the independent variables
#' @param survey_object A survey object created using the `svydesign` function
#' @param output_type A character string specifying the type of output.
#' @param n_digits An integer specifying the number of digits to round to
#' @param percentage_type A character string specifying the type of percentage calculation. Can be "column" or "row"

compareGroups_wtd <- function(data, dep_var,
                              varlist, survey_object, 
                              output_type = "full",
                              n_digits = 2,
                              percentage_type = "column") {  
    # check if depvar is not numeric
    if (is.numeric(data[[dep_var]])) {
        stop("Dependent variable is numeric")
    }
    # Create an empty lists and dataframes
    df_table_one <- list()
    # initialize variable i for the loop
    i <- 0
    # identify the independent variables
    varlist_comp <- setdiff(varlist, dep_var)
    # Get the number of levels in dep_var
    num_levels <- length(levels(data[[dep_var]]))
    # Loop through the independent variables
    for (var in varlist_comp) {
        i <- i + 1
        if(!is.numeric(data[[var]])) {
            ## Step-1: Create formulas
            fmla_uni_cat <- as.formula(paste0("~", var))
            fmla_bi_cat <- as.formula(paste0("~", var, "+", dep_var))

            ## Step-2: Create Table for UNWTD Ns
            #### Univariate
            df_unwtd_N_uni <- table(data[[var]])
            #### Bivariate
            df_unwtd_N_cat <- table(data[[var]], data[[dep_var]]) |> as.data.frame.array()
            ##### Extract unwtd numerators for each level of dep_var
            # Get the total unwtd numerators
            numer_tot <- sum(!is.na(data[[var]]))
            ## Step-3: Create table for WTD Ns
            ### Univariate
            df_wtd_N_uni <- svytable(fmla_uni_cat, design = survey_object)
            ### Categorical
            df_wtd_N_cat <- svytable(fmla_bi_cat, design = survey_object)
            ## Step-4: Get wtd. proportions from Wtd Ns
            ### Univariate
            df_wtd_prop_uni <- df_wtd_N_uni |>
                prop.table() |>
                magrittr::multiply_by(100) |>
                round(digits = n_digits) |>
                as.data.frame.array() |>
                tibble::rownames_to_column(var = "levels") |>
                dplyr::mutate(N = df_unwtd_N_uni)
            colnames(df_wtd_prop_uni) <- c("levels", "Freq", "N")

            df_wtd_prop_uni <- df_wtd_prop_uni |>
                dplyr::mutate(N_prop_comb = paste0(N, " (", round(Freq, 2), " %)"))

            ### Bivariate
            df_wtd_prop_cat <-
                df_wtd_N_cat |>
                prop.table(if(percentage_type == "column") 2 else 1) |>  
                magrittr::multiply_by(100) |>
                round(digits = n_digits) |>
                as.data.frame.array() |>
                tibble::rownames_to_column(var = "levels") 

            df_wtd_prop_cat_comb <- df_unwtd_N_cat |>
                mutate(across(everything(), 
                       ~paste(.x, " (", df_wtd_prop_cat[[cur_column()]], " %)", sep = "")))

            ### Step-5: Get chi-sq values
            chi <- chisq.test(df_wtd_N_cat)
            chi_p <- round(chi$p.value, 2)
            chi_stat <- round(chi$statistic, 2)

            ### Step-6: Create the necessary vectors
            #### Levels
            vec_levels <- as.vector(df_wtd_prop_uni[,1])
            #### Overall n and %
            vec_unwtd_n_uni <- as.vector(df_wtd_prop_uni[,4])
            ##### Total N
            num_rows <- nrow(df_wtd_prop_cat_comb)
            numer_tot_vec <- c(numer_tot, rep(NA, num_rows - 1))
            ##### vectors for chi-sq stat and p-value
            chi_p_vec <- c(chi_p, rep(NA, num_rows - 1))
            chi_stat_vec <- c(chi_stat, rep(NA, num_rows - 1))

            ### Step-7: Get it all together
            df_comb <- as.data.frame(cbind(vec_levels, vec_unwtd_n_uni, 
                df_wtd_prop_cat_comb, numer_tot_vec, chi_p_vec, chi_stat_vec))
            ## Combine the label and the table
            var_label <- c(Hmisc::label(data[[var]]), rep("", dim(df_comb)[1] - 1))
            df_comb <- cbind(var_label, df_comb)

        } else if (is.numeric(data[[var]])) {
			    ## Step-1: create formulas
				    fmla_var <- as.formula(paste0("~", var))
				    fmla_depvar <- as.formula(paste0("~", dep_var))
				    fmla_signi <- as.formula(paste0(var, "~", dep_var))
			    ## Step-2: Get mean and SD for the independent variable (Overall)
				    tab_uni <- survey::svymean(fmla_var,
					    design = survey_object,
					    na.rm = TRUE) |> 
					    as.data.frame()
				    mean_uni <- tab_uni[1]
		                    var_uni <- svyvar(fmla_var, design = survey_object)
				    samp_size_uni <- sum(!is.na(data[[var]]))
				    sd_uni <- sqrt(var_uni)
			    ## Step-3: Get stats for crosstab
			    ### 3.1: Create the table
				    tab_bi <- survey::svyby(fmla_var, fmla_depvar,
						    design = survey_object,
						    svymean,
						    vartype = "var",
						    na.rm = TRUE) |>
						    dplyr::mutate(sd = sqrt(var)) |>
						    as.data.frame()
			    ### 3.2: Get the mean and SD for the two categories
				    mean_cat <- tab_bi[,2]
				    sd_cat <- tab_bi[,4]
			    ### Step-3.2: Get p-values from t-test
				    glm_fit <- survey::svyglm(fmla_signi, design = survey_object)
				    glm_out <- lmtest::waldtest(glm_fit)
				    glm_p <- glm_out[2,4]
				    glm_F_stat <- glm_out[2,3]
			    ## Step-4: Create the necessary vectors
				    var_label <- Hmisc::label(data[[var]])
			    ### Overall
				    overall_vec <- paste0(round(mean_uni, 2), " (", round(sd_uni, 2), ")")
			    ### Crosstabs
				    mean_vec <- as.vector(t(round(mean_cat, 2)))
				    sd_vec <- as.vector(t(round(sd_cat, 2)))
				    merged_vec <- t(paste0(mean_vec, " (", sd_vec, ")"))
				    # Blank column
				    blank_vec <- "Mean(Std. dev.)"
				    ## paste elements of the         
				    ### p-values and stat
				    glm_p_vec <- round(glm_p, 3)
				    glm_F_stat_vec <- round(glm_F_stat, 3)
			    ### Step-6: Get it all together
			    df_comb <- as.data.frame(cbind(var_label, blank_vec, overall_vec, merged_vec,
					    samp_size_uni, glm_p_vec, glm_F_stat_vec))            
        }
        ## append this to the list
        df_table_one[[i]] <- df_comb                        
    }
    ## Generate Output
    df_table_one_full <- data.table::rbindlist(df_table_one)
    # Rename variables
    var_levels <- levels(as.factor(data[[dep_var]]))
    colnames(df_table_one_full) <- c("Variable", "Levels_or_Mean_SD", "Overall_Prop_or_Mean",
                    var_levels, "N", "p-value", "chisq_or_Fstat")
    # Choose output type
    if (output_type == "full") {
        return(df_table_one_full)
    } else if (output_type == "univariate") {
        df_table_one_full <- df_table_one_full[,1:3]
    }
    return(df_table_one_full)
}