// Read dataset
use "Z:\Shared drives\Benmarhnia Lab\Arnab\project-datasets\dissertation\heat-and-complications-India\processed-data\2.3-df_hv_90_exp.dta"
cd "Z:\Shared drives\Benmarhnia Lab\Arnab\project-datasets\dissertation\heat-and-complications-India\outputs\models\stata-models"


// Process variables
//// Convert from character to numeric

// encode meta_rural, generate(meta_rural_num)
// encode meta_psu, generate(meta_psu_num)
encode meta_dist_name, generate(meta_dist_name_num)
encode meta_state_name, generate(meta_state_name_num)

// encode ses_religion_3, generate(ses_religion_3_num)
// encode ses_caste_3, generate(ses_caste_3_num)
// encode ses_wealth_bi, generate(ses_wealth_bi_num)
encode ses_access_issue_distance, generate(ses_access_issue_distance_num)


// Multinominal regression
//// Any hw contact
model_glmer_hv <- glmer(dv_met_flw_3mo ~ 
                        exp_bin_below_10_10 + 
                        exp_bin_10_15_10 + 
                        # exp_bin_15_20_10 + 
                        # exp_bin_20_25_10 + 
                        exp_bin_25_30_10 + 
                        exp_bin_above_30_10 +
                          ses_wealth_bi +
                          ses_caste_3 +
                          ses_religion_3 +
                          ses_access_issue_distance + 
                          mat_edu_level +
                          mat_age_at_int_scaled +
                          mat_cur_preg +
                          month_int + 
                          (1 | meta_dist_name), 
                          data = df_hv, 
                        #   subset = ses_access_issue_distance == "not-a-big-prob",
                          family = binomial(link = "logit"))
						  
mepoisson dv_met_flw_3mo exp_bin_below_10_10 exp_bin_10_15_10 exp_bin_25_30_10 exp_bin_above_30_10  ///
        i.ses_caste_3 i.ses_religion_3 i.ses_wealth_bi i.ses_access_issue_distance_num  ///
		mat_age_at_int_scaled mat_cur_preg mat_edu_level ///
        i.month_int || meta_dist_name_num:, vce(robust) irr
		
outreg2 using anc_pc_full.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(iv_pc_full_bin1 iv_pc_full_bin2 iv_pc_full_bin4 iv_pc_full_bin5) 
    