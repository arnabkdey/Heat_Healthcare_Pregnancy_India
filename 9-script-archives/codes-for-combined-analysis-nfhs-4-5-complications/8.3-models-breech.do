// Read dataset
use "Z:\Shared drives\Benmarhnia Lab\Arnab\project-datasets\manuscripts\heat-and-complications-India\processed-data\7.4-dhs-5-IR-merged-breech.dta", clear


cd "D:\Arnab\git\manuscripts\BEL-complications-paper\3-outputs\models\stata-models"
// Process variables
//// Convert from character to numeric
encode clim_zone, generate(clim_zone_num)
encode region, generate(region_num)
encode ses_season_birth, generate(season_birth_num)
encode ses_religion_tri, generate(ses_religion_tri_num)
encode ses_caste_tri, generate(ses_caste_tri_num)
encode ses_edu, generate(ses_edu_num)
encode ses_wealth_bi, generate(ses_wealth_bi_num)
encode ses_residence, generate(ses_residence_num)
encode psu, generate(psu_num)

//// Standardize Fertility Age
egen ses_fertAge_std = std(ses_fertAge)

// Multinominal regression
/// Percentile
//// Trimester-1
mepoisson dv_comp_breech_bi iv_pc_t1_bin1 iv_pc_t1_bin2 iv_pc_t1_bin4 iv_pc_t1_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num || psu_num:, vce(robust) irr
	
outreg2 using breech_pc_t1.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(iv_pc_t1_bin1 iv_pc_t1_bin2 iv_pc_t1_bin4 iv_pc_t1_bin5) 
	
//// Trimester-2
mepoisson dv_comp_breech_bi iv_pc_t2_bin1 iv_pc_t2_bin2 iv_pc_t2_bin4 iv_pc_t2_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num || psu_num:, vce(robust) irr

outreg2 using breech_pc_t2.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(iv_pc_t2_bin1 iv_pc_t2_bin2 iv_pc_t2_bin4 iv_pc_t2_bin5) 
	
//// Trimester-3
mepoisson dv_comp_breech_bi iv_pc_t3_bin1 iv_pc_t3_bin2 iv_pc_t3_bin4 iv_pc_t3_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num || psu_num:, vce(robust) irr

outreg2 using breech_pc_t3.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(iv_pc_t3_bin1 iv_pc_t3_bin2 iv_pc_t3_bin4 iv_pc_t3_bin5) 


////// Absolute 
//// Trimester-1
mepoisson dv_comp_breech_bi iv_abs_t1_bin1 iv_abs_t1_bin2 iv_abs_t1_bin4 iv_abs_t1_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num || psu_num:, vce(robust) irr

outreg2 using breech_abs_t1.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(iv_abs_t1_bin1 iv_abs_t1_bin2 iv_abs_t1_bin4 iv_abs_t1_bin5) 


//// Trimester-2
mepoisson dv_comp_breech_bi iv_abs_t2_bin1 iv_abs_t2_bin2 iv_abs_t2_bin4 iv_abs_t2_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num || psu_num:, vce(robust) irr

outreg2 using breech_abs_t2.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(iv_abs_t2_bin1 iv_abs_t2_bin2 iv_abs_t2_bin4 iv_abs_t2_bin5) 

	
//// Trimester-3
mepoisson dv_comp_breech_bi iv_abs_t3_bin1 iv_abs_t3_bin2 iv_abs_t3_bin4 iv_abs_t3_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num || psu_num:, vce(robust) irr

outreg2 using breech_abs_t3.xls, replace excel dec(3) eform sideway stat(coef ci) level(97) ///
    keep(iv_abs_t3_bin1 iv_abs_t3_bin2 iv_abs_t3_bin4 iv_abs_t3_bin5) 
