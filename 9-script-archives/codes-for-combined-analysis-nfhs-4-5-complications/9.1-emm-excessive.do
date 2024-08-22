// Read dataset
use "Z:\Shared drives\Benmarhnia Lab\Arnab\project-datasets\manuscripts\heat-and-complications-India\processed-data\7.4-dhs-5-IR-merged-excessive.dta", clear


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
//// Trimester-1
mepoisson dv_comp_excessive_bi iv_abs_t1_bin1 iv_abs_t1_bin2 iv_abs_t1_bin4 iv_abs_t1_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num || psu_num:, vce(robust) irr

outreg2 using excessive_abs_t1.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(iv_abs_t1_bin1 iv_abs_t1_bin2 iv_abs_t1_bin4 iv_abs_t1_bin5) 


//// Trimester-2
mepoisson dv_comp_excessive_bi iv_abs_t2_bin1 iv_abs_t2_bin2 iv_abs_t2_bin4 iv_abs_t2_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num || psu_num:, vce(robust) irr

outreg2 using excessive_abs_t2.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(iv_abs_t2_bin1 iv_abs_t2_bin2 iv_abs_t2_bin4 iv_abs_t2_bin5) 

	
//// Trimester-3
////// Wealth = Rich
poisson dv_comp_excessive_bi iv_abs_t3_bin1 iv_abs_t3_bin2 iv_abs_t3_bin4 iv_abs_t3_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num#i.region_num if clim_zone == "Cwa: Temperate, dry winter, hot summer", vce(robust) irr
		
estimates store model_excessive_cwa_t3


poisson dv_comp_excessive_bi iv_abs_t3_bin1 iv_abs_t3_bin2 iv_abs_t3_bin4 iv_abs_t3_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num#i.region_num if clim_zone == "Aw: Tropical, savannah", vce(robust) irr
		
estimates store model_excessive_aw_t3

poisson dv_comp_excessive_bi iv_abs_t3_bin1 iv_abs_t3_bin2 iv_abs_t3_bin4 iv_abs_t3_bin5 ///
        i.ses_caste_tri_num i.ses_religion_tri_num i.ses_wealth_bi_num i.ses_residence_num ses_fertAge_std ///
        i.season_birth_num#i.region_num if clim_zone == "BSh: Arid, steppe, hot", vce(robust) irr
		

// For Rich
estimates restore model_excessive_wb_t3
matrix b0 = e(b)
matrix V0 = e(V)

// For Poor
estimates restore model_excessive_delhi_t3
matrix b1 = e(b)
matrix V1 = e(V)

///
local coef_name "iv_abs_t3_bin5"

clear
set obs 2
gen ses_wealth_bi = _n
gen b = .
gen se = .

forvalues i = 1/2 {
    local j = `i' - 1
    matrix b`j' = e(b)
    matrix V`j' = e(V)
    local col = colnumb(b`j', "`coef_name'")
    replace b = b`j'[1,`col'] if _n == `i'
    replace se = sqrt(V`j'[`col',`col']) if _n == `i'
}

metan b se, label(namevar=ses_wealth_bi) lcols(ses_wealth_bi)