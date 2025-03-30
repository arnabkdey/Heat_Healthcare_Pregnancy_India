// Read dataset
use "D:\Arnab\Shared drives\Benmarhnia Lab\Arnab\project-datasets\manuscripts\clim_papers\heat\heat-and-flw-visits\processed-data\2.3-final-hv-data-6mo.dta"
cd "D:\Arnab\Shared drives\Benmarhnia Lab\Arnab\project-datasets\manuscripts\clim_papers\heat\heat-and-flw-visits\outputs\models\stata-models"


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


// Mixed effects models 
//// Full Model  
melogit dv_no_contact_3mo ///
		exp_bin_below_10_10 exp_bin_10_15_10 ///
		exp_bin_25_30_10 exp_bin_above_30_10  ///
		i.ses_wealth_bi_richer ///
		i.ses_access_issue_distance_num  ///
		i.mat_edu_level ///
		meta_rural ////
        i.month_int || meta_state_name_num:, vce(robust) or
		
outreg2 using full_model.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(exp_bin_below_10_10 exp_bin_10_15_10 exp_bin_25_30_10 exp_bin_above_30_10) 
    
// Effect Modification
//// By Rural/Urban
////// Urban
melogit dv_no_contact_3mo ///
		exp_bin_below_10_10 exp_bin_10_15_10  ///
		exp_bin_25_30_10 exp_bin_above_30_10  ///
        i.ses_wealth_bi_richer ///
		i.ses_access_issue_distance_num  ///
		/// mat_age_at_int_scaled 
		i.mat_edu_level ///
        i.month_int if meta_rural == 1 || meta_state_name_num:, vce(robust) or
		
estimates store m_urban

outreg2 using em-urban.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(exp_bin_below_10_10 exp_bin_10_15_10 exp_bin_25_30_10 exp_bin_above_30_10) 
		

////// Rural
melogit dv_no_contact_3mo ///
		exp_bin_below_10_10 exp_bin_10_15_10  ///
		exp_bin_25_30_10 exp_bin_above_30_10  ///
        i.ses_wealth_bi_richer ///
		i.ses_access_issue_distance_num  ///
		/// mat_age_at_int_scaled 
		i.mat_edu_level ///
        i.month_int if meta_rural == 2 || meta_state_name_num:, vce(robust) or

estimates store m_rural
outreg2 using em-rural.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(exp_bin_below_10_10 exp_bin_10_15_10 exp_bin_25_30_10 exp_bin_above_30_10) 

/////// Wald test for heterogeniety
estimates table m_rural m_urban, b(%9.3f) se(%9.3f) p(%9.3f)
lincom [m_rural]exp_bin_below_10_10 - [m_urban]exp_bin_below_10_10


//// By access issue distance
////// Big Problem
melogit dv_no_contact_3mo ///
		exp_bin_below_10_10 exp_bin_10_15_10 ///
		exp_bin_25_30_10 exp_bin_above_30_10  ///
		i.ses_wealth_bi_richer ///
		i.mat_edu_level ///
		meta_rural ////
        i.month_int if ses_access_issue_distance_num == 1 || meta_state_name_num:, ///
		vce(robust) or

outreg2 using em-big-prob.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(exp_bin_below_10_10 exp_bin_10_15_10 exp_bin_25_30_10 exp_bin_above_30_10) 

////// Not a big problem
melogit dv_no_contact_3mo ///
		exp_bin_below_10_10 exp_bin_10_15_10 ///
		exp_bin_25_30_10 exp_bin_above_30_10  ///
		i.ses_wealth_bi_richer ///
		i.mat_edu_level ///
		meta_rural ////
        i.month_int if ses_access_issue_distance_num == 2 || meta_state_name_num:, ///
		vce(robust) or

outreg2 using em-not-big-prob.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(exp_bin_below_10_10 exp_bin_10_15_10 exp_bin_25_30_10 exp_bin_above_30_10) 

		
		
//// Wealth
////// Richer
melogit dv_no_contact_3mo ///
		exp_bin_below_10_10 exp_bin_10_15_10 ///
		exp_bin_25_30_10 exp_bin_above_30_10  ///
		ses_access_issue_distance_num  ///
		i.mat_edu_level ///
		meta_rural ////
        i.month_int if ses_wealth_bi_richer == 1 || meta_state_name_num:, vce(robust) or

outreg2 using em-richer.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(exp_bin_below_10_10 exp_bin_10_15_10 exp_bin_25_30_10 exp_bin_above_30_10) 

		
////// Poorer
melogit dv_no_contact_3mo ///
		exp_bin_below_10_10 exp_bin_10_15_10 ///
		exp_bin_25_30_10 exp_bin_above_30_10  ///
		ses_access_issue_distance_num  ///
		i.mat_edu_level ///
		meta_rural ////
        i.month_int if ses_wealth_bi_richer == 2 || meta_state_name_num:, vce(robust) or

outreg2 using em-poorer.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(exp_bin_below_10_10 exp_bin_10_15_10 exp_bin_25_30_10 exp_bin_above_30_10) 

//// Education
////// Primary or less
melogit dv_no_contact_3mo ///
		exp_bin_below_10_10 exp_bin_10_15_10 ///
		exp_bin_25_30_10 exp_bin_above_30_10  ///
		ses_access_issue_distance_num  ///
		i.ses_wealth_bi_richer ///
		meta_rural ////
        i.month_int if mat_edu_level == 3 || meta_state_name_num:, vce(robust) or
		
outreg2 using em-less-edu.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(exp_bin_below_10_10 exp_bin_10_15_10 exp_bin_25_30_10 exp_bin_above_30_10) 
	
////// Higher education
melogit dv_no_contact_3mo ///
		exp_bin_below_10_10 exp_bin_10_15_10 ///
		exp_bin_25_30_10 exp_bin_above_30_10  ///
		ses_access_issue_distance_num  ///
		i.ses_wealth_bi_richer ///
		meta_rural ////
        i.month_int if mat_edu_level != 1 || meta_state_name_num:, vce(robust) or
		
outreg2 using em-high-edu.xls, replace excel dec(3) eform sideway stat(coef ci) level(95) ///
    keep(exp_bin_below_10_10 exp_bin_10_15_10 exp_bin_25_30_10 exp_bin_above_30_10) 

