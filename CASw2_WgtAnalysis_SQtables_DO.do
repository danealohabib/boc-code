clear all
set maxvar 10000

* Locate the correct Excel column
program drop _all
program define excol, rclass
	syntax anything
	
	local n = `anything'
	
	if `n' <= 26 {
		local co = word("`c(ALPHA)'", `n')
	} 
	else {
		local dig2 = mod(`n'-1, 26) + 1
		local n = `n' - `dig2'
		local dig1 = `n'/26
		
		local co =  word("`c(ALPHA)'", `dig1') 
		local co2 = word("`c(ALPHA)'", `dig2')
		local co = "`co'`co2'"
	}
	
	return local col = "`co'"
	end

*cd "C:\Users\ohad\Documents\MOP\trunk\Methods-of-Payment surveys\2020\CAS Wave 2"

global outdata "Weighting\output" 
global indata "Weighting/data"
**** bring in first set of weights **********

/*
use "Weighting/data/CAS_w2_v4_NRA_2.dta", clear

drop _merge

* Merge in additional weights
foreach x in "CAS_w2_v3_PSNRA_2.dta" "CAS_w2_v3_NRA_2.dta" "CAS_w2_v4_psNRA_2.dta" {
	preserve
	use "Weighting/data/`x'", clear
	drop _merge
	save temp.dta
	restore
	merge 1:1 ID using "temp.dta"
	drop if _merge == 2
	drop _merge
	erase temp.dta
}

*/

use "Weighting/data/CAS_w2_v4.dta", clear

capture drop _merge
	merge 1:1 ID using "${indata}/CAS_w2_v4_NRA_2.dta",gen(_merge1)
capture drop _merge1
	merge 1:1 ID using "${indata}/CAS_w2_v4_psNRA_2.dta",gen(_merge1)

capture drop _merge1
*merge 1:1 ID using "Data\2020CASW2_NotWeightedandCleaned.dta",gen(_merge1)

merge 1:1 ID using "Data\2020CASW2_WeightedandCleaned.dta", gen(_merge1)

* gen variables 
gen empfeb = inlist(EMP01_feb,1,2,3,9) if EMP01_feb<10
gen empnow = inlist(EMP01    ,1,2,3,9) if EMP01    <10

gen Q15b_nonotsure = Q9_new_1 | Q9_new_6

tab plan_nocash , gen(plan_nocash_)

gen truecashless = plan_nocash_1 & cashonhand_c == 0

gen num_dc_own =  Q12_new_2
winsor num_dc_own , p(0.005) highonly gen(num_dc_own_w99p5)
winsor num_dc_own , p(0.01) highonly gen(num_dc_own_w99)
winsor num_dc_own , p(0.02) highonly gen(num_dc_own_w98)
gen own_dc = num_dc_own > 0


gen num_cc_own = Q12_new_1
winsor num_cc_own , p(0.005) highonly gen(num_cc_own_w99p5)
winsor num_cc_own , p(0.01) highonly gen(num_cc_own_w99)
winsor num_cc_own , p(0.02) highonly gen(num_cc_own_w98)
gen own_cc = num_cc_own > 0

gen coh = cashonhand_c
gen och = othercash_c
gen tch = coh + och
gen has_coh = coh>0
gen has_och = och>0
gen has_tch = tch>0
winsor coh , highonly p(0.01) gen(coh_w99)
winsor och , highonly p(0.01) gen(och_w99)
winsor tch , highonly p(0.01) gen(tch_w99)

gen atm = Q20_new_1
gen teller = Q20_new_2
gen wd_atm = atm>0
gen wd_teller = teller>0
winsor atm , highonly p(0.01) gen(atm_w99)
winsor teller , highonly p(0.01) gen(teller_w99)

*made any purchase
gen ind_any_sto=0
replace ind_any_sto = 1 if ind_1_sto==1 | ind_2_sto==1 | ind_3_sto==1 | ind_4_sto==1 | ind_5_sto==1 | ind_6_sto==1 | ind_7_sto==1 | ind_8_sto==1 | ind_9_sto==1 | ind_10_sto==1 | ind_11_sto==1
replace ind_any_sto = . if ind_1_sto==. & ind_2_sto==. & ind_3_sto==. & ind_4_sto==. & ind_5_sto==. & ind_6_sto==. & ind_7_sto==. & ind_8_sto==. & ind_9_sto==. & ind_10_sto==. & ind_11_sto==.

gen ind_any_onl=0
replace ind_any_onl = 1 if ind_1_onl==1 | ind_2_onl==1 | ind_3_onl==1 | ind_4_onl==1 | ind_5_onl==1 | ind_6_onl==1 | ind_7_onl==1 | ind_8_onl==1 | ind_9_onl==1 | ind_10_onl==1 | ind_11_onl==1
replace ind_any_onl = . if ind_1_onl==. & ind_2_onl==. & ind_3_onl==. & ind_4_onl==. & ind_5_onl==. & ind_6_onl==. & ind_7_onl==. & ind_8_onl==. & ind_9_onl==. & ind_10_onl==. & ind_11_onl==.

egen numpur_tot = rowtotal(Q6_new_1_IP - Q6_new_11_IP)

gen own = Q40==1
gen aware = Q39==1

**************** LOOP THROUGH EACH SURVEY WEIGHT **************************


local colstart = 1

foreach sw in wgt_miRK wgt_miRKP1 wgt_miRKP1_often5 wgt_miRKP2 wgt_miRKP2_often5 wgt_miRKP3 wgt_miRKP3_often5 wgt_miRKP4 wgt_miRKP4_often5 ///
wgt_miRK_NR3 wgt_miRK_often5_NR3 wgt_miRK_psNR3 wgt_miRK_often5_psNR3 ///
wgt_miRKP1_NR3 wgt_miRKP1_often5_NR3 wgt_miRKP1_psNR3 wgt_miRKP1_often5_psNR3 ///
wgt_miRKP2_NR3 wgt_miRKP2_often5_NR3 wgt_miRKP2_psNR3 wgt_miRKP2_often5_psNR3 ///
wgt_miRKP3_NR3 wgt_miRKP3_often5_NR3 wgt_miRKP3_psNR3 wgt_miRKP3_often5_psNR3 ///
wgt_miRKP4_NR3 wgt_miRKP4_often5_NR3 wgt_miRKP4_psNR3 wgt_miRKP4_often5_psNR3 {

	preserve

	local colstart = `colstart' + 1
	
	svyset [pweight = `sw']

	**************************************************************************

	* produce output 

	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new.xlsx", sheet("Table 1") modify

	matrix define cash_est = J(1, 9, .)
	matrix define cash_est_sd = J(1, 9, .)
	epctile cashonhand_c_w99 if cashonhand_c >0 , svy p(50)
		matrix b = r(table)
		matrix cash_est[1, 1] = b[1,1]
		matrix cash_est_sd[1, 1] = b[2,1]

		
		epctile och_w99 if och >0 , svy p(50)
		matrix b = r(table)
		matrix cash_est[1, 2] = b[1,1]
		matrix cash_est_sd[1, 2] = b[2,1]
		 
		
		svy: mean cashonhand_c_w99 if cashonhand_c_w99>0
		mat b = r(table)
		matrix cash_est[1, 4] = b[1,1]
		matrix cash_est_sd[1, 4] = b[2,1]


		svy: mean othercash_c_w99 if othercash_c_w99>0
		mat b = r(table)
		matrix cash_est[1, 5] = b[1,1]
		matrix cash_est_sd[1, 5] = b[2,1]
		

		svy: mean has_coh 
		mat b = r(table)
		matrix cash_est[1, 7] = b[1,1]
		matrix cash_est_sd[1, 7] = b[2,1]


		svy: mean has_och
		mat b = r(table)
		matrix cash_est[1, 8] = b[1,1]
		matrix cash_est_sd[1, 8] = b[2,1]
		
	matewd cash_est_sd cash_est cash_est_cv
	mat cash_est_cv = 100*cash_est_cv

	local rowstart = 14*(`colstart'-1) + 1
	local rows2 = `rowstart' + 1

	local t1rowstart = 2*(`colstart'-1)+1
	local t1rows2 = 2*(`colstart'-1)+2
	
	*putexcel C`t1rowstart' = matrix(cash_est)
	*putexcel C`t1rows2' = matrix(cash_est_cv)
	
	
	local k=`colstart'+2

	putexcel C`k' = matrix(cash_est), nformat(number_d2)
	
	* label to output with the current weight
	putexcel B`k' = "`sw'", bold
	*ADD COLUMN NAMES*/
	putexcel C3="median_coh" 
	putexcel D3="median_och"
	putexcel F3="mean_coh"
	putexcel G3="mean_och"
	putexcel I3="has_coh"
	putexcel J3="has_och"
	
/*
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new.xlsx", sheet("tstats") modify
	
	putexcel B`rowstart' = "`sw'", bold
	
	putexcel C`rowstart' = matrix(cash_est)
	putexcel C`rows2' = matrix(cash_est_sd)
*/

	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("Table 2") modify
	gen only_atm = 1 if wd_atm==1 & wd_teller==0
	replace only_atm=0 if only_atm==.
	gen only_teller = 1 if wd_atm==0 & wd_teller==1
	replace only_teller = 0 if only_teller==.
	gen both_atm_teller = 1 if wd_atm==1 & wd_teller==1
	replace both_atm_teller=0 if both_atm_teller==.
	gen neither_atm_teller = 1 if wd_atm==0 & wd_teller==0
	replace neither_atm_teller = 0 if neither_atm_teller==.

	matrix define wtd_est = J(4, 1, .)
	matrix define wtd_est_sd = J(4, 1, .)

	svy: mean only_atm
		mat b= r(table)
		matrix wtd_est[1, 1] = b[1,1]
		matrix wtd_est_sd[1, 1] = b[2,1]
		
	svy: mean only_teller
		mat b= r(table)
		matrix wtd_est[2, 1] = b[1,1]
		matrix wtd_est_sd[2, 1] = b[2,1]
		
	svy: mean both_atm_teller
		mat b= r(table)
		matrix wtd_est[3, 1] = b[1,1]
		matrix wtd_est_sd[3, 1] = b[2,1]

	svy: mean neither_atm_teller
		mat b= r(table)
		matrix wtd_est[4, 1] = b[1,1]
		matrix wtd_est_sd[4, 1] = b[2,1]

	matewd wtd_est_sd wtd_est wtd_est_cv
	mat wtd_est_cv = (100*wtd_est_cv)'
	mat wtd_est = (wtd_est)'

	local t2row = `colstart' + 1
	
	putexcel C`t2row' = matrix(wtd_est), nformat(number_d2)

	* label to output with the current weight
	putexcel B`t2row' = "`sw'", bold

	putexcel B1="Proportion of Canadians that withdrew cash in the past week (%)"
	putexcel C2="Only ATM"
	putexcel D2="Only teller"
	putexcel E2="ATM and teller"
	putexcel F2="Did not withdraw from either ATM or teller"
	
/*
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("tstats") modify
	putexcel L`rowstart' = matrix(wtd_est)
	putexcel M`rowstart' = matrix(wtd_est_sd)
*/	
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("Table 3") modify

	matrix define used_est = J(10, 1, .)
	matrix define used_est_sd = J(10, 1, .)
		svy: mean used_cash
		mat b= r(table)
		matrix used_est[1, 1] = b[1,1]
		matrix used_est_sd[1, 1] = b[2,1]

		svy: mean used_debit
		mat b= r(table)
		matrix used_est[2, 1] = b[1,1]
		matrix used_est_sd[2, 1] = b[2,1]
		
		svy: mean used_debittng
		mat b= r(table)
		matrix used_est[3, 1] = b[1,1]
		matrix used_est_sd[3, 1] = b[2,1]
		
		svy: mean used_debitpin
		mat b= r(table)
		matrix used_est[4, 1] = b[1,1]
		matrix used_est_sd[4, 1] = b[2,1]
		
		svy: mean used_credit
		mat b= r(table)
		matrix used_est[5, 1] = b[1,1]
		matrix used_est_sd[5, 1] = b[2,1]
		
		svy: mean used_credittng
		mat b= r(table)
		matrix used_est[6, 1] = b[1,1]
		matrix used_est_sd[6, 1] = b[2,1]
		
		svy: mean used_creditpin
		mat b= r(table)
		matrix used_est[7, 1] = b[1,1]
		matrix used_est_sd[7, 1] = b[2,1]
		
		svy: mean used_etrans
		mat b= r(table)
		matrix used_est[8, 1] = b[1,1]
		matrix used_est_sd[8, 1] = b[2,1]
		
		svy: mean used_mobile
		mat b= r(table)
		matrix used_est[9, 1] = b[1,1]
		matrix used_est_sd[9, 1] = b[2,1]
		
		svy: mean used_svc
		mat b= r(table)
		matrix used_est[10, 1] = b[1,1]
		matrix used_est_sd[10, 1] = b[2,1]
		
	matewd used_est_sd used_est used_est_cv
	mat used_est_cv = (100*used_est_cv)'
	mat used_est = (used_est)'

	putexcel C`t2row' = matrix(used_est), nformat(number_d2)
	*putexcel `coliter2'2 = matrix(used_est_cv)	

	* label to output with the current weight
	putexcel B`t2row' = "`sw'", bold
	
	
	putexcel C2="Cash"
	putexcel D2="Debit"
	putexcel E2="Debit - tap & go"
	putexcel F2="Debit - Chip and PIN"
	
	putexcel G2="Credit"
	putexcel H2="Credit - tap & go"
	putexcel I2="Credit - Chip and PIN"
	
	
	putexcel J2="E-Transfer"
	putexcel K2="Mobile"
	putexcel L2="Prepaid card"
	
/*
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("tstats") modify
	putexcel R`rowstart' = matrix(used_est)
	putexcel S`rowstart' = matrix(used_est_sd)
*/
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("Table 4") modify
	gen overall_pur = 0
	replace overall_pur = 1 if ind_1_sto==1 | ind_2_sto==1 | ind_3_sto==1 | ind_4_sto==1 | ind_5_sto==1 | ind_6_sto==1 | ind_7_sto==1 | ind_8_sto==1 | ind_9_sto==1 | ind_10_sto==1 | ind_11_sto==1
	replace overall_pur = . if ind_1_sto==. & ind_2_sto==. & ind_3_sto==. & ind_4_sto==. & ind_5_sto==. & ind_6_sto==. & ind_7_sto==. & ind_8_sto==. & ind_9_sto==. & ind_10_sto==. & ind_11_sto==.


	egen tot_times = rowtotal(Q6_new_1_IP - Q6_new_11_IP)

	gen overall_pur_onl = 0
	replace overall_pur_onl = 1 if ind_1_onl==1 | ind_2_onl==1 | ind_3_onl==1 | ind_4_onl==1 | ind_5_onl==1 | ind_6_onl==1 | ind_7_onl==1 | ind_8_onl==1 | ind_9_onl==1 | ind_10_onl==1 | ind_11_onl==1
	replace overall_pur_onl = . if ind_1_onl==. & ind_2_onl==. & ind_3_onl==. & ind_4_onl==. & ind_5_onl==. & ind_6_onl==. & ind_7_onl==. & ind_8_onl==. & ind_9_onl==. & ind_10_onl==. & ind_11_onl==.


	egen tot_times_onl = rowtotal(Q6_new_1_OL - Q6_new_11_OL)

	mat purtype = J(11, 2, .)
	mat purtype_sd = J(11, 2, .)

	forvalues i = 1/11{
	*replace ind_`i'_sto = 0 if ind_`i'_sto==.
	capture svy: prop ind_`i'_sto
	if _rc==2000{
	mat mat`i' = J(2, 1, .)
	}
	else{
	mat mat`i' = r(table)
	}
	mat purtype[`i', 1] = mat`i'[1,2]
	mat purtype_sd [`i', 1] = mat`i'[2,2]

	capture svy: mean Q6_new_`i'_IP if Q6_new_`i'_IP >0
	if _rc==2000{
	mat mat`i' = J(2, 1, .)
	}
	else{
	mat mat`i' = r(table)
	}
	mat purtype[`i', 2] = mat`i'[1,1]
	mat purtype_sd [`i', 2] = mat`i'[2, 1]



	}

	mat overall = J(1,2, .)
	mat overall_sd = J(1, 2, .)

	svy: prop overall_pur
	mat b = r(table)
	mat overall[1, 1] = b[1, 2]
	mat overall_sd[1,1] = b[2,2]

	svy: mean tot_times if tot_times>0
	mat b = r(table)
	mat overall[1, 2] = b[1, 1]
	mat overall_sd[1,2] = b[2,1]

	mat purtype = overall \ purtype

	mat purtype_sd = overall_sd \ purtype_sd
	matewd purtype_sd purtype purtype_cv
	mat purtype_cv  = (100*purtype_cv)'
	mat purtype = (purtype)'
	
	*excol (`colstart'-1)*4 + 1
	local t4row = (`colstart'-1)*2 + 1
	local t4row2 = `t4row' + 1
		
	putexcel D`t4row' = matrix(purtype), nformat(number_d2)
	*putexcel `t4coliter'4 = matrix(purtype)

	local madepur "Made purchase (%)"
	local npur "# purchases"

	putexcel C`t4row' = "`madepur'"
	putexcel C`t4row2' = "`npur'"
	
	*putexcel `t4coliter'15 = matrix(purtype_cv)

	* label to output with the current weight
	putexcel B`t4row' = "`sw'", bold

	putexcel D2 = "pur_n"
	putexcel E2 = "Overall"
	putexcel F2 = "Groceries or Drugs"
	putexcel G2 = "Personal Attire"
	putexcel H2 = "Entertainment"
	putexcel I2 = "Meals"
	putexcel J2 = "Gasoline"
	putexcel K2 = "Hobby/Sporting Goods"
	putexcel L2 = "Health Care"
	putexcel M2 = "Professional/Personal Services"
	putexcel N2 = "Durable Goods"
	putexcel O2 = "Travel/Parking"

/*
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("tstats") modify
	putexcel Z`rowstart' = matrix(purtype)
	putexcel AA`rowstart' = matrix(purtype_sd)
*/

	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("Table 5") modify
	matrix define acc_est = J(5, 1, .)
	matrix define acc_est_sd = J(5, 1, .)
		svy: mean Q9_new_1
		mat b= r(table)
		matrix acc_est[1, 1] = b[1,1]
		matrix acc_est_sd[1, 1] = b[2,1]
		
		svy: mean Q9_new_4
		mat b= r(table)
		matrix acc_est[2, 1] = b[1,1]
		matrix acc_est_sd[2, 1] = b[2,1]

		svy: mean Q9_new_2
		mat b= r(table)
		matrix acc_est[3, 1] = b[1,1]
		matrix acc_est_sd[3, 1] = b[2,1]
		
		svy: mean Q9_new_5
		mat b= r(table)
		matrix acc_est[4, 1] = b[1,1]
		matrix acc_est_sd[4, 1] = b[2,1]
		
		svy: mean Q9_new_3
		mat b= r(table)
		matrix acc_est[5, 1] = b[1,1]
		matrix acc_est_sd[5, 1] = b[2,1]
		
	matewd acc_est_sd acc_est acc_est_cv
	mat acc_est_cv = (100*acc_est_cv)'
	mat acc_est = (acc_est)'

	local t5row = `colstart' + 1
	
	putexcel C`t5row' = matrix(acc_est), nformat(number_d2)
	*putexcel C`t5row' = matrix(acc_est_cv)
	
	putexcel C2="I didn't hear, see or experience a merchant refusing to accept cash"
	putexcel D2="I saw a sign that stated a merchant was not accepting cash"
	putexcel E2= "I saw a sign that stated cash was accepted but other payment methods were preferred"
	putexcel F2="I heard news reports that merchants stated cash was not accepted"
	putexcel G2="I was not able to use cash at a merchant's point of sale"

	* label to output with the current weight
	putexcel B`t5row' = "`sw'", bold
/*
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("tstats") modify
	putexcel AG`rowstart' = matrix(acc_est)
	putexcel AH`rowstart' = matrix(acc_est_sd)
*/
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("Table 6") modify
	matrix define plan_est = J(5, 1, .)
	matrix define plan_est_sd = J(5, 1, .)
		svy: mean plan_nocash_1
		mat b= r(table)
		matrix plan_est[1, 1] = b[1,1]
		matrix plan_est_sd[1, 1] = b[2,1]
		
		svy: mean plan_nocash_2
		mat b= r(table)
		matrix plan_est[2, 1] = b[1,1]
		matrix plan_est_sd[2, 1] = b[2,1]
		
		svy: mean plan_nocash_3
		mat b= r(table)
		matrix plan_est[3, 1] = b[1,1]
		matrix plan_est_sd[3, 1] = b[2,1]
		
		svy: mean plan_nocash_4
		mat b= r(table)
		matrix plan_est[4, 1] = b[1,1]
		matrix plan_est_sd[4, 1] = b[2,1]
		
		svy: mean truecashless
		mat b= r(table)
		matrix plan_est[5, 1] = b[1,1]
		matrix plan_est_sd[5, 1] = b[2,1]
		
	matewd plan_est_sd plan_est plan_est_cv
	mat plan_est_cv = 100*plan_est_cv
	mat plan_est = (plan_est)'
	
	local t6row = `colstart' + 1

	putexcel C`t6row' = matrix(plan_est), nformat(number_d2)
	*putexcel `coliter2'2 = matrix(plan_est_cv)
	
	* label to output with the current weight
	putexcel B`t6row' = "`sw'", bold
	
	
	putexcel C2="Already cashless"
	putexcel D2="within 5 years"
	putexcel E2="more than 5 years"
	putexcel F2= "No plans"
	putexcel G2= "True cashless"


/*
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("tstats") modify
	putexcel AL`rowstart' = matrix(plan_est)
	putexcel AN`rowstart' = matrix(plan_est_sd)
*/
	*preparation for COVID
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("Table A2") modify
	gen preparation = inlist(1,Q35_new_01,Q35_new_02)
	gen planning = inlist(1,Q35_new_03,Q35_new_04,Q35_new_05)
	gen socialdistance = inlist(1,Q35_new_06,Q35_new_07,Q35_new_08,Q35_new_11,Q35_new_12)
	gen cleanliness = inlist(1,Q35_new_09,Q35_new_10)
	gen other = Q35_new_14
	gen none = Q35_new_15
	gen woremask = Q35_new_13


	svy: mean preparation planning socialdistance cleanliness woremask
	mat b = r(table)
	mat prep = b[1, 1...]
	mat prep_sd = b[2, 1...]
	matewd prep_sd prep prep_cv
	mat prep_cv = 100*prep_cv
	
	*putexcel `coliter'2 = matrix(used_est)
	local t1rowstart = 1*(`colstart'-1)+2
	
	putexcel B`t1rowstart' = matrix(prep), nformat(number_d2)
	
	*putexcel `coliter'2 = matrix(prep)
	
	*putexcel B`t1rows2' = matrix(prep_cv)

	* output in rows
	* label to output with the current weight
	putexcel A`t1rowstart' = "`sw'", bold
* label to output with the current weight
	*putexcel `coliter'1 = "`sw'", bold
		
	putexcel A1 = "Canadians' Preparation for COVID	"
	putexcel B2="preparation"
	putexcel C2="planning"
	putexcel D2="socialdistance"
	putexcel E2= "cleanliness"
	putexcel F2= "wore a mask"

/*
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("tstats") modify
	putexcel BE`rowstart' = matrix(prep)
	putexcel BF`rowstart' = matrix(prep_sd)
*/
	*finlit by demographic

	* Specify demographics list
	global demo "age_3 income_3 resp_gender educ_3 FLscoreMOP"
	global demoshare "QRegion income_3 educ_3 FLscoreMOP"
	global democashless "age_3 QRegion"
	global demoused "QRegion income_3 educ_3 FLscoreMOP"
	global demo2017 "Prov_condense age_3 income_3 resp_gender educ_3 FLscoreMOP "

	global demoA4 "resp_gender age_3 educ_3 QRegion"

	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("Table A3") modify

	matrix define overall = J(1, 3, .)
	matrix define overall_sd = J(1, 3, .)

	svy: prop FLscoreMOP
	mat b = r(table)
	mat overall[1, 1] = b[1, 1...]
	mat overall_sd[1,1] = b[2, 1...]

	foreach vardem in $demoA4 {
	levelsof `vardem', local(levels) 
	quietly tab `vardem'
	matrix define `vardem' = J(`r(r)', 3, .)
	matrix define `vardem'_sd = J(`r(r)', 3, .)
	foreach l of local levels {
		svy: prop FLscoreMOP if  `vardem'==`l'
		matrix b = r(table)
		matrix `vardem'[`l', 1] = b[1,1...]
		matrix `vardem'_sd[`l', 1] = b[2,1...]


	}
	}
	mat finlit = overall \ resp_gender \ age_3 \ educ_3 \ QRegion
	mat finlit_sd = overall_sd \ resp_gender_sd \ age_3_sd \ educ_3_sd \ QRegion_sd
	matewd finlit_sd finlit finlit_cv
	mat finlit_cv = 100*finlit_cv

	excol (`colstart'-1)*3 + 1
	local ta3coliter = r(col)
	excol (`colstart'-1)*3 + 2
	local ta3coliter2 = r(col)
	excol (`colstart'-1)*3 + 3
	local ta3coliter3 = r(col)
	
	
	local finlow low 
	local finmed medium 

	putexcel `ta3coliter'3 = matrix(finlit), nformat(number_d2)
	
	putexcel `ta3coliter'2 = "Low"
	putexcel `ta3coliter2'2 = "Medium"
	putexcel `ta3coliter3'2 = "High"
	
	* label to output with the current weight
	putexcel `ta3coliter'1 = "`sw'", bold
	
	putexcel C3="Overall"
	putexcel C4="Male"
	putexcel C5="Female"
	putexcel C6= "18-34"
	putexcel C7= "34-54"
	putexcel C8="55+"
	putexcel C9="Highschool"
	putexcel C10="College"
	putexcel C11= "University"
	putexcel C12= "British Columbia"
	putexcel C13="Praires"
	putexcel C14="Ontario"
	putexcel C15="Quebec"
	putexcel C16= "Atlantic"
	
	putexcel B4="Gender"
	putexcel B6="Age"
	putexcel B9="Education"
	putexcel B12="Region"


	
/*
	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("tstats") modify
	putexcel BM`rowstart' = matrix(finlit)
	*putexcel BN`rowstart' = matrix(finlit_sd)
*/
	
	*********************************************
	**********************************************

	*****************************************************************

	* compare demographics with different weights

	putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("demographics") modify

	global demlist "resp_gender Age_10yr Prov_condense marst LFS2_feb Educ_condense LFS2 HHInc HHsize FLscoreMOP frame Q37"

	*svyset [pw =]

	foreach var in $demlist {
	svy: tab `var'
	matrix `var'_0 = e(b)'*100
	}

	mat results_now = resp_gender_0 \ Age_10yr_0 \ Prov_condense_0 \ marst_0 \ LFS2_feb_0 \ Educ_condense_0 \ LFS2_0 \ HHInc_0\ HHsize_0 \ FLscoreMOP_0 \ frame_0 \ Q37_0

	excol `colstart'+1
	local demcol = r(col)
	
	putexcel `demcol'3 = matrix(results_now), nformat(number_d2)

	* generate variable for covid questions
	gen Q35_c_preparation=0

	replace Q35_c_preparation = 1 if Q35_new_01 == 1 | Q35_new_02 == 1

	gen Q35_c_planning =0

	replace Q35_c_planning = 1 if Q35_new_03 == 1 | Q35_new_04 == 1 | Q35_new_05 == 1

	gen Q35_c_distancing =0

	replace Q35_c_distancing = 1 if Q35_new_06 == 1 | Q35_new_07 == 1 | Q35_new_08 == 1 | Q35_new_11 == 1 | Q35_new_12 == 1

	gen Q35_c_clean = 0

	replace Q35_c_clean = 1 if Q35_new_09 == 1 | Q35_new_10 == 1

	gen Q35_c_other = 0

	replace Q35_c_other = 1 if Q35_new_14 == 1 

	gen Q35_c_none = 0

	replace Q35_c_none = 1 if Q35_new_15 == 1 


	svy: mean Q35_c_* Q36_*
		matrix b = r(table)
		matrix Q35_36_mean = b[1,1..14]' 
		matrix Q35_36_mean = Q35_36_mean * 100
		
	putexcel `demcol'49 = matrix(Q35_36_mean), nformat(number_d2)

	* add header to compare weights
	putexcel `demcol'2 = "`sw'", bold
	
	restore
	
}	
	
putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("demographics") modify	
putexcel B3 = "Male", 
putexcel B4 = "Female"
putexcel B5 = "age <=24"
putexcel B6 = "25-34"
putexcel B7 = "35-44"
putexcel B8 = "45-54"
putexcel B9 = "55-64"
putexcel B10 = "65+"

putexcel B11 = "British Columbia", 
putexcel B12 = "Prairies"
putexcel B13 = "Ontario"
putexcel B14 = "Quebec"
putexcel B15 = "Atlantic"

putexcel B16 = "Married/common law"
putexcel B17 = "Single/widowed/divorced "

putexcel B18 = "Employed (in Feb2020)"
putexcel B19 = "Unemployed (in Feb2020)"
putexcel B20 = "Not in labour force (in Feb2020)"

putexcel B21 = "HS"
putexcel B22 = "College"
putexcel B23 = "University"

putexcel B24 = "Employed (current)"
putexcel B25 = "LF2 Unemployed (current)"
putexcel B26 = "LF2 Not in labour force (current)"

putexcel B27 = "HH inc <25"
putexcel B28 = "HH inc 25-45"
putexcel B29 = "HH inc 45-65"
putexcel B30 = "HH inc 65-85"
putexcel B31 = "HH inc 85-125"
putexcel B32 = "HH inc >125"

putexcel B33 = "HS size 1"
putexcel B34 = "HS size 2"
putexcel B35 = "HS size 3"
putexcel B36 = "HS size 4"
putexcel B37 = "HS size 5+"

putexcel B38 = "Fin Lit Low"
putexcel B39 = "Medium"
putexcel B40 = "High"

putexcel B41 = "I-say"
putexcel B42 = "Ampario"
putexcel B43 = "RTS"
putexcel B44 = "Other"

putexcel B45 = "Q 37 Increased"
putexcel B46 = "About the same"
putexcel B47 = "Decreased"
putexcel B48 = "Not Applicable"

putexcel B49 = "Q 35 preperation"
putexcel B50 = "planning"
putexcel B51 = "distancing"
putexcel B52 = "cleanliness"
putexcel B53 = "other"
putexcel B54 = "none"

putexcel B55 = "Q 36 Did not let websites remember personal information"
putexcel B56 = "Did not let websites remember credit card information"
putexcel B57 = "Shopped only on reputable websites"
putexcel B58 = "Used credit card with low credit amount"
putexcel B59 = "Used a third-party payment service (e.g., PayPal)"
putexcel B60 = "Looked for HTTPS in the website address and lock symbol"
putexcel B61 = "Used strong passwords or passphrases"
putexcel B62 = "Did not shop online"


**** fixing output 

/*
putexcel set "${outdata}\CASw2_WgtAnalysis_SQTables_new", sheet("Table A3") modify


svyset [pweight = wgt_miRKP1_often5_psNR3]

    putexcel B1="."
	putexcel B3=""
	putexcel B5=""
	
	putexcel B7=""
	putexcel B8=""
	putexcel B10=""
	putexcel B11=""
	
	putexcel B13="."
	putexcel B14=""
	putexcel B15=""
	putexcel B16=""


	matrix define overall = J(1, 3, .)
	matrix define overall_sd = J(1, 3, .)

	svy: prop FLscoreMOP
	mat b = r(table)
	mat overall[1, 1] = b[1, 1...]
	mat overall_sd[1,1] = b[2, 1...]

	foreach vardem in $demoA4 {
	levelsof `vardem', local(levels) 
	quietly tab `vardem'
	matrix define `vardem' = J(`r(r)', 3, .)
	matrix define `vardem'_sd = J(`r(r)', 3, .)
	foreach l of local levels {
		svy: prop FLscoreMOP if  `vardem'==`l'
		matrix b = r(table)
		matrix `vardem'[`l', 1] = b[1,1...]
		matrix `vardem'_sd[`l', 1] = b[2,1...]


	}
	}
	mat finlit = overall \ resp_gender \ age_3 \ educ_3 \ QRegion
	mat finlit_sd = overall_sd \ resp_gender_sd \ age_3_sd \ educ_3_sd \ QRegion_sd
	matewd finlit_sd finlit finlit_cv
	mat finlit_cv = 100*finlit_cv
 

putexcel AZ3 = matrix(finlit)	

putexcel AZ1 = "wgt_miRKP1_often5_psNR3", bold	
putexcel AZ2 = "low"
putexcel AZ3 = "medium"
putexcel AZ4 = "high"
