* Project: Anuvaad - Poshan Tracker vs NFHS5
* Programmer: Lindsay Jaacks
* Date created: 2 December 2023

cls
clear all 

use "UPDATE\IAKR7AFL.DTA"
 
keep v002 v021 v022 v024 v025 hw2 hw7 hw8 hw10 hw11 hw71 hw72 sweight hw3 hw4 hw5 hw70 b4 hw1 b5 hw2 hw3 s558 s559 s563 b16 
order v024 v025 sweight v021 v022 v002 b16 b5 b4 hw1 hw2 hw7 hw8 hw71 hw3 hw4 hw5 hw70 hw10 hw11 hw72 s558 s559 s563  
 
********************************************************************************
*	Survey sampling variables
********************************************************************************
gen age_yr=hw1/12
label var age_yr "child's age in years"
order age_yr, after(hw1)

gen measure=0
replace measure=1 if hw1!=. //not missing
replace measure=. if b5==0 //not alive
lab var measure "child was alive and measured"
label define yesno 0"No" 1"Yes"
label val measure yesno
order measure, before(hw1)

*Women's questionnaire
* Question 563, In the last 12 months, how often has (NAME)'s weight been measured by the anganwadi/ICDS centre?
gen awc=0
replace awc=1 if s563>=1 & s563<8 //value of 8 = don't know, assume no
label var awc "child was weighed by the anganwadi/ICDS centre in the last 12 months"
label val awc yesno
order awc, after(s563)

gen sample=0
replace sample=1 if b5==1 & measure==1 & awc==1 
label var sample "sample subpopulation for poshan tracker comparison"
order sample, after(sweight)
	*n=8,702 not alive (b5)
	*n=2,955 of those alive were not measured (hw1)
	*n=98,692 did not have weight measured at the AWC/ICDS centre in the last 12 months

svyset [pweight = sweight], psu(v021) strata(v022) singleunit(centered) vce(linearized)

********************************************************************************
*	Output prevalence estimates overall and by state
********************************************************************************

*Stunting
gen stunting=0 
	replace stunting=1 if hw70<-200 
	replace stunting =. if hw70>=9996 //flagged cases and age in days out of plausible limits
label var stunting "stunting (height for age)"
label val stunting yesno

estpost svy, subpop(sample): tab v024 stunting, row
esttab using "UPDATE\stunting.txt", cell("b(f(4))") unstack noobs replace wide plain 

*Underweight 
gen underweight=0
	replace underweight=1 if hw71<-200  
	replace underweight=. if hw71>=9996  
label var underweight "underweight (weight for age)"
label val underweight yesno
	
estpost svy, subpop(sample): tab v024 underweight, row
esttab using "UPDATE\underweight.txt", cell("b(f(4))") unstack noobs replace wide plain 

*Wasting
gen wasting=0 
	replace wasting=1 if hw72<-200 
	replace wasting=. if hw72>= 9996 
label val wasting yesno
label var wasting "wasting (weight for height)"
		
estpost svy, subpop(sample): tab v024 wasting, row
esttab using "UPDATE\wasting.txt", cell("b(f(4))") unstack noobs replace wide plain 

*Overweight		
gen overweight=0
	replace overweight=1 if hw72>200 
	replace overweight=. if hw72>=9996 
label var overweight "overweight (weight for height)"
label val overweight yesno
	
estpost svy, subpop(sample): tab v024 overweight, row
esttab using "UPDATE\overweight.txt", cell("b(f(4))") unstack noobs replace wide plain 