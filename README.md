# Poshan-Tracker
This code analyses publicly available data from the Poshan Tracker dashboard and National Family Health Survey 5 in India.

There are 2 code files:

**1.	Stata file called 'NFHS.do'**

This file analyses the child's recode file downloaded from the DHS website (https://www.dhsprogram.com/data/dataset/India_Standard-DHS_2020.cfm?flag=0), 'IAKR7AFL.dta'
   
It creates a subpopulation of living children aged 0-5 years who had their height and weight measured as part of NFHS5 and who reported having their weight measured at the AWC/ICDS centre in the last 12 months (see 'Women's questionnaire', Question 563, In the last 12 months, how often has (NAME)'s weight been measured by the Anganwadi/ICDS centre?) in order to improve comparability of NFHS5 data to the Poshan Tracker beneficiary data.
   
It derives survey-weighted estimates, taking into account the PSU and strata variables, for stunting, underweight, wasting, and overweight for children and exports them to tab-delimited files to be imported into R for visualisation.

**2.	R file called 'Poshan Tracker.R'**
   
This file merges estimates of stunting, underweight, wasting, and overweight from NFHS5 (above) with estimates from the Poshan Tracker at the all-India level and state level. It includes visualisations of the differences between the 2 data sources using lollipop charts.
   
The file also visualises trends in the number of days Anganwadi centres (AWCs) are open and the distribution of take-home rations (THR), from July 2022 to September 2023.


There are 2 data files:

**1. Excel data file called 'Poshan Tracker AWC 2023-12-02.xlxs'**

This contains monthly monitoring data on the number of days Anganwadi centres (AWCs) are open and the distribution of take-home rations (THR), from April 2022 to October 2023. Data are complete for July 2022 to September 2023.
   
**2. Excel data file called 'Poshan Tracker Growth 2023-12-02.xlxs'**

This contains all-India and state-wise estimates for child growth monitoring from September 2023.
