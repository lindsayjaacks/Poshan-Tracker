
# Project: Anuvaad - Poshan Tracker vs NFHS5
# Programmer: Lindsay Jaacks
# Date created: 2 December 2023 

################################################################################
# Prepare R Environment ####
################################################################################
# Clear R Environment
rm(list=ls())

# Check if the packages that we need are installed
want = c("tidyverse", "haven", "writexl","readxl","labelled","gtsummary","flextable","janitor","magrittr","xtable","ggthemes","hrbrthemes")
have = want %in% rownames(installed.packages())

# Install any missing packages
if ( any(!have) ) { install.packages( want[!have] ) }

# Load the packages
junk <- lapply(want, library, character.only = T)

# Remove the temporary objects we created
rm(have, want, junk)

# Assign directory
setwd("C:/Users/ljaacks/OneDrive - University of Edinburgh/Anuvaad/Poshan Tracker/Poshan Tracker Analysis/")

# Import data
tracker.df <- read_excel("Data/Poshan Tracker AWC 2023-12-02.xlsx")
growth.df <- read_excel("Data/Poshan Tracker Growth 2023-12-02.xlsx")
stunting.df <- read_table("Output/stunting.txt")
underweight.df <- read_table("Output/underweight.txt")
wasting.df <- read_table("Output/wasting.txt")
overweight.df <- read_table("Output/overweight.txt")

# Drop Poshan Tracker data outside range of July 2022 to September 2023 as data were incomplete for those months
tracker.df$month <- factor(tracker.df$month)
tracker.df <- tracker.df %>% filter(!is.na(awc_15d)) %>%
                           filter(month!="2023-10-01")

# Clean up NFHS5 output data from Stata 
stunting.df <- stunting.df %>% 
      mutate(nfhs_stunting_0to5y=Yes*100,
             state=as.factor(case_when(State=="Total" ~ "100",
                       State!="Total" ~ State))) %>% 
      select(state,nfhs_stunting_0to5y)

underweight.df <- underweight.df %>% 
  mutate(nfhs_underweight_0to5y=Yes*100,
         state=as.factor(case_when(State=="Total" ~ "100",
                                   State!="Total" ~ State))) %>% 
  select(state,nfhs_underweight_0to5y)

wasting.df <- wasting.df %>% 
  mutate(nfhs_wasting_0to5y=Yes*100,
         state=as.factor(case_when(State=="Total" ~ "100",
                                   State!="Total" ~ State))) %>% 
  select(state,nfhs_wasting_0to5y)

overweight.df <- overweight.df %>% 
  mutate(nfhs_overweight_0to5y=Yes*100,
         state=as.factor(case_when(State=="Total" ~ "100",
                                   State!="Total" ~ State))) %>% 
  select(state,nfhs_overweight_0to5y)

nfhs.df <- full_join(stunting.df,underweight.df,by="state") 
nfhs.df <- full_join(nfhs.df,wasting.df,by="state") 
nfhs.df <- full_join(nfhs.df,overweight.df,by="state") 

# Label states
nfhs.df %<>% mutate(state=fct_recode(state,"Jammu and Kashmir"="1","Himachal Pradesh"="2","Punjab"="3","Chandigarh"="4","Uttarakhand"="5",
                                      "Haryana"="6","Delhi"="7","Rajasthan"="8","Uttar Pradesh"="9","Bihar"="10","Sikkim"="11",
                                      "Arunachal Pradesh"="12","Nagaland"="13","Manipur"="14","Mizoram"="15","Tripura"="16","Meghalaya"="17",
                                      "Assam"="18","West Bengal"="19","Jharkhand"="20","Odisha"="21","Chhattisgarh"="22","Madhya Pradesh"="23",
                                      "Gujarat"="24","Dadra & Nagar Haveli and Daman & Diu"="25","Maharashtra"="27","Andhra Pradesh"="28","Karnataka"="29",
                                      "Goa"="30","Lakshadweep"="31","Kerala"="32","Tamil Nadu"="33","Puducherry"="34","Andaman & Nicobar Islands"="35",
                                      "Telangana"="36","Ladakh"="37","India"="100"))

# Merge NFHS5 and Poshan Tracker data
growth.df <- left_join(growth.df,nfhs.df,by="state")
                                  
################################################################################
# AWCs functioning ####
################################################################################
tracker.df <- tracker.df %>% mutate(awc_15d_prop=round((awc_15d/awc_1d)*100,1),
                                  awc_21d_prop=round((awc_21d/awc_1d)*100,1))

awc.long <- tracker.df %>% select(month,awc_15d_prop,awc_21d_prop) %>%
  pivot_longer(cols = starts_with("awc"),
               names_to = "days_open",
               values_to = "proportion")

awc.long <- awc.long %>% mutate(days_open = as.factor(days_open))

relabel.days_open = as_labeller(c(awc_15d_prop = "15 days", 
                                  awc_21d_prop = "21 days"))

awc.long <- awc.long %>% rowwise() %>% mutate(days_open2 = factor(relabel.days_open(days_open))) 

fig1.fig <- ggplot(awc.long, aes(x = month,y = proportion, group = days_open2, color = days_open2)) +
  geom_line() +  labs(title = "Proportion of AWCs open 15 days or 21 days over time",
       x = "",
       y = "Proportion of AWCs",
       color = "Number of Days Open") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
  lims(y=c(0,100))

ggsave(fig1.fig, filename = "Output/AWCs_Open_Days.jpeg", width = 9, height = 4)

################################################################################
# Receipt of THR ####
################################################################################
thr.long <- tracker.df %>% select(month,thr_15d,thr_21d) %>%
  pivot_longer(cols = starts_with("thr"),
               names_to = "days_received",
               values_to = "number")

thr.long <- thr.long %>% mutate(days_received = as.factor(days_received),
                                number_lakh = number/100000)

relabel.days_received = as_labeller(c(thr_15d = "15 days", 
                                      thr_21d = "21 days"))

thr.long <- thr.long %>% rowwise() %>% mutate(days_received2 = factor(relabel.days_received(days_received))) 

fig2.fig <- ggplot(thr.long, aes(x = month,y = number_lakh, group = days_received2, color = days_received2)) +
  geom_line() +  labs(title = "Number of beneficiaries receiving 15 days or 21 days of THR over time",
                      x = "",
                      y = "Number of beneficiaries (lakh)",
                      color = "Number of Days Receiving THR") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
  lims(y=c(0,400))

ggsave(fig2.fig, filename = "Output/THR_Receive_Days.jpeg", width = 9, height = 4)

################################################################################
# Receipt of HCM ####
################################################################################
hcm.long <- tracker.df %>% select(month,hcm_15d,hcm_21d) %>%
  pivot_longer(cols = starts_with("hcm"),
               names_to = "days_received",
               values_to = "number")

hcm.long <- hcm.long %>% mutate(days_received = as.factor(days_received),
                                number_lakh = number/100000)

relabel.days_received = as_labeller(c(hcm_15d = "15 days", 
                                      hcm_21d = "21 days"))

hcm.long <- hcm.long %>% rowwise() %>% mutate(days_received2 = factor(relabel.days_received(days_received))) 

fig3.fig <- ggplot(hcm.long, aes(x = month,y = number_lakh, group = days_received2, color = days_received2)) +
  geom_line() +  labs(title = "Number of beneficiaries receiving 15 days or 21 days of HCM over time",
                      x = "",
                      y = "Number of beneficiaries (lakh)",
                      color = "Number of Days Receiving HCM") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
  lims(y=c(0,400))

ggsave(fig3.fig, filename = "Output/HCM_Receive_Days.jpeg", width = 9, height = 4)

################################################################################
# Growth monitoring ####
################################################################################
growth.long <- tracker.df %>% select(month,growth) %>%
  pivot_longer(cols = starts_with("growth"),
               values_to = "number") %>% 
  mutate(growth_lakh = number/100000) %>% 
  select(month,name,growth_lakh)

fig4.fig <- ggplot(growth.long, aes(x = month,y = growth_lakh, group = name, colour = name)) +
  geom_line() +  labs(title = "Number of children monitored over time",
                      x = "",
                      y = "Number of children measured (lakh)") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),legend.position = "none") +
  lims(y=c(0,900))

ggsave(fig4.fig, filename = "Output/Growt_Monitoring_Number.jpeg", width = 9, height = 4)

################################################################################
# Stunting ####
################################################################################
stunting.df.long <- growth.df %>% select(state, stunting_0to6y, nfhs_stunting_0to5y) %>% 
  pivot_longer(cols = c(stunting_0to6y,nfhs_stunting_0to5y),
               names_to = "measures",
               values_to = "stunting") 

stunting.df.long <- stunting.df.long %>% mutate(measures = as.factor(measures))

relabel.measures = as_labeller(c(stunting_0to6y = "Poshan Tracker", 
                                 nfhs_stunting_0to5y = "NFHS-5"))

stunting.df.long <- stunting.df.long %>% rowwise() %>% mutate(measures2 = factor(relabel.measures(measures))) 

fig5.fig <- ggplot(stunting.df.long) +
  geom_segment(data = growth.df, aes(x=stunting_0to6y, xend=nfhs_stunting_0to5y, y=state, yend=state)) +
  geom_point(aes(x=stunting, y=state, color = measures2), size=3 ) +
  labs(x = "Stunting (%)",
       y = "",
       color = "Data Source") +
  lims(x=c(0,50)) +
  theme_bw()

ggsave(fig5.fig, filename = "Output/Stunting_NFHS5_PoshanTracker.jpeg",width = 9, height = 6)

################################################################################
# Underweight ####
################################################################################
underweight.df.long <- growth.df %>% select(state, underweight_0to6y, nfhs_underweight_0to5y) %>% 
  pivot_longer(cols = c(underweight_0to6y,nfhs_underweight_0to5y),
               names_to = "measures",
               values_to = "underweight") 

underweight.df.long <- underweight.df.long %>% mutate(measures = as.factor(measures))

relabel.measures = as_labeller(c(underweight_0to6y = "Poshan Tracker", 
                                 nfhs_underweight_0to5y = "NFHS-5"))

underweight.df.long <- underweight.df.long %>% rowwise() %>% mutate(measures2 = factor(relabel.measures(measures))) 

fig6.fig <- ggplot(underweight.df.long) +
  geom_segment(data = growth.df, aes(x=underweight_0to6y, xend=nfhs_underweight_0to5y, y=state, yend=state)) +
  geom_point(aes(x=underweight, y=state, color = measures2), size=3 ) +
  labs(x = "Underweight (%)",
       y = "",
       color = "Data Source") +
  lims(x=c(0,50)) +
  theme_bw()

ggsave(fig6.fig, filename = "Output/Underweight_NFHS5_PoshanTracker.jpeg",width = 9, height = 6)

################################################################################
# Wasting ####
################################################################################
wasting.df.long <- growth.df %>% select(state, wasting_0to5y, nfhs_wasting_0to5y) %>% 
  pivot_longer(cols = c(wasting_0to5y,nfhs_wasting_0to5y),
               names_to = "measures",
               values_to = "wasting") 

wasting.df.long <- wasting.df.long %>% mutate(measures = as.factor(measures))

relabel.measures = as_labeller(c(wasting_0to5y = "Poshan Tracker", 
                                 nfhs_wasting_0to5y = "NFHS-5"))

wasting.df.long <- wasting.df.long %>% rowwise() %>% mutate(measures2 = factor(relabel.measures(measures))) 

fig7.fig <- ggplot(wasting.df.long) +
  geom_segment(data = growth.df, aes(x=wasting_0to5y, xend=nfhs_wasting_0to5y, y=state, yend=state)) +
  geom_point(aes(x=wasting, y=state, color = measures2), size=3 ) +
  labs(x = "Wasting (%)",
       y = "",
       color = "Data Source") +
  lims(x=c(0,50)) +
  theme_bw()

ggsave(fig7.fig, filename = "Output/Wasting_NFHS5_PoshanTracker.jpeg",width = 9, height = 6)

################################################################################
# Overweight ####
################################################################################
overweight.df.long <- growth.df %>% select(state, overweight_0to5y, nfhs_overweight_0to5y) %>% 
  pivot_longer(cols = c(overweight_0to5y,nfhs_overweight_0to5y),
               names_to = "measures",
               values_to = "overweight") 

overweight.df.long <- overweight.df.long %>% mutate(measures = as.factor(measures))

relabel.measures = as_labeller(c(overweight_0to5y = "Poshan Tracker", 
                                 nfhs_overweight_0to5y = "NFHS-5"))

overweight.df.long <- overweight.df.long %>% rowwise() %>% mutate(measures2 = factor(relabel.measures(measures))) 

fig8.fig <- ggplot(overweight.df.long) +
  geom_segment(data = growth.df, aes(x=overweight_0to5y, xend=nfhs_overweight_0to5y, y=state, yend=state)) +
  geom_point(aes(x=overweight, y=state, color = measures2), size=3 ) +
  labs(x = "Overweight (%)",
       y = "",
       color = "Data Source") +
  lims(x=c(0,50)) +
  theme_bw()

ggsave(fig8.fig, filename = "Output/Overweight_NFHS5_PoshanTracker.jpeg",width = 9, height = 6)

################################################################################
# Quantify difference ####
################################################################################
growth.df <- growth.df %>% mutate(stunting_diff=stunting_0to6y-nfhs_stunting_0to5y,
                                  underweight_diff=underweight_0to6y-nfhs_underweight_0to5y,
                                  wasting_diff=wasting_0to5y-nfhs_wasting_0to5y,
                                  overweight_diff=overweight_0to5y-nfhs_overweight_0to5y)
mean(growth.df$stunting_diff)
mean(growth.df$underweight_diff)
mean(growth.df$wasting_diff)
mean(growth.df$overweight_diff)
