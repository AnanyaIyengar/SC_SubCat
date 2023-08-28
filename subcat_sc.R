################################################################
###Scheduled Caste Sub-Categorisation: Facts from 2011 Census###
################################################################

#Setting Working Directory

setwd("C:/Users/anniy/OneDrive/Desktop/census11_sc/subcat_census")

###############################################################################

#Loading Packages 

library(readxl)
library(dplyr)

###############################################################################

#Loading Data Sets. Source: A-10 Appendix, District-Wise Scheduled Caste Population, Census 2011
#Link: https://censusindia.gov.in/census.website/data/census-tables#

#Tamil Nadu: 

tn <- read_excel("tamilnadu.xlsx")
ap <- read_excel("andhra.xlsx")
pb <- read_excel("punjab.xlsx")
hr <- read_excel("haryana.xlsx")

##############################################################################

#Data Cleaning:

#Getting state-wide data, and removing district level observations and rows with rural/urban data.

#Tamil Nadu:
tn_state <- tn%>%dplyr::filter(dist_code==0)
tn_state <- tn_state%>%dplyr::filter(region == "Total")

#Andhra Pradesh:
ap_state <- ap%>%dplyr::filter(dist_code==0)
ap_state <- ap_state%>%dplyr::filter(region == "Total")

#Punjab
pb_state <- pb%>%dplyr::filter(dist_code==0)
pb_state <- pb_state%>%dplyr::filter(region == "Total")

#Haryana
hr_state <- hr%>%dplyr::filter(dist_code==0)
hr_state <- hr_state%>%dplyr::filter(region == "Total")

###############################################################################

#Creating caste groups on the basis of sub-categorisation in the state. 


#Tamil Nadu:

#The 3% sub-category for Arundhatiyars includes:
# - Arundhatiyar (sc_code = 5)
# - Chakkiliyan (sc_code = 12)
# - Madari (sc_code = 37)
# - Madiga (sc_code = 38)
# - Pagadai (sc_code = 48)
# - Thoti (sc_code = 67)
# - Adi Andhra (sc_code = 1)


arundhatiyars <- tn_state%>%dplyr::filter(sc_code == 5 | sc_code == 12 | sc_code == 37 |
                                            sc_code == 38 | sc_code == 48 | sc_code == 67 | sc_code == 1)

non_arundhatiyar <- tn_state%>%dplyr::filter(!sc_code %in% c(5, 12, 37, 38, 48, 67, 1, 0))

#We make a variable for total Scheduled Caste population in Tamil Nadu. 

total_sc_tn <- tn_state$tot_pop[1]

#Total Arundhatiyar Population in Tamil Nadu

total_arundhatiyar <- sum(arundhatiyars$tot_pop)

#Total Non-Arundhatiyar Population in Tamil Nadu

total_non_arundhatiyar <- sum(non_arundhatiyar$tot_pop)

#Total Population in Tamil Nadu

total_pop_tn <- 72147030



#Andhra Pradesh:

# - Group A: Caste Codes 8, 12, 16, 18, 20, 22, 23, 48, 51, 52, 54, 57
# - Group B: Caste Codes 5, 9, 10, 14, 15, 17, 19, 24, 28, 29, 30, 32, 33, 43, 44, 47, 55, 58 
# - Group C: Caste Codes 2, 3, 4, 6, 7, 11, 13, 21, 25, 26, 27, 31, 34, 35, 36, 37, 38, 39, 40, 41, 42, 45, 50, 56
# - Group D: Caste Codes 1, 46, 49, 53

ap_a <- ap_state%>%dplyr::filter(sc_code %in% c(8, 12, 16, 18, 20, 22, 23, 48, 51, 52, 54, 57))
ap_b <- ap_state%>%dplyr::filter(sc_code %in% c(5, 9, 10, 14, 15, 17, 19, 24, 28, 29, 30, 32, 33, 43, 44, 47, 55, 58))
ap_c <- ap_state%>%dplyr::filter(sc_code %in% c(2, 3, 4, 6, 7, 11, 13, 21, 25, 26, 27, 31, 34, 35, 36, 37, 38, 39, 40, 41, 42, 45, 50, 56))
ap_d <- ap_state%>%dplyr::filter(sc_code %in% c(1, 46, 49, 53))

#Note: The group Pambada, Pambanda was initially included in Group C, while the group Kolupulvandlu was included in Group B.
#These two categories were combined into one by the 2002 (Second Amendment) changes to the SC list. 
#Since in 2011, the combined castes have a total population of 1101 people, the caste is retained here in Group B. 

#Total Population in Andhra Pradesh

total_pop_ap <- 84580777

#Total Scheduled Caste Population in Andhra Pradesh

total_sc_ap <- ap_state$tot_pop[1]

#Total Group A Population in Andhra Pradesh

total_a <- sum(ap_a$tot_pop)

#Total Group B Population in Andhra Pradesh 

total_b <- sum(ap_b$tot_pop)

#Total Group C Population in Andhra Pradesh

total_c <- sum(ap_c$tot_pop)

#Total Group D Population in Andhra Pradesh

total_d <- sum(ap_d$tot_pop)


#Punjab

# - Balmikis (Balmiki, Chuhra, Bhangi): Caste Code 2
# - Mazhabis (Mazhabi, Mazhabi Sikhs): Caste Code 23
# _ Vimukt Jatis & Bazigars: Caste Codes 3, 4, 6, 25, 18, 7, 32
#Source for Vimukt Jati List: http://www.welfare.punjab.gov.in/Static/Vimukatjatis.html

balmiki_mazhabi <- pb_state%>%dplyr::filter(sc_code %in% c(2, 13))
othersc_pb <- pb_state%>%dplyr::filter(!sc_code %in% c(0,2,13))
vimukt <- pb_state%>%dplyr::filter(sc_code %in% c(3, 4, 6, 25, 18, 7, 32))

#Total Population in Punjab

total_pop_pb <- 27743338

#Total Scheduled Caste Population in Punjab

total_sc_pb <- pb_state$tot_pop[1]

#Total Balmiki and Mazhabi Sikh Population in Punjab

total_balmiki_mazhabi <- sum(balmiki_mazhabi$tot_pop)

#Total Population of all other Scheduled Castes in Punjab

total_other_pb <- sum(othersc_pb$tot_pop)

#Total Population of Vimukt Jatis and Bazigars in Punjab

total_vimukt <- sum(vimukt$tot_pop)

#Haryana

# - Deprived Scheduled Castes: Ad Dharmi; Balmiki; Bangali; Barar, Burar, Berar; Batwal, Barwala; Bauria, Bawaria;
#Bazigar; Bhanjra; Chanal; Dagi; Darain; Deha, Dhaya, Dhea; Dhanak; Dhogri, Dhangri, Siggi; Dumna, Mahasha, Doom; Gagra;
#Gandhila, Gandil Gondola; Kabirpanthi, Julaha; Khatik; Kori, Koli; Marija, Marecha; Mazhabi, Mazhabi Sikh; Megh, Meghwal;
#Nat, Badi; Od; Pasi; Perna; Pherera; Sanhai; Sanhal; Sansi, Bhedkut, Manesh; Sansoi; Sapela, Sapera; Sarera; Sikligar, Bariya; Sirkiband.

#- Caste Codes: all except 9 (chamars)

deprived_hr <- hr_state%>%dplyr::filter(!sc_code %in% c(9,0))
non_deprived_hr <- hr_state%>%dplyr::filter(sc_code %in% c(9))

#Total Population of Deprived Scheduled Castes in Haryana

total_deprived_hr <- sum(deprived_hr$tot_pop)

#Total Population of Non-Deprived Scheduled Castes in Haryana

total_nondeprived_hr <- sum(non_deprived_hr$tot_pop)

#Total Scheduled Caste Population in Haryana

total_sc_hr <- hr_state$tot_pop[1]

#Total Population of Haryana 

total_pop_hr <- 25351462


###############################################################################

#Evaulating Proportions- Tamil Nadu:

#Arundhatiyars as a percentage of SC population in Tamil Nadu (TN-A)

(total_arundhatiyar/total_sc_tn)*100

#Arundhatiyars as a percentage of total state population in Tamil Nadu (TN-B)

(total_arundhatiyar/total_pop_tn)*100

#Non-Arundhatiyars as a percentage of SC population in Tamil Nadu (TN-C)

(total_non_arundhatiyar/total_sc_tn)*100 #Note this is != 100 - (TN-A)

#Non-Arundhatiyars as a percentage of total state population in Tamil Nadu (TN-D)

(total_non_arundhatiyar/total_pop_tn)*100

#Scheduled Castes as a percentage of total state population in Tamil Nadu (TN-E)

(total_sc_tn/total_pop_tn)*100

#Discrepancies exist in that categories that must be mutually exhaustive (like TN-A and TN-C) do not add up to 100%.
#This is attributed to the fact that the total figure for scheduled castes includes data for Generic Castes i.e. those who returned as Anusuchit Jati, Harijan, etc. as mention by the Census 2011 Appendix 10 footnotes. 

###############################################################################

#Evaluating Proportions- Andhra Pradesh:

#Group A as a percentage of total SC population in Andhra Pradesh (AP-A)

(total_a/total_sc_ap)*100

#Group B as a percentage of total SC population in Andhra Pradesh (AP-B)

(total_b/total_sc_ap)*100

#Group C as a percentage of total SC population in Andhra Pradesh (AP-C)

(total_c/total_sc_ap)*100

#Group D as a percentage of total SC population in Andhra Pradesh (AP-D)

(total_d/total_sc_ap)*100

#Group A as a percentage of total population in Andhra Pradesh (AP-E)

(total_a/total_pop_ap)*100

#Group B as a percentage of total population in Andhra Pradesh (AP-F)

(total_b/total_pop_ap)*100

#Group C as a percentage of total population in Andhra Pradesh (AP-G)

(total_c/total_pop_ap)*100

#Group D as a percentage of total population in Andhra Pradesh (AP-H)

(total_d/total_pop_ap)*100

#Scheduled castes as a percentage of total population in Andhra Pradesh (AP-I)

(total_sc_ap/total_pop_ap)*100

#Discrepancies exist in that categories that must be mutually exhaustive do not add up to 100%.
#This is attributed to the fact that the total figure for scheduled castes includes data for Generic Castes i.e. those who returned as Anusuchit Jati, Harijan, etc. as mention by the Census 2011 Appendix 10 footnotes. 

###############################################################################

#Evaluating Proportions- Punjab:

#Balmikis and Mazhabi Sikhs as a percentage of total SC population in Punjab (PB-A)

(total_balmiki_mazhabi/total_sc_pb)*100

#Other Scheduled Castes as a percentage of total SC population in Punjab (PB-B)

(total_other_pb/total_sc_pb)*100

#Balmikis and Mazhabi Sikhs as a percentage of total population in Punjab (PB-C)

(total_balmiki_mazhabi/total_pop_pb)*100

#Other Scheduled Castes as a percentage of total population in Punjab (PB-D)

(total_other_pb/total_pop_pb)*100

#Total Scheduled Caste population as a percentage of total population in Punjab (PB-E)

(total_sc_pb/total_pop_pb)*100

#Vimukt Jatis as a percentage of total SC population in Punjab (PB-F)

(total_vimukt/total_sc_pb)*100

#Vimukt Jatis as a percentage of total population in Punjab (PB-G)

(total_vimukt/total_pop_pb)*100

#Discrepancies exist in that categories that must be mutually exhaustive do not add up to 100%.
#This is attributed to the fact that the total figure for scheduled castes includes data for Generic Castes i.e. those who returned as Anusuchit Jati, Harijan, etc. as mention by the Census 2011 Appendix 10 footnotes. 

##############################################################################

#Evaluating Proportions- Haryana:

#Deprived Scheduled Castes as a percentage of total SC population in Haryana (HR-A)

(total_deprived_hr/total_sc_hr)*100

#Non-deprived Scheduled Castes as a percentgae of total SC population in Haryana (HR-B)

(total_nondeprived_hr/total_sc_hr)*100

#Deprived Scheduled Castes as a percentage of total population in Haryana (HR-C)

(total_deprived_hr/total_pop_hr)*100

#Non-deprived Scheduled Castes as a percentage of total population in Haryana (HR-D)

(total_nondeprived_hr/total_pop_hr)*100

#Total Scheduled Caste population as a percentage of total population in Haryana (HR-E)

(total_sc_hr/total_pop_hr)*100

#Discrepancies exist in that categories that must be mutually exhaustive do not add up to 100%.
#This is attributed to the fact that the total figure for scheduled castes includes data for Generic Castes i.e. those who returned as Anusuchit Jati, Harijan, etc. as mention by the Census 2011 Appendix 10 footnotes. 

########################################################################################

#Sessions Info 
#R version 4.2.1 (2022-06-23 ucrt)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 22621)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_India.utf8  LC_CTYPE=English_India.utf8    LC_MONETARY=English_India.utf8
#[4] LC_NUMERIC=C                   LC_TIME=English_India.utf8    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     

#loaded via a namespace (and not attached):
# [1] fansi_1.0.3      assertthat_0.2.1 utf8_1.2.2       dplyr_1.0.10     R6_2.5.1        
# [6] DBI_1.1.3        lifecycle_1.0.2  magrittr_2.0.3   pillar_1.8.1     rlang_1.0.6     
# [11] cli_3.4.1        rstudioapi_0.14  vctrs_0.4.2      generics_0.1.3   tools_4.2.1     
# [16] glue_1.6.2       purrr_0.3.4      compiler_4.2.1   pkgconfig_2.0.3  tidyselect_1.1.2
# [21] tibble_3.1.8    






