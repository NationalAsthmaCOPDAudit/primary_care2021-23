# primary care further analysis

library(tidyverse)
library(openxlsx2)



practice_details <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
                              sheet = "Practice Details")
ethnicity_groups <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
                              sheet = "Ethnicity Groups")
age_dat_ast <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
                              sheet = "Registers by Age", cols = 1:4) %>% filter(!is.na(Practice))
age_dat_copd <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
                         sheet = "Registers by Age", cols = 7:10) %>% filter(!is.na(Practice))

# WIMD needs checking

# wimd_dat_CYPA <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
#                               sheet = "Registers by Deprivation")
# imd_dat_AA <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
#                              sheet = "Registers by Deprivation")
# imd_dat_COPD <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
#                              sheet = "Registers by Deprivation")

wimd_dat_CYPA <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Revised Registers by Deprivation.xlsx", 
                                sheet = "Registers by Deprivation", cols = 11:13) %>% filter(!is.na(Practice))
wimd_dat_AA <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Revised Registers by Deprivation.xlsx", 
                              sheet = "Registers by Deprivation", cols = 6:8) %>% filter(!is.na(Practice))
wimd_dat_COPD <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Revised Registers by Deprivation.xlsx", 
                                sheet = "Registers by Deprivation", cols = 16:18) %>% filter(!is.na(Practice))


ethnicity_dat_CYPA <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
                              sheet = "Registers by Ethnicity", cols = 11:13) %>% filter(!is.na(Practice))
ethnicity_dat_AA <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
                           sheet = "Registers by Ethnicity", cols = 6:8) %>% filter(!is.na(Practice))
ethnicity_dat_COPD <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
                           sheet = "Registers by Ethnicity", cols = 16:18) %>% filter(!is.na(Practice))

indiv_dat <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
                              sheet = "Audit Data")
practice_summary <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
                              sheet = "Practice Summary")
wales_summary <- read_xlsx("Z:/Group_work/NACAP/2023/Wales NRAP Data as at 31st July 2023/Wales NRAP Data as at 31st July 2023.xlsx", 
                              sheet = "Wales Summary")


streamlined <- practice_details %>% select(`Practice Code`, `Local Health Board`, `Cluster`, `Practice Name`) 



# Breakdown that we need:

# for each disease: 
# general - practice level
# general - LHB level
# general - Wales
# 
# ethnicity - practice level
# ethnicity - LHB level
# eethnicity - Wales
# 
# age - practice level
# age - LHB level
# age - Wales
#
# imd - practice level
# imd - LHB level
# imd - Wales


# general_pc is already split by disease

general_pc <- left_join(practice_summary, streamlined, by = c("Practice" = "Practice Code")) %>%
  relocate(Register:Percent, .after = `Practice Name`) %>% mutate(Percent = round(Percent*100, 1))

# get rid of the ones we don't need

general_pc <- general_pc %>% filter(!(`Search Description` %in% 
                                      c("Any spirometry code",
                                        "0 COPD exacerbations in the last 15 months (recorded codes)",
                                        "1 COPD exacerbation in the last 15 months (recorded codes)",
                                        "2 COPD exacerbations in the last 15 months (recorded codes)",
                                        "More than 2 COPD exacerbations in the last 15 months (recorded codes)",
                                        "0 asthma attacks in the last 15 months (recorded codes)",
                                        "1 asthma attack in the last 15 months (recorded codes)",
                                        "2 asthma attacks in the last 15 months (recorded codes)",
                                        "More than 2 asthma attacks in the last 15 months (recorded codes)")))

# arrange it and then save it as it is
head(general_pc$Practice)
general_pc_arr <- general_pc %>% arrange(Register, Practice, `Search ID`)

write_xlsx(general_pc_arr, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/general_primary_care_level.xlsx")

# LHB level

general_LHB <- general_pc %>% group_by(Register, `Local Health Board`, `Search Description`) %>% 
  summarise(numerator = sum(Numerator), denominator = sum(Denominator), percent = round((numerator/denominator)*100, 1)) %>%
  arrange(Register, `Local Health Board`) %>% ungroup()

write_xlsx(general_LHB, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/general_LHB_level.xlsx")

# Country level

general_wales <- general_pc %>% group_by(Register, `Search Description`) %>% 
  summarise(numerator = sum(Numerator), denominator = sum(Denominator), percent = round((numerator/denominator)*100, 1)) %>%
  arrange(Register) %>% ungroup()

write_xlsx(general_wales, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/general_wales_level.xlsx")


# ethnicity prep: 


ethnicity_dat_AA <- ethnicity_dat_AA %>% rename(N = `Adult Asthma Patients (19+)`) %>%
  mutate(Register = "Adult Asthma")


ethnicity_dat_CYPA <- ethnicity_dat_CYPA %>% rename(N = `Child Asthma Patients (6 - 18)`) %>%
  mutate(Register = "CYP Asthma")

ethnicity_dat_COPD <- ethnicity_dat_COPD %>% rename(N = `COPD Patients`) %>%
  mutate(Register = "COPD")


ethnicity_all <- bind_rows(ethnicity_dat_AA, ethnicity_dat_CYPA, ethnicity_dat_COPD)
ethnicity_all <- left_join(ethnicity_all, ethnicity_groups, by = c("Ethnicity Group" = "Category"))
ethnicity_all <- left_join(ethnicity_all, streamlined, by = c("Practice" = "Practice Code"))

head(ethnicity_all)

# ethnicity practice level

ethnicity_pc <- ethnicity_all %>% group_by(Register, Practice) %>% mutate(Total_N = sum(N)) %>% 
  mutate(Percentage = round((N/Total_N)*100, 1)) %>%  arrange(`Ethnicity Group`) %>% 
  pivot_wider(id_cols = c(Register, Practice, `Local Health Board`, Cluster, `Practice Name`, Total_N), 
              names_from = Description, 
              values_from = c(N, Percentage),values_fill = 0) %>% ungroup() %>% arrange(Register, Practice)
  
write_xlsx(ethnicity_pc, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/ethnicity_primary_care_level.xlsx")


# ethnicity LHB level

ethnicity_LHB <- ethnicity_all %>% group_by(Register, `Local Health Board`) %>% mutate(Total_N = sum(N)) %>% 
  group_by(Register, `Local Health Board`, Description) %>% summarise(ethnicity_N = sum(N), Total_N = Total_N, 
                                                                           Percentage = round((ethnicity_N/Total_N)*100,1)) %>%
  unique() %>% rename(N = ethnicity_N) %>%
  pivot_wider(id_cols = c(Register, `Local Health Board`, Total_N), 
              names_from = Description, 
              values_from = c(N, Percentage),values_fill = 0) %>% ungroup()

write_xlsx(ethnicity_LHB, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/ethnicity_LHB_level.xlsx")



# ethnicity Wales level

ethnicity_wales <- ethnicity_all %>% group_by(Register) %>% mutate(Total_N = sum(N)) %>% 
  group_by(Register, Description) %>% summarise(ethnicity_N = sum(N), Total_N = Total_N, 
                                                                      Percentage = round((ethnicity_N/Total_N)*100,1)) %>%
  unique() %>% rename(N = ethnicity_N) %>%
  pivot_wider(id_cols = c(Register, Total_N), 
              names_from = Description, 
              values_from = c(N, Percentage),values_fill = 0) %>% ungroup()

write_xlsx(ethnicity_wales, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/ethnicity_wales_level.xlsx")


# WIMD

# wimd prep: 

wimd_dat_AA <- wimd_dat_AA %>% rename(N = `Adult Asthma Patients (19+)`) %>%
  mutate(Register = "Adult Asthma")


wimd_dat_CYPA <- wimd_dat_CYPA %>% rename(N = `Child Asthma Patients (6 - 18)`) %>%
  mutate(Register = "CYP Asthma")

wimd_dat_COPD <- wimd_dat_COPD %>% rename(N = `COPD Patients`) %>%
  mutate(Register = "COPD")


wimd_all <- bind_rows(wimd_dat_AA, wimd_dat_CYPA, wimd_dat_COPD)
wimd_all <- left_join(wimd_all, streamlined, by = c("Practice" = "Practice Code"))
wimd_all$`WIMD Decile`[is.na(wimd_all$`WIMD Decile`)] <- "Missing WIMD"
wimd_all$`WIMD Decile` <- factor(wimd_all$`WIMD Decile`, levels = c(as.character(1:10), "Missing WIMD"))

# wimd practice level
wimd_pc <- wimd_all %>% group_by(Register, Practice) %>% mutate(Total_N = sum(N)) %>% 
  mutate(Percentage = round((N/Total_N)*100, 1)) %>% arrange(`WIMD Decile`) %>% 
  pivot_wider(id_cols = c(Register, Practice, `Local Health Board`, Cluster, `Practice Name`, Total_N), 
              names_from = `WIMD Decile`, 
              values_from = c(N, Percentage),values_fill = 0) %>% ungroup() %>% arrange(Register, Practice)

write_xlsx(wimd_pc, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/wimd_primary_care_level.xlsx")


# wimd LHB level

wimd_LHB <- wimd_all %>% group_by(Register, `Local Health Board`) %>% mutate(Total_N = sum(N)) %>% 
  group_by(Register, `Local Health Board`, `WIMD Decile`) %>% summarise(wimd_N = sum(N), Total_N = Total_N, 
                                                                      Percentage = round((wimd_N/Total_N)*100,1)) %>%
  unique() %>% rename(N = wimd_N) %>%
  pivot_wider(id_cols = c(Register, `Local Health Board`, Total_N), 
              names_from = `WIMD Decile`, 
              values_from = c(N, Percentage),values_fill = 0) %>% ungroup()

write_xlsx(wimd_LHB, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/wimd_LHB_level.xlsx")



# wimd Wales level

wimd_wales <- wimd_all %>% group_by(Register) %>% mutate(Total_N = sum(N)) %>% 
  group_by(Register, `WIMD Decile`) %>% summarise(wimd_N = sum(N), Total_N = Total_N, 
                                                Percentage = round((wimd_N/Total_N)*100,1)) %>%
  unique() %>% rename(N = wimd_N) %>%
  pivot_wider(id_cols = c(Register, Total_N), 
              names_from = `WIMD Decile`, 
              values_from = c(N, Percentage),values_fill = 0) %>% ungroup()

write_xlsx(wimd_wales, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/wimd_wales_level.xlsx")


# age prep

age_dat_ast <- age_dat_ast %>% rename(N = `Asthma Patients`) %>%
  mutate(Register = "Adult Asthma")
age_dat_ast$Register[age_dat_ast$Age < 19] <- "CYP Asthma"

age_dat_copd <- age_dat_copd %>% rename(N = `COPD Patients`) %>%
  mutate(Register = "COPD")

age_all <- bind_rows(age_dat_ast, age_dat_copd)

age_all <- left_join(age_all, streamlined, by = c("Practice" = "Practice Code"))

# age_all %>% group_by(Register, Age) %>% summarise(age_n = sum(N)) %>% filter(Register == "COPD") %>%
#   ungroup() %>%  select(-Register) %>% plot() # not normal - use median and IQR

# sex practice level

totals_and_percentages <- age_all %>%
  group_by(Register, Practice) %>%
  summarise(
    Total_Males = sum(N[Sex == "Male"]),
    Total_Females = sum(N[Sex == "Female"]),
    Total = sum(N),
    Percentage_Males = round((Total_Males / Total) * 100, 1),
    Percentage_Females = round((Total_Females / Total) * 100, 1),
    .groups = 'drop'  # Drop the grouping structure
  )

sex_pc <- left_join(totals_and_percentages, streamlined, by = c("Practice" = "Practice Code"))
colnames(sex_pc)
sex_pc <- sex_pc %>% select(Register, Practice, `Local Health Board`, `Cluster`, `Practice Name`, 
                            Total, Total_Males, Total_Females, Percentage_Males, Percentage_Females)


# sex LHB level
sex_LHB <- age_all %>%
  group_by(Register, `Local Health Board`) %>%
  summarise(
    Total_Males = sum(N[Sex == "Male"]),
    Total_Females = sum(N[Sex == "Female"]),
    Total = sum(N),
    Percentage_Males = round((Total_Males / Total) * 100, 1),
    Percentage_Females = round((Total_Females / Total) * 100, 1),
    .groups = 'drop'  # Drop the grouping structure
  )

sex_LHB <- sex_LHB %>% select(Register, `Local Health Board`,
                            Total, Total_Males, Total_Females, Percentage_Males, Percentage_Females)


# sex Wales level

sex_wales <- age_all %>%
  group_by(Register) %>%
  summarise(
    Total_Males = sum(N[Sex == "Male"]),
    Total_Females = sum(N[Sex == "Female"]),
    Total = sum(N),
    Percentage_Males = round((Total_Males / Total) * 100, 1),
    Percentage_Females = round((Total_Females / Total) * 100, 1),
    .groups = 'drop'  # Drop the grouping structure
  )

sex_wales <- sex_wales %>% select(Register, Total, Total_Males, Total_Females, Percentage_Males, Percentage_Females)



# Assuming age_all has columns: Practice, Age, Sex, and N (number of individuals)

# Expand the dataset so each row represents one patient
expanded_age_all <- age_all %>%
  uncount(N)  # Repeat each row N times

# practice level for age

# Calculate median and IQR for each practice
median_and_IQR_pc <- expanded_age_all %>%
  group_by(Register, Practice) %>%
  summarise(
    Median_Age = median(Age),
    Lower_Quartile = quantile(Age, 0.25),
    Upper_Quartile = quantile(Age, 0.75),
    .groups = 'drop'  # Drop the grouping structure
  )


# Calculate median and IQR for each LHB
median_and_IQR_LHB <- expanded_age_all %>%
  group_by(Register, `Local Health Board`) %>%
  summarise(
    Median_Age = median(Age),
    Lower_Quartile = quantile(Age, 0.25),
    Upper_Quartile = quantile(Age, 0.75),
    .groups = 'drop'  # Drop the grouping structure
  )

# Calculate median and IQR for Wales
median_and_IQR_wales <- expanded_age_all %>%
  group_by(Register) %>%
  summarise(
    Median_Age = median(Age),
    Lower_Quartile = quantile(Age, 0.25),
    Upper_Quartile = quantile(Age, 0.75),
    .groups = 'drop'  # Drop the grouping structure
  )

age_and_sex_pc <- left_join(sex_pc, median_and_IQR_pc, by = c("Register", "Practice"))
age_and_sex_LHB <- left_join(sex_LHB, median_and_IQR_LHB, by = c("Register", "Local Health Board"))
age_and_sex_wales <- left_join(sex_wales, median_and_IQR_wales, by = "Register")

write_xlsx(age_and_sex_pc, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/sex_and_age_primary_care_level.xlsx")
write_xlsx(age_and_sex_LHB, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/sex_and_age_LHB_level.xlsx")
write_xlsx(age_and_sex_wales, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/sex_and_age_wales_level.xlsx")

# included practices

status_LHB <- practice_details %>%
  group_by(`Local Health Board`) %>%
  summarise(
    Total = n(),
    Total_Included = sum(Status == "Included"),
    Total_Excluded = sum(Status == "Excluded"),
    Percentage_Included = round((Total_Included / Total) * 100, 1),
    Percentage_Excluded = round((Total_Excluded / Total) * 100, 1)  # Drop the grouping structure
  )


status_wales <- practice_details %>%
  summarise(
    Total = n(),
    Total_Included = sum(Status == "Included"),
    Total_Excluded = sum(Status == "Excluded"),
    Percentage_Included = round((Total_Included / Total) * 100, 1),
    Percentage_Excluded = round((Total_Excluded / Total) * 100, 1),
    .groups = 'drop'  # Drop the grouping structure
  )

status_all <- bind_rows(status_LHB, status_wales)
status_all <- status_all[c(8, 1:7), ]
status_all[1,1] <- "Wales (all)"

status_all

write_xlsx(status_all, "Z:/Group_work/NACAP/2023/Primary_care_analysis_output/included_practices.xlsx")
