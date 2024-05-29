# sort out the drugs!

library(tidyverse)
library(openxlsx2)

setwd("Z:/Database guidelines and info/NHS TRUD/NHSBSA dm+d/nhsbsa_dmd_11.0.0_20231106000001/excel/")

# datasets
vtm <- read_xlsx("f_vtm.xlsx", sheet = "VTM")
vmp <- read_xlsx("f_vmp.xlsx", sheet = "VMP")
vmp_DrugForm <- read_xlsx("f_vmp.xlsx", sheet = "DrugForm")
vmp_DrugRoute <- read_xlsx("f_vmp.xlsx", sheet = "DrugRoute")
vmp_vpi <- read_xlsx("f_vmp.xlsx", sheet = "VPI")
amp <- read_xlsx("f_amp.xlsx", sheet = "AmpType")
bnf <- read_xlsx("f_bnf.xlsx", sheet = "BNF_Vmp")

# look ups
ROUTECD <- read_xlsx("f_lookup.xlsx", sheet = "Route")
FORMCD <- read_xlsx("f_lookup.xlsx", sheet = "Form")
UOMCD <- read_xlsx("f_lookup.xlsx", sheet = "UoM")

colnames(UOMCD)



# # # vtm cleaning

head(vtm)
vtm <- vtm %>% rename(CHEMICAL_NAME = NM) %>% 
  filter(is.na(INVALID)) %>% select(VTMID, CHEMICAL_NAME)

# # # drug route cleaning

vmp_DrugRoute <- left_join(vmp_DrugRoute, rename(ROUTECD, ROUTECD = CD, ROUTE_DESC = DESC), by = "ROUTECD")
vmp_DrugRoute <- vmp_DrugRoute %>% select(VPID, ROUTECD, ROUTE_DESC)

vmp_DrugRoute %>% select(VPID) %>% unique() %>% nrow() # not unique - can have multiple routes

vmp_DrugRoute %>% group_by(VPID) %>% add_count() %>% filter(n>1) %>% ungroup() %>% select(ROUTE_DESC) %>% table()
vmp_DrugRoute %>% group_by(VPID) %>% add_count() %>% filter(n>1) %>% ungroup() %>% arrange(VPID, ROUTE_DESC) %>%
  print(n=1000) # select(ROUTE_DESC) %>% table()


# # # drug form cleaning

colnames(vmp_DrugForm)
colnames(FORMCD)

vmp_DrugForm <- left_join(vmp_DrugForm, rename(FORMCD, FORMCD = CD, FORM_DESC = DESC), by = "FORMCD")

vmp_DrugForm %>% select(VPID) %>% unique() %>% nrow() # all unique

# # # amp cleaning

amp <- amp %>% filter(is.na(INVALID))


# # # vmp cleaning



colnames(vmp)
head(vmp)

vmp %>% select(NM, DF_INDCD:UNIT_DOSE_UOMCD) %>% filter(DF_INDCD != 3) %>% head(38)

# remove invalid vmps
vmp <- vmp %>% filter(is.na(INVALID))

# only keep what is needed for the vmp  - see the end of the script for potential things
# that could be added

colnames(vmp)


vmp <- vmp %>% select(VPID, VTMID, GENERIC_NAME = NM) 
nrow(vmp)

vmp <- left_join(vmp, vtm, by = "VTMID")
nrow(vmp)

vmp <- left_join(vmp, vmp_DrugForm, by = "VPID")
nrow(vmp)

vmp <- left_join(vmp, vmp_DrugRoute, by = "VPID")
nrow(vmp) # more rows created as one VMP can have multiple routes

vmp <- left_join(vmp, bnf, by = "VPID")
nrow(vmp)

colnames(vmp)

# now I think we are in a position to single out the drugs.

# let's start out with just the antibiotics.

# lets do the three stages.

#change the working directory

setwd("Z:/Group_work/NACAP/2023/Primary care/dmd_drugs/")

# # # # # # # stage 1

# antibiotics
vmp %>% filter(str_starts(BNF, "0501")) %>% 
  filter(ROUTE_DESC == "Oral") %>% 
  select(CHEMICAL_NAME) %>% 
  unique() %>% 
  write.csv("antibiotics_stage1.csv", row.names = FALSE)


# inhalers

#agonists
vmp %>% filter(str_starts(BNF, "030101")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("B2A_stage1.csv", row.names = FALSE)

# muscarinic antagonists
vmp %>% filter(str_starts(BNF, "030102")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("antimuscarinics_stage1.csv", row.names = FALSE)

# ICS
vmp %>% filter(str_starts(BNF, "030200")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("ICS_stage1.csv", row.names = FALSE)

# theophyllines
vmp %>% filter(str_starts(BNF, "030103")) %>% 
  filter(ROUTE_DESC == "Oral") %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("theophyllines_stage1.csv", row.names = FALSE)

# compounds
vmp %>% filter(str_starts(BNF, "030104")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("compounds_stage1.csv", row.names = FALSE)

# oral steroids 
vmp %>% filter(str_starts(BNF, "060302")) %>% 
  filter(ROUTE_DESC == "Oral") %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("oral_steroids_stage1.csv", row.names = FALSE)



# # # # # # stage 1.5, 2 and 3
# Once Jenni has looked through the drugs of each section and has marked them with 1s and 0s,
# we just double check that there are not any more combinations of drugs out there that
# we have missed somehow. 

# antibiotics

vmp %>% filter(CHEMICAL_NAME %in% c("Amoxicillin", "Doxycycline", "Moxifloxacin", "Cefixime",
                                    "Azithromycin", "Co-amoxiclav", "Ciprofloxacin", "Clarithromycin",
                                    "Cefalexin", "Cefradine", "Cefuroxime", "Levofloxacin", 
                                    "Erythromycin ethyl succinate", "Cefaclor", "Erythromycin stearate",
                                    "Erythromycin")) %>%
  select(CHEMICAL_NAME) %>% unique() -> in_orig

vmp %>% filter(str_detect(CHEMICAL_NAME, paste(c("Amoxicillin", "Doxycycline", "Moxifloxacin", "Cefixime",
                                    "Azithromycin", "Co-amoxiclav", "Ciprofloxacin", "Clarithromycin",
                                    "Cefalexin", "Cefradine", "Cefuroxime", "Levofloxacin", 
                                    "Erythromycin ethyl succinate", "Cefaclor", "Erythromycin stearate",
                                    "Erythromycin"), collapse = "|"))) %>% filter(ROUTE_DESC == "Oral") %>%
  select(CHEMICAL_NAME) %>% unique() %>% 
  filter(!(CHEMICAL_NAME %in% in_orig$CHEMICAL_NAME))
  # write.csv("antibiotics_stage1.5.csv")

# Everything is fine so we just go to stages 2 and three

# stage 2
vmp %>% filter(CHEMICAL_NAME %in% c("Amoxicillin", "Doxycycline", "Moxifloxacin", "Cefixime",
                                    "Azithromycin", "Co-amoxiclav", "Ciprofloxacin", "Clarithromycin",
                                    "Cefalexin", "Cefradine", "Cefuroxime", "Levofloxacin", 
                                    "Erythromycin ethyl succinate", "Cefaclor", "Erythromycin stearate",
                                    "Erythromycin")) %>% filter(ROUTE_DESC == "Oral") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("antibiotics_stage2.csv", row.names = FALSE)

# stage 3
vmp %>% filter(CHEMICAL_NAME %in% c("Amoxicillin", "Doxycycline", "Moxifloxacin", "Cefixime",
                                    "Azithromycin", "Co-amoxiclav", "Ciprofloxacin", "Clarithromycin",
                                    "Cefalexin", "Cefradine", "Cefuroxime", "Levofloxacin", 
                                    "Erythromycin ethyl succinate", "Cefaclor", "Erythromycin stearate",
                                    "Erythromycin")) %>% filter(ROUTE_DESC == "Oral") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  write.csv("antibiotics_stage3.csv", row.names = FALSE)

head(amp)

# SABA    # stage 1.5
vmp %>% filter(CHEMICAL_NAME %in% c("Salbutamol", "Terbutaline", "Fenoterol")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique() -> in_orig

vmp %>% filter(str_detect(CHEMICAL_NAME, paste(c("Salbutamol", "Terbutaline", "Fenoterol"), collapse = "|"))) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  select(CHEMICAL_NAME) %>% unique() %>% 
  filter(!(CHEMICAL_NAME %in% in_orig$CHEMICAL_NAME))  # (we find a SABA_ICS inhaler)

# stage 2
vmp %>% filter(CHEMICAL_NAME %in% c("Salbutamol", "Terbutaline", "Fenoterol")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("SABA_stage2.csv", row.names = FALSE)

# stage 3
vmp %>% filter(CHEMICAL_NAME %in% c("Salbutamol", "Terbutaline", "Fenoterol")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) %>% 
  write.csv("SABA_stage3.csv", row.names = FALSE)


# SABA_ICS

# Ipratropium = SAMA
# Salbutamol = SABA

# stage 2
vmp %>% filter(CHEMICAL_NAME %in% c("Salbutamol + Beclometasone")) %>%
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("SABA_ICS_stage2.csv", row.names = FALSE)

# stage 3
vmp %>% filter(CHEMICAL_NAME %in% c("Salbutamol + Beclometasone")) %>%
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) %>% 
  write.csv("SABA_ICS_stage3.csv", row.names = FALSE)

# LABA

# have a double-check of the different drugs out there - all the other ones are combinations gotten later
vmp %>% filter(str_detect(CHEMICAL_NAME, paste(c("Olodaterol", "Indacaterol", "Salmeterol", "Formoterol"),
                                                 collapse = "|"))) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  filter(!(CHEMICAL_NAME %in% in_orig$CHEMICAL_NAME)) 
 

vmp %>% filter(CHEMICAL_NAME %in% c("Olodaterol", "Indacaterol", "Salmeterol", "Formoterol")) %>%
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("LABA_stage2.csv", row.names = FALSE)

vmp %>% filter(CHEMICAL_NAME %in% c("Olodaterol", "Indacaterol", "Salmeterol", "Formoterol")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) %>% 
  write.csv("LABA_stage3.csv", row.names = FALSE)

# remember - this is in LABA but should be in triple Beclometasone + Formoterol + Glycopyrronium bromide


# SAMA
vmp %>% filter(str_detect(CHEMICAL_NAME, "Ipratropium"))  %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique() 

vmp %>% filter(CHEMICAL_NAME %in% c("Ipratropium")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("SAMA_stage2.csv", row.names = FALSE)

vmp %>% filter(CHEMICAL_NAME %in% c("Ipratropium")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) %>% 
  write.csv("SAMA_stage3.csv", row.names = FALSE)



# let's not forget SABA-SAMA

vmp %>% filter(str_detect(CHEMICAL_NAME, "Terbutaline"))  %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique() 


vmp %>% filter(CHEMICAL_NAME %in% c("Fenoterol + Ipratropium", "Salbutamol + Ipratropium")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("SABA_SAMA_stage2.csv", row.names = FALSE)

vmp %>% filter(CHEMICAL_NAME %in% c("Fenoterol + Ipratropium", "Salbutamol + Ipratropium")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) %>% 
  write.csv("SABA_SAMA_stage3.csv", row.names = FALSE)

# LAMA

vmp %>% filter(str_starts(BNF, "030102")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique()

vmp %>% filter(CHEMICAL_NAME %in% c("Tiotropium", "Umeclidinium bromide", "Glycopyrronium bromide",
                                    "Aclidinium bromide", "Oxitropium")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("LAMA_stage2.csv", row.names = FALSE)

vmp %>% filter(CHEMICAL_NAME %in% c("Tiotropium", "Umeclidinium bromide", "Glycopyrronium bromide",
                                    "Aclidinium bromide", "Oxitropium")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) %>% 
  write.csv("LAMA_stage3.csv", row.names = FALSE)




# ICS
vmp %>% filter(str_starts(BNF, "030200")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique() 

vmp %>% filter(CHEMICAL_NAME %in% c("Budesonide", "Beclometasone", "Ciclesonide",
                                    "Fluticasone", "Mometasone")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("ICS_stage2.csv", row.names = FALSE)

vmp %>% filter(CHEMICAL_NAME %in% c("Budesonide", "Beclometasone", "Ciclesonide",
                                    "Fluticasone", "Mometasone")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) %>% 
  write.csv("ICS_stage3.csv", row.names = FALSE)


# LAMA-ICS - doesn't exist

# LAMAS:

# Tiotropium				      l
# Umeclidinium bromide		l
# Glycopyrronium bromide	l
# Aclidinium bromide			l
# Ipratropium				      s
# Oxitropium				      l

# LABAs:

# Beclometasone + Formoterol + Glycopyrronium bromide								triple
# Olodaterol								l
# Indacaterol								l
# Salbutamol								s
# Adrenaline								0
# Salmeterol								l
# Formoterol								l
# Fenoterol								  s
# Terbutaline								s

# vilanterol is LABA but only used in combination
# all the ICS combinations (apart from triples) are ICS - LABA,
# apart from the SABA-ICS ones

# salbutamol is SABA


# LABA-ICS

vmp %>% filter(str_starts(BNF, "030200")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique() 

vmp %>% filter(CHEMICAL_NAME %in% c("Fluticasone + Salmeterol", "Budesonide + Formoterol", "Indacaterol + Mometasone",
                                    "Fluticasone + Formoterol", "Beclometasone + Formoterol", "Fluticasone + Vilanterol")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("LABA_ICS_stage2.csv", row.names = FALSE)

vmp %>% filter(CHEMICAL_NAME %in% c("Fluticasone + Salmeterol", "Budesonide + Formoterol", "Indacaterol + Mometasone",
                                    "Fluticasone + Formoterol", "Beclometasone + Formoterol", "Fluticasone + Vilanterol")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) %>% 
  write.csv("LABA_ICS_stage3.csv", row.names = FALSE)

# LABA-LAMA

# Glycopyrronium bromide + Formoterol
# Tiotropium + Olodaterol
# Fenoterol + Ipratropium
# Aclidinium bromide + Formoterol
# Indacaterol + Glycopyrronium bromide
# Umeclidinium bromide + Vilanterol

# compounds
vmp %>% filter(str_starts(BNF, "030104")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("compounds_stage1.csv", row.names = FALSE)

vmp %>% filter(CHEMICAL_NAME %in% c("Glycopyrronium bromide + Formoterol", "Tiotropium + Olodaterol",
                                    "Fenoterol + Ipratropium", "Aclidinium bromide + Formoterol",
                                    "Indacaterol + Glycopyrronium bromide", "Umeclidinium bromide + Vilanterol")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("LABA_LAMA_stage2.csv", row.names = FALSE)

vmp %>% filter(CHEMICAL_NAME %in% c("Glycopyrronium bromide + Formoterol", "Tiotropium + Olodaterol",
                                    "Fenoterol + Ipratropium", "Aclidinium bromide + Formoterol",
                                    "Indacaterol + Glycopyrronium bromide", "Umeclidinium bromide + Vilanterol")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) %>% 
  write.csv("LABA_LAMA_stage3.csv", row.names = FALSE)




# Triple is made up of different things from various other chapters - not clear why its separated out

vmp %>% filter(CHEMICAL_NAME %in% c("Beclometasone + Formoterol + Glycopyrronium bromide", 
                                    "Formoterol + Glycopyrronium bromide + Budesonide", 
                                    "Mometasone + Glycopyrronium bromide + Indacaterol", 
                                    "Fluticasone + Umeclidinium bromide + Vilanterol")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("Triple_stage2.csv", row.names = FALSE)

vmp %>% filter(CHEMICAL_NAME %in% c("Beclometasone + Formoterol + Glycopyrronium bromide", 
                                    "Formoterol + Glycopyrronium bromide + Budesonide", 
                                    "Mometasone + Glycopyrronium bromide + Indacaterol", 
                                    "Fluticasone + Umeclidinium bromide + Vilanterol")) %>% 
  filter(ROUTE_DESC == "Inhalation") %>% 
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  arrange(GENERIC_NAME) %>% 
  write.csv("Triple_stage3.csv", row.names = FALSE)





# theophyllines
vmp %>% filter(str_starts(BNF, "030103")) %>% 
  filter(ROUTE_DESC == "Oral") %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("theophyllines_stage1.csv", row.names = FALSE)

vmp %>% filter(str_detect(CHEMICAL_NAME, paste(c("Theophylline", "Aminophylline"), collapse = "|"))) %>% 
  filter(ROUTE_DESC == "Oral") %>%
  select(CHEMICAL_NAME) %>% unique() %>% 
  filter(!(CHEMICAL_NAME %in% in_orig$CHEMICAL_NAME))
# write.csv("antibiotics_stage1.5.csv")

# Everything is fine so we just go to stages 2 and three

# stage 2
vmp %>% filter(CHEMICAL_NAME %in% c("Theophylline", "Aminophylline")) %>% 
  filter(ROUTE_DESC == "Oral") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("theophyllines_stage2.csv", row.names = FALSE)

# stage 3
vmp %>% filter(CHEMICAL_NAME %in% c("Theophylline", "Aminophylline")) %>% 
  filter(ROUTE_DESC == "Oral") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  write.csv("theophyllines_stage3.csv", row.names = FALSE)


# oral steroids 
vmp %>% filter(str_starts(BNF, "060302")) %>% 
  filter(ROUTE_DESC == "Oral") %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("oral_steroids_stage1.csv", row.names = FALSE)

vmp %>% filter(str_detect(CHEMICAL_NAME, paste(c("Prednisolone", 
                                                 "Dexamethasone", "Prednisone"), collapse = "|"))) %>% 
  filter(ROUTE_DESC == "Oral") %>%
  select(CHEMICAL_NAME) %>% unique() %>% 
  filter(!(CHEMICAL_NAME %in% in_orig$CHEMICAL_NAME))

vmp %>% filter(CHEMICAL_NAME %in% c("Prednisolone", 
                                    "Dexamethasone", "Prednisone")) %>% 
  filter(ROUTE_DESC == "Oral") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% arrange(GENERIC_NAME) %>%
  write.csv("oral_steroids_stage2.csv", row.names = FALSE)


vmp %>% filter(CHEMICAL_NAME %in% c("Prednisolone", 
                                    "Dexamethasone", "Prednisone")) %>% 
  filter(ROUTE_DESC == "Oral") %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME) %>% unique() %>% left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, DESC) %>% unique() %>%
  write.csv("oral_steroids_stage3.csv", row.names = FALSE)



# stop smoking drugs

vmp %>% filter(str_starts(BNF, "04100200")) %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("smoking_cessation_stage1.csv", row.names = FALSE) # nicobrevin is withdrawn, has 4 ingredients but no evidence

vmp %>% filter(str_starts(BNF, "04100200")) %>% 
  select(VPID, GENERIC_NAME, CHEMICAL_NAME, FORM_DESC) %>% arrange(GENERIC_NAME) %>%
  filter(GENERIC_NAME != "Generic Nicobrevin capsules") %>%
  write.csv("smoking_cessation_stage2.csv", row.names = FALSE) # nicobrevin is withdrawn, has 4 ingredients but no evidence

vmp %>% filter(str_starts(BNF, "04100200")) %>% 
  select(VPID, GENERIC_NAME, CHEMICAL_NAME, FORM_DESC) %>% arrange(GENERIC_NAME) %>%
  filter(GENERIC_NAME != "Generic Nicobrevin capsules") %>%
  left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, FORM_DESC, DESC) %>% unique() %>%
  write.csv("smoking_cessation_stage3.csv", row.names = FALSE)

# flu vaccine

vmp %>% filter(str_starts(BNF, "14040000")) %>% 
  select(CHEMICAL_NAME) %>%
  unique() %>% 
  write.csv("flu_vaccine_stage1.csv", row.names = FALSE) # nicobrevin is withdrawn, has 4 ingredients but no evidence



vmp %>% filter(str_starts(BNF, "14040000")) %>% 
  filter(str_detect(CHEMICAL_NAME, paste(c("Influenza vaccine", "Influenza H1N1 vaccine"), collapse = "|"))) %>%
  select(CHEMICAL_NAME) %>% unique()

vmp %>% filter(str_starts(BNF, "14040000")) %>% 
  filter(str_detect(CHEMICAL_NAME, paste(c("Influenza vaccine", "Influenza H1N1 vaccine"), collapse = "|"))) %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME, FORM_DESC) %>% arrange(GENERIC_NAME) %>% unique() %>%
  write.csv("flu_vaccine_stage2.csv", row.names = FALSE) # 

vmp %>% filter(str_starts(BNF, "14040000")) %>% 
  filter(str_detect(CHEMICAL_NAME, paste(c("Influenza vaccine", "Influenza H1N1 vaccine"), collapse = "|"))) %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME, FORM_DESC) %>% arrange(GENERIC_NAME) %>% unique() %>%
  left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, FORM_DESC, DESC) %>% unique() %>%
  write.csv("flu_vaccine_stage3.csv", row.names = FALSE)

# Nearly forgot about LRTAs 

vmp %>%  
  filter(str_detect(CHEMICAL_NAME, paste(c("Montelukast", "Zafirlukast"), collapse = "|"))) %>%
  select(CHEMICAL_NAME) %>% unique()

vmp %>%  
  filter(str_detect(CHEMICAL_NAME, paste(c("Montelukast", "Zafirlukast"), collapse = "|"))) %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME, FORM_DESC) %>% arrange(GENERIC_NAME) %>% unique() %>%
  write.csv("LTRA_stage2.csv", row.names = FALSE) # 

vmp %>%  
  filter(str_detect(CHEMICAL_NAME, paste(c("Montelukast", "Zafirlukast"), collapse = "|"))) %>%
  select(VPID, GENERIC_NAME, CHEMICAL_NAME, FORM_DESC) %>% arrange(GENERIC_NAME) %>% unique() %>%
  left_join(., amp, by = "VPID") %>% 
  select(VPID, APID, GENERIC_NAME, CHEMICAL_NAME, FORM_DESC, DESC) %>% unique() %>%
  write.csv("LTRA_stage3.csv", row.names = FALSE)

