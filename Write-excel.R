# Library ####
library(purrr)
library(flextable)
library(writexl)
library(gtsummary)

# 1. Availability of infrastructure for surgical services in health facilities in baseline survey, Cambodia, 2024 ####
sca_infra = clean.soa %>% select(2:3,9:11, matches(c("infrastructure_")), -matches("numeric_"))

infrastructure.availability <- sca_infra %>% 
  select(-c(70)) %>% 
  mutate(across(-c(1:5), ~case_when(
    str_detect(.x, "100%") ~ 5,
    str_detect(.x, "76-99%") ~ 4,
    str_detect(.x, "51-75%") ~ 3,
    str_detect(.x, "26-50%") ~ 2,
    str_detect(.x, "1-25%") ~ 1,
    str_detect(.x, "0%") ~ 0,
    TRUE ~ NA_real_ )),
    totalscore = rowSums(across(c(6:69)), na.rm = TRUE),
    score = round((totalscore/((length(names(.))-6)*5)) *100, digits = 2),
    `infrastructure availability` = case_when(score == 0 ~ "Never",
                                              score > 0 & score <= 25 ~ "Rarely",
                                              score > 25 & score <= 50 ~ "Sometimes",
                                              score > 50 & score <= 75 ~ "Often",
                                              score > 75 & score < 100 ~ "Almost always",
                                              score >= 100 ~ "Always")) %>% 
  select(c(1:5,`infrastructure availability`)) %>% 
  group_by(geninfo_hcfacilitylevel, `infrastructure availability`) %>% 
  summarise(`number of health facility` = n(), .groups = "drop_last") %>% #  mutate(`shared percent` = round(`number of health facility` / sum(`number of health facility`) *100,digits = 2))
  pivot_wider(id_cols = geninfo_hcfacilitylevel, names_from = `infrastructure availability`, values_from = `number of health facility`) %>% 
  mutate(Total = rowSums(across(c(1:6)), na.rm = TRUE),
         geninfo_hcfacilitylevel = factor( geninfo_hcfacilitylevel,
                                           levels = c("National Hospital", "Provincial Hospital", "District Hospital", "Private", "Total"))) %>% 
  select(geninfo_hcfacilitylevel, Always,`Almost always`, Often , Sometimes, Rarely,Never,Total ) %>% 
  arrange(geninfo_hcfacilitylevel)

# 2. Distribution of SOA service providers in the baseline surveyed health facilities ####
#workforce.dist = totalden.wforce %>% select(1,3)
#totals.wdist <- data.frame(workforces = "Total",tvalue = sum(workforce.dist$tvalue, na.rm = TRUE) )
workforce.distribution = den.wforce.adj$byhfl %>% ungroup() %>% select(1,3:5) %>% 
  pivot_wider(names_from = geninfo_hcfacilitylevel, values_from = c(staff, adjusted.staff)) %>% 
  select(1,`National HF`=3,`Provincial HF`=5,`District HF`=2,`Private HF` =4,`Adjusted Private HF` =8) %>% 
  mutate(`Public HF`= `National HF`+`Provincial HF`+`District HF`,
         `Adjusted Private WF` = round(`Adjusted Private HF`, digits = 2),
         `Total WF` = `Public HF`+`Private HF`,
         `Total adjusted WF` = `Public HF`+`Adjusted Private WF`) %>% 
  select(1:4,7,5:6,8:10)

workforce.distribution.hft = den.wforce.adj$byhft %>% ungroup() %>% select(1,3:5) %>% 
  pivot_wider(names_from = geninfo_hcfacilitytype, values_from = c(staff, adjusted.staff)) %>% 
  select(1,`CPA3`=3,`CPA2`=2,`Private Hospital`=6,Policlinic =5, Clinic=4,
         `Adjusted private Hospital`=11,`Adjusted Policlinic` =10, `Adjusted Clinic`=9) %>% 
  mutate(`Public HF`= `CPA3`+`CPA2`,
         `Private HF` = `Private Hospital`+Policlinic+Clinic,
         `Adjusted Private WF` = round(`Adjusted private Hospital`+`Adjusted Policlinic`+`Adjusted Clinic`, digits = 2),
         `Total WF` = `Public HF`+`Private HF`,
         `Total adjusted WF` = `Public HF`+`Adjusted Private WF`)

workforce.distribution.prov = den.wforce.adj$byprovince %>% select(1:5) %>% 
  pivot_wider(names_from = geninfo_hospitalfund, values_from = c(staff, adjusted.staff)) %>% 
  select(Province=1,2,`Public HF`=4, `Private HF`=3, `Adjusted Private HF`=5) %>% 
  mutate(`Adjusted Private WF` = round(`Adjusted Private HF`, digits = 2),
         `Total WF` = replace_na(`Public HF`,0) + replace_na(`Private HF`,0),
         `Total adjusted WF` = replace_na(`Public HF`,0) + replace_na(`Adjusted Private WF`,0))

SOAdist.province = workforce.distribution.prov %>%
  select(1:3,6,8) %>% 
  filter(workforces %in% c("Anaesthesiologists","General surgeons","Obstetricians gynaecologists",
                           "Paediatric surgeons","Total workforce","Specilist wf")) %>% 
  pivot_wider(names_from = workforces, values_from = c(`Public HF`, `Adjusted Private WF`, `Total adjusted WF`))

tworkforce.distribution <- data.frame( workforces = "Total",
                                            `National HF` = sum(workforce.distribution$`National HF`, na.rm = TRUE),
                                            `Provincial HF` = sum(workforce.distribution$`Provincial HF`, na.rm = TRUE), 
                                            `District HF` = sum(workforce.distribution$`District HF`, na.rm = TRUE),
                                            `Public HF` = sum(workforce.distribution$`Public HF`, na.rm = TRUE),
                                            `Private HF` = sum(workforce.distribution$`Private HF`, na.rm = TRUE),
                                            `Adjusted Private HF` = sum(workforce.distribution$`Adjusted Private HF`, na.rm = TRUE),
                                            `Adjusted Private WF` = sum(workforce.distribution$`Adjusted Private WF`, na.rm = TRUE),
                                            `Total WF` = sum(workforce.distribution$`Total WF`, na.rm = TRUE),
                                            `Total adjusted WF` = sum(workforce.distribution$`Total adjusted WF`, na.rm = TRUE),
                                            check.names = FALSE)

total.workforce.distribution <- bind_rows(workforce.distribution, tworkforce.distribution)

pubhf.percap = round((total.workforce.distribution$`Public HF`[[19]]/province$Total[nrow(province)]) * 100000, digits = 0)
gap.pubhf.percap = round(20 - pubhf.percap, digits = 0)
privhf.percap = round((total.workforce.distribution$`Private HF`[[19]]/province$Total[nrow(province)]) * 100000, digits = 0)
gap.privhf.percap = round(20 - privhf.percap, digits = 0)

# 3. Access to healthcare facilities within a 2-hour timeframe in the baseline surveyed health facilities ####
accesstohf2hours = clean.soa %>% 
  select(2:3,9:11, access2hfwithin2hour =  which(str_detect(names(.), "patients.reach.within.2.hours"))) %>% 
  mutate(across(-c(1:5), ~case_when(
    str_detect(.x, "100%") ~ "Always",
    str_detect(.x, "76-99%") ~ "Almost always",
    str_detect(.x, "51-75%") ~ "Often",
    str_detect(.x, "26-50%") ~ "Sometimes",
    str_detect(.x, "1-25%") ~ "Rarely",
    str_detect(.x, "0%") ~ "Never",
    TRUE ~ .x )))

acc.tohf.within2hour = accesstohf2hours %>% 
#  filter(geninfo_hospitalfund == "Public") %>% 
  group_by(geninfo_hcfacilitylevel, access2hfwithin2hour) %>% 
  summarise( `number of health facility` = n(), .groups = 'drop_last') %>% 
  pivot_wider(id_cols = geninfo_hcfacilitylevel, names_from = access2hfwithin2hour, values_from = `number of health facility`) %>% 
  mutate(Total = rowSums(across(c(1:6)), na.rm = TRUE),
         geninfo_hcfacilitylevel = factor( geninfo_hcfacilitylevel,
                                           levels = c("National Hospital", "Provincial Hospital", "District Hospital", "Private", "Total"))) %>% 
  select(geninfo_hcfacilitylevel, Always,`Almost always`, Often , Sometimes, Rarely,Never,Total ) %>% 
  arrange(geninfo_hcfacilitylevel)

# Create a dataframe with specified values
totals.access2 <- data.frame( geninfo_hcfacilitylevel = "Total",
                              Always = sum(acc.tohf.within2hour$Always, na.rm = TRUE),
                              `Almost always` = sum(acc.tohf.within2hour$`Almost always`, na.rm = TRUE), 
                              Often = sum(acc.tohf.within2hour$Often, na.rm = TRUE), 
                              Sometimes = sum(acc.tohf.within2hour$Sometimes, na.rm = TRUE), 
                              Rarely = sum(acc.tohf.within2hour$Rarely, na.rm = TRUE),
                              Never = sum(acc.tohf.within2hour$Never, na.rm = TRUE),
                              Total = sum(acc.tohf.within2hour$Total, na.rm = TRUE),
                              check.names = FALSE)
access.2hf.within2hour <- bind_rows(acc.tohf.within2hour, totals.access2)
# 4. Surgical volume per level of facility in the baseline surveyed health facilities ####
# Surgical volume by type 
allsurgeryvolume = tserdel$byhfl %>% 
  pivot_wider(id_cols = geninfo_hcfacilitylevel, names_from = main.structure, values_from = `staff`) %>% 
  mutate(Total = rowSums(across(c(1:6)), na.rm = TRUE),
         geninfo_hcfacilitylevel = factor( geninfo_hcfacilitylevel,
                                           levels = c("National Hospital", "Provincial Hospital", "District Hospital", "Private", "Total"))) %>% 
  select(geninfo_hcfacilitylevel, `Total surgery` =2 ,Specialty, `Injury-related` = 5,
         `Obstetrics Gynaecology` = 6,`Non trauma orthopedic`=4, `Urgent cases`=3,Total ) %>% 
  arrange(geninfo_hcfacilitylevel)

# Create a dataframe with specified values
total.volume <- data.frame( geninfo_hcfacilitylevel = "Total",
                            `Total surgery` = sum(allsurgeryvolume$`Total surgery`, na.rm = TRUE),
                            Specialty = sum(allsurgeryvolume$Specialty, na.rm = TRUE), 
                            `Injury-related` = sum(allsurgeryvolume$`Injury-related`, na.rm = TRUE),
                            `Obstetrics Gynaecology` = sum(allsurgeryvolume$`Obstetrics Gynaecology`, na.rm = TRUE),
                            `Non trauma orthopedic` = sum(allsurgeryvolume$`Non trauma orthopedic`, na.rm = TRUE),
                            `Urgent cases` = sum(allsurgeryvolume$`Urgent cases`, na.rm = TRUE),
                            Total = sum(allsurgeryvolume$Total, na.rm = TRUE),
                            check.names = FALSE)

surgeryvolume <- bind_rows(allsurgeryvolume, total.volume)

# Other surgical volume
surgical.volume = clean.soa %>% 
  select(2:3,9:11, matches(c("serprocedure_|sersurvol_|perioperativedeathslastyear"))) %>% 
  mutate(across(-c(1:5), ~ as.numeric(str_extract(as.character(.x), "\\d+"))),
         `Total surgical volume` = rowSums(across(c(6:58)), na.rm = TRUE),
         `operating surgeries` = case_when(`Total surgical volume` > 0 ~ "Surgical Procedure",
                                           `Total surgical volume` == 0 ~ "No Surgical Procedure")) %>% 
  select(c(1:5,`operating surgeries`,`Total surgical volume`)) %>% 
#  group_by(geninfo_province, geninfo_hospitalfund, `operating surgeries`) %>%
#  group_by(geninfo_hospitalfund, `operating surgeries`) %>%
#  group_by(geninfo_province, `operating surgeries`) %>%
#  group_by(geninfo_hcfacilitytype, `operating surgeries`) %>%
  group_by(geninfo_hcfacilitylevel,`operating surgeries`) %>% 
  summarise(`Total number of health facility operating surgeries` = n(),
            `Total surgical volume` = sum(`Total surgical volume`, na.rm = TRUE),
            .groups = "drop_last") %>% 
  filter(`operating surgeries` == "Surgical Procedure") %>% 
  select(-2) %>% 
  mutate(geninfo_hcfacilitylevel = factor( geninfo_hcfacilitylevel,
                                           levels = c("National Hospital", "Provincial Hospital", "District Hospital", "Private", "Total"))) %>% 
  arrange(geninfo_hcfacilitylevel)

# Create a dataframe with specified values
totals.surgvol <- data.frame( geninfo_hcfacilitylevel = "Total",  
                             `Total number of health facility operating surgeries` = sum(surgical.volume$`Total number of health facility operating surgeries`, na.rm = TRUE),
                             `Total surgical volume` = sum(surgical.volume$`Total surgical volume` , na.rm = TRUE),
                             check.names = FALSE)
surgical_volume <- bind_rows(surgical.volume, totals.surgvol)


# 5. Surgical volume of Bellwether procedures in the baseline surveyed health facilities ####
sbw <- cond.bellwether %>% 
  select(c(2,5,staff = 6)) %>% 
  filter(staff != 0)

# Test normality distribution before runing statistics testing
#ggplot(sbw, aes(sample = staff)) +
#  stat_qq() +
#  stat_qq_line(color = "red") +
#  labs(title = "QQ Plot for Surgical volume of Bellwether procedures") +
#  theme_minimal()

# Step 1: Summarize by type and facility
sbw.summary <- sbw %>%
  group_by(type, geninfo_hcfacilitylevel) %>%
  summarise( `Total Surgeries` = sum(staff, na.rm = TRUE),
             med = median(staff, na.rm = TRUE),
             iqr_low = quantile(staff, 0.25, na.rm = TRUE),
             iqr_high = quantile(staff, 0.75, na.rm = TRUE),
             .groups = "drop" ) %>%
  mutate(`Median (IQR)` = paste0(med, " (", iqr_low, "–", iqr_high, ")")) %>%
  select(type, Facility = geninfo_hcfacilitylevel, `Total Surgeries`, `Median (IQR)`)

# Step 2: Add total row per type
sbw.totals <- sbw.summary %>%
  group_by(type) %>%
  summarise(  Facility = "Total",
              `Total Surgeries` = sum(`Total Surgeries`),
              med = median(as.numeric(gsub(" .*", "", `Median (IQR)`))),
              iqr_low = median(as.numeric(gsub(".*\\(|–.*", "", `Median (IQR)`))),
              iqr_high = median(as.numeric(gsub(".*–|\\)", "", `Median (IQR)`))),
              .groups = "drop" ) %>%  
  mutate(`Median (IQR)` = paste0(med,  " (", iqr_low, "–", iqr_high, ")")) %>%
  select(type, Facility, `Total Surgeries`, `Median (IQR)`)

# Step 3: Combine summaries and totals
surgical_bellwether <- bind_rows(sbw.summary, sbw.totals) %>%
  arrange(type, match(Facility, c("National Hospital", "Provincial Hospital", "District Hospital", "Private", "Total")))

# Step 4: Make the flextable
ft <- flextable(surgical_bellwether) %>%
  bold(i = ~ Facility %in% c("Laparotomy", "Caesarean Delivery", "Open Fracture", "Total"), bold = TRUE) %>%
  align(align = "center", part = "all") %>%
  set_header_labels(
    Facility = "Facility Level",
    `Total Surgeries` = "Total Surgeries",
    `Median (IQR)` = "Median (IQR)"
  ) %>%
  autofit()

# 6. Use of surgical safety checklist for surgical procedures in the baseline surveyed health facilities ##### 
# Frequency of use of WHO surgical safety checklist
who.safety = clean.soa %>% 
  select(2:3,9:11, checklist =  which(str_detect(names(.), "safety.checklist.used"))) %>% 
  mutate(across(-c(1:5), ~case_when(
    str_detect(.x, "100%") ~ "Always",
    str_detect(.x, "76-99%") ~ "Almost always",
    str_detect(.x, "51-75%") ~ "Often",
    str_detect(.x, "26-50%") ~ "Sometimes",
    str_detect(.x, "1-25%") ~ "Rarely",
    str_detect(.x, "0%") ~ "Never",
    TRUE ~ .x )))

who.checklist = who.safety %>% 
#  filter(geninfo_hospitalfund == "Public") %>% 
  group_by(geninfo_hcfacilitylevel, checklist) %>% 
  summarise(`number of health facility` = n(), .groups = 'drop_last') %>% 
  pivot_wider(id_cols = geninfo_hcfacilitylevel, names_from = checklist, values_from = `number of health facility`) %>% 
  mutate(Total = rowSums(across(c(1:6)), na.rm = TRUE),
         geninfo_hcfacilitylevel = factor( geninfo_hcfacilitylevel,
                                           levels = c("National Hospital", "Provincial Hospital", "District Hospital", "Private", "Total")))%>% 
  select(geninfo_hcfacilitylevel, Always,`Almost always`, Often , Sometimes, Rarely,Never,Total ) %>% 
  arrange(geninfo_hcfacilitylevel)

# Create a dataframe with specified values
safety.checklist <- data.frame( geninfo_hcfacilitylevel = "Total",
                                Always = sum(who.checklist$Always, na.rm = TRUE),
                                `Almost always` = sum(who.checklist$`Almost always`, na.rm = TRUE), 
                                Often = sum(who.checklist$Often, na.rm = TRUE), 
                                Sometimes = sum(who.checklist$Sometimes, na.rm = TRUE), 
                                Rarely = sum(who.checklist$Rarely, na.rm = TRUE),
                                Never = sum(who.checklist$Never, na.rm = TRUE),
                                Total = sum(who.checklist$Total, na.rm = TRUE),
                                check.names = FALSE)
who.safety.checklist <- bind_rows(who.checklist, safety.checklist)

a = who.safety %>% 
  select(5,6) %>% 
  tbl_summary(
    by = geninfo_hospitalfund,
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    missing = "no") %>% 
  add_n() %>% 
  add_overall()

# 7. All beds information: `Catchment population`= 16 ####
allbeds = allinformation %>% 
  select(1:5,`Admissions per year`=6,`Functioning ORs` =14, 
         `ICU beds` = 11, `Neonatal ICU beds` = 12, `post-op recovery beds`= 10, `Surgical beds` = 9, `Total beds` = 8, `Anesthetic machines` = 15)

hfl.beds <- allbeds %>%
  select(-c(1:2,4:5)) %>%
  group_by(geninfo_hcfacilitylevel) %>%
  summarise(`number of health facility` = n(),
            across(everything(), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

hft.beds = allbeds %>%
  select(-c(1:2,3,5)) %>% 
  group_by(geninfo_hcfacilitytype) %>% 
  summarise(`number of health facility` = n(),
            across(everything(), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

province.beds = allbeds %>%
  select(-c(1,3:5)) %>% 
  group_by(geninfo_province) %>%
  summarise(`number of health facility` = n(),
            across(everything(), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

tbl = allbeds %>%  
  select(3,6:13) %>% 
  tbl_summary(
    by = geninfo_hcfacilitylevel,
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    missing = "no") %>% 
  add_n() %>% 
  add_overall() %>% 
  add_p()

# Convert tbl_summary to data frame
tbl_df <- as_tibble(tbl$table_body, col_labels = TRUE)

#  ####
# Write dataset into Excel ####

excel.file <- list(infraavailable =infrastructure.availability,
                   workforce = total.workforce.distribution,
                   WFbyprovince = workforce.distribution.prov,
                   WFSOAspecialist = SOAdist.province,
                   access2hour = access.2hf.within2hour,
                   `surgicalvolume` = surgical_volume,
                   surgery = surgeryvolume,
                   `Bellwether` = surgical_bellwether,
                   `whochecklist` = who.safety.checklist,
                   bedhfl = hfl.beds,
                   bedhft = hft.beds)


write_xlsx(excel.file, "Excel datatable/chapter3.xlsx")

# Acknowledgement staff in HF #####
staffhf = clean.soa %>% 
  select(2:3,9:11,5:8) %>% 
  mutate(number = 1,
         geninfo_hcfacilitytype = case_when(
           geninfo_hcfacilitytype == "CPA3" & geninfo_hcfacilitylevel == "National Hospital" ~ "CPA3 National Hospital",
           geninfo_hcfacilitytype == "CPA3" & geninfo_hcfacilitylevel == "Provincial Hospital" ~ "CPA3 Provincial Hospital",
           geninfo_hcfacilitytype == "CPA2" & geninfo_hcfacilitylevel == "Provincial Hospital" ~ "CPA2 Provincial Hospital",
           geninfo_hcfacilitytype == "CPA2" & geninfo_hcfacilitylevel == "District Hospital" ~ "CPA2 District Hospital",
           TRUE ~ geninfo_hcfacilitytype )) %>% 
  select(2,1,6:7,4)

write_xlsx(staffhf, "Excel datatable/Acknowledging staff.xlsx")


