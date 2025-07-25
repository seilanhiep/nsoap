# Library #####
library(tidyverse)
library(cowplot)
library(dplyr)
library(purrr)
library(flextable)
library(gt)
library(ggh4x)
library(ggplot2)
library(highcharter)
library(echarts4r)
library(ggforce)
library(grid)
library(forcats)
library(bslib)
library(htmltools)
library(fontawesome)
library(stringdist)
library(gtsummary)
library(writexl)
library(openxlsx)

# Call dataset for NSOAP Hospital Assessment tool 2025 #####
clean.soa = readxl::read_excel("clean/cleaningdata.xlsx", sheet = 1)
# Population 2024
province = readxl::read_excel("data/NSOAP-clean.xlsx", sheet = 3) 

# Factor #####
infra.plot_factor = c("Oxygen","Runningwater","Or equipment and supplies","Laboratory","Electricity","Internet","Pharmacy","Spinal anesthesia","Radiology","Iv sedation anesthesia","Regional anesthesia","Blood supply")

factor_hf = c("All service delivery","National Hospital","Provincial Hospital","District Hospital","Private","CPA3","CPA2","Private Hospital","Policlinic","Clinic")

factor_wf = c("All workforce","National Hospital","Provincial Hospital","District Hospital","Private","CPA3","CPA2","Private Hospital","Policlinic","Clinic")

factor_allwf = c("all workforce","SAO specialist","anesthesia professionals","surgeons","allied health provider")

factor_sdtype = c("total surgical operation","obstetrics gynaecology","injury","specialty","non trauma orthopedic","urgentcases","total surgery","major surgery","minor surgery","paediatric surgery","bellwether","laparotomy","caesarean delivery","treatment open fracture")

factor_sdtype1 = c("total surgical operation","major surgery","minor surgery","paediatric surgery",
                   "laparotomy","caesarean delivery","treatment open fracture")

factor_bellwether = c("All three types of bellwether procedure", "At least 2 types of bellwether procedure", "At least 1 type of bellwether procedure", "No bellwether procedure")

factor_readiness = c("Needs Improvement","Partial completed readiness","Almost completed readiness","Completed readiness")

factor_wforceavai_allday = c("Full Availability","High Availability","Partial Availability","Needs Improvement")

factor_serdel = c("Specialty","Obstetricsgynaecology","Injury related","Non trauma orthopedic","Percenturgentcases","Totalsurgery","Perioperativedeath")

factor_hflevel = c("National Hospital", "Provincial Hospital","District Hospital", "Private")

factor_pubprivate = c("Public", "Private")

factor_training = c("Never", "As needed", "Weekly", "Monthly", "Quarterly", "Yearly")

factor_hftype = c("CPA3","CPA2","Private Hospital","Policlinic","Clinic")

factor_wforce = c("Other.related.surgery.obstetric.and.anesthesia","Biomedical.technicians","Pharmacists","Radiologists","Pathologists",
                  "Nurses.providing.anesthesia", "Nurses.Providing.surgery",
                  "Total.Midwives.on.Labour.and.delivery", "Midwives", 
                  "Medical.Officers.Providing.surgery", "Medical.Officers.Providing.anesthesia",
                  "General.doctors.providing.Anesthesia","General.doctors.providing.surgery", 
                  "Paediatric.anaesthesiologists","Anaesthesiologists","Obstetricians.gynaecologists","Paediatric.surgeons","General.surgeons")

factor_main <- str_to_sentence(str_replace_all(
  c("pulse.oximetry","inhalational.general.anesthesia","IV.sedation.anesthesia", "spinal.anesthesia", "regional.anesthesia", 
    "OR.equipment.and.supplies", "laboratory","blood.supply","radiology","pharmacy", "internet","electricity","runningwater","oxygen"),  c("\\." = " ", "scale" = "")))

factor_scale = c("Always", "Almost always", "Often","Sometimes", "Rarely", "Never")

factor_recordkeeping = c("Electronic","Electronic and paper","Paper","None")

factor_fund = c("Public","Private")

factor_yesno = c("Yes","No")

factor_safety = c("All","Almost all","Most","Some", "Few", "None")

factor_province = c("Banteay Meanchey","Battambang","Kampong Cham","Kampong Chhnang","Kampong Speu","Kampong Thom","Kampot","Kandal","Koh Kong",
                    "Kratie","Mondul Kiri","Phnom Penh","Preah Vihear","Prey Veng","Pursat","Ratanak Kiri","Siem Reap","Preah Sihanouk","Stung Treng","Svay Rieng","Takeo","Oddar Meanchey","Kep","Pailin","Tboung Khmum")

###########################################################
# ggplot function ##### 
plot_score = function(data,xaxis,yaxis,fillname,titles,subtitles){
  ggplot(data %>% filter(main.structure != "NA"),
         aes(x = {{xaxis}}, y = {{yaxis}}, fill = {{fillname}})) +
    geom_col(position = "stack", width = 0.6) +
    geom_text(aes(label = round(value, digits = 2)),
              position = position_stack(vjust = 0.5),
              size = 2.5, color = "black") +
    coord_flip() +  # allows labels outside bounds if needed
    scale_fill_manual( values = c(score = "#7d9bd1", gaps = "#ffd34e"),
                       labels = c(score = "Score", gaps = "Gaps")  ) +
    theme_classic() +
    labs(
      title = titles,
      subtitle = subtitles,
      caption = ""
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5),
      plot.caption = element_text(size = 8, face = "italic", hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      legend.position = "top")
}

all.plot = function(data,xaxis,yaxis,fillname,titles,subtitles,captions){
  ggplot(data, aes(x = {{xaxis}}, y = {{yaxis}})) + 
    geom_col(aes(fill = {{fillname}}), position = 'stack') +
    geom_text(aes(label = {{yaxis}}, group = {{fillname}}),
              position = position_stack(vjust = 0),
              hjust = 0, size = 3, color = "black") +
    coord_flip(clip = "off") +  
    theme_classic() + #  theme_bw() + #  theme_light() +
    labs(
      title = titles,
      subtitle = subtitles,
      caption = captions) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5), 
          plot.caption = element_text(size = 8, face = "italic", hjust = 0.5),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 8), 
          strip.text = element_text(size = 8),
          legend.position = 'none',
          legend.title = element_blank()
    )
}

group_ggplot = function(data,xaxis,yaxis,fillname,titles,subtitles,captions,legends) {
  ggplot(data, aes(x = {{xaxis}}, y = {{yaxis}})) + 
    geom_col(aes(fill = {{fillname}}), position = 'stack') +
    geom_text(aes(label = {{yaxis}}, group = {{fillname}}),
              position = position_stack(vjust = 0),
              hjust = 0, size = 3, color = "black") +
    facet_wrap2(vars({{fillname}}), nrow = 1, scales = "free_x", axes = "all", remove_labels = "all") +
    coord_flip(clip = "off") +
    theme_classic() +
    labs(
      title = titles,
      subtitle = subtitles,
      caption = captions) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5), 
          plot.caption = element_text(size = 9, face = "italic", hjust = 0.5),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 9), 
          strip.text = element_text(size = 9),
          legend.position = legends,
          legend.title = element_blank()
    )
}

facetwrap_ggplot = function(data,xaxis,yaxis,fillname,facewrap1,facewrap2,titles,subtitles,captions,legends) {
  ggplot(data, aes(x = {{xaxis}}, y = {{yaxis}})) + 
    geom_col(aes(fill = {{fillname}}), position = 'stack') +
    geom_text(aes(label = {{yaxis}}, group = {{fillname}}),
              position = position_stack(vjust = 0),
              hjust = 0, size = 3, color = "black") +
    facet_wrap2(vars({{facewrap1}}, {{facewrap2}}), nrow = 1, scales = "free_x", 
                axes = "all", remove_labels = "all") +
    coord_flip(clip = "off") +
    theme_classic() +
    labs(
      title = titles,
      subtitle = subtitles,
      caption = captions) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5), 
          plot.caption = element_text(size = 9, face = "italic", hjust = 0.5),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 9), 
          strip.text = element_text(size = 9),
          legend.position = legends,
          legend.title = element_blank()
    )
}

hf.group <- function(data) {
  # by public and private health facility
  bypubpri <- data %>% 
    group_by(geninfo_hospitalfund) %>% 
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate(percent = round(number / sum(number, na.rm = TRUE) * 100, digits = 2),
           geninfo_hospitalfund = factor(geninfo_hospitalfund, levels = factor_pubprivate))
  # Facility Readiness by health facility level
  by_level <- data %>%
    group_by(geninfo_hcfacilitylevel) %>% 
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate(percent = round(number / sum(number, na.rm = TRUE) * 100, digits = 2),
           geninfo_hcfacilitylevel = factor(geninfo_hcfacilitylevel, levels = factor_hflevel))
  
  # Facility Readiness by health facility type
  by_type <- data %>%
    group_by(geninfo_hcfacilitytype) %>%
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate( percent = round(number / sum(number, na.rm = TRUE) * 100, digits = 2),
            geninfo_hcfacilitytype = factor(geninfo_hcfacilitytype, levels = factor_hftype))
  
  # Facility Readiness by province
  by_province <- data %>%
    group_by(geninfo_province) %>%
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate( percent = round(number / sum(number, na.rm = TRUE) * 100, digits = 2),
            geninfo_province = factor(geninfo_province, levels = rev(factor_province))) %>%
    filter(!is.na(geninfo_province))
  
  # Return all as a named list
  return(list( bypubpri=bypubpri, hflevel = by_level,  hftype = by_type, hfprovince = by_province))
}

percent.workforce <- function(data,values, fname = NULL) {
  # by public and private health facility
  bypubpri <- data %>% 
    group_by(geninfo_hospitalfund) %>% 
    summarise(number = n(),
              staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_hospitalfund = factor(geninfo_hospitalfund,
                                         levels = factor_pubprivate))
  # by health facility level
  byhfl <- data %>% 
    group_by(geninfo_hcfacilitylevel) %>% 
    summarise(number = n(),
              staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_hcfacilitylevel = factor(geninfo_hcfacilitylevel,
                                            levels = factor_hflevel))
  # by health facility type
  byhft <- data %>% 
    group_by(geninfo_hcfacilitytype) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_hcfacilitytype = factor(geninfo_hcfacilitytype,
                                           levels = factor_hftype))
  # by province
  byprovince <- data %>% 
    group_by(geninfo_province) %>% 
    summarise(number = n(),
              staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_province = factor(geninfo_province,
                                     levels = rev(factor_province))) %>% 
    filter(geninfo_province != "NA")
  # return results as a list
  return(list(bypubpri = bypubpri, byhfl = byhfl,  byhft = byhft,  byprovince = byprovince
  ))
}

surbed_tsur <- function(data,bed,surgery, fname) {
  # Facility Readiness by health facility level
  by_level <- data %>%
    group_by(geninfo_hcfacilitylevel) %>% 
    summarise(number = n(),
              tsurbed = sum({{bed}}, na.rm = TRUE),
              tsurgery = sum({{surgery}}, na.rm = TRUE),
              bedratio = round(tsurbed/tsurgery *100000, digits = 0), .groups = 'drop_last') %>%
    pivot_longer(!c(1:4), names_to = "type", values_to = "values") %>%
    mutate(geninfo_hcfacilitylevel = factor(geninfo_hcfacilitylevel, levels = factor_hflevel))
  
  # Facility Readiness by health facility type
  by_type <- data %>%
    group_by(geninfo_hcfacilitytype) %>%
    summarise(number = n(),
              tsurbed = sum({{bed}}, na.rm = TRUE),
              tsurgery = sum({{surgery}}, na.rm = TRUE),
              bedratio = round(tsurbed/tsurgery *100000, digits = 0), .groups = 'drop_last') %>%
    pivot_longer(!c(1:4), names_to = "type", values_to = "values") %>%
    mutate( geninfo_hcfacilitytype = factor(geninfo_hcfacilitytype, levels = factor_hftype))
  
  # Facility Readiness by province
  by_province <- data %>%
    group_by(geninfo_province) %>%
    summarise(number = n(),
              tsurbed = sum({{bed}}, na.rm = TRUE),
              tsurgery = sum({{surgery}}, na.rm = TRUE),
              bedratio = round(tsurbed/tsurgery *100000, digits = 0), .groups = 'drop_last') %>%
    pivot_longer(!c(1:4), names_to = "type", values_to = "values") %>%
    mutate( geninfo_province = factor(geninfo_province, levels = rev(factor_province))) %>%
    filter(!is.na(geninfo_province))
  
  # Return all as a named list
  return(list( hflevel = by_level,  hftype = by_type, hfprovince = by_province))
}

hf_g1 <- function(data, group1, fname) {
  # by public and private health facility
  bypubpri <- data %>% 
    group_by(geninfo_hospitalfund, {{group1}}) %>% 
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate( percent = round(number / sum(number, na.rm = TRUE) * 100, digits = 2),
            tpercent = round((number / nrow(data)) * 100, 2),
            geninfo_hospitalfund = factor(geninfo_hospitalfund, levels = factor_pubprivate))
  
  # Facility Readiness by health facility level
  byhfl =  data %>%
    group_by(geninfo_hcfacilitylevel, {{group1}}) %>%
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate( percent = round(number / sum(number, na.rm = TRUE) * 100, digits = 2),
            tpercent = round((number / nrow(data)) * 100, 2),
            geninfo_hcfacilitylevel = factor(geninfo_hcfacilitylevel, levels = factor_hflevel))
  
  # by health facility type
  byhft = data %>%
    group_by(geninfo_hcfacilitytype, {{group1}}) %>%
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate( percent = round(number / sum(number, na.rm = TRUE) * 100, digits = 2),
            tpercent = round((number / nrow(data)) * 100, 2),
            geninfo_hcfacilitytype = factor(geninfo_hcfacilitytype, levels = factor_hftype))
  
  # by province
  byprovince = data %>%
    group_by(geninfo_province, {{group1}}) %>%
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate( percent = round(number / sum(number, na.rm = TRUE) * 100, digits = 2),
            tpercent = round((number / nrow(data)) * 100, 2),
            geninfo_province = factor(geninfo_province, levels = rev(factor_province))  ) %>%
    filter(!is.na(geninfo_province))
  # return results as a list
  return(list( bypubpri=bypubpri, byhfl = byhfl, byhft = byhft,byprovince = byprovince ))
}

hf_g1.g2 <- function(data, group1, group2, fname) {
  # by public and private health facility
  bypubpri = data %>% 
    group_by(geninfo_hospitalfund, {{group1}}, {{group2}}) %>%
    summarise(staff = n(), .groups = 'drop_last') %>%
    mutate( percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
            tpercent = round((staff / nrow(data)) * 100, 2),
      geninfo_hospitalfund = factor(geninfo_hospitalfund, levels = factor_pubprivate))
  # by health facility level
  byhfl = data %>%
    group_by(geninfo_hcfacilitylevel, {{group1}}, {{group2}}) %>%
    summarise(staff = n(), .groups = 'drop_last') %>%
    mutate( percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
            tpercent = round((staff / nrow(data)) * 100, 2),
            geninfo_hcfacilitylevel = factor(geninfo_hcfacilitylevel, levels = factor_hflevel))
  # by health facility type
  byhft = data %>%
    group_by(geninfo_hcfacilitytype, {{group1}}, {{group2}}) %>%
    summarise(staff = n(), .groups = 'drop_last') %>%
    mutate( percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
            tpercent = round((staff / nrow(data)) * 100, 2),
            geninfo_hcfacilitytype = factor(geninfo_hcfacilitytype, levels = factor_hftype))
  # by province
  byprovince = data %>%
    group_by(geninfo_province, {{group1}}, {{group2}}) %>%
    summarise(staff = n(), .groups = 'drop_last') %>%
    mutate( percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
            tpercent = round((staff / nrow(data)) * 100, 2),
            geninfo_province = factor(geninfo_province, levels = rev(factor_province))) %>%
    filter(!is.na(geninfo_province))
  # return results as a list
  return(list( bypubpri=bypubpri,  byhfl = byhfl,    byhft = byhft,    byprovince = byprovince  ))
}

hf_score <- function(data, maingroup, fname) {
  # by health facility level
  byhfl = data %>%
    group_by(geninfo_hcfacilitylevel, {{maingroup}}) %>%
    summarise( number = n(),
               total_value = sum(nscale, na.rm = TRUE), .groups = 'drop' ) %>% 
    mutate(score = round((total_value/(number *5))*100, digits = 2),
           gaps = 100-score) %>% 
    select(-c(number,total_value)) %>% 
    pivot_longer(!c(geninfo_hcfacilitylevel, {{maingroup}}), names_to = "type", values_to = "value") %>% 
    mutate(geninfo_hcfacilitylevel <- factor(geninfo_hcfacilitylevel, levels = factor_hflevel))
  # by health facility type
  byhft = data %>%
    group_by(geninfo_hcfacilitytype, {{maingroup}}) %>%
    summarise( number = n(),
               total_value = sum(nscale, na.rm = TRUE), .groups = 'drop' ) %>% 
    mutate(score = round((total_value/(number *5))*100, digits = 2),
           gaps = 100-score) %>% 
    select(-c(number,total_value)) %>% 
    pivot_longer(!c(geninfo_hcfacilitytype, {{maingroup}}), names_to = "type", values_to = "value") %>% 
    mutate(geninfo_hcfacilitytype <- factor(geninfo_hcfacilitytype, levels = factor_hftype))
  
  # by province
  byprovince = data %>%
    group_by(geninfo_province, {{maingroup}}) %>%
    summarise( number = n(),
               total_value = sum(nscale, na.rm = TRUE), .groups = 'drop' ) %>% 
    mutate(score = round((total_value/(number *5))*100, digits = 2),
           gaps = 100-score) %>%
    select(-c(number,total_value)) %>% 
    pivot_longer(!c(geninfo_province, {{maingroup}}), names_to = "type", values_to = "value") %>% 
    mutate(geninfo_province <- factor(geninfo_province,  levels = factor_province)) %>%
    filter(!is.na(geninfo_province))
  # return results as a list
  return(list(    byhfl = byhfl,    byhft = byhft,    byprovince = byprovince  ))
}

fullmainfunction <- function(data, group1, group2, fname) {
  # by health facility level
  byhfl = data %>%
    group_by(geninfo_hcfacilitylevel, {{group1}}, {{group2}}) %>%
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate( score = round(number/sum(number) * 100, digits = 2),
            gaps = 100-score) %>%
    filter(scale == "Always") %>%
    select(-c(number,scale)) %>% 
    pivot_longer(!c(geninfo_hcfacilitylevel, {{group1}}), names_to = "type", values_to = "value") %>% 
    mutate(main.structure = factor(str_to_sentence(
      str_replace_all(main.structure,  c("\\." = " ", "scale" = ""))),
      levels = factor_main),
      geninfo_hcfacilitylevel = factor(geninfo_hcfacilitylevel, levels = factor_hflevel))
  # by health facility type
  byhft = data %>%
    group_by(geninfo_hcfacilitytype, {{group1}}, {{group2}}) %>%
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate( score = round(number/sum(number) * 100, digits = 2),
            gaps = 100-score) %>%
    filter(scale == "Always") %>%
    select(-c(number,scale)) %>% 
    pivot_longer(!c(geninfo_hcfacilitylevel, {{group1}}), names_to = "type", values_to = "value") %>% 
    mutate(main.structure = factor(str_to_sentence(
      str_replace_all(main.structure,  c("\\." = " ", "scale" = ""))),
      levels = factor_main),
      geninfo_hcfacilitytype = factor(geninfo_hcfacilitytype, levels = factor_hftype))
  # by province
  byprovince = data %>%
    group_by(geninfo_province, {{group1}}, {{group2}}) %>%
    summarise(number = n(), .groups = 'drop_last') %>%
    mutate( score = round(number/sum(number) * 100, digits = 2),
            gaps = 100-score) %>%
    filter(scale == "Always") %>%
    select(-c(number,scale)) %>% 
    pivot_longer(!c(geninfo_hcfacilitylevel, {{group1}}), names_to = "type", values_to = "value") %>% 
    mutate(main.structure = factor(str_to_sentence(
      str_replace_all(main.structure,  c("\\." = " ", "scale" = ""))),
      levels = factor_main),
      geninfo_province = factor(geninfo_province, levels = rev(factor_province))) %>%
    filter(!is.na(geninfo_province))
  # return results as a list
  return(list(    byhfl = byhfl,    byhft = byhft,    byprovince = byprovince  ))
}

hf_subscore <- function(data, submain, fname) {
  # by health facility level
  byhfl = data %>%
    group_by(geninfo_hcfacilitylevel, {{submain}}) %>%
    summarise( number = n(),
               total_value = sum(nscale, na.rm = TRUE), .groups = 'drop' ) %>% 
    mutate(score = round((total_value/(number *5))*100, digits = 2),
           gaps = 100-score,
           sub.structure =  str_to_sentence(str_replace_all( sub.structure, c("\\." = " ")))) %>% 
    select(-c(number,total_value)) %>% 
    pivot_longer(!c(geninfo_hcfacilitylevel, {{submain}}), names_to = "type", values_to = "value") %>% 
    mutate(geninfo_hcfacilitylevel <- factor(geninfo_hcfacilitylevel, levels = factor_hflevel))
  
  # by health facility type
  byhft = data %>%
    group_by(geninfo_hcfacilitytype, {{submain}}) %>%
    summarise( number = n(),
               total_value = sum(nscale, na.rm = TRUE), .groups = 'drop' ) %>% 
    mutate(score = round((total_value/(number *5))*100, digits = 2),
           gaps = 100-score,
           sub.structure =  str_to_sentence(str_replace_all( sub.structure, c("\\." = " ")))) %>% 
    select(-c(number,total_value)) %>% 
    pivot_longer(!c(geninfo_hcfacilitytype, {{submain}}), names_to = "type", values_to = "value") %>% 
    mutate(geninfo_hcfacilitytype <- factor(geninfo_hcfacilitytype, levels = factor_hftype))
  
  # by province
  byprovince = data %>%
    group_by(geninfo_province,main.structure, {{submain}}) %>%
    summarise( number = n(),
               total_value = sum(nscale, na.rm = TRUE), .groups = 'drop' ) %>% 
    mutate(score = round((total_value/(number *5))*100, digits = 2),
           gaps = 100-score,
           sub.structure =  str_to_sentence(str_replace_all( sub.structure, c("\\." = " ")))) %>%
    select(-c(number,total_value)) %>% 
    pivot_longer(!c(geninfo_province,main.structure, {{submain}}), names_to = "type", values_to = "value") %>% 
    mutate(geninfo_province <- factor(geninfo_province,  levels = factor_province)) %>%
    filter(!is.na(geninfo_province))
  # return results as a list
  return(list(    byhfl = byhfl,    byhft = byhft,    byprovince = byprovince  ))
}

onlysum <- function(data, group1,values, fname = NULL) {
  # by public and private health facility
  bypubpri <- data %>% 
    group_by(geninfo_hospitalfund, {{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_hospitalfund = factor(geninfo_hospitalfund,
                                         levels = factor_pubprivate))
  # by health facility level
  byhfl <- data %>% 
    group_by(geninfo_hcfacilitylevel, {{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_hcfacilitylevel = factor(geninfo_hcfacilitylevel,
                                            levels = factor_hflevel))
  # by health facility type
  byhft <- data %>% 
    group_by(geninfo_hcfacilitytype, {{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_hcfacilitytype = factor(geninfo_hcfacilitytype,
                                           levels = factor_hftype))
  # by province
  byprovince <- data %>% 
    group_by(geninfo_province, {{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_province = factor(geninfo_province,
                                     levels = rev(factor_province))) %>% 
    filter(geninfo_province != "NA")
  # return results as a list
  return(list(bypubpri = bypubpri, byhfl = byhfl,  byhft = byhft,  byprovince = byprovince
  ))
}

rescale <- function(x, to = c(0, 1)) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / diff(rng) * diff(to) + to[1]
} 

semi_hf <- function(Parties, shares, cols = NULL, repr = c("absolute", "proportion"), chart_title = NULL, small_thresh = 0.1) {
  repr <- match.arg(repr)
  stopifnot(length(Parties) == length(shares))
  if (repr == "proportion") stopifnot(sum(shares) == 1)
  if (!is.null(cols)) names(cols) <- Parties
  
  # Angles
  angles <- cumsum(c(-pi/2, switch(repr,
                                   "absolute" = (shares / sum(shares)) * pi,
                                   "proportion" = shares * pi)))
  angles[length(angles)] <- pi/2
  meanAngles <- colMeans(rbind(angles[-1], angles[-length(angles)]))
  
  # Percentages and label sizing
  percentages <- if (repr == "absolute") shares / sum(shares) else shares
  is_small <- percentages < small_thresh
  
  # Scaled label size (3 to 6 pt)
  label_size <- rescale(percentages, to = c(3, 5))
  
  # Label positions
  label_r <- ifelse(is_small, 1.15, 0.75)
  labelX <- label_r * sin(meanAngles)
  labelY <- label_r * cos(meanAngles)
  
  # Data frame for plotting
  label_df <- data.frame(
    x = labelX,
    y = labelY,
    Parties = Parties,
    shares = shares,
    is_small = is_small,
    label_size = label_size
  )
  
  # Connector lines
  connector_data <- data.frame(
    x = 0.9 * sin(meanAngles[is_small]),
    y = 0.9 * cos(meanAngles[is_small]),
    xend = 1.1 * sin(meanAngles[is_small]),
    yend = 1.1 * cos(meanAngles[is_small])
  )
  
  # Plot
  p <- ggplot() +
    theme_void() +
    coord_fixed() +
    expand_limits(x = c(-1.4, 1.4), y = c(0, 1.4)) +
    theme(legend.position = "none") +
    
    # Arcs
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1,
                     start = angles[1:length(shares)], 
                     end = c(angles[2:length(shares)], pi/2), fill = Parties)) +
    switch(is.null(cols) + 1, scale_fill_manual(values = cols), NULL) +
    
    # Inside labels
    geom_text(data = subset(label_df, !is_small),
              aes(x = x, y = y,
                  label = switch(repr,
                                 "absolute" = paste0(Parties, "\n", format(shares, big.mark = ",")),
                                 "proportion" = Parties),
                  size = label_size),
              fontface = "bold", color = "black") +
    
    # Outside labels
    geom_text(data = subset(label_df, is_small),
              aes(x = x, y = y,
                  label = switch(repr,
                                 "absolute" = paste0(Parties, "\n", format(shares, big.mark = ",")),
                                 "proportion" = Parties),
                  size = label_size),
              fontface = "bold", hjust = 0.5, color = "darkred") +
    
    # Connector lines
    geom_segment(data = connector_data,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 color = "gray", size = 0.2) +
    
    # Center total
    geom_text(aes(x = 0, y = 0.2, label = switch(repr, 
                                                 "absolute" = format(sum(shares), big.mark = ","), 
                                                 "proportion" = "")),
              fontface = "bold", size = 8) +
    
    # Title
    labs(title = chart_title) +
    theme(plot.title = element_text(hjust = 0.5, vjust = -10, size = 16, face = "bold")) +
    scale_size_identity()  # Use label_size directly as point size
  
  return(p)
}

wf.table = function(data,title,sub.title){
  # Add totals row
  totals_row <- data %>%  select(-workforces) %>%
    summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(workforces = "Total") %>%
    select(workforces, everything())
  # Bind to original data
  data <- bind_rows(data, totals_row)
  
  gt(data) %>% 
    opt_table_font(font = "Arial") %>% 
    tab_options(
      table.font.size = "small",  # or use "small", "medium", "large"
      heading.title.font.size = px(18),
      heading.subtitle.font.size = px(14) ) %>% 
    tab_header(
      title = md(title),
      subtitle = md(sub.title)) %>% 
    tab_source_note(source_note = "")
}

bellwether.table = function(data,title,sub.title){
  # Add totals row
  totals_row <- data %>%  select(-type) %>%
    summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(type = "Total") %>%
    select(type, everything())
  # Bind to original data
  data <- bind_rows(data, totals_row)
  
  gt(data) %>% 
    opt_table_font(font = "Arial") %>% 
    tab_options(
      table.font.size = "small",  # or use "small", "medium", "large"
      heading.title.font.size = px(18),
      heading.subtitle.font.size = px(14) ) %>% 
    tab_header(
      title = md(title),
      subtitle = md(sub.title)) %>% 
    tab_source_note(source_note = "")
}

leg.all.plot = function(data,xaxis,yaxis,fillname,titles,subtitles,captions,legends){
  ggplot(data, aes(x = {{xaxis}}, y = {{yaxis}})) + 
    geom_col(aes(fill = {{fillname}}), position = 'stack') +
    geom_text(aes(label = {{yaxis}}, group = {{fillname}}),
              position = position_stack(vjust = 0),
              hjust = 0, size = 3, color = "black") +
    coord_flip(clip = "off") +  
    theme_classic() + #  theme_bw() + #  theme_light() +
    labs(
      title = titles,
      subtitle = subtitles,
      caption = captions) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5), 
          plot.caption = element_text(size = 8, face = "italic", hjust = 0.5),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 8), 
          strip.text = element_text(size = 8),
          legend.position = legends,
          legend.title = element_blank()
    )
}

plot_unmetneed = function(data,xaxis,yaxis,fillname,titles,subtitles){
  ggplot(data,
         aes(x = {{xaxis}}, y = {{yaxis}}, fill = {{fillname}})) +
    geom_col(position = "stack", width = 0.6) +
    geom_text(aes(label = round(values, digits = 2)),
              position = position_stack(vjust = 0.5),
              size = 2.5, color = "black") +
    coord_flip() +  # allows labels outside bounds if needed
    scale_fill_manual( values = c(status = "#7d9bd1", `unmet needs` = "#ffd34e"),
                       labels = c(status = "Current status", `unmet needs` = "Unmet needs")) +
    theme_classic() +
    labs(
      title = titles,
      subtitle = subtitles,
      caption = ""
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5),
      plot.caption = element_text(size = 8, face = "italic", hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      legend.position = "top")
}

onlysum.bellwether <- function(data, group1,values, fname = NULL) {
  # by public and private health facility
  bypubpri <- data %>% 
    group_by(geninfo_hospitalfund, {{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(geninfo_hospitalfund = factor(geninfo_hospitalfund,
                                         levels = factor_pubprivate))
  # by health facility level
  byhfl <- data %>% 
    group_by(geninfo_hcfacilitylevel, {{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last')
  # by health facility type
  byhft <- data %>% 
    group_by(geninfo_hcfacilitytype, {{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last')
  # by province
  byprovince <- data %>% 
    group_by(geninfo_province, {{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(geninfo_province = factor(geninfo_province,
                                     levels = rev(factor_province))) %>% 
    filter(geninfo_province != "NA")
  # return results as a list
  return(list(bypubpri = bypubpri, byhfl = byhfl,  byhft = byhft,  byprovince = byprovince
  ))
}

onlysum.wf <- function(data, group1,values, fname = NULL) {
  # by public and private health facility
  bypubpri <- data %>% 
    group_by(geninfo_hospitalfund, {{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(workforces = case_when(workforces == "All workforce" ~ "Total workforce",
                                  TRUE ~ workforces),
           adjusted.staff = case_when(geninfo_hospitalfund == "Private" ~ round((staff*12.7)/100,digits = 2),
                                      TRUE ~ staff),
           percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_hospitalfund = factor(geninfo_hospitalfund,
                                         levels = factor_pubprivate))
  # by health facility level
  byhfl <- data %>% 
    group_by(geninfo_hcfacilitylevel,geninfo_hospitalfund,{{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(workforces = case_when(workforces == "All workforce" ~ "Total workforce",
                                  TRUE ~ workforces),
           adjusted.staff = case_when(geninfo_hospitalfund == "Private" ~ round((staff*12.7)/100,digits = 2),
                                      TRUE ~ staff),
           percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_hcfacilitylevel = factor(geninfo_hcfacilitylevel,
                                            levels = factor_hflevel))
  # by health facility type
  byhft <- data %>% 
    group_by(geninfo_hcfacilitytype,geninfo_hospitalfund,{{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(workforces = case_when(workforces == "All workforce" ~ "Total workforce",
                                  TRUE ~ workforces),
           adjusted.staff = case_when(geninfo_hospitalfund == "Private" ~ round((staff*12.7)/100,digits = 2),
                                      TRUE ~ staff),
           percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_hcfacilitytype = factor(geninfo_hcfacilitytype,
                                           levels = factor_hftype))
  # by province
  byprovince <- data %>% 
    group_by(geninfo_province,geninfo_hospitalfund,{{group1}}) %>% 
    summarise(staff = sum({{values}}, na.rm = TRUE), .groups = 'drop_last') %>% 
    mutate(workforces = case_when(workforces == "All workforce" ~ "Total workforce",
                                  TRUE ~ workforces),
           adjusted.staff = case_when(geninfo_hospitalfund == "Private" ~ round((staff*12.7)/100,digits = 2),
                                      TRUE ~ staff),
           percent = round(staff / sum(staff, na.rm = TRUE) * 100, digits = 2),
           geninfo_province = factor(geninfo_province,
                                     levels = rev(factor_province)))
  # return results as a list
  return(list(bypubpri = bypubpri, byhfl = byhfl,  byhft = byhft,  byprovince = byprovince
  ))
}
################################################################################
################################################################################
################################################################################
hfoverview = clean.soa %>% 
  group_by(geninfo_hospitalfund,geninfo_hcfacilitytype,geninfo_hcfacilitylevel) %>% 
  summarise(number = n(), .groups = "drop_last") %>% 
  mutate(percent = round( number / sum(number,na.rm = TRUE),digits = 2) )
# Overview
overview = hf.group(clean.soa)

pro.hf.overview = clean.soa %>% 
  group_by(geninfo_province,geninfo_hospitalfund) %>% 
  summarise(number = n(), .groups = 'drop_last') %>%
  mutate( percent = round(number / sum(number, na.rm = TRUE) * 100, digits = 2),
          geninfo_province = factor(geninfo_province, levels = rev(factor_province))) %>%
  filter(!is.na(geninfo_province))

write_xlsx(c(overview, setNames(list(pro.hf.overview), "pro.hf.overview"),
             setNames(list(hfoverview), "hf overview")),
           "Excel datatable/HFoverview.xlsx")
#####
clean.soa.alltype = readxl::read_excel("clean/clean.soa.alltype.xlsx", sheet = 1)
denominator = clean.soa.alltype %>% 
  group_by(geninfo_hcfacilitytype) %>% 
  summarise(number = n(), .groups = "drop_last") %>% 
  mutate(percent = round(number/sum(number), digits = 2))

hf.denominator = readxl::read_excel("data/NSOAP-clean.xlsx", sheet = 4)

# General information about infrastructure #####
# 18. How many post-op recovery beds are there?
# 19. How many intensive care unit (ICU) beds are there?
# 20. How many neonatal ICU beds are there?
# 21. How many functional ventilators are available for the ICU?
# 22. How many functioning operating rooms are there?
bed.infor <- clean.soa %>% 
  select(3,9:11, matches(c("infrastructure_generalnumeric","sersurvol_"))) %>% 
  select(-matches("scale")) %>%
  mutate(across(matches(c("infrastructure_generalnumeric","sersurvol_"), ignore.case = TRUE),
                ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>% 
  select(1:4,7:13) %>% 
  pivot_longer(!c(1:4), names_to = "type", values_to = "values") %>% 
  mutate(type = str_to_sentence(str_replace_all(type,  
                                                c( "infrastructure_generalnumeric_" = "", "\\." = " "))),
         type = case_when(type == "Functioning operatingrooms" ~ "Functioning Operating Rooms",
                          type == "Icu beds" ~ "ICU beds",
                          type == "Neonatal icu beds" ~ "Neonatal ICU beds",
                          type == "Post op recoverybeds" ~ "Post-operative recovery beds",
                          type == "Ventilators icu" ~ "Ventilators ICU",
                          TRUE ~ type
         ))

tbeds = bed.infor %>% 
  group_by(type) %>% 
  summarise(value = sum(values, na.rm = TRUE))

tbed.infor = onlysum(bed.infor, type,values, fname = NULL)

write_xlsx(c(setNames(list(tbeds), "totalbeds"),tbed.infor), 
           "Excel datatable/HFbeds.xlsx")

# Number of beds
ICU.beds = sum(tbeds$value[[2]] + tbeds$value[[3]] + tbeds$value[[7]])
Surgical.beds = tbeds$value[[5]]
total.beds = tbeds$value[[6]]
# Surgery ICU bed ratio #####
# the number of ICU beds by the total number of hospital beds
hosbed.icubed.ratio = round(ICU.beds/total.beds *100, digits = 2)

# the number of ICU beds by the number of surgical beds
surbed.icubed.ratio = round(ICU.beds/Surgical.beds *100, digits = 2)

##############################################################################
# Infrastructure dataset #####
sca_infra = clean.soa %>% select(3,9:11, matches(c("infrastructure_")), -matches("numeric_"))
num_soa = clean.soa %>% 
  select(3,9:11,which(str_detect(names(.), "infrastructure_") & 
                        str_detect(names(.), "numeric_"))) %>% 
  mutate(across(-c(1:4), ~ as.numeric(str_extract(as.character(.x), "\\d+"))))

# Transform raw dataset #####
t_sca_infra = sca_infra %>% 
  pivot_longer(!c(1:4), names_to = "type", values_to = "scale") %>% 
  mutate(st.pattern = sub("^(([^_]*_[^_]*)).*", "\\1", type),
         structure = str_split(type, "_", simplify = TRUE)[, 1],
         main.structure = str_split(type, "_", simplify = TRUE)[, 2],
         sub.structure = str_split(type, "_", simplify = TRUE)[, 3],
         scale = case_when( str_detect(scale, "^All") ~ "Always (100%)", str_detect(scale, "^Almost all") ~ "Almost always (76-99%)", str_detect(scale, "^Most") ~ "Often (51-75%)", str_detect(scale, "^Some") ~ "Sometimes (26-50%)", str_detect(scale, "^Few") ~ "Rarely (1-25%)", str_detect(scale, "^None") ~ "Never (0%)",
                            TRUE ~ scale)) %>%
  select(1:4,structure:sub.structure,scale)
# Finalize dataset
data.infra = t_sca_infra %>% 
  filter(structure == "infrastructure") %>% 
  separate(scale, into = c("scale", "pscale"), sep = " \\(") %>%
  mutate(pscale = gsub("\\)", "", pscale),
         nscale = case_when( scale == "Never" ~ 0,  scale == "Rarely" ~ 1, 
                             scale == "Sometimes" ~ 2,  scale == "Often" ~ 3,  
                             scale == "Almost always" ~ 4,   scale == "Always" ~ 5,  
                             TRUE ~ NA_real_),
         main.structure = factor(str_to_sentence(
           str_replace_all(main.structure,  c("\\." = " ", "scale" = ""))),
           levels = factor_main))

write_xlsx(c(setNames(list(num_soa), "general infra"),
             setNames(list(data.infra), "infra scale")), 
           "Excel datatable/infrastructure.xlsx")
################################################################################
# Facility Readiness #####
fac.readiness <- sca_infra %>% 
#  select(-c(58,61,62,69)) %>% # 24hour.access.to.radiology/functioning.CT.scanner/MRI.scanner/patients.reach.within.2.hours
#  select(-c(15,17,19,21,23,58,61,62,69)) %>% # Paediatric & Adult.Magill.forceps2/IV.sedation.anesthesia & regional.anesthesia
  mutate(across(.cols = -c(1:4), ~ case_when(
    . %in% c("Always (100%)","All (100%)") ~ 1,
    TRUE ~ 0))) %>% 
  mutate(number = rowSums(across(-c(1:4)), na.rm = TRUE),
         score = round(number/(length(names(.))-4)*100, digits = 2),
         readiness = case_when(score >0 & score <= 25 ~ "Needs Improvement",
                               score > 25 & score <= 75 ~ "Partial completed readiness",
                               score > 75 & score < 100 ~ "Almost completed readiness",
                               score == 100 ~ "Completed readiness"),
         readiness =  factor( readiness, levels = factor_readiness)) %>% 
  filter( !is.na(readiness))

ffac.readness = fac.readiness %>% 
  select(1:4,(ncol(fac.readiness)-3):ncol(fac.readiness))

gt.hf.readiness = ffac.readness %>% 
  group_by(readiness) %>% 
  summarise(number = n(),.groups = 'drop') %>% 
  mutate(percent = round((number/sum(number))*100, digits = 2))

# Facility Readiness by health facility level/type/province
facreadiness = hf_g1(fac.readiness, readiness, fname = NULL)

write_xlsx(c(setNames(list(fac.readiness), "readiness data"),
             setNames(list(ffac.readness), "readiness score"),
             setNames(list(gt.hf.readiness), "infra readiness"),
             facreadiness), 
           "Excel datatable/infrastructure readiness-rawdata.xlsx")
################################################################################
# Main structure of infrastructure by score #####
main.infra1 = data.infra %>% 
  group_by(main.structure) %>% 
  summarise( number = n(),
             total_value = sum(nscale, na.rm = TRUE), 
             meanscore = round(mean(nscale, na.rm = TRUE), digits = 2), .groups = 'drop' ) %>% 
  mutate(score = round((total_value/(number *5))*100, digits = 2),
         gaps = 100-score)%>% 
  select(-c(number, total_value)) %>% 
  pivot_longer(!c(main.structure), names_to = "type", values_to = "value") %>% 
  mutate(main.structure = factor(str_to_sentence(
    str_replace_all(main.structure,  c("\\." = " ", "scale" = ""))),
    levels = factor_main))
main.infra = main.infra1 %>% filter(type != "meanscore")

# Sub structure of infrastructure by score #####
score.sub.infra1 = data.infra %>% 
  group_by(main.structure, sub.structure) %>% 
  summarise( number = n(),
             total_value = sum(nscale, na.rm = TRUE),
             meanscore = round(mean(nscale, na.rm = TRUE), digits = 2), .groups = 'drop' ) %>% 
  mutate(score = round((total_value/(number *5))*100, digits = 2),
         gaps = 100-score,
         sub.structure =  str_to_sentence(str_replace_all( sub.structure, c("\\." = " ")))
  )%>% 
  select(-c(number, total_value)) %>% 
  pivot_longer(!c(main.structure,sub.structure), names_to = "type", values_to = "value") %>% 
  mutate(main.structure = factor(str_to_sentence(
    str_replace_all(main.structure,  c("\\." = " ", "scale" = ""))),
    levels = factor_main))
score.sub.infra = score.sub.infra1 %>% filter(type != "meanscore")

###################################
# Main structure of infrastructure by scale #####
scale.main.infra = data.infra %>% 
  group_by(main.structure, scale) %>% 
  summarise(number = n(), .groups = 'drop_last') %>% 
  mutate(percent = round((number / sum(number, na.rm = TRUE) * 100), digits = 1),
         main.structure = factor(str_to_sentence(
           str_replace_all(main.structure,  c("\\." = " ", "scale" = ""))),
           levels = factor_main),
         scale = factor(scale, levels = factor_scale)) %>% 
  filter(scale != "NA", !is.na(main.structure))

# Sub structure of infrastructure by scale #####
scale.sub.infra = data.infra %>% 
  group_by(main.structure, sub.structure,scale) %>% 
  summarise( number = n(),.groups = 'drop_last' ) %>% 
  mutate(percent = round((number / sum(number, na.rm = TRUE) * 100), digits = 2),
         sub.structure =  str_to_sentence(str_replace_all( sub.structure, c("\\." = " "))),
         scale <- factor(scale, levels = factor_scale)) %>% 
  filter(scale != "NA")

# Main structure of infrastructure by health facility level/type/province
dis.mainstruct = hf_score(data.infra, 
                          main.structure, fname = NULL)
# Distribution of Sub infrastructure by health facility level/type/province
dis.substruct = hf_subscore(data.infra, sub.structure, fname = NULL)

write_xlsx(c(setNames(list(main.infra1), "maininfra score"),
             setNames(list(scale.main.infra), "maininfra scale"),
             dis.mainstruct), 
           "Excel datatable/main infrastructure.xlsx")

write_xlsx(c(setNames(list(score.sub.infra1), "submaininfra score"),
             setNames(list(scale.sub.infra), "submaininfra scale"),
             dis.substruct), 
           "Excel datatable/submain infrastructure.xlsx")
################################################################################
## Fully-functioning hospital facilities by its main infrastructure (Always)
fullfun.maininfra = data.infra %>% 
  group_by(geninfo_hcfacilitylevel,main.structure,scale) %>% 
  summarise(number = n(), .groups = 'drop_last') %>% 
  mutate(score = round(number/sum(number, na.rm = TRUE)*100, digits = 2),
         gaps = 100-score) %>% 
  filter(scale == "Always") %>%
  select(-c(number,scale)) %>% 
  pivot_longer(!c(geninfo_hcfacilitylevel, main.structure), names_to = "type", values_to = "value") %>% 
  mutate(main.structure = factor(str_to_sentence(
    str_replace_all(main.structure,  c("\\." = " ", "scale" = ""))),
    levels = factor_main),
    geninfo_hcfacilitylevel = factor(geninfo_hcfacilitylevel, levels = factor_hflevel))

# Fully-functioning hospital facilities by health facility level/type/province
fullmainfunction = hf_g1.g2(data.infra, main.structure, scale, fname = NULL)

## Fully-functioning hospital facilities by its sub infrastructure (Always)
fulfunc.subinfra = data.infra %>% 
  group_by(geninfo_hcfacilitylevel, main.structure, sub.structure,scale) %>% 
  summarise(number = n(), .groups = 'drop_last') %>% 
  mutate(score = round(number/sum(number, na.rm = TRUE)*100, digits = 2),
         gaps = 100-score,
         sub.structure =  str_to_sentence(str_replace_all( sub.structure, c("\\." = " ")))) %>% 
  filter(scale == "Always") %>%
  select(-c(number,scale)) %>% 
  pivot_longer(!c(geninfo_hcfacilitylevel, main.structure, sub.structure), names_to = "type", values_to = "value") %>% 
  mutate(geninfo_hcfacilitylevel <- factor(geninfo_hcfacilitylevel, levels = factor_hflevel)) %>% 
  filter(type == "score",
         !is.na(main.structure),
         !is.na(geninfo_hcfacilitylevel))

# Fully-functioning hospital facilities by health facility level/type/province
fullsubfunction = hf_g1.g2(data.infra, sub.structure, scale, fname = NULL)

write_xlsx(c(setNames(list(fullfun.maininfra), "maininfra fulfunctioning"),
             fullmainfunction), 
           "Excel datatable/maininfra fulfunctioning.xlsx")

write_xlsx(c(setNames(list(fulfunc.subinfra), "submaininfra fulfunctioning"),
             fullsubfunction), 
           "Excel datatable/submaininfra fulfunctioning.xlsx")
###########################################################################
# Infrastructure readiness ##### 
# when we remove (Paediatric and Adult.Magill.forceps2) it will get complete
# when we remove ( IV.sedation.anesthesia & regional.anesthesia) it will improve
# 24hour.access.to.radiology/functioning.CT.scanner/MRI.scanner/patients.reach.within.2.hours

infastruct.readiness <- sca_infra %>% 
#  select(-c(58, 61, 62, 69)) %>% 
  mutate(across(.cols = -c(1:4), ~ case_when(
    . %in% c("Always (100%)", "All (100%)") ~ 1,
    TRUE ~ 0))) %>% 
  mutate( `general utility` = rowSums(across(c(5:8)), na.rm = TRUE) / (8-5+1),
          `anesthesia utility` = rowSums(across(c(10:13)), na.rm = TRUE) / (13-10+1),
#          `anesthesia utility` = rowSums(across(c(10,12)), na.rm = TRUE) / (13-10+1-2),
#          `Operating Room Equipment and Supplies` = rowSums(across(c(14:22,24:50)), na.rm = TRUE) / (50-14+1),
          `Operating Room Equipment and Supplies` = rowSums(across(c(14,16,18,20,22,24:50)), na.rm = TRUE) / (50-14+1-5),
          pharmacy = rowSums(across(c(51:57)), na.rm = TRUE) / (57 - 51 + 1),
          radiology = rowSums(across(c(59:60)), na.rm = TRUE) / (60 - 59 + 1),
          `blood transfusion within 2 hours` = rowSums(across(c(63)), na.rm = TRUE),
          laboratory = rowSums(across(c(64:68)), na.rm = TRUE) / (68 - 64 + 1),
          other = rowSums(across(c(15,17,19,21,23,58,61,62,69)), na.rm = TRUE) / 9) %>% 
  select(c(1:4, (length(names(.)) - 7):length(names(.)))) %>% 
  mutate(across(where(is.numeric),
                ~ case_when( . >= 0 & . <= 0.25 ~ "Needs Improvement",
                             . > 0.25 & . <= 0.75 ~ "Partial completed readiness",
                             . > 0.75 & . < 1 ~ "Almost completed readiness",
                             . == 1 ~ "Completed readiness",
                             TRUE ~ NA_character_ ))) %>% 
  pivot_longer(!c(1:4), names_to = "type", values_to = "readiness")

infra_readiness = infastruct.readiness %>% 
  group_by(type,readiness) %>% 
  summarise(number = n(), .groups = "drop_last") %>% 
  mutate(infra.readiness = round(number/sum(number)*100, digits = 2),
         readiness = factor(readiness, levels = rev(factor_readiness)))

tinfra_readiness = infastruct.readiness %>% 
  group_by(readiness) %>% 
  summarise(number = n(), .groups = "drop_last") %>% 
  mutate(infra.readiness = round(number/sum(number)*100, digits = 2),
         readiness = factor(readiness, levels = rev(factor_readiness)))

# Facility Readiness by health facility level/type/province
infra.hfreadiness = hf_g1(infastruct.readiness %>% 
                            mutate(readiness = factor(readiness, levels = factor_readiness)), 
                          readiness, fname = NULL)
# Automatically wrap labels to 10 characters per line 
infra_readiness$type <- str_wrap(infra_readiness$type, width = 22)
write_xlsx(c(setNames(list(infastruct.readiness), "readiness data"),
             setNames(list(infra_readiness), "readiness in percent"),
             setNames(list(tinfra_readiness), "overall readiness"),
             infra.hfreadiness), 
           "Excel datatable/infrastructure readiness.xlsx")

###########################################################
num_serdel = clean.soa %>% select(3,9:11, matches(c("serprocedure_|sersurvol_|perioperativedeathslastyear")))
sca_serdel = clean.soa %>% select(3,9:11, matches(c("safetychecklistscale_")))
# services #####
service <- num_serdel %>% 
  mutate(across(-c(1:4), ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>% 
  pivot_longer(!c(1:4), names_to = "type", values_to = "values") %>% 
  mutate(st.pattern = sub("^(([^_]*_[^_]*)).*", "\\1", type),
         structure = str_split(type, "_", simplify = TRUE)[, 1],
         main.structure = str_to_sentence(str_replace_all( 
           str_split(type, "_", simplify = TRUE)[, 2], c("\\." = " "))),
         sub.structure = str_to_sentence(str_replace_all(
           str_split(type, "_", simplify = TRUE)[, 3], c("\\." = " ")))) %>% 
  select(1:4,structure,main.structure,sub.structure,values)

#######################################################################
## Number of surgical procedures done per year #####
# Total surgery volume #####
surgery <- service %>% 
  filter( main.structure == "Totalsurgery")

ma.mi.surg = surgery %>% 
  group_by(sub.structure) %>% 
  summarise(tmamisurg = sum(values, na.rm = TRUE)) %>% 
  mutate(permanisurg = round(tmamisurg/sum(tmamisurg)*100, digits = 2))

# Total sub surgery volume by hf level/type
tsub.surgery = onlysum(surgery,
                   sub.structure, values, fname = NULL)

# Total surgery volume by hf level/type
tsurgery = onlysum(surgery,
                   main.structure, values, fname = NULL)
# Total surgery volume by province
ytsurgery = left_join(tsurgery$byprovince, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`Surgical procedures per 100,000 population` = round((staff/Total)*100000 ,digits = 0)) %>% 
  select(1,`Total Surgical procedures`= 3,8) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

camsurpercapita = round((sum(tsurgery$byhfl$staff)/province$Total[nrow(province)]) * 100000, digits = 0)
gap.camsurpercapita = (100- (round(5000 - camsurpercapita, digits = 0)/5000 *100))

write_xlsx(c(setNames(list(surgery), "Surgery"),
             tsurgery,
             setNames(list(ytsurgery), "Surgeryper100000")), 
           "Excel datatable/surgery volume.xlsx")

write_xlsx(c(setNames(list(ma.mi.surg), "Surgery volume"),tsub.surgery), 
           "Excel datatable/major-minor surgery volume.xlsx")

#######################################################################
# Total Operating Theatres #####
operating.theatre <- service %>% 
  filter( main.structure %in% c(unique(service$main.structure)[1:6])) %>% 
  group_by(geninfo_province,geninfo_hcfacilitylevel,geninfo_hcfacilitytype,geninfo_hospitalfund,structure) %>% 
  summarise(values = sum(values, na.rm = TRUE), .groups = "drop_last") %>% 
  mutate(structure = "service")
# Total Operating Theatres by hf level/type
toperating.theatre = onlysum(operating.theatre,
                             structure, values, fname = NULL)

# Total Operating Theatres by province
ytoperating.theatre = left_join(toperating.theatre$byprovince, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`Operating theatre procedures per 100,000 population` = round((staff/Total)*100000 ,digits = 0)) %>% 
  select(1,`Total operating theatre procedures`= 3,8) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

oper.theatre.percapita = round((sum(toperating.theatre$byhfl$staff)/province$Total[nrow(province)]) * 100000, digits = 0)
gap.oper.theatre.percapita = (100- (round(5000 - oper.theatre.percapita, digits = 0)/5000 *100))
write_xlsx(c(setNames(list(operating.theatre), "operatingtheatre"),
             toperating.theatre,
             setNames(list(ytoperating.theatre), "operatingtheatreyper100000")), 
           "Excel datatable/total operating theatre.xlsx")
#######################################################################
# Procedures  #####
procedure <- service %>% 
  group_by(main.structure,sub.structure) %>% 
  summarise(procedure = sum(values, na.rm = TRUE), .groups = 'drop_last' ) %>%
  arrange(desc(procedure))

# Total procedure #####
tprocedure = procedure %>% 
  group_by(main.structure) %>% 
  summarise(number = n(),
            total_value = sum(procedure, na.rm = TRUE), .groups = 'drop') %>% 
  filter(main.structure != "Perioperativedeath")

# Service delivery by hostipal level/type/province #####
tserdel = onlysum(service %>% 
                    mutate(main.structure = factor(main.structure, levels = rev(factor_serdel))) %>% 
                    filter(main.structure != "Perioperativedeath"),
                  main.structure, values, fname = NULL)

write_xlsx(c(setNames(list(service), "service delivery"),
             setNames(list(tprocedure), "Operating procedures"),
             setNames(list(procedure), "Operating procedures by subtype"),
             tserdel),
           "Excel datatable/service delivery.xlsx")
##################################################################
# Bellwether procedure #####
# 207-208-209: conditional bellwether #####
cond.bellwether = clean.soa %>% 
  select(3,9:11,matches(c("otherinfo_t"))) %>% 
  select(1:4,7:9) %>% 
  mutate(across(matches(c("otherinfo_t"), ignore.case = TRUE),
                ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>% 
  pivot_longer(!c(1:4), names_to = "type", values_to = "values") %>% 
  mutate(type = str_replace_all(type,  c("\\." = " ", "otherinfo_t" = "")))


# bellwether by hospital level/type and province
totalconbell = onlysum(cond.bellwether,
                       type, values, fname = NULL)

acond.bellwether = cond.bellwether %>%
  group_by(type) %>% 
  summarise(number = n(),
            tvalue = sum(values, na.rm = TRUE))

write_xlsx(c(setNames(list(cond.bellwether),"bellwether-data"),
             setNames(list(acond.bellwether),"bellwether"),
             totalconbell),
           "Excel datatable/bellwether procedures.xlsx")

hmis.data =  readxl::read_excel("data/NSOAP-clean.xlsx", sheet = 5)
hmis.surgery.data = hmis.data %>% 
  mutate(`surveyed data` = c(acond.bellwether$tvalue[[1]],sum(tsurgery$byhfl$staff),
                             acond.bellwether$tvalue[[1]]+sum(tsurgery$byhfl$staff)),
         `surgery&bellwether` = c(acond.bellwether$tvalue[[1]],
                                  sum(tsurgery$byhfl$staff) + acond.bellwether$tvalue[[2]] + acond.bellwether$tvalue[[3]],
                                  sum(tsurgery$byhfl$staff) + acond.bellwether$tvalue[[1]] + acond.bellwether$tvalue[[2]] + acond.bellwether$tvalue[[3]]))

pub.cond.bellwether = cond.bellwether %>% filter(geninfo_hospitalfund == "Public")
pub.bellwether = onlysum.bellwether(pub.cond.bellwether,
                                    type, values, fname = NULL)
# 207-208-209: conditional bellwether #####
cond.bellwether1 = clean.soa %>% 
  select(3,9:11,matches(c("otherinfo_t"))) %>% 
  select(1:4,7:9) %>% 
  mutate(across(matches(c("otherinfo_t"), ignore.case = TRUE),
                ~ as.numeric(str_extract(as.character(.x), "\\d+"))),
         caesarean = case_when(
           otherinfo_tcaesarean.delivery >0 ~ 1,
           TRUE ~ 0),
         treatment.open.fracture = case_when(
           otherinfo_ttreatment.open.fracture >0 ~ 1,
           TRUE ~ 0),
         laparotomy = case_when(
           otherinfo_tlaparotomy >0 ~ 1,
           TRUE ~ 0))

totalconbell1 <- cond.bellwether1 %>%
  mutate(tcon.bellw = caesarean + laparotomy + treatment.open.fracture,
         tcon.bellw = case_when(tcon.bellw == 3 ~ "All three types of bellwether procedure",
                               tcon.bellw == 2 ~ "At least 2 types of bellwether procedure",
                               tcon.bellw == 1 ~ "At least 1 type of bellwether procedure",
                               tcon.bellw == 0 ~ "No bellwether procedure"))

tconbell = hf_g1(totalconbell1, tcon.bellw, fname = NULL)

write_xlsx(c(setNames(list(cond.bellwether1),"bellwether-data"),
             setNames(list(totalconbell1),"bellwether-classes"),
             tconbell),
           "Excel datatable/bellwether-classification.xlsx")

################################################################################
# Quality and Safety checklist #####
whosafetychecklist = clean.soa %>% 
  select(3,9:11,
         which(str_detect(names(.), "safety.checklist.used"))) %>%
  pivot_longer(!c(1:4), names_to = "type", values_to = "scales") %>%
  mutate( type = str_to_sentence(str_replace_all(
    str_remove(type,"serqualitysafety_safetychecklistscale_"), c("\\." = " ")))) %>% 
  separate(scales, into = c("scale", "pscale"), sep = " \\(") %>%
  mutate(pscale = gsub("\\)", "", pscale)) %>% 
  select(-pscale)

pub.pri.checklist = whosafetychecklist %>% 
  mutate(scale = case_when(scale %in% c("Always") ~ "Always",
                           TRUE ~ "Not Always")) %>% 
  group_by(geninfo_hospitalfund, scale) %>% 
  summarise( number = n(), .groups = 'drop') %>% 
  mutate(percent = round((number/sum(number) *100 ), digits = 2))

# WHO safety checklist #####
checklist = whosafetychecklist %>% 
  group_by(scale) %>% 
  summarise( number = n(), .groups = 'drop_last') %>% 
  mutate(percent = round((number/sum(number) *100 ), digits = 2))

safetychecklist = hf_g1(whosafetychecklist,scale, fname = NULL)

write_xlsx(c(setNames(list(whosafetychecklist),"data"),
             setNames(list(checklist),"percent"),
             setNames(list(pub.pri.checklist),"pub-private"),
             safetychecklist),
           "Excel datatable/whosafetychecklist.xlsx")

# Association between surgical facility standards and performance of other essential procedures #####
# Safe surgery Infrastructure
safe.sur.infra <- sca_infra %>% 
  mutate(across(.cols = -c(1:4), ~ case_when(
    . %in% c("Always (100%)","All (100%)") ~ 1,
    TRUE ~ 0))) %>% 
  mutate(number = rowSums(across(-c(1:4)), na.rm = TRUE),
         score = round(number/(length(names(.))-4)*100, digits = 2),
         safe.infra = case_when(score >=0 & score <100 ~ "Not available",
                                score == 100 ~ "Fully Available")) %>% 
  select(1:4,72)

##################################################################
# 142 & 206: Peri-operation death by categorie/province/hospitals #####
peri.operation = clean.soa %>% 
  select(3,9:11,142,206) %>% 
  mutate(across(matches(c("serqualitysafety_|otherinfo_"), ignore.case = TRUE),
                ~ as.numeric(str_extract(as.character(.x), "\\d+"))),
         fperioper = case_when(otherinfo_tperioperative.death > serqualitysafety_perioperativedeath_perioperativedeathslastyear ~ otherinfo_tperioperative.death,
                               TRUE ~ serqualitysafety_perioperativedeath_perioperativedeathslastyear)) %>% 
  pivot_longer(!c(1:4), names_to = "type", values_to = "values") %>% 
  filter(type == "fperioper")

# Peri-operation death by hostipal level/type/province #####
tperi.operation = onlysum(peri.operation,
                          type, values, fname = NULL)

write_xlsx(c(setNames(list(peri.operation),"data"),
             tperi.operation),
           "Excel datatable/peri operation.xlsx")

##################################################################
# All Other procedure and surgerical procedure #####
sur.other.procedure <- num_serdel %>%
  mutate(across(-c(1:4), ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>% 
  mutate(tother.procedure = rowSums(select(., matches("specialty_|obstetricsgynaecology_|non.trauma.orthopedic_|injury.related_")),
                                    na.rm = TRUE),
         tsur.procedure = rowSums(select(., matches("totalsurgery_")),
                                  na.rm = TRUE),
  ) %>% 
  select(1:4,59:60)

#colSums(select(num_serdel %>%  mutate(across(-c(1:4), ~ as.numeric(str_extract(as.character(.x), "\\d+")))),  matches("specialty_|obstetricsgynaecology_|non.trauma.orthopedic_|injury.related_")), na.rm = TRUE)

#############################################################################
# Peri operation death #####
ntperi.operation = sum(peri.operation$values, na.rm = TRUE)
ntoperating.theatre = sum(operating.theatre$values, na.rm = TRUE)
#tadmission = sum(num_soa$infrastructure_generalnumeric_total.admission, na.rm = TRUE)
# Surgery mortality rate per 100000 population
nsurg.mortality.rate = round(ntperi.operation/ntoperating.theatre *100000, digits = 0)
p.nsurg.mortality.rate = round(ntperi.operation/ntoperating.theatre *100, digits = 2)

###########################################################
# Density of workforce #####
wforce = clean.soa %>% select(3,9:11, matches(c("workforce_"))) %>% 
  mutate(across(matches(c("workforce_"), ignore.case = TRUE), 
                ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>% 
  pivot_longer(!c(1:4), names_to = "workforces", values_to = "values") %>% 
  mutate(workforces = str_to_sentence(str_replace_all(
    str_remove(workforces, "workforce_"), c("\\." = " ", "Total " = ""))))

# National workforce #####
totalden.wforce = wforce %>% 
  group_by(workforces) %>%
  summarise(number = n(),
            tvalue = sum(values, na.rm = TRUE), .groups = 'drop_last') %>%
  mutate( percent = round(tvalue / sum(tvalue, na.rm = TRUE) * 100, digits = 2))

# by hostipal level/type/province
den.wforce = onlysum(wforce,workforces,values)

write_xlsx(c(setNames(list(wforce),"data"),
             setNames(list(totalden.wforce),"total workforces"),
             den.wforce),
           "Excel datatable/workforce.xlsx")

# adjusted National workforce distribution of SOA service providers in health facilities
totalden.wforce.adj = wforce %>% 
  filter(workforces != "Specilist wf") %>% 
  mutate(values = case_when(geninfo_hospitalfund == "Private" ~ round((values*12.7)/100,digits = 4),
                            TRUE ~ values)) %>% 
  group_by(workforces) %>%
  summarise(number = n(),
            tvalue = round(sum(values, na.rm = TRUE),digits = 2), .groups = 'drop_last') %>%
  mutate( percent = round(tvalue / sum(tvalue, na.rm = TRUE) * 100, digits = 2))

# by hostipal level/type/province
den.wforce.adj = onlysum.wf(wforce,workforces,values)
prov.wforce.adj = den.wforce.adj$byprovince %>% 
  group_by(geninfo_province,workforces) %>% 
  summarise(surveyed.staff = sum(staff),
            adjusted.staff = round(sum(adjusted.staff), digits = 2), .groups = "drop_last")

# Final adjusted workforces #####
adj.nat.wforce = left_join(totalden.wforce %>% select(1,`surveyed workforces` = 3),totalden.wforce.adj %>% select(1,`adjusted workforces` = 3), by= "workforces")

write_xlsx(c(setNames(list(adj.nat.wforce),"WFadjusted"),
             den.wforce.adj,
             setNames(list(prov.wforce.adj),"WFadjusted by province")),
           "Excel datatable/workforce-adjustment.xlsx")

###############################################################################
# Workforce availability #####
scale_wforce = clean.soa %>% select(3,9:11, matches(c("workforcescale_surgicalavailability_","workforcescale_education_")))

wforce.avail = scale_wforce %>% 
  select(1:8) %>% 
  pivot_longer(!c(1:4), names_to = "workforces", values_to = "scales") %>% 
  mutate(workforces = str_to_sentence(str_replace_all(
    str_remove(workforces, "workforcescale_surgicalavailability_"), c("\\." = ""))))
###############################################################################
# Nurse patients ratio
wforce.availability = wforce.avail %>% 
  group_by(scales) %>% 
  summarise(number = n(), .groups = "drop_last") %>%
  mutate(percent = round((number/sum(number))*100, digits = 2))

# ward workforce by health facility level/type/province #####
ward = hf_g1(wforce.avail, scales, fname = NULL)

write_xlsx(c(setNames(list(wforce.avail),"data"),
             setNames(list(wforce.availability),"Nurse patients ratio"),
             ward),
           "Excel datatable/workforce-nurse patients ratio.xlsx")
###############################################################################
# Provider availability #####
provider.avail = scale_wforce %>% 
  select(1:4,9:(ncol(scale_wforce)-1)) %>% 
  pivot_longer(!c(1:4), names_to = "workforces", values_to = "scales") %>% 
  mutate(workforces = str_to_sentence(str_replace_all(
    str_remove(workforces, "workforcescale_surgicalavailability_"), c("\\." = " ")))) %>% 
  mutate(scale = case_when( str_detect(scales, "(100%)") ~ "Always", 
                            str_detect(scales, "(76-99%)") ~ "Almost always", 
                            str_detect(scales, "(51-75%)") ~ "Often", 
                            str_detect(scales, "(26-50%)") ~ "Sometimes", 
                            str_detect(scales, "(1-25%)") ~ "Rarely)", 
                            str_detect(scales, "(0%)") ~ "Never",
                            TRUE ~ scales))

# Automatically wrap labels to 10 characters per line and Wrap facet labels to 20 characters per line
provider.avail$workforces <- 
  str_wrap(provider.avail$workforces, width = 30)

aprovider = hf_g1.g2(provider.avail, workforces, scale, fname = NULL)
write_xlsx(c(setNames(list(provider.avail),"data"),
             aprovider),
           "Excel datatable/workforce-provider availability.xlsx")

# Workforce availability 24h/7days a week #####
wforce.avail.allday <- scale_wforce %>% 
  select(1:4, 9:(ncol(scale_wforce)-1)) %>%
  mutate(across(-c(1:4), ~case_when(
    str_detect(.x, "100%") ~ 5,
    str_detect(.x, "76-99%") ~ 4,
    str_detect(.x, "51-75%") ~ 3,
    str_detect(.x, "26-50%") ~ 2,
    str_detect(.x, "1-25%") ~ 1,
    str_detect(.x, "0%") ~ 0,
    TRUE ~ NA_real_ ))) %>% 
  mutate(sumnumber = rowSums(across(c(5:8)), na.rm = TRUE),
         score = round((sumnumber/((length(names(.))-4)*5)) *100, digits = 2),
         fullavail = case_when(score >0 & score <= 25 ~ "Needs Improvement",
                               score > 25 & score <= 75 ~ "Partial Availability",
                               score > 75 & score < 100 ~ "High Availability",
                               score == 100 ~ "Full Availability"))
write_xlsx(wforce.avail.allday,
           "Excel datatable/Workforce availability 24h.xlsx")
# Workforce availability 24h/7days a week #####
gt.wforce.avail.allday = wforce.avail.allday %>% 
  group_by(fullavail) %>% 
  summarise(number = n(),.groups = 'drop_last') %>% 
  mutate(percent = round(number/sum(number)*100, digits = 2)) %>% 
  filter(fullavail != "NA")
#writexl::write_xlsx(gt.wforce.avail.allday, "Excel datatable/Workforce full availability.xlsx")
###############################################################################
# Ward availability
wforce.avail.proportion <- wforce.avail %>% 
  group_by(workforces, scales) %>% 
  summarise(staff = n(), .groups = 'drop_last') %>% 
  mutate(percent = round((staff/sum(staff) *100 ), digits = 2),
         tpercent = round((staff/(nrow(wforce.avail))) * 100, 2))

# by hostipal level/type/province
den.wforce.avail = hf_g1.g2(wforce.avail,workforces, scales, fname = NULL)

write_xlsx(c(setNames(list(wforce.avail.proportion),"data"),
             den.wforce.avail),
           "Excel datatable/workforce ward availability.xlsx")

###########################################################################
# All Workforces #####
raw_workforce <- clean.soa %>%
  select(3,9:11, matches("workforce_", ignore.case = TRUE)) %>%
  mutate(across(matches("workforce_", ignore.case = TRUE),
                ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>%
  mutate( surgeons = rowSums(across(c(5,6)), na.rm = TRUE),
          `anesthesia professionals` = rowSums(across(c(8,9,11,13,15,22)), na.rm = TRUE),
          `allied health provider` = rowSums(across(c(16:21)), na.rm = TRUE),
          `SAO specialist` = rowSums(across(c(5,6,7,8)), na.rm = TRUE),
          `all workforce` = rowSums(across(c(5:22)), na.rm = TRUE))
raw.workforce =  raw_workforce %>% select(1:4,23:27)

psurgeon = raw.workforce %>% 
  select(1:5) %>% 
  mutate(per.surgeon = case_when(surgeons > 0 ~ "Surgeon",
                                 surgeons == 0 ~ "No surgeon"))

perc.nonsurgeon = psurgeon %>% 
  group_by(per.surgeon) %>% 
  summarise(number = n(), .groups = "drop_last") %>% 
  mutate(percent = round(number/sum(number)*100, digits = 2) )

tpsurgeon = percent.workforce(psurgeon,surgeons, fname = NULL)

allworkforce <- raw.workforce %>% pivot_longer(!c(1:4), names_to = "type", values_to = "values")

# Total workforce by hf level/type
tallworkforce = onlysum(allworkforce,
                        type, values, fname = NULL)
# Total workforces by province
ytallworkforce = left_join(tallworkforce$byprovince, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`Workforces per 100,000 population` = round((staff/Total)*100000 ,digits = 0))

write_xlsx(c(setNames(list(raw_workforce),"rawdata"),
             setNames(list(allworkforce),"workforces"),
             tallworkforce,
             setNames(list(ytallworkforce),"workforces per 100000")),
           "Excel datatable/workforce by type.xlsx")

## Specialist surgical workforce density
## all workforce per 100,000 population
ytallwf = ytallworkforce %>% 
  filter(type == "all workforce")  %>% 
  select(1,`all workforce`= 3,8) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

ytallwfpercapita = round((sum(raw.workforce$`all workforce`)/province$Total[nrow(province)]) * 100000, digits = 0)
gap.ytallwfpercapita = round(20- ytallwfpercapita, digits = 0) 

## Number of specialist surgical, anesthetic, and obstetric physicians who are working per 100,000 population
ytsao.specialist = ytallworkforce %>% 
  filter(type == "SAO specialist")  %>% 
  select(1,`SAO specialist`= 3,8) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

saopercapita = round((sum(raw.workforce$`SAO specialist`)/province$Total[nrow(province)]) * 100000, digits = 0)
gap.saopercapita = round(20- saopercapita, digits = 0) 

## Number of surgeons per 100,000 population
ytsurgeons = ytallworkforce %>% 
  filter(type == "surgeons")  %>% 
  select(1,`surgeons`= 3,8) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

surgeonspercapita = round((sum(raw.workforce$surgeons)/province$Total[nrow(province)]) * 100000, digits = 0)
gap.surgeonspercapita =  round(20- surgeonspercapita, digits = 0)

## Number of anesthesia professionals per 100,000 population
ytanesthesia.professionals = ytallworkforce %>% 
  filter(type == "anesthesia professionals")  %>% 
  select(1,`anesthesia professionals`= 3,8) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

anesthesiapercapita = round((sum(raw.workforce$`anesthesia professionals`)/province$Total[nrow(province)]) * 100000, digits = 0)
gap.anesthesiapercapita = round(20- anesthesiapercapita, digits = 0)

## Number of allied health provider per 100,000 population
ytalliedhprovider = ytallworkforce %>% 
  filter(type == "allied health provider")  %>% 
  select(1,`allied health provider`= 3,8) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

alliedhprovider.percapita = round((sum(raw.workforce$`allied health provider`)/province$Total[nrow(province)]) * 100000, digits = 0)
gap.alliedhprovider.percapita = round(20- alliedhprovider.percapita, digits = 0)

############################################
workforce.adjust = clean.soa %>%
  select(3,9:11, matches("workforce_", ignore.case = TRUE)) %>%
  mutate(across(matches("workforce_", ignore.case = TRUE),
                ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>%
  mutate(`specilist wf` = rowSums(across(c(5:8)), na.rm = TRUE),
         `adjusted specilist wf` = case_when(geninfo_hospitalfund == "Private" ~ round((`specilist wf`*12.7)/100,digits = 2),
                                             TRUE ~ `specilist wf`),
         nurses = rowSums(across(c(14,15)), na.rm = TRUE),
         `adjusted nurses` = case_when(geninfo_hospitalfund == "Private" ~ round((nurses*12.7)/100,digits = 2),
                                         TRUE ~ nurses),
         midwives = rowSums(across(c(16:17)), na.rm = TRUE),
         `adjusted midwives` = case_when(geninfo_hospitalfund == "Private" ~ round((midwives*12.7)/100,digits = 2),
                                          TRUE ~ midwives),
         `allied health provider` = rowSums(across(c(18:21)), na.rm = TRUE),
         `adjusted allied HP` = case_when(geninfo_hospitalfund == "Private" ~ round((`allied health provider`*12.7)/100,digits = 2),
                                              TRUE ~ `allied health provider`),
         `all workforce` = rowSums(across(c(5:22)), na.rm = TRUE),
         `adjusted all workforce` = case_when(geninfo_hospitalfund == "Private" ~ round((`all workforce`*12.7)/100,digits = 2),
                                             TRUE ~ `all workforce`)) %>% 
  select(1:4,23:32)

allworkforce.adjust <- workforce.adjust %>% pivot_longer(!c(1:4), names_to = "type", values_to = "values")

# Total workforce by hf level/type
tallworkforce.adjust = onlysum(allworkforce.adjust,
                        type, values, fname = NULL)
# Total workforces by province
ytallworkforce.adjust = left_join(tallworkforce.adjust$byprovince, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`Workforces per 100,000 population` = round((staff/Total)*100000 ,digits = 0))

write_xlsx(c(setNames(list(workforce.adjust),"rawdata"),
             setNames(list(allworkforce.adjust),"workforces"),
             tallworkforce.adjust,
             setNames(list(ytallworkforce.adjust),"workforces per 100000")),
           "Excel datatable/workforce adjust.xlsx")

wfperpop = ytallworkforce.adjust %>% select(1:3,8) %>% 
  pivot_wider(names_from = type, values_from = c(staff, `Workforces per 100,000 population`)) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

## all workforce per 100,000 population
ytallwf.adjust = ytallworkforce.adjust %>% 
  filter(type == "adjusted all workforce")  %>% 
  select(1,`all workforce`= 3,8) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

ytallwf.adjust.percapita = round((sum(adj.nat.wforce$`adjusted workforces`)/province$Total[nrow(province)]) * 100000, digits = 0)
gap.ytallwf.adjust.percapita = round(20- ytallwf.adjust.percapita, digits = 0)

## Specialist surgical workforce density
ytspecialistwf.adjust = ytallworkforce.adjust %>% 
  filter(type == "adjusted specilist wf")  %>% 
  select(1,`specilist workforce`= 3,`specilist workforce per 100,000 population` = 8) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

all.specialistwf.adjust = allworkforce.adjust %>% 
  group_by(type) %>% 
  summarise(tvalue = sum(values,na.rm = TRUE)) %>% 
  filter(type == "adjusted specilist wf")

yall.specialistwf.adjust.percapita = round((sum(all.specialistwf.adjust$tvalue)/province$Total[nrow(province)]) * 100000, digits = 0)
gap.yall.specialistwf.adjust.percapita = round(20 - yall.specialistwf.adjust.percapita, digits = 0)

###########################################################################
# Information dataset #####
sca_infmang = clean.soa %>% select(3,9:11, matches(c("infmanagement_")))
num_infmang = clean.soa %>% select(3,9:11, matches(c("infmanagementnumeric_")))

info.mang = sca_infmang %>%  
  pivot_longer(!c(1:4), names_to = "type", values_to = "scales") %>% 
  mutate(st.pattern = sub("^(([^_]*_[^_]*)).*", "\\1", type),
         structure = str_split(type, "_", simplify = TRUE)[, 1],
         main.structure = str_split(type, "_", simplify = TRUE)[, 2],
         sub.structure = str_to_sentence(str_replace_all(
           str_split(type, "_", simplify = TRUE)[, 3], c("\\." = " ")))) %>% 
  select(1:4,main.structure,sub.structure,scales)

trans.ds <- sca_infmang %>% 
  select(1:4,5:9,11) %>% 
  mutate(across(.cols = -c(1:4), ~ case_when(
    . %in% c("Electronic", "Paper", "Both", "Yes", "Always (100%)", "Yes (if yes, what kind?)") ~ 1,
    . %in% c("Never (0%)", "Rarely (1-25%)", "Sometimes (26-50%)", "Often (51-75%)", "Almost always (76-99%)", "None", "No") ~ 0,
    TRUE ~ NA_real_))) %>% 
  mutate(score = rowSums(across(-c(1:4)), na.rm = TRUE),
         ds.scale = case_when(score >= 0 & score <= 1 ~ "Needs Improvement",
                              score >= 2 & score <= 4 ~ "Partial completed readiness",
                              score == 5 ~ "Almost completed readiness",
                              score == 6 ~ "Completed readiness"))
# Presence of data management system
# by health facility level/type/province
presence.ds = hf_g1(trans.ds, ds.scale, fname = NULL)

write_xlsx(c(setNames(list(info.mang),"rawd ata"),
             setNames(list(trans.ds),"scale data"),
             presence.ds),
           "Excel datatable/data system-presence.xlsx")

# Method of record keeping
nmrk_recordkeeping = info.mang %>% filter(sub.structure == unique(info.mang$sub.structure)[1]) %>% 
  mutate(scales = case_when(scales == "Both" ~ "Electronic and paper",
                            TRUE ~ scales
  )) %>% 
  #  mutate(scales = case_when(scales == "Both" ~ "Electronic and paper",scales == "Electronic" ~ "Electronic and paper", TRUE ~ scales  )) %>% 
  group_by(scales) %>% 
  summarise( number = n(), .groups = 'drop') %>% 
  mutate(percent = round((number/sum(number) *100 ), digits = 2),
         scales = factor(scales, levels = factor_recordkeeping)) %>% 
  filter(scales != "NA")

# by health facility level/type/province
recordkeeping = hf_g1(info.mang %>% 
                        filter(sub.structure == unique(info.mang$sub.structure)[1]) %>% 
                        mutate(scales = case_when(scales == "Both" ~ "Electronic and paper", TRUE ~ scales),
                               scales = factor(scales, levels = factor_recordkeeping)),
                      #  mutate(scales = case_when(scales == "Both" ~ "Electronic and paper",scales == "Electronic" ~ "Electronic and paper", TRUE ~ scales  )),
                      scales, fname = NULL)
write_xlsx(c(setNames(list(nmrk_recordkeeping),"data"),
             recordkeeping),
           "Excel datatable/data system-recordkeeping.xlsx")
# Tele-medicine
telemedicince = info.mang %>% filter(sub.structure == unique(info.mang$sub.structure)[7]) %>% 
  group_by(scales) %>% 
  summarise( number = n(), .groups = 'drop') %>% 
  mutate(percent = round((number/sum(number) *100 ), digits = 2),
         scales = case_when(scales == "Yes (if yes, what kind?)" ~ "Yes",
                            TRUE ~ scales)) %>% 
  filter(scales != "NA")

# by health facility level/type/province
ttelemedicince = hf_g1(info.mang %>% 
                         filter(sub.structure == unique(info.mang$sub.structure)[7]) %>% 
                         mutate(scales = case_when(scales == "Yes (if yes, what kind?)" ~ "Yes",
                                                   TRUE ~ scales)) %>% 
                         filter(scales != "NA"),
                       scales, fname = NULL)

# Telemedicine tools used
ctelemedicine = info.mang %>% 
  filter(str_detect(sub.structure, regex("whatkind", ignore_case = TRUE))) %>% 
  distinct(scales) %>% 
  pull() %>% 
  tibble(raw = .) %>% 
  distinct()

# Define common communication tools/devices (adjust as needed)
comm_tools <- str_to_lower(c("telegram", "zoom", "phone", "mobile", "facebook", "messenger", "video call",
                             "google meet", "linkedin", "email", "social media", "call", "line",
                             "Camlis","Antibiotics","HC","hospital system", "System Record",
                             "Telemedicine in the Radiology Field","System Record",
                             "twitter","LinkedIn","Aischannel","Educare boston scientific",
                             "Medscape","UpToDate","EmBoss","AI","dashboard" ))
# Detect and count how many tools are mentioned
tool.telemedicine <- ctelemedicine %>%
  mutate( raw_lower = str_to_lower(raw),
          tools_used = str_extract_all(raw_lower, str_c(comm_tools, collapse = "|")),
          n_tools = lengths(tools_used)) %>% 
  unnest(tools_used) %>%
  count(tools_used, sort = TRUE)
tool.telemedicine = as.data.frame(tool.telemedicine) %>% arrange(desc(n))

write_xlsx(c(setNames(list(telemedicince),"telemedicine used"),
             setNames(list(tool.telemedicine),"tools used"),
             ttelemedicince),
           "Excel datatable/data system-telemedicince.xlsx")

## MoH report submission
Mohreporting = info.mang %>% filter(sub.structure %in% c(unique(info.mang$sub.structure)[6])) %>% 
  group_by(scales) %>% 
  summarise( number = n(), .groups = 'drop_last') %>% 
  mutate(percent = round((number/sum(number) *100 ), digits = 2),
         scales = factor(scales, levels = factor_training)) %>% 
  filter(scales != "NA")

# Medical record maintenance-Patient chart accessibility
personnel.chart = info.mang %>% filter(sub.structure %in% c(unique(info.mang$sub.structure)[c(2:3)])) %>% 
  group_by(sub.structure, scales) %>% 
  summarise( number = n(), .groups = 'drop_last') %>% 
  mutate(percent = round((number/sum(number) *100 ), digits = 2)) %>% 
  filter(scales != "NA")

# by health facility level/type/province
tpersonnel.chart = hf_g1.g2(info.mang %>% 
                              filter(sub.structure %in% c(unique(info.mang$sub.structure)[c(2:3)])) %>% 
                              mutate(sub.structure = str_wrap(sub.structure, width = 20)),
                            sub.structure, scales, fname = NULL)
#############################
# all management system
allms = info.mang %>% filter(sub.structure %in% c(unique(info.mang$sub.structure)[c(1:5,7)])) %>% 
  group_by(sub.structure, scales) %>% 
  summarise( number = n(), .groups = 'drop_last') %>% 
  mutate(percent = round((number/sum(number) *100 ), digits = 2)) %>% 
  filter(scales != "NA")

# by health facility level/type/province
hfallms = hf_g1.g2(info.mang %>% 
                     filter(sub.structure %in% c(unique(info.mang$sub.structure)[c(1:5,7)])) %>% 
                     mutate(sub.structure = str_wrap(sub.structure, width = 20)),
                   sub.structure, scales, fname = NULL)
write_xlsx(c(setNames(list(allms),"allMS"),
             hfallms),
             "Excel datatable/all data management system.xlsx")
             
##########################
# Training
training = info.mang %>% filter(sub.structure %in% c(unique(info.mang$sub.structure)[c(10:13)])) %>% 
  group_by(sub.structure, scales) %>% 
  summarise( number = n(), .groups = 'drop_last') %>% 
  mutate(percent = round((number/sum(number) *100 ), digits = 2),
         scales = factor(scales, levels = factor_training)) %>% 
  filter(scales != "NA")

## Ongoing training courses
ongoingresearch <- num_infmang %>%
  rename_with(.fn = ~ str_split(., "_", simplify = TRUE)[, 3], .cols = 5) %>%
  mutate(nresearch = str_extract_all(.[[5]], "\\d+") %>%
           lapply(as.numeric) %>%
           sapply(sum, na.rm = TRUE)) %>% 
  group_by(nresearch) %>% 
  summarise(ogresearch = n(), .groups = "drop_last") %>% 
  mutate(percent = round(ogresearch/sum( ogresearch,na.rm = TRUE) *100, digits = 2))

## Research funding
research.fund = info.mang %>% filter(sub.structure %in% c(unique(info.mang$sub.structure)[c(14)])) %>% 
  group_by(scales) %>% 
  summarise( number = n(), .groups = 'drop_last') %>% 
  mutate(percent = round((number/sum(number) *100 ), digits = 2),
         scales = factor(scales, levels = factor_yesno)) %>% 
  filter(scales != "NA")

write_xlsx(c(setNames(list(Mohreporting),"Mohreporting"),
             setNames(list(personnel.chart),"personnel chart"),
             tpersonnel.chart,
             setNames(list(training),"training"),
             setNames(list(ongoingresearch),"ongoing research"),
             setNames(list(research.fund),"research fund")),
             "Excel datatable/data system-other.xlsx")

###########################################################################
# Finance #####
financing = clean.soa %>% select(3,9:11, matches(c("financing")))
# Health insurance
hinsurance = financing %>% 
  select(1:4,finance = 5) %>% 
  separate(finance, into = c("insurance", "pscale"), sep = " \\(") %>% 
  select(1:5) %>% 
  pivot_longer(!c(1:4), names_to = "insurance", values_to = "scale") %>% 
  mutate(scale = factor(scale, levels = factor_safety))

thealth.insurance = hinsurance %>% 
  group_by(insurance,scale) %>%
  summarise(number = n(), .groups = "drop_last") %>%
  mutate( percent = round((number / sum(number)) * 100, digits = 0),
          scale = factor(scale, levels = factor_safety)) %>% 
  filter(scale != "NA")

dshealth.insurance = hf_g1(hinsurance, scale, fname = NULL)

# Out-of-Pocket expenditure
##### Reference list of correct names
cor.curency <- c("na", "kh", "usd", "riel","dollar","ខ្មែរ","រៀល","ដុល្លា")
##### Function to find the closest correct name
match_name <- function(geninfo_province, reference_list) {
  distances <- stringdist(geninfo_province, reference_list, method = "lv")  # Levenshtein distance
  reference_list[which.min(distances)]
}

# Define a conversion function to extract and normalize numbers
extract_numeric_value <- function(x) {
  x <- tolower(as.character(x))
  
  # Remove anything after a dash, newline, or unwanted separator
  x <- str_split_fixed(x, "[-\r\n]", 2)[, 1]
  
  # Convert Khmer number words to multipliers
  x <- str_replace_all(x, "ម៉ឺន", "0000")  # 1 ម៉ឺន = 10,000
  x <- str_replace_all(x, "លាន", "000000") # 1 លាន = 1,000,000
  x <- str_replace_all(x, "millions", "000000")
  x <- str_replace_all(x, "[^\\d\\.]", "") # Keep only digits and dot
  
  # Extract only the first numeric sequence (digits with optional decimal)
  num <- as.numeric(str_extract(x, "\\d+(\\.\\d+)?"))
}

# Apply convert_khmer_numbers to all character columns
outofpocket = financing %>% 
  select(-5) %>%
  mutate(financing_budget.allocation_currency = case_when(financing_budget.allocation_currency %in% c("User fee list", "National Currency","self-paying illness  and nssf gov","By Phone","ក្រដាសប្រាក់", "៛") ~ "riel",
                                                          financing_budget.allocation_currency %in% c("Not sure","unclear") ~ "na",
                                                          financing_budget.allocation_currency == "$" ~ "usd",
                                                          TRUE ~ financing_budget.allocation_currency),
         currency = sapply(str_to_lower(financing_budget.allocation_currency), match_name, reference_list = cor.curency)) %>% 
  select(1:4,19,6:18) %>% 
  mutate(across(6:ncol(.), ~ extract_numeric_value(.x)))


###########################################################################
# 202: SOA committee #####
committee <- clean.soa %>%
  select(3,9:11,
         which(str_detect(names(.), "SOA.committee"))) %>% 
  pivot_longer(!c(1:4), names_to = "otherservice", values_to = "scale") 

committee.soa = committee %>% 
  group_by(otherservice,scale) %>%
  summarise(number = n(), .groups = "drop_last") %>%
  mutate( percent = round((number / sum(number)) * 100, digits = 0),
          otherservice = str_split(otherservice, "_", simplify = TRUE)[, 3],
          scale = factor(scale, levels = factor_yesno)) %>% 
  filter(scale != "NA")

hf.committee.soa = committee %>% 
  group_by(geninfo_hospitalfund,scale) %>%
  summarise(number = n(), .groups = "drop_last") %>%
  mutate( percent = round((number / sum(number)) * 100, digits = 2),
          scale = factor(scale, levels = factor_yesno))

tcommittee = hf_g1(committee, scale, fname = NULL)

write_xlsx(c(setNames(list(committee.soa),"soacommittee"),
             tcommittee),
           "Excel datatable/governance-soacommittee.xlsx")

# 202: SOA committee for public hospital facility only
pub.committee <- clean.soa %>%
  select(3,9:11,
         which(str_detect(names(.), "SOA.committee"))) %>% 
  pivot_longer(!c(1:4), names_to = "otherservice", values_to = "scale") %>% 
  filter(geninfo_hospitalfund == "Public")

pub.committee.soa = committee %>% 
  group_by(otherservice,scale) %>%
  summarise(number = n(), .groups = "drop_last") %>%
  mutate( percent = round((number / sum(number)) * 100, digits = 0),
          otherservice = str_split(otherservice, "_", simplify = TRUE)[, 3],
          scale = factor(scale, levels = factor_yesno)) %>% 
  filter(scale != "NA")

pub.tcommittee = hf_g1(committee, scale, fname = NULL)


# 211: surgery bellwether
surgery.bellwether <- clean.soa %>%
  select(3,9:11,
         which(str_detect(names(.), "surgery.bellwether"))) %>% 
  pivot_longer(!c(1:4), names_to = "otherservice", values_to = "scale") %>% 
  group_by(otherservice,scale) %>%
  summarise(number = n(), .groups = "drop_last") %>%
  mutate( percent = round((number / sum(number)) * 100, digits = 0),
          otherservice = str_split(otherservice, "_", simplify = TRUE)[, 3],
          scale = factor(scale, levels = factor_yesno)) %>% 
  filter(scale != "NA")

###########################################################################
# Surgery #####
other.surgery = clean.soa %>% 
  select(3,9:11,matches(c("otherinfo_t","sersurvol_totalsurgery_","serqualitysafety_perioperativedeath_"))) %>%
  mutate(across(matches(c("otherinfo_t","sersurvol_totalsurgery_","serqualitysafety_perioperativedeath_"), ignore.case = TRUE),
                ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>% 
  pivot_longer(!c(1:4), names_to = "type", values_to = "values") %>%
  mutate(type = str_remove(type, "otherinfo_|sersurvol_totalsurgery_|serqualitysafety_perioperativedeath_"))

tn.surgery = other.surgery %>% split(.$type) %>%
  map(~ summarise(.x, total = sum(values, na.rm = TRUE)))

# 138-139-140: Major, minor and paediatric surgery (=tprocedure$total_value[7])
all.surg.vol = (tn.surgery$majorsurgerylastyear + tn.surgery$minorsurgerylastyear + tn.surgery$paediatricsurgerylastyear)

# 205: Total surgical operation
totalsurgery = tn.surgery$tsurgery.procedure.in.last.12.months

# 142&206: Total perioperative death
totalperioperativedeath = tn.surgery$tperioperative.death
tperioperdeath = tn.surgery$perioperativedeathslastyear

# 207-208-209: Total caesarean, laparotomy and treatment open fracture delivery
all.surg.delivery = tn.surgery$tcaesarean.delivery + tn.surgery$tlaparotomy + tn.surgery$ttreatment.open.fracture

# 210: Total surgery patient access service within 2 hours
all.surg.patient.2hour = tn.surgery$tsurgery.patient.access.service.within.2.hours

################################################################################
# Population with 2 hours access to health facilities #####
acc2hf = clean.soa %>% 
  select(3,9:11,
         which(str_detect(names(.), "patients.reach.within.2.hours"))) %>%
  pivot_longer(!c(1:4), names_to = "type", values_to = "scales") %>%
  separate(scales, into = c("scale", "pscale"), sep = " \\(") %>%
  select(1:4,access2hour = scale) %>% 
  mutate(access2hour = factor(access2hour, levels = rev(factor_safety)))
#  mutate(scale = factor(scale, levels = rev(factor_safety))) %>% filter(scale != "NA")

acc2hfbellwether = clean.soa %>% 
  select(3,9:11,bellwether=211,access2hour=87) %>% 
  filter(bellwether == "Yes") %>% 
  select(-c(5)) %>%
  pivot_longer(!c(1:4), names_to = "type", values_to = "scales") %>%
  separate(scales, into = c("scale", "pscale"), sep = " \\(") %>%
  select(-pscale) %>% 
  mutate(scale = factor(scale, levels = rev(factor_safety))) %>% 
  filter(scale != "NA")

acc2hf2hour = acc2hfbellwether %>% 
  group_by(scale) %>% 
  filter(scale != "NA") %>% 
  summarise( number = n(), .groups = 'drop_last') %>% 
  mutate(percent = round((number/sum(number) *100 ), digits = 2),
         scale = factor(scale, levels = rev(factor_safety)))

# Population with 2 hours access to health facilities by health facility level/type/province
houraccess = hf_g1(acc2hfbellwether, scale, fname = NULL)

write_xlsx(c(setNames(list(acc2hf),"rawdata"),
             setNames(list(acc2hfbellwether),"accbellwetherdata"),
             setNames(list(acc2hf2hour),"percent"),
             houraccess),
           "Excel datatable/2 hours access to health facilities.xlsx")
################################################################################
# 211: surgery bellwether #####
bellwether = clean.soa %>%
  select(3,9:11,
         which(str_detect(names(.), "surgery.bellwether"))) %>%
  pivot_longer(!c(1:4), names_to = "type", values_to = "scales")%>%
  mutate(type = str_remove(type, "otherinfo_"))

tsur.bellwether = bellwether %>% 
  group_by(scales) %>% 
  summarise(number = n(), .groups = "drop_last") %>%
  mutate(percent = round((number/sum(number))*100, digits = 2))

sur.bellwether = bellwether %>% 
  #  filter(geninfo_hospitalfund == "Public") %>% 
  group_by(geninfo_hospitalfund, scales) %>% 
  summarise(number = n(), .groups = "drop_last") %>%
  mutate(percent = round((number/sum(number))*100, digits = 2))

# surgery bellwether by health facility level/type/province
hfbellwether = hf_g1(bellwether, scales, fname = NULL)

write_xlsx(c(setNames(list(bellwether),"data"),
             setNames(list(tsur.bellwether),"percent"),
             hfbellwether),
           "Excel datatable/bellwether yesno.xlsx")
################################################################################
# Surgery beds per 100,000 people #####
geninfo <- clean.soa %>% 
  select(3,9:11, matches(c("infrastructure_generalnumeric","sersurvol_"))) %>% 
  select(-matches("scale")) %>%
  mutate(across(matches(c("infrastructure_generalnumeric","sersurvol_"), ignore.case = TRUE),
                ~ as.numeric(str_extract(as.character(.x), "\\d+"))))

bed.surgery = geninfo %>% 
  select(1:4,8,14:16) %>% 
  mutate(totalsurgery = rowSums(across(c(6,7,8)), na.rm = TRUE),
         type = "surgicalbeds") %>% 
  select(1:4,type,values = 5)

# Surgery beds ratio per 100,000 population by hf level/type
total.bed.surgery = onlysum(bed.surgery,
                            type, values, fname = NULL)

# Surgery beds ratio per 100,000 population by province
ytotal.bed.surgery = left_join(total.bed.surgery$byprovince, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`Surgical beds ratio per 100,000 population` = round((staff/Total)*100000 ,digits = 2)) %>% 
  select(1,`Surgical beds`= 3,8) %>%  
  pivot_longer(!c(1), names_to = "type", values_to = "staff")


per.surgerybed = round((sum(bed.surgery$values, na.rm = TRUE)/province$Total[nrow(province)]) * 100000, digits = 2)


################################################################################
# 191: Postoperative in-hospital mortality rate #####
postoperative = clean.soa %>% 
  select(3,9:11,191) %>% 
  select(1:4, postoperation = 5) %>% 
  separate(postoperation, into = c("scale", "pscale"), sep = " \\(") %>% 
  select(1:5)

tpostoperation = postoperative %>% 
  group_by(scale) %>% 
  summarise(number = n(), .groups = "drop_last") %>% 
  mutate(percent = round(number/sum(number)*100, digits = 2)) %>% 
  filter(scale != "NA")

# Postoperative in-hospital mortality rate by categorie/province/hospitals
ytpostoperative = hf_g1(postoperative,scale, fname = NULL)

write_xlsx(c(setNames(list(postoperative),"data"),
             setNames(list(tpostoperation),"percent"),
             ytpostoperative),
           "Excel datatable/post operative.xlsx")
################################################################################
# check other infrastructure here #####
# 207-208-209:: Bellwether per 100,000 people #####
bellwether.per <- clean.soa %>% 
  select(3,9:11,matches(c("otherinfo_t|sersurvol_"))) %>% 
  mutate(across(-c(1:4), ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>% 
  mutate(totalsurgery = rowSums(across(c(5,6,7)), na.rm = TRUE),
         totalbellwether = rowSums(across(c(11,12,13)), na.rm = TRUE)) %>%
  select(1:4,16) %>% 

#  filter(geninfo_hospitalfund == "Public") %>% 
  pivot_longer(!c(1:4), names_to = "type", values_to = "values")

# Total bellwether by hf level/type
ds.bellwether.per = onlysum(bellwether.per,
                            type, values, fname = NULL)

# Total surgery volume by province
ytbellwether.per = left_join(ds.bellwether.per$byprovince, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`Bellwether procedure per 100,000 population` = round((staff/Total)*100000 ,digits = 2)) %>% 
  select(1,`Total bellwether`= 3,8) %>%  
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

bellwether.percapita = round((sum(bellwether.per$values)/province$Total[nrow(province)]) * 100000, digits = 2)

################################################################################
# 24: Functional ORs per 100,000 people #####
fun.ors <- clean.soa %>% 
  select(3,9:11,24,matches(c("sersurvol_"))) %>% 
  mutate(across(-c(1:4), ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>% 
  mutate(totalsurgery = rowSums(across(c(6,7,8)), na.rm = TRUE)) %>%
  select(1:4,functioning.operatingrooms = 5,10)

ds.funors = surbed_tsur(fun.ors,functioning.operatingrooms,totalsurgery, fname = NULL)

# 203 & 204: operation rooms in general and OR for Obstetric in your hospital #####
OR = clean.soa %>% 
  select(3,9:11,matches(c("otherinfo_"))) %>% 
  select(1:4,5:7) %>% 
  mutate(across(-c(1:4), ~ as.numeric(str_extract(as.character(.x), "\\d+")))) %>% 
  pivot_longer(!c(1:4), names_to = "type", values_to = "values") %>% 
  mutate( type = str_to_sentence(str_replace_all(
    str_remove(type,"otherinfo_"), c("\\." = " ")))) %>% 
  filter(type == "Number or")

non.functioning.ORs = OR %>% 
  mutate(type = case_when(values > 0 ~ "Functioning ORs",
                          values == 0 ~ "No Functioning ORs"))

tnon.functioning.ORs = hf_g1(non.functioning.ORs,type)

# Total Operation Room and OR for Obstetric by hf level/type
tOR = onlysum(OR, type, values, fname = NULL)

# Total Operation Room and OR for Obstetric by province
ytOR = left_join(tOR$byprovince, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`Functioning Surgery Operationing Room per 100,000 population` = round((staff/Total)*100000 ,digits = 2)) %>% 
  select(1,3,8) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "staff")

ytOR.percapita = round((sum(tOR$byhfl$staff)/province$Total[nrow(province)]) * 100000, digits = 2)
gap.ytOR.percapita = round(6.2 - ytOR.percapita, digits = 2)

#################################################################
sallinformation = clean.soa %>% 
  select(2:3,9:11,12:15,25:29,31:85,87,143:144,163:173,187:202)

allinformation = clean.soa %>% 
  select(2:3,9:11,16:24,30,86,89:141,142,145:162,174:175,203:211) %>%
  mutate(across(.cols = -c(1:5), ~ as.numeric(str_extract(as.character(.x), "\\d+"))),
         `obstetrics gynaecology` = rowSums(across(c(17:39)), na.rm = TRUE),
         injury = rowSums(across(c(40:50)), na.rm = TRUE),
         `non trauma orthopedic` = rowSums(across(c(51:58)), na.rm = TRUE),
         specialty = rowSums(across(c(59:65)), na.rm = TRUE),
         `total surgery` = rowSums(across(c(66:68)), na.rm = TRUE),
         urgentcases = rowSums(across(c(69)), na.rm = TRUE),
         `total surgical operation` = rowSums(across(c(17:69)), na.rm = TRUE), 
         surgeon = rowSums(across(c(71:73)), na.rm = TRUE),
         `anesthesia professionals` = rowSums(across(c(74:75,77,79,81,88)), na.rm = TRUE),
         `allied health provider` = rowSums(across(c(82:87)), na.rm = TRUE),
         `SAO specialist` = rowSums(across(c(71:81,88)), na.rm = TRUE),
         `all workforce` = rowSums(across(c(71:88)), na.rm = TRUE),
         bellwether = rowSums(across(c(95:97)), na.rm = TRUE),
         `major surgery` = rowSums(across(c(66)), na.rm = TRUE),
         `minor surgery` = rowSums(across(c(67)), na.rm = TRUE),
         `paediatric surgery` = rowSums(across(c(68)), na.rm = TRUE),
         `caesarean delivery` = rowSums(across(c(95)), na.rm = TRUE),
         laparotomy = rowSums(across(c(96)), na.rm = TRUE),
         `treatment open fracture` = rowSums(across(c(97)), na.rm = TRUE))

write_xlsx(allinformation,
           "Excel datatable/allinformation.xlsx")

##############################################################
per100000 = allinformation %>% 
  select(1:5,`total beds` = 8,`surgical beds` = 9, `post operative recovery beds` = 10,`icu beds` = 11, `neonatal icu beds` = 12,
         `functioning ORs`= 14,`functional anesthesiainthe oRs` = 15,
         `perioperative death` = 94,104,106,107,108,109:112) %>% 
  pivot_longer(!c(1:5), names_to = "type", values_to = "values")
# WHO criteria #####
who.per100000 = per100000 %>% 
  group_by(type) %>% 
  summarise(`total values` = sum(values, na.rm = TRUE), .groups = "drop_last") %>% 
  mutate(`per 100000 population` = round((`total values`/province$Total[nrow(province)])*100000, digits = 2))

# WHO criteria by province #####
prov.per100000 = onlysum(per100000, type, values, fname = NULL)

allper100000 = prov.per100000$byprovince %>% select(1,2, value = 3)
yallper100000 = left_join(allper100000, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`per 100,000 population` = round((value/Total)*100000 ,digits = 2)) %>% 
  select(1,2,3,6:7)

write_xlsx(c(setNames(list(who.per100000),"national"),
             setNames(list(yallper100000),"provincial")),
           "Excel datatable/cases per 100000 population.xlsx")

######################################################################
# Create vectors for each column
factorname <- c("The percentage of the population that can access within 2 hours",
                "Infrastructure full availability","Workforce availability 24h/7days a week",
                "Specialist surgical workforce density (surgical, anaesthesia and obstetric specialist) per 100,000 population",
                "Surgical  volume-procedures performed in an operating theatre per 100,000 population per year",
                "Surgery Operation Room per 100000 population","Medical Electronic Records",
                "SOA committee (public only)","Health Facility using WHO safety checklist for surgery operation",
                "Public health facility capable of providing bellwether procedures (Caesarean delivery, laparotomy, and open fracture treatment)",
                "Surgery bed per 100000")
status = c(acc2hf2hour$percent[[6]], tinfra_readiness$infra.readiness[[2]],gt.wforce.avail.allday$percent[[1]],saopercapita,
           oper.theatre.percapita,ytOR.percapita,nmrk_recordkeeping$percent[[1]],
           hf.committee.soa$percent[[6]],checklist$percent[[2]],sur.bellwether$percent[[6]],
           per.surgerybed)
target = c(100, 100,100,20,5000,6.2,100,100,100,100,3000)
description <- c("100% allways", "100% allways", "100% allways","20 surgical, anesthetic and obstetric physicians per 100,000 population",
                 "5,000 procedures per 100,000 population", "6.2 operating theatres per 100 000","100% allways", "100% allways",
                 "100% allways", "Public Health facilities with Bellwelther Procedure",
                 "A surgical bed ratio of approximately 0.03:1000 population")
# Combine into a dataframe
who.ind <- data.frame(factorname, status, target, description) %>% 
  mutate(`unmet needs` = round(((target - status)/target)*100, digits = 2))

ggwho.ind = who.ind %>% 
  select(1,2,5) %>% 
  mutate(status = 100 - `unmet needs`) %>% 
  pivot_longer(!c(1), names_to = "type", values_to = "values")


writexl::write_xlsx(who.ind, "Excel datatable/who_unmet needs.xlsx")

############################################################
############################################################
allfac.readiness <- sallinformation %>% 
  select(-c(70:ncol(sallinformation))) %>% # 24hour.access.to.radiology/functioning.CT.scanner/MRI.scanner/patients.reach.within.2.hours
  #  select(-c(15,17,19,21,23,58,61,62,69)) %>% # Paediatric & Adult.Magill.forceps2/IV.sedation.anesthesia & regional.anesthesia
  mutate(across(.cols = -c(1:5), ~ case_when(
    . %in% c("Always (100%)","All (100%)") ~ 1,
    TRUE ~ 0))) %>% 
  mutate(number = rowSums(across(-c(1:5)), na.rm = TRUE),
         score = round(number/(length(names(.))-4)*100, digits = 2),
         readiness = case_when(score >=0 & score <= 25 ~ "Needs Improvement",
                               score > 25 & score <= 75 ~ "Partial completed readiness",
                               score > 75 & score < 100 ~ "Almost completed readiness",
                               score == 100 ~ "Completed readiness"),
         readiness =  factor( readiness, levels = factor_readiness)) %>% 
  select(1:5,71:72)

analysis = left_join(allfac.readiness, allinformation, by = "geninfo_hcfacility")
#ggplot(analysis, aes(x = `all workforce`, y = `total surgical operation`, color = geninfo_hcfacilitylevel.x, shape = geninfo_hcfacilitytype.x, size =score)) + 
#  geom_point(position = "identity" ) +
#  coord_flip(clip = "off")


