#################### Library ######################
library(tidyverse)
library(lubridate)
library(writexl)
library(stringdist)
library(summarytools)
library(purrr)
############# cleaning raw data ########################################
# Public Hospital data
public = readxl::read_excel("data/NSOAP-clean.xlsx", sheet = 1)
soa_public = public %>% 
  mutate(geninfo_hcfacility = str_to_lower(geninfo_hcfacility)) %>%
  filter(!is.na(geninfo_province),
         !is.na(geninfo_hcfacility)) %>% 
  arrange(desc(timestamp)) %>%
  distinct(geninfo_hcfacility, .keep_all = TRUE) %>% 
  mutate(geninfo_hcfacilitylevel = case_when( is.na(geninfo_hcfacilitylevel) ~ "Clinic",
                                              str_detect(geninfo_hcfacilitylevel, "^District") ~ "District Hospital",
                                              TRUE ~ geninfo_hcfacilitylevel),
         geninfo_hcfacilitytype = case_when( geninfo_hcfacilitylevel == "National Hospital" & is.na(geninfo_hcfacilitytype) ~ "CPA3",
                                             geninfo_hcfacilitylevel == "Clinic" & !is.na(geninfo_hcfacilitytype) ~ "Have operation theater",
                                             TRUE ~ geninfo_hcfacilitytype),
         geninfo_hospitalfund = case_when( geninfo_hospitalfund == "មន្ទីរពេទ្យបង្អែក" ~ "Public",
                                           geninfo_hospitalfund == "រដ្ឋាភិបាល និងហិរញ្ញប្បទានមន្ទីរពេទ្យ" ~ "Public",
                                           !is.na(geninfo_hcfacilitylevel) & is.na(geninfo_hospitalfund) ~ "Public", 
                                           TRUE ~ geninfo_hospitalfund))
# Private Hospital data
private = readxl::read_excel("data/NSOAP-clean.xlsx", sheet = 2)
soa_private = private %>% 
  mutate(geninfo_hcfacility = str_to_lower(geninfo_hcfacility)) %>%
  filter(!is.na(geninfo_province),
         !is.na(geninfo_hcfacility)) %>% 
  arrange(desc(timestamp)) %>%
  distinct(geninfo_hcfacility, .keep_all = TRUE) %>% 
  mutate(geninfo_hcfacilitylevel = case_when( is.na(geninfo_hcfacilitylevel) & !is.na(geninfo_hcfacilitytype) ~ "Clinic",
                                              str_detect(geninfo_hcfacilitylevel, "^Pol") ~ "Poly clinic",
                                              geninfo_hcfacilitylevel == "មន្ទីរពេទ្យខេត្ត" ~ "Clinic",
                                              geninfo_hcfacilitylevel == "Clinic មន្ទីរសម្រាកព្យាបាល" ~ "Clinic",
                                              geninfo_hcfacilitylevel == "Obstetric clinic មន្ទីរសម្ភព" ~ "Obstetric clinic",
                                              geninfo_hcfacilitylevel == "Pediatric clinic only មន្ទីរសម្រាកព្យាបាលកុមារ" ~ "Pediatric clinic",
                                              geninfo_hcfacilitylevel == "Private hospital មន្ទីរពេទ្យឯកជន" ~ "Private hospital",
                                              str_detect(geninfo_hcfacilitylevel, "^Non-profit") ~ "NGO",
                                              geninfo_hcfacilitylevel == "Public hospital" ~ "District Hospital",
                                              geninfo_hcfacilitylevel == "state hospital" ~ "Provincial Hospital",
                                              TRUE ~ geninfo_hcfacilitylevel),
         geninfo_hcfacilitytype = case_when( geninfo_hcfacilitylevel == "District Hospital" ~ "CPA2",
                                             geninfo_hcfacilitylevel == "Provincial Hospital" ~ "CPA3",
                                             !is.na(geninfo_hcfacilitylevel) & is.na(geninfo_hcfacilitytype) ~ "No operation theater",
                                             TRUE ~ geninfo_hcfacilitytype),
         geninfo_hospitalfund = case_when( !is.na(geninfo_hcfacilitylevel) & is.na(geninfo_hospitalfund) ~ "Private",
                                           geninfo_hospitalfund %in% c("Hospital","Governmental") ~ "Public",
                                           TRUE ~ geninfo_hospitalfund))
# Population 2024
province = readxl::read_excel("data/NSOAP-clean.xlsx", sheet = 3)
# Combine data into one dataset
soa_private = soa_private %>% select(-c(4))
soakh = rbind(soa_public,soa_private)

################### Define convert Khmer numerals to regular Arabic digits function #####
convert_khmer_to_arabic <- function(khmer_num) {
  khmer_digits <- unlist(strsplit(khmer_num, ""))
  arabic_digits <- c("០"=0, "១"=1, "២"=2, "៣"=3, "៤"=4, 
                     "៥"=5, "៦"=6, "៧"=7, "៨"=8, "៩"=9)
  paste0(sapply(khmer_digits, function(ch) {
    if (ch %in% names(arabic_digits)) arabic_digits[ch] else ch
  }), collapse = "")
}
# Apply conversion across entire dataframe
soa <- as.data.frame(lapply(soakh, function(col) {
  sapply(col, function(cell) {
    if (is.character(cell)) {
      convert_khmer_to_arabic(cell)
    } else {
      cell  # leave non-character values unchanged
    }  })
}))

##### Reference list of correct names #####
correct_names <- c("Banteay Meanchey","Battambang","Kampong Cham","Kampong Chhnang","Kampong Speu","Kampong Thom","Kampot","Kandal","Koh Kong","Kratie","Mondul Kiri","Phnom Penh","Preah Vihear","Prey Veng","Pursat","Ratanak Kiri","Siem Reap","Preah Sihanouk","Stung Treng","Svay Rieng","Takeo","Kep","Pailin","Oddar Meanchey","Tboung Khmum",
                   "បន្ទាយមានជ័យ","បាត់ដំបង","កំពង់ចាម","កំពង់ឆ្នាំង","កំពង់ស្ពឺ","កំពង់ធំ","កំពត","កណ្ដាល","កោះកុង","ក្រចេះ","មណ្ឌលគីរី","ភ្នំពេញ","ព្រះវិហារ","ព្រៃវែង","ពោធិ៍សាត់","រតនគីរី","សៀមរាប","ព្រះសីហនុ","ស្ទឹងត្រែង","ស្វាយរៀង","តាកែវ","កែប","ប៉ៃលិន","ឧត្ដរមានជ័យ","ត្បូងឃ្មុំ")
##### Function to find the closest correct name #####
match_name <- function(geninfo_province, reference_list) {
  distances <- stringdist(geninfo_province, reference_list, method = "lv")  # Levenshtein distance
  reference_list[which.min(distances)]
}
##### Cleaning data #####
clean.soa = soa %>% filter(!is.na(geninfo_hcfacility) & !is.na(geninfo_province) & !is.na(geninfo_staffname)) %>% 
  filter( geninfo_staffname != "removed") %>% 
  mutate(geninfo_province = str_replace(str_to_lower(geninfo_province),"province",""),
         geninfo_email = str_to_lower(geninfo_email),
         geninfo_phonenumber = str_remove(geninfo_phonenumber," "),
         cor_province = sapply(geninfo_province, match_name, reference_list = correct_names),
         geninfo_province = case_when(cor_province=="បន្ទាយមានជ័យ"~"Banteay Meanchey",cor_province=="បាត់ដំបង"~"Battambang",cor_province=="កំពង់ចាម"~"Kampong Cham",cor_province=="កំពង់ឆ្នាំង"~"Kampong Chhnang",cor_province=="កំពង់ស្ពឺ"~"Kampong Speu",
                                      cor_province=="កំពង់ធំ"~"Kampong Thom",cor_province=="កំពត"~"Kampot",cor_province=="កណ្ដាល"~"Kandal",cor_province=="កោះកុង"~"Koh Kong",cor_province=="ក្រចេះ"~"Kratie",
                                      cor_province=="មណ្ឌលគីរី"~"Mondul Kiri",cor_province=="ភ្នំពេញ"~"Phnom Penh",cor_province=="ព្រះវិហារ"~"Preah Vihear",cor_province=="ព្រៃវែង"~"Prey Veng",cor_province=="ពោធិ៍សាត់"~"Pursat",
                                      cor_province=="រតនគីរី"~"Ratanak Kiri",cor_province=="សៀមរាប"~"Siem Reap",cor_province=="ព្រះសីហនុ"~"Preah Sihanouk",cor_province=="ស្ទឹងត្រែង"~"Stung Treng",cor_province=="ស្វាយរៀង"~"Svay Rieng",
                                      cor_province=="តាកែវ"~"Takeo",cor_province=="កែប"~"Kep",cor_province=="ប៉ៃលិន"~"Pailin",cor_province=="ឧត្ដរមានជ័យ"~"Oddar Meanchey",cor_province=="ត្បូងឃ្មុំ"~"Tboung Khmum",
                                      .default = cor_province),
         geninfo_hcfacilitylevel = case_when( str_detect(geninfo_hcfacilitylevel, "^District") ~ "District Hospital",
                                              str_detect(geninfo_hcfacilitylevel, "^Private") ~ "Private Hospital",
                                              str_detect(geninfo_hcfacilitylevel,"^NGO") ~ "Private Hospital",
                                              str_detect(geninfo_hcfacilitylevel,"^Policy") ~ "Policlinic",
                                              str_detect(geninfo_hcfacilitylevel,"^Poly") ~ "Policlinic",
                                              str_detect(geninfo_hcfacilitylevel,"^Clinic") ~ "Clinic",
                                              str_detect(geninfo_hcfacilitylevel,"^Pediatric") ~ "Clinic",
                                              str_detect(geninfo_hcfacilitylevel,"^Obstetric") ~ "Clinic",
                                              .default = str_to_title(geninfo_hcfacilitylevel)
         )) %>% 
  mutate(geninfo_hcfacilitytype = case_when(geninfo_hcfacilitytype == "CPA3" ~ "CPA3",
                                            geninfo_hcfacilitytype == "CPA2" ~ "CPA2",
                                            TRUE ~ geninfo_hcfacilitylevel ),
         geninfo_hcfacilitylevel = case_when(geninfo_hcfacilitylevel %in% c("Policlinic","Private Hospital","Clinic") ~ "Private",
                                             TRUE ~ geninfo_hcfacilitylevel )) %>% 
  select(-c(cor_province))
##### Write data into folder #####
write_xlsx(clean.soa, "clean/cleaningdata.xlsx")
#############################################################################
clean.soa.alltype = soa %>% filter(!is.na(geninfo_hcfacility) & !is.na(geninfo_province) & !is.na(geninfo_staffname)) %>% 
  filter( geninfo_staffname != "removed") %>% 
  mutate(geninfo_province = str_replace(str_to_lower(geninfo_province),"province",""),
         geninfo_email = str_to_lower(geninfo_email),
         geninfo_phonenumber = str_remove(geninfo_phonenumber," "),
         cor_province = sapply(geninfo_province, match_name, reference_list = correct_names),
         geninfo_province = case_when(cor_province=="បន្ទាយមានជ័យ"~"Banteay Meanchey",cor_province=="បាត់ដំបង"~"Battambang",cor_province=="កំពង់ចាម"~"Kampong Cham",cor_province=="កំពង់ឆ្នាំង"~"Kampong Chhnang",cor_province=="កំពង់ស្ពឺ"~"Kampong Speu",
                                      cor_province=="កំពង់ធំ"~"Kampong Thom",cor_province=="កំពត"~"Kampot",cor_province=="កណ្ដាល"~"Kandal",cor_province=="កោះកុង"~"Koh Kong",cor_province=="ក្រចេះ"~"Kratie",
                                      cor_province=="មណ្ឌលគីរី"~"Mondul Kiri",cor_province=="ភ្នំពេញ"~"Phnom Penh",cor_province=="ព្រះវិហារ"~"Preah Vihear",cor_province=="ព្រៃវែង"~"Prey Veng",cor_province=="ពោធិ៍សាត់"~"Pursat",
                                      cor_province=="រតនគីរី"~"Ratanak Kiri",cor_province=="សៀមរាប"~"Siem Reap",cor_province=="ព្រះសីហនុ"~"Preah Sihanouk",cor_province=="ស្ទឹងត្រែង"~"Stung Treng",cor_province=="ស្វាយរៀង"~"Svay Rieng",
                                      cor_province=="តាកែវ"~"Takeo",cor_province=="កែប"~"Kep",cor_province=="ប៉ៃលិន"~"Pailin",cor_province=="ឧត្ដរមានជ័យ"~"Oddar Meanchey",cor_province=="ត្បូងឃ្មុំ"~"Tboung Khmum",
                                      .default = cor_province),
         geninfo_hcfacilitylevel = case_when( str_detect(geninfo_hcfacilitylevel, "^District") ~ "District Hospital",
                                              str_detect(geninfo_hcfacilitylevel, "^Private") ~ "Private Hospital",
                                              str_detect(geninfo_hcfacilitylevel,"^NGO") ~ "Private Hospital",
                                              str_detect(geninfo_hcfacilitylevel,"^Policy") ~ "Policlinic",
                                              str_detect(geninfo_hcfacilitylevel,"^Poly") ~ "Policlinic",
                                              str_detect(geninfo_hcfacilitylevel,"^Clinic") ~ "Clinic",
                                              str_detect(geninfo_hcfacilitylevel,"^Pediatric") ~ "Pediatric Clinic",
                                              str_detect(geninfo_hcfacilitylevel,"^Obstetric") ~ "Obstetric Clinic",
                                              .default = str_to_title(geninfo_hcfacilitylevel)
         )) %>% 
  mutate(geninfo_hcfacilitytype = case_when(geninfo_hcfacilitytype == "CPA3" ~ "CPA3",
                                            geninfo_hcfacilitytype == "CPA2" ~ "CPA2",
                                            TRUE ~ geninfo_hcfacilitylevel )) %>% 
  select(-c(cor_province))

write_xlsx(clean.soa.alltype, "clean/clean.soa.alltype.xlsx")
##############################################################################

# freq(clean.soa$geninfo_province, report.nas = FALSE, headings = FALSE)
#summary.pubsoa <- dfSummary(public)
#print(summary.pubsoa, method="browser", file = "data/summary.rawpubsoa.html")

#summary.prisoa <- dfSummary(private)
#print(summary.prisoa, method="browser", file = "data/summary.rawprisoa.html")

#private = private %>% select(-c(4))
#bothsoa = rbind(public,private)

#summary.bothsoa <- dfSummary(bothsoa)
#print(summary.bothsoa, method="browser", file = "data/summary.bothrawsoa.html")

###########
summary.cleanprisoa <- dfSummary(soa_private)
print(summary.cleanprisoa, method="browser", file = "clean/summary.cleanprisoa.html")

summary.cleanpubsoa <- dfSummary(soa_public)
print(summary.cleanpubsoa, method="browser", file = "clean/summary.cleanpubsoa.html")

summary.soakh <- dfSummary(soakh)
print(summary.soakh, method="browser", file = "clean/summary.cleanboth.html")


