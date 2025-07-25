# Referral Hospital Workforce #####
cpa2.wforce = wforce %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>% 
  group_by(workforces) %>%
  summarise(number = n(),
            tvalue = sum(values, na.rm = TRUE), .groups = 'drop_last') %>%
  mutate(percent = round(tvalue / sum(tvalue, na.rm = TRUE) * 100, digits = 2))
all.plot(cpa2.wforce, 
             fct_rev(workforces),tvalue,'',
             "Density of Workforce", 
             "in total",
             "")
# by province
cpa2.twforce.province = wforce %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>% 
  group_by(geninfo_province) %>% 
  summarise(value = sum(values, na.rm = TRUE), .groups = "drop_last")

group_ggplot(cpa2.twforce.province, 
             geninfo_province,value,'', 
             "Bed-related infrastructure", 
             "in number by province",
             "",
             'none')

# by province and type
cpa2.wforce.province = wforce %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>% 
  group_by(geninfo_province, workforces) %>%
  summarise(number = n(),
            tvalue = sum(values, na.rm = TRUE), .groups = 'drop_last') %>%
  mutate(percent = round(tvalue / sum(tvalue, na.rm = TRUE) * 100, digits = 2))

#### Specialist workforces
group_ggplot(cpa2.wforce.province %>% 
               filter(workforces %in% c("Anaesthesiologists","General surgeons","Obstetricians gynaecologists",
                                        "Paediatric surgeons")), 
             geninfo_province,tvalue,workforces, 
             "Surgeons/Obstetricians gynaecologists/Anaesthesiologists/Pediatric", 
             "in number by province",
             "",
             'none')
#### General doctors/Medical officers/Nurses
group_ggplot(cpa2.wforce.province %>% 
               filter(workforces %in% c("General doctors providing surgery","General doctors providing anesthesia",
                                        "Medical officers providing surgery","Medical officers providing anesthesia",
                                        "Nurses providing surgery","Nurses providing anesthesia")) %>% 
               mutate(workforces = str_wrap(workforces, width = 20)), 
             geninfo_province,tvalue,workforces, 
             "General doctors/Medical officers/Nurses", 
             "in number by province",
             "",
             'none')
#### Allied health providers
group_ggplot(cpa2.wforce.province %>% 
               filter(workforces %in% c("Midwives","Midwives on labour and delivery",
                                        "Radiologists","Pathologists","Pharmacists","Biomedical technicians",
                                        "Other related surgery obstetric and anesthesia")) %>% 
               mutate(workforces = str_wrap(workforces, width = 20)), 
             geninfo_province,tvalue,workforces, 
             "Allied health providers", 
             "in number by province",
             "",
             'none')

### Workforce ratio per 100,000 population
cpa2.wfperpop = left_join(cpa2.twforce.province, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`workforce per 100,000 population` = round((value/Total)*100000 ,digits = 0)) %>% 
  select(1,`Workforces` = 2,`Catchment population`= 5,6)

group_ggplot(cpa2.wfperpop %>% 
               pivot_longer(-c(1), names_to = "type", values_to = "values"),
             geninfo_province,values,type, 
             "Workforces and ratio per 100,000 population", 
             "distributed by province",
             "",
             'none')

cpa2.wf.capita = cpa2.wforce %>%
  mutate(specialist_flag = if_else(row_number() %in% c(1,5,12,15), tvalue, 0)) %>%
  summarise( specialist_wf = sum(specialist_flag, na.rm = TRUE),
             total_workforce = sum(tvalue, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "tvalue") %>% 
  mutate( sdcapita = round(tvalue / province$Total[nrow(province)] * 100000, digits = 0),
          score = (25 - sdcapita))

###################################################
# Referral Hospital Service Delivery #####
cpa2.service = allinformation %>% 
  select(1:5,100:106, 112:118) %>% 
  pivot_longer(cols = -c(1:5), names_to = "type", values_to = "values") %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>% 
  group_by(type) %>%
  summarise(tvalue = sum(values, na.rm = TRUE), .groups = 'drop_last')

all.plot(cpa2.service, 
         reorder(type,tvalue),tvalue,'',
         "Service delivery", 
         "in total",
         "")
# by province
cpa2.tservice.province = allinformation %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>% 
  group_by(geninfo_province) %>%
  summarise(tvalue = sum(`total surgical operation`, na.rm = TRUE), .groups = 'drop_last')

group_ggplot(cpa2.tservice.province, 
             geninfo_province,tvalue,'', 
             "Service delivery", 
             "in number by province",
             "",
             'none')

# by province and type
cpa2.sdperpop = left_join(cpa2.tservice.province, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`workforce per 100,000 population` = round((tvalue/Total)*100000 ,digits = 0)) %>% 
  select(1,`surgical volumes` = 2,`Catchment population`= 5, `surgical volume per 100,000 population` = 6)

group_ggplot(cpa2.sdperpop %>% 
               pivot_longer(-c(1), names_to = "type", values_to = "values"),
             geninfo_province,values,type, 
             "Surgical volume and ratio per 100,000 population", 
             "distributed by province",
             "",
             'none')
# Density service delivery per 100,000 population
cpa2.sd.capita = cpa2.service %>% 
  filter(type %in% c("total surgical operation","bellwether","total surgery","obstetrics gynaecology")) %>% 
  mutate(sdcapita = round(tvalue/province$Total[nrow(province)]*100000, digits = 0),
         score = (100-(round(5000-sdcapita, digits = 0)/5000 *100)))

#########
all.services = allinformation %>% 
  select(1:5,100:106,112:118) %>% 
  pivot_longer(cols = -c(1:5), names_to = "type", values_to = "values")

cpa2.service.province = all.services %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>% 
  group_by(geninfo_province,type) %>%
  summarise(tvalue = sum(values, na.rm = TRUE), .groups = 'drop_last')

cpa2.sdperpop = left_join(cpa2.service.province, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`procedures per 100,000 population` = round((tvalue/Total)*100000 ,digits = 0)) %>% 
  select(1,service = 2,`Total procedures`= 3,7)
############
# Bed
cpa2.tbeds = bed.infor %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>% 
  group_by(type) %>% 
  summarise(value = sum(values, na.rm = TRUE))

all.plot(cpa2.tbeds, 
         reorder(type,value),value,'', 
         "All bed-related infrastructure", 
         "Total number",
         "")

cpa2.tbeds.province = bed.infor %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>% 
  group_by(geninfo_province, type) %>% 
  summarise(value = sum(values, na.rm = TRUE), .groups = "drop_last")

group_ggplot(cpa2.tbeds.province, 
             geninfo_province,value,type, 
             "Bed-related infrastructure", 
             "in number by province",
             "",
             'none')
# CPA2 Infrastructure #####
cpa2.mainscore = data.infra %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>% 
  group_by(main.structure) %>% 
  summarise( number = n(),
             total_value = sum(nscale, na.rm = TRUE), .groups = 'drop' ) %>% 
  mutate(score = round((total_value/(number *5))*100, digits = 2),
         gaps = 100-score)%>% 
  select(-c(number, total_value))

plot_score(cpa2.mainscore %>% 
             pivot_longer(!c(main.structure), names_to = "type", values_to = "value") %>% 
             mutate(main.structure = factor(str_to_sentence(
               str_replace_all(main.structure,  c("\\." = " ", "scale" = ""))),
               levels = factor_main)),
           main.structure, value, type, 
           "Score of each main infrastructure",
           "% of its main structure")

### sub main score
cpa2.submainscore = data.infra %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>%
  group_by(main.structure, sub.structure) %>% 
  summarise( number = n(),
             total_value = sum(nscale, na.rm = TRUE), .groups = 'drop' ) %>% 
  mutate(score = round((total_value/(number *5))*100, digits = 2),
         gaps = 100-score,
         sub.structure =  str_to_sentence(str_replace_all( sub.structure, c("\\." = " "))))%>% 
  select(-c(number, total_value))

# OR Equipement and Supplies
pcpa2.submainscore = cpa2.submainscore %>%
  pivot_longer(-c(main.structure,sub.structure), names_to = "type", values_to = "value")

### Operating room equipment and supplies
plot_score(pcpa2.submainscore %>% 
             filter(main.structure %in% factor_main[6]),
           sub.structure, value, type, 
           "Score of operation room equipment and supplies", 
           "where are the problem occurred at sub-domain?")

### Laboratory
plot_score(pcpa2.submainscore %>% 
             filter(main.structure %in% factor_main[7]),
           sub.structure, value, type, 
           "Score of laboratory", 
           "where are the problem occurred at sub-domain?")
           
### Radiology
plot_score(pcpa2.submainscore %>% 
             filter(main.structure %in% factor_main[9]), 
           sub.structure, value, type, 
           "Score of radiology", 
           "where are the problem occurred at sub-domain?")

### Pharmacy
  plot_score(pcpa2.submainscore %>% 
               filter(main.structure %in% factor_main[10]),
             sub.structure, value, type, 
             "Score of pharmacy", 
             "where are the problem occurred at sub-domain?")

# infrastructure by scale #####
cpa2.mainscale = data.infra %>% 
    filter(geninfo_hcfacilitytype == "CPA2") %>%
    group_by(main.structure, scale) %>% 
    summarise(number = n(), .groups = 'drop_last') %>% 
    mutate(percent = round((number / sum(number, na.rm = TRUE) * 100), digits = 1),
           main.structure = factor(str_to_sentence(
             str_replace_all(main.structure,  c("\\." = " ", "scale" = ""))),
             levels = factor_main),
           scale = factor(scale, levels = factor_scale)) %>% 
    filter(scale != "NA", !is.na(main.structure))

group_ggplot(cpa2.mainscale %>% 
                 filter(main.structure != "Access and referral systems"),
               main.structure,percent,scale, 
               "Scale of each main infrastructure", 
               "by default, the scale is in rank.",
               "",
               'none')

cpa2.mainscale.province = data.infra %>% 
  filter(geninfo_hcfacilitytype == "CPA2") %>% 
  group_by(geninfo_province,main.structure,scale) %>% 
  summarise(number = n(), .groups = 'drop_last') %>% 
  mutate(score = round(number/sum(number, na.rm = TRUE)*100, digits = 2),
         gaps = 100-score) %>% 
  filter(scale == "Always") %>%
  select(-c(number,scale)) %>% 
  pivot_longer(-c(geninfo_province, main.structure), names_to = "type", values_to = "value")

#### Electricity, Water, Oxygen, Internet, blood
group_ggplot(cpa2.mainscale.province %>% 
               filter(type == "score",
                      main.structure %in% factor_main[c(11:14,8)],
                      !is.na(main.structure)), 
             geninfo_province,value,main.structure, 
             "Fully-functioning Electricity, Water, Oxygen, Internet", 
             "% of hospitals answering Always by province",
             "",
             'none')

#### Operation room equipment and supplies, Laboratory, Radiology and Pharmacy
group_ggplot(cpa2.mainscale.province %>% 
               filter(type == "score",
                      main.structure %in% factor_main[c(6,7,9,10)],
                      !is.na(main.structure)), 
             geninfo_province,value,main.structure, 
             "Fully-functioning OR equipment and supplies/Laboratory/Radiology/Pharmacy", 
             "% of hospitals answering Always by province",
             "",
             'none')

#### Other
group_ggplot(cpa2.mainscale.province %>% 
               filter(type == "score",
                      main.structure %in% factor_main[c(1:4)],
                      !is.na(main.structure)), 
             geninfo_province,value,main.structure, 
             "Fully-functioning general infrastructure", 
             "% of hospitals answering Always by province",
             "",
             'none')

# Infrastructure readiness #####
tcpa2.infra_readiness = infastruct.readiness %>%  
  filter(geninfo_hcfacilitytype == "CPA2") %>%
  group_by(readiness) %>% 
  summarise(number = n(), .groups = "drop_last") %>% 
  mutate(infra.readiness = round(number/sum(number)*100, digits = 2),
         readiness = factor(readiness, levels = rev(factor_readiness)))

all.plot(tcpa2.infra_readiness %>% 
           mutate(readiness = factor(readiness, levels = factor_readiness)),
         readiness,infra.readiness,readiness, 
         "Infrastructure readiness", 
         "",
         "")
### Infrastructure by its types
cpa2.infra_readiness = infastruct.readiness %>%  
  filter(geninfo_hcfacilitytype == "CPA2") %>%
  group_by(type,readiness) %>% 
  summarise(number = n(), .groups = "drop_last") %>% 
  mutate(infra.readiness = round(number/sum(number)*100, digits = 2),
         readiness = factor(readiness, levels = rev(factor_readiness)))

leg.all.plot(cpa2.infra_readiness %>% 
               mutate(readiness = factor(readiness, levels = factor_readiness)),
             type,infra.readiness,readiness,
             "Infrastructure full availability",
             "in percent","","top")

cpa2.infra_readiness.province = infastruct.readiness %>%  
  filter(geninfo_hcfacilitytype == "CPA2") %>%
  group_by(geninfo_province, readiness) %>% 
  summarise(number = n(), .groups = "drop_last") %>% 
  mutate(infra.readiness = round(number/sum(number)*100, digits = 2),
         readiness = factor(readiness, levels = rev(factor_readiness)))

group_ggplot(cpa2.infra_readiness.province %>% 
               mutate(readiness = factor(readiness, levels = rev(factor_readiness))), 
             geninfo_province,infra.readiness,readiness, 
             "Hospital facility answering (Always-100%)", 
             "% of hostipal facility readiness by province",
             "",
             'none')

write_xlsx(c(setNames(list(cpa2.wforce),"totalWF"),
             setNames(list(cpa2.wforce.province),"WFbyprovince"),
             setNames(list(cpa2.wfperpop),"WFper100000byprovince"),
             setNames(list(cpa2.wf.capita),"WFper100000"),
             setNames(list(cpa2.service),"totalSD"),
             setNames(list(cpa2.sdperpop),"SDbyprovince"),
             setNames(list(cpa2.sd.capita),"SDper100000"),
             setNames(list(cpa2.tbeds),"beds"),
             setNames(list(cpa2.tbeds.province),"bedbyprovince"),
             setNames(list(cpa2.mainscore),"mainscore"),
             setNames(list(cpa2.submainscore),"submainscore"),
             setNames(list(cpa2.mainscale),"mainscale"),
             setNames(list(cpa2.mainscale.province),"mainscalebyprovince"),
             setNames(list(tcpa2.infra_readiness),"infrareadiness"),
             setNames(list(cpa2.infra_readiness),"infrareadinesstype"),
             setNames(list(cpa2.infra_readiness.province),"infrareadinessbyprovince")),
           "Excel datatable/CPA2-data.xlsx")

