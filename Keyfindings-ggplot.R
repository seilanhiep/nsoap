#source("main-script.R")
#library(writexl)
################################################################################
my.plot = function(data,xaxis,yaxis,fillname){
  ggplot(data, aes(x = {{xaxis}}, y = {{yaxis}})) + 
    geom_col(aes(fill = {{fillname}}), position = 'stack') +
    geom_text(aes(label = {{yaxis}}, group = {{fillname}}),
              position = position_stack(vjust = 0),
              hjust = 0, size = 2, color = "black") +
    coord_flip(clip = "off") +  
    theme_classic() + # theme_classic()  theme_bw() + #  theme_light() +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 7, angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 7), 
          strip.text = element_text(size = 6),
          legend.position = 'none',
          legend.title = element_blank()
    )
}
my_ggplot = function(data,xaxis,yaxis,fillname,legends) {
  ggplot(data, aes(x = {{xaxis}}, y = {{yaxis}})) + 
    geom_col(aes(fill = {{fillname}}), position = 'stack') +
    geom_text(aes(label = {{yaxis}} , group = {{fillname}}),
              position = position_stack(vjust = 0),
              hjust = 0, size = 2, color = "black") +
#    facet_wrap2(vars({{fillname}}), nrow = 1, axes = "all", remove_labels = "all") +
    facet_wrap2(vars({{fillname}}), nrow = 1, axes = "all", remove_labels = "all") +
    coord_flip(clip = "off") +
    theme_classic() +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x = element_text(size = 7, angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 7), 
          strip.text = element_text(size = 6),
          legend.position = legends,
          legend.title = element_blank()
    )
}
utility.plot = function(data,xaxis,yaxis,fillname,subtitles){
  ggplot(data, aes(x = {{xaxis}}, y = {{yaxis}})) + 
    geom_col(aes(fill = {{fillname}}), position = 'stack') +
    geom_text(aes(label = {{yaxis}}, group = {{fillname}}),
              position = position_stack(vjust = 0),
              hjust = 0, size = 3, color = "black") +
    coord_flip(clip = "off") +  
    theme_classic() + #  theme_bw() + #  theme_light() +
    labs( subtitle = subtitles) +
    theme(plot.subtitle = element_text(size = 8, face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 7, angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 7), 
          strip.text = element_text(size = 6),
          legend.position = 'none',
          legend.title = element_blank()
    )
}
utility.plot1 = function(data,xaxis,yaxis,fillname,subtitles){
  ggplot(data, aes(x = {{xaxis}}, y = {{yaxis}})) + 
    geom_col(aes(fill = {{fillname}}), position = 'stack') +
    geom_text(aes(label = {{yaxis}}, group = {{fillname}}),
              position = position_stack(vjust = 0),
              hjust = 0, size = 3, color = "black") +
    coord_flip(clip = "off") +  
    scale_y_continuous(labels = scales::label_number(scale = 1/1000)) +
    theme_classic() + #  theme_bw() + #  theme_light() +
    labs( subtitle = subtitles) +
    theme(plot.subtitle = element_text(size = 8, face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 7, angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 7), 
          strip.text = element_text(size = 6),
          legend.position = 'none',
          legend.title = element_blank()
    )
}
utility_ggplot = function(data,xaxis,yaxis,fillname,subtitles,legends) {
  ggplot(data, aes(x = {{xaxis}}, y = {{yaxis}})) + 
    geom_col(aes(fill = {{fillname}}), position = 'stack') +
    geom_text(aes(label = {{yaxis}}, group = {{fillname}}),
              position = position_stack(vjust = 0),
              hjust = 0, size = 3, color = "black") +
#    facet_wrap2(vars({{fillname}}), nrow = 1, axes = "all", remove_labels = "all") +
    facet_wrap2(vars({{fillname}}), nrow = 1, axes = "all", remove_labels = "all") +
    coord_flip(clip = "off") +
    theme_classic() +
    labs(subtitle = subtitles) +
    theme(plot.subtitle = element_text(size = 8, face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x = element_text(size = 7, angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 7), 
          strip.text = element_text(size = 6),
          legend.position = legends,
          legend.title = element_blank()
    )
}
utility_ggplot1 = function(data,xaxis,yaxis,fillname,subtitles,legends) {
  ggplot(data, aes(x = {{xaxis}}, y = {{yaxis}})) + 
    geom_col(aes(fill = {{fillname}}), position = 'stack') +
    geom_text(aes(label = {{yaxis}}, group = {{fillname}}),
              position = position_stack(vjust = 0),
              hjust = 0, size = 3, color = "black") +
    facet_wrap2(vars({{fillname}}), nrow = 1, axes = "all", remove_labels = "all") +
    coord_flip(clip = "off") +
    #scale_y_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::label_number(scale = 1/1000)) +
    theme_classic() +
    labs(subtitle = subtitles) +
    theme(plot.subtitle = element_text(size = 8, face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x = element_text(size = 7, angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 7), 
          strip.text = element_text(size = 6),
          legend.position = legends,
          legend.title = element_blank()
    )
}
################################################################################
# Access to HF with 2 hour #####
p1 = utility.plot(acc2hf2hour,scale,percent,scale,"Total percentage")
p2 = utility_ggplot(houraccess$byhfl,
                 scale,percent,geninfo_hcfacilitylevel,"by health facility level",'none')
p3 = utility_ggplot(houraccess$byhft,
                 scale,percent,geninfo_hcfacilitytype,"by health facility type",'none')
p4 = utility_ggplot(houraccess$byprovince %>% 
                    mutate(scale = factor(scale, levels = factor_safety)),
                  geninfo_province,percent,scale,"by province",'none')

p123 = plot_grid(p1, p2, p3,
          nrow = 3,
          labels = "",
          label_fontfamily = "serif",
          label_fontface = "plain",
          label_colour = "blue",
          label_size = 8,
          align = "v",
          axis = "lr")

# Your plot_grid code
combined_plot <- plot_grid(
  p123, p4,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(4, 6),
  nrow = 1
)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "Population with 2 hours access to health facility (%)",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/infrastructure-access2hour.JPEG", final_plot, base_height = 6, base_width = 10)


all.acc2hf2hour = rbind(acc2hf2hour %>% mutate(healthfacility = "All") %>% select(4,1,2,3),
                        houraccess$byhfl %>% select(healthfacility = 1,2,3,4),
                        houraccess$byhft %>% select(healthfacility = 1,2,3,4))

p1 = utility_ggplot1(all.acc2hf2hour %>% 
                       mutate(healthfacility = factor(healthfacility, levels = c("All", "National Hospital","Provincial Hospital","District Hospital","CPA3","CPA2","Private","Private Hospital","Policlinic","Clinic")
                       )),
                     scale,percent,healthfacility,"Population with 2 hours access to health facility (%)",'none')
save_plot("ggplot/access to HF within 2 hours.JPEG", p1, base_height = 2.5, base_width = 7)

################################################################################
# Surgical Facilities #####
bothhf = utility_ggplot(overview$bypubpri %>% 
                          mutate(geninfo_hospitalfund = fct_rev(geninfo_hospitalfund)),
                        geninfo_hospitalfund,number,'',"by health facility",'none')

hfl = utility_ggplot(overview$hflevel %>% 
                       mutate(geninfo_hcfacilitylevel = factor(geninfo_hcfacilitylevel, levels = rev(factor_hflevel))),
                     geninfo_hcfacilitylevel,number,'',"by health facility leve",'none')

hft = utility_ggplot(overview$hftype  %>% 
                       mutate(geninfo_hcfacilitytype = factor(geninfo_hcfacilitytype, levels = rev(factor_hftype))),
                     geninfo_hcfacilitytype,number,'',"by health facility type",'none')

ggplotprovince = my_ggplot(pro.hf.overview %>% 
                          mutate(geninfo_hospitalfund = fct_rev(geninfo_hospitalfund)),
                        geninfo_province,number, geninfo_hospitalfund, 'none')

leftplot = plot_grid(bothhf, hfl, hft,
                 nrow = 3,
                 rel_heights = c(2.5,3.5,4),
                 labels = "",
                 label_fontfamily = "serif",
                 label_fontface = "plain",
                 label_colour = "blue",
                 label_size = 12,
                 align = "v")

# Your plot_grid code
combined_plot <- plot_grid(
  leftplot, ggplotprovince,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(6,4),
  nrow = 1
)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "Total number of surgical facilities",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/infrastructure-Health facilities.JPEG", final_plot, base_height = 6, base_width = 10)

################################################################################
# Beds and Rooms #####
allbeds = allinformation %>% 
  select(1:5,`Admissions per year`=6,`Functioning ORs` =14, ORs =91,
         `ICU beds` = 11, `Neonatal ICU beds` = 12, `post-op recovery beds`= 10, `Surgical beds` = 9, `Total beds` = 8, `Anesthetic machines` = 15)

hfl.beds <- allbeds %>%
  select(-c(1:2,4:5)) %>%
  group_by(geninfo_hcfacilitylevel) %>%
  summarise(`number of health facility` = n(),
            across(everything(), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>% 
  mutate(geninfo_hcfacilitylevel = factor( geninfo_hcfacilitylevel,
                                           levels = c("National Hospital", "Provincial Hospital", "District Hospital", "Private", "Total")))%>% 
  arrange(geninfo_hcfacilitylevel)

flextable(hfl.beds)

################################################################################
# Utility, Diagnostic, Medication, Blood and ORs equipement and supply #####
p1 = utility.plot(main.infra %>% 
               filter(type == "score" & !is.na(main.structure)),
             reorder(main.structure, value),value,'',"Main structure (%)")
p21 = utility.plot(score.sub.infra %>%
                filter(main.structure %in% factor_main[7] &  type == "score"),
              reorder(sub.structure, value),value,'',"Laboratory (%)")
p22 = utility.plot(score.sub.infra %>%
                filter(main.structure %in% factor_main[9] &  type == "score"),
              reorder(sub.structure, value),value,'',"Radiology (%)")
p23 = utility.plot(score.sub.infra %>%
                filter(main.structure %in% factor_main[10] &  type == "score"),
              reorder(sub.structure, value),value,'',"Pharmacy (%)")

p3 = utility.plot(score.sub.infra %>%
               filter(main.structure %in% factor_main[6] &  type == "score"),
             reorder(sub.structure, value),value,'',"Operating room equipment and supplies (%)")

p2123 = plot_grid(p21,p23, p22, 
                  nrow = 3,
                  labels = "",
                  label_fontfamily = "serif",
                  label_fontface = "plain",
                  label_colour = "blue",
                  label_size = 8,
                  align = "v")

# Your plot_grid code
combined_plot <- plot_grid(
  p1, p2123, p3,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(2.5,3,4.5),
  nrow = 1)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "Utility, Diagnostic, Medication, Blood and ORs equipement and supplies by score",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/Infrastructure-scoreutility.JPEG", final_plot, base_height = 6, base_width = 10)


################################################################################
# Utility, Diagnostic, Medication, Blood and ORs equipement and supplies-Always (100%) #####
p1 = utility.plot(scale.main.infra %>% 
                    filter(main.structure != "Access and referral systems" & scale == "Always"),
                  reorder(main.structure, percent),percent,'',"Main structure (%)")
p21 = utility.plot(scale.sub.infra %>%
                     filter(main.structure %in% factor_main[7]  & scale == "Always"),
                   reorder(sub.structure, percent),percent,'',"Laboratory (%)")
p22 = utility.plot(scale.sub.infra %>%
                     filter(main.structure %in% factor_main[9]  & scale == "Always"),
                   reorder(sub.structure, percent),percent,'',"Radiology (%)")
p23 = utility.plot(scale.sub.infra %>%
                     filter(main.structure %in% factor_main[10]  & scale == "Always"),
                   reorder(sub.structure, percent),percent,'',"Pharmacy (%)")

p3 = utility.plot(scale.sub.infra %>%
                    filter(main.structure %in% factor_main[6]  & scale == "Always"),
                  reorder(sub.structure, percent),percent,'',"Operating room equipment and supplies (%)")

p2123 = plot_grid(p21,p23,p22,
                  nrow = 3,
                  labels = "",
                  label_fontfamily = "serif",
                  label_fontface = "plain",
                  label_colour = "blue",
                  label_size = 8,
                  align = "v")

# Your plot_grid code
combined_plot <- plot_grid(
  p3, p1, p2123,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(3.33,3.33,3.33),
  nrow = 1)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "Utility, Diagnostic, Medication, Blood and ORs equipement and supplies-Always (100%)",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/Infrastructure sub scale utility.JPEG", final_plot, base_height = 6, base_width = 10)

all.utility = rbind(scale.main.infra %>% 
                      filter(main.structure != "Access and referral systems" & scale == "Always") %>%
                      mutate(healthfacility = "All") %>% select(5,1,3,4),
                    fullmainfunction$byhfl %>% 
                      filter(scale == "Always",
                             main.structure  != "Access and referral systems" & 
                               main.structure != "NA") %>% 
                      mutate(main.structure = factor(main.structure,levels = factor_main)) %>% 
                      select(healthfacility = 1,2,number = 4,5),
                    fullmainfunction$byhft %>% 
                      filter(scale == "Always",
                             main.structure  != "Access and referral systems" & 
                               main.structure != "NA") %>% 
                      mutate(main.structure = factor(main.structure,levels = factor_main)) %>% 
                      select(healthfacility = 1,2,number = 4,5))

p1 = utility_ggplot(all.utility %>% 
                      mutate(healthfacility = factor(healthfacility, levels = c("All", "National Hospital","Provincial Hospital","District Hospital","CPA3","CPA2","Private","Private Hospital","Policlinic","Clinic")
                      )),
                    main.structure,percent,healthfacility,"Utility, diagnostics, medicination, blood and ORs (Always-100%)",'none')
save_plot("ggplot/Infrastructure scale utility.JPEG", p1, base_height = 2.5, base_width = 7)

################################################################################
# Facility readiness by its level #####
p1 = utility_ggplot(fullmainfunction$byhfl %>% 
                      filter(scale == "Always",
                             main.structure  != "Access and referral systems" & 
                               main.structure != "NA") %>% 
                      mutate(main.structure = factor(main.structure,levels = factor_main)),
                    main.structure,percent,geninfo_hcfacilitylevel,"HF with Always(100%) answer",'none')

p2 = utility_ggplot(fullmainfunction$byhft %>% 
                      filter(scale == "Always",
                             main.structure  != "Access and referral systems" & 
                               main.structure != "NA") %>% 
                      mutate(main.structure = factor(main.structure,levels = factor_main)),
                    main.structure, percent,geninfo_hcfacilitytype,"HF with Always(100%) answer",'none')

p12 = plot_grid(p1, p2,
                ncol = 2,
                labels = "",
                label_fontfamily = "serif",
                label_fontface = "plain",
                label_colour = "blue",
                label_size = 8,
                align = "h")

p3 = utility_ggplot(fullmainfunction$byprovince %>% 
                      filter(scale == "Always",
                             !is.na(main.structure)),
                    geninfo_province, percent,main.structure,"HF with Always(100%) answer",'none')

combined_plot <- plot_grid(
  p12, p3,
  align = "v", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_heights = c(3.5,6.5),
  nrow = 2)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "Where do utility, Diagnostic, Medication, Blood and ORs equipement and supplies get Always(100%) answer",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/Infrastructure-utilitybyHF.JPEG", final_plot, base_height = 7, base_width = 10)

################################################################################
# Infrastructure readiness #####
p1 = utility.plot(tinfra_readiness %>% 
                    mutate(readiness = factor(readiness, levels = factor_readiness)),
                  readiness,infra.readiness,readiness,"Infrastructure Availability")

p2 = utility_ggplot(infra.hfreadiness$byhfl %>% 
                      mutate(readiness = factor(readiness, levels = factor_readiness)),
                    readiness,percent,geninfo_hcfacilitylevel,"Infrastructure Availability by HF level",'none')

p3 = utility_ggplot(infra.hfreadiness$byhft %>% 
                      mutate(readiness = factor(readiness, levels = factor_readiness)),
                    readiness,percent,geninfo_hcfacilitytype,"Infrastructure Availability by HF type",'none')

p4 = utility_ggplot(infra.hfreadiness$byprovince %>% 
                      mutate(readiness = factor(readiness, levels = rev(factor_readiness))),
                    geninfo_province,percent,readiness,"Infrastructure Availability by province",'none')

p123 = plot_grid(p1, p2, p3,
                 nrow = 3,
                 labels = "",
                 label_fontfamily = "serif",
                 label_fontface = "plain",
                 label_colour = "blue",
                 label_size = 8,
                 align = "v",
                 axis = "lr")

# Your plot_grid code
combined_plot <- plot_grid(
  p123, p4,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(5, 6),
  nrow = 1
)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "All factors-based infrastructure availability (%)",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/infrastructure-availability.JPEG", final_plot, base_height = 6, base_width = 10)

################################################################################
# Workforces by its type #####
typeworkforces = allworkforce %>% 
  group_by(type) %>% 
  summarise(number = n(),
            tvalues = sum(values, na.rm = TRUE), .groups = "drop_last")

p1 = utility.plot(typeworkforces %>% 
                    mutate(type = factor(type, levels = rev(factor_allwf))),
                  type,tvalues,type,"Total number of workforces")
p2 = utility_ggplot1(tallworkforce$byhfl %>% 
                      mutate(type = factor(type, levels = rev(factor_allwf))),
                    type,staff,geninfo_hcfacilitylevel,"by health facility level in thousand",'none')
p3 = utility_ggplot1(tallworkforce$byhft %>% 
                      mutate(type = factor(type, levels = rev(factor_allwf))),
                    type,staff,geninfo_hcfacilitytype,"by health facility type in thousand",'none')
p4 = utility_ggplot1(tallworkforce$byprovince %>% 
                      mutate(type = factor(type, levels = factor_allwf)),
                    geninfo_province,staff,type,"by province in thousand",'none')

p123 = plot_grid(p1, p2, p3,
                 nrow = 3,
                 labels = "",
                 label_fontfamily = "serif",
                 label_fontface = "plain",
                 label_colour = "blue",
                 label_size = 8,
                 align = "v",
                 axis = "lr")

# Your plot_grid code
combined_plot <- plot_grid(
  p123, p4,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(4, 6),
  nrow = 1
)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "All workforces by its type",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/workforces by type.JPEG", final_plot, base_height = 6, base_width = 10)

################################################################################
# All workforces #####
all.workforce = rbind( totalden.wforce %>% mutate(type = "All workforce") %>% select(5,1, staff = 3),
                       den.wforce$byhfl %>% select(type = 1,2,3),
                       den.wforce$byhft %>% select(type = 1,2,3))
all.workforce$workforces = str_wrap(all.workforce$workforces, width = 25)
p1 = utility_ggplot(all.workforce %>% 
                      mutate(type = factor(type, levels = factor_wf)),
                    workforces,staff,type,"All workforce by health facilities",'none')
save_plot("ggplot/workforces by HF.JPEG", p1, base_height = 6, base_width = 10)

den.wforce$byprovince$workforces <- str_wrap(den.wforce$byprovince$workforces, width = 10)
p2 = utility_ggplot(den.wforce$byprovince,
                    geninfo_province,staff,workforces,"All workforce by province",'none')
save_plot("ggplot/workforces by province.JPEG", p2, base_height = 6, base_width = 10)

################################################################################
# service delivery #####
services = allinformation %>% 
  select(1:5,106,113:115,117,116,118) %>% 
  pivot_longer(cols = -c(1:5), names_to = "type", values_to = "values")

allservices = services %>% 
  group_by(type) %>% 
  summarise(number = n(),
            tvalue = sum(values, na.rm = TRUE), .groups = "drop_last")

hf.services = onlysum(services,type,values, fname = '')

p1 = utility.plot1(allservices %>% 
                     mutate(type = factor(type, levels = rev(factor_sdtype1))),
                    type,tvalue,type,"Total Service volume in thousand")

p12 = utility_ggplot1(hf.services$bypubpri %>% 
                        mutate(type = factor(type, levels = rev(factor_sdtype1))),
                      type,staff,geninfo_hospitalfund,"by health facility level in thousand",'none')
  
p2 = utility_ggplot1(hf.services$byhfl %>% 
                       mutate(type = factor(type, levels = rev(factor_sdtype1))),
                     type,staff,geninfo_hcfacilitylevel,"by health facility level in thousand",'none')
p3 = utility_ggplot1(hf.services$byhft %>% 
                       mutate(type = factor(type, levels = rev(factor_sdtype1))),
                     type,staff,geninfo_hcfacilitytype,"by health facility type  in thousand",'none')
#hf.services$byprovince$type <- str_wrap(hf.services$byprovince$type, width = 10)
p4 = utility_ggplot1(hf.services$byprovince %>% 
                       mutate(type = factor(type, levels = factor_sdtype1)),
                    geninfo_province,staff,type,"by province  in thousand",'none')

p123 = plot_grid(p1, p2, p3,
                 nrow = 3,
                 labels = "",
                 label_fontfamily = "serif",
                 label_fontface = "plain",
                 label_colour = "blue",
                 label_size = 8,
                 align = "v",
                 axis = "lr")

# Your plot_grid code
combined_plot <- plot_grid(
  p123, p4,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(4, 6),
  nrow = 1
)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "Service Volume",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/service delivery.JPEG", final_plot, base_height = 6, base_width = 10)

################################################################################
# All service delivery #####
all.services = allinformation %>% 
  select(1:5,100:106,112:118) %>% 
  pivot_longer(cols = -c(1:5), names_to = "type", values_to = "values")

all_services = all.services %>% 
  group_by(type) %>% 
  summarise(number = n(),
            tvalue = sum(values, na.rm = TRUE), .groups = "drop_last")

hf.services = onlysum(all.services,type,values, fname = '')

yhf.services = left_join(hf.services$byprovince, province, by = c("geninfo_province" = "Provinces")) %>% 
  mutate(`procedures per 100,000 population` = round((staff/Total)*100000 ,digits = 0)) %>% 
  select(1,service = 2,`Total procedures`= 3,8) 
#  pivot_longer(-c(1,2), names_to = "type", values_to = "staff") 

write_xlsx(c(setNames(list(all_services), "service delivery"),
             hf.services,
             setNames(list(yhf.services), "service per 100000")), 
           "Excel datatable/all services.xlsx")

all.service.delivery = rbind( all_services %>% mutate(healthfacility = "All service delivery") %>% select(4,1, staff = 3),
                              hf.services$byhfl %>% select(healthfacility = 1,2,3),
                              hf.services$byhft %>% select(healthfacility = 1,2,3))

p1 = utility_ggplot1(all.service.delivery %>% 
                      mutate(healthfacility = factor(healthfacility, levels = factor_hf),
                             type = factor(type, levels = rev(factor_sdtype))),
                    type,staff,healthfacility,"All service delivery by health facilities in thousand",'none')
save_plot("ggplot/service delivery by HF.JPEG", p1, base_height = 6, base_width = 10)

p4 = utility_ggplot1(hf.services$byprovince %>% 
                      mutate(type = factor(type, levels = factor_sdtype)) ,
                    geninfo_province,staff,type,"by province in thousand",'none')
save_plot("ggplot/service delivery by all type.JPEG", p4, base_height = 6, base_width = 10)
################################################################################
################################################################################
################################################################################
# presence of data system #####
dspresence = trans.ds %>% 
  group_by(ds.scale) %>% 
  summarise(number = n(), .groups = "drop_last") %>% 
  mutate(percent = round(number/sum(number)*100,digits = 2),
         ds.scale = factor(ds.scale, levels = factor_readiness))

p1 = utility.plot(dspresence %>% 
                    mutate(ds.scale = factor(ds.scale, levels = factor_readiness)),
                  ds.scale,percent,ds.scale,"Percent of presence of data system")
p2 = utility_ggplot(presence.ds$byhfl %>% 
                      mutate(ds.scale = factor(ds.scale, levels = factor_readiness)),
                    ds.scale,percent,geninfo_hcfacilitylevel,"by health facility level",'none')
p3 = utility_ggplot(presence.ds$byhft,
                    ds.scale,percent,geninfo_hcfacilitytype,"by health facility type",'none')
p4 = utility_ggplot(presence.ds$byprovince %>% 
                      mutate(ds.scale = factor(ds.scale, levels = rev(factor_readiness))),
                    geninfo_province,percent,ds.scale,"by province",'none')

p123 = plot_grid(p1, p2, p3,
                 nrow = 3,
                 labels = "",
                 label_fontfamily = "serif",
                 label_fontface = "plain",
                 label_colour = "blue",
                 label_size = 8,
                 align = "v",
                 axis = "lr")

# Your plot_grid code
combined_plot <- plot_grid(
  p123, p4,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(4, 6),
  nrow = 1
)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "Presence of data system (%)",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/presence of data system.JPEG", final_plot, base_height = 6, base_width = 10)
################################################################################
# Percent of Telemedicine used #####
p1 = utility.plot(telemedicince, 
                  scales,percent,scales,"Percent of Telemedicine used")
p2 = utility_ggplot(ttelemedicince$byhfl,
                    scales,percent,geninfo_hcfacilitylevel,"by health facility level",'none')
p3 = utility_ggplot(ttelemedicince$byhft,
                    scales,percent,geninfo_hcfacilitytype,"by health facility type",'none')
p4 = utility_ggplot(ttelemedicince$byprovince %>% 
                      mutate(scales = factor(scales, levels = factor_yesno)),
                    geninfo_province,percent,scales,"by province",'none')

p123 = plot_grid(p1, p2, p3,
                 nrow = 3,
                 labels = "",
                 label_fontfamily = "serif",
                 label_fontface = "plain",
                 label_colour = "blue",
                 label_size = 8,
                 align = "v",
                 axis = "lr")

# Your plot_grid code
combined_plot <- plot_grid(
  p123, p4,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(4, 6),
  nrow = 1
)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "Percent of Telemedicine used (%)",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/Telemedicine.JPEG", final_plot, base_height = 6, base_width = 10)
################################################################################
# WHO Quality and safety checklist #####
p1 = utility.plot(checklist %>% filter(scale != "NA") %>% 
                    mutate(scale = factor(scale, levels = rev(factor_scale))), 
                  scale,percent,scale,"Percent of WHO Quality and safety checklist used")
p2 = utility_ggplot(safetychecklist$byhfl %>%
                      filter(scale != "NA") %>% 
                      mutate(scale = factor(scale, levels = rev(factor_scale))),
                    scale,percent,geninfo_hcfacilitylevel,"by health facility level",'none')
p3 = utility_ggplot(safetychecklist$byhft %>%
                      filter(scale != "NA") %>% 
                      mutate(scale = factor(scale, levels = rev(factor_scale))),
                    scale,percent,geninfo_hcfacilitytype,"by health facility type",'none')
p4 = utility_ggplot(safetychecklist$byprovince %>%
                      filter(scale != "NA") %>% 
                      mutate(scale = factor(scale, levels = factor_scale)),
                    geninfo_province,percent,scale,"by province",'none')

p123 = plot_grid(p1, p2, p3,
                 nrow = 3,
                 labels = "",
                 label_fontfamily = "serif",
                 label_fontface = "plain",
                 label_colour = "blue",
                 label_size = 8,
                 align = "v",
                 axis = "lr")

# Your plot_grid code
combined_plot <- plot_grid(
  p123, p4,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(4, 6),
  nrow = 1
)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "WHO Quality and safety checklist used (%)",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/whosaftychecklist.JPEG", final_plot, base_height = 6, base_width = 10)
################################################################################
# Record Keeping used #####
p1 = utility.plot(nmrk_recordkeeping %>% filter(scales != "NA") %>% 
                    mutate(scales = factor(scales, levels = rev(factor_recordkeeping))), 
                  scales,percent,scales,"Percent of Record Keeping used")
p2 = utility_ggplot(recordkeeping$byhfl %>% filter(scales != "NA") %>% 
                      mutate(scales = factor(scales, levels = rev(factor_recordkeeping))),
                    scales,percent,geninfo_hcfacilitylevel,"by health facility level",'none')
p3 = utility_ggplot(recordkeeping$byhft  %>% filter(scales != "NA") %>% 
                      mutate(scales = factor(scales, levels = rev(factor_recordkeeping))),
                    scales,percent,geninfo_hcfacilitytype,"by health facility type",'none')
p4 = utility_ggplot(recordkeeping$byprovince  %>% filter(scales != "NA") %>% 
                      mutate(scales = factor(scales, levels = factor_recordkeeping)),
                    geninfo_province,percent,scales,"by province",'none')

p123 = plot_grid(p1, p2, p3,
                 nrow = 3,
                 labels = "",
                 label_fontfamily = "serif",
                 label_fontface = "plain",
                 label_colour = "blue",
                 label_size = 8,
                 align = "v",
                 axis = "lr")

# Your plot_grid code
combined_plot <- plot_grid(
  p123, p4,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(4, 6),
  nrow = 1
)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "Record Keeping used (%)",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/recordkeeping.JPEG", final_plot, base_height = 6, base_width = 10)

################################################################################
# SOA committee #####
p1 = utility.plot(committee.soa %>% 
                    mutate(scale = factor(scale, levels = rev(factor_yesno))), 
                  scale,percent,scale,"Percent of SOA committee")
p2 = utility_ggplot(tcommittee$byhfl %>%  
                      filter(scale != "NA"),
                    scale,percent,geninfo_hcfacilitylevel,"by health facility level",'none')
p3 = utility_ggplot(tcommittee$byhft %>%
                      filter(scale != "NA"),
                    scale,percent,geninfo_hcfacilitytype,"by health facility type",'none')
p4 = utility_ggplot(tcommittee$byprovince %>%
                      filter(scale != "NA") %>% 
                      mutate(scale = factor(scale, levels = factor_yesno)),
                    geninfo_province,percent,scale,"by province",'none')

p123 = plot_grid(p1, p2, p3,
                 nrow = 3,
                 labels = "",
                 label_fontfamily = "serif",
                 label_fontface = "plain",
                 label_colour = "blue",
                 label_size = 8,
                 align = "v",
                 axis = "lr")

# Your plot_grid code
combined_plot <- plot_grid(
  p123, p4,
  align = "h", 
  axis = "b",
  labels = "", 
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue",
  label_size = 12,
  rel_widths = c(4, 6),
  nrow = 1
)

# Add title across the graphic
final_plot <- ggdraw() +
  draw_label(
    "SOA committee (%)",
    fontface = 'bold',
    x = 0.5, y = 0.99,   # center top
    hjust = 0.5, vjust = 1,
    size = 12
  ) +
  draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.95) + 
  theme_half_open() #  theme_map()

save_plot("ggplot/SOA committee.JPEG", final_plot, base_height = 6, base_width = 10)

# Unmet need plot #####
ggwho.ind$factorname = str_wrap(ggwho.ind$factorname,width = 50)
final_plot = plot_unmetneed(ggwho.ind %>% 
                 filter(values > 0),
               factorname, values, type, 
               "WHO gaps analysis", 
               "Remaining gaps in percent compared with WHO standard")

save_plot("ggplot/Remaining WHO gaps.JPEG", final_plot, base_height = 6, base_width = 10)
# Infrastructure with Always (100%) available for surgical procedure #####
infra.plot_factor = c("Oxygen","Runningwater","Or equipment and supplies","Laboratory","Electricity","Internet","Pharmacy","Spinal anesthesia","Radiology","Iv sedation anesthesia","Regional anesthesia","Blood supply")

infra.gaps = scale.main.infra %>% 
  filter(main.structure != "Access and referral systems",
         scale == "Always") %>% 
  select(1,score=4) %>% 
  mutate(gaps = 100 - score) %>% 
  pivot_longer(cols = -c(1), names_to = "type", values_to = "value") %>% 
  mutate(main.structure = factor(main.structure, 
                                 levels = rev(infra.plot_factor)))

final_plot = plot_score(infra.gaps,
           main.structure, value, type, 
           "Scale of each main infrastructure", 
           "% of its main structure")
save_plot("ggplot/Infrastructure scales gaps.JPEG", final_plot, base_height = 6, base_width = 10)

#############################################
a = scale.main.infra %>% 
  filter(main.structure != "Access and referral systems" & scale == "Always") %>% 
  mutate(type = "Fully functioning infrastructure") %>% 
  select(5,1,staff = 3,4)

b = fullmainfunction$bypubpri %>% 
  filter(scale == "Always",
         main.structure  != "Access and referral systems" & 
           main.structure != "NA") %>% 
  mutate(main.structure = factor(main.structure,levels = factor_main)) %>% 
  select(type = 1,2,4,5) %>% 
  filter(type == "Public")

c = fullmainfunction$byhfl %>% 
  filter(scale == "Always",
         main.structure  != "Access and referral systems" & 
           main.structure != "NA") %>% 
  mutate(main.structure = factor(main.structure,levels = factor_main)) %>% 
  select(type = 1,2,4,5)

d = fullmainfunction$byhft %>% 
  filter(scale == "Always",
         main.structure  != "Access and referral systems" & 
           main.structure != "NA") %>% 
  mutate(main.structure = factor(main.structure,levels = factor_main)) %>% 
  select(type = 1,2,4,5)


infrastructure_always = rbind(a,b,c,d) %>% 
  pivot_wider(id_cols = main.structure, 
              names_from = type, 
              values_from = c(staff,percent))

write_xlsx(setNames(list(infrastructure_always), "service delivery"), 
           "Excel datatable/infrastructure_always.xlsx")

