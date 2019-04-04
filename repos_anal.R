library(tidyverse)
library(grid)
library(gtable)
library(gridExtra)



dat_raw <- 
  read_csv('Adaptation option repository_consolidated.csv') %>% 
  select(-c(grep('X', names(read_csv('Adaptation option repository_consolidated.csv')), value = T))) 

  #dat_raw %>%
  #group_by(Implemented) %>% 
  #summarise(n = n())

dat <-
  dat_raw %>% 
  mutate(Implemented = ifelse(Implemented=='Y' | Implemented=='y' | Implemented=='Y?', 'Y',
                              ifelse(Implemented=='N' |Implemented=='n' | Implemented=='N?', 'N', NA)),
         Community = ifelse(Community=='Y' | Community=='y' | Community=='Y?', 'Y',
                            ifelse(Community=='N' |Community=='n' | Community=='N?', 'N', NA)),
         Context = ifelse(Context=='Y', 'CC', Context),
         Example = ifelse(Example=='Adaptation programme', 'Adaptation programs', 
                          ifelse(Example=='cooperatives', 'Cooperatives', 
                                 ifelse(Example=='disaster funds', 'Disaster funds',
                                        ifelse(Example=='Disaster Risk Management', 'Disaster risk management',
                                               ifelse(Example=='Diversifying livelihoods', 'Diversify livelihoods',
                                                      ifelse(Example=='Dynamic Ocean Management', 'Dynamic ocean management',
                                                             ifelse(Example=='effort' | Example=='flexibility' | Example=='Financial assistance' | Example=='N/A see Notes', NA,
                                                                    ifelse(Example=='Forecasting', 'Research',
                                                                           ifelse(Example=='insurance', 'Insurance',
                                                                                  ifelse(Example=='International agreement', 'International agreements',
                                                                                         ifelse(Example=='Investments in new gear' | Example=='Investment in new gear or innvovations' | Example=='Investments in new gear or innovations' | Example== 'Investment in new gear or technology', 'Investment in new gear or innovations',
                                                                                                ifelse(Example=='Investments to improve the value chain' | Example=='Improve Value Chain', 'Investments to improve value chain',
                                                                                                       ifelse(Example=='Market Diversification', 'Market diversification',
                                                                                                              ifelse(Example=='MPA' | Example=="MPA's", 'MPAs',
                                                                                                                     ifelse(Example=='Preemptive Infrastructure', 'Investment in new gear or innovations',
                                                                                                                            ifelse(Example=='Reserarch' | Example == 'research' | Example== 'Research (monitoring)', 'Research',
                                                                                                                                   ifelse(Example=='economic/community development' | Example=='Economic/Community development', 'Economic/community development',
                                                                                                                                          ifelse(Example=='education', 'Education',
                                                                                                                                                 ifelse(Example=='enforcement', 'Enforcement',
                                                                                                                                                        ifelse(Example=='Market diversification (catch new species)', 'Market diversification',
                                                                                                                                                               ifelse(Example=='Permit/license/quota bank' | Example=='Permit/licence/quota bank', 'Permit/license/quota banks',
                                                                                                                                                                      ifelse(Example=='Restrictions on discards (partial or full)', 'Reductions in other stressors',
                                                                                                                                                                             ifelse(Example=='Review program/regulations' | Example=='Review programs', 'Review programs / regulations',
                                                                                                                                                                                    ifelse(Example=='Transition out of fishery' | Example=='Investments to help transition out of fisheries', 'Transition out of fisheries',
                                                                                                                                                                                           Example)))))))))))))))))))))))),
         Anticipatory = ifelse(Anticipatory %in% c('x', 'X'), 1, Anticipatory) %>% as.numeric(.),
         Responsive = ifelse(Responsive %in% c('x', 'X'), 1, Responsive) %>% as.numeric(.),
         Both = ifelse(Both %in% c('x', 'X'), 1, Both) %>% as.numeric(.),
         Internat. = ifelse(Internat. %in% c('x', 'X'), 1, Internat.) %>% as.numeric(.),
         `Nat. govt.` = ifelse(`Nat. govt.` %in% c('x', 'X'), 1, `Nat. govt.`) %>% as.numeric(.),
         `Region. govt.` = ifelse(`Region. govt.` %in% c('x', 'X'), 1, `Region. govt.`) %>% as.numeric(.),
         `Local govt.` = ifelse(`Local govt.` %in% c('x', 'X'), 1, `Local govt.`) %>% as.numeric(.),
         NGO = ifelse(NGO %in% c('x', 'X'), 1, NGO) %>% as.numeric(.),
         Uni. = ifelse(Uni. %in% c('x', 'X'), 1, Uni.) %>% as.numeric(.),
         `Community assoc.` = ifelse(`Community assoc.` %in% c('x', 'X'), 1, `Community assoc.`) %>% as.numeric(.),
         `Business coop.` = ifelse(`Business coop.` %in% c('x', 'X'), 1, `Business coop.`) %>% as.numeric(.),
         Business = ifelse(Business %in% c('x', 'X'), 1, Business) %>% as.numeric(.),
         Individual = ifelse(Individual %in% c('x', 'X'), 1, Individual) %>% as.numeric(.),
         Country = ifelse(Country == 'USA-WC', 'USA-NW',
                          ifelse(Country=='US-CAN', 'USA',
                                 ifelse(Country=='Int', 'INT',
                                        ifelse(Country=='US', 'USA',
                                               ifelse(Country=='FIN-AX', 'FIN',
                                                      ifelse(Country=='Baltic Sea', 'EU',
                                                             ifelse(Country=='NL (Southwest Delta)', 'NL', 
                                                                    ifelse(Country == 'Italy', 'IT', 
                                                                           ifelse(Country == 'Sweden', 'SW',
                                                                                  Country)))))))))
  )
         


#What are examples most often in response to? In CC vs not?
CC_stressor_spread <- 
  dat %>% 
  mutate(stress_tot = apply(dat[5:14], 1, sum, na.rm = T),
         max_tot = apply(dat[5:14], 1, max, na.rm = T),
         `Stock decline` = (max_tot + 1 - `Stock decline`)/stress_tot,
         `Sp. distributional shifts` = (max_tot + 1 - `Sp. distributional shifts`)/stress_tot,
         `Ocean acidification` = (max_tot + 1 - `Ocean acidification`)/stress_tot,
         `Extreme climatic events` = (max_tot + 1 - `Extreme climatic events`)/stress_tot,
         `Uncertainty (ecological)` = (max_tot + 1 - `Uncertainty (ecological)`)/stress_tot,
         `Market changes` = (max_tot + 1 - `Market changes`)/stress_tot,
         `Regulation change` = (max_tot + 1 - `Regulation change`)/stress_tot,
         `Consolidation` = (max_tot + 1 - `Consolidation`)/stress_tot,
         `Globalization` = (max_tot + 1 - `Globalization`)/stress_tot,
         `Uncertainty (social)` = (max_tot + 1 - `Uncertainty (social)`)/stress_tot) %>% 
  filter(!(is.na(Context) | is.na(Example) | stress_tot==0)) %>% 
  group_by(Context, Example) %>%
  summarise(`Stock decline` = sum(`Stock decline`, na.rm = T),
            `Sp. distributional shifts` = sum(`Sp. distributional shifts`, na.rm = T),
            `Ocean acidification` = sum(`Ocean acidification`, na.rm = T),
            `Extreme climatic events` = sum(`Extreme climatic events`, na.rm = T),
            `Uncertainty (ecological)` = sum(`Uncertainty (ecological)`, na.rm = T),
            `Market changes` = sum(`Market changes`, na.rm = T),
            `Regulation change` = sum(`Regulation change`, na.rm = T),
            `Consolidation` = sum(`Consolidation`, na.rm = T),
            `Globalization` = sum(`Globalization`, na.rm = T),
            `Uncertainty (social)` = sum(`Uncertainty (social)`, na.rm = T)
            ) %>% 
  mutate(`Total score` = `Stock decline` +  `Sp. distributional shifts` + `Ocean acidification` + `Extreme climatic events` +
           `Uncertainty (ecological)` + `Market changes` + `Regulation change` + `Consolidation` + `Globalization` +`Uncertainty (social)`) %>% 
  arrange(Context, desc(`Total score`)) %>% 
  ungroup() %>% 
  filter(!is.na(`Total score`)) %>% 
  mutate(order = n():1) %>% 
  select(order, Context, Example, `Uncertainty (ecological)`, `Ocean acidification`, `Uncertainty (social)`,`Sp. distributional shifts`, `Extreme climatic events`, `Stock decline`, `Market changes`,`Regulation change`, `Globalization`, `Consolidation`) 

CC_stressor <-
  CC_stressor_spread %>% 
  gather(val = 'Score', key = 'Stressor', -c(Context, Example, order)) %>% 
  mutate(Score = ifelse(Score==0, NA, Score))

#View(CC_stressor_spread)

br <- CC_stressor_spread %>% select(order) %>% unlist %>% c(.)
lab <- CC_stressor_spread %>% select(Example) %>% unlist %>% substr(., 1, 49) 

labs <- data.frame(x = -12.5, br, lab = ifelse(lab=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', lab), col = c('aquamarine3', 'darkgoldenrod3', 'darkblue', 'aquamarine3', 'darkgoldenrod3', 'darkblue', 'aquamarine3', 'aquamarine3', 'darkblue', 'aquamarine3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'aquamarine3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'aquamarine3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'aquamarine3', 'aquamarine3', 'darkgoldenrod3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'aquamarine3', 'darkgoldenrod3', 'darkgoldenrod3', 'aquamarine3', 'aquamarine3', 'darkgoldenrod3', 'aquamarine3', 'aquamarine3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'darkblue', 'darkblue', 'darkblue', 'darkgoldenrod3'))

CC_stress_plot <-
  CC_stressor %>% 
  mutate(Stressor_name = Stressor, 
         Stressor = recode(Stressor, `Uncertainty (ecological)` = 'A', `Ocean acidification` = 'B', `Uncertainty (social)` = 'C', `Sp. distributional shifts` = 'D', `Extreme climatic events` = 'E', `Stock decline` = 'F', `Market changes` = 'G', `Regulation change` = 'H', `Globalization` = 'I', `Consolidation` = 'J')) %>% 
  ggplot(aes(Stressor, order, size = Score, color = Context)) + 
  geom_point() + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.margin = unit(c(1,5,1,17), "lines")) +
  ylab('') + 
  scale_x_discrete(breaks=c('A', 'B','C', 'D', 'E', 'F', 'G', 'H', 'I' , 'J'),
                                  labels=c("Uncertainty (ecological)", "Ocean acidification", "Uncertainty (social)",
                                           "Sp. distributional shifts", "Extreme climatic events", "Stock decline", "Market changes","Regulation change", "Globalization", "Consolidation")) +
  scale_y_continuous(breaks = c(0,57), labels = c('', ''), limits = c(0,57), expand = expand_scale())


 for(i in 1:length(labs$br)){ 
 CC_stress_plot <-
   CC_stress_plot + 
   annotation_custom(
    grob = grid::textGrob(label = labs$lab[i], just = 'left',gp = gpar(col = as.character(labs$col[i]))), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = labs$br[i]+0.2,      # Vertical position of the textGrob
    ymax = labs$br[i]-0.2,
    xmin = labs$x[i],         # Note: The grobs are positioned outside the plot area
    xmax = labs$x[i])
 }


CC_stress_plot <-
  CC_stress_plot + 
  annotation_custom(
    grob = grid::textGrob(label = 'Adaptation options:', just = 'left',gp = gpar(fontsize = 12, fontface = 'bold')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 57,      # Vertical position of the textGrob
    ymax = 57,
    xmin = 11,         # Note: The grobs are positioned outside the plot area
    xmax = 11)+ 
  annotation_custom(
    grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 55,      # Vertical position of the textGrob
    ymax = 55,
    xmin = 11,         # Note: The grobs are positioned outside the plot area
    xmax = 11)+ 
  annotation_custom(
    grob = grid::textGrob(label = 'Natural resource mgmt', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 56,      # Vertical position of the textGrob
    ymax = 56,
    xmin = 11,         # Note: The grobs are positioned outside the plot area
    xmax = 11)+ 
  annotation_custom(
    grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 54,      # Vertical position of the textGrob
    ymax = 54,
    xmin = 11,         # Note: The grobs are positioned outside the plot area
    xmax = 11)

pdf('Stressor_plot.pdf', width = 8.3, height = 11.7)

  gt <- ggplot_gtable(ggplot_build(CC_stress_plot))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.draw(gt)

dev.off()

#What examples are used most within a CC context vs without? Difference between implemented versus idea?

CC_ex <-
  dat %>% 
  filter(!(is.na(Implemented) | is.na(Community) | is.na(Context) | is.na(Example))) %>% 
  group_by(Context, Community, Implemented, Example) %>% 
  summarise(N_Example = n()) %>% 
  left_join(dat %>% 
              filter(!(is.na(Implemented) | is.na(Community) | is.na(Context)| is.na(Example))) %>% 
              group_by(Context, Community, Implemented) %>%
              summarise(total = n())) %>% 
  mutate(Prop_Example = N_Example/total) %>% 
  arrange(Context, Community, Implemented, desc(Prop_Example)) %>% 
  mutate(id = 1:n()) %>% 
  filter(id < 11)
  
write_csv(CC_ex, 'CC_ex.csv')

CC_ex_plot <-
  CC_ex %>% 
  ungroup %>% 
  rename(`Proportion of adaptation options` = Prop_Example) %>% 
  mutate(Context = ifelse(Context=='CC', 'Climate change', 'Fisheries'),
         Community = ifelse(Community == 'Y', 'Community focused', 'Not community focused'),
         Implemented = ifelse(Implemented == 'Y', 'Implemented', 'Not implemented'),
         Example = ifelse(grepl('Financial assistance or investment for entering,', Example), 'Financial assistance or investment for entering,', Example)) %>% 
  ggplot(aes(x = 1, y = 11 - id, label = Example, size = `Proportion of adaptation options`)) + 
  geom_text()+
  theme_classic()+ 
  theme(panel.border = element_rect(fill = NA), axis.text.y = element_blank(),axis.text.x = element_blank(), axis.ticks = element_blank())+
  xlab('')+ylab('')+
  labs(size="Proportion of\nadaptation\noptions") +
  scale_y_continuous(expand = expand_scale(), limits = c(0,11))+
  facet_grid( Context ~ Community*Implemented) 

g <- ggplot_gtable(ggplot_build(CC_ex_plot))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("goldenrod1","thistle1","goldenrod1","white", "white","thistle1","white","white")
k <- 1
for (i in strip_both[1:4]) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  jj <- which(grepl('rect', g$grobs[[i]]$grobs[[2]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  g$grobs[[i]]$grobs[[2]]$children[[jj]]$gp$fill <- fills[k+1]
  k <- k+2
}
fills <- c("orange", "cyan")
k <- 1
for (i in strip_both[5:6]) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

pdf('CC_ex.pdf', width = 12.5, height = 4)
grid.draw(g)
dev.off()


#What examples are anticipatory versus responsive versus both?


CC_ant <-
  dat %>% 
  filter(!(is.na(Context) | is.na(Example) | is.na(Implemented))) %>% 
  group_by(Context, Example, Implemented) %>% 
  summarise(Anticipatory = sum(Anticipatory, na.rm = T),
            Responsive = sum(Responsive, na.rm = T),
            Both = sum(Both, na.rm = T)
            )%>% 
  mutate(all = Anticipatory+Responsive+Both) %>% 
  left_join(dat %>% 
              filter(!(is.na(Context) | is.na(Example))) %>% 
              group_by(Context, Example) %>% 
              filter(Context=='CC') %>% 
              summarise(Anticipatory = sum(Anticipatory, na.rm = T),
                        Responsive = sum(Responsive, na.rm = T),
                        Both = sum(Both, na.rm = T)
              )%>% 
              mutate(Prop_Ant = Anticipatory/(Anticipatory+Responsive+Both)) %>% 
              arrange(desc(Prop_Ant)) %>% 
              mutate(id = 1:n()) %>%
              ungroup() %>% 
              select(Example, id)
              ) %>% 
  mutate(Prop_Anticipatory = Anticipatory/all,
         Prop_Responsive = Responsive/all,
         Prop_Both = Both/all) %>% 
  arrange(id)
  #filter(id < 11)

write_csv(CC_ant, 'CC_ant.csv')


ant_labs <-
  dat %>% 
  filter(!(is.na(Context) | is.na(Example) | is.na(Implemented))) %>% 
  group_by(Context, Example, Implemented) %>% 
  filter(Context=='CC') %>% 
  summarise(Anticipatory = sum(Anticipatory, na.rm = T),
            Responsive = sum(Responsive, na.rm = T),
            Both = sum(Both, na.rm = T)
  )%>% 
  mutate(Prop_Ant = Anticipatory/(Anticipatory+Responsive+Both)) %>% 
  arrange(desc(Prop_Ant)) %>%
  select(Example) %>% 
  distinct() %>% 
  mutate(id = 1:n()) %>%
  ungroup() %>% 
  select(Example, id) %>% 
  rename(lab = Example) %>% 
  mutate(lab = substr(lab, 1, 49),
         lab = ifelse(lab=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', lab)
  ) %>% 
  left_join(labs %>% 
              select(lab, col)) %>% 
  distinct() %>% 
  mutate(col_order=ifelse(col=='aquamarine3',1,
                          ifelse(col=='darkgoldenrod3',2,3))) %>% 
  arrange(lab) %>% 
  mutate(br = n():1,
         x = 2) %>% 
  arrange(col_order, lab)

Ant_plot1 <-
  CC_ant %>% 
  select(Context, Example, Implemented, Prop_Anticipatory, Prop_Responsive, Prop_Both) %>%
  rename(Anticipatory = Prop_Anticipatory, Responsive = Prop_Responsive, Both = Prop_Both) %>% 
  ungroup() %>% 
  mutate(Example = substr(Example, 1, 49),
         Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
         Context = ifelse(Context=='N', 'F', Context),
         Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
  mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
  gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
  group_by(Context, Example, Implemented) %>% 
  ggplot(aes(x="", y=Count, fill=Group))+
  geom_bar(width = 1, stat = "identity")+ 
  scale_fill_manual(values=c("aquamarine", "blue", "red"))+ 
  coord_polar("y", start=0) + 
  theme_void() + 
  theme(legend.position="left")+
  labs(fill = '') +
  facet_grid(Example ~Context*Implemented) 

Ant_plot <-
  CC_ant %>% 
  select(Context, Example, Implemented, Prop_Anticipatory, Prop_Responsive, Prop_Both) %>%
  rename(Anticipatory = Prop_Anticipatory, Responsive = Prop_Responsive, Both = Prop_Both) %>% 
  ungroup() %>% 
  mutate(Example = substr(Example, 1, 49),
         Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
         Context = ifelse(Context=='N', 'F', Context),
         Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
  mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
  gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
  group_by(Context, Example, Implemented) %>% 
  ggplot(aes(x="", y=Count, fill=Group))+
  geom_bar(width = 1, stat = "identity")+ 
  scale_fill_manual(values=c("aquamarine", "blue", "red"))+
  coord_polar("y", start=0) + 
  theme_void() + 
  theme(legend.position="left", strip.text.y = element_text(colour = 'white'))+
  labs(fill = '') +
  facet_grid(Example ~Context*Implemented)  

#, strip.text.y = element_text(colour = 'white')

gp1 <- ggplot_gtable(ggplot_build(Ant_plot1))
gp <- ggplot_gtable(ggplot_build(Ant_plot))
gp$widths  <- gp1$widths
gp$heights <- gp1$heights
gp$widths[10] <- gp1$widths[10] + gp1$widths[10]
gp$widths[14] <- gp1$widths[14] + gp1$widths[14]*0.5
#gp <- ggplotGrob(gp)
#gtable_show_layout(gp)
areas <- gtable_filter(gp, "strip-r", trim = F)$layout

leg <- which(sapply(gp$grobs, function(x) x$name) == "guide-box")
legend <- gp$grobs[[leg]] 
gp0 <- gp 

leg2 <- ggplot() + geom_blank(aes(c(0,rep(1,4)),1:5)) +  theme_void() +
  scale_y_continuous(expand = expand_scale())+
  scale_x_continuous(expand = expand_scale())+
  theme(plot.margin = unit(c(0,0,0,0), "lines")) +
  annotation_custom(
    grob = grid::textGrob(label = 'Adaptation\noptions:', just = 'left',gp = gpar(fontsize = 12, fontface = 'bold')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 4,      # Vertical position of the textGrob
    ymax = 5,
    xmin = 0,         # Note: The grobs are positioned outside the plot area
    xmax = 0) +
  annotation_custom(
    grob = grid::textGrob(label = 'Natural\nresource\nmgmt', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 3,      # Vertical position of the textGrob
    ymax = 4,
    xmin = 0,         # Note: The grobs are positioned outside the plot area
    xmax = 0) +
  annotation_custom(
    grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 2,      # Vertical position of the textGrob
    ymax = 3,
    xmin = 0,         # Note: The grobs are positioned outside the plot area
    xmax = 0)+
  annotation_custom(
    grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 1.5,      # Vertical position of the textGrob
    ymax = 2.5,
    xmin = 0,         # Note: The grobs are positioned outside the plot area
    xmax = 0)

for(i in 1:dim(areas)[1]){
 gp0 <- gtable_add_grob(x = gp0,
                       grobs = tableGrob(ant_labs$lab[i],
                                         theme = ttheme_minimal(base_colour = as.character(ant_labs$col[i]))),
                       t = areas$t[i], 
                       l = areas$l[i], 
                       b = areas$b[i], 
                       r = areas$r[i], 
                       name = paste0("new_strip-r-",i))
}


grid::grid.draw(gp0)

pdf('Ant_plot.pdf', width = 6, height = 7)
grid.newpage()
pushViewport(viewport(layout = grid.layout(8, 6)))
print(leg2, vp=viewport(layout.pos.row = 1:3, layout.pos.col = 1))
#print(grid.draw(legend), vp=viewport(layout.pos.row = 5, layout.pos.col = 1))
print(grid.draw(gp0), vp =viewport(layout.pos.row = 1:8, layout.pos.col = 2:6))
dev.off()

#What managers implement which examples?

CC_man <-
  dat %>% 
  filter(!(is.na(Context) | is.na(Example) | is.na(Implemented))) %>% 
  group_by(Context, Example, Implemented) %>% 
  summarise(Internat. = sum(Internat., na.rm = T),
            `Nat. govt.` = sum(`Nat. govt.`, na.rm = T),
            `Region. govt.` = sum(`Region. govt.`, na.rm = T),
            `Local govt.` = sum(`Local govt.`, na.rm = T),
             NGO = sum(NGO, na.rm = T),
            Uni. = sum(Uni., na.rm = T),
            `Community assoc.` = sum(`Community assoc.`, na.rm = T),
            `Business coop.` = sum(`Business coop.`, na.rm = T),
            Business = sum(Business, na.rm = T),
            Individual = sum(Individual, na.rm = T)
  )%>% 
  mutate(all = Internat. + `Nat. govt.` + `Region. govt.` + `Local govt.` + NGO + Uni. + `Community assoc.` + `Business coop.` + Business + Individual) %>% 
  left_join(dat %>% 
              filter(!(is.na(Context) | is.na(Example))) %>% 
              group_by(Context, Example) %>% 
              filter(Context=='CC') %>% 
              summarise(Internat. = sum(Internat., na.rm = T),
                        `Nat. govt.` = sum(`Nat. govt.`, na.rm = T),
                        `Region. govt.` = sum(`Region. govt.`, na.rm = T),
                        `Local govt.` = sum(`Local govt.`, na.rm = T),
                        NGO = sum(NGO, na.rm = T),
                        Uni. = sum(Uni., na.rm = T),
                        `Community assoc.` = sum(`Community assoc.`, na.rm = T),
                        `Business coop.` = sum(`Business coop.`, na.rm = T),
                        Business = sum(Business, na.rm = T),
                        Individual = sum(Individual, na.rm = T)
              )%>% 
              mutate(all = Internat. + `Nat. govt.` + `Region. govt.` + `Local govt.` + NGO + Uni. + `Community assoc.` + `Business coop.` + Business + Individual) %>% 
              mutate(Prop_top = (Internat. + `Nat. govt.` + `Region. govt.`)/all) %>% 
              arrange(desc(Prop_top)) %>% 
              mutate(id = 1:n()) %>%
              ungroup() %>% 
              select(Example, id)
  ) %>% 
  mutate(Prop_top = (Internat. + `Nat. govt.` + `Region. govt.`)/all,
         Prop_bottom = (`Local govt.` + `Community assoc.` + `Business coop.`)/all,
         Prop_NGO = (NGO + Uni.)/all,
         Prop_ind = (Business + Individual)/all) %>% 
  arrange(id)
#filter(id < 11)

write_csv(CC_man, 'CC_man.csv')


Man_plot1 <-
  CC_man %>% 
  select(Context, Example, Implemented, Prop_top, Prop_bottom, Prop_NGO, Prop_ind) %>%
  rename(`Top-down` = Prop_top, `Bottom-up` = Prop_bottom, `Non-profit` = Prop_NGO, Individual = Prop_ind ) %>% 
  ungroup() %>% 
  mutate(Example = substr(Example, 1, 49),
         Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
         Context = ifelse(Context=='N', 'F', Context),
         Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
  mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
  gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
  group_by(Context, Example, Implemented) %>% 
  ggplot(aes(x="", y=Count, fill=Group))+
  geom_bar(width = 1, stat = "identity")+ 
#  scale_fill_discrete(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual')) +
  scale_fill_manual(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual'), values=c("aquamarine", "blue", "red","yellow"))+ 
  coord_polar("y", start=0) + 
  theme_void() + 
  theme(legend.position="left")+
  labs(fill = '') +
  facet_grid(Example ~Context*Implemented) 

Man_plot <-
  CC_man %>% 
  select(Context, Example, Implemented, Prop_top, Prop_bottom, Prop_NGO, Prop_ind) %>%
  rename(`Top-down` = Prop_top, `Bottom-up` = Prop_bottom, `Non-profit` = Prop_NGO, Individual = Prop_ind ) %>% 
  ungroup() %>% 
  mutate(Example = substr(Example, 1, 49),
         Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
         Context = ifelse(Context=='N', 'F', Context),
         Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
  mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
  gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
  group_by(Context, Example, Implemented) %>% 
  ggplot(aes(x="", y=Count, fill=Group))+
  geom_bar(width = 1, stat = "identity")+ 
  #  scale_fill_discrete(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual')) +
  scale_fill_manual(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual'), values=c("aquamarine", "blue", "red","yellow"))+ 
  coord_polar("y", start=0) + 
  theme_void() + 
  theme(legend.position="left", strip.text.y = element_text(colour = 'white'))+
  labs(fill = '') +
  facet_grid(Example ~Context*Implemented) 
#, strip.text.y = element_text(colour = 'white')

gp1 <- ggplot_gtable(ggplot_build(Man_plot1))
gp <- ggplot_gtable(ggplot_build(Man_plot))
gp$widths  <- gp1$widths
gp$heights <- gp1$heights
gp$widths[10] <- gp1$widths[10] + gp1$widths[10]
gp$widths[14] <- gp1$widths[14] + gp1$widths[14]*0.5
#gp <- ggplotGrob(gp)
#gtable_show_layout(gp)
areas <- gtable_filter(gp, "strip-r", trim = F)$layout

leg <- which(sapply(gp$grobs, function(x) x$name) == "guide-box")
legend <- gp$grobs[[leg]] 
gp0 <- gp 

leg2 <- ggplot() + geom_blank(aes(c(0,rep(1,4)),1:5)) +  theme_void() +
  scale_y_continuous(expand = expand_scale())+
  scale_x_continuous(expand = expand_scale())+
  theme(plot.margin = unit(c(0,0,0,0), "lines")) +
  annotation_custom(
    grob = grid::textGrob(label = 'Adaptation\noptions:', just = 'left',gp = gpar(fontsize = 12, fontface = 'bold')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 4,      # Vertical position of the textGrob
    ymax = 5,
    xmin = 0,         # Note: The grobs are positioned outside the plot area
    xmax = 0) +
  annotation_custom(
    grob = grid::textGrob(label = 'Natural\nresource\nmgmt', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 3,      # Vertical position of the textGrob
    ymax = 4,
    xmin = 0,         # Note: The grobs are positioned outside the plot area
    xmax = 0) +
  annotation_custom(
    grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 2,      # Vertical position of the textGrob
    ymax = 3,
    xmin = 0,         # Note: The grobs are positioned outside the plot area
    xmax = 0)+
  annotation_custom(
    grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
    ymin = 1.5,      # Vertical position of the textGrob
    ymax = 2.5,
    xmin = 0,         # Note: The grobs are positioned outside the plot area
    xmax = 0)

for(i in 1:dim(areas)[1]){
  gp0 <- gtable_add_grob(x = gp0,
                         grobs = tableGrob(ant_labs$lab[i],
                                           theme = ttheme_minimal(base_colour = as.character(ant_labs$col[i]))),
                         t = areas$t[i], 
                         l = areas$l[i], 
                         b = areas$b[i], 
                         r = areas$r[i], 
                         name = paste0("new_strip-r-",i))
}


grid::grid.draw(gp0)

pdf('Man_plot.pdf', width = 6, height = 7)
grid.newpage()
pushViewport(viewport(layout = grid.layout(8, 6)))
print(leg2, vp=viewport(layout.pos.row = 1:3, layout.pos.col = 1))
#print(grid.draw(legend), vp=viewport(layout.pos.row = 5, layout.pos.col = 1))
print(grid.draw(gp0), vp =viewport(layout.pos.row = 1:8, layout.pos.col = 2:6))
dev.off()


#What are some differences among countries in examples, managers, and responsiveness?


#NOW ADD FILTERS THROUGHOUT AND JOIN TO INTRODUCE NAs


allcountries <- dat %>% select(Country) %>% unique %>% unlist
country_list <- NULL
country_list[[1]]<-c(allcountries)
for(i in (length(allcountries)+1):2){
  country_list[[i]] <- c(allcountries[i-1])
}
set_names(country_list[-1], allcountries)
set_names(country_list[1], 'all')


country_list[-1] %>% 
  map(function(x){
    
    #What are examples most often in response to? In CC vs not?
    CC_stressor_spread_c <- 
      dat %>% 
      filter(Country %in% country_list[[x]]) %>% 
      mutate(stress_tot = apply(dat[5:14], 1, sum, na.rm = T),
             max_tot = apply(dat[5:14], 1, max, na.rm = T),
             `Stock decline` = (max_tot + 1 - `Stock decline`)/stress_tot,
             `Sp. distributional shifts` = (max_tot + 1 - `Sp. distributional shifts`)/stress_tot,
             `Ocean acidification` = (max_tot + 1 - `Ocean acidification`)/stress_tot,
             `Extreme climatic events` = (max_tot + 1 - `Extreme climatic events`)/stress_tot,
             `Uncertainty (ecological)` = (max_tot + 1 - `Uncertainty (ecological)`)/stress_tot,
             `Market changes` = (max_tot + 1 - `Market changes`)/stress_tot,
             `Regulation change` = (max_tot + 1 - `Regulation change`)/stress_tot,
             `Consolidation` = (max_tot + 1 - `Consolidation`)/stress_tot,
             `Globalization` = (max_tot + 1 - `Globalization`)/stress_tot,
             `Uncertainty (social)` = (max_tot + 1 - `Uncertainty (social)`)/stress_tot) %>% 
      filter(!(is.na(Context) | is.na(Example) | stress_tot==0)) %>% 
      group_by(Context, Example) %>%
      summarise(`Stock decline` = sum(`Stock decline`, na.rm = T),
                `Sp. distributional shifts` = sum(`Sp. distributional shifts`, na.rm = T),
                `Ocean acidification` = sum(`Ocean acidification`, na.rm = T),
                `Extreme climatic events` = sum(`Extreme climatic events`, na.rm = T),
                `Uncertainty (ecological)` = sum(`Uncertainty (ecological)`, na.rm = T),
                `Market changes` = sum(`Market changes`, na.rm = T),
                `Regulation change` = sum(`Regulation change`, na.rm = T),
                `Consolidation` = sum(`Consolidation`, na.rm = T),
                `Globalization` = sum(`Globalization`, na.rm = T),
                `Uncertainty (social)` = sum(`Uncertainty (social)`, na.rm = T)
      ) %>% 
      mutate(`Total score` = `Stock decline` +  `Sp. distributional shifts` + `Ocean acidification` + `Extreme climatic events` +
               `Uncertainty (ecological)` + `Market changes` + `Regulation change` + `Consolidation` + `Globalization` +`Uncertainty (social)`) %>% 
      arrange(Context, desc(`Total score`)) %>% 
      ungroup() %>% 
      filter(!is.na(`Total score`)) %>% 
      right_join(CC_stressor_spread %>% 
                   select(Context, Example)) %>%
      mutate(order = n():1) %>% 
      select(order, Context, Example, `Uncertainty (ecological)`, `Ocean acidification`, `Uncertainty (social)`,`Sp. distributional shifts`, `Extreme climatic events`, `Stock decline`, `Market changes`,`Regulation change`, `Globalization`, `Consolidation`)
    
    CC_stressor <-
      CC_stressor_spread_c %>% 
      gather(val = 'Score', key = 'Stressor', -c(Context, Example, order)) %>% 
      mutate(Score = ifelse(Score==0, NA, Score))
    
    #View(CC_stressor_spread)
    
    br <- CC_stressor_spread_c %>% select(order) %>% unlist %>% c(.)
    lab <- CC_stressor_spread_c %>% select(Example) %>% unlist %>% substr(., 1, 49) 
    
    labs <- data.frame(x = -12.5, br, lab = ifelse(lab=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', lab), col = c('aquamarine3', 'darkgoldenrod3', 'darkblue', 'aquamarine3', 'darkgoldenrod3', 'darkblue', 'aquamarine3', 'aquamarine3', 'darkblue', 'aquamarine3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'aquamarine3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'aquamarine3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'aquamarine3', 'aquamarine3', 'darkgoldenrod3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'aquamarine3', 'darkgoldenrod3', 'darkgoldenrod3', 'aquamarine3', 'aquamarine3', 'darkgoldenrod3', 'aquamarine3', 'aquamarine3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'darkblue', 'darkblue', 'darkblue', 'darkgoldenrod3'))
    
    CC_stress_plot <-
      CC_stressor %>% 
      mutate(Stressor_name = Stressor, 
             Stressor = recode(Stressor, `Uncertainty (ecological)` = 'A', `Ocean acidification` = 'B', `Uncertainty (social)` = 'C', `Sp. distributional shifts` = 'D', `Extreme climatic events` = 'E', `Stock decline` = 'F', `Market changes` = 'G', `Regulation change` = 'H', `Globalization` = 'I', `Consolidation` = 'J')) %>% 
      ggplot(aes(Stressor, order, size = Score, color = Context)) + 
      geom_point() + 
      theme_light() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            plot.margin = unit(c(1,5,1,17), "lines")) +
      ylab('') + 
      scale_x_discrete(breaks=c('A', 'B','C', 'D', 'E', 'F', 'G', 'H', 'I' , 'J'),
                       labels=c("Uncertainty (ecological)", "Ocean acidification", "Uncertainty (social)",
                                "Sp. distributional shifts", "Extreme climatic events", "Stock decline", "Market changes","Regulation change", "Globalization", "Consolidation")) +
      scale_y_continuous(breaks = c(0,57), labels = c('', ''), limits = c(0,57), expand = expand_scale())
    
    
    for(i in 1:length(labs$br)){ 
      CC_stress_plot <-
        CC_stress_plot + 
        annotation_custom(
          grob = grid::textGrob(label = labs$lab[i], just = 'left',gp = gpar(col = as.character(labs$col[i]))), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
          ymin = labs$br[i]+0.2,      # Vertical position of the textGrob
          ymax = labs$br[i]-0.2,
          xmin = labs$x[i],         # Note: The grobs are positioned outside the plot area
          xmax = labs$x[i])
    }
    
    
    CC_stress_plot <-
      CC_stress_plot + 
      annotation_custom(
        grob = grid::textGrob(label = 'Adaptation options:', just = 'left',gp = gpar(fontsize = 12, fontface = 'bold')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 57,      # Vertical position of the textGrob
        ymax = 57,
        xmin = 11,         # Note: The grobs are positioned outside the plot area
        xmax = 11)+ 
      annotation_custom(
        grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 55,      # Vertical position of the textGrob
        ymax = 55,
        xmin = 11,         # Note: The grobs are positioned outside the plot area
        xmax = 11)+ 
      annotation_custom(
        grob = grid::textGrob(label = 'Natural resource mgmt', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 56,      # Vertical position of the textGrob
        ymax = 56,
        xmin = 11,         # Note: The grobs are positioned outside the plot area
        xmax = 11)+ 
      annotation_custom(
        grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 54,      # Vertical position of the textGrob
        ymax = 54,
        xmin = 11,         # Note: The grobs are positioned outside the plot area
        xmax = 11)
    
    pdf(paste0('country_figs/Stressor_plot_', country_list[[x]],'.pdf'), width = 8.3, height = 11.7)
    
    gt <- ggplot_gtable(ggplot_build(CC_stress_plot))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid::grid.draw(gt)
    
    dev.off()
    
    #What examples are used most within a CC context vs without? Difference between implemented versus idea?
    
    CC_ex_c <-
      dat %>%
      filter(Country %in% country_list[[x]]) %>% 
      filter(!(is.na(Implemented) | is.na(Community) | is.na(Context) | is.na(Example))) %>% 
      group_by(Context, Community, Implemented, Example) %>% 
      summarise(N_Example = n()) %>% 
      left_join(dat %>%
                  filter(Country %in% country_list[[x]]) %>%  
                  filter(!(is.na(Implemented) | is.na(Community) | is.na(Context)| is.na(Example))) %>% 
                  group_by(Context, Community, Implemented) %>%
                  summarise(total = n())) %>% 
      mutate(Prop_Example = N_Example/total) %>% 
      arrange(Context, Community, Implemented, desc(Prop_Example)) %>% 
      right_join(CC_ex %>% 
                   select(Context, Community, Implemented)) %>% 
      mutate(id = 1:n()) %>% 
      filter(id < 11)
    
    write_csv(CC_ex_c, paste0('country_figs/CC_ex_', country_list[[x]], '.csv'))
    
    CC_ex_plot <-
      CC_ex_c %>% 
      ungroup %>% 
      rename(`Proportion of adaptation options` = Prop_Example) %>% 
      mutate(Context = ifelse(Context=='CC', 'Climate change', 'Fisheries'),
             Community = ifelse(Community == 'Y', 'Community focused', 'Not community focused'),
             Implemented = ifelse(Implemented == 'Y', 'Implemented', 'Not implemented'),
             Example = ifelse(grepl('Financial assistance or investment for entering,', Example), 'Financial assistance or investment for entering,', Example)) %>% 
      ggplot(aes(x = 1, y = 11 - id, label = Example, size = `Proportion of adaptation options`)) + 
      geom_text()+
      theme_classic()+ 
      theme(panel.border = element_rect(fill = NA), axis.text.y = element_blank(),axis.text.x = element_blank(), axis.ticks = element_blank())+
      xlab('')+ylab('')+
      labs(size="Proportion of\nadaptation\noptions") +
      scale_y_continuous(expand = expand_scale(), limits = c(0,11))+
      facet_grid( Context ~ Community*Implemented) 
    
    g <- ggplot_gtable(ggplot_build(CC_ex_plot))
    strip_both <- which(grepl('strip-', g$layout$name))
    fills <- c("goldenrod1","thistle1","goldenrod1","white", "white","thistle1","white","white")
    k <- 1
    for (i in strip_both[1:4]) {
      j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
      jj <- which(grepl('rect', g$grobs[[i]]$grobs[[2]]$childrenOrder))
      g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
      g$grobs[[i]]$grobs[[2]]$children[[jj]]$gp$fill <- fills[k+1]
      k <- k+2
    }
    fills <- c("orange", "cyan")
    k <- 1
    for (i in strip_both[5:6]) {
      j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
      g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
      k <- k+1
    }
    
    pdf(paste0('country_figs/CC_ex_', country_list[[x]], '.pdf'), width = 12.5, height = 4)
    grid.draw(g)
    dev.off()
    
    
    #What examples are anticipatory versus responsive versus both?
    
    
    CC_ant_c <-
      dat %>%
      filter(Country %in% country_list[[x]]) %>% 
      filter(!(is.na(Context) | is.na(Example) | is.na(Implemented))) %>% 
      group_by(Context, Example, Implemented) %>% 
      summarise(Anticipatory = sum(Anticipatory, na.rm = T),
                Responsive = sum(Responsive, na.rm = T),
                Both = sum(Both, na.rm = T)
      )%>% 
      mutate(all = Anticipatory+Responsive+Both) %>% 
      left_join(dat %>%
                  filter(Country %in% country_list[[x]]) %>% 
                  filter(!(is.na(Context) | is.na(Example))) %>% 
                  group_by(Context, Example) %>% 
                  filter(Context=='CC') %>% 
                  summarise(Anticipatory = sum(Anticipatory, na.rm = T),
                            Responsive = sum(Responsive, na.rm = T),
                            Both = sum(Both, na.rm = T)
                  )%>% 
                  mutate(Prop_Ant = Anticipatory/(Anticipatory+Responsive+Both)) %>% 
                  arrange(desc(Prop_Ant)) %>% 
                  mutate(id = 1:n()) %>%
                  ungroup() %>% 
                  select(Example, id)
      ) %>% 
      mutate(Prop_Anticipatory = Anticipatory/all,
             Prop_Responsive = Responsive/all,
             Prop_Both = Both/all) %>% 
      arrange(id) %>% 
      right_join(CC_ant %>% 
                   select(Context, Example, Implemented)) 
    #filter(id < 11)
    
    write_csv(CC_ant_c, paste0('country_figs/CC_ant_', country_list[[x]], '.csv'))
    
    
    ant_labs <-
      dat %>% 
      filter(!(is.na(Context) | is.na(Example) | is.na(Implemented))) %>% 
      group_by(Context, Example, Implemented) %>% 
      filter(Context=='CC') %>% 
      summarise(Anticipatory = sum(Anticipatory, na.rm = T),
                Responsive = sum(Responsive, na.rm = T),
                Both = sum(Both, na.rm = T)
      )%>% 
      mutate(Prop_Ant = Anticipatory/(Anticipatory+Responsive+Both)) %>% 
      arrange(desc(Prop_Ant)) %>%
      select(Example) %>% 
      distinct() %>% 
      mutate(id = 1:n()) %>%
      ungroup() %>% 
      select(Example, id) %>% 
      rename(lab = Example) %>% 
      mutate(lab = substr(lab, 1, 49),
             lab = ifelse(lab=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', lab)
      ) %>% 
      left_join(labs %>% 
                  select(lab, col)) %>% 
      distinct() %>% 
      mutate(col_order=ifelse(col=='aquamarine3',1,
                              ifelse(col=='darkgoldenrod3',2,3))) %>% 
      arrange(lab) %>% 
      mutate(br = n():1,
             x = 2) %>% 
      arrange(col_order, lab)
    
    Ant_plot1 <-
      CC_ant_c %>% 
      select(Context, Example, Implemented, Prop_Anticipatory, Prop_Responsive, Prop_Both) %>%
      rename(Anticipatory = Prop_Anticipatory, Responsive = Prop_Responsive, Both = Prop_Both) %>% 
      ungroup() %>% 
      mutate(Example = substr(Example, 1, 49),
             Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
             Context = ifelse(Context=='N', 'F', Context),
             Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
      mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
      gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
      group_by(Context, Example, Implemented) %>% 
      ggplot(aes(x="", y=Count, fill=Group))+
      geom_bar(width = 1, stat = "identity")+ 
      scale_fill_manual(values=c("aquamarine", "blue", "red"))+ 
      coord_polar("y", start=0) + 
      theme_void() + 
      theme(legend.position="left")+
      labs(fill = '') +
      facet_grid(Example ~Context*Implemented) 
    
    Ant_plot <-
      CC_ant_c %>% 
      select(Context, Example, Implemented, Prop_Anticipatory, Prop_Responsive, Prop_Both) %>%
      rename(Anticipatory = Prop_Anticipatory, Responsive = Prop_Responsive, Both = Prop_Both) %>% 
      ungroup() %>% 
      mutate(Example = substr(Example, 1, 49),
             Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
             Context = ifelse(Context=='N', 'F', Context),
             Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
      mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
      gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
      group_by(Context, Example, Implemented) %>% 
      ggplot(aes(x="", y=Count, fill=Group))+
      geom_bar(width = 1, stat = "identity")+ 
      scale_fill_manual(values=c("aquamarine", "blue", "red"))+
      coord_polar("y", start=0) + 
      theme_void() + 
      theme(legend.position="left", strip.text.y = element_text(colour = 'white'))+
      labs(fill = '') +
      facet_grid(Example ~Context*Implemented)  
    
    #, strip.text.y = element_text(colour = 'white')
    
    gp1 <- ggplot_gtable(ggplot_build(Ant_plot1))
    gp <- ggplot_gtable(ggplot_build(Ant_plot))
    gp$widths  <- gp1$widths
    gp$heights <- gp1$heights
    gp$widths[10] <- gp1$widths[10] + gp1$widths[10]
    gp$widths[14] <- gp1$widths[14] + gp1$widths[14]*0.5
    #gp <- ggplotGrob(gp)
    #gtable_show_layout(gp)
    areas <- gtable_filter(gp, "strip-r", trim = F)$layout
    
    leg <- which(sapply(gp$grobs, function(x) x$name) == "guide-box")
    legend <- gp$grobs[[leg]] 
    gp0 <- gp 
    
    leg2 <- ggplot() + geom_blank(aes(c(0,rep(1,4)),1:5)) +  theme_void() +
      scale_y_continuous(expand = expand_scale())+
      scale_x_continuous(expand = expand_scale())+
      theme(plot.margin = unit(c(0,0,0,0), "lines")) +
      annotation_custom(
        grob = grid::textGrob(label = 'Adaptation\noptions:', just = 'left',gp = gpar(fontsize = 12, fontface = 'bold')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 4,      # Vertical position of the textGrob
        ymax = 5,
        xmin = 0,         # Note: The grobs are positioned outside the plot area
        xmax = 0) +
      annotation_custom(
        grob = grid::textGrob(label = 'Natural\nresource\nmgmt', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 3,      # Vertical position of the textGrob
        ymax = 4,
        xmin = 0,         # Note: The grobs are positioned outside the plot area
        xmax = 0) +
      annotation_custom(
        grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 2,      # Vertical position of the textGrob
        ymax = 3,
        xmin = 0,         # Note: The grobs are positioned outside the plot area
        xmax = 0)+
      annotation_custom(
        grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 1.5,      # Vertical position of the textGrob
        ymax = 2.5,
        xmin = 0,         # Note: The grobs are positioned outside the plot area
        xmax = 0)
    
    for(i in 1:dim(areas)[1]){
      gp0 <- gtable_add_grob(x = gp0,
                             grobs = tableGrob(ant_labs$lab[i],
                                               theme = ttheme_minimal(base_colour = as.character(ant_labs$col[i]))),
                             t = areas$t[i], 
                             l = areas$l[i], 
                             b = areas$b[i], 
                             r = areas$r[i], 
                             name = paste0("new_strip-r-",i))
    }
    
    
    #grid::grid.draw(gp0)
    
    pdf(paste0('country_figs/Ant_plot_', country_list[[x]], '.pdf'), width = 6, height = 7)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(8, 6)))
    print(leg2, vp=viewport(layout.pos.row = 1:3, layout.pos.col = 1))
    #print(grid.draw(legend), vp=viewport(layout.pos.row = 5, layout.pos.col = 1))
    print(grid.draw(gp0), vp =viewport(layout.pos.row = 1:8, layout.pos.col = 2:6))
    dev.off()
    
    #What managers implement which examples?
    
    CC_man_c <-
      dat %>%
      filter(Country %in% country_list[[x]]) %>% 
      filter(!(is.na(Context) | is.na(Example) | is.na(Implemented))) %>% 
      group_by(Context, Example, Implemented) %>% 
      summarise(Internat. = sum(Internat., na.rm = T),
                `Nat. govt.` = sum(`Nat. govt.`, na.rm = T),
                `Region. govt.` = sum(`Region. govt.`, na.rm = T),
                `Local govt.` = sum(`Local govt.`, na.rm = T),
                NGO = sum(NGO, na.rm = T),
                Uni. = sum(Uni., na.rm = T),
                `Community assoc.` = sum(`Community assoc.`, na.rm = T),
                `Business coop.` = sum(`Business coop.`, na.rm = T),
                Business = sum(Business, na.rm = T),
                Individual = sum(Individual, na.rm = T)
      )%>% 
      mutate(all = Internat. + `Nat. govt.` + `Region. govt.` + `Local govt.` + NGO + Uni. + `Community assoc.` + `Business coop.` + Business + Individual) %>% 
      left_join(dat %>% 
                  filter(Country %in% country_list[[x]]) %>% 
                  filter(!(is.na(Context) | is.na(Example))) %>% 
                  group_by(Context, Example) %>% 
                  filter(Context=='CC') %>% 
                  summarise(Internat. = sum(Internat., na.rm = T),
                            `Nat. govt.` = sum(`Nat. govt.`, na.rm = T),
                            `Region. govt.` = sum(`Region. govt.`, na.rm = T),
                            `Local govt.` = sum(`Local govt.`, na.rm = T),
                            NGO = sum(NGO, na.rm = T),
                            Uni. = sum(Uni., na.rm = T),
                            `Community assoc.` = sum(`Community assoc.`, na.rm = T),
                            `Business coop.` = sum(`Business coop.`, na.rm = T),
                            Business = sum(Business, na.rm = T),
                            Individual = sum(Individual, na.rm = T)
                  )%>% 
                  mutate(all = Internat. + `Nat. govt.` + `Region. govt.` + `Local govt.` + NGO + Uni. + `Community assoc.` + `Business coop.` + Business + Individual) %>% 
                  mutate(Prop_top = (Internat. + `Nat. govt.` + `Region. govt.`)/all) %>% 
                  arrange(desc(Prop_top)) %>% 
                  mutate(id = 1:n()) %>%
                  ungroup() %>% 
                  select(Example, id)
      ) %>% 
      mutate(Prop_top = (Internat. + `Nat. govt.` + `Region. govt.`)/all,
             Prop_bottom = (`Local govt.` + `Community assoc.` + `Business coop.`)/all,
             Prop_NGO = (NGO + Uni.)/all,
             Prop_ind = (Business + Individual)/all) %>% 
      arrange(id)%>% 
      right_join(CC_man %>% 
                   select(Context, Example, Implemented)) 
    #filter(id < 11)
    
    write_csv(CC_man_c, paste0('country_figs/CC_man_', country_list[[x]],'.csv'))
    
    
    Man_plot1 <-
      CC_man_c %>% 
      select(Context, Example, Implemented, Prop_top, Prop_bottom, Prop_NGO, Prop_ind) %>%
      rename(`Top-down` = Prop_top, `Bottom-up` = Prop_bottom, `Non-profit` = Prop_NGO, Individual = Prop_ind ) %>% 
      ungroup() %>% 
      mutate(Example = substr(Example, 1, 49),
             Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
             Context = ifelse(Context=='N', 'F', Context),
             Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
      mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
      gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
      group_by(Context, Example, Implemented) %>% 
      ggplot(aes(x="", y=Count, fill=Group))+
      geom_bar(width = 1, stat = "identity")+ 
      #  scale_fill_discrete(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual')) +
      scale_fill_manual(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual'), values=c("aquamarine", "blue", "red","yellow"))+ 
      coord_polar("y", start=0) + 
      theme_void() + 
      theme(legend.position="left")+
      labs(fill = '') +
      facet_grid(Example ~Context*Implemented) 
    
    Man_plot <-
      CC_man_c %>% 
      select(Context, Example, Implemented, Prop_top, Prop_bottom, Prop_NGO, Prop_ind) %>%
      rename(`Top-down` = Prop_top, `Bottom-up` = Prop_bottom, `Non-profit` = Prop_NGO, Individual = Prop_ind ) %>% 
      ungroup() %>% 
      mutate(Example = substr(Example, 1, 49),
             Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
             Context = ifelse(Context=='N', 'F', Context),
             Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
      mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
      gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
      group_by(Context, Example, Implemented) %>% 
      ggplot(aes(x="", y=Count, fill=Group))+
      geom_bar(width = 1, stat = "identity")+ 
      #  scale_fill_discrete(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual')) +
      scale_fill_manual(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual'), values=c("aquamarine", "blue", "red","yellow"))+ 
      coord_polar("y", start=0) + 
      theme_void() + 
      theme(legend.position="left", strip.text.y = element_text(colour = 'white'))+
      labs(fill = '') +
      facet_grid(Example ~Context*Implemented) 
    #, strip.text.y = element_text(colour = 'white')
    
    gp1 <- ggplot_gtable(ggplot_build(Man_plot1))
    gp <- ggplot_gtable(ggplot_build(Man_plot))
    gp$widths  <- gp1$widths
    gp$heights <- gp1$heights
    gp$widths[10] <- gp1$widths[10] + gp1$widths[10]
    gp$widths[14] <- gp1$widths[14] + gp1$widths[14]*0.5
    #gp <- ggplotGrob(gp)
    #gtable_show_layout(gp)
    areas <- gtable_filter(gp, "strip-r", trim = F)$layout
    
    leg <- which(sapply(gp$grobs, function(x) x$name) == "guide-box")
    legend <- gp$grobs[[leg]] 
    gp0 <- gp 
    
    leg2 <- ggplot() + geom_blank(aes(c(0,rep(1,4)),1:5)) +  theme_void() +
      scale_y_continuous(expand = expand_scale())+
      scale_x_continuous(expand = expand_scale())+
      theme(plot.margin = unit(c(0,0,0,0), "lines")) +
      annotation_custom(
        grob = grid::textGrob(label = 'Adaptation\noptions:', just = 'left',gp = gpar(fontsize = 12, fontface = 'bold')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 4,      # Vertical position of the textGrob
        ymax = 5,
        xmin = 0,         # Note: The grobs are positioned outside the plot area
        xmax = 0) +
      annotation_custom(
        grob = grid::textGrob(label = 'Natural\nresource\nmgmt', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 3,      # Vertical position of the textGrob
        ymax = 4,
        xmin = 0,         # Note: The grobs are positioned outside the plot area
        xmax = 0) +
      annotation_custom(
        grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 2,      # Vertical position of the textGrob
        ymax = 3,
        xmin = 0,         # Note: The grobs are positioned outside the plot area
        xmax = 0)+
      annotation_custom(
        grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = 1.5,      # Vertical position of the textGrob
        ymax = 2.5,
        xmin = 0,         # Note: The grobs are positioned outside the plot area
        xmax = 0)
    
    for(i in 1:dim(areas)[1]){
      gp0 <- gtable_add_grob(x = gp0,
                             grobs = tableGrob(ant_labs$lab[i],
                                               theme = ttheme_minimal(base_colour = as.character(ant_labs$col[i]))),
                             t = areas$t[i], 
                             l = areas$l[i], 
                             b = areas$b[i], 
                             r = areas$r[i], 
                             name = paste0("new_strip-r-",i))
    }
    
    
    grid::grid.draw(gp0)
    
    pdf(paste0('country_figs/Man_plot_',country_list[[x]],'.pdf'), width = 6, height = 7)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(8, 6)))
    print(leg2, vp=viewport(layout.pos.row = 1:3, layout.pos.col = 1))
    #print(grid.draw(legend), vp=viewport(layout.pos.row = 5, layout.pos.col = 1))
    print(grid.draw(gp0), vp =viewport(layout.pos.row = 1:8, layout.pos.col = 2:6))
    dev.off()
    
    
  })


