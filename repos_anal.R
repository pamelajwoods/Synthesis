  library(tidyverse)
  library(grid)
  library(gtable)
  library(gridExtra)
  library(devtools)
  #install_github('fawda123/ggord')
  library(ggord)
  library(ca)
  
  #the noresearch option removes research and enforcement because they are so dominant to see what figures look like
  #the byregion option creates country-specific figures by broader region rather than country
  #country-specific figures ar not created when noresearch==TRUE
  
  #for(noresearch in c(FALSE)){
   noresearch <- FALSE 
  
  dat_raw <- 
    read_csv('Adaptation option repository_consolidated.csv') %>% 
    select(-c(grep('X', names(read_csv('Adaptation option repository_consolidated.csv')), value = T))) 
  
    #dat_raw %>%
    #group_by(Implemented) %>% 
    #summarise(n = n())
    
  #This takes care of recording inconsistencies
  dat <-
    dat_raw %>% 
    mutate(Implemented = ifelse(Implemented=='Y' | Implemented=='y' | Implemented=='Y?', 'Y',
                                ifelse(Implemented=='N' |Implemented=='n' | Implemented=='N?', 'N', NA)),
           Community = ifelse(Community=='Y' | Community=='y' | Community=='Y?', 'Y',
                              ifelse(Community=='N' |Community=='n' | Community=='N?', 'N', NA)),
           Context = ifelse(Context=='Y', 'CC', Context),
           Example = ifelse(Example=='Adaptation programme'| Example=='Adaptation programs?? Breeding program', 'Adaptation programs', 
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
                                                                                                                                                                        ifelse(Example=='Restrictions on discards (partial or full)'| Example=='Reduction in other stressors', 'Reductions in other stressors',
                                                                                                                                                                               ifelse(Example=='Review program/regulations' | Example=='Review programs', 'Review programs / regulations',
                                                                                                                                                                                      ifelse(Example=='Transition out of fishery' | Example=='Investments to help transition out of fisheries', 'Transition out of fisheries',
                                                                                                                                                                                             Example)))))))))))))))))))))))),
           Anticipatory = ifelse(Anticipatory %in% c('x', 'X'), 1, Anticipatory) %>% as.numeric(.),
           Responsive = ifelse(Responsive %in% c('x', 'X'), 1, Responsive) %>% as.numeric(.),
           Both = ifelse(Both %in% c('x', 'X'), 1, Both) %>% as.numeric(.),
           `Stock decline` = ifelse(is.na(`Stock decline`) | `Stock decline`=='none', NA, `Stock decline`),
           `Sp. distributional shifts` = ifelse(is.na(`Sp. distributional shifts`), NA, `Sp. distributional shifts`),
           `Ocean acidification` = ifelse(is.na(`Ocean acidification`), NA, `Ocean acidification`),
           `Extreme climatic events` = ifelse(is.na(`Extreme climatic events`), NA, `Extreme climatic events`),
           `Uncertainty (ecological)` = ifelse(is.na(`Uncertainty (ecological)`), NA, `Uncertainty (ecological)`),
           `Market changes` = ifelse(is.na(`Market changes`), NA, `Market changes`),
           `Regulation change` = ifelse(is.na(`Regulation change`), NA, `Regulation change`),
           `Consolidation` = ifelse(is.na(`Consolidation`), NA, `Consolidation`),
           `Globalization` = ifelse(is.na(`Globalization`), NA, `Globalization`),
           `Uncertainty (social)` = ifelse(is.na(`Uncertainty (social)`), NA, `Uncertainty (social)`),
           `Stock decline` = ifelse(`Stock decline`%in% c('x', 'X', '1'), 1, `Stock decline`),
           `Sp. distributional shifts` = ifelse(`Sp. distributional shifts` %in% c('x', 'X', '1'), 1, `Sp. distributional shifts`),
           `Ocean acidification` = ifelse(`Ocean acidification` %in% c('x', 'X', '1'), 1, `Ocean acidification`),
           `Extreme climatic events` = ifelse(`Extreme climatic events` %in% c('x', 'X', '1'), 1, `Extreme climatic events`),
           `Uncertainty (ecological)` = ifelse(`Uncertainty (ecological)` %in% c('x', 'X', '1'), 1, `Uncertainty (ecological)`),
           `Market changes` = ifelse(`Market changes` %in% c('x', 'X', '1'), 1, `Market changes`),
           `Regulation change` = ifelse(`Regulation change` %in% c('x', 'X', '1'), 1, `Regulation change`),
           `Consolidation` = ifelse(`Consolidation` %in% c('x', 'X', '1'), 1, `Consolidation`),
           `Globalization` = ifelse(`Globalization` %in% c('x', 'X', '1'), 1, `Globalization`),
           `Uncertainty (social)` = ifelse(`Uncertainty (social)` %in% c('x', 'X', '1'), 1, `Uncertainty (social)`),
           `Reduce stressor` = ifelse(`Reduce stressor` %in% c('x', 'X', '1'), 1, NA) %>% as.numeric(.),
           `Reduce sensitivity` = ifelse(`Reduce sensitivity` %in% c('x', 'X', '1'), 1, NA) %>% as.numeric(.),
           Cope = ifelse(Cope %in% c('x', 'X', '1'), 1, NA) %>% as.numeric(.),
           `No change` = ifelse(`No change` %in% c('x', 'X', '1'), 1, NA) %>% as.numeric(.),
           `Take advantage` = ifelse(`Take advantage` %in% c('x', 'X', '1'), 1, NA) %>% as.numeric(.),
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
                                                 #ifelse(Country=='FIN-AX', 'FIN',
                                                        ifelse(Country=='Baltic Sea', 'SW', 
                                                               ifelse(Country=='NL (Southwest Delta)', 'NL', 
                                                                      ifelse(Country == 'Italy', 'IT', 
                                                                             ifelse(Country == 'Sweden', 'SW',
                                                                                    ifelse(Country == 'USA-NE', 'US-NE',
                                                                                                  Country))))))))),
           Country = ifelse(Country=='USA-NW', 'US-NW',Country)
    ) %>% 
    filter(!(grepl('Pinnegar', Source) & Initials=='PJW')) %>% 
    mutate(Context=ifelse(grepl('Badjeck', Source), 'CC', Context))
           
  
  nr <-''
  if(noresearch){
    dat <- 
      dat %>% 
      filter(!(Example %in% c('Research', 'Enforcement')))
    nr <- '_noresearch'
  }
  
  
  #What are examples most often in response to? In CC vs not?
  CC_stressor_spread <- 
      dat %>% 
      mutate(stress_tot = apply(dat[5:14], 1, sum, na.rm = T), #total of rankings across stressor options 
             max_tot = apply(dat[5:14], 1, max, na.rm = T), #max of rankings across stressor options
             `Stock decline` = (max_tot + 1 - `Stock decline`)/stress_tot, #converted to a score ranging 0 -1, 
             #where rankings are represented as a proportion of total rankings indicated (number of rankings vary by line)
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
      summarise(`Stock decline` = round(sum(`Stock decline`, na.rm = T), 2),
                `Sp. distributional shifts` = round(sum(`Sp. distributional shifts`, na.rm = T), 2),
                `Ocean acidification` = round(sum(`Ocean acidification`, na.rm = T), 2),
                `Extreme climatic events` = round(sum(`Extreme climatic events`, na.rm = T), 2),
                `Uncertainty (ecological)` = round(sum(`Uncertainty (ecological)`, na.rm = T), 2),
                `Market changes` = round(sum(`Market changes`, na.rm = T), 2),
                `Regulation change` = round(sum(`Regulation change`, na.rm = T), 2),
                `Consolidation` = round(sum(`Consolidation`, na.rm = T), 2),
                `Globalization` = round(sum(`Globalization`, na.rm = T), 2),
                `Uncertainty (social)` = round(sum(`Uncertainty (social)`, na.rm = T), 2)
                ) %>% 
      mutate(tot = `Stock decline` +  `Sp. distributional shifts` + `Ocean acidification` + `Extreme climatic events` +
               `Uncertainty (ecological)` + `Market changes` + `Regulation change` + `Consolidation` + `Globalization` +`Uncertainty (social)`,
             `Total score` = round(tot,2),
             ) %>% 
    arrange(Context, desc(`Total score`)) %>% 
    ungroup() %>% 
    filter(!is.na(`Total score`)) %>% 
    mutate(order = n():1) %>% 
    select(order, Context, Example, `Total score`, `Uncertainty (ecological)`, `Ocean acidification`, `Uncertainty (social)`,`Sp. distributional shifts`, `Extreme climatic events`, `Stock decline`, `Market changes`,`Regulation change`, `Globalization`, `Consolidation`) 
  
  CC_stressor <-
    CC_stressor_spread %>% 
    # mutate(max_tot = apply(CC_stressor_spread[5:14], 1, max, na.rm = T),
    #        min_tot = apply(CC_stressor_spread[5:14], 1, min, na.rm = T),
    #        `Stock decline` = (`Stock decline` - min_tot+0.001)/(max_tot- min_tot+0.001),
    #        `Sp. distributional shifts` = (`Sp. distributional shifts`- min_tot+0.001)/(max_tot- min_tot+0.001),
    #        `Ocean acidification` = (`Ocean acidification`- min_tot+0.001)/(max_tot- min_tot+0.001),
    #        `Extreme climatic events` = (`Extreme climatic events`- min_tot+0.001)/(max_tot- min_tot+0.001),
    #        `Uncertainty (ecological)` = (`Uncertainty (ecological)`- min_tot+0.001)/(max_tot- min_tot+0.001),
    #        `Market changes` = (`Market changes`- min_tot+0.001)/(max_tot- min_tot+0.001),
    #        `Regulation change` = (`Regulation change`- min_tot+0.001)/(max_tot- min_tot+0.001),
    #        `Consolidation` = (`Consolidation`- min_tot+0.001)/(max_tot- min_tot+0.001),
    #        `Globalization` = (`Globalization`- min_tot+0.001)/(max_tot- min_tot+0.001),
    #        `Uncertainty (social)` = (`Uncertainty (social)`- min_tot)/(max_tot- min_tot+0.001)) %>% 
    # select(-max_tot, -min_tot) %>% 
    gather(val = 'Score', key = 'Stressor', -c(Context, Example, order)) %>% 
    mutate(Score = ifelse(Score==0, NA, Score))
  
  #View(CC_stressor_spread)
  
  br <- CC_stressor_spread %>% select(order) %>% unlist %>% c(.)
  lab <- CC_stressor_spread %>% select(Example) %>% unlist %>% substr(., 1, 49) 
  
  labs <- data.frame(x = -12.5, 
                     Context = CC_stressor_spread %>% select(Context) %>% unlist,
                     Example = CC_stressor_spread %>% select(Example) %>% unlist,
                     br, 
                     lab = ifelse(lab=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', lab), 
                     col = c('aquamarine3', 'aquamarine3', 'darkblue', 'darkgoldenrod3', 'darkblue', 'aquamarine3','darkblue','aquamarine3', 'aquamarine3', 'aquamarine3',  'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'aquamarine3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'aquamarine3', 'aquamarine3', 'darkgoldenrod3', 'darkblue', 'darkgoldenrod3', 'darkblue','darkgoldenrod3',  'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'aquamarine3', 'darkgoldenrod3', 'darkgoldenrod3',  'darkblue', 'darkgoldenrod3', 'aquamarine3', 'aquamarine3', 'darkgoldenrod3', 'darkblue', 'aquamarine3', 'aquamarine3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkgoldenrod3', 'darkblue', 'darkgoldenrod3', 'darkgoldenrod3')) %>% 
    mutate(col=ifelse(Example=='Fair trade laws' | Example=='International agreements', 'darkblue', 
                      ifelse(Example=='Education', 'darkgoldenrod3', col)))
  
  
  CC_stress_plot <-
    CC_stressor %>% 
    mutate(Stressor_name = Stressor, 
           Stressor = recode(Stressor,`Total score` = 'A', `Uncertainty (ecological)` = 'B', `Ocean acidification` = 'C', `Uncertainty (social)` = 'D', `Sp. distributional shifts` = 'E', `Extreme climatic events` = 'F', `Stock decline` = 'G', `Market changes` = 'H', `Regulation change` = 'I', `Globalization` = 'J', `Consolidation` = 'K')) %>% 
    filter(Stressor != 'A') %>%
    ggplot(aes(Stressor, order, size = Score, color = Context)) + 
    geom_hline(aes(yintercept = order), color = 'lightgrey', size = 0.1) +
    geom_point() + 
    theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
          plot.margin = unit(c(1,5,1,17), "lines")) +
    ylab('') + 
    #scale_size_continuous(trans = 'boxcox')+
    scale_x_discrete(breaks=c('B','C', 'D', 'E', 'F', 'G', 'H', 'I' , 'J', 'K'),
                                    labels=c("Uncertainty (ecological)", "Ocean acidification", "Uncertainty (social)",
                                             "Sp. distributional shifts", "Extreme climatic events", "Stock decline", "Market changes","Regulation change", "Globalization", "Consolidation"), 
                     position = 'top') +
    scale_y_continuous(breaks = c(0,57), labels = c('', ''), limits = c(0,57), expand = expand_scale()) + 
    scale_size(breaks = c(1, 5, 10, 20, 50 ,85), labels = as.character(c(1, 5, 10, 20, 50 ,85)))
  
  
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
      grob = grid::textGrob(label = 'Ecological', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
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
  
  pdf(paste0('Stressor_plot',nr,'.pdf'), width = 8.3, height = 11.7)
  
    gt <- ggplot_gtable(ggplot_build(CC_stress_plot))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid::grid.draw(gt)
  
  dev.off()
  
  #What are examples most often intended to do? In CC vs not?
  CC_goal_spread <- 
    dat %>% 
    mutate(stress_tot = apply(dat[30:34], 1, sum, na.rm = T), # total of goals indicated (sum across columns)
           max_tot = apply(dat[30:34], 1, max, na.rm = T), # max of goals indicated (is 1 for all across columns)
           `Reduce stressor` = (max_tot + 1 - `Reduce stressor`)/stress_tot, #converted to a score that ranges 0 - 1 and 
           #essentially similar to stressor except presences tied rankings of 1
           `Reduce sensitivity` = (max_tot + 1 - `Reduce sensitivity`)/stress_tot,
            Cope = (max_tot + 1 - Cope)/stress_tot,
           `No change` = (max_tot + 1 - `No change`)/stress_tot,
           `Take advantage` = (max_tot + 1 - `Take advantage`)/stress_tot) %>% 
    filter(!(is.na(Context) | is.na(Example) | stress_tot==0)) %>% 
    group_by(Context, Example) %>%
    summarise(`Reduce stressor` = round(sum(`Reduce stressor`, na.rm = T), 2),
              `Reduce sensitivity` = round(sum(`Reduce sensitivity`, na.rm = T), 2),
              Cope = round(sum(Cope, na.rm = T), 2),
              `No change` = round(sum(`No change`, na.rm = T), 2),
              `Take advantage` = round(sum(`Take advantage`, na.rm = T), 2)
    ) %>% 
    mutate(`Total score` = round(`Reduce stressor` +  `Reduce sensitivity` + Cope + `No change` + `Take advantage`, 2)) %>% 
    left_join(labs %>% select(order = br, Example, Context)) %>%  
    ungroup() %>% 
    filter(!is.na(`Total score`)) %>% 
    arrange(desc(order)) %>% 
    select(order, Context, Example, `Reduce stressor`, `Reduce sensitivity`, Cope, `No change`, `Take advantage`) 
  
  CC_goal <-
    CC_goal_spread %>% 
    gather(val = 'Score', key = 'Goal', -c(Context, Example, order)) %>% 
    mutate(Score = ifelse(Score==0, NA, Score))
  
  #View(CC_stressor_spread)
  
  #br <- CC_goal_spread %>% select(order) %>% unlist %>% c(.)
  #lab <- CC_goal_spread %>% select(Example) %>% unlist %>% substr(., 1, 49) 
  
  labsg <- #data.frame(x = -13.75, br, lab = ifelse(lab=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', lab)) %>% 
    #left_join(labs %>% select(lab, col) %>% distinct)
    labs %>% mutate(x = -13.75)
  
  CC_goal_plot <-
    CC_goal %>% 
    mutate(Goal_name = Goal, 
           Goal = recode(Goal, `Reduce stressor` = 'A', `Reduce sensitivity` = 'B', Cope = 'C', `No change` = 'D', `Take advantage` = 'E')) %>% 
    ggplot(aes(Goal, order, size = Score, color = Context)) + 
    geom_hline(aes(yintercept = order), color = 'lightgrey', size = 0.1) +
    geom_point() + 
    theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
          plot.margin = unit(c(1,5,1,17), "lines")) +
    ylab('') + 
    scale_x_discrete(breaks=c('A', 'B','C', 'D', 'E'),
                     labels=c("Reduce stressor", "Reduce sensitivity", "Cope",
                              "No change", "Take advantage"),
                     position = 'top') +
    scale_y_continuous(breaks = c(0,57), labels = c('', ''), limits = c(0,57), expand = expand_scale())+
    scale_size(breaks = c(1, 5, 10, 20, 50 ,85, 125), labels = as.character(c(1, 5, 10, 20, 50 ,85, 125)))
  
  
  
  for(i in 1:length(labs$br)){ 
    CC_goal_plot <-
      CC_goal_plot + 
      annotation_custom(
        grob = grid::textGrob(label = labsg$lab[i], just = 'left',gp = gpar(col = as.character(labsg$col[i]))), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
        ymin = labsg$br[i]+0.2,      # Vertical position of the textGrob
        ymax = labsg$br[i]-0.2,
        xmin = labsg$x[i],         # Note: The grobs are positioned outside the plot area
        xmax = labsg$x[i])
  }
  
  
  CC_goal_plot <-
    CC_goal_plot + 
    annotation_custom(
      grob = grid::textGrob(label = 'Adaptation options:', just = 'left',gp = gpar(fontsize = 12, fontface = 'bold')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
      ymin = 57,      # Vertical position of the textGrob
      ymax = 57,
      xmin = 5.75,         # Note: The grobs are positioned outside the plot area
      xmax = 5.75)+ 
    annotation_custom(
      grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
      ymin = 55,      # Vertical position of the textGrob
      ymax = 55,
      xmin = 5.75,         # Note: The grobs are positioned outside the plot area
      xmax = 5.75)+ 
    annotation_custom(
      grob = grid::textGrob(label = 'Ecological', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
      ymin = 56,      # Vertical position of the textGrob
      ymax = 56,
      xmin = 5.75,         # Note: The grobs are positioned outside the plot area
      xmax = 5.75)+ 
    annotation_custom(
      grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
      ymin = 54,      # Vertical position of the textGrob
      ymax = 54,
      xmin = 5.75,         # Note: The grobs are positioned outside the plot area
      xmax = 5.75)
  
  pdf(paste0('Goal_plot',nr,'.pdf'), width = 6.8, height = 11.7)
  
  gt <- ggplot_gtable(ggplot_build(CC_goal_plot))
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
    mutate(Prop_Example = round(N_Example/total, 2)) %>% 
    arrange(Context, Community, Implemented, desc(Prop_Example)) %>% 
    mutate(id = 1:n()) %>% 
    filter(id < 11) %>% 
    ungroup %>% 
    rename(`Proportion adaptation options` = Prop_Example) %>% 
    mutate(#Context = ifelse(Context=='CC', 'Climate change context', 'Non-climate-change context'),
           Community = ifelse(Community == 'Y', 'Community focused', 'Not community focused'),
           Implemented = ifelse(Implemented == 'Y', 'Implemented', 'Not implemented'),
           Example = ifelse(grepl('Financial assistance or investment for entering,', Example), 'Financial assistance or investment for entering,', Example))
    
  write_csv(CC_ex %>% 
              unite(Attributes, Context, Community, Implemented) %>% 
              select(-c(N_Example, total, id)) %>% 
              spread(key = Attributes, value = `Proportion adaptation options`), 'CC_ex.csv')
  
  CC_ex_plot <-
    CC_ex  %>% 
    ggplot(aes(x = 1, y = 11 - id, label = Example, size = `Proportion adaptation options`)) + 
    geom_text()+
    theme_classic()+ 
    theme(panel.border = element_rect(fill = NA), axis.text.y = element_blank(),axis.text.x = element_blank(), axis.ticks = element_blank())+
    xlab('')+ylab('')+
    labs(size="Proportion\nadaptation\noptions") +
    scale_y_continuous(expand = expand_scale(), limits = c(0,11))+
    scale_size(breaks = c(0.05, 0.1, 0.2, 0.3), labels = as.character(c(0.05, 0.1, 0.2, 0.3)))+
    facet_grid(Community*Implemented ~ Context) 
  
  g <- ggplot_gtable(ggplot_build(CC_ex_plot))
  strip_both <- which(grepl('strip-', g$layout$name))
  fills <- c("green","gold","white","gold", "green","white","white","white")
  k <- 1
  for (i in strip_both[3:6]) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    jj <- which(grepl('rect', g$grobs[[i]]$grobs[[2]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    g$grobs[[i]]$grobs[[2]]$children[[jj]]$gp$fill <- fills[k+1]
    k <- k+2
  }
  fills <- c('#F8766D', '#00BFC4')
  k <- 1
  for (i in strip_both[1:2]) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  pdf(paste0('CC_ex',nr,'.pdf'), width = 6.6, height = 6.2)
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
  
  CC_ant %>% 
    select(Context, Example, Implemented, Prop_Anticipatory, Prop_Responsive, Prop_Both) %>%
    #  rename(Anticipatory = Prop_Anticipatory, Responsive = Prop_Responsive, Both = Prop_Both) %>% 
    mutate(Anticipatory = round(Prop_Anticipatory,2), `Resp. or Both` = Prop_Responsive + Prop_Both,
           Implemented = ifelse(Implemented == 'Y', 'Implemented', 'Not implemented')
    ) %>% 
    select(-c(Prop_Anticipatory, Prop_Responsive, Prop_Both, `Resp. or Both`)) %>%
    unite(Attributes, Context, Implemented) %>% 
    spread(key = Attributes, value = Anticipatory) %>% 
    write_csv('CC_ant.csv')
  
  
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
  #  rename(Anticipatory = Prop_Anticipatory, Responsive = Prop_Responsive, Both = Prop_Both) %>% 
    mutate(Anticipatory = Prop_Anticipatory, `Resp. or Both` = Prop_Responsive + Prop_Both) %>% 
    select(-c(Prop_Anticipatory, Prop_Responsive, Prop_Both)) %>% 
    ungroup() %>% 
    mutate(Example = substr(Example, 1, 49),
           Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
           #Context = ifelse(Context=='N', 'F', Context),
           Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
    mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
    gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
    group_by(Context, Example, Implemented) %>% 
    ggplot(aes(x="", y=Count, fill=Group))+
    geom_bar(width = 1, stat = "identity", colour = "darkgrey", size = 0.1)+ 
    #scale_fill_manual(values=c("aquamarine", "blue", "red"))+ 
    scale_fill_manual(values=c("darkviolet", "darkolivegreen1"))+ 
    coord_polar("y", start=0) + 
    theme_void() + 
    theme(legend.position="left", plot.margin=unit(c(0,0,0,0.1),"cm"))+
    labs(fill = '') +
    facet_grid(Example ~Context*Implemented) 
  
  Ant_plot <-
    CC_ant %>% 
    select(Context, Example, Implemented, Prop_Anticipatory, Prop_Responsive, Prop_Both) %>%
    #  rename(Anticipatory = Prop_Anticipatory, Responsive = Prop_Responsive, Both = Prop_Both) %>% 
    mutate(Anticipatory = Prop_Anticipatory, `Resp. or Both` = Prop_Responsive + Prop_Both) %>% 
    select(-c(Prop_Anticipatory, Prop_Responsive, Prop_Both)) %>% 
    ungroup() %>% 
    mutate(Example = substr(Example, 1, 49),
           Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
           #Context = ifelse(Context=='N', 'F', Context),
           Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
    mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
    gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
    group_by(Context, Example, Implemented) %>% 
    ggplot(aes(x="", y=Count, fill=Group))+
    geom_bar(width = 1, stat = "identity", colour = "darkgrey", size = 0.1)+ 
    #scale_fill_manual(values=c("aquamarine", "blue", "red"))+ 
    scale_fill_manual(values=c("darkviolet", "darkolivegreen1"))+ 
    coord_polar("y", start=0) + 
    theme_void() + 
    theme(legend.position="left", strip.text.y = element_text(colour = 'white'), plot.margin=unit(c(0,0,0,0.1),"cm"))+
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
      xmin = 0.05,         # Note: The grobs are positioned outside the plot area
      xmax = 0.05) +
    annotation_custom(
      grob = grid::textGrob(label = 'Ecological', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
      ymin = 2.5,      # Vertical position of the textGrob
      ymax = 3.5,
      xmin = 0.05,         # Note: The grobs are positioned outside the plot area
      xmax = 0.05) +
    annotation_custom(
      grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
      ymin = 2,      # Vertical position of the textGrob
      ymax = 3,
      xmin = 0.05,         # Note: The grobs are positioned outside the plot area
      xmax = 0.05)+
    annotation_custom(
      grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
      ymin = 1.5,      # Vertical position of the textGrob
      ymax = 2.5,
      xmin = 0.05,         # Note: The grobs are positioned outside the plot area
      xmax = 0.05)
  
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
  
  pdf(paste0('Ant_plot_',nr,'.pdf'), width = 6.5, height = 7)
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
    mutate(Prop_top = round((Internat. + `Nat. govt.` + `Region. govt.`)/all,3),
           Prop_bottom = round((`Local govt.` + `Community assoc.` + `Business coop.`)/all,3),
           Prop_NGO = round((NGO + Uni.)/all,3),
           Prop_ind = round((Business + Individual)/all,3)) %>% 
    arrange(id)
  #filter(id < 11)
  
  write_csv(CC_man %>% 
              select(Example, Context, Implemented, 
                     `Proportion Top-down` = Prop_top, 
                     `Proportion Bottom-up` = Prop_bottom, 
                     `Proportion NGO` = Prop_NGO,
                     `Proportion Individual` = Prop_ind) %>% 
              arrange(Example, Context, Implemented), 'CC_man.csv')
  
  
  Man_plot1 <-
    CC_man %>% 
    select(Context, Example, Implemented, Prop_top, Prop_bottom, Prop_NGO, Prop_ind) %>%
    rename(`Top-down` = Prop_top, `Bottom-up` = Prop_bottom, `Non-profit` = Prop_NGO, Individual = Prop_ind ) %>% 
    ungroup() %>% 
    mutate(Example = substr(Example, 1, 49),
           Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
           #Context = ifelse(Context=='N', 'F', Context),
           Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
    mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
    gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
    group_by(Context, Example, Implemented) %>% 
    ggplot(aes(x="", y=Count, fill=Group))+
    geom_bar(width = 1, stat = "identity", colour = "darkgrey", size = 0.1) +
  #  scale_fill_discrete(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual')) +
    scale_fill_manual(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual'), values=c("blue", "red", "darkorange","yellow"))+ 
    coord_polar("y", start=0) + 
    theme_void() + 
    theme(legend.position="left", plot.margin=unit(c(0.1,0,0.1,0.1),"cm"))+
    labs(fill = '') +
    facet_grid(Example ~Context*Implemented) 
  
    Man_plot <-
    CC_man %>% 
    select(Context, Example, Implemented, Prop_top, Prop_bottom, Prop_NGO, Prop_ind) %>%
    rename(`Top-down` = Prop_top, `Bottom-up` = Prop_bottom, `Non-profit` = Prop_NGO, Individual = Prop_ind ) %>% 
    ungroup() %>% 
    mutate(Example = substr(Example, 1, 49),
           Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
           #Context = ifelse(Context=='N', 'F', Context),
           Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
    mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
    gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
    group_by(Context, Example, Implemented) %>% 
    ggplot(aes(x="", y=Count, fill=Group))+
    geom_bar(width = 1, stat = "identity", colour = "darkgrey", size = 0.1)+ 
    #  scale_fill_discrete(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual')) +
    scale_fill_manual(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual'), values=c("blue", "red", "darkorange","yellow"))+ #"aquamarine"
    coord_polar("y", start=0) + 
    theme_void() + 
    theme(legend.position="left", strip.text.y = element_text(colour = 'white'), plot.margin=unit(c(0.1,0,0.1,0.1),"cm"))+
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
      xmin = 0.05,         # Note: The grobs are positioned outside the plot area
      xmax = 0.05) +
    annotation_custom(
      grob = grid::textGrob(label = 'Ecological', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
      ymin = 2.5,      # Vertical position of the textGrob
      ymax = 3.5,
      xmin = 0.05,         # Note: The grobs are positioned outside the plot area
      xmax = 0.05) +
    annotation_custom(
      grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
      ymin = 2,      # Vertical position of the textGrob
      ymax = 3,
      xmin = 0.05,         # Note: The grobs are positioned outside the plot area
      xmax = 0.05)+
    annotation_custom(
      grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
      ymin = 1.5,      # Vertical position of the textGrob
      ymax = 2.5,
      xmin = 0.05,         # Note: The grobs are positioned outside the plot area
      xmax = 0.05)
  
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
  
  pdf(paste0('Man_plot',nr,'.pdf'), width = 6, height = 7)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(8, 6)))
  print(leg2, vp=viewport(layout.pos.row = 1:3, layout.pos.col = 1))
  #print(grid.draw(legend), vp=viewport(layout.pos.row = 5, layout.pos.col = 1))
  print(grid.draw(gp0), vp =viewport(layout.pos.row = 1:8, layout.pos.col = 2:6))
  dev.off()
  
  #Frequency tables
  freq <-
    dat %>% 
    ungroup() %>% 
    group_by(Country, Context, Implemented, Community) %>% 
    count() %>% 
    filter(!is.na(Context),!is.na(Implemented), !is.na(Community)) %>% 
    mutate(Community = ifelse(Community == 'Y', 'Community focused', 'Not community focused'),
           Implemented = ifelse(Implemented == 'Y', 'Implemented', 'Not implemented'),
    ) %>%   
    unite(Attributes, Context, Implemented, Community) %>% 
    spread(key = Attributes, value = n)
    #table(.) %>% 
  write.csv(freq,'Freq_table.csv')
  
  
  #What are some differences among countries in examples, managers, and responsiveness?
  
  #ADD CA
  
  #prcomp([,c(1:7,10,11)], center = TRUE,scale. = TRUE)
  #install.packages('ca')
  
  
  dat_ca_tmp <-
    dat %>% 
    ungroup() %>% 
    mutate(Region = ifelse(Country %in% c('USA', 'US-SE', 'US-SW', 'US-NE', 'US-NW', 'US-AK', 'US-HA', 'US','CAN'), 'NAM', 
                           ifelse(Country %in% c('AUS', 'NZ'), 'SP',
                                  ifelse(Country %in% c('ICE', 'FIN', 'FIN-AX', 'FAROES', 'SW','NOR'), 'NEU',
                                         ifelse(Country %in% c('UK', 'IT', 'EU', 'GER','NL'), 'SEU', Country))))) %>%
    mutate(Country = ifelse(Country %in% c('USA', 'US-SE', 'US-SW', 'US-NE', 'US-NW', 'US-AK', 'US-HA', 'US'), 'USA', 
                            ifelse(Country=='FAROES', 'FAR', Country))) %>% 
                                   group_by(Region, Country, Context, Community, Example) %>% 
    count() %>%
    filter(!is.na(Community), !is.na(Country), !is.na(Context), !is.na(Example), Region!='INT') 
  
  abbr <-
    dat_ca_tmp %>% 
    ungroup %>% 
    select(Example) %>% 
    distinct %>% 
    mutate(AO = c('ADA', 'ORG', 'DIV', 'ENF', 'IAG','NEW','VAL','MAR', 'PLQ', 'RED', 'RES','RET','REV', 'OUT', 'EDU', 'CRI','COO','ECD','ENT','SUR','TRA','INS', 'IRI', 'DRM','DOM', 'MPAs','DFU', 'FAI')) %>% 
    left_join(labs %>% 
                ungroup %>% 
                select(Example, col) %>% 
                distinct %>% 
                mutate(Type = ifelse(col=='aquamarine3', 'Ecological', 
                                     ifelse(col=='darkgoldenrod3', 'Social',
                                            ifelse(col=='darkblue', 'Insitutional',NA)))))
  
  dat_ca <-
    dat_ca_tmp %>% 
    left_join(abbr) %>% 
    ungroup %>% 
    select(-c(Example)) %>% 
    group_by(Region, Country, Context, Community, col, Type) %>% 
    summarise(n = sum(n, na.rm = T)) 
  
  Country_Key<-
    dat_ca %>% 
    ungroup() %>% 
    select(Region, Country) %>% 
    distinct() %>% 
    mutate(ID = c('C', 'U', 'R', 'F', 'X', 'I', 'O', 'S', 'E', 'G', 'T', 'N', 'K', 'A', 'Z'))
  
  
  dat_ca1<-
    dat_ca %>% 
    filter(Context=='CC', Community=='Y') %>% 
    spread(key = Type, value = n, fill = 0) %>% 
    ungroup() %>% 
    group_by(Region, Country, Context, Community) %>% 
    summarise(Institutional = sum(Insitutional), 
              Ecological = sum(Ecological),
              Social = sum(Social)) %>% 
    ungroup #%>% 
    #left_join(Country_Key) %>% 
    #mutate(Abbrev = Country, Country = ID) %>% select(-ID)
  
  ord_cc_comm <- ca(dat_ca1 %>% 
                      column_to_rownames(var = "Country") %>% 
                      select(-c(Region, Context, Community)))
  
  dat_ca2 <-
    dat_ca %>% 
    filter(Context=='N', Community=='Y') %>% 
    spread(key = Type, value = n, fill = 0) %>% 
    group_by(Region, Country, Context, Community) %>% 
    summarise(Institutional = sum(Insitutional), 
              Ecological = sum(Ecological),
              Social = sum(Social))%>% 
    ungroup #%>% 
    #left_join(Country_Key) %>% 
    #mutate(Abbrev = Country, Country = ID) %>% select(-ID)
  
  ord_f_comm <- ca(dat_ca2 %>% 
                     column_to_rownames(var = "Country") %>% 
                     select(-c(Region, Context, Community)))
  dat_ca3 <-
    dat_ca %>% 
    filter(Context=='CC', Community=='N') %>% 
    spread(key = Type, value = n, fill = 0) %>% 
    group_by(Region, Country, Context, Community) %>% 
    summarise(Institutional = sum(Insitutional), 
              Ecological = sum(Ecological),
              Social = sum(Social))%>% 
    ungroup #%>% 
    #left_join(Country_Key) %>% 
    #mutate(Abbrev = Country, Country = ID) %>% select(-ID)
  ord_cc_ncomm <- ca(dat_ca3 %>% 
                       column_to_rownames(var = "Country") %>% 
                       select(-c(Region, Context, Community)))
  dat_ca4 <-
    dat_ca %>% 
    filter(Context=='N', Community=='N') %>% 
    spread(key = Type, value = n, fill = 0) %>% 
    group_by(Region, Country, Context, Community) %>% 
    summarise(Institutional = sum(Insitutional), 
              Ecological = sum(Ecological),
              Social = sum(Social))%>% 
    ungroup #%>% 
    #left_join(Country_Key) %>% 
    #mutate(Abbrev = Country, Country = ID) %>% select(-ID)
  
  ord_f_ncomm <- ca(dat_ca4 %>% 
                      column_to_rownames(var = "Country") %>% 
                      select(-c(Region, Context, Community)))
  
  xlims <- c(-1.5,1.5)
  ylims <- c(-1.5,1.5)
  sc <- 0.8
  
  ord_cc_comm$rowcoord[,1] <- ord_cc_comm$rowcoord[,1]*-1
  ord_cc_comm$colcoord[,1] <- ord_cc_comm$colcoord[,1]*-1
  
  ord_cc_comm$rowcoord[5,]<-  ord_cc_comm$rowcoord[5,]*1.05
  ord_cc_comm$rowcoord[3,]<-  ord_cc_comm$rowcoord[3,]*0.95
  ord_cc_comm$rowcoord[6,2]<-  ord_cc_comm$rowcoord[6,2]*1.1
  ord_cc_comm$rowcoord[8,2]<-  ord_cc_comm$rowcoord[8,2]*0.9
  
  pdf(paste0('Ord_CC_Comm_',nr,'.pdf'), width = 5*sc, height = 5*sc)
    ggord(ord_cc_comm, dat_ca1$Region, cols = c('#F8766D', '#00BFC4', 'darkblue', '#C77CFF'), ellipse = FALSE, obslab = TRUE, #'#7CAE00',
          xlims = xlims,
          ylims = ylims)+ 
      theme(legend.position = 'none')
  dev.off()
  
  
  ord_f_comm$rowcoord[3,] <-   ord_f_comm$rowcoord[3,]*1.075
  ord_f_comm$rowcoord[8,1] <-   ord_f_comm$rowcoord[8,1]*0.925
  ord_f_comm$rowcoord[8,2] <-   ord_f_comm$rowcoord[8,2]*1.1
  ord_f_comm$rowcoord[7,2] <-   ord_f_comm$rowcoord[7,2]*0.8
  pdf(paste0('Ord_F_Comm_',nr,'.pdf'), width = 5*sc, height = 5*sc)
    ggord(ord_f_comm, dat_ca2$Region, cols = c('#F8766D', '#00BFC4', 'darkblue', '#C77CFF'), ellipse = FALSE, obslab = TRUE,
          xlims = xlims,
          ylims = ylims)+ 
      theme(legend.position = 'none')
  dev.off()
  
  ord_cc_ncomm$rowcoord[8,2] <-   ord_cc_ncomm$rowcoord[8,2]*0.8
  ord_cc_ncomm$rowcoord[3,2] <-   ord_cc_ncomm$rowcoord[3,2]*1.2
  ord_cc_ncomm$rowcoord[1,2] <-   ord_cc_ncomm$rowcoord[1,2]*1.2
  ord_cc_ncomm$rowcoord[5,2] <-   ord_cc_ncomm$rowcoord[5,2]*0.9
  ord_cc_ncomm$rowcoord[4,1] <-   ord_cc_ncomm$rowcoord[4,1]*0.9
  ord_cc_ncomm$rowcoord[10,1] <-   ord_cc_ncomm$rowcoord[10,1]*1.55
  ord_cc_ncomm$rowcoord[10,2] <-   ord_cc_ncomm$rowcoord[10,2]*2
  ord_cc_ncomm$rowcoord[,2] <- ord_cc_ncomm$rowcoord[,2]*-1
  ord_cc_ncomm$colcoord[,2] <- ord_cc_ncomm$colcoord[,2]*-1
  pdf(paste0('Ord_CC_Ncomm_',nr,'.pdf'), width = 5*sc, height = 5*sc)
    ggord(ord_cc_ncomm, dat_ca3$Region, cols = c('#F8766D', '#00BFC4', 'darkblue', '#C77CFF'), ellipse = FALSE, obslab = TRUE,
          xlims = xlims,
          ylims = ylims)+ 
      theme(legend.position = 'none')
  dev.off()
  
  
  ord_f_ncomm$rowcoord[1,] <-   ord_f_ncomm$rowcoord[1,]*0.9
  ord_f_ncomm$rowcoord[9,] <-   ord_f_ncomm$rowcoord[9,]*1.1
  ord_f_ncomm$rowcoord[3,] <-   ord_f_ncomm$rowcoord[3,]*0.9
  ord_f_ncomm$rowcoord[11,] <-   ord_f_ncomm$rowcoord[11,]*1.1
  
  ord_f_ncomm$rowcoord[,1] <- ord_f_ncomm$rowcoord[,1]*-1
  ord_f_ncomm$colcoord[,1] <- ord_f_ncomm$colcoord[,1]*-1
  
  ord_f_ncomm$rowcoord[,2] <- ord_f_ncomm$rowcoord[,2]*-1
  ord_f_ncomm$colcoord[,2] <- ord_f_ncomm$colcoord[,2]*-1
  
  pdf(paste0('Ord_F_Ncomm_',nr,'.pdf'), width = 5*sc, height = 5*sc)
    ggord(ord_f_ncomm, dat_ca4$Region, cols = c('#F8766D', '#00BFC4', 'darkblue', '#C77CFF'), ellipse = FALSE, obslab = TRUE,
          xlims = xlims,
          ylims = ylims)+ 
      theme(legend.position = 'none')
  dev.off()
  
  pdf(paste0('Ord_F_Ncomm_leg',nr,'.pdf'), width = 5.85*sc, height = 5*sc)
    ggord(ord_f_ncomm, dat_ca4$Region, cols = c('#F8766D', '#00BFC4', 'darkblue', '#C77CFF'), ellipse = FALSE, obslab = TRUE,
          xlims = xlims,
          ylims = ylims) 
  dev.off()
  
  
  #NOW ADD FILTERS THROUGHOUT AND JOIN TO INTRODUCE NAs
  if(noresearch==FALSE){
  
    for(byregion in c(FALSE,TRUE)){
  
      if(byregion){
        dat <-
          dat %>% 
          mutate(region = ifelse(Country %in% c('USA', 'US-SE', 'US-SW', 'US-NE', 'US-NW', 'US-AK', 'US-HA', 'US','CAN'), 'NAM', 
                                 ifelse(Country %in% c('AUS', 'NZ'), 'SP',
                                        ifelse(Country %in% c('ICE', 'FIN', 'FIN-AX', 'FAROES', 'SW','NOR'), 'NEU',
                                               ifelse(Country %in% c('UK', 'IT', 'EU', 'GER','NL'), 'SEU', Country))))) %>%
          mutate(Country = region) %>% 
          select(-region)
        
      }
      
      allcountries <- dat %>% select(Country) %>% unique %>% unlist
      country_list <- NULL
      country_list[[1]]<-c(allcountries)
      for(i in (length(allcountries)+1):2){
        country_list[[i]] <- c(allcountries[i-1])
      }
      
      #if(!byregion){country_list <- country_list[-1]}
      
      country_list <-
        set_names(country_list[-1], allcountries)
      #set_names(country_list[1], 'all')
      
      
      
      #country_list[-1] %>% 
      #  map(function(x){
      
      for( i in 1:length(country_list)){
        #x <- country_list[[i]]
        x <- i
        print(country_list[[i]])
        
        try(
          {
            dat2 <- 
              dat %>% 
              filter(Country %in% country_list[[x]]) 
            
            
            #What are examples most often in response to? In CC vs not?
            CC_stressor_spread_c <- 
              dat2 %>% 
              mutate(stress_tot = apply(dat2[5:14], 1, sum, na.rm = T),
                     max_tot = apply(dat2[5:14], 1, max, na.rm = T),
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
              summarise(`Stock decline` = round(sum(`Stock decline`, na.rm = T),2),
                        `Sp. distributional shifts` = round(sum(`Sp. distributional shifts`, na.rm = T),2),
                        `Ocean acidification` = round(sum(`Ocean acidification`, na.rm = T),2),
                        `Extreme climatic events` = round(sum(`Extreme climatic events`, na.rm = T),2),
                        `Uncertainty (ecological)` = round(sum(`Uncertainty (ecological)`, na.rm = T),2),
                        `Market changes` = round(sum(`Market changes`, na.rm = T),2),
                        `Regulation change` = round(sum(`Regulation change`, na.rm = T),2),
                        `Consolidation` = round(sum(`Consolidation`, na.rm = T),2),
                        `Globalization` = round(sum(`Globalization`, na.rm = T),2),
                        `Uncertainty (social)` = round(sum(`Uncertainty (social)`, na.rm = T),2)
              ) %>% 
              mutate(tot = `Stock decline` +  `Sp. distributional shifts` + `Ocean acidification` + `Extreme climatic events` +
                       `Uncertainty (ecological)` + `Market changes` + `Regulation change` + `Consolidation` + `Globalization` +`Uncertainty (social)`,
                     `Total score` = round(tot,2),
              ) %>% 
              arrange(Context, desc(`Total score`)) %>% 
              ungroup() %>% 
              filter(!is.na(`Total score`)) %>% 
              mutate(order = n():1) %>% 
              select(order, Context, Example, `Total score`, `Uncertainty (ecological)`, `Ocean acidification`, `Uncertainty (social)`,`Sp. distributional shifts`, `Extreme climatic events`, `Stock decline`, `Market changes`,`Regulation change`, `Globalization`, `Consolidation`) 
            
            CC_stressor_c <-
              CC_stressor_spread_c %>% 
              # mutate(max_tot = apply(CC_stressor_spread[5:14], 1, max, na.rm = T),
              #        min_tot = apply(CC_stressor_spread[5:14], 1, min, na.rm = T),
              #        `Stock decline` = (`Stock decline` - min_tot+0.001)/(max_tot- min_tot+0.001),
              #        `Sp. distributional shifts` = (`Sp. distributional shifts`- min_tot+0.001)/(max_tot- min_tot+0.001),
              #        `Ocean acidification` = (`Ocean acidification`- min_tot+0.001)/(max_tot- min_tot+0.001),
              #        `Extreme climatic events` = (`Extreme climatic events`- min_tot+0.001)/(max_tot- min_tot+0.001),
              #        `Uncertainty (ecological)` = (`Uncertainty (ecological)`- min_tot+0.001)/(max_tot- min_tot+0.001),
              #        `Market changes` = (`Market changes`- min_tot+0.001)/(max_tot- min_tot+0.001),
              #        `Regulation change` = (`Regulation change`- min_tot+0.001)/(max_tot- min_tot+0.001),
              #        `Consolidation` = (`Consolidation`- min_tot+0.001)/(max_tot- min_tot+0.001),
              #        `Globalization` = (`Globalization`- min_tot+0.001)/(max_tot- min_tot+0.001),
            #        `Uncertainty (social)` = (`Uncertainty (social)`- min_tot)/(max_tot- min_tot+0.001)) %>% 
            # select(-max_tot, -min_tot) %>% 
            gather(val = 'Score', key = 'Stressor', -c(Context, Example, order)) %>% 
              mutate(Score = ifelse(Score==0, NA, Score))
            
            #View(CC_stressor_spread)
            
            br <- CC_stressor_spread_c %>% select(order) %>% unlist %>% c(.)
            lab <- CC_stressor_spread_c %>% select(Example) %>% unlist %>% substr(., 1, 49) 
            
            labsc <- data.frame(x = -12.5, 
                                br, 
                                lab = ifelse(lab=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', lab),
                                Example = CC_stressor_spread_c %>% select(Example) %>% unlist,
                                Context = CC_stressor_spread_c %>% select(Context) %>% unlist) %>% 
              left_join(labs %>% select(lab, col, Example, Context) %>% distinct)
            
            CC_stress_plot <-
              CC_stressor_c %>% 
              mutate(Stressor_name = Stressor, 
                     Stressor = recode(Stressor,`Total score` = 'A', `Uncertainty (ecological)` = 'B', `Ocean acidification` = 'C', `Uncertainty (social)` = 'D', `Sp. distributional shifts` = 'E', `Extreme climatic events` = 'F', `Stock decline` = 'G', `Market changes` = 'H', `Regulation change` = 'I', `Globalization` = 'J', `Consolidation` = 'K')) %>% 
              filter(Stressor != 'A') %>% 
              ggplot(aes(Stressor, order, size = Score, color = Context)) + 
              geom_hline(aes(yintercept = order), color = 'lightgrey', size = 0.1) +
              geom_point() + 
              theme_light() + 
              theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
                    plot.margin = unit(c(1,5,1,17.5), "lines")) +
              ylab('') + 
              #scale_size_continuous(trans = 'boxcox')+
              scale_x_discrete(breaks=c('B','C', 'D', 'E', 'F', 'G', 'H', 'I' , 'J', 'K'),
                               labels=c("Uncertainty (ecological)", "Ocean acidification", "Uncertainty (social)",
                                        "Sp. distributional shifts", "Extreme climatic events", "Stock decline", "Market changes","Regulation change", "Globalization", "Consolidation"), 
                               position = 'top') +
              scale_y_continuous(breaks = NULL, labels = c('', ''), limits = c(0,max(CC_stressor_c$order)+1), expand = expand_scale())+
              scale_size(breaks = c(1, 5, 10, 20, 50 ,85), labels = as.character(c(1, 5, 10, 20, 50 ,85)))
            
            #c(0, max(CC_stressor_c$order)+1)
            
            for(i in 1:length(labsc$br)){ 
              CC_stress_plot <-
                CC_stress_plot + 
                annotation_custom(
                  grob = grid::textGrob(label = labsc$lab[i], just = 'left',gp = gpar(col = as.character(labsc$col[i]))), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                  ymin = labsc$br[i]+0.2,      # Vertical position of the textGrob
                  ymax = labsc$br[i]-0.2,
                  xmin = labsc$x[i],         # Note: The grobs are positioned outside the plot area
                  xmax = labsc$x[i])
            }
            
            
            CC_stress_plot <-
              CC_stress_plot + 
              annotation_custom(
                grob = grid::textGrob(label = 'Adaptation options:', just = 'left',gp = gpar(fontsize = 12, fontface = 'bold')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = max(CC_stressor_c$order)+1,      # Vertical position of the textGrob
                ymax = max(CC_stressor_c$order)+1,
                xmin = 11,         # Note: The grobs are positioned outside the plot area
                xmax = 11)+ 
              annotation_custom(
                grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = (max(CC_stressor_c$order)+1)*13/15,      # Vertical position of the textGrob
                ymax = (max(CC_stressor_c$order)+1)*13/15,
                xmin = 11,         #   Note: The grobs are positioned outside the plot area
                xmax = 11)+ 
              annotation_custom(
                grob = grid::textGrob(label = 'Ecological', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = (max(CC_stressor_c$order)+1)*14/15,      # Vertical position of the textGrob
                ymax = (max(CC_stressor_c$order)+1)*14/15,
                xmin = 11,         # Note: The grobs are positioned outside the plot area
                xmax = 11)+ 
              annotation_custom(
                grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = (max(CC_stressor_c$order)+1)*12/15,      # Vertical position of the textGrob
                ymax = (max(CC_stressor_c$order)+1)*12/15,
                xmin = 11,         # Note: The grobs are positioned outside the plot area
                xmax = 11)
            
            pdf(paste0('country_figs/Stressor_plot_', country_list[[x]],nr,'.pdf'), width = 8.3, height = 11.7)
            
            gt <- ggplot_gtable(ggplot_build(CC_stress_plot))
            gt$layout$clip[gt$layout$name == "panel"] <- "off"
            grid::grid.draw(gt)
            
            dev.off()
            
            
            #What are examples most often intended to do? In CC vs not?
            CC_goal_spread_c <- 
              dat2 %>% 
              mutate(stress_tot = apply(dat2[30:34], 1, sum, na.rm = T),
                     max_tot = apply(dat2[30:34], 1, max, na.rm = T),
                     `Reduce stressor` = (max_tot + 1 - `Reduce stressor`)/stress_tot,
                     `Reduce sensitivity` = (max_tot + 1 - `Reduce sensitivity`)/stress_tot,
                     Cope = (max_tot + 1 - Cope)/stress_tot,
                     `No change` = (max_tot + 1 - `No change`)/stress_tot,
                     `Take advantage` = (max_tot + 1 - `Take advantage`)/stress_tot) %>% 
              filter(!(is.na(Context) | is.na(Example) | stress_tot==0)) %>% 
              group_by(Context, Example) %>%
              summarise(`Reduce stressor` = round(sum(`Reduce stressor`, na.rm = T),2),
                        `Reduce sensitivity` = round(sum(`Reduce sensitivity`, na.rm = T),2),
                        Cope = round(sum(Cope, na.rm = T),2),
                        `No change` = round(sum(`No change`, na.rm = T),2),
                        `Take advantage` = round(sum(`Take advantage`, na.rm = T),2)
              ) %>% 
              mutate(`Total score` = round(`Reduce stressor` +  `Reduce sensitivity` + Cope + `No change` + `Take advantage`,2)) %>% 
              left_join(labsc %>% select(order = br, Example, Context)) %>%  
              ungroup() %>% 
              filter(!is.na(`Total score`)) %>% 
              arrange(desc(order)) %>% 
              select(order, Context, Example, `Reduce stressor`, `Reduce sensitivity`, Cope, `No change`, `Take advantage`) 
            
            CC_goal_c <-
              CC_goal_spread_c %>% 
              gather(val = 'Score', key = 'Goal', -c(Context, Example, order)) %>% 
              mutate(Score = ifelse(Score==0, NA, Score))
            
            #View(CC_stressor_spread)
            
            br <- CC_goal_spread_c %>% select(order) %>% unlist %>% c(.)
            lab <- CC_goal_spread_c %>% select(Example) %>% unlist %>% substr(., 1, 49) 
            
            labsg <- #data.frame(x = -13.75, br, lab = ifelse(lab=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', lab)) %>% 
              #left_join(labs %>% select(lab, col) %>% distinct)
              labsc %>% mutate(x = -13.75)
            
            CC_goal_plot <-
              CC_goal_c %>% 
              mutate(Goal_name = Goal, 
                     Goal = recode(Goal, `Reduce stressor` = 'A', `Reduce sensitivity` = 'B', Cope = 'C', `No change` = 'D', `Take advantage` = 'E')) %>% 
              ggplot(aes(Goal, order, size = Score, color = Context)) + 
              geom_hline(aes(yintercept = order), color = 'lightgrey', size = 0.1) +
              geom_point() + 
              theme_light() + 
              theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
                    plot.margin = unit(c(1,5,1,17.5), "lines")) +
              ylab('') + 
              scale_x_discrete(breaks=c('A', 'B','C', 'D', 'E'),
                               labels=c("Reduce stressor", "Reduce sensitivity", "Cope",
                                        "No change", "Take advantage"),
                               position = 'top') +
              scale_y_continuous(breaks = NULL, labels = c('', ''), limits = c(0,max(CC_goal_c$order)+1), expand = expand_scale())+
              scale_size(breaks = c(1, 5, 10, 20, 50 ,85, 125), labels = as.character(c(1, 5, 10, 20, 50 ,85, 125)))
            
            
            for(i in 1:length(labsg$br)){ 
              CC_goal_plot <-
                CC_goal_plot + 
                annotation_custom(
                  grob = grid::textGrob(label = labsg$lab[i], just = 'left',gp = gpar(col = as.character(labsg$col[i]))), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                  ymin = labsg$br[i]+0.2,      # Vertical position of the textGrob
                  ymax = labsg$br[i]-0.2,
                  xmin = labsg$x[i],         # Note: The grobs are positioned outside the plot area
                  xmax = labsg$x[i])
            }
            
            
            CC_goal_plot <-
              CC_goal_plot + 
              annotation_custom(
                grob = grid::textGrob(label = 'Adaptation options:', just = 'left',gp = gpar(fontsize = 12, fontface = 'bold')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = max(CC_goal_c$order, na.rm = T)+1,      # Vertical position of the textGrob
                ymax = max(CC_goal_c$order, na.rm = T)+1,
                xmin = 5.75,         # Note: The grobs are positioned outside the plot area
                xmax = 5.75)+ 
              annotation_custom(
                grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = (max(CC_goal_c$order)+1)*13/15,      # Vertical position of the textGrob
                ymax = (max(CC_goal_c$order)+1)*13/15,
                xmin = 5.75,         # Note: The grobs are positioned outside the plot area
                xmax = 5.75)+ 
              annotation_custom(
                grob = grid::textGrob(label = 'Ecological', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = (max(CC_goal_c$order)+1)*14/15,      # Vertical position of the textGrob
                ymax = (max(CC_goal_c$order)+1)*14/15,
                xmin = 5.75,         # Note: The grobs are positioned outside the plot area
                xmax = 5.75)+ 
              annotation_custom(
                grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = (max(CC_goal_c$order)+1)*12/15,      # Vertical position of the textGrob
                ymax = (max(CC_goal_c$order)+1)*12/15,
                xmin = 5.75,         # Note: The grobs are positioned outside the plot area
                xmax = 5.75)
            
            pdf(paste0('country_figs/Goal_plot_', country_list[[x]],nr,'.pdf'), width = 6.8, height = 11.7)
            
            gt <- ggplot_gtable(ggplot_build(CC_goal_plot))
            gt$layout$clip[gt$layout$name == "panel"] <- "off"
            grid::grid.draw(gt)
            
            dev.off()
            
            
            #What examples are used most within a CC context vs without? Difference between implemented versus idea?
            
            CC_ex_c <-
              dat2 %>% 
              filter(!(is.na(Implemented) | is.na(Community) | is.na(Context) | is.na(Example))) %>% 
              group_by(Context, Community, Implemented, Example) %>% 
              summarise(N_Example = n()) %>% 
              left_join(dat2 %>% 
                          filter(!(is.na(Implemented) | is.na(Community) | is.na(Context)| is.na(Example))) %>% 
                          group_by(Context, Community, Implemented) %>%
                          summarise(total = n())) %>% 
              mutate(Prop_Example = round(N_Example/total, 2)) %>% 
              arrange(Context, Community, Implemented, desc(Prop_Example)) %>% 
              mutate(id = 1:n()) %>% 
              filter(id < 11) %>% 
              ungroup %>% 
              rename(`Proportion of adaptation options` = Prop_Example) %>% 
              mutate(#Context = ifelse(Context=='CC', 'Climate change context', 'Non-climate-change context'),
                Community = ifelse(Community == 'Y', 'Community focused', 'Not community focused'),
                Implemented = ifelse(Implemented == 'Y', 'Implemented', 'Not implemented'),
                Example = ifelse(grepl('Financial assistance or investment for entering,', Example), 'Financial assistance or investment for entering,', Example))
            
            write_csv(CC_ex_c %>% 
                        unite(Attributes, Context, Community, Implemented) %>% 
                        select(-c(N_Example, total, id)) %>% 
                        spread(key = Attributes, value = `Proportion of adaptation options`), paste0('country_figs/CC_ex_', country_list[[x]], '.csv'))
  
            CC_ex_plot <-
              CC_ex_c  %>% 
              ggplot(aes(x = 1, y = 11 - id, label = Example, size = `Proportion of adaptation options`)) + 
              geom_text()+
              theme_classic()+ 
              theme(panel.border = element_rect(fill = NA), axis.text.y = element_blank(),axis.text.x = element_blank(), axis.ticks = element_blank())+
              xlab('')+ylab('')+
              labs(size="Proportion of\nadaptation\noptions") +
              scale_y_continuous(expand = expand_scale(), limits = c(0,11))+
              scale_size(breaks = c(0.05, 0.1, 0.2, 0.3), labels = as.character(c(0.05, 0.1, 0.2, 0.3)))+
              facet_grid(Community*Implemented ~ Context) 
            
            g <- ggplot_gtable(ggplot_build(CC_ex_plot))
            strip_both <- which(grepl('strip-', g$layout$name))
            fills <- c("green","gold","white","gold", "green","white","white","white")
            k <- 1
            for (i in strip_both[3:6]) {
              j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
              jj <- which(grepl('rect', g$grobs[[i]]$grobs[[2]]$childrenOrder))
              g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
              g$grobs[[i]]$grobs[[2]]$children[[jj]]$gp$fill <- fills[k+1]
              k <- k+2
            }
            fills <- c('#F8766D', '#00BFC4')
            k <- 1
            for (i in strip_both[1:2]) {
              j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
              g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
              k <- k+1
            }
            
            pdf(paste0('country_figs/CC_ex_', country_list[[x]],nr, '.pdf'), width = 7.8, height = 6.2)
            grid.draw(g)
            dev.off()
            
            
            
            #What examples are anticipatory versus responsive versus both?
            
            
            CC_ant_c <-
              dat2 %>% 
              filter(!(is.na(Context) | is.na(Example) | is.na(Implemented))) %>% 
              group_by(Context, Example, Implemented) %>% 
              summarise(Anticipatory = sum(Anticipatory, na.rm = T),
                        Responsive = sum(Responsive, na.rm = T),
                        Both = sum(Both, na.rm = T)
              )%>% 
              mutate(all = Anticipatory+Responsive+Both) %>% 
              left_join(dat2 %>% 
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
            CC_ant_c %>% 
              select(Context, Example, Implemented, Prop_Anticipatory, Prop_Responsive, Prop_Both) %>%
              #  rename(Anticipatory = Prop_Anticipatory, Responsive = Prop_Responsive, Both = Prop_Both) %>% 
              mutate(Anticipatory = round(Prop_Anticipatory,2), `Resp. or Both` = Prop_Responsive + Prop_Both,
                     Implemented = ifelse(Implemented == 'Y', 'Implemented', 'Not implemented')
              ) %>% 
              select(-c(Prop_Anticipatory, Prop_Responsive, Prop_Both, `Resp. or Both`)) %>%
              unite(Attributes, Context, Implemented) %>% 
              spread(key = Attributes, value = Anticipatory) %>% 
              write_csv(paste0('country_figs/CC_ant_', country_list[[x]], '.csv'))
            
            
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
              select(Example, Context, id) %>% 
              mutate(lab = substr(Example, 1, 49),
                     lab = ifelse(lab=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', lab)
              ) %>% 
              left_join(labsc %>% 
                          select(lab, col, Example, Context)) %>% 
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
              #      rename(Anticipatory = Prop_Anticipatory, Responsive = Prop_Responsive, Both = Prop_Both) %>% 
              mutate(Anticipatory = Prop_Anticipatory, `Resp. or Both` = Prop_Responsive + Prop_Both) %>% 
              select(-c(Prop_Anticipatory, Prop_Responsive, Prop_Both)) %>% 
              ungroup() %>% 
              mutate(Example = substr(Example, 1, 49),
                     Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
                     Context = ifelse(Context=='N', 'F', Context),
                     Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
              mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
              gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
              group_by(Context, Example, Implemented) %>% 
              ggplot(aes(x="", y=Count, fill=Group))+
              geom_bar(width = 1, stat = "identity", colour = "darkgrey", size = 0.1)+ 
              #scale_fill_manual(values=c("aquamarine", "darkviolet", "darkolivegreen1"))+ 
              scale_fill_manual(values=c("darkviolet", "darkolivegreen1"))+ 
              coord_polar("y", start=0) + 
              theme_void() + 
              theme(legend.position="left", plot.margin=unit(c(0,0,0,0.1),"cm"))+
              labs(fill = '') +
              facet_grid(Example ~Context*Implemented) 
            
            Ant_plot <-
              CC_ant_c %>% 
              select(Context, Example, Implemented, Prop_Anticipatory, Prop_Responsive, Prop_Both) %>%
              #rename(Anticipatory = Prop_Anticipatory, Responsive = Prop_Responsive, Both = Prop_Both) %>% 
              mutate(Anticipatory = Prop_Anticipatory, `Resp. or Both` = Prop_Responsive + Prop_Both) %>% 
              select(-c(Prop_Anticipatory, Prop_Responsive, Prop_Both)) %>% 
              ungroup() %>% 
              mutate(Example = substr(Example, 1, 49),
                     Example = ifelse(Example=='Financial assistance to help transition out of fi', 'Financial assistance to help transition out of fish', Example),
                     Context = ifelse(Context=='N', 'F', Context),
                     Implemented = ifelse(Implemented=='Y', 'I', 'N')) %>%
              mutate(Example = factor(Example, levels = ant_labs$lab %>% unique())) %>% 
              gather(value = 'Count', key = 'Group', -c('Context', 'Example','Implemented')) %>% 
              group_by(Context, Example, Implemented) %>% 
              ggplot(aes(x="", y=Count, fill=Group))+
              geom_bar(width = 1, stat = "identity", colour = "darkgrey", size = 0.1)+ 
              #scale_fill_manual(values=c("aquamarine", "blue", "red"))+
              scale_fill_manual(values=c("darkviolet", "darkolivegreen1"))+
              coord_polar("y", start=0) + 
              theme_void() + 
              theme(legend.position="left", strip.text.y = element_text(colour = 'white'), plot.margin=unit(c(0,0,0,0.1),"cm"))+
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
                xmin = 0.05,         # Note: The grobs are positioned outside the plot area
                xmax = 0.05) +
              annotation_custom(
                grob = grid::textGrob(label = 'Ecological', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = 2.5,      # Vertical position of the textGrob
                ymax = 3.5,
                xmin = 0.05,         # Note: The grobs are positioned outside the plot area
                xmax = 0.05) +
              annotation_custom(
                grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = 2,      # Vertical position of the textGrob
                ymax = 3,
                xmin = 0.05,         # Note: The grobs are positioned outside the plot area
                xmax = 0.05)+
              annotation_custom(
                grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = 1.5,      # Vertical position of the textGrob
                ymax = 2.5,
                xmin = 0.05,         # Note: The grobs are positioned outside the plot area
                xmax = 0.05)
            
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
            
            pdf(paste0('country_figs/Ant_plot_', country_list[[x]],nr, '.pdf'), width = 6.25, height = 7.5)
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(8, 6)))
            print(leg2, vp=viewport(layout.pos.row = 1:3, layout.pos.col = 1))
            #print(grid.draw(legend), vp=viewport(layout.pos.row = 5, layout.pos.col = 1))
            print(grid.draw(gp0), vp =viewport(layout.pos.row = 1:8, layout.pos.col = 2:6))
            dev.off()
            
            #What managers implement which examples?
            
            CC_man_c <-
              dat2 %>% 
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
              left_join(dat2 %>% 
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
              mutate(Prop_top = round((Internat. + `Nat. govt.` + `Region. govt.`)/all,3),
                     Prop_bottom = round((`Local govt.` + `Community assoc.` + `Business coop.`)/all,3),
                     Prop_NGO = round((NGO + Uni.)/all,3),
                     Prop_ind = round((Business + Individual)/all,3)) %>% 
              arrange(id)%>% 
              right_join(CC_man %>% 
                           select(Context, Example, Implemented)) 
            #filter(id < 11)
            
            write_csv(CC_man_c %>% 
                        select(Example, Context, Implemented, 
                               `Proportion Top-down` = Prop_top, 
                               `Proportion Bottom-up` = Prop_bottom, 
                               `Proportion NGO` = Prop_NGO,
                               `Proportion Individual` = Prop_ind) %>% 
                        arrange(Example, Context, Implemented), 
                      paste0('country_figs/CC_man_', country_list[[x]],'.csv'))
            
            
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
              geom_bar(width = 1, stat = "identity", colour = "darkgrey", size = 0.1)+ 
              #  scale_fill_discrete(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual')) +
              scale_fill_manual(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual'), values=c("blue", "red", "darkorange","yellow"))+ 
              coord_polar("y", start=0) + 
              theme_void() + 
              theme(legend.position="left", plot.margin=unit(c(0.1,0,0.1,0.1),"cm"))+
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
              geom_bar(width = 1, stat = "identity", colour = "darkgrey", size = 0.1)+ 
              #  scale_fill_discrete(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual')) +
              scale_fill_manual(limits=c('Top-down', 'Bottom-up', 'Non-profit', 'Individual'), values=c("blue", "red", "darkorange","yellow"))+ 
              coord_polar("y", start=0) + 
              theme_void() + 
              theme(legend.position="left", strip.text.y = element_text(colour = 'white'), plot.margin=unit(c(0.1,0,0.1,0.1),"cm"))+
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
                xmin = 0.05,         # Note: The grobs are positioned outside the plot area
                xmax = 0.05) +
              annotation_custom(
                grob = grid::textGrob(label = 'Ecological', just = 'left',gp = gpar(fontsize = 12, col = 'aquamarine3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = 2.5,      # Vertical position of the textGrob
                ymax = 3.5,
                xmin = 0.05,         # Note: The grobs are positioned outside the plot area
                xmax = 0.05) +
              annotation_custom(
                grob = grid::textGrob(label = 'Social', just = 'left',gp = gpar(fontsize = 12, col = 'goldenrod3')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = 2,      # Vertical position of the textGrob
                ymax = 3,
                xmin = 0.05,         # Note: The grobs are positioned outside the plot area
                xmax = 0.05)+
              annotation_custom(
                grob = grid::textGrob(label = 'Institutional', just = 'left',gp = gpar(fontsize = 12, col = 'darkblue')), #df$n[i], hjust = 0, gp = gpar(cex = 1.5)),
                ymin = 1.5,      # Vertical position of the textGrob
                ymax = 2.5,
                xmin = 0.05,         # Note: The grobs are positioned outside the plot area
                xmax = 0.05)
            
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
            
            pdf(paste0('country_figs/Man_plot_',country_list[[x]],nr,'.pdf'), width = 6.25, height = 7.5)
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(8, 6)))
            print(leg2, vp=viewport(layout.pos.row = 1:3, layout.pos.col = 1))
            #print(grid.draw(legend), vp=viewport(layout.pos.row = 5, layout.pos.col = 1))
            print(grid.draw(gp0), vp =viewport(layout.pos.row = 1:8, layout.pos.col = 2:6))
            dev.off()
          }
          , silent = T)  
      }   
      #  })
      
    }
  }
  #}
