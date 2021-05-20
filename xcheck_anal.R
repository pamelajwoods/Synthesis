library(tidyverse)
# library(grid)
# library(gtable)
# library(gridExtra)
# library(devtools)
# install_github('fawda123/ggord')
# library(ggord)
# library(ca)


datX_raw <- 
  read_csv('Adaptation_option_repository_consolidated_r2_Xcheck.csv') %>% 
  select(-c(grep('X', names(read_csv('Adaptation_option_repository_consolidated_r2_Xcheck.csv')), value = T))) 

#This takes care of recording inconsistencies
datX <-
  datX_raw %>% 
  mutate(Implemented = ifelse(Implemented=='Y' | Implemented=='y' | Implemented=='Y?', 'Y',
                              ifelse(Implemented=='N' |Implemented=='n' | Implemented=='N?', 'N', NA)),
         Community = ifelse(Community=='Y' | Community=='y' | Community=='Y?', 'Y',
                            ifelse(Community=='N' |Community=='n' | Community=='N?', 'N', NA)),
         Context = ifelse(Context=='Y'|Context=='cc', 'CC', Context),
         Context = ifelse(Context=='n', 'N', Context),
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
                                                                                                                                                 ifelse(Example=='Adaptation program', 'Adaptation',
                                                                                                                                                 ifelse(Example=='enforcement', 'Enforcement',
                                                                                                                                                        ifelse(Example=='Market diversification (catch new species)', 'Market diversification',
                                                                                                                                                               ifelse(Example=='Permit/license/quota bank' | Example=='Permit/licence/quota bank', 'Permit/license/quota banks',
                                                                                                                                                                      ifelse(Example=='Restrictions on discards (partial or full)'|'reduction in other stressors', 'Reductions in other stressors',
                                                                                                                                                                             ifelse(Example=='Review program/regulations' | Example=='Review programs', 'Review programs / regulations',
                                                                                                                                                                                    ifelse(Example=='Transition out of fisheries' | Example=='Transition out of fishery' | Example=='Investments to help transition out of fisheries', 'Transition out of fisheries',
                                                                                                                                                                                           Example))))))))))))))))))))))))),
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
  ) 

ex_counts<-
  datX %>% 
  group_by(ID,rep,Example) %>% 
  count() %>% 
  spread(key = rep, value = n)

View(ex_counts)
