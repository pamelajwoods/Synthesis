library(tidyverse)
# library(grid)
# library(gtable)
# library(gridExtra)
# library(devtools)
# install_github('fawda123/ggord')
# library(ggord)
# library(ca)

#source('repos_anal.R')


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
         Context = ifelse(Context=='Y'|Context=='cc'|Context=='C', 'CC', Context),
         Context = ifelse(Context=='n', 'N', Context),
         Example = ifelse(Example=='Adaptation programme'|Example=='Adaptation program'|Example=='Adaptation', 'Adaptation programs', 
                          ifelse(Example=='cooperatives'|Example=='Cooperative', 'Cooperatives', 
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
                                                                                                                                                                      ifelse(Example=='Restrictions on discards (partial or full)'|Example=='reduction in other stressors'|Example=='Reduction in other stressors', 'Reductions in other stressors',
                                                                                                                                                                             ifelse(Example=='Review program/regulations' | Example=='Review programs', 'Review programs / regulations',
                                                                                                                                                                                    ifelse(Example=='Transition out of fisheries' | Example=='Transition out of fishery' | Example=='Investments to help transition out of fisheries', 'Transition out of fisheries',
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
  ) 

ex_counts<-
  datX %>% 
  group_by(ID,rep,Example) %>% 
  count() %>% 
  spread(key = rep, value = n)%>% 
  mutate(original = ifelse(is.na(a), 0.01, a),
         repeated = ifelse(is.na(b), 0.01, b)) %>% 
  left_join(labs %>% select(Example, col) %>% distinct()) %>% 
  mutate(Type = ifelse(col=='aquamarine3', 'Ecological', 
                       ifelse(col=='darkblue', 'Institutional', 'Social')))

ex_count_plot<-
  ex_counts %>% 
  ggplot(aes(original,repeated, color = Type)) +
  geom_jitter()+
  theme_bw() +
  scale_color_discrete(type = c('aquamarine3', 'darkblue', 'darkgoldenrod3')) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) 
  
pdf('ex_count_plot.pdf', width = 5, height = 4)
  print(ex_count_plot)
dev.off()


ex_counts %>% 
  mutate(diff=abs(original-repeated)/(original+repeated)) %>% 
  filter(diff > 0.5, abs(original-repeated)>1) %>% 
  View

ex_counts %>% 
  mutate(diff=abs(original-repeated)/(original+repeated)) %>% 
  filter(diff > 0.5, original==0.01|repeated==0.01) %>% 
  View

diff_to_cat1<-
  ex_counts %>% 
  mutate(diff=abs(original-repeated)/(original+repeated)) %>% 
  filter(diff > 0.5, original==0.01|repeated==0.01) %>% 
  ungroup %>% 
  select(Example) %>% 
  group_by(Example) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  spread(key = difficult, value = n) %>% 
  arrange(desc(difficult)) 
  


diff_to_cat_all<-
  ex_counts %>% 
  mutate(diff=abs(original-repeated)/(original+repeated)) %>% 
  mutate(difficult = ifelse(diff >= 0.5, 'difficult', 'not')) %>% 
  ungroup %>% 
  select(Example, difficult) %>% 
  group_by(Example, difficult) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  spread(key = difficult, value = n)%>% 
  arrange(desc(difficult)) 

write.csv(diff_to_cat_all, file = 'diff_to_cat_all.csv',row.names = F)

datX <- 
  datX %>% 
  mutate(`Stock decline` = as.numeric(`Stock decline`),
         `Sp. distributional shifts` = as.numeric(`Sp. distributional shifts`),
         `Ocean acidification` = as.numeric(`Ocean acidification`),
         `Uncertainty (ecological)` = as.numeric(`Uncertainty (ecological)`)) 

str<-
  datX%>% 
  mutate(stress_tot = apply(datX[7:16], 1, sum, na.rm = T),
         max_tot = apply(datX[7:16], 1, max, na.rm = T),
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
  group_by(ID, rep) %>%
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
  gather(key = Stressor, value = Sum, -c(ID,rep)) %>% 
  spread(key = rep, value = Sum) %>% 
  rename(original =a, repeated = b)

str_plot <-
  str %>% 
  ggplot(aes(original, repeated, color = Stressor)) +
  geom_jitter() +
  #scale_y_continuous(trans = 'log', breaks = c(1,3,7,20,30))+
  #scale_x_continuous(trans = 'log', breaks = c(,1,3,7,20,30))+
  theme_bw()


pdf('str_plot.pdf', width = 5, height = 4)
  print(str_plot)
dev.off()

diff_to_cat_str<-
  str %>% 
  mutate(diff=abs(original-repeated)/(original+repeated)) %>% 
  mutate(difficult = ifelse(diff >= 0.5, 'difficult', 'not')) %>% 
  filter(!is.na(difficult)) %>% 
  ungroup %>% 
  select(Stressor, difficult) %>% 
  group_by(Stressor, difficult) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  spread(key = difficult, value = n, fill = 0)%>% 
  arrange(desc(difficult)) 

str %>% 
  mutate(diff=abs(original-repeated)/(original+repeated)) %>% 
  filter(diff >= 0.5) %>% 
  ungroup %>% 
  group_by(Stressor, ID) %>% 
  View
  
write.csv(diff_to_cat_str, file = 'diff_to_cat_str.csv',row.names = F)


goal<-
  datX%>% 
  mutate(stress_tot = apply(datX[32:36], 1, sum, na.rm = T),
         max_tot = apply(datX[32:36], 1, max, na.rm = T),
         `Reduce stressor` = (max_tot + 1 - `Reduce stressor`)/stress_tot,
         `Reduce sensitivity` = (max_tot + 1 - `Reduce sensitivity`)/stress_tot,
         Cope = (max_tot + 1 - Cope)/stress_tot,
         `No change` = (max_tot + 1 - `No change`)/stress_tot,
         `Take advantage` = (max_tot + 1 - `Take advantage`)/stress_tot) %>% 
  filter(!(is.na(Context) | is.na(Example) | stress_tot==0)) %>% 
  group_by(ID,rep) %>%
  summarise(`Reduce stressor` = round(sum(`Reduce stressor`, na.rm = T), 2),
            `Reduce sensitivity` = round(sum(`Reduce sensitivity`, na.rm = T), 2),
            Cope = round(sum(Cope, na.rm = T), 2),
            `No change` = round(sum(`No change`, na.rm = T), 2),
            `Take advantage` = round(sum(`Take advantage`, na.rm = T), 2)
  ) %>% 
  gather(key = Goal, value = Sum, -c(ID,rep)) %>% 
  spread(key = rep, value = Sum) %>% 
  rename(original =a, repeated = b)

goal_plot <-
  goal %>% 
  ggplot(aes(original, repeated, color = Goal)) +
  geom_jitter() +
  #scale_y_continuous(trans = 'log', breaks = c(1,3,7,20,30))+
  #scale_x_continuous(trans = 'log', breaks = c(,1,3,7,20,30))+
  theme_bw()


pdf('goal_plot.pdf', width = 5, height = 4)
print(goal_plot)
dev.off()

diff_to_cat_goal<-
  goal %>% 
  mutate(diff=abs(original-repeated)/(original+repeated)) %>% 
  mutate(difficult = ifelse(diff >= 0.5, 'difficult', 'not')) %>% 
  ungroup %>% 
  select(Goal, difficult) %>% 
  filter(!is.na(difficult)) %>% 
  group_by(Goal, difficult) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  spread(key = difficult, value = n)%>% 
  arrange(desc(difficult)) 

goal %>% 
  mutate(diff=abs(original-repeated)/(original+repeated)) %>% 
  filter(diff >= 0.5) %>% 
  ungroup %>% 
  group_by(Goal, ID) %>% 
  View

write.csv(diff_to_cat_goal, file = 'diff_to_cat_goal.csv',row.names = F)


comm<-
  datX%>% 
  group_by(rep, ID, Community)%>%
  count() %>%
  filter(!is.na(Community)) %>% 
  spread(key = Community, value = n, fill = 0) %>%
  mutate(Np = N / (N + Y), Yp = Y / (N + Y)) %>% 
  select(-c(N, Y)) %>% 
  gather(key = Community, value = n, -c(ID, rep)) %>% 
  spread(key = rep, value = n) %>% 
  mutate(a = ifelse(is.na(a), 0, a),
         b = ifelse(is.na(b), 0, b)) %>% 
  rename(original =a, repeated = b)

comm_plot <-
  comm %>% 
  filter(Community == 'Yp') %>% 
  ggplot(aes(original, repeated, color = Community)) +
  geom_jitter() +
  theme_bw()


pdf('comm_plot.pdf', width = 5, height = 4)
print(comm_plot)
dev.off()

diff_to_cat_comm<-
  comm %>% 
  filter(Community=='Yp') %>% 
  mutate(diff=abs(original-repeated)) %>% 
  mutate(difficult = ifelse(diff >= 0.5, 'difficult', 'not')) %>% 
  ungroup %>% 
  select(Community, difficult) %>% 
  filter(!is.na(difficult)) %>% 
  group_by(Community, difficult) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  spread(key = difficult, value = n)%>% 
  arrange(desc(difficult)) 

comm %>% 
  filter(Community=='Yp') %>% 
  mutate(diff=abs(original-repeated)) %>% 
  filter(diff >= 0.5) %>% 
  ungroup %>% 
  group_by(Community, ID) %>% 
  View

write.csv(diff_to_cat_comm, file = 'diff_to_cat_comm.csv',row.names = F)

imp<-
  datX%>% 
  group_by(rep, ID, Implemented)%>%
  count() %>%
  filter(!is.na(Implemented)) %>% 
  spread(key = Implemented, value = n, fill = 0) %>%
  mutate(Np = N / (N + Y), Yp = Y / (N + Y)) %>% 
  select(-c(N, Y)) %>% 
  gather(key = Implemented, value = n, -c(ID, rep)) %>% 
  spread(key = rep, value = n) %>% 
  mutate(a = ifelse(is.na(a), 0, a),
         b = ifelse(is.na(b), 0, b)) %>% 
  rename(original =a, repeated = b)

imp_plot <-
  imp %>% 
  filter(Implemented == 'Yp') %>% 
  ggplot(aes(original, repeated, color = Implemented)) +
  geom_jitter() +
  theme_bw()


pdf('imp_plot.pdf', width = 5, height = 4)
print(imp_plot)
dev.off()

diff_to_cat_imp<-
  imp %>% 
  filter(Implemented=='Yp') %>% 
  mutate(diff=abs(original-repeated)) %>% 
  mutate(difficult = ifelse(diff >= 0.5, 'difficult', 'not')) %>% 
  ungroup %>% 
  select(Implemented, difficult) %>% 
  filter(!is.na(difficult)) %>% 
  group_by(Implemented, difficult) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  spread(key = difficult, value = n)%>% 
  arrange(desc(difficult)) 

imp %>% 
  filter(Implemented=='Yp') %>% 
  mutate(diff=abs(original-repeated)) %>% 
  filter(diff >= 0.5) %>% 
  ungroup %>% 
  group_by(Implemented, ID) %>% 
  View

write.csv(diff_to_cat_imp, file = 'diff_to_cat_imp.csv',row.names = F)



CC<-
  datX%>% 
  group_by(rep, ID, Context)%>%
  count() %>%
  filter(!is.na(Context)) %>% 
  spread(key = Context, value = n, fill = 0) %>%
  mutate(CCp = CC / (CC + N), Np = N / (N + CC)) %>% 
  select(-c(N, CC)) %>% 
  gather(key = Context, value = n, -c(ID, rep)) %>% 
  spread(key = rep, value = n) %>% 
  mutate(a = ifelse(is.na(a), 0, a),
         b = ifelse(is.na(b), 0, b)) %>% 
  rename(original =a, repeated = b)

CC_plot <-
  CC %>% 
  filter(Context == 'CCp') %>% 
  ggplot(aes(original, repeated, color = Context)) +
  geom_jitter() +
  theme_bw() 


pdf('CC_plot.pdf', width = 5, height = 4)
print(CC_plot) #jitter overly exaggerating differences
dev.off()

diff_to_cat_CC<-
  CC %>% 
  filter(Context=='CCp') %>% 
  mutate(diff=abs(original-repeated)) %>% 
  mutate(difficult = ifelse(diff >= 0.5, 'difficult', 'not')) %>% 
  ungroup %>% 
  select(Context, difficult) %>% 
  filter(!is.na(difficult)) %>% 
  group_by(Context, difficult) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  spread(key = difficult, value = n)%>% 
  arrange(desc(difficult)) 

CC %>% 
  filter(Context=='CCp') %>% 
  mutate(diff=abs(original-repeated)) %>% 
  filter(diff >= 0.5) %>% 
  ungroup %>% 
  group_by(Context, ID) %>% 
  View

write.csv(diff_to_cat_CC, file = 'diff_to_cat_CC.csv',row.names = F)



man <-
  datX %>% 
  group_by(ID,rep) %>% 
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
  mutate(Prop_top = round((Internat. + `Nat. govt.` + `Region. govt.`)/all,3),
         Prop_bottom = round((`Local govt.` + `Community assoc.` + `Business coop.`)/all,3),
         Prop_NGO = round((NGO + Uni.)/all,3),
         Prop_ind = round((Business + Individual)/all,3)) %>% 
  select(ID, rep,  Prop_top, Prop_bottom, Prop_NGO) %>% 
  gather(key = Proportion, value = p, -c(ID, rep)) %>% 
  ungroup %>% 
#  select(-Example) %>% 
  spread(key = rep, value = p) %>% 
  mutate(a = ifelse(is.na(a), 0, a),
         b = ifelse(is.na(b), 0, b)) %>% 
  rename(original =a, repeated = b)



man_plot <-
  man %>% 
  ggplot(aes(original, repeated, color = Proportion)) +
  geom_jitter() +
  theme_bw()


pdf('man_plot.pdf', width = 5, height = 4)
print(man_plot)
dev.off()

diff_to_cat_man<-
  man %>% 
  mutate(diff=abs(original-repeated)) %>% 
  mutate(difficult = ifelse(diff >= 0.5, 'difficult', 'not')) %>% 
  ungroup %>% 
  select(Proportion, difficult) %>% 
  filter(!is.na(difficult)) %>% 
  group_by(Proportion, difficult) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  spread(key = difficult, value = n)%>% 
  arrange(desc(difficult)) 

man %>% 
  mutate(diff=abs(original-repeated)) %>% 
  filter(diff >= 0.5) %>% 
  ungroup %>% 
  group_by(Proportion, ID) %>% 
  View

write.csv(diff_to_cat_man, file = 'diff_to_cat_man.csv',row.names = F)



ant <-
  datX %>% 
  group_by(ID,rep) %>% 
  summarise(Anticipatory = sum(Anticipatory, na.rm = T),
            Responsive = sum(Responsive, na.rm = T),
            Both = sum(Both, na.rm = T)
  )%>% 
  mutate(all = Anticipatory+Responsive+Both) %>% 
  mutate(Prop_Anticipatory = Anticipatory/all,
         Prop_Responsive = Responsive/all,
         Prop_Both = Both/all) %>% 
  select(ID, rep,  Prop_Anticipatory, Prop_Responsive, Prop_Both) %>% 
  mutate(Anticipatory = round(Prop_Anticipatory,2), `Resp. or Both` = Prop_Responsive + Prop_Both) %>% 
  select(-c(Prop_Anticipatory, Prop_Responsive, Prop_Both)) %>% 
  gather(key = Proportion, value = p, -c(ID, rep)) %>% 
  ungroup %>% 
  #  select(-Example) %>% 
  spread(key = rep, value = p) %>% 
  mutate(a = ifelse(is.na(a), 0, a),
         b = ifelse(is.na(b), 0, b)) %>% 
  rename(original =a, repeated = b)



ant_plot <-
  ant %>% 
  filter(Proportion=='Anticipatory') %>% 
  ggplot(aes(original, repeated, color = Proportion)) +
  geom_jitter() +
  theme_bw()


pdf('ant_plot.pdf', width = 5, height = 4)
print(ant_plot)
dev.off()

diff_to_cat_ant<-
  ant %>% 
  filter(Proportion=='Anticipatory') %>% 
  mutate(diff=abs(original-repeated)) %>% 
  mutate(difficult = ifelse(diff >= 0.5, 'difficult', 'not')) %>% 
  ungroup %>% 
  select(Proportion, difficult) %>% 
  filter(!is.na(difficult)) %>% 
  group_by(Proportion, difficult) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  spread(key = difficult, value = n)%>% 
  arrange(desc(difficult)) 

ant %>% 
  filter(Proportion=='Anticipatory')%>% 
  mutate(diff=abs(original-repeated)) %>% 
  filter(diff >= 0.5) %>% 
  ungroup %>% 
  group_by(Proportion, ID) %>% 
  View

write.csv(diff_to_cat_ant, file = 'diff_to_cat_ant.csv',row.names = F)
