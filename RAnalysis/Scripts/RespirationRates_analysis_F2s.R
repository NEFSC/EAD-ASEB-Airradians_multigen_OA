





#  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 20220830 spat :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

F2_spat   <- RR_master %>% 
  dplyr::filter(Date %in% '8/30/2022') %>% # call the date we completed respiromatery on F2 spat and larvae
  dplyr::filter(Num_indivs %in% 1)# call only rows that say we used 1 animal per well (Note: 1 indiv per well for spat)


F2_spat[!is.na(F2_spat$resp_umol_L_hr),] %>% 
  ggplot( aes(x = factor(pCO2, level = c('500 μatm', '800 μatm')), resp_µmol_L_mm_Length_hr , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, width=0.5, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("white","grey50")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic()

#  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 20220830 larvae :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

F2_larvae <- RR_master %>% 
  dplyr::filter(Date %in% '8/30/2022') %>% # call the date we completed respiromatery on F2 spat and larvae
  dplyr::filter(Num_indivs %in% 5)# call only rows that say we used 1 animal per well (Note: 1 indiv per well for spat)


F2_larvae[!is.na(F2_larvae$resp_umol_L_hr),] %>% 
  ggplot( aes(x = factor(pCO2, level = c('500 μatm', '800 μatm')), resp_µmol_L_mm_Length_hr , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, width=0.5, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("white","grey50")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic()



#  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 20220830 spat and larvae  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

F2_larvae_spat  <- RR_master %>% 
  dplyr::filter(Date %in% '8/30/2022') %>% # call the date we completed respiromatery on F2 spat and larvae
  dplyr::mutate(Type = 
                  case_when(Num_indivs == 1 ~ 'spat', 
                            Num_indivs == 5 ~ 'larvae')) %>% 
  dplyr::mutate(resp_µmol_L_mm_Length_hr = resp_µmol_L_mm_Length_hr/Num_indivs)

F2_larvae_spat[!is.na(F2_larvae_spat$resp_umol_L_hr),] %>% 
  ggplot( aes(x = factor(pCO2, level = c('500 μatm', '800 μatm')), resp_µmol_L_mm_Length_hr , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, width=0.5, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("white","grey50")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() + 
  facet_wrap(~Type)




