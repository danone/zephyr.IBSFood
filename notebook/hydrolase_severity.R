library(Rtsne)
library(dplyr)
library(ggplot2)

load("notebook/hydrolase_count_rescaled.rda")


hydrolase_count_rescaled %>%
  select(msp_name,`New Class`, contains("M", ignore.case=FALSE)) %>%
  reshape2::melt(id.vars=c("msp_name","New Class")) %>%
  filter(value>1) %>%
  mutate(variable = variable %>% as.character) %>%
  reshape2::dcast(msp_name+variable~`New Class`, fill=0) -> hydrolase_count_rescaled_by_msp



hydrolase_tsne = Rtsne::Rtsne(hydrolase_count_rescaled_by_msp[,-c(1:2)] %>% distinct() ,perplexity=50)



A      <- colnames(hydrolase_count_rescaled_by_msp[,-c(1:2)])
A_dots <- lapply(A, as.symbol)

hydrolase_count_rescaled_by_msp[,-c(1:2)] %>%
  tibble::as_tibble() %>%
  dplyr::group_by_all() %>%
  summarise(n=n())




tmp = hydrolase_count_rescaled_by_msp[,-c(1:2)]


tmp[tmp>0] = 1

tmp %>%
  ungroup() %>%
  #tibble::as_tibble() %>%
  dplyr::group_by_all() %>%
  summarise(n=n()) -> test_tmp


hydrolase_tsne = Rtsne::Rtsne(test_tmp %>% filter(n>0) %>% select(-n) ,perplexity=5)


pdf("hydrolase_MSP.pdf")
ggplot(cbind(hydrolase_tsne$Y,test_tmp %>% filter(n>0) %>% as.data.frame)) +
  geom_point(aes(x=`1`, y=`2`, col=`[FeFe] Group A3`, size=n)) + scale_size_continuous(trans="log10")


ggplot(cbind(hydrolase_tsne$Y,test_tmp %>% filter(n>0) %>% as.data.frame)) +
  geom_point(aes(x=`1`, y=`2`, col=`[FeFe] Group A2`, size=n)) + scale_size_continuous(trans="log10")



ggplot(cbind(hydrolase_tsne$Y,test_tmp %>% filter(n>0) %>% as.data.frame)) +
  geom_point(aes(x=`1`, y=`2`, col=`[FeFe] Group A1`, size=n)) + scale_size_continuous(trans="log10")



ggplot(cbind(hydrolase_tsne$Y,test_tmp %>% filter(n>0) %>% as.data.frame)) +
  geom_point(aes(x=`1`, y=`2`, col=`[FeFe] Group B`, size=n)) + scale_size_continuous(trans="log10")



ggplot(cbind(hydrolase_tsne$Y,test_tmp %>% filter(n>0) %>% as.data.frame)) +
  geom_point(aes(x=`1`, y=`2`, col=`[FeFe] Group C`, size=n)) + scale_size_continuous(trans="log10")
dev.off()




#################################

dim(MSP_KO_counts)
dim(MSP_cazy_counts)

dim(hydrolase_count_rescaled)

#MSP_KO_counts = read.csv2("notebook/MSP_KO_counts.csv", header=T, row.names=1)

MSP_hydrogenase_counts =
  hydrolase_count_rescaled %>%
  select(msp_name,`New Class`, contains("M", ignore.case = FALSE))


dim(MSP_hydrogenase_counts)

############ CAZY ####################
MSP_cazy_counts %>%
  #select(msp_name,`New Class`, contains("M", ignore.case=FALSE)) %>%
  reshape2::melt(id.vars=c("msp_name","cazy_family")) %>%
  filter(value>1) %>%
  mutate(variable = variable %>% as.character) %>%
  reshape2::dcast(msp_name+variable~`cazy_family`, fill=0) -> cazy_count_rescaled_by_msp






tmp = cazy_count_rescaled_by_msp[,-c(1:2)]


tmp[tmp>0] = 1

tmp %>%
  ungroup() %>%
  #tibble::as_tibble() %>%
  dplyr::group_by_all() %>%
  summarise(n=n()) -> test_tmp


cazy_tsne = Rtsne::Rtsne(test_tmp %>% filter(n>0) %>% select(-n) ,perplexity=50)



ggplot(cbind(cazy_tsne$Y,test_tmp %>% filter(n>0) %>% as.data.frame)) +
  geom_point(aes(x=`1`, y=`2`, col=`GH13`%>% as.logical(), size=n),alpha=0.3) +
  scale_size_continuous(trans="log10") + theme_void()



########### CAZY + hydrogenase ###############

MSP_hydrogenase_counts %>%
  dplyr::rename(cazy_family = `New Class`) %>%
  as.data.frame() %>%
  rbind(MSP_cazy_counts %>% as.data.frame %>%
          filter(cazy_family %in% c("GH2","GH13","GH43","GT2")  )) -> hydrogenase_cazy_counts

write.csv2(hydrogenase_cazy_counts, file="hydrogenase_cazy_counts.csv")

# MSP_hydrogenase_counts %>%
#   select(1,2) %>% merge(MSP_tax, by.x="msp_name", by.y="MSP_ID", all.y = TRUE) %>%
#   select(2,genus,family) %>% mutate(`New Class` = ifelse(is.na(`New Class`),"none", `New Class` )) %>%
#   reshape2::dcast(genus+family~`New Class`) %>% View()

hydrogenase_cazy_counts %>%
  #select(msp_name,`New Class`, contains("M", ignore.case=FALSE)) %>%
  reshape2::melt(id.vars=c("msp_name","cazy_family")) %>%
  filter(value>1) %>%
  mutate(variable = variable %>% as.character) %>%
  reshape2::dcast(msp_name+variable~`cazy_family`, fill=0) -> hydrogenase_cazy_count_rescaled_by_msp






tmp = hydrogenase_cazy_count_rescaled_by_msp[,-c(1:2)]


tmp[tmp>0] = 1

tmp %>%
  ungroup() %>%
  #tibble::as_tibble() %>%
  dplyr::group_by_all() %>%
  summarise(n=n()) -> test_tmp


hydrogenase_cazy_tsne = Rtsne::Rtsne(test_tmp %>% filter(n>0) %>% select(-n) ,perplexity=10)



ggplot(cbind(hydrogenase_cazy_tsne$Y,test_tmp %>% filter(n>0) %>% as.data.frame)) +
  geom_point(aes(x=`1`, y=`2`, col=`[NiFe] Group 4e`%>% as.logical(), size=n),alpha=0.3) +
  scale_size_continuous(trans="log10") + theme_void()






############# KEGG ###########
#
#
# MSP_KO_counts %>%
#   #select(msp_name,`New Class`, contains("M", ignore.case=FALSE)) %>%
#   reshape2::melt(id.vars=c("msp_name","KO")) %>%
#   filter(value>1) %>%
#   mutate(variable = variable %>% as.character) %>%
#   reshape2::dcast(msp_name+variable~`KO`, fill=0) -> KO_count_rescaled_by_msp
#
#
#
#
#
#
# tmp = KO_count_rescaled_by_msp[,-c(1:2)]
#
#
# tmp[tmp>0] = 1
#
# tmp %>%
#   ungroup() %>%
#   #tibble::as_tibble() %>%
#   dplyr::group_by_all() %>%
#   summarise(n=n()) -> test_tmp
#
#
# KO_tsne = Rtsne::Rtsne(test_tmp %>% filter(n>0) %>% select(-n) ,perplexity=50)
#
# save(KO_tsne,file="KO_tsne.rda")
#
#
#
#
#
#

############################### noise removal ####

infraspecies = readr::read_csv2(system.file("infraspecies", "infraspecies_table_full.csv", package="IBSFood"))[,-1]

infraspecies %<>% as.data.frame()
row.names(infraspecies) = infraspecies$msp_name_partition


infraspecies %>%
  select(contains("M", ignore.case = FALSE)) %>%
  as.matrix %>% prop.table(2) %>%
  BiotypeR::noise.removal(percent=0.01) %>% row.names() -> infra_select



infraspecies[infra_select,] %>% select(species,msp_name) %>% filter(species %in% grep("Fusicatenibacter", species, value = TRUE))


infraspecies[infra_select,] %>% select(species,msp_name) %>% distinct() %>% pull(msp_name) -> msp_name_select

msp_name_select %>% length

########################


GMM_KO = readLines("data-raw/gmms.ko.txt")

MSP_KO_counts = read.csv2("notebook/MSP_KO_counts.csv")[,-1]
MSP_cazy_counts = read.csv2("notebook/MSP_cazy_counts.csv")[,-1]

MSP_KO_counts %>%
  filter(KO %in% GMM_KO) %>%
  filter(msp_name %in% msp_name_select) %>%
    reshape2::melt(id.vars=c("msp_name","KO")) %>%
    filter(value>1) %>%
    mutate(variable = variable %>% as.character) %>%
    reshape2::dcast(msp_name+variable~`KO`, fill=0) -> KO_count_rescaled_by_msp


MSP_cazy_counts %>%
  filter(msp_name %in% msp_name_select) %>%
  reshape2::melt(id.vars=c("msp_name","cazy_family")) %>%
  filter(value>1) %>%
  mutate(variable = variable %>% as.character) %>%
  reshape2::dcast(msp_name+variable~`cazy_family`, fill=0) -> cazy_count_rescaled_by_msp


MSP_hydrogenase_counts %>%
  filter(msp_name %in% msp_name_select) %>%
  reshape2::melt(id.vars=c("msp_name","New Class")) %>%
  filter(value>1) %>%
  mutate(variable = variable %>% as.character) %>%
  reshape2::dcast(msp_name+variable~`New Class`, fill=0) -> hydrogenase_count_rescaled_by_msp



cazy_count_rescaled_by_msp %>%
  #merge(KO_count_rescaled_by_msp, by=c("msp_name","variable"), all = TRUE) %>%
  merge(hydrogenase_count_rescaled_by_msp, by=c("msp_name","variable"), all = TRUE) -> counts_rescaled_by_msp


#counts_rescaled_by_msp = cazy_count_rescaled_by_msp


tmp = counts_rescaled_by_msp[,-c(1:2)]

tmp[tmp>0] = 1
tmp[is.na(tmp)] = 0


tmp_jitter = tmp %>% multiply_by(rnorm(n = dim(tmp)[1], mean = 1, sd=0.00001 ))


vars <- rlang::syms(tmp %>% select(-contains("[")) %>% colnames)

counts_tnse =
cbind(counts_rescaled_by_msp[,1:2],tmp_jitter) %>% distinct(!!!vars, .keep_all = FALSE) %>%
  Rtsne::Rtsne(., perplexity=50)





cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp) %>% distinct(!!!vars, .keep_all = TRUE)  ) %>%
  as.data.frame() %>% ggplot() + geom_point(aes(x=`1`,y=`2`)) +
  scale_alpha(range = c(0,1))


cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp) %>% distinct(!!!vars, .keep_all = TRUE)  ) %>%
  as.data.frame() %>% ggplot() + geom_point(aes(x=`1`,y=`2`, alpha=`GH2`)) +
  scale_alpha(range = c(0,1))


cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp) %>% distinct(!!!vars, .keep_all = TRUE)  ) %>%
  as.data.frame() %>% ggplot() + geom_point(aes(x=`1`,y=`2`, alpha=`GH13`)) +
  scale_alpha(range = c(0,1))


cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp) %>% distinct(!!!vars, .keep_all = TRUE)  ) %>%
  as.data.frame() %>% ggplot() + geom_point(aes(x=`1`,y=`2`, alpha=`GT2`)) +
  scale_alpha(range = c(0,1))



# cowplot::plot_grid(
#
# cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp) %>% distinct(!!!vars, .keep_all = TRUE)  ) %>%
#   merge(IBSMicrobiota::IBSData$metadata %>%
#           select(Patient_ID,SSgroup) %>%
#           distinct(), by.x="variable", by.y="Patient_ID") %>%
#     as.data.frame() %>% ggplot() + geom_point(aes(x=`1`,y=`2`, alpha=`GH2`, col=SSgroup)) +
#     scale_alpha(range = c(0,0.5)) + theme_void(),
#
# cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp) %>% distinct(!!!vars, .keep_all = TRUE)  ) %>%
#   merge(IBSMicrobiota::IBSData$metadata %>%
#           select(Patient_ID,SSgroup) %>%
#           distinct(), by.x="variable", by.y="Patient_ID") %>%
#     as.data.frame() %>% ggplot() + geom_point(aes(x=`1`,y=`2`, alpha=`GH13`, col=SSgroup)) +
#     scale_alpha(range = c(0,0.5)) + theme_void(),
#
# cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp) %>% distinct(!!!vars, .keep_all = TRUE)  ) %>%
#   merge(IBSMicrobiota::IBSData$metadata %>%
#           select(Patient_ID,SSgroup) %>%
#           distinct(), by.x="variable", by.y="Patient_ID") %>%
#   as.data.frame() %>% ggplot() + geom_point(aes(x=`1`,y=`2`, alpha=`GT2`, col=SSgroup)) +
#   scale_alpha(range = c(0,0.5)) + theme_void(),
#
# cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp) %>% distinct(!!!vars, .keep_all = TRUE)  )  %>%
#   merge(IBSMicrobiota::IBSData$metadata %>%
#           select(Patient_ID,SSgroup) %>%
#           distinct(), by.x="variable", by.y="Patient_ID") %>%
#   as.data.frame() %>% ggplot() + geom_point(aes(x=`1`,y=`2`, alpha=`[FeFe] Group A1`, col=SSgroup)) +
#   scale_alpha(range = c(0,0.5)) + theme_void(),
#
# cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp) %>% distinct(!!!vars, .keep_all = TRUE)  )  %>%
#   merge(IBSMicrobiota::IBSData$metadata %>%
#           select(Patient_ID,SSgroup) %>%
#           distinct(), by.x="variable", by.y="Patient_ID") %>%
#   as.data.frame() %>% ggplot() + geom_point(aes(x=`1`,y=`2`, alpha=`[FeFe] Group B`, col=SSgroup)) +
#   scale_alpha(range = c(0,0.5)) + theme_void(),
#
# cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp) %>% distinct(!!!vars, .keep_all = TRUE)  )  %>%
#   merge(IBSMicrobiota::IBSData$metadata %>%
#           select(Patient_ID,SSgroup) %>%
#           distinct(), by.x="variable", by.y="Patient_ID") %>%
#   as.data.frame() %>% ggplot() + geom_point(aes(x=`1`,y=`2`, alpha=`[FeFe] Group A3`, col=SSgroup)) +
#   scale_alpha(range = c(0,0.5)) + theme_void(),
#
# nrow = 2)
#



cbind(counts_tnse$Y, cbind(counts_rescaled_by_msp[,1:2],tmp_jitter) %>% distinct(!!!vars, .keep_all = TRUE)  )  %>%
  merge(IBSMicrobiota::IBSData$metadata %>%
          select(Patient_ID,SSgroup) %>%
          distinct(), by.x="variable", by.y="Patient_ID") %>%
  select(`1`,`2`,SSgroup, GH2,GH13,GT2,`[FeFe] Group A1`,`[FeFe] Group B`,`[FeFe] Group A3`) %>%
  reshape2::melt(id.vars=c("1","2","SSgroup")) %>%
  filter(value !=0 ) %>%
  ggplot() + geom_hex(aes(x=`1`,y=`2`)) +
  scale_alpha(range = c(0,0.5)) + facet_grid(SSgroup~variable) + theme_void()




IBSMicrobiota::IBSData$metadata %>%
  select(Patient_ID,SSgroup) %>%
  distinct()





tmp = KO_count_rescaled_by_msp[,-c(1:2)]


tmp[tmp>0] = 1

tmp %>%
  ungroup() %>%
  #tibble::as_tibble() %>%
  dplyr::group_by_all() %>%
  summarise(n=n()) -> test_tmp

test_tmp %>% apply(1,sum) -> KO_nb_per_MSP

KO_tsne =
  Rtsne::Rtsne(test_tmp %>%
                 filter(n>0) %>%
                 select(-n) %>% cbind(KO_nb_per_MSP=KO_nb_per_MSP) %>% filter(KO_nb_per_MSP>30) %>% select(-KO_nb_per_MSP)
               ,perplexity=50)

save(KO_tsne,file="KO_tsne.rda")


ggplot(data.frame(KO_tsne$Y,
             test_tmp %>%
               filter(n>0) %>%
               cbind(KO_nb_per_MSP=KO_nb_per_MSP) %>%
               filter(KO_nb_per_MSP>30))) +
  geom_point(aes(x=`X1`, y=`X2`, size=n, col=K00281 %>% as.logical), alpha=0.3) +
  scale_size_continuous(trans="log10") + theme_void()


ggplot(cbind(KO_tsne$Y,test_tmp %>% filter(n>0) %>% as.data.frame, KO_nb_per_MSP)) +
  geom_point(aes(x=n, y=KO_nb_per_MSP)) + scale_x_log10() + scale_y_log10()


ggplot(cbind(KO_tsne$Y,test_tmp %>% filter(n>0) %>% as.data.frame, KO_nb_per_MSP)) +
  geom_histogram(aes(x=n)) + scale_x_log10()




data.frame(KO_tsne$Y,
           test_tmp %>%
             filter(n>0) %>%
             cbind(KO_nb_per_MSP=KO_nb_per_MSP) %>%
             filter(KO_nb_per_MSP>30)) %>%
  cor(method="spearman") %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  .[,1:2] %>% filter(abs(X1)>0.3)









