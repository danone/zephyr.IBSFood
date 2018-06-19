library(readr)
library(dplyr)


MSP_tax = readxl::read_excel("data-raw/MSP_taxonomy.xlsx", sheet=4)


MSP_tax %>%
  rename(MSP_ID = X__1) %>%
  select(MSP_ID,species,genus,family,order,class,phylum,superkingdom) -> MSP_tax

devtools::use_data(MSP_tax, overwrite=TRUE)




MSP_genes_id_corresp = read_tsv("data-raw/MetaHIT_v2_MSP_corresp_v3.tsv")


MSP_genes_id_corresp %>% na.omit() -> MSP_genes_id_corresp

devtools::use_data(MSP_genes_id_corresp, overwrite=TRUE)


