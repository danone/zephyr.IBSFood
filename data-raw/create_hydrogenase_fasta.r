
library(seqinr)
library(readxl)


KGMI_table = readxl::read_xlsx("data-raw/KGMI_A_1182288_Table S2.xlsx", skip = 1)

seq = lapply(KGMI_table$`Protein Sequence`, s2c)

names(seq) = KGMI_table$`NCBI Accession`

write.fasta(sequences = seq, names = names(seq), nbchar = 80, file.out = "data-raw/hydrogenases.fasta")

## then blastp on metahitdb
## import blast result
hydro_blast = readr::read_tsv("data-raw/1550678091_MetaHit_39M_pep.tab")

hydro_blast_best_hit=

hydro_blast %>%
  select(`# Fields: query id`, `subject id`, `% identity`, `subject length`, `alignment length`, `bit score`, `q. end` , `q. start`) %>%
  group_by(`subject id`) %>%
  top_n(n=1,wt=`bit score`) %>%
  ungroup() %>% filter(`% identity` > 60, `q. end` - `q. start` > 40 )


hydrolase_hit =
KGMI_table %>%
  merge(hydro_blast_best_hit, by.x="NCBI Accession", by.y="# Fields: query id") %>%
  select(`NCBI Accession`,Organism,`New Class`,Phylum,Order,`subject id`)

# KGMI_table %>%
#   merge(hydro_blast_best_hit, by.x="NCBI Accession", by.y="# Fields: query id") %>%
#   select(`NCBI Accession`,Organism,`New Class`,Phylum,Order,`subject id`) %>%
#   merge(MSP_genes_id_corresp %>% select(msp_name,module_name,gene_name_v2), by.x="subject id", by.y="gene_name_v2") %>%
#   merge(MSP_tax, by.x="msp_name", by.y="MSP_ID") %>%
#   select(Phylum,Organism,phylum,species)


devtools::use_data(hydrolase_hit, overwrite = TRUE)
