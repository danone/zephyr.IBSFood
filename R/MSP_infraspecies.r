
binarize_MSP = function(msp_counts, cutoff_med=2) {

  #check colnames
  if(c("genes.id","length","msp_name","module_name") %in% colnames(msp_counts) %>%
    as.numeric() %>% sum() != 4) stop("msp_counts did not contains all useful parameters")

  #compute stat per sample
  msp_counts %>%
    select(-length) %>%
    reshape2::melt(id.vars=c("genes.id","msp_name","module_name")) %>%
    group_by(variable) %>%
    filter(module_name=="core") %>%
    summarise(value_min=quantile(value,0.025), value_max=quantile(value,0.975),value_med=median(value)) %>%
    filter(value_med >= cutoff_med) -> raw_reads_core_stat


  # convert rescaled count abundance data to binary matrix

  msp_counts %>%
    select(-length) %>%
    reshape2::melt(id.vars=c("genes.id","msp_name","module_name")) %>%
    merge(.,raw_reads_core_stat, by=c("variable"), all_y = TRUE) %>%
    mutate(bin = ifelse(value<value_max,ifelse(value>value_min+1,1,0),0)) %>%
    reshape2::dcast(genes.id+module_name~variable, value.var="bin") %>%
    mutate(module_name = ifelse(module_name=="core","core","other")) -> binary_msp

return(binary_msp)


}

partitionize_MSP = function(binary_msp) {


  #check colnames
  if(c("genes.id","module_name") %in% colnames(msp_counts) %>%
     as.numeric() %>% sum() != 2) stop("binary_msp did not contains all useful parameters")

binary_msp %>% filter(module_name  != "core") %>%
  select(-module_name,-genes.id) %>% as.matrix %>%
  t %>%
  ade4::dist.binary(.,method=1) %>%
  fpc::clusterboot(.,B=100,bootmethod=
                     "subset",clustermethod=fpc::pamkCBI, count=FALSE,
                   k=1:6, showplot=FALSE) -> clusters_msp

binary_msp %>% filter(module_name  != "core") %>%
  select(-module_name,-genes.id) %>% as.matrix %>%
  #t %>%
  dist(.,method="binary") %>%
  fpc::clusterboot(.,B=100,bootmethod=
                     "subset",clustermethod=fpc::pamkCBI, count=FALSE,
                   k=1:6, showplot=FALSE) -> clusters_msp_gene


return(list(sample=clusters_msp,gene=clusters_msp_gene))

}


plot_MSP = function(binary_msp, clusters_msp, clusters_msp_gene) {


#
#   heatmap(binary_msp %>%
#             filter(module_name  != "core") %>%
#             select(-module_name,-genes.id) %>%
#             as.matrix %>% .[,order(clusters_msp$partition)]
#             ,
#           labRow=NA, Colv = NA, keep.dendro=FALSE,
#           distfun = function(x) dist(x,method="binary"),
#           hclustfun = function(x) hclust(x,method="centroid"),
#           scale = "none",cexRow = 0.01,
#           ColSideColors = viridis::viridis(clusters_msp$nccl)[clusters_msp$partition %>% as.factor %>% as.numeric][order(clusters_msp$partition)],
#           col=c("black","yellow"))


  heatmap(binary_msp %>%
            filter(module_name  != "core") %>%
            select(-module_name,-genes.id) %>%
            as.matrix %>% .[order(clusters_msp_gene$partition),order(clusters_msp$partition)]
          ,
          labRow=NA, Colv = NA, Rowv=NA, keep.dendro=FALSE,
          distfun = function(x) dist(x,method="binary"),
          hclustfun = function(x) hclust(x,method="centroid"),
          scale = "none",cexRow = 0.01,
          ColSideColors = viridis::viridis(clusters_msp$nccl)[clusters_msp$partition %>% as.factor %>% as.numeric][order(clusters_msp$partition)],
          RowSideColors = viridis::magma(clusters_msp_gene$nccl)[clusters_msp_gene$partition %>% as.factor %>% as.numeric][order(clusters_msp_gene$partition)],

          col=c("black","yellow"))





}


test_MSP_gene = function(binary_msp,clusters_msp){

binary_msp %>%
  filter(module_name != "core") %>%
  reshape2::melt(id.vars=c("genes.id","module_name")) %>%
  merge(., clusters_msp$partition %>%
          as.matrix %>%
          as.data.frame %>%
          tibble::rownames_to_column("variable") %>%
          dplyr::rename(id=V1),
        by=c("variable")) %>%
  group_by(genes.id) %>%
  mutate(s=sum(value),l=length(value)) %>%
  filter(s>0, s<l) %>%
  do(w = with(.,chisq.test(id,value,
                           simulate.p.value = TRUE))) %>%
  broom::tidy(w) %>%
  mutate(fdr=p.adjust(p.value, method="fdr")) %>%
  filter(fdr<0.05) -> test_msp

  return(test_msp)


}
