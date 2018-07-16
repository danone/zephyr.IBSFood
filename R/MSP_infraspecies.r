#' @title binarize MSP
#' @description convert read abundance matrix from MSP to a binary matrix
#' @param msp_counts msp count matrix
#' @param cutoff_med average read number gene, Default: 2
#' @return a binary matrix
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[reshape2]{melt}},\code{\link[reshape2]{cast}}
#' @rdname binarize_MSP
#' @export
#' @importFrom reshape2 melt dcast
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

#' @title partitionize MSP
#' @description the aim of this function is to find sample cluster base on gene presence/absence per MSP
#' @param binary_msp MSP binary matrix
#' @return clusters from fpc::clusterboot
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ade4]{dist.binary}}
#'  \code{\link[fpc]{clusterboot}},\code{\link[fpc]{kmeansCBI}}
#' @rdname partitionize_MSP
#' @export
#' @importFrom ade4 dist.binary
#' @importFrom fpc clusterboot pamkCBI
partitionize_MSP = function(binary_msp) {


  #check colnames
  if(c("genes.id","module_name") %in% colnames(msp_counts) %>%
     as.numeric() %>% sum() != 2) stop("binary_msp did not contains all useful parameters")

binary_msp %>% filter(module_name  != "core") %>%
  select(-module_name,-genes.id) %>% as.matrix %>%
  t %>%
  ade4::dist.binary(.,method=1) %>%
  fpc::clusterboot(.,B=100,
                   bootmethod="subset",
                   clustermethod=fpc::pamkCBI,
                   count=FALSE,
                   k=1:6, showplot=FALSE) -> clusters_msp

# binary_msp %>% filter(module_name  != "core") %>%
#   tibble::column_to_rownames("genes.id") %>%
#   select(-module_name) %>% as.matrix %>%
#   #t %>%
#   dist(.,method="binary") %>%
#   fpc::clusterboot(.,B=10,
#                    bootmethod="subset",
#                    clustermethod=fpc::pamkCBI, count=FALSE,
#                    k=1:6, showplot=FALSE) -> clusters_msp_gene

# binary_msp %>% filter(module_name  != "core") %>%
#   tibble::column_to_rownames("genes.id") %>%
#   select(-module_name) %>% as.matrix %>%
#   #t %>%
#   dist(.,method="binary") %>%
#   fpc::pamk(data=., krange = clusters_msp$nccl,criterion = "ch", seed = 444) -> clusters_msp_gene

clusters_msp_gene = NULL

return(list(sample=clusters_msp,gene=clusters_msp_gene))

}

#' @title plot MSP with clusters
#' @description function to visualize MSP clustering
#' @param binary_msp MSP binary matrix
#' @param clusters_msp MSP sample clusters
#' @param clusters_msp_gene MSP gene clusters
#' @return heatmap plot
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[viridis]{reexports}}
#' @rdname plot_MSP
#' @export
#' @importFrom viridis viridis
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
            #as.matrix %>% .[order(clusters_msp_gene$partition),order(clusters_msp$partition)]
            as.matrix %>% .[,order(clusters_msp$partition)]
            ,
          labRow=NA, Colv = NA,  keep.dendro=FALSE,
          distfun = function(x) dist(x,method="binary"),
          hclustfun = function(x) hclust(x,method="centroid"),
          scale = "none",cexRow = 0.01,
          ColSideColors = viridis::viridis(clusters_msp$nccl)[clusters_msp$partition %>% as.factor %>% as.numeric][order(clusters_msp$partition)],
          #RowSideColors = viridis::magma(clusters_msp_gene$nc)[clusters_msp_gene$pamobject$clustering %>% as.factor %>% as.numeric][order(clusters_msp_gene$pamobject$clustering)],

          col=c("black","yellow"))





}

#' @title test gene MSP as function of sample clusters
#' @description the aim of this fonction is to test whether gene prevalence is associated with specific sample clusters
#' @param binary_msp MSP binary matrix
#' @param clusters_msp MSP cluters
#' @return statistical table
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[dplyr]{select}}
#'  \code{\link[broom]{tidy}}
#' @rdname test_MSP_gene
#' @export
#' @importFrom reshape2 melt
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename
#' @importFrom broom tidy
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
  do(w = with(.,suppressWarnings(chisq.test(id,value,
                           simulate.p.value = FALSE)))) %>%
  broom::tidy(w) %>%
  ungroup() %>%
  mutate(fdr=p.adjust(p.value, method="fdr")) %>%
  filter(fdr<0.05) %>%
  select(genes.id,p.value,fdr) -> test_msp

  return(test_msp)


}


#' @title summarize MSP
#' @description FUNCTION_DESCRIPTION
#' @param binary_msp MSP binary matrix
#' @param clusters_msp MSP clusters
#' @param test_msp MSP test object
#' @return a global summary of MSP analysis
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[reshape2]{melt}},\code{\link[reshape2]{cast}}
#' @rdname summarize_MSP_analysis
#' @export
#' @importFrom reshape2 melt dcast
summarize_MSP_analysis = function(binary_msp,clusters_msp,test_msp){
stop("this function is not ready")
  binary_msp %>%
    filter(genes.id %in% names(clusters_msp$gene$partition)) %>%
    merge(., clusters_msp$gene$partition %>%
            as.matrix %>% as.data.frame,
          by.x="genes.id", by.y="row.names") %>%
    rename(gene_cl = V1) %>%
    reshape2::melt(id.vars=c("genes.id","module_name","gene_cl")) %>%
    merge(.,clusters_msp$sample$partition %>%
            as.matrix %>% as.data.frame, by.x="variable", by.y="row.names") %>%
    rename(msp_cl = V1) %>%
    group_by(msp_cl,gene_cl, genes.id) %>%
    summarize(prop = sum(value)/n()) %>%
    reshape2::dcast(genes.id+gene_cl~msp_cl, value.var="prop") %>%
    merge(.,test_msp, by="genes.id", all.x=TRUE) %>%
    mutate(p.value = ifelse(is.na(p.value),1,p.value ), fdr = ifelse(is.na(fdr),1,fdr )) %>%
    filter(fdr<0.05)

}







convert_MSP_to_infraspecies =  function(MSP_abundance,clusters_msp_sample, msp_counts){


  msp_name_target = msp_counts$msp_name %>% unique


  MSP_abundance %>%
    filter(msp_name == msp_name_target) %>%
    reshape2::melt(id.vars="msp_name") %>%
    merge(.,clusters_msp_sample$partition %>%
            as.matrix %>% as.data.frame %>%
            tibble::rownames_to_column("sample_id"), by.x="variable",by.y="sample_id", all.x=TRUE) %>%
    rename(partition = V1) %>%
    mutate(partition =  ifelse(is.na(partition),"unassigned",paste0("cl_",partition))) -> infraspecies_abundance

  return(infraspecies_abundance)

}










