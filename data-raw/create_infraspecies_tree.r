library(dplyr)


# import infraspecies table

infraspecies_table_full = read.csv2("data-raw/infraspecies_table_full.csv")


#create tree

## recursion function
traverse <- function(a,i,innerl){
  if(i < (ncol(df))){
    alevelinner <- as.character(unique(df[which(as.character(df[,i])==a),i+1]))
    desc <- NULL
    if(length(alevelinner) == 1) (newickout <- traverse(alevelinner,i+1,innerl))
    else {
      for(b in alevelinner) desc <- c(desc,traverse(b,i+1,innerl))
      il <- NULL; if(innerl==TRUE) il <- a
      (newickout <- paste("(",paste(paste0(desc),collapse=","),")",il,sep=""))
    }
  }
  else { (newickout <- a) }
}

## data.frame to newick function
df2newick <- function(df, innerlabel=FALSE){
  alevel <- as.character(unique(df[,1]))
  newick <- NULL
  for(x in alevel) newick <- c(newick,traverse(x,1,innerlabel))
  (newick <- paste("(",paste(newick,collapse=","),");",sep=""))
}





infraspecies_table_full %>%
  select(`superkingdom`, `phylum`, `class`,`order`, family, genus,species,msp_name, msp_name_partition ) %>%
  unique %>%
  mutate(root="root") %>%
  select(root, `superkingdom`, `phylum`, `class`,`order`, family, genus,species,msp_name, msp_name_partition ) %>%
  #head() %>%
  as.data.frame -> df

innerlabel = FALSE
alevel <- as.character(unique(df[,1]))
newick <- NULL
for(x in alevel) newick <- c(newick,traverse(x,1,innerlabel))
(newick <- paste("(",paste(newick,collapse=","),");",sep=""))
writeLines(newick, con="inst/biom/infraspecies.tree")









