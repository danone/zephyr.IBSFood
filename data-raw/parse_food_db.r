library(tidyr)
library(magrittr)
library(readxl)
library(dplyr)
library(stringdist)
library(magrittr)
library(reshape2)
library(DirichletMultinomial)
library(biomformat)

food_data = read_excel("data-raw/MOSAIC_big_food_db.xlsx")

food_swe = read_excel("data-raw/LivsmedelsDatabas-med-livsmedelsgrupper.xls",sheet = 1)[-1,]
food_eng = read_excel("data-raw/LivsmedelsDB_201709141657.xlsx")
food_fab = read_excel("data-raw/Fabrikanter.xlsx")
food_swe2 = read_excel("data-raw/LivsmedelsDB_201709151913_SWE.xlsx")


food_group = read_excel("data-raw/MOSAIC food groups.xlsx", sheet = "Food groups lvl")

food_group %<>%
  tidyr::separate(`Food groups lvl3`,
                  into = c("num_lvl3","Food groups lvl3"),
                  sep=", ", extra = "merge") %>%
  as.data.frame

# this code shows that this two database do not match with food ids
# food_swe %>%
#   merge(.,food_eng, by="Livsmedelsnummer", all.x = TRUE) %>%
#   as_tibble %>%
#   select(contains("kcal"),contains("Livsmedel")) %>%
#   mutate(deviation = abs(`Energi (kcal)(kcal)` - `Energy (kcal)`)) %>%
#   filter(deviation != 0) %>%
#   arrange(desc(deviation)) %>%
#   select(contains("Livsmedelsnamn"), deviation) %>% head %>% as.data.frame




tmp = food_swe %>% select(Livsmedelsnamn,Livsmedelsnummer,Livsmedelsgrupp, `Energi (kcal)(kcal)`)

tmp$id = NULL

a = food_swe$Livsmedelsnamn %>% gsub("[[:space:]]", "", .)
b = food_swe2$Livsmedelsnamn %>% gsub("[[:space:]]", "", .)


compteur_perfect_match = compteur_match_number = compteur_global_dist = NULL


for( i in 1:length(food_swe$Livsmedelsnamn)) {


  #perfect match
  id = match(a[i], b)

  if(!is.na(id)) {tmp$id[i] = id; compteur_perfect_match = c(compteur_perfect_match,i)} else {

    #match number id

    id = match(food_swe$Livsmedelsnummer[i], food_swe2$Livsmedelsnummer)

    if(!is.na(id)) {

      #test dist
      d = c(a[i], b[id]) %>%
        lapply(enc2utf8) %>%
        lapply(utf8ToInt) %>%
        seq_distmatrix(method="cosine") %>%
        as.numeric() %>%
        multiply_by(-1) %>%
        add(1)


      if(d < 0.85) {tmp$id[i] = NA} else {

        #test kcal

        if(food_swe$`Energi (kcal)(kcal)`[i] < food_swe2$`Energi (kcal)`[id] * 1.05 &
           food_swe$`Energi (kcal)(kcal)`[i] > food_swe2$`Energi (kcal)`[id] * 0.95) {

          tmp$id[i] = id; compteur_match_number = c(compteur_match_number,i)

        } else {

         tmp$id[i] = NA

        }
      }

    } else {


      #test dist global
      d = seq_dist(
        as.list(a[i]) %>%
          lapply(enc2utf8) %>%
          lapply(utf8ToInt),
        as.list(b) %>%
          lapply(enc2utf8) %>%
          lapply(utf8ToInt),
        method="cosine")

      id = which.min(d)

      d = 1 - d[which.min(d)]

      if(d < 0.85) {tmp$id[i] = NA} else {

        #test kcal

        if(food_swe$`Energi (kcal)(kcal)`[i] < food_swe2$`Energi (kcal)`[id] * 1.001 &
           food_swe$`Energi (kcal)(kcal)`[i] > food_swe2$`Energi (kcal)`[id] * 0.999) {

          tmp$id[i] = id; compteur_global_dist = c(compteur_global_dist,i)

        } else {


          tmp$id[i] = NA

        }
      }



    }




  }



}

tmp$id %>% is.na %>% table()
length(compteur_perfect_match)
length(compteur_match_number)
length(compteur_global_dist)

food_swe_eng =
  data.frame(
    tmp,
    Livsmedelsnamn_eng = food_eng$Livsmedelsnamn[tmp$id],
    check.names = FALSE) %>%
  as_tibble()


food_swe_eng_fab =
food_fab %>%
  rename(
    Livsmedelsnamn = Foodstuffs,
    Livsmedelsgrupp = food_group,
    Livsmedelsnummer = Code,

    `Energi (kcal)(kcal)` = `Energy kcal`
    ) %>%
  mutate(Livsmedelsnamn_eng = Livsmedelsnamn) %>%
  select(Livsmedelsnamn,
         Livsmedelsnummer,
         Livsmedelsgrupp,
         Livsmedelsnamn_eng,
         `Energi (kcal)(kcal)`
         ) %>%
  rbind(food_swe_eng %>% select(-id))


food_swe_eng_fab %>%
  merge(food_data %>% select(Code, Foodstuffs) %>% unique %>% mutate(used = "yes"), by.x = "Livsmedelsnummer", by.y="Code", all.y = TRUE, all.x = TRUE) %>%
  as_tibble() %>%
  select(Livsmedelsnamn, Livsmedelsnamn_eng, Foodstuffs,used, Livsmedelsnummer, Livsmedelsgrupp) %>%
  unique %>%
  write.csv2(file="food_item_mosaic.csv")



food_swe_eng_fab %>%
  merge(food_data %>%
      filter(Code != "Meal sum", !is.na(Code)) %>%
      select(Code, Foodstuffs) %>%
      unique %>%
      mutate(used = "yes"),
    by.x = "Livsmedelsnummer",
    by.y="Code",
    all.y = TRUE,
    all.x = TRUE) %>%
  as_tibble() %>%
  select(Livsmedelsnamn, Livsmedelsnamn_eng, Foodstuffs,used, Livsmedelsnummer, Livsmedelsgrupp) %>%
  unique %>%
  mutate(used = ifelse(is.na(used), "no", "yes")) %>%
  mutate(Foodstuffs = ifelse(is.na(Foodstuffs) & used == "yes", Livsmedelsnamn, Foodstuffs )) %>%
  mutate(Livsmedelsnamn_eng = ifelse(used == "yes", Foodstuffs, Livsmedelsnamn_eng)) %>% # keep original english name from the study
  merge(food_group,., by.y="Livsmedelsgrupp", by.x="num_lvl3", all.y=TRUE) -> food_group_levels


## the code below create an html data tree
# food_group_levels %>%
#   merge(food_swe %>% select(Livsmedelsnummer, `Energi (kcal)(kcal)`), by="Livsmedelsnummer" ) %>%
#   mutate(pathString=paste(`Food groups lvl0`,`Food groups lvl1`,`Food groups lvl2`,`Food groups lvl3`,Livsmedelsnamn, sep="/")) %>%
#   data.tree::as.Node() -> food_tree
#
# food_html = circlepackeR::circlepackeR(food_tree, size="Energi (kcal)(kcal)")
# htmlwidgets::saveWidget(food_html, "./data-raw/food_tree.html", selfcontained = TRUE)


## the code below create an html data tree for mosaic study
 # food_group_levels %>%
 #   merge(food_swe %>% select(Livsmedelsnummer, `Energi (kcal)(kcal)`), by="Livsmedelsnummer" ) %>%
 #     mutate(pathString=paste(`Food groups lvl0`,`Food groups lvl1`,`Food groups lvl2`,`Food groups lvl3`,Foodstuffs, sep="//")) %>%
 #   filter(used == "yes") %>%
 #   data.tree::as.Node(pathDelimiter = "//") -> food_tree
 #
 # food_html = circlepackeR::circlepackeR(food_tree, size="Energi (kcal)(kcal)")
 # htmlwidgets::saveWidget(food_html, "food_tree_mosaic.html", selfcontained = TRUE)



 ## the code below create an html data tree for mosaic study with fiber
 # food_group_levels %>%
 #   merge(food_swe %>% select(Livsmedelsnummer, `Fibrer(g)`), by="Livsmedelsnummer" ) %>%
 #   #mutate(`Fibrer(g)` =  `Fibrer(g)` + 0.01 ) %>%
 #   mutate(pathString=paste(`Food groups lvl0`,`Food groups lvl1`,`Food groups lvl2`,`Food groups lvl3`,Foodstuffs, sep="//")) %>%
 #   filter(used == "yes", `Fibrer(g)` != 0) %>%
 #   data.tree::as.Node(pathDelimiter = "//") -> food_tree
 #
 # food_html = circlepackeR::circlepackeR(food_tree, size="Fibrer(g)")
 # htmlwidgets::saveWidget(food_html, "food_tree_mosaic_fiber.html", selfcontained = TRUE)


# fabrikanter db and Livsmedels db do not have the same nutrient....
# food_fab %>%
#   rename(
#     Livsmedelsnamn = Foodstuffs,
#     Livsmedelsgrupp = food_group,
#     Livsmedelsnummer = Code
#    ) %>%
#   melt(id.vars=c("Livsmedelsnamn", "Livsmedelsgrupp", "Livsmedelsnummer", "Gram")) %>%
#   mutate(value = ifelse(is.na(value), 0, value)) %>%
#   mutate(value = (100/Gram) * value, Gram = (100/Gram) * Gram) %>%
#   select(-Gram) %>%
#   dcast(Livsmedelsnamn+Livsmedelsgrupp+Livsmedelsnummer~variable) %>%
#   as_tibble() %>%
#   colnames -> a
#
# food_eng %>% colnames ->b

#
# food_data %>%
#   filter(Code %in% food_group_levels$Livsmedelsnummer) %>%
#   melt(id.vars = c("Identity",  "Day", "Meal", "Code", "Foodstuffs", "Gram")) %>%
#   mutate(value = ifelse(is.na(value), 0, value)) %>%
#   group_by(variable, Foodstuffs, Code ) %>%
#   summarise(Gram = sum(Gram), value = sum(value)) %>%
#   mutate(value = (100/Gram) * value) %>%
#   select(-Gram) %>%
#   dcast(Foodstuffs+Code~variable) %>%
#   arrange(desc(`Fibers(g)`))
#


food_cluster_lvl4 = NULL

for(i in food_swe %>% pull(Livsmedelsgrupp) %>% unique %>% sort %>% .[1:34]) {

  grp=i

food_swe %>%
  filter(Livsmedelsgrupp == grp) %>%
  select(
    #-Livsmedelsnamn,
    -Livsmedelsnummer,
    -Livsmedelsgrupp,
    -`Energi (kJ)(kJ)`,
    -`Energi (kcal)(kcal)`
    ) %>%
  melt(id.vars="Livsmedelsnamn") %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  mutate(value = ifelse(grepl("(mg)",variable), value/(10^3), value)) %>%
  mutate(value = ifelse(grepl("(µg)",variable), value/(10^6), value)) %>%
  mutate(value = 10000000*value) %>%
  mutate(value = round(value)) %>%
  filter(grepl("g)", variable)) %>%
  dcast(Livsmedelsnamn~variable) %>%
  select(-contains("Summa")) %>%
  tibble::column_to_rownames("Livsmedelsnamn") -> tt


fit = mclapply(1:7, dmn, count=as.matrix(tt/1000), verbose=TRUE) # to avoid crash need to divide by 1000

lplc <- sapply(fit, laplace)

cl = mixture(fit[[which.min(lplc)]], assign = TRUE)
#
# res = data.frame(food_swe %>%
#              filter(Livsmedelsgrupp == grp) %>%
#              select(
#                Livsmedelsnamn,
#                Livsmedelsnummer,
#                Livsmedelsgrupp), cluster=paste(grp, cl, sep="_"))

res = data.frame(Livsmedelsnamn=names(cl), cl=paste(grp, cl, sep="_"))

food_cluster_lvl4 = rbind(food_cluster_lvl4,res)
}



food_group_levels %<>%
  merge(food_cluster_lvl4, by="Livsmedelsnamn", all.x=TRUE) %>%
  as_tibble() %>%
  mutate(cl = as.character(cl)) %>%
  dplyr::rename(`Food groups lvl4` = cl)


devtools::use_data(food_group_levels, overwrite = TRUE)
devtools::use_data(food_swe, overwrite = TRUE)

food_data %<>%
  filter(Code != "Meal sum", Meal != "Day sum")

devtools::use_data(food_data, overwrite = TRUE)



food_group_levels %>%
  merge(food_swe %>% select(Livsmedelsnummer, `Energi (kcal)(kcal)`), by="Livsmedelsnummer" ) %>%
    mutate(pathString=paste(`Food groups lvl0`,`Food groups lvl1`,`Food groups lvl2`,`Food groups lvl3`, `Food groups lvl4`, Foodstuffs, sep="//")) %>%
  filter(used == "yes") %>%
  data.tree::as.Node(pathDelimiter = "//") -> food_tree

food_html = circlepackeR::circlepackeR(food_tree, size="Energi (kcal)(kcal)")
htmlwidgets::saveWidget(food_html, "food_tree_subgroup_mosaic.html", selfcontained = TRUE)


food_group_levels %>%
  merge(food_swe %>% select(Livsmedelsnummer, `Fibrer(g)`), by="Livsmedelsnummer" ) %>%
  #mutate(`Fibrer(g)` =  `Fibrer(g)` + 0.01 ) %>%
  mutate(pathString=paste(`Food groups lvl0`,`Food groups lvl1`,`Food groups lvl2`,`Food groups lvl3`,`Food groups lvl4`,Foodstuffs, sep="//")) %>%
  filter(used == "yes", `Fibrer(g)` != 0) %>%
  data.tree::as.Node(pathDelimiter = "//") -> food_tree

food_html = circlepackeR::circlepackeR(food_tree, size="Fibrer(g)")
htmlwidgets::saveWidget(food_html, "food_tree_subgroup_mosaic_fiber.html", selfcontained = TRUE)





# create a biom file from food data


#create contingency table


food_data %>%
  merge(food_group_levels, by.x="Code",by.y="Livsmedelsnummer", all = FALSE) %>%
  select(-Foodstuffs.y) %>%
  dplyr::rename(Foodstuffs = Foodstuffs.x) %>%
  group_by(Identity, `Food groups lvl1`, `Food groups lvl2`, `Food groups lvl3`,`Food groups lvl4`) %>%
  summarise(Gram = sum(Gram)) %>%
  na.omit() %>%
  dcast(Identity ~ `Food groups lvl4`, value.var = "Gram",fill = 0 ) %>%
    tibble::column_to_rownames("Identity") %>%
  t() %>%
  as_tibble %>%
  round() -> food_otu

b = biomformat::make_biom(data = food_otu,
                          observation_metadata = food_group_levels %>%
                            select(`Food groups lvl1`, `Food groups lvl2`, `Food groups lvl3`,`Food groups lvl4`) %>%
                            unique %>%
                            na.omit() %>%
                            filter(`Food groups lvl4` %in% rownames(food_otu)) %>%
                            as.data.frame,
                          matrix_element_type = "int")


biomformat::write_biom(b, "inst/biom/food.biom")

biomformat::read_biom("inst/biom/food.biom")



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





food_group_levels %>%
  select(`Food groups lvl1`, `Food groups lvl2`, `Food groups lvl3`,`Food groups lvl4`) %>%
  unique %>%
  na.omit() %>%
  filter(`Food groups lvl4` %in% rownames(food_otu)) %>%
  as.data.frame %>%
  df2newick(.) %>%
  writeLines(con="inst/biom/food.tree")



nutrients_data =
readxl::read_excel("data-raw/MOSAIC food groups.xlsx", sheet = "Nutr & food groups") %>%
  filter(!is.na(kcal)) %>%
  #colnames() %>%
  select(1,10:58) %>% .[-c(143:145),]

devtools::use_data(nutrients_data, overwrite = TRUE)




























