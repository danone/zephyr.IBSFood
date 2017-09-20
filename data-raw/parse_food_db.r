library(tidyr)
library(magrittr)
library(readxl)
library(dplyr)
library(stringdist)
library(magrittr)
library(reshape2)
library(DirichletMultinomial)

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



food_fab %>%
  rename(
    Livsmedelsnamn = Foodstuffs,
    Livsmedelsgrupp = food_group,
    Livsmedelsnummer = Code
   ) %>%
  melt(id.vars=c("Livsmedelsnamn", "Livsmedelsgrupp", "Livsmedelsnummer", "Gram")) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  mutate(value = (100/Gram) * value, Gram = (100/Gram) * Gram) %>%
  select(-Gram) %>%
  dcast(Livsmedelsnamn+Livsmedelsgrupp+Livsmedelsnummer~variable) %>%
  as_tibble() %>%
  colnames -> a

food_eng %>% colnames ->b


food_data %>%
  filter(Code %in% food_group_levels$Livsmedelsnummer) %>%
  melt(id.vars = c("Identity",  "Day", "Meal", "Code", "Foodstuffs", "Gram")) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  group_by(variable, Foodstuffs, Code ) %>%
  summarise(Gram = sum(Gram), value = sum(value)) %>%
  mutate(value = (100/Gram) * value) %>%
  select(-Gram) %>%
  dcast(Foodstuffs+Code~variable) %>%
  arrange(desc(`Fibers(g)`))




grp=2

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


fit = mclapply(1:7, dmn, count=as.matrix(tt), verbose=TRUE)


lplc <- sapply(fit, laplace)













