library(tidyr)
library(magrittr)
library(readxl)
library(dplyr)
library(stringdist)

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




food_swe %>%
  mutate(Livsmedelsnamn = gsub(" rå\b", "", Livsmedelsnamn)) %>%
  #mutate(Livsmedelsnamn = gsub("Bouillabaisse fisksoppa", "Fisksoppa2", Livsmedelsnamn)) %>%
  mutate(Livsmedelsnamn = gsub(" %", "%", Livsmedelsnamn)) %>%
  select(Livsmedelsnamn, Livsmedelsnummer, Livsmedelsgrupp, contains("kcal")) %>%
  merge(.,
        food_swe2 %>%
          mutate(Livsmedelsnamn = gsub(" rå\b", "", Livsmedelsnamn)) %>%
          #mutate(Livsmedelsnamn = gsub("Bouillabaisse fisksoppa", "Fisksoppa1", Livsmedelsnamn)) %>%
          mutate(Livsmedelsnamn = gsub("\\ %", "%", Livsmedelsnamn)) %>%
          select(Livsmedelsnamn, Livsmedelsnummer, contains("kcal")),

        by="Livsmedelsnamn", all.x = TRUE) %>%
  as_tibble %>%
  merge(.,
        food_swe2 %>%
          mutate(Livsmedelsnamn = gsub(" rå\b", "", Livsmedelsnamn)) %>%
          #mutate(Livsmedelsnamn = gsub("Bouillabaisse fisksoppa", "Fisksoppa1", Livsmedelsnamn)) %>%
          mutate(Livsmedelsnamn = gsub("\\ %", "%", Livsmedelsnamn)) %>%
          select(Livsmedelsnamn, Livsmedelsnummer, contains("kcal")),

        by.y="Livsmedelsnummer",
        by.x="Livsmedelsnummer.x", all.x = TRUE) %>%
    mutate(Livsmedelsnummer.x = Livsmedelsnummer.x %>%
             replace(Livsmedelsnummer.x==312, 5864) %>%
             replace(Livsmedelsnummer.x==892, 5866) %>%
             replace(Livsmedelsnummer.x==1354, 2108)
          ) %>%
  as_tibble() %>%
  #filter(Livsmedelsnamn.x != Livsmedelsnamn.y | Livsmedelsnummer.x != Livsmedelsnummer.y, `Energi (kcal)(kcal)` != `Energi (kcal).x`) %>%
  #filter(Livsmedelsnummer.x != Livsmedelsnummer.y) %>%
  #select(Livsmedelsnamn.x, Livsmedelsnamn.y, `Energi (kcal)(kcal)`, `Energi (kcal).x` , Livsmedelsnummer.x, Livsmedelsnummer.y ) %>% head(50)
  rename(Livsmedelsnummer  = Livsmedelsnummer.x) %>%
  rename(Livsmedelsnummer2 = Livsmedelsnummer.y) %>%
  rename(Livsmedelsnamn_swe2   = Livsmedelsnamn.y) %>%
  rename(Livsmedelsnamn_swe1   = Livsmedelsnamn.x) %>%
  select(-`Energi (kcal).x`) %>%
  select(-`Energi (kcal).y`) %>%
  select(-`Energi (kcal)(kcal)`) %>%
  merge(.,  food_eng, by.y="Livsmedelsnummer", by.x="Livsmedelsnummer", all.x = TRUE) %>%
  as_tibble()







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







