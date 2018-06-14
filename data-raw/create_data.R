library(readr)

report = read.csv("data-raw/counting_report.csv", header=T, check.names=FALSE)
devtools::use_data(report)


counts = read_tsv("data-raw/counts_mapping.txt", n_max=4*10^6)

names(counts) = gsub("V4","",gsub("Lactulose_" , "", names(counts)))
devtools::use_data(counts, compress=FALSE)

metadata = read.csv2("data-raw/ibs_metadata.csv", header=TRUE, check.names=FALSE, row.names=1)
metadata$SSgroup    = factor(as.character(metadata$SSgroup), 
                                    levels=c("control","remission","mild","moderate","severe"))
                                    
levels(metadata$SSgroup) = c("control","mild","mild","moderate","severe") #merge remission with mild

metadata[,"IBS_subtypes"] = as.factor(paste(metadata[,"IBS_subtypes"], metadata[,"Health"], sep="_"))

levels(metadata[,"IBS_subtypes"])= c("IBS-C","IBS-D","IBS-M","IBS-U","Healthy", NA)

devtools::use_data(metadata, overwrite=TRUE)

MGS_CAG_dependency = read.csv2("data-raw/nbt.2939-S7.csv", header=T)[,c(1:3,6)]

MGS_CAG_dependency[,1] = gsub("MGS|CAG", "GU", MGS_CAG_dependency[,1])
MGS_CAG_dependency[,2] = gsub("MGS|CAG", "GU", MGS_CAG_dependency[,2])

devtools::use_data(MGS_CAG_dependency, overwrite=TRUE)


diet = read.csv2("data-raw/MOSAIC_diet_database.csv", header=T, check.names=FALSE)
devtools::use_data(diet, overwrite=TRUE)


library(magrittr)
library(dplyr)


MOSAIC_food_groups <- readxl::read_excel("~/Danone/Lactulose/IBSMetagenomics/MOSAIC food groups.xlsx", 
                                                              sheet = "Nutr & food groups")[1:251,]

MOSAIC_food_groups %<>% as.data.frame


row.names(MOSAIC_food_groups) = MOSAIC_food_groups[,1]
MOSAIC_food_groups            = MOSAIC_food_groups[,-1]


food_groups_idx = MOSAIC_food_groups %>% colnames %>% grep("^[0-9]", .)
ind_select      = MOSAIC_food_groups[, c(food_groups_idx  )] %>% t %>% apply(.,2, function(x){ length(which(is.na(x))) != 33} )
food_groups     = MOSAIC_food_groups[ind_select, food_groups_idx] %>% t


food_groups[is.na(food_groups)] = 0

days_intake = rep(4, dim(food_groups)[2])

days_intake[grep("\\.",food_groups %>% colnames)] = 3

food_groups = t(t(food_groups)/days_intake)

colnames(food_groups) %<>% gsub(", 3d","",.)

food_groups_category = 

c(

  rep("Cereals",6), rep("Dairy product", 6), "Eggs", "Fats", 
  rep("Meat",3), rep("Fish",2), rep("Vegetables",4), 
  rep("Fruits",2), rep("Nutsandseeds", 1), rep("Sweets",3),
  "Dietdrinks",	"Alcohol",	"Coffeeandtea",	"Legumes"
  )

food_groups = data.frame(food_groups, category=food_groups_category)

#apply(food_groups %>% select(-category), 2, tapply,food_groups$category , sum) %>% t


devtools::use_data(food_groups, overwrite=TRUE)


