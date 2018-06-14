enterotypes = read.csv("data-raw/IBS_enterotypes.csv")[,-1]

cazy_counts = read.csv2("data-raw/cazy_counts.csv",row.names=1)

cazotypes = read.csv2("data-raw/cazotypes.csv",row.names=1)





devtools::use_data(enterotypes)
devtools::use_data(cazy_counts)
devtools::use_data(cazotypes)
