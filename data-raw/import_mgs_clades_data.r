mgs_clades_prop = read.csv2("data-raw/all_mgs_clades_prop.csv", row.names=1)
devtools::use_data(mgs_clades_prop, overwrite = TRUE)
