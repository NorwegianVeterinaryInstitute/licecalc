## code to prepare internall datasets for new lice model
## files taken from project local storage
## /34055_NewTechAqua/WP2/licecalc/data/
## these data need to be documented
## see 'https://r-pkgs.org/data.html'

infestation_pressure_all = read.table("data-raw/LarverTotaltFra2012.txt",
                                      sep = "\t",
                                      dec = ",")

usethis::use_data(infestation_pressure_all, overwrite = TRUE)



sea_temperature_all = read.table("data-raw/Sjoetemp.txt",
                                 sep = "\t",
                                 dec = ",")

usethis::use_data(sea_temperature_all, overwrite = TRUE)
