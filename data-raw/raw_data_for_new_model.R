# code to prepare internall datasets for new lice model
# files taken from project local storage
# /34055_NewTechAqua/WP2/licecalc/data/
# these data need to be documented
# see 'https://r-pkgs.org/data.html'

infestation_pressure_all = read.table("data-raw/LarverTotaltFra2012.txt",
                                      sep = "\t",
                                      dec = ",")

usethis::use_data(infestation_pressure_all, overwrite = TRUE)



sea_temperature_all = read.table("data-raw/Sjoetemp.txt",
                                 sep = "\t",
                                 dec = ",")

usethis::use_data(sea_temperature_all, overwrite = TRUE)


# data for lice age.
# af - adult lice
# om - mobile lice
# fx - sessile lice

adult_lice = read.table("data-raw/VoksneHunnlus.txt", sep = "\t", dec = ",")

usethis::use_data(adult_lice, overwrite = TRUE)


mobile_lice = read.table("data-raw/BevegligeLus.txt", sep = "\t", dec = ",")

usethis::use_data(mobile_lice, overwrite = TRUE)

# Note: there is an error in the "Fastsittendelus.txt"
# example file, so this does not work 100 % yet
sessile_lice = read.table("data-raw/Fastsittendelus.txt",
                          sep = "\t",
                          dec = ",")

usethis::use_data(sessile_lice, overwrite = TRUE)
