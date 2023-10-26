## code to prepare `lusedemodata` dataset goes here

## d0 in the original code

luse_demo_data <- read.csv("data-raw/lusedata.csv", header = T, dec = ",", sep = ";")

usethis::use_data(luse_demo_data, overwrite = TRUE)

## create empty df for data import

empty <- luse_demo_data

empty$adult_females <- NA_integer_
empty$other_motiles <- NA_integer_
empty$sessiles <- NA_integer_
empty$fish_weight <- NA_integer_
empty$fish_abundance <- NA_integer_
empty$cleaner_fish <- NA_integer_

usethis::use_data(empty, overwrite = TRUE)


## d in the original code

siste_uker_demodata <- read.table("data-raw/MobileTotaltFra2012SisteUker.txt", dec = ",")

usethis::use_data(siste_uker_demodata, overwrite = TRUE)

## dAlle in the original code

total_fra_demodata <- read.csv("data-raw/MobileTotaltFra2012.txt", header = T, dec = ",", sep = "\t")

usethis::use_data(total_fra_demodata, overwrite = TRUE)
