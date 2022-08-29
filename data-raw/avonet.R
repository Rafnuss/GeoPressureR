## code to prepare `avonet` dataset goes here

avonet <- utils::read.csv("data-raw/avonet_clements.csv")
usethis::use_data(avonet, overwrite = TRUE)
