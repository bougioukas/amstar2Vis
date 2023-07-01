library(amstar2)

DATASET <- readxl::read_excel(system.file("extdata", "sample_dat.xlsx", package = "amstar2"))

tb <- amstar2_table(DATASET)
