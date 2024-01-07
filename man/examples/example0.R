library(amstar2Vis)

DATASET <- readxl::read_excel(system.file("extdata", "sample_dat.xlsx", package = "amstar2Vis"))

tb <- amstar2_table(DATASET)
