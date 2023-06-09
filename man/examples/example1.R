DATASET <- readxl::read_excel(system.file('extdata','dat.xlsx', package = 'amstar2'))

tb <- amstar2_table(DATASET)
