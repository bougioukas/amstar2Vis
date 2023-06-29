#' @title Generates the AMSTAR 2 main table
#'
#' @description It creates a nice-looking “gt” table that includes the item ratings and the overall confidence rating for each SR according to AMSTAR 2 guidelines.
#'
#' @param data A dataset containing the item ratings.
#'
#' @return amstar_gtable
#'
#' @example man/examples/example1.R
#'
#' @export

amstar2_gtable <- function(data = data){

  # select the AMSTAR items
  amstar2_gt <- amstar2_table(data)


  # items
  levels(amstar2_gt$`1. PICO components`) <- c("Yes", "No")
  amstar2_gt$`1. PICO components` <- factor(amstar2_gt$`1. PICO components`, levels = c("Yes", "No"))
  categories_fills1 = c("#3a5e8cFF", "#ffcf20FF")

  levels(amstar2_gt$`2.* Preestablished protocol`) <- c("Yes", "Partial Yes", "No")
  amstar2_gt$`2.* Preestablished protocol` <- factor(amstar2_gt$`2.* Preestablished protocol`, levels = c("Yes", "Partial Yes", "No"))
  categories_fills2 = c("#3a5e8cFF", "#35b779", "#ffcf20FF")

  levels(amstar2_gt$`3. Explanation of inlcuded studies' design`) <- c("Yes", "No")
  amstar2_gt$`3. Explanation of inlcuded studies' design` <- factor(amstar2_gt$`3. Explanation of inlcuded studies' design`, levels = c("Yes", "No"))
  categories_fills3 = c("#3a5e8cFF", "#ffcf20FF")

  levels(amstar2_gt$`4.* Comprehensive search strategy`) <- c("Yes", "Partial Yes", "No")
  amstar2_gt$`4.* Comprehensive search strategy` <- factor(amstar2_gt$`4.* Comprehensive search strategy`, levels = c("Yes", "Partial Yes", "No"))
  categories_fills4 = c("#3a5e8cFF", "#35b779", "#ffcf20FF")

  levels(amstar2_gt$`5. Duplicate study selection`) <- c("Yes", "No")
  amstar2_gt$`5. Duplicate study selection` <- factor(amstar2_gt$`5. Duplicate study selection`, levels = c("Yes", "No"))
  categories_fills5 = c("#3a5e8cFF", "#ffcf20FF")

  levels(amstar2_gt$`6. Duplicate data extraction`) <- c("Yes", "No")
  amstar2_gt$`6. Duplicate data extraction` <- factor(amstar2_gt$`6. Duplicate data extraction`, levels = c("Yes", "No"))
  categories_fills6 = c("#3a5e8cFF", "#ffcf20FF")

  levels(amstar2_gt$`7.* List of excluded studies and justification`) <- c("Yes", "Partial Yes", "No")
  amstar2_gt$`7.* List of excluded studies and justification` <- factor(amstar2_gt$`7.* List of excluded studies and justification`, levels = c("Yes", "Partial Yes", "No"))
  categories_fills7 = c("#3a5e8cFF", "#35b779", "#ffcf20FF")

  levels(amstar2_gt$`8. Description of included studies`) <- c("Yes", "Partial Yes", "No")
  amstar2_gt$`8. Description of included studies` <- factor(amstar2_gt$`8. Description of included studies`, levels = c("Yes", "Partial Yes", "No"))
  categories_fills8 = c("#3a5e8cFF", "#35b779", "#ffcf20FF")

  levels(amstar2_gt$`9.* Risk of bias (RoB) assessment`) <- c("Yes", "Partial Yes", "No")
  amstar2_gt$`9.* Risk of bias (RoB) assessment` <- factor(amstar2_gt$`9.* Risk of bias (RoB) assessment`, levels = c("Yes", "Partial Yes", "No"))
  categories_fills9 = c("#3a5e8cFF", "#35b779", "#ffcf20FF")

  levels(amstar2_gt$`10. Funding sources`) <- c("Yes", "No")
  amstar2_gt$`10. Funding sources` <- factor(amstar2_gt$`10. Funding sources`, levels = c("Yes", "No"))
  categories_fills10 = c("#3a5e8cFF", "#ffcf20FF")

  levels(amstar2_gt$`11.* Use of appropriate statistical methods`) <- c("Yes", "No", "No MA")
  amstar2_gt$`11.* Use of appropriate statistical methods` <- factor(amstar2_gt$`11.* Use of appropriate statistical methods`, levels = c("Yes", "No", "No MA"))
  categories_fills11 = c("#3a5e8cFF", "#ffcf20FF", "#303030")

  levels(amstar2_gt$`12. RoB impact on synthesized results`) <- c("Yes", "No", "No MA")
  amstar2_gt$`12. RoB impact on synthesized results` <- factor(amstar2_gt$`12. RoB impact on synthesized results`, levels = c("Yes", "No", "No MA"))
  categories_fills12 = c("#3a5e8cFF", "#ffcf20FF", "#303030")

  levels(amstar2_gt$`13.* Results interpretation with RoB reference`) <- c("Yes", "No")
  amstar2_gt$`13.* Results interpretation with RoB reference` <- factor(amstar2_gt$`13.* Results interpretation with RoB reference`, levels = c("Yes", "No"))
  categories_fills13 = c("#3a5e8cFF", "#ffcf20FF")

  levels(amstar2_gt$`14. Heterogeneity explanation`) <- c("Yes", "No")
  amstar2_gt$`14. Heterogeneity explanation` <- factor(amstar2_gt$`14. Heterogeneity explanation`, levels = c("Yes", "No"))
  categories_fills14 = c("#3a5e8cFF", "#ffcf20FF")

  levels(amstar2_gt$`15.* Publication/ small study bias investigation`) <- c("Yes", "No", "No MA")
  amstar2_gt$`15.* Publication/ small study bias investigation` <- factor(amstar2_gt$`15.* Publication/ small study bias investigation`, levels = c("Yes", "No", "No MA"))
  categories_fills15 = c("#3a5e8cFF", "#ffcf20FF", "#303030")

  levels(amstar2_gt$`16. Conflict of interest declaration`) <- c("Yes", "No")
  amstar2_gt$`16. Conflict of interest declaration` <- factor(amstar2_gt$`16. Conflict of interest declaration`, levels = c("Yes", "No"))
  categories_fills16 = c("#3a5e8cFF", "#ffcf20FF")


  # overall
  levels(amstar2_gt$overall) <- c("Critically Low", "Low", "Moderate", "High")
  amstar2_gt$overall <- factor(amstar2_gt$overall, levels = c("Critically Low", "Low", "Moderate", "High"))
  categories_fills <- c("Critically Low" = "#FD9567FF", "Low" = "#FDE4A6FF", "Moderate" = "#9F2F7FFF", "High" = "#00021E")


# gt table
amstar_gtable <- amstar2_gt |>
  dplyr::rename("Overall confidence" = "overall") |>
  gt::gt() |>
  gt::data_color(
      columns = 2,
      method = "factor",
      palette = categories_fills1
    ) |>
  gt::data_color(
      columns = 3,
      method = "factor",
      palette = categories_fills2
    )|>
  gt::data_color(
      columns = 4,
      method = "factor",
      palette = categories_fills3
    )|>
  gt::data_color(
      columns = 5,
      method = "factor",
      palette = categories_fills4
    )|>
  gt::data_color(
      columns = 6,
      method = "factor",
      palette = categories_fills5
    ) |>
  gt::data_color(
      columns = 7,
      method = "factor",
      palette = categories_fills6
    ) |>
  gt::data_color(
      columns = 8,
      method = "factor",
      palette = categories_fills7
    ) |>
  gt::data_color(
      columns = 9,
      method = "factor",
      palette = categories_fills8
    ) |>
  gt::data_color(
      columns = 10,
      method = "factor",
      palette = categories_fills9
    ) |>
  gt::data_color(
      columns = 11,
      method = "factor",
      palette = categories_fills10
    ) |>
  gt::data_color(
      columns = 12,
      method = "factor",
      palette = categories_fills11
    )|>
  gt::data_color(
      columns = 13,
      method = "factor",
      palette = categories_fills12
    )|>
  gt::data_color(
      columns = 14,
      method = "factor",
      palette = categories_fills13
    )|>
  gt::data_color(
      columns = 15,
      method = "factor",
      palette = categories_fills14
    )|>
  gt::data_color(
      columns = 16,
      method = "factor",
      palette = categories_fills15
    )|>
  gt::data_color(
      columns = 17,
      method = "factor",
      palette = categories_fills16
    ) |>
  gt::data_color(
      columns = 18,
      method = "factor",
      palette = categories_fills
    ) |>
  gt::cols_align(
    align = "center",
    columns = 2:18
  ) |>
  gt::tab_footnote(
    footnote = "*Asterisk indicates a critical item (domain) based on the source publication of AMSTAR 2 tool (Shea et al. 2017; doi: 10.1136/bmj.j4008)."
  ) |>
  gt::tab_footnote(
    footnote = "Possible responses: Yes/Partial Yes/No. A 'Partial Yes' response is evaluated positively to the overall confidence rating.",
    gt::cells_column_labels(columns = c(3, 5, 8, 9, 10))
  ) |>
  gt::tab_footnote(
    footnote = "Item response depends on separate assessment of randomized controlled trials  (RCTs) and non-randomized studies of healthcare interventions (NRSI) in the review.",
    gt::cells_column_labels(columns = c(10, 12))
  ) |>
  gt::tab_footnote(
    footnote = "Possible responses: Yes/No/No MA. A 'No MA' response stands for 'no meta-analysis conducted' and does not affect the overall confidence rating.",
    gt::cells_column_labels(columns = c(12, 13, 16))
  ) |>
  gt::tab_footnote(
    footnote = "Critically Low/Low/Moderate/High confidence in the results of the review according to AMSTAR 2 rating scheme (see Shea et al. 2017; doi: 10.1136/bmj.j4008). Critically Low: more than one critical flaw with or without non-critical weaknesses, Low: One critical flaw with or without non-critical weaknesses, Moderate: No critical flaws but more than one non-critical weakness, High: No flaws at all or one non-critical weakness.",
    gt::cells_column_labels(columns = 18)
  ) |>
  gt::tab_spanner(
    label = "AMSTAR 2 items",
    columns = 2:17) |>
  gt::tab_options(
    column_labels.font.weight = "bold"
  ) |>
  gt::tab_style(
    style = gt::cell_borders(
      sides = c("left"),
      color = "gray60",
      weight = gt::px(5.0),
      style = "solid"
    ),
    locations = gt::cells_body(columns = 18)
    ) |>
  gt::tab_style(
    style = gt::cell_borders(
      sides = "bottom",
      color = "black",
      weight = gt::px(3.0)
    ),
    locations = gt::cells_body(rows = nrow(amstar2_gt))
  )

  return(amstar_gtable)

}

