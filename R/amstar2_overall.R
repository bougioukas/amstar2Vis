#' @title Plots the AMSTAR overall plot.
#'
#' @param data A dataset.
#'
#' @param r1 Outer circle radius. Default is `r1 = 1`.
#'
#' @param r2 Inner circle radius, should inferior  to `r1` value. Default is `r2 = 0.75`.
#'
#' @param table_size It controls the size of the inset table. Default is `table_size = 14`.
#'
#' @param caption_size It controls the font size of the caption. Default is `caption_size = 12`.
#'
#' @return halfdonut
#'
#' @example man/examples/example3.R
#'
#' @export


amstar2_overall <- function(data = data, r1 = 1, r2 = 0.75, table_size = 14, caption_size = 12){

  c(missing(r1), missing(r2), missing(table_size), missing(caption_size))

  tb <- amstar2_table(data)

# order the categories of assessment
  tb$overall <- factor(tb$overall, levels = c("Critically Low", "Low", "Moderate", "High"))

  dt <- tb |>
    dplyr::select(overall) |>
    dplyr::count(overall)


# create a a color palette with four distinct colors corresponding to each category
  categories_fills <- c("Critically Low" = "#FD9567FF", "Low" = "#FDE4A6FF", "Moderate" = "#9F2F7FFF", "High" = "#150E38FF")


# create a table with the number of reviews and their percentages
  dt1 <- tb |>
    dplyr::select(overall) |>
    dplyr::count(overall) |>
    dplyr::mutate("Percentage (%):" = round(n/sum(n)*100, digits = 1)) |>
    dplyr::rename("*Overall:" = "overall",
                  "Number of reviews:" = "n")

# reformat the table
  dt2 <- gridExtra::tableGrob(t(dt1), theme = gridExtra::ttheme_default(base_size = table_size))


# create a half donut plot
  halfdonut <- dt |>
    ggplot2::ggplot() +
    ggtricks::geom_donut_slice(ggplot2::aes(cat = forcats::fct_rev(overall), val = n, fill = forcats::fct_rev(overall)),
                               r1 = r1, r2 = r2,
                               slice_angle = 180) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_manual(values = categories_fills) +
    ggthemes::theme_fivethirtyeight(base_size = 14) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, title = paste0("Rating Scale, \nN=",  nrow(tb)))) +
    ggplot2::labs(caption = "*Critically Low/Low/Moderate/High confidence in the results of the review according to AMSTAR 2 rating scheme. \n For more details see the source publication of AMSTAR 2 (Shea et al. 2017; doi: 10.1136/bmj.j4008).") +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, size = caption_size, face = "italic"),
                   axis.text = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   legend.position = c(0.5, 0.15),
                   legend.text = ggplot2::element_text(size = 14),
                   legend.title = ggplot2::element_text(size = 14, face = "bold"),
                   legend.key.size = grid::unit(0.8, "cm")) +
    ggplot2::annotation_custom(dt2,
                               xmin = -1.05, ymin = -0.35,
                               xmax = 1)

  return(halfdonut)

}
