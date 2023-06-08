#' @title Plots the AMSTAR halfdonut plot.
#'
#' @param data A dataset.
#'
#' @param r1 Outer circle radius.
#'
#' @param r2 Inner circle radius, should inferior  to `r1` value.
#'
#' @return halfdonut
#'
#' @export
#'
#' @examples


amstar2_halfdonut <- function(data = data, r1 = 1, r2 = 0.75){

  c(missing(r1), missing(r2))

  tb <- amstar2_table(data)

  tb$overall <- factor(tb$overall, levels = c("Critically Low", "Low", "Moderate", "High"))


  dt <- tb |>
    dplyr::select(overall) |>
    dplyr::count(overall)


  categories_fills <- c("Critically Low" = "#FFCD00", "Low" = "#FFE8AE",
                        "Moderate" = "#B4CFEE", "High" = "#004B87")


# create a table with the number of reviews and their percentages
  dt1 <- tb |>
    dplyr::select(overall) |>
    dplyr::count(overall) |>
    dplyr::mutate("Percentage (%)" = round(n/sum(n)*100, digits = 1)) |>
    dplyr::rename("Overall quality" = "overall",
                  "No. reviews" = "n")

# reformat the table
  dt2 <- gridExtra::tableGrob(t(dt1))


  halfdonut <- dt |>
    ggplot2::ggplot() +
    ggtricks::geom_donut_slice(ggplot2::aes(cat = forcats::fct_rev(overall), val = n, fill = forcats::fct_rev(overall)),
                               r1 = r1, r2 = r2,
                               slice_angle = 180) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_manual(values = categories_fills) +
    ggthemes::theme_fivethirtyeight(base_size = 14) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, title = "Rating")) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          legend.position = c(0.5, 0.45),
          legend.text = ggplot2::element_text(size = 14),
          legend.title = ggplot2::element_text(size = 14, face = "bold"),
          legend.key.size = grid::unit(0.8, "cm")) +
    ggplot2::annotation_custom(dt2,
                               xmin = -1.05, ymin = -0.65,
                               xmax = 1)

  return(halfdonut)

}
