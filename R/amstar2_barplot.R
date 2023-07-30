#' @title Creates a publication-ready stacked barplot based on the ggplot2 package.
#'
#' @description It shows the distribution of ratings (“Yes”, “Partial Yes”, “No”, “No MA”) as percentages of SRs for each AMSTAR 2 item.
#'
#' @param data A dataset containing the item ratings.
#'
#' @param font_size A number that controls the aesthetic of font size of the percentages in the bars. Default is `font_size = 3.5`.
#'
#' @param font_color A color that controls the aesthetic of color of the percentages in the bars. Default is `font_color = "white"`.
#'
#' @param bar_width A number that controls the width between the bars. Default is `bar_width = 0.8`.
#'
#' @return A “ggplot2”-based stacked barplot.
#'
#' @example man/examples/example2.R
#'
#' @export


amstar2_barplot <- function(data = data, font_size = 3.5, font_color = "white", bar_width = 0.8){

  c(missing(font_size), missing(font_color), missing(bar_width))

  n <- prop <- item <- assessment <- NULL

# select the AMSTAR items
  amstar2_data <- amstar2_table(data) |>
    dplyr::select(2:17)

# transform the data to long format
  amstar2_data_long <- tidyr::pivot_longer(amstar2_data, cols = 1:16, names_to = "item", values_to = "assessment")

# order the items
  label <- colnames(amstar2_data)
  amstar2_data_long$item <- factor(amstar2_data_long$item, levels= label)

# order the categories of assessment
  amstar2_data_long$assessment <- factor(amstar2_data_long$assessment, levels = c("Yes", "Partial Yes", "No", "No MA"))


# generate the proportions
  amstar2_proportions <- amstar2_data_long |>
    dplyr::group_by(item, assessment) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::mutate(prop = n/sum(n)) |>
    dplyr::ungroup()


# create a a color palette with four distinct colors corresponding to each category
  colpalette <- c("Yes" = "#3a5e8cFF", "Partial Yes" = "#35b779", "No" = "#ffcf20FF", "No MA" = "#303030")


# create the barplot
amstar_plot <- ggplot2::ggplot(amstar2_proportions, ggplot2::aes(x = prop, y = forcats::fct_rev(item), fill = forcats::fct_rev(assessment))) +
  ggplot2::geom_col(position = "fill", width = bar_width) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(prop*100, digits = 1),"%")), size = font_size,
                     color = font_color,
                     position = ggplot2::position_stack(vjust = 0.5)) +
  ggplot2::labs(x = paste0("Percentage of SRs (%), N=",  nrow(amstar2_data)),
                y = "Items of AMSTAR 2 checklist",
                caption = "*Asterisk indicates critical item (domain) based on the source publication of AMSTAR 2 tool (Shea et al. 2017; doi: 10.1136/bmj.j4008). \nPossible responses: Yes/No (items 1, 3, 5, 6, 10, 13, 14, 16); Yes/Partial Yes/No (items 2, 4, 7, 8, 9); Yes/No/No MA (items 11, 12, 15). \nPICO, participant, intervention, comparison, outcome; RoB, Risk of bias; No MA, No meta-analysis conducted.") +
  ggplot2::scale_fill_manual(values = colpalette) +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, title = "Rating Scale")) +
  ggplot2::scale_x_continuous(labels = scales::percent, n.breaks = 10, expand = c(0, 0.01)) +
  ggplot2::theme(plot.caption = ggplot2::element_text(size = 10, face = "italic"),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(size = 12),
                 axis.title.x = ggplot2::element_text(size = 14, vjust = -0.75),
                 axis.title.y = ggplot2::element_text(size = 14, vjust = +0.8),
                 legend.position = "bottom",
                 legend.text = ggplot2::element_text(size = 12),
                 legend.title = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(0, 30, 0, 0)),
                 legend.key.size = grid::unit(0.6, "cm")
                 )

  return(amstar_plot)
}
