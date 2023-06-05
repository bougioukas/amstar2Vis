#' @title Plots the AMSTAR plot
#'
#' @description This function plots the AMSTAR bar plot.
#'
#' @param data A dataset.
#'
#' @param fontsize A number that controls the aesthetic of font size of the percentages in the bars. Default is fontsize = 3.5 .
#'
#' @param fontcolor A color that controls the aesthetic of color of the percentages in the bars. Default is fontcolor = "white".
#'
#' @param barwidth A number that controls the width between the bars. Default is barwidth = 0.8 .
#'
#' @return amstar2_barplot
#'
#' @export
#'
#' @examples


amstar2_barplot <- function(data = data, fontsize = 3.5, fontcolor = "white", barwidth = 0.8){

  c(missing(fontsize), missing(fontcolor), missing(barwidth))

  data <- data[, -1]
  amstar2_data <- stats::na.omit(data)

  label <- c("1. PICO components",
             "2. Preestablished protocol",
             "3. Explanation of inlcuded studies' design",
             "4. Comprehensive search strategy",
             "5. Duplicate study selection",
             "6. Duplicate data extraction",
             "7. List of excluded studies and justification",
             "8. Description of included studies",
             "9. Risk of bias (RoB) assessment",
             "10. Funding sources",
             "11. Use of appropriate statistical methods",
             "12. RoB impact on synthesized results",
             "13. Results interpretation with RoB reference",
             "14. Heterogeneity explanation",
             "15. Publication/ small study bias investigation",
             "16. Conflict of interest declaration")

  names(amstar2_data) <- label

  amstar2_data_long <- tidyr::pivot_longer(amstar2_data, cols = 1:16, names_to = "item", values_to = "assessment")

  amstar2_data_long$item <- factor(amstar2_data_long$item, levels= label)

  amstar2_data_long$assessment <- factor(amstar2_data_long$assessment, levels = c("Yes", "Partial Yes", "No", "Not Applicable"))

# generate the proportions
  amstar2_proportions <- amstar2_data_long |>
    dplyr::group_by(item, assessment) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::mutate(prop = n/sum(n)) |>
    dplyr::ungroup()

# create a a color palette with four distinct colors
colpalette <- c("#999999" , "#ffcf20FF",  "#20908C", "#3a5e8cFF")


# create the ggplot
amstar_plot <- ggplot2::ggplot(amstar2_proportions, ggplot2::aes(x = prop, y = forcats::fct_rev(item), fill = forcats::fct_rev(assessment))) +
  ggplot2::geom_col(position = "fill", width = barwidth) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(prop, digits = 3)*100,"%")), size = fontsize,
              color = fontcolor,
              position = ggplot2::position_stack(vjust = 0.5)) +
  ggplot2::labs(x = paste0("Percentage of SRs (%), N=",  nrow(amstar2_data)),
                y = "Items of AMSTAR 2 checklist") +
  ggplot2::scale_fill_manual(values = colpalette) +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, title = "Rating")) +
  ggplot2::scale_x_continuous(labels = scales::percent, n.breaks = 10) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(size = 12),
          axis.title.x = ggplot2::element_text(size = 14, vjust = -0.75),
          axis.title.y = ggplot2::element_text(size = 14, vjust = +0.8),
          legend.position = "bottom",
          legend.text = ggplot2::element_text(size = 12),
          legend.title = ggplot2::element_text(size = 12, face = "bold", margin = ggplot2::margin(0, 50, 0, 0)),
          legend.key.size = grid::unit(0.6, "cm"))

  return(amstar_plot)
}
