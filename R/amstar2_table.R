#' @title Calculates the overall rating based on critical items
#'
#' @description This function adds the overall assessment.
#'
#' @param data A dataset.
#'
#' @return amstar_table
#'
#' @example man/examples/example0.R
#'
#' @export


amstar2_table <- function(data = data){

  data <- stats::na.omit(data)
  colnames(data)[1] <- "reviews"
  amstar2_data <- data[, -1]

  label <- c("1. PICO components",
             "2.* Preestablished protocol",
             "3. Explanation of inlcuded studies' design",
             "4.* Comprehensive search strategy",
             "5. Duplicate study selection",
             "6. Duplicate data extraction",
             "7.* List of excluded studies and justification",
             "8. Description of included studies",
             "9.* Risk of bias (RoB) assessment",
             "10. Funding sources",
             "11.* Use of appropriate statistical methods",
             "12. RoB impact on synthesized results",
             "13.* Results interpretation with RoB reference",
             "14. Heterogeneity explanation",
             "15.* Publication/ small study bias investigation",
             "16. Conflict of interest declaration")

  names(amstar2_data) <- label


  # Calculate overall
  overall = c()


  for(i in 1:nrow(amstar2_data)){ #use a for-loop for every row

    # Calculate the number of critical items with negative ratings
    n_critical =  as.numeric((amstar2_data[i, 2] == "No") +
                               (amstar2_data[i, 4] == "No") +
                               (amstar2_data[i, 7] == "No") +
                               (amstar2_data[i, 9] == "No") +
                               (amstar2_data[i, 11] == "No") +
                               (amstar2_data[i, 13] == "No") +
                               (amstar2_data[i, 15] == "No"))

    # Calculate the number of non-critical items with negative ratings
    n_non_critical = as.numeric((amstar2_data[i, 1] == "No") +
                                  (amstar2_data[i, 3] == "No") +
                                  (amstar2_data[i, 5] == "No") +
                                  (amstar2_data[i, 6] == "No") +
                                  (amstar2_data[i, 8] == "No") +
                                  (amstar2_data[i, 10] == "No") +
                                  (amstar2_data[i, 12] == "No") +
                                  (amstar2_data[i, 14] == "No") +
                                  (amstar2_data[i, 16] == "No"))

    # Use the algorithm provided in the AMSTAR publication
    if (n_critical > 1) {
      overall[i] <- "Critically Low"

    } else if (n_critical == 1) {

      overall[i] <- "Low"

    } else if (n_critical == 0 & n_non_critical > 1) {

      overall[i] <- "Moderate"

    } else {
      overall[i] <- "High"
    }

  }

  amstar_table <- tibble::tibble(Reviews = data$reviews, amstar2_data, overall)


  return(amstar_table)

}
