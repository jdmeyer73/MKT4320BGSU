#' @title Traditional Conjoint Results
#' @description
#' Produce overall part-worth plots for each attribute in a traditional conjoint
#' analysis, along with an attribute-importance plot and a case-level table of
#' part-worths and importances.
#'
#' @details
#' This is a legacy function retained for backwards compatibility with older
#' course materials. It is not used in current student workflows.
#'
#' For best results, save output to an object.
#'
#' @param formula A model formula with a DV on the left side and attributes on the
#'   right side (e.g., \code{value ~ attr1 + attr2 + attr3}).
#' @param data Data frame containing the profile ratings.
#' @param idvar Character string giving the case id variable name.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{$pwplot} mean part-worth plot(s)
#'   \item \code{$impplot} mean importance plot
#'   \item \code{$casetable} case-level part-worths and importances
#'   \item \code{$coutlong} long-format case-level part-worths
#' }
#'
#' @examples
#' \dontrun{
#' caform <- value ~ airline + connect + price
#' results <- tradca(formula = caform, data = airlineca, idvar = "caseid")
#'
#' results$pwplot
#' results$impplot
#' head(results$casetable)
#' }
#'
#' @importFrom broom tidy
#' @importFrom stringr str_replace str_split_fixed
#' @importFrom stats lm terms
#' @keywords internal
tradca <- function(formula, data, idvar) {
   
   # Legacy helper: avoid library()/require() in package code; check availability.
   pkgs <- c("broom", "dplyr", "stringr", "ggplot2", "magrittr")
   for (pkg in pkgs) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
         stop("Package '", pkg, "' is required for tradca(). Please install it.",
              call. = FALSE)
      }
   }
   
   if (!is.character(idvar) || length(idvar) != 1L) {
      stop("idvar must be a single character string.", call. = FALSE)
   }
   if (!idvar %in% names(data)) {
      stop("idvar '", idvar, "' was not found in data.", call. = FALSE)
   }
   
   # Use the idvar column correctly (the original code used min(idvar) which is the string).
   minid <- min(data[[idvar]], na.rm = TRUE)
   
   temp <- data |>
   dplyr::filter(.data[[idvar]] == minid) |>
   stats::lm(formula, data = .)
   
   len <- length(formula)
   idvarnum <- grep(idvar, colnames(data))
   butes <- attr(stats::terms(formula), "term.labels")
   
   caseout <- data |>
   dplyr::group_by(.data[[idvar]]) |>
   dplyr::do(suppressWarnings(broom::tidy(stats::lm(formula = formula, data = .)))) |>
   dplyr::select(1:3) |>
   dplyr::filter(.data$term != "(Intercept)") |>
   dplyr::mutate_at(dplyr::vars(dplyr::starts_with("estimate")), dplyr::funs(round(., 2))) |>
   as.data.frame()
   
   coutlong <- caseout
   
   for (i in 1:len) {
      coutlong <- coutlong |>
      dplyr::group_by_at(1) |>
      dplyr::summarise(term = paste0(butes[i], (temp$xlevels[[i]])[[1]]), .groups = "drop") |>
      dplyr::mutate(estimate = 0) |>
      dplyr::bind_rows(coutlong, .) |>
      dplyr::arrange(.data[[idvar]]) |>
      dplyr::mutate(term = gsub(butes[i], paste0(butes[i], "_"), .data$term)) |>
      dplyr::arrange(.data[[idvar]], .data$term)
   }
   
   clongtemp <- coutlong
   
   caseout <- reshape(
      data = coutlong,
      idvar = idvar,
      v.names = "estimate",
      timevar = "term",
      direction = "wide"
   ) |>
   dplyr::rename_at(
      dplyr::vars(dplyr::starts_with("estimate")),
      dplyr::funs(stringr::str_replace(., "estimate\\.", ""))
   )
   
   coutlong[, 4:5] <- stringr::str_split_fixed(coutlong$term, "_", 2)
   
   coutlong <- coutlong |>
   dplyr::rename(attribute = 4, level = 5)
   
   cout <- caseout
   varlist <- NULL
   
   for (i in 1:len) {
      minc <- min(grep(butes[i], colnames(caseout)))
      maxc <- max(grep(butes[i], colnames(caseout)))
      varlist[i] <- paste0("range", i)
      
      for (j in 1:nrow(caseout)) {
         maxatt.row <- max(caseout[j, minc:maxc])
         minatt.row <- min(caseout[j, minc:maxc])
         range <- maxatt.row - minatt.row
         caseout[j, paste0("range", i)] <- range
         
         for (k in minc:maxc) {
            if (minatt.row != 0) {
               caseout[j, k] <- caseout[j, k] + range
            }
         }
      }
   }
   
   caseout$totrange <- rowSums(caseout[(ncol(caseout) - (len - 1)):ncol(caseout)])
   for (i in 1:len) {
      for (j in 1:nrow(caseout)) {
         imp <- round(100 * caseout[j, varlist[i]] / caseout[j, "totrange"], 2)
         caseout[j, paste0("Imp_", butes[i])] <- imp
      }
   }
   caseout <- caseout[, -((ncol(caseout) - len * 2):(ncol(caseout) - len))]
   
   impdata <- caseout |>
   dplyr::select(dplyr::starts_with("Imp_")) |>
   dplyr::summarise_all(~round(mean(.), 2))
   
   colnames(impdata) <- gsub("Imp_", "", colnames(impdata))
   impdata <- data.frame(t(impdata))
   names(impdata)[1] <- "importance"
   impdata$attribute <- rownames(impdata)
   
   impplot <- impdata |>
   ggplot2::ggplot(ggplot2::aes(x = .data$attribute, y = .data$importance, fill = .data$attribute)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::labs(x = "Attribute", y = "Mean Importance") +
      ggplot2::geom_text(ggplot2::aes(label = .data$importance), vjust = .95, fontface = "bold", color = "white")
   
   pwplot <- coutlong |>
   dplyr::group_by(.data$attribute, .data$level) |>
   dplyr::summarise(pw = mean(.data$estimate), .groups = "drop") |>
   ggplot2::ggplot(ggplot2::aes(x = .data$level, y = .data$pw, group = 1)) +
      ggplot2::geom_line(ggplot2::aes(color = .data$attribute), show.legend = FALSE, size = 2) +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_wrap(~attribute, scales = "free_x") +
      ggplot2::labs(x = "Attribute Level", y = "Mean Part-Worth")
   
   return(list("casetable" = caseout, "impplot" = impplot, "pwplot" = pwplot, "coutlong" = coutlong))
}
