#' @title ROC Curve for Binary Logistic Regression
#' @description
#' Create an ROC curve (with AUC) for a binary logistic regression model.
#' Can be used for a single data set (e.g., training) or for two data sets
#' (e.g., training and test), using the same fitted model.
#'
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item pROC
#'   \item ggplot2
#' }
#'
#' This function:
#' \itemize{
#'   \item Obtains predicted probabilities from the logistic model.
#'   \item Computes ROC curves and AUC using \code{pROC}.
#'   \item Creates ggplot2 ROC curves with:
#'         \itemize{
#'           \item Title: "ROC Curve for \[label\]"
#'           \item AUC displayed on the plot
#'           \item Background style matching \code{easy_mp()} (theme_bw)
#'         }
#'   \item When two data sets are provided, returns two ROC plots.
#' }
#'
#' @param MOD Fitted logistic regression model (\code{glm} with
#'   \code{family = "binomial"}).
#' @param DATA Data frame for ROC computation (e.g., training).
#' @param LABEL1 Character label for \code{DATA}'s ROC plot title.
#'   Defaults to \code{"Sample 1"}.
#' @param DATA2 Optional second data frame (e.g., test set).
#'   Defaults to \code{NULL} (no second data set).
#' @param LABEL2 Character label for \code{DATA2}'s ROC plot title.
#'   Defaults to \code{"Sample 2"}.
#'
#' @return
#' If only \code{DATA} is provided: a single ROC \code{ggplot} object.
#'
#' If both \code{DATA} and \code{DATA2} are provided: a list containing:
#' \itemize{
#'   \item \code{$sample1}: ROC for \code{DATA}
#'   \item \code{$sample2}: ROC for \code{DATA2}
#' }
#'
#' @examples
#' \dontrun{
#' # Single ROC curve (e.g., training data)
#' roc_logistic(mod, DATA = train, LABEL1 = "Training")
#'
#' # Training and test ROC curves
#' roc_logistic(mod,
#'              DATA   = train,
#'              LABEL1 = "Training",
#'              DATA2  = test,
#'              LABEL2 = "Test")
#' }
#'
#' @importFrom pROC roc auc ggroc
#' @importFrom stats predict family formula
#' @importFrom ggplot2 annotate labs theme_bw theme element_blank
#' @export
roc_logistic <- function(MOD,
                         DATA,
                         LABEL1 = "Sample 1",
                         DATA2  = NULL,
                         LABEL2 = "Sample 2") {
   
   # ---- basic checks ----
   
   if (!inherits(MOD, "glm")) {
      stop("MOD must be a fitted glm() object for binary logistic regression.",
           call. = FALSE)
   }
   
   fam <- stats::family(MOD)$family
   if (!identical(fam, "binomial")) {
      stop("MOD must be a glm() with family = 'binomial'.",
           call. = FALSE)
   }
   
   if (!is.data.frame(DATA)) {
      stop("DATA must be a data frame.", call. = FALSE)
   }
   
   if (!is.null(DATA2) && !is.data.frame(DATA2)) {
      stop("If provided, DATA2 must be a data frame.", call. = FALSE)
   }
   
   # Outcome variable name from model formula
   dv_name <- toString(stats::formula(MOD)[[2]])
   
   # ---- internal helper ----
   
   build_roc_plot <- function(dat, label) {
      
      if (!dv_name %in% names(dat)) {
         stop(sprintf("Outcome variable '%s' not found in provided data.",
                      dv_name),
              call. = FALSE)
      }
      
      true   <- dat[[dv_name]]
      dvprob <- stats::predict(MOD, newdata = dat, type = "response")
      
      rocobj <- pROC::roc(response = true, predictor = dvprob)
      aucval <- as.numeric(pROC::auc(rocobj))
      auclab <- paste0("AUC = ", round(aucval, 3))
      
      # pROC::ggroc currently uses `size` for lines; suppress that deprecation warning
      base_roc <- suppressWarnings(
         pROC::ggroc(rocobj, color = "darkorange", size = 1.2)
      )
      
      p <- base_roc +
         ggplot2::annotate(
            "text",
            label = auclab,
            x     = 0.52,
            y     = 0.35,
            size  = 5,
            color = "black"
         ) +
         ggplot2::labs(
            title = paste0("ROC Curve for ", label),
            x     = "Specificity",
            y     = "Sensitivity"
         ) +
         ggplot2::annotate(
            "segment",
            x = 0, y = 1,
            xend = 1, yend = 0,
            linetype = "dashed"
         ) +
         ggplot2::theme_bw() +
         ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor   = ggplot2::element_blank()
         )
      
      p
   }
   
   # ---- compute ROC(s) ----
   
   p1 <- build_roc_plot(DATA, LABEL1)
   
   if (is.null(DATA2)) {
      return(p1)
   }
   
   p2 <- build_roc_plot(DATA2, LABEL2)
   
   list(sample1 = p1, sample2 = p2)
}
