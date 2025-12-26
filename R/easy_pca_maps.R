#' @title Easy Attribute-Based Perceptual Maps (PCA Pairwise)
#' @description
#' Create attribute-based perceptual maps from PCA coordinates using grouped data.
#' Produces all pairwise 2D maps among up to the first 4 retained components.
#'
#' @details
#' \itemize{
#'   \item Data are first aggregated by \code{group} using means of \code{vars}
#'     (and \code{pref} if supplied).
#'   \item PCA is fit on the aggregated attribute means with \code{scale.=TRUE}.
#'   \item Students specify \code{comp}, the number of components to retain.
#'   \item The function uses up to the first 4 components
#'     (\code{k = min(comp, 4, \# PCs available)}).
#'   \item Pairwise perceptual maps are created:
#'     \itemize{
#'       \item 2 components \eqn{\rightarrow} 1 map
#'       \item 3 components \eqn{\rightarrow} 3 maps
#'       \item 4 components \eqn{\rightarrow} 6 maps
#'     }
#'   \item Attribute vectors are derived from PCA loadings and (by default)
#'     varimax-rotated for interpretability.
#'   \item Object (brand) coordinates are computed as standardized grouped means
#'     projected onto the retained component space.
#'   \item If \code{pref} is supplied, a preference vector is computed in the
#'     retained component space and projected onto each 2D map.
#'   \item Attribute labels are positioned using constant padding beyond arrow tips
#'     and adjusted with \pkg{ggrepel} to reduce overlap.
#'   \item Object (brand) labels use \pkg{ggrepel} without leader lines.
#' }
#'
#' @param data Data frame containing individual-level observations.
#' @param vars Character vector of numeric attribute variable names used in PCA.
#' @param group Single string specifying the grouping variable
#'   (e.g., brand or product name).
#' @param comp Integer specifying the number of components to retain (must be \eqn{\ge 2}).
#' @param pref Optional single string specifying a numeric preference variable name.
#' @param rotate Logical; if \code{TRUE} (default), apply varimax rotation to the
#'   retained component space before creating perceptual maps.
#' @param arrow_scale Numeric in (0, 1]; scales arrow lengths relative to the object range
#'   (default = 0.75).
#' @param label_pad Numeric; distance (as a proportion of the axis range) used to
#'   push attribute arrow labels beyond arrow tips (default = 0.04).
#'
#' @return A list with:
#' \itemize{
#'   \item \code{$plots}: Named list of \pkg{ggplot2} perceptual maps
#'     (one per component pair).
#'   \item \code{$objects}: Data frame of object coordinates in the retained
#'     component space.
#'   \item \code{$attributes}: Data frame of attribute vectors in the retained
#'     component space.
#'   \item \code{$pref_vector}: Numeric vector representing the preference direction
#'     in the retained component space (or \code{NULL} if not supplied).
#'   \item \code{$pairs}: Matrix of component pairs used to generate maps.
#'   \item \code{$k_used}: Number of components actually used for mapping.
#'   \item \code{$pcaobj}: \code{prcomp} object fit on grouped attribute means.
#'   \item \code{$data_grouped}: Grouped data frame used to fit the PCA.
#' }
#' @seealso \code{\link{percmap}}
#' @examples
#' \dontrun{
#' maps <- easy_pca_maps(
#'   data  = greekbrands,
#'   vars  = c("perform", "leader", "fun", "serious", "bargain", "value"),
#'   group = "brand",
#'   comp  = 3
#' )
#'
#' maps$plots[[1]]
#' }

#'
#' @importFrom dplyr group_by summarise across all_of select
#' @importFrom stats prcomp varimax complete.cases
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline geom_segment geom_point
#' @importFrom ggplot2 theme element_blank element_rect scale_x_continuous scale_y_continuous labs
#' @importFrom grid arrow unit
#' @importFrom ggrepel geom_text_repel
#'
#' @export

easy_pca_maps <- function(data,
                          vars,
                          group,
                          comp,
                          pref = NULL,
                          rotate = TRUE,
                          arrow_scale = 0.75,
                          label_pad = 0.04) {
   
   # ---- checks ----
   if (!is.data.frame(data)) stop("`data` must be a data frame.")
   if (missing(vars) || is.null(vars) || length(vars) < 2) {
      stop("`vars` is required and must contain at least 2 numeric variables.")
   }
   if (!is.character(vars)) stop("`vars` must be a character vector of column names.")
   if (missing(group) || is.null(group) || !is.character(group) || length(group) != 1) {
      stop("`group` is required and must be a single column name string.")
   }
   if (!group %in% names(data)) stop("`group` not found in `data`.")
   if (!all(vars %in% names(data))) {
      stop("Some `vars` are not in `data`: ", paste(setdiff(vars, names(data)), collapse = ", "))
   }
   
   if (missing(comp) || length(comp) != 1 || !is.numeric(comp) || comp < 2) {
      stop("`comp` must be a single integer >= 2.")
   }
   comp <- as.integer(comp)
   
   if (!is.null(pref)) {
      if (!is.character(pref) || length(pref) != 1) stop("`pref` must be a single column name string.")
      if (!pref %in% names(data)) stop("`pref` not found in `data`.")
      if (!is.numeric(data[[pref]])) stop("`pref` must be numeric.")
   }
   
   non_numeric <- vars[!vapply(data[vars], is.numeric, logical(1))]
   if (length(non_numeric) > 0) {
      stop("All `vars` must be numeric. Non-numeric: ", paste(non_numeric, collapse = ", "))
   }
   
   if (!is.logical(rotate) || length(rotate) != 1) stop("`rotate` must be TRUE/FALSE.")
   if (!is.numeric(arrow_scale) || length(arrow_scale) != 1 || arrow_scale <= 0 || arrow_scale > 1) {
      stop("`arrow_scale` must be a single number in (0, 1].")
   }
   if (!is.numeric(label_pad) || length(label_pad) != 1 || label_pad < 0) {
      stop("`label_pad` must be a single number >= 0.")
   }
   
   # ---- group aggregation ----
   agg_vars <- vars
   if (!is.null(pref)) agg_vars <- unique(c(vars, pref))
   
   gdf <- dplyr::group_by(data, .data[[group]])
   gdf <- dplyr::summarise(
      gdf,
      dplyr::across(dplyr::all_of(agg_vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
   )
   
   key <- gdf[[group]]
   X <- dplyr::select(gdf, dplyr::all_of(vars))
   
   keep <- stats::complete.cases(X)
   X_used <- X[keep, , drop = FALSE]
   key_used <- key[keep]
   
   if (nrow(X_used) < 2) stop("Not enough complete grouped rows to run PCA after dropping missing values.")
   if (ncol(X_used) < 2) stop("Need at least 2 attribute variables after preprocessing.")
   
   pref_vec_raw <- NULL
   if (!is.null(pref)) {
      pref_vec_raw <- gdf[[pref]][keep]
   }
   
   # ---- PCA ----
   pcaobj <- stats::prcomp(X_used, scale. = TRUE, center = TRUE)
   
   maxcomp <- ncol(pcaobj$rotation)
   k <- min(comp, 4L, maxcomp, ncol(X_used))
   if (k < 2) stop("PCA did not produce at least 2 components (check your data).")
   if (comp > 4) message("Note: comp > 4 requested; using first 4 components for mapping.")
   if (comp > maxcomp) message("Note: comp exceeds available components; using ", k, " component(s).")
   
   comp_names <- paste0("Comp_", seq_len(k))
   
   # ---- attribute coordinates (k-D) ----
   rotk <- pcaobj$rotation[, 1:k, drop = FALSE]
   if (rotate) {
      rv <- stats::varimax(rotk, normalize = FALSE)
      acoord <- rotk %*% rv$rotmat
   } else {
      acoord <- rotk
   }
   colnames(acoord) <- comp_names
   
   # ---- object coordinates (k-D) ----
   Z <- scale(X_used)
   bcoord <- as.data.frame(as.matrix(Z) %*% as.matrix(acoord))
   colnames(bcoord) <- comp_names
   rownames(bcoord) <- as.character(key_used)
   
   # ---- preference vector in k-D (optional) ----
   pcoord_k <- NULL
   if (!is.null(pref)) {
      w <- as.numeric(scale(pref_vec_raw))
      pcoord_k <- colMeans(bcoord * w, na.rm = TRUE)
      
      if (all(is.finite(pcoord_k)) && sum(pcoord_k^2) > 0) {
         pcoord_k <- as.numeric(pcoord_k)
         names(pcoord_k) <- comp_names
      } else {
         pcoord_k <- NULL
      }
   }
   
   # ---- build all pairs and plots ----
   pairs <- utils::combn(seq_len(k), 2)
   plots <- vector("list", ncol(pairs))
   
   p <- ncol(X_used)
   eig <- (pcaobj$sdev)^2
   var_pct <- (eig[1:k] / p) * 100
   names(var_pct) <- comp_names
   
   for (m in seq_len(ncol(pairs))) {
      
      i <- pairs[1, m]
      j <- pairs[2, m]
      
      xi <- comp_names[i]
      yj <- comp_names[j]
      
      # axis scaling for this (i,j) plot
      bcmax_ij <- max(abs(bcoord[[xi]]), abs(bcoord[[yj]]), na.rm = TRUE)
      if (!is.finite(bcmax_ij) || bcmax_ij == 0) bcmax_ij <- 1
      
      # attribute arrows for this (i,j) plot
      a_ij <- as.data.frame(acoord[, c(xi, yj), drop = FALSE])
      amax_ij <- max(abs(a_ij), na.rm = TRUE)
      if (!is.finite(amax_ij) || amax_ij == 0) amax_ij <- 1
      
      scale_factor <- (bcmax_ij / amax_ij) * arrow_scale
      a_ij_scaled <- a_ij * scale_factor
      a_ij_scaled$Variable <- rownames(acoord)
      
      # ---- CONSTANT padding beyond arrow tips (in map units) ----
      pad_units <- label_pad * bcmax_ij
      u <- sqrt(a_ij_scaled[[xi]]^2 + a_ij_scaled[[yj]]^2)
      u[u == 0] <- 1
      a_ij_scaled$lab_x <- a_ij_scaled[[xi]] + pad_units * (a_ij_scaled[[xi]] / u)
      a_ij_scaled$lab_y <- a_ij_scaled[[yj]] + pad_units * (a_ij_scaled[[yj]] / u)
      
      # object points for this (i,j) plot
      b_ij <- data.frame(
         Label = rownames(bcoord),
         x = bcoord[[xi]],
         y = bcoord[[yj]]
      )
      
      g <- ggplot2::ggplot() +
         ggplot2::geom_hline(yintercept = 0) +
         ggplot2::geom_vline(xintercept = 0) +
         
         ggplot2::geom_segment(
            data = a_ij_scaled,
            ggplot2::aes(x = 0, y = 0, xend = .data[[xi]], yend = .data[[yj]]),
            linewidth = 1,
            color = "red4",
            arrow = grid::arrow(ends = "last", length = grid::unit(0.25, "cm"))
         ) +
         
         # attribute labels (repelled, no leader lines)
         ggrepel::geom_text_repel(
            data = a_ij_scaled,
            ggplot2::aes(x = lab_x, y = lab_y, label = Variable),
            color = "red4",
            size = 5,
            box.padding = 0.25,
            point.padding = 0.10,
            min.segment.length = 0,
            segment.color = NA,   # no leader lines
            seed = 1              
         ) +
         
         ggplot2::geom_point(
            data = b_ij,
            ggplot2::aes(x = x, y = y),
            color = "navy",
            size = 2
         ) +
         
         # object labels (repelled, NO leader lines)
         ggrepel::geom_text_repel(
            data = b_ij,
            ggplot2::aes(x = x, y = y, label = Label),
            color = "navy",
            size = 5,
            box.padding = 0.25,
            point.padding = 0.15,
            min.segment.length = 0,
            segment.color = NA,   # no leader lines
            seed = 1              # <- SAME seed
         ) +
         
         ggplot2::theme(
            aspect.ratio = 1,
            axis.ticks = ggplot2::element_blank(),
            axis.text  = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "white", color = "black")
         ) +
         ggplot2::scale_x_continuous(limits = c(-bcmax_ij, bcmax_ij), expand = c(0.07, 0.07)) +
         ggplot2::scale_y_continuous(limits = c(-bcmax_ij, bcmax_ij), expand = c(0.07, 0.07)) +
         ggplot2::labs(
            x = paste0(xi, " (", round(var_pct[xi], 2), "%)"),
            y = paste0(yj, " (", round(var_pct[yj], 2), "%)")
         )
      
      # ---- preference vector projected to this plane (optional) ----
      if (!is.null(pcoord_k)) {
         
         pv <- c(x = as.numeric(pcoord_k[i]), y = as.numeric(pcoord_k[j]))
         
         if (all(is.finite(pv)) && sum(pv^2) > 0) {
            
            a_arrow_maxlen <- max(
               rowSums(as.matrix(a_ij_scaled[, c(xi, yj), drop = FALSE])^2),
               na.rm = TRUE
            )
            if (!is.finite(a_arrow_maxlen) || a_arrow_maxlen == 0) a_arrow_maxlen <- 1
            
            pv_scale <- sqrt(a_arrow_maxlen / (pv["x"]^2 + pv["y"]^2))
            
            pv2 <- data.frame(
               xend = as.numeric(pv["x"]) * pv_scale,
               yend = as.numeric(pv["y"]) * pv_scale,
               row.names = NULL
            )
            
            g <- g +
               ggplot2::geom_segment(
                  data = pv2,
                  ggplot2::aes(x = 0, y = 0, xend = xend, yend = yend),
                  linewidth = 1,
                  color = "darkgreen",
                  arrow = grid::arrow(ends = "last", length = grid::unit(0.25, "cm"))
               ) +
               ggrepel::geom_text_repel(
                  data = pv2,
                  ggplot2::aes(x = xend, y = yend, label = "Pref"),
                  color = "darkgreen",
                  size = 5,
                  box.padding = 0.25,
                  point.padding = 0.10,
                  segment.color = NA,
                  min.segment.length = 0
               )
         }
      }
      
      plots[[m]] <- g
      names(plots)[m] <- paste0(comp_names[i], "_vs_", comp_names[j])
   }
   
   list(
      plots = plots,
      objects = bcoord,
      attributes = as.data.frame(acoord),
      pref_vector = pcoord_k,
      pairs = t(pairs),
      k_used = k,
      pcaobj = pcaobj,
      data_grouped = gdf
   )
}
