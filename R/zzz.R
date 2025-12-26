#' @keywords internal
#' @importFrom stats na.omit setNames terms
#' @importFrom utils head
NULL

utils::globalVariables(c(
   ".", "Cluster", "Mean", "CI_Lo", "CI_Hi",
   "Component", "Eigenvalue",
   "lab_x", "lab_y", "x", "y", "xend", "yend", "Label",
   "p.value", "r.squared",
   "cutoff", "value", "measure",
   "covariate", "lowerci", "upperci", "lift", "count",
   "cum_per", "cm_gain", "type",
   "bg", "pos", "obs", "percpos",
   "varied_alt", "choice_alt", "focal_value", "mean_prob", "mean_x",
   "prob", "L.prob", "U.prob", "lower.CI", "upper.CI",
   "Variable", "predicted", "conf.low", "conf.high", "group", "g1", "g2"
))
