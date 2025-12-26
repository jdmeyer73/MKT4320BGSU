#' Split a Dataset into Training and Test Samples
#'
#' @description
#' Create training and test samples from a data frame. The function supports
#' both a classic row-level split (stratified by a factor outcome) and a
#' group-level split that keeps all rows from the same group together.
#'
#' @details
#' **Row-level split (default):** If \code{group} is \code{NULL}, the function
#' performs a row-level split and requires \code{outcome} to be a factor; the
#' split is stratified on outcome levels using \code{caret::createDataPartition()}.
#'
#' **Group-level split:** If \code{group} is provided, splitting is done at the
#' group level so all rows belonging to the same group are assigned to the same
#' partition (useful for long-format data such as alternative-specific choice
#' data).
#'
#' Group-level stratification is attempted when:
#' \itemize{
#'   \item \code{choice} and \code{alt} are provided (stratify by chosen alternative), or
#'   \item \code{outcome} is provided and is a factor that is effectively constant within group.
#' }
#' If stratification labels are not available, groups are split by simple random
#' sampling.
#'
#' @param data A data frame to split.
#' @param outcome Outcome variable used for stratification. Required when
#'   \code{group} is \code{NULL}. Optional when \code{group} is provided.
#' @param group Optional grouping variable (e.g., choice situation id or respondent id).
#'   If provided, splitting is done at the group level.
#' @param choice Optional 0/1 (or \code{TRUE}/\code{FALSE}) indicator for the chosen
#'   alternative. Used only when \code{group} is provided.
#' @param alt Optional alternative label/ID. Used with \code{choice} to stratify
#'   at the group level.
#' @param p Proportion of observations (or groups) to place in the training set.
#'   Must be strictly between 0 and 1. Default is 0.75.
#' @param seed Random seed for reproducibility. Default is 4320.
#'
#' @return
#' An (invisible) list with elements \code{train} and \code{test}. When
#' \code{group} is provided, also returns \code{train_groups} and
#' \code{test_groups}.
#'
#' @export
#'
#' @importFrom caret createDataPartition
#' @importFrom rlang ensym as_name
#'
#' @examples
#' \dontrun{
#' # --- Classic row-level split (stratified by factor outcome) ---
#' directmktg$buy <- factor(directmktg$buy, levels = c("no", "yes"))
#' sp <- splitsample(directmktg, outcome = buy)
#' train <- sp$train
#' test  <- sp$test
#'
#' # --- Group-level split (keep each choice set together) ---
#' sp2 <- splitsample(my_long, group = chid, choice = choice, alt = alt, p = 0.8)
#' train2 <- sp2$train
#' test2  <- sp2$test
#'
#' # --- Group-level split by respondent (keep all rows per person together) ---
#' sp3 <- splitsample(my_long, group = id, choice = choice, alt = alt)
#' }
splitsample <- function(data,
                        outcome = NULL,
                        group = NULL,
                        choice = NULL,
                        alt = NULL,
                        p = 0.75,
                        seed = 4320) {
   
   # --- checks ---
   if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
   }
   
   if (!is.numeric(p) || length(p) != 1L || !is.finite(p) || p <= 0 || p >= 1) {
      stop("`p` must be a single number strictly between 0 and 1.", call. = FALSE)
   }
   
   set.seed(seed)
   
   # ============================================================
   # CASE 1: GROUP-LEVEL SPLIT (long-format safe)
   # ============================================================
   if (!is.null(group)) {
      
      group_name <- rlang::as_name(rlang::ensym(group))
      if (!group_name %in% names(data)) {
         stop(sprintf("Group variable '%s' not found in `data`.", group_name), call. = FALSE)
      }
      
      groups <- unique(data[[group_name]])
      groups <- groups[!is.na(groups)]
      if (length(groups) < 2L) {
         stop("`group` must have at least 2 non-missing unique values.", call. = FALSE)
      }
      
      # ---- optional stratification label at group level ----
      y_group <- NULL
      
      # Preferred for choice data: stratify by the chosen alternative within each group
      if (!is.null(choice) && !is.null(alt)) {
         
         choice_name <- rlang::as_name(rlang::ensym(choice))
         alt_name    <- rlang::as_name(rlang::ensym(alt))
         
         if (!choice_name %in% names(data)) {
            stop(sprintf("`choice` '%s' not found in `data`.", choice_name), call. = FALSE)
         }
         if (!alt_name %in% names(data)) {
            stop(sprintf("`alt` '%s' not found in `data`.", alt_name), call. = FALSE)
         }
         
         # Coerce choice safely to integer 0/1
         ch <- data[[choice_name]]
         ch <- if (is.logical(ch)) as.integer(ch) else suppressWarnings(as.integer(ch))
         
         chosen_alt <- tapply(seq_len(nrow(data)), data[[group_name]], function(idx) {
            idx1 <- idx[which(ch[idx] == 1L)]
            if (length(idx1) == 0L) return(NA)
            data[[alt_name]][idx1[1L]]
         })
         
         # Use stratification only if we have enough non-missing labels
         nonmiss <- !is.na(chosen_alt)
         if (sum(nonmiss) >= max(2L, round(0.5 * length(chosen_alt)))) {
            y_group <- as.factor(chosen_alt[nonmiss])
         }
      }
      
      # If user supplied a factor outcome and wants group-level stratification from it:
      # (only works cleanly if outcome is effectively constant within group)
      if (is.null(y_group) && !is.null(outcome)) {
         outcome_name <- rlang::as_name(rlang::ensym(outcome))
         if (!outcome_name %in% names(data)) {
            stop(sprintf("Outcome variable '%s' not found in `data`.", outcome_name), call. = FALSE)
         }
         
         if (is.factor(data[[outcome_name]])) {
            first_out <- tapply(seq_len(nrow(data)), data[[group_name]], function(idx) {
               data[[outcome_name]][idx[1L]]
            })
            y_group <- as.factor(first_out[!is.na(first_out)])
         }
      }
      
      # ---- partition groups ----
      if (!is.null(y_group)) {
         inTrain <- caret::createDataPartition(y = y_group, p = p, list = FALSE)
         grp_names <- names(y_group)
         train_groups <- grp_names[inTrain]
         test_groups  <- grp_names[-inTrain]
      } else {
         n_train <- floor(p * length(groups))
         train_groups <- sample(groups, size = n_train, replace = FALSE)
         test_groups  <- setdiff(groups, train_groups)
      }
      
      train <- data[data[[group_name]] %in% train_groups, , drop = FALSE]
      test  <- data[data[[group_name]] %in% test_groups,  , drop = FALSE]
      
      return(invisible(list(
         train = train,
         test = test,
         train_groups = train_groups,
         test_groups = test_groups
      )))
   }
   
   # ============================================================
   # CASE 2: CLASSIC ROW-LEVEL SPLIT
   # ============================================================
   if (is.null(outcome)) {
      stop("When `group` is NULL, you must provide `outcome`.", call. = FALSE)
   }
   
   outcome_name <- rlang::as_name(rlang::ensym(outcome))
   
   if (!outcome_name %in% names(data)) {
      stop(sprintf("Outcome variable '%s' not found in `data`.", outcome_name), call. = FALSE)
   }
   
   # enforce factor outcome (original behavior)
   if (!is.factor(data[[outcome_name]])) {
      stop(
         paste0(
            "The outcome variable '", outcome_name, "' must be a FACTOR.\n",
            "It appears to be numeric or character.\n",
            "Convert it first, for example:\n\n",
            "  data$", outcome_name, " <- factor(data$", outcome_name, ")\n\n",
            "or, if binary classification:\n",
            "  data$", outcome_name, " <- factor(data$", outcome_name,
            ", levels = c('no','yes'))\n"
         ),
         call. = FALSE
      )
   }
   
   inTrain <- caret::createDataPartition(
      y    = data[[outcome_name]],
      p    = p,
      list = FALSE
   )
   
   train <- data[inTrain, , drop = FALSE]
   test  <- data[-inTrain, , drop = FALSE]
   
   invisible(list(train = train, test = test))
}
