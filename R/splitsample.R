#' @title Split Sample
#' @description
#' Create training and test samples from a data set.
#'
#' If `group` is NULL, this function performs a classic row-level split and
#' requires a *factor* `outcome` (stratified split).
#'
#' If `group` is provided, the split is done at the group level so all rows
#' belonging to the same group are kept together (useful for long-format
#' alternative-specific choice data).
#'
#' If `group` is provided AND `choice` + `alt` are also provided, the function
#' additionally returns `train.mdata` and `test.mdata` as `dfidx` objects
#' (ready for `mlogit::mlogit()`), created via `dfidx::dfidx()`.
#'
#' @details
#' Required packages:
#' \itemize{
#'   \item caret
#'   \item dfidx (only needed when `group` + `choice` + `alt` are used)
#' }
#'
#' @param data A data frame.
#' @param outcome Character string naming the factor outcome variable.
#'   Required when `group` is NULL. Ignored otherwise.
#' @param group Optional character string naming a grouping variable
#'   (e.g., choice set id or respondent id). If provided, splitting is done
#'   at the group level.
#' @param choice Optional character string naming the chosen indicator
#'   (0/1, TRUE/FALSE, or common yes-like strings). Used only when `group`
#'   is provided (to create `train.mdata`/`test.mdata` and/or stratify groups).
#' @param alt Optional character string naming the alternative label/ID.
#'   Used with `choice` when `group` is provided.
#' @param p Proportion for the training split (strictly between 0 and 1).
#' @param seed Random seed for reproducibility.
#'
#' @return A list containing `train` and `test`.
#' When `group` is provided, also returns `train_groups`, `test_groups`,
#' and (if possible) `train.mdata`, `test.mdata`.
#'
#' @examples
#' \dontrun{
#' # --- Classic classification split (row-level) ---
#' directmktg$buy <- factor(directmktg$buy, levels = c("no","yes"))
#' splits <- splitsample(directmktg, outcome = "buy")
#' train <- splits$train
#' test  <- splits$test
#'
#' # --- Long-format AS-MNL split (group-level) ---
#' # group = choice situation id (chid); also create dfidx objects
#' splits2 <- splitsample(my_long, group = "chid", choice = "choice", alt = "alt", p = 0.8)
#'
#' # Students then estimate on splits2$train.mdata
#' # mod <- mlogit::mlogit(choice ~ price + promo | 0, data = splits2$train.mdata)
#' }
#'
#' @importFrom caret createDataPartition
#' @export
splitsample <- function(data,
                        outcome = NULL,
                        group   = NULL,
                        choice  = NULL,
                        alt     = NULL,
                        p       = 0.75,
                        seed    = 4320) {
   
   # ---- helpers ----
   is_scalar_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
   
   assert_col_string <- function(x, arg) {
      if (is.null(x)) return(invisible(NULL))
      if (!is_scalar_string(x)) {
         stop(sprintf("`%s` must be a single character string (e.g., \"%s\").", arg, arg), call. = FALSE)
      }
      invisible(NULL)
   }
   
   assert_has_col <- function(df, col, arg) {
      if (!col %in% names(df)) {
         stop(sprintf("`%s` '%s' not found in `data`.", arg, col), call. = FALSE)
      }
      invisible(NULL)
   }
   
   # robust "is chosen?" converter
   as_chosen_logical <- function(x) {
      if (is.logical(x)) return(x)
      
      if (is.numeric(x) || is.integer(x)) {
         return(!is.na(x) & x == 1)
      }
      
      xx <- tolower(trimws(as.character(x)))
      yes_vals <- c("yes","y","true","t","1","chosen","choice","select","selected")
      out <- xx %in% yes_vals
      out[is.na(xx) | xx == ""] <- FALSE
      out
   }
   
   # ---- validate basics ----
   if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
   if (!is.numeric(p) || length(p) != 1 || p <= 0 || p >= 1) {
      stop("`p` must be a single number strictly between 0 and 1.", call. = FALSE)
   }
   
   # Require names as strings (when provided)
   assert_col_string(outcome, "outcome")
   assert_col_string(group,   "group")
   assert_col_string(choice,  "choice")
   assert_col_string(alt,     "alt")
   
   set.seed(seed)
   
   # ============================================================
   # CASE 1: GROUP-LEVEL SPLIT
   # ============================================================
   if (!is.null(group)) {
      
      assert_has_col(data, group, "group")
      
      groups <- unique(data[[group]])
      groups <- groups[!is.na(groups)]
      if (length(groups) < 2) stop("`group` must have at least 2 unique values.", call. = FALSE)
      
      have_choice_alt <- !is.null(choice) && !is.null(alt)
      
      # Optional stratification label at group level (based on chosen alternative)
      y_group <- NULL
      
      if (have_choice_alt) {
         assert_has_col(data, choice, "choice")
         assert_has_col(data, alt,    "alt")
         
         ch <- as_chosen_logical(data[[choice]])
         
         chosen_alt <- tapply(seq_len(nrow(data)), data[[group]], function(idx) {
            idx1 <- idx[ch[idx]]
            if (length(idx1) == 0) return(NA_character_)
            as.character(data[[alt]][idx1[1]])
         })
         
         nonmiss <- !is.na(chosen_alt)
         if (sum(nonmiss) >= 2) y_group <- as.factor(chosen_alt[nonmiss])
      }
      
      # Split groups
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
      
      train <- data[data[[group]] %in% train_groups, , drop = FALSE]
      test  <- data[data[[group]] %in% test_groups,  , drop = FALSE]
      
      # dfidx objects (optional)
      train_mdata <- NULL
      test_mdata  <- NULL
      
      if (have_choice_alt) {
         if (!requireNamespace("dfidx", quietly = TRUE)) {
            stop("Package 'dfidx' is required to create `train.mdata`/`test.mdata`.", call. = FALSE)
         }
         
         train2 <- train
         test2  <- test
         train2[[choice]] <- as_chosen_logical(train2[[choice]])
         test2[[choice]]  <- as_chosen_logical(test2[[choice]])
         
         chosen_per_case <- tapply(train2[[choice]], train2[[group]], sum, na.rm = TRUE)
         
         if (any(chosen_per_case != 1)) {
            warning(
               "Not creating train.mdata/test.mdata: some groups do not have exactly one chosen alternative.",
               call. = FALSE
            )
         } else {
            train_mdata <- dfidx::dfidx(train2, idx = c(group, alt), choice = choice)
            test_mdata  <- dfidx::dfidx(test2,  idx = c(group, alt), choice = choice)
         }
      }
      
      return(invisible(list(
         train = train,
         test = test,
         train_groups = train_groups,
         test_groups = test_groups,
         train.mdata = train_mdata,
         test.mdata = test_mdata
      )))
   }
   
   # ============================================================
   # CASE 2: CLASSIC ROW-LEVEL SPLIT
   # ============================================================
   if (is.null(outcome)) {
      stop("When `group` is NULL, you must provide `outcome` as a string.", call. = FALSE)
   }
   
   assert_has_col(data, outcome, "outcome")
   if (!is.factor(data[[outcome]])) stop("Outcome must be a factor.", call. = FALSE)
   
   inTrain <- caret::createDataPartition(y = data[[outcome]], p = p, list = FALSE)
   train <- data[inTrain,  , drop = FALSE]
   test  <- data[-inTrain, , drop = FALSE]
   
   invisible(list(train = train, test = test))
}
