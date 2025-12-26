# Internal helpers for easy_mp() ---------------------------------------------

get_x_title <- function(x) {
   # If you already had specific logic in mind, plug it in here.
   # This default is safe and simple.
   if (is.null(x) || !nzchar(x)) return("")
   x
}

get_legend_title <- function(group) {
   if (is.null(group) || !nzchar(group)) return("")
   group
}
