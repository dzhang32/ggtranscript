#' @keywords internal
#' @noRd
.check_coord_object <- function(x) {
    if (!is.data.frame(x)) {
        stop(
            "x must be a data.frame object. ",
            "GRanges objects are currently not supported and must be converted ",
            "using e.g. as.data.frame()"
        )
    }

    if (!all(c("start", "end") %in% colnames(x))) {
        stop("x must have the columns 'start' and 'end'")
    }
}

#' @keywords internal
#' @noRd
.check_group_var <- function(x, group_var) {
    if (!is.null(group_var)) {
        if (!all(group_var %in% colnames(x))) {
            stop(
                "group_var ('", group_var, "') ",
                "must be a column in x"
            )
        }
    }
}
