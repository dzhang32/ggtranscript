


#' @noRd
to_intron <- function(x, group_var = NULL, start_var = start, end_var = end) {

    # grouping by NULL (default) does nothing
    x <- x %>%
        dplyr::group_by({{ group_var }})

    # make sure exons are arranged by coord, so that dplyr::lag works correctly
    x <- x %>%
        dplyr::arrange({{ start_var }}, {{ end_var }})

    # obtain intron start and ends
    x <- x %>%
        dplyr::mutate(
            intron_start := dplyr::lag({{ end_var }}) + 1,
            intron_end := {{ start_var }} - 1
        ) %>%
        dplyr::select(-{{ start_var }}, -{{ end_var }})

    # for each group, output N of introns should be N - 1 the inputted exons
    # remove the introduced artifact NAs
    x <- x %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(intron_start) & !is.na(intron_end))

    return(x)
}
