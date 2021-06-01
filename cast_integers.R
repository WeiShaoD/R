#' Safely cast numeric columns to integers
#'
#' `cast_integers()` casts all eligible numeric columns in a data frame to integers, without data loss, using `vctrs::vec_cast()` for coercion.
#' @param .data A data frame
#' @return A tibble. If the input data frame has rownames, these will be preserved in a new rowname column.
#' @examples (mtcars_integerized <- cast_integers(mtcars))
#' @export
cast_integers <- function(.data) {
    stopifnot(is.data.frame(.data))
    .data <- tibble::rownames_to_column(.data)
    .data <- tibble::as_tibble(.data)
    int_index <- purrr::map_lgl(
        .data,
        ~ !inherits(try(vctrs::vec_cast(.x, to = integer()), silent = TRUE), "try-error")
    )
    .data <- dplyr::mutate(
        .data,
        dplyr::across(
            .cols = any_of(names(which(int_index))),
            .fns = ~ vctrs::vec_cast(.x, to = integer())
        )
    )
    return(.data)
}