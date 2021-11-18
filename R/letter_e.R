#' Knot points describing the shape of lowercase "e"
#'
#' @export
letter.e <- tibble::tribble(
  ~t, ~x, ~y,
  1, 449, 103,
  2, 261, 236,
  3, 189, 482,
  4, 275, 681,
  5, 448, 742,
  6, 581, 722,
  7, 734, 636,
  8, 732, 556,
  9, 682, 584,
  10, 434, 643,
  11, 301, 508,
  12, 302, 490,
  13, 428, 521,
  14, 635, 322,
  15, 449, 103
) |> dplyr::mutate(y = 750-y)

#' @export
letter.e.hole <- tibble::tribble(
  ~ t, ~ x, ~ y,
  1, 474, 216,
  2, 521, 284,
  3, 381, 421,
  4, 322, 409,
  5, 341, 343,
  6, 361, 309,
  7, 474, 216
)  |> dplyr::mutate(y = 750-y)
