#' @title
#' Monty Hall Door Selector
#'
#' @description
#' `select_door(game)` randomly chooses a door among in a Monty Hall door.
#'
#' @details
#' This function does the first step of a Monty Hall game where the contestant has to
#' choose a door. Given a game vector, this function returns a door number.
#'
#' @param game A vector of any class and length.
#'
#' @return The output is a number between, and including, 1 and `length(game)`.
#'
#'
#' @examples
#' select_door(c("car", "car", "goat"))
#' select_door(c(1, 2, 3, 4, 5))
#' select_door(create_game(4,1))
#'
#'@export


select_door <- function(game)
{
  doors <- 1:length(game)
  a.pick <- sample( doors, size=1, replace=F )
  return( a.pick )
}
