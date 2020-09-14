#' @title
#' Monty Hall Game Generator
#'
#' @description
#' `create_game(game.size, num.cars)` creates a randomly generated
#' Monty Hall game, generalized for any number of doors in the game.
#'
#' @details
#' This function generates a vector of length `game.size` the elements of which represent
#' the doors where either a goat or a car is hidden behind each. `num.cars` is the number of
#' cars in the game.
#'
#' @param game.size An integer.
#' @param num.cars An integer greater than zero and not larger than `game.size`-2.
#'
#' @return The output is a character vector of length `game.size`. Each element is
#' either "car" or "goat".
#'
#'
#' @examples
#' create_game(3,1)
#' create_game(10,3)
#'
#' \dontrun{
#' create_game(5,4)
#' }
#' 
#' @export


create_game <- function(game.size=3, num.cars=1)
{
  cars <- rep("car",times=num.cars)
  goats <- rep("goat",times=game.size-num.cars)
  doors <- c(cars,goats)
  a.game <- sample(doors, size=game.size, replace=F )
  return( a.game )
}
