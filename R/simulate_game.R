#' @title
#' Monty Hall Game Simulation
#'
#' @description
#' `simulate_game(game.size=3, num.cars=1, rounds=10000)` simulates a generalized
#' version of the Monty Hall game.
#'
#' @details
#' This function first generates and plays random Monty Hall games where in all of the games
#' there are `game.size` doors and behind `num.cars` doors there are cars and behind the rest
#' we have goats. The game is played for `rounds` number of times. The function reports the
#' number of times where stay and switch strategies lead to winning or losing.
#'
#' Read more about the game in <https://en.wikipedia.org/wiki/Monty_Hall_problem>.
#'
#' @param rounds An integer.
#' @param game.size An integer.
#' @param num.cars An integer greater than zero and not larger than `game.size`-2.
#'
#' @return The function returns a table showing the number of times where stay
#' and switch strategies lead to winning or losing.
#'
#'
#' @examples
#' simulate_game(4, 2, 20000)
#' simulate_game()
#'
#' \dontrun{
#' simulate_game(3, 2, 10000)
#' }
#'
#' @export


simulate_game <- function(game.size=3, num.cars=1, rounds=10000)
{

  results.df <- NULL   # collector
  for( i in 1:rounds )  # iterator
  {
    game.outcome <- play_game(game.size,num.cars)
    game.final.outcome <- game.outcome[ , c( 4 , 6 ) ]
    # binding step
    results.df <- rbind( results.df, game.final.outcome )
  }

  return( table( results.df ) )

}

