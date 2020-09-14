#' @title
#' Monty Hall Game (Single Game)
#'
#' @description
#' `play_game(game.size=3, num.cars=1)` randomly generates and plays a generalized
#' version of the Monty Hall game.
#'
#' @details
#' This function first generates a random Monty Hall game where there are `game.size` doors.
#' Behind `num.cars` doors there are cars and behind the rest we have goats. The contestant
#' selects a door, then the host reveals a goat behind one of the remaining doors. Then the
#' contestant can choose to either stay with their initial selection or to switch to another
#' door. Finally, based on the final selection, the contestant wins if there is a car behind
#' their selected door. The original version of the game includes 3 doors and 1 car.
#'
#' Read more about the game in <https://en.wikipedia.org/wiki/Monty_Hall_problem>.
#'
#'
#' @param game.size An integer.
#' @param num.cars An integer greater than zero and not larger than `game.size`-2.
#'
#' @return The function returns a data frame including the game vector, the initial door selection,
#' the goat door opened by the host, the strategy to stay or to switch, the final door
#' selection, and the outcome.
#'
#'
#' @examples
#' play_game(3,1)
#' play_game(10,3)
#'
#' \dontrun{
#' play_game(5,4)
#' }
#'
#' @export


play_game <- function (game.size=3, num.cars=1)
{
  new.game <- create_game(game.size,num.cars)
  first.pick <- select_door(game.size)
  opened.goat.door <- open_goat_door( new.game, first.pick )
  final.pick.stay <- change_door( stay=T, new.game, opened.goat.door, first.pick )
  final.pick.switch <- change_door( stay=F, new.game, opened.goat.door, first.pick )
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  game <- paste0( new.game, collapse=" " )
  final.pick <- c(final.pick.stay,final.pick.switch)
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( game, first.pick, opened.goat.door, strategy, final.pick, outcome,
                              stringsAsFactors=F )
  return( game.results )
}
