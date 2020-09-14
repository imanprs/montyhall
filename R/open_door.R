#' @title
#' Monty Hall Game Goat Door Reveal
#'
#' @description
#' `open_goat_door( game, a.pick )` randomly chooses a goat door
#' that was not picked by the contestant.
#'
#' @details
#' This function does the second step of a Monty Hall game where the host reveals a
#' door with a goat behind it. Given a game vector and a number for the door selectd by
#' the contestant, this function returns a door number.
#'
#' @param game A vector of any class and length.
#' @param a.pick An integer that is not larger than the length of the `game` vector.
#'
#' @return The output is an integer number between, and including, 1 and `length(game)`
#' which is different from `a.pick`. There should a "goat" element in the `game` vector in
#' an element different from `a.pick`th element.
#'
#'
#' @examples
#' open_goat_door( c("goat", "goat", "car" ), 3 )
#' open_goat_door( create_game(5), 11 )
#'
#' \dontrun{
#' open_goat_door( c("goat", "car", "car" ), 1 )
#' }
#' 
#' @export


open_goat_door <- function( game, a.pick )
{
  doors <- 1:length(game)

  if (game[a.pick]=="goat") {
    num.goats <- sum(game=="goat") - 1
  } else {
    num.goats <- sum(game=="goat")
  }

  if(num.goats>1) {

    goats <- doors[doors!=a.pick & game=="goat"]
    opened.door <- sample(goats, size=1, replace=F )

  } else {
    opened.door <- doors[doors!=a.pick & game=="goat"]
  }
  return( opened.door )
}