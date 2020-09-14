#' @title
#' Monty Hall Game Door Change
#'
#' @description
#' `change_door ( stay = TRUE, game, opened.door, a.pick )` runs the third step of
#'  a Monty Hall game where the contestant decides either to stay on the same door or to change
#'  their decision.
#'
#' @details
#' Given a game vector, a door number that was the contestant's first selection, the
#' goat door revealed by the host, and a decision by the contestant whether to stay or
#' switch doors, this function makes the final door selection. If the contestant stays,
#' the function returns the door number of the initial selection, otherwise, it randomly
#' chooses one of the doors that were neither selected by the contestant or revealed by
#' the host.
#'
#' @param stay Logical. Stay on the same door or not?
#' @param game A vector of any class and length.
#' @param opened.door An integer that is not larger than the length of the `game` vector.
#' @param a.pick An integer that is not larger than the length of the `game` vector.
#'
#' @return The output is an integer number between, and including, 1 and `length(game)`
#' which equals `a.pick` if `stay = TRUE`. Otherwise, it returns a number different from
#' `a.pick` and `opened.door`.
#'
#'
#' @examples
#' change_door( stay=T, c("goat", "goat", "car" ), 2, 1 )
#' change_door( stay=F, create_game(5), 4, 2 )
#'
#' \dontrun{
#' change_door( stay=T, c("goat", "car", "car" ), 1, 5 )
#' }
#' @export


change_door <- function( stay=T, game, opened.door, a.pick )
{

  if(stay==T){
    final.pick <- a.pick
  } else {
    doors <- 1:length(game)
    options <- doors[doors!=a.pick & doors!=opened.door]
    final.pick <- sample(options,size = 1,replace=F)
  }

  return( final.pick )

}
