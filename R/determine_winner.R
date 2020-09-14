#' @title
#' Monty Hall Game Winner Test
#'
#' @description
#' `determine_winner( final.pick, game )` determines if the selected door is
#' a car door and the contestant is a winner.
#'
#' @details
#' Given a final door selection and a game vector, this function determines if that door
#' selection leads to the contestant being the winner. It checks if the door number is
#' associated with a "car" in the game vector.
#'
#' @param game A vector of any class and length.
#' @param final.pick An integer greatr than zero and not larger than the length of the `game` vector.
#'
#' @return If the `final.pick`th element oof the game vector is "car", it returns "WINNER", and if
#' it equals "goat" it returns "LOSER".
#'
#'
#' @examples
#' determine_winner( 2, c("goat", "goat", "car" ))
#' determine_winner( 1, create_game(6), 3 )
#'
#' \dontrun{
#' determine_winner( 4, c("goat", "car", "car" ))
#' }
#' 
#' @export

determine_winner <- function( final.pick, game )
{

  if(game[final.pick]=="car")
  {
    return( "WIN" )
  }
  if(game[final.pick]=="goat")
  {
    return( "LOSE" )
  }

}
