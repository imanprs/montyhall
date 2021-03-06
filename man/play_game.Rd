% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/play_game.R
\name{play_game}
\alias{play_game}
\title{Monty Hall Game (Single Game)}
\usage{
play_game(game.size = 3, num.cars = 1)
}
\arguments{
\item{game.size}{An integer.}

\item{num.cars}{An integer greater than zero and not larger than \code{game.size}-2.}
}
\value{
The function returns a data frame including the game vector, the initial door selection,
the goat door opened by the host, the strategy to stay or to switch, the final door
selection, and the outcome.
}
\description{
\code{play_game(game.size=3, num.cars=1)} randomly generates and plays a generalized
version of the Monty Hall game.
}
\details{
This function first generates a random Monty Hall game where there are \code{game.size} doors.
Behind \code{num.cars} doors there are cars and behind the rest we have goats. The contestant
selects a door, then the host reveals a goat behind one of the remaining doors. Then the
contestant can choose to either stay with their initial selection or to switch to another
door. Finally, based on the final selection, the contestant wins if there is a car behind
their selected door. The original version of the game includes 3 doors and 1 car.

Read more about the game in \url{https://en.wikipedia.org/wiki/Monty_Hall_problem}.
}
\examples{
play_game(3,1)
play_game(10,3)

\dontrun{
play_game(5,4)
}

}
