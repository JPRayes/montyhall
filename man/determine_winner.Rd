% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{determine_winner}
\alias{determine_winner}
\title{Determine if the contestant won or lost}
\usage{
determine_winner(final.pick, game)
}
\arguments{
\item{parameters}{are set around the final pick in the game}
}
\value{
"LOSE" or "WIN"
}
\description{
The host reveals whether the contestant wins or loses
by revealing the door with the car
}
\details{
the 'determine_winner' function is the culmination of the previous functions.
if, else statements are again used to determine the outcome of the game.
if the final pick is a car, the result displays "WIN"
if the final pick is a goat, the result displays "LOSE"
}
\examples{
determine_winner -> LOSE
determine_winner -> WIN
}
