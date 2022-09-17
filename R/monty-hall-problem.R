#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#' Contestant randomly selects one of the three doors
#' @description
#' 'doors' divides each of the items into three separate parts.
#' 'sample()' randomly assigns each of the items to a door
#'  and 'a.pick' randomly selects one of the three doors.
#'
#' @details
#' doors creates a vector of three integers
#' a.pick samples one of the three integers (1,2,or 3) as doors
#'
#' @param
#' no arguments are used by the function.
#' @return
#' 'a.pick' randomly selects a sample of one of the three integers in the vector 'doors'
#' @examples
#' create(select_door)
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens a goat door
#' @description
#' After the contestant selects a door, the host then opens one door, revealing a goat.
#' This sets the stage for the next step where the contestant can keep his first pick or change his pick.
#' @details
#' #' the function 'open_goat_door' is created, combining the a.game and a.pick functions
#' if, else statements are created select a goat door depending on the contestant's first pick
#' if the contestant picks the car door, the host can open one of two remaining doors.
#' if the contestant picks a goat door, the host must open the one remaining goat door.
#' @param
#' parameters are set around the hosts door selection based on the contestant's first pick.
#'
#' @return
#' 'opened.door' selects a goat door based on the conditions of the game.
#' @examples
#' open_goat_door -> 3
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Contestant chooses to change or not change door
#' @description
#' At this point in the game, the contestant is now given the option
#' to keep their first door selection or switch their choice
#' to the last remaining door
#' @details
#' 'change_door" function is created building on the 'opened.door' and 'a.pick' functions.
#' if, else statements are again created, this time based on the choice of the contestant.
#' if the contestant chooses to stay,
#' their final pick 'final.pick' is equal to their initial pick 'a.pick'
#' if the contestant chooses to switch,
#' their final pick 'final.pick' is not equal to their initial pick
#' and also not equal to the host's pick 'opened.door'
#' @param
#' parameters are set around the contestant's choice to stay or switch
#' @return
#' return(final.pick) -> returns the door number of the contestant's final pick
#' @examples
#' change_door -> 1
#' change_door -> 2
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if the contestant won or lost
#' @description
#' The host reveals whether the contestant wins or loses
#' by revealing the door with the car
#' @details
#' the 'determine_winner' function is the culmination of the previous functions.
#' if, else statements are again used to determine the outcome of the game.
#' if the final pick is a car, the result displays "WIN"
#' if the final pick is a goat, the result displays "LOSE"
#' @param
#' parameters are set around the final pick in the game
#' @return
#' "LOSE" or "WIN"
#' @examples
#' determine_winner -> LOSE
#' determine_winner -> WIN
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play the game from start to finish to test the code
#' @description
#' Now that the code is created,
#' the game can be play from beginning to end using one function.
#' All of the previous functions are packaged into play_game.
#' @details
#' the 'play_game' function encompasses all of the steps of the game
#' @param
#' parameters are set around the play_game function.
#'
#' the stay command is incorporated into the final code
#' @return
#' return(game.results)
#' @examples
#' play_game ->
#' Game setup: GOAT GOAT CAR
#' Initial Selection: 1,
#' The opened goat door: 2
#' Final door selection: 3
#' Game Outcome: WIN
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Simulate the monty hall game
#' @description
#' Now that the game has been created and packaged into one function,
#' we will simulate the game using loops to test whether staying
#' or switching is the dominant strategy for the contestant
#' @details
#' the game is played a set number of times using 'n'
#' the 'for()' argument is made to iterate the number of time the game is played
#' the results are then displayed in a dataframe 'results.df'
#' @param
#' parameters are set around the number of iterations the game will have
#' @return
#' return(results.df)
#' @examples
#' table(results.df)
#' outcome ->
#' strategy -> stay: Lose-60, Win-40; switch: Lose: 40, Win: 60
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
