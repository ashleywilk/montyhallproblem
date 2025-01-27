#'#'1.2.1 Install Development Packages
#'
install.packages(c("devtools", "roxygen2","usethis","testthat","knitr"))
# if devtools is not working try
# devtools::build_github_devtools()
#'
library(devtools)
has_devel()
#'
#'
#1.2.2 Build a Package Skeleton


# devtools::create_package() has been deprecated
usethis::create_package( "montyhall" )

#This example is using the default “documents” directory:
setwd( ".." )  # move up one level with two periods
getwd(documents)        # should be /documents NOT /montyhall
devtools::install( "montyhall" )

documents     # should be back here
├─ montyhall
│  ├─ R
│  ├─ man
#'
#'
#'
#'
#'1.2.3 Document Your Functions
#'
#'
#'
#' @title
#'  Monty Hall Game Function
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
#' Selecting a door Function
#' @description
#'  Simulates the contestant selecting one of the three doors at random.
#' @details
#' The player will choose 1 of 3 doors at random, 2 doors have goats
#' and 1 door is the winner, a car.
#' @param
#' no arguments are used by the function
#' @return
# An integer (1, 2, or 3) representing the contestant's selected door.
#' @examples
#' select_door ()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Assigning the goat door function
#' @description
#' Simulates the host opening a door to reveal a goat, ensuring
# it is not the players pick or the car door.
#' @param
#'  a.game A character vector representing the game setup.
#' @param
#' a.pick An integer representing the contestant's initial door pick.
#' @return
#' An integer representing the door opened by the host.
#' @details
#' Ensures the door that is opened is a goat door, and not a car door.
#' @examples
#' opened.door <- open_goat_door(this.game, my.initial.pick)
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
#' Allowing the door to be changed by the player if they
#' choose to switch
#' @description
#' Decides whether or not the players door is a goat or a car
#' and dependent on them switching or staying
#' @details
#' Setting the change_door function to allow the player to decide and give
#' the game the stay or switch logic and finding which door the player chose
#' based on the strategy they selected.
#' @param
#' stay strategy; if TRUE, the player stays with their initial pick.
#'  If FALSE, they switch to the other unopened door.
#' @param
#' opened.door  representing the door opened by the game
#' @param
#' a.pick  representing the players first door pick.
#' @return
#' representing the players final door choice.
#' @examples
#'
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
#' Determine the Winner function
#' @description
#' Determining the winner of the game if the player selected
#' the door with the car behind it.
#' @details
#' If door pick equals car they win, if the door pick is a goat
#' they lose.
#' @param
# final.pick representing the players final door choice.
#' @param
#' game representing the game setup.
#' @return
#' A character string: "WIN" if the player selected the door
#' with the car, otherwise "LOSE"
#' @examples
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
#' Play a single game of Monty Hall
#' @description
#'Simulates a single round of the Monty Hall
#'game with both strategies: staying and switching.
#' @details
#' The function executes one game by creating a new setup,
#'  making initial and final picks, and
#'  determining the outcomes for both strategies.
#' @return
#' A data frame with strategies ("stay" and "switch") and
#'  their outcomes ("WIN" or "LOSE").
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
#' Play 100 games of Monty Hall
#' @description
#' Simulates 100 rounds of the Monty Hall
#'game with both strategies: staying and switching.
#' @details
#' The function executes 100 games by creating a new setup,
#'  making initial and final picks, and
#'  determining the outcomes for both strategies.
#' @return
#' A data frame with strategies ("stay" and "switch") and
#'  their outcomes ("WIN" or "LOSE").
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
