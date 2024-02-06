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
#'    Contestant selects a door.
#'
#' @description
#'    `select_door()` replicates a contestant choosing one of
#'    the 3 available doors.
#'
#' @details
#'    This step allows us to replicate the randomized selection of a door,
#'    which directly imitates how this action is done during the real game.
#'
#' @param ... no arguments are used by the function.
#'
#' @return  The function returns a numeric variable that correlates to the
#'    door number selected.
#'
#' @examples
#'    select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens a goat door.
#'
#' @description
#'   In the game, after the contestant makes a pick, the host opens a door
#'   with a goat behind it. If the contestant originally picked a goat door,
#'   the host only has one option to open. However, if the contestant picked
#'   the door with a car behind it, then the host has to pick one of 2 available
#'   doors that have a goat behind it.
#'
#' @details
#'   For this step, the function will pick a door that is neither the "car"
#'   door or the door picked by the `select_door()` function.
#'   This will always be a goat door.
#'
#' @param
#'   Arguments for this function are game, which is the current game being
#'   played, as in the current order of the goats and car doors. The other
#'   argument is a.pick, which is the contestants original pick.
#'
#' @return
#'   The function returns a numeric variable that correlates to the goat door
#'   opened by the host.
#'
#' @examples
#'   open_goat_door( game, a.pick )
#'
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
#'    Contestant changes their selection.
#'
#' @description
#'    After the host opens a goat door, the change_door() is the contestant
#'    deciding whether to stay with their current selection or switch to a
#'    new door using the argument "stay".
#'
#' @details
#'    If the contestant chooses to stay (stay = T), then the numeric value
#'    "a.pick" is returned. If the contestant chooses to switch (stay = F),
#'    than the numeric value that is not "a.pick" or "opened.door" is returned.
#'
#' @param
#'    The arguments for this function are "stay=T" which is if the contestant
#'    chooses to stay or switch. opened.door is the numeric value of the opened
#'    goat door in the previous step "open_goat_door". And a.pick is the
#'    numeric value for the contestants original pick.
#'
#' @return
#'    The function returns a numeric value representing the
#'    final door selection.
#'
#' @examples
#'    change_door( stay=T, opened.door, a.pick )
#'    change_door( stay=F, opened.door, a.pick )
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
#'    Determine whether the contestant has won or lost the game.
#'
#' @description
#'    `determine_winner()` function takes the character value of "final" pick,
#'    and if the value is "car", the function returns "WIN". And if the value
#'    is "goat", the function returns "LOSE".
#'
#' @details
#'    "final.pick" contains the numeric value for the door selection. Using the
#'    [] operator on the vector "game", if the value of "final.pick" corresponds
#'    to "car" in the "game" vector, than the function returns the character
#'    value "WIN". If the value of "final.pick" corresponds to "goat" in the
#'    "game" vector, the the function returns the character value "LOSE".
#'
#' @param
#'    The arguments for this function are as follows: final.pick, which
#'    contains the numeric value corresponding with the door selection. The
#'    other argument, game, contains the character vector the contains the
#'    current game being played.
#'
#' @return
#'    The function returns a character value that tells you if the contestant
#'    wins or loses, those values being "WIN" and "LOSE".
#'
#' @examples
#'    determine_winner( final.pick, game )
#'
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
#'    Combine steps into single play_game() function
#'
#' @description
#'    `play_game()` combines all previous step into one single function so you
#'    can now complete a game for the Monty Hall problem.
#'
#' @details
#'    This function uses the results generated from previous steps and will
#'    result in a table that shows how each strategy (stay vs switch) works
#'    out (win vs loss).
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#'    Function returns a table with that shows if staying or swithing wins or
#'    loses.
#'
#' @examples
#'    play_game()
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
#'    Play game simulation/loop
#'
#' @description
#'    This function allows you to loop the `play_game()` function n times to get
#'    a better idea of which strategy works best.
#'
#' @details
#'    This function will currently run the game 100 times and print those
#'    results in a table to show how many times each strategy (stay or switch)
#'    wins and losses.
#'
#' @param
#'    The argument for this function is n where you can set it n equal to the
#'    number of times you wish to simulate the game.
#'
#' @return
#'    Returns a table listing the numeric count of wins and losses for the
#'    strategies.
#'
#' @examples
#'    play_n_games (n=100)
#'    play_n_games (n=1000)
#'    play_n_games (n=10000)
#'
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
