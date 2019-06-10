#================= BlackJack Game model development and fun ===============#
#------------------ Created by Kyung Jang - 2019-05-31 --------------------#

rm(list=ls())
library(stringr)

##########################################################################
#----------- Create your game strategy to win BlackJack -----------------#

should_hit <- function(player_total, dealer_card_val, player_aces){
  return(FALSE)
}

##########################################################################

#BlackJack

deck <- c(as.character(2:10), 'A','J', 'Q', 'K')

deal <- function(x) {
  return(sample(deck, x))
}

# Improvement Needed: Incase of multiple Aces! 
#(Work with total instead of counting deck over again)
deck_total <- function(x) {
  x <- str_replace(x, '[JQK]', '10')
  
  if (sum(as.numeric(str_replace(x, '[A]', '11'))) <= 21) {
    x <- str_replace(x, '[A]', '11')
  } else {
    x <- str_replace(x, '[A]', '1')
  }
  
  return(sum(as.numeric(x)))
}

# Temp function: Train case bahaviour!
player_should_hit <- function(x) {
  if (x >= 17) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

dealer_should_hit <- function(x, y) {
  if (x >= 16 || x >= y || y > 21) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#Game over when - Player or Dealer busted
#               - No more deal
# FIxxxxx
game_over <- function(player_total, dealer_total){
  if (player_total > 21 || dealer_total >21) {
    return(TRUE)
  } else if(!player_should_hit(player_total) && !dealer_should_hit(dealer_total, player_total)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

who_won <- function(x, y) {
  if (x > 21) {
    return(FALSE)
  } else {
    if (y >= x) {
      if (y > 21) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  }
}

line_sep <- function() {
  print(paste(rep('-', 50), collapse ='' ))
}




#-------------------------- sub main function ------------------------------#
simulate_one_game <- function() {
  dealer <- deal(1)
  player <- deal(2)
  
  dealer_total <- deck_total(dealer)
  player_total <- deck_total(player)
  
  print(paste('Dealer starts with ', dealer))
  print(paste('Player starts with ', player [1], 'and ', player [2], ' (Total = ', player_total, ')'))
  line_sep()
  
  # Could be a function : play <- function(x)...
  while (player_should_hit(player_total)) {
    deal_card <- deal(1)
    player <- c(player, deal_card)
    player_total <- sum(deck_total(player))
    
    print(paste('Player hits and receives ', deal_card, '. (Total = ', player_total, ')'))
  }
  line_sep()
  
  # Could be a function : play <- function(x)...
  while (dealer_should_hit(x = dealer_total, y=player_total)) {
    deal_card <- deal(1)
    dealer <- c(dealer, deal_card)
    dealer_total <- sum(deck_total(dealer))
    
    print(paste('Dealer hits and receives ', deal_card, '. (Total = ', dealer_total, ')'))
  }
  line_sep()
  
  print('Game Ends:')
  print(paste('Dealer total: ', dealer_total, ' & Player total: ', player_total))
  
  if (player_total > 21) {
    print('You are Busted: You lose!')
  } else {
    if (dealer_total >= player_total) {
      if (dealer_total > 21) {
        print('Dealer Bust!: You win!')
      } else {
        print('Dealer Wins: You lose!')
      }
    } else {
      print('Dealer loses: You win!')
    }
  }
}


simulate_one_game()

#------ Simulate multiple at once ------#

simulate_blackjack <- function(x =500) {
  win_total <- c()
  v_hit_count <- c()
  play_data <- data.frame()
  
  for (i in 1:x) {
    dealer <- deal(1)
    player <- deal(2)
    
    dealer_total <- deck_total(dealer)
    player_total <- sum(deck_total(player))
    
    hit_count <- 0
    
    while (!game_over(player_total, dealer_total)) {
      if (player_should_hit(player_total)){
        deal_card <- deal(1)
        player <- c(player, deal_card)
        player_total <- sum(deck_total(player))
      }
      
      if (dealer_should_hit(x = dealer_total, y = player_total)){
        deal_card <- deal(1)
        dealer <- c(dealer, deal_card)
        dealer_total <- sum(deck_total(dealer))
      }
    }
    
    winner <- who_won(player_total, dealer_total)
    win_total <- c(win_total, winner)
  
    df_temp <- data.frame(player_card = paste(player, collapse = ' '), dealer_card =  paste(dealer, collapse = ' '), win_lose = winner)
    play_data <- rbind(play_data, df_temp)
  }
 
   result <-(paste('You won ', sum(win_total), ' / ', x, '. Win rate: ',sum(win_total)/x))
   
  return(play_data)
}

simulate_blackjack(500)

