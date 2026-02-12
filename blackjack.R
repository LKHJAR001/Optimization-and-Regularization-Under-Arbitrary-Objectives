rm(list = ls())
library(compiler)
enableJIT(3)
# Early surrender, no re-split, no surrender on split, DAS allowed
#========================================================================
#                 Set up a game universe (Encoding)
#========================================================================
blackjack_env <- function(n_decks = 8, reshuffle_threshold = 4*52) {
  env <- list()
  state <- new.env()
  
  init_shoe <- function() {
    shoe <- rep(c(1:9, 10, 10, 10, 10), 4 * n_decks)
    sample(shoe)  # Shuffle the shoe
  }
  
  update_count <- function(card) {
    if (card >= 2 && card <= 6) state$running_count <- state$running_count + 1
    else if (card == 1 || card == 10) state$running_count <- state$running_count - 1
  }
  
  update_at_hist <- function(card) {
    state$at_hist <- c(state$at_hist, card)
  }
  
  draw_card <- function(count = TRUE) {
    if (length(state$deck) == 0) {
      state$deck <- init_shoe()
      state$running_count <- 0
    }
    card <- state$deck[1]
    state$deck <- state$deck[-1]
    if (count) {
      update_count(card)
      update_at_hist(card)
    }
    return(card)
  }
  
  hand_value <- function(hand) {
    val <- sum(hand)
    if (1 %in% hand && val + 10 <= 21) val <- val + 10
    val
  }
  
  usable_ace <- function(hand) {
    1 %in% hand && (sum(hand) + 10 <= 21)
  }
  
  env$reset <- function(bet_size = 1) {
    if (is.null(state$deck) || length(state$deck) < reshuffle_threshold) {
      state$deck <- init_shoe()
      state$running_count <- 0
      state$at_hist <- c()
    }
    
    state$player <- c(draw_card(), draw_card())
    dealer_up <- draw_card()
    dealer_hole <- draw_card(count = FALSE)
    state$dealer <- c(dealer_up, dealer_hole)
    state$done <- FALSE
    state$bet_size <- bet_size
    state$history <- list(
      player = list(state$player),
      dealer = list(dealer_up)
    )
    state$can_split <- (state$player[1] == state$player[2])
    state$split <- FALSE

    if (hand_value(state$player) == 21) {
      state$done <- TRUE
      update_count(dealer_hole)
      update_at_hist(dealer_hole)
      state$history$dealer[[length(state$history$dealer) + 1]] <- state$dealer
      reward <- if (hand_value(state$dealer) == 21) 0 else 1.5
      return(list(obs = env$get_obs(), reward = reward * state$bet_size, done = TRUE))
    }
    
    return(list(obs = env$get_obs(), reward = 0, done = FALSE))
  }
  
  env$get_obs <- function() {
    list(
      player_sum = hand_value(state$player),
      dealer_show = state$dealer[1],
      usable_ace = usable_ace(state$player),
      running_count = state$running_count,
      true_count = env$true_count(),
      done = state$done,
      bet_size = state$bet_size,
      history = state$history,
      at_history = state$at_hist,
      can_split = state$can_split && !state$split && length(state$player) == 2,
      can_surrender = length(state$player) == 2 && !state$split,
      can_doubledown = length(state$player) == 2
    )
  }
  
  env$step <- function(action) {
    if (state$done) stop("Game is over. Please reset.")
    

    
    if (action == 1) {
      # Hit
      new_card <- draw_card()
      state$player <- c(state$player, new_card)
      state$history$player[[length(state$history$player) + 1]] <- state$player
      if (state$split) {
        state$player_hands[[state$current_hand_index]] <- state$player
      }
      
      if (hand_value(state$player) == 21){
        return(env$step(action = 0))
      }
      
      if (hand_value(state$player) > 21) {
        if (state$split) {
          state$hand_rewards[state$current_hand_index] <- -1 * state$bet_sizes[state$current_hand_index]
          # Move to next hand if there is one
          if (state$current_hand_index < length(state$player_hands)) {
            state$current_hand_index <- state$current_hand_index + 1
            state$player <- state$player_hands[[state$current_hand_index]]
            return(list(obs = env$get_obs(), reward = 0, done = FALSE))
          } else {
            state$done <- TRUE
            return(list(obs = env$get_obs(), reward = sum(state$hand_rewards), done = TRUE))
          }
        }else{
          state$done <- TRUE
          return(list(obs = env$get_obs(), reward = -1*state$bet_size, done = TRUE)) 
        }
      }else{
        return(list(obs = env$get_obs(), reward = 0, done = FALSE)) 
      }
      
 

    } 
    if (action == 0) {
      # Stand
      if (state$split && state$current_hand_index < length(state$player_hands)) {
        # Move to next split hand
        state$current_hand_index <- state$current_hand_index + 1
        state$player <- state$player_hands[[state$current_hand_index]]
        # state$history$player <- list(state$player)
        return(list(obs = env$get_obs(), reward = 0, done = FALSE))
      }
      
      # Reveal hole card
      update_count(state$dealer[2])
      update_at_hist(state$dealer[2])
      state$history$dealer[[length(state$history$dealer) + 1]] <- state$dealer
      
      while (hand_value(state$dealer) < 17) {
        new_card <- draw_card()
        state$dealer <- c(state$dealer, new_card)
        state$history$dealer[[length(state$history$dealer) + 1]] <- state$dealer
      }
      
      # Evaluate rewards for each hand
      if (state$split) {
        for (i in seq_along(state$player_hands)) {
          hand <- state$player_hands[[i]]
          player_val <- hand_value(hand)
          dealer_val <- hand_value(state$dealer)
          if (player_val > 21) {
            state$hand_rewards[i] <- -1 * state$bet_sizes[i]
          }else{
            state$hand_rewards[i] <- if (dealer_val > 21 || player_val > dealer_val) {
              1 * state$bet_sizes[i]
            } else if (player_val < dealer_val) {
              -1 * state$bet_sizes[i]
            } else {
              0
            }
          }
        }
      state$done <- TRUE
      return(list(obs = env$get_obs(), reward = sum(state$hand_rewards), done = TRUE))
      }else {
        player_val <- hand_value(state$player)
        dealer_val <- hand_value(state$dealer)
        if (player_val > 21)
        {
          reward = -1*state$bet_size
        }else
        {
          reward <- if (dealer_val > 21 || player_val > dealer_val) {
            1 * state$bet_size
          } else if (player_val < dealer_val) {
            -1 * state$bet_size
          } else {
            0
          }
        }
        state$done <- TRUE
        return(list(obs = env$get_obs(), reward = reward, done = TRUE))
      }
    }
    
    if (action == 2 && state$can_split && !state$split && length(state$player) == 2) {
      # Perform split
      card1 <- state$player[1]
      card2 <- state$player[2]
      
      state$player_hands <- list(
        c(card1, draw_card()),
        c(card2, draw_card())
      )
      state$current_hand_index <- 1
      state$split <- TRUE
      state$hand_rewards <- c(0, 0)
      state$bet_sizes <- rep(state$bet_size, 2)
      state$player <- state$player_hands[[1]]
      # state$history$player <- list(state$player)
      state$history$player <- state$player_hands
      return(list(obs = env$get_obs(), reward = 0, done = FALSE))
    }
    if (action == 3) {
      # Surrender (only allowed if no hit/split has occurred)
      if (length(state$player) == 2 && !state$split) {
        state$done <- TRUE
        return(list(obs = env$get_obs(), reward = -0.5 * state$bet_size, done = TRUE))
      } 
    }
    if (action == 4) {
      # Double down: allowed if current hand has exactly 2 cards and hasn't been hit yet
      if (length(state$player) == 2) {
        new_card <- draw_card()
        state$player <- c(state$player, new_card)
        state$history$player[[length(state$history$player) + 1]] <- state$player
        
        # Handle bet increase
        if (state$split) {
          state$bet_sizes[state$current_hand_index] <- 2 * state$bet_sizes[state$current_hand_index]
        } else {
          state$bet_size <- 2 * state$bet_size
        }
        
        if (hand_value(state$player) > 21) {
          if (state$split) {
            state$hand_rewards[state$current_hand_index] <- -1 * state$bet_sizes[state$current_hand_index]
            # Move to next hand if there is one
            if (state$current_hand_index < length(state$player_hands)) {
              state$current_hand_index <- state$current_hand_index + 1
              state$player <- state$player_hands[[state$current_hand_index]]
              return(list(obs = env$get_obs(), reward = 0, done = FALSE))
            } else {
              state$done <- TRUE
              return(list(obs = env$get_obs(), reward = sum(state$hand_rewards), done = TRUE))
            }
          }else{
            state$done <- TRUE
            return(list(obs = env$get_obs(), reward = -2*state$bet_size, done = TRUE)) 
          }
        }else{
          return(env$step(action = 0))
        }

      }
    }
  }
  
  env$true_count <- function() {
    remaining_decks <- max(1, length(state$deck) / 52)
    round(state$running_count / remaining_decks, 2)
  }
  
  env$get_state <- function() state
  
  return(env)
}


basic_strategy_action <- function(player_sum, dealer_show, usable_ace, can_split, player_cards, can_surrender, can_doubledown) {
  # Check for pair (splits)
  if (can_split && length(player_cards) == 2 && player_cards[1] == player_cards[2]) {
    pair_val <- player_cards[1]
    if (pair_val == 1 || pair_val == 8) return(2)  # Always split Aces and 8s
    if (pair_val == 10) return(0)
    if (pair_val == 9 && dealer_show %in% c(2:6, 8, 9)) return(2)
    if (pair_val == 7 && dealer_show %in%  c(2:7)) return(2)
    if (pair_val == 6 && dealer_show %in% 2:6) return(2)
    if (pair_val == 5) return(1)  # treat as 10, don’t split
    if (pair_val == 4 && dealer_show %in% 5:6) return(2)
    if (pair_val == 3 && dealer_show %in%  c(2:7)) return(2)
    if (pair_val == 2 && dealer_show %in%  c(2:7)) return(2)
  }
  
  # Surrender
  if (!usable_ace && can_surrender) {
    if ((player_sum == 16 && dealer_show %in% c(9, 10, 1)) ||
        (player_sum == 15 && dealer_show == 10)) {
      return(3)  # Surrender
    }
  }
  
  # Soft totals (usable ace)
  if (usable_ace && can_doubledown) {
    if (player_sum == 19 && dealer_show == 6) return(4)
    if (player_sum == 18 && dealer_show %in% 2:6) return(4)  # Double soft 18 vs 3–6
    if (player_sum == 17 && dealer_show %in% 3:6) return(4)  # Double soft 17 vs 3–6
    if (player_sum == 16 && dealer_show %in% 4:6) return(4)  # etc.
    if (player_sum == 15 && dealer_show %in% 4:6) return(4)
    if (player_sum == 14 && dealer_show %in% 5:6) return(4)
    if (player_sum == 13 && dealer_show %in% 5:6) return(4)
  }
  
  # Hard totals with double down
  if (!usable_ace && can_doubledown) {
    if (player_sum == 11) return(4)  # Always double 11
    if (player_sum == 10 && dealer_show %in% 2:9) return(4)
    if (player_sum == 9 && dealer_show %in% 3:6) return(4)
  }
  
  # Default soft strategy
  if (usable_ace) {
    if (player_sum >= 20) return(0)
    if (player_sum == 19  && dealer_show %in% c(1:5,7:10)) return(0)
    if (player_sum == 18) {
      if (dealer_show %in% c(9, 10, 1)) return(1) else return(0)
    }
    return(1)
  }
  
  # Default hard strategy
  if (player_sum >= 17) return(0)
  if (player_sum >= 13 && dealer_show %in% 2:6) return(0)
  if (player_sum == 12 && dealer_show %in% 4:6) return(0)
  return(1)
}


get_bet_size <- function(true_count, min_bet = 0, max_bet = 10) {
  if (is.null(true_count) || is.na(true_count) || length(true_count) ==0 ) return(min_bet)
  if (true_count < 3) return(0)
  bet <- (true_count/3)
  return(min(bet, max_bet))
}


#========================================================================
#                           Control NN
#========================================================================
softmax = function(Z_mat)
{
  Z <- exp(Z_mat)
  den <- colSums(Z)
  den <- rep(den, nrow(Z))
  den <- matrix(den, nrow(Z), ncol(Z), byrow = TRUE)
  return (Z/den)
}
# softmax <- function(Z_mat) {
#   # Subtract max of each column for numerical stability
#   Z_shifted <- Z_mat - matrix(apply(Z_mat, 2, max), nrow(Z_mat), ncol(Z_mat), byrow = TRUE)
#   Z_exp <- exp(Z_shifted)
#   den <- colSums(Z_exp)
#   return(Z_exp / matrix(den, nrow(Z_mat), ncol(Z_mat), byrow = TRUE))
# }


model = function(X,theta,nodes, q = 1)
{
  # Infer dimensions:
  N = dim(X)[1]
  p = dim(X)[2]
  dims = c(p,nodes,q)
  
  # Populate weight and bias matrices:
  index = 1:(dims[1]*dims[2])
  W1    = matrix(theta[index],dims[1],dims[2])
  index = max(index)+1:(dims[2]*dims[3])
  W2    = matrix(theta[index],dims[2],dims[3])
  index = max(index)+1:(dims[3]*dims[4])
  W3    = matrix(theta[index],dims[3],dims[4])
  
  index = max(index)+1:(dims[2])
  b1    = matrix(theta[index],dims[2],1)
  index = max(index)+1:(dims[3])
  b2    = matrix(theta[index],dims[3],1)
  index = max(index)+1:(dims[4])
  b3    = matrix(theta[index],dims[4],1)
  
  ones    = matrix(1,1,N)
  a0      = t(X)
  
  # Evaluate the updating equation in matrix form
  a1 = tanh(t(W1)%*%a0+b1%*%ones)
  a2 = tanh(t(W2)%*%a1+b2%*%ones)
  # a3 = softmax(t(W3)%*%a2+b3%*%ones)
  # return(list(action = t(a3)))
  a3 = (t(W3)%*%a2+b3%*%ones)
  return(list(logits = t(a3)))
}

sigmoid = function(z)
{
  1/(1+exp(-z))
}

model_bet = function(X,theta,nodes, q = 1)
{
  # Infer dimensions:
  N = dim(X)[1]
  p = dim(X)[2]
  dims = c(p,nodes,q)

  # Populate weight and bias matrices:
  index = 1:(dims[1]*dims[2])
  W1    = matrix(theta[index],dims[1],dims[2])
  index = max(index)+1:(dims[2]*dims[3])
  W2    = matrix(theta[index],dims[2],dims[3])
  index = max(index)+1:(dims[3]*dims[4])
  W3    = matrix(theta[index],dims[3],dims[4])
  
  index = max(index)+1:(dims[2])
  b1    = matrix(theta[index],dims[2],1)
  index = max(index)+1:(dims[3])
  b2    = matrix(theta[index],dims[3],1)
  index = max(index)+1:(dims[4])
  b3    = matrix(theta[index],dims[4],1)
  
  ones    = matrix(1,1,N)
  a0      = t(X)
  
  # Evaluate the updating equation in matrix form
  a1 = tanh(t(W1)%*%a0+b1%*%ones)
  a2 = tanh(t(W2)%*%a1+b2%*%ones)
  a3 = sigmoid(t(W3)%*%a2+b3%*%ones)
  return(list(bet = a3))
}


control = function(obs,pars)
{
  # tablemat <- matrix(0, nrow = 1, ncol = 10)  # 1-9, 10 (J, Q, K)
  # for (i in 2:9) {
  #   tablemat[, i-1] <-  (i*4* n_decks - i*sum(obs$at_history == i)) / (i*4* n_decks)
  # }
  # tablemat[, 9] <-  (10*16*n_decks - 10*sum(obs$at_history == 10)) / (10*16* n_decks)
  # tablemat[, 10] <-  (11*4*n_decks - 11*sum(obs$at_history == 11)) / (11*4* n_decks)
  # res_model = model( cbind(obs$player_sum/21, obs$dealer_show/10, obs$true_count/3, tablemat, obs$can_split, obs$usable_ace,
                    # length(obs$history$player[[length(obs$history$player)]])/5, obs$bet_size, length(obs$at_history)/(52*n_decks) ),pars,rep(nodes,2), q= q)
  res_model = model( cbind(obs$player_sum/21, obs$dealer_show/10, obs$usable_ace),pars,rep(nodes,2), q= q)
  # res_model = model( cbind(obs$player_sum/21, obs$dealer_show/10, obs$true_count/4, tablemat),pars,rep(nodes,2), q= q)
  logits <- res_model$logits
  allowed_actions <- c(TRUE, TRUE, obs$can_split, obs$can_surrender, obs$can_doubledown)
  logits[!allowed_actions] <- -Inf
  action_probs <- softmax(t(logits))
  return(list(action =  t(action_probs)))
  
  # allowed_actions <- c(TRUE, TRUE, obs$can_split, obs$can_surrender, obs$can_doubledown)
  # if (all(allowed_actions ==  rep(TRUE, 5))){
  #   # print ('first')
  #   res_model = model( cbind(obs$player_sum/21, obs$dealer_show/10, obs$true_count/3, tablemat, obs$can_split, obs$usable_ace,
  #                           length(obs$history$player[[length(obs$history$player)]])/5, obs$bet_size, length(obs$at_history)/(52*n_decks) ),pars[1:npars_first],rep(nodes,2), q= 5)
  #   # print (res_model$action)
  #   return (list(action = res_model$action))
  # }
  # if (all(allowed_actions ==  c(TRUE, TRUE, FALSE, TRUE, TRUE))){
  #   # print ('second')
  #   res_model = model( cbind(obs$player_sum/21, obs$dealer_show/10, obs$true_count/3, tablemat, obs$can_split, obs$usable_ace,
  #                            length(obs$history$player[[length(obs$history$player)]])/5, obs$bet_size, length(obs$at_history)/(52*n_decks) ),pars[1:npars_first],rep(nodes,2), q= 4)
  #   # print (res_model$action)
  #   return (list(action = c(res_model$action[1], res_model$action[2], 0,  res_model$action[3], res_model$action[4])))
  # }
  # if (all(allowed_actions == c(TRUE, TRUE, FALSE, FALSE, TRUE))){
  #   # print ('third')
  #   res_model = model( cbind(obs$player_sum/21, obs$dealer_show/10, obs$true_count/3, tablemat, obs$can_split, obs$usable_ace,
  #                            length(obs$history$player[[length(obs$history$player)]])/5, obs$bet_size, length(obs$at_history)/(52*n_decks) ),pars[(npars_first+1): (npars_first+npars_second)],rep(nodes,2), q= 3)
  #   # print ( c(res_model$action[1], res_model$action[2], 0, 0, res_model$action[3]))
  #   return (list(action = c(res_model$action[1], res_model$action[2], 0, 0, res_model$action[3])))
  # }
  # if (all(allowed_actions == c(TRUE, TRUE, FALSE, FALSE, FALSE))){
  #   # print ('fourth')
  #   res_model = model( cbind(obs$player_sum/21, obs$dealer_show/10, obs$true_count/3, tablemat, obs$can_split, obs$usable_ace,
  #                            length(obs$history$player[[length(obs$history$player)]])/5, obs$bet_size, length(obs$at_history)/(52*n_decks) ),pars[(npars_first+npars_second+1):npars],rep(nodes,2), q= 2)
  #   # print (res_model$action)
  #   return (list(action = res_model$action))
  # }
}

control_bet = function(obs, pars)
{
  tablemat <- matrix(0, nrow = 1, ncol = 10)  # 1-9, 10 (J, Q, K)
  for (i in 2:9) {
    tablemat[, i-1] <-  (i*4* n_decks - i*sum(obs$at_history == i)) / (i*4* n_decks)
  }
  tablemat[, 9] <-  (10*16*n_decks - 10*sum(obs$at_history == 10)) / (10*16* n_decks)
  tablemat[, 10] <-  (11*4*n_decks - 11*sum(obs$at_history == 11)) / (11*4* n_decks)
  res_model = model_bet( cbind(obs$true_count/3, tablemat),pars,rep(nodesb,2), q= qb)
  # res_model = model_bet( cbind(obs$true_count/3),pars,rep(nodesb,2), q= qb)
  # res_model = model_bet( cbind(tablemat),pars,rep(nodesb,2), q= qb)
  return(list(bet = res_model$bet))
}


# NN parameters for control
nodes = 3
p= 3;q = 5;npars = p*nodes+nodes*nodes+nodes*q+nodes+nodes+q
# p_first = 3+10+5;q_first     = 5;npars_first = p_first*nodes+nodes*nodes+nodes*q_first+nodes+nodes+q_first
# p_second = 3+10+5;q_second     = 4;npars_second = p_second*nodes+nodes*nodes+nodes*q_second+nodes+nodes+q_second
# p_third = 3+10+5;q_third     = 3;npars_third = p_third*nodes+nodes*nodes+nodes*q_third+nodes+nodes+q_third
# p_fourth = 3+10+5;q_fourth    = 2;npars_fourth = p_fourth*nodes+nodes*nodes+nodes*q_fourth+nodes+nodes+q_fourth
# npars = npars_first+npars_second+npars_third+npars_fourth
theta_rand =  c(runif(npars,-10,10))



# NN parameters for control_bet
pb     = 1+10
qb     = 1
nodesb = 3
nparsb = pb*nodesb+nodesb*nodesb+nodesb*qb+nodesb+nodesb+qb
nparsb
theta_randb =  c(runif(nparsb,-10,10))


#========================================================================
#                   Objectives and Fitness 
#========================================================================
nu  = 0e-4
n_decks = 8
reshuffle_threshold = n_decks*52 *0.5
n_hands = 1e3

play_a_game = function(theta)
{
  reward_list = c()
  bet_list = c()
  set.seed(1)
  env <- blackjack_env(n_decks = n_decks, reshuffle_threshold = reshuffle_threshold )
  for (i in 1:n_hands) {
    # if (length(env$get_state()$deck) < reshuffle_threshold || is.null(env$get_state()$deck)){
      # bet_size = 1
    # }else{
      # bet_size = 1+ 9*control_bet(obs$obs, theta[(npars+1):(npars+nparsb)])$bet
      # bet_size =1+9*control_bet(obs$obs, theta)$bet
    # }
    # bet_size = get_bet_size(env$true_count())
    bet_size = 1
    obs = env$reset(bet_size = bet_size)
    actions = c()
    while (!obs$done) {
      action = control(obs$obs, theta[1:npars])$action
      obs <- env$step(which.max(action) - 1)
      # action = basic_strategy_action(player_sum = obs$obs$player_sum, dealer_show =obs$obs$dealer_show,
                                     # # usable_ace = obs$obs$usable_ace, can_split= obs$obs$can_split,
                                     # player_cards = env$get_state()$player, can_surrender = obs$obs$can_surrender,
                                     # can_doubledown = obs$obs$can_doubledown)
      # obs = env$step(action)
      
      # actions = c(actions, action)
      actions = c(actions, which.max(action) - 1)
    }
    reward_list[i] = obs$reward
    bet_list[i] <- bet_size * (1 + sum(actions == 2) + sum(actions == 4))
  }
  roi = sum(reward_list)/sum(bet_list)
  # return (roi - nu*sum(theta^2))
  return (roi)
}
play_a_game(theta_rand)
# play_a_game(c(theta_rand, theta_randb))
# play_a_game(c(theta_randb))


#========================================================================
#                    Evolutionary Learning
#========================================================================
library('GA')
popSize =100
n_gens = 100
obj = play_a_game
# GA  = ga(type = "real-valued",  obj, lower = rep(-10,(npars+nparsb)), upper = rep(10,(npars+nparsb)),popSize = popSize,maxiter = n_gens,keepBest = TRUE, pmutation = 0.8)
# GA  = ga(type = "real-valued",  obj, lower = rep(-10,(npars)), upper = rep(10,(npars)),popSize = popSize,maxiter = n_gens,keepBest = TRUE)
GA  = ga(type = "real-valued",  obj, lower = rep(-10,(nparsb)), upper = rep(10,(nparsb)),popSize = popSize,maxiter = n_gens,keepBest = TRUE, pmutation = 0.8)


GA@solution
theta_hat = GA@solution[1,]
theta_hat
play_a_game(theta_hat)



# Just plots
for (j in 1:100)
{


reward_list = c()
reward_list_bs = c()
tc_list =c(0)
sum_list = c()
dealer_list = c()
hit_list = c()
bs_list = c()
bet_list =c()
sum_deal_mat_stay_ace = matrix(0, nrow = 21, ncol = 10)
sum_deal_mat_hit_ace = matrix(0, nrow = 21, ncol = 10)
sum_deal_mat_stay = matrix(0, nrow = 21, ncol = 10)
sum_deal_mat_hit = matrix(0, nrow = 21, ncol = 10)
mat_split = matrix(0, nrow = 10, ncol = 10 )
sum_deal_mat_dd_ace = matrix(0, nrow = 21, ncol = 10)
sum_deal_mat_dd = matrix(0, nrow = 21, ncol = 10)
mat_surr = matrix(0, nrow = 21, ncol = 10)

set.seed(j+1)
n_decks = 8
n_hands = 1e3
reshuffle_threshold = n_decks*52 *0.5

env <- blackjack_env(n_decks = n_decks, reshuffle_threshold = reshuffle_threshold)

for (i in 1:n_hands) {
  # tc_list = c(tc_list, env$true_count())
  if (length(env$get_state()$deck) < reshuffle_threshold || is.null(env$get_state()$deck)){
    bet_size = 1
  }else{
    bet_size = 1+9*control_bet(obs$obs, MAP_estimates[(npars+1):(npars+nparsb)])$bet
    # bet_size = 1+9*control_bet(obs$obs, MAP_estimates)$bet
  }
  # bet_size = 1
  # bet_size = 1+9*runif(1)
  # bet_size <- 1+9*get_bet_size(env$true_count())
  bet_list = c(bet_list, bet_size)
  obs = env$reset(bet_size = bet_size)
  
  # if (tc_list[i] >2){print (c(tc_list[i], bet_list[i]))}
  # cat("i =", i, "\n")
  # print(obs$obs$history$player)
  # print (obs$obs$history$dealer)
  while (!obs$done) {
    action = control(obs$obs, MAP_estimates[1:npars])$action


    # if (obs$obs$usable_ace){
      # if (which.max(action) - 1 == 0){sum_deal_mat_stay_ace[obs$obs$player_sum, obs$obs$dealer_show] =  sum_deal_mat_stay_ace[obs$obs$player_sum, obs$obs$dealer_show] +1}
      # if (which.max(action) - 1 == 1){sum_deal_mat_hit_ace[obs$obs$player_sum, obs$obs$dealer_show] =  sum_deal_mat_hit_ace[obs$obs$player_sum, obs$obs$dealer_show] +1}
      # if (which.max(action) - 1 == 4){sum_deal_mat_dd_ace[obs$obs$player_sum, obs$obs$dealer_show] =  sum_deal_mat_dd_ace[obs$obs$player_sum, obs$obs$dealer_show] +1}
    # }
    # if (!obs$obs$usable_ace){
    #   if (which.max(action) - 1 == 0){sum_deal_mat_stay[obs$obs$player_sum, obs$obs$dealer_show] =  sum_deal_mat_stay[obs$obs$player_sum, obs$obs$dealer_show] +1}
    #   if (which.max(action) - 1 == 1){sum_deal_mat_hit[obs$obs$player_sum, obs$obs$dealer_show] =  sum_deal_mat_hit[obs$obs$player_sum, obs$obs$dealer_show] +1}
    #   if (which.max(action) - 1 == 4){sum_deal_mat_dd[obs$obs$player_sum, obs$obs$dealer_show] =  sum_deal_mat_dd[obs$obs$player_sum, obs$obs$dealer_show] +1}
    # }
    # 
    # if (which.max(action) - 1 ==2){mat_split[env$get_state()$player[1],obs$obs$dealer_show] =mat_split[env$get_state()$player[1],obs$obs$dealer_show]+1}
    # if (which.max(action) - 1 ==3){mat_surr[obs$obs$player_sum, obs$obs$dealer_show] =mat_surr[obs$obs$player_sum, obs$obs$dealer_show] + 1}

    # action_bs = basic_strategy_action(player_sum = obs$obs$player_sum, dealer_show =obs$obs$dealer_show,
                                   # usable_ace = obs$obs$usable_ace, can_split= obs$obs$can_split,
                                   # player_cards = env$get_state()$player, can_surrender = obs$obs$can_surrender,
                                   # can_doubledown = obs$obs$can_doubledown)
    # bs_list = c(bs_list, action_bs)
    # obs = env$step(action_bs)
    # cat ('action = ', action, "\n")
    
    
    
    # cat ('action = ', c(env$get_state()$player, obs$obs$player_sum, obs$obs$dealer_show, which.max(action) - 1, action_bs), "\n")
    obs <- env$step(which.max(action) - 1)
    # print (action_bs)
    # print (obs$obs$history)
    
    # hit_list = c(hit_list,which.max(action) - 1)
    
  }
  # tc_list = c(tc_list, obs$obs$true_count)
  reward_list[i] = obs$reward
  # if (reward_list[i] ==0 ){print (obs)}
  # reward_list_bs[i] = obs_bs$reward
  # cat ('reward =',  obs$reward, "\n")
}


png(sprintf("C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/Blackjack/hybrid/Hybrid/pics/both/bet_%d.png", j), width = 800, height = 600);
hist(bet_list, breaks = 100, xlab = 'Bet Size', main = NULL,  freq = FALSE, col = c('red', 'green')[(sum(reward_list)>0)*1+1], ylim = c(0, 4), xlim = c(0.5,  11));axis(1, at = 1:10, labels = 1:10)
legend('top', legend = c(sprintf("Total Risked: $%.2f", sum(bet_list)), sprintf("Net Profit/Loss: $%.2f", sum(reward_list)), sprintf("ROI: %.2f%%", 100 * sum(reward_list) / sum(bet_list))), bty = "n")
dev.off()
}


# rho
cor(as.vector(bet_mat), as.vector(tc_mat))
# SBD
sum(((as.vector(tc_mat) - mean(as.vector(tc_mat)))/sd(as.vector(tc_mat)) - (as.vector(bet_mat) - mean(as.vector(bet_mat)))/sd(as.vector(bet_mat)))^2)/length(as.vector(bet_mat))



plot.ts(sum_list)
hist(tc_list, breaks = 100)

bet_list
hist(bet_list, breaks = 100)
hist(hit_list, breaks = 100, xlim = c(0, 4), freq = TRUE)
hist(bs_list-0.1, breaks = 100, add = TRUE, col = 'blue', freq = TRUE)
plot.ts(bet_list)
sum(reward_list>0) / (sum(reward_list>0) +sum(reward_list <0 ))
sum(reward_list>0)
sum(reward_list ==0 )
hist(hit_list, breaks =100)
hit_list

par(mfrow = c(2,  1))
plot.ts(bet_list, xlab = 'Time (t)', ylab = expression(bet[t]), col = '#7b1e3b')
# plot.ts(ifelse(tc_list < 0, 0, tc_list))
plot.ts(tc_list, col = '#7b1e3b',  xlab = 'Time (t)', ylab = expression(TC[t]),)


mean(reward_list)
hist(reward_list, breaks = 100)
sum(reward_list)
plot.ts(reward_list)
plot.ts(hit_list)


# Plot of cumuative night's returns
plot.ts(cumsum(reward_list))
# ROi
sum(reward_list)/sum(bet_list)*100




# Response for bet
bet_res = c()
tcs = seq(-4, 3, 0.01)
for (tc in tcs){
  # bet_res = c(bet_res, model_bet( cbind(tc/3), theta_hat[(npars+1):(npars+nparsb)],rep(nodesb,2), q= qb)$bet)
  bet_res = c(bet_res, model_bet( cbind(tc_list/3), theta_hat,rep(nodesb,2), q= qb)$bet)
}
plot.ts(tc_list,  model_bet( cbind(tc_list/3), theta_hat,rep(nodesb,2), q= qb)$bet[1, ])

theta_hat[(npars+1):(npars+nparsb)]



# Tables
build_strategy_df <- function(hit_mat, stay_mat, dd_mat, soft = FALSE) {
  if (!soft){hit_mat = hit_mat[4:20, ];stay_mat = stay_mat[4:20, ];dd_mat = dd_mat[4:20, ]}
  if (soft){hit_mat = hit_mat[12:20, ];stay_mat = stay_mat[12:20, ];dd_mat = dd_mat[12:20, ]}
  actions <- matrix(NA, nrow = nrow(hit_mat), ncol = ncol(hit_mat))
  actions[hit_mat > 0] <- "H"
  actions[stay_mat > 0] <- "S"
  actions[dd_mat > 0] <- "D"
  df <- as.data.frame((actions))
  # print (df)
  colnames(df) <-c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  df <- df[, c("2", "3", "4", "5", "6", "7", "8", "9", "10", "A")]
  if (!soft){rownames(df) = 4:20}
  if (soft){
    rownames(df) = c("A, A", "A, 2", "A, 3", "A, 4", "A, 5", "A, 6", "A, 7", "A, 8", "A, 9")
    df = df[c("A, 2", "A, 3", "A, 4", "A, 5", "A, 6", "A, 7", "A, 8", "A, 9", "A, A"), ]
  }
  df
}
df_hard <- build_strategy_df(sum_deal_mat_hit, sum_deal_mat_stay, sum_deal_mat_dd)
df_hard
df_soft <- build_strategy_df(sum_deal_mat_hit_ace, sum_deal_mat_stay_ace, sum_deal_mat_dd_ace, TRUE)
df_soft

build_pair_split_df <- function(split_mat) {
  df <- as.data.frame((ifelse(split_mat > 0, "Sp", "N")))
  colnames(df)  =  c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  df <- df[, c("2", "3", "4", "5", "6", "7", "8", "9", "10", "A")]
  rownames(df) = c("A, A", "2, 2", "3, 3", "4, 4", "5, 5", "6, 6", "7, 7", "8, 8", "9, 9", "10, 10")
  df = df[  c("2, 2", "3, 3", "4, 4", "5, 5", "6, 6", "7, 7", "8, 8", "9, 9", "10, 10", "A, A") , ]
  df
}
df_split <- build_pair_split_df(mat_split)
df_split

build_surrender_df <- function(surr_mat) {
  surr_mat = surr_mat[4:20, ]
  df <- as.data.frame((ifelse(surr_mat > 0, "SUR", "N")))
  colnames(df) <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  rownames(df) = 4:20
  df <- df[, c("2", "3", "4", "5", "6", "7", "8", "9", "10", "A")]
  df
}
df_surr <- build_surrender_df(mat_surr)
df_surr

library(ggplot2)
library(tidyr)

df_hard_long <- df_hard %>%
  tibble::rownames_to_column("Player_Sum") %>%
  pivot_longer(cols = -Player_Sum, names_to = "Dealer_Upcard", values_to = "Action")
df_hard_long$Player_Sum <- factor(df_hard_long$Player_Sum, levels = 4:20)
df_hard_long$Dealer_Upcard <- factor(df_hard_long$Dealer_Upcard, levels = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "A"))
hard = ggplot(df_hard_long, aes(x = Dealer_Upcard, y = Player_Sum)) +
  geom_tile(aes(fill = Action), color = "black") +
  geom_text(aes(label = Action), color = "white", size = 4, fontface = 'bold') +
  scale_fill_manual(values = c("H" = "#4CBB17", "S" = "#FF2800", "D" = "#0047AB", "NA" = "grey"), 
                    na.value = "grey") +
  labs(title = "",
       x = "Dealer's Upcard",
       y = "Hard Totals") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(), axis.title.x = element_text(size = 14, face = "bold"),  
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold"),  
        axis.text.y = element_text(size = 14, face = "bold"))+
  scale_x_discrete(position = "top") 
hard 


df_soft_long <- df_soft%>%
  tibble::rownames_to_column("Player_Sum") %>%
  pivot_longer(cols = -Player_Sum, names_to = "Dealer_Upcard", values_to = "Action")

df_soft_long$Player_Sum <- factor(df_soft_long$Player_Sum, levels =c("A, 2", "A, 3", "A, 4", "A, 5", "A, 6", "A, 7", "A, 8", "A, 9", "A, A") )
df_soft_long$Dealer_Upcard <- factor(df_soft_long$Dealer_Upcard, levels = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "A"))
soft = ggplot(df_soft_long, aes(x = Dealer_Upcard, y = Player_Sum)) +
  geom_tile(aes(fill = Action), color = "black") +
  geom_text(aes(label = Action), color = "white", size = 4, fontface = 'bold') +
  scale_fill_manual(values = c("H" = "#4CBB17", "S" = "#FF2800", "D" = "#0047AB", "NA" = "grey"), 
                    na.value = "grey") +
  labs(title = "",
       x = "",
       y = "Soft Totals") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),  
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_blank(),  
        axis.text.y = element_text(size = 14, face = "bold"))+
  scale_x_discrete(position = "top") 
soft

df_split_long <- df_split %>%
  tibble::rownames_to_column("Player_Pair") %>%
  pivot_longer(cols = -Player_Pair, names_to = "Dealer_Upcard", values_to = "Action")
df_split_long$Player_Pair <- factor(df_split_long$Player_Pair, 
                                    levels = c("2, 2", "3, 3", "4, 4", "5, 5", "6, 6", "7, 7", "8, 8", "9, 9", "10, 10", "A, A"))
df_split_long$Dealer_Upcard <- factor(df_split_long$Dealer_Upcard, 
                                      levels = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "A"))

split = ggplot(df_split_long, aes(x = Dealer_Upcard, y = Player_Pair)) +
  geom_tile(aes(fill = Action), color = "black") +
  geom_text(aes(label = Action), color = "black", size = 4, fontface = 'bold') +
  scale_fill_manual(values = c("Sp" = "dodgerblue", "N" = "white"), na.value = "grey") +
  labs(title = "",
       x = "",
       y = "Pair") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"))


legend_df <- data.frame(
  Action = c("Stay", "Hit", "Double-down", "Split", "No Split", "Surrender"),
  Code = c("S", "H", "D", "Y", "N", "R"),
  Color = c("#FF2800", "#4CBB17", "#0047AB", "dodgerblue", "white", "black")
)

# Convert Action to factor with desired order
legend_df$Action <- factor(legend_df$Action, levels = c("Stay", "Hit", "Double-down", "Split", "No Split",  "Surrender"))

legend_plot <- ggplot(legend_df, aes(x = Action, y = 1, fill = Code)) +
  geom_tile(color = "black", width = 0.9, height = 0.9) +
  geom_text(aes(label = Action), color = c(rep("white", 4), 'black', 'white'), fontface = "bold", size = 3, vjust = .1) +
  scale_fill_manual(values = setNames(legend_df$Color, legend_df$Code)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    panel.background = element_rect(fill = "transparent", color = NA)
  ) +
  coord_fixed(ratio = 0.4) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.3))) +
  scale_y_continuous(expand = expansion(add = c(0.5, 0.5)))

legend_plot



library('patchwork')

p_hard <- hard 
p_soft <- soft 
p_split <- split


(p_hard / p_soft / p_split / legend_plot) + 
  plot_layout(ncol = 1, heights = c(.5, 0.28, 0.28, 0.1)) +
  plot_annotation(
    caption = bquote(nu == .(format(nu, scientific=FALSE)))
  ) & 
  theme( plot.caption.position= 'plot',
    plot.caption= element_text(hjust = 0.95, vjust =290, size = 16, face = "bold", margin = margin(b = 20))
  )









df_surrender_long <- df_surr %>%
  tibble::rownames_to_column("Player_Sum") %>%
  pivot_longer(cols = -Player_Sum, names_to = "Dealer_Upcard", values_to = "Action")
df_surrender_long$Player_Sum <- factor(df_surrender_long$Player_Sum, levels = 4:20)
df_surrender_long$Dealer_Upcard <- factor(df_surrender_long$Dealer_Upcard, 
                                          levels = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "A"))
ggplot(df_surrender_long, aes(x = Dealer_Upcard, y = Player_Sum)) +
  geom_tile(aes(fill = Action), color = "black") +
  geom_text(aes(label = Action), color = "black", size = 4) +
  scale_fill_manual(values = c("SUR" = "orange", "N" = "white"), na.value = "grey") +
  labs(title = "Blackjack Strategy: Surrender",
       x = "Dealer's Upcard",
       y = "Player's Sum") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5))



y <- as.numeric(tc_list)
x <- seq_along(y)
gradient_colors <- colorRampPalette(c("#f4e7ec", "#7b1e3b"))(100)
y_norm <- (y - min(y)) / (max(y) - min(y))
color_indices <- round(1 + y_norm * (length(gradient_colors) - 1))
lwd_range <- c(1, 5)  
lwd_values <- lwd_range[1] + y_norm * (lwd_range[2] - lwd_range[1])  
plot(x, y, type = "n", xlab = "Time (t)", ylab = expression(TC[t]))
for (i in 1:(length(y) - 1)) {
  segments(x0 = x[i], y0 = y[i], x1 = x[i + 1], y1 = y[i + 1], 
           col = gradient_colors[color_indices[i]], 
           lwd = lwd_values[i])
}














# OOS: 10000 nights
reward_tot =c()
n_decks = 8
reshuffle_threshold = n_decks*52 *0.5
n_hands = 1e3
nights = 1e4
reward_mat = matrix(NA, nrow = n_hands, ncol = nights)
bet_mat = matrix(NA, nrow = n_hands, ncol = nights)
tc_mat = matrix(NA, nrow = n_hands, ncol = nights)


for (j in 1:nights)
{
  set.seed(j+1)
  reward_list = c()
  env <- blackjack_env(n_decks = n_decks, reshuffle_threshold = reshuffle_threshold)
  for (i in 1:n_hands) {
    # if (length(env$true_count()) == 0){tc_mat[i, j] = 0}else{tc_mat[i, j] =  env$true_count() }
    # if (length(env$get_state()$deck) < reshuffle_threshold || is.null(env$get_state()$deck)){
      # bet_size = 1
    # }else{
      # bet_size =1+9* control_bet(obs$obs, theta_hat[(npars+1):(npars+nparsb)])$bet
      # bet_size = 1+9*control_bet(obs$obs, theta_hat)$bet
    # }
    bet_size = 1
    # bet_size = 1+9*runif(1)
    # bet_size <- 1+9*get_bet_size(env$true_count())
    obs = env$reset(bet_size = bet_size)
    while (!obs$done) {
      # action = control(obs$obs, theta_hat[1:npars])$action
      # obs <- env$step(which.max(action) - 1)
      action_bs = basic_strategy_action(player_sum = obs$obs$player_sum, dealer_show =obs$obs$dealer_show,
                  usable_ace = obs$obs$usable_ace, can_split= obs$obs$can_split,
                  player_cards = env$get_state()$player, can_surrender = obs$obs$can_surrender,
                  can_doubledown = obs$obs$can_doubledown)
      obs = env$step(action_bs)
      # allowed_actions <- c(TRUE, TRUE, obs$obs$can_split, obs$obs$can_surrender, obs$obs$can_doubledown)
      # if (all(allowed_actions ==  rep(TRUE, 5))){action_random = sample(0:4, 1)}
      # if (all(allowed_actions ==  c(TRUE, TRUE, FALSE, TRUE, TRUE))){action_random = sample(c(0,1,3,4), 1)}
      # if (all(allowed_actions == c(TRUE, TRUE, FALSE, FALSE, TRUE))){action_random = sample(c(0, 1, 4), 1)}
      # if (all(allowed_actions == c(TRUE, TRUE, FALSE, FALSE, FALSE))){action_random = sample(c(0, 1), 1)}
      # action_random = sample(c(0, 1), 1)
      # S17 Strat
      # if (obs$obs$player_sum < 17){action_random = 1}else{action_random = 0}
      
      # # H17: Hit on soft 17 only
      # if (obs$obs$player_sum < 17 ||
      #     (obs$obs$player_sum == 17 && obs$obs$usable_ace)) {
      #   action_random <- 1  # hit
      # } else {
      #   action_random <- 0  # stand
      # }
      
      # obs <- env$step(action_random)
      

    }
    reward_mat[i, j] = obs$reward
    bet_mat[i, j] = bet_size
  }
}



# hit-rate
sum(as.vector(reward_mat)>0) / (sum(as.vector(reward_mat)>0)+ sum(as.vector(reward_mat)<0))

# rho
cor(as.vector(bet_mat), as.vector(tc_mat))
# SBD
sum(((as.vector(tc_mat) - mean(as.vector(tc_mat)))/sd(as.vector(tc_mat)) - (as.vector(bet_mat) - mean(as.vector(bet_mat)))/sd(as.vector(bet_mat)))^2)/length(as.vector(bet_mat))


roi = colSums(reward_mat)/colSums(bet_mat)
roi*100

plot.ts(roi)
hist(roi, breaks  = 200, freq = FALSE)
hist(hit_list)
hist(bs_list)


hist(roi, breaks = 200, freq = FALSE, xlab = "ROI %", col = '#FF2800', ylim  = c(0, 12), xlim  = c(-0.20, 0.20), xaxt = 'n'
     # ,main = substitute(nu == x, list(x = sprintf("%.4f", nu))))
     # ,main = 'Purely Random')
     ,main = expression(TC[t - 1] > 3))
     
mean(roi)*100
sd(roi*100)
curve(dnorm(x, mean = mean(roi), sd = sd(roi)),
      col = "#002147", lwd = 4,
      ylab = 'Density', xlab = 'ROI %', add = TRUE)
x_ticks <- round(seq(-0.20, 0.20, by = 0.05), 2)
axis(1, at = x_ticks, labels = paste0(x_ticks * 100, "%"))
legend(x = 0.07, y = 8,
       legend = c(bquote(N(mu == .(round(mean(roi*100), 4)) * "%" ~ "," ~ 
                             sigma^2 == .(round(sd(roi*100), 4))^2))),
       bty = "n",      # remove box
       cex = 1.2)      # increase text size





curve(dnorm(x, mean = mean(roi0.05, na.rm = TRUE), sd = sd(roi0.05, na.rm = TRUE)),
      col = "#9E9AC8", add = TRUE, lwd = 2)
curve(dnorm(x, mean = mean(roi0.1, na.rm = TRUE), sd = sd(roi0.1, na.rm = TRUE)),
      col = "#756BB1", add = TRUE, lwd = 2)
curve(dnorm(x, mean = mean(roi0.15, na.rm = TRUE), sd = sd(roi0.15, na.rm = TRUE)),
      col = "#54278F", add = TRUE, lwd = 2)

legend("topleft",
       legend = c(
         bquote(nu == 0 ~ ":" ~ N(mu == .(round(mean(roi0, na.rm = TRUE), 2)), sigma^2 == .(round(sd(roi0, na.rm = TRUE), 2))^2)),
         bquote(nu == 0.05 ~ ":" ~ N(mu == .(round(mean(roi0.05, na.rm = TRUE), 2)), sigma^2 == .(round(sd(roi0.05, na.rm = TRUE), 2))^2)),
         bquote(nu == 0.1 ~ ":" ~ N(mu == .(round(mean(roi0.1, na.rm = TRUE), 2)), sigma^2 == .(round(sd(roi0.1, na.rm = TRUE), 2))^2)),
         bquote(nu == 0.15 ~ ":" ~ N(mu == .(round(mean(roi0.15, na.rm = TRUE), 2)), sigma^2 == .(round(sd(roi0.15, na.rm = TRUE), 2))^2))
       ),
       col = c("#CBC9E2", "#9E9AC8", "#756BB1", "#54278F"),  lwd = 2, bty = "n")




plot.ts(reward_tot)
hist(reward_tot, breaks = 100)
summary(reward_tot)

reward_tot = reward_tot[2:10000] 
profit = sum(reward_tot[reward_tot>0]) + sum(reward_tot[reward_tot<0])
total = sum(reward_tot[reward_tot>0]) + abs(sum(reward_tot[reward_tot<0]))
profit/total

reward_tot



#========================================================================
#                   Visual Game
#========================================================================


reward_list = c()
bet_list = c()
plt_cnt = 1
set.seed(5)
n_decks = 8
n_hands = 35
reshuffle_threshold = n_decks*52 *0.5
env <- blackjack_env(n_decks = n_decks, reshuffle_threshold = reshuffle_threshold)
for (i in 1:n_hands) {
  # if (length(env$get_state()$deck) < reshuffle_threshold || is.null(env$get_state()$deck)){
    # bet_size = 1
  # }else{
    # bet_size = 1+9*control_bet(obs$obs, theta_hat[(npars+1):(npars+nparsb)])$bet
    # bet_size = 1+9*control_bet(obs$obs, theta_hat)$bet
  # }
  bet_size = 1
  # bet_size <- 1+9*get_bet_size(env$true_count())
  obs = env$reset(bet_size = bet_size)
  tau = 1
  first_tau = 1
  split = FALSE
  hand_index = 1
  dd = FALSE
  dd1 =FALSE
  dd2 =FALSE
  while (!obs$done) {
    # action = control(obs$obs, theta_hat[1:npars])$action
    # obs <- env$step(which.max(action) - 1)
    
    if (tau == 1){
      # print (env$get_state()$player)
      # print(obs$obs$dealer_show)
      plot_blackjack_on_table(env$get_state()$player, obs$obs$dealer_show, dd, reward_list,plt_cnt = plt_cnt); plt_cnt = plt_cnt +1
    }
    action_bs = basic_strategy_action(player_sum = obs$obs$player_sum, dealer_show =obs$obs$dealer_show,
                                      usable_ace = obs$obs$usable_ace, can_split= obs$obs$can_split,
                                      player_cards = env$get_state()$player, can_surrender = obs$obs$can_surrender,
                                      can_doubledown = obs$obs$can_doubledown)
    obs = env$step(action_bs)
    

    if (tau == 1 & action_bs==2){split = TRUE}
    if (split & tau ==2){
      plot_blackjack_on_table_split(list(obs$obs$history$player[[1]], obs$obs$history$player[[2]]), obs$obs$dealer_show, dd1, dd2, reward_list, plt_cnt = plt_cnt); plt_cnt = plt_cnt +1
    }
    if (split & tau>1){
      if ( (action_bs ==0 | hand_value(env$get_state()$player) ==21) & hand_index ==2  ){
        for (del in 2:length(obs$obs$history$dealer)){
          # print (del)
          # print (obs$obs$history$dealer[[del]])
          plot_blackjack_on_table_split(list(obs$obs$history$player[[first_tau+1*(first_tau != 1)]],env$get_state()$player),obs$obs$history$dealer[[del]], dd1, dd2, reward_list, plt_cnt = plt_cnt); plt_cnt = plt_cnt +1
        }
      }

      if (action_bs ==1 & hand_index ==1){
        first_tau = first_tau+1
        # print (env$get_state()$player)
        plot_blackjack_on_table_split(list(obs$obs$history$player[[first_tau+1]], obs$obs$history$player[[2]]), obs$obs$dealer_show, reward_list, plt_cnt = plt_cnt);plt_cnt = plt_cnt +1
      }else if (action_bs ==1 & hand_index ==2){
        # print (env$get_state()$player)
        plot_blackjack_on_table_split(list(obs$obs$history$player[[first_tau+1*(first_tau != 1)]],env$get_state()$player),obs$obs$dealer_show, dd1, dd2, reward_list, plt_cnt = plt_cnt);plt_cnt = plt_cnt +1
      }

      if(action_bs ==4 & hand_index == 1){
        first_tau = first_tau+1
        dd1 = TRUE
        plot_blackjack_on_table_split(list(obs$obs$history$player[[first_tau+1]], obs$obs$history$player[[2]]), obs$obs$dealer_show, dd1, dd2, reward_list, plt_cnt = plt_cnt);plt_cnt = plt_cnt +1
        hand_index = 2
      }else if(action_bs == 4 & hand_index == 2){
        dd2 = TRUE
        plot_blackjack_on_table_split(list(obs$obs$history$player[[first_tau+1*(first_tau != 1)]],env$get_state()$player),obs$obs$dealer_show, dd1, dd2, reward_list, plt_cnt = plt_cnt);plt_cnt = plt_cnt +1
        for (del in 2:length(obs$obs$history$dealer)){
          # print (obs$obs$history$dealer[[del]])
          plot_blackjack_on_table_split(list(obs$obs$history$player[[first_tau+1*(first_tau != 1)]],env$get_state()$player),obs$obs$history$dealer[[del]], dd1, dd2, reward_list, plt_cnt = plt_cnt);plt_cnt = plt_cnt +1
        }
      }
      
      if ( (action_bs ==0 | hand_value(obs$obs$history$player[[first_tau+1*(first_tau != 1)]]) ==21) & hand_index ==1 ){hand_index =2}
      if ( hand_value( obs$obs$history$player[[first_tau+1*(first_tau != 1)]]) > 21 & hand_index ==1 ){hand_index =2}
    }
    
    
    
    
    
    if  (!split){
    #   print (c(action_bs,tau))
      if (action_bs ==0 | hand_value(env$get_state()$player) ==21 ){
        for (del in 2:length(obs$obs$history$dealer)){
          # print (obs$obs$history$dealer[[del]])
          plot_blackjack_on_table(env$get_state()$player,obs$obs$history$dealer[[del]], dd, reward_list,plt_cnt = plt_cnt);plt_cnt = plt_cnt +1
        }
      }
      if (action_bs ==1){
        # print (env$get_state()$player)
        plot_blackjack_on_table(env$get_state()$player,obs$obs$dealer_show, dd, reward_list,plt_cnt = plt_cnt);plt_cnt = plt_cnt +1
      }
      if (action_bs ==4){
        # print (env$get_state()$player)
        plot_blackjack_on_table(env$get_state()$player,obs$obs$dealer_show, dd = TRUE, reward_list,plt_cnt = plt_cnt);plt_cnt = plt_cnt +1
        if ( hand_value(env$get_state()$player) <= 21){
          for (del in 2:length(obs$obs$history$dealer)){
            # print (obs$obs$history$dealer[[del]])
            plot_blackjack_on_table(env$get_state()$player,obs$obs$history$dealer[[del]], dd = TRUE, reward_list,plt_cnt = plt_cnt);plt_cnt = plt_cnt +1
          }
        }
      }
    }

    tau  =tau+1
    
  }
  reward_list[i] = obs$reward
}




# Run the funciotns first before the above
# plot_reward((reward_list))

library(ggplot2)
plot_reward <- function(reward_list) {
  if (is.null(reward_list)) {
    df <- data.frame(Hands = 1, CumProfitLoss = 0)
    return(
      ggplot(df, aes(x = Hands, y = CumProfitLoss)) +
        geom_hline(yintercept = 0, color = "black", size = 0.5) +
        scale_x_continuous(limits = c(-0.5, n_hands), breaks = seq(0, n_hands + 1, by = 5), expand = c(0, 0)) +
        scale_y_continuous(limits = c(-10, 10)) +
        labs(x = expression(k^{th}~Hand), y = "Cumulative Profit/Loss") +
        theme_minimal() +
        theme(
          panel.grid.major = element_line(color = "gray95"),
          panel.grid.minor = element_line(color = "gray95"),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7)
        )
    )
  }
  
  rewards <- unlist(reward_list)
  cum_rewards <- cumsum(rewards)

  df <- data.frame(
    Hands = 1:length(rewards),
    CumProfitLoss = cum_rewards
  )
  
  ggplot(df, aes(x = Hands, y = CumProfitLoss)) +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    geom_line(color = "#191970", size = 0.5) +
    geom_point(color = "#191970", size = 1) +
    scale_x_continuous(limits = c(-.5, n_hands), breaks = seq(0, n_hands + 1, by = 5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-10, 10)) +
    labs(x = expression(k^{th}~Hand), y = "Cumulative Profit/Loss") +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray95"),
      panel.grid.minor = element_line(color = "gray95"),
      axis.title.x = element_text(size = 7),
      axis.title.y = element_text(size = 7)
    )
}

library(png)
library(grid)
library(ggplot2)

library(jpeg)

library(jpeg)
library(grid)
library(ggplot2)
library(patchwork)

hand_value <- function(hand) {
  val <- sum(hand)
  if (1 %in% hand && val + 10 <= 21) val <- val + 10
  val
}

plot_blackjack_on_table <- function(player_hand, dealer_hand,  dd = FALSE, reward_list, table_path = "C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/Blackjack/bj graphic.png",
                                    chip_path = "C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/Blackjack/chip.png", 
                                    save_path = 'C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/Blackjack/pics/bj chop', plt_cnt) {
  player_hand <- randomize_10_to_face(randomize_suits(convert_1_to_ace(player_hand)))
  dealer_hand <- randomize_10_to_face(randomize_suits(convert_1_to_ace(dealer_hand)))
  g <- rasterGrob(readJPEG(table_path), width = unit(1, "npc"), height = unit(1, "npc"))
  chip_grob <- rasterGrob(readPNG(chip_path), width = unit(0.05, "npc"))
  
  
  # Dummy parse_card function (replace with your own)
  parse_card <- function(hand) {
    data.frame(
      label = hand,
      color = ifelse(grepl("♦|♥", hand), "red", "black"),
      stringsAsFactors = FALSE
    )
  }
  
  dealer_cards <- parse_card(dealer_hand)
  player_cards <- parse_card(player_hand)
  
  # Set diagonal offset
  x_offset <- 0.04
  y_offset <- 0.06
  
  dealer_cards$x <- 0.4 + 0.06 * (0:(nrow(dealer_cards) - 1))
  dealer_cards$y <- rep(0.85, nrow(dealer_cards))  # all same y
  
  # Player: fixed starting point at (0.4, 0.4), fan right and down
  player_cards$x <- 0.4 + x_offset * (0:(nrow(player_cards) - 1))
  player_cards$y <- 0.4 + y_offset * (0:(nrow(player_cards) - 1))
  
  
  cards_df <- rbind(
    cbind(dealer_cards, who = "Dealer"),
    cbind(player_cards, who = "Player")
  )

  
  p = ggplot() +
    annotation_custom(g, xmin = -.1, xmax = 1.1, ymin = 0, ymax = 1) +
    annotation_custom(chip_grob, xmin = -.07, xmax = 0.8, ymin = -0.22, ymax = 0.7) +
    geom_tile(
      data = cards_df,
      aes(x = x, y = y),
      width = 0.05, height = 0.12,
      fill = "white", color = "black", linewidth = 0.5, linejoin = "round"
    ) +
    geom_text(
      data = cards_df,
      aes(x = x, y = y, label = label, color = color),
      size = 4, fontface = "bold"
    ) +
    scale_color_identity() +
    # coord_fixed(ratio = img_ratio) +  # Fix aspect ratio according to image
    xlim(0, 1) + ylim(0, 1) +
    theme_void() 
  if(dd){p = p+ annotation_custom(chip_grob, xmin = -.07+0.02, xmax = 0.8+0.02, ymin = -0.22+0.02, ymax = 0.7+0.02) }
  # combined_plot <- p / plot_reward((reward_list)) + plot_layout(heights = c(2, 1))
  combined_plot <- p
  # print (combined_plot)
  if (!is.null(save_path)) {
    filename <- file.path(save_path, paste0("blackjack_plot_", plt_cnt, ".png"))
    # ggsave(filename = filename, plot = combined_plot, width = 6, height = 6, dpi = 150)
    ggsave(filename = filename, plot = combined_plot, width = 8, height = 6, dpi = 150)
  }
}


randomize_suits <- function(ranks) {
  set.seed(i)
  suits <- c("♦", "♠", "♣", "♥")
  assigned_suits <- sample(suits, length(ranks), replace = TRUE)  # randomly pick suits (with possible repeats)
  paste0(ranks, assigned_suits)
}
randomize_10_to_face <- function(ranks) {
  set.seed(i)
  sapply(ranks, function(rank) {
    if (rank == "10") {
      sample(c("10", "J", "Q", "K"), 1)
    } else {
      rank
    }
  })
}
convert_1_to_ace <- function(ranks) {
  sapply(ranks, function(rank) {
    if (rank == "1") {
      "A"
    } else {
      rank
    }
  })
}


plot_blackjack_on_table_split <- function(player_hands, dealer_hand,  dd1 = FALSE, dd2 = FALSE, reward_list, table_path = "C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/Blackjack/bj graphic.png",
                                    chip_path = "C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/Blackjack/chip.png", 
                                    save_path = 'C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/Blackjack/pics/bj chop', plt_cnt) {
  player_hand1 <- randomize_10_to_face(randomize_suits(convert_1_to_ace(player_hands[[1]])))
  player_hand2 <- randomize_10_to_face(randomize_suits(convert_1_to_ace(player_hands[[2]])))
  dealer_hand <- randomize_10_to_face(randomize_suits(convert_1_to_ace(dealer_hand)))
  g <- rasterGrob(readJPEG(table_path), width = unit(1, "npc"), height = unit(1, "npc"))
  chip_grob <- rasterGrob(readPNG(chip_path), width = unit(0.05, "npc"))
  
  
  # Dummy parse_card function (replace with your own)
  parse_card <- function(hand) {
    data.frame(
      label = hand,
      color = ifelse(grepl("♦|♥", hand), "red", "black"),
      stringsAsFactors = FALSE
    )
  }
  
  dealer_cards <- parse_card(dealer_hand)
  player_card1 <- parse_card(player_hand1)
  player_card2 <- parse_card(player_hand2)
  
  # Set diagonal offset
  x_offset <- 0.04
  y_offset <- 0.06
  
  dealer_cards$x <- 0.4 + 0.06 * (0:(nrow(dealer_cards) - 1))
  dealer_cards$y <- rep(0.85, nrow(dealer_cards))  # all same y
  
  # Player: fixed starting point at (0.4, 0.4), fan right and down
  player_card1$x <- 0.4 + x_offset * (0:(nrow(player_card1) - 1))
  player_card1$y <- 0.4 + y_offset * (0:(nrow(player_card1) - 1))
  player_card2$x <- 0.6 + x_offset * (0:(nrow(player_card2) - 1))
  player_card2$y <- 0.4 + y_offset * (0:(nrow(player_card2) - 1))
  
  
  cards_df1 <- rbind(
    cbind(dealer_cards, who = "Dealer"),
    cbind(player_card1, who = "Player")
  )

  cards_df2 <- rbind(
    cbind(dealer_cards, who = "Dealer"),
    cbind(player_card2, who = "Player")
  )
  
  p = ggplot() +
    annotation_custom(g, xmin = -0.1, xmax = 1.1, ymin = 0, ymax = 1) +
    annotation_custom(chip_grob, xmin = -.07, xmax = 0.8, ymin = -0.22, ymax = 0.7) +
    annotation_custom(chip_grob, xmin = 0.2, xmax = 1.07, ymin = -0.22, ymax = 0.7) +
    geom_tile(
      data = cards_df1,
      aes(x = x, y = y),
      width = 0.05, height = 0.12,
      fill = "white", color = "black", linewidth = 0.5, linejoin = "round"
    ) +
    geom_text(
      data = cards_df1,
      aes(x = x, y = y, label = label, color = color),
      size = 4, fontface = "bold"
    ) +
    geom_tile(
      data = cards_df2,
      aes(x = x, y = y),
      width = 0.05, height = 0.12,
      fill = "white", color = "black", linewidth = 0.5, linejoin = "round"
    ) +
    geom_text(
      data = cards_df2,
      aes(x = x, y = y, label = label, color = color),
      size = 4, fontface = "bold"
    ) +
    scale_color_identity() +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() 
  if(dd1){p = p+annotation_custom(chip_grob, xmin = -.07+.02, xmax = 0.8+.02, ymin = -0.22+.02, ymax = 0.7+.02) }
  if(dd2){p = p+ annotation_custom(chip_grob, xmin = 0.2 +.02, xmax = 1.07+.02, ymin = -0.22+.02, ymax = 0.7+.02)}
  # combined_plot <- p / plot_reward((reward_list)) + plot_layout(heights = c(2, 1))
  combined_plot <- p
  # print (combined_plot)
  if (!is.null(save_path)) {
    filename <- file.path(save_path, paste0("blackjack_plot_", plt_cnt, ".png"))
    # ggsave(filename = filename, plot = combined_plot, width = 6, height = 6, dpi = 150)
    ggsave(filename = filename, plot = combined_plot, width = 8, height = 6, dpi = 150)
  }
}






















































  

#========================================================================
#                  Metropolis-Hastings
#========================================================================




library("invgamma")
library('MASS')
a = 1e-3
b = 1e-3
beta = 1
S = npars
prop_var = 1
theta = rnorm(S, mean = 0, sd = sqrt(prop_var))
# theta = theta_hat
epsilon0 <- 1e-6
cov_prop = diag(S)*1e-2
window_size <- 100
stride = 100
kappa = 0.6
target_accept = 0.234
s = 1

sig_theta2 = rinvgamma(1, shape = a + S/2, rate = b + sum(theta^2)/2)
runs = 1e5
theta_store = matrix(NA, nrow = S, ncol = runs)
theta_accept = matrix(NA, nrow = S, ncol = runs)
sig_theta2_store = matrix(NA, nrow = 1, ncol = runs)
theta_store[, 1] = theta
sig_theta2_store[, 1] = sig_theta2
alphastore = c()
post_store = matrix(NA, nrow = 1, ncol = runs)
post_store[, 1] = beta*play_a_game(theta) - sum(theta^2)/(2*sig_theta2)
new_list = c()
old_list = c()

acpt_cnt = 0
burnin = floor(0.2 * runs)



# load("mcmc_checkpoint.RData")  
# Restart loop from i_last + 1
# for (i in (i_last+1):runs){
for (i in (2):runs){
  set.seed(NULL)
  # Block 1
  theta_new <- mvrnorm(1, theta, cov_prop)
  
  
  new = play_a_game(theta_new)
  new_list[i] = new
  old = play_a_game(theta)
  old_list[i] = old
  alpha = exp(min(beta*new - sum(theta_new^2)/(2*sig_theta2) - beta*old+ sum(theta^2)/(2*sig_theta2), 0))
  set.seed(NULL)
  random = runif(1, 0, 1)
  if (random < alpha){
    theta = theta_new
    acpt_cnt = acpt_cnt +1
    post_store[, i] = beta*new- sum(theta^2)/(2*sig_theta2)
  }else{post_store[, i] = beta*old- sum(theta^2)/(2*sig_theta2)}
  theta_store[, i] = theta
  alphastore[i-1] = alpha

  #Block 2 
  set.seed(NULL)
  sig_theta2 = rinvgamma(1, shape = a + S/2, rate = b + sum(theta^2)/2)
  sig_theta2_store[, i] = sig_theta2
  

  if (i < stride)
  {   
    gamma_n <- 1 / (i^kappa)
    s <- s * exp(gamma_n * (alpha - target_accept))
    cov_emp <- cov(t(theta_store[,1:i]))
    cov_prop <- s*(cov_emp + epsilon0 * diag(S))
  }
  
  if (i > stride)
  {
    gamma_n <- 1 / (i^kappa)
    s <- s * exp(gamma_n * (alpha - target_accept))
    idx_recent = seq(from = i - floor(i/stride)*stride, to = i, by = stride)
    # idx_recent = tail(idx_recent, window_size)
    theta_recent <- theta_store[, idx_recent]
    cov_emp <- cov(t(theta_recent))
    cov_prop <- s*(cov_emp + epsilon0 * diag(SS))
  }
  
}

i_last <- i  # Last completed iteration



plot.ts(theta_store[1, ])




# MAP estimates
burnin = 20000
MAP_estimates = c()
for (i in 1:nrow(theta_store[, burnin:runs])){
  dens = density(theta_store[i, burnin:runs])
  MAP_estimates[i] = dens$x[which.max(dens$y)]  
}
play_a_game(MAP_estimates)




par(mfrow = c(1, 1), mar = c(4, 5, 3, 2))  # adjust margins if needed

x1 <- apply(theta_store[, burnin:runs], 2, function(x) sum(x^2))

plot.ts(x1)
plot.ts(x1,
        col = "#FA7A35",  # bold blue-purple
        lwd = 2,
        xlab = "Iteration",
        ylab = expression("||" * bold(theta) * "||"^2),
        main = 'MCMC', cex.main = 1.6, font.main = 2, ylim = c(0, 300))




x2 <- sig_theta2_store[burnin:runs]
hist(x2, breaks = 100, probability = TRUE,
     col = "#FA7A35",
     border = "black",
     main = NULL, cex.main = 1.6, font.main = 2,
     xlab = expression(sigma[theta]^2), ylim = c(0, 5), xlim = c(0, 4))

invgamma_loglik <- function(params) {
  shape <- params[1]
  rate <- params[2]
  if (shape <= 0 || rate <= 0) return(-Inf)
  sum(dinvgamma(x2, shape = shape, rate = rate, log = TRUE))
}
start <- c(shape =1, rate = 10)
fit <- optim(start, invgamma_loglik, control = list(fnscale = -1), hessian = TRUE)
fit
shape_hat <- fit$par[1]
rate_hat <- fit$par[2]
curve(dinvgamma(x, shape , shape =shape_hat, rate = rate_hat), 
      col = "black", lwd = 2, add = TRUE)

legend("topright", legend = bquote(IG(.(round(shape_hat, 2)),~.(round(rate_hat, 2)))), 
       col = "black", lwd = 2, bty = "n")











#======================================================================================================================================================================
#                                           Hybrid
#======================================================================================================================================================================


library("invgamma")
library('MASS')

log_cond = function(theta, sigma_theta2){
  beta* play_a_game(theta) - 1/(2*sigma_theta2)*sum(theta^2) - S/2 * log(2*pi *sigma_theta2) 
}

popSize =100
n_gens = 100
beta= 100
S = npars 
a = 1e-3
b = 1e-3
sigma_theta2_store = matrix(NA, nrow = popSize, ncol = n_gens)
pop_store = array(NA, dim = c(popSize, S, n_gens))
fitness_store =  matrix(NA, nrow = popSize, ncol = n_gens)

monitor_func <- function(ga_obj) {
  current_gen <<- ga_obj@iter  
  sigma_theta2_store[, current_gen] <<- apply(ga_obj@population, 1, function(theta) {
    rinvgamma(1, shape = a + S/2, rate = b + sum(theta^2)/2)
  })
  fitness_store[, current_gen] <<- ga_obj@fitness
  pop_store[, , current_gen] <<- (ga_obj@population)
  gaMonitor(ga_obj)
}


best_log_cond <<- -Inf
best_theta <<- NULL
best_sigma_theta2 <<- NULL
obj = function(theta){
  sigma_theta2 = rinvgamma(1, shape = a + S/2, rate = b + sum(theta^2)/2)
  current_log_cond  = log_cond(theta, sigma_theta2)
  if (current_log_cond > best_log_cond) {
    best_log_cond <<- current_log_cond
    best_theta <<- theta
    best_sigma_theta2 <<- sigma_theta2
  }
  return(current_log_cond)
}

GA <- ga(type = "real-valued", fitness = obj,lower = rep(-10, S), upper = rep(10, S), popSize = popSize, maxiter = n_gens, 
         keepBest = TRUE, monitor = monitor_func, pmutation = 0.8)


param_store = matrix(NA, nrow = S, ncol = popSize*n_gens)
for (j in 1:S){
  onestore =c()
  for (i in 1:n_gens){
    onestore = c(onestore, pop_store[1:popSize,j, i])
  }
  param_store[j, ] = onestore
}

play_a_game(MAP_estimates)





x1 <- apply(param_store[, burnin:runs], 2, function(x) sum(x^2))

plot.ts(x1)
plot.ts(x1,
        col = "#2323FF",  # bold blue-purple
        lwd = 2,
        xlab = "Iteration",
        ylab = expression("||" * bold(theta) * "||"^2),
        main = bquote(beta == .(beta)), cex.main = 1.6, font.main = 2, ylim = c(350, 1250))



library(invgamma)
x2 <- c(sigma_theta2_store)[burnin:runs]
hist(x2, breaks =100)
hist(x2, breaks = 100, probability = TRUE,
     col = "#FF2800",
     border = "black",
     main = bquote(sigma[Init]^2 == .(prop_var)), cex.main = 1.6, font.main = 2,
     xlab = expression(sigma[theta]^2), ylim = c(0, 35))





















hist(lambda_store[5, burnin:(runs+1)], breaks = 200, freq = FALSE ,col = 'darkgrey', ylim = c(0, 25), xlab = expression(bar(sigma)), main = NULL)
hist(param_store[, 1], breaks = 20000, freq =FALSE, col = rgb(0, 0, 1, alpha = 0.8))
legend("topright", legend = c("MH", "GA"), fill = c("darkgrey", rgb(0, 0, 1, alpha = 0.8)), border = NA)
plot.ts(param_store[, 1])

hist(lambda_store[S+1, burnin:(runs+1)], breaks = 200, col  = 'green', freq = FALSE,  xlab = expression(sigma[theta]^2), main = NULL, ylim = c(0, .7))

hist(c(sig_theta2_store[1, ]),breaks = 5000, col = 'green', freq = FALSE, xlim = c(0, 5), xlab = expression(sigma[theta]^2), main = NULL)
hist(c(sigma_theta2_store[, 1:n_gens]), breaks = 5000, col  = rgb(1, 0, 0, alpha = 0.6), freq = FALSE, add = TRUE)
legend("topright", legend = c("GD Hybrid", "GA Hybrid"), fill = c("green", rgb(1, 0, 0, alpha = 0.6)), border = NA)



MAP_estimates =  apply(round(param_store,5), 2, function(x){
  unique_x = unique(x)
  print (length(unique((x))))
  return( unique_x[which.max(tabulate(match(x, unique_x)))])
}
)
MAP_estimates

round(param_store[1,1], 2)

find_mode_hist <- function(x, bins =  ceiling((max(x) - min(x)) / (2 * IQR(x) / length(x)^(1/3) )  )) {
  hist_data <- hist(x, breaks = bins, plot = FALSE)
  mode_bin <- which.max(hist_data$counts)  # Bin with max count
  mode_value <- mean(hist_data$breaks[mode_bin:(mode_bin + 1)])  # Midpoint of bin
  return(mode_value)
}
Modes = apply(param_store, 2, find_mode_hist)



theta_hat = GA@solution[1, ]
theta_hat
1/(2*best_sigma_theta2)


colors <- function(X, colorss = c('red', 'blue', 'green')){
  cols <- colorss[apply(X, 1, function(X) which.max(X))]
  return(cols)
}
K <- 100
x1 <- seq(min(X[, 1]), max(X[, 1]), length.out = K)
x2 <- seq(min(X[, 2]), max(X[, 2]), length.out = K)
xx1 <- rep(x1, K)
xx2 <- rep(x2, each = K)
XX <- cbind(xx1, xx2)
YY <- matrix(1, K^2, q)
res_fitted <- neural_net(XX, YY, theta = best_theta, m, nu = 0)
plot(XX[, 1], XX[, 2], col = colors((res_fitted$A2)), cex  =1/2, xlab = 'X1', ylab = 'X2')
points(X_val[, 1], X_val[, 2], col = cols[apply(Y_val, 1, function(x) which.max(x))], pch = 16)
legend("bottomright", legend = c(expression(alpha),expression(beta), expression(rho)), col = c('red', 'blue', 'green'), pch = 16, inset = c(0.01, 0.05), bty = 'n', cex = 0.8)


mean( apply(neural_net(X_val, Y_val, theta = best_theta, m, nu = 0)$A2, 1, which.max) == apply(Y_val, 1, which.max))


