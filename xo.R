rm(list = ls())

#===============================================================================
# Populate the state matrix
#===============================================================================
S = matrix(0,9,8)
# Row Sums:
S[1:3,1] = 1
S[4:6,2] = 1
S[7:9,3] = 1
# Col Sums:
S[c(1:3)*3-2,4] = 1
S[c(1:3)*3-1,5] = 1
S[c(1:3)*3-0,6] = 1
# Diag Sums
S[c(1,5,9),7] = 1
S[c(3,5,7),8] = 1
S

#===============================================================================
# Evaluate the Game State rho(m,S)
#===============================================================================
rho = function(m,S)
{
  m = matrix(m,ncol = 1)
  t(m) %*% S
  player1  = any(t(m)%*%S==-3) # X
  player2  = any(t(m)%*%S==+3) # O
  m_1      = (m==-1)
  m_2      = (m==+1)
  tie      = sum((t(m_1)%*%S>0)*(t(m_2)%*%S>0))==8
  winner   = c(-1,0,1)[c(player1,tie,player2)] #NULL of nothing
  terminal = player1|tie|player2 #FALSE if nothing
  ret      = list(terminal = terminal,winner = winner)
  return(ret)
}

m = as.matrix(c(1,1,-1,0,0,0,0,0,-1))
matrix(m,3,3,byrow = TRUE)
rho(m,S)

#===============================================================================
#                             Environment
#===============================================================================
play = function(theta, seed = 1)
{
  set.seed(seed)
  random_placements = c()
  player_placements = c()
  m = as.matrix(rep(0, 9))
  k = 1
  while(!rho(m, S)$terminal){
    player_placement =which.max(control(m, theta)$action)
    m[player_placement] = k
    player_placements = c(player_placements, player_placement)
    k = k*-1
    if (!rho(m, S)$terminal){
      if (length(which(m==0)) ==1 ){
        random_placement = which(m ==0)
        m[random_placement] = k
        random_placements = c(random_placements, random_placement)
        k = k*-1
      }else{
        random_placement = sample(which(m ==0), 1)
        m[random_placement] = k
        random_placements = c(random_placements, random_placement)
        k = k*-1 
      }
    }
  }
  return (list(winner = rho(m, S)$winner, m = matrix(m, 3, 3, byrow = TRUE), random_placements = random_placements, player_placements = player_placements))
}

play_random = function(theta, seed = 1)
{
  set.seed(seed)
  m = as.matrix(rep(0, 9))
  random_placements = c()
  k = 1
  while(!rho(m, S)$terminal){
    if (length(which(m==0)) ==1 ){
      m[(which(m ==0))] = k; k = k*-1
    }else{
      m[sample(which(m ==0), 1)] = k;k = k*-1 
    }
    if (!rho(m, S)$terminal){
      if (length(which(m==0)) ==1 ){
        random_placement = which(m==0);random_placements = c(random_placements, random_placement)
        m[random_placement] = k; k = k*-1
      }else{
        random_placement = sample(which(m ==0), 1); random_placements = c(random_placements, random_placement)
        m[random_placement] = k
        k = k*-1 
      }
    }
  }
  return (list(winner = rho(m, S)$winner, m = matrix(m, 3, 3, byrow = TRUE), random_placements = random_placements))
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



control = function(m, pars)
{
  # res_model = model( cbind(t(m)),pars,rep(nodes,2), q= q)
  res_model = model( cbind(t(m), t(m)%*%S),pars,rep(nodes,2), q= q)
  logits <- res_model$logits
  allowed_actions <- (m ==0)
  logits[!allowed_actions] <- -Inf
  action_probs <- softmax(t(logits))
  return(list(action =  t(action_probs)))
}


# NN parameters for control
nodes = 3
p = 9+8
# p= 9
q = 9;npars = p*nodes+nodes*nodes+nodes*q+nodes+nodes+q
theta_rand =  c(runif(npars,-10,10))


#========================================================================
#                   Objectives and Fitness 
#========================================================================
no_train_games= 1e2
play_a_game = function(theta)
{
  winner_list = c()
  random_placements = c()
  for (i in 1:no_train_games) {
    winner_list[i] = play(theta, seed  = i)$winner
    random_placements[[i]] =  play(theta, seed  = i)$random_placements
  }
  return (list(wins = sum(winner_list == 1)/ no_train_games, random_placements = random_placements))
}
play_a_game(theta_rand)


nu = 0
play_a_game_reg = function(theta, nu = 0)
{
  return (play_a_game(theta)$wins - nu*sum(theta^2))
}
play_a_game_reg(theta_rand, nu = nu)


#========================================================================
#                    Evolutionary Learning
#========================================================================
library('GA')
popSize =100
n_gens = 10
GA  = ga(type = "real-valued",  fitness = function(theta) play_a_game_reg(theta, nu = nu), lower = rep(-10,(npars)), upper = rep(10,(npars)),popSize = popSize,maxiter = n_gens,keepBest = TRUE, pmutation=0.8)







GA@solution
plot(GA)
theta_hat = GA@solution[1,]
theta_hat
# IS
res = play_a_game(theta_hat)
res$wins
all_time_placements = res$random_placements



# OOS
check_if_oos = function(oos_random_placement, all_time_placements)
{
  for (k in 1:length(all_time_placements)){
    res = try(all.equal(oos_random_placement, all_time_placements[[k]]), silent = TRUE)
    # print (res)
    if (isTRUE(res)){
      return (FALSE)
    }
  }
  return (TRUE)
}

check_if_oos(c(9, 2,8), all_time_placements)


no_test_games = 4e1
winner_list = c()
random_placements = c()
player_placements = c()
seed = no_train_games+1
cnt = 1
while (!(length(winner_list) == no_test_games)){
  oos = play(theta_hat, seed  = seed)
  if (check_if_oos(oos$random_placements, all_time_placements)){
    winner_list[cnt]= oos$winner
    random_placements[[cnt]] = oos$random_placements 
    player_placements[[cnt]] = oos$player_placements
    cnt = cnt+1
    all_time_placements = c(all_time_placements,oos$random_placements )
  }
  seed = seed+1
}

Owins = sum(winner_list==1); Xwins = sum(winner_list==-1);Ties = sum(winner_list == 0)

c(Owins, Xwins, Ties)*2
sum(Owins)/ (sum(Owins) + sum(Xwins))


# Check all OOS
tlist = c()
for (i in 1:length(random_placements))
{
  check = check_if_oos(random_placements[[i]], res$random_placements)
  tlist[i] = check
  if (check == FALSE){
    print (random_placements[[i]])
  }
}
sum(tlist)


library(ggplot2)
library(ggtext)
library(gridExtra)
board_plot = function(board_state,save_path = "C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/XO/pics/more"){
  positions <- data.frame(
    pos = 1:9,
    row = rep(3:1, each = 3),   # rows 3,3,3, 2,2,2, 1,1,1 (top to bottom)
    col = rep(1:3, times = 3)   # cols 1,2,3 repeated
  )
  
  # Merge board_state tokens with positions
  df <- merge(positions, data.frame(pos = 1:9, token = board_state), by = "pos")
  
  p = ggplot(df, aes(x = col, y = row)) +
    geom_tile(fill = "white", color = "black", linewidth = 2) +
    geom_richtext(aes(label = token),
                  size = 24,
                  fill = NA, label.color = NA,
                  color = c("X" = "#FF2800", "O" = "#2323FF")[df$token],
                  family = "sans", fontface = "bold") +
    scale_x_continuous(breaks = 1:3, labels = NULL, expand = c(0,0)) +
    scale_y_continuous(breaks = 1:3, labels = NULL, expand = c(0,0)) +
    coord_fixed() +
    theme_void() +
    theme(
      plot.margin = margin(20, 20, 20, 20),
      panel.background = element_rect(fill = "white", color = NA)
    )
  legend_df <- data.frame(
    Label = c("Wins", "Losses", "Ties"),
    Count = c(wins, losses, ties)
  )
  legend_table <- tableGrob(legend_df, rows = NULL, cols = NULL,
                            theme = ttheme_minimal(
                              core = list(fg_params = list(fontface = "bold", fontsize = 13,
                                                           col = c("#2323FF", "#FF2800", "#F28C38"))), 
                              colhead = list(fg_params = list(fontsize = 0))
                            ))
  p2 <- p + annotation_custom(
    grob = legend_table,
    xmin = 3.3, xmax = 4.5,   # shift right beyond column 3
    ymin = 2.5, ymax = 3.5    # stays near top
  )
  png(sprintf("%s/frame_%03d.png", save_path, plot_counter), width = 800, height = 600)
  plot (p2)
  dev.off()
  plot_counter <<- plot_counter + 1
  # plot(p2)
}



plot_counter = 1

for (k in 1:length(player_placements)){
# k = 1
  player <- player_placements[[k]]
  random <-random_placements[[k]]
  
  if( k== 1){
    wins = 0; losses = 0; ties = 0
  }else{
    wins = sum(winner_list[1:k-1] == 1); losses = sum(winner_list[1:k-1] == -1) ; ties = sum(winner_list[1:k-1] == 0)  
  }
  max_moves <- length(player) + length(random)
  board_state <- rep("", 9)
  p_cnt = 1
  r_cnt = 1
  
  
  for (i in 1:max_moves) {
    if (i %% 2 == 1) {
      move <- player[p_cnt]
      board_state[move] <- "O"
      board_plot(board_state)
      p_cnt <- p_cnt + 1
    } 
    if (i %% 2 == 0) {
      move <- random[r_cnt]
      board_state[move] <- "X"
      board_plot(board_state)
      r_cnt <- r_cnt + 1
    }
  }
}















# OOS Rando vs Rando
no_test_games = 1e4
winner_list = c()
all_time_placements = c()
seed = no_train_games+1
cnt = 1
while (!(length(winner_list) == no_test_games)){
  oos = play_random(theta_hat, seed  = seed)
  if (length(all_time_placements) ==0 ){
    winner_list[cnt]= oos$winner
    all_time_placements[[cnt]] = oos$random_placements 
    cnt = cnt+1
  }
  if (check_if_oos(oos$random_placements, all_time_placements)){
    winner_list[cnt]= oos$winner
    all_time_placements[[cnt]] = oos$random_placements 
    cnt = cnt+1
  }
  seed = seed+1
}

Owins = sum(winner_list==1); Xwins = sum(winner_list==-1);Ties = sum(winner_list == 0)

c(Owins, Xwins, Ties)*4
sum(Owins)/ (sum(Owins) + sum(Xwins))



# Check all OOS
tlist = c()
for (i in 1:length(all_time_placements))
{
  check = check_if_oos(all_time_placements[[i]], all_time_placements[-i])
  tlist[i] = check
  if (check == FALSE){
    print (all_time_placements[[i]])
  }
}
sum(tlist)



# # IS Rando vs Rando
# winner_list = c()
# for (i in 1:100){
#   winner_list[i] = play_random(theta_hat, seed  = i)$winner
# }
# Owins = sum(winner_list==1); Xwins = sum(winner_list==-1);Ties = sum(winner_list == 0)
# 
# c(Owins, Xwins, Ties)
# sum(Owins)/ (sum(Owins) + sum(Xwins))





# nu Curve
n_nus = 100
n_seeds = 1e4
winner_mat = matrix(NA, nrow = n_seeds, ncol = n_nus+1)
for (nu in 0:n_nus)
{
  nu = nu/1e3
  popSize =100;n_gens = 100
  obj = play_a_game
  GA  = ga(type = "real-valued",fitness = function(theta) play_a_game(theta, nu = nu), lower = rep(-10,(npars)), upper = rep(10,(npars)),popSize = popSize,maxiter = n_gens,keepBest = TRUE, pmutation=0.8, monitor = FALSE)
  theta_hat = GA@solution[1,]
  cnt = 1
  for (seed in (no_games+1):(no_games+n_seeds))
  {
    winner_mat[cnt, (nu*1e3+1)] = play(theta_hat, seed)$winner
    cnt = cnt+1
  }
}
  
plot.ts(colSums(winner_mat)/n_seeds, xlab = expression(nu %*% 1000), ylab ='Hit-rate')
colSums(winner_mat)/n_seeds



#========================================================================
#                  Metropolis-Hastings
#========================================================================

library("invgamma")
library('MASS')
a = 1e-3
b = 1e-3
beta = 1
SS = npars
prop_var = 10
theta = rnorm(SS, mean = 0, sd = sqrt(prop_var))
# theta = rep(10, SS)
# theta = theta_hat
epsilon0 <- 1e-6
cov_prop = diag(SS)*prop_var
window_size <- 100
stride = 100
kappa = 0.6
target_accept = 0.234
s = 1

sig_theta2 = rinvgamma(1, shape = a + SS/2, rate = b + sum(theta^2)/2)
runs = 1e5
theta_store = matrix(NA, nrow = SS, ncol = runs)
sig_theta2_store = matrix(NA, nrow = 1, ncol = runs)
theta_store[, 1] = theta
sig_theta2_store[, 1] = sig_theta2
alphastore = c()
post_store = matrix(NA, nrow = 1, ncol = runs)
post_store[, 1] = beta*play_a_game(theta) - sum(theta^2)/(2*sig_theta2)

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
  old = play_a_game(theta)
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
  sig_theta2 = rinvgamma(1, shape = a + SS/2, rate = b + sum(theta^2)/2)
  # print (sig_theta2)
  sig_theta2_store[, i] = sig_theta2

  if (i < stride)
  {   
    gamma_n <- 1 / (i^kappa)
    s <- s * exp(gamma_n * (alpha - target_accept))
    cov_emp <- cov(t(theta_store[,1:i]))
    cov_prop <- s*(cov_emp + epsilon0 * diag(SS))
  }


  if (i > stride & i < burnin & i < stride*window_size)
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


# For checkpoints
i_last <- i  # Last completed iteration


# ESS
library(mcmcse)
ess = multiESS(t(theta_store[, burnin:runs]))
ess


lambda_store = rbind(theta_store, sig_theta2_store)[,]
# Just checks
for (i in 1:(S+1)){
  plot.ts((lambda_store[i, ]), main  = i)
}
plot.ts(lambda_store[1,  ])
hist(lambda_store[1,burnin:runs ], breaks =100)
hist(lambda_store[SS+1, burnin:runs], breaks = 200)


# Posterior
post = post_store[1, burnin:runs] -  log(sqrt((2*pi*sig_theta2_store[1,  (burnin:runs) - 1])^SS))
hist(post, breaks =100, freq = FALSE)
plot.ts(post)
dens = density(post)
dens$x[which.max(dens$y)] 




# MAP estimates
burnin = 40000
MAP_estimates = c()
for (i in 1:nrow(theta_store[, burnin:runs])){
  dens = density(theta_store[i, burnin:runs])
  MAP_estimates[i] = dens$x[which.max(dens$y)]  
}
play_a_game(MAP_estimates)



find_mode_hist <- function(x, bins =  ceiling((max(x) - min(x)) / (2 * IQR(x) / length(x)^(1/3) )  )) {
  hist_data <- hist(x, breaks = bins, plot = FALSE)
  mode_bin <- which.max(hist_data$counts)  # Bin with max count
  mode_value <- mean(hist_data$breaks[mode_bin:(mode_bin + 1)])  # Midpoint of bin
  return(mode_value)
}
Modes = apply(theta_store, 1, find_mode_hist)



# IS
res = play_a_game(MAP_estimates)
res$wins
all_time_placements = res$random_placements



# OOS
check_if_oos = function(oos_random_placement, all_time_placements)
{
  for (k in 1:length(all_time_placements)){
    res = try(all.equal(oos_random_placement, all_time_placements[[k]]), silent = TRUE)
    # print (res)
    if (isTRUE(res)){
      return (FALSE)
    }
  }
  return (TRUE)
}

check_if_oos(c(9, 2,8), all_time_placements)


no_test_games = 1e4
winner_list = c()
random_placements = c()
seed = no_train_games+1
cnt = 1
while (!(length(winner_list) == no_test_games)){
  oos = play(MAP_estimates, seed  = seed)
  if (check_if_oos(oos$random_placements, all_time_placements)){
    winner_list[cnt]= oos$winner
    random_placements[[cnt]] = oos$random_placements 
    cnt = cnt+1
    all_time_placements = c(all_time_placements,oos$random_placements )
  }
  seed = seed+1
}

Owins = sum(winner_list==1); Xwins = sum(winner_list==-1);Ties = sum(winner_list == 0)

c(Owins, Xwins, Ties)*2
sum(Owins)/ (sum(Owins) + sum(Xwins))


# Check all OOS
tlist = c()
for (i in 1:length(random_placements))
{
  check = check_if_oos(random_placements[[i]], res$random_placements)
  tlist[i] = check
  if (check == FALSE){
    print (random_placements[[i]])
  }
}
sum(tlist)





# Fitting invgamma
x <- lambda_store[SS + 1, burnin:runs]
plot.ts(x)
# x = post_store[1, ]
invgamma_loglik <- function(params) {
  shape <- params[1]
  rate <- params[2]
  if (shape <= 0 || rate <= 0) return(-Inf)
  sum(dinvgamma(x, shape = shape, rate = rate, log = TRUE))
}
start <- c(shape =1, rate = 10)
fit <- optim(start, invgamma_loglik, control = list(fnscale = -1), hessian = TRUE)
fit
shape_hat <- fit$par[1]
rate_hat <- fit$par[2]

hist(x, breaks =200, probability = TRUE,
     main = "Fitted Inverse Gamma Distribution",
     xlab = expression(lambda),
     col = "lightgray", border = "blue")

curve(dinvgamma(x, shape , shape =shape_hat, rate = rate_hat), 
      col = "yellow", lwd = 2, add = TRUE)

legend("topright", legend = bquote(IG(.(round(shape_hat, 2)),~.(round(rate_hat, 2)))), 
       col = "blue", lwd = 2, bty = "n")





# par(mfrow = c(1, 2), mar = c(4, 5, 3, 2))  # adjust margins if needed
par(mfrow = c(1, 2), mar = c(4, 5, 5, 2), oma = c(0, 0, 0, 0))  # adjust margins if needed


x1 <- apply(theta_store[, burnin:runs], 2, function(x) sum(x^2))

# plot.ts(x1)
# plot.ts(x1,
#         col = "#2323FF",  # bold blue-purple
#         lwd = 2,
#         xlab = "Iteration",
#         ylab = expression("||" * bold(theta) * "||"^2),
#         main = bquote(beta == .(beta)), cex.main = 1.6, font.main = 2, ylim = c(350, 1250))
plot.ts(x1,
        col = "#2323FF",  # bold blue-purple
        lwd = 2,
        xlab = "Iteration",
        ylab = expression("||" * bold(theta) * "||"^2),
        main = NULL, cex.main = 1.6, font.main = 2, ylim = c(350, 1250))



library(invgamma)
x2 <- sig_theta2_store[1, burnin:runs]
# hist(x2, breaks = 100, probability = TRUE,
#      col = "#FF2800",
#      border = "black",
#      main = bquote(sigma[Init]^2 == .(prop_var)), cex.main = 1.6, font.main = 2,
#      xlab = expression(sigma[theta]^2), ylim = c(0, 35))
hist(x2, breaks = 100, probability = TRUE,
     col = "#2323FF",
     border = "black",
     main = NULL, cex.main = 1.6, font.main = 2,
     xlab = expression(sigma[theta]^2), ylim = c(0, .35), xlim = c(0, 16))

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
mtext(bquote(beta == .(beta)), outer = TRUE, cex = 1.6, font = 2, line = -3)











#========================================================================
#                  Random Search 
#========================================================================

# 
# 
# initialize_population <- function(popSize, npars, lower, upper) {
#   matrix(runif(popSize * npars, lower, upper), nrow = popSize)
# }
# evaluate_fitness <- function(pop, fitness_fn) {
#   apply(pop, 1, fitness_fn)
# }
# roulette_wheel_selection <- function(pop, fitness) {
#   fitness <- fitness - min(fitness) + 1e-6  # prevent negative weights
#   probs <- fitness / sum(fitness)
#   selected_indices <- sample(1:nrow(pop), size = nrow(pop), replace = TRUE, prob = probs)
#   pop[selected_indices, , drop = FALSE]
# }
# blend_crossover <- function(parent1, parent2, alpha = 0.5) {
#   d <- abs(parent1 - parent2)
#   min_gene <- pmin(parent1, parent2) - alpha * d
#   max_gene <- pmax(parent1, parent2) + alpha * d
#   child1 <- runif(length(parent1), min_gene, max_gene)
#   child2 <- runif(length(parent2), min_gene, max_gene)
#   rbind(child1, child2)
# }
# gaussian_mutation <- function(individual, pmutation, sigma) {
#   mask <- runif(length(individual)) < pmutation
#   individual[mask] <- individual[mask] + rnorm(sum(mask), mean = 0, sd = sigma[mask])
#   individual
# }
# 
# perturb_population <- function(pop, perturbation_sd, lower, upper) {
#   noise <- matrix(rnorm(length(pop), mean = 0, sd = perturbation_sd), nrow = nrow(pop))
#   perturbed <- pop + noise
#   # Enforce bounds
#   # perturbed <- pmin(pmax(perturbed, matrix(lower, nrow = nrow(pop), ncol = length(lower), byrow = TRUE)),
#   # matrix(upper, nrow = nrow(pop), ncol = length(upper), byrow = TRUE))
#   perturbed
# }
# 
# 
# 
# 
# run_custom_ga <- function(fitness_fn, lower, upper, popSize, n_gens, pmutation = 0.8,  perturbation_sd = 0.2) {
#   npars <- length(lower)
#   sigma <- 0.1 * (upper - lower)
#   
#   pop <- initialize_population(popSize, npars, lower, upper)
#   fitness <- evaluate_fitness(pop, fitness_fn)
#   
#   best_idx <- which.max(fitness)
#   best <- pop[best_idx, ]
#   best_fit <- fitness[best_idx]
#   fit_mat = matrix(NA, nrow = popSize, ncol = n_gens)
#   
#   for (gen in 1:n_gens) {
#     selected <- roulette_wheel_selection(pop, fitness)
#     # new_pop <- matrix(NA, nrow = popSize, ncol = npars)
#     # 
#     # for (i in seq(1, popSize, by = 2)) {
#     #   p1 <- selected[i, ]
#     #   p2 <- selected[i + 1, ]
#     #   children <- blend_crossover(p1, p2, alpha = 0.5)
#     #   new_pop[i, ] <- gaussian_mutation(children[1, ], pmutation, sigma)
#     #   new_pop[i + 1, ] <- gaussian_mutation(children[2, ], pmutation, sigma)
#     # }
#     
#     new_pop <- perturb_population(selected, perturbation_sd, lower, upper)
#     
#     # Elitism: replace worst with best
#     new_fitness <- evaluate_fitness(new_pop, fitness_fn)
#     worst_idx <- which.min(new_fitness)
#     new_pop[worst_idx, ] <- best
#     new_fitness[worst_idx] <- best_fit
#     
#     pop <- new_pop
#     fitness <- new_fitness
#     
#     if (max(fitness) > best_fit) {
#       best_idx <- which.max(fitness)
#       best <- pop[best_idx, ]
#       best_fit <- fitness[best_idx]
#     }
#     print (best_fit)
#     fit_mat[, gen] = new_fitness
#   }
#   list(best = best, fitness = best_fit, fit_mat = fit_mat)
# }
# 
# result <- run_custom_ga(
#   fitness_fn = function(theta) play_a_game_reg(theta, nu = nu),
#   lower = rep(-10, npars),
#   upper = rep(10, npars),
#   popSize = 100,
#   n_gens = 10,
#   perturbation_sd = 10
#   # pmutation = 0.8
# )
# 
# 
# colMeans(result$fit_mat)
# apply(result$fit_mat, 2, max)
setwd("C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/XO/m + m's/rs")

objective_fn = function(theta) play_a_game_reg(theta, nu = nu)
n_iter = 1e5
lower  = -10
upper = 10
best_theta <- NULL
best_score <- -Inf
theta_mat = matrix(NA, nrow = npars, ncol = n_iter)
score_mat = c()
for (i in 1:n_iter) {
  set.seed(NULL)
  theta <- runif(npars, lower, upper)
  theta_mat[, i] = theta
  score <- objective_fn(theta)
  score_mat[i] = score
  if (score > best_score) {
    best_score <- score
    best_theta <- theta
    cat("Iteration:", i, "| Best Obj Func:", best_score, "\n")
  }
}



# IS
res = play_a_game(best_theta)
res$wins
all_time_placements = res$random_placements



# OOS
check_if_oos = function(oos_random_placement, all_time_placements)
{
  for (k in 1:length(all_time_placements)){
    res = try(all.equal(oos_random_placement, all_time_placements[[k]]), silent = TRUE)
    # print (res)
    if (isTRUE(res)){
      return (FALSE)
    }
  }
  return (TRUE)
}

check_if_oos(c(9, 2,8), all_time_placements)


no_test_games = 5e3
winner_list = c()
random_placements = c()
seed = no_train_games+1
cnt = 1
while (!(length(winner_list) == no_test_games)){
  oos = play(best_theta, seed  = seed)
  if (check_if_oos(oos$random_placements, all_time_placements)){
    winner_list[cnt]= oos$winner
    random_placements[[cnt]] = oos$random_placements 
    cnt = cnt+1
    all_time_placements = c(all_time_placements,oos$random_placements )
  }
  seed = seed+1
}

Owins = sum(winner_list==1); Xwins = sum(winner_list==-1);Ties = sum(winner_list == 0)

c(Owins, Xwins, Ties)*2
sum(Owins)/ (sum(Owins) + sum(Xwins))





set.seed(NULL)
runif(1)









