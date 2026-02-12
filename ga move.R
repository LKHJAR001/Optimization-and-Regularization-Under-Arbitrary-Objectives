rm(list = ls())
library(compiler)
enableJIT(3)
# setwd('C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6')
#========================================================================
#                 1. Set up a game universe (Encoding)
#========================================================================

# draw_objects = function(J)
# {  
#   r_o       =  runif(J,0.3,1)
#   pi_vals   =  runif(J,-1,1)*pi
#   o1        =  r_o*cos(pi_vals)     # x-coordinates of objects
#   o2        =  r_o*sin(pi_vals)
#   return(cbind(o1,o2))
# }

J       =  50
tot_steps = 1000/4
delt    = .0025*4         # How big are the steps you can take?
rad     = 0.05              # How close to an object before you crash?


set.seed(2024)
omega = 2*pi/runif(J, tot_steps*2, tot_steps*3)
phase_shifts = runif(J, 0, 2*pi)
R = runif(J, 0.3, 1)

orbits_func = function(t = 0)
{  
  theta = omega * t + phase_shifts
  x<- R * cos(theta)
  y <- R * sin(theta)
  return(cbind(x, y))
}

draw_starts = function(N = 1)
{
  r_x      =  runif(N,0,0.25)
  pi_x     =  runif(N,-1,1)*pi
  xt       =  matrix(cbind(r_x*cos(pi_x),r_x*sin(pi_x)),nrow = N,byrow = TRUE)
}


objects = orbits_func(0)
xt      = draw_starts(100)

# set.seed(2024)
# J       =  50 
# objects = draw_objects(J)
# xt      = draw_starts(100)
# delt    = 0.002             # How big are the steps you can take?
# rad     = 0.05              # How close to an object before you crash?
# rad_x = 0.1
# tot_steps = 1000


# delt    = 0.0025             # How big are the steps you can take?
# rad     = 0.05              # How close to an object before you crash?
# rad_x = 0.1
# tot_steps = 1000

# Just a function to plot the game:
nu = NULL
success_count = NULL
plot_game = function(xt,objects,rad,cols = 1, trajectories = NULL, orbits = NULL, step = NULL)
{
  plot(objects[,2]~objects[,1],type = 'n', ylim = c(-1,1), xlim = c(-1,1), xlab = expression(x[1]), ylab = expression(x[2]))
  # title(main = bquote(nu == .(nu)))
  text(x = par("usr")[2], y = par("usr")[4],labels = paste("Success:", success_count),adj = c(1, 1), cex = 1.2, col ='darkgreen')
  symbols(objects[,1],objects[,2],circles = rep(rad,J),ylim = c(-1,1),xlim = c(-1,1),inches = FALSE,add = TRUE,bg = 'seagreen')
  points(xt[,2]~xt[,1], pch = 16, cex = 2,col = cols)
  pi_v = seq(-1,1,1/100)*pi
  y_edge = sin(pi_v)
  x_edge = cos(pi_v)
  lines(y_edge~x_edge)
  lines(c(0.25*y_edge)~c(0.25*x_edge),lty = 2)
  # if (!is.null(trajectories) && !is.null(step)) {
  #   for(i in 1:dim(xt)[1])
  #   {
  #     lines(trajectories[i,2,1:(step+1)]~trajectories[i,1,1:(step+1)], col = i)
  #   }
  # }
  # if (!is.null(orbits) && !is.null(step)) {
  #   for(i in 1:J)
  #   {
  #     lines(orbits[i,2,1:(step+1)]~orbits[i,1,1:(step+1)], col = 'darkgrey')
  #   }
  # }
}

plot_game(xt,objects,rad,'black')


# Diagram

nu = NULL
success_count = NULL
plot_game = function(xt,objects,rad,cols = 1, trajectories = NULL, orbits = NULL, step = NULL)
{
  plot(c(-0.5, -0.3), c(0.5, -0.5), type = 'n', ylim = c(-1,1), xlim = c(-1,1), xlab = expression(x[1]), ylab = expression(x[2]))
  # title(main = bquote(nu == .(nu)))
  text(x = par("usr")[2], y = par("usr")[4], cex = 1.2, col ='darkgreen')
  symbols(c(-0.5, -0.3), c(0.5, -0.5), circles = rep(rad, 2),ylim = c(-1,1),xlim = c(-1,1),inches = FALSE,add = TRUE,bg = 'seagreen')
  points(0.5, -0.5, pch = 16, cex = 2,col = cols)
  pi_v = seq(-1,1,1/100)*pi
  y_edge = sin(pi_v)
  x_edge = cos(pi_v)
  lines(y_edge~x_edge)
  lines(c(0.25*y_edge)~c(0.25*x_edge),lty = 2)
  # if (!is.null(trajectories) && !is.null(step)) {
  #   for(i in 1:dim(xt)[1])
  #   {
  #     lines(trajectories[i,2,1:(step+1)]~trajectories[i,1,1:(step+1)], col = i)
  #   }
  # }
  # if (!is.null(orbits) && !is.null(step)) {
  #   for(i in 1:J)
  #   {
  #     lines(orbits[i,2,1:(step+1)]~orbits[i,1,1:(step+1)], col = 'darkgrey')
  #   }
  # }
  # choose angles so lines don't overlap anything
  theta_outer <- pi/6
  theta_inner <- pi/2
  
  R_outer <- 1        # your outer circle radius
  R_inner <- 0.25     # your dashed inner circle radius
  
  arrows(0,0,
         R_outer*cos(theta_outer),
         R_outer*sin(theta_outer),
         lwd = 2,
         length = 0.12)     # arrow head size
  
  arrows(0,0,
         R_inner*cos(theta_inner),
         R_inner*sin(theta_inner),
         lwd = 2,
         lty = 1,
         length = 0.12)
  
  r_crash <- 0.1
  x_obj <- c(-0.5, -0.3)
  y_obj <- c(0.5, -0.5)
  
  phi <- seq(0, 2*pi, length.out = 400)
  Rhalo <- rad + r_crash
  
  lines(x_obj[2] + Rhalo*cos(phi),
        y_obj[2] + Rhalo*sin(phi),
        lty = 2, lwd = 1)
  
  # --- radius line for R_crash ---
  # --- radius line for R_crash ---
  theta_crash <- -pi/2   # straight south
  
  x0 <- x_obj[2]
  y0 <- y_obj[2]
  
  # arrow from green centre to crash boundary
  arrows(x0, y0,
         x0 + Rhalo*cos(theta_crash),
         y0 + Rhalo*sin(theta_crash),
         lwd = 2,
         length = 0.12)
  
  # label position
  s2  <- 0.6     # distance down the arrow
  off2 <- 0.02   # shift to the RIGHT
  
  lx2 <- x0 + off2
  ly2 <- y0 + s2*r_crash*sin(theta_crash) - 0.01
  
  text(lx2, ly2, expression(R[crash]), cex = 1.1, adj = 0)
  
  
  # --- radius line to OTHER green object (object 1) labelled r_j ---
  xj <- x_obj[1]
  yj <- y_obj[1]
  
  arrows(0, 0, xj, yj,
         lwd = 2,
         length = 0.12)
  
  # label along the arrow with a perpendicular offset
  sj   <- 0.6
  offj <- 0.06
  
  theta_j <- atan2(yj, xj)
  
  lxj <- sj*xj - offj*sin(theta_j)
  lyj <- sj*yj + offj*cos(theta_j)
  
  text(lxj, lyj, expression(r[j]), cex = 1.1)
  
  
  
  # --- radius line to black dot (drone) labelled r_t ---
  xt_pt <- c(0.5, -0.5)
  
  arrows(0, 0,
         xt_pt[1], xt_pt[2],
         lwd = 2,
         lty = 1,      # optional: dashed so it differs from r_j
         length = 0.12)
  
  # label with perpendicular offset (same style as r_j)
  st   <- 0.6
  offt <- 0.06
  
  theta_t <- atan2(xt_pt[2], xt_pt[1])
  
  lxt <- st*xt_pt[1] - offt*sin(theta_t)
  lyt <- st*xt_pt[2] + offt*cos(theta_t)
  
  text(lxt, lyt, expression(r[t]), cex = 1.1)
  
  
  
  # label position along the radius
  s <- 0.55
  
  # small perpendicular offset (rotate by +90 degrees)
  off <- 0.08
  lx <- s*R_outer*cos(theta_outer) - off*sin(theta_outer)
  ly <- s*R_outer*sin(theta_outer) + off*cos(theta_outer)
  
  text(lx, ly, expression(R[outer]), cex = 1.1)
  
  
  text(0.5*R_inner*cos(theta_inner),
       0.5*R_inner*sin(theta_inner),
       expression(R[inner]),
       pos = 4)
  
  
  
  # --- draw angle arc for theta_t ---
  xt_pt <- c(0.5, -0.5)              # black dot position
  theta_t <- atan2(xt_pt[2], xt_pt[1])
  
  # --- small dotted x-axis near origin ---
  axis_len <- 0.25
  lines(c(0, axis_len), c(0, 0), lty = 3, lwd = 1)
  
  # --- angle arc ---
  r_arc <- 0.1
  phi_arc <- seq(0, theta_t, length.out = 120)
  
  lines(r_arc*cos(phi_arc),
        r_arc*sin(phi_arc),
        lwd = 2)
  
  # --- arrow head at end of arc (from x-axis direction) ---
  arrows(r_arc*cos(phi_arc[length(phi_arc)-1]),
         r_arc*sin(phi_arc[length(phi_arc)-1]),
         r_arc*cos(phi_arc[length(phi_arc)]),
         r_arc*sin(phi_arc[length(phi_arc)]),
         length = 0.08,
         lwd = 2)
  
  # --- label ---
  theta_mid <- theta_t/2
  off_arc <- 0.03
  
  text((r_arc+off_arc)*cos(theta_mid),
       (r_arc+off_arc)*sin(theta_mid),
       expression(theta[t]),
       cex = 1.1)
  
  # --- phase shift phi_j arc ---
  
  xj <- x_obj[1]
  yj <- y_obj[1]
  
  phi_j <- atan2(yj, xj)
  
  r_arc_phi <- 0.16
  
  # CLOCKWISE arc
  phi_seq <- seq(0, phi_j - 2*pi, length.out = 120)
  
  lines(r_arc_phi*cos(phi_seq),
        r_arc_phi*sin(phi_seq),
        lwd = 2, lty = 3)
  
  # arrow head (clockwise direction)
  arrows(r_arc_phi*cos(phi_seq[length(phi_seq)-1]),
         r_arc_phi*sin(phi_seq[length(phi_seq)-1]),
         r_arc_phi*cos(phi_seq[length(phi_seq)]),
         r_arc_phi*sin(phi_seq[length(phi_seq)]),
         length = 0.08,
         lwd = 2)
  
  # label placement (use midpoint of arc)
  phi_mid <- mean(phi_seq)
  off_phi <- 0.025
  
  text((r_arc_phi+off_phi)*cos(phi_mid),
       (r_arc_phi+off_phi)*sin(phi_mid),
       expression(phi[j]),
       cex = 1.1)
  
  
  
  
}

plot_game(xt,objects,rad,'black')



res_final$trajectories[,, 100]
res_final$orbits[, , 100]
# Arrows between steps:
nu = NULL
success_count = NULL
step_final = 50
step_init = 49
plot_game_step = function(xt,objects,rad,cols = 1, trajectories = NULL, orbits = NULL)
{
  plot(res_final$orbits[, 2, step_final]~res_final$orbits[, 1, step_final],type = 'n', ylim = c(-1,1), xlim = c(-1,1), xlab = expression(x[1]), ylab = expression(x[2]))
  # title(main = bquote(nu == .(nu)))
  text(x = par("usr")[2], y = par("usr")[4],cex = 1.2, col ='darkgreen')
  symbols(res_final$orbits[, 1, step_final],res_final$orbits[, 2, step_final],circles = rep(rad,J),ylim = c(-1,1),xlim = c(-1,1),inches = FALSE,add = TRUE,bg = 'seagreen')
  points(res_final$trajectories[,2, step_final]~res_final$trajectories[,1, step_final], pch = 16, cex = 2,col = cols)
  pi_v = seq(-1,1,1/100)*pi
  y_edge = sin(pi_v)
  x_edge = cos(pi_v)
  lines(y_edge~x_edge)
  lines(c(0.25*y_edge)~c(0.25*x_edge),lty = 2)
  if (!is.null(trajectories)) {
    for(i in 1:dim(xt)[1])
    {
      lines(trajectories[i,2,step_init:(step_final+1)]~trajectories[i,1,step_init:(step_final+1)], col = i)
    }
  }

}

plot_game_step(xt,objects,rad,'black', trajectories = res_final$trajectories)



res_final$trajectories[1,2,90:(100+1)]





#========================================================================
#                  2. Evaluating the game state
#========================================================================


game_status=function(xt,objects, rad)
{
  status    = rep(0,dim(xt)[1])
  min_dists = rep(0,dim(xt)[1])
  J         = dim(objects)[1]
  ones      = matrix(1,J,1)
  # min_dists_x = rep(0, dim(xt)[1])
  # ones_x = matrix(1, dim(xt)[1]- 1, 1)
  for(i in 1:dim(xt)[1])
  {
    min_dists[i] = min(sqrt(rowSums((objects - ones%*%xt[i,])^2)))
    # min_dists_x[i] = min(sqrt(rowSums((xt[-i, ] - ones_x%*%xt[i,])^2)))
  }
  rads <- sqrt(rowSums(xt^2))
  status = 1*(rads > 1)- 1 *(min_dists<rad)
  # status = 1*(rads > 1)- 1 *((min_dists<rad)|(min_dists_x<rad_x))
  ret = list(status = status,dists = min_dists) # You can return min dists
  return(ret)
}

# Just randomly move pieces for now:
control = function(xt,theta)
{
  N_games = dim(xt)[1]
  return(cbind(runif(N_games,-1,1),1))
}



play = function(x0,delt,objects,rad,theta,plt = FALSE,trace = FALSE, pltlast = FALSE, save_path = NULL)
{
  k           =  0  # Count how many steps
  xt          = x0 # Set the initial coordinate(s) for the drone.
  trajectories = NULL
  # Check the game status:
  res_status  = game_status(xt,objects,rad) 
  status      = res_status$status
  # Check which games are still active:
  terminal      =  (status!=0) 
  if(trace)
  {
    trajectories =  array(dim = c(dim(xt)[1],dim(xt)[2],tot_steps+1))
    orbits =  array(dim = c(dim(objects)[1],dim(objects)[2],tot_steps+1))
    trajectories[,,1] = xt
    orbits[,,1] = objects
    
  }
  if (plt) {
    if (!is.null(save_path)) {
      png(sprintf("%s/frame_%03d.png", save_path, k), width = 800, height = 600)
    }
    plot_game(xt, objects, rad, 'black')
    if (!is.null(save_path)) dev.off()
  }
  
  while((any(status==0))&(k<tot_steps))
  {
    
    k =  k+1
    
    
    # if ((k-1)%%every ==0 & k> every){
    #   set.seed(test*tot_steps +k )
    #   objects = objects + cbind(rnorm(J,0, delt), rnorm(J,0, delt))
    # }
    objects = orbits_func(k)
    res_status  = game_status(xt,objects,rad)
    status      = res_status$status
    terminal    = terminal | (status!=0) 
      
      
    # Now, let the model update the position of the pieces:
    # ct = control(xt,theta[(1+((k-1)%/%20)*(npars+1)):((((k-1)%/%20)+1)*(npars+1))], objects)
    # if (k < 50){
    #   thetatemp = theta[1:(npars+1)]
    # }else{
    #   thetatemp = theta[(npars+2):(2*(npars+1)) ]
    # }
    ct = control(xt,theta, objects)
    xt = xt+ct*delt*cbind(1-terminal,1-terminal)
    
    # Checkk the game status after the positions are updates:
    res_status  = game_status(xt,objects,rad)
    status      = res_status$status
    terminal    = terminal | (status!=0)   # Keep terminal drones terminal
    if(trace){
      trajectories[,,k+1] = xt
      orbits[,,k+1] = objects
    }
    status[terminal ==1 & status !=1 ] = -1
    if (plt) {
      if (!is.null(save_path)) {
        png(sprintf("%s/frame_%03d.png", save_path, k), width = 800, height = 600)
      }
      plot_game(xt, objects, rad, c('red', 'black', 'green')[status + 2], trajectories = trajectories, orbits = orbits, step = k)
      if (!is.null(save_path)) dev.off()
    }

  }
  if(pltlast){plot_game(xt,objects,rad,c('red','black','green')[status+2], trajectories = trajectories, orbits = orbits, step = k)}
  return(list(k = k, status = status,xt= xt,trajectories = trajectories, orbits = orbits))
}

# k = 100
# (k-1)%%5 ==0 & k> 5
# seq(1,33*20)[(1+((k-1)%/%5)*(npars+1)):((((k-1)%/%5)+1)*(npars+1))]
# 
# k = 10
# 
# l  =1
# if ((k-1)%%10 ==0){
#   theta = c(theta[1:npars], theta[npars+l])
#   l = l+1
# }else{theta = theta}
#========================================================================
#                  3. Control (Giving a Model Agency.)
#========================================================================


model = function(X,theta,nodes)
{
  # Infer dimensions:
  N = dim(X)[1]
  p = dim(X)[2]
  q = 2
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
  a3 = tanh(t(W3)%*%a2+b3%*%ones)
  
  # Return a list of relevant objects:
  return(list(a3 = t(a3)))
}

# NN parameters only (does not include parameter for R_detection)
p     = 1
q     = 2
nodes = 3
npars = p*nodes+nodes*nodes+nodes*q+nodes+nodes+q
npars
every =1

# theta_rand = c()
# for (k in 1:(2)){
#   theta_rand = c(theta_rand, c(runif(npars,-10,10), 0.08))
# }

theta_rand =  c(runif(npars,-10,10), rep(0.08,1))



squash1 = 1
# squash_x = 0.01
control = function(xt,pars, objects)
{
  R1 = squash1*1/(1+exp(-pars[npars+1]))
  ones      = matrix(1,J,1)
  raddists1 = rep(0, dim(xt)[1])
  # R_x = squash_x*1/(1+exp(-pars[npars+2]))
  # ones_x      = matrix(1,dim(xt)[1]-1,1)
  # raddists_x = rep(0, dim(xt)[1])
  # quad1 = objects[,1]  > 0 & objects[, 2] > 0
  # quad2 = objects[,1]  < 0 & objects[, 2] > 0
  # quad3 = objects[, 1]  < 0 & objects[, 2] < 0
  # quad4 = objects[, 1]  > 0 & objects[, 2] < 0
  # sumquad1 = rep(0, dim(xt)[1])
  # sumquad2 = rep(0, dim(xt)[1])
  # sumquad3 = rep(0, dim(xt)[1])
  # sumquad4 = rep(0, dim(xt)[1])
  for(i in 1:dim(xt)[1])
  {
    tempdists = sqrt(rowSums((objects - ones%*%xt[i,])^2))
    # sumquad1[i] = sum(tempdists[quad1])
    # sumquad2[i] = sum(tempdists[quad2])
    # sumquad3[i] = sum(tempdists[quad3])
    # sumquad4[i] = sum(tempdists[quad4])
    
    tempdists1 = tempdists[tempdists < R1]
    # if (length(tempdists1)  ==0){raddists1[i] = 1/(min(tempdists) -rad)}else{raddists1[i] = sum(1/(tempdists1-rad))^(1+length(tempdists))}
    # if (length(tempdists1)  ==0){raddists1[i] = 1/(min(tempdists) -rad)}else{raddists1[i] = sum(1/(tempdists1-rad))}
    if (length(tempdists1)  ==0){raddists1[i] = 0}else{raddists1[i] = sum(1/(tempdists1-rad))}
    # if(raddists1[i] > 2000){print (min(tempdists))}
    # temp  <<- c(temp, raddists1[i])
    # if (raddists1[i] < 0 ){print (c(i, min(tempdists), terminal[i] ))}
    # if (raddists1[i] < 0){print (min(tempdists) )}
    # tempdists2 = tempdists[tempdists < R2]
    # if (length(tempdists2)  ==0){raddists2[i] = 0}else{raddists2[i] = sum(1/abs(tempdists2 - rad))^length(tempdists2)}
    # tempdists_x = sqrt(rowSums((xt[-i, ] - ones_x%*%xt[i,])^2))
    # tempdists_x = tempdists_x[tempdists_x < R_x]
    # if (length(tempdists_x)  ==0){raddists_x[i] = 0}else{raddists_x[i] = sum(1/abs(tempdists_x - rad_x))^(length(tempdists_x))}
  }
  res_model = model(cbind(raddists1),pars,rep(nodes,2))
  return(res_model$a3)
}

# lin_model <- function(xt, pars){
#   return( cbind(xt[, 1]*pars[1] + xt[, 1]^2*pars[2], xt[, 2]*pars[3] + xt[, 2]^2*pars[4]) )
# }
# 
# control = function(xt,pars)
# {
#   res_model = lin_model(xt,pars)
#   return(res_model)
# }
# npars<- 4
# theta_rand = runif(npars,-1,1)



# model = function(X,theta,nodes)
# {
#   # Infer dimensions:
#    N = dim(X)[1]
#    p = dim(X)[2]
#    q = 2
#    dims = c(p,nodes,q)
# 
#    # Populate weight and bias matrices:
#    index = 1:(dims[1]*dims[2])
#    W1    = matrix(theta[index],dims[1],dims[2])
#    index = max(index)+1:(dims[2]*dims[3])
#    W2    = matrix(theta[index],dims[2],dims[3])
# 
# 
#    index = max(index)+1:(dims[2])
#    b1    = matrix(theta[index],dims[2],1)
#    index = max(index)+1:(dims[3])
#    b2    = matrix(theta[index],dims[3],1)
# 
#    ones    = matrix(1,1,N)
#    a0      = t(X)
# 
#    # Evaluate the updating equation in matrix form
#    a1 = tanh(t(W1)%*%a0+b1%*%ones)
#    a2 = tanh(t(W2)%*%a1+b2%*%ones)
# 
# 
#    # Return a list of relevant objects:
#    return(list(a2 = t(a2)))
# }
# 
# p     = 1
# q     = 2
# nodes = 1
# npars = p*nodes+nodes*q+nodes+q
# npars
# theta_rand = runif(npars,-1,1)
# 
# 
# control = function(xt,pars)
# {
#   res_model = model(xt,pars,nodes)
#   return(res_model$a2)
# }






#========================================================================
#                  4. Objectives and Fitness OG
#========================================================================
nu =0
test = 0
play_a_game = function(theta)
{
  # objects = draw_objects(J)
  # xt      = draw_starts()
  res   = play(xt,delt,objects,rad,theta,plt = FALSE)
  score = mean(res$status==1)
  # return(list(mean = score, successes = sum(res$status==1)))
  # return (score - nu*sum(theta^2))
  return (mean(res$status ==1))
}
play_a_game(theta_rand)


res_final = play(xt,delt,objects,rad,theta_rand,plt = FALSE, trace = TRUE, pltlast = TRUE)
# for(i in 1:dim(xt)[1])
# {
#   lines(res_final$trajectories[i,2,]~res_final$trajectories[i,1,], col = 'grey')
# }

obj = play_a_game


#========================================================================
#                  4. Objectives and Fitness
#========================================================================

play_a_game = function(theta)
{
  res   = play(xt,delt,objects,rad,theta,plt = FALSE)
  score = sum(res$status==1)
  return (score)
}

f= function(k){
  choose(t, k)*(k/t)^k*(1-k/t)^(t - k)
}
lik = function(k){
  f(t/2)/(1-f(t/2))*(1-f(k))*(k < t/2) + f(k)*(k >= t/2)
}



t = nrow(xt)
S = npars+1
popSize =100
n_gens = 2000
sigma_theta2_store = matrix(NA, nrow = popSize, ncol = n_gens)
a = 0
b = 0



obj = function(theta){
  sigma_theta2 = rinvgamma(n = 1, alpha = a + S/2, beta = b + sum(theta^2)/2)
  k_theta = play_a_game(theta)
  return(log(lik(k_theta)) - 1/(2*sigma_theta2)*sum(theta^2))
} 

monitor_func <- function(ga_obj) {
  current_gen <<- ga_obj@iter  
  sigma_theta2_store[, current_gen] <<- apply(ga_obj@population, 1, function(theta) {
    rinvgamma(n = 1, alpha = a+ S/2, beta = b+ sum(theta^2)/2)
  })
  pop_store[, , current_gen] <<- ga_obj@population
}

#========================================================================
#                  5. Evolutionary Learning
#========================================================================
library('GA')
popSize =100
n_gens = 100
# GA  = ga(type = "real-valued",  obj, lower = rep(-20,(2)*(npars+1)), upper = rep(20,(2)*(npars+1)),popSize = popSize,maxiter = n_gens,keepBest = TRUE)
GA  = ga(type = "real-valued",  obj, lower = rep(-10,(npars+1)), upper = rep(10,(npars+1)),popSize = popSize,maxiter = n_gens,keepBest = TRUE, pmutation  = 0.8)


plot(GA)

warnings(GA)


GA@solution
theta_hat = GA@solution[1,]
theta_hat

play_a_game(theta_hat)
squash1*1/(1+exp(-theta_hat[npars+1]))

# 1000 Initializations
success = c()
crash = c()
timeout = c()
for (seed in 1:1000){
  set.seed(seed)
  omega = 2*pi/runif(J, tot_steps*2, tot_steps*3)
  phase_shifts = runif(J, 0, 2*pi)
  R = runif(J, 0.3, 1)
  objects = orbits_func(0)
  xt      = draw_starts(100)
  res_final = play(xt,delt,objects,rad,theta_hat,FALSE,FALSE)
  success = c(success, sum(res_final$status ==1))
  crash = c(crash, sum(res_final$status ==-1))
  timeout = c(timeout, sum(res_final$status ==0))
}
hist(success, breaks = 50, col ='red', xlab = 'Number of Successes', main = '')
hist(crash, breaks = 50, col ='red')

summary(success)
summary(crash)
summary(timeout)




# Playing with different initializations of o_j
set.seed(1)
omega = 2*pi/runif(J, tot_steps*2, tot_steps*3)
phase_shifts = runif(J, 0, 2*pi)
R = runif(J, 0.3, 1)
objects = orbits_func(0)
xt      = draw_starts(5)

success_count = sum(res_final$status == 1)
res_final = play(xt,delt,objects,rad,theta_hat,TRUE,TRUE, TRUE, save_path ="C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/temppics3")
res_final = play(xt,delt,objects,rad, theta_hat,FALSE,TRUE, TRUE)
sum(res_final$status == 1)




library(magick)
files <- list.files("C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/temppics", pattern = "\\.png$", full.names = TRUE, recursive = TRUE)
# files <- sort(files)
img_list <- image_read(files)
animated <- image_animate(img_list, fps =10)
image_write(animated, path = "C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/temppics/0.gif")


# GIF

main_dir <- "D:/drone_images"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, showWarnings = TRUE, recursive = TRUE)
}
for (seed in 1:20) {
  set.seed(seed)
  objects <- draw_objects(J) 
  xt <- draw_starts(100)      
  test= seed
  res_final = play(xt,delt,objects,rad,theta_hat,FALSE,FALSE, FALSE)
  success_count = sum(res_final$status == 1)
  out_dir <- sprintf("%s/seed_%02d", main_dir, seed)
  dir.create(out_dir, showWarnings = FALSE)
  res_final = play(xt,delt,objects,rad,theta_hat,TRUE,TRUE,FALSE, save_path = out_dir)
}
library(magick)
files <- list.files("D:/drone_images", pattern = "\\.png$", full.names = TRUE, recursive = TRUE)
# files <- sort(files)
img_list <- image_read(files)
animated <- image_animate(img_list, fps = 20)
image_write(animated, path = "D:/drone_images/full_drone_animation.gif")











#========================================================================
#                  6. Metropolis-Hastings
#========================================================================

library("invgamma")
library('MASS')
a = 1e-3
b = 1e-3
S = npars+1
prop_var =1
theta = rnorm(S, mean = 0, sd = sqrt(prop_var))
epsilon0 <- 1e-6
cov_prop = diag(S)*prop_var
window_size = 100
stride = 1000
kappa = 0.6
target_accept = 0.234
s = 1

beta = 100



sig_theta2 = rinvgamma(n = 1, shape = a + S/2, rate = b + sum(theta^2)/2)
runs = 1e5
t = nrow(xt)
theta_store = matrix(NA, nrow = S, ncol = runs+1)
theta_accept = matrix(NA, nrow = S, ncol = runs)
sig_theta2_store = matrix(NA, nrow = 1, ncol = runs+1)
kstore = c()
theta_store[, 1] = theta
theta_accept[, 1] = theta
sig_theta2_store[, 1] = sig_theta2

alphastore = c()
k_theta_store = c()
k_theta_new_store = c()

# Binomial-Based
f= function(k){
  choose(t, k)*(k/t)^k*(1-k/t)^(t - k)
}
h = function(k){
  2/t* f(t/2)*k*(k < t/2) + f(k)*(k >= t/2)
}

lik = function(beta, k){
  h(k)^beta
}


t =100


lik(1,1/2)
-100/log(0.1)

seq = seq(0, 1, length = 1000)
plot.ts(c(0:100), log(lik(100, c(0:100))) )
lines(c(0:100), (lik(10, c(0:100))) )
lines(c(0:100), (lik(.1, c(0:100))) )

lines(c(0:100), log(lik(10, c(0:100))) )
lines(c(0:100), log(lik(200, c(0:100))) )

lines(seq*100, log(exp(100*seq)), add = TRUE)
plot.ts(seq*100, log(exp(10*seq)))
plot.ts(seq*100, log(exp(seq)^10))
exp(100*0.8)

exp(100*1)
# Beta-Based
f = function(x){
  1/(gamma(x+1)*gamma(2-x))*(x^x)*((1-x)^(1-x))
}
h = function(x){
  2* f(1/2)*x*(x < 1/2) + f(x)*(x >= 1/2)
}
lik = function(beta, x){
  h(x)^beta
}
plot.ts(seq*100, log(lik(1, seq)))
lines(seq*100, log(lik(1, seq)))

plot.ts(seq, f(seq))



# Exponential-Based
lik = function(beta, x){
  exp(beta*x)
}



acpt_cnt = 0
k_theta_store = c()
p_theta = play_a_game(theta)
k_theta = p_theta*t
k_theta_store[1] = k_theta
post_store = matrix(NA, nrow = 1, ncol = runs)
post_store[, 1] =log(lik(beta, k_theta)) - sum(theta^2)/(2*sig_theta2)
# post_store[, 1] =log(lik(beta, p_theta_new)) - sum(theta^2)/(2*sig_theta2)



for (i in 2:runs){
  # Block 1
  theta_new <- mvrnorm(1, theta, cov_prop)
  
  
  p_theta = play_a_game(theta)
  p_theta_new = play_a_game(theta_new)
  k_theta = p_theta*t 
  k_theta_store[i] = k_theta
  k_theta_new = p_theta_new*t
  k_theta_new_store[i] = k_theta_new
  
  
  if (k_theta_new > 0){
    # Binomial-Based
    alpha = exp(min(log(lik(beta, k_theta_new)) - sum(theta_new^2)/(2*sig_theta2) - log(lik(beta, k_theta)) + sum(theta^2)/(2*sig_theta2), 0))
    # Beta-based
    # alpha = exp(min(log(lik(beta, p_theta_new)) - sum(theta_new^2)/(2*sig_theta2) - log(lik(beta, p_theta)) + sum(theta^2)/(2*sig_theta2), 0))
    # Exp-based
    # alpha = exp(min(log(lik(beta, p_theta_new)) - sum(theta_new^2)/(2*sig_theta2) - log(lik(beta, p_theta)) + sum(theta^2)/(2*sig_theta2), 0))
  }else {alpha = 0}
  if (runif(1, 0, 1) < alpha){
    theta = theta_new
    acpt_cnt = acpt_cnt +1
    post_store[, i] = log(lik(beta, k_theta_new)) - sum(theta^2)/(2*sig_theta2)
  }else{
    post_store[, i] =  log(lik(beta, k_theta))- sum(theta^2)/(2*sig_theta2)
  }
  theta_store[, i] = theta
  alphastore[i] = alpha
  
  #Block 2 
  sig_theta2 = rinvgamma(n = 1, shape = a + S/2, rate = b + sum(theta^2)/2)
  sig_theta2_store[, i] = sig_theta2
  
  
  if (i < stride)
  {   
    gamma_n <- 1 / (i^kappa)
    s <- s * exp(gamma_n * (alpha - target_accept))
    cov_emp <- cov(t(theta_store[,1:i]))
    cov_prop <- s*(cov_emp + epsilon0 * diag(SS))
  }
  
  if (i > stride)
  {
    gamma_n <- 1 / (i^kappa)
    s <- s * exp(gamma_n * (alpha - target_accept))
    idx_recent = seq(from = i - floor(i/stride)*stride, to = i, by = stride)
    # idx_recent = tail(idx_recent, window_size)
    theta_recent <- theta_store[, idx_recent]
    cov_emp <- cov(t(theta_recent))
    cov_prop <- s*( cov_emp + epsilon0 * diag(S))
  }
}

alphastore
burnin = runs/5
burnin = 60000


library(mcmcse)
ess = multiESS(t(theta_store[, burnin:runs]))
ess

for (i in 1:(npars+1)){
  plot.ts((theta_store[i, ]), main  = i)
}
plot.ts(theta_store[1,  burnin:runs ])
hist(sig_theta2_store[1, 1:runs], breaks =100, freq = FALSE)


# L2 Norm
x1 <- apply(theta_store[,1:runs], 2, function(x) sum(x^2)) 
plot.ts(x1)



par(mfrow = c(2, 1), mar = c(4, 5, 3, 2))  # adjust margins if needed
# par(mfrow = c(1, 2), mar = c(4, 5, 3, 2), oma = c(0, 0, 1, 0))  # adjust margins if needed

x1 <- apply(theta_store[, 1:runs], 2, function(x) sum(x^2))

plot.ts(x1,
        col = "#5B3A29",  # bold blue-purple
        lwd = 2,
        xlab = "Iteration",
        ylab = expression("||" * bold(theta) * "||"^2),
        main = 'Binomial-Based', cex.main = 1.6, font.main = 2, ylim = c(0, 80))





x2 <- sig_theta2_store[1, 2e4:runs]
hist(x2, breaks = 200, probability = TRUE,
     col = "#5B3A29",
     border = "black",
     main = NULL, cex.main = 1.6, font.main = 2,
     xlab = expression(sigma[theta]^2), ylim = c(0, 10), xlim = c(0, 1.5))

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

# mtext("Exponential-based", outer = TRUE, cex = 1.6, font = 2, line = -2)

# Posterior
post = post_store[1, 1:runs] -  S/2 * log(2*pi*sig_theta2_store[1,  (1:runs) - 1])
hist(post, breaks =100, freq = FALSE)
plot.ts(post)
dens = density(post)
dens$x[which.max(dens$y)] 








































# MAP Estimates & Performance
MAP_estimates = c()
for (i in 1:nrow(theta_store[, burnin:runs])){
  dens = density(theta_store[i, burnin:runs])
  MAP_estimates[i] = dens$x[which.max(dens$y)]  
}
play_a_game(MAP_estimates)



# 1000 Initializations
success = c()
crash = c()
timeout = c()
for (seed in 1:1000){
  set.seed(seed)
  omega = 2*pi/runif(J, tot_steps*2, tot_steps*3)
  phase_shifts = runif(J, 0, 2*pi)
  R = runif(J, 0.3, 1)
  objects = orbits_func(0)
  xt      = draw_starts(100)
  res_final = play(xt,delt,objects,rad,MAP_estimates,FALSE,FALSE)
  success = c(success, sum(res_final$status ==1))
  crash = c(crash, sum(res_final$status ==-1))
  timeout = c(timeout, sum(res_final$status ==0))
}
hist(success, breaks = 50, col ='red', xlab = 'Number of Successes', main = '')
hist(crash, breaks = 50, col ='red')

summary(success)



squash1*1/(1+exp(-MAP_estimates[npars+1]))




#========================================================================
#                  Random Search 
#========================================================================




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





# 1000 Initializations OOS
success = c()
crash = c()
timeout = c()
for (seed in 1:1000){
  set.seed(seed)
  omega = 2*pi/runif(J, tot_steps*2, tot_steps*3)
  phase_shifts = runif(J, 0, 2*pi)
  R = runif(J, 0.3, 1)
  objects = orbits_func(0)
  xt      = draw_starts(100)
  res_final = play(xt,delt,objects,rad,best_theta,FALSE,FALSE)
  success = c(success, sum(res_final$status ==1))
  crash = c(crash, sum(res_final$status ==-1))
  timeout = c(timeout, sum(res_final$status ==0))
}
hist(success, breaks = 50, col ='red', xlab = 'Number of Successes', main = '')
hist(crash, breaks = 50, col ='red')


summary(success)
summary(crash)
summary(timeout)



# IS

set.seed(2024)
omega = 2*pi/runif(J, tot_steps*2, tot_steps*3)
phase_shifts = runif(J, 0, 2*pi)
R = runif(J, 0.3, 1)
objects = orbits_func(0)
xt      = draw_starts(100)

success_count = sum(res_final$status == 1)
res_final = play(xt,delt,objects,rad,best_theta,TRUE,TRUE, TRUE, save_path ="C:/Users/Jared/OneDrive - University of Cape Town/Documents/Machine Learning/Lectures/Lec 6/temppics")
res_final = play(xt,delt,objects,rad, best_theta,FALSE,TRUE, TRUE)
sum(res_final$status == 1)


squash1*1/(1+exp(-best_theta[npars+1]))



