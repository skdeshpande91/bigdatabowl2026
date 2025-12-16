get_Sigma <- function(n, sigma = 1/6, tau = 1, D = 10){
  Phi <- matrix(nrow = n, ncol = D)
  time <- seq(0, 1, length = n)
  for(d in 1:D){
    Phi[,d] <- choose(D,d) * time^d * (1 - time)^(D-d)
  }
  Sigma <- 
    tcrossprod(Phi) * tau^2 + 
    diag(rep(sigma^2, times = n))
  return(Sigma)
}

get_posterior_params <- function(paths, inform_mean = TRUE,
                                 rec_target = NULL,
                                 def_target = NULL,
                                 sigma = 1/36, tau = 1, D = 10){
  
  n <- nrow(paths)
  los <- paths$los[1]
  ball_x <- paths$ball_land_x[1] 
  ball_y <- paths$ball_land_y[1]
  
  n_frame_in <- paths$num_frames_input[1]
  n_frame_out <- paths$num_frames_output[1]
  
  
  ############################
  # Receiver posterior first
  ############################
  rec_y0 <- paths$rec_y[1] # receiver's initial Y
  rec_x <- paths$rec_x - los # receiver's X relative to LOS
  rec_y <- paths$rec_y - rec_y0 # receiver's Y relative to start
  
  def_y0 <- paths$def_y[1] # defender's initial Y
  def_x <- paths$def_x - los # defender's X relatie to LOS
  def_y <- paths$def_y - def_y0
  
  
  prior_mean <-
    data.frame(rec_x = rep(0, times = n),
               rec_y = rep(0, times = n),
               def_x = rep(0, times = n),
               def_y = rep(0, times = n))
  
  if(inform_mean){
    if(is.null(rec_target)) rec_target <- c(ball_x, ball_y)
    
    prior_mean$rec_x <- seq(from = rec_x[1], to = rec_target[1] - los, length = n)
    prior_mean$rec_y <- seq(from = rec_y[1], to = rec_target[2] - rec_y0, length = n)
    
    if(is.null(def_target)) def_target <- c(ball_x, ball_y)
    prior_mean$def_x <- seq(from = def_x[1], to = def_target[1] - los, length = n)
    prior_mean$def_y <- seq(from = def_y[1], to = def_target[2] - def_y0, length = n)
  }
  
  Sigma <- get_Sigma(n, sigma = sigma, tau = tau, D = D)
  post_mean_list <- list()
  post_cov_list <- list()
  for(ix in 1:(n-1)){
    train_index <- 1:ix
    test_index <- (ix+1):n
    
    Sigma_11 <- Sigma[train_index, train_index]
    Sigma_12 <- Sigma[train_index, test_index]
    Sigma_21 <- Sigma[test_index, train_index]
    Sigma_22 <- Sigma[test_index, test_index]
    
    Omega_11 <- solve(Sigma_11)
    M1 <- Sigma_21 %*% Omega_11
    M2 <- Sigma_22 - Sigma_21 %*% Omega_11 %*% Sigma_12
    
    
    post_mean <- 
      data.frame(
        rec_x = prior_mean$rec_x[test_index] + M1 %*% (rec_x[train_index] - prior_mean$rec_x[train_index]),
        rec_y = prior_mean$rec_y[test_index] + M1 %*% (rec_y[train_index] - prior_mean$rec_y[train_index]),
        def_x = prior_mean$def_x[test_index] + M1 %*% (def_x[train_index] - prior_mean$def_x[train_index]),
        def_y = prior_mean$def_y[test_index] + M1 %*% (def_y[train_index] - prior_mean$def_y[train_index]))
    
    post_mean$rec_x <- post_mean$rec_x + los
    post_mean$rec_y <- post_mean$rec_y + rec_y0
    post_mean$def_x <- post_mean$def_x + los
    post_mean$def_y <- post_mean$def_y + def_y0
    
    post_mean_list[[ix]] <- post_mean
    post_cov_list[[ix]] <- M2
  }
  
  prior_mean$rec_x <- prior_mean$rec_x + los
  prior_mean$rec_y <- prior_mean$rec_y + rec_y0
  prior_mean$def_x <- prior_mean$def_x + los
  prior_mean$def_y <- prior_mean$def_y + def_y0
  
  return(list(prior_mean = prior_mean, post_mean = post_mean_list, post_cov = post_cov_list))
  
}


draw_paths <- function(post, paths, n_draws = 500){
  
  n <- nrow(paths)
  
  draws_list <- list()
  probs_list <- list()
  ystar_list <- list()
  max_y_dens <- rep(NA, times = n-1)
  max_sep <- rep(NA, times = n-1)
  max_sep_dens <- rep(NA, times = n-1)
  sep_dens_list <- list()
  final_frame_list <- list()
  
  for(ix in 1:(n-1)){
    #print(ix)
    rec_x_samples <- MASS::mvrnorm(n = n_draws, mu = post$post_mean[[ix]][["rec_x"]], Sigma = post$post_cov[[ix]])
    rec_y_samples <- MASS::mvrnorm(n = n_draws, mu = post$post_mean[[ix]][["rec_y"]], Sigma = post$post_cov[[ix]])
    def_x_samples <- MASS::mvrnorm(n = n_draws, mu = post$post_mean[[ix]][["def_x"]], Sigma = post$post_cov[[ix]])
    def_y_samples <- MASS::mvrnorm(n = n_draws, mu = post$post_mean[[ix]][["def_y"]], Sigma = post$post_cov[[ix]])
    
    final_frame_ix <- ncol(rec_x_samples)
    final_frame_samples <-
      data.frame(
        frame_id = rep(nrow(paths), times = n_draws),
        last_qb_s = rep(paths$last_qb_s[1], times = n_draws),
        pass_length = rep(paths$pass_length[1], times = n_draws),
        ball_land_x = rep(paths$ball_land_x[1], times = n_draws),
        ball_land_y = rep(paths$ball_land_y[1], times = n_draws),
        rec_x = rec_x_samples[,final_frame_ix],
        rec_y = rec_y_samples[,final_frame_ix],
        def_x = def_x_samples[,final_frame_ix],
        def_y = def_y_samples[,final_frame_ix]) |>
      dplyr::mutate(
        def2ball = sqrt( (def_x - ball_land_x)^2 + (def_y - ball_land_y)^2),
        rec2ball = sqrt( (rec_x - ball_land_x)^2 + (rec_y - ball_land_y)^2),
        def2rec = sqrt( (def_x - rec_x)^2 + (def_y - rec_y)^2),
        rec2side = ifelse(rec_y <= 27, rec_y, 54-rec_y),
        def2side = ifelse(def_y <= 27, def_y, 54-def_y),
        rec2goal = 110 - rec_x,
        ball2side = ifelse(ball_land_y <= 27, ball_land_y, 54-ball_land_y))
    
    final_frame_list[[ix]] <- final_frame_samples
    
    prob_samples <- predict(object = rf_fit, data = final_frame_samples)$predictions
    ystar_samples <- rep(NA, times = n_draws)
    for(i in 1:n_draws){
      ystar_samples[i] <- sample(c("IN", "I", "C"), size = 1, prob = prob_samples[i,])
    }
    
    c_dens <- density(prob_samples[,"C"], from = 0, to = 1)
    in_dens <- density(prob_samples[,"IN"], from = 0, to = 1)
    i_dens <- density(prob_samples[,"I"], from = 0, to = 1)
    
    
    max_y_dens[ix] <- max(c(c_dens$y, in_dens$y, i_dens$y))
    max_sep[ix] <- max(final_frame_samples$def2rec)
    
    draws_list[[ix]] <- 
      list(rec_x = rec_x_samples, rec_y = rec_y_samples, 
           def_x = def_x_samples, def_y = def_y_samples)
    probs_list[[ix]] <- list(c_dens = c_dens, in_dens = in_dens, i_dens = i_dens)
    ystar_list[[ix]] <- ystar_samples
  }
  ix <- n
  print(ix)
  final_frame_list[[ix]] <- 
    rf_final_frame |>
    dplyr::filter(game_id == paths$game_id[1] & play_id == paths$play_id[1])
  
  
  
  # Now do separation densities & summaries
  pred_sep_summary <- 
    data.frame(MEAN = rep(NA, times = n-1),
               L95 = rep(NA, times = n-1),
               L80 = rep(NA, times = n-1),
               L50 = rep(NA, times = n-1),
               U50 = rep(NA, times = n-1),
               U80 = rep(NA, times = n-1),
               U95 = rep(NA, times = n-1))

  for(ix in 1:(n-1)){
    sep <- final_frame_list[[ix]]$def2rec
    sep_dens <- density(sep, from = 0, to = max(max_sep))
    max_sep_dens[ix] <- max(sep_dens$y)
    sep_dens_list[[ix]] <- sep_dens
    
    pred_sep_summary$MEAN[ix] <- mean(sep)
    pred_sep_summary$L95[ix] <- quantile(sep, prob = 0.025)
    pred_sep_summary$L80[ix] <- quantile(sep, prob = 0.10)
    pred_sep_summary$L50[ix] <- quantile(sep, prob = 0.25)
    pred_sep_summary$U50[ix] <- quantile(sep, prob = 0.75)
    pred_sep_summary$U80[ix] <- quantile(sep, prob = 0.90)
    pred_sep_summary$U95[ix] <- quantile(sep, prob = 0.975)
  }
  
  return(list(draws = draws_list, probs = probs_list, ystar = ystar_list, final_frame = final_frame_list,
              max_y_dens = max_y_dens, max_sep = max_sep, max_sep_dens = max_sep_dens, sep_dens = sep_dens_list,
              pred_sep = pred_sep_summary))
}

plot_field <- function(los, ball_land_x, ball_land_y){
  
  x_lim <- c(-27,27)
  y_lim <- 
    c(floor(los/10) * 10 - 15, floor(ball_land_x/10) * 10 + 15) - los
  
  plot(1, type = "n", xlim = x_lim, ylim = y_lim, xaxt="n", yaxt = "n",
       xlab = "", ylab = "")
  axis(side = 2, at = seq(20, 100, by = 10)-los,
       labels = c(seq(10, 50, by = 10), seq(40, 10, by = -10)),
       las = 2, tick = FALSE)
  axis(side = 4, at = seq(20, 100, by = 10)-los,
       labels = c(seq(10, 50, by = 10), seq(40, 10, by = -10)), 
       las = 2, tick = FALSE)
  
  abline(h = seq(10*floor(los/10) + 5, 105, by = 10) - los, lwd = 0.5)
  abline(h = seq(10*floor(los/10), 
                 10*floor(ball_land_x/10) * 10 + 10, by = 10) - los, 
         lwd = 1)
  
  abline(h = 0, col = oi_colors[3], lwd = 2)
}