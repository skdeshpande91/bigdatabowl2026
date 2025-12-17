library(ranger)
load("data/raw_data.RData")
load("data/rf_fit.RData")
source("scripts/functions.R")

gid <- 2023091710
pid <- 3860

paths <-
  raw_path |>
  dplyr::filter(game_id == gid & play_id == pid)

post <- get_posterior_params(paths = paths, sigma = 1/36, tau = 1)


n_frames_in <- paths$num_frames_input[1]
n_frames_out <- paths$num_frames_output[1]

los <- paths$los[1]

n_draws <- 1000

ball_start_x <- paths$qb_x[n_frames_in]
ball_start_y <- paths$qb_y[n_frames_in]

ball_land_x <- paths$ball_land_x[1]
ball_land_y <- paths$ball_land_y[1]

ball_path_x <- ball_start_x + (ball_land_x - ball_start_x)/n_frames_out * seq(0, n_frames_out, length = n_frames_out)
ball_path_y <- ball_start_y + (ball_land_y - ball_start_y)/n_frames_out * seq(0, n_frames_out, length = n_frames_out)

set.seed(28)
test <- draw_paths(post = post, paths = paths, n_draws = n_draws)

oi_colors <- palette.colors(palette = "Okabe-Ito")
n <- nrow(paths)



for(ix in 1:(n-1)){
  write_ix <- ix
  if(ix < 10) write_ix <- paste0("0",ix)
  png(paste0("hooker_animations/paths_", write_ix, ".png"), 
      width = 6, height = 6, units = "in", res = 400)
  par(mar = c(1,3,1,3), mgp = c(1.8, 0.5, 0))
  plot_field(los = los, ball_land_x = ball_land_x, ball_land_y = ball_land_y)
  
  for(i in 1:n_draws){
    
    lines(
      x = 27-c(paths$rec_y[ix], test$draws[[ix]][["rec_y"]][i,]),
      y = c(paths$rec_x[ix], test$draws[[ix]][["rec_x"]][i,]) - los,
      col = adjustcolor(oi_colors[4], alpha.f = 0.1), lwd = 0.1)
    
    lines(
      x = 27-c(paths$def_y[ix], test$draws[[ix]][["def_y"]][i,]),
      y = c(paths$def_x[ix], test$draws[[ix]][["def_x"]][i,]) - los,
      col = adjustcolor(oi_colors[6], alpha.f = 0.1), lwd = 0.1)
  }
  points(27-paths$rec_y[1:ix], paths$rec_x[1:ix]-los, pch = 16, col = oi_colors[4], cex = 0.8)
  points(27-paths$def_y[1:ix], paths$def_x[1:ix]-los, pch = 16, col = oi_colors[6], cex = 0.8)
  
  points(27-paths$rec_y[ix], paths$rec_x[ix]-los, pch = 16, col = oi_colors[4], cex = 1)
  points(27-paths$def_y[ix], paths$def_x[ix]-los, pch = 16, col = oi_colors[6], cex = 1)
  
  if(ix <= n_frames_in){
    lines(27-paths$qb_y[1:ix], paths$qb_x[1:ix]-los, col = oi_colors[1], lwd = 0.8)
    points(27-paths$qb_y[ix], paths$qb_x[ix]-los, pch = 16, col = oi_colors[1], cex = 1)
  } else{
    lines(27-paths$qb_y[1:n_frames_in], paths$qb_x[1:n_frames_in]-los, lwd = 0.5, col = oi_colors[1])
    points(27-paths$qb_y[n_frames_in], paths$qb_x[n_frames_in]-los, pch = 16, col = oi_colors[1], cex = 0.8)
    points(27-ball_path_y[ix-n_frames_in], ball_path_x[ix-n_frames_in]-los, pch = 18, col = oi_colors[8], cex = 1.8)
  }
  probs <- table(test$ystar[ix])/n_draws
  legend("bottomright", cex = 1, bty = "n",
         legend = c(
           paste0("P(C) = ", round(probs["C"], digits = 3)),
           paste0("P(I) = ", round(probs["I"], digits = 3)),
           paste0("P(IN) = ", round(probs["IN"], digits = 3))))
  legend("bottomleft", bty = "n", cex = 1,
         legend = c("D. Bland", "G. Wilson", "Z. Wilson", "Ball"),
         pch = c(16, 16, 16, 15),
         col = oi_colors[c(6,4,1,8)])
  dev.off()
}

for(ix in 1:(n-1)){
  probs <- table(test$ystar[ix])/n_draws
  write_ix <- ix
  y_dens_lim <- test$max_y_dens[ix]
  
  if(ix < 10) write_ix <- paste0("0",ix)
  png(paste0("hooker_animations/probs_", write_ix, ".png"), width = 8, height = 4.5, units = "in", res = 400)
  par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
  plot(1, type = "n", xlim = c(0,1), ylim = c(0, y_dens_lim), 
       main = "Forecasted Probabilities", 
       xlab = "Prob.", ylab = "Density")
  
  c_dens <- test$probs[[ix]][["c_dens"]]
  i_dens <- test$probs[[ix]][["i_dens"]]
  in_dens <- test$probs[[ix]][["in_dens"]]
  
  polygon(x = c(c_dens$x, rev(c_dens$x)),
          y = c(c_dens$y, rep(0, times = length(c_dens$y))),
          col = adjustcolor(oi_colors[4], alpha.f = 0.3), border = NA)
  polygon(x = c(i_dens$x, rev(i_dens$x)),
          y = c(i_dens$y, rep(0, times = length(i_dens$y))),
          col = adjustcolor(oi_colors[7], alpha.f = 0.3), border = NA)
  polygon(x = c(in_dens$x, rev(in_dens$x)),
          y = c(in_dens$y, rep(0, times = length(in_dens$y))),
          col = adjustcolor(oi_colors[6], alpha.f = 0.3), border = NA)
  abline(v = probs, lty = 2, lwd = 2, col = oi_colors[c(4,7,6)])
  
  legend("topleft", horiz = TRUE, pch = c(15, 15, 15), cex = 1, 
         col = adjustcolor(oi_colors[c(4,7,6)], alpha.f = 0.6),
         legend = c("Completion", "Incompletion", "Interception"))
  dev.off()
}
