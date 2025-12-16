library(ranger)
load("data/rf_fit.RData")
source("scripts/functions.R")
load("data/ceedee_path2.RData")

set.seed(88)
post <- get_posterior_params(paths = paths, init_noise = c(0,0,0,0), sigma = 1/12, tau = 1)

n_frames_in <- paths$num_frames_input[1]
n_frames_out <- paths$num_frames_output[1]


ball_start_x <- paths$qb_x[n_frames_in]
ball_start_y <- paths$qb_y[n_frames_in]

ball_land_x <- paths$ball_land_x[1]
ball_land_y <- paths$ball_land_y[1]

ball_path_x <- ball_start_x + (ball_land_x - ball_start_x)/n_frames_out * seq(0, n_frames_out, length = n_frames_out)
ball_path_y <- ball_start_y + (ball_land_y - ball_start_y)/n_frames_out * seq(0, n_frames_out, length = n_frames_out)


n_draws <- 1000
set.seed(4)
test <- draw_paths(post = post, paths = paths, n_draws = n_draws)
max_sep <- max(test$max_sep)
max_sep_dens <- max(test$max_sep_dens)
oi_colors <- palette.colors(palette = "Okabe-Ito")
n <- nrow(paths)



x_lim <- c(floor(paths$los[1]/10) * 10 - 15, floor(paths$ball_land_x[1]/10) * 10 + 15)
y_lim <- c(0, 54)

for(ix in 1:(n-1)){
  write_ix <- ix
  if(ix < 10) write_ix <- paste0("0",ix)
  png(paste0("ceedee2/paths_", write_ix, ".png"), width = 8, height = 4.5, units = "in", res = 400)
  par(mar = c(3,1,3,1), mgp = c(1.8, 0.5, 0))
  plot(1, type = "n", 
       xlim = x_lim, 
       ylim = y_lim,
       xlab="", xaxt = "n", yaxt = "n", ylab = "", main = "")
  #main = paste("Lamb Reception: Frame", ix))
  axis(side = 1, at = seq(20, 100, by = 10),
       labels = c(seq(10, 50, by = 10), seq(40, 10, by = -10)))
  
  abline(v = seq(10*floor(paths$los[1]/10) + 5, 105, by = 10), lwd = 0.5)
  abline(v = seq(10*floor(paths$los[1]/10), 10*floor(paths$ball_land_x[1]/10 * 10 + 10), by = 10))
  
  abline(v = paths$los[1], col = oi_colors[3], lwd = 2)
  
  for(i in 1:n_draws){
    
    lines(c(paths$rec_x[ix],test$draws[[ix]][["rec_x"]][i,]),
          c(paths$rec_y[ix],test$draws[[ix]][["rec_y"]][i,]),
          col = adjustcolor(oi_colors[6], alpha.f = 0.3), lwd = 0.1)
    lines(c(paths$def_x[ix],test$draws[[ix]][["def_x"]][i,]),
          c(paths$def_y[ix],test$draws[[ix]][["def_y"]][i,]),
          col = adjustcolor(oi_colors[4], alpha.f = 0.3), lwd = 0.1)
  }
  points(paths$rec_x[1:ix], paths$rec_y[1:ix], pch = 16, col = oi_colors[6], cex = 0.8)
  points(paths$def_x[1:ix], paths$def_y[1:ix], pch = 16, col = oi_colors[4], cex = 0.8)
  
  points(paths$rec_x[ix], paths$rec_y[ix], pch = 16, col = oi_colors[6], cex = 1)
  points(paths$def_x[ix], paths$def_y[ix], pch = 16, col = oi_colors[4], cex = 1)
  
  if(ix <= n_frames_in){
    lines(paths$qb_x[1:ix], paths$qb_y[1:ix], col = oi_colors[1], lwd = 0.8)
    points(paths$qb_x[ix], paths$qb_y[ix], pch = 16, col = oi_colors[1], cex = 1)
  } else{
    lines(paths$qb_x[1:n_frames_in], paths$qb_y[1:n_frames_in], lwd = 0.5, col = oi_colors[1])
    points(paths$qb_x[n_frames_in], paths$qb_y[n_frames_in], pch = 16, col = oi_colors[1], cex = 0.8)
    points(ball_path_x[ix-n_frames_in], ball_path_y[ix-n_frames_in], pch = 18, col = oi_colors[8], cex = 1.8)
  }
  
  probs <- table(test$ystar[ix])/n_draws
  legend("topleft", bty = "n", cex = 1.25,
         legend = c(
           paste0("P(C) = ", round(probs["C"], digits = 3)),
           paste0("P(I) = ", round(probs["I"], digits = 3)),
           paste0("P(IN) = ", round(probs["IN"], digits = 3))))
  legend("bottomleft", bty = "n", cex = 1.25,
         legend = c("C. Lamb", "E. Ricks", "D. Prescott", "Ball"),
         pch = c(16, 16, 16, 15),
         col = oi_colors[c(6,4,1,8)])
  dev.off()
  
  
  y_dens_lim <- max(test$max_y_dens[-(n-1)])
  png(paste0("ceedee2/probs_", write_ix, ".png"), width = 8, height = 4.5, units = "in", res = 400)
  par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
  plot(1, type = "n", xlim = c(0,1), ylim = c(0, y_dens_lim), 
       main = paste("Lamb Completion Forecasted Event Probabilities: Frame", ix), 
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
  
  legend("topleft", horiz = TRUE, pch = c(15, 15, 15), cex = 1.5, 
         col = adjustcolor(oi_colors[c(4,7,6)], alpha.f = 0.6),
         legend = c("Completion", "Incompletion", "Interception"))
  dev.off()
  
  # Separation
  png(paste0("ceedee2/sep_", write_ix, ".png"), width = 8, height = 4.5, units = "in", res = 400)
  par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
  plot(1, type = "n", xlim = c(0, n), ylim = c(0, max(test$pred_sep$U95)),
       xlab = "Frame", ylab = "Separation", main = paste("Lamb Completition Forecasted Final Separation: Frame", ix))
  abline(h = test$final_frame[[n]]$def2rec, col = oi_colors[5], lwd = 2, lty = 2)
  
  
  if(ix > 1){
    polygon(x = c(1:ix, rev(1:ix)),
            y = c(test$pred_sep$L95[1:ix], rev(test$pred_sep$U95[1:ix])),
            col = adjustcolor(oi_colors[3], alpha.f = 0.2), border = NA)
    lines(x = 1:ix, y = test$pred_sep$MEAN[1:ix], col = oi_colors[3], lwd = 2)
  }
  lines(x = c(ix,ix), y = c(test$pred_sep$L95[ix], test$pred_sep$U95[ix]), lwd = 0.8, col = oi_colors[3])
  lines(x = c(ix,ix), y = c(test$pred_sep$L80[ix], test$pred_sep$U80[ix]), lwd = 1, col = oi_colors[3])
  lines(x = c(ix,ix), y = c(test$pred_sep$L50[ix], test$pred_sep$U50[ix]), lwd = 1.2, col = oi_colors[3])
  points(x = ix, y = test$pred_sep$MEAN[ix], pch = 16, col = oi_colors[3], cex = 2)
  
  legend("topright", lty = 1, col = oi_colors[3], lwd = 2, legend = "Predicted", bty = "n", cex = 1.5)
  legend("bottomleft", lty = 2, col = oi_colors[5], lwd = 2, legend = "Actual", bty = "n", cex = 1.5)
  dev.off()
  
}
# 19, 28, 40, 42