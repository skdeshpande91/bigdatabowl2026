library(ranger)
load("data/rf_fit.RData")
source("scripts/functions.R")
load("data/ceedee_path2.RData")

n <- nrow(paths)


n_frames_in <- paths$num_frames_input[1]
n_frames_out <- paths$num_frames_output[1]


ball_start_x <- paths$qb_x[n_frames_in]
ball_start_y <- paths$qb_y[n_frames_in]

ball_land_x <- paths$ball_land_x[1]
ball_land_y <- paths$ball_land_y[1]

ball_path_x <- ball_start_x + (ball_land_x - ball_start_x)/n_frames_out * seq(0, n_frames_out, length = n_frames_out)
ball_path_y <- ball_start_y + (ball_land_y - ball_start_y)/n_frames_out * seq(0, n_frames_out, length = n_frames_out)

los <- paths$los[1]

post <- get_posterior_params(paths, inform_mean = TRUE,
                             rec_target = c(97.42, 38.34),
                             def_target = c(97, 38))

n_draws <- 1000
set.seed(4)
test <- draw_paths(post = post, paths = paths, n_draws = n_draws)

save(post, test, file = "data/ceedee2_draws.RData")



############################
# Full animation
############################

for(ix in 1:(n-1)){
  write_ix <- ix
  if(ix < 10) write_ix <- paste0("0",ix)
  png(paste0("ceedee2/paths_", write_ix, ".png"), width = 6, height = 6, units = "in", res = 400)
  par(mar = c(1,3,1,3), mgp = c(1.8, 0.5, 0))
  plot_field(los = los, ball_land_x = ball_land_x, ball_land_y = ball_land_y)
  for(i in 1:n_draws){
    
    lines(
      x = 27-c(paths$rec_y[ix], test$draws[[ix]][["rec_y"]][i,]),
      y = c(paths$rec_x[ix], test$draws[[ix]][["rec_x"]][i,]) - los,
      col = adjustcolor(oi_colors[6], alpha.f = 0.1), lwd = 0.1)
    
    lines(
      x = 27-c(paths$def_y[ix], test$draws[[ix]][["def_y"]][i,]),
      y = c(paths$def_x[ix], test$draws[[ix]][["def_x"]][i,]) - los,
      col = adjustcolor(oi_colors[4], alpha.f = 0.1), lwd = 0.1)
  }
  points(27-paths$rec_y[1:ix], paths$rec_x[1:ix]-los, pch = 16, col = oi_colors[6], cex = 0.8)
  points(27-paths$def_y[1:ix], paths$def_x[1:ix]-los, pch = 16, col = oi_colors[4], cex = 0.8)
  
  points(27-paths$rec_y[ix], paths$rec_x[ix]-los, pch = 16, col = oi_colors[6], cex = 1)
  points(27-paths$def_y[ix], paths$def_x[ix]-los, pch = 16, col = oi_colors[4], cex = 1)
  
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
         legend = c("C. Lamb", "E. Ricks", "D. Prescott", "Ball"),
         pch = c(16, 16, 16, 15),
         col = oi_colors[c(6,4,1,8)])
  dev.off()
}

y_dens_lim <- max(test$max_y_dens[-(n-1)])
for(ix in 1:(n-1)){
  probs <- table(test$ystar[ix])/n_draws
  write_ix <- ix
  if(ix < 10) write_ix <- paste0("0",ix)
  png(paste0("ceedee2/probs_", write_ix, ".png"), width = 8, height = 4.5, units = "in", res = 400)
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
  

###################################



###########################
# Freeze Frame Animation
###########################
# At Frame 19: animation of drawing a few paths
ix <- 19

png("figures/ceedee_frame19.png", width = 6, height = 6, units = "in", res = 400)
par(mar = c(1,3,1,3), mgp = c(1.8, 0.5, 0))

x_lim <- c(-27,27)
y_lim <- 
  c(floor(paths$rec_x[ix]/5) * 5, 120) - los
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
for(i in 2:2){
  
  lines(
    x = 27-c(paths$rec_y[ix], test$draws[[ix]][["rec_y"]][i,]),
    y = c(paths$rec_x[ix], test$draws[[ix]][["rec_x"]][i,]) - los,
    col = adjustcolor(oi_colors[6], alpha.f = 0.5), lwd = 1)
  
  lines(
    x = 27-c(paths$def_y[ix], test$draws[[ix]][["def_y"]][i,]),
    y = c(paths$def_x[ix], test$draws[[ix]][["def_x"]][i,]) - los,
    col = adjustcolor(oi_colors[4], alpha.f = 0.5), lwd = 1)
}

points(27-paths$rec_y[ix], paths$rec_x[ix]-los, pch = 16, col = oi_colors[6], cex = 1)
points(27-paths$def_y[ix], paths$def_x[ix]-los, pch = 16, col = oi_colors[4], cex = 1)


lines(x = 27 - c(test$draws[[ix]][["rec_y"]][i,n-ix], test$draws[[ix]][["def_y"]][i,n-ix]),
      y = c(test$draws[[ix]][["rec_x"]][i,n-ix], test$draws[[ix]][["def_x"]][i,n-ix]) - los,
      lty = 2, col = oi_colors[2], lwd = 2)

lines(x = 27 - c(test$draws[[ix]][["rec_y"]][i,n-ix], test$draws[[ix]][["rec_y"]][i,n-ix]),
      y = c(test$draws[[ix]][["rec_x"]][i,n-ix], 110) - los,
      lty = 2, col = oi_colors[8], lwd = 2)
lines(x = c(par("usr")[1], 27-test$draws[[ix]][["rec_y"]][i,n-ix]),
      y = c(test$draws[[ix]][["rec_x"]][i,n-ix], test$draws[[ix]][["rec_x"]][i,n-ix]) - los,
      lty = 2, col = oi_colors[3], lwd = 2)

points(27-test$draws[[ix]][["rec_y"]][i,n-ix],
       test$draws[[ix]][["rec_x"]][i,n-ix]-los,
       pch = 16, col = oi_colors[6], cex = 1)
points(27-test$draws[[ix]][["def_y"]][i,n-ix],
       test$draws[[ix]][["def_x"]][i,n-ix]-los,
       pch = 16, col = oi_colors[4], cex = 1)

pred19 <- predict(rf_fit, test$final_frame[[ix]][i,])$predictions

legend("topright",
       lty = 2, lwd = 2,
       legend = c(
         paste0("sep: ", round(test$final_frame[[ix]]$def2rec[i], digits = 2)),
         paste0("rec2goal: ", round(test$final_frame[[ix]]$rec2goal[i], digits = 2)),
         paste0("rec2side: ", round(test$final_frame[[ix]]$rec2side[i], digits = 2))),
       col = oi_colors[c(2,8,3)])
legend("bottomright", cex = 1,
       legend = c(
         paste0("P(C) = ", round(pred19[1,"C"], digits = 3)),
         paste0("P(I) = ", round(pred19[1,"I"], digits = 3)),
         paste0("P(IN) = ", round(pred19[1,"IN"], digits = 3))))
dev.off()

#

png("ceedee_frame19/ceedee_dens.png", width = 9, height = 6, units = "in", res = 400)
par(mfrow = c(1,2))

par(mar = c(1,3,1,3), mgp = c(1.8, 0.5, 0))

x_lim <- c(-27,27)
y_lim <- 
  c(floor(paths$rec_x[ix]/5) * 5, 120) - los
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
for(i in 1:n_draws){
  lines(
    x = 27-c(paths$rec_y[ix], test$draws[[ix]][["rec_y"]][i,]),
    y = c(paths$rec_x[ix], test$draws[[ix]][["rec_x"]][i,]) - los,
    col = adjustcolor(oi_colors[6], alpha.f = 0.1), lwd = 0.1)
  
  lines(
    x = 27-c(paths$def_y[ix], test$draws[[ix]][["def_y"]][i,]),
    y = c(paths$def_x[ix], test$draws[[ix]][["def_x"]][i,]) - los,
    col = adjustcolor(oi_colors[4], alpha.f = 0.1), lwd = 0.1)
}

points(27-paths$rec_y[ix], paths$rec_x[ix]-los, pch = 16, col = oi_colors[6], cex = 1)
points(27-paths$def_y[ix], paths$def_x[ix]-los, pch = 16, col = oi_colors[4], cex = 1)

probs <- table(test$ystar[ix])/n_draws
legend("bottomright", cex = 1,
       legend = c(
         paste0("P(C) = ", round(probs["C"], digits = 3)),
         paste0("P(I) = ", round(probs["I"], digits = 3)),
         paste0("P(IN) = ", round(probs["IN"], digits = 3))))

y_dens_lim <- max(test$max_y_dens[-(n-1)])
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = c(0,1), ylim = c(0, y_dens_lim), 
     main = "Event Probabilities", 
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

legend("topright", horiz = FALSE, pch = c(15, 15, 15), cex = 1, 
       col = adjustcolor(oi_colors[c(4,7,6)], alpha.f = 0.6),
       legend = c("Completion", "Incompletion", "Interception"))
dev.off()



### Vertical:
# new_X = -Y + 27
# new_Y = X - los
# (0,54) --> (-27,-27)
# (0,120) --> (-los, 120-los)

oi_colors <- palette.colors(palette = "Okabe-Ito")
par(mar = c(1,3,1,3), mgp = c(1.8, 0.5, 0))
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

ix <- 19

