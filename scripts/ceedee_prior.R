library(ranger)
load("data/raw_data.RData")
load("data/rf_fit.RData")
source("scripts/functions.R")

gid <- 2023110510
pid <- 744
week <- 9


paths <-
  raw_path |>
  dplyr::filter(game_id == gid & play_id == pid)



post0 <- get_posterior_params(paths = paths, inform_mean = FALSE, init_noise = c(0,0,0,0))
post1 <- get_posterior_params(paths = paths, inform_mean = TRUE, init_noise = c(0,0,0,0))

draw0 <- draw_paths(post = post0, paths = paths, n_draws = 500)
draw1 <- draw_paths(post = post1, paths = paths, n_draws = 500)

y_lim <- c(0, 54)

oi_colors <- palette.colors(palette = "Okabe-Ito")

ix <- paths$num_frames_input[1] + 1


png("figures/ceedee_prior.png", width = 8, height = 6, units = "in", res = 400)
par(mar = c(3,1,3,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", 
     xlim = c(70,110), 
     ylim = y_lim,
     xlab="", xaxt = "n", yaxt = "n", ylab = "", 
     main = "Prior Influence")
axis(side = 1, at = seq(20, 100, by = 10),
     labels = c(seq(10, 50, by = 10), seq(40, 10, by = -10)))

#abline(v = seq(15, 105, by = 10), lwd=0.5)
#abline(v = seq(10, 110, by = 10), lwd = 1.1)
abline(v = seq(10*floor(paths$los[1]/10) - 5, 105, by = 10), lwd = 0.5)
abline(v = seq(10*floor(paths$los[1]/10), 10*floor(paths$ball_land_x[1]/10 * 10 + 10), by = 10))
#abline(v = seq(10*floor(los/10) + 5, 105, by = 10), lwd = 0.5)
abline(v = paths$los[1], col = oi_colors[6], lwd = 2)

n_draws <- 500
n <- nrow(paths)
for(i in 1:n_draws){

  lines(c(paths$rec_x[ix],draw0$draws[[ix]][["rec_x"]][i,]),
        c(paths$rec_y[ix],draw0$draws[[ix]][["rec_y"]][i,]),
        col = adjustcolor(oi_colors[8], alpha.f = 0.3), lwd = 0.1)
  lines(c(paths$rec_x[ix],draw1$draws[[ix]][["rec_x"]][i,]),
        c(paths$rec_y[ix],draw1$draws[[ix]][["rec_y"]][i,]),
        col = adjustcolor(oi_colors[4], alpha.f = 0.3), lwd = 0.1)
}
points(paths$rec_x[1:ix], paths$rec_y[1:ix], pch = 16, cex = 0.8)
points(paths$rec_x[(ix+1):n], paths$rec_y[(ix+1):n], pch = 1, cex = 0.8)

lines(x = c(paths$rec_x[1], paths$ball_land_x[1]), y = c(paths$rec_y[1], paths$ball_land_y[1]), lty = 2, col = oi_colors[4], lwd = 2)
points(x = paths$rec_x[1], paths$rec_y[1], pch = 16, col = oi_colors[8])
points(x = paths$ball_land_x[1], paths$ball_land_y[1], pch = 4, col = oi_colors[8])

legend("bottomright", legend = c("Observed Trajectory", "Remaining Trajectory", "Ball Landing", "Receiver Start"), pch = c(16, 1, 4, 16), col = oi_colors[c(1,1,8, 8)])
legend("topright", legend = c("Simulations w/ Zero-Mean Prior", "Our Prior Mean", "Simulations w/ Our Model"), lty = c(1,1), lwd = 2, col = oi_colors[c(8,4, 4)])
dev.off()
