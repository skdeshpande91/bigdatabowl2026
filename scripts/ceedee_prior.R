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



post0 <- 
  get_posterior_params(paths = paths, inform_mean = FALSE)

post1 <- get_posterior_params(paths, inform_mean = TRUE,
                             rec_target = c(97.42, 38.34),
                             def_target = c(97, 38))

draw0 <- draw_paths(post = post0, paths = paths, n_draws = 1000)
draw1 <- draw_paths(post = post1, paths = paths, n_draws = 1000)


oi_colors <- palette.colors(palette = "Okabe-Ito")

n_frames_in <- paths$num_frames_input[1]
n_frames_out <- paths$num_frames_output[1]

ix <- n_frames_in + 1


ball_start_x <- paths$qb_x[n_frames_in]
ball_start_y <- paths$qb_y[n_frames_in]

ball_land_x <- paths$ball_land_x[1]
ball_land_y <- paths$ball_land_y[1]

ball_path_x <- ball_start_x + (ball_land_x - ball_start_x)/n_frames_out * seq(0, n_frames_out, length = n_frames_out)
ball_path_y <- ball_start_y + (ball_land_y - ball_start_y)/n_frames_out * seq(0, n_frames_out, length = n_frames_out)

los <- paths$los[1]




png("figures/ceedee_prior.png", width = 6, height = 6, units = "in", res = 400)
par(mar = c(1,3,1,3), mgp = c(1.8, 0.5, 0))

plot_field(los = los , ball_land_x = ball_land_x, ball_land_y = ball_land_y)

n_draws <- 1000
n <- nrow(paths)


for(i in 1:n_draws){
  
  lines(
    x = 27-c(paths$rec_y[ix], draw0$draws[[ix]][["rec_y"]][i,]),
    y = c(paths$rec_x[ix], draw0$draws[[ix]][["rec_x"]][i,]) - los,
    col = adjustcolor(oi_colors[8], alpha.f = 0.1), lwd = 0.1)
  
  lines(
    x = 27-c(paths$rec_y[ix], draw1$draws[[ix]][["rec_y"]][i,]),
    y = c(paths$rec_x[ix], draw1$draws[[ix]][["rec_x"]][i,]) - los,
    col = adjustcolor(oi_colors[6], alpha.f = 0.1), lwd = 0.1)
}

lines(x = 27 - c(paths$rec_y[1], ball_land_y),
      y = c(paths$rec_x[1], ball_land_x) - los, 
      lty = 2, col = oi_colors[6], lwd = 2)
points(x = 27 - paths$rec_y[1], paths$rec_x[1] - los, pch = 16, col = oi_colors[8])
points(x = 27 - ball_land_y, ball_land_x - los, pch = 4, col = oi_colors[8])

points(x = 27 - paths$rec_y[1:ix], y = paths$rec_x[1:ix]-los, pch = 16, cex = 0.5)
points(x = 27-paths$rec_y[(ix+1):n], y = paths$rec_x[(ix+1):n] - los, pch =1, cex = 0.5)
legend("bottomleft", bty = "n",
       legend = 
         c("Observed Trajectory", "Remaining Trajectory", 
           "Ball Landing", "Receiver Start"), 
       pch = c(16, 1, 4, 16), col = oi_colors[c(1,1,8, 8)])
legend("bottomright", bty = "n",
       legend = c("Simulations w/ Constant mu", "Our mu", "Simulations w/ Our mu"), 
       lty = c(1,1), lwd = 2, col = oi_colors[c(8,6, 6)])
dev.off()
