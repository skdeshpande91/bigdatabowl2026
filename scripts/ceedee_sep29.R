load("data/ceedee_path2.RData")
load("data/ceedee2_draws.RData")
oi_colors <- palette.colors(palette = "Okabe-Ito")


sep29 <- test$final_frame[[29]]$def2rec
final_sep <- test$final_frame[[44]]$def2rec

png("figures/ceedee_sep29.png", width = 8, height = 4.5, units = "in", res = 400)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
hist(sep29, breaks = 50, 
     main = "Forecasted Final Separation",
     xlab = "Final separation")
abline(v = final_sep, col = oi_colors[2], lwd = 2, lty = 2)
legend("topright", legend = "Actual separation", lty = 2, col = oi_colors[2], bty = "n")
dev.off()