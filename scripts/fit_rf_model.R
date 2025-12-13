library(ranger)

load("data/raw_data.RData")


final_frame <-
  raw_final_frame |>
  dplyr::select(pass_result, rec2side, rec2goal, last_qb_s, def2rec, 
                pass_length, frame_id, game_id, play_id)

set.seed(129)
rf_fit <- ranger(pass_result ~ ., 
                 data = final_frame |> dplyr::select(-game_id, -play_id),
                 probability = TRUE)
rf_preds <- rf_fit$predictions
colnames(rf_preds) <- c("prob_IN", "prob_I", "prob_C")

rf_final_frame <- dplyr::bind_cols(final_frame, rf_preds)



save(rf_final_frame, rf_fit, file = "data/rf_fit.RData")  
