supplement <- readr::read_csv(file = "supplementary_data.csv")



# pass_length, dropback_distance, 

path_list <- list()
ff_list <- list()

for(wk in 1:18){
  if(wk < 10){
    wk_char <- paste0("0", wk)
  } else {
    wk_char <- wk
  }
  
  raw_in <- 
    readr::read_csv(file = paste0("train/input_2023_w", wk_char, ".csv"))
  rec_in <-
    raw_in |>
    dplyr::filter(player_role == "Targeted Receiver") |>
    dplyr::rename(rec_id = nfl_id, rec_x = x, rec_y = y) |>
    dplyr::select(game_id, play_id, frame_id, rec_id, rec_x, rec_y)
  
  qb_in <-
    raw_in |>
    dplyr::filter(player_role == "Passer") |>
    dplyr::rename(qb_id = nfl_id, qb_x = x, qb_y = y, qb_s = s)
  last_qb_in <-
    qb_in |>
    dplyr::group_by(game_id, play_id) |>
    dplyr::slice_max(frame_id) |>
    dplyr::ungroup() |>
    dplyr::select(game_id, play_id, qb_s) |>
    dplyr::rename(last_qb_s = qb_s)
  
  context <-
    raw_in |>
    dplyr::select(game_id, play_id, absolute_yardline_number, play_direction, ball_land_x, ball_land_y, num_frames_output) |>
    unique()
  
  n_frame_input <-
    raw_in |>
    dplyr::filter(player_role == "Targeted Receiver") |>
    dplyr::group_by(game_id, play_id) |>
    dplyr::summarise(num_frames_input = dplyr::n(), .groups = "drop")
  
  roles <-
    raw_in |>
    dplyr::filter(player_to_predict) |>
    dplyr::select(game_id, play_id, nfl_id, player_role) |>
    unique()
  
  raw_out <-
    readr::read_csv(file = paste0("train/output_2023_w", wk_char, ".csv")) |>
    dplyr::inner_join(y = roles, by = c("game_id", "play_id", "nfl_id")) |>
    dplyr::inner_join(y = n_frame_input, by = c("game_id", "play_id"))
  
  rec_out <-
    raw_out |>
    dplyr::filter(player_role == "Targeted Receiver") |>
    dplyr::select(game_id, play_id, frame_id, nfl_id, x, y) |>
    dplyr::rename(rec_x = x, rec_y = y, rec_id = nfl_id)
  
  
  nearest_def <-
    raw_out |>
    dplyr::inner_join(y = rec_out, by = c("game_id", "play_id", "frame_id")) |>
    dplyr::mutate(rec_sep = sqrt ( (x - rec_x)^2 + (y - rec_y)^2 )) |>
    dplyr::filter(player_role == "Defensive Coverage") |>
    dplyr::group_by(game_id, play_id) |>
    dplyr::slice_max(frame_id) |>
    dplyr::slice_min(rec_sep) |>
    dplyr::select(game_id, play_id, nfl_id) |>
    dplyr::rename(def_id = nfl_id)
  
  path_in <-
    raw_in |>
    dplyr::inner_join(y = rec_in, by = c("game_id", "play_id", "frame_id")) |>
    dplyr::inner_join(y = nearest_def, by = c("game_id", "play_id")) |>
    dplyr::inner_join(y = qb_in, by = c("game_id", "play_id", "frame_id")) |>
    dplyr::filter(nfl_id == def_id) |>
    dplyr::rename(def_x = x, def_y = y) |>
    dplyr::select(game_id, play_id, frame_id, rec_id, rec_x, rec_y, def_id, def_x, def_y, qb_id, qb_x, qb_y, qb_s)
  
  
  path_out <-
    raw_out |>
    dplyr::inner_join(y = rec_out, by = c("game_id", "play_id", "frame_id")) |> 
    dplyr::inner_join(y = nearest_def, by = c("game_id", "play_id")) |>
    dplyr::filter(nfl_id == def_id) |>
    dplyr::mutate(frame_id = frame_id + num_frames_input) |>
    dplyr::rename(def_x = x, def_y = y) |>
    dplyr::mutate(qb_id = NA, qb_x = NA, qb_y = NA, qb_s = NA) |>
    dplyr::select(game_id, play_id, frame_id, rec_id, rec_x, rec_y, def_id, def_x, def_y, qb_id, qb_x, qb_y, qb_s)
  
  path <- 
    dplyr::bind_rows(path_in, path_out) |>
    dplyr::inner_join(y = context, by = c("game_id", "play_id")) |>
    dplyr::inner_join(y = n_frame_input, by = c("game_id", "play_id")) |>
    dplyr::arrange(game_id, play_id, frame_id) |>
    dplyr::inner_join(y = supplement |> dplyr::select(game_id, play_id, pass_result, pass_length, dropback_distance), by = c("game_id", "play_id")) |>
    dplyr::mutate(
      rec_x = ifelse(play_direction == "right", rec_x, 120-rec_x),
      rec_y = ifelse(play_direction == "right", rec_y, 54-rec_y),
      def_x = ifelse(play_direction == "right", def_x, 120-def_x),
      def_y = ifelse(play_direction == "right", def_y, 54-def_y),
      los = ifelse(play_direction == "right", absolute_yardline_number, 120 - absolute_yardline_number),
      ball_land_x = ifelse(play_direction == "right", ball_land_x, 120-ball_land_x),
      ball_land_y = ifelse(play_direction == "right", ball_land_y, 54-ball_land_y),
      qb_x = ifelse(play_direction == "right", qb_x, 120-qb_x),
      qb_y = ifelse(play_direction == "right", qb_y, 54-qb_y)) |>
    dplyr::inner_join(y = last_qb_in, by = c("game_id", "play_id"))
  
  final_frame <-
    path |>
    dplyr::group_by(game_id, play_id) |>
    dplyr::slice_max(frame_id) |>
    dplyr::ungroup() |> 
    dplyr::mutate(
      def2ball = sqrt( (def_x - ball_land_x)^2 + (def_y - ball_land_y)^2),
      rec2ball = sqrt( (rec_x - ball_land_x)^2 + (rec_y - ball_land_y)^2),
      def2rec = sqrt( (def_x - rec_x)^2 + (def_y - rec_y)^2),
      rec2side = ifelse(rec_y <= 27, rec_y, 54-rec_y),
      def2side = ifelse(def_y <= 27, def_y, 54-def_y),
      ball2side = ifelse(ball_land_y <= 27, ball_land_y, 54-ball_land_y),
      rec2goal = 110 - rec_y,
      pass_result = factor(pass_result, levels = c("IN", "I", "C")))
  path_list[[wk]] <- path
  ff_list[[wk]] <- final_frame
}


raw_path <- dplyr::bind_rows(path_list)
raw_final_frame <- dplyr::bind_rows(ff_list)



save(raw_path, raw_final_frame, file = "raw_data.RData")
