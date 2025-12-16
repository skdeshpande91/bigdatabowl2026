gid <- 2023110510
pid <- 744
week <- 9

rec_id <- 52425
def_id <- 56242

supplement <- 
  readr::read_csv(file = "data/training/supplementary_data.csv") |>
  dplyr::filter(game_id==gid & play_id == pid)

raw_in <- 
  readr::read_csv(file = "data/training/input_2023_w09.csv") |>
  dplyr::filter(game_id == gid & play_id == pid)

rec_in <-
  raw_in |>
  dplyr::filter(player_role == "Targeted Receiver") |>
  dplyr::rename(rec_id = nfl_id, rec_x = x, rec_y = y) |>
  dplyr::select(game_id, play_id, frame_id, rec_id, rec_x, rec_y)


context <-
  raw_in |>
  dplyr::select(game_id, play_id, absolute_yardline_number, 
                play_direction, ball_land_x, ball_land_y, num_frames_output) |>
  unique() |>
  dplyr::mutate(num_frames_input = nrow(rec_in))

qb_in <-
  raw_in |>
  dplyr::filter(player_role == "Passer") |>
  dplyr::rename(qb_id = nfl_id, qb_x = x, qb_y = y, qb_s = s)

last_qb_in <-
  qb_in |>
  dplyr::slice_max(frame_id) |>
  dplyr::select(game_id, play_id, qb_s) |>
  dplyr::rename(last_qb_s = qb_s)

def_in <-
  raw_in |>
  dplyr::filter(nfl_id == def_id) |>
  dplyr::select(game_id, play_id, frame_id, nfl_id, x, y) |>
  dplyr::rename(def_id = nfl_id, def_x = x, def_y = y)


path_in <-
  rec_in |>
  dplyr::inner_join(def_in, by = c("game_id", "play_id", "frame_id")) |>
  dplyr::inner_join(qb_in, by = c("game_id", "play_id", "frame_id")) |>
  dplyr::select(game_id, play_id, frame_id,
                rec_id, rec_x, rec_y, 
                def_id, def_x, def_y,
                qb_id, qb_x, qb_y, qb_s)

raw_out <-
  readr::read_csv(file = "data/training/output_2023_w09.csv") |>
  dplyr::filter(game_id == gid & play_id == pid)

rec_out <- 
  raw_out |>
  dplyr::filter(nfl_id == rec_id) |>
  dplyr::select(game_id, play_id, frame_id, nfl_id, x, y) |>
  dplyr::rename(rec_id = nfl_id, rec_x = x, rec_y = y)

def_out <-
  raw_out |>
  dplyr::filter(nfl_id == def_id) |>
  dplyr::select(game_id, play_id, frame_id, nfl_id, x,y) |>
  dplyr::rename(def_id = nfl_id, def_x = x, def_y = y)

path_out <-
  rec_out |>
  dplyr::inner_join(y = def_out, by = c("game_id", "play_id", "frame_id")) |>
  dplyr::mutate(qb_id = NA, qb_x = NA, qb_y = NA, qb_s = NA) |>
  dplyr::select(game_id, play_id, frame_id,
                rec_id, rec_x, rec_y, 
                def_id, def_x, def_y,
                qb_id, qb_x, qb_y, qb_s)
  

paths <- 
  dplyr::bind_rows(path_in, path_out) |>
  dplyr::inner_join(y = context, by = c("game_id", "play_id")) |>
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

save(paths, file = "data/ceedee_path2.RData")
