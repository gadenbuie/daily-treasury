pkgload::load_all()

START_DATE <- Sys.Date() - (2 * 365)

fd_dts_deposits_withrawals <-
  fd_req_dts_deposits_withdrawals(fd_req(), record_date = START_DATE) |>
    fd_req_dts_perform_json(page_size = 1000) |>
    fd_resps_combine_json()

readr::write_rds(
  fd_dts_deposits_withrawals,
  here::here("data/fd_dts_deposits_withrawals.rds")
)
