fd_req <- function() {
  httr2::request(
    "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/"
  )
}

fd_req_dts <- function(
  req = fd_req(),
  endpoint = "deposits_withdrawals_operating_cash"
) {
  # https://fiscaldata.treasury.gov/datasets/daily-treasury-statement/deposits-and-withdrawals-of-operating-cash
  req |>
    httr2::req_url_path_append("/v1/accounting/dts/") |>
    httr2::req_url_path_append(endpoint)
}

fd_req_dts_deposits_withdrawals <- function(
  req = fd_req(),
  record_date = Sys.Date() - 7,
  record_op = c("gte", "gt", "lt", "lte", 'eq'),
  format = c("csv", "json", "xml")
) {
  format <- rlang::arg_match(format)
  record_op <- rlang::arg_match(record_op)

  req |>
    fd_req_dts("deposits_withdrawals_operating_cash") |>
    httr2::req_url_query(
      # filter=record_date:eq:2025-01-31
      filter = sprintf("record_date:%s:%s", record_op, record_date),
      format = format
    )
}

fd_req_dts_perform_json <- function(req, page_size = 250, max_reqs = Inf) {
  req <- httr2::req_url_query(req, "page[size]" = page_size, format = "json")

  resps <- httr2::req_perform_iterative(
    req,
    max_reqs = max_reqs,
    on_error = "return",
    next_req = function(resp, req) {
      if (httr2::resp_is_error(resp)) {
        rlang::warn("Incomplete data!", parent = resp)
        return()
      }

      body <- httr2::resp_body_json(resp)
      if (is.null(body$links$`next`)) {
        return()
      }

      httr2::signal_total_pages(body$meta$`total-pages`)

      Sys.sleep(1)
      # "&page[number]=91&page[size]=1000"
      next_query <- utils::URLdecode(body$links$`next`)
      next_query <- strsplit(next_query, "[&=]")[[1]]
      next_query <- next_query[nzchar(next_query)]

      next_query <- rlang::set_names(
        next_query[c(2, 4)],
        next_query[c(1, 3)]
      )

      httr2::req_url_query(req, !!!next_query)
    }
  )

  resps
}

fd_resps_combine_json <- function(resps) {
  resps |>
    purrr::map(httr2::resp_body_json, simplifyDataFrame = TRUE) |>
    purrr::map_dfr("data") |>
    dplyr::as_tibble()
}

fd_req_dts_perform_csv <- function(req, page_size = 250) {
  is_complete = function(resp) {
    data <-
      resp |>
        httr2::resp_body_string() |>
        readr::read_csv(show_col_types = FALSE)
    nrow(data) < page_size
  }

  req <- httr2::req_url_query(req, "page[size]" = page_size, format = "csv")

  resps <- httr2::req_perform_iterative(
    req,
    next_req = httr2::iterate_with_offset(
      "page[number]",
      resp_complete = is_complete
    )
  )

  resps |>
    purrr::map(httr2::resp_body_string) |>
    purrr::reduce(.init = "", function(acc, x) {
      if (acc == "") {
        return(x)
      }
      n_pos <- regexpr("\n", x)
      x <- substring(x, n_pos + 1)
      paste0(acc, x)
    }) |>
    readr::read_csv(show_col_types = FALSE)
}
