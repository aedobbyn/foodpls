cookies_template <- 
  tibble::tibble(
    name = c(
      "session_id",
      "session-time",
      "session-token",
      "ubid-main"
    ),
    value = rep("", 4)
  )

readr::write_csv(cookies_template, "data-raw/cookies_template.csv")
