#' Log in to Amazon
#' 
#' Launch a Selenium session in a Chrome browser, attach cookies to it to bypass 2FA, and log in.
#'
#' @return Session object is stored in the global environment.
#' @export
#'
#' @examples
#' \dontrun{
#' get_in()
#' }
get_in <- function() {
  # Read in cookies
  cookies <- 
    readr::read_csv(
      "data-raw/cookies.csv",
      col_types = 
        readr::cols(
          name = readr::col_character(),
          value = readr::col_character()
        )
    )

  message("Launching session.")
  sess <<-
    start_session(url)
  
  Sys.sleep(1)

  sess %>%
    add_cookies(cookie_tbl = cookies)

  Sys.sleep(1)

  message("Inputting email.")
  sess %>%
    input_text("id", email_id, email)

  Sys.sleep(1)

  sess %>%
    click("id", "continue")

  Sys.sleep(2)

  auth(sess)

  Sys.sleep(3)
}

#' Check out
#'
#' @param cart One of \code{"whole foods"} or \code{"amazon fresh"}.
#'
#' @return A session object
#' @export
#'
#' @examples
#' \dontrun{
#' check_out("amazon fresh")
#' }
check_out <- function(cart = "amazon fresh") {
  checkout_button <-
    switch(cart,
      "whole foods" = checkout_name_wf,
      "amazon fresh" = checkout_name_af
    )
  
  if (checkif_got_captchad(sess)) {
    message("Got captcha'd :( \n Wait a bit and try again.")
    return(invisible())
  }

  message("Clicking the cart button.")
  sess %>%
    click("id", cart_id)

  Sys.sleep(1)

  glue_message("Selecting {stringr::str_to_title(cart)} checkout.")
  sess %>%
    click("name", checkout_button)

  Sys.sleep(2)

  continue_sequence(sess)
  
  if (!checkif_on_schedule_order_page(sess)) {
    message("Trying continue sequence again.")
    continue_sequence()
  }

  sess
}


#' Click the earliest available time
#'
#' If no times are available, keep refreshing.
#'
#' @param n_tries Number of times to try refreshing before giving up.
#' @param timeout_after Number of seconds to try refreshing before giving up.
#' @param end_earlier If true, the loop will end after either \code{n_tries} is reached or \code{timeout_after} is reached. If false, both conditions must be met.
#' @param sleep_time Average number of seconds to sleep in between refreshes. Exact sleep time is randomly chosen from a uniform distribution of \code{sleep_time - sleep_time/2} to \code{sleep_time + sleep_time/2}.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' buy(sleep_time = 2)
#' }
buy <- function(n_tries = 100, 
                timeout_after = 30 * 60, 
                end_earlier = TRUE, 
                sleep_time = 1) {
  if (!checkif_on_schedule_order_page(sess)) {
    return(invisible())
  }

  `%*%` <- if (end_earlier) `&&` else `||`

  start_time <- lubridate::now()
  try_num <- 1
  elapsed_time <- (lubridate::now() - start_time) %>% round(0)

  while ((try_num < n_tries) %*% (elapsed_time < timeout_after)) {
    raw <-
      sess %>%
      extract_html()

    need_to_auth <-
      raw %>%
      rvest::html_nodes("h1") %>%
      rvest::html_text() %>%
      stringr::str_detect(need_to_auth_text) %>%
      any()

    if (need_to_auth) auth()

    glue_message("On try {try_num} of {n_tries} and {elapsed_time} seconds of {timeout_after}.")

    available_windows <-
      raw %>%
      rvest::html_nodes("h4") %>%
      rvest::html_text() %>%
      stringr::str_detect(available_windows_text) %>%
      any()

    if (available_windows) {
      message("Selecting the first available window.")
      sess %>%
        click("class", select_price_class)

      message("Continuing.")
      sess %>%
        click("class", continue_class)

      message("More continuing.")
      sess %>%
        click("id", continue_top_id)

      message("Placing order.")
      sess %>%
        click("name", place_order_name)

      raw <-
        sess %>%
        extract_html()

      success <-
        raw %>%
        rvest::html_nodes("h2") %>%
        rvest::html_text() %>%
        clean_html() %>%
        stringr::str_detect(success_text) %>%
        any()

      if (success) {
        message("Yay!")
        return(invisible())
      }
    }
    Sys.sleep(runif(1, sleep_time - sleep_time / 2, sleep_time + sleep_time / 2))

    seleniumPipes::refresh(sess)
    try_num %<>% +1
    elapsed_time <- (lubridate::now() - start_time) %>% round(0)
  }
}

#' Do the thing
#'
#' @param cart One of \code{"whole foods"} or \code{"amazon fresh"}.
#' @param ... Further args to \code{\link{buy}}.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' run
#' }
run <- function(cart = "amazon fresh", ...) {
  get_in()
  check_out(cart = cart)
  buy(...)
}
