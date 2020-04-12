library(bonanza)

url <- "https://www.amazon.com/ap/signin?_encoding=UTF8&ignoreAuthState=1&openid.assoc_handle=usflex&openid.claimed_id=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.identity=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.mode=checkid_setup&openid.ns=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0&openid.ns.pape=http%3A%2F%2Fspecs.openid.net%2Fextensions%2Fpape%2F1.0&openid.pape.max_auth_age=0&openid.return_to=https%3A%2F%2Fwww.amazon.com%2F%3Fref_%3Dnav_signin&switch_account="

email_id <- "ap_email"
password_id <- "ap_password"
pass <- Sys.getenv("AMAZON_PASS")
sign_in_id <- "signInSubmit"
cart_id <- "nav-cart-count"
checkout_name <- "proceedToALMCheckout-VUZHIFdob2xlIEZvb2Rz"
proceed_to_checkout_name <- "proceedToCheckout"
continue_class <- "a-button-input"
continue_top_id <- "continue-top"
# select_free_class <- "a-button-text ufss-slot-toggle-native-button"
select_price_class <- "ufss-slot-price-container"
place_order_name <- "placeYourOrder1"
available_windows_text <- "Next available"
success_text <- "Thank you, your Whole Foods Market order has been placed."

browse.launch_session <- function(
  url = "https://democrats.org",
  port = 4444L,
  server_name = ".deck_headless",
  server_environment = .GlobalEnv,
  browser = "chrome",
  download_folder = NA, # Only applicable for Chrome
  use_version = "lowest",
  ...) {
  browse.close_session(server_environment = server_environment)
  
  # Make sure the port is valid
  if (port == 4444L) {
    while (any(!is.na(pingr::ping_port("localhost", port)))) {
      port %<>% add(1) %>% as.integer()
    }
  }
  
  # Create simulated display if needed
  if (!sys.is_osx() & browser == "chrome") {
    display_pids <- sys.cmd("pgrep -f vfb") %>% .[. %in% sys.cmd("pgrep -f vfb")]
    if (length(display_pids) >= 1) for (pid in display_pids) sys.cmd(paste("kill -9", pid))
    system("Xvfb :99 -ac", wait = FALSE)
    Sys.setenv("DISPLAY" = ":99")
  }
  
  # Start a PhantomJS driver accessible from the global environment
  assign(
    server_name,
    if (browser == "phantomjs") {
      wdman::phantomjs(port = as.integer(port), ...)
    } else
      if (browser == "chrome") {
        wdman:::chrome_check(verbose = T)
        
        os <-
          ifelse(
            Sys.info()[["sysname"]] == "Darwin",
            "mac64", "linux64"
          )
        
        versions <- binman::list_versions("chromedriver")[[os]]
        
        if (use_version == "lowest") {
          v_idx <- 1
        } else if (use_version == "highest") {
          v_idx <- length(versions)
        } else if (is.numeric(use_version)) {
          v_idx <- use_version
        } else {
          stop("use_version must be numeric or one of: 'lowest', 'highest'.")
        }
        
        wdman::chrome(
          port = as.integer(port),
          version = versions[v_idx],
          ...
        )
      } else {
        stop("Invalid browser.")
      },
    envir = server_environment
  )
  
  # Browse to the given URL on the PhantomJS driver
  site <-
    seleniumPipes::remoteDr(
      browserName = browser,
      port = as.integer(port),
      extraCapabilities = list(
        chromeOptions = list(
          args = list("--no-sandbox"),
          prefs = list(
            "profile.default_content_settings.popups" = 0L,
            "download.prompt_for_download" = FALSE,
            "download.default_directory" = if (is.na(download_folder)) getwd() else download_folder
          )
        )
      )
    ) %>%
    seleniumPipes::go(url) %>% 
    seleniumPipes::addCookie(name = "session-id", value = "135-9942881-1066911") %>% 
    seleniumPipes::addCookie(name = "session-time", value = "2082787201l") %>% 
    seleniumPipes::addCookie(name = "session-token", value = "bOjytyUSh5D99jpWDknCwlWgDkZx8coOGn/dK2cGpmbAgg7Dki5kIrjOIlCPqe4py3rVKqDjCrJ5TNmNZuaex063xkwRJLd9w4ZPncMNTyxoiwligCV0CuLaFM3/01AHbTbExwh1AOa0ROEaan+z4vg9x74/Bosxy345OPEboJer0cdgxUrfR0q3mns0JmBe2y7mEjnMqBfgDgexg7Re9ldGqWTEdsrkQwrCFV6s8522wuLmvHNZz2YhHc/3kiqKo2Sr6xQxSg1BTV1HTEY7jg==") %>% 
    seleniumPipes::addCookie(name = "ubid-main", value = "132-9234009-4556454")
  return(site)
}

browse.start_session <- function(url, browser = "chrome", port = 4444L) {
  browse.close_session()

  # Make sure the port is valid
  if (port == 4444L) {
    while (any(!is.na(pingr::ping_port("localhost", port)))) {
      port %<>% add(1) %>% as.integer()
    }
  }

  # Create simulated display if needed
  if (!sys.is_osx() & browser == "chrome") {
    display_pids <- sys.cmd("pgrep -f vfb") %>% .[. %in% sys.cmd("pgrep -f vfb")]
    if (length(display_pids) >= 1) for (pid in display_pids) sys.cmd(paste("kill -9", pid))
    system("Xvfb :99 -ac", wait = FALSE)
    Sys.setenv("DISPLAY" = ":99")
  }

  sess <- seleniumPipes::remoteDr(browserName = "chrome", port = port) %>%
    seleniumPipes::go(url) %>%
    seleniumPipes::addCookie(name = "session-id", value = "135-9942881-1066911") %>%
    seleniumPipes::addCookie(name = "session-time", value = "2082787201l") %>%
    seleniumPipes::addCookie(name = "session-token", value = "bOjytyUSh5D99jpWDknCwlWgDkZx8coOGn/dK2cGpmbAgg7Dki5kIrjOIlCPqe4py3rVKqDjCrJ5TNmNZuaex063xkwRJLd9w4ZPncMNTyxoiwligCV0CuLaFM3/01AHbTbExwh1AOa0ROEaan+z4vg9x74/Bosxy345OPEboJer0cdgxUrfR0q3mns0JmBe2y7mEjnMqBfgDgexg7Re9ldGqWTEdsrkQwrCFV6s8522wuLmvHNZz2YhHc/3kiqKo2Sr6xQxSg1BTV1HTEY7jg==") %>%
    seleniumPipes::addCookie(name = "ubid-main", value = "132-9234009-4556454")
  sess
}

auth <- function(sess) {
  # Input password
  sess %>% 
    browse.send_text("id", password_id, pass)
  
  sess %>% 
    browse.click_element("id", sign_in_id)
}

sess <- browse.start_session(url)

sess %>% 
  browse.send_text("id", email_id, "amanda.e.dobbyn@gmail.com")

sess %>% 
  browse.click_element("id", "continue")

Sys.sleep(2)

auth(sess)

# Assume we can get past 2FA

sess %>% 
  browse.click_element("id", cart_id)

Sys.sleep(2)

sess %>% 
  browse.click_element("name", checkout_name)

# Skip the "before you checkout do you want to add stuff"
sess %>% 
  browse.click_element("name", proceed_to_checkout_name)

# Skip the note about subsitutions
sess %>% 
  browse.click_element("class", continue_class)

hit_window <- function(n_tries = 100, timeout_after = 30*60, and = TRUE) {
  `%*%` <- if (and) `&&` else `||`
  
  start_time <- lubridate::now()
  try_num <- 1
  elapsed_time <- (lubridate::now() - start_time) %>% round(0)
  
  while ((try_num < n_tries) %*% (elapsed_time < timeout_after)) {
    
    raw <- 
      sess %>% 
      browse.extract_html() 
    
    need_to_auth <- 
      raw %>% 
      rvest::html_nodes("h1") %>% 
      rvest::html_text() %>% 
      stringr::str_detect("Sign-In") %>% 
      any()
    
    if (need_to_auth) auth()

    dev.glue_message("On try {try_num} of {n_tries} and {elapsed_time} seconds of {timeout_after}.")
    
    raw <- 
      sess %>% 
      browse.extract_html() 
    
    available_windows <- 
      raw %>% 
      rvest::html_nodes("h4") %>% 
      rvest::html_text() %>% 
      stringr::str_detect(available_windows_text) %>% 
      any()
    
    if (available_windows) {
      # Try and hit a free slot button
      sess %>% 
        browse.click_element("class", select_price_class)
      
      sess %>% 
        browse.click_element("class", continue_class)
      
      sess %>% 
        browse.click_element("id", continue_top_id)
      
      sess %>% 
        browse.click_element("name", place_order_name)
      
      raw <- 
        sess %>% 
        browse.extract_html()
      
      success <- 
        raw %>% 
        rvest::html_nodes("h2") %>% 
        rvest::html_text() %>% 
        wrangle.clean_html() %>% 
        stringr::str_detect(success_text) %>% 
        any()
      
      if (success) {
        message("Yay!")
        return(invisible())
      } 
    } 
    seleniumPipes::refresh(sess)
    try_num %<>% +1
    elapsed_time <- (lubridate::now() - start_time) %>% round(0)
  }
}

hit_window()
