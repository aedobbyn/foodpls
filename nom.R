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

browse.start_session <- function(url, browser = "chrome", port = 4444L) {
  browse.close_session()

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
  message("Inputting password.")
  # Input password
  sess %>% 
    browse.send_text("id", password_id, pass)
  
  Sys.sleep(1)
  
  message("Hitting continue.")
  sess %>% 
    browse.click_element("id", sign_in_id)
}

sess <- browse.start_session(url)

Sys.sleep(1)

sess %>% 
  browse.send_text("id", email_id, "amanda.e.dobbyn@gmail.com")

Sys.sleep(1)

sess %>% 
  browse.click_element("id", "continue")

Sys.sleep(2)

auth(sess)

Sys.sleep(1)

sess %>% 
  browse.click_element("id", cart_id)

Sys.sleep(1)

sess %>% 
  browse.click_element("name", checkout_name)

Sys.sleep(2)

# Skip the "before you checkout do you want to add stuff"
sess %>% 
  browse.click_element("name", proceed_to_checkout_name)

Sys.sleep(1)

# Skip the note about subsitutions
sess %>% 
  browse.click_element("class", continue_class)

Sys.sleep(1)

check_on_schedule_order_page <- function(sess) {
  on_correct_page <- 
    sess %>% 
      browse.extract_html() %>% 
      rvest::html_nodes("h1") %>% 
      rvest::html_text() %>% 
      stringr::str_detect("Schedule your order") %>% 
      any()
  
  if (!on_correct_page) message("Didn't make it to Schedule Order page.")
  
  on_correct_page
}

hit_window <- function(n_tries = 100, timeout_after = 30*60, and = TRUE) {
  if (!check_on_schedule_order_page(sess)) return(invisible())
  
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
    Sys.sleep(runif(1, 0.5, 1.5))
    
    seleniumPipes::refresh(sess)
    try_num %<>% +1
    elapsed_time <- (lubridate::now() - start_time) %>% round(0)
  }
}

hit_window()
