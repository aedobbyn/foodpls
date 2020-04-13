source("utils.R")

url <- "https://www.amazon.com/ap/signin?_encoding=UTF8&ignoreAuthState=1&openid.assoc_handle=usflex&openid.claimed_id=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.identity=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.mode=checkid_setup&openid.ns=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0&openid.ns.pape=http%3A%2F%2Fspecs.openid.net%2Fextensions%2Fpape%2F1.0&openid.pape.max_auth_age=0&openid.return_to=https%3A%2F%2Fwww.amazon.com%2F%3Fref_%3Dnav_signin&switch_account="
email <- Sys.getenv("EMAIL")
pass <- Sys.getenv("AMAZON_PASS")

# Element info
email_id <- "ap_email"
password_id <- "ap_password"
sign_in_id <- "signInSubmit"
cart_id <- "nav-cart-count"
checkout_name <- "proceedToALMCheckout-VUZHIFdob2xlIEZvb2Rz"
proceed_to_checkout_name <- "proceedToCheckout"
continue_class <- "a-button-input"
continue_top_id <- "continue-top"
select_price_class <- "ufss-slot-price-container"
place_order_name <- "placeYourOrder1"

# Text
available_windows_text <- "Next available"
success_text <- "Thank you, your Whole Foods Market order has been placed."

# Read in cookies
cookies <- readr::read_csv("cookies.csv")

sess <- 
  start_session(url) 

sess %>% 
  add_cookies(cookie_tbl = cookies)

Sys.sleep(1)

sess %>% 
  input_text("id", email_id, email)

Sys.sleep(1)

sess %>% 
  click("id", "continue")

Sys.sleep(2)

auth(sess)

Sys.sleep(3)

# Click the cart button
sess %>% 
  click("id", cart_id)

Sys.sleep(1)

sess %>% 
  click("name", checkout_name)

Sys.sleep(2)

# Skip the "before you checkout do you want to add stuff"
sess %>% 
  click("name", proceed_to_checkout_name)

Sys.sleep(1)

# Skip the note about subsitutions
sess %>% 
  click("class", continue_class)

Sys.sleep(1)

buy <- function(n_tries = 100, timeout_after = 30*60, end_earlier = TRUE, sleep_time = 1) {
  if (!check_on_schedule_order_page(sess)) return(invisible())
  
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
      stringr::str_detect("Sign-In") %>% 
      any()
    
    if (need_to_auth) auth()

    glue_message("On try {try_num} of {n_tries} and {elapsed_time} seconds of {timeout_after}.")
    
    raw <- 
      sess %>% 
      extract_html() 
    
    available_windows <- 
      raw %>% 
      rvest::html_nodes("h4") %>% 
      rvest::html_text() %>% 
      stringr::str_detect(available_windows_text) %>% 
      any()
    
    if (available_windows) {
      # Try and hit a free slot button
      sess %>% 
        click("class", select_price_class)
      
      sess %>% 
        click("class", continue_class)
      
      sess %>% 
        click("id", continue_top_id)
      
      sess %>% 
        click("name", place_order_name)
      
      raw <- 
        sess %>% 
        extract_html()
      
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
    Sys.sleep(runif(1, sleep_time - 0.5, sleep_time + 0.5))
    
    seleniumPipes::refresh(sess)
    try_num %<>% +1
    elapsed_time <- (lubridate::now() - start_time) %>% round(0)
  }
}

buy()
