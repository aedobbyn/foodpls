library(bonanza)

url <- "https://www.amazon.com/ap/signin?_encoding=UTF8&ignoreAuthState=1&openid.assoc_handle=usflex&openid.claimed_id=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.identity=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.mode=checkid_setup&openid.ns=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0&openid.ns.pape=http%3A%2F%2Fspecs.openid.net%2Fextensions%2Fpape%2F1.0&openid.pape.max_auth_age=0&openid.return_to=https%3A%2F%2Fwww.amazon.com%2F%3Fref_%3Dnav_signin&switch_account="

email_id <- "ap_email"
password_id <- "ap_password"
pass <- Sys.getenv("AMAZON_PASS")
sign_in_id <- "signInSubmit"
cart_id <- "nav-cart-count"
checkout_name <- "proceedToALMCheckout-VUZHIFdob2xlIEZvb2Rz"
continue_class <- "a-button-input"
continue_top_id <- "continue-top"
# select_free_class <- "a-button-text ufss-slot-toggle-native-button"
select_price_class <- "ufss-slot-price-container"
place_order_class <- "a-button-text place-your-order-button"
no_windows_text <- "No delivery windows available."
success_text <- "Thank you, your Whole Foods Market order has been placed."

sess <- browse.launch_session(url)

sess %>% 
  browse.send_text("id", email_id, "amanda.e.dobbyn@gmail.com")

sess %>% 
  browse.click_element("id", "continue")

Sys.sleep(2)

# Input password
sess %>% 
  browse.send_text("id", password_id, pass)

sess %>% 
  browse.click_element("id", sign_in_id)

# Assume we can get past 2FA

sess %>% 
  browse.click_element("id", cart_id)

Sys.sleep(2)

sess %>% 
  browse.click_element("name", checkout_name)

# Skip the "before you checkout do you want to add stuff"
sess %>% 
  browse.click_element("class", continue_class)

# Skip the note about subsitutions
sess %>% 
  browse.click_element("class", continue_class)

hit_window <- function(n_tries = 100, timeout_after = 30*60, and = TRUE) {
  `%*%` <- if (and) `&&` else `||`
  
  start_time <- lubridate::now()
  try_num <- 1
  elapsed_time <- (lubridate::now() - start_time) %>% round(0)
  
  while ((try_num < n_tries) %*% (elapsed_time < timeout_after)) {

    dev.glue_message("On try {try_num} of {n_tries} and {elapsed_time} seconds of {timeout_after}.")
    
    raw <- 
      sess %>% 
      browse.extract_html() 
    
    no_available_windows <- 
      raw %>% 
      rvest::html_nodes("h4") %>% 
      rvest::html_text() %>% 
      stringr::str_detect(no_windows_text) %>% 
      any()
    
    if (!no_available_windows) {
      # Try and hit a free slot button
      sess %>% 
        browse.click_element("class", select_price_class)
      
      sess %>% 
        browse.click_element("class", continue_class)
      
      sess %>% 
        browse.click_element("id", continue_top_id)
      
      sess %>% 
        browse.click_element("id", place_order_class)
      
      raw <- 
        sess %>% 
        browse.extract_html()
      
      success <- 
        raw %>% 
        rvest::html_nodes("h2") %>% 
        rvest::html_text() %>% 
        stringr::str_detect(success_text) %>% 
        any()
      
      if (success) {
        message("Yay!")
        return()
      } 
    } 
    seleniumPipes::refresh(sess)
    try_num %<>% +1
    elapsed_time <- (lubridate::now() - start_time) %>% round(0)
  }
}

hit_window()
