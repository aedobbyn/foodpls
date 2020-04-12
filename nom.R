library(bonanza)

url <- "https://www.amazon.com/ap/signin?_encoding=UTF8&ignoreAuthState=1&openid.assoc_handle=usflex&openid.claimed_id=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.identity=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.mode=checkid_setup&openid.ns=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0&openid.ns.pape=http%3A%2F%2Fspecs.openid.net%2Fextensions%2Fpape%2F1.0&openid.pape.max_auth_age=0&openid.return_to=https%3A%2F%2Fwww.amazon.com%2F%3Fref_%3Dnav_signin&switch_account="

email_id <- "ap_email"
password_id <- "ap_password"
pass <- Sys.getenv("AMAZON_PASS")
sign_in_id <- "signInSubmit"
cart_id <- "nav-cart-count"
checkout_name <- "proceedToALMCheckout-VUZHIFdob2xlIEZvb2Rz"
skip_beforeyoucheckout_name <- "proceedToCheckout"
select_free_class <- "a-button-text ufss-slot-toggle-native-button"

sess <- browse.launch_session(url)

sess %>% 
  browse.send_text("id", email_id, "amanda.e.dobbyn@gmail.com")

sess %>% 
  browse.click_element("id", "continue")

Sys.sleep(2)

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

sess %>% 
  browse.click_element("name", skip_beforeyoucheckout_name)

sess %>% 
  browse.click_element("class", "a-button-input")


hit_window <- function(n_tries = 100, timeout_after = 30*60, and = TRUE) {
  `%*%` <- if (and) `&&` else `||`
  
  while (try_num < n_tries %*% elapsed_time < timeout_after) {
    raw <- 
      sess %>% 
      browse.extract_html() 
    
    available_windows <- 
      raw %>% 
      rvest::html_nodes("h4") %>% 
      rvest::html_text() %>% 
      stringr::str_detect("No delivery windows available.") %>% 
      any() %>% 
      `!`
    
    if (available_windows) {
      # Try and hit a free slot button
      sess %>% 
        browse.click_element("class", select_free_class)
    } else {
      seleniumPipes::refresh()
    }
  }
}

