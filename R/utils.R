url <- "https://www.amazon.com/ap/signin?_encoding=UTF8&ignoreAuthState=1&openid.assoc_handle=usflex&openid.claimed_id=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.identity=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0%2Fidentifier_select&openid.mode=checkid_setup&openid.ns=http%3A%2F%2Fspecs.openid.net%2Fauth%2F2.0&openid.ns.pape=http%3A%2F%2Fspecs.openid.net%2Fextensions%2Fpape%2F1.0&openid.pape.max_auth_age=0&openid.return_to=https%3A%2F%2Fwww.amazon.com%2F%3Fref_%3Dnav_signin&switch_account="
email <- Sys.getenv("EMAIL")
pass <- Sys.getenv("AMAZON_PASS")

# Element info
email_id <- "ap_email"
password_id <- "ap_password"
sign_in_id <- "signInSubmit"
cart_id <- "nav-cart-count"
checkout_name_wf <- "proceedToALMCheckout-VUZHIFdob2xlIEZvb2Rz"
checkout_name_af <- "proceedToALMCheckout-QW1hem9uIEZyZXNo"
proceed_to_checkout_name <- "proceedToCheckout"
continue_class <- "a-button-input"
continue_top_id <- "continue-top"
select_price_class <- "ufss-slot-price-container"
place_order_name <- "placeYourOrder1"

# Text
available_windows_text <- "Next available"
need_to_auth_text <- "Sign-In"
captcha_text <- "Enter the characters you see"
success_text <- "Thank you, your Whole Foods Market order has been placed."

glue_message <- glue::glue %>>>% message

start_session <- function(url, browser = "chrome", port = 4444L) {
  seleniumPipes::remoteDr(browserName = "chrome", port = port) %>%
    seleniumPipes::go(url)
}

add_cookies <- function(sess, cookie_tbl) {
  for (i in 1:nrow(cookie_tbl)) {
    glue_message("Adding {cookie_tbl$name[i]} cookie.")
    sess %>%
      seleniumPipes::addCookie(
        name = cookie_tbl$name[i],
        value = cookie_tbl$value[i]
      )
  }
}

auth <- function(sess) {
  message("Inputting password.")
  sess %>%
    input_text("id", password_id, pass)

  Sys.sleep(1)

  message("Hitting continue.")
  sess %>%
    click("id", sign_in_id)
}

input_text <- function(sess, id_type, unique_id, text) {
  element <- seleniumPipes::findElement(sess, id_type, unique_id)
  seleniumPipes::elementClear(element)
  seleniumPipes::elementSendKeys(element, text)
}

click <- function(sess, id_type, unique_id) {
  seleniumPipes::findElement(sess, id_type, unique_id) %>%
    seleniumPipes::elementClick()
}

clean_html <- function(x) {
  x %>%
    stringr::str_squish() %>%
    stringr::str_remove_all("[\\n\\r\\t]")
}

extract_html <- function(sess) {
  sess %>%
    seleniumPipes::getPageSource() %>%
    as.character() %>%
    xml2::read_html()
}

checkif_got_captchad <- function(sess) {
  sess %>% 
    extract_html() %>% 
    rvest::html_nodes("h4") %>% 
    rvest::html_text() %>%
    clean_html() %>% 
    stringr::str_detect(captcha_text) %>%
    any()
}

# Make sure we're on the page where we pick a delivery time before trying buy()
checkif_on_schedule_order_page <- function(sess) {
  on_correct_page <-
    sess %>%
    extract_html() %>%
    rvest::html_nodes("h1") %>%
    rvest::html_text() %>%
    stringr::str_detect("Schedule your order") %>%
    any()

  if (!on_correct_page) message("Didn't make it to Schedule Order page.")

  on_correct_page
}

continue_sequence <- function(sess) {
  message("Continuing on the did-you-forget page.")
  sess %>%
    click("name", proceed_to_checkout_name)
  
  Sys.sleep(1)
  
  message("Continuing on the substitutions page.")
  sess %>%
    click("class", continue_class)
  
  Sys.sleep(1)
} 
