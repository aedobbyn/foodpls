library(gestalt)
library(magrittr)

glue <- glue::glue %>>>% as.character

glue_message <- glue %>>>% message

start_session <- function(url, browser = "chrome", port = 4444L) {
  seleniumPipes::remoteDr(browserName = "chrome", port = port) %>%
    seleniumPipes::go(url)
}

add_cookies <- function(sess, cookie_tbl) {
  for (i in 1:nrow(cookie_tbl)) {
    glue_message("Adding cookie {i}")
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

extract_html <- function(sess) {
  sess %>% 
    seleniumPipes::getPageSource() %>%
    as.character() %>% 
    xml2::read_html()
}

# Make sure we're on the page where we pick a delivery time before trying buy()
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
