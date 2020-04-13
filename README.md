
# foodpls

<!-- badges: start -->

<!-- badges: end -->

This is a bot that can be used to check out an Amazon cart. Use at your
own risk\!

## Installation

``` r
# install.packages("devtools")
devtools::install_github("aedobbyn/foodpls")
```

## Configuration

  - Add some Amazon cookies to `data-raw/cookies_template.csv` and
    change the file name to `data-raw/cookies.csv`
      - This allows Selenium to bypass the 2FA that you’d normally have
        to go through when you’re not already logged in
      - You can find these by opening the Chrome inspector on an Amazon
        page, heading to the Applications tab, then looking under
        Cookies
  - Save your email in an environment variable called `EMAIL`
  - Save your Amazon password in an environment variable called
    `AMAZON_PASS`

e.g.

    Sys.setenv(EMAIL="ratatouille@gmail.com")

## Run

There are three main functions in this package.

  - `get_in`: logs you in
  - `checkout`: checks out your cart and navigates you to the page where
    you would select a delivery time if one is available
  - `buy`: selects the first available delivery time if it’s available.
    If it’s not, it keeps refreshing the page until a certain number of
    tries is reached or a time limit is reached, or both.

`run` evaluates all of these in order.

``` r
library(foodpls)
```

There are two carts to choose from: `"whole foods"` or `"amazon fresh"`.

``` r
run(cart = "whole foods")
```

The `...` args to `run` are passed to `buy`.

``` r
run(cart = "amazon fresh", n_tries = 3, sleep_time = 2)
```
