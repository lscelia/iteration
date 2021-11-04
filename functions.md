Writing Functions
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 3)
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.81251179  1.33392047  0.84485847  0.33072887 -0.96582837 -1.79702438
    ##  [7] -0.81278865  0.28522843  0.40552577  0.06187685 -1.31030048  0.64032907
    ## [13] -1.91302172  0.01871384  1.24939893  0.26950443  0.49563779 -0.36667528
    ## [19]  0.67531797 -1.01188719 -0.05263939 -0.61399549  1.88401266  1.52430527
    ## [25] -0.36268612

``` r
#as a function
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  return(z)
}

#call function
z_scores(x = x_vec)
```

    ##  [1] -0.81251179  1.33392047  0.84485847  0.33072887 -0.96582837 -1.79702438
    ##  [7] -0.81278865  0.28522843  0.40552577  0.06187685 -1.31030048  0.64032907
    ## [13] -1.91302172  0.01871384  1.24939893  0.26950443  0.49563779 -0.36667528
    ## [19]  0.67531797 -1.01188719 -0.05263939 -0.61399549  1.88401266  1.52430527
    ## [25] -0.36268612

``` r
#another example
y_vec = rnorm(40, mean = 12, sd = .3)
z_scores(y_vec)
```

    ##  [1] -1.15894836 -0.58973886 -0.42818666  0.69023207 -1.17821107 -0.28704379
    ##  [7] -0.63492340  0.65935443  0.25700076  1.24866521 -0.65759217 -0.89233187
    ## [13] -0.43089349 -0.21451436  1.95541944  3.48315403 -0.70707592 -0.06056359
    ## [19] -0.04200147 -0.72021269  0.81800511 -0.48401367 -0.72823716 -0.33157897
    ## [25]  0.50851539  0.11789816  1.30455802  0.31064539 -0.56058837 -0.05368875
    ## [31] -0.12473867  0.89637198 -1.67208036  0.48977734 -0.08581358 -1.19003503
    ## [37] -0.46383646 -1.26969634  1.72918598  0.49776173

How great is this? Only kinda great –&gt; gives error is no numerical
input

``` r
z_scores = function(x){
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  if (length(x) < 3 ) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  return(z)
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("My", "name", "is"))
```

    ## Error in z_scores(c("My", "name", "is")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(y_vec)
```

    ##  [1] -1.15894836 -0.58973886 -0.42818666  0.69023207 -1.17821107 -0.28704379
    ##  [7] -0.63492340  0.65935443  0.25700076  1.24866521 -0.65759217 -0.89233187
    ## [13] -0.43089349 -0.21451436  1.95541944  3.48315403 -0.70707592 -0.06056359
    ## [19] -0.04200147 -0.72021269  0.81800511 -0.48401367 -0.72823716 -0.33157897
    ## [25]  0.50851539  0.11789816  1.30455802  0.31064539 -0.56058837 -0.05368875
    ## [31] -0.12473867  0.89637198 -1.67208036  0.48977734 -0.08581358 -1.19003503
    ## [37] -0.46383646 -1.26969634  1.72918598  0.49776173

## Multiple Outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
  )
  
  return(output_df)

}

mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.9 0.220

``` r
#scoping
#x = y_vec
#mean_and_sd(x)
#the function may not works
#try rm(x) to make sure the code still works
```

## Different sampjle sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.56  3.18

Let’s write a function that simulates data, computes the mean and sd

``` r
sim_mean_sd = function(n, mu, sigma) {
  
  # do checks on inputs
  # shift + option + select lines ==> tab multiple lines
  
  sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}


#positional matching
sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.75  2.71

``` r
#name by hand
sim_mean_sd(n = 30, sigma = 3, mu = 4)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.02  3.09

``` r
#in the argument, set mu = 4, sigma = 3 by default
#sim_mean_sd(30)
```

## Naopleon Daytime

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Okay but there are a lot of pages of reviews… Write a function that gets
reviews based on page url

``` r
get_page_revivews = function(page_url) {
  
  page_html = read_html(page_url)

  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = 
    tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
    )
  
  return(reviews)
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
  get_page_revivews(urls[1]),
  get_page_revivews(urls[2]),
  get_page_revivews(urls[3]),
  get_page_revivews(urls[4]),
  get_page_revivews(urls[5])
)
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…
    ## # … with 40 more rows

## Scoping

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
