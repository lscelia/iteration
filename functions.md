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

    ##  [1]  0.3008219  2.5946294  0.1667645 -1.1898032 -0.6392758 -0.2718384
    ##  [7]  1.0702054 -0.6563566  0.8118672  0.9764790  0.1034383 -1.6264451
    ## [13]  0.6821471  0.2264728  1.3756596 -0.4168225 -0.6139557 -0.1978992
    ## [19]  0.5492058 -0.2320501 -1.1597448  1.1723248 -1.3387941 -0.6011796
    ## [25] -1.0858505

``` r
#as a function
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  return(z)
}

#call function
z_scores(x = x_vec)
```

    ##  [1]  0.3008219  2.5946294  0.1667645 -1.1898032 -0.6392758 -0.2718384
    ##  [7]  1.0702054 -0.6563566  0.8118672  0.9764790  0.1034383 -1.6264451
    ## [13]  0.6821471  0.2264728  1.3756596 -0.4168225 -0.6139557 -0.1978992
    ## [19]  0.5492058 -0.2320501 -1.1597448  1.1723248 -1.3387941 -0.6011796
    ## [25] -1.0858505

``` r
#another example
y_vec = rnorm(40, mean = 12, sd = .3)
z_scores(y_vec)
```

    ##  [1] -1.14849081 -0.22321525  0.97457581  1.89785958  0.66746722  1.06117580
    ##  [7]  0.23821027 -1.10927158 -0.95218208 -0.14719320  0.19290692  0.62349906
    ## [13]  1.26418965  0.27919024  0.69905659 -0.46615589 -0.82144270  0.84456714
    ## [19]  1.76599799 -1.69872277  0.61216301 -0.64413534  1.34797712 -1.97652872
    ## [25]  1.16460433 -1.89260915 -0.60129619  1.25785808 -1.29877040  0.28782211
    ## [31]  0.51718612 -0.08994238 -0.34318988 -1.28189676 -0.16631507  0.21366280
    ## [37]  0.06757010 -1.29893509  0.32410150 -0.14134817

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

    ##  [1] -1.14849081 -0.22321525  0.97457581  1.89785958  0.66746722  1.06117580
    ##  [7]  0.23821027 -1.10927158 -0.95218208 -0.14719320  0.19290692  0.62349906
    ## [13]  1.26418965  0.27919024  0.69905659 -0.46615589 -0.82144270  0.84456714
    ## [19]  1.76599799 -1.69872277  0.61216301 -0.64413534  1.34797712 -1.97652872
    ## [25]  1.16460433 -1.89260915 -0.60129619  1.25785808 -1.29877040  0.28782211
    ## [31]  0.51718612 -0.08994238 -0.34318988 -1.28189676 -0.16631507  0.21366280
    ## [37]  0.06757010 -1.29893509  0.32410150 -0.14134817

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
    ## 1  12.0 0.281

``` r
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
    ## 1  2.68  2.85

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
    ## 1  3.78  2.45

``` r
#name by hand
sim_mean_sd(n = 30, sigma = 3, mu = 4)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.80  2.51

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
