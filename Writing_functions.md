Writing functions
================
Boya Guo
10/25/205

### Normal distribution function

``` r
x = rnorm(25, mean = 5, sd = 3)

(x - mean(x)) / sd(x)
##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872
##  [6] -1.04107494  0.33550276  0.59957343  0.42849461 -0.49894708
## [11]  1.41364561  0.23279252 -0.83138529 -2.50852027  1.00648110
## [16] -0.22481531 -0.19456260  0.81587675  0.68682298  0.44756609
## [21]  0.78971253  0.64568566 -0.09904161 -2.27133861  0.47485186
```

### Z score function

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  z
  
}

z_scores(x)
##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872
##  [6] -1.04107494  0.33550276  0.59957343  0.42849461 -0.49894708
## [11]  1.41364561  0.23279252 -0.83138529 -2.50852027  1.00648110
## [16] -0.22481531 -0.19456260  0.81587675  0.68682298  0.44756609
## [21]  0.78971253  0.64568566 -0.09904161 -2.27133861  0.47485186
```

Check to see if this works...

``` r
y=runif(100)
z_scores(y)
##   [1] -0.08859690  1.33226122 -0.23499255 -0.95099593 -1.59594860
##   [6] -1.48931803 -0.68624695  0.06332575  0.59438668 -0.35080859
##  [11]  1.52363931 -0.77021290 -0.15732253 -0.62652574  0.55314289
##  [16] -0.90202947 -0.08516828  0.98074583 -1.54569176  1.38453305
##  [21] -0.60178873  1.25162603 -0.57359843 -0.62141312 -0.09329510
##  [26]  1.44704729  1.34385506 -0.41318814  1.02152816  1.70048109
##  [31] -0.24772587  0.78147968 -0.37612919 -0.65261200  0.94658091
##  [36] -1.10695746  0.77631817 -1.40699142 -0.94843551 -1.32693655
##  [41] -0.97013825 -1.63945212  0.52135346  1.38804411  1.02743243
##  [46]  1.09556633 -0.17136581 -0.33875581  1.14579928  0.38298664
##  [51]  0.56741652 -0.54947068 -0.85667872  1.81925724  0.48877586
##  [56] -1.06800551 -1.37854229 -0.08675072  1.56511993  0.36012369
##  [61]  1.75808995  0.85288685 -0.53639651 -0.25952640 -1.30875982
##  [66] -1.80931065  0.79278235 -1.47554588 -0.20466612  0.51325179
##  [71]  1.81612563 -0.02201943 -0.06366861 -1.21530232  0.93818665
##  [76] -0.17647364  0.03567651 -1.08898196 -1.01077702  0.34882997
##  [81]  0.27163712 -1.57229664 -1.72610527  0.52323231  1.58193928
##  [86]  0.35764733  0.21988536  0.09071192  1.79114737  0.02260856
##  [91]  0.67136916  0.37042203 -0.97295610 -0.90147701  0.84368997
##  [96] -0.18138031 -1.20906300  0.90809938 -1.46886588  1.34461617
```

Compute z score for 3, character factor, and Ture/False. My function works for wha I anticipated.

``` r
z_scores(3)
## [1] NA

z_scores("my name is jeff")
## Warning in mean.default(x): argument is not numeric or logical: returning
## NA
## Error in x - mean(x): 二进列运算符中有非数值参数

z_scores(iris)
## Warning in mean.default(x): argument is not numeric or logical: returning
## NA
## Warning in Ops.factor(left, right): '-' not meaningful for factors
## Error in is.data.frame(x): (串列)对象不能强制改变成'double'种类

z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
##  [1] -1.1053588 -1.1053588  0.8684962  0.8684962 -1.1053588  0.8684962
##  [7] -1.1053588  0.8684962  0.8684962  0.8684962  0.8684962 -1.1053588
## [13]  0.8684962 -1.1053588 -1.1053588  0.8684962  0.8684962  0.8684962
## [19] -1.1053588  0.8684962 -1.1053588 -1.1053588 -1.1053588  0.8684962
## [25]  0.8684962
```

### Condtional evaluation.

First check if the input numeric or not. If false, will do nothing and move to the next step to check if length is 1 or not.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

``` r
z_scores(3)
## Error in z_scores(3): Z scores cannot be computed for length 1 vectors

z_scores("my name is jeff")
## Error in z_scores("my name is jeff"): Argument x should be numeric

z_scores(iris)
## Error in z_scores(iris): Argument x should be numeric

z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
## Error in z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE)): Argument x should be numeric
```

### Multiple output

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  list(mean = mean_x, 
       sd = sd_x)
}
```

### Store values in a data frame

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

### Multiple inputs
