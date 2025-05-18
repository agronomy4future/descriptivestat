<!-- README.md is generated from README.Rmd. Please edit that file -->
# descriptivestat
<!-- badges: start -->
<!-- badges: end -->

The goal of the descriptivestat package is to automatically embed descriptive statistics (mean, standard deviation, standard error, 95% confidence interval, coefficient of variation, and interquartile range) in the dataset.

□ Code summary: https://github.com/agronomy4future/r_code/blob/main/Embedding_Key_Descriptive_Statistics_within_Original_Data_(Feat__descriptivestat).ipynb

□ Code explained: https://agronomy4future.com/archives/24197

## Installation

You can install the development version of interpolate like so:

Before installing, please download Rtools (https://cran.r-project.org/bin/windows/Rtools)

``` r
if(!require(remotes)) install.packages("remotes")
if (!requireNamespace("descriptivestat", quietly = TRUE)) {
  remotes::install_github("agronomy4future/descriptivestat", force= TRUE)
}
library(remotes)
library(descriptivestat)
```

## Example

This is a basic code to calculate descriptive statistics

``` r
df1= descriptivestat(data=df, group_vars= c("tr1", "tr2"),
                     value_vars= c("y"),
                     output_stats= c("v","sd","se","ci","cv","iqr"))
print(df1)
```

## Let’s practice with actual dataset

``` r
# data upload 
if(!require(readr)) install.packages("readr")
library(readr)
github="https://raw.githubusercontent.com/agronomy4future/raw_data_practice/refs/heads/main/wheat_grain_grea_and_heat_tolerance.csv"
df=data.frame(read_csv(url(github), show_col_types=FALSE))
df= subset(df, select = -tolerance)
print(tail(df,5))
     genotype     thinning  area
.
.
.
7970      cv2 manipulation 19.71
7971      cv2 manipulation 16.40
7972      cv2 manipulation 21.38
7973      cv2 manipulation 14.33
7974      cv2 manipulation 10.83

# descriptivestat() package to calculate standard deviation and interquartile range
df1= descriptivestat(data= df, group_vars= c("genotype", "thinning"),
                     value_vars= c("area"),
                     output_stats= c("sd","iqr"))

print(head(df1, 5))
  genotype thinning  area category group_id    sd.area Q1.area Q2.area Q3.area
  <chr>    <chr>    <dbl> <chr>    <chr>         <dbl>   <dbl>   <dbl>   <dbl>
1 cv1      control  17.8  mean     cv1_control    2.97    16.1    18.2    20.0
2 cv1      control   7.56 observed cv1_control   NA       NA      NA      NA  
3 cv1      control   7.25 observed cv1_control   NA       NA      NA      NA  
4 cv1      control   7.99 observed cv1_control   NA       NA      NA      NA  
5 cv1      control   5.86 observed cv1_control   NA       NA      NA      NA
.
.
.
