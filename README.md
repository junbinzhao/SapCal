
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SapCal

<!-- badges: start -->
<!-- badges: end -->

The goal of SapCal is to calculate tree sap flow using HFD (Nadezhdina
et al. 2012) and LHB methods (Trcala and Cermak 2016). This package is
created in association with the study “Inconsistent results of sap flow
calculations from Heat Field Deformation (HFD) and Linear Heat Balance
(LHB) methods” (Zhao et al., to be published).

## Installation

You can install the package of SapCal from [GitHub](https://github.com/)
with:

``` r
install.packages("remotes")
remotes::install_github("junbinzhao/SapCal")
```

## Example

1.  Calculate the sap flow density (SFD) using the LHB approach

``` r
library(SapCal)
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.1.2     v dplyr   1.0.6
#> v tidyr   1.1.3     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

# load example data
df <- read.csv(file = system.file("extdata", "HFD2J704_202006.csv", package = "SapCal"),
header=T)
names(df)
#>  [1] "Date_time" "Sym1"      "Sym2"      "Sym3"      "Sym4"      "Sym5"     
#>  [7] "Sym6"      "Sym7"      "Sym8"      "Asym1"     "Asym2"     "Asym3"    
#> [13] "Asym4"     "Asym5"     "Asym6"     "Asym7"     "Asym8"     "Temp1U"   
#> [19] "Temp1L"    "Temp1S"    "Temp2U"    "Temp2L"    "Temp2S"    "Temp3U"   
#> [25] "Temp3L"    "Temp3S"    "Temp4U"    "Temp4L"    "Temp4S"    "Temp5U"   
#> [31] "Temp5L"    "Temp5S"    "Temp6U"    "Temp6L"    "Temp6S"    "Temp7U"   
#> [37] "Temp7L"    "Temp7S"    "Temp8U"    "Temp8L"    "Temp8S"    "K8"       
#> [43] "K7"        "K6"        "K5"        "K4"        "K3"        "K2"       
#> [49] "K1"        "HeaterWcm"

# convert the data into long format
df <- df %>%
  pivot_longer(cols = Temp1U:Temp8S,
               names_to = c(".value","Position"),
               names_sep = c(5)) %>%
  pivot_longer(cols = starts_with(c("K","Temp")),
               names_to = c(".value","Depth"),
               names_sep = -1) %>%
  pivot_wider(names_from = Position,
              values_from = Temp)

# calculate sap flow density using dynamic K values that are provided as a variable
df_lhb1 <- Cal_LHB(df, # input data
                   T_up = "U", # column name (string) for temperature of the upper sensor
                   T_low = "L", # the lower sensor
                   T_side = "S", # the side sensor
                   K="K", # column name for K values
                   Heat=2.6 # heating power used, W m-1
                   )
tapply(df_lhb1$SFD_lhb,df_lhb1$Depth,summary)
#> $`1`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#> -0.1790  0.1378  0.8927  1.4721  2.3270  9.2459      93 
#> 
#> $`2`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#> -0.1156  0.1262  0.4107  1.0772  1.8757  5.1805       3 
#> 
#> $`3`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#> -0.08554  0.11469  0.35968  0.84923  1.51492  3.94365       14 
#> 
#> $`4`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#> -0.09217  0.08798  0.24279  0.50856  0.89263  2.29313        7 
#> 
#> $`5`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#> -0.05562  0.07905  0.18003  0.29300  0.46455  1.20093       16 
#> 
#> $`6`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#> -0.06058  0.07266  0.12996  0.18837  0.27583  0.76646        5 
#> 
#> $`7`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#> -0.06750  0.03900  0.08785  0.10276  0.14490  0.46315       39 
#> 
#> $`8`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#> -0.01726  0.01999  0.03685  0.04937  0.06704  0.28330       13

# calculate sap flow density using constant K value
df_lhb2 <- Cal_LHB(df,T_up = "U",T_low = "L",T_side = "S",
                   K=.8, # use a fixed number for the K value
                   Heat=2.6)
tapply(df_lhb2$SFD_lhb,df_lhb2$Depth,summary)
#> $`1`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#>  0.00992  0.50206  1.80948  2.32890  3.43135 10.42370      168 
#> 
#> $`2`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#> 0.00064 0.36573 1.81243 2.10927 3.20139 7.60512     153 
#> 
#> $`3`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#> 0.00783 0.30557 1.28731 1.38484 2.10139 4.70204     153 
#> 
#> $`4`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#> 0.00683 0.29179 0.65413 0.76284 1.13786 2.51387     173 
#> 
#> $`5`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#> 0.00139 0.14212 0.26132 0.32802 0.46281 1.12877     198 
#> 
#> $`6`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#> 0.00139 0.02871 0.08075 0.12368 0.17716 0.55452     228 
#> 
#> $`7`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#> 0.00139 0.01319 0.04415 0.06356 0.09703 0.25655     311 
#> 
#> $`8`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  0.0031  0.0239  0.0344  0.0382  0.0357  0.1059     347
```

2.  Calculate the SFD using the HFD approach

``` r
# calculate sap flow density using dynamic K values that are provided as a variable
df_hfd1 <- Cal_HFD(df,
                   T_up = U, # column name (non-string) for temperature of the upper sensor
                   T_low = L, # the lower sensor
                   T_side = S, # the side sensor
                   K=K, # column name for K values
                   L=7.5 # sapwood depth, cm
                   )
#> [1] "dynamic K"
tapply(df_hfd1$SFD_hfd, df_hfd1$Depth, summary)
#> $`1`
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -0.500866 -0.008588  0.379739  0.805587  1.324789  4.954852 
#> 
#> $`2`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -0.2770  0.2986  0.6902  0.9587  1.4718  3.9752 
#> 
#> $`3`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -0.1727  0.2631  0.5007  0.6932  1.0245  2.5507 
#> 
#> $`4`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -0.1519  0.2120  0.3819  0.4860  0.6913  1.8108 
#> 
#> $`5`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -0.1039  0.1750  0.2815  0.3252  0.4524  1.1201 
#> 
#> $`6`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -0.1024  0.1689  0.2255  0.2555  0.3230  0.8631 
#> 
#> $`7`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -0.11083  0.05096  0.11722  0.14661  0.23492  0.61693 
#> 
#> $`8`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -0.05367  0.04501  0.07793  0.08381  0.10399  0.37112

# calculate sap flow density using constant K value
df <- df %>% select(-K) # remove the K value column to avoid confusion 
df_hfd2 <- Cal_HFD(df,T_up = U,T_low = L,T_side = S,
                   K=.6, # use a fixed number for the K value
                   L=7.5)
#> [1] "Constant K"
tapply(df_hfd2$SFD_hfd, df_hfd2$Depth, summary)
#> $`1`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -1.22192 -0.61459  0.06363  0.19875  0.78651  3.25480 
#> 
#> $`2`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -1.0565 -0.5121  0.1936  0.2112  0.7844  2.5844 
#> 
#> $`3`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -0.97003 -0.49447  0.10114  0.06664  0.57131  1.61710 
#> 
#> $`4`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -0.94523 -0.55292 -0.08418 -0.10501  0.31664  0.99297 
#> 
#> $`5`
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -0.96957 -0.57807 -0.26048 -0.24218 -0.01121  0.94995 
#> 
#> $`6`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -0.9872 -0.6280 -0.3898 -0.3651 -0.1768  0.8931 
#> 
#> $`7`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -0.9613 -0.6339 -0.4328 -0.4222 -0.2721  0.7550 
#> 
#> $`8`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -0.9554 -0.6395 -0.4722 -0.4768 -0.3384  0.7126
```
