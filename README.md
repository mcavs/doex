<img align="right" width="220" height="240" src="https://github.com/mcavs/doex/blob/master/doexlogo.png">

# doex: An R Package for The One-Way Heteroscedastic ANOVA Tests 



## Installation

To install the package from the CRAN, the package can be installed by using following code:

```r
install.packages("doex")
library(doex)
```

## Usage

`doex` provides 25 statistical test functions, two datasets and an outlier generation function:

- `AF()`, `AG()`, `AGF()`, `B2()`, `BF()`, `BX()`, `CF()`, `FA()`, `GF()`, `JF()`, `MBF()`,  `MW()`, `OS()`, `OSR()`, `PB()`, `PF()`, `SS()`, `WA()`, and `WE()` are used for heteroscedastic one-way ANOVA setup. 
- `MGF()` and `RGF()` are used for heteroscedastic one-way ANOVA under non-normality caused by outliers and skewness, respectively.
- `fa_exp()`, `gpv_exp()`, and `pb_exp()` are used for testing equality of two-parameter distributed means under unequal scale parameters. 
- `component` and `hybrid` are the datasets can be used for the usage examples of the tests.
- `outly()` is an outlier generation algorithm.

The Alexander-Govern (AG) test is performed with the inputs are the observations and the group labels of the observations as follows:

```r
library(doex)
AG(hybrid$data, hybrid$species)
```


The modified generalized F-test is performed with the inputs are the observations and the group labels of the observations as follows:

```r
library(doex)
MBF(hybrid$data, hybrid$species)
```


The parametric bootstrap test for two-parameter exponential distributed populations is performed with the inputs are the observations, the group labels of the observations and the repetition as follows:

```r
library(doex) 
fa_exp(component$lifetime, component$supplier)
```

To generate outliers based on interquartile range approach, `outly()` function is used with some options as follows:

```r
library(doex) 
outly(8, 2, 2, 0.05, FALSE)
```


## Citation

If you use `doex`, please cite it:

```
@article{cavus2021,
  author = {Mustafa Cavus and Berna Yazıcı},
  title = {{Testing the equality of normal distributed and independent
            groups' means under unequal variances by doex package}},
  year = {2021},
  journal = {{The R Journal}},
  doi = {10.32614/RJ-2021-008},
  url = {https://doi.org/10.32614/RJ-2021-008},
  pages = {134--154},
  volume = {12},
  number = {2}
}
```


## Contact

For any questions and feedback, please dont hesitate to contact me via following e-mail adresses:

mustafacavus@eskisehir.edu.tr 
