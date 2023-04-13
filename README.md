# Overlapping Group Elastic Net Using the Orthogonalizing EM (OEM) Algorithm

## Introduction

**ovganet** is an R package that fits overlapping group lasso/elastic nets using the latent group lasso approach [(Jacob et al., 2009)](https://icml.cc/Conferences/2009/papers/471.pdf). 
Our package is an extension of the [oem](https://github.com/jaredhuling/oem) package developed by [Huling, J.D. and Chien, P.](https://arxiv.org/abs/1801.09661), based on [Xiong, S., Dai, B., Huling, J., Qian, P. Z. G. (2016)](https://www.tandfonline.com/doi/full/10.1080/00401706.2015.1054436). Certain parts of our code are adapted from [grpregOverlap](https://github.com/YaohuiZeng/grpregOverlap).

Author(s): Haosen He, Alison Morantz 

## Installation

You can use the `devtools` package to install **ovganet**. If you don't have `devtools` installed, you can install it by running the following code:

```R
install.packages("devtools", repos = "http://cran.us.r-project.org")
```

Then go ahead and install **ovganet** from GitHub:

```R
library(devtools)
devtools::install_github("HaosenHe/ovganet")
```
