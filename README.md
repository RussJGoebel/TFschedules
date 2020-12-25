
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TFschedules

<!-- badges: start -->

<!-- badges: end -->

The goal of TFschedules is to make it easier to select teaching fellow
preferences by determining which courses conflict with a given schedule.
The courses considered for conflicts are obtained from html tables found
on the websites for the
[CAS](http://www.bu.edu/link/bin/uiscgi_dept_class_sched?Sem=SPRG&Year=21&Col=CAS&Dept=MA)
courses and
[GRS](http://www.bu.edu/link/bin/uiscgi_dept_class_sched?Sem=SPRG&Year=21&Col=GRS&Dept=MA)
courses.

## Installation

This package can be installed from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RussJGoebel/TFschedules")
```

## Use

To use this app, you can run:

``` r
TFschedules::view_schedule()
```
