# curves <img src="man/figures/logo.png" align="right" width="120"/>

[![R build
status](https://github.com/rvalavi/curves/workflows/R-CMD-check/badge.svg)](https://github.com/rvalavi/curves/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

**curves** is an R package for creating partial dependence and response plots for fitted 
models, using ggplot2 for visualization. While packages like `pdp` and `vip` also offer 
similar functionality, curves provides a few additional features that may be useful 
in certain cases:
* Supports raster-based predictions, making it suitable for spatial models.
* Highlights areas of extrapolation in the response curves.
* Offers interactive visualizations for better exploration of model outputs.

With a simple syntax that fits into typical R workflows, curves makes it easier to 
examine how predictions change with individual predictors while keeping others constant.
