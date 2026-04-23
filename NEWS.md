# curves 0.3.0

## New features

- Extended `univariate()` with `method = "profile"`, `"pdp"`, `"ice"`, and
  `"ice+pdp"` so single-profile, partial dependence, and ICE plots share one
  entry point.
- Added `method = "ale"` to `univariate()` for accumulated local effects
  curves on numeric predictors.
- Changed `univariate(method = "ale")` to warn and skip factor predictors
  instead of failing when numeric predictors are also available.
- Split `univariate()` and `interactive_map_curves()` sampling controls so `n`
  sets numeric grid resolution while `background_n` sets the number of
  randomly sampled background rows used for PDP/ICE.
- Added `pdp_band` to `univariate()` and `interactive_map_curves()` so
  `method = "pdp"` can draw central quantile ribbons for numeric predictors.
- Added adaptive raster sampling for `univariate()` so PDP/ICE methods can draw
  more background predictor combinations when `predict_data` comes from a
  `SpatRaster`.
- Added `bivariate()` for bivariate response surfaces with static heatmap and
  contour views.
- Extended `bivariate()` with `method = "pdp"` and `method = "ale"`, plus
  optional marginal rugs for numeric predictor pairs in static plots.
- Added optional interactive 3D response surfaces for numeric predictor pairs
  when `plotly` is installed.
- Added support for selecting predictor pairs by column name or column index.
- Added support for list-valued `fun` in `multimodel()` so mixed model types
  can use model-specific prediction wrappers before averaging curves.

## Changes

- Renamed the bivariate plotting mode argument from `type` to `plot_type` so
  model-specific `type` arguments can still be passed through `...` to
  `predict()`.
- Changed `bivariate(plot_type = "heatmap")` to use a viridis fill scale by
  default and to stop drawing contour overlays on heatmaps.
- Updated the random forest species distribution vignette to use a smaller
  predictor set and to include `univariate()` examples for profile, PDP, and
  ICE + PDP plots.
- Stopped drawing connecting lines for unordered factor predictors in
  `univariate()`, so categorical panels no longer imply numeric intervals.
