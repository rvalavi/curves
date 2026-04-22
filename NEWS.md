# curves 0.3.0

## New features

- Extended `univariate()` with `method = "profile"`, `"pdp"`, `"ice"`, and
  `"ice+pdp"` so single-profile, partial dependence, and ICE plots share one
  entry point.
- Added `method = "ale"` to `univariate()` for accumulated local effects
  curves on numeric predictors.
- Changed `univariate(method = "ale")` to warn and skip factor predictors
  instead of failing when numeric predictors are also available.
- Reused `n` in `univariate()` as the sampling control for PDP/ICE methods, so
  it now controls both numeric grid resolution and the number of sampled
  predictor rows.
- Added adaptive raster sampling for `univariate()` so PDP/ICE methods can draw
  more background predictor combinations when `predict_data` comes from a
  `SpatRaster`.
- Added `bivariate()` for bivariate response surfaces with static heatmap and
  contour views.
- Added optional interactive 3D response surfaces for numeric predictor pairs
  when `plotly` is installed.
- Added support for selecting predictor pairs by column name or column index.

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
