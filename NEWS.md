# curves 0.2.0

## New features

- Added `bivariate()` for bivariate response surfaces with static heatmap and
  contour views.
- Added optional interactive 3D response surfaces for numeric predictor pairs
  when `plotly` is installed.
- Added support for selecting predictor pairs by column name or column index.

## Changes

- Renamed the bivariate plotting mode argument from `type` to `plot_type` so
  model-specific `type` arguments can still be passed through `...` to
  `predict()`.
