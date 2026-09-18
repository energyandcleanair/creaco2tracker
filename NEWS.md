# Release notes

## creaco2tracker 1.4

- Reconstructs supported gaps in hard-coal coking and coke consumption from validated country
  history and industrial activity, while preserving reported observations and exposing the
  evidence and validation used for each estimate.
- Keeps historical coke reconstructions stable as later data arrive, and leaves long, unbounded,
  duplicate, or insufficiently supported gaps unresolved.
- Preserves known sector consumption when it exceeds a forecast total by raising the total and
  retaining the original forecast and conflict diagnostics.
- Repairs supported current-year coal-power allocations month by month, including Greek lignite
  consumption when later months are unavailable.
- Carries verified country power corrections into reported EU sector aggregates while preserving
  total EU coal emissions.
- Validates countries using complete, consecutive calendar years and routes external inputs
  through dedicated data-access functions for more reliable testing and caching.

## creaco2tracker 1.3

- Produces a more complete monthly coal-emissions history by filling gaps from annual Eurostat
  data while preserving reported observations and country- and fuel-specific seasonal patterns.
- Provides more stable sector estimates and projections while keeping coal fuels separate and
  retaining usable reported detail.
- Makes data limitations clearer: incomplete components remain visible, and EU totals are rebuilt
  from countries only when all 27 member states are represented.
- Makes reconstructed coal data easier to audit through additional diagnostics and validation,
  without changing the public output schema.

## creaco2tracker 1.2

### Model changes

- Reworked power-generation estimates using ENTSO-E daily data and Ember monthly and annual
  totals.
- Improved recent EU CO2 estimates with member-state gap filling, interpolation, and seasonal
  fallback methods.
- Added stronger structural validation for published output.
- Redesigned gas and electricity demand decomposition, with GAM as the default model.
- Added weather-corrected demand using climatological heating and cooling demand.
- Added source-level data masking to reconstruct historical data availability.
- Fixed a version 1.1 issue where daily downscaling could produce records with missing dates.

### Developer changes

- Added revision analysis for historical CO2 estimate vintages.
- Added comparisons across Git versions and against external emissions datasets.
- Improved data access with AGSI completeness checks, Eurostat retries, Parquet caching, and
  structured logging.
- Added public helpers for IPCC emission factors and net calorific values.
- Added containerized production and development workflows.

## creaco2tracker 1.1

Major update for the January 2026 CO2 update.

- Added weather correction and demand splitting.
- Combined ENTSO-E and Ember data for power-generation estimates, with Ember as the source of
  truth.
- Added oil- and peat-fired electricity, including for Finland, using Ember's other-fossil series
  as a potential predictor and downscaler.
- Adopted the shared IEA approach for net calorific values.
- Scaled gas demand to Eurostat data.
- Excluded international transport from comparisons with the Global Carbon Budget.

Follow-up items recorded at release time:

- Embed the heating-separation approach from the analysis in the package.
- Scale Ember monthly data to Ember yearly totals.

## creaco2tracker 0.9

- Added national-level estimates for all sectors, including transport emissions.
- Restored peat and shale-oil emissions.
- Improved data imputation.
- Used for the March 2025 report documented in the
  [version 0.9 methodology](https://energyandcleanair.org/wp/wp-content/uploads/2025/03/CREA-CO2-methodology-v0.9.pdf).

## creaco2tracker 0.7

- Added country-level estimates and improved EU estimates.

## creaco2tracker 0.6

- Added oil use in transportation.

## creaco2tracker 0.5

- Added confidence intervals.

## creaco2tracker 0.4

- Improved handling of partially available Eurostat data, such as coal data reported for
  electricity but not industrial use.
- Used industrial-production data to estimate coal and coke consumption outside the electricity
  sector.
- Prevented incomplete source data from causing an artificial drop in coal emissions at the end
  of 2023.

## creaco2tracker 0.3

- Refined the Eurostat energy categories, emission factors, and net calorific values, bringing
  results closer to other widely used datasets while retaining daily updates.
- Documented the approach in the
  [version 0.3 methodology](https://energyandcleanair.org/wp/wp-content/uploads/2024/02/CREA-CO2-methodology-v0.3.pdf).

## creaco2tracker 0.2

- Final release before correcting the energy-balance categories; this version was used for the
  EU emissions article published on 24 January 2024.
