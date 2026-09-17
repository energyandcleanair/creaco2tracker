# CREA CO2 Tracker
The EU CO2 Emission Tracker is an initiative by the Centre for Research on Energy and Clean Air (CREA) to produce timely and publicly available data on Europe’s CO2 emissions. By monitoring and analyzing emissions across the power sector, transport, industry, and buildings, the CO2 Tracker aims to help decision-makers, researchers, and the wider society understand the latest trends and respond effectively to climate challenges.

Live charts are available on [CREA's website](https://energyandcleanair.org/product/eu-co2-emission-tracker/).

Latest methodology document is available [here](https://energyandcleanair.org/wp/wp-content/uploads/2026/01/CO2-methodology.pdf)

For more details about the documentation for surpporting code:
- [Revision analysis](./doc/revision-analysis.md)
- [External comparison](./doc/external.md)

## Recommended Development Setup

Open this repository in the provided VS Code dev container.

The dev container is the recommended way to work on `creaco2tracker` because it includes the R runtime, system libraries, and editor tooling that this project expects. It also gives GitHub Copilot access to the repository's compatible local skills and keeps package installation isolated to the workspace.

In VS Code:

1. Install the Dev Containers extension if needed.
2. Open the repository.
3. Run `Dev Containers: Reopen in Container`.

The container configuration lives in `.devcontainer/` and sets up the project library under `.r-lib/`.

## How It Works

### CO2
`get_co2()` builds a consistent monthly CO2 time series using Eurostat fossil fuel and external proxy datasets, then optionally downscales to daily and applies post-processing/validation.


#### Monthly pipeline

1. Gather inputs: Eurostat fossil fuel consumption (annual + monthly), industrial production, ENTSOE+EMBER power generation, and gas demand based on ENTSOG (optionally scaled to Eurostat). Building `eurostat_cons` is the main step where much of the sector/fuel attribution logic happens.
2. Convert energy to CO2 using NCV choices and IPCC emission factors to produce monthly emissions by fuel and sector.
3. Project and impute missing/recent months with proxy models and EU‑level heuristics, then forecast remaining gaps with uncertainty bounds and reconcile sector totals.
4. Post‑process outputs (split gas, recombine fuels like peat → coal, add totals, validate, add region names, apply filters).

```mermaid
flowchart TB
  classDef input fill:#edf2ff,stroke:#4263eb,stroke-width:1px,color:#111827
  classDef process fill:#fff4e6,stroke:#f08c00,stroke-width:1px,color:#111827
  classDef data fill:#e6fcf5,stroke:#0ca678,stroke-width:1px,color:#111827
  classDef modelled fill:#fff4e6,stroke:#e67700,stroke-width:2px,color:#111827
  classDef output fill:#f3f0ff,stroke:#7048e8,stroke-width:2px,color:#111827

  subgraph power_stage["Power generation"]
    direction TB

    entsoe[(ENTSO-E daily<br/>power generation)]
    ember[(EMBER monthly/yearly<br/>power generation)]
    blend_power{{Blend and correct<br/>power generation}}
    power_daily[Daily power generation]

    entsoe --> blend_power
    ember --> blend_power
    blend_power --> power_daily
  end

  subgraph gas_stage["Gas demand"]
    direction TB

    entsog[(ENTSOG gas flows)]
    agsi[(AGSI storage)]
    eurostat_gas[(Eurostat monthly gas<br/>for gas-demand correction)]
    estimate_gas{{Estimate and correct<br/>gas demand}}
    gas_daily[Daily gas demand]

    entsog --> estimate_gas
    agsi --> estimate_gas
    eurostat_gas --> estimate_gas
    estimate_gas --> gas_daily
  end

  subgraph industry_stage["Industry proxy"]
    direction TB

    indprod[(Eurostat industrial<br/>production)]
    industry_proxy[Industry proxy]

    indprod --> industry_proxy
  end

  subgraph stable_stage["Reported consumption and emissions"]
    direction TB

    eurostat_energy[(Eurostat energy<br/>oil, solid fuels, gas)]
    build_cons{{Build Eurostat consumption<br/>and sector/fuel mapping}}
    eurostat_cons[Monthly consumption<br/>by sector/fuel]

    ncv[(NCV values<br/>IEA or IPCC)]
    factors[(IPCC emission factors)]
    convert_co2{{Convert energy to CO2}}
    co2_unprojected[Monthly CO2 from<br/>reported consumption]

    eurostat_energy --> build_cons
    build_cons --> eurostat_cons
    eurostat_cons --> convert_co2
    ncv --> convert_co2
    factors --> convert_co2
    convert_co2 --> co2_unprojected
  end

  subgraph model_stage["Imputation and forecasting"]
    direction TB

    project{{Project, impute,<br/>forecast, reconcile}}
    co2_monthly[Monthly CO2 by sector/fuel<br/>with uncertainty]

    project --> co2_monthly
  end

  subgraph final_stage["Monthly output preparation"]
    direction TB

    finalize{{Split gas, recombine fuels,<br/>add totals, validate, label}}
    get_co2_output["Monthly get_co2()<br/>emissions table"]

    finalize --> get_co2_output
  end

  power_stage --> stable_stage

  stable_stage --> model_stage
  power_stage --> model_stage
  industry_stage --> model_stage
  gas_stage --> model_stage

  model_stage --> final_stage

  class eurostat_energy,eurostat_gas,entsoe,ember,entsog,agsi,indprod,ncv,factors input
  class blend_power,estimate_gas,build_cons,convert_co2,project,finalize process
  class power_daily,gas_daily,industry_proxy,eurostat_cons,co2_unprojected data
  class co2_monthly modelled
  class get_co2_output output

  style power_stage fill:#f8f9ff,stroke:#4263eb,stroke-width:1px,color:#111827
  style gas_stage fill:#f8f9ff,stroke:#4263eb,stroke-width:1px,color:#111827
  style industry_stage fill:#f8f9ff,stroke:#4263eb,stroke-width:1px,color:#111827
  style stable_stage fill:#f4fff8,stroke:#0ca678,stroke-width:2px,color:#111827
  style model_stage fill:#fffbea,stroke:#f08c00,stroke-width:2px,color:#111827
  style final_stage fill:#faf5ff,stroke:#7048e8,stroke-width:2px,color:#111827
```

#### Downscaling

This shows the optional downscaling of monthly values to daily using power and gas proxies.

```mermaid
flowchart TB
  classDef input fill:#edf2ff,stroke:#4263eb,stroke-width:1px,color:#111827
  classDef process fill:#fff4e6,stroke:#f08c00,stroke-width:1px,color:#111827
  classDef modelled fill:#fff4e6,stroke:#e67700,stroke-width:2px,color:#111827
  classDef downscaled fill:#e7f5ff,stroke:#1c7ed6,stroke-width:2px,color:#111827
  classDef output fill:#f3f0ff,stroke:#7048e8,stroke-width:2px,color:#111827

  co2_monthly[Monthly CO2 by sector/fuel<br/>with uncertainty]
  power_daily[Daily power generation]
  gas_daily[Daily gas demand]

  downscale{{Allocate monthly emissions<br/>to individual days}}
  co2_daily[Daily CO2 by sector/fuel]

  finalize{{Split gas, recombine fuels,<br/>add totals, validate, label}}
  get_co2_output["Daily get_co2()<br/>emissions table"]

  co2_monthly --> downscale
  power_daily --> downscale
  gas_daily --> downscale

  downscale --> co2_daily
  co2_daily --> finalize
  finalize --> get_co2_output

  class power_daily,gas_daily input
  class downscale,finalize process
  class co2_monthly modelled
  class co2_daily downscaled
  class get_co2_output output
```
### Weather-controlled CO2
`get_weather_corrected_co2()` takes the output of `get_co2()` and adjusts emissions for weather‑driven effects in demand and the power mix. It returns a corrected CO2 series plus the correction factors used.


1. Renewables / power‑mix: estimates how weather‑driven hydro/solar/wind output shifts the fossil share, and applies a yearly correction factor that adjusts emissions for that mix change.
2. Demand: estimates weather‑driven demand using HDD/CDD, applies a daily correction factor by fuel/sector, and scales emissions accordingly.

> **Warning**
> Weather‑controlled CO2 and demand components currently use different weather‑correction models. They should be aligned for full consistency.

### Demand components
`get_demand_components()` decomposes daily gas and electricity demand into heating, cooling, and non‑weather components using HDD/CDD regressions. It also provides weather‑corrected demand based on climatological mean HDD/CDD.

### Data masking for historical availability scenarios
`get_co2()`, `get_demand_components()`, `get_corrected_demand()`, and `update_all()` accept a `data_masking` argument to simulate missing historical data by source.

Masking is now available at two levels:
- Granular pre-blend sources (recommended): masks raw source inputs before reconciliation.
- Compatibility coarse sources: masks post-processed tables as in previous versions.

Use `get_data_masking_config()` as a template and then define global (`all`) and per-source rules.

```r
mask_cfg <- get_data_masking_config()

# Global rule: for EU, hide all source values up to Jan-2020
mask_cfg$all <- list(
  date_to = "2020-01-31",
  iso2 = "EU"
)

# Source-specific rule: hide ENTSOE wind data from 2023 onward
mask_cfg$entsoe_power_daily <- list(
  list(
    date_from = "2023-01-01",
    source = "Wind"
  )
)

# Source-specific rule: hide EMBER monthly hydro for 2021
mask_cfg$ember_power_monthly <- list(
  list(
    date_from = "2021-01-01",
    date_to = "2021-12-31",
    source = "Hydro"
  )
)

# Source-specific rule: hide ENTSOG storage flows in 2022
mask_cfg$entsog_flow_raw <- list(
  list(
    date_from = "2022-01-01",
    date_to = "2022-12-31",
    type = "storage"
  )
)

# Source-specific rule: hide HDD values for EU weather in 2024
mask_cfg$weather <- list(
  list(
    date_from = "2024-01-01",
    date_to = "2024-12-31",
    variable = "hdd",
    region_id = "EU"
  )
)

co2 <- get_co2(data_masking = mask_cfg)
```

Granular pre-blend source keys:
- `entsoe_power_daily`, `ember_power_monthly`, `ember_power_yearly`
- `entsog_flow_raw`, `agsi_storage_daily`, `eurostat_gas_monthly_for_correction`
- `eurostat_oil_monthly`, `eurostat_oil_yearly`
- `eurostat_solid_monthly`, `eurostat_solid_yearly`
- `eurostat_gas_monthly`, `eurostat_gas_yearly`
- `eurostat_indprod`, `weather`

Compatibility coarse keys (still supported):
- `power_generation`, `gas_demand`, `eurostat_cons`

Rule fields:
- `date_from`/`available_from` and `date_to`/`available_to` define time windows.
- Any other field is treated as a column filter for the source table (for example `iso2`, `source`, `fuel`, `sector`, `variable`, `nace_r2_code`, `siec_code`).
- Matching rows are masked by removing them from the source table, simulating unavailable data as absent rows.

For the default publication-lag revision-analysis setup, use `data_masking_as_of()` or the
historical CO2 revision-analysis workflow:

```r
results <- validate_get_co2_revision_analysis()
```

This runs `get_co2()` at each unique month-end needed to validate 2020 through 2024 for
all EU countries plus the EU aggregate, using the default as-of masks. Each validation
year is compared with the January vintage two years later. Outputs are written by default
to `diagnostics/get_co2_revision_analysis_2020_2024`.

Key outputs:
- `tables/vintage_revision_comparison.csv`: One row per comparable estimate with lag buckets,
  revision metrics, and source-maturity shares.
- `tables/debug_revision_summary.csv`: Lag-bucket and maturity-stage summaries for totals,
  country totals, and country-components.
- `tables/revision_outliers.csv`: Largest absolute revisions, sorted by tonnes CO2.
- `tables/following_year_absolute_revision_by_year.csv` and
  `tables/following_year_absolute_revision_mean.csv`: Inputs for the historical
  following-year revision charts.
- `charts/summary/`: Aggregate debugging charts by lag bucket, data maturity, and gross
  revision contribution, plus following-year absolute, year-share, and raw-revision charts.
- `charts/details/`: Country and country-component revision heatmaps and maturity-stage
  debugging charts.


## Running Scripts

Use `./rr` to run repository scripts in a containerized R runtime.

### Requirements

- Podman
- `GITHUB_PAT` in your enviironment or `.Renviron` to get the private R dependencies in our repo

### Modes

- `./rr run <script-or-rscript-args...>`: runs `Rscript` in the runtime image.
- `./rr shell`: opens an interactive shell in the same runtime context.
- `./rr clean [--all] [--purge-cache]`: removes project-scoped container artifacts.

### Behavior

- Automatically builds the runtime image on first use.
- Automatically rebuilds when build inputs change (`rr.Dockerfile`, `DESCRIPTION`, `rr`).
- Bind-mounts the full repository to preserve existing relative-path behavior.
- Accepts `RR_WORK_DIR` to run from a repository-relative working directory inside the runtime
  image when a script needs an isolated source root.
- Keeps R dependencies in image-managed library paths outside the workspace mount.
- Persists pak cache in a project-scoped Podman volume.

### Examples

```bash
./rr run <path-to-script>
./rr run -e "<custom-code>"
./rr shell
./rr clean --all --purge-cache
```


## Release Notes

See [NEWS](NEWS.md) for the release notes.
