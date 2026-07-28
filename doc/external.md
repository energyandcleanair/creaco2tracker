# Comparing CREA's EU CO₂ estimates with external datasets

## Overview

This analysis checks whether CREA's estimates of fossil carbon dioxide (CO₂) emissions for the
European Union (EU) are broadly consistent with other published estimates. We compare annual
CREA estimates with nine external series drawn from inventories, energy balances and
near-real-time emissions datasets. The comparison covers 1990–2024 where data overlap, using
CREA estimates available on 31 January 2026. We compare both emission levels and the direction
of year-to-year change, and examine the Global Carbon Budget (GCB) separately by fuel because it
is the closest match to CREA's fossil-fuel scope. The datasets are not fully comparable: they
differ in sector coverage, territorial allocation, treatment of international transport and
methods for converting energy use into emissions. Agreement is therefore a useful consistency
check, not evidence that either dataset is the final truth.

## Scope and comparison period

The main result is an annual comparison for the current 27 EU Member States, treated as a fixed
EU27 geography throughout the period. Country-level comparisons are also calculated for the 27
members where both datasets report a value. The United Kingdom is excluded.

The analysis uses CREA's central estimate for all fuels and sectors from the January 2026
vintage. Monthly values are summed to calendar years and expressed in million tonnes of CO₂
(MtCO₂). The comparison begins in 1990 and ends in 2024. Ending in 2024 avoids comparing an
incomplete 2025 with annual datasets that were not yet complete. Each external series contributes
only the years and countries for which it reports a value, so its effective comparison period may
be shorter.

The headline comparison statistics and both EU charts use CREA's published total. This includes
international aviation where it is available in the CREA estimates. No adjustment is made to make
CREA match the scope of a particular external dataset. The paired data also retain an
aviation-adjusted CREA series for the separate GCB fuel diagnostic described below.

> **Interpretation note:** Dataset scopes and methods differ. These differences contribute to
> variation between CREA and external series, but do not explain every difference and should not
> be interpreted as evidence that either estimate is wrong.

## External datasets

All external values are standardised to annual MtCO₂. Climate Watch, United Nations Framework
Convention on Climate Change (UNFCCC) and Potsdam Institute for Climate Impact Research (PIK)
inputs are labelled MtCO₂-equivalent in their source files, but contain CO₂ only. Their numerical
values are therefore treated as MtCO₂. The ranges below are the years that overlap the
configured 1990–2024 comparison, not necessarily the full coverage of each source.

- **Climate Watch, 1990–2020:** energy-sector CO₂ totals from the Climate Watch snapshot
  held with the analysis inputs. The snapshot does not record a release version or retrieval date.
- **UNFCCC, 1990–2021:** energy-sector CO₂ totals from the UNFCCC snapshot held
  with the analysis inputs. The snapshot does not record a release version or retrieval date.
- **PIK, 1990–2022:** energy-sector CO₂ totals from the PIK snapshot held with the analysis
  inputs. The snapshot does not record a specific release version or retrieval date.
- **Global Carbon Budget 2025, 1990–2024:** version 15 of the 2025 release. Only coal, oil
  and gas are summed; cement, flaring and the dataset's “other” category are excluded. EU totals
  are the sum of the 27 Member States. See the [source record][gcb-source].
- **IEA Energy Balance CO₂, 1990–2023:** a combustion-CO₂ estimate derived from
  International Energy Agency (IEA) energy balances, rather than a published IEA emissions
  series. The calculation is described below.
- **Carbon Monitor, 2019–2024:** the Carbon Monitor Europe daily dataset, summed to complete
  calendar years. See the [source download][carbon-monitor-source].
- **Carbon Monitor excluding bunkers, 2019–2024:** the same data after excluding sectors
  whose labels identify bunkers, aviation, shipping, maritime or marine activity.
- **PRIMAP Energy and Industry, 1990–2023:** PRIMAP-hist version 2.6, scenario HISTCR,
  CO₂ from energy and industrial processes and product use. See the
  [source record][primap-source].
- **PRIMAP Energy and Industry excluding mineral industry, 1990–2023:** the same PRIMAP
  series, with mineral-industry category 2.A subtracted to improve comparability with an
  energy-focused total.

The analysis was produced on 15 July 2026. The CREA cutoff and named releases above define the
main vintages used. The three project-held snapshots do not contain enough metadata to identify a
more precise release, and the Carbon Monitor and IEA inputs do not record an independent retrieval
timestamp in the analytical output. This limits exact reconstruction from provider websites alone.

## EU comparison charts

The EU level chart shows annual fossil CO₂ emissions. The companion trend chart shows the
year-on-year change in each series as a percentage of its value in the previous calendar year.
Trend values are calculated only for consecutive calendar years, so the chart does not bridge gaps
in a source series.

CREA and the Global Carbon Budget are emphasised in colour. Other datasets are shown in grey only
when they have at least 10 annual EU observations in the configured comparison period. This keeps
short series, such as Carbon Monitor, out of the charts while retaining them in the underlying
comparison tables where their available observations can still be inspected.

## Aligning the estimates

External observations are matched to CREA by calendar year and geography. A pair is included only
when both sources report a non-missing value; missing observations are not interpolated or replaced.
EU values supplied directly by Climate Watch, UNFCCC, PIK and PRIMAP are used as published.
GCB and IEA EU totals are formed by summing the 27 Member States. For Carbon Monitor, the EU27
series is reconstructed as its “EU27 & UK” aggregate minus the United Kingdom, then summed
across sectors.

Carbon Monitor months are retained only when they contain observations for every day of the
month, and annual values are retained only when all 12 months are present. For the IEA-derived
series, implausibly small country-year totals that are below 20% of the median of up to five prior
positive years are treated as incomplete and excluded.

The IEA-derived estimate converts energy balances in terajoules to CO₂ for coal, natural gas and
oil using the IPCC emission factors applied by CREA. It starts from total final consumption, then
subtracts energy-sector own use, main-activity and autoproducer electricity, combined heat and
power and heat production, and non-energy use. This is intended to approximate fossil-fuel
combustion without double-counting transformation inputs. Because it uses only three broad fuel
groups and a constructed balance, it should not be interpreted as an official IEA CO₂ total.

## Comparison measures

For each matched year and geography, the absolute difference is:

\[
\text{difference}_{s,g,y} = \text{CREA}_{g,y} - \text{external}_{s,g,y}.
\]

The percentage difference uses the external estimate as the denominator:

\[
\text{percentage difference}_{s,g,y} =
100 \times \frac{\text{CREA}_{g,y} - \text{external}_{s,g,y}}
{\text{external}_{s,g,y}}.
\]

We report the mean signed difference, mean absolute difference, mean absolute percentage
difference and correlation separately for the EU and for the pooled country-year pairs. These are
unweighted arithmetic means: a small country-year has the same weight as a large country-year in
the country summary. EU statistics are based on annual EU totals and are not averages of the
country results.

Trend agreement measures whether CREA and an external dataset show the same sign of change
between successive available observations for a geography. It is the share of comparable changes
for which both rise, both fall or both remain unchanged. A high share indicates similar movement;
it does not show that the level or magnitude of change is accurate. Correlation likewise describes
co-movement and is not a measure of absolute agreement.

## The GCB fuel comparison

GCB receives visual priority because its coal, oil and gas series most closely match CREA's
fossil-fuel accounting. This diagnostic is separate from the headline comparison and charts. It
combines CREA's coal and coke estimates as coal, compares gas directly, and subtracts
international aviation from CREA oil. Totals on both sides are then the sum of coal, oil and gas.
Cement production, flaring and GCB's “other” category are excluded rather than estimated.

This alignment does not remove all conceptual differences. In particular, CREA and GCB may treat
fossil fuels used as feedstocks, and the subsequent oxidation of those non-energy products,
differently. Differences may also reflect activity data, emission factors, revisions and country
allocation. The analysis does not isolate how much of the observed gap is caused by each choice,
so these are possible explanations rather than attributed causes.

## Validation and interpretation

The comparison uses several source types because agreement with one dataset could reflect shared
inputs or definitions. We inspect levels, absolute and percentage differences, correlations and
the direction of annual changes. We also compare CREA and GCB by fuel to identify whether a gap is
concentrated in coal, oil or gas. Every configured external source had usable annual observations
in the July 2026 run.

These checks describe consistency, not total uncertainty. External datasets are themselves
estimated and revised, and several are not independent of one another. Sector coverage differs,
especially for industrial processes and international transport. Country-level missingness also
means that source summaries cover different sets of observations. Stability or close agreement
cannot establish accuracy, while a difference does not by itself show that CREA or the external
source is wrong.

For replication, use the source releases and transformations listed above, the January 2026 CREA
vintage, and the analysis code retained with the results. The run manifest identifies the
base package revision as Git commit
[`9442afefc75b514155e6ee41746c919cc985641d`][code-version]. A commit identifier does not record
uncommitted working-copy changes, so the retained analysis code and working copy are also needed
for exact replication. CREA's underlying EU CO₂ estimation method is documented in the [EU CO₂
tracker methodology][crea-methodology].

[carbon-monitor-source]: https://datas.carbonmonitor.org/API/downloadFullDataset.php?source=carbon_eu
[code-version]: https://github.com/energyandcleanair/creaco2tracker/tree/9442afefc75b514155e6ee41746c919cc985641d
[crea-methodology]: https://energyandcleanair.org/wp/wp-content/uploads/2026/01/CO2-methodology.pdf
[gcb-source]: https://zenodo.org/records/17417124
[primap-source]: https://zenodo.org/records/13752654
