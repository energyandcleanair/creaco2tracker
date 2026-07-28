# Methodology for the revision analysis

## Overview

This analysis tests how much CREA's European Union carbon dioxide (CO₂) estimates
change as more source data become available. We reconstruct the information that would
normally have been available at successive month-ends, rerun the EU CO₂ estimation
method for each of those dates, and compare the resulting annual and first-half estimates
with a later reference estimate. The analysis covers 2020–2024 and uses the same CREA EU CO₂
package for every vintage, changing only which source observations are treated as available.
It measures historical revision behaviour, not every source of uncertainty: the vintages are
reconstructed from source-availability rules rather than a complete archive of every value
published at the time.

## Scope and definitions

The geographical scope is the EU27 aggregate used by the CREA EU CO₂ Tracker. The
analysis uses the central estimate for total fossil-fuel CO₂ emissions across all covered
sectors and fuels. It does not analyse weather-corrected emissions, uncertainty bounds or
individual countries, sectors and fuels. CO₂ means carbon dioxide from fossil-fuel use,
not all greenhouse gases expressed as CO₂ equivalent.

The validation years are 2020, 2021, 2022, 2023 and 2024. Monthly estimates are retained
in tonnes of CO₂ and summed to form:

- an **annual estimate**, covering January to December; and
- a **first-half (H1) estimate**, covering January to June.

Annual totals are assessed from January to December of the following year. H1 totals are
assessed from July of the reporting year to December of the following year. The first
publication points used in the summary statistics are January of the following year for
annual results and September of the reporting year for H1 results. These correspond to
the normal timing of CREA's annual and H1 analyses.

For each reporting year, the **reference estimate** is the estimate produced using data
available at the end of January two years later. For example, the reference for 2024 uses
an information cutoff of 31 January 2026. These reference estimates are later, more mature
and relatively stable: in the historical observations, revisions have generally settled by
this point. They are not treated as final, error-free or the true value.

## Constructing historical vintages

Each vintage applies the same CREA EU CO₂ estimation method, but excludes observations
that would not normally have been published by that month-end. This isolates the practical
trade-off faced by a rapid estimate: recent source data are incomplete, so proxy estimates,
projections and reconciliation play a larger role; later estimates incorporate more reported
energy data.

Availability is represented using conservative typical publication delays.

At each vintage, the masked inputs are passed to the same EU CO₂ package. Its normal treatment
of unavailable recent observations is left unchanged. Monthly output is used, so daily
downscaling does not affect this analysis. The package's data sources and estimation method are
described in the linked EU CO₂ Tracker methodology rather than repeated here.

This is a reconstruction of data availability, not a complete recreation of the databases
as they appeared on every historical date. If a source later revised a value that was already
published, the reconstructed vintage can contain the revised value. The results therefore
primarily measure revisions caused by increasing data coverage and the replacement of CREA
estimates with reported inputs. They may understate or otherwise differ from revisions that
would be observed from a fully archived real-time dataset.

The following rules determine which observations are retained in each historical vintage:

### Daily datasets

Daily observations are assumed to become available after two days. Observations after the
resulting cutoff date are excluded from each historical vintage.

| Dataset | Assumed publication lag |
| --- | --- |
| European Network of Transmission System Operators for Electricity (ENTSO-E) power | 2 days |
| European Network of Transmission System Operators for Gas (ENTSOG) gas flows | 2 days |
| Aggregated Gas Storage Inventory (AGSI) gas storage | 2 days |
| Weather observations | 2 days |

### Monthly datasets

Monthly observations are assigned either a 60-day or 100-day publication lag. Only complete
months up to the resulting cutoff are retained.

| Dataset | Assumed publication lag |
| --- | --- |
| Ember power | 60 days |
| Eurostat industrial production | 60 days |
| Eurostat gas data used for correction | 100 days |
| Eurostat oil data | 100 days |
| Eurostat solid-fuel data | 100 days |
| Eurostat gas data | 100 days |

### Annual datasets

Annual data for reporting year *y* are treated as available only from their assumed publication
month in year *y* + 1.

| Dataset | Assumed availability |
| --- | --- |
| Ember power | February of *y* + 1 |
| Eurostat oil data | July of *y* + 1 |
| Eurostat solid-fuel data | July of *y* + 1 |
| Eurostat gas data | July of *y* + 1 |


## Measuring revisions in emissions levels

Only period totals containing rows for all 12 annual months or all six H1 months are
included. Missing numeric values are omitted when months are summed; the completeness
check is based on the presence of monthly rows and does not separately require every value
to be non-missing.

For reporting year *y*, period *p* and vintage *v*, the signed revision is:

\[
R_{y,p,v} = E_{y,p,v} - E_{y,p,ref}
\]

where \(E_{y,p,v}\) is the estimate at vintage *v* and \(E_{y,p,ref}\) is the January
*y* + 2 reference estimate. A positive value means the earlier estimate was higher than
the reference; a negative value means it was lower.

The relative revision is calculated using the reference estimate as the denominator:

\[
r_{y,p,v} = 100 \times \frac{E_{y,p,v} - E_{y,p,ref}}{E_{y,p,ref}}.
\]

The main chart uses \(|r_{y,p,v}|\), the absolute revision as a percentage of the
reference total. Each validation year has equal status; no weighting is applied across
years. The chart shows individual years and the median across the five validation years.

## Testing when estimates settle

The settling analysis tests thresholds of 0.5%, 1% and 2% of the reference total. For each
period and months-since-publication offset, it first takes the largest absolute revision
observed across the five validation years. An estimate is described as having settled below
a threshold at the first offset for which that maximum remains at or below the threshold
at every subsequent observed offset.

This deliberately conservative definition avoids calling an estimate settled merely because
it falls below a threshold for one month and later moves outside it again. The resulting
month is an empirical description of these five years, not a guaranteed deadline for future
estimates. Structural changes, exceptional shocks, changes in reporting delays or revisions
to major input datasets could produce a different pattern.

## Measuring revisions to trends

For every vintage and reference estimate, the annual or H1 year-on-year change is calculated
against the same period one year earlier:

\[
g_{y,p,v} = 100 \times \left(\frac{E_{y,p,v}}{E_{y-1,p,v}} - 1\right).
\]

The trend revision is the vintage growth rate minus the reference growth rate:

\[
T_{y,p,v} = g_{y,p,v} - g_{y,p,ref}.
\]

It is reported in percentage points, rather than as a percentage of the reference trend.
This avoids unstable ratios when the underlying change is close to zero. A direction switch
is recorded when the vintage and reference growth rates have opposite signs. A value of
exactly zero is not classified as a direction switch. Non-finite growth rates are excluded.

Sensitivity summaries count how often the absolute trend revision reaches at least 0.5,
1 or 2 percentage points at each vintage offset. Counts of direction switches are made once
per year and vintage; repeating the calculation at several thresholds does not create extra
observations.

## Interpretation and limitations

The analysis answers a bounded question: how much would CREA's EU-wide annual and H1
central estimates for 2020–2024 have differed when produced with the source coverage
typically available at successive publication dates? It does not estimate total statistical
uncertainty and does not test whether CREA and the reference estimate are accurate against
an independent truth. Stability is useful evidence that conclusions are less sensitive to
newly arriving data, but stability alone is not proof of accuracy.

Five years are too few to represent every market condition or data disruption, and the
period includes the exceptional energy-demand changes associated with the COVID-19 pandemic
and the European energy crisis. Results should not be assumed to apply unchanged if source
publication schedules, EU coverage, the estimation method or the composition of emissions
changes materially.

For the underlying emissions methodology, see CREA's
[EU CO₂ Tracker methodology](https://energyandcleanair.org/wp/wp-content/uploads/2026/01/CO2-methodology.pdf).
The analysis was produced on 15 July 2026. Its run manifest records base package revision
[`9442afefc75b514155e6ee41746c919cc985641d`](https://github.com/energyandcleanair/creaco2tracker/tree/9442afefc75b514155e6ee41746c919cc985641d).
A commit identifier does not record uncommitted working-copy changes, so the retained analysis
code and working copy are also needed for exact replication.
