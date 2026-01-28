# CREA CO2 Tracker
The EU CO2 Emission Tracker is an initiative by the Centre for Research on Energy and Clean Air (CREA) to produce timely and publicly available data on Europe’s CO2 emissions. By monitoring and analyzing emissions across the power sector, transport, industry, and buildings, the CO2 Tracker aims to help decision-makers, researchers, and the wider society understand the latest trends and respond effectively to climate challenges.


The project welcomes collaboration with partners—be they government bodies, think tanks, civil society organizations, or academic institutions.

Live charts are available on [CREA's website](https://energyandcleanair.org/product/eu-co2-emission-tracker/).


## TO DO
[ ] scale monthly power generation data to yearly values (the latter is more accurate and can be significantly different)

## Release Notes

### Version 1.1
Version used for January 2026 Report.

#### Changes
- Power generation now taken as a mix of ENTSOE and EMBER
- Peat and oil for electriciy are now considered
- EMBER Other fossil used for as potential predictor and downscaler for peat/oil electricity
- added a IEA shared NCV option (all countries sharing the same NCV values)
- International transport excluded from comparison with GCB


### Version 0.9
Version used for 31 March 2025 Report.

#### Changes
- National level data is now available for all sectors
- Peat and shale oil restored
- Improved data imputation


### Version 0.6
#### Changes
- Added transportation sector (oil only)

### Version 0.5
#### Changes
- Added confidence interval in the projection of most recent data

### Version 0.4

#### Changes
- Improved handling of partial data in EUROSTAT datasets (e.g., availability of coal data for electricity but not for industrial uses)
- Utilized industrial production data to estimate coal and coke consumption in non-electricity sectors

#### Impact
This update eliminates the previously observed sudden drop in coal emissions at the end of 2023.
