# PlateData
![development status](https://img.shields.io/badge/status-under_development-orange)
![GitHub Downloads (all assets, all releases)](https://img.shields.io/github/downloads/OliverDietrich/microtiter-plate-analysis-toolkit/total)

R package for the analysis of microtiter plate-based data. Data must be attributable to a specific well (e.g. A1) in a plate (e.g. P1). The PlateData object stores the layout and data as data.frames connected by an index column. The plate type (e.g. 6-well, 24-well, 96-well is determined automatically and stored as 'type').

## Installation
There is no official release version yet, since the package is under development.

You can install the development version from GitHub using the `remotes` package:
```
remotes::install_github("OliverDietrich/microtiter-plate-analysis-toolkit@main")
```

> [!WARNING]
> Early development, no stable features.

## Documentation
![PlateData](img/overview.png | width = 80)

## ToDO
- [ ] Get example data for import methods
  - [ ] Layout
  - [ ] Tekan Spark
- [ ] Write tutorial for simple timecourse experiment (design + analysis)
- [ ] Create development branch to stabilize main
