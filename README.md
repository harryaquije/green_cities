# Green Areas Analysis: London & Lima

This repository contains the code and resources for the geospatial analysis of urban green areas in London and Lima. The analysis supports the article *“What Does Green Access Look Like in Your City?”*.

---

## Project Structure
scripts/ <- R scripts for analysis
│ ├── 01_london_v1.R
│ ├── 01_london_v2.R
│ ├── 01_lima_v1.R
│ ├── 02_london_v1.R
│ ├── 02_lima_v1.R
│ ├── 03_sta_map_v1.R
│ ├── 03_inte_map_v1.R
│ └── utils.R <- Helper functions
├── figures/ <- Output plots and maps
└── docs/ <- Optional, additional documentation

---

## Data Sources

- **ONS**: Office for National Statistics, London green areas data  
- **OMS**: Open Map Services / OpenStreetMap, for geospatial mapping  
- **Note**: Only non-sensitive and shareable data is included in this repository. Any personal or confidential information has been removed.

---

## Getting Started

### Requirements

- R (version ≥ 4.2)
- R packages: `osmdata`, `sf`, `dplyr`, `units`, `ggplot2`, `ggspatial`, `here`

Install required packages using:

```r
install.packages(c("osmdata", "sf", "dplyr", "units", "ggplot2", "ggspatial", "here"))

## Authors & Contact

**Harry Aquije Ballon**   
Email: [harry.aquije@gmail.com](mailto:harry.aquije@gmail.com)
