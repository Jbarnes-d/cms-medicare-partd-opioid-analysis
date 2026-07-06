# cms-medicare-partd-opioid-analysis

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Healthcare Analytics](https://img.shields.io/badge/Healthcare-Analytics-blue?style=for-the-badge)
![GIS](https://img.shields.io/badge/GIS-Spatial%20Analysis-green?style=for-the-badge)
![CMS Data](https://img.shields.io/badge/Data-CMS%20Medicare-orange?style=for-the-badge)

## Project Overview

This project analyzes opioid prescribing patterns among Medicare Part D beneficiaries in the United States using publicly available Centers for Medicare & Medicaid Services (CMS) data.

The analysis examines national prescribing trends between **2013 and 2022**, identifies geographic variation at the state and county levels, and uses geographic information systems (GIS) to visualize opioid prescribing rates through county-level choropleth maps.

This project was completed as part of my Master of Public Health (Health Informatics) capstone and demonstrates practical healthcare analytics using real-world public health data.

---

## Objectives

- Analyze national opioid prescribing trends over time
- Compare prescribing rates across U.S. states and counties
- Visualize geographic disparities using GIS mapping
- Explore spatial patterns in opioid prescribing
- Demonstrate reproducible healthcare analytics using R

---

## Technologies Used

- R
- dplyr
- ggplot2
- sf
- tigris
- plotly
- readr
- tidyr
- CMS Medicare Part D Public Use Files

---

## Skills Demonstrated

- Healthcare Data Analytics
- Public Health Informatics
- Data Cleaning
- Data Wrangling
- Exploratory Data Analysis
- Geographic Information Systems (GIS)
- Spatial Data Analysis
- Data Visualization
- Statistical Programming in R
- Healthcare Research
- Technical Documentation

---

## Repository Structure

```
cms-medicare-partd-opioid-analysis
│
├── data
│
├── figures
│   ├── charts
│   └── maps
│
├── report
│
├── scripts
│
└── README.md
```

---

## Data Sources

The analysis uses publicly available datasets from the Centers for Medicare & Medicaid Services (CMS), including:

- Medicare Part D Public Use Files
- Geographic Prescribing Rate Files

County boundary shapefiles were obtained using the **tigris** R package.

No protected health information (PHI) or individually identifiable patient data were used.

---

## Example Analyses

This repository includes:

- National opioid prescribing trends (2013–2022)
- State prescribing rate comparisons
- County prescribing rate comparisons
- Top prescribing counties
- Top prescribing states
- County-level choropleth maps for every U.S. state
- Interactive geographic visualizations

---

## Key Findings

- Medicare Part D opioid prescribing rates declined steadily over the study period.
- Geographic variation exists across both states and counties.
- Several rural counties demonstrated substantially higher prescribing rates than national averages.
- County-level data suppression by CMS resulted in some counties appearing gray on choropleth maps where data were unavailable.

---

## Running the Analysis

1. Download the CMS Medicare Part D Public Use Files.
2. Place the datasets into the `data` folder.
3. Open `scripts/medicare_partd_opioid_analysis.R` in RStudio.
4. Install the required R packages.
5. Run the script to reproduce the analyses and visualizations.

---

## Report

The complete capstone research paper is available in the `report` folder and describes the study methodology, statistical analyses, findings, discussion, and limitations.

---

## About Me

**Josiah Barnes**

Master of Public Health (Health Informatics)

Bachelor of Science in Biology

Interested in:

- Healthcare Data Analytics
- Clinical Informatics
- Population Health
- Quality Improvement
- Public Health Research

---

## Disclaimer

This repository was developed for educational and research purposes using publicly available CMS data. The analyses and conclusions presented here are those of the author and do not represent the views of the Centers for Medicare & Medicaid Services.
