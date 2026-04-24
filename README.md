# Data Checking Workflow

## Overview

This repository contains a workflow for identifying potential data issues in survey data by comparing current (edit) data from Oracle to historical records. The goal is to flag records that may require review during survey operations.

The workflow focuses on **four types of outliers**:

-   **Length outliers** – Unusually large or small lengths relative to historical distributions.

-   **Weight outliers** – Unusual specimen weights based on species-specific distributions.

-   **Specimen outliers** – Unusual length–weight relationships.

-   **Range outliers** – Unusual spatial occurrences based on historical data (using clustering).

> [!IMPORTANT]
>
> These flags are **not errors**. They are intentionally sensitive and are meant to highlight records for human review.

## Repository Structure

Plaintext

```         
├── run.R                  # Main script to run entire workflow 
├── code/ 
│   ├── 00_download_data.R 
│   ├── 01_clean_data.R 
│   ├── 02_specimen_checks.R 
│   ├── 03_range_checks.R 
│   └── functions.R 
├── data/ 
│   └── oracle/            # Cached Oracle tables (auto-created) 
├── output/                # Output files (auto-created) 
└── README.md 
```

## Requirements

### R Packages

The workflow requires several specialized packages for data manipulation, spatial analysis, and database connectivity.

```         
install.packages(c( 
  "dplyr", "tidyr", "purrr", "readr", "janitor",  
  "stringr", "ggplot2", "ggforce", "dbscan",   
  "mgcv", "googlesheets4", "RODBC", "here" 
)) 
```

## Setup

### 1. Download the Repository

-   **Option 1: Clone with Git**

    ```         
    git clone https://github.com/SarahFriedman-NOAA/data_checking 
    ```

-   **Option 2: Download ZIP**

    -   Click **Code** → **Download ZIP** on GitHub.

    -   Extract the contents locally.

### 2. Open in R

-   Open the project in **RStudio** (recommended), or

-   Set your working directory to the repository root.

### 3. Oracle Connection

The workflow requires access to Oracle data. It will attempt to:

1.  Use a local script (e.g., `ConnectToOracle.R`), or

2.  Use `gapindex::get_connected()`.

**Note:** If no connection is established, cached data in `data/oracle/` will be used if available.

## Running the Workflow

Run the entire pipeline with a single command:

```         
source("run.R") 
```

**This script will perform the following actions:**

-   Download missing or stale Oracle tables (or use cached data).

-   Clean and merge datasets.

-   Run outlier detection checks.

-   Generate outputs and visualizations.

-   Update the Google Sheet with new outliers (you may be prompted to authenticate in your browser).

## Outputs

All outputs are written automatically to the following locations:

### Local (`output/` folder)

|  |  |
|----|----|
| **Output Type** | **Description** |
| **CSV File** | Contains all flagged outliers for offline review. |
| **PDF Diagnostic Plots** | Visualizations for length/weight distributions, relationships, and spatial ranges. |

### Google Sheet

-   New outliers are appended for tracking.

-   **Users can:** Mark records as checked and add notes on resolution.

## Usage Guidelines

### Recommended Usage

-   Run the workflow **periodically** during the survey season.

-   Coordinate with field teams (e.g., Deck Leads) to resolve flagged issues.

-   Use the **Google Sheet** to track review status, avoid duplicate effort, and document decisions.

### Caching Behavior

To improve speed, Oracle tables are cached in `data/oracle/`. The workflow re-downloads missing or stale tables while reusing current cached tables to keep data up to date without unnecessary downloads.

## Troubleshooting

-   **Oracle connection issues:** Ensure your VPN is active and credentials are correct. Try re-running the connection script or `gapindex::get_connected()`.

-   **Missing data errors:** Delete the `data/oracle/` folder and re-run `run.R` to force a fresh download.

-   **Google Sheets authentication:** Ensure you follow the browser prompt to grant permissions when running the main script.

## This code is primarily maintained by:

**Sarah Friedman** (Sarah.Friedman AT noaa.gov; [\@SarahFriedman-NOAA](https://github.com/SarahFriedman-NOAA))

Alaska Fisheries Science Center (AFSC) National Oceanic and Atmospheric Administration (NOAA)\
Resource Assessment and Conservation Engineering Division (RACE)\
Groundfish Assessment Program (GAP) 7600 Sand Point Way, N.E. bldg. 4\
Seattle, WA 98115 USA

# Suggestions and comments

If the data or metadata can be improved, please [submit an issue to the code’s repository](https://github.com/SarahFriedman-NOAA/data_finalization/issues).

# NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

# NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

# NOAA Fisheries

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) \| [NOAA Fisheries](https://www.fisheries.noaa.gov/)
