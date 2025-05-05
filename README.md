# Refining FEMA's NFIP V2 Open Flood Insurance Claims Dataset: Comprehensive Enhancements through Data Augmentation and Feature Engineering

This repository contains the full suite of code and data processing pipelines developed for our AGU 2023 poster presentation:

**"Refining FEMA's NFIP V2 Open Flood Insurance Claims Dataset: Comprehensive Enhancements through Data Augmentation and Feature Engineering"**  
📎 [View the AGU iPoster](https://agu23.ipostersessions.com/Default.aspx?s=5D-2E-DC-C5-1A-81-51-79-82-C6-76-67-96-4A-82-D2)

---

## Overview

This project enhances FEMA’s NFIP V2 flood insurance claims dataset through:

- Generation of new geographic units from historical and modern shapefiles  
- Spatial anomaly detection using various machine learning techniques  
- Augmentation of missing location data with heuristics and modeling  
- Crosswalk creation across decades (1980–2020) for geographic consistency  
- Feature engineering for improved flood damage prediction and analysis  

---

## Dataset

This project utilizes the **FIMA NFIP Redacted Claims - Version 2** dataset, provided by FEMA via the OpenFEMA platform. This dataset contains over 2 million flood insurance claims from the National Flood Insurance Program (NFIP), spanning multiple decades. Personally identifiable information has been redacted.

Key fields include:

- Claim amounts (building, contents, ICC)
- Water depth and cause of damage
- Flood zone and community identifiers
- Property characteristics (e.g., number of units, replacement cost)
- Event designation numbers and disaster indicators

The dataset is provided in CSV and Parquet formats and is updated periodically.

**Access the dataset here:**  
[FEMA OpenFEMA - FIMA NFIP Redacted Claims v2](https://www.fema.gov/openfema-data-page/fima-nfip-redacted-claims-v2)

**Note:** FEMA source datasets and shapefiles are not included in this repository.

## Repository Structure

### Data Pipeline and Geographic Unit Creation

- `Data_Pipeline/`: Scripts related to ETL and geographic data processing
- `GeographicUnitGenerationCode.ipynb`, `New_geographic_unit_creation*.ipynb`: Create and validate new geographic units
- `Shapefile_creation_*.ipynb`: Tools for building shapefiles using TIGER/pygris
- `Conversion_code.ipynb`: Coordinate system transformations
- `CensusWaterDepthStatistics.ipynb`: Preliminary water depth exploration

### Location Data Augmentation & Validation

- `Model_location_info/`: Location inference via fractional regression
- `Lat_Long_Rounding_validation_code.R`: Lat/lon rounding precision validation
- `Crosswalk_data_python_code.ipynb`, `Crosswalk data.R`: Time-consistent geographic crosswalk creation
- `Creation_of_geometries_lat_long.ipynb`: Geometry creation for entries with missing coordinates

### Anomaly & Inconsistency Detection

- `Spatial_Anomaly_Detection/`: Spatial anomaly detection tools
- `Anomaly_Detection_using_induction_idea.ipynb`, `Pei_KNN_Anomaly_Detection.ipynb`: KNN and inductive anomaly detection
- `Semi-supervised_detection.ipynb`, `IsolationForestAnomalyDetection.ipynb`: Machine learning-based semi-supervised detection
- `VisualGenerationForAnomalyDetection.ipynb`: Visual outputs for anomaly review

### Inconsistency Analysis

- `Inconsistency_Dataset_*.ipynb`, `Inconsistency_dataset*_new.ipynb`: Detect and fix geographic inconsistencies
- `Inconsistency_Methodology.ipynb`: Summary of methodology used
- `inconsistency_utils.py`: Utility functions for detecting inconsistency and data cleaning

### Wind & Water Modules

- `Wind/`: Wind data modeling scripts
- `WaterDepth_validation.ipynb`, `Water_Depth6csv.ipynb`: Missing water depth validation and imputation

---

## Dependencies

This project uses:

- Python: `pandas`, `geopandas`, `shapely`, `scikit-learn`, `xgboost`, `statsmodels`, `pygris`, `matplotlib`, `seaborn`
- R 

---

## Citation

If you use this codebase, please cite the AGU presentation and/or this GitHub repository.

> "Refining FEMA's NFIP V2 Open Flood Insurance Claims Dataset: Comprehensive Enhancements through Data Augmentation and Feature Engineering"  
> AGU Fall Meeting 2023 Poster  
> [AGU Poster Link](https://agu23.ipostersessions.com/Default.aspx?s=5D-2E-DC-C5-1A-81-51-79-82-C6-76-67-96-4A-82-D2)

---

**Note:** This project is no longer actively maintained. Responses to issues or inquiries may be delayed or unavailable.


