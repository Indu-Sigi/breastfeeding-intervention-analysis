# Breastfeeding Intervention Study Analysis

This repository contains an R-based analysis of a two-phase intervention study examining breastfeeding initiation and duration.

## Overview

The project evaluates two interventions:
- an in-hospital intervention designed to increase breastfeeding initiation
- an at-home intervention aimed at extending breastfeeding duration over a 6-month follow-up period

The analysis includes:
- descriptive statistics
- regression modeling for binary outcomes
- survival analysis for time-to-event outcomes
- interaction and stratified analyses

## Methods

The following approaches were used:
- logistic regression to assess initiation of breastfeeding
- modified Poisson regression with robust standard errors to estimate risk ratios
- Kaplan-Meier curves and log-rank tests for survival analysis
- Cox proportional hazards models for adjusted time-to-event analysis
- interaction and stratified models to assess effect modification

## Files

- `breastfeeding_intervention_analysis.R` — main analysis script
- `README.md` — project overview

## Data

The dataset used for this analysis is not included in this repository.

## Notes

This project was completed as part of a graduate-level coursework experience. The repository is intended to demonstrate data analysis workflow, statistical methods, and reproducible coding practices in R.

## Disclaimer

This repository reflects an independent student analysis and is shared for portfolio purposes.