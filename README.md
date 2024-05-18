# ChatGPT-4 Impact on Market Performance

This repository contains the analysis, code, and visualizations related to the study on the impact of the release of ChatGPT-4 on stock market performance. This project focuses on understanding how significant AI innovations influence the stock prices of companies involved in AI compared to those that are not.

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Problem Definition and Significance](#problem-definition-and-significance)
3. [Target Audience](#target-audience)
4. [Prior Literature](#prior-literature)
5. [Data Source and Preparation](#data-source-and-preparation)
6. [Variables and Measurement](#variables-and-measurement)
7. [Data Cleaning and Preparation](#data-cleaning-and-preparation)
8. [Descriptive Analysis and Visualizations](#descriptive-analysis-and-visualizations)
9. [Statistical Modeling](#statistical-modeling)
10. [Model Selection and Justification](#model-selection-and-justification)
11. [Recommendations](#recommendations)
12. [References](#references)
13. [Appendix](#appendix)

## Executive Summary

This project investigates the impact of the release of ChatGPT-4 on March 14, 2023, on the stock performance of AI-centric companies compared to non-AI companies. Utilizing stock returns data and search engine trends, advanced statistical models were used to analyze abnormal and cumulative abnormal returns around the release date. Significant positive abnormal returns were observed for AI-centric companies, indicating a favorable market reaction to AI advancements.

## Problem Definition and Significance

### Problem Definition

This study aims to evaluate the effects of the ChatGPT-4 release on the equity performance of companies engaged in AI technologies versus those not involved in AI. The focus is on understanding how major technological breakthroughs influence stock prices, reflecting market dynamics in response to AI innovations.

### Significance

The study addresses market sensitivity to AI developments, investor behavior, and comparative impacts on AI-centric versus non-AI companies. Insights gained can aid investors, financial analysts, and corporate managers in making informed decisions and strategizing effectively.

## Target Audience

This analysis is intended for investors, financial analysts, corporate executives, and policymakers. It provides valuable insights into how AI advancements impact stock valuations and market behavior, guiding strategic planning and investment decisions.

## Prior Literature

The study builds on existing research on AI innovations and their impact on financial markets. Previous studies have examined the broad implications of AI on financial systems, the immediate financial repercussions following AI investment announcements, and investor behavior in response to AI developments.

## Data Source and Preparation

### Data Sources

- **Stock Prices:** Daily closing prices were obtained from Yahoo Finance for selected companies.
- **Google Trends:** Data on search interest related to ChatGPT-4 were used to measure public interest.

### Variables and Measurement

- **Dependent Variable:** Cumulative Abnormal Return (CAR)
- **Independent Variables:** lagged_return, after_release, hits (from Google Trends)

### Data Cleaning and Preparation

Data was cleaned and prepared by extracting closing prices, calculating Abnormal Returns (AR) and CAR, and creating model-friendly datasets.

## Descriptive Analysis and Visualizations

### Abnormal Returns

Graphs of abnormal returns for AI-centric and non-AI companies show significant volatility for AI companies around the release date, indicating investor response to AI advancements.

### Cumulative Abnormal Returns

Graphs of cumulative abnormal returns for AI-centric companies show an upward trend post-release, contrasting with the stability or slight decline in non-AI companies.

### Google Search Interests

Google Trends data shows a significant spike in searches for "ChatGPT-4" around its release, correlating with increased volatility in AI stock prices.

## Statistical Modeling

### Panel Linear Models (PLM)

Two PLMs were used: Fixed Effects Model (FE) and Random Effects Model (RE). The FE model controls for individual heterogeneity, while the RE model accounts for random individual effects.

### Linear Mixed-Effects Model (LMER)

This model handles both fixed and random effects, making it flexible for data with multiple levels of correlation.

## Model Selection and Justification

The Fixed Effects Panel Linear Model (PLM) was selected as the optimal model due to its high explanatory power and robust fit, accurately capturing the impact of the ChatGPT-4 release on stock returns.

## Recommendations

- **For Investors:** Diversify portfolios with stocks of companies heavily invested in AI technologies.
- **For Companies:** Time AI-related announcements strategically and engage in proactive investor relations.
- **For Non-AI Companies:** Develop a long-term AI integration strategy to capture future growth.

## References

1. Xie, M. (2019). Development of artificial intelligence and effects on financial system. Journal of Physics. Conference Series, 1187(3), 032084. [DOI:10.1088/1742-6596/1187/3/032084](https://doi.org/10.1088/1742-6596/1187/3/032084)
2. Lui, A. K., Lee, M. C. M., & Ngai, E. W. (2021). Impact of artificial intelligence investment on firm value. Annals of Operation Research, 308(1–2), 373–388. [DOI:10.1007/s10479-020-03862-8](https://doi.org/10.1007/s10479-020-03862-8)

## Appendix

The appendix contains the R code used for data analysis and visualizations, including scripts for calculating abnormal and cumulative abnormal returns and plotting the results.
