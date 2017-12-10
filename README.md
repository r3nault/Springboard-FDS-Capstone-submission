# Springboard-FDS-Capstone-submission
Springboard Foundations of Data Science Capstone project - final submission

# Project description
This analysis is a proof-of-concept using data science techniques in a sports analytics context; to predict and interpret winning factors in Australian-rules Football League (AFL) from player performance metrics.

## Sourcing raw data
**Player performance metrics** - used as independent x variables - were scraped with code.

- Code: *code-01-scrape-xvars-playermetrics.R*
- Saved data: *player_metrics.Rda*

**Team results** - used as dependent y variable - were manually copied and formatted initially in Excel. Some wrangling was done in R to interpret the scores from the unique AFL string format of goals.behinds

- Excel prep: *match-results-prep.xlsx*
- Code: *code-02-prepare-yvars-matchresults.R*
- Saved data: *team_results.Rda*

## Data wrangling, Exploratory Analysis and Feature Engineering
Steps required to merge raw data into meaningful data frames, perform visualisation and initial statistical analysis for exploration of data relationships and generate indices (features) of the data to facilitate deeper analysis.

- Code: *code-03-datawrangling-EDA.R*

## Statistical analysis of indices
Steps to run two statistical tests of the engineered indices to find their significance.

- Code: *code-04-index-statistical-analysis.R*



# Final reports
Note- R markdown files use the css file: *ioslides.css*

## Data wrangling

- **Data wrangling report - Rmd**: *fds-capstone-jameshooi-AFL-020-data-wrangling.Rmd*
- **Data wrangling report - html**: *fds-capstone-jameshooi-AFL-020-data-wrangling.html*

## Technical write-up

- **Technical report - Rmd**: *fds-capstone-jameshooi-AFL-040-report.Rmd*
- **Technical report - html**: *fds-capstone-jameshooi-AFL-040-report.html*

## Non-technical write-up (client version)

- **High level report - pptx**: *fds-capstone-jameshooi-AFL-report-final.pptx*
- **High level report - pdf**: *fds-capstone-jameshooi-AFL-report-final.pdf*
