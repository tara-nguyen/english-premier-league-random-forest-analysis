# Analysis of Match Statistics and Team Performances in the Premier League From Season 2015/16 to Season 2019/20

- AUTHOR: [**TARA NGUYEN**](https://www.linkedin.com/in/taranguyen264/)
- Final project for the course *Introduction to Data Science* at UCLA Extension
- Completed in December 2020

## Abstract

**Background**: The [English Premier League](https://www.premierleague.com/) (EPL), the top level of competition in English soccer, is one of the most popular and most competitive sport events in the world.

**Data and research question**: In this project I analyzed match statistics (results, number of goals, passing, shooting, etc.) and team performances in the EPL from the 2015/2016 season to the 2019/2020 season. The main research question was: **_Which statistics are the most predictive of match results?_**

**Method and findings**: The entire project was done in R. Using the random forest algorithm, I found three features that were the highly important in predicting match results: how many goals the home team scored, how many goals the away team scored, and which team scored first. Other sub-questions such as "Is there really a home-team advantage?" and "Which team formation was the most/least effective?" were also answered along the way.

For a complete report, see the [wiki page](https://github.com/tara-nguyen/english-premier-league-random-forest-analysis/wiki).

## List of files and directory in the repo

[`Plots`](Plots) - directory for plots created during data visualization

`README.md` - this document you are currently reading

[`epldat5seasons_Analysis.R`](epldat5seasons_Analysis.R) - main R script for data wrangling, visualization, and statistical analyses

[`epldat5seasons_WebScraping.R`](epldat5seasons_WebScraping.R) - R script for scraping original match statistics from webpages

[`matchstats.csv`](matchstats.csv) - final data set
- **IMPORTANT NOTE**: If you wish to open the file, please make a copy first and open the copy. **Do NOT open the original file** because that might distort the format of some of the values in the file.

## Usage Note

The dataset and R scripts are free for download and use, **provided that proper credit is given**.

If you mention or use any part of my research report, please provide a link to this repo.
