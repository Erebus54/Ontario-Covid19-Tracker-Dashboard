Ontario-Covid19-Tracker-Dashboard
================

-   [Overview](#overview)
-   [Setup](#setup)
    -   [R](#r)
    -   [Data](#data)
        -   [Git](#git)
        -   [Manually download repositories](#manually-download-repositories)
-   [Running/Vieweing the App](#runningvieweing-the-app)
    -   [Within Rstudio](#within-rstudio)
    -   [Command line](#command-line)
        -   [Using R Console](#using-r-console)
        -   [Using Shell](#using-shell)
    -   [Website](#website)
-   [About](#about)


Overview
========
Flexdashboard shiny app for displaying stats about Ontario covid19 pandemic response provincially and regionally. 

![](images/Capture_01.PNG)
![](images/Capture_02.PNG)
![](images/Capture_03.PNG)
![](images/Capture_04.PNG)

Setup
=====

R
-
Packages to run Data Downloader 
`install.packages(c('lubridate', 'data.table', 'dplyr', 'beepr'))`

Packages to run Flexdashboard
`install.packages(c('flexdashboard', 'rsconnect', 'dplyr', 'tidyverse', 'lubridate', 'zoo', 'data.table', 'plotly'))`

Data
----
This repository depends on two datasets from [Ontario Open Data](https://data.ontario.ca/). You need to set up a folder named 'datasets' in your directory, the datadownloader will find the folder and download the data to it. Currently it is set up to read the contents of that folder and update by deleting and then writing the new files to disk. 

The two datasets needed are as follows: 

[status-of-covid-19-cases-in-ontario](https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario) -- provides case specific data per region with demographic data, acquisition type, and outcome 

[confirmed-positive-cases-of-covid-19-in-ontario](https://data.ontario.ca/en/dataset/confirmed-positive-cases-of-covid-19-in-ontario) - provides a high level summary of daily test statistics, hospitalizations, and long term care homes 

### Git

If you know your way around git, go to a clean directory that is not already in version control and run

``` bash
git clone https://github.com/Erebus54/Ontario-Covid19-Tracker-Dashboard.git
```

Website
-------

The app is currently deployed here: <https://patrickschnurbusch.shinyapps.io/ON_Dashboard/>
