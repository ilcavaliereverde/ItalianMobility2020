# Italian mobility changes, 2020-2021

<br>

Google Mobility data was made available for COVID-19 research purposes, but it lacks displayability.  
This dashboard solves this issue by plotting Google Mobility data with a fair level of personalisation.  

### How it works

This dashboard is built as a `R` `shiny` app. It downloads Google Mobility data, processes it and renders plots according to user preferences.  
Source code is available [here](https://github.com/ilcavaliereverde/ItalianMobility2020/); this project is licensed under the [MIT license](./license.Rmd). 

### Data 

Google harvests mobility data and compiles it into `.csv` files, as explained in the [COVID-19 Community Mobility Reports](https://www.google.com/covid19/mobility/).  
You can download the `.csv` [here](https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv) (**warning**: very large file, over 200 MB). 
Google Mobility data is updated weekly.  

### Plots 

Withing this dashboard, you can choose to plot:
* different start-end dates;
* different types of mobility change;
* different Italian regions and provinces.  

Plots are rendered as `.png` files and can be easily downloaded by right-clicking on the plot.

### Interpretation 

In order to correctly interpret plots, please read the concise [Google Guide to Community Reports](https://support.google.com/covid19-mobility/answer/9824897?hl=en-GB&ref_topic=9822927).  
Only a few of several notes: 

* change is relative to a baseline (Jan-Feb 2020 period); 
* baseline data is biased; 
* data has gaps; 
* variables are not fully comparable.  

<br>

&copy; Edoardo Giovannini 2020-2021