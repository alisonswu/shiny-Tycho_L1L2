## A shiny app for exploring Project Tycho data at L1 and L2 level 

data credit: [Project Tycho](https://www.tycho.pitt.edu)

app author: [Alison Wu](swu11@ncsu.edu)


## Usage

You will need RStudio and internet connection to run the app. Click [here](https://www.rstudio.com/home/) to install RStudio.

To launch the app, open RStudio and run the following code. 

```R
# install shiny package if not found
if(!"shiny" %in% installed.packages()){install.packages("shiny")}

library(shiny)
runGitHub("shiny-Tycho_L1L2","alisonswu")
```

  
