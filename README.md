# ST558---Project-3

Creating a Shiny App
Authors: Yejun Han
Date: July 31, 2023

### Brief description of the app and its purpose

The app is about the effect of covid-19 on trade as of December, 2021 in new Zealand. 
In the app, the value and cumulative of trade in different year (from 2015 to 2021)， the value and cumulative of trade in different direction and transport mode, and the value and cumulative of trade in different weekday was be found. 
    

### A list of packages needed to run the app

shiny,
shinydashboard
caret
tidyverse
DT
ggplot2
dplyr
tree


### A line of code that would install all the packages used (so we can easily grab that and run it prior to running your app)

install.packages("shiny")
install.packages("shinydashboard")
install.packages("caret")
install.packages("tidyverse")
install.packages("DT")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tree")

library(shiny)
library(shinydashboard)
library(caret)
library(tidyverse)
library(DT)
library(ggplot2)
library(dplyr)
library(tree)


### The shiny::runGitHub() code that we can copy and paste into RStudio to run your app

shiny::runGitHub(username = "johnsmith", repository = "my-shiny-app")




