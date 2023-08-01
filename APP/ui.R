#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Library
library(shiny)
library(shinydashboard)
library(caret)
library(tidyverse)
library(DT)
library(ggplot2)
library(dplyr)
library(tree)

# Load dataset 
tradedata <- read.csv(file = "E:/Statistics/ST558/project 3/effects-of-covid-19-on-trade-at-15-december-2021-provisional.csv")


# Define UI for the app
fluidPage(
  titlePanel( "Provisional effects of covid-19 on trade"),
  
  # Page 1: Three tabs
  tabsetPanel(
    
    ## Tab 1
    tabPanel("About: purpose of the app", h3("The purpose of the app is to check the effect of covid-19 on trade as of December, 2021. In the app, the value and cumulative of trade in different year (from 2015 to 2021) can be checked. The value and cumulative of trade in different direction and transport mode can also be checked. In addition, the value and cumulative of trade in different weekday was also analyzed. ")),
    
    ## Tab 2
    tabPanel("About: data and its source", h3("The data “Effects of COVID-19 on trade: At 15 December 2021 (provisional) is from Newzealand Government. The link to the data is https://www.stats.govt.nz/information-releases/effects-of-covid-19-on-trade-at-15-december-2021-provisional/.  The data is about the effects of COVID-19 on trade of New Zealand’s daily goods trade with the world, which is weekly updated, the present data is released on 15 December 2021. The data was compared with previous years and aimed to show the impacts of COVID-19.")),
    
    ## Tab 3
    tabPanel("About: purpose of each tab", h2("The first page of the APP is about the basic information, that is “About page”. In the About page, the tab “purpose of the app” describes the aim of the APP; The tab “data and its source” describes the data and its source and the link to more information about the data. The tab “purpose of each tab” is about the purpose of each tab (page) of the APP.  The tab “picture related to the data” is about the a picture related to the data, that is the a picture of Newzealand state logo.
The second is Data Exploration page, the third page is Modeling page, the fourth page is the data page. On the Modeling page, Modeling Info tab, Model Fitting tab, and Prediction tab are included.
")),
    
    ## Tab 4
    tabPanel("About: picture related to the data", 
             img(src = "https://www.stats.govt.nz/themes/stats/images/logo.svg", width = "500px", height = "300px")),
    
  ),
  
  # Page 2
  pageWithSidebar(
    headerPanel("Page 2: Data Exploration"),
    sidebarPanel(
      ## graphical summaries
      radioButtons("para", "Graphical summaries", 
                   choices=list("Transport_Mode"="Transport_Mode",
                                "Direction"="Direction", 
                                "Year"="Year",
                                "Weekday"="Weekday",
                                "Transport_Mode_change1"="Transport_Mode_change1",
                                "Direction_change1"="Direction_change1", 
                                "Year_change1"="Year_change1",
                                "Weekday_change1"="Weekday_change1",
                                "Transport_Mode_change2"="Transport_Mode_change2",
                                "Direction_change2"="Direction_change2", 
                                "Year_change2"="Year_change2",
                                "Weekday_change2"="Weekday_change2"),selected="Transport_Mode"),
      br(),
      
      ## numerical summaries
      radioButtons("suma", "Numerical summaries", 
                   choices=list("Value"="Value",
                                "Cumulative"="Cumulative"),selected="Value"),
      ),
    
    
    mainPanel(
      ## main panel content for Page 2
      plotOutput("tradeplot"),
      dataTableOutput("Summary1"), 
      #dataTableOutput("Summary2"),
    )
  ),
  
  # Page 3
  pageWithSidebar(
    headerPanel("Page 3:Modeling page."),
    sidebarPanel(
      ##  Models 
      ## For the user to choose model settings for each model, select the variables used.
      radioButtons("model", "Models fitting", 
                   choices=list("Multiple linear regression"="Multi_linear",
                                "classification tree"="Class_tree", 
                                "random forest model"="random_model"),selected="Multi_linear"),
      br(),
      
       ##  numericInput
      numericInput("percent", "Percent of train", value = 0.75, min = 0, max = 1),
      br(),
      radioButtons("trans", "Transport_Mode", 
                   choices=list("Air"="air",
                                "Sea"="sea", 
                                "All"="all"),selected="air"),
      br(),
      ## Direction        
      radioButtons("direc", "Direction", 
                   choices=list("Exports"="exp",
                                "Imports"="imp", 
                                "Reimports"="rei"),selected="exp"),
      br(),
    
    ),
    
    ## Main panel with three tabs 
    
    mainPanel(
      ### Three tabs
      tabsetPanel(
        ### Tab 1:
        tabPanel("Modeling Info tab", p("Multiple linear regression is a statistical method used to model the relationship between a dependent variable and more than one independent variables.
The advantages of it include:
1)	Can model complex relationships between the dependent variable and two and more independent variables.
2)	The model's predictive power is higher than the simple linear regression, with the consideration of multiple predictors.
The drawback includes:
1)	It assumes the relationship between the variables is linear, and the predictors are not highly correlated. 
2)	it can lead to overfitting when too many independent variables are included in the model.

                                       The multiple linear regression model is represented as: yi=β0+β1xi1+β2xi2+...+βpxip+εi; x1,x2,...,xp: the independent variables (predictors); yi be the value of the dependent variable for the ith observation;xi1,xi2,...,xip be the values of the independent variables for the ith observation. 
                                      Classification tree: Classification tree is a machine learning algorithm and can be used in both classification and regression tasks. The primary goal of a classification tree is to create a predictive model, it can partition the input data into subsets based on the features, ultimately creating a tree-like structure, each internal node represents a decision based on a feature, and each leaf node represents a predicted class or value.
Benefits : The classification tree structure is easy to understand and visualize. 
It can capture complex nonlinear relationships between input features and the target variable without the need for feature engineering or transformations. It’s easy to identify the most relevant features for classification.
Drawbacks of Classification Trees: it can overfit the training data, especially when the tree grows too deep; Even small changes in the data can lead to significantly different tree structures, and making tree is not table.
Random Forest is an ensemble learning technique used for both classification and regression tasks. Random Forests extends idea of bagging, and generally better than bagging. Each tree is constructed using a random subset of the data and a random subset of the features (variables). The final prediction is made by averaging (for regression) or voting (for classification) the predictions from all the individual trees. 
Benefits of Random Forest: provide higher accuracy compared to single decision trees, reduces the risk of overfitting by aggregating predictions from multiple trees, less sensitive to outliers and noisy data, can efficiently handle large datasets.
Drawbacks of Random Forest: It provides valuable insights into feature importance, is not easily interpretable; Random Forest provides valuable insights into feature importance.
")),
        
        ### Tab 2: Output of Tab 2 (I do not know how to do)
        tabPanel("Model Fitting tab", plotOutput("treeplot")),
        
        ### Tab 3:  Output of Tab 3 (I do not know how to do)
        tabPanel("Prediction tab", plotOutput("treeplot2"))
        
        
      )
    )
  ),
  
  # Page 4
  ## Sidebar
  pageWithSidebar(
    headerPanel("Page 4: Data page"),
    sidebarPanel(
      radioButtons("data_page", "Content", 
                   choices=list("Scroll through the data set"="data_set",
                                "Subset this data set column"="data_subsetc", 
                                "Subset this data set row"="data_subsetr"),selected="data_set"),
      br(),
      
          ),
    # mainPanel
    
    mainPanel(
            # Output of main panel content for Page 4 
      dataTableOutput("dataset"), 
    )
  )
  )
  
  
  
     
    
  








