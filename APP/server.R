#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library
library(shiny)
library(shinydashboard)
library(caret)
library(tidyverse)
library(DT)
library(ggplot2)
library(dplyr)
library(tree)

# Server

shinyServer(function(input, output) {
  
  # Load dataset (replace "my_dataset.csv" with your dataset's file path)
  tradedata <- read.csv(file = "E:/Statistics/ST558/project 3/effects-of-covid-19-on-trade-at-15-december-2021-provisional.csv")
  
  
 # Page 2: Data Exploration page
   ## Import data
  output$tradeplot <- renderPlot({
    selectedpara <- input$para
    
  ## If Transport_Mode  
    if (selectedpara == "Transport_Mode") {
      ### Total_Value  
      aggregated_data <- tradedata %>%
        group_by(Transport_Mode) %>%
        summarise(Total_Value = sum(Value))
      
      ### Plot the histogram
      
      ggplot(aggregated_data, aes(x = Transport_Mode, y = Total_Value)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Total Value by Transport_Mode", x = "Transport_Mode", y = "Total Value")
          } 
    
    ## If Direction
    else if (selectedpara == "Direction") {
      ## T#otal_Value  
      
      aggregated_data <- tradedata %>%
        group_by(Direction) %>%
        summarise(Total_Value = sum(Value))
      
      ### Plot the histogram
      
      ggplot(aggregated_data, aes(x = Direction, y = Total_Value)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Total Value by Direction", x = "Direction", y = "Total Value")
    }
    
    ## If year
    else if (selectedpara == "Year") {
      ### Total_Value  
      
      aggregated_data <- tradedata %>%
        group_by(Year) %>%
        summarise(Total_Value = sum(Value))
      
   ### Plot the histogram
      
      ggplot(aggregated_data, aes(x = Year, y = Total_Value)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Total Value by Year", x = "Year", y = "Total Value")
    }
    ## If weekday
    else if (selectedpara == "Weekday") {
      ## #Total_Value 
      
        aggregated_data <- tradedata %>%
        group_by(Weekday) %>%
        summarise(Total_Value = sum(Value))
      
  ### Plot the histogram
      
      ggplot(aggregated_data, aes(x = Weekday, y = Total_Value)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Total Value by Weekday", x = "Weekday", y = "Total Value")
    }
    
    ## Change 1: change the type of plot shown and type of summary reported
     
    ### If Transport_Mode_change1
    else if (selectedpara == "Transport_Mode_change1") {
      ### Total_Value 
      aggregated_data <- tradedata %>% filter(Transport_Mode != "All")%>%
        group_by(Transport_Mode) %>%
        summarise(Total_Value = sum(Value))
      
    ### Plot the histogram
      
      ggplot(aggregated_data, aes(x = Transport_Mode, y = Total_Value)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Total Value by Transport_Mode", x = "Transport_Mode", y = "Total Value")
    } 
    
    ## If Direction_change1
    else if (selectedpara == "Direction_change1") {
      ### Total_Value
      
      aggregated_data <- tradedata %>% filter(Direction != "Reimports") %>%
        group_by(Direction) %>%
        summarise(Total_Value = sum(Value))
      
      ### Plot
      
      ggplot(aggregated_data, aes(x = Direction, y = Total_Value)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Total Value by Direction", x = "Direction", y = "Total Value")
    }
    
    ### Year_change1
    else if (selectedpara == "Year_change1") {
      ### Total_Value
      
      aggregated_data <- tradedata %>% filter(Year != 2020) %>%
        group_by(Year) %>%
        summarise(Total_Value = sum(Value))
      
      ### Plot
      
      ggplot(aggregated_data, aes(x = Year, y = Total_Value)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Total Value by Year", x = "Year", y = "Total Value")
    }
    
    ### If Weekday_change1
    else if (selectedpara == "Weekday_change1") {
      # Total_Value
      
      aggregated_data <- tradedata %>% filter(Weekday != "Sunday") %>%
        group_by(Weekday) %>%
        summarise(Total_Value = sum(Value))
      
      ### Plot the histogram
      
      ggplot(aggregated_data, aes(x = Weekday, y = Total_Value)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Total Value by Weekday", x = "Weekday", y = "Total Value")
    }
    
    
  ### change 2: Change the variables and filter the rows to change the data in the plots/summaries
  
  ### If Transport_Mode_change2
    else if (selectedpara == "Transport_Mode_change2") {
    # Total_Value 
    aggregated_data <- tradedata %>%
      group_by(Transport_Mode) %>%
      summarise(Total_Value = sum(Value))
    
    ### Plot the histogram
    
    ggplot(aggregated_data, aes(x = Transport_Mode, y = Total_Value)) +
      geom_point() +
      labs(title = "Total Value by Transport_Mode", x = "Transport_Mode", y = "Total Value")
  } 
  
  ### If Direction_change2
    
  else if (selectedpara == "Direction_change2") {
    ### Total_Value
    
    aggregated_data <- tradedata %>%
      group_by(Direction) %>%
      summarise(Total_Value = sum(Value))
    
    ### Plot the histogram
    
    ggplot(aggregated_data, aes(x = Direction, y = Total_Value)) +
      geom_point() +
      labs(title = "Total Value by Direction", x = "Direction", y = "Total Value")
  }
  
  ### if Year_change2
    
  else if (selectedpara == "Year_change2") {
    # Total_Value 
    
    aggregated_data <- tradedata %>%
      group_by(Year) %>%
      summarise(Total_Value = sum(Value))
    
    ### Plot the histogram
    
    ggplot(aggregated_data, aes(x = Year, y = Total_Value)) +
      geom_point() +
      labs(title = "Total Value by Year", x = "Year", y = "Total Value")
  }
  ### if Weekday_change2
    
  else if (selectedpara == "Weekday_change2") {
    ### Plot
    
    aggregated_data <- tradedata %>%
      group_by(Weekday) %>%
      summarise(Total_Value = sum(Value))
    
    ### Plot the histogram
    
    ggplot(aggregated_data, aes(x = Weekday, y = Total_Value)) +
      geom_point() +
      labs(title = "Total Value by Weekday", x = "Weekday", y = "Total Value")
  }
  })
   
  # Page 2: Numeric summaries
    
  output$Summary1 <-   DT::renderDataTable({
    selectedsuma <- input$suma
    if (selectedsuma == "Value")  
    {tradedata           } 
    else if (selectedsuma == "Cumulative")  
    {tradedata
    }
  })
  
  #output$Summary2 <-   DT::renderDataTable({
      #selectedsuma <- input$suma
      #if (selectedsuma == "Value")  
       # {summa1 <- summarise(tradedata$Value)
       # summa1            } 
      #else if (selectedsuma == "Cumulative")  
      #  {summa2 <-summarise(tradedata$Cumulative)
      #  summa2
     # }
 # })
    
   
  # page 3 
  
  output$treeplot <- renderPlot({
    
    # Fit multiple regression model with Sales as the response variable
    treeFit <- tree(Cumulative ~ Value + Direction+Transport_Mode , data = tradedata)
    
    plot(treeFit)
    text(treeFit)
  })
  
  output$treeplot2 <- renderPlot({
    
    # Fit multiple regression model with Sales as the response variable
    treeFit2 <- tree(Cumulative ~ ., data = tradedata)
    
    plot(treeFit2)
    text(treeFit2)
  })
  
  
  # Page 3: Modeling page
  ## Drop the Date
  ##tradedata2 <- subset(tradedata, select = -c(Date, Measure))
  
  ##observeEvent(input$percent , {
    ## Split the data into a training and test set
    
    ##train <- sample(1:nrow(tradedata), size = nrow(tradedata)*input$percent)
    ##test <- dplyr::setdiff(1:nrow(tradedata), train)
    
    ## training and testing subsets
    
    ##train_Data <- tradedata[train, ]
   ## test_Data <- tradedata[test, ]
  ##})
  
  # Multiple linear regression 
  ## Convert categorical data "Region" into dummy variables using dummy coding (one-hot encoding), aims to fit multiple regression model
  ##data_dummies <- train_Data %>%
  ##  mutate(
    ##  Air = ifelse(Transport_Mode == "Air", 1, 0),
    ##  Sea = ifelse(Transport_Mode == "Sea", 1, 0),
    ##  All = ifelse(Transport_Mode == "All", 1, 0),
    ##  Exports = ifelse(Direction == "Exports", 1, 0),
    #3  Imports = ifelse(Direction == "Imports", 1, 0),
    ##  Reimports = ifelse(Direction == "Reimports", 1, 0),
  ##  )
  
  ## Fit multiple regression model 
  
  ##lmFit1 <- train(Cumulative ~ Value + Air + Sea + All, data = data_dummies, method = "lm")
  
 ## lmFit2 <- train(Cumulative ~ Value + Exports + Imports + Reimports, data = data_dummies, method = "lm")
  
 ## lmFit3 <- train(Cumulative ~ Value + Air + Sea + All + Exports + Imports + Reimports, data = data_dummies, method = "lm")
  
  ## The results will be shown in Model Fitting tab of page 3
  
 ## lmFit1
 ## lmFit2
 ## lmFit3
  # models on the test set
  
 ## predlm1 <- predict(lmFit1, newdata = test_Data)
 ## predlm2 <- predict(lmFit2, newdata = test_Data)
 ## predlm3 <- predict(lmFit3, newdata = test_Data)
  
  # classification tree
  ##create text info
 ## output$clatree <- renderPlot({
    # Fit classification tree 
  ##  treeFit <- tree(Cumulative ~ Value + Direction+Transport_Mode, data = train_Data)
  ##  plot(treeFit)
  ##  text(treeFit)
    
 ## })
  ## model on the test set
 ## predtreeFit <- predict(treeFit, newdata = test_Data)
  
  # random forest model
  
 ## rfFit <- randomForest(Cumulative ~ ., data = train_Data)
 ## predrft <- predict(rfFit, newdata = dplyr::select(test_Data, -Cumulative ))
  
  ## summary of Fit multiple regression model 
  
 ## summary(lmFit1) 
 ## summary(lmFit2) 
 ## summary(lmFit3) 
 ## summary(treeFit) 
 ## summary(rfFit) 
  
  ## Prediction tab:
##  predlm1 <- predict(lmFit1, newdata = test_Data)
##  predlm2 <- predict(lmFit2, newdata = test_Data)
##  predlm3 <- predict(lmFit3, newdata = test_Data)
##  predtreeFit <- predict(treeFit, newdata = test_Data)
##  predrft <- predict(rfFit, newdata = dplyr::select(test_Data, -Cumulative ))
  
  # page 4
    
    output$dataset <-   DT::renderDataTable({
      selectedpage <- input$data_page
      if (selectedpage == "data_set")  
      {tradedata } 
      else if (selectedpage == "data_subsetc")  
      {tradedata3 <- subset(tradedata, select = -c(Date))
      tradedata3}
      else if (selectedpage == "data_subsetr")  
      {filter_data <- tradedata %>% filter(Year != 2015) 
      filter_data}
          })
    
     
    })
    

# Save the (possibly subsetted) data as a file (.csv is fine but whatever you’d like)
# write.csv(filter_data, "filter_data.csv", row.names = FALSE)
 
