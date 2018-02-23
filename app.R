#install.packages("shiny")
#install.packages("flexclust")

setwd ("C:/Users/rraj/Documents/Studies/Hacathon")

library(shiny)
library(flexclust)

data <- read.csv ("survey_data_subset_Shiny.csv")


survey_data_subset_Scaled <- scale(subset(data, select= c(1:3,8,12), center=TRUE, scale=TRUE))
cl1 = kcca(survey_data_subset_Scaled[, c(1:5)], k=6, kccaFamily("kmeans"))


ui<-fluidPage(
  titlePanel("Enter your details for healthy diabetes free living! "),
  
  fluidRow(
    
    column(3,
           numericInput("age" , h3("Age") , value = 40)),
    
    column(3,
           numericInput("ht" , h3("Heigh (in cm)") , value = 145)),
    
    column(3,
           numericInput("wt" , h3("Weight (in Kg)") , value = 75)),
    
    column(3,
           numericInput("sleep" , h3("Sleep Hours") , value = 7)),
    
    column(3,
           numericInput("income" , h3("Annual Household Income (in $)") , value = 15000))
    
    
  ),
  
  fluidRow(
    actionButton("submit",h3("See Recommendations"))
  ),
  
  
  
  mainPanel(textOutput("selected_wt"),
            textOutput("clusterRuntime"),
            textOutput("BMI"),
            textOutput("AlcRec"),
            textOutput("slpHours"),
            textOutput("Smoke")
  )
  
  
)


server <-function(input , output)
{
  
  columns <- cbind("Age", "Height(CM)", "Weight(Kg)", "SleepHours" , "Household Annual Income")
  test_input <- cbind(67, 149.86, 90.7184, 7, 5000)
  colnames(test_input) <- columns
  
  values <- reactiveValues()
  values$df <- test_input
  addData <- observe({
    if (input$submit > 0)
    {
      
      newLine <- isolate(c(input$age,input$ht,input$wt,input$sleep,input$income))
      # update your data
      isolate(values$df <- rbind(as.matrix(values$df), unlist(newLine)))
      
      #df1 <- scale(values$df, center=TRUE, scale=TRUE)
      
      pred_test <- predict (cl1, newdata = values$df)
      
      
      clus_Var <- pred_test[nrow(values$df)]
      
      var_BMI <- ifelse(clus_Var == 1 , "19-20" , ifelse(clus_Var == 2 , "25-26" , ifelse(clus_Var == 3, "20-21" , ifelse(clus_Var == 4, "21-22" 
                                                                                                                    , 
                                                                                                                    ifelse( clus_Var ==5 
                                                                                                                            , "18-19", "22-23"
                                                                                                                    ) ) ) ) )
      
      var_Alc <- ifelse(clus_Var == 1 , "1-2" , ifelse(clus_Var == 2 , "2-3" , ifelse(clus_Var == 3, "2-3" , ifelse(clus_Var == 4, "1-2" 
                                                                                                                    , 
                                                                                                                    ifelse( clus_Var ==5 
                                                                                                                            , "2-3", "1-2"
                                                                                                                    ) ) ) ) )
      
      var_Slp <- ifelse(clus_Var == 1 , "7-8" , ifelse(clus_Var == 2 , "5-6" , ifelse(clus_Var == 3, "6-7" , ifelse(clus_Var == 4, "7-8" 
                                                                                                                    , 
                                                                                                                    ifelse( clus_Var ==5 
                                                                                                                            , "6-7", "7-8"
                                                                                                                    ) ) ) ) )
      
      var_Smoke <- ifelse(clus_Var == 1 , " 65 " , ifelse(clus_Var == 2 , "40" , ifelse(clus_Var == 3, "65" , ifelse(clus_Var == 4, "85" 
                                                                                                                    , 
                                                                                                                    ifelse( clus_Var ==5 
                                                                                                                            , "75", "72"
                                                                                                                    ) ) ) ) )
      
      
      output$clusterRuntime <- renderText({
        paste("You belong to Cluster" , pred_test[nrow(values$df)])
        
      })
      
      output$BMI <- renderText({
        paste("Non Diabetic people similar to you maintain their BMI in the range of" , var_BMI , ".") })
      
      output$AlcRec <- renderText({
      paste("Non Diabetic people similar to you consume" , var_Alc , "alcoholic beverages in last 30 days.") })
        
      output$slpHours <- renderText({
          paste("Non Diabetic people similar to you sleep for " , var_Slp , "hours everyday on an average.") })  
      
      output$slpHours <- renderText({
        paste(var_Smoke , " % of Non Diabetic people similar to you don't smoke at all") }) 
      
     
      
    }
  })
}

shinyApp(ui= ui , server = server)