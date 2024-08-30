#install.packages('shiny')
#install.packages('shinydashboard')

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

setwd("C:/Users/chowd/Documents/NEU/2nd quarter/Communications and Visualizations/College_Project/College_Completion_Data")
library(readr)
combined <- read_csv("all_combined.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Enrollment, Graduation and Dropout Analysis", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      selectInput("StartingYear", "StartingYear:", unique(combined$StartingYear), selected = unique(combined$StartingYear[1])),
      selectInput("Borough", "Borough:", unique(combined$Borough), selected = unique(combined$Borough[1]))
    )
  ),
  dashboardBody(
    fluidRow(
      box(width = 2, status = "primary",
          h3(textOutput("enrollment_rate")),
      ),
      box(width = 2, status = "primary",
          h3(textOutput("dropout_rate")),
          
      ),
      box(width = 2, status = "primary",
          h3(textOutput("graduation_rate")),
      ),
      box(width = 3, 
          plotOutput("total_enrollment_race_bar")
      ),
      box(width = 3,
          plotOutput("total_enrollment_gender_donut")
      ),
      box(width = 3,
          plotOutput("total_Dropout_race_bar")
      ),
      box(width = 3,
          plotOutput("total_dropout_gender_donut")
      ),
      box(width = 3,
          plotOutput("total_Graduation_race_bar")
      ),
      box(width = 3,
          plotOutput("total_graduation_gender_donut")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on user inputs for Year and Borough
  filtered_data <- reactive({
    combined[combined$StartingYear == input$StartingYear & combined$Borough == input$Borough, ]
  })
  
  # Display Enrollment rate
  output$enrollment_rate <- renderText({
    paste("Enrollment Rate: ", filtered_data()$Enrollment_rate)
  })
  
  # Display Graduation rate
  output$graduation_rate <- renderText({
    paste("Graduation Rate: ", filtered_data()$Graduation_rate)
  })
  
  # Display Dropout rate
  output$dropout_rate <- renderText({
    paste("Dropout Rate: ", filtered_data()$Dropout_rate)
  })
  
  enrollment_df <- reactive({
    data.frame(Race = c("Asian", "Black", "Hispanic", "White"),
               Enrollment = c(mean((filtered_data()$total_enrollment)*(filtered_data()$asian_per)), 
                              mean((filtered_data()$total_enrollment)*(filtered_data()$black_per)), 
                              mean((filtered_data()$total_enrollment)*(filtered_data()$hispanic_per)), 
                              mean((filtered_data()$total_enrollment)*(filtered_data()$white_per))))
  })
  
  combined$female_per<- 100-combined$male_per
  
  enrollment_df_gender <- reactive({
    data.frame(Gender = c("Male", "Female"),
                                     Enrollment = c(mean((filtered_data()$total_enrollment)*(filtered_data()$male_per)), 
                                                    mean((filtered_data()$total_enrollment)*(filtered_data()$female_per))))
  })
  
  dropout_df_gender <- reactive({
    data.frame(Gender = c("Male", "Female"),
               Dropout = c(mean((filtered_data()$`Dropped Out - % of cohort`)*(filtered_data()$male_per)), 
                              mean((filtered_data()$`Dropped Out - % of cohort`)*(filtered_data()$female_per))))
  })
  
  dropout_df <- reactive({
    data.frame(Race = c("Asian", "Black", "Hispanic", "White"),
               Dropout = c(mean((filtered_data()$`Dropped Out - % of cohort`)*(filtered_data()$asian_per)), 
                              mean((filtered_data()$`Dropped Out - % of cohort`)*(filtered_data()$black_per)), 
                              mean((filtered_data()$`Dropped Out - % of cohort`)*(filtered_data()$hispanic_per)), 
                              mean((filtered_data()$`Dropped Out - % of cohort`)*(filtered_data()$white_per))))
  })
  
  graduation_df_gender <- reactive({
    data.frame(Gender = c("Male", "Female"),
               Dropout = c(mean((filtered_data()$`Total Grads - % of cohort`)*(filtered_data()$male_per)), 
                           mean((filtered_data()$`Total Grads - % of cohort`)*(filtered_data()$female_per))))
  })
  
  graduation_df <- reactive({
    data.frame(Race = c("Asian", "Black", "Hispanic", "White"),
               Dropout = c(mean((filtered_data()$`Total Grads - % of cohort`)*(filtered_data()$asian_per)), 
                           mean((filtered_data()$`Total Grads - % of cohort`)*(filtered_data()$black_per)), 
                           mean((filtered_data()$`Total Grads - % of cohort`)*(filtered_data()$hispanic_per)), 
                           mean((filtered_data()$`Total Grads - % of cohort`)*(filtered_data()$white_per))))
  })
  
  # Display Enrollment rate
  output$enrollment_rate <- renderText({
    paste("Total Enrollment: ", round(mean(filtered_data()$`total_enrollment`)))
  })
  
  # Display Graduation rate
  output$graduation_rate <- renderText({
    paste("Graduation Rate: ", round(mean(filtered_data()$`Total Grads - % of cohort`), 2))
  })
  
  # Display Dropout rate
  output$dropout_rate <- renderText({
    paste("Total Dropout Rate: ", round(mean(filtered_data()$`Dropped Out - % of cohort`), 2))
  })
  
  # Display Total Enrollment by Gender in Donut Chart
  output$total_enrollment_gender_donut <- renderPlot({
    ggplot(enrollment_df_gender(), aes(x = "", y = Enrollment, fill = Gender)) +
      geom_bar(width = 1, stat = "identity") +
      #geom_text(aes(label = "Text Label"), x = "", y = total_enrollment, size = 10)
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("Male" = "Red", "Female" = "Orange")) +
      ggtitle("Total Enrollment by Gender") +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key = element_blank())
  })
  
  # Display Total Dropout by Gender in Donut Chart
  output$total_dropout_gender_donut <- renderPlot({
    ggplot(dropout_df_gender(), aes(x = "", y = Dropout, fill = Gender)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("Male" = "Green", "Female" = "Dark Green")) +
      ggtitle("Dropout Rate by Gender") +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key = element_blank())
  })
  
  # Display Total Graduation Rate by Gender in Donut Chart
  output$total_graduation_gender_donut <- renderPlot({
    ggplot(graduation_df_gender(), aes(x = "", y = Dropout, fill = Gender)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("Male" = "Dark Blue", "Female" = "Pink")) +
      ggtitle("Graduation Rate by Gender") +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
          axis.ticks = element_blank(), 
            plot.title = element_text(hjust = 0.5, size = 18),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key = element_blank())
  })
  
  # Display Total Enrollment by Race in Bar Chart
  output$total_enrollment_race_bar <- renderPlot({
    ggplot(enrollment_df(), aes(x = Race, y = Enrollment, fill = Race)) +
      geom_bar(stat = "identity") +
      xlab("Race") +
      ylab("Enrollment")+
      ggtitle("Total Enrollment by Race")
  })
  
  # Display Total Dropout by Race in Bar Chart
  output$total_Dropout_race_bar <- renderPlot({
    ggplot(dropout_df(), aes(x = Race, y = Dropout, fill = Race)) +
      geom_bar(stat = "identity") +
      xlab("Race") +
      ylab("Dropout Rate")+
      ggtitle("Dropout Rate by Race")
  })
  
  # Display Total Graduation Rate by Race in Bar Chart
  output$total_Graduation_race_bar <- renderPlot({
    ggplot(graduation_df(), aes(x = Dropout, y = Race, fill = Race)) +
      geom_bar(stat = "identity") +
      xlab("Graduation Rate") +
      ylab("Race") +
      ggtitle("Graduation Rate by Race")
  })
  
  # Display Total Dropout by Gender and Race in Stacked Bar Chart
  output$total_dropout_gender_stacked_bar <- renderPlot({
    ggplot(dropout_df(), aes(x = Race, y = Dropout, fill = Gender)) +
      geom_bar(stat = "identity", position = "stack") +
      xlab("Race") +
      ylab("Graduation Rate") +
      ggtitle("Dropout Rate by Race and Gender") +
      scale_fill_manual(values = c("Male" = "Green", "Female" = "Dark Green"))
  })
}

shinyApp(ui, server)
