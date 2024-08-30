library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

setwd("C:/Users/chowd/Documents/NEU/2nd quarter/Communications and Visualizations/College_Project/College_Completion_Data")
library(readr)
combined <- read_csv("all_combined.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Analysis on Socio Economic Conditions", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      selectInput("features", "Features:",
                  c("Total Grads - % of cohort", "Dropped Out - % of cohort", "total_enrollment"))
    ),
    sidebarMenu(
      selectInput("StartingYear", "StartingYear:", unique(combined$StartingYear), selected = unique(combined$StartingYear[1])),
      selectInput("Borough", "Borough:", unique(combined$Borough), selected = unique(combined$Borough[1]))
    )
      ),
  dashboardBody(
    fluidRow(
      box(width = 2, status = "primary",
          h3(textOutput("frl_percent")),
      ),
      fluidRow(
        box(width = 4, height = "500ppx"
            plotOutput("frl_plot"),
        ),
        box(width = 4,
            plotOutput("sped_plot")
        ),
        box(width = 2, status = "primary",
            h3(textOutput("sped_percent")),
        ),
      box(width = 2, status = "primary",
          h3(textOutput("ell_percent")),
          
      ),
      box(width = 4,
          plotOutput("ell_plot"),
      )
    )
  )
)
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    combined[combined$StartingYear == input$StartingYear & combined$Borough == input$Borough, ]
  })

  combined$frl_bin <- cut(combined$frl_percent, breaks = c(0, 20, 40, 60, 80, 100),
                          labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))
  combined$ell_bin <- cut(combined$ell_percent, breaks = c(0, 20, 40, 60, 80, 100),
                          labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))
  
  combined$sped_bin <- cut(combined$sped_percent, breaks = c(0, 10, 20, 30),
                          labels = c("0-10%", "10-20%", "20-20%"))
  output$frl_plot <- renderPlot({
    # Filter the data based on the input
    filtered_data_frl <- filtered_data()
    filtered_data_frl <- filtered_data_frl[!is.na(filtered_data_frl[["frl_bin"]]), ]
    ggplot(filtered_data_frl, aes(x = frl_bin, y = filtered_data_frl[[input$features]], fill = frl_bin)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Free Lunch %") +
      ylab(" Feature") +
      ggtitle("Feature vs. Free Lunch %") +
      theme_classic()
  })
  
  output$ell_plot <- renderPlot({
    # Filter the data based on the input
    filtered_data_ell <- filtered_data()
    filtered_data_ell <- filtered_data_ell[!is.na(filtered_data_ell[["ell_bin"]]), ]
    ggplot(filtered_data_ell, aes(x = filtered_data_ell[[input$features]], y = ell_bin, fill = ell_bin)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("% of Students Learning English") +
      ylab(" Feature") +
      ggtitle("Feature vs. % of Students Learning English") +
      theme_classic()
  })
  
  output$sped_plot <- renderPlot({
    # Filter the data based on the input
    filtered_data_sped <- filtered_data()
    filtered_data_sped <- filtered_data_sped[!is.na(filtered_data_sped[["sped_bin"]]), ]
    ggplot(filtered_data_sped, aes(x = sped_bin, y = filtered_data_sped[[input$features]], fill = sped_bin)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("% of Students Recieving Specialized Education") +
      ylab(" Feature") +
      ggtitle("Feature vs. % of Students Recieving Specialized Education") +
      theme_classic()
  })
  
  # Display frl
  output$frl_percent <- renderText({
    paste("Total Free Lunch %: ", round(mean(filtered_data()$`frl_percent`), 2))
  })
  
  # Display Graduation rate
  output$ell_percent <- renderText({
    paste("Total English Learning %: ", round(mean(filtered_data()$`ell_percent`), 2))
  })
  
  # Display Dropout rate
  output$sped_percent <- renderText({
    paste("Specialized Education %: ", round(mean(filtered_data()$`sped_percent`), 2))
  })
}

shinyApp(ui, server)
