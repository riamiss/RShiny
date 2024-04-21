# Load necessary libraries
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(knitr)
library(tidyr)

# Set working directory
setwd("...")

# Load data
occ_data <- read.csv("occ.csv")
soc_data <- read.csv("soc.csv")

# Merge data frames
merged_data <- merge(occ_data, soc_data, by = "soc6_id", all = TRUE)

# Remove NA values
merged_data <- na.omit(merged_data)


ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        .plot-margin {
          margin-bottom: 70px;
        }
        .table-margin {
          margin-bottom: 70px;
        }
      ")
    )
  ),
  titlePanel("Exposure to Technology in the Labor Market"),
  sidebarLayout(
    sidebarPanel(
      selectInput("industry", "Choose Type of Occupation", choices = c("No Selection", unique(merged_data$soc2_label))),
      selectInput("job", "Choose a Job", choices = "No Selection"),
      selectInput("tech", "Choose Technology", choices = c('No Selection', 'AI', 'Robot', 'Software'), selected = 'No Selection')
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.industry != 'No Selection' && input.job == 'No Selection' && input.tech != 'No Selection'",
        plotlyOutput("scatter_plot"),
      ),
      conditionalPanel(
        condition = "input.industry == 'No Selection' && input.job == 'No Selection' && input.tech == 'No Selection'",
        div(plotOutput("density_plot"), class = "plot-margin"),
        div(plotlyOutput("weighted_mean_graph"), class = "plot-margin")
      ),
      verbatimTextOutput("job_info"),
      div(class = "table-margin", tableOutput("ai_robot_software_exposure")),  # Add margin after table
      plotlyOutput("correlation_scatter"),
      plotlyOutput("rc_scores")
    )
  )
)




server <- function(input, output, session) {
  
  # Filter jobs based on selected industry
  observeEvent(input$industry, {
    if (input$industry != "No Selection") {
      jobs <- unique(merged_data$soc6_label[merged_data$soc2_label == input$industry])
    } else {
      jobs <- c("No Selection")
    }
    updateSelectInput(session, "job", choices = c("No Selection", jobs), selected = "No Selection")
  })
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    data <- merged_data
    if (input$industry != "No Selection") {
      data <- data[data$soc2_label == input$industry, ]
    }
    if (input$job != "No Selection") {
      data <- data[data$soc6_label == input$job, ]
    }
    if (input$tech != "No Selection") {
      data <- data[data$technology_column == input$tech, ]
    }
    data
  })
  
  
  #---------------------when type of occupation and job are chosen---------------------------
  # Render employment, wage, and skills requirement based on selected job if both type of occupation and job are chosen,
  # otherwise render plot based on filtered data
  
  #technology automatically set to no selection when job and type of occupation selected
  observeEvent(c(input$industry, input$job), {
    if (input$industry != "No Selection" && input$job != "No Selection") {
      updateSelectInput(session, "tech", selected = "No Selection")
    }
  })
  
  output$job_info <- renderText({
    data <- filtered_data()
    if (input$industry != "No Selection" && input$job != "No Selection" && input$tech == "No Selection" && !is.null(data) && nrow(data) > 0) {
      paste("Employment Size: ", format(round(data$employment, 2), nsmall = 2), "\n",
            "Wage: $", format(round(data$wage, 2), nsmall = 2), " per hour", "\n",
            "Skills Requirement: ", unique(data$req), sep = "")
    } else if (input$industry != "No Selection" && input$job == "No Selection" && input$tech == "No Selection") {
      ""
    }
  })
  
  
  #--------------------------when type of occupation and technology are chosen-------------------
  # Scatter plot of AI exposure vs. wages with employment size as point size
  output$scatter_plot <- renderPlotly({
    if (input$industry != "No Selection" && input$tech == "AI") {
      filtered_data <- merged_data %>%
        filter(soc2_label == input$industry)
      
      # Create scatter plot
      plot <- ggplot(filtered_data, aes(x = ai, y = wages, size = employment)) +
        geom_point(alpha = 0.5) +
        scale_size_continuous(name = "Employment Size", breaks = c(100, 500, 1000), labels = c("100", "500", "1000")) +
        labs(title = "AI Exposure vs. Wages",
             x = "AI Exposure",
             y = "Wages") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5, face = "bold"), # Set title size, center it, and make it bold
          axis.title = element_text(face = "bold"), # Make axis labels bold
          legend.title = element_text(face = "bold"), # Make legend title bold
          legend.text = element_text(face = "bold"), # Make legend text bold
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank()   # Remove minor grid lines
        )
      
      # Convert ggplot to plotly for interactivity
      ggplotly(plot)
    } else if (input$industry != "No Selection" && input$tech == "Robot") {
      filtered_data <- merged_data %>%
        filter(soc2_label == input$industry)
      
      # Create scatter plot for Robot technology
      plot <- ggplot(filtered_data, aes(x = robot, y = wages, size = employment)) +
        geom_point(alpha = 0.5) +
        scale_size_continuous(name = "Employment Size", breaks = c(100, 500, 1000), labels = c("100", "500", "1000")) +
        labs(title = "Robot Exposure vs. Wages",
             x = "Robot Exposure",
             y = "Wages") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5, face = "bold"), # Set title size, center it, and make it bold
          axis.title = element_text(face = "bold"), # Make axis labels bold
          legend.title = element_text(face = "bold"), # Make legend title bold
          legend.text = element_text(face = "bold"), # Make legend text bold
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank()   # Remove minor grid lines
        )
      
      # Convert ggplot to plotly for interactivity
      ggplotly(plot)
    } else if (input$industry != "No Selection" && input$tech == "Software") {
      filtered_data <- merged_data %>%
        filter(soc2_label == input$industry)
      
      # Create scatter plot for Software technology
      plot <- ggplot(filtered_data, aes(x = software, y = wages, size = employment)) +
        geom_point(alpha = 0.5) +
        scale_size_continuous(name = "Employment Size", breaks = c(100, 500, 1000), labels = c("100", "500", "1000")) +
        labs(title = "Software Exposure vs. Wages",
             x = "Software Exposure",
             y = "Wages") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5, face = "bold"), # Set title size, center it, and make it bold
          axis.title = element_text(face = "bold"), # Make axis labels bold
          legend.title = element_text(face = "bold"), # Make legend title bold
          legend.text = element_text(face = "bold"), # Make legend text bold
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank()   # Remove minor grid lines
        )
      
      # Convert ggplot to plotly for interactivity
      ggplotly(plot)
    }
  })
  
  # Additional scatter plot functions for Robot and Software technologies
  # Output for Robot scatter plot
  output$robot_scatter_plot <- renderPlotly({
    if (input$industry != "No Selection" && input$tech == "Robot") {
      filtered_data <- merged_data %>%
        filter(soc2_label == input$industry)
      
      # Create scatter plot for Robot technology
      plot <- ggplot(filtered_data, aes(x = robot, y = wages, size = employment)) +
        geom_point(alpha = 0.5) +
        scale_size_continuous(name = "Employment Size", breaks = c(100, 500, 1000), labels = c("100", "500", "1000")) +
        labs(title = "Robot Exposure vs. Wages",
             x = "Robot Exposure",
             y = "Wages") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5, face = "bold"), # Set title size, center it, and make it bold
          axis.title = element_text(size = 15, face = "bold"), # Make axis labels bold
          legend.title = element_text(face = "bold"), # Make legend title bold
          legend.text = element_text(face = "bold"), # Make legend text bold
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank()   # Remove minor grid lines
        )
      
      # Convert ggplot to plotly for interactivity
      ggplotly(plot)
    }
  })
  
  # Output for Software scatter plot
  output$software_scatter_plot <- renderPlotly({
    if (input$industry != "No Selection" && input$tech == "Software") {
      filtered_data <- merged_data %>%
        filter(soc2_label == input$industry)
      
      # Create scatter plot for Software technology
      plot <- ggplot(filtered_data, aes(x = software, y = wages, size = employment)) +
        geom_point(alpha = 0.5) +
        scale_size_continuous(name = "Employment Size", breaks = c(100, 500, 1000), labels = c("100", "500", "1000")) +
        labs(title = "Software Exposure vs. Wages",
             x = "Software Exposure",
             y = "Wages") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5, face = "bold"), # Set title size, center it, and make it bold
          axis.title = element_text(size = 15, face = "bold"), # Make axis labels bold
          legend.title = element_text(face = "bold"), # Make legend title bold
          legend.text = element_text(face = "bold"), # Make legend text bold
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank()   # Remove minor grid lines
        )
      
      # Convert ggplot to plotly for interactivity
      ggplotly(plot)
    }
  })
  
  
  
  
  
  
  
  
  #------------------------when no selection is made across all categories-----------------
  # Density plot across all the scores for the types of jobs (industry) when no selection is made
  output$density_plot <- renderPlot({
    if (input$industry == "No Selection" && input$job == "No Selection" && input$tech == "No Selection") {
      # Create a dataframe with all scores for each industry
      all_scores <- merged_data %>%
        group_by(soc2_label) %>%
        summarise(mean_nrm = mean(nrm),
                  mean_rm = mean(rm),
                  mean_rc = mean(rc),
                  mean_nri = mean(nri),
                  mean_nra = mean(nra))
      
      # Convert the data to long format for plotting
      all_scores_long <- pivot_longer(all_scores, 
                                      cols = c(mean_nrm, mean_rm, mean_rc, mean_nri, mean_nra),
                                      names_to = "Score", values_to = "Mean_Score")
      
      # Plot the density plot
      ggplot(all_scores_long, aes(x = Mean_Score, fill = Score)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density Plot of Mean Scores Across Types of Jobs", x = "Mean Score", y = "Density") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5, face = "bold"), # Set title size, center it, and make it bold
          axis.title = element_text(size = 15, face = "bold"), # Make axis labels bold
          legend.title = element_text(face = "bold"), # Make legend title bold
          legend.text = element_text(face = "bold"), # Make legend text bold
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank()   # Remove minor grid lines
        ) +
        scale_fill_manual(values = c("mean_nrm" = "lightcoral", "mean_rm" = "lightblue", "mean_rc" = "lightgreen", "mean_nri" = "lightpink", "mean_nra" = "orange"),
                          labels = c("Non-Routine Manual", "Routine Manual", "Routine Cognitive", "Non-Routine Interpersonal", "Non-Routine Analytical")) # Change legend labels and colors
    }
  })
  
  
      
  
  
  # Weighted mean of each industry for each technology (stacked bar chart)
  output$weighted_mean_graph <- renderPlotly({
    if (input$industry == "No Selection" && input$job == "No Selection" && input$tech == "No Selection") {
      # Compute weighted mean for each industry and each technology
      weighted_means <- merged_data %>%
        group_by(soc2_label) %>%
        summarise(ai_weighted_mean = weighted.mean(ai),
                  robot_weighted_mean = weighted.mean(robot),
                  software_weighted_mean = weighted.mean(software))
      
      # Reshape data for plotting
      weighted_means_long <- pivot_longer(weighted_means, cols = -soc2_label, names_to = "Technology", values_to = "Weighted_Mean")
      
      # Plot the stacked bar chart
      plot_ly(data = weighted_means_long, x = ~soc2_label, y = ~Weighted_Mean, color = ~Technology, type = "bar", 
              hovertemplate = "<b>%{x}</b><br>%{y}<extra></extra>") %>%
        layout(title = list(text = "<b> Exposure of AI, Robot, and Software Across Industries <b>", x = 0.5),
               xaxis = list(title = "<b> Industry <b>", tickangle = 315),
               yaxis = list(title = "<b> Weighted Mean Exposure <b>"),
               barmode = "stack",
               hovermode = "closest",
               height = 1500,
               width = 1000) 
    }
  })
  
  # Automatically set job to "No Selection" when both industry and tech are selected
  observe({
    if (input$industry != "No Selection" && input$tech != "No Selection") {
      updateSelectInput(session, "job", selected = "No Selection")
    }
  })
  
  #-----------------when ONLY type of occupation is chosen--------------------------
  
  # Table of overall exposure of AI, robots, and software across jobs in the chosen industry
  output$ai_robot_software_exposure <- renderTable({
    if (input$industry != "No Selection" && input$job == "No Selection" && input$tech == "No Selection") {
      mean_scores <- filtered_data() %>% 
        group_by(soc6_label) %>% 
        summarize(Job = unique(soc6_label), 
                  `AI Exposure Score` = mean(ai), 
                  `Robot Exposure Score` = mean(robot), 
                  `Software Exposure Score` = mean(software)) %>%
        select(-soc6_label)
    }
  })
  
  
  # Bar graph showing the routine cognitive task scores across the different jobs for the chosen industry
  output$rc_scores <- renderPlotly({
    if (input$industry != "No Selection" && input$job == "No Selection" && input$tech == "No Selection") {
      filtered_data() %>%
        plot_ly(x = ~soc6_label, 
                y = ~rc, 
                type = "bar",
                hovertemplate = "Job: %{x} <br> Score: %{y} <extra></extra>"
        ) %>% 
        layout(
          title = list(text = "<b>Routine Cognitive Task Scores Across Jobs</b>", x = 0.5, y = 1.1),
          xaxis = list(title = "<b>Jobs</b>", tickangle = 315, titlefont = list(size = 14, color = "black", family = "Arial, sans-serif")),
          yaxis = list(title = "<b>Routine Cognitive Task Score</b>", titlefont = list(size = 14, color = "black", family = "Arial, sans-serif")),
          height = 800,
          annotations = list(
            list(
              text = "Source: Acemoglu and Autor, 2011",
              xref = "paper",
              yref = "paper",
              x = 1,
              y = -0.75,
              showarrow = FALSE,
              font = list(size = 12)
            )
          )
        )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
