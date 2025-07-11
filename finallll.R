# Load required libraries
library(shiny)
library(dplyr)
library(cluster)
library(arules)
library(ggplot2)
library(readxl)

# User Interface
ui <- fluidPage(
  titlePanel("Data Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset_path", "Choose Excel File"),
      actionButton("clean", "Clean Data"),
      actionButton("visualize", "Generate Visualizations"),
      numericInput("clusters", "Number of Clusters (2-4):", min = 2, max = 4, value = 2),
      actionButton("cluster", "Clustering"),
      actionButton("mine_rules", "Extract Association Rules"),
      numericInput("min_support", "Enter Minimum Support (between 0.001 and 1):", min = 0.001, max = 1, value = 0.001),
      numericInput("min_confidence", "Enter Minimum Confidence (between 0.001 and 1):", min = 0.001, max = 1, value = 0.001)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cleaned Data", tableOutput("cleaned_data")),
        tabPanel("Visualizations",
                 fluidRow(
                   column(6, plotOutput("paymentType_plot")),
                   column(6, plotOutput("age_spending_plot"))
                 ),
                 fluidRow(
                   column(6, plotOutput("city_spending_plot")),
                   column(6, plotOutput("total_spending_boxplot"))
                 )
        ),
        tabPanel("Clustering",
                 tableOutput("cluster_summary_table"),
                 plotOutput("cluster_summary_plot")
        ),
        tabPanel("Association Rules", verbatimTextOutput("association_rules"))
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  # Load Data
  data <- reactiveVal(NULL)
  
  observeEvent(input$dataset_path, {
    req(input$dataset_path)
    data(read_excel(input$dataset_path$datapath, sheet = "in"))
  })
  
  # Data Cleaning (without removing the outlier)
  cleaned_data <- reactive({
    req(data())  # Ensure data is loaded
    cleaned <- na.omit(data())  # Remove missing values
    cleaned <- distinct(cleaned)  # Remove duplicate rows
    cleaned
  })
  
  output$cleaned_data <- renderTable({
    req(input$clean)  # Trigger cleaning when "Clean Data" button is clicked
    cleaned_data()
  })
  
  # Generate Visualizations
  observeEvent(input$visualize, {
    req(cleaned_data())
    
    # Pie chart: Cash vs Credit Spending
    output$paymentType_plot <- renderPlot({
      cash_credit_totals <- aggregate(total ~ paymentType, data = cleaned_data(), sum)
      spending_values <- cash_credit_totals$total
      spending_labels <- cash_credit_totals$paymentType
      
      pie(spending_values,
          labels = paste(spending_labels, round(spending_values / sum(spending_values) * 100, 1), "%"),
          col = c("blue", "green"),
          main = "Cash vs Credit Spending")
      legend("bottomright", legend = spending_labels, fill = c("blue", "green"))
    })
    
    # Bar chart: Total Spending by Age
    output$age_spending_plot <- renderPlot({
      age_totals <- aggregate(total ~ age, data = cleaned_data(), sum)
      barplot(age_totals$total,
              names.arg = age_totals$age,
              col = "deeppink",
              main = "Total Spending by Age",
              xlab = "Age",
              ylab = "Total Spending",
              las = 2)
    })
    
    # Bar chart: Total Spending by City
    output$city_spending_plot <- renderPlot({
      city_totals <- aggregate(total ~ city, data = cleaned_data(), sum)
      city_totals <- city_totals[order(-city_totals$total), ]
      barplot(city_totals$total,
              names.arg = city_totals$city,
              col = "purple",
              main = "Total Spending by City",
              xlab = "City",
              ylab = "Total Spending",
              las = 2)
    })
    
    # Histogram: Distribution of Total Spending
    output$total_spending_boxplot <- renderPlot({
      hist(cleaned_data()$total,
           breaks = 20,
           col = "yellow",
           main = "Distribution of Total Spending",
           xlab = "Total Spending",
           ylab = "Frequency",
           border = "black")
    })
  })
  
  # K-Means Clustering with Original Input Validation
  cluster_summary <- reactive({
    req(cleaned_data())  # Ensure cleaned data is available
    req(input$clusters)  # User-selected number of clusters
    
    # Validate number of clusters (should be between 2 and 4)
    if (input$clusters < 2 || input$clusters > 4) {
      showNotification("Invalid number of clusters. Please select a value between 2 and 4.", type = "error")
      return(NULL)  # Prevent clustering if input is invalid
    }
    
    # Group data by customer and age and calculate total spending
    clusterdata <- cleaned_data() %>%
      group_by(customer, age) %>%
      summarise(newTotal = sum(total), .groups = 'drop')
    
    # Scale the features
    scaled_data <- scale(data.frame(clusterdata$age, clusterdata$newTotal))
    
    # Perform K-means clustering
    kmeans_result2 <- kmeans(scaled_data, centers = input$clusters)
    
    # Combine the cluster assignments with the original data
    cbind.data.frame(clusterdata, cluster_group = kmeans_result2$cluster)
  })
  
  # Render Clustering Table
  output$cluster_summary_table <- renderTable({
    req(input$cluster)  # Trigger clustering when "Cluster" button is clicked
    cluster_summary()
  })
  
  # Render Clustering Plot
  output$cluster_summary_plot <- renderPlot({
    req(cluster_summary())  # Ensure cluster summary data is available
    
    ggplot(cluster_summary(), aes(x = age, y = newTotal, color = factor(cluster_group))) +
      geom_point(size = 3) +
      labs(title = "Customer Clustering", x = "Age", y = "Sum of Total Spending", color = "Cluster Group") +
      theme_minimal()
  })
  
  # Association Rule Mining
  output$association_rules <- renderPrint({
    req(cleaned_data())  # Ensure cleaned data is available
    req(input$mine_rules)  # Trigger rule mining when "Extract Association Rules" button is clicked
    
    # Prepare transactions: assuming cleaned_data() has the 'items' column
    AS <- strsplit(cleaned_data()$items, ",")  # Split the 'items' by commas
    AS <- as(AS, "transactions")  # Convert to 'transactions' format
    
    # Get user inputs for minimum support and confidence
    min_support <- input$min_support
    min_confidence <- input$min_confidence
    
    # Run the Apriori algorithm
    apriori_rules <- apriori(
      AS,
      parameter = list(supp = min_support, conf = min_confidence, minlen = 2)
    )
    
    # Check if any rules are found
    if (length(apriori_rules) > 0) {
      # Print the generated rules
      print("Generated Association Rules:")
      inspect(apriori_rules)
    } else {
      print("No association rules found with the specified parameters.")
    }
  })
}

# Run the App
shinyApp(ui = ui, server = server)