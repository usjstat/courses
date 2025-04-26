---
  title: "Interactive Confidence Interval Explorer"
output: html_document
runtime: shiny
---
  
  ```{r setup, include=FALSE}
library(shiny)
library(ggplot2)
```

```{r ui_server, echo=FALSE}
# UI + Server all in embedded Shiny document
fluidPage(
  titlePanel("Confidence Interval Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Sample Size (n):", min = 5, max = 200, value = 30, step = 5),
      sliderInput("conf", "Confidence Level (%):", min = 80, max = 99, value = 95, step = 1),
      actionButton("resample", "Resample Data")
    ),
    
    mainPanel(
      plotOutput("ci_plot"),
      verbatimTextOutput("ci_text")
    )
  )
) -> app_ui

# Define server logic
app_server <- function(input, output, session) {
  population <- rlnorm(10000, meanlog = 11, sdlog = 0.5)
  sample_data <- reactiveVal(sample(population, 30))
  
  observeEvent(input$resample, {
    sample_data(sample(population, size = input$n))
  })
  
  output$ci_plot <- renderPlot({
    smp <- sample_data()
    mean_smp <- mean(smp)
    se <- sd(smp) / sqrt(length(smp))
    alpha <- 1 - input$conf / 100
    z <- qnorm(1 - alpha / 2)
    ci_lower <- mean_smp - z * se
    ci_upper <- mean_smp + z * se
    
    ggplot(data.frame(x = smp), aes(x)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "white") +
      geom_vline(xintercept = mean_smp, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = ci_lower, color = "darkgreen", linetype = "dotted", size = 1) +
      geom_vline(xintercept = ci_upper, color = "darkgreen", linetype = "dotted", size = 1) +
      labs(
        title = paste0(input$conf, "% Confidence Interval"),
        subtitle = paste0("Mean = ", round(mean_smp, 2),
                          " | CI: [", round(ci_lower, 2), ", ", round(ci_upper, 2), "]"),
        x = "Sample Values", y = "Frequency"
      ) +
      theme_minimal()
  })
  
  output$ci_text <- renderPrint({
    smp <- sample_data()
    mean_smp <- mean(smp)
    se <- sd(smp) / sqrt(length(smp))
    alpha <- 1 - input$conf / 100
    z <- qnorm(1 - alpha / 2)
    ci_lower <- mean_smp - z * se
    ci_upper <- mean_smp + z * se
    
    cat("Sample Mean:", round(mean_smp, 2), "\n")
    cat(input$conf, "% Confidence Interval:\n[", round(ci_lower, 2), ",", round(ci_upper, 2), "]")
  })
}

shinyApp(ui = app_ui, server = app_server)
```
