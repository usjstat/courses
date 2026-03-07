library(shiny)

ui <- fluidPage(
  titlePanel("Visualizing Power, Type I and Type II Errors"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", HTML("Significance Level (&alpha;):"), 
                  min = 0.01, max = 0.10, value = 0.05, step = 0.01),
      sliderInput("mu_null", HTML("Null Hypothesis Mean (&mu;<sub>0</sub>):"), 
                  min = 0, max = 100, value = 35),
      sliderInput("mu_alt", HTML("Alternative Hypothesis Mean (&mu;<sub>1</sub>):"), 
                  min = 0, max = 100, value = 65),
      sliderInput("sd", HTML("Standard Deviation (&sigma;):"), 
                  min = 1, max = 30, value = 10)
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      br(),
      htmlOutput("definitions")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    alpha <- input$alpha
    mu_null <- input$mu_null
    mu_alt <- input$mu_alt
    sd <- input$sd
    x <- seq(-50, 120, length.out = 1000)
    
    null_dist <- dnorm(x, mean = mu_null, sd = sd)
    alt_dist <- dnorm(x, mean = mu_alt, sd = sd)
    critical_value <- qnorm(1 - alpha, mean = mu_null, sd = sd)
    
    plot(x, null_dist, type = "l", lwd = 2, col = "blue", 
         ylab = "Density", main = "Power, Type I and Type II Error")
    lines(x, alt_dist, lwd = 2, col = "red")
    abline(v = critical_value, col = "black", lty = 2)
    
    x_alpha <- x[x > critical_value]
    polygon(c(critical_value, x_alpha, max(x_alpha)), 
            c(0, dnorm(x_alpha, mu_null, sd), 0), 
            col = rgb(0, 0, 1, 0.3), border = NA)
    
    polygon(c(critical_value, x_alpha, max(x_alpha)), 
            c(0, dnorm(x_alpha, mu_alt, sd), 0), 
            col = rgb(1, 0, 0, 0.3), border = NA)
    
    x_beta <- x[x < critical_value]
    polygon(c(min(x_beta), x_beta, critical_value), 
            c(0, dnorm(x_beta, mu_alt, sd), 0), 
            col = rgb(1, 0.5, 0, 0.4), border = NA)
    
    legend("topright", 
           legend = c("Null (H0)", "Alternative (H1)", 
                      "Type I Error", "Power", "Type II Error"),
           col = c("blue", "red", rgb(0,0,1,0.3), 
                   rgb(1,0,0,0.3), rgb(1,0.5,0,0.4)),
           pch = c(NA, NA, 15, 15, 15), 
           lty = c(1, 1, NA, NA, NA), pt.cex = 2)
  })
  
  output$definitions <- renderUI({
    HTML("
      <hr>
      <b>Power (1 - &beta;):</b> The probability of correctly rejecting the null hypothesis when the alternative is true.<br>
      This is the <span style='color:red;'>red-shaded</span> area under the alternative distribution beyond the critical value.<br><br>

      <b>Type I Error (&alpha;):</b> The probability of rejecting the null hypothesis when it is actually true.<br>
      This is the <span style='color:blue;'>blue-shaded</span> area under the null distribution beyond the critical value.<br><br>

      <b>Type II Error (&beta;):</b> The probability of failing to reject the null hypothesis when the alternative is true.<br>
      This is the <span style='color:orange;'>orange-shaded</span> area under the alternative distribution left of the critical value.<br><br>

      <b>Tip:</b> Use the sliders above to dynamically change these error regions.
    ")
  })
}

shinyApp(ui = ui, server = server)
