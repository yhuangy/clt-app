# Load packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(gridExtra)

# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Central Limit Theorem (CLT)", windowTitle = "CLT for means"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        # Select distribution ----
        radioButtons(
          inputId = "dist",
          label   = "Population distribution",
          choices = c("Normal" = "rnorm",
                      "Bernoulli (coin flip)" = "rbinom",
                      "Right skewed" = 'rlnorm'),
          selected = "rnorm"
        ),
        
        hr(),
        
        # Distribution parameters / features ----
        uiOutput("mu"),
        uiOutput("sd"),
        uiOutput("coin"),
        uiOutput("skew"),
        
        # Select sample size ----
        sliderInput("n", "Sample size:", value = 30, min = 2, max = 500),
        br(),
        
        # Number of samples ----
        helpText(tags$small(style = "color: gray;", "(Used for sampling distribution only)")),
        sliderInput("k", "Repeated samples:", value = 1000, min = 10, max = 1000),
        
        
        # Information ----
        br(),
        HTML(paste0(
          "Developed for <strong>DSE1101</strong>: Introduction to Data Science for Economics, ",
          "with adaptations from <a href='https://www.openintro.org/book/ims/' target='_blank'>IMS</a> ",
          "(Introduction to Modern Statistics)."
        ))
      ) 
    ),
  
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # First tab ----
        tabPanel(
          title = "Population Distribution",
          plotOutput("pop.dist", height = "500px"),
          br(),
          conditionalPanel(
            condition = "input.dist == 'rlnorm'",
            helpText(
              HTML(
                "Note: The right-skewed distribution above is generated from a ",
                "<a href='https://en.wikipedia.org/wiki/Log-normal_distribution' target='_blank'>lognormal distribution</a>."
              )
            )
          )
        ),
        # Second tab ----
        tabPanel(
          title = "Samples",
          br(),
          plotOutput("sample.dist", height = "600px"),
          div(h3(textOutput("num.samples")), align = "center"),
          br()
        ),
        # Third tab ----
        tabPanel(
          title = "Sampling Distribution",
          fluidRow(
            column(
              width = 6, br(), br(),
              div(textOutput("CLT.descr"), align = "justify")
            ),
            column(
              width = 6, br(),
              plotOutput("pop.dist.two", width = "85%", height = "200px")
            )
          ),
          fluidRow(
            column(
              width = 12, br(),
              plotOutput("sampling.dist"),
              div(textOutput("sampling.descr", inline = TRUE), align = "center")
            )
          )
        ),
        # Forth tab ---
        tabPanel("Base R Code",
                 h3("Generate a Single Sample"),
                 verbatimTextOutput("repro_code"),
                 h3("Repeated Sampling"),
                 verbatimTextOutput("repro_code1"),
                 h3("Visualize the sampling distribution"),
                 verbatimTextOutput("repro_code2")
        )
      )
    )
  )
)

# Define server function --------------------------------------------
#seed <- as.numeric(Sys.time())
seed <- 101

server <- function(input, output, session) {
  
  # Normal params ----
  output$mu <- renderUI({
    if (input$dist == "rnorm") {
      sliderInput("mu", "Mean:", value = 0, min = -40, max = 50)
    }
  })
  
  output$sd <- renderUI({
    if (input$dist == "rnorm") {
      sliderInput("sd", "Standard deviation:", value = 10, min = 1, max = 20)
    }
  })
  
  # Bernoulli param (probability) ----
  output$coin <- renderUI({
    if (input$dist == "rbinom") {
      sliderInput("p", "Probability of heads (p):",
                  value = 0.5, min = 0.05, max = 0.95, step = 0.01)
    }
  })
  
  # skew slider for rlnorm and rbeta ----
  output$skew = renderUI(
    {
      
      if (input$dist == "rlnorm"){
        selectInput(inputId = "skew",
                    label = "Skewness:",
                    choices = c("Moderately skew" = "low",
                                "Highly skewed" = "high"),
                    selected = "high")
      }
    })
  
  # RNG helper ----
  rand_draw <- function(dist, n, mu, sd, prob = NULL, skew = NULL) {
    if (dist == "rnorm") {
      
      req(mu, sd)
      return(rnorm(n, mean = mu, sd = sd))
      
    } else if (dist == "rbinom") {
      
      req(prob)
      return(rbinom(n, size = 1, prob = prob))  # Bernoulli( p )
      
    } else if (dist == "rlnorm"){
      
      req(skew)
      if (skew == "low"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=.5))
      } else if (skew == "high"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=1))
      }
    }
  }
  rep_rand_draw <- repeatable(rand_draw)
  
  # Population (large) ----
  parent <- reactive({
    n_sample <- 1e6
    rep_rand_draw(input$dist, n_sample, input$mu, input$sd, input$p, input$skew)
  })
  
  # Samples matrix: k columns, each column is a sample of size n ----
  samples <- reactive({
    pop <- parent()
    replicate(input$k, sample(pop, input$n, replace = TRUE))
  })
  
  # Code to reproduce one sample
  output$repro_code <- renderText({
    seed_line <- sprintf("set.seed(%s)", seed)
    
    if (input$dist == "rnorm") {
      paste(
        seed_line,
        sprintf("n  <- %d", input$n),
        sprintf("mu <- %s", signif(input$mu, 6)),
        sprintf("sd <- %s", signif(input$sd, 6)),
        "rnorm(n, mean = mu, sd = sd)",
        sep = "\n"
      )
    } else if (input$dist == "rlnorm") {
      paste(
        seed_line,
        sprintf("n <- %d", input$n),
        "rlnorm(n, meanlog = 0, sdlog = 0.5)     # lognormal distribution",
        sep = "\n"
      )
    } else { # Bernoulli (coin flip)
      paste(
        seed_line,
        sprintf("n <- %d", input$n),
        sprintf("p <- %s", signif(input$p, 6)),
        "rbinom(n, size = 1, prob = p)",
        sep = "\n"
      )
    }
  })
  
  # Repeated sampling
  output$repro_code1 <- renderText({
    #fmt <- function(x) formatC(x, digits = 6, format = "fg", flag = "#")
    seed_line <- sprintf("set.seed(%s)", seed)
    
    if (input$dist == "rnorm") {
      paste(
        sprintf("k  <- %d", input$k),
        "sample_means <- numeric(k)",
        "for (i in 1:k) {",
        "  samples <- rnorm(n, mean = mu, sd = sd)",
        "  sample_means[i] <- mean(samples)",
        "}",
        "sample_means",
        sep = "\n"
      )
    } else if (input$dist == "rlnorm") {
      paste(
        seed_line,
        sprintf("k <- %d", input$k),
        "sample_means <- numeric(k)",
        "for (i in 1:k) {",
        "  samples <- rlnorm(n, meanlog = 0, sdlog = 0.5)",
        "  sample_means[i] <- mean(samples)",
        "}",
        "sample_means",
        sep = "\n"
      )
    } else { # Bernoulli (coin flip)
      paste(
        sprintf("k <- %d", input$k),
        "sample_means <- numeric(k)",
        "for (i in 1:k) {",
        "  samples <- rbinom(n, size = 1, prob = p)",
        "  sample_means[i] <- mean(samples)",
        "}",
        "sample_means",
        sep = "\n"
      )
    }
  })
  
  # Plotting
  output$repro_code2 <- renderText({
    paste(
      'plot(density(sample_means),',
      '     main = "Sampling Distribution (x_bar)",',
      '     xlab = "Sample means")',
      sep = "\n"
    )
  })
  
  # --- Plots --------------------------------------------------------
  
  # Population plot (tab 1) ----
  output$pop.dist <- renderPlot({
    distname <- switch(input$dist,
                       rnorm  = "Population distribution: Normal",
                       rbinom = "Population distribution: Bernoulli",
                       rlnorm = "Population distribution: Lognormal")
    
    pop <- parent()
    m_pop <- round(mean(pop), 1)
    sd_pop <- round(sd(pop), 1)
    df <- tibble(samples = pop)
    
    if (input$dist == "rnorm") {
      pdens <- density(df$samples)
      y_pos <- max(pdens$y) - 0.2 * max(pdens$y)
      mu <- req(input$mu)
      x_pos <- ifelse(mu > 0, min(-100, min(df$samples)) + 40,
                      max(100, max(df$samples)) - 40)
      
      ggplot(df, aes(x = samples, y = ..density..)) +
        stat_density(geom = "line", color = "steelblue", size = 1) +
        scale_x_continuous(limits = c(min(-100, df$samples), max(100, df$samples))) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("mean =", m_pop,
                               "\nSD =", sd_pop),
                 color = "black", size = 5) +
        theme_classic(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$dist == "rlnorm"){
      
      pop <- parent()
      df <- tibble(samples = pop)
      x_range <- max(df$samples) - min(df$samples)
      y_pos <- 0.8 * max(density(df$samples)$y)  # or compute after density
      x_pos <- max(df$samples) - 0.1 * x_range
      
      ggplot(data = df, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "steelblue") +
        stat_density(geom = "line", color = "steelblue", size = 1) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("mean", "=", bquote(.(m_pop)), 
                               "\n", "SD", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_light(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else { # Bernoulli
      ggplot(df, aes(x = factor(samples))) +
        geom_bar(color = "white", fill = "steelblue") +
        scale_x_discrete(labels = c("0 (Tail)", "1 (Head)")) +
        labs(title = distname, x = "x", y = "Count") +
        annotate("text", x = 2, y = Inf, vjust = 3.5,
                 label = paste("mean =", m_pop, "\nSD =", sd_pop),
                 color = "white", size = 5) +
        scale_y_continuous(name = NULL, breaks = NULL) +
        theme_classic(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
  })
  
  # Population distribution plot (small plot on tab 3) ----
  output$pop.dist.two <- renderPlot({
    distname <- switch(input$dist,
                       rnorm  = "Population distribution: Normal",
                       rbinom = "Population distribution: Bernoulli (coin flip)",
                       rlnorm = "Population distribution: Lognormal")
    
    pop <- parent()
    m_pop <- round(mean(pop), 1)
    sd_pop <- round(sd(pop), 1)
    df <- tibble(samples = pop)
    
    if (input$dist == "rnorm") {
      pdens <- density(df$samples)
      y_pos <- max(pdens$y) - 0.2 * max(pdens$y)
      mu <- req(input$mu)
      x_pos <- ifelse(mu > 0, min(-100, min(df$samples)) + 27,
                      max(100, max(df$samples)) - 27)
      
      ggplot(df, aes(x = samples, y = ..density..)) +
        #geom_histogram(bins = 25, color = "white", fill = "steelblue") +
        stat_density(geom = "line", color = "steelblue", size = 1) +
        scale_x_continuous(limits = c(min(-100, df$samples), max(100, df$samples))) +
        labs(title = distname, x = "x", y = "",
             subtitle = paste("mean =", m_pop, ", SD =", sd_pop)) +
        theme_classic(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$dist == "rlnorm"){
      
      pop <- parent()
      df <- tibble(samples = pop)
      x_range <- max(df$samples) - min(df$samples)
      y_pos <- 0.8 * max(density(df$samples)$y)  # or compute after density
      x_pos <- max(df$samples) - 0.1 * x_range
      
      ggplot(data = df, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "steelblue") +
        stat_density(geom = "line", color = "steelblue", size = 1) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("mean", "=", bquote(.(m_pop)), 
                               "\n", "SD", "=", bquote(.(sd_pop))),
                 color = "black", size = 3) +
        theme_light(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else { # Bernoulli
      ggplot(df, aes(x = factor(samples))) +
        geom_bar(color = "white", fill = "steelblue", width = 0.7) +
        scale_x_discrete(labels = c("0", "1")) +
        labs(title = "Population distribution: Bernoulli", 
             x = "x", y = "Count",
             subtitle = paste("mean =", m_pop, "SD =", sd_pop)) +
        scale_y_continuous(name = NULL, breaks = NULL) +
        theme_classic(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
  })
  
  # Sample panels (4 samples, tab 2) ----
  output$sample.dist <- renderPlot({
    y <- samples()
    x <- as_tibble(samples())
    
    plots <- vector("list", 4)
    for (i in 1:4) {
      m <- round(mean(y[, i]), 2)
      s <- round(sd(y[, i]), 2)
      x_range <- max(y[, i]) - min(y[, i])
      x_pos <- max(y[, i]) - 0.1 * ifelse(x_range == 0, 1, x_range)
      
      plots[[i]] <- ggplot(x, aes_string(x = paste0("V", i))) +
        geom_dotplot(alpha = 0.6, dotsize = 0.7) +
        labs(title = paste("Sample", i), x = "", y = "",
             subtitle = paste("mean =", m, ", SD =", s)) +
        theme_classic(base_size = 13) +
        scale_y_continuous(name = NULL, breaks = NULL) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) 
    }
    
    grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol = 2)
  })
  
  # text for sample plots ----
  output$num.samples <- renderText({
    paste0("... continuing to Sample ", input$k, ".")
  })
  
  # Sampling distribution (tab 3) ----
  output$sampling.dist <- renderPlot({
    distname <- switch(input$dist,
                       rnorm  = "normal population",
                       rbinom = "Bernoulli population",
                       rlnorm  = "right skewed population")
    
    ndist <- tibble(means = colMeans(samples()))
    m_samp <- round(mean(ndist$means), 2)
    sd_samp <- round(sd(ndist$means), 2)
    
    ndens <- density(ndist$means)
    y_pos <- max(ndens$y) - 0.1 * max(ndens$y)
    
    x_range <- max(ndist$means) - min(ndist$means)
    x_pos <- ifelse(m_samp > 0,
                    min(ndist$means) + 0.1 * x_range,
                    max(ndist$means) - 0.1 * x_range)
    
    ggplot(ndist, aes(x = means, y = ..density..)) +
      geom_histogram(bins = 15, color = "white", fill = "darkgray") +
      stat_density(geom = "line", color = "black", size = 1) +
      labs(title = expression("Sampling Distribution of sample means (" * bar(x) * ")"),
           x = expression("Sample means (" * bar(x) * ")"), y = "",
           subtitle = paste("mean =", m_samp, ", SE =", sd_samp)) +
      theme_classic(base_size = 19) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  # description for sampling distribution plot ----
  output$sampling.descr <- renderText({
    distname <- switch(input$dist,
                       rnorm  = "normal population",
                       rbinom = "Bernoulli (coin flip)",
                       rlnorm  = "right skewed population")
    paste("Distribution of means of", input$k, "random samples,",
          "each with", input$n, "observations from a", distname)
  })
  
  # description for CLT ----
  output$CLT.descr <- renderText({
    pop <- parent()
    m_pop <- round(mean(pop), 1)
    s_pop <- round(sd(pop), 1)
    se <- round(s_pop / sqrt(input$n), 2)
    paste0(
        "Sampling distribution is the distribution of sample statistics ",
        "(e.g., sample mean) across repeated samples of the same size drawn from the same population. ",
        "When the sample size is sufficiently large, CLT suggests that ",
        "the sampling distribution of the sample mean will be approximately normal. ", 
        "In this example, we expect the sampling distribution to be centered around ",
        "the population mean (", m_pop, "), with a standard error close to ",
        "the population standard deviation (", s_pop, ") ", 
        "divided by the square root of the sample size: ",
        s_pop, "/sqrt(", input$n, ") = ", se, "."
    )
  })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)