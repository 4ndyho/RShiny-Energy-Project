# app.R - Shiny Dashboard - Linear Model - Energy

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(broom)    
library(plotly)
library(shinya11y)
library(shinycssloaders)

ui <- dashboardPage(
  dashboardHeader(    
    titleWidth = 375,
    title = tagList(
      tags$img(
        src = "https://img.freepik.com/premium-psd/sun-energy-ball-powerful-round-isolated-transparent-background_220739-73761.jpg?w=360",
        height = "40px",
        style = "margin-right: 8px;",
        alt = "Sun-energy sphere icon",
      ),
      "Energy Consumption Dashboard"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("User Guide", tabName = "userguide", icon = icon("info-circle")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-line")),
      menuItem("Model Building", tabName = "model", icon = icon("cogs")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("wrench")),
      menuItem("Prediction", tabName = "prediction", icon = icon("magic"))
    )
  ),
  
  dashboardBody(
    includeCSS("rshiny-customisations.css"),
    use_tota11y(),
    tags$head(
      tags$style(HTML("
        body {
          font-family: 'Arial', sans-serif;
        }
        .main-header .logo {
          background-color: #003366 !important;
          color: #ffffff !important;
          font-weight: bold;
          font-size: 18px;
        }
        .main-header .navbar {
          background-color: #003366 !important;
        }
        .sidebar {
          background-color: #003366 !important;
        }
        .sidebar-menu > li > a {
          color: #ffffff !important;
        }
        .content-wrapper, .right-side, .main-footer {
          background-color: #f4f4f4 !important;
          color: #333333;
        }
        @media (max-width: 768px) {
          .main-header .logo {
            font-size: 16px;
          }
          .sidebar-menu > li > a {
            font-size: 14px;
          }
          
        }
      "))
    ),
    tabItems(
      # -- home tab
      tabItem(tabName = "home",
              fluidRow(
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  title = NULL,
                  tags$h1("Welcome"),
                  p("This dashboard helps you learn about linear regression using energy consumption data. 
         Use the sidebar to navigate to different sections. 
         For more detailed instructions, check the User Guide.")
                )
              )
      ),
      
      # -- user guide tab
      tabItem(tabName = "userguide",
              fluidRow(
                box(
                  title = NULL,  # Removes the auto <h3>
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  tags$h2("User Guide for the Energy Consumption Dashboard"),
                    h2("Introduction"),
                    p("This Energy Consumption Shiny App allows you to explore a real-world dataset and understand 
                linear regression in an interactive way. You can view relationships between variables, build 
                and evaluate models, and even make predictions for new data."),
                    h2("User Interface"),
                    p("The app has a sidebar on the left that contains several main sections:"),
                    tags$ul(
                      tags$li("Home: A quick welcome page with a brief overview."),
                      tags$li("User Guide: A detailed guide explaining each part of the dashboard."),
                      tags$li("Exploratory Data Analysis: Tools to visualise and summarise the dataset."),
                      tags$li("Model Building: An interface to create linear regression models with selected predictors."),
                      tags$li("Diagnostics: Interactive plots to check if your model meets standard assumptions."),
                      tags$li("Prediction: A form where you can input new values to predict energy consumption.")
                    ),
                    p("Each tab has instructions or tooltips to help you understand how to use it. You can move 
                between tabs at any time using the sidebar."),
                    h2("Usage"),
                    p("Below is a step-by-step overview of how to use each main tab. Refer back here if you need guidance."),
                    h3("1) Exploratory Data Analysis (EDA)"),
                    p("In the EDA tab, you can:"),
                    tags$ul(
                      tags$li("Select which variables to plot (X vs Y)."),
                      tags$li("Click 'Generate Plot' to view a scatter plot (if X and Y differ) or a histogram (if X = Y)."),
                      tags$li("See summary statistics.")
                    ),
                    p("This is a great place to spot trends or outliers before building your model."),
                    h3("2) Model Building"),
                    p("In this section, you can:"),
                    tags$ul(
                      tags$li("Choose one or more predictors (Temperature, Humidity, etc.)."),
                      tags$li("Click 'Build Linear Model' to run the regression."),
                      tags$li("Review the model summary, including coefficients and p-values.")
                    ),
                    p("If you do not build a model manually, the app will use a default model (Temperature + Humidity) for predictions."),
                    h3("3) Diagnostics"),
                    p("Once you have built a model, check how well it performs here:"),
                    tags$ul(
                      tags$li("Residuals vs Fitted Plot: See if the errors are randomly distributed around zero."),
                      tags$li("Normal Q-Q Plot: Check if the residuals are normally distributed.")
                    ),
                    p("If the diagnostics look poor (e.g., strong patterns in residuals), you may want to reconsider your predictors or transform your data."),
                    h3("4) Prediction"),
                    p("Finally, you can make predictions for new data by:"),
                    tags$ul(
                      tags$li("Entering values for each predictor (e.g., Temperature, Occupancy)."),
                      tags$li("Clicking 'Predict Energy Consumption' to get a predicted value from your model—or from the default model if none is built.")
                    ),
                    p("This helps you see how the model might perform in real-world scenarios.")
                )
              )
      ),
      
      # -- exploratory data analysis tab
      tabItem(tabName = "eda",
              fluidRow(
                box(title = "Data Overview", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("eda_xvar", "Select an X variable:", 
                                choices = c("Temperature", "Humidity", 
                                            "SquareFootage", "Occupancy", 
                                            "HVACUsage", "LightingUsage", 
                                            "RenewableEnergy"), 
                                selected = "Temperature"),
                    selectInput("eda_yvar", "Select a Y variable:", 
                                choices = c("EnergyConsumption", "Temperature", "Humidity",
                                            "SquareFootage", "Occupancy", "HVACUsage", 
                                            "LightingUsage", "RenewableEnergy"),
                                selected = "EnergyConsumption"),
                    br(),
                    actionButton("goPlot", "Generate Plot")
                ),
                box(title = "Summary Statistics & Plot", width = 8, status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("summaryStats"),
                    br(),
                    h3("Interactive Scatter Plot / Distribution"),
                    plotlyOutput("edaPlot")
                )
              )
      ),
      
      # -- model building tab
      tabItem(tabName = "model",
              fluidRow(
                box(title = "Select Predictors", width = 4, status = "warning", solidHeader = TRUE,
                    tags$ul(
                      tags$li("The default predictors are Temperature and Humidity."),
                      tags$li("Click on the box to add any predictors."),
                      tags$li("Once happy with the predictors, click on the Build Linear Model button."),
                      tags$li("The Clear All Predictors button removes the current predictors selected.")
                    ),
                    selectInput("model_vars", "Predictors for the Model:", 
                                choices = c("Temperature", "Humidity", 
                                            "SquareFootage", "Occupancy", 
                                            "HVACUsage", "LightingUsage", 
                                            "RenewableEnergy"),
                                multiple = TRUE,
                                selected = c("Temperature", "Humidity")),
                    br(),
                    fluidRow(
                      column(6, actionButton("buildModel", "Build Linear Model")),
                      column(6, actionButton("resetPredictors", "Clear All Predictors"))
                    )
                ),
                box(title = "Model Output", width = 8, status = "warning", solidHeader = TRUE,
                    verbatimTextOutput("modelSummary"),
                    br(),
                    h4("Interpretation Tips"),
                    tags$ul(
                      tags$li("The Coefficients table shows how each predictor affects EnergyConsumption."),
                      tags$li("The Estimate column indicates how much EnergyConsumption changes when a predictor changes by one unit (or factor levels)."),
                      tags$li("Check the p-values to see which predictors are statistically significant.")
                    )
                )
              )
      ),
      
      # -- diagnostics tab
      tabItem(tabName = "diagnostics",
              fluidRow(
                box(title = "Interactive Residual Plot", width = 6, status = "danger", solidHeader = TRUE,
                    plotlyOutput("residPlot")
                ),
                box(title = "Interactive QQ Plot", width = 6, status = "danger", solidHeader = TRUE,
                    plotlyOutput("qqPlot")
                )
              ),
              fluidRow(
                box(title = "Model Assumptions", width = 12, status = "danger", solidHeader = TRUE,
                    tags$ul(
                      tags$li("The Residual Plot shows if the errors are spread out randomly around zero."),
                      tags$li("The QQ Plot checks if the residuals follow a normal distribution.")
                    )
                )
              )
      ),
      
      # -- prediction tab
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "Predict New Values", width = 4, status = "success", solidHeader = TRUE,
                    numericInput("pred_temp", "Temperature (°C)", value = 20),
                    numericInput("pred_hum", "Humidity (%)", value = 50),
                    numericInput("pred_sqft", "SquareFootage", value = 1000),
                    numericInput("pred_occ", "Occupancy", value = 10),
                    radioButtons("pred_hvac", "HVAC Usage:",
                                 choices = c("Off", "On"),
                                 selected = "Off"),
                    radioButtons("pred_lighting", "Lighting Usage:",
                                 choices = c("Off", "On"),
                                 selected = "Off"),
                    numericInput("pred_renew", "RenewableEnergy (%)", value = 10),
                    br(),
                    actionButton("goPredict", "Predict Energy Consumption")
                ),
                box(title = "Predicted Energy Consumption (kWh)", width = 8, status = "success", solidHeader = TRUE,
                    verbatimTextOutput("predictionOutput")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # -- load energy data, ensuring HVACUsage and LightingUsage use EXACT "Off" and "On"
  dfData <- reactive({
    df <- read.csv("Energy_consumption_dataset.csv")
    df$HVACUsage <- factor(df$HVACUsage, levels = c("Off", "On"))
    df$LightingUsage <- factor(df$LightingUsage, levels = c("Off", "On"))
    df
  })
  
  # -- create a reactiveVal to store the user's custom model
  userModel <- reactiveVal(NULL)
  
  # -- create a default model on app load (Temperature + Humidity)
  defaultModel <- reactive({
    df <- dfData()
    req(df)
    lm(EnergyConsumption ~ Temperature + Humidity, data = df)
  })
  
  # --- exploratory data analysis ---
  output$summaryStats <- renderPrint({
    req(dfData())
    summary(dfData())
  })
  
  observeEvent(input$goPlot, {
    output$edaPlot <- renderPlotly({
      req(dfData())
      df <- dfData()
      xvar <- input$eda_xvar
      yvar <- input$eda_yvar
      
      p <- if (xvar == yvar) {
        ggplot(df, aes_string(x = xvar)) +
          geom_histogram(bins = 30, fill = "skyblue", color = "black") +
          labs(title = paste("Distribution of", xvar), x = xvar, y = "Count")
      } else {
        ggplot(df, aes_string(x = xvar, y = yvar)) +
          geom_point() +
          labs(title = paste("Scatter Plot of", yvar, "vs", xvar), x = xvar, y = yvar)
      }
      
      ggplotly(p)
    })
  })
  
  # --- model building ---
  observeEvent(input$resetPredictors, {
    updateSelectInput(session, "model_vars", selected = character(0))
  })
  
  observeEvent(input$buildModel, {
    df <- dfData()
    req(df)
    
    if (length(input$model_vars) == 0) {
      showNotification("Please select at least one predictor for the model.", type = "warning")
      userModel(NULL)
    } else {
      form <- as.formula(paste("EnergyConsumption ~", paste(input$model_vars, collapse = " + ")))
      mod <- lm(form, data = df)
      userModel(mod)
    }
  })
  
  output$modelSummary <- renderPrint({
    mod <- userModel()
    validate(need(mod, "No model built. Please select at least one predictor and click 'Build Linear Model' or rely on the default model for predictions."))
    summary(mod)
  })
  
  # --- diagnostics ---
  output$residPlot <- renderPlotly({
    mod <- userModel()
    validate(need(mod, "No model available for diagnostics. Build your own model in 'Model Building'."))
    
    df <- augment(mod)
    p <- ggplot(df, aes(.fitted, .resid)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")
    ggplotly(p)
  })
  
  output$qqPlot <- renderPlotly({
    mod <- userModel()
    validate(need(mod, "No model available for diagnostics. Build your own model in 'Model Building'."))
    
    df <- augment(mod)
    plot <- ggplot(df, aes(sample = .resid)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
    ggplotly(plot)
  })
  
  # --- prediction ---
  observeEvent(input$goPredict, {
    req(dfData())
    
    mod <- userModel()
    if (is.null(mod)) {
      mod <- defaultModel()
      showNotification("No custom model built; using default model (Temperature + Humidity).", type = "message")
    }
    
    # Build new_data with EXACT same factor names as in the CSV
    new_data <- data.frame(
      Temperature     = input$pred_temp,
      Humidity        = input$pred_hum,
      SquareFootage   = input$pred_sqft,
      Occupancy       = input$pred_occ,
      HVACUsage       = factor(input$pred_hvac, levels = c("Off", "On")),
      LightingUsage   = factor(input$pred_lighting, levels = c("Off", "On")),
      RenewableEnergy = input$pred_renew
    )
    
    # get the original predictor names from the model formula (excluding response)
    model_vars <- all.vars(formula(mod))[-1]
    new_data <- new_data[, intersect(names(new_data), model_vars), drop = FALSE]
    
    missing_vars <- setdiff(model_vars, names(new_data))
    if (length(missing_vars) > 0) {
      for (v in missing_vars) {
        if (v %in% c("HVACUsage", "LightingUsage")) {
          new_data[[v]] <- factor("Off", levels = c("Off", "On"))
        } else {
          new_data[[v]] <- 0
        }
      }
    }
    
    # debugging
    print("Model Predictor Names:")
    print(model_vars)
    print("New Data Columns:")
    print(names(new_data))
    
    pred_val <- predict(mod, newdata = new_data)
    output$predictionOutput <- renderPrint({
      pred_val
    })
  })
}

shinyApp(ui, server)