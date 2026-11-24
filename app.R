
# %% Packages
library(fpp3)
library(gt)
library(shiny)
library(bslib)
# %% Load Data
wine_raw <- readr::read_csv("data/AustralianWines.csv")
dplyr::glimpse(wine_raw)
# %% Prepare Monthly Tsibble
wine <- wine_raw |>
  dplyr::mutate(
    Month = yearmonth(
      paste0(
        substr(Month, 1, 3), "-19", substr(Month, 5, 6)
      ),
      format = "%b-%Y"
    ),
    Rose = readr::parse_number(Rose, na = "*")
  ) |>
  tidyr::pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  tsibble::as_tsibble(index = Month, key = Varietal)

wine
# %% User Interface
ui <- page_sidebar(
  title = "Australian Wine Sales Forecast",

  sidebar = sidebar(
    h4("Forecast Settings"),

    selectInput(
      inputId = "varietal",
      label = "Select Varietal(s):",
      choices = unique(wine$Varietal),
      selected = c("Red", "sparkling"),
      multiple = TRUE
    ),

    dateRangeInput(
      inputId = "date_range",
      label = "Select Date Range:",
      start = as.Date(min(wine$Month)),
      end = as.Date(max(wine$Month)),
      min = as.Date(min(wine$Month)),
      max = as.Date(max(wine$Month))
    ),

    dateInput(
      inputId = "train_end",
      label = "Training End Date:",
      value = as.Date("1992-12-01"),
      min = as.Date(min(wine$Month)),
      max = as.Date(max(wine$Month))
    ),

    numericInput(
      inputId = "horizon",
      label = "Forecast Horizon (Months):",
      value = 12,
      min = 1,
      max = 24,
      step = 1
    )
  ),

  card(
    card_header("Monthly Wine Sales"),
    plotOutput("overview_plot")
  ),

  card(
    card_header("Comparative Forecasts"),
    plotOutput("forecast_plot")
  ),

  card(
    card_header("Model Specifications"),
    tableOutput("model_specs")
  ),
  card(
  card_header("Training Accuracy"),
  tableOutput("training_accuracy")
  ),

  card(
  card_header("Validation Accuracy"),
  tableOutput("validation_accuracy")
  ),

  card(
    card_header("Best Validation Model"),
    tableOutput("best_model")
  )
)
# %% Server
server <- function(input, output, session) {

  filtered_wine <- reactive({
    req(input$varietal, input$date_range)

    wine |>
      filter(
        Varietal %in% input$varietal,
        Month >= yearmonth(input$date_range[1]),
        Month <= yearmonth(input$date_range[2]))
  })
  training_data <- reactive({
    req(input$train_end)

    filtered_wine() |>
      filter(Month <= yearmonth(input$train_end))
  })

  validation_data <- reactive({
    req(input$train_end)

    filtered_wine() |>
      filter(Month > yearmonth(input$train_end))
  })

  fitted_models <- reactive({
    training_data() |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales))
  })

  validation_forecasts <- reactive({
    req(nrow(validation_data()) > 0)

    fitted_models() |>
      forecast(new_data = validation_data())
  })

  training_accuracy <- reactive({
    fitted_models() |>
      fabletools::augment() |>
      tibble::as_tibble() |>
      dplyr::filter(
        !is.na(Sales),
        !is.na(.resid)
      ) |>
      dplyr::group_by(Varietal, .model) |>
      dplyr::summarise(
        RMSE = sqrt(mean(.resid^2)),
        MAE = mean(abs(.resid)),
        MAPE = mean(abs(.resid / Sales) * 100),
        .groups = "drop"
      ) |>
      dplyr::rename(Model = .model)
  })

  model_specs <- reactive({
    models <- fitted_models()

    tibble(
      Varietal = models$Varietal,
      TSLM = "Sales ~ trend() + season()",
      ETS = purrr::map_chr(models$ETS, format),
      ARIMA = purrr::map_chr(models$ARIMA, format)
  )
  })
  
  future_forecasts <- reactive({
    req(input$horizon)

    fitted_models() |>
      forecast(h = input$horizon)
  })

  validation_accuracy <- reactive({
  forecast_values <- validation_forecasts() |>
    tibble::as_tibble() |>
    dplyr::select(Varietal, .model, Month, .mean)

  actual_values <- validation_data() |>
    tibble::as_tibble() |>
    dplyr::select(
      Varietal,
      Month,
      Actual = Sales
    )

  forecast_values |>
    dplyr::left_join(
      actual_values,
      by = c("Varietal", "Month")
    ) |>
    dplyr::filter(
      !is.na(Actual),
      !is.na(.mean)
    ) |>
    dplyr::mutate(
      Error = Actual - .mean
    ) |>
    dplyr::group_by(Varietal, .model) |>
    dplyr::summarise(
      RMSE = sqrt(mean(Error^2)),
      MAE = mean(abs(Error)),
      MAPE = mean(abs(Error / Actual) * 100),
      .groups = "drop"
    ) |>
    dplyr::rename(Model = .model)
})

  best_model <- reactive({
  validation_accuracy() |>
    group_by(Varietal) |>
    slice_min(RMSE, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(Varietal, Model, RMSE)
  })

  output$overview_plot <- renderPlot({
    filtered_wine() |>
      ggplot(aes(x = Month, y = Sales, color = Varietal)) +
      geom_line(na.rm = TRUE) +
      labs(
        title = "Monthly Wine Sales",
        x = "Month",
        y = "Sales",
        color = "Varietal"
      ) +
      theme_minimal()
  })
  output$forecast_plot <- renderPlot({
  future_forecasts() |>
    autoplot(
      training_data(),
      level = c(80, 95)
    ) +
    facet_wrap(
      vars(Varietal),
      scales = "free_y"
    ) +
    labs(
      title = "Comparative Model Forecasts",
      subtitle = paste("Forecast horizon:", input$horizon, "months"),
      x = "Month",
      y = "Sales",
      caption = "Shaded areas show 80% and 95% prediction intervals"
    ) +
    theme_minimal()
  })

  output$model_specs <- renderTable({
    model_specs()
  })

  output$training_accuracy <- renderTable({
    training_accuracy()
  }, digits = 2)

  output$validation_accuracy <- renderTable({
    validation_accuracy()
  }, digits = 2)

   output$best_model <- renderTable({
  best_model()
  }, digits = 2)
  
}
# %% Run App
shinyApp(ui = ui, server = server)