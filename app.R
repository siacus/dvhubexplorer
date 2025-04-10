# Dataverse Explorer Shiny App
# This app connects to the Dataverse Hub API to visualize statistics about Dataverse installations

# MIT License
#
# Copyright (c) 2025 Institute for Quantitative Social Science, Stefano M. Iacus
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(leaflet)
library(plotly)
library(DT)
library(tidyr)
library(lubridate)
library(shinycssloaders)

# Base URL for the API
base_url <- "https://hub.dataverse.org/api"

# closes https://github.com/siacus/dvhubexplorer/issues/1
parse_semver <- function(version) {
  parts <- strsplit(gsub("^v", "", version), "\\.")
  do.call(rbind, lapply(parts, function(x) {
    as.numeric(c(x, rep(0, 3 - length(x)))) # ensure major.minor.patch
  }))
}

# Function to safely call APIs with improved error handling
safe_api_call <- function(endpoint, query_params = list()) {
  tryCatch(
    {
      full_url <- paste0(base_url, endpoint)
      message(paste("Calling API:", full_url))
      response <- GET(
        url = full_url,
        query = query_params,
        timeout(10)
      )
      status <- status_code(response)
      message(paste("Status code:", status))

      if (status == 200) {
        content <- content(response, "text", encoding = "UTF-8")
        message(paste("Response sample:", substr(content, 1, 100)))
        ret <- jsonlite::fromJSON(content)
        return(ret)
      } else {
        message(paste("API error:", http_status(response)$message))
        return(NULL)
      }
    },
    error = function(e) {
      message(paste("API call failed:", e$message))
      return(NULL)
    }
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dataverse Hub Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Map View", tabName = "map", icon = icon("globe")),
      menuItem("Installation Metrics", tabName = "metrics", icon = icon("chart-bar")),
      menuItem("Monthly Trends", tabName = "monthly", icon = icon("chart-line")),
      menuItem("Country Analysis", tabName = "country", icon = icon("flag")),
      menuItem("Developer Metrics", tabName = "dev", icon = icon("code"))
    ),

    # Add the footer link here
    div(
      style = "margin-top: 30px; padding-left: 15px; font-size: 10px;",
      HTML("Powered by <a href='https://hub.dataverse.org' target='_blank'>Dataverse Hub</a>")
    )
  ),
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
        }
        .box-header {
          color: #2c3e50;
        }
        .small-box {
          border-radius: 5px;
        }
      "))
    ),
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12, title = "About Dataverse Explorer", status = "primary",
            p("This dashboard provides insights into Dataverse repositories worldwide. Explore installation metrics, geographical distribution, and development statistics."),
            p(HTML("The data is retrieved in real-time from the <a href='https://hub.dataverse.org'>Dataverse Hub APIs</a>."))
          )
        ),
        fluidRow(
          valueBoxOutput("total_installations", width = 3),
          valueBoxOutput("total_datasets", width = 3),
          valueBoxOutput("total_files", width = 3),
          valueBoxOutput("total_downloads", width = 3)
        ),
        fluidRow(
          box(
            width = 6, title = "Recent Installations", style = "min-height: 500px;",
            div(
              style = "margin-bottom: 10px;",
              actionButton(
                inputId = "reset_selection", label = tagList(icon("circle-notch"), "Reset Selection"),
                class = "btn btn-secondary"
              )
            ),
            div(
              style = "overflow-y: auto; max-height: 400px;",
              withSpinner(dataTableOutput("recent_installations_table"), type = 4, color = "#3c8dbc")
            )
          ),
          box(
            width = 6, title = "Installation Count by Continent", style = "min-height: 500px;",
            withSpinner(plotlyOutput("continent_pie", height = "440px"), type = 4, color = "#3c8dbc")
          )
        )
      ),
      # Map View Tab
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            width = 12, title = "Global Distribution of Dataverse Installations", status = "primary",
            leafletOutput("installation_map", height = 600)
          )
        ),
        fluidRow(
          box(
            width = 6, title = "Installation Details",
            dataTableOutput("map_selected_installation")
          ),
          box(
            width = 6, title = "Installation Filters",
            selectInput("map_filter_continent", "Filter by Continent:",
              choices = c("All", "North America", "South America", "Europe", "Asia", "Africa", "Oceania")
            ),
            checkboxInput("map_filter_gdcc", "Show only GDCC members", value = FALSE)
          )
        )
      ),

      # Installation Metrics Tab
      tabItem(
        tabName = "metrics",
        fluidRow(
          box(
            width = 12, title = "Installation Metrics Explorer", status = "primary",
            p("Explore and compare metrics across different Dataverse installations.")
          )
        ),
        fluidRow(
          box(
            width = 4,
            selectInput("metrics_installation", "Select Installation(s):", choices = NULL),
            sliderInput("metrics_min_datasets", "Minimum Datasets:", min = 0, max = 10000, value = 0),
            sliderInput("metrics_min_files", "Minimum Files:", min = 0, max = 100000, value = 0)
          ),
          box(
            width = 8,
            plotlyOutput("metrics_comparison_plot")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Detailed Metrics",
            dataTableOutput("metrics_details_table")
          )
        )
      ),

      # Monthly Trends Tab
      tabItem(
        tabName = "monthly",
        fluidRow(
          box(
            width = 12, title = "Controls", status = "primary", solidHeader = TRUE,
            div(
              style = "display: flex; align-items: stretch; justify-content: space-between; gap: 40px;",
              div(
                style = "flex: 0 0 35%; display: flex; flex-direction: column; justify-content: center;",
                div(
                  style = "margin-bottom: 10px; text-align: center;",
                  selectInput("monthly_installation", "Select Installation(s):", choices = NULL, multiple = TRUE)
                ),
                div(
                  style = "text-align: center;",
                  dateRangeInput("monthly_date_range", "Date Range:",
                    start = Sys.Date() - months(12),
                    end = Sys.Date()
                  )
                )
              ),
              div(style = "border-left: 2px solid #3c8dbc; height: auto; align-self: stretch; margin: 0 10px;"),
              div(
                style = "flex: 0 0 35%; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                div(
                  style = "margin-bottom: 10px;",
                  checkboxInput("monthly_relative", "Show Relative Growth (%)", value = FALSE)
                ),
                div(
                  style = "margin-bottom: 10px;",
                  downloadButton("download_monthly_data", "Download CSV (Selected)")
                ),
                div(
                  style = "margin-bottom: 5px;",
                  downloadButton("download_all_data", "Download All Installations")
                ),
                tags$p("CSV includes selected installations or all hubs, based on the button.",
                  style = "margin-top: 10px; font-size: 12px; color: #666; text-align: center;"
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Monthly Growth Comparison", status = "primary", solidHeader = TRUE,
            withSpinner(uiOutput("plot_datasets_ui"), type = 4, color = "#3c8dbc"),
            div(style = "border-top: 2px solid #3c8dbc; margin: 20px 0;"),
            withSpinner(uiOutput("plot_files_ui"), type = 4, color = "#3c8dbc"),
            div(style = "border-top: 2px solid #3c8dbc; margin: 20px 0;"),
            withSpinner(uiOutput("plot_downloads_ui"), type = 4, color = "#3c8dbc")
          )
        )
      ),

      # Country Analysis Tab
      tabItem(
        tabName = "country",
        fluidRow(
          box(
            width = 12, title = "Installations by Country", status = "primary",
            p("Analyze the distribution and metrics of Dataverse installations by country.")
          )
        ),
        fluidRow(
          box(
            width = 6,
            plotlyOutput("country_bar_chart")
          ),
          box(
            width = 6,
            dataTableOutput("country_stats_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Country Comparison",
            selectInput("country_compare", "Compare Countries:",
              choices = NULL, multiple = TRUE
            ),
            plotlyOutput("country_comparison_chart")
          )
        )
      ),

      # Developer Metrics Tab
      tabItem(
        tabName = "dev",
        fluidRow(
          box(
            width = 12, title = "Developer Activity", status = "primary",
            p("View developer metrics and release information for Dataverse repositories.")
          )
        ),
        fluidRow(
          box(
            width = 6, title = "GitHub Repository Metrics",
            plotlyOutput("github_metrics_plot")
          ),
          box(
            width = 6, title = "Release Timeline",
            plotlyOutput("releases_timeline")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  rv <- reactiveValues(
    installations = data.frame(
      dvHubId = character(),
      name = character(),
      hostname = character(),
      country = character(),
      continent = character(),
      launchYear = numeric(),
      gdccMember = logical(),
      latitude = numeric(),
      longitude = numeric(),
      stringsAsFactors = FALSE
    ),
    metrics = data.frame(
      dvHubId = character(),
      name = character(),
      datasets = numeric(),
      files = numeric(),
      downloads = numeric(),
      stringsAsFactors = FALSE
    ),
    monthly_metrics = NULL,
    country_stats = NULL,
    dev_metrics = NULL,
    releases = NULL
  )

  observeEvent(input$reset_selection, {
    proxy <- dataTableProxy("recent_installations_table")
    selectRows(proxy, NULL) # clears all selections
  })
  # Define reactive data sources
  installations_data <- reactive({
    message("Fetching installations data...")
    data <- safe_api_call("/installation")

    if (!is.null(data) && nrow(data) > 0) {
      message(paste("Received", nrow(data), "installations"))

      # Update installation dropdown choices
      installation_choices <- setNames(
        data$dvHubId,
        data$name
      )
      updateSelectInput(session, "metrics_installation", choices = installation_choices)
      updateSelectInput(session, "monthly_installation", choices = installation_choices)

      return(data)
    } else {
      message("No installation data received or error occurred")
      # Return sample data if API fails
      return(data.frame(
        dvHubId = c("sample1", "sample2"),
        name = c("Sample Installation 1", "Sample Installation 2"),
        hostname = c("sample1.dataverse.org", "sample2.dataverse.org"),
        country = c("United States", "Germany"),
        continent = c("North America", "Europe"),
        launchYear = c(2018, 2020),
        gdccMember = c(TRUE, FALSE),
        latitude = c(42.3736, 52.5200),
        longitude = c(-71.1097, 13.4050),
        stringsAsFactors = FALSE
      ))
    }
  })

  metrics_data <- reactive({
    message("Fetching metrics data...")
    data <- safe_api_call("/installation/metrics")

    data <- data %>%
      mutate(metrics = lapply(metrics, as.data.frame)) %>%
      unnest_wider(metrics)

    if (!is.null(data) && nrow(data) > 0) {
      message(paste("Received metrics data with", nrow(data), "rows"))

      # Check if required columns exist
      required_cols <- c("datasets", "files", "downloads")
      missing_cols <- setdiff(required_cols, colnames(data))

      if (length(missing_cols) > 0) {
        message(paste("Warning: Missing columns in metrics data:", paste(missing_cols, collapse = ", ")))
        # Add missing columns with default values
        for (col in missing_cols) {
          data[[col]] <- 0
        }
      }

      # Check for data validity
      message(paste("Total datasets:", sum(data$datasets, na.rm = TRUE)))
      message(paste("Total files:", sum(data$files, na.rm = TRUE)))
      message(paste("Total downloads:", sum(data$downloads, na.rm = TRUE)))

      return(data)
    } else {
      message("No metrics data received or error occurred")
      # Return sample data if API fails
      return(data.frame(
        dvHubId = c("sample1", "sample2"),
        name = c("Sample Installation 1", "Sample Installation 2"),
        datasets = c(1250, 873),
        files = c(8750, 6245),
        downloads = c(25000, 18500),
        dataverses = c(45, 32),
        localDatasets = c(1000, 800),
        harvestedDatasets = c(250, 73),
        stringsAsFactors = FALSE
      ))
    }
  })

  country_stats_data <- reactive({
    message("Fetching country stats...")
    data <- safe_api_call("/installation/country")

    if (!is.null(data) && nrow(data) > 0) {
      message(paste("Received country stats with", nrow(data), "rows"))

      # Update country dropdown choices
      country_choices <- unique(data$country)
      updateSelectInput(session, "country_compare", choices = country_choices)

      return(data)
    } else {
      message("No country stats received or error occurred")
      # Return sample data if API fails
      return(data.frame(
        country = c("United States", "Germany", "Canada", "Brazil", "Japan"),
        count = c(15, 8, 5, 3, 2),
        recordDate = rep(as.character(Sys.Date()), 5),
        stringsAsFactors = FALSE
      ))
    }
  })

  dev_metrics_data <- reactive({
    message("Fetching developer metrics...")
    data <- safe_api_call("/dev")

    if (!is.null(data)) {
      data <- as.data.frame(data)
      message("Received developer metrics data")
      return(data)
    } else {
      message("No developer metrics received or error occurred")
      # Return sample data if API fails
      return(data.frame(
        name = "dataverse",
        watchers = 120,
        forks = 85,
        open_issues = 45,
        subscribers_count = 60,
        stringsAsFactors = FALSE
      ))
    }
  })

  releases_data <- reactive({
    message("Fetching releases data...")
    data <- safe_api_call("/dev/releases")

    if (!is.null(data) && nrow(data) > 0) {
      message(paste("Received releases data with", nrow(data), "rows"))
      return(data)
    } else {
      message("No releases data received or error occurred")
      # Return sample data if API fails
      return(data.frame(
        repoName = c("dataverse", "dataverse", "dataverse"),
        tag_name = c("v5.3", "v5.2", "v5.1"),
        published_at = c("2023-06-15", "2023-01-20", "2022-09-05"),
        stringsAsFactors = FALSE
      ))
    }
  })


  observe({
    met_data <- rv$metrics

    if (is.null(met_data)) {
      return()
    }

    filtered <- met_data %>%
      filter(
        datasets >= input$metrics_min_datasets,
        files >= input$metrics_min_files
      )

    if (nrow(filtered) > 0) {
      updateSelectInput(session, "metrics_installation",
        selected = filtered$dvHubId[1]
      )
    }
  })

  observe({
    rv$installations <- installations_data()
  })

  observe({
    rv$metrics <- metrics_data()
  })

  observeEvent(
    {
      input$metrics_min_datasets
      input$metrics_min_files
    },
    {
      met_data <- isolate(rv$metrics)

      if (is.null(met_data)) {
        return()
      }

      filtered <- met_data %>%
        filter(
          datasets >= input$metrics_min_datasets,
          files >= input$metrics_min_files
        )

      installation_choices <- setNames(filtered$dvHubId, filtered$name)

      if (nrow(filtered) > 0) {
        updateSelectInput(session, "metrics_installation",
          choices = installation_choices,
          selected = filtered$dvHubId[1]
        )
      } else {
        updateSelectInput(session, "metrics_installation",
          choices = character(0),
          selected = ""
        )
      }
    }
  )



  output$plot_datasets_ui <- renderUI({
    df <- rv$monthly_metrics
    if (!is.null(df) && "datasets" %in% names(df) && nrow(df) > 0) {
      return(plotlyOutput("plot_datasets", height = "250px"))
    }
    if (!is.null(input$monthly_installation) && length(input$monthly_installation) > 0 &&
      !is.null(input$monthly_date_range)) {
      return(tags$div("Fetching data...",
        style = "text-align: center; color: #3c8dbc; padding: 100px 0; font-size: 16px;"
      ))
    }
    return(tags$div("Please select installations",
      style = "text-align: center; color: #999; padding: 100px 0; font-size: 16px;"
    ))
  })


  output$plot_files_ui <- renderUI({
    df <- rv$monthly_metrics
    if (is.null(df) || !"files" %in% names(df) || nrow(df) == 0) {
      return(tags$div("Please select installations",
        style = "text-align: center; color: #999; padding: 100px 0; font-size: 18px;"
      ))
    } else {
      return(plotlyOutput("plot_files", height = "250px"))
    }
  })

  output$plot_downloads_ui <- renderUI({
    df <- rv$monthly_metrics
    if (is.null(df) || !"downloads" %in% names(df) || nrow(df) == 0) {
      return(tags$div("Please select installations",
        style = "text-align: center; color: #999; padding: 100px 0; font-size: 18px;"
      ))
    } else {
      return(plotlyOutput("plot_downloads", height = "250px"))
    }
  })

  observe({
    rv$country_stats <- country_stats_data()
  })

  observe({
    rv$dev_metrics <- dev_metrics_data()
  })

  observe({
    rv$releases <- releases_data()
  })


  output$plot_datasets <- renderPlotly({
    df <- rv$monthly_metrics
    if (is.null(df) || !"datasets" %in% names(df)) {
      return(NULL)
    }

    df <- df %>%
      mutate(date = as.Date(recordDate)) %>%
      arrange(date)

    if (input$monthly_relative) {
      df <- df %>%
        group_by(installation) %>%
        mutate(datasets = datasets / first(datasets) * 100) %>%
        ungroup()
    }

    y_label <- if (input$monthly_relative) "%" else "Count"

    plot_ly(df,
      x = ~date, y = ~datasets, color = ~installation, type = "scatter", mode = "lines",
      line = list(width = 2)
    ) %>%
      layout(title = "Datasets", yaxis = list(title = y_label), hovermode = "x unified")
  })

  output$plot_files <- renderPlotly({
    df <- rv$monthly_metrics
    if (is.null(df) || !"files" %in% names(df)) {
      return(NULL)
    }

    df <- df %>%
      mutate(date = as.Date(recordDate)) %>%
      arrange(date)

    if (input$monthly_relative) {
      df <- df %>%
        group_by(installation) %>%
        mutate(files = files / first(files) * 100) %>%
        ungroup()
    }

    y_label <- if (input$monthly_relative) "%" else "Count"

    plot_ly(df,
      x = ~date, y = ~files, color = ~installation, type = "scatter", mode = "lines",
      line = list(width = 2)
    ) %>%
      layout(title = "Files", yaxis = list(title = y_label), hovermode = "x unified")
  })
  output$plot_downloads <- renderPlotly({
    df <- rv$monthly_metrics
    if (is.null(df) || !"downloads" %in% names(df)) {
      return(NULL)
    }

    df <- df %>%
      mutate(date = as.Date(recordDate)) %>%
      arrange(date)

    if (input$monthly_relative) {
      df <- df %>%
        group_by(installation) %>%
        mutate(downloads = downloads / first(downloads) * 100) %>%
        ungroup()
    }

    y_label <- if (input$monthly_relative) "%" else "Count"

    plot_ly(df,
      x = ~date, y = ~downloads, color = ~installation, type = "scatter", mode = "lines",
      line = list(width = 2)
    ) %>%
      layout(title = "Downloads", yaxis = list(title = y_label), hovermode = "x unified")
  })

  output$total_installations <- renderValueBox({
    inst_data <- rv$installations
    selected <- input$recent_installations_table_rows_selected

    if (is.null(inst_data) || nrow(inst_data) == 0) {
      count <- 0
    } else if (!is.null(selected) && length(selected) > 0) {
      count <- length(selected)
    } else {
      count <- nrow(inst_data)
    }

    valueBox(
      count, "Installations",
      icon = icon("server"),
      color = "blue"
    )
  })


  output$download_monthly_data <- downloadHandler(
    filename = function() {
      paste0("monthly_metrics_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- rv$monthly_metrics
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        selected_ids <- isolate(input$monthly_installation)
        all_ids <- names(updateSelectInput(session, "monthly_installation")$choices)
        ids_to_fetch <- if (length(selected_ids) == 0) all_ids else selected_ids

        all_data <- list()
        for (id in ids_to_fetch) {
          result <- safe_api_call("/installation/metrics/monthly", list(
            dvHubId = id,
            fromDate = format(input$monthly_date_range[1], "%Y-%m"),
            toDate = format(input$monthly_date_range[2], "%Y-%m")
          ))
          if (!is.null(result$metrics[[1]]) && is.data.frame(result$metrics[[1]])) {
            df_part <- result$metrics[[1]]
            df_part$installation <- result$name
            all_data[[length(all_data) + 1]] <- df_part
          }
        }
        df <- bind_rows(all_data)
      }
      df <- df %>%
        select(recordDate, installation, datasets, files, downloads, dataverses, localDatasets, harvestedDatasets) %>%
        arrange(recordDate, installation)

      write.csv(df, file, row.names = FALSE)
    }
  )


  output$download_all_data <- downloadHandler(
    filename = function() {
      paste0("all_installations_", Sys.Date(), ".csv")
    },
    content = function(file) {
      inst_data <- rv$installations
      if (is.null(inst_data) || nrow(inst_data) == 0) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }

      ids_to_fetch <- inst_data$dvHubId
      all_data <- list()

      for (id in ids_to_fetch) {
        result <- safe_api_call("/installation/metrics/monthly", list(
          dvHubId = id,
          fromDate = format(input$monthly_date_range[1], "%Y-%m"),
          toDate = format(input$monthly_date_range[2], "%Y-%m")
        ))
        if (!is.null(result$metrics[[1]]) && is.data.frame(result$metrics[[1]])) {
          df_part <- result$metrics[[1]]
          df_part$installation <- result$name
          all_data[[length(all_data) + 1]] <- df_part
        }
      }

      df <- if (length(all_data) > 0) {
        bind_rows(all_data)
      } else {
        data.frame(
          recordDate = character(),
          installation = character(),
          datasets = numeric(),
          files = numeric(),
          downloads = numeric(),
          dataverses = numeric(),
          localDatasets = numeric(),
          harvestedDatasets = numeric(),
          stringsAsFactors = FALSE
        )
      }

      df <- df %>%
        select(recordDate, installation, datasets, files, downloads, dataverses, localDatasets, harvestedDatasets) %>%
        arrange(recordDate, installation)

      write.csv(df, file, row.names = FALSE)
    }
  )




  observeEvent(input$metrics_details_table_rows_selected, {
    selected_row <- input$metrics_details_table_rows_selected
    met_data <- rv$metrics %>%
      filter(
        datasets >= input$metrics_min_datasets,
        files >= input$metrics_min_files
      )

    if (!is.null(selected_row) && length(selected_row) == 1 && nrow(met_data) >= selected_row) {
      selected_dvHubId <- met_data$dvHubId[selected_row]
      updateSelectInput(session, "metrics_installation", selected = selected_dvHubId)
    }
  })

  selected_metrics <- reactive({
    met_data <- rv$metrics
    selected <- input$recent_installations_table_rows_selected

    if (is.null(met_data) || nrow(met_data) == 0) {
      return(NULL)
    }

    if (!is.null(selected) && length(selected) > 0) {
      inst_data <- rv$installations
      selected_names <- inst_data$name[selected]
      met_data <- met_data %>% filter(name %in% selected_names)
    }

    return(met_data)
  })


  output$total_datasets <- renderValueBox({
    data <- selected_metrics()

    total <- if (is.null(data) || !"datasets" %in% colnames(data)) {
      0
    } else {
      sum(data$datasets, na.rm = TRUE)
    }

    valueBox(
      formatC(total, format = "d", big.mark = ","), "Datasets",
      icon = icon("database"),
      color = "green"
    )
  })

  output$total_files <- renderValueBox({
    data <- selected_metrics()

    total <- if (is.null(data) || !"files" %in% colnames(data)) {
      0
    } else {
      sum(data$files, na.rm = TRUE)
    }

    valueBox(
      formatC(total, format = "d", big.mark = ","), "Files",
      icon = icon("file"),
      color = "purple"
    )
  })

  output$total_downloads <- renderValueBox({
    data <- selected_metrics()

    total <- if (is.null(data) || !"downloads" %in% colnames(data)) {
      0
    } else {
      sum(data$downloads, na.rm = TRUE)
    }

    valueBox(
      formatC(total, format = "d", big.mark = ","), "Downloads",
      icon = icon("download"),
      color = "orange"
    )
  })

  output$recent_installations_table <- renderDataTable({
    inst_data <- rv$installations

    if (is.null(inst_data) || nrow(inst_data) == 0) {
      return(NULL)
    }

    installations_df <- inst_data %>%
      arrange(desc(launchYear)) %>%
      mutate(name = sprintf(
        '<a href="https://%s" target="_blank" title="Visit %s"><i class="fas fa-external-link-alt"></i></a>&nbsp;&nbsp;&nbsp;&nbsp;%s',
        hostname, name, name
      )) %>%
      select(name, hostname, country, launchYear)

    datatable(installations_df,
      options = list(
        paging = FALSE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        searching = TRUE
      ),
      selection = "multiple",
      escape = FALSE,
      rownames = FALSE,
      colnames = c(
        "Name" = "name",
        "Hostname" = "hostname",
        "Country" = "country",
        "Launch Year" = "launchYear"
      )
    )
  })


  # selected_installations <- reactive({
  #   data <- rv$developer_metrics$installations
  #   selected_rows <- input$installations_df_rows_selected

  #   if (!is.null(selected_rows) && length(selected_rows) > 0) {
  #     data[selected_rows, ]
  #   } else {
  #     data
  #   }
  # })

  output$continent_pie <- renderPlotly({
    inst_data <- rv$installations

    if (is.null(inst_data) || nrow(inst_data) == 0 || !"continent" %in% colnames(inst_data)) {
      return(NULL)
    }

    continent_counts <- inst_data %>%
      count(continent) %>%
      arrange(desc(n))

    plot_ly(continent_counts,
      labels = ~continent, values = ~n, type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(colors = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f"))
    ) %>%
      layout(
        title = "Distribution by Continent",
        showlegend = FALSE
      )
  })

  # Map View Tab Outputs
  output$installation_map <- renderLeaflet({
    inst_data <- rv$installations

    if (is.null(inst_data) || nrow(inst_data) == 0) {
      return(leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 20, zoom = 2))
    }

    installations_filtered <- inst_data

    # Apply continent filter
    if (!is.null(input$map_filter_continent) && input$map_filter_continent != "All") {
      installations_filtered <- installations_filtered %>%
        filter(continent == input$map_filter_continent)
    }

    # Apply GDCC filter
    if (!is.null(input$map_filter_gdcc) && input$map_filter_gdcc) {
      installations_filtered <- installations_filtered %>%
        filter(gdccMember)
    }

    # Check if we have valid lat/long data
    if (!"latitude" %in% colnames(installations_filtered) ||
      !"longitude" %in% colnames(installations_filtered) ||
      all(is.na(installations_filtered$latitude)) ||
      all(is.na(installations_filtered$longitude))) {
      return(leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 20, zoom = 2) %>%
        addControl("No valid location data available", position = "topright"))
    }

    leaflet(installations_filtered) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~ paste0(
          "<b>", name, "</b><br>",
          'Host: <a href="https://', hostname, '" target="_blank">', hostname, "</a><br>",
          "Country: ", country, "<br>",
          "Launch Year: ", launchYear, "<br>",
          "GDCC Member: ", ifelse(gdccMember, "Yes", "No")
        ),
        label = ~name,
        clusterOptions = markerClusterOptions()
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })


  output$map_selected_installation <- renderDataTable({
    inst_data <- rv$installations

    if (is.null(inst_data) || nrow(inst_data) == 0) {
      return(NULL)
    }

    installations_filtered <- inst_data

    # Apply continent filter
    if (!is.null(input$map_filter_continent) && input$map_filter_continent != "All") {
      installations_filtered <- installations_filtered %>%
        filter(continent == input$map_filter_continent)
    }

    # Apply GDCC filter
    if (!is.null(input$map_filter_gdcc) && input$map_filter_gdcc) {
      installations_filtered <- installations_filtered %>%
        filter(gdccMember)
    }

    # Inject icon into the name column
    installations_filtered <- installations_filtered %>%
      mutate(name = sprintf(
        '<a href="https://%s" target="_blank" title="Visit %s"><i class="fas fa-external-link-alt"></i></a>&nbsp;&nbsp;&nbsp;&nbsp;%s',
        hostname, name, name
      ))

    datatable(
      installations_filtered %>%
        select(name, hostname, country, launchYear, gdccMember),
      options = list(
        paging = FALSE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        dom = "t"
      ),
      selection = "single",
      escape = FALSE,
      rownames = FALSE,
      colnames = c(
        "Name" = "name",
        "Hostname" = "hostname",
        "Country" = "country",
        "Launch Year" = "launchYear",
        "GDCC Member" = "gdccMember"
      )
    )
  })


  observeEvent(input$map_selected_installation_rows_selected, {
    selected_row <- input$map_selected_installation_rows_selected
    inst_data <- rv$installations

    if (!is.null(inst_data) && nrow(inst_data) > 0) {
      installations_filtered <- inst_data

      if (!is.null(input$map_filter_continent) && input$map_filter_continent != "All") {
        installations_filtered <- installations_filtered %>%
          filter(continent == input$map_filter_continent)
      }

      if (!is.null(input$map_filter_gdcc) && input$map_filter_gdcc) {
        installations_filtered <- installations_filtered %>%
          filter(gdccMember)
      }
      if (!is.null(selected_row) && length(selected_row) == 1 &&
        selected_row <= nrow(installations_filtered)) {
        selected_installation <- installations_filtered[selected_row, ]
        lat <- selected_installation$latitude
        lng <- selected_installation$longitude

        if (!is.na(lat) && !is.na(lng)) {
          leafletProxy("installation_map") %>%
            setView(lng = lng, lat = lat, zoom = 10)
        }
      }
    }
  })

  # Installation Metrics Tab Outputs
  output$metrics_comparison_plot <- renderPlotly({
    met_data <- rv$metrics

    if (is.null(met_data) || nrow(met_data) == 0 || is.null(input$metrics_installation)) {
      return(NULL)
    }
    required_cols <- c("datasets", "files", "downloads")
    for (col in required_cols) {
      if (!col %in% colnames(met_data)) {
        met_data[[col]] <- 0
      }
    }

    metrics_filtered <- met_data %>%
      filter(
        datasets >= input$metrics_min_datasets,
        files >= input$metrics_min_files
      )

    if (nrow(metrics_filtered) == 0) {
      return(NULL)
    }

    selected_installation <- metrics_filtered %>%
      filter(dvHubId == input$metrics_installation)

    if (nrow(selected_installation) == 0) {
      return(NULL)
    }

    plot_ly() %>%
      add_bars(
        data = metrics_filtered,
        x = ~name,
        y = ~datasets,
        name = "Datasets",
        marker = list(color = "#1f77b4")
      ) %>%
      add_bars(
        data = metrics_filtered,
        x = ~name,
        y = ~ files / 100,
        name = "Files (÷100)",
        marker = list(color = "#ff7f0e")
      ) %>%
      add_bars(
        data = metrics_filtered,
        x = ~name,
        y = ~ downloads / 1000,
        name = "Downloads (÷1000)",
        marker = list(color = "#2ca02c")
      ) %>%
      layout(
        title = "Installation Metrics Comparison",
        xaxis = list(title = "Installation"),
        yaxis = list(title = "Count"),
        barmode = "group",
        annotations = list(
          list(
            x = selected_installation$name,
            y = max(metrics_filtered$datasets, na.rm = TRUE) * 1.1,
            text = "Selected",
            showarrow = TRUE,
            arrowhead = 2,
            arrowsize = 1,
            arrowwidth = 2,
            ax = 0,
            ay = -40
          )
        )
      )
  })


  output$metrics_details_table <- renderDataTable({
    met_data <- rv$metrics
    inst_data <- rv$installations

    if (is.null(met_data) || nrow(met_data) == 0) {
      return(NULL)
    }

    required_cols <- c("datasets", "files", "downloads", "dataverses", "localDatasets", "harvestedDatasets")
    for (col in required_cols) {
      if (!col %in% colnames(met_data)) {
        met_data[[col]] <- 0
      }
    }

    if (!is.null(inst_data) && "name" %in% colnames(inst_data) && "hostname" %in% colnames(inst_data)) {
      met_data <- left_join(met_data, inst_data[, c("name", "hostname")], by = "name")
    } else {
      met_data$hostname <- NA
    }

    metrics_filtered <- met_data %>%
      filter(
        datasets >= input$metrics_min_datasets,
        files >= input$metrics_min_files
      ) %>%
      mutate(name = ifelse(!is.na(hostname),
        sprintf(
          '<a href="https://%s" target="_blank" title="Visit %s"><i class="fas fa-external-link-alt"></i></a>&nbsp;&nbsp;&nbsp;&nbsp;%s',
          hostname, name, name
        ),
        name
      )) %>%
      select(name, datasets, files, downloads, dataverses, localDatasets, harvestedDatasets)

    datatable(
      metrics_filtered,
      options = list(
        paging = FALSE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        dom = "t"
      ),
      selection = "single",
      rownames = FALSE,
      escape = FALSE,
      colnames = c(
        "Name" = "name",
        "Datasets" = "datasets",
        "Files" = "files",
        "Downloads" = "downloads",
        "Dataverses" = "dataverses",
        "Local Datasets" = "localDatasets",
        "Harvested" = "harvestedDatasets"
      )
    )
  })


  # Monthly Trends Tab Outputs
  observeEvent(
    {
      input$monthly_installation
      input$monthly_date_range
    },
    {
      selected <- input$monthly_installation
      if (is.null(selected) || length(selected) == 0) {
        return()
      }

      all_data <- list()

      for (id in selected) {
        params <- list(
          dvHubId = id,
          fromDate = format(input$monthly_date_range[1], "%Y-%m"),
          toDate = format(input$monthly_date_range[2], "%Y-%m")
        )

        result <- safe_api_call("/installation/metrics/monthly", params)

        if (!is.null(result$metrics[[1]]) && is.data.frame(result$metrics[[1]])) {
          df <- result$metrics[[1]]
          df$installation <- result$name
          all_data[[length(all_data) + 1]] <- df
        }
      }

      if (length(all_data) > 0) {
        rv$monthly_metrics <- bind_rows(all_data)
      } else {
        rv$monthly_metrics <- NULL
      }
    }
  )



  output$monthly_trend_plot <- renderPlotly({
    df <- rv$monthly_metrics
    if (is.null(df) || !is.data.frame(df)) {
      return(NULL)
    }

    df <- df %>%
      mutate(date = as.Date(recordDate)) %>%
      arrange(date)

    metrics <- c("datasets", "files", "downloads")
    metric_titles <- c("Datasets", "Files", "Downloads")
    y_axis_label <- if (input$monthly_relative) "%" else "Count"

    if (input$monthly_relative) {
      df <- df %>%
        group_by(installation) %>%
        arrange(date) %>%
        mutate(across(all_of(metrics), ~ . / first(.) * 100)) %>%
        ungroup()
    }

    plots <- lapply(seq_along(metrics), function(i) {
      metric <- metrics[i]
      title <- metric_titles[i]

      plot_data <- df %>%
        select(date, installation, value = !!sym(metric))

      plot_ly(
        data = plot_data, x = ~date, y = ~value, color = ~installation,
        type = "scatter", mode = "lines", line = list(width = 2)
      ) %>%
        layout(
          title = list(text = title, x = 0),
          xaxis = list(title = if (i == length(metrics)) "Date" else ""),
          yaxis = list(title = y_axis_label),
          showlegend = (i == 1)
        )
    })

    subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE, margin = 0.05) %>%
      layout(hovermode = "x unified")
  })




  # Country Analysis Tab Outputs
  output$country_bar_chart <- renderPlotly({
    country_data <- rv$country_stats

    if (is.null(country_data) || nrow(country_data) == 0) {
      return(NULL)
    }

    if (!"country" %in% colnames(country_data) || !"count" %in% colnames(country_data)) {
      message("Missing required columns in country data")
      return(NULL)
    }

    country_df <- country_data %>%
      arrange(desc(count))

    plot_ly(country_df,
      x = ~ reorder(country, -count), y = ~count, type = "bar",
      marker = list(color = "#636EFA")
    ) %>%
      layout(
        title = "Installations per Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Number of Installations")
      )
  })

  output$country_stats_table <- renderDataTable({
    country_data <- rv$country_stats

    if (is.null(country_data) || nrow(country_data) == 0) {
      return(NULL)
    }

    required_cols <- c("country", "count", "recordDate")
    for (col in required_cols) {
      if (!col %in% colnames(country_data)) {
        if (col == "recordDate") {
          country_data[[col]] <- as.character(Sys.Date())
        } else {
          country_data[[col]] <- NA
        }
      }
    }

    datatable(
      country_data %>%
        select(country, count, recordDate) %>%
        arrange(desc(count)),
      options = list(
        paging = FALSE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        dom = "t"
      ),
      colnames = c(
        "Country" = "country",
        "Installations" = "count",
        "Record Date" = "recordDate"
      )
    )
  })

  output$country_comparison_chart <- renderPlotly({
    met_data <- rv$metrics
    country_selection <- input$country_compare

    if (is.null(met_data) || nrow(met_data) == 0 ||
      is.null(country_selection) || length(country_selection) == 0 ||
      !"country" %in% colnames(met_data)) {
      return(NULL)
    }

    required_cols <- c("datasets", "files", "downloads")
    for (col in required_cols) {
      if (!col %in% colnames(met_data)) {
        met_data[[col]] <- 0
      }
    }

    tryCatch(
      {
        country_metrics <- met_data %>%
          filter(country %in% country_selection) %>%
          group_by(country) %>%
          summarize(
            Installations = n(),
            Datasets = sum(datasets, na.rm = TRUE),
            Files = sum(files, na.rm = TRUE) / 1000,
            Downloads = sum(downloads, na.rm = TRUE) / 1000,
            .groups = "drop"
          ) %>%
          pivot_longer(
            cols = c(Installations, Datasets, Files, Downloads),
            names_to = "Metric", values_to = "Value"
          )

        if (nrow(country_metrics) == 0) {
          return(NULL)
        }

        plot_ly(country_metrics, x = ~country, y = ~Value, color = ~Metric, type = "bar") %>%
          layout(
            title = "Country Metrics Comparison",
            xaxis = list(title = "Country"),
            yaxis = list(title = "Count (Files & Downloads in thousands)"),
            barmode = "group"
          )
      },
      error = function(e) {
        message("Error creating country comparison chart: ", e$message)
        return(NULL)
      }
    )
  })

  # Developer Metrics Tab Outputs
  output$github_metrics_plot <- renderPlotly({
    dev_data <- rv$dev_metrics

    if (is.null(dev_data)) {
      return(NULL)
    }

    required_cols <- c("name", "watchers", "forks", "open_issues", "subscribers_count")
    for (col in required_cols) {
      if (!col %in% colnames(dev_data)) {
        dev_data[[col]] <- 0
      }
    }

    github_df <- data.frame(
      Metric = c("Watchers", "Forks", "Open Issues", "Subscribers"),
      Count = c(
        dev_data$watchers, dev_data$forks,
        dev_data$open_issues, dev_data$subscribers_count
      )
    )

    plot_ly(github_df,
      x = ~Metric, y = ~Count, type = "bar",
      marker = list(color = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"))
    ) %>%
      layout(
        title = paste("GitHub Metrics for", dev_data$name),
        xaxis = list(title = ""),
        yaxis = list(title = "Count")
      )
  })

  output$releases_timeline <- renderPlotly({
    releases_data <- rv$releases

    if (is.null(releases_data) || nrow(releases_data) == 0) {
      return(NULL)
    }

    required_cols <- c("tag_name", "published_at")
    for (col in required_cols) {
      if (!col %in% colnames(releases_data)) {
        message("Missing required column in releases data: ", col)
        return(NULL)
      }
    }

    tryCatch(
      {
        releases_df <- releases_data %>%
          mutate(published_at = as.Date(published_at))
        semver_matrix <- parse_semver(releases_df$tag_name) # Semantic version sort
        sorted_versions <- releases_df$tag_name[order(semver_matrix[, 1], semver_matrix[, 2], semver_matrix[, 3])]
        releases_df$tag_name_factor <- factor(releases_df$tag_name, levels = sorted_versions)

        plot_ly(releases_df,
          x = ~published_at, y = ~tag_name_factor, type = "scatter", mode = "markers",
          marker = list(size = 10, color = "#1f77b4")
        ) %>%
          layout(
            title = "Release Timeline",
            xaxis = list(title = "Publication Date"),
            yaxis = list(title = "Version (Semantic Order)")
          )
      },
      error = function(e) {
        message("Error creating releases timeline: ", e$message)
        return(NULL)
      }
    )
  })



  output$releases_table <- renderDataTable({
    releases_data <- rv$releases

    if (is.null(releases_data) || nrow(releases_data) == 0) {
      return(NULL)
    }

    required_cols <- c("repoName", "tag_name", "published_at")
    for (col in required_cols) {
      if (!col %in% colnames(releases_data)) {
        if (col == "repoName") {
          releases_data[[col]] <- "dataverse"
        } else {
          releases_data[[col]] <- NA
        }
      }
    }

    tryCatch(
      {
        datatable(
          releases_data %>%
            mutate(published_at = as.Date(published_at)) %>%
            arrange(desc(published_at)) %>%
            select(repoName, tag_name, published_at),
          options = list(pageLength = 10),
          rownames = FALSE,
          colnames = c(
            "Repository" = "repoName",
            "Version" = "tag_name",
            "Publication Date" = "published_at"
          )
        )
      },
      error = function(e) {
        message("Error creating releases table: ", e$message)
        return(NULL)
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
