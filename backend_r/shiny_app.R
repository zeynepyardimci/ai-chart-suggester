library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(httr)
library(jsonlite)

# Helper function to detect column types
get_col_types <- function(df) {
    sapply(df, function(x) {
        if (is.numeric(x)) {
            "numeric"
        } else if (is.factor(x) || is.character(x)) {
            "categorical"
        } else {
            "other"
        }
    })
}

# Smart chart recommendation based on data characteristics
recommend_chart_type <- function(df, x_var = NULL, y_var = NULL) {
    if (is.null(df) || nrow(df) == 0) {
        return("No recommendation")
    }

    types <- get_col_types(df)

    # If X and Y are specified
    if (!is.null(x_var) && !is.null(y_var)) {
        x_type <- types[x_var]
        y_type <- types[y_var]

        # X numeric, Y numeric → Scatter (check correlation)
        if (x_type == "numeric" && y_type == "numeric") {
            correlation <- cor(df[[x_var]], df[[y_var]], use = "complete.obs")
            if (abs(correlation) > 0.7) {
                return(list(
                    primary = "scatterplot",
                    secondary = "line",
                    reason = paste0("Strong correlation (", round(correlation, 2), ") - consider line fit")
                ))
            }
            return(list(primary = "scatterplot", reason = "Numeric vs Numeric"))
        }

        # X categorical, Y numeric → Boxplot/Violin
        if (x_type == "categorical" && y_type == "numeric") {
            n_categories <- length(unique(df[[x_var]]))
            if (n_categories <= 5) {
                return(list(
                    primary = "violin", secondary = "boxplot",
                    reason = "Distribution comparison across categories"
                ))
            } else {
                return(list(primary = "boxplot", reason = "Many categories - boxplot preferred"))
            }
        }

        # X categorical, Y categorical → Grouped Bar
        if (x_type == "categorical" && y_type == "categorical") {
            return(list(primary = "grouped_bar", reason = "Categorical vs Categorical"))
        }

        # Check for time series
        if (any(grepl("date|time|year|month", tolower(x_var)))) {
            return(list(primary = "line", reason = "Time series detected"))
        }
    }

    # If only X is specified
    if (!is.null(x_var) && is.null(y_var)) {
        x_type <- types[x_var]
        cardinality <- length(unique(df[[x_var]]))

        # X numeric, cardinality ≤ 20 → Histogram
        if (x_type == "numeric" && cardinality <= 20) {
            return(list(
                primary = "histogram", secondary = "bar",
                reason = "Low cardinality numeric - histogram or bar"
            ))
        }

        # X numeric → Histogram
        if (x_type == "numeric") {
            return(list(
                primary = "histogram", secondary = "density",
                reason = "Numeric distribution"
            ))
        }

        # X categorical, cardinality ≤ 40 → Bar Chart
        if (x_type == "categorical" && cardinality <= 40) {
            if (cardinality <= 8) {
                return(list(
                    primary = "pie", secondary = "bar",
                    reason = "Few categories - pie or bar"
                ))
            }
            return(list(primary = "bar", reason = "Categorical distribution"))
        }
    }

    # No specific variables - analyze overall data
    numeric_cols <- names(df)[types == "numeric"]

    # Multiple numeric columns - check correlation
    if (length(numeric_cols) >= 2) {
        cor_matrix <- cor(df[numeric_cols], use = "complete.obs")
        high_cor <- sum(abs(cor_matrix[upper.tri(cor_matrix)]) > 0.7)

        if (high_cor > 0) {
            return(list(primary = "heatmap", reason = "High correlations detected"))
        }
    }

    return(list(primary = "scatterplot", reason = "Default recommendation"))
}

# Call Python API for chart detection
detect_chart_python <- function(image_path) {
    tryCatch(
        {
            response <- POST(
                "http://localhost:8001/detect-chart",
                body = list(file = upload_file(image_path)),
                encode = "multipart"
            )

            if (status_code(response) == 200) {
                result <- content(response, "parsed")
                if (result$success) {
                    return(result$chart_type)
                } else {
                    return(paste("Hata:", result$error))
                }
            } else {
                return("Python API'ye ulaşılamıyor")
            }
        },
        error = function(e) {
            return(paste("Bağlantı hatası:", e$message))
        }
    )
}

# UI
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
      body { background-color: #0f172a; color: #e2e8f0; font-family: 'Inter', sans-serif; }
      .main-title { color: #38bdf8; font-size: 2.5rem; font-weight: bold; margin-bottom: 1rem; }
      .subtitle { color: #94a3b8; margin-bottom: 2rem; }
      .card { background: #1e293b; border-radius: 12px; padding: 2rem; margin-bottom: 2rem; box-shadow: 0 4px 6px rgba(0,0,0,0.3); }
      .section-title { color: #38bdf8; font-size: 1.5rem; font-weight: 600; margin-bottom: 1rem; }
      .info-box { background: #334155; padding: 1rem; border-radius: 8px; margin-top: 1rem; }
      .recommendation-box { background: #065f46; color: #d1fae5; padding: 1rem; border-radius: 8px; margin: 1rem 0; }
      .shiny-input-container { margin-bottom: 1.5rem; }
      label { color: #cbd5e1; font-weight: 500; margin-bottom: 0.5rem; display: block; }
      select, input[type='file'] { background: #334155; color: #e2e8f0; border: 1px solid #475569; border-radius: 6px; padding: 8px 12px; width: 100%; }
      .plot-container { background: white; border-radius: 8px; padding: 1rem; margin-top: 1rem; }
    "))
    ),
    div(
        class = "container", style = "max-width: 1400px; margin: 0 auto; padding: 2rem;",
        h1(class = "main-title", "📊 Smart Chart Assistant"),
        p(class = "subtitle", "AI-Powered Chart Detection + Smart Recommendations"),
        fluidRow(
            # Left Panel - Image Upload
            column(
                6,
                div(
                    class = "card",
                    h3(class = "section-title", "🖼️ Image Detection"),
                    fileInput("image_file", "Upload Chart Image",
                        accept = c("image/png", "image/jpeg", "image/jpg"),
                        buttonLabel = "Select Image",
                        placeholder = "No image selected"
                    ),
                    uiOutput("detected_chart_type_ui"),
                    div(
                        class = "info-box",
                        p(
                            style = "margin: 0; font-size: 0.9rem; color: #94a3b8;",
                            "🐍 Python FastAPI + Feature-Based Detection"
                        )
                    )
                )
            ),

            # Right Panel - CSV Upload
            column(
                6,
                div(
                    class = "card",
                    h3(class = "section-title", "📁 CSV Upload & Smart Recommendations"),
                    fileInput("csv_file", "Upload CSV File",
                        accept = c(".csv"),
                        buttonLabel = "Select CSV",
                        placeholder = "No CSV selected"
                    ),
                    uiOutput("recommendation_ui"),
                    uiOutput("chart_controls_ui"),
                    div(
                        class = "info-box",
                        uiOutput("data_info")
                    )
                )
            )
        ),

        # Chart Display
        div(
            class = "card",
            h3(class = "section-title", "📈 Chart Preview"),
            div(
                class = "plot-container",
                plotOutput("dynamic_plot", height = "600px")
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    # Reactive: Uploaded CSV data
    uploaded_data <- reactive({
        req(input$csv_file)

        tryCatch(
            {
                df <- read_csv(input$csv_file$datapath, show_col_types = FALSE)

                if (is.null(df) || nrow(df) == 0) {
                    showNotification("CSV file is empty or invalid!", type = "error")
                    return(NULL)
                }

                showNotification(paste("✓ Data loaded:", nrow(df), "rows,", ncol(df), "columns"),
                    type = "message", duration = 3
                )
                return(df)
            },
            error = function(e) {
                showNotification(paste("Error:", e$message), type = "error")
                return(NULL)
            }
        )
    })

    # Reactive: Column info
    col_info <- reactive({
        req(uploaded_data())
        df <- uploaded_data()

        types <- get_col_types(df)
        list(
            all_cols = names(df),
            numeric_cols = names(df)[types == "numeric"],
            categorical_cols = names(df)[types == "categorical"]
        )
    })

    # Reactive: Smart recommendation
    recommendation <- reactive({
        req(uploaded_data())

        x <- input$x_var
        y <- input$y_var

        recommend_chart_type(uploaded_data(), x, y)
    })

    # Detected chart type from Python API
    output$detected_chart_type_ui <- renderUI({
        req(input$image_file)

        withProgress(message = "🐍 Python AI analyzing...", value = 0.5, {
            detected_type <- detect_chart_python(input$image_file$datapath)
        })

        # Read image and convert to base64 for display
        image_path <- input$image_file$datapath
        image_data <- base64enc::base64encode(image_path)
        image_ext <- tools::file_ext(input$image_file$name)
        mime_type <- switch(tolower(image_ext),
            "png" = "image/png",
            "jpg" = "image/jpeg",
            "jpeg" = "image/jpeg",
            "image/png"
        )

        div(
            div(
                style = "background: #065f46; color: #d1fae5; padding: 1rem; border-radius: 8px; margin-top: 1rem;",
                h4(style = "margin: 0 0 0.5rem 0; color: #6ee7b7;", "🤖 Detected Chart Type:"),
                p(style = "margin: 0; font-size: 1.2rem; font-weight: 600;", detected_type)
            ),
            div(
                style = "margin-top: 1rem;",
                tags$img(
                    src = paste0("data:", mime_type, ";base64,", image_data),
                    style = "max-width: 100%; height: auto; border-radius: 8px; border: 2px solid #475569;",
                    alt = "Uploaded Image"
                )
            )
        )
    })

    # Smart recommendation UI
    output$recommendation_ui <- renderUI({
        req(uploaded_data())

        rec <- recommendation()

        if (is.list(rec)) {
            div(
                class = "recommendation-box",
                h4(style = "margin: 0 0 0.5rem 0; color: #6ee7b7;", "💡 Smart Recommendation:"),
                p(
                    style = "margin: 0; font-size: 1.1rem; font-weight: 600;",
                    toupper(gsub("_", " ", rec$primary))
                ),
                if (!is.null(rec$secondary)) {
                    p(
                        style = "margin: 0.5rem 0 0 0; font-size: 0.9rem;",
                        paste("Alternative:", toupper(gsub("_", " ", rec$secondary)))
                    )
                },
                p(
                    style = "margin: 0.5rem 0 0 0; font-size: 0.85rem; font-style: italic;",
                    rec$reason
                )
            )
        }
    })

    # Chart controls
    output$chart_controls_ui <- renderUI({
        req(uploaded_data(), col_info())

        # Get recommendation
        rec <- recommendation()
        default_chart <- if (is.list(rec)) rec$primary else "scatterplot"

        tagList(
            selectInput("chart_type", "Chart Type (13 Options)",
                choices = c(
                    "Scatterplot" = "scatterplot",
                    "Line Chart" = "line",
                    "Bar Chart" = "bar",
                    "Grouped Bar Chart" = "grouped_bar",
                    "Stacked Bar Chart" = "stacked_bar",
                    "Boxplot" = "boxplot",
                    "Violin Plot" = "violin",
                    "Histogram" = "histogram",
                    "Histogram + Density" = "hist_density",
                    "Density Plot" = "density",
                    "Area Chart" = "area",
                    "Stacked Area Chart" = "stacked_area",
                    "Pie Chart" = "pie"
                ),
                selected = default_chart
            ),
            selectInput("x_var", "X Axis",
                choices = col_info()$all_cols,
                selected = col_info()$all_cols[1]
            ),
            selectInput("y_var", "Y Axis",
                choices = col_info()$all_cols,
                selected = if (length(col_info()$all_cols) >= 2) col_info()$all_cols[2] else col_info()$all_cols[1]
            ),
            selectInput("color_var", "Color/Group (optional)",
                choices = c("None" = "", col_info()$all_cols),
                selected = ""
            ),
            selectInput("facet_var", "Facet (optional)",
                choices = c("None" = "", col_info()$categorical_cols),
                selected = ""
            )
        )
    })

    # Data info
    output$data_info <- renderUI({
        req(uploaded_data())
        df <- uploaded_data()

        tagList(
            p(
                style = "margin: 0; font-size: 0.9rem;",
                strong("Rows: "), nrow(df), " | ",
                strong("Columns: "), ncol(df), br(),
                strong("Variables: "), paste(names(df), collapse = ", ")
            )
        )
    })

    # Dynamic plot (same as before)
    output$dynamic_plot <- renderPlot({
        req(uploaded_data(), input$chart_type, input$x_var, input$y_var)

        df <- uploaded_data()

        # Build ggplot
        p <- NULL

        if (!is.null(input$color_var) && input$color_var != "") {
            p <- ggplot(df, aes_string(
                x = input$x_var, y = input$y_var,
                color = input$color_var, fill = input$color_var
            ))
        } else {
            p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var))
        }

        # Add geom based on chart type
        if (input$chart_type == "scatterplot") {
            p <- p + geom_point(alpha = 0.7, size = 3)
        } else if (input$chart_type == "line") {
            p <- p + geom_line(linewidth = 1.2)
        } else if (input$chart_type == "bar") {
            p <- p + geom_bar(stat = "identity", alpha = 0.8)
        } else if (input$chart_type == "grouped_bar") {
            if (!is.null(input$color_var) && input$color_var != "") {
                p <- p + geom_bar(stat = "identity", position = "dodge", alpha = 0.8)
            } else {
                p <- p + geom_bar(stat = "identity", alpha = 0.8)
            }
        } else if (input$chart_type == "stacked_bar") {
            if (!is.null(input$color_var) && input$color_var != "") {
                p <- p + geom_bar(stat = "identity", position = "stack", alpha = 0.8)
            } else {
                p <- p + geom_bar(stat = "identity", alpha = 0.8)
            }
        } else if (input$chart_type == "boxplot") {
            p <- p + geom_boxplot(alpha = 0.7)
        } else if (input$chart_type == "violin") {
            p <- p + geom_violin(alpha = 0.7)
        } else if (input$chart_type == "histogram") {
            p <- ggplot(df, aes_string(x = input$x_var)) +
                geom_histogram(bins = 30, alpha = 0.8, fill = "#3b82f6")
        } else if (input$chart_type == "hist_density") {
            p <- ggplot(df, aes_string(x = input$x_var)) +
                geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.6, fill = "#3b82f6") +
                geom_density(alpha = 0.4, fill = "#f59e0b", color = "#f59e0b", linewidth = 1)
        } else if (input$chart_type == "density") {
            p <- ggplot(df, aes_string(x = input$x_var)) +
                geom_density(alpha = 0.5, fill = "#3b82f6")
        } else if (input$chart_type == "area") {
            p <- p + geom_area(alpha = 0.6)
        } else if (input$chart_type == "stacked_area") {
            if (!is.null(input$color_var) && input$color_var != "") {
                p <- p + geom_area(position = "stack", alpha = 0.7)
            } else {
                p <- p + geom_area(alpha = 0.6)
            }
        } else if (input$chart_type == "pie") {
            if (!is.null(input$color_var) && input$color_var != "") {
                p <- ggplot(df, aes_string(x = "''", y = input$y_var, fill = input$color_var)) +
                    geom_bar(stat = "identity", width = 1) +
                    coord_polar("y", start = 0)
            }
        }

        # Add faceting
        if (!is.null(input$facet_var) && input$facet_var != "") {
            p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
        }

        # Theme
        p <- p +
            theme_minimal(base_size = 14) +
            theme(
                plot.title = element_text(face = "bold", size = 18, color = "#1e293b"),
                plot.background = element_rect(fill = "white", color = NA),
                panel.background = element_rect(fill = "white", color = NA),
                legend.position = "right",
                panel.grid.minor = element_blank()
            ) +
            labs(
                title = paste(input$chart_type, ":", input$y_var, "vs", input$x_var),
                x = input$x_var,
                y = input$y_var
            )

        p
    })
}

# Run app
shinyApp(ui = ui, server = server)
