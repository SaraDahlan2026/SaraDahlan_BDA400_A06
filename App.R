library(shiny)
library(ggplot2)
library(quantmod)

# Fetch Apple stock data from Yahoo Finance
stock_data <- getSymbols(
  "AAPL",
  src = "yahoo",
  from = "2023-01-01",
  to = "2023-07-02",
  auto.assign = FALSE
)

# Create the user interface
ui <- fluidPage(
  titlePanel("Stock Portfolio Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-07-01",
        min = "2023-01-01",
        max = "2023-07-01"
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c("Line", "Candlestick", "Area")
      ),
      
      checkboxGroupInput(
        "technical_indicators",
        "Select Technical Indicators:",
        choices = c("Moving Averages", "RSI", "MACD")
      )
    ),
    
    mainPanel(
      plotOutput("stock_chart", height = "500px")
    )
  )
)
# Create the server logic
server <- function(input, output) {
  
  output$stock_chart <- renderPlot({
    req(input$date_range)
    
    # Filter data by the selected dates
    dates <- as.Date(index(stock_data))
    
    filtered_data <- stock_data[
      dates >= input$date_range[1] &
        dates <= input$date_range[2]
    ]
    
    validate(
      need(NROW(filtered_data) > 0,
           "Please select a date range with trading data.")
    )
    
    # Convert daily data to weekly or monthly data
    if (input$time_frame == "Weekly") {
      filtered_data <- to.weekly(
        filtered_data,
        indexAt = "endof",
        drop.time = TRUE
      )
    } else if (input$time_frame == "Monthly") {
      filtered_data <- to.monthly(
        filtered_data,
        indexAt = "endof",
        drop.time = TRUE
      )
    }
    
    # Prepare the data for ggplot
    chart_data <- data.frame(
      Date = as.Date(index(filtered_data)),
      Open = as.numeric(Op(filtered_data)),
      High = as.numeric(Hi(filtered_data)),
      Low = as.numeric(Lo(filtered_data)),
      Close = as.numeric(Cl(filtered_data))
    )
    
    p <- ggplot(chart_data, aes(x = Date))
    
    # Draw the selected chart type
    if (input$chart_type == "Line") {
      p <- p + geom_line(
        aes(y = Close),
        colour = "steelblue",
        linewidth = 0.8
      ) + geom_point(aes(y = Close), colour = "steelblue")
      
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(
        aes(y = Close),
        fill = "steelblue",
        alpha = 0.4
      ) + geom_line(aes(y = Close), colour = "steelblue")
      
    } else {
      candle_width <- switch(
        input$time_frame,
        "Daily" = 0.7,
        "Weekly" = 4,
        "Monthly" = 18
      )
      
      p <- p +
        geom_segment(
          aes(xend = Date, y = Low, yend = High)
        ) +
        geom_rect(
          aes(
            xmin = Date - candle_width / 2,
            xmax = Date + candle_width / 2,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Close >= Open
          ),
          colour = "black"
        ) +
        scale_fill_manual(
          values = c("FALSE" = "red", "TRUE" = "seagreen"),
          guide = "none"
        )
    }
    
    p <- p +
      labs(
        title = paste("AAPL -", input$time_frame, input$chart_type),
        x = "Date",
        y = "Price (USD)"
      ) +
      theme_minimal()
    # Overlay 20-day and 50-day moving averages
    if ("Moving Averages" %in% input$technical_indicators) {
      
      ma20 <- TTR::SMA(Cl(stock_data), n = 20)
      ma50 <- TTR::SMA(Cl(stock_data), n = 50)
      
      positions <- match(
        chart_data$Date,
        as.Date(index(stock_data))
      )
      
      chart_data$MA20 <- as.numeric(ma20)[positions]
      chart_data$MA50 <- as.numeric(ma50)[positions]
      
      p <- p +
        geom_line(
          data = chart_data,
          aes(y = MA20, colour = "20-day MA"),
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        geom_line(
          data = chart_data,
          aes(y = MA50, colour = "50-day MA"),
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        scale_colour_manual(
          name = "Moving Averages",
          values = c(
            "20-day MA" = "orange",
            "50-day MA" = "purple"
          )
        )
    }   
    # Prepare daily indicator values
    positions <- match(
      chart_data$Date,
      as.Date(index(stock_data))
    )
    # Generate signals using the assignment's example rule
    short_ma <- as.numeric(
      TTR::SMA(Cl(stock_data), n = 20)
    )[positions]
    
    long_ma <- as.numeric(
      TTR::SMA(Cl(stock_data), n = 50)
    )[positions]
    
    chart_data$Signal <- ifelse(
      is.na(short_ma) | is.na(long_ma),
      NA_character_,
      ifelse(
        short_ma > long_ma,
        "Buy",
        ifelse(short_ma < long_ma, "Sell", "Hold")
      )
    )
    
    # Annotate the stock price chart
    p <- p + geom_text(
      data = chart_data,
      aes(
        x = Date,
        y = Close,
        label = Signal
      ),
      vjust = 1.5,
      colour = "blue",
      size = 3,
      check_overlap = TRUE,
      na.rm = TRUE
    )    
    plots <- list(p)
    heights <- c(3)
    
    # RSI panel
    if ("RSI" %in% input$technical_indicators) {
      rsi_values <- TTR::RSI(Cl(stock_data), n = 14)
      chart_data$RSI <- as.numeric(rsi_values)[positions]
      
      rsi_plot <- ggplot(chart_data, aes(Date, RSI)) +
        geom_line(colour = "purple", na.rm = TRUE) +
        geom_hline(
          yintercept = c(30, 70),
          linetype = "dashed",
          colour = "red"
        ) +
        coord_cartesian(ylim = c(0, 100)) +
        labs(
          title = "14-day RSI",
          x = NULL,
          y = "RSI"
        ) +
        theme_minimal()
      
      plots <- append(plots, list(rsi_plot))
      heights <- c(heights, 1.5)
    }
    
    # MACD panel
    if ("MACD" %in% input$technical_indicators) {
      macd_values <- TTR::MACD(
        Cl(stock_data),
        nFast = 12,
        nSlow = 26,
        nSig = 9,
        percent = FALSE
      )
      
      chart_data$MACD <-
        as.numeric(macd_values[, 1])[positions]
      chart_data$Signal <-
        as.numeric(macd_values[, 2])[positions]
      chart_data$Histogram <-
        chart_data$MACD - chart_data$Signal
      
      macd_plot <- ggplot(chart_data, aes(x = Date)) +
        geom_col(
          aes(y = Histogram),
          fill = "grey70",
          na.rm = TRUE
        ) +
        geom_line(
          aes(y = MACD, colour = "MACD"),
          na.rm = TRUE
        ) +
        geom_line(
          aes(y = Signal, colour = "Signal"),
          na.rm = TRUE
        ) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_colour_manual(
          name = NULL,
          values = c("MACD" = "blue", "Signal" = "orange")
        ) +
        labs(
          title = "Daily MACD (12, 26, 9)",
          x = NULL,
          y = "MACD (USD)"
        ) +
        theme_minimal()
      
      plots <- append(plots, list(macd_plot))
      heights <- c(heights, 1.5)
    }
    
    # Display the selected panels
    grid::grid.newpage()
    grid::pushViewport(
      grid::viewport(
        layout = grid::grid.layout(
          nrow = length(plots),
          ncol = 1,
          heights = grid::unit(heights, "null")
        )
      )
    )
    
    for (i in seq_along(plots)) {
      print(
        plots[[i]],
        vp = grid::viewport(
          layout.pos.row = i,
          layout.pos.col = 1
        )
      )
    }
    
    grid::popViewport()
  })
}

# Launch the app
shinyApp(ui = ui, server = server)