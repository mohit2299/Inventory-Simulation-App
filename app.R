# Inventory Simulation App (refactored)
# MIT License

library(shiny)
library(ggplot2)
library(DT)
library(stringr)

# ---------------------------
# Helpers
# ---------------------------

parse_monthly <- function(x, n = 12) {
  # Accept "a,b,c" or empty; coerce to numeric length n (pad with zeros)
  vals <- str_split(trimws(x), "\\s*,\\s*", simplify = TRUE)
  vals <- suppressWarnings(as.numeric(vals[vals != ""]))
  if (length(vals) == 0) vals <- numeric(0)
  if (length(vals) < n) vals <- c(vals, rep(0, n - length(vals)))
  if (length(vals) > n) vals <- vals[1:n]
  replace(vals, is.na(vals), 0)
}

round_up_pack <- function(qty, pack) {
  if (pack <= 1) return(max(0, qty))
  if (qty <= 0) return(0)
  ceiling(qty / pack) * pack
}

# ---------------------------
# UI
# ---------------------------

ui <- fluidPage(
  titlePanel("Inventory Simulation - Flexible Replenishment Cycle"),

  sidebarLayout(
    sidebarPanel(
      h4("Policy & Controls"),
      numericInput("start_inventory", "Starting Inventory", value = 12000, min = 0, step = 1),
      selectInput("lead_time", "Lead Time (months)", choices = c(2,4), selected = 4),
      numericInput("zscore", "Z-score (service level)", value = 1.65, min = 0, step = 0.05),
      numericInput("reorder_point", "Reorder Point (units)", value = 54350, min = 0, step = 1),
      numericInput("tolerance_pct", "Understock Tolerance (%)", value = 5, min = 0, max = 50, step = 1),
      selectInput("cycle", "Replenishment Cycle (months)", choices = c(1, 2, 4), selected = 4),
      numericInput("pack_size", "Pack Size (round up orders to)", value = 1, min = 1, step = 1),

      tags$hr(),
      h4("Demand (12 months)"),
      fluidRow(
        column(6,
               numericInput("m1",  "M1", value = 380,   min = 0),
               numericInput("m2",  "M2", value = 673,   min = 0),
               numericInput("m3",  "M3", value = 7646,  min = 0),
               numericInput("m4",  "M4", value = 12081, min = 0),
               numericInput("m5",  "M5", value = 6426,  min = 0),
               numericInput("m6",  "M6", value = 1595,  min = 0)
        ),
        column(6,
               numericInput("m7",  "M7", value = 4366,  min = 0),
               numericInput("m8",  "M8", value = 626,   min = 0),
               numericInput("m9",  "M9", value = 6047,  min = 0),
               numericInput("m10", "M10", value = 152,  min = 0),
               numericInput("m11", "M11", value = 1890, min = 0),
               numericInput("m12", "M12", value = 637,  min = 0)
        )
      ),

      tags$hr(),
      h4("On-Order Arrivals by Month"),
      helpText("Comma-separated 12 values; arrivals are ADDED to the month they land."),
      textInput("in_transit", "In-Transit Arrivals (units)", value = "0,0,0,0,0,0,0,0,0,0,0,0"),
      textInput("factory",    "Factory Arrivals (units)",   value = "0,0,0,0,0,0,0,0,0,0,0,0"),

      actionButton("runSim", "Run Simulation", class = "btn-primary")
    ),

    mainPanel(
      h4("Simulation Results"),
      DTOutput("table"),
      br(),
      plotOutput("inventoryPlot"),
      br(),
      plotOutput("orderPlot")
    )
  )
)

# ---------------------------
# Server
# ---------------------------

server <- function(input, output) {

  simulateInventory <- eventReactive(input$runSim, {
    demand <- c(input$m1, input$m2, input$m3, input$m4, input$m5, input$m6,
                input$m7, input$m8, input$m9, input$m10, input$m11, input$m12)
    months <- length(demand)

    # arrivals supplied by user
    in_transit_arr <- parse_monthly(input$in_transit, months)
    factory_arr    <- parse_monthly(input$factory, months)
    ext_inbound    <- in_transit_arr + factory_arr

    # derived policy numbers
    L   <- as.numeric(input$lead_time)
    Dbar <- mean(demand)                 # avg monthly demand
    # NOTE: supply your own historical SD if needed; using sample SD from inputs as proxy
    sigma <- sd(demand)
    if (is.na(sigma) || sigma == 0) sigma <- 0.2 * Dbar
    SS  <- input$zscore * sigma * sqrt(L)
    LTD <- Dbar * L
    threshold <- LTD * (1 - input$tolerance_pct / 100)

    # vectors
    inventory_start <- numeric(months)
    inventory_end   <- numeric(months)
    order_qty_raw   <- numeric(months)
    order_qty       <- numeric(months)
    decision        <- character(months)
    flag            <- character(months)

    inventory_start[1] <- input$start_inventory

    for (i in seq_len(months)) {
      # Order decision only on cycle month, and only if at/below ROP
      place_order <- (i %% as.numeric(input$cycle) == 0) && (inventory_start[i] <= input$reorder_point)

      if (place_order) {
        decision[i] <- "Order"
        target <- input$reorder_point
        order_needed <- max(0, target - inventory_start[i])
        order_qty_raw[i] <- order_needed
        order_qty[i] <- round_up_pack(order_needed, input$pack_size)
      } else {
        decision[i] <- "Review Only"
        order_qty_raw[i] <- 0
        order_qty[i] <- 0
      }

      inbound_total <- order_qty[i] + ext_inbound[i]

      # Ending inventory
      inventory_end[i] <- inventory_start[i] + inbound_total - demand[i]

      # Flags
      if (inventory_end[i] < 0) {
        flag[i] <- "Stockout"
      } else if (inventory_end[i] < threshold) {
        flag[i] <- "Understock Risk"
      } else if (inventory_end[i] > input$reorder_point) {
        flag[i] <- "Overstock"
      } else {
        flag[i] <- "Normal"
      }

      if (i < months) inventory_start[i + 1] <- inventory_end[i]
    }

    data.frame(
      Month               = 1:months,
      Demand              = demand,
      InTransit_Arrival   = in_transit_arr,
      Factory_Arrival     = factory_arr,
      Starting_Inventory  = inventory_start,
      Ordered_Qty_Packed  = order_qty,
      Ending_Inventory    = inventory_end,
      Decision            = decision,
      Flag                = flag,
      # Policy reference columns
      Lead_Time_Months    = L,
      Z_Score             = input$zscore,
      LTD                 = round(LTD, 2),
      Safety_Stock        = round(SS, 2),
      Threshold           = round(threshold, 2),
      Reorder_Point       = input$reorder_point,
      Pack_Size           = input$pack_size,
      stringsAsFactors = FALSE
    )
  })

  output$table <- renderDT({
    datatable(simulateInventory(), options = list(pageLength = 12)) %>%
      formatStyle("Flag",
                  target = "row",
                  backgroundColor = styleEqual(
                    c("Stockout", "Understock Risk", "Normal", "Overstock"),
                    c("#FFCCCC", "#FFF2CC", "#CCFFCC", "#CCE5FF")
                  ),
                  fontWeight = styleEqual("Stockout", "bold"))
  })

  output$inventoryPlot <- renderPlot({
    df <- simulateInventory()
    ggplot(df, aes(x = Month, y = Ending_Inventory, fill = Flag)) +
      geom_col() +
      scale_fill_manual(values = c("Stockout" = "#FF9999",
                                   "Understock Risk" = "#FFD580",
                                   "Normal" = "#99FF99",
                                   "Overstock" = "#99CCFF")) +
      labs(title = "Ending Inventory with Risk Flags", y = "Units") +
      theme_minimal()
  })

  output$orderPlot <- renderPlot({
    df <- simulateInventory()
    ggplot(df, aes(x = Month, y = Ordered_Qty_Packed)) +
      geom_col() +
      labs(title = "Order Quantities (rounded to pack size)", y = "Units") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
