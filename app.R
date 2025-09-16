library(shiny)
library(leaflet)
library(terra)
library(sf)
library(dplyr)

# Load raster files
raster_files <- list.files(
  "data",
  pattern = "_prediction.tif$", full.names = TRUE
)

# Normalize names to lowercase
names(raster_files) <- tolower(gsub("_prediction.tif", "", basename(raster_files)))

# Load soil samples
soil_samples <- st_read("data/sierra_leone_soil_data.shp")

# Define custom fertility-aware palettes
soil_palettes <- list(
  "phosphorus" = colorRampPalette(c("#d73027", "#fee08b", "#1a9850")),
  "ph"         = colorRampPalette(c("#4575b4", "#ffffbf", "#a50026")),
  "nitrogen"   = colorRampPalette(c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")),
  "potassium"  = colorRampPalette(c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8")),
  "carbon"     = colorRampPalette(c("#d9f0a3", "#addd8e", "#78c679", "#31a354", "#006837"))
)

# Legend titles
legend_titles <- list(
  "phosphorus" = "Phosphorus (mg/kg)",
  "ph"         = "Soil pH",
  "nitrogen"   = "Nitrogen (%)",
  "potassium"  = "Potassium (mg/kg)",
  "carbon"     = "Carbon (%)"
)

# RMSE table
rmse_values <- list(
  phosphorus = 9.762,
  ph = 0.432,
  nitrogen = 0.050,
  potassium = 21.285,
  carbon = 1.412
)

# Importance statements
importance_statements <- list(
  phosphorus = "Phosphorus is essential for root development and crop maturity.",
  ph = "Soil pH controls nutrient availability and microbial activity.",
  nitrogen = "Nitrogen promotes leafy growth and overall productivity.",
  potassium = "Potassium enhances disease resistance and water regulation.",
  carbon = "Soil Carbon enhances water retention, nutrient availability, and Soil Structure. 
  It also contributes to climate change mitigation by acting as a carbon sink."
)

# UI
ui <- fluidPage(
  titlePanel("Soil Fertility Map Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("soil_var", "Select Soil Variable:", choices = names(raster_files)),
      checkboxInput("show_points", "Show Soil Sample Points", TRUE),
      selectInput("basemap", "Select Basemap:",
                  choices = c("Topo Map" = "Esri.WorldTopoMap",
                              "Satellite" = "Esri.WorldImagery",
                              "OpenStreetMap" = "OpenStreetMap")),
      tags$hr(),
      tags$h4("Model Performance (RMSE)"),
      tags$ul(
        tags$li("Phosphorus: 9.762"),
        tags$li("pH: 0.432"),
        tags$li("Nitrogen: 0.050"),
        tags$li("Potassium: 21.285"),
        tags$li("Carbon: 1.412")
      ),
      tags$hr(),
      tags$h4("Importance of Soil Layers"),
      tags$p(importance_statements$phosphorus),
      tags$p(importance_statements$ph),
      tags$p(importance_statements$nitrogen),
      tags$p(importance_statements$potassium),
      tags$p(importance_statements$carbon)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(" Map", leafletOutput("map", height = 400)),
        tabPanel("Histogram", plotOutput("hist", height = 400))
      ),
      br(),
      verbatimTextOutput("stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  selected_raster <- reactive({
    r <- rast(raster_files[[input$soil_var]])
    valid_vals <- values(r)
    if (all(is.na(valid_vals))) {
      showNotification(paste("Selected raster for", input$soil_var, "has no valid values."), type = "error")
      return(NULL)
    }
    r
  })
  
  selected_palette <- reactive({
    var_name <- tolower(input$soil_var)
    pal_func <- soil_palettes[[var_name]]
    if (is.null(pal_func)) {
      showNotification(paste("Palette not defined for", input$soil_var), type = "error")
      return(NULL)
    }
    pal_colors <- pal_func(100)
    if (length(pal_colors) == 0) {
      showNotification(paste("Palette returned no colors for", input$soil_var), type = "error")
      return(NULL)
    }
    pal_colors
  })
  
  nice_title <- reactive({
    legend_titles[[tolower(input$soil_var)]] %||% input$soil_var
  })
  
  output$map <- renderLeaflet({
    r <- selected_raster()
    pal_colors <- selected_palette()
    req(r, pal_colors)
    
    vals <- values(r)
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NULL)
    
    pal <- colorNumeric(palette = pal_colors, domain = vals, na.color = "transparent")
    
    # Reproject points if needed
    samples_trans <- if (!st_crs(soil_samples) == crs(r)) {
      st_transform(soil_samples, crs(r))
    } else {
      soil_samples
    }
    
    leaflet() %>%
      addProviderTiles(input$basemap, group = "Basemap") %>%
      addRasterImage(r, colors = pal, opacity = 0.9, group = input$soil_var) %>%
      addLegend(pal = pal, values = vals, title = nice_title()) %>%
      {
        if (input$show_points) {
          addCircleMarkers(., data = samples_trans,
                           radius = 4,
                           fillColor = "purple",
                           fillOpacity = 0.6,
                           color = "white",
                           weight = 0.5,
                           label = ~paste("District:", District, "<br>pH:", pH),
                           group = "Soil Samples")
        } else .
      } %>%
      addLayersControl(
        baseGroups = c("Topo Map", "Satellite", "OpenStreetMap"),
        overlayGroups = c(input$soil_var, "Soil Samples"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      fitBounds(xmin(r), ymin(r), xmax(r), ymax(r))
  })
  
  output$hist <- renderPlot({
    r <- selected_raster()
    pal_colors <- selected_palette()
    req(r, pal_colors)
    
    vals <- values(r)
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return()
    
    hist(vals,
         breaks = 30,
         main = paste("Distribution of", nice_title()),
         xlab = nice_title(),
         col = pal_colors[50],  # Use middle color for bars
         border = "white")
  })
  
  output$stats <- renderPrint({
    r <- selected_raster()
    req(r)
    
    vals <- values(r)
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return("No valid data in selected raster.")
    summary(vals)
  })
}

# Run the app
shinyApp(ui, server)