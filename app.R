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
names(raster_files) <- gsub("_prediction.tif", "", basename(raster_files))

# Load soil samples
soil_samples <- st_read("data/sierra_leone_soil_data.shp")

# Define custom fertility-aware palettes
soil_palettes <- list(
  "phosphorus" = colorRampPalette(c("#d73027", "#fee08b", "#1a9850")),  # red → yellow → green
  "pH"         = colorRampPalette(c("#4575b4", "#ffffbf", "#a50026")),  # blue → yellow → red
  "nitrogen"   = colorRampPalette(c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")),  # light orange → red
  "potassium"  = colorRampPalette(c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8"))   # yellow → teal
)

# Legend titles
legend_titles <- list(
  "phosphorus" = "Phosphorus (mg/kg)",
  "pH"         = "Soil pH",
  "nitrogen"   = "Nitrogen (%)",
  "potassium"  = "Potassium (mg/kg)"
)

# RMSE table
rmse_values <- list(
  phosphorus = 6.60,
  pH = 0.22,
  nitrogen = 0.04,
  potassium = 12.69
)

# Importance statements
importance_statements <- list(
  phosphorus = "Phosphorus is essential for root development and crop maturity.",
  pH = "Soil pH controls nutrient availability and microbial activity.",
  nitrogen = "Nitrogen promotes leafy growth and overall productivity.",
  potassium = "Potassium enhances disease resistance and water regulation."
)

# UI
ui <- fluidPage(
  titlePanel("🧪 Soil Fertility Map Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("soil_var", "Select Soil Variable:", choices = names(raster_files)),
      checkboxInput("show_points", "Show Soil Sample Points", TRUE),
      selectInput("basemap", "Select Basemap:",
                  choices = c("Topo Map" = "Esri.WorldTopoMap",
                              "Satellite" = "Esri.WorldImagery",
                              "OpenStreetMap" = "OpenStreetMap")),
      tags$hr(),
      tags$h4("📈 Model Performance (RMSE)"),
      tags$ul(
        tags$li("Phosphorus: 6.60"),
        tags$li("pH: 0.22"),
        tags$li("Nitrogen: 0.04"),
        tags$li("Potassium: 12.69")
      ),
      tags$hr(),
      tags$h4("🧠 Importance of Soil Layers"),
      tags$p("Phosphorus: Essential for root development and crop maturity."),
      tags$p("pH: Controls nutrient availability and microbial activity."),
      tags$p("Nitrogen: Promotes leafy growth and crop productivity."),
      tags$p("Potassium: Enhances disease resistance and water regulation.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("🗺️ Map", leafletOutput("map", height = 400)),
        tabPanel("📊 Histogram", plotOutput("hist", height = 400))
      ),
      br(),
      verbatimTextOutput("stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  selected_raster <- reactive({
    rast(raster_files[[input$soil_var]])
  })
  
  selected_palette <- reactive({
    pal_func <- soil_palettes[[input$soil_var]]
    if (is.null(pal_func)) colorRampPalette(c("grey90", "grey10")) else pal_func(100)
  })
  
  nice_title <- reactive({
    legend_titles[[input$soil_var]] %||% input$soil_var
  })
  
  output$map <- renderLeaflet({
    r <- selected_raster()
    
    # Reproject points if needed
    if (!st_crs(soil_samples) == crs(r)) {
      samples_trans <- st_transform(soil_samples, crs(r))
    } else {
      samples_trans <- soil_samples
    }
    
    # Prepare color mapping
    vals <- values(r)
    vals <- vals[!is.na(vals)]
    pal <- colorNumeric(palette = selected_palette(), domain = vals, na.color = "transparent")
    
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
    vals <- values(r)
    vals <- vals[!is.na(vals)]
    
    hist(vals,
         breaks = 30,
         main = paste("Distribution of", nice_title()),
         xlab = nice_title(),
         col = "#66c2a5",
         border = "white")
  })
  
  output$stats <- renderPrint({
    r <- selected_raster()
    vals <- values(r)
    vals <- vals[!is.na(vals)]
    summary(vals)
  })
}

# Run the app
shinyApp(ui, server)