#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(sf)
library(shiny)
library(shinydashboard)
library(leaflet)
library(scales)
library(ggplot2)
library(rsconnect)
library(plotly)
library(ggpmisc)

# load data
NDMN_HEV_data <- readRDS('NDMN_HEV_data.RDS')
NDMN_map_data <- readRDS('NDMN_map_data.RDS')

# load EA areas
url_request <- "https://environment.data.gov.uk/arcgis/rest/services/EA/AdminBoundEAandNEpublicFaceAreas/FeatureServer/0/query?where=seaward='No'&outFields=*&f=geojson"
ea.areas <- st_read(url_request)

## define dashboard format
ui <- dashboardPage(
    dashboardHeader(title = "NDMN HEV plots"),
    dashboardSidebar(sidebarMenu(
        menuItem(varSelectInput('biol_metric_selector', 'Select Invertebrate Metric', 
                                NDMN_HEV_data[, c("WHPT_ASPT_OE", "WHPT_NTAXA_OE", "LIFE_F_OE", "PSI_OE")])),
        menuItem(varSelectInput('flow_metric_selector', 'Select Flow Metric', NDMN_HEV_data[11:21])),
        menuItem(sliderInput('date_range_selector', 'Select date range', min= 1995, 
                             max= 2023, value = c(1995, 2023), sep = "")),
        menuItem("Select invertebrate sample site", leafletOutput("map"), startExpanded = TRUE)
    ), width = 275),
    dashboardBody(
        fluidRow(conditionalPanel("input.map_marker_click",
                                  plotlyOutput("plot1", height = 400), br())),
        fluidRow(conditionalPanel("input.map_marker_click",
                                  plotOutput("plot2", height = 400))
        )
    )
)

## render dashboard
server <- function(input, output, session) {
    
    # leaflet map
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            addPolygons(data = ea.areas, color = "green") %>% 
            addCircleMarkers(data = NDMN_map_data, ~unique(lon), ~unique(lat), 
                             layerId = ~unique(biol_site_id), popup = ~paste(unique(biol_site_id), "<br>", 
                                                                             WATER_BODY))
    })
    
    # reactive expression to activate map clicks
    ggplot_data <- reactive({
        site <- input$map_marker_click$id
        NDMN_HEV_data[NDMN_HEV_data$biol_site_id %in% site,]
    })
    
    # HEV plot
    output$plot1 <- renderPlotly({
        
        plot_ly(data = ggplot_data() %>% filter(Year >= input$date_range_selector[1] & Year <= input$date_range_selector[2])) %>% 
            add_lines(x= ~date, y = ~get(input$flow_metric_selector), line = list(color = "deepskyblue4"), name = "Flow") %>% 
            add_markers(x= ~date, y = ~get(input$biol_metric_selector), 
                        marker = list(color = "#c08f3c", line = list(color = "black", width =2)), name = "O:E ratio", yaxis = "y2", 
                        type = "scatter", size = 10) %>% layout(title = paste(input$flow_metric_selector,"vs",input$biol_metric_selector, "at site",input$map_marker_click$id), 
                                                                xaxis = list(title = "Date"), yaxis = list(side = "left", title = "Flow (m3/s)",
                                                                                                           zeroline = F, hoverformat = ".2f", showgrid = F), 
                                                                yaxis2 = list(side = "right", title = "Invertebrate O:E ratio", overlaying = "y", zeroline = F, 
                                                                              hoverformat = ".3f", showgrid = F)) 
        
    })
    
    # regression plot
    formula <- y ~ poly(x, 3, raw = TRUE)
    
    output$plot2 <- renderPlot({
        ggplot(data = ggplot_data() %>% filter(Year >= input$date_range_selector[1] & Year <= input$date_range_selector[2]),
               aes(x = !!input$flow_metric_selector, y = !!input$biol_metric_selector)) +
            geom_point(size=3, color="#c08f3c") +
            stat_poly_line(color="gray", formula = formula) +
            stat_poly_eq(formula = formula, size=6) +
            labs(x="Flow (m3/s)", y = "Invertebrate O:E ratio") +
            theme_minimal()+
            theme(text = element_text(size = 16), axis.text.x = element_text(size = 14),
                  axis.text.y = element_text(size = 14))
        
    }) 
    
}

NDMN_HEV_dashboard <- shinyApp(ui, server)
runApp(NDMN_HEV_dashboard)

deployApp()
