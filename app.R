# ATLAS of Montesinho/Nogueira special protection area (MN-SPA)
# Created for MontObEO project


#Shiny library and dashboard
library(shiny)
library(shinydashboard)
library(shinyjs)

# Library for WebGIS
library(terra)
library(raster)
library(leaflet)
library(leaflet.extras)

## Library for spp. tree
library(tibble)
library(echarts4r)

# Load objects:
source('Objects.R')


# Define UI for application
ui <- dashboardPage( skin = "purple",
                     
                     # Dashboard title
                     dashboardHeader(title="Menu",titleWidth = 250),
                     
                     
                     # Dashboard sidebar
                     dashboardSidebar(
                       width = 250,
                       sidebarMenu(
                         menuItem("WebGIS", tabName = "webgis", badgeLabel = icon("map"), badgeColor = "maroon"),
                         menuItem("Biodiversity data", tabName = "Biodata", badgeLabel = icon("tree"), badgeColor = "green"),
                         menuItem("Biodiversity curiosities", tabName = "BioCur", badgeLabel = icon("random"), badgeColor = "teal"),
                         HTML(paste0(
                           "<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>",
                           "<a href='https://montobeo.wordpress.com/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src= 'https://github.com/BravoAlpha2/WebGIS/blob/main/Images/Logo1.png?raw=true' width = '166'></a>",
                           "<br>",
                           "<p style = 'text-align: center;'><small><a href='https://montobeo.wordpress.com/' target='_blank'>MontObEO logo</a></small></p>"
                         )),
                         HTML(paste0(
                           "<br><br><br>",
                           "<table style='margin-left:auto; margin-right:auto;'>",
                           "<tr>",
                           "<td style='padding: 5px;'><a href='https://www.facebook.com/MontObEO' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
                           "<td style='padding: 5px;'><a href='https://www.linkedin.com/in/montobeo-project-520b00234/' target='_blank'><i class='fab fa-linkedin fa-lg'></i></a></td>",
                           "<td style='padding: 5px;'><a href='https://twitter.com/SBLab2' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
                           "<td style='padding: 5px;'><a href='https://www.instagram.com/montobeo/' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
                           "<td style='padding: 5px;'><a href='https://montobeo.wordpress.com/' target='_blank'><i class='fab fa-flickr fa-lg'></i></a></td>",
                           "</tr>",
                           "</table>",
                           "<br>"),
                           HTML(paste0(
                             "<script>",
                             "var today = new Date();",
                             "var yyyy = today.getFullYear();",
                             "</script>",
                             "<p style = 'text-align: center;'><small>&copy; - <a href='https://montobeo.wordpress.com/' target='_blank'>MontObEO.com</a> - <script>document.write(yyyy);</script></small></p>")
                           ))
                       )
                     ),  #sidebarMenu End
                     
                     # Dashboard body
                     dashboardBody(
                       tabItems(
                         tabItem("webgis",
                                 
                                 fluidPage(
                                   
                                          box(
                                               leafletOutput(outputId = "map", height = 685)
                                              ),
                                           
                                           box(br(),
                                               
                                               sidebarPanel(
                                                 br(),
                                                 selectInput(inputId = "Taxons", label = "Select the taxonomic group:", choices = c("Taxonomic group", as.character(sort(unique(AtlasEN$Taxonomic.groups))))),
                                                 selectInput(inputId = "Genuss", label = "Select the genus (scientific name):", choices = ""),
                                                 selectInput(inputId = "Speciess", label = "Select the species (scientific name):", choices = ""), width = 6
                                               ),
                                               sidebarPanel(
                                                 selectInput("EVs_EN", "Enviromental Variables (EVs):",h3("Checkbox group"), choices=names(modis.rasters_EN)),
                                                 sliderInput("Opacity", "Opacity of EVs:", min = 0, max = 1, value = 0.35), width = 6
                                               )
                                           ),
                                           
                                           tabBox(
                                             
                                             tabPanel("Description",
                                                      "MontObEO Web Geographic Information System (WebGIS), designed for Montesinho/Nogueira special protection area (MN-SPA) which integrates Natura 2000 network (https://natura2000.eea.europa.eu/Natura2000/SDF.aspx?site=PTCON0002).
                                                      Biodiversity data is represent on 1km spatial resolution and 
                                                      enviromental variables (EVs) presented were used in ecological niche models (ENMs) on MontObEO project."),
                                             
                                             tabPanel("Species","Biodiversity data was compiled for the period 2000-2021 using several data sources such as online databases 
                                                    (e.g., Global Biodiversity Information Facility (GBIF; https://www.gbif.org/)) distribution atlases, inventory datasets and field-collected data.  
                                                    The data was filtered and corrected for five major taxonomic groups: flora (vascular plants), amphibians, reptiles, birds and mammals. 
                                                    For more informations, please consult in menu: 'Biodiversity data'."),
                                             
                                             tabPanel("Enviromental Variables","These Enviromental Variables (EPs) were collected trought Google Earth Engine (GEE; https://earthengine.google.com/) and represent the mean values for the 2001-2021 period. 
                                               The EPs compiled were: 'Enhance Vegetation Index' (EVI); 'Day Land Surface Temperature' (LSTDay) (°C); 'Night Land Surface Temperature' (LSTNight) (°C);
                                              'Surface Refletance (Band 1: 620-670nm)' (SR_B1); 'Time Since Fire' (TSF) (Years); 'Area Annualy Burned (presence/absence)' (AAB).
                                               P.S.: Area Annualy Burned (presence/absence) is not present in the selection of EPs."),
                                             
                                             tabPanel("Authors and Citation", "The main creator of this WebGIS was Nuno Garcia, post-graduate student in the MontObEO project.
                                                 If you would like to quote this WebGIS, please use the following citation: Garcia, N.; Campos, J.C.; Duarte, L.; Sillero N.(2023).Using ENMs, GEE, and WebGIS for biodiversity monitoring (Masters dissertation, Faculty of Sciences of the University of Porto).")
                                           ),
                                           
                                           infoBox(
                                             "", "Support", "If you have founded a problem about WebGIS, please contact immediately the authors." , icon = icon("warning"), color ="red"
                                           ),
                                           infoBox(
                                             "", "Questions", "If you have any questions about the WebGIS, send us an e-mail: montobeo.project@gmail.com" , icon = icon("question"), color ="blue"
                                           )
                                           
                                 )
                                 
                         ), #tabItem End
                         
                         tabItem("Biodata", box(dataTableOutput("DataTable"), width = 12)),
                         
                         
                         tabItem(tabName = "BioCur",
                                 box(
                           title = h2(strong("Welcome to the Biodiversity curiosities"), align = "center"),
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           solidHeader = TRUE,
                           valueBox("What?", "Here you can found species trees and other informations.", icon = icon("check"), color = "green"),
                           valueBox("Why?", "To arouse curiosity about the species recorded in MN-SPA!", icon = icon("info"), color = "blue"),
                           valueBox("Error?", "If the species trees aren't loading, please reload the page.", icon = icon("warning"), color = "red")),
                           
                                 box(echarts4rOutput("tree1")),
                                 box(echarts4rOutput("tree2")))
                          
                         
                       ) #End of tabItems
                       
                     ) #End of dashboard
                     
)  # End of dashpage (UI)



# Define server logic required to draw the map and other stuff
server <- function(input, output, session) {
  
  ############    Web GIS    ###############
  
  observeEvent(input$Taxons, {
    updateSelectInput(session, "Genuss",
                      label = "Select the genus (scientific name):",
                      choices = unique(AtlasEN$Genus[AtlasEN$Taxonomic.groups == input$Taxons])
    )
  })
  
  observeEvent(input$Genuss, {
    updateSelectInput(session, "Speciess",
                      label = "Select the species (scientific name):",
                      choices = unique(AtlasEN$Species[AtlasEN$Genus == input$Genuss])
    )
  })
  
  seleccionar_dadoss <- reactive({
    dados <- AtlasEN[AtlasEN$Species == input$Speciess, ]
    dados
  })
  
  
  #Output 'map'
  output$map <- renderLeaflet({
    leaflet(data = Grid_EN) %>%
      setView(lng= -6.904, lat= 41.8102, zoom = 10)  %>%
      addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), group = "OpenStreetMap") %>%
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$Esri, group = "Esri") %>%
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$Stamen.Terrain, group = "Stamen.Terrain") %>%
      addControl(html =  "<img src= 'https://github.com/BravoAlpha2/WebGIS/blob/main/Images/presence.png?raw=true'>  Ocorrences <br/>", position = "bottomright") %>%
      addControl(html =  "<img src= 'https://github.com/BravoAlpha2/WebGIS/blob/main/Images/Legend.png?raw=true'><br/>", position = "bottomright") 
  })
  
  
  # Put raster input reactive
  reactiveRaster_EN <- reactive({modis.rasters_EN[[input$EVs_EN]]})
  
  
  Grid_EN <- as(Grid_EN, "Spatial")
  
  observe({
    
    dadoss <- seleccionar_dadoss()
    
    leafletProxy("map", data = Grid_EN) %>%
      
      clearShapes() %>%
      
       # Add shps. 
      addPolygons(data = Grid_EN[Grid_EN$CUTM1K %in% dadoss[!is.na(dadoss$Confirmed), "CUTM1K"], ], fillColor = "darkred", fillOpacity = 0.65, stroke = FALSE,  group = "Ocorrences")  %>%
      addPolygons(data = Montesinho_EN, color = "black", weight = 5, fillColor = "transparent", group = "Montesinho/Nogueira SPA") %>%
      addPolygons(color = "darkgrey", fillColor = NULL, fillOpacity = 0, weight = 1, label = ~CUTM1K, labelOptions = labelOptions(noHide = FALSE, textOnly = TRUE, opacity = 0.8, textsize = "16px", style = list("color" = "darkgreen")), group = "Grid") %>%
      
      #Rasters Images
      addRasterImage(reactiveRaster_EN(), colors=terrain.colors(21), layerId = input$EVs_EN, opacity=input$Opacity) %>%
      
      #Control layers
      addLayersControl(baseGroups = c("OpenStreetMap", "Stamen.Terrain", "Esri"), overlayGroups = c("Montesinho/Nogueira SPA", "Grid", "Ocorrences")) %>%
      
      # Map Plugins
      addMeasure(position = "topright", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", activeColor = "#3D535D", completedColor = "#7D4479") %>%
      addMiniMap(position = "bottomleft",tiles = providers$OpenStreetMap,toggleDisplay = TRUE) %>%
      addSearchOSM()
  })
  
  ############    Biodiversity data    ###############
  
  output$DataTable <- renderDataTable(DataTable)
  
  ############    More Info.    ###############
  
  tree1 <- tibble(
    name = "Taxonomic groups",       
    children = list(
      tibble(name = c("Amphibians", "Reptiles", "Mammals", "Birds", "Flora (vascular plants)"), 
             children = list(
               tibble(name = NULL, NULL),   
               
               tibble(name = NULL, NULL),  
               
               tibble(name = c("Galemys pyrenaicus"), 
                      NULL),  
               
               tibble(name = NULL, NULL), 
               
               tibble(name = c("Eryngium viviparum", "Herniaria lusitanica"),  
                      NULL)), 
      )
    ))
  
  tree2 <- tibble(
    name = "Taxonomic groups",       
    children = list(
      tibble(name = c("Amphibians", "Reptiles", "Mammals", "Birds", "Flora (vascular plants)"), 
             children = list(
               tibble(name = c("Pelobates cultripes", "Rana iberica"), NULL),  
               
               tibble(name = c("Iberolacerta monticola", "Mauremys leprosa", "Vipera latastei"), NULL),  
               
               tibble(name = c("Arvicola sapidus", "Barbastella barbastellus"), NULL),  
               
               tibble(name = c("Clamator glandarius", "Gallinago gallinago", "Lanius meridionalis", "Neophron percnopterus", "Streptopelia turtur"), NULL), 
               
               tibble(name = c("Festuca brigantina", "Herniaria lusitanica", "Veronica micrantha"), NULL)
             ),  
      )
    ))
  
  output$tree1 <- renderEcharts4r({
    tree1 |> 
      e_charts() |> 
      e_tree(initialTreeDepth = 1, label = list(offset = c(0, -11))) |> 
      e_title(text="European IUCN status: Endangered (EN) species in MN-SPA", subtext = "(Click to see)")
  })
  
  output$tree2 <- renderEcharts4r({
    tree2 |> 
      e_charts() |> 
      e_tree(initialTreeDepth = 1, label = list(offset = c(0, -11))) |> 
      e_title(text="European IUCN status: Vulnerable (VU) species in MN-SPA", subtext = "(Click to see)")
  })

  
} #End of server

# Run the application 
shinyApp(ui = ui, server = server)


