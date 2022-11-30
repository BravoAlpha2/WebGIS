# ATLAS of Montesinho/Nogueira special protection area (MN-SPA)
# by Nuno Garcia (https://www.linkedin.com/in/nuno-garcia-97b780158/)
# Created for MontObEO project


#Shiny library and dashboard
library(shiny)
library(shinydashboard)


# Library for WebSIG / WebGIS
library(terra)
library(raster)
library(leaflet)
library(leaflet.extras)


## Library for spp. tree
library(tibble)


## Library for globe animation (and assets)
#install.packages("remotes")
#remotes::install_github("JohnCoene/echarts4r")
#remotes::install_github("JohnCoene/echarts4r.maps")
#remotes::install_github("JohnCoene/echarts4r.assets")
library(remotes)
library(echarts4r)
library(echarts4r.maps)
library(echarts4r.assets)


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
                         menuItem("WebSIG (PT)", tabName = "websig", badgeLabel = icon("map"), badgeColor = "maroon"),
                         menuItem("WebGIS (EN)", tabName = "webgis", badgeLabel = icon("map"), badgeColor = "maroon"),
                         menuItem("Biodiversity data", tabName = "Biodata", badgeLabel = icon("tree"), badgeColor = "green"),
                         menuItem("Biodiversity curiosities", tabName = "BioCur", badgeLabel = icon("random"), badgeColor = "teal"),
                         menuItem("More Information of MN-SPA", tabName = "plots", badgeLabel = icon("cloud"), badgeColor = "blue"),
                         HTML(paste0(
                           "<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>",
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
                         
                         tabItem("websig",
                                 
                                 fluidPage(box(width = NULL,
                                               leafletOutput(outputId = "mapa", height = 500)),
                                           
                                           box(br(),
                                               
                                               sidebarPanel(
                                                 selectInput(inputId = "Taxon", label = "Selecione o grupo taxonómico:", choices = c("All", as.character(sort(unique(Atlas$Taxonomic.groups))))),
                                                 selectInput(inputId = "Genus", label = "Selecione o género (nome cientifico):", choices = "", selectize = FALSE),
                                                 selectInput(inputId = "Species", label = "Selecione a espécie (nome cientifico):", choices = "", selectize = FALSE), width = 6, height = 10 
                                               ),
                                               
                                               sidebarPanel(
                                                 selectInput("EVs", "Variáveis ambientais (VAs):",h3("Checkbox group"),choices=names(modis.rasters)),
                                                 sliderInput("Opacidade", "Opacidade dos VAs:", min = 0, max = 1, value = 0), width = 6
                                               ),
                                           ),
                                           
                                           tabBox(
                                             
                                             tabPanel("Descrição",
                                                      "Sistema de Informação Geográfico Web (WebSIG), desenhado para a área de proteção especial de Montesinho e Nogueira (MN-SPA) que integra a rede Natura 2000 (https://natura2000.eea.europa.eu/Natura2000/SDF.aspx?site=PTCON0002).
                                                      Os dados de biodiversidade estão representados a uma resolução espacial de 1km e foram utilizados em Modelos de Nicho Ecológico (ENMs) no projeto MontObEO."),
                                             
                                             tabPanel("Espécies","Os dados de biodiversidade foram compilados para o período 2000-2021 utilizando várias fontes, como bases de dados online  
                                                    (p.e., Global Biodiversity Information Facility (GBIF; https://www.gbif.org/)) atlas de distribuição de espécies, inventários e dados recolhidos no terreno.  
                                                    Os dados foram filtrados e corrigidos para cinco grandes grupos taxonómicos: flora (plantas vasculares), anfíbios, répteis, aves e mamíferos.
                                                    Para mais informações, consultar no menu: 'Biodiversity data'."),
                                             
                                             tabPanel("Variáveis ambientais","Estes Variáveis Ambientais (VAs) foram compilados a partir do Google Earth Engine (GEE; https://earthengine.google.com/), com uma projeção pseudo-mercartor (EPSG:3857). Os VAs selecionados representam os valores médios para o periodo de 2001-2021.
                                               Os VAs compilados foram os seguintes: 'Enhance Vegetation Index' (EVI); 'Day Land Surface Temperature' (LSTDay) (°C); 'Night Land Surface Temperature' (LSTNight) (°C);
                                              'Surface Refletance (Band 1: 620-670nm)' (SR_B1); 'Time Since Fire' (TSF) (Anos); 'Area Annualy Burned (presence/absence)' (AAB).
                                               P.S.: Area Annualy Burned (presence/absence) não está presente na seleção dos VAs."),
                                             
                                             tabPanel("Autores e Citação", "O criador principal deste WebSIG foi Nuno Garcia, estudante pós-graduado do projecto MontObEO.
                                                 Se pretende citar este WebSIG, por favor, utilize a seguinte citação: Garcia, N.; Campos, J.C.; Duarte, L.; Sillero N.(2023).Using ENMs, GEE, and Web SIG for biodiversity monitoring (Masters dissertation, Faculty of Sciences of the University of Porto).")
                                           ),
                                           
                                           infoBox(
                                             "", "Suporte", "Se detetou um problema no funcionamento do WebGIS, contacte imediatamente os autores." , icon = icon("warning"), color ="red"
                                           ),
                                           infoBox(
                                             "", "Questões", "Se tiver alguma dúvida sobre o WebGIS, envie-nos um e-mail: montobeo.project@gmail.com" , icon = icon("question"), color ="blue"
                                           )
                                           
                                 )
                                 
                         ), #tabItem End
                         
                         
                         tabItem("webgis",
                                 
                                 fluidPage(box(width = NULL,
                                               leafletOutput(outputId = "map", height = 500)),
                                           
                                           box(br(),
                                               
                                               sidebarPanel(
                                                 selectInput(inputId = "Taxons", label = "Select your Taxonomic Group:", choices = c("All", as.character(sort(unique(AtlasEN$Taxonomic.groups))))),
                                                 selectInput(inputId = "Genuss", label = "Select your Genus (scientific name):", choices = "", selectize = FALSE),
                                                 selectInput(inputId = "Speciess", label = "Select your Species (scientific name):", choices = "", selectize = FALSE), width = 6
                                               ),
                                               sidebarPanel(
                                                 selectInput("EVs_EN", "Enviromental Variables (EVs):",h3("Checkbox group"), choices=names(modis.rasters_EN)),
                                                 sliderInput("Opacity", "Opacity of EVs:", min = 0, max = 1, value = 0), width = 6
                                               )
                                           ),
                                           
                                           tabBox(
                                             
                                             tabPanel("Description",
                                                      "MontObEO Web Geographic Information System (WebGIS), designed for Montesinho and Nogueira special protection area (MN-SPA) which integrates que integra Natura 2000 network (https://natura2000.eea.europa.eu/Natura2000/SDF.aspx?site=PTCON0002).
                                                      Biodiversity data is represent on 1km spatial resolution and 
                                                      Enviromental Variables presented in overlayer group were used in Ecological Niche Models (ENMs) on MontObEO project."),
                                             
                                             tabPanel("Species","Biodiversity data was compiled for the period 2000-2021 using several data sources such as online databases 
                                                    (e.g., Global Biodiversity Information Facility (GBIF; https://www.gbif.org/)) distribution atlases, inventory datasets and field-collected data.  
                                                    The data was filtered and corrected for five major taxonomic groups: flora (vascular plants), amphibians, reptiles, birds and mammals. 
                                                    For more informations, please consults in menu: 'Biodiversity data'."),
                                             
                                             tabPanel("Enviromental Variables","These Enviromental Variables (EPs) were collected trought Google Earth Engine (GEE; https://earthengine.google.com/) and represent the mean values for the 2001-2021 period. 
                                               The EPs compiled were: 'Enhance Vegetation Index' (EVI); 'Day Land Surface Temperature' (LSTDay) (°C); 'Night Land Surface Temperature' (LSTNight) (°C);
                                              'Surface Refletance (Band 1: 620-670nm)' (SR_B1); 'Time Since Fire' (TSF) (Years); 'Area Annualy Burned (presence/absence)' (AAB).
                                               P.S.: Area Annualy Burned (presence/absence) is not present in the selection of EPs."),
                                             
                                             tabPanel("Authors and Citation", "The main creator of this WebSIG was Nuno Garcia, post-graduate student in the MontObEO project.
                                                 If you would like to quote this WebSIG, please use the following quotation: Garcia, N.; Campos, J.C.; Duarte, L.; Sillero N.(2023).Using ENMs, GEE, and Web SIG for biodiversity monitoring (Masters dissertation, Faculty of Sciences of the University of Porto).")
                                           ),
                                           
                                           infoBox(
                                             "", "Support", "If you have founded a problem about WebGIS, please contact immediately the authors." , icon = icon("warning"), color ="red"
                                           ),
                                           infoBox(
                                             "", "Questions", "If you have any questions about MontObEO app, send us an e-mail: montobeo.project@gmail.com" , icon = icon("question"), color ="blue"
                                           )
                                           
                                 )
                                 
                         ), #tabItem End
                         
                         tabItem("Biodata", box(dataTableOutput("DataTable"), width = 100)),
                         
                         tabItem(tabName = "BioCur",
                                 box(echarts4rOutput("tree1")),
                                 box(echarts4rOutput("tree2"))),
                         
                         tabItem(tabName = "plots",
                                 
                                 box(echarts4rOutput("plot1", height = 343), br(), h4('Fig. 1: Location of MN-SPA around the globe (blue spot marks the location).'),),
                                 
                                 tabBox(title = tagList(shiny::icon("globe"),"MN-SPA"), height = 425, 
                                        
                                        tabPanel("Descrição (PT)","A área de proteção especial de Montesinho/Nogueira (MN-SPA) está integrada como um dos sítios natura 2000 da União Europeia (https://natura2000.eea.europa.eu/Natura2000/SDF.aspx?site=PTCON0002).         
                                                  Localizado no extremo nordeste de Portugal continental (Fig. 1), o MN-SPA acolhe uma elevada diversidade de habitats e espécies (consulte 'Biodiversity data'). É composta por duas regiões principais: Parque Natural de Montesinho (MNP) e Serra da Nogueira (NM). 
                                                  O MNP, uma das maiores áreas protegidas em Portugal, tem mais de 74 mil hectares, predominantemente constituídos por uma paisagem agrícola de montanha tradicional. 
                                                  Por outro lado, o NM detém principalmente florestas de carvalhos homogéneas e compactas e não foi profundamente alterada pela gestão humana, principalmente devido ao contraste da topografia.
                                                  O MN-SPA caracteriza-se por um bioclima supra e oro-mediterrânico com elevado gradiente de temperatura, de -12°C a 40°C. 
                                                  A elevação varia mais de 1000 m, desde o ponto mais baixo da fronteira ocidental, rio Mente (380m), até ao mais alto, perto do rio Sabor, a 1472 m.
                                                  A topografia, o clima e o ambiente da região promovem valores de conservação, particularmente significativos para espécies como o lobo ibérico (Canis lupus) e o veado vermelho (Capreolus capreolus)."),
                                        
                                        tabPanel("Description (EN)", "Montesinho/Nogueira special protection area (MN-SPA) is integrated as one of European Union’s Natura 2000 sites (https://natura2000.eea.europa.eu/Natura2000/SDF.aspx?site=PTCON0002). 
                                                  Located in the far northeast of continental Portugal (Fig. 1), MN-SPA hosts a high diversity of habitats and species (see 'Biodiversity data'). It is composed by two main regions: Montesinho Natural Park (MNP) and Nogueira Mountains (NM). 
                                                  The MNP, one of the largest protected areas in Portugal, has more than 74 thousand hectares, predominantly consisting with a traditional mountain agricultural landscape. 
                                                  On the other hand, the NM mainly holds homogeneous and compact oak forests and has not been deeply altered by human management, mainly due to the contrast of topography. 
                                                  The MN-SPA is characterized by supra and oro-mediterranean bioclimate with high gradient temperature, from -12°C to 40°C. 
                                                  The elevation ranges more than 1000 m, from the lowest point in the western border, Mente river (380m), to the highest, near Sabor river, at 1472 m. 
                                                  The topography, climate and environment of the region promotes conservation values, particularly significant for species such as the Iberian wolf (Canis lupus) and the red deer (Capreolus capreolus).")
                                 ),
                                 
                         ) #End of tabItems
                         
                       ) #End of tabItems
                       
                     ) #End of dashboard
                     
)  # End of dashpage (UI)



# Define server logic required to draw the map and other stuff
server <- function(input, output, session) {
  
  ############    Web SIG    ###############
  
  # Put data in reactive mode
  select_genus <- reactive({
    tax <- input$Taxon
    if (tax == "All")  genus <- sort(unique(Atlas$Genus))
    else  genus <- sort(unique(Atlas[Atlas$Taxon == tax, "Genus"]))
    genus
  })
  
  observe({
    updateSelectInput(session, "Genus", choices = select_genus()
    )})
  
  select_species <- reactive({
    Genus <- input$Genus
    if (Genus == "ALL")  species <- sort(unique(Atlas$Species))
    else  species <- sort(unique(Atlas[Atlas$Genus == Genus, "Species"]))
    species
  })
  
  observe({
    updateSelectInput(session, "Species", choices = select_species()
    )})
  
  
  seleccionar_dados <- reactive({
    dados <- Atlas[Atlas$Species == input$Species, ]
    dados
  })
  
  
  # Output 'mapa'
  output$mapa <- renderLeaflet({
    leaflet(data = Grid) %>%
      setView(lng= -6.904, lat= 41.8102, zoom = 10)  %>%
      addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), group = "OpenStreetMap") %>%
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$Esri, group = "Esri") %>%
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$Stamen.Terrain, group = "Stamen.Terrain") %>%
      addControl(html =  "<img src= 'https://github.com/BravoAlpha2/WebGIS/blob/main/Images/presence.png?raw=true'>  Ocorrências <br/>", position = "bottomright") %>%
      addControl(html =  "<img src= 'https://github.com/BravoAlpha2/WebGIS/blob/main/Images/Legenda.png?raw=true'><br/>", position = "bottomright") 
  })
  
  
  # Put rasters in reactive mode
  reactiveRaster <- reactive({modis.rasters[[input$EVs]]})
  
  
  observe({
    
    # Put data in object format
    dados <- seleccionar_dados()
    
    # Put Grid in spatial format 
    Grid <- as(Grid, "Spatial")
    
    leafletProxy("mapa", data = Grid) %>%
      
      clearShapes() %>%
      
      # Add shps.
      addPolygons(data = Grid[Grid$CUTM1K %in% dados[!is.na(dados$Confirmed) & dados$Confirmed == 1, "CUTM1K"], ], fillColor = "darkred", fillOpacity = 0.65, stroke = FALSE,  group = "Presença da espécie")  %>%
      addPolygons(data = Montesinho, color = "black", weight = 5, fillColor = "transparent", group = "Montesinho/Nogueira SPA") %>%
      addPolygons(color = "darkgrey", fillColor = NULL, fillOpacity = 0, weight = 1, label = ~CUTM1K, labelOptions = labelOptions(noHide = FALSE, textOnly = TRUE, opacity = 0.8, textsize = "16px", style = list("color" = "darkgreen")), group = "Grid") %>%
      
      #Rasters Images
      addRasterImage(reactiveRaster(), colors=rev(terrain.colors(21)), layerId = input$EVs, opacity=input$Opacidade) %>%
      #addLegend(layerId = input$EVs, pal = terrain.colors(21), values =  values(character(na.omit(reactiveRaster()))), title = "Legend", position = "bottomright") %>%
      
      #Control layers
      addLayersControl(baseGroups = c("OpenStreetMap", "Stamen.Terrain", "Esri"), overlayGroups = c("Montesinho/Nogueira SPA", "Grid", "Ocorrência da espécie")) %>%
      
      # Map Plugins
      addMeasure(position = "topright", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", activeColor = "#3D535D", completedColor = "#7D4479") %>%
      addMiniMap(position = "bottomleft",tiles = providers$OpenStreetMap,toggleDisplay = TRUE) %>% 
      addSearchOSM() 
  })
  
  
  ############    Web GIS    ###############
  
  # Put data in reactive mode
  select_genuss <- reactive({
    taxs <- input$Taxons
    if (taxs == "All")  genuss <- sort(unique(AtlasEN$Genus))
    else  genuss <- sort(unique(AtlasEN[AtlasEN$Taxon == taxs, "Genus"]))
    genuss
  })
  
  observe({
    updateSelectInput(session, "Genuss", choices = select_genuss()
    )})
  
  select_speciess <- reactive({
    Genusss <- input$Genuss
    if (Genusss == "ALL")  species <- sort(unique(AtlasEN$Species))
    else  speciess <- sort(unique(AtlasEN[AtlasEN$Genus == Genusss, "Species"]))
    speciess
  })
  
  observe({
    updateSelectInput(session, "Speciess", choices = select_speciess()
    )})
  
  
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
  
  
  observe({
    
    dadoss <- seleccionar_dadoss()
    
    Grid_EN <- as(Grid_EN, "Spatial")
    
    leafletProxy("map", data = Grid_EN) %>%
      
      clearShapes() %>%
      
      # Add shps.
      addPolygons(data = Grid_EN[Grid_EN$CUTM1K %in% dadoss[!is.na(dadoss$Confirmed) & dadoss$Confirmed == 1, "CUTM1K"], ], fillColor = "darkred", fillOpacity = 0.65, stroke = FALSE,  group = "Recent")  %>%
      addPolygons(data = Montesinho_EN, color = "black", weight = 5, fillColor = "transparent", group = "Montesinho/Nogueira SPA") %>%
      addPolygons(color = "darkgrey", fillColor = NULL, fillOpacity = 0, weight = 1, label = ~CUTM1K, labelOptions = labelOptions(noHide = FALSE, textOnly = TRUE, opacity = 0.8, textsize = "16px", style = list("color" = "darkgreen")), group = "Grid") %>%
      
      #Rasters Images
      addRasterImage(reactiveRaster_EN(), colors=terrain.colors(21), layerId = input$EVs_EN, opacity=input$Opacity) %>%
      
      #Control layers
      addLayersControl(baseGroups = c("OpenStreetMap", "Stamen.Terrain", "Esri"), overlayGroups = c("Montesinho/Nogueira SPA", "Grid", "Recent")) %>%
      
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
  
  output$plot1 <- renderEcharts4r({
    AtlasEN |> 
      e_charts() |> 
      e_globe(
        environment = ea_asset("starfield"),
        base_texture = ea_asset("world topo"), 
        height_texture = ea_asset("world topo"),
        globeOuterRadius = 100
      ) |> 
      e_scatter_3d(
        Longitude, 
        Latitude, 
        coord_system = "globe"
      ) 
  })
  
} #End of server

# Run the application 
shinyApp(ui = ui, server = server)