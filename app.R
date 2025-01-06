# Atlas - WebGIS app - of the Special Area of Conservation Montesinho/Nogueira (SAC-MN).
# Developed for the MontObEO project (https://montobeop.wordpress.com/)

# Author: Nuno Garcia
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
# ResearchGate: https://www.researchgate.net/profile/Nuno-Garcia-4



#Shiny library and dashboard
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(httr)
library(jsonlite)  

# Library for WebGIS
library(terra)
library(raster)
library(leaflet)
library(leaflet.extras)
library(rgeos)  

## Library for spp. tree
library(tibble)
library(echarts4r)
library(dplyr)
library(purrr)

# Load objects:
source('Objects.R')

# Set the secure token
secure_token <- "montesinho"

# Load the expected structure for validation
expected_structure <- readRDS("Dados/AtlasEN_optimized.rds")  # Replace with the correct path if needed

# Define predefined habitat information
habitat_info <- data.frame(
  HabitatCode = c("9340pt1", "9230pt2", "4030+9230pt2", "4030+9340pt1", "9230pt2+4030"),
  HabitatName = c(
    "Mixed oak woodlands", 
    "Galicio-Portuguese oak woods", 
    "Dry heathlands and oak woods", 
    "Heathlands with oak woodlands", 
    "Mixed oak and heathland"
  ),
  Description = c(
    "Mixed oak woodlands are home to a variety of flora and fauna and are important for ecosystem diversity.",
    "Galicio-Portuguese oak woods are unique ecosystems in the Iberian Peninsula, supporting diverse wildlife.",
    "Dry heathlands and oak woods are characterized by low rainfall and are adapted to dry conditions.",
    "Heathlands with oak woodlands provide mixed habitats supporting species from both ecosystems.",
    "Mixed oak and heathland habitats offer a blend of woodland and heathland flora and fauna."
  ),
  ImageURL = c(
    "https://cdn.pixabay.com/photo/2018/10/19/08/30/oak-3759273_1280.jpg",  # Replace with relevant image URLs
    "https://cdn.pixabay.com/photo/2017/07/18/17/34/oak-2514436_1280.jpg",
    "https://cdn.pixabay.com/photo/2016/12/18/12/37/heather-1911314_1280.jpg",
    "https://cdn.pixabay.com/photo/2017/08/09/20/46/heathland-2612716_1280.jpg",
    "https://cdn.pixabay.com/photo/2020/10/10/17/33/mixed-woodland-5642137_1280.jpg"
  ),
  stringsAsFactors = FALSE
)


# Define UI for application
ui <- dashboardPage( skin = "purple",
                     
                     # Dashboard title
                     dashboardHeader(title="MontObEO WebGIS",titleWidth = 250),
                     
                     
                     # Dashboard sidebar
                     dashboardSidebar(
                       width = 250,
                       sidebarMenu(
                         menuItem("Welcome", tabName = "Welcome", badgeLabel = icon("info-circle"), badgeColor = "purple"),
                         menuItem("WebGIS: Species", tabName = "webgis", badgeLabel = icon("map"), badgeColor = "olive"),
                         menuItem("WebGIS: Priority habitats", tabName = "webgis_habitats", badgeLabel = icon("map"), badgeColor = "maroon"),
                         menuItem("Biodiversity curiosities", tabName = "BioCur", badgeLabel = icon("tree"), badgeColor = "teal"),
                         menuItem("Habitats curiosities", tabName = "HabitatsCur", badgeLabel = icon("random"), badgeColor = "green"),
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
                         tabItem(
                           tabName = "Welcome",
                           fluidRow(
                             box(
                               width = 12,
                               title = "Projeto MontObEO",
                               status = "primary",
                               solidHeader = TRUE,
                               tabsetPanel(
                                 tabPanel(
                                   "English",
                                   h4("MontObEO: Biodiversity Observatory of Montesinho - An earth observation tool for biodiversity conservation"),
                                   p("Welcome to the MontObEO Shiny app, a Web Geographic Information System (WebGIS) designed for the special area of conservation Montesinho/Nogueira (SAC-MN). This WebGIS serves as a platform for exploring biodiversity data and understanding the distribution of species across this protected area."),
                                   p("With access to over 1,312 individual species distributions, the MontObEO WebGIS provides a user-friendly interface that allows quick insights into species distributions and showcases priority habitats that the ICNF (Portuguese Institute for Nature Conservation and Forests) aims to protect and enhance within the SAC-MN."),
                                   p("MontObEO is a 3-year research initiative focused on developing an environmental alert system through the integration of remote sensing time series and ecological niche models. This innovative approach helps identify changes in habitat quality and assess the extinction risks of species over time and space."),
                                   p("Funded by the Portuguese Foundation for Science and Technology (FCT: MTS/BRB/0091/2020), the MontObEO project is supported by a multidisciplinary team from the Faculty of Sciences of the University of Porto, the University of Córdoba, and the ForestWISE laboratory."),
                                   p("Learn more about the project by visiting the official website:"),
                                   tags$a(href = "https://montobeo.wordpress.com/", "https://montobeo.wordpress.com/", target = "_blank"),
                                   
                                   
                                   tags$hr(),
                                   
                                   # Additional information about the creator and citation
                                   p("The main creator of this WebGIS is Nuno Garcia, post-graduate student in the MontObEO project."),
                                   p("To cite this Shiny app - MontObEO WebGIS, use: 'Garcia, N.; Campos, J.C.; Duarte, L.; Sillero N. (2023).", 
                                     tags$em("Using GEE, ecological niche models, and WebGIS for biodiversity monitoring"),
                                     "(Masters dissertation, Faculty of Sciences of the University of Porto).'")
                                 ),
                                 tabPanel(
                                   "Português",
                                   h4("Observatório de Biodiversidade de Montesinho: Uma ferramenta de observação da terra para a conservação da biodiversidade"),
                                   p("Bem-vindo ao aplicativo Shiny do MontObEO, um Sistema de Informação Geográfica Web (WebSIG) desenvolvido especialmente para a área de conservação Montesinho/Nogueira (SAC-MN). Este WebGIS é uma ferramenta que permite explorar a biodiversidade e compreender a distribuição de espécies dentro desta área protegida."),
                                   p("Com acesso a mais de 1.312 distribuições de espécies individuais, o MontObEO WebSIG oferece uma experiência intuitiva e amigável, permitindo a visualização rápida das distribuições de espécies e dos habitats prioritários que o ICNF (Instituto da Conservação da Natureza e das Florestas) visa preservar e melhorar dentro do SAC-MN."),
                                   p("O projeto MontObEO é um esforço de investigação de três anos, focado em criar um sistema de alerta ambiental utilizando séries temporais de deteção remota e modelos de nicho ecológico. Esta abordagem inovadora identifica mudanças na qualidade do habitat e riscos de extinção de espécies, acompanhando essas dinâmicas ao longo do tempo e do espaço."),
                                   p("Financiado pela Fundação para a Ciência e a Tecnologia (FCT: MTS/BRB/0091/2020), o MontObEO conta com a colaboração de uma equipa multidisciplinar da Faculdade de Ciências da Universidade do Porto, da Universidade de Córdoba e do laboratório ForestWISE."),
                                   p("Saiba mais sobre o projeto visitando o site oficial:"),
                                   tags$a(href = "https://montobeo.wordpress.com/", "https://montobeo.wordpress.com/", target = "_blank"),
                                   
                                   tags$hr(),
                                   
                                   # Informação adicional sobre o criador e citação
                                   p("O principal criador deste WebGIS é Nuno Garcia, investigador júnior no projeto MontObEO."),
                                   p("Para citar esta Shiny app - MontObEO WebSIG, utilize: 'Garcia, N.; Campos, J.C.; Duarte, L.; Sillero, N. (2023).", 
                                     tags$em("Using GEE, ecological niche models, and WebGIS for biodiversity monitoring"),
                                     "(Dissertação de Mestrado, Faculdade de Ciências da Universidade do Porto).'")
                                 )
                               )
                             )
                           )
                         ),
                         
                         tabItem(
                           "webgis",
                           fluidPage(
                             # Main Layout with Sidebar and Map
                             fluidRow(
                               # Main Map Box
                               box(
                                 leafletOutput(outputId = "map", height = 765),
                                 width = 8,
                                 height = 950,
                                 tags$hr(),
                                 # hr(),
                                 tabBox(
                                   width = 12,
                                   title = tagList(icon("info-circle"), 
                                                   # "Additional Information"
                                                   ),
                                   
                                   tabPanel(
                                     "Description",
                                     p("MontObEO WebGIS for the Montesinho/Nogueira SAC (SAC-MN) within Natura 2000 (https://natura2000.eea.europa.eu/Natura2000/SDF.aspx?site=PTCON0002) displays biodiversity data at 1 km resolution. 
                                       Environmental variables support ecological niche models (ENMs) for the MontObEO project.")),
                                   tabPanel(
                                     "Species Dataset",
                                     p("Biodiversity data (2000-2021) from GBIF, atlases, inventories, and field data was refined to include flora, amphibians, reptiles, birds, and mammals. See the paper: ", 
                                       tags$em("'Biodiversity Dataset and Atlas of the SAC Montesinho/Nogueira, Portugal'"), 
                                       " or visit: https://zenodo.org/doi/10.5281/zenodo.7657330.")),
                                   tabPanel(
                                     "Environmental Variables",
                                     p("Environmental Variables (EVs) from Google Earth Engine (GEE) represent mean values from 2001-2021, including: 
                                       Enhanced Vegetation Index (EVI), Day/Night Land Surface Temperature (LST), Surface Reflectance (SR_B1), 
                                       Time Since Fire (TSF), and Area Annually Burned (AAB).")),
                                   tabPanel(
                                     "Authors and Citation",
                                     p("The main creator of this WebGIS is Nuno Garcia, a post-graduate student in the MontObEO project.
           To cite this WebGIS, use: Garcia, N.; Campos, J.C.; Duarte, L.; Sillero N.(2023). Using GEE, ecological niche models, and WebGIS for biodiversity monitoring (Masters dissertation, Faculty of Sciences of the University of Porto).")
                                   )
                                 )
                               ),
                               
                               # Right Sidebar for Species and Environmental Selection
                               column(
                                 width = 4,
                                 
                                 # Species Selection Box
                                 box(
                                   title = tagList(icon("paw"), "Species selection"),
                                   solidHeader = TRUE,
                                   status = "primary",
                                   width = 12,
                                   collapsible = TRUE,
                                   collapsed = FALSE,
                                   style = "background-color: #f9f9f9; padding: 15px;",
                                   
                                   # Selection Inputs
                                   selectInput(
                                     inputId = "Taxons",
                                     label = "Taxonomic Group:",
                                     choices = c("Select group", as.character(sort(unique(AtlasEN$Taxonomic.groups)))),
                                     selected = NULL
                                   ),
                                   uiOutput("Order_menu"),
                                   uiOutput("Family_menu"),
                                   uiOutput("genus_menu"),
                                   uiOutput("species_menu"),
                                   
                                   # Additional Options
                                   checkboxInput(
                                     inputId = "showGrid",
                                     label = "Show 1x1 km grid across the park",
                                     value = FALSE
                                   ),
                                   tags$hr(),
                                   
                                   # Species Information Display
                                   uiOutput("species_image"),
                                   br(),
                                   uiOutput("species_status"),
                                   uiOutput("species_info")
                                 ),
                                 
                                 # Data Download Box
                                 box(
                                   title = tagList(icon("download"), "Data upload and download"),
                                   solidHeader = TRUE,
                                   status = "primary",
                                   width = 12,
                                   collapsible = TRUE,
                                   collapsed = TRUE,
                                   style = "background-color: #f9f9f9; padding: 15px;",
                                   
                                   # File Upload input
                                   p(strong("Choose format to download data:")),
                                   downloadButton("download_csv", "CSV"),
                                   downloadButton("download_rds", "RDS"),
                                   downloadButton("download_json", "JSON"),
                                   
                                   tags$hr(),
                                   
                                   # Token input
                                   passwordInput("upload_token", "Enter security password to upload files:", value = ""),
                                   
                                   # Conditional panel that displays only if the token is correct
                                   conditionalPanel(
                                     condition = sprintf("input.upload_token == '%s'", secure_token),
                                     
                                     
                                     
                                     # File upload input
                                     fileInput("file_upload", "Choose a .csv or .rds file", accept = c(".rds", ".csv")),
                                     actionButton("upload_btn", "Upload Data"),
                                     
                                     # Additional instructions
                                     tags$p(
                                       style = "color: #555; margin-top: 5px;",
                                       "Note: Please ensure that the uploaded RDS file has the same format and column structure as the existing dataset to avoid errors during data processing.
                 Uploading and saving data may take several seconds. A confirmation message will appear once the process is complete."
                                     )
                                   ) 
                                 ),
                                 
                                 # Environmental Variables Box
                                 box(
                                   title = tagList(icon("globe"), "Environmental Variables"),
                                   solidHeader = TRUE,
                                   status = "primary",
                                   width = 12,
                                   collapsible = TRUE,
                                   collapsed = TRUE,
                                   style = "background-color: #f9f9f9; padding: 15px;",
                                   
                                   selectInput(
                                     inputId = "EVs_EN",
                                     label = "Select Environmental Variable:",
                                     choices = names(modis.rasters_EN)
                                   ),
                                   sliderInput(
                                     inputId = "Opacity",
                                     label = "Opacity:",
                                     min = 0,
                                     max = 1,
                                     value = 0.0
                                   )
                                 )
                               )
                             ),

                             # Info Boxes
                             fluidRow(
                               infoBox(
                                 title = "Technical Support", 
                                 tags$span("If you experience any issues with the WebGIS platform, please contact the development team for assistance.", 
                                           style = "font-weight: normal; font-size: 14px;"), 
                                 icon = icon("tools"), 
                                 color = "red",
                                 width = 4
                               ),
                               infoBox(
                                 title = "General Inquiries", 
                                 tags$span("For any questions regarding the use or functionality of the WebGIS, please reach out via email: montobeo.project@gmail.com", 
                                           style = "font-weight: normal; font-size: 14px;"), 
                                 icon = icon("info-circle"), 
                                 color = "blue",
                                 width = 4
                               ),
                               infoBox(
                                 title = "Feedback and Suggestions", 
                                 tags$span("We welcome feedback to improve the WebGIS; please share at montobeo.project@gmail.com", 
                                           style = "font-weight: normal; font-size: 14px;"), 
                                 icon = icon("comments"), 
                                 color = "green",
                                 width = 4
                               )
                             )
                           )
                         ),
                         
                         
                         tabItem("webgis_habitats",
                                 fluidPage(
                                   # Map display box
                                   box(
                                     leafletOutput(outputId = "habitatsMap", height = 685),
                                     width = 12
                                   ),
                                   
                                   # Information tabs for Description and Citation
                                   tabBox(
                                     width = 12,
                                     tabPanel(
                                       "Description",
                                       "The MontObEO Web Geographic Information System (WebGIS) is specifically designed for the Special Area of Conservation Montesinho/Nogueira (SAC-MN), which is part of the Natura 2000 network (https://natura2000.eea.europa.eu/Natura2000/SDF.aspx?site=PTCON0002).
                                        It also focuses on priority habitats identified by the Portuguese Institute for Nature Conservation and Forests (ICNF) as critically important within the MontObEO project, 
                                        offering essential data and tools for their management and preservation."
                                     ),
                                     tabPanel(
                                       "Authors and Citation", 
                                       "The main creator of this WebGIS is Nuno Garcia, a post-graduate student involved in the MontObEO project.
                                        If you would like to cite this WebGIS, please use the following citation: Garcia, N.; Campos, J.C.; Duarte, L.; Sillero, N. (2023).", 
                                       tags$em("Using Google Earth Engine, ecological niche models, and web geographic information system for biodiversity monitoring"), "Masters dissertation, Faculty of Sciences of the University of Porto)."
                                     )
                                   ),
                                     
                                   infoBox(
                                     title = "Technical Support", 
                                     tags$span("If you experience any issues with the WebGIS platform, please contact the development team for assistance.", 
                                               style = "font-weight: normal; font-size: 14px;"), 
                                     icon = icon("tools"), 
                                     color = "red",
                                     width = 4
                                   ),
                                   
                                   infoBox(
                                     title = "General Inquiries", 
                                     tags$span("For any questions regarding the use or functionality of the WebGIS, please reach out via email: montobeo.project@gmail.com", 
                                               style = "font-weight: normal; font-size: 14px;"), 
                                     icon = icon("info-circle"), 
                                     color = "blue",
                                     width = 4
                                   ),
                                   
                                   infoBox(
                                     title = "Feedback and Suggestions", 
                                     tags$span("We welcome feedback to improve the WebGIS; please share at montobeo.project@gmail.com", 
                                               style = "font-weight: normal; font-size: 14px;"), 
                                     icon = icon("comments"), 
                                     color = "green",
                                     width = 4
                                   )
                                   
                                 )
                         ),
                         
                         tabItem(tabName = "BioCur",
                                   box(echarts4rOutput("species_tree")),
                                 box(echarts4rOutput("species_tree_2")),
                                box(echarts4rOutput("pie")),
                              box(echarts4rOutput("treemap"))
                           ),
                         
                         # Habitats curiosities tab
                         tabItem(
                           tabName = "HabitatsCur",
                           fluidRow(
                             box(
                               title = "Select a Habitat",
                               selectInput("selected_habitat", "Habitat:", choices = habitat_info$HabitatCode, selected = NULL),
                               width = 12
                             ),
                             box(
                               title = "Habitat Image",
                               uiOutput("habitat_image"),
                               width = 6
                             ),
                             box(
                               title = "Habitat Description",
                               uiOutput("habitat_description"),
                               width = 6
                             )
                           )
                         )
                         
                       ) #End of tabItems
                       
                     ) #End of dashboard
                     
)  # End of dashpage (UI)



# Define server logic required to draw the map and other stuff
server <- function(input, output, session) {
  
  ############    Web GIS    ###############
  
  output$Order_menu <- renderUI({
    class_selected <- input$Taxons
    order_choices <- sort(unique(AtlasEN$Order[AtlasEN$Taxonomic.groups == class_selected]))
    
    selectInput(
      inputId = "Order",
      label = "Select the order (scientific name):",
      choices = c("Order", order_choices),
      selected = NULL
    )
  })
  
  output$Family_menu <- renderUI({
    Order_selected <- input$Order
    Family_choices <- sort(unique(AtlasEN$Family[AtlasEN$Order == Order_selected]))
    
    selectInput(
      inputId = "Family",
      label = "Select the family (scientific name):",
      choices = c("Family", Family_choices),
      selected = NULL
    )
  })
  
  output$genus_menu <- renderUI({
    class_selected <- input$Family
    genus_choices <- sort(unique(AtlasEN$Genus[AtlasEN$Family == class_selected]))
    
    selectInput(
      inputId = "Genus",
      label = "Select the genus (scientific name):",
      choices = c("Genus", genus_choices),
      selected = NULL
    )
  })
  
  output$species_menu <- renderUI({
    genus_selected <- input$Genus
    species_choices <- sort(unique(AtlasEN$Species[AtlasEN$Genus == genus_selected]))
    
    selectInput(
      inputId = "Species",
      label = "Select the species (scientific name):",
      choices = c("Species", species_choices),
      selected = NULL
    )
  })
  
  
  
  observeEvent(input$Species, {
    req(input$Species)  # Ensure a species is selected
    species_name <- input$Species
    
    # Initialize default values for statuses
    european_status <- "Status not available"
    regional_status <- "Status not available"
    species_info <- "For more information about the species, please visit"
    
    # Search the Dataset for conservation status
    dataset_status <- Dataset %>% 
      filter(Species == species_name) %>% 
      select(European..EU..status, Regional..Portugal..Status) %>%
      distinct()
    
    # Ensure dataset_status has values and retrieve them if available
    if (nrow(dataset_status) > 0) {
      if (!is.na(dataset_status$European..EU..status[1])) {
        european_status <- as.character(dataset_status$European..EU..status[1])
      }
      if (!is.na(dataset_status$Regional..Portugal..Status[1])) {
        regional_status <- as.character(dataset_status$Regional..Portugal..Status[1])
      }
    }
    
    # Construct link to the IUCN Red List search page
    iucn_link <- paste0("https://www.iucnredlist.org/search?query=", URLencode(species_name), "&searchType=species")
    
    # Render both European and regional statuses in the UI
    output$species_status <- renderUI({
      tagList(
        tags$p(paste("European (EU) conservation status:", european_status)),
        tags$p(paste("Regional (PT) conservation status:", regional_status))
      )
    })
    
    # Render species information with link to IUCN Red List page
    output$species_info <- renderUI({
      tags$p(species_info, tags$a("IUCN Red List page.", href = iucn_link, target = "_blank"))
    })
    
    # Generate example image URL (with fallback)
    image_url <- paste0("https://en.wikipedia.org/wiki/Special:FilePath/", gsub(" ", "_", species_name), ".jpg")
    
    # Render species image with fallback if image is not available
    output$species_image <- renderUI({
      tagList(
        tags$img(src = image_url, alt = paste("Image of", species_name), height = "150px", width = "auto",
                 onerror = "this.onerror=null; this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/No_image_available.svg/1024px-No_image_available.svg.png';"),  # Fallback image if not available
        tags$p("Image credit: Wikipedia (https://www.wikipedia.org/)", style = "font-size: 12px; color: #666;")
      )
    })
  })
  
  # Reactive expression for selected data
  select_data__ <- reactive({
    class_selected <- input$Taxons
    genus_selected <- input$Genus
    species_selected <- input$Species
    
    if (is.null(species_selected)) {
      data <- data.frame()  # Return empty data frame if no species selected
    } else {
      data <- AtlasEN[AtlasEN$Taxonomic.groups == class_selected & 
                        AtlasEN$Genus == genus_selected & 
                        AtlasEN$Species == species_selected, ]
    }
    
    data
  })
  
  # CSV download handler
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("extracted_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(select_data__(), file, row.names = FALSE)
    }
  )

  # JSON download handler
  output$download_json <- downloadHandler(
    filename = function() {
      paste("extracted_data_", Sys.Date(), ".json", sep = "")
    },
    content = function(file) {
      jsonlite::write_json(select_data__(), file)
    }
  )
  
  # RDS download handler
  output$download_rds <- downloadHandler(
    filename = function() {
      paste("extracted_data_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(select_data__(), file)
    }
  )

  
  # Function to validate the structure of the uploaded file
  validate_structure <- function(uploaded_data, expected_data) {
    # Check if column names match
    columns_match <- identical(names(uploaded_data), names(expected_data))
    
    # Check if column types match
    types_match <- all(sapply(uploaded_data, class) == sapply(expected_data, class))
    
    return(columns_match && types_match)
  }
  
  # File upload logic (only proceeds if file input is enabled after token entry)
  observeEvent(input$upload_btn, {
    req(input$upload_token == secure_token, input$file_upload)  # Ensure token is valid and file is uploaded
    
    uploaded_data <- NULL
    file_ext <- tools::file_ext(input$file_upload$name)
    
    # Read the file based on its extension
    if (file_ext == "csv") {
      uploaded_data <- read.csv(input$file_upload$datapath)
    } else if (file_ext == "rds") {
      uploaded_data <- readRDS(input$file_upload$datapath)
    } else {
      showNotification("Unsupported file type. Please upload a CSV or RDS file.", type = "error")
      return(NULL)
    }
    
    # Validate structure before appending
    if (!is.null(uploaded_data) && validate_structure(uploaded_data, expected_structure)) {
      # Structure is valid; proceed with upload
      AtlasEN <- readRDS("Dados/AtlasEN_optimized.rds")
      AtlasEN <- bind_rows(AtlasEN, uploaded_data)
      saveRDS(AtlasEN, "Dados/AtlasEN_optimized.rds")
      showNotification("Data uploaded and saved successfully!", type = "message")
    } else {
      # Structure is invalid; show error notification
      showNotification("The uploaded file structure does not match the expected format. Please check and try again.", type = "error")
    }
  })
  
  
  # Define reactive expression for selected data
  select_data <- reactive({
    class_selected <- input$Taxons
    genus_selected <- input$Genus
    species_selected <- input$Species
    
    if (is.null(species_selected)) {
      data <- data.frame() # Return empty data frame
    } else {
      data <- AtlasEN[AtlasEN$Taxonomic.groups == class_selected & AtlasEN$Genus == genus_selected & AtlasEN$Species == species_selected, ]
    }
    
    data
  })
  

  output$habitatsMap <- renderLeaflet({

  # Define color mapping directly within addPolygons for clarity
  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%  # OpenStreetMap as one of the layers
    addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeoWorldMap") %>%  # NatGeoWorldMap
    addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%  # Esri World Imagery map
    addPolygons(data = Montesinho_EN, color = "black", weight = 5, fillColor = "transparent", group = "SAC Montesinho/Nogueira") %>%
    
    # Habitat polygons with explicit colors
    addPolygons(
      data = habitats,
      color = ~ifelse(Habitats == "9340pt1", "blue",        # Mixed oak woodlands
                      ifelse(Habitats == "9230pt2", "green", # Galicio-Portuguese oak woods
                             ifelse(Habitats == "4030+9230pt2", "red", # Dry heathlands and oak woods
                                    ifelse(Habitats == "4030+9340pt1", "yellow", # Heathlands with oak woodlands
                                           ifelse(Habitats == "9230pt2+4030", "purple", # Mixed oak and heathland
                                                  "orange"))))), # Other
      weight = 2,
      opacity = 0.7,
      fillOpacity = 0.5,
      fillColor = ~ifelse(Habitats == "9340pt1", "blue",
                          ifelse(Habitats == "9230pt2", "green",
                                 ifelse(Habitats == "4030+9230pt2", "red",
                                        ifelse(Habitats == "4030+9340pt1", "yellow",
                                               ifelse(Habitats == "9230pt2+4030", "purple", "orange"))))),
      popup = ~paste("Habitat:", Habitats),
      group = "Habitats"
    ) %>%
    addMiniMap(
      position = "bottomleft",
      tiles = providers$OpenStreetMap,
      toggleDisplay = TRUE
    ) %>%
    setView(lng = -6.840, lat = 41.8252, zoom = 10) %>%
    addSearchOSM() %>%
    
    # Custom legend mapping
    addLegend(
      position = "bottomright",
      colors = c("blue", "green", "red", "yellow", "purple", "orange"),
      labels = c("Mixed oak woodlands (9340pt1)",
                 "Galicio-Portuguese oak woods (9230pt2)",
                 "Dry heathlands and oak woods (4030+9230pt2)",
                 "Heathlands with oak woodlands (4030+9340pt1)",
                 "Mixed oak and heathland (9230pt2+4030)",
                 "Other"),
      title = "Habitat Types",
      opacity = 0.7
    ) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Esri World Imagery",  "NatGeoWorldMap"),
      overlayGroups = c("Habitats"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addMeasure(
      position = "topright",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479"
    )
})



  # Put raster input reactive
  reactiveRaster_EN <- reactive({modis.rasters_EN[[input$EVs_EN]]})
  
  # Output 'map'
  output$map <- renderLeaflet({
    selected_data <- select_data()
    
    map <- leaflet() %>%
      setView(lng= -6.904, lat= 41.8102, zoom = 10)  %>%
      addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), group = "OpenStreetMap") %>%
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$Esri, group = "Esri") %>%
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$Stamen.Terrain, group = "Stamen.Terrain") %>%
      addControl(html =  "<img src= 'https://github.com/BravoAlpha2/WebGIS/blob/main/Images/presence.png?raw=true'>  Occurrences <br/>", position = "bottomright") 
    
    
    if (input$showGrid) {
      map <- map %>%
        addPolygons(
          color = "darkgrey",
          weight = 1,
          label = ~CUTM1K,
          data = Grid_EN,
          group = "Grid"
        )
    }
    
    if (nrow(selected_data) > 0) {
      map <- map %>% 
        addPolygons(
          color = "darkgrey",
          fillColor = "darkred",
          fillOpacity = 0.65,
          weight = 1,
          label = ~CUTM1K,
          data = Grid_EN[Grid_EN$CUTM1K %in% selected_data$CUTM1K, ],
          group = "Occurrences"
        )
    }
    
    
    map %>% 
      addPolygons(data = Montesinho_EN, color = "black", weight = 5, fillColor = "transparent", group = "SAC Montesinho/Nogueira") %>%
      addRasterImage(reactiveRaster_EN(), colors=terrain.colors(21), layerId = input$EVs_EN, opacity=input$Opacity) %>%
      addLayersControl(baseGroups = c("OpenStreetMap", "Stamen.Terrain", "Esri"), overlayGroups = c("SAC Montesinho/Nogueira")) %>%
      addMeasure(position = "topright", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", activeColor = "#3D535D", completedColor = "#7D4479") %>%
      addMiniMap(position = "bottomleft",tiles = providers$OpenStreetMap,toggleDisplay = TRUE) %>%
      addSearchOSM()
  })

     
  
  
  ############    More Info. Biodiversity data    ###############
  
  # Assume 'species_data' is your dataset
  species_data <- Dataset %>%
    filter(European..EU..status == "Endangered (EN)") %>%  # Filter for endangered species
    select(Phylum, Order, Family, Genus, Species) %>%
    distinct()  # Keep only unique rows
  
  # Function to create a nested list
  create_tree_structure <- function(data) {
    # Build nested structure for each unique path
    data %>%
      rowwise() %>%
      mutate(
        children = list(
          list(
            tibble(name = Species)
          )
        ),
        children = list(
          list(
            tibble(name = Genus, children = children)
          )
        ),
        children = list(
          list(
            tibble(name = Family, children = children)
          )
        ),
        children = list(
          list(
            tibble(name = Order, children = children)
          )
        ),
        children = list(
          list(
            tibble(name = Phylum, children = children)
          )
        )
      ) %>%
      ungroup() %>%
      select(Phylum, children) %>%
      distinct() %>%  # Ensure unique combinations of Phylum and children
      group_by(Phylum) %>%
      summarise(children = list(bind_rows(children)), .groups = 'drop') %>%
      summarise(
        children = list(
          tibble(
            name = " ",
            children = list(bind_rows(children))
          )
        )
      ) %>%
      pull(children) %>%
      .[[1]]
  }
  
  # Create tree structure
  tree_data <- create_tree_structure(species_data)
  
  # Render the tree with echarts4r
  output$species_tree <- renderEcharts4r({
    tree_data %>%
      e_charts() %>%
      e_tree(initialTreeDepth = 1, label = list(offset = c(0, -10))) %>%
      e_title(text = "European IUCN status: Endangered (EN) species in SAC-MN", subtext = "(Click to see)")
  })
  
  
  
  
  # Assume 'species_data' is your dataset
  species_data_1 <- Dataset %>%
    filter(European..EU..status == "Least Concern (LC)") %>%  # Filter for vulnerable species
    select(Taxonomic.group, Species) %>%  # Focus on Taxonomic.group and Species
    distinct() %>%  # Keep only unique rows
    mutate(Taxonomic.group = gsub("Flora \\(vascular plants\\)", "Vascular plants", Taxonomic.group))  # Rename "Flora (vascular plants)" to "Flora"
  
  # Count species per Taxonomic.group
  species_counts <- species_data_1 %>%
    count(Taxonomic.group)
  
  # Render the bar plot with echarts4r
  output$species_tree_2 <- renderEcharts4r({
    species_counts %>%
      e_charts(Taxonomic.group) %>%
      e_bar(n, name = "Number of Species") %>%  # Create a bar chart with counts
      e_title(
        text = "European status: Number of Least Concern (LC) species by Taxa in SAC-MN",
        subtext = " "
      ) %>%
      e_x_axis(type = "category", name = " ", axisLabel = list(rotate = 0)) %>%  # Keep x-axis labels horizontal
      e_y_axis(name = " ") %>%  # Label y-axis
      e_tooltip(trigger = "axis") %>%  # Tooltip for interactivity
      e_legend(show = FALSE)  # Hide legend if not needed
  })
  
  
  species_data_2 <- Dataset %>%
    filter(European..EU..status == "Vulnerable (VU)") %>%  # Filter for vulnerable species
    select(Taxonomic.group, Species) %>%  # Focus on Taxonomic.group and Species
    distinct() %>%  # Keep only unique rows
    mutate(Taxonomic.group = gsub("Flora \\(vascular plants\\)", "Vascular plants", Taxonomic.group))  # Rename "Flora (vascular plants)" to "Flora"
  
  species_counts2 <- species_data_2 %>%
    count(Taxonomic.group)
  
  output$pie <- renderEcharts4r({
    species_counts2 %>%
      e_charts(Taxonomic.group) %>%
      e_pie(n, name = " ") %>%  # Create a pie chart
      e_title(
        text = "European status: Number of Vulnerable (VU) species by Taxa in SAC-MN",
        subtext = " "
      ) %>%
      e_tooltip(trigger = "item") %>%  # Tooltip for interactivity
      e_legend(show = FALSE)  # Show legend for better interpretation
  })
  
  
  
  
  species_data_3 <- Dataset %>%
    filter(European..EU..status == "Near Threatened (NT)") %>%  # Filter for Near Threatened (NT) species
    select(Taxonomic.group, Species) %>%  # Focus on Taxonomic.group and Species
    distinct() %>%  # Keep only unique rows
    mutate(Taxonomic.group = gsub("Flora \\(vascular plants\\)", "Vascular plants", Taxonomic.group))  # Rename "Flora (vascular plants)" to "Flora"
  
  species_counts3 <- species_data_3 %>%
    count(Taxonomic.group)
  
  # Render a treemap with echarts4r
  output$treemap <- renderEcharts4r({
    species_counts3 %>%
      e_charts(Taxonomic.group) %>%
      e_bar(n, name = " ") %>%  # Create a bar chart with counts
      e_title(
        text = "European status: Number of Near Threatened (NT) Species by Taxa in SAC-MN",
        subtext = " "
      ) %>%
      e_x_axis(type = "category", name = " ", axisLabel = list(rotate = 45)) %>%  # Rotate labels for readability
      e_y_axis(name = " ") %>%  # Label y-axis
      e_tooltip(trigger = "axis") %>% # Tooltip for interactivity
    e_legend(show = FALSE) 
  })
  
  ############    More Info. Habitats data    ###############
  
  # Habitat information handling for the Habitats curiosities tab
  selected_habitat_info <- reactive({
    req(input$selected_habitat)
    habitat_info %>% filter(HabitatCode == input$selected_habitat)
  })
  
  output$habitat_image <- renderUI({
    habitat_data <- selected_habitat_info()
    if (nrow(habitat_data) > 0 && !is.na(habitat_data$ImageURL)) {
      tags$img(
        src = habitat_data$ImageURL, 
        alt = habitat_data$HabitatName, 
        width = "100%", 
        height = "auto",
        onerror = "this.onerror=null; this.src='https://via.placeholder.com/640x400?text=Image+Not+Available';"
      )
    } else {
      tags$img(src = "https://via.placeholder.com/640x400?text=Image+Not+Available", alt = "No image available", width = "100%", height = "auto")
    }
  })
  
  output$habitat_description <- renderUI({
    habitat_data <- selected_habitat_info()
    description <- habitat_data$Description
    if (nrow(habitat_data) > 0 && !is.na(description)) {
      tags$p(description)
    } else {
      tags$p("No description available for this habitat.")
    }
  })
  
  
} #End of server

# Run the application 
shinyApp(ui = ui, server = server)


