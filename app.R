#install.packages(c('shiny','leaflet','leaflet.extras','viridis','leaflegend','leafem','archive','DT','sf','raster','gdistance'))
# install.packages('archive')
# install.packages('sf')
# install.packages('raster')
# install.packages('gdistance')
# install.packages('leaflet')
# install.packages('leaflet.extras')
# install.packages('leaflegend')
# install.packages('leafem')
# install.packages('viridis')

library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(leafem)
library(archive)
library(DT)
library(sf)
library(viridis)
library(raster)
library(gdistance)


fileInputArea <- function(inputId, label, multiple = FALSE, accept = NULL,
                          width = NULL, buttonLabel = "Browse...", placeholder = "No file selected") {
  restoredValue <- restoreInput(id = inputId, default = NULL)
  
  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  
  inputTag <- tags$input(
    id = inputId,
    name = inputId,
    type = "file",
    # Don't use "display: none;" style, which causes keyboard accessibility issue; instead use the following workaround: https://css-tricks.com/places-its-tempting-to-use-display-none-but-dont/
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restoredValue
  )
  
  if (multiple) {
    inputTag$attribs$multiple <- "multiple"
  }
  if (length(accept) > 0) {
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  }
  
  div(
    class = "form-group shiny-input-container",
    style = htmltools::css(width = htmltools::validateCssUnit(width)),
    shiny:::shinyInputLabel(inputId, ""),
    div(
      class = "input-group",
      # input-group-prepend is for bootstrap 4 compat
      tags$label(
        class = "input-group-btn input-group-prepend",
        span(
          class = "btn btn-area", inputTag,
          div(tags$image(src = icon_encoded, width = "80px;"), style = "margin-top: 0rem;"),
          div(p(label), style = "font-size: 1.2rem; font-weight: 700; padding-top: 0rem;"),
          div(p(buttonLabel), style = "font-size: 1rem; font-weight: 400; margin-bottom: 0rem;")
        )
      )
    ),
    tags$div(
      id = paste(inputId, "_progress", sep = ""),
      class = "progress active shiny-file-input-progress",
      tags$div(class = "progress-bar")
    )
  )
}

# Use Bootstrap 5 colors $gray-700 and $gray-600
css_btn_area <- textConnection("
.btn-area {
  color: #495057;
  border-color: #495057;
  border-style: dashed;
  border-width: 2px;
  border-radius: 20px;
  background-color: transparent;
}

.btn-area:hover {
  color: #6c757d;
}

.progress {
  height: 32px;
}

.progress .progress-bar {
  font-size: 16px;
  line-height: 28px;
}")

# Icon from <https://icons.getbootstrap.com/icons/upload/>
icon_file <- tempfile(fileext = ".svg")
writeLines('
<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="#495057" class="bi bi-upload" viewBox="0 0 16 16">
  <path d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5z"/>
  <path d="M7.646 1.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1-.708.708L8.5 2.707V11.5a.5.5 0 0 1-1 0V2.707L5.354 4.854a.5.5 0 1 1-.708-.708l3-3z"/>
</svg>',
           con = icon_file
)
icon_encoded <- xfun::base64_uri(icon_file)

#############################################################
##### Accesibilidad Previa
municipios = sf::read_sf("Accesibilidad//municipiosjair.shp")
#setwd("Accesibilidad/Accesibilidad/")
uso_de_suelo=raster("Accesibilidad/uso_de_suelo_friccion.tif")
pendiente=raster("Accesibilidad/pendiente.tif")
carreteras=raster("Accesibilidad/carreteras.tif")
extent(carreteras)==extent(pendiente) &
  extent(uso_de_suelo)==extent(pendiente)

#Sí me voy a tomar la libertad de actualizar los valores del raster que estén cerca de 90 grados
pendiente[pendiente<95.9 & pendiente>=90]=95.9
pendiente[pendiente<=90 & pendiente>84.9]=84.9

####Accesibilidad a pie
slp_walk = 6 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.05))  # Calcula la velocidad de caminata ajustada por la pendiente.
terrain_walk_spd = uso_de_suelo * slp_walk       #Le quité el /5.0. Quiero pensar que es la velocidad de caminata según uso de suelo. El promedio es de 5.5 km/h         # Calcula la velocidad sobre el terreno ajustada por la pendiente y el uso de suelo.

##Accesibilidad por carreteras
slp_car = 50 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.12))  # Calcula la velocidad sobre carreteras ajustada por la pendiente.
sloped_road_spd = carreteras * slp_car / 50.0 # Calcula la velocidad ajustada por pendiente para carreteras y la convierte en un raster.
merged_spd = merge(sloped_road_spd, terrain_walk_spd)     # Combina los rasters de velocidad de carreteras y terreno.
friction = 1.0 / (merged_spd * 1000 / 60.0 ) 

library(gdistance)
Trans = transition(friction, function(x) 1 / mean(x), 8)  # Crea una matriz de transición basada en la fricción.
T.GC = geoCorrection(Trans, type="c") 

hidalgo= sf::st_read("Accesibilidad/hidalgo/LIM_MUNICIPALES.shp")



card <- function(title, ...) {
  htmltools::tags$div(
    class = "card",
    htmltools::tags$div(class = "card-header", title),
    htmltools::tags$div(class = "card-body", ...)
  )
}

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  includeCSS(css_btn_area), # Make sure css_btn_area is defined or loaded
  
  fluidRow(
    # Left Column: Sidebar with explanation and card
    column(width = 4,xs=12,sm=12,md=4,lg=4,xl=4, style = "background-color: #f5f5f5; padding: 20px;", 
           h2("Cálculo de Accesibilidad"),
           HTML(
             "<p>
             La accesibilidad se calcula como el costo de traslado a un lugar de destino predefinido. Para obtenerlo, se considera:
             <ul>
               <li><strong>Vialidades carreteras</strong> en el estado, así como sus velocidades promedio.</li>
               <li>Tipo de <strong>uso de suelo</strong>.</li>
               <li>Modelo digital de <strong>elevación</strong>.</li>
             </ul>
             Un modelo de movilidad sobre grafos determina el costo mínimo de traslado (en minutos) desde cada punto del estado hacia el más cercano de los lugares destino.
           </p>"
           ),
           
           # Card for file input
           card(
             title = "Agrega las ubicaciones. Puedes seleccionar varios archivos o subir un archivo .rar",
             # Removed full_screen as it's not a standard argument for card() based on your definition
             
             # Centering content and adding scroll if content overflows
             div(style = "display: flex; flex-direction: column; justify-content: center; align-items: center; padding: 0px; overflow-y: auto;", # Added padding and overflow-y
                 fileInputArea(
                   inputId = "filemap",
                   label = "Arrastra o selecciona tus archivos .shp, .dbf, .shx, .prj, etc. aquí:",
                   buttonLabel = "Click para seleccionar archivos",
                   multiple = TRUE,
                   accept = c('.shp',".kml",".GeoJSON",".kmz", ".rar", ".zip")
                 ),
                 shiny::tableOutput("files")
             )
           ),
           downloadButton("downloadTiff", "Descargar TIFF")
    ),
    
    # Right Column: Map
    column(width = 8,xs=12,sm=12,md=8,lg=8,xl=8, style = "height: 100vh;", # Keep 100vh for the map column
           leafletOutput("map", height = "100%")
    )
  )
)
# Función para manejar archivos temporales ----
rutina_crear_copias_temporales <- function(inputFiles) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  if (!grepl("\\.(rar|zip|kmz)$", inputFiles$datapath[1], ignore.case = TRUE)) {
    for (i in seq_along(inputFiles$name)) {
      file.copy(inputFiles$datapath[i], file.path(temp_dir, inputFiles$name[i]))
    }
  } else {
    file.copy(inputFiles$datapath[1], file.path(temp_dir, inputFiles$name[1]))
    archive_extract(file.path(temp_dir, inputFiles$name[1]), dir = temp_dir)
  }
  return(temp_dir)
}

server <- function(input, output, session) {
  df <- reactive({
    req(input$filemap)
    temp_dir <- rutina_crear_copias_temporales(input$filemap)
    shapes=list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    kmls=list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE)
    geojsons=list.files(temp_dir, pattern = "\\.geojson$", full.names = TRUE)
    para_leer=list(shapes,kmls,geojsons)[which.max(list(shapes,kmls,geojsons) |> lapply(length))]
    if(which.max(list(shapes,kmls,geojsons) |> lapply(length))==1){
      read_sf(para_leer) |> st_zm()
    }
    else{
      st_read(para_leer)|> st_zm()
    }
    
  })
  tiempo_zona_p=reactive({
    req(df())
    if(is.na(st_crs(df()))){
      df=st_set_crs(df(),value ="EPSG:4326" )
      puntos = df
    }else{
      puntos = df()
    }
    puntos=puntos |> st_transform(st_crs(hidalgo))
    coordenadas = sf::st_coordinates(puntos)
    tiempo_zona_p = accCost(T.GC, coordenadas)
    raster::crs(tiempo_zona_p) = crs(hidalgo)
    tiempo_zona_p
  })
  
  # Descargar TIFF
  output$downloadTiff <- downloadHandler(
    filename = function() {
      paste0("mi_raster_shiny_", Sys.Date(), ".tif")
    },
    content = function(file) {
      writeRaster(tiempo_zona_p(), file, overwrite = TRUE)
    }
  )

  # Mostrar mapa
  output$map <- renderLeaflet({
    req(tiempo_zona_p())
    ###Pendiente: Municipio de ubicación.
    tiempo_zona=tiempo_zona_p()
    tiempo_zona[is.infinite(tiempo_zona)] = NA
    tiempo_zona[tiempo_zona >= 300] = 300
    min_valor = raster::cellStats(tiempo_zona, stat = 'min')
    max_valor = raster::cellStats(tiempo_zona, stat = 'max')
    
    paleta = colorNumeric(
      palette = viridisLite::turbo(n = length(min_valor:max_valor), direction = -1, alpha = 0.5),
      domain = values(tiempo_zona),  
      na.color = "transparent"
    )
  print(colnames(df()))
  #writeRaster(tiempo_zona,"A.tif")
    leaflet() |>
      addTiles() |> 
      addMarkers(data=df() |> st_cast("POINT") |> as("Spatial"), 
                 popup = ~paste0(
                   ifelse("Name" %in% colnames(df()),
                          paste0("Nombre: <b>", Name, "</b><br>")
                          ,""),
                   ifelse("nombre" %in% colnames(df()),
                          paste0("Nombre: <b>", nombre, "</b><br>")
                          ,"")
                   ,
                   ifelse("scrtr_n" %in% colnames(df()),
                          paste0("Secretaría: <b>", scrtr_n, "</b><br>")
                          ,"")
                   ,
                   ifelse("dpndnc_n" %in% colnames(df()),
                          paste0("Dependencia: <b>", dpndnc_n, "</b><br>")
                          ,"")
                   ,
                   ifelse("nmbr_st" %in% colnames(df()),
                          paste0("Nombre establecimiento: <b>", nmbr_st, "</b><br>")
                          ,"")
                   ,
                   ifelse("horr_st" %in% colnames(df()),
                          paste0("Horario del establecimiento: <b>", horr_st, "</b>")
                          ,"")
                   )
                 ) |> 
      addRasterImage(x = tiempo_zona, colors = viridis::turbo(n = length(min_valor:max_valor), direction = -1, alpha = 0.5)) |> 
      addPolygons(data=municipios |> as("Spatial"),
                  label = municipios$NOM_MUN,fillColor = "gray",fillOpacity = 0.1,color = "white",weight = 1,opacity = 0.4,group = "Municipios") |> 
      addLegend(values = c(min_valor:max_valor) , pal = paleta, title = paste0("Tiempo", "<br>","Aproximado"), position = "bottomright",
                labFormat = labelFormat(
                  between = " – ",
                  suffix = " min",
                  transform = function(x) {x}
                )) |> 
      addSearchFeatures(targetGroups = c("Municipios"),
                        options = searchFeaturesOptions(
                          zoom = 12,
                          openPopup = F,
                          firstTipSubmit =F,initial = F,
                          hideMarkerOnCollapse =T))|>setView(lng = -98.7591, lat = 20.0511, zoom = 9) |> 
      addLogo(img = "https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/laboratorio_planeacion.png", position = "bottomleft", src = "remote", width = "399px",height = "80px" )
  })
}

shinyApp(ui, server)
