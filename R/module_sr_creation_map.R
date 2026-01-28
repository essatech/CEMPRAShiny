module_sr_creation_map_ui <- function(id) {
  ns <- NS(id)
  
  
  fluidRow(column(
    width = 12,
    # Mouse-over text for basin name and HUC code
    htmlOutput(ns("txt_basin_name")),
    htmlOutput(ns("txt_huc_code")),
    
    # Main leaflet output with variable width and fixed height
    leafletOutput(ns("srcmap"), height = 550),
  ))
  
  
}

module_sr_creation_map_server <- function(id, rv_formula_df, color_ramp) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    # --------------------------------------
    # Main leaflet map reactive expression
    # --------------------------------------
    # Main leaflet map is called here, but only static elements
    # are included that do not need updating with dynamic events.
    # HUCs are called later with proxy since we will update then frequently.
    output$srcmap <- renderLeaflet({
      print("Running renderLeaflet()...")
      mymap <- leaflet() %>%
        addProviderTiles(
          providers$Esri.WorldTopoMap,
          group = "Topo",
          options = providerTileOptions(noWrap = TRUE, opacity = 0.9)
        ) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
        addProviderTiles(
          providers$Esri.WorldGrayCanvas,
          group = "Grey",
          options = providerTileOptions(noWrap = TRUE, opacity = 0.9)
        ) %>%
        addLayersControl(
          baseGroups = c("Topo", "Imagery", "Grey"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
      if (isolate(session$userData$geom_type) == "lines") {
        line_weight <- 3
        if (!(is.null(
          isolate(session$userData$rv_HUC_layer_load$data$WIDTH)
        ))) {
          line_weight <- isolate(session$userData$rv_HUC_layer_load$data$WIDTH)
        }
        mymap <- addPolylines(
          map = mymap,
          data = session$userData$rv_HUC_layer_load$data,
          layerId = session$userData$rv_HUC_layer_load$data$uid,
          color = "#3b9ab2",
          weight = line_weight,
          smoothFactor = 0.5,
          opacity = 0.7,
          highlightOptions = highlightOptions(
            color = "#c2fffe",
            weight = 4,
            bringToFront = TRUE
          )
        )
      } else if (isolate(session$userData$geom_type) == "points") {
        # Point geometry - use circle markers
        point_radius <- 8
        if (!(is.null(isolate(session$userData$rv_HUC_layer_load$data$RADIUS)))) {
          point_radius <- isolate(session$userData$rv_HUC_layer_load$data$RADIUS)
        }
        mymap <- addCircleMarkers(
          map = mymap,
          data = session$userData$rv_HUC_layer_load$data,
          layerId = session$userData$rv_HUC_layer_load$data$uid,
          radius = point_radius,
          color = "#444444",
          weight = 1.5,
          opacity = 0.8,
          fillOpacity = 0.6,
          fillColor = "#d9d9d9"
        )
      } else {
        mymap <- addPolygons(
          map = mymap,
          data = session$userData$rv_HUC_layer_load$data,
          layerId = session$userData$rv_HUC_layer_load$data$uid,
          color = "#444444",
          weight = 1.2,
          smoothFactor = 0.5,
          opacity = 0.5,
          fillOpacity = 0.5,
          fillColor = "#d9d9d9",
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          )
        )
      }
      
      mymap
      # fitBounds(-119.04060, 52.37167, -114.78314, 54.76054)
    })
    
    
    # ---------------------------------------------------------
    # Reactive expression to reload, repaint or redraw polygons
    # ---------------------------------------------------------
    r_huc_polygons <- reactive({
      
      print("r_huc_polygons() triggered in sr creation...")

      # If clear all selected - then trigger redraw.
      session$userData$rv_redraw$redraw
      
      # HUC spatial geometry
      huc_geom <- session$userData$rv_HUC_geom$huc_geom
      
      # Check if geometry is line, point, or polygon
      geom_type <-
        st_geometry_type(session$userData$rv_HUC_layer_load$data)
      if (unique(geom_type)[1] %in% c("LINESTRING", "MULTILINESTRING")) {
        session$userData$geom_type <- "lines"
      } else if (unique(geom_type)[1] %in% c("POINT", "MULTIPOINT")) {
        session$userData$geom_type <- "points"
      } else {
        session$userData$geom_type <- "polygons"
      }
      
      # Merge stressor magnitude values to sf object
      huc_geom$values <- NA
      
      # Define the color vector (for P10-P90)
      i_colors <- c('#3288bd', '#66c2a5', '#abdda4', '#e6f598', 
                    '#ffffbf', '#fee08b', '#fdae61', '#f46d43', '#d53e4f')
      
      # Reverse colors if needed
      if(color_ramp() == "Blue High, Red Low") {
        m_colors <- rev(i_colors)
      } else {
        m_colors <- i_colors
      }
      
      
      if(length(rv_formula_df$dat) == 0) {
        
        # Values are null
        huc_geom$values <- NA
        huc_geom$color_vec2 <- "lightgrey"
        
      } else {
        
        # Get the formula value
        mvals <- rv_formula_df$dat$val
        
        if(all(is.na(mvals))) {
          
          huc_geom$values <- NA
          huc_geom$color_vec2 <- "lightgrey"
          
        } else {
          
          mpercentiles <- quantile(mvals, probs = seq(0, 1, 0.1), na.rm = TRUE)
          
          # Handle missing data with merge
          if(length(huc_geom$values) != length(mvals)) {
            dat2 <- rv_formula_df$dat
            dat2$val2 <- dat2$val
            dat2 <- dat2[, c("HUC_ID", "val2")]
            huc_geom <- merge(huc_geom, dat2, by.x = "HUC_ID", by.y = "HUC_ID", all.x = TRUE)
            huc_geom$values <- huc_geom$val2
            huc_geom$val2 <- NULL
          } else {
            huc_geom$values <- mvals
          }
          
          huc_geom$color_vec2 <- "lightgrey"
          huc_geom$color_vec2 <- ifelse(huc_geom$values <= mpercentiles[2], m_colors[1], huc_geom$color_vec2)
          huc_geom$color_vec2 <- ifelse(huc_geom$values > mpercentiles[2],  m_colors[2], huc_geom$color_vec2)
          huc_geom$color_vec2 <- ifelse(huc_geom$values > mpercentiles[3],  m_colors[3], huc_geom$color_vec2)
          huc_geom$color_vec2 <- ifelse(huc_geom$values > mpercentiles[4], m_colors[4], huc_geom$color_vec2)
          huc_geom$color_vec2 <- ifelse(huc_geom$values > mpercentiles[5], m_colors[5], huc_geom$color_vec2)
          huc_geom$color_vec2 <- ifelse(huc_geom$values > mpercentiles[6], m_colors[6], huc_geom$color_vec2)
          huc_geom$color_vec2 <- ifelse(huc_geom$values > mpercentiles[7], m_colors[7], huc_geom$color_vec2)
          huc_geom$color_vec2 <- ifelse(huc_geom$values > mpercentiles[8], m_colors[8], huc_geom$color_vec2)
          huc_geom$color_vec2 <- ifelse(huc_geom$values > mpercentiles[9], m_colors[9], huc_geom$color_vec2)
        }
      }
      
      huc_geom$color_vec2 <- ifelse(is.na(huc_geom$color_vec2), "lightgrey", huc_geom$color_vec2)

      # Update reference color dataframe rv to reset colors after selection
      color_df2 <-
        data.frame(id = huc_geom$HUC_ID, col = huc_geom$color_vec2)
      
      session$userData$rv_HUC_geom$color_df2 <- color_df2
      
      return(huc_geom)
    })
    
    
    # --------------------------------------
    # HUC polygon draw
    # --------------------------------------
    # Draw update or edit HUC polygons on leaflet map
    # use an observe() function to capture changes and use
    # leafletProxy() to only update the target layer.
    
    print("HUC polygon draw...")
    
    observe({
      print("Updating polygons with observer()...")

      # Get the data and determine geometry type directly from the data
      huc_data <- r_huc_polygons()
      req(huc_data)

      # Detect geometry type from the actual data
      geom_types <- unique(sf::st_geometry_type(huc_data))
      if (any(geom_types %in% c("POINT", "MULTIPOINT"))) {
        current_geom_type <- "points"
      } else if (any(geom_types %in% c("LINESTRING", "MULTILINESTRING"))) {
        current_geom_type <- "lines"
      } else {
        current_geom_type <- "polygons"
      }

      if (current_geom_type == "lines") {
        line_weight <- 3
        if (!(is.null(
          isolate(session$userData$rv_HUC_layer_load$data$WIDTH)
        ))) {
          line_weight <- isolate(session$userData$rv_HUC_layer_load$data$WIDTH)
        }
        leafletProxy("srcmap") %>%
          clearShapes() %>%
          # Add or update HUC polygons on the map
          addPolylines(
            data = huc_data,
            layerId = huc_data$uid,
            color = huc_data$color_vec2,
            weight = line_weight,
            smoothFactor = 0.5,
            opacity = 0.7,
            highlightOptions = highlightOptions(
              color = "#c2fffe",
              weight = 4,
              bringToFront = TRUE
            )
          ) %>%
          # Delete any old pre-existing legend
          clearControls()
      } else if (current_geom_type == "points") {
        # Point geometry - use circle markers
        point_radius <- 8
        if (!(is.null(isolate(session$userData$rv_HUC_layer_load$data$RADIUS)))) {
          point_radius <- isolate(session$userData$rv_HUC_layer_load$data$RADIUS)
        }
        leafletProxy("srcmap") %>%
          clearMarkers() %>%
          # Add or update points on the map
          addCircleMarkers(
            data = huc_data,
            layerId = huc_data$uid,
            radius = point_radius,
            color = "#444444",
            weight = 1.5,
            opacity = 0.8,
            fillOpacity = 0.7,
            fillColor = huc_data$color_vec2
          ) %>%
          # Delete any old pre-existing legend
          clearControls()
      } else {
        # Default to polygons
        leafletProxy("srcmap") %>%
          clearShapes() %>%
          # Add or update HUC polygons on the map
          addPolygons(
            data = huc_data,
            layerId = huc_data$uid,
            color = "#444444",
            weight = 1.2,
            smoothFactor = 0.5,
            opacity = 0.5,
            fillOpacity = 0.5,
            fillColor = huc_data$color_vec2,
            highlightOptions = highlightOptions(
              color = "white",
              weight = 2,
              bringToFront = TRUE
            )
          ) %>%
          # Delete any old pre-existing legend
          clearControls()
      }

      # Add on selected HUCs (if any)
      selected_hucs <- isolate(session$userData$rv_clickedIds$ids)
      
      if (length(selected_hucs) > 0) {
        print("Adding selected HUCs")
        huc_geom_sel <- session$userData$rv_HUC_geom$huc_geom
        
        # Get subset of selected HUCs
        huc_geom_sel <-
          huc_geom_sel[which(huc_geom_sel$uid %in% selected_hucs), ]
        
        # Update special ID
        huc_geom_sel$uid <- paste0("select|", huc_geom_sel$uid)
        
        # Add selected HUCs to map
        if (current_geom_type == "lines") {
          line_weight <- 3
          if (!(is.null(
            isolate(session$userData$rv_HUC_layer_load$data$WIDTH)
          ))) {
            line_weight <- isolate(session$userData$rv_HUC_layer_load$data$WIDTH)
          }
          leafletProxy("srcmap") %>%
            addPolylines(
              data = huc_geom_sel,
              layerId = huc_geom_sel$uid,
              color = "#4dfff3",
              weight = line_weight,
              smoothFactor = 0.5,
              opacity = 0.9,
              highlightOptions = highlightOptions(
                color = "#c2fffe",
                weight = 4,
                bringToFront = TRUE
              )
            )
        } else if (current_geom_type == "points") {
          # Selected points are slightly larger (radius + 4)
          point_radius <- 12
          if (!(is.null(isolate(session$userData$rv_HUC_layer_load$data$RADIUS)))) {
            point_radius <- isolate(session$userData$rv_HUC_layer_load$data$RADIUS) + 4
          }
          leafletProxy("srcmap") %>%
            addCircleMarkers(
              data = huc_geom_sel,
              layerId = huc_geom_sel$uid,
              radius = point_radius,
              color = "#c2fffe",
              weight = 2,
              opacity = 0.95,
              fillOpacity = 0.95,
              fillColor = "#4dfff3"
            )
        } else {
          # Default to polygons
          leafletProxy("srcmap") %>%
            addPolygons(
              data = huc_geom_sel,
              layerId = huc_geom_sel$uid,
              color = "#c2fffe",
              weight = 1.2,
              smoothFactor = 0.5,
              opacity = 0.9,
              fillOpacity = 0.95,
              fillColor = "#4dfff3",
              highlightOptions = highlightOptions(
                color = "white",
                weight = 2,
                bringToFront = TRUE
              )
            )
        }

      }
    })
    
    
    
    
    
    # ---------------------------------------------------------
    # Show the HUC Code and Basin Name Above Map
    # ---------------------------------------------------------
    output$txt_huc_code <- renderUI({
      if (session$userData$rv_map_shape()) {
        tags$p(session$userData$rv_map_location$huc_id, style = "float: right; color:#3b9ab2;")
      } else {
        tags$p("Location ID", style = "float: right; color:#3b9ab2;")
      }
    })
    
    output$txt_basin_name <- renderUI({
      if (session$userData$rv_map_shape()) {
        tags$p(session$userData$rv_map_location$huc_name, style = "float: left; color:#3b9ab2;")
      } else {
        tags$p("Location Name", style = "float: left; color:#3b9ab2;")
      }
    })
    
    
    # ---------------------------------------------------------
    # Mouse-over and mouse-out events
    # ---------------------------------------------------------
    # Observe mouseover events over leaflet map
    # note the event concatenation 'object name' + '_click'; 'object name' + '_shape_mouseover'
    observeEvent(input$srcmap_shape_mouseout, {
      session$userData$rv_map_shape(FALSE)
      session$userData$rv_stressor_response$active_values_raw <-
        NULL
    })
    
    observeEvent(input$srcmap_shape_mouseover, {
      # User hovers mouse over a polygon (layer specific)
      srcmap_shape_mouseover_info <-
        input$srcmap_shape_mouseover

      if (!(is.null(srcmap_shape_mouseover_info))) {
        # Parse the ID and HUC name
        session$userData$rv_map_shape(TRUE)
        poly_obj <-
          srcmap_shape_mouseover_info$id # note leaflet id slot
        parse_id <- strsplit(as.character(poly_obj), "\\|")[[1]]
        huc_id <- parse_id[1]
        huc_name <- parse_id[2]
        session$userData$rv_map_location$huc_id <- huc_id
        session$userData$rv_map_location$huc_name <- huc_name
        session$userData$rv_stressor_response$active_values_raw <- NULL

      }
    })

    # ---------------------------------------------------------
    # Mouse-over and mouse-out events for markers (points)
    # ---------------------------------------------------------
    # Circle markers fire marker_mouseover/mouseout events
    observeEvent(input$srcmap_marker_mouseout, {
      session$userData$rv_map_shape(FALSE)
      session$userData$rv_stressor_response$active_values_raw <- NULL
    })

    observeEvent(input$srcmap_marker_mouseover, {
      # User hovers mouse over a point marker
      marker_mouseover_info <- input$srcmap_marker_mouseover

      if (!(is.null(marker_mouseover_info))) {
        # Parse the ID and HUC name
        session$userData$rv_map_shape(TRUE)
        marker_obj <- marker_mouseover_info$id
        parse_id <- strsplit(as.character(marker_obj), "\\|")[[1]]
        huc_id <- parse_id[1]
        huc_name <- parse_id[2]
        session$userData$rv_map_location$huc_id <- huc_id
        session$userData$rv_map_location$huc_name <- huc_name
        session$userData$rv_stressor_response$active_values_raw <- NULL
      }
    })


    # ------------------------------------------
    # Update selected class on layer panel
    observe({
      req(input$hiddenload)
      req(session$userData$rv_stressor_response$active_layer)
      print("Setting style...")
      # print(active)
      # Strip class away from any other selected
      # q_code <- paste0("jQuery('.map-variable').removeClass('var-selected');")
      # shinyjs::runjs(code = q_code)
      
      # Add class to system capacity
      #  q_code <- paste0("jQuery('#main_map-var_id').addClass('var-selected');")
      # shinyjs::runjs(code = q_code)
    })
    
    
    
    
    
    
    
  })
}
  