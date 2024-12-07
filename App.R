### Add Libraries
library(sfnetworks)
library(sf)
library(tidygraph)
library(igraph)
library(ggplot2)
library(dplyr)
library(mapview)
library(purrr)
library(TSP)
library(lwgeom)
library(dodgr)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(readr)
library(shinycssloaders)
library(waiter)
library(crosstalk)
library(tibble)
library(RColorBrewer)
library(tidycensus)
library(osmdata)
library(geodist)

# Path to custom LTS weighting file
weight <- "/Users/Username/OneDrive - California Department of Transportation/Documents/PedAccessibilityApp/AccessAnalysis/data/wt_profile.json"

### UI
tab1 <- tabPanel(
  title = "Access Analysis",
  sidebarLayout(
    sidebarPanel(
      textInput("project_name", "Project Name", value = ""),
      radioButtons(
        "mode_select",
        "Select Mode",
        choices = list("Walk" = "walk", "Bike" = "bike"),
        selected = "walk"
      ),
      radioButtons(
        "modification_select",
        "Select Modification Type",
        choices = list("Add New Network Links" = "new", "Modify Existing Network Links" = "modify"),
        selected = "new"
      ),
      selectInput(
        "mod_type",
        "Select OSM Way Type for Modification:",
        c("motorway",
          "trunk",
          "primary",
          "secondary",
          "tertiary",
          "unclassified",
          "residential",
          "service",
          "track",
          "cycleway",
          "path",
          "steps",
          "ferry",
          "living_street",
          "bridleway",
          "footway",
          "pedestrian",
          "motorway_link",
          "trunk_link",
          "primary_link",
          "secondary_link",
          "tertiary_link"),
        selected = "residential",
        multiple = FALSE
      ),
      actionButton("run_analysis", "Run Analysis")
    ),
    
    mainPanel(
      leafletOutput("map"),
      DTOutput("feature_table")
    )
  )
)

tab2 <- tabPanel(
  title = "Advanced Configuration",
  mainPanel(
    strong("Advanced configuration parameters:"),
    p("Parameters will go here."),
    strong("Analysis Area Parameters:"),
    p("Set the buffer that will determine the size of the study area for each mode."),
    numericInput(
      "walk_radius",
      "Set Walk Analysis Radius (in miles):",
      1,
      min = .25,
      max = 3,
      step = .25
    ),
    numericInput(
      "bike_radius",
      "Set Bike Analysis Radius (in miles):",
      3,
      min = .25,
      max = 10,
      step = .25
    )
  )
)

tab3 <- tabPanel(
  title = "Instructions",
  mainPanel(
    strong("Instructions on how to use this web mapping application:"),
    p("The instructions will go here.")
  )
)

ui <- navbarPage(
  title = "Acccess to Destinations Scenario Analysis Tool",
  tab1,
  tab2,
  tab3
)

### Server
server <- function(input, output, session) {
  
  # Reactive value to store the drawn feature
  drawn_feature <- reactiveVal(NULL)

  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 5.2) %>%
      addDrawToolbar(
        targetGroup = "drawn_feature",
        polylineOptions = drawPolylineOptions(),
        polygonOptions = drawPolygonOptions(),
        rectangleOptions = FALSE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        markerOptions = FALSE,
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      ) 
  })
  
  # Observe event for when input is drawn
  observeEvent(input$map_draw_new_feature, {
    
    feature <- input$map_draw_new_feature
    
      # Get the coordinates of the drawn line
      if (feature$geometry$type == "LineString") {
        line_coords <- feature$geometry$coordinates
        # Store the drawn line coordinates
        drawn_feature(st_as_sf(data.frame(
          id = 1, geometry = st_sfc(st_linestring(matrix(unlist(line_coords), ncol = 2, byrow = TRUE)))),
          crs = 4326))
      }
      
      # Get the coordinates of the drawn polygon
      if (feature$geometry$type == "Polygon") {
        coords <- feature$geometry$coordinates[[1]]
        coords <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
        # Convert the coordinates into an SF object
        polygon_sf <- st_sfc(st_polygon(list(coords)), crs = 4326)
        drawn_feature(polygon_sf)
      }
  })
  
  # Run the analysis when the run analysis button is pressed
  observeEvent(input$run_analysis, {
    req(drawn_feature())
    
    if(input$mode_select == "walk") {
      buff_dist <- input$walk_radius * 1609.34
    } else if(input$mode_select == "bike") {
      buff_dist <- input$bike_radius * 1609.34
    } 
    
    line_sf <- drawn_feature()
    
    buffer_distance <- buff_dist
    
    # Create the buffer
    buffer_sf <- st_buffer(line_sf, dist = buffer_distance)
    
    # Create grid
    grid <- st_make_grid(buffer_sf, square = FALSE, cellsize = .003)
    
    grid_int = lengths(st_intersects(grid, buffer_sf)) > 0
    
    grid <- grid[grid_int]
    
    grid <- grid %>%
      st_as_sf()
    grid$grid_id <- seq.int(nrow(grid))
    
    # Create bounding box around the grid
    bbox <- st_bbox(grid)
    
    # Define origin/dest points
    geo_point <- grid %>%
      st_as_sf() %>%
      st_centroid(geo) %>%
      dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                    lat = sf::st_coordinates(.)[,2]) %>%
      mutate(from_x = lon,
             from_y = lat,
             to_x = lon,
             to_y = lat) %>%
      st_drop_geometry()
    
    from <- geo_point %>%
      select(from_x, from_y) %>%
      as.matrix()
    
    to <- geo_point %>%
      select(to_x, to_y) %>%
      as.matrix()
    
    # Download population data
    ca_pop <- get_acs(
      geography = "tract",
      state = 06,
      variables = "B01003_001",
      year = 2021,
      survey = "acs5",
      geometry = T
    ) %>%
      dplyr::select(GEOID, estimate) %>%
      st_transform(3310) %>%
      st_as_sf()
    
    ca_pop$original_area <- st_area(ca_pop)
    
    grid_int <- grid %>%
      st_transform(3310)
    
    geo_intersect <- st_intersection(grid_int, ca_pop)
    
    geo_intersect$new_area <- st_area(geo_intersect)
    
    geo_intersect <- geo_intersect %>%
      mutate(pop = as.numeric(estimate * (new_area / original_area))) %>%
      group_by(grid_id) %>%
      summarise(pop_est = sum(pop))
    
    geo_pop <- geo_intersect %>%
      st_drop_geometry()
    
    
    ### Download OSM POIs
    poi_type <- "amenity"
    poi_value <- "restaurant"
    
    osm_data <- opq(bbox = bbox) %>%
      add_osm_feature(key = poi_type, value = poi_value) %>%
      osmdata_sf()
    
    poi_points <- osm_data$osm_points %>%
      mutate(count = 1)
    
    points_int <- st_intersection(poi_points, grid) %>%
      group_by(grid_id) %>%
      summarise(poi_count = sum(count)) %>%
      st_drop_geometry()
    
    grid_merge <- merge(grid,
                        points_int,
                        by = "grid_id",
                        all.x = T)
    
    grid_merge[is.na(grid_merge)] <- 0
    
    
    ### Routing 
    
    if(input$mode_select == "walk") {
      weight_profile <- "foot"
    } else if(input$mode_select == "bike") {
      weight_profile <- "bicycle"
    } 
    
    # Extract OSM network
    baseline_net <- dodgr_streetnet(bbox)
    
    baseline_graph <- weight_streetnet(baseline_net, wt_profile = weight_profile, wt_profile_file = weight, type_col = "highway", id_col = "osm_id")
    
    baseline_graph$time <- baseline_graph$time_weighted
    
    baseline_time <- dodgr_times(baseline_graph, from = from, to = to, shortest = FALSE)
    
    if(input$modification_select == "new") {
      
      line_sf <- line_sf %>%
        st_as_sf() %>%
        mutate(osm_id = as.character(max(as.numeric(baseline_net$osm_id)) + 1),
               highway = input$mod_type)
      
      baseline_net_split <- st_split(baseline_net, line_sf) %>%
        st_collection_extract("LINESTRING") %>%
        st_as_sf()
      
      modification_split <- st_split(line_sf, baseline_net) %>%
        st_collection_extract("LINESTRING") %>%
        st_as_sf()
      
      # Merge
      combined_geometry <- dplyr::bind_rows(baseline_net_split, modification_split) %>%
        sf::st_as_sf()
      
      build_graph <- weight_streetnet(combined_geometry, wt_profile = weight_profile, wt_profile_file = weight, type_col = "highway", id_col = "osm_id")
      build_graph$time <- build_graph$time_weighted
      build_time<- dodgr_times(build_graph, from = from, to = to, shortest = FALSE)
      
    } else if(input$modification_select == "modify") {
      
      combined_geometry <- baseline_net %>%
        st_transform(4326)
      
      line_sf <- line_sf %>%
        st_as_sf(crs = 4326) %>%
        st_transform(4326)
      
      line_sf <- line_sf[st_geometry_type(line_sf) %in% c("POLYGON", "MULTIPOLYGON"), ]
    
      contained_indices <- st_within(combined_geometry, drawn_feature(), sparse = FALSE)
      
      if (any(contained_indices)) {
        # Add a new attribute to road segments contained entirely within polygon
        combined_geometry[["highway"]] <- ifelse(contained_indices[, 1], input$mod_type, combined_geometry[["highway"]])
        
      } else {
        cat("No road segments contained within selection area:")
      }
      
      build_graph <- weight_streetnet(combined_geometry, wt_profile = weight_profile, wt_profile_file = weight, type_col = "highway", id_col = "osm_id")
      build_graph$time <- build_graph$time_weighted
      build_time<- dodgr_times(build_graph, from = from, to = to, shortest = FALSE)
    }
    
    ### Calculate access
    poi_matrix <- grid_merge %>%
      select(poi_count) %>%
      st_drop_geometry() %>%
      as.matrix()
    
    poi_matrix <- matrix(rep(poi_matrix, ncol(baseline_time)), nrow = nrow(grid_merge), byrow = FALSE)
    poi_matrix <- apply(t(poi_matrix), 2, rev)
    
    baseline_access <- poi_matrix * exp(log(0.5) / (15 * 60) * (((baseline_time / 60)) * 60))
    build_access <- poi_matrix * exp(log(0.5) / (15 * 60) * (((build_time / 60)) * 60))
    
    df_baseline <- apply(baseline_access, 1, FUN=sum, na.rm=TRUE) %>% as_data_frame() %>%
      rename("baseline_access" = "value")
    
    df_build <- apply(build_access, 1, FUN=sum, na.rm=TRUE) %>% as_data_frame() %>%
      rename("build_access" = "value")
    
    geo <- st_sf(data_frame(grid, df_baseline))
    
    geo <- st_sf(data_frame(geo, df_build)) %>%
      mutate(access_change = build_access - baseline_access)
    
    geo <- merge(geo,
                 geo_pop,
                 by = "grid_id",
                 all.x = T)
    
    geo_table <- geo %>%
      st_drop_geometry() %>%
      mutate(access_pct_change = (build_access - baseline_access) / baseline_access) %>%
      mutate(access_pct_change_clean = ifelse(is.infinite(access_pct_change), 0, access_pct_change)) %>%
      mutate(access_change = build_access - baseline_access) %>%
      mutate(access_change_clean = ifelse(is.infinite(access_change), 0, access_change)) %>%
      mutate(pop_est_clean = ifelse(is.na(pop_est), 0, pop_est))
    
    avg_pct_change <- weighted.mean(geo_table$access_pct_change_clean, geo_table$pop_est_clean, na.rm = T)
    avg_change <- weighted.mean(geo_table$access_change_clean, geo_table$pop_est_clean, na.rm = T)
    
    output_table <- data.frame(Project = input$project_name,
                               Avg_Pct_Change = avg_pct_change,
                               Avg_Change = avg_change)
    
    # Create a color palette
    palette <- colorNumeric(palette = "viridis", domain = geo$access_change)
    
    #Render the buffer on the map
    leafletProxy("map") %>%
      clearGroup("buffer") %>%
      addPolygons(
        data = geo,
        fillColor = ~palette(geo$access_change),
        fillOpacity = 0.5,
        weight = 0,
        popup = ~paste("Change in Access: ", geo$access_change)
      ) %>%
      addLegend("bottomright",
                pal = palette,
                values = geo$access_change,
                title = "Access Change",
                opacity = 0.5)
    
    output$feature_table <- renderDT({
      datatable(output_table,
                options = list(
                  dom = "t",
                  ordering = FALSE,
                  paging = FALSE,
                  searching = FALSE
                ), 
                selection = 'none',
                class = 'row-border',
                escape = FALSE,
                rownames = TRUE,
                filter = "none",
                width = 500
      ) %>%
        formatPercentage(c("Avg_Pct_Change"), 2) %>%
        formatCurrency(c("Avg_Change"), currency = "", interval = 3, mark = ",", digits = 2)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
