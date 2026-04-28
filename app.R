#SECTION A===============================================================================================
library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(stringr)
library(leaflet)
library(jsonlite)
library(shinycssloaders)

# ============================================================
#  CONTINENT CLASSIFIER (7 continents, Hawaii excluded)
# ============================================================
continent_from_latlon = function(lat, lon) {
  case_when(
    !is.na(lat) & lat <= -60 ~ "Antarctica",
    
    # North America (Hawaii excluded)
    !is.na(lat) & !is.na(lon) &
      lat >= 5  & lat <= 83 &
      lon >= -170 & lon <= -50 &
      !(lat >= 18 & lat <= 23 & lon >= -161 & lon <= -154) ~ "North America",
    
    !is.na(lat) & !is.na(lon) &
      lat >= -56 & lat <= 12 &
      lon >= -82 & lon <= -34 ~ "South America",
    
    !is.na(lat) & !is.na(lon) &
      lat >= 35 & lat <= 71 &
      lon >= -25 & lon <= 45 ~ "Europe",
    
    !is.na(lat) & !is.na(lon) &
      lat >= -35 & lat <= 37 &
      lon >= -20 & lon <= 52 ~ "Africa",
    
    !is.na(lat) & !is.na(lon) &
      lat >= 1 & lat <= 77 &
      lon >= 26 & lon <= 180 ~ "Asia",
    
    !is.na(lat) & !is.na(lon) &
      lat >= -50 & lat <= 0 &
      lon >= 110 & lon <= 180 ~ "Oceania",
    
    TRUE ~ NA_character_
  )
}

# ============================================================
#  JS: LIST CLICK + SCROLL HANDLER
# ============================================================
js = "
$(document).on('click', '.hz-item', function() {
  var ptId = $(this).data('pt-id');
  Shiny.setInputValue('hz_list_click', ptId, {priority: 'event'});
});

Shiny.addCustomMessageHandler('scrollToHzItem', function(message) {
  var el = document.getElementById(message.id);
  if (!el) return;

  var container = $('#hz-list-container');
  if (container.length) {
    var elementOffset = $(el).offset().top;
    var containerOffset = container.offset().top;
    var scrollPos = container.scrollTop() + (elementOffset - containerOffset) - 20;
    container.animate({ scrollTop: scrollPos }, 200);
  } else {
    el.scrollIntoView({ behavior: 'smooth', block: 'center' });
  }
});
"

# ============================================================
#  DATABASE CONNECTION (Local Version)
# ============================================================
con = dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("HZ_DB_NAME"),
  host     = Sys.getenv("HZ_DB_HOST"),
  port     = Sys.getenv("HZ_DB_PORT"),
  user     = Sys.getenv("HZ_DB_USER"),
  password = Sys.getenv("HZ_DB_PASS")
)

onStop(function() dbDisconnect(con))

# ============================================================
#  LOAD + CLEAN DATA
# ============================================================
# Using your original local table name
hz = dbReadTable(con, "hybrid_zone_final") %>%
  mutate(
    id    = row_number(),
    pt_id = paste0("pt_", id),
    across(
      .cols = c(species1_name, species2_name, species1_common_name, 
                species2_common_name, taxon_category, habitat_type),
      .fns  = ~ str_squish(.x)
    ),
    species1_clean        = tolower(species1_name),
    species2_clean        = tolower(species2_name),
    species1_common_clean = tolower(species1_common_name),
    species2_common_clean = tolower(species2_common_name),
    
    taxon_category_clean  = tolower(taxon_category),
    habitat_type          = str_squish(habitat_type),
    continent             = continent_from_latlon(latitude, longitude)
  ) %>%
  rename(
    new_pheno_geno_cline_center = pheno_geno_cline_center,
    new_genomic_cline_center    = genomic_cline_center,
    new_mt_cline_center         = mt_cline_center,
    new_n_cline_center          = n_cline_center,
    new_pheno_cline_center      = pheno_cline_center,
    
    new_pheno_geno_cline_width  = pheno_geno_cline_width,
    new_genomic_cline_width     = genomic_cline_width,
    new_mt_cline_width          = mt_cline_width,
    new_n_cline_width           = n_cline_width,
    new_pheno_cline_width       = pheno_cline_width,
    
    new_mt_width_uncertainty_lower_bound    = mt_width_uncertainty_lower_bound,
    new_mt_width_uncertainty_upper_bound    = mt_width_uncertainty_upper_bound,
    new_nuc_width_uncertainty_lower_bound   = nuc_width_uncertainty_lower_bound,
    new_nuc_width_uncertainty_upper_bound   = nuc_width_uncertainty_upper_bound,
    new_pheno_width_uncertainty_lower_bound = pheno_width_uncertainty_lower_bound,
    new_pheno_width_uncertainty_upper_bound = pheno_width_uncertainty_upper_bound
  )

# ============================================================
#  COLOR PALETTE & AUTOCOMPLETE LIST
# ============================================================
safe_palette = c(
  "#E69F00", "#56B4E9", "#D55E00",
  "#CC79A7", "#F0E442", "#A6761D"
)

# 1. Get the unique lowercase values
taxa = sort(unique(hz$taxon_category_clean))
taxa = taxa[taxa != "" & !is.na(taxa) & taxa != "na"]

# 2. Create the display names (Title Case)
taxa_display_names = tools::toTitleCase(taxa)

# 3. Create a named vector: names = What User Sees, values = What Server Uses
taxa_choices = setNames(taxa, taxa_display_names)

# 4. Keep your palette logic using the lowercase values
pal_colors = setNames(safe_palette[seq_along(taxa)], taxa)
pal = colorFactor(unname(pal_colors), taxa)

# Create a master list of all names (Scientific and Common) for search
all_species = sort(unique(c(
  hz$species1_name, hz$species2_name, 
  hz$species1_common_name, hz$species2_common_name
)))
# Remove NAs or empty strings from the search list
all_species = all_species[all_species != "" & !is.na(all_species)]

#SECTION B===========================================================================================================
ui = fluidPage(
  
  tags$head(
    tags$style(HTML("
      .with-spinner > div {
        padding-bottom: 0 !important;
      }

      #map-summary {
        position: absolute;
        bottom: 5px;
        left: 50%;
        transform: translateX(-50%);
        background: none;
        padding: 0;
        font-size: 14px;
        text-align: center;
        white-space: pre-line;
        pointer-events: none;
        z-index: 9999;
        line-height: 1.25;
        border: none;
      }

      /* Force all sidebar inputs to take full width */
      .sidebar-inputs .shiny-input-container {
        width: 100% !important;
      }
      .sidebar-inputs .selectize-control {
        width: 100% !important;
      }

      .hz-item {
        padding: 6px 8px;
        margin-bottom: 4px;
        border-radius: 4px;
        cursor: pointer;
        border: 1px solid #ddd;
        background-color: #ffffff;
        font-size: 0.9em;
      }
      .hz-item:hover {
        background-color: #f5f7fb;
      }
      .hz-item-selected {
        background-color: #f0f4ff !important;
        border-color: #4a90e2 !important;
      }
      .hz-item-title {
        font-weight: 600;
        margin-bottom: 2px;
      }
      .hz-item-meta {
        font-size: 0.85em;
      }
      
      .equal-height-row {
        display: flex;
        flex-wrap: wrap;
      }
    "))
  ),
  
  titlePanel("Hybrid Zone Explorer"),
  
  fluidRow(
    class = "equal-height-row",
    
    column(
      3,
      div(
        class = "sidebar-inputs", # Added class for the CSS rule above
        style = "
          background-color: #f5f5f5;
          padding: 15px;
          border-radius: 8px;
          border: 1px solid #ddd;
          box-shadow: 0 2px 5px rgba(0,0,0,0.05);
          height: 935px;
          display: flex;
          flex-direction: column;
        ",
        
        tags$div(
          style = "text-align:center; margin-bottom:20px;",
          tags$img(src = "lab.logo.png", height = "80px")
        ),
        
        textInput(
          "species_text",
          "Search species:",
          value = "",
          placeholder = "Type a species name...",
          width = "100%" # Set width here
        ),
        
        tags$script(HTML(sprintf("
          $(function() {
            var speciesList = %s;
            $('#species_text').autocomplete({
              source: speciesList,
              minLength: 1
            });
          });
        ", jsonlite::toJSON(all_species)))),
        
        selectInput("taxon_filter", "Filter by Taxon Category:", 
                    choices = c("All" = "All", taxa_choices), 
                    width = "100%"),
        
        selectInput("habitat_filter", "Habitat type:", choices = c("All", "Terrestrial", "Aquatic"), width = "100%"),
        
        selectInput(
          "continent_filter",
          "Continent:",
          choices = c(
            "All",
            "Africa","Antarctica","Asia","Europe",
            "North America","Oceania","South America",
            "None / Open Water"
          ),
          width = "100%"
        ),
        
        actionButton("reset", "Reset filters", width = "100%"), # Matched width for symmetry
        
        # Inside your sidebarPanel, after continent_filter:
        tags$hr(),
        downloadButton("download_filtered_data", "Download Filtered Data (.csv)", 
                       style = "width: 100%; background-color: #2c3e50; color: white; border: none;"),
        
        h4("Matching Hybrid Zones"),
        
        tags$div(
          id = "hz-list-container",
          style = "
            flex-grow: 1; 
            overflow-y: auto; 
            background-color: #f5f5f5; 
            padding: 10px; 
            border-radius: 6px;
          ",
          uiOutput("results_list")
        )
      )
    ),
    
    column(
      9,
      div(
        style = "position: relative;",
        
        withSpinner(
          div(
            style = "position: relative;",
            leafletOutput("map", height = "935px"),
            div(id = "map-summary", textOutput("map_summary"))
          )
        )
      )
    )
  ),
  
  fluidRow(
    column(
      12,
      
      div(
        style = "
          margin-top: 15px;
          padding: 10px;
          border: 1px solid #ddd;
          border-radius: 6px;
          background: #fafafa;
        ",
        
        h3("Selected Hybrid Zone Details"),
        
        uiOutput("details_panel"),
        
        br(),
        
        plotOutput("cline_plot", height = "400px")
      )
    )
  ),
  
  tags$script(HTML(js))
)
#SECTION C=========================================================================================================
server = function(input, output, session) {
  
  format_citations = function(citation_str, doi_str) {
    # If the citation is NA, empty, or literally "na", return nothing
    if (is.na(citation_str) || !nzchar(citation_str) || citation_str == "na") {
      return(tags$span("")) 
    }
    
    cites = str_split(citation_str, fixed("|"))[[1]] %>% str_trim()
    dois  = if(!is.na(doi_str)) str_split(doi_str, fixed("|"))[[1]] %>% str_trim() else character(0)
    
    tagList(
      lapply(seq_along(cites), function(i) {
        has_doi = i <= length(dois) && !is.na(dois[i]) && nzchar(dois[i]) && dois[i] != "na"
        
        tagList(
          if (has_doi) {
            tags$a(href = dois[i], target = "_blank", cites[i])
          } else {
            cites[i]
          },
          if (i < length(cites)) " | " else ""
        )
      })
    )
  }
  
  selected_row = reactiveVal(NULL)
  
  observeEvent(input$reset, {
    updateTextInput(session, "species_text", value = "")
    updateSelectInput(session, "taxon_filter", selected = "All")
    updateSelectInput(session, "habitat_filter", selected = "All")
    updateSelectInput(session, "continent_filter", selected = "All")
    selected_row(NULL)
  })
  
  filtered_data = reactive({
    data = hz
    
    # Expanded search logic to include common names
    if (input$species_text != "") {
      sp = tolower(str_squish(input$species_text))
      data = data %>%
        filter(
          str_detect(species1_clean, fixed(sp)) |
            str_detect(species2_clean, fixed(sp)) |
            str_detect(species1_common_clean, fixed(sp)) |
            str_detect(species2_common_clean, fixed(sp))
        )
    }
    
    # Taxon Category Filter
    if (input$taxon_filter != "All") {
      data = data %>% filter(taxon_category_clean == input$taxon_filter)
    }
    
    # Habitat Filter
    if (input$habitat_filter != "All") {
      data = data %>% filter(habitat_type == input$habitat_filter)
    }
    
    # Continent Filter
    if (input$continent_filter == "None / Open Water") {
      data = data %>% filter(is.na(continent))
    } else if (input$continent_filter != "All") {
      data = data %>% filter(continent == input$continent_filter)
    }
    
    data
  })
  
  visible_data = reactive({
    data = filtered_data()
    bounds = input$map_bounds
    if (is.null(bounds)) return(data)
    
    data %>%
      filter(
        latitude  <= bounds$north,
        latitude  >= bounds$south,
        longitude <= bounds$east,
        longitude >= bounds$west
      )
  })
  
  output$map_summary = renderText({
    data = visible_data()
    if (nrow(data) == 0) return("No hybrid zones visible")
    
    line1 = paste0(nrow(data), " hybrid zones visible")
    
    tax_counts = data %>%
      count(taxon_category_clean) %>%
      arrange(taxon_category_clean)
    
    line2 = paste(
      paste0(
        tax_counts$n, " ",
        tools::toTitleCase(tax_counts$taxon_category_clean)
      ),
      collapse = " • "
    )
    
    paste0(line1, "\n", line2)
  })
  
  output$map = renderLeaflet({
    leaflet(options = leafletOptions(
      worldCopyJump = FALSE,
      preferCanvas  = TRUE,
      minZoom       = 2,
      maxBounds     = list(c(-85, -180), c(85, 180))
    )) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "CartoDB", "Satellite"),
        options    = layersControlOptions(collapsed = FALSE)
      )
  })
  
  highlight_point = function(row) {
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addCircleMarkers(
        lng = row$longitude,
        lat = row$latitude,
        radius = 10,
        color  = "black",
        weight = 4,
        fillOpacity = 0,
        opacity = 1,
        group = "highlight"
      ) %>%
      addCircleMarkers(
        lng = row$longitude,
        lat = row$latitude,
        radius = 8,
        color  = pal(row$taxon_category_clean),
        fillColor = pal(row$taxon_category_clean),
        fillOpacity = 1,
        weight = 1,
        opacity = 1,
        group = "highlight"
      )
  }
  
  observe({
    data = filtered_data()
    
    proxy = leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      clearControls() %>%
      clearGroup("highlight")
    
    if (nrow(data) > 0) {
      proxy %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = 6,
          color  = ~pal(taxon_category_clean),
          fillOpacity = 0.8,
          layerId = ~pt_id,
          # 1. Capitalize the first letter of each species name
          label = ~paste0("<i>", tools::toTitleCase(species1_name), "</i> × <i>", tools::toTitleCase(species2_name), "</i>") %>% 
            lapply(htmltools::HTML)
        ) %>%
        addLegend(
          "bottomright",
          colors = unname(pal_colors),
          # 2. Capitalize the first letter of the legend labels
          labels = tools::toTitleCase(names(pal_colors)),
          title  = "Taxon Category",
          opacity = 1
        )
    }
  })
  
  
  output$results_list = renderUI({
    data = filtered_data()
    sel  = selected_row()
    sel_id = if (is.null(sel)) NA_integer_ else sel$id
    
    if (nrow(data) == 0) {
      return(tags$em("No hybrid zones match the current filters."))
    }
    
    lapply(seq_len(nrow(data)), function(i) {
      row = data[i, ]
      
      item_id = paste0("hz_item_", row$id)
      classes = "hz-item"
      if (!is.na(sel_id) && row$id == sel_id) {
        classes = paste(classes, "hz-item-selected")
      }
      
      meta_line = if (!is.na(row$continent)) {
        paste(row$taxon_category, "•", row$continent, "•", row$habitat_type)
      } else {
        paste(row$taxon_category, "•", row$habitat_type)
      }
      
      tags$div(
        id = item_id,
        class = classes,
        `data-pt-id` = row$pt_id,
        `data-row-id` = row$id,
        # Updated Title Section: Common name bold, Scientific name smaller/italics
        tags$div(
          class = "hz-item-title",
          tags$span(style = "display: block;", paste(row$species1_common_name, "×", row$species2_common_name)),
          tags$small(style = "color: #666; font-style: italic;",
                     paste0("(", row$species1_name, " × ", row$species2_name, ")")
          )
        ),
        tags$div(class = "hz-item-meta", meta_line)
      )
    })
  })
  
  observeEvent(input$hz_list_click, {
    pt_id = input$hz_list_click
    if (is.null(pt_id)) return()
    
    data = filtered_data()
    row  = data[data$pt_id == pt_id, ]
    if (nrow(row) == 0) return()
    row = row[1, ]
    
    selected_row(row)
    highlight_point(row)
    
    leafletProxy("map") %>%
      setView(
        lng = row$longitude,
        lat = row$latitude,
        zoom = 5
      )
  })
  
  observeEvent(input$map_marker_click, {
    click = input$map_marker_click
    if (is.null(click)) return()
    
    data = filtered_data()
    row  = data[data$pt_id == click$id, ]
    if (nrow(row) == 0) return()
    row = row[1, ]
    
    selected_row(row)
    highlight_point(row)
    
    leafletProxy("map") %>%
      setView(
        lng = row$longitude,
        lat = row$latitude,
        zoom = 5
      )
    
    session$sendCustomMessage(
      "scrollToHzItem",
      list(id = paste0("hz_item_", row$id))
    )
  })
  
  output$details_panel = renderUI({
    row = selected_row()
    
    if (is.null(row)) {
      return(tags$em("Click a hybrid zone in the list or on the map to see details."))
    }
    
    cont = ifelse(is.na(row$continent), "None / Open Water", row$continent)
    bg_color = paste0(pal(row$taxon_category_clean), "20")
    
    div(
      style = paste0(
        "background-color:", bg_color, ";
         padding:16px;
         border-radius:10px;"
      ),
      
      # --- Header Section ---
      tags$h2(
        style = "margin-top: 0; margin-bottom: 5px;",
        tags$strong(paste(row$species1_common_name, "×", row$species2_common_name))
      ),
      tags$h4(
        style = "margin-top: 0; color: #555; font-style: italic;",
        paste0("(", row$species1_name, " × ", row$species2_name, ")")
      ),
      
      # --- Hybrid Zone Info ---
      div(
        style = "border:1px solid #ddd; border-radius:6px; padding:12px; margin-bottom:12px; background: white;",
        tags$h4(tags$strong("Hybrid Zone Information")),
        tags$hr(),
        div(
          style = "display:grid; grid-template-columns: repeat(auto-fit,minmax(220px,1fr)); gap:10px;",
          div(strong("Taxon:"), br(), row$taxon_category),
          div(strong("Habitat:"), br(), row$habitat_type),
          div(strong("Latitude:"), br(), row$latitude),
          div(strong("Longitude:"), br(), row$longitude),
          div(strong("Continent:"), br(), cont),
          div(strong("Coordinates Citation:"), br(), 
              format_citations(row$coordinates_citation, row$coordinates_doi)
          ),
          if (!is.na(row$notes) && nzchar(row$notes) && row$notes != "na") {
            div(strong("Notes:"), br(), row$notes)
          }
        )
      ),
      
      # --- Cline Centers ---
      div(
        style = "border:1px solid #ddd; border-radius:6px; padding:12px; margin-bottom:12px; background: white;",
        tags$h4(tags$strong("Cline Centers")),
        tags$small("All values in km"),
        tags$hr(),
        div(
          style = "display:grid; grid-template-columns: repeat(auto-fit,minmax(220px,1fr)); gap:10px;",
          div(strong("Pheno-Genomic:"), br(), row$new_pheno_geno_cline_center),
          div(strong("Genomic:"), br(), row$new_genomic_cline_center),
          div(strong("mtDNA:"), br(), row$new_mt_cline_center),
          div(strong("nDNA:"), br(), row$new_n_cline_center),
          div(strong("Phenotype:"), br(), row$new_pheno_cline_center),
          div(strong("Citation:"), br(),
              format_citations(row$center_citation, row$center_doi)
          )
        )
      ),
      
      # --- Cline Widths ---
      div(
        style = "border:1px solid #ddd; border-radius:6px; padding:12px; background: white;",
        tags$h4(tags$strong("Cline Widths")),
        tags$small("All values in km"),
        tags$hr(),
        div(
          style = "display:grid; grid-template-columns: repeat(auto-fit,minmax(220px,1fr)); gap:10px;",
          div(strong("Pheno-Genomic:"), br(), row$new_pheno_geno_cline_width),
          div(strong("Genomic:"), br(), row$new_genomic_cline_width),
          div(strong("mtDNA:"), br(), row$new_mt_cline_width),
          div(strong("nDNA:"), br(), row$new_n_cline_width),
          div(strong("Phenotype:"), br(), row$new_pheno_cline_width),
          
          # Logic to hide Interval if data is missing
          div(strong("mtDNA 95% CI:"), br(), 
              if(!is.na(row$new_mt_width_uncertainty_lower_bound)) paste0(row$new_mt_width_uncertainty_lower_bound," – ", row$new_mt_width_uncertainty_upper_bound) else "NA"),
          
          div(strong("nDNA 95% CI:"), br(), 
              if(!is.na(row$new_nuc_width_uncertainty_lower_bound)) paste0(row$new_nuc_width_uncertainty_lower_bound," – ", row$new_nuc_width_uncertainty_upper_bound) else "NA"),
          
          div(strong("Phenotype 95% CI:"), br(), 
              if(!is.na(row$new_pheno_width_uncertainty_lower_bound)) paste0(row$new_pheno_width_uncertainty_lower_bound," – ", row$new_pheno_width_uncertainty_upper_bound) else "NA"),
          
          div(strong("Citation:"), br(),
              format_citations(row$width_citation, row$width_doi)
          )
        )
      ),
      
      # --- Generation Time & Dispersal ---
      div(
        style = "border:1px solid #ddd; border-radius:6px; padding:12px; margin-top:12px; background: white;",
        tags$h4(tags$strong("Generation Time & Dispersal")),
        tags$hr(),
        div(
          style = "display:grid; grid-template-columns: repeat(auto-fit,minmax(220px,1fr)); gap:10px;",
          div(strong("Generation Time (Years):"), br(), row$generation_time),
          div(strong("Generation Time Citation:"), br(),
              format_citations(row$generation_time_citation, row$generation_time_doi)
          ),
          div(strong("Dispersal (km/√Gen):"), br(), row$dispersal_km_sqrtgen),
          div(strong("Male Dispersal (km/√Gen):"), br(), row$male_dispersal_km_sqrtgen),
          div(strong("Female Dispersal (km/√Gen):"), br(), row$female_dispersal_km_sqrtgen),
          div(strong("Dispersal Citation:"), br(),
              format_citations(row$dispersal_citation, row$dispersal_doi)
          ),
          div(strong("Notes:"), br(), 
              if(!is.na(row$gen_time_and_dispersal_note) && row$gen_time_and_dispersal_note != "na") row$gen_time_and_dispersal_note else "")
        )
      )
    )
  })
  
  output$cline_plot = renderPlot({
    
    row = selected_row()
    if (is.null(row)) return(NULL)
    
    centers = c(
      row$new_pheno_geno_cline_center,
      row$new_genomic_cline_center,
      row$new_mt_cline_center,
      row$new_n_cline_center,
      row$new_pheno_cline_center
    )
    
    widths = c(
      row$new_pheno_geno_cline_width,
      row$new_genomic_cline_width,
      row$new_mt_cline_width,
      row$new_n_cline_width,
      row$new_pheno_cline_width
    )
    
    width_lower = c(
      NA,  # Pheno-Genomic (no CI available)
      NA,  # Genomic (no CI available)
      row$new_mt_width_uncertainty_lower_bound,
      row$new_nuc_width_uncertainty_lower_bound,
      row$new_pheno_width_uncertainty_lower_bound
    )
    
    width_upper = c(
      NA,  # Pheno-Genomic
      NA,  # Genomic
      row$new_mt_width_uncertainty_upper_bound,
      row$new_nuc_width_uncertainty_upper_bound,
      row$new_pheno_width_uncertainty_upper_bound
    )
    
    labels = c(
      "Pheno-Genomic",
      "Genomic",
      "mtDNA",
      "nDNA",
      "Phenotype"
    )
    
    cols = c(
      "#0072B2",  # blue
      "#E69F00",  # orange
      "#009E73",  # bluish green
      "#CC79A7",  # purple/pink
      "#56B4E9"   # light blue
    )
    
    valid = !(is.na(centers) | is.na(widths))
    
    if (sum(valid) == 0) {
      plot.new()
      text(0.5,0.5,"No cline available for this hybrid zone",cex=1.4)
      return()
    }
    
    xmin = min(centers[valid]-3*widths[valid], na.rm=TRUE)
    xmax = max(centers[valid]+3*widths[valid], na.rm=TRUE)
    
    # --- Flat line ends ---
    par(lend = 0)
    
    plot(
      NA,
      xlim = c(xmin, xmax),
      ylim = c(0, 1),
      xlab = "Transect distance (km, relative units)",
      ylab = "Trait / allele frequency",
      main = "Hybrid Zone Clines",
      cex.lab = 1.4,
      cex.axis = 1.2,
      cex.main = 1.6,
      xaxs = "i",  # remove extra padding on x-axis
      yaxs = "i",  # remove extra padding on y-axis
      yaxt = "n"   # suppress automatic y-axis
    )
    
    # Custom y-axis with all ticks
    axis(
      side = 2,
      at = seq(0, 1, 0.2),
      labels = seq(0, 1, 0.2),
      las = 1,       # horizontal text for y-axis
      cex.axis = 1.2,
      tck = -0.02    # small tick marks
    )
    
    offset_step = 0.06
    offset_index = 0
    
    for (i in seq_along(centers)) {
      
      c = centers[i]
      w = widths[i]
      
      if (is.na(c) || is.na(w)) next
      
      x = seq(xmin, xmax, length.out = 200)
      x_centered = x - mean(centers[valid])
      y = 1/(1 + exp(-4*(x_centered - (c - mean(centers[valid])))/w))
      
      # --- Shaded 95% CI using your bounds ---
      w_lower = width_lower[i]   # e.g., new_mt_width_uncertainty_lower_bound[i]
      w_upper = width_upper[i]   # e.g., new_mt_width_uncertainty_upper_bound[i]
      
      if (!is.na(w_lower) && !is.na(w_upper)) {
        ci_upper = 1/(1 + exp(-4*(x - c)/w_lower))
        ci_lower = 1/(1 + exp(-4*(x - c)/w_upper))
        polygon(
          c(x, rev(x)),
          c(ci_upper, rev(ci_lower)),
          col = adjustcolor(cols[i], alpha.f = 0.2),
          border = NA
        )
      }
      
      # --- Thick cline line ---
      lines(x, y, lwd = 7.5, col = cols[i])
      
      # --- Center vertical line (dotted, rectangular ends) ---
      dash_len = 0.05
      gap_len  = 0.04
      y_vals = seq(0, 1, by = dash_len + gap_len)
      for (y_start in y_vals) {
        y_end = min(y_start + dash_len, 1)
        segments(
          x0 = c, x1 = c,
          y0 = y_start, y1 = y_end,
          col = adjustcolor(cols[i], alpha.f = 0.6),
          lwd = 7.5,
          lend = 1
        )
      }
      
      # --- Width line (offset to avoid overlap) ---
      y_offset = 0.5 + (offset_index - (sum(valid)-1)/2) * offset_step
      segments(
        x0 = c - w/2, x1 = c + w/2,
        y0 = y_offset, y1 = y_offset,
        col = adjustcolor(cols[i], alpha.f = 0.7),
        lwd = 7.5,
        lend = 1
      )
      
      # --- End caps for width ---
      cap_height = 0.03
      segments(
        x0 = c - w/2, x1 = c - w/2,
        y0 = y_offset - cap_height/2,
        y1 = y_offset + cap_height/2,
        col = adjustcolor(cols[i], alpha.f = 0.7),
        lwd = 7.5, lend = 1
      )
      segments(
        x0 = c + w/2, x1 = c + w/2,
        y0 = y_offset - cap_height/2,
        y1 = y_offset + cap_height/2,
        col = adjustcolor(cols[i], alpha.f = 0.7),
        lwd = 7.5, lend = 1
      )
      
      offset_index = offset_index + 1
    }
    
    legend(
      "bottomright",
      legend = labels[valid],
      col = cols[valid],
      lty = 1,
      lwd = 10,       
      cex = 1.5,      
      pt.cex = 2,     
      y.intersp = 1.3, 
      x.intersp = 1.2, 
      bty = "n"
    )
  })
  
  # ============================================================
  #  DOWNLOAD HANDLER (With Dynamic Filename)
  # ============================================================
  output$download_filtered_data = downloadHandler(
    filename = function() {
      # 1. Collect filter values, replacing "All" with a blank or descriptive term
      taxon_tag    = if(input$taxon_filter == "All") "AllTaxa" else input$taxon_filter
      habitat_tag  = if(input$habitat_filter == "All") "AllHabitats" else input$habitat_filter
      region_tag   = if(input$continent_filter == "All") "Global" else input$continent_filter
      
      # 2. Clean up names (remove spaces/special chars) to keep the filename tidy
      # e.g., "North America" becomes "NorthAmerica"
      clean_tag = function(x) gsub("[^[:alnum:]]", "", x)
      
      # 3. Construct the filename
      paste0(
        "HZ_Export_", 
        clean_tag(taxon_tag), "_", 
        clean_tag(habitat_tag), "_", 
        clean_tag(region_tag), "_",
        Sys.Date(), 
        ".csv"
      )
    },
    content = function(file) {
      export_data = filtered_data()
      
      # Clean up internal columns and revert the "new_" prefix
      export_data = export_data %>%
        select(-id, -pt_id, -continent, -ends_with("_clean")) %>%
        rename_with(~ str_remove(., "^new_"), starts_with("new_"))
      
      write.csv(export_data, file, row.names = FALSE, na = "na")
    }
  )
  
}

shinyApp(ui, server)