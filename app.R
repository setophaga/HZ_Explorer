# SECTION A ===============================================================================================
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
#  DATABASE CONNECTION (Railway / Environment Variables)
# ============================================================
con = dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("PGDATABASE"),
  host     = Sys.getenv("PGHOST"),
  port     = as.numeric(Sys.getenv("PGPORT")),
  user     = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)

# ============================================================
#  LOAD + CLEAN DATA
# ============================================================
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
# Taxonomic order: Amphibian, Bird, Fish, Invert, Mammal, Reptile
safe_palette = c("#CC6677", "#44AA99", "#88CCEE","#332288","#F0E442","#E69F00" )

taxa = sort(unique(hz$taxon_category_clean))
taxa = taxa[taxa != "" & !is.na(taxa) & taxa != "na"]
taxa_display_names = tools::toTitleCase(taxa)
taxa_choices = setNames(taxa, taxa_display_names)
pal_colors = setNames(safe_palette[seq_along(taxa)], taxa)
pal = colorFactor(unname(pal_colors), taxa)

all_species = sort(unique(c(
  hz$species1_name, hz$species2_name, 
  hz$species1_common_name, hz$species2_common_name
)))
all_species = all_species[all_species != "" & !is.na(all_species)]

# SECTION B ===========================================================================================================
ui = fluidPage(
  tags$head(
    tags$style(HTML("
      /* Firefox scrollbar support */
      html {
        scrollbar-color: #ffd700 #23433a;
        scrollbar-width: auto;
      }

      /* Webkit Custom Gold Scrollbar for main page */
      ::-webkit-scrollbar {
        width: 12px;
      }
      ::-webkit-scrollbar-track {
        background: #23433a; 
      }
      ::-webkit-scrollbar-thumb {
        background-color: #ffd700;
        border-radius: 6px;
        border: 2px solid #23433a;
      }
      ::-webkit-scrollbar-thumb:hover {
        background-color: #e6c200;
      }

      /* Existing Styles */
      .with-spinner > div { padding-bottom: 0 !important; }
      
      #map-summary {
        position: absolute; 
        bottom: 10px;       
        left: 50%; 
        transform: translateX(-50%); 
        background: none; 
        font-size: 14px; 
        text-align: center; 
        white-space: pre-line;
        pointer-events: none; 
        z-index: 9999; 
        line-height: 1.25; 
        border: none;
        color: #000000;      
        width: 100%;        
      }
      
      .sidebar-inputs .shiny-input-container, .sidebar-inputs .selectize-control { width: 100% !important; }
      .hz-item { padding: 6px 8px; margin-bottom: 4px; border-radius: 4px; cursor: pointer; border: 1px solid #ddd; background-color: #ffffff; font-size: 0.9em; }
      .hz-item:hover { background-color: #f5f7fb; }
      .hz-item-selected { background-color: #f0f4ff !important; border-color: #4a90e2 !important; }
      .hz-item-title { font-weight: 600; margin-bottom: 2px; }
      .hz-item-meta { font-size: 0.85em; }
      .equal-height-row { display: flex; flex-wrap: wrap; }

      /* Legend Style: Scaled to 80% and moved to Top Left via transform-origin */
      .leaflet .legend {
        transform: scale(0.8);
        transform-origin: top left;
        border: 2px solid #000000 !important;
        border-radius: 5px;
        padding: 10px;
        background: rgba(255, 255, 255, 0.9) !important;
        box-shadow: 0 0 15px rgba(0,0,0,0.2);
        color: black;
      }
      
      /* Legend Color Swatches: Added 1px black border */
      .leaflet .legend i {
        border: 1px solid #000000;
        width: 14px;
        height: 14px;
        opacity: 1.0 !important;
      }
    "))
  ),
  
  div(id = "login_page",
      style = "max-width: 450px; margin: 100px auto; padding: 20px;",
      wellPanel(
        style = "border: 2px solid #000000; border-radius: 8px; background-color: #f5f5f5;", 
        h3("Hybrid Zone Explorer Access", style = "text-align: center; font-weight: bold;"),
        hr(style = "border-top: 1px solid #000000;"), 
        passwordInput("password_input", "Enter Lab Password:", placeholder = "Required for database access"),
        actionButton("login_btn", "Log In", class = "btn-primary", style = "width: 100%;"),
        
        tags$script(HTML("
  $(document).on('shiny:visualchange', function(event) {
    $('#password_input').focus();
    
    $('#password_input').on('keypress', function(e) {
      if (e.which === 13) {
        e.preventDefault();
        $(this).trigger('change');
        setTimeout(function() {
          $('#login_btn').click();
        }, 50);
      }
    });
  });
"))
      ),
  ),
  
  uiOutput("secure_ui")
)

# SECTION C =========================================================================================================
server = function(input, output, session) {
  
  # 1. AUTHENTICATION SETUP ---------------------------------------------------
  INTERNAL_PASS <- Sys.getenv("LAB_PASSWORD", unset = "admin")
  auth <- reactiveVal(FALSE)
  
  observeEvent(input$login_btn, {
    if (input$password_input == INTERNAL_PASS) {
      auth(TRUE)
      removeUI(selector = "#login_page") 
    } else {
      showNotification("Incorrect Password. Check with the lab manager.", type = "error")
    }
  })
  
  # 2. SECURE UI WRAPPER ------------------------------------------------------
  output$secure_ui <- renderUI({
    req(auth())
    
    div(style = "background-color: #316053; color: black; min-height: 100vh; width: 100%; margin: 0; padding: 20px; box-sizing: border-box; position: absolute; left: 0; top: 0;",
        tagList(
          titlePanel(h2("Hybrid Zone Explorer", style = "color: #89D9B2; margin-top: 0; font-weight: bold;")),
          
          fluidRow(
            class = "equal-height-row",
            # --- 1. LEFT COLUMN ---
            column(3,
                   div(class = "sidebar-inputs", 
                       # Height set to 500px to reveal lower panel
                       style = "background-color: #f5f5f5; padding: 15px; border-radius: 8px; border: 2px solid #000000; box-shadow: 0 2px 5px rgba(0,0,0,0.05); height: 500px; display: flex; flex-direction: column; color: black;",
                       
                       tags$div(style = "text-align:center; margin-bottom:15px;",
                                tags$img(src = "lab.logo.png", height = "80px")
                       ),
                       
                       div(style = "margin-bottom: -10px;", 
                           textInput("species_text", "Search species:", value = "", placeholder = "Type a species name...", width = "100%")),
                       
                       tags$script(HTML(sprintf("
                          $(function() {
                            var speciesList = %s;
                            $('#species_text').autocomplete({ source: speciesList, minLength: 1 });
                          });
                        ", jsonlite::toJSON(all_species)))),
                       
                       div(style = "margin-bottom: -10px;",
                           selectInput("taxon_filter", "Filter by Taxon Category:", choices = c("All" = "All", taxa_choices), width = "100%")),
                       
                       div(style = "margin-bottom: 5px;",
                           selectInput("continent_filter", "Continent:", choices = c("All", "Africa","Antarctica","Asia","Europe","North America","Oceania","South America","None / Open Water"), width = "100%")),
                       
                       # DISABLED FOR NOW:
                       # downloadButton("download_filtered_data", "Download Filtered Data (.csv)", 
                       #                style = "width: 100%; background-color: #2c3e50; color: white; border: none; margin-bottom: 10px;"),
                       
                       h4("Matching Hybrid Zones", style = "margin-top: 5px;"),
                       tags$div(id = "hz-list-container", 
                                style = "flex-grow: 1; overflow-y: auto; background-color: #f5f5f5; padding: 10px; border-radius: 6px;",
                                uiOutput("results_list")
                       )
                   )
            ),
            
            # --- 2. RIGHT COLUMN (MAP) ---
            column(9,
                   div(style = "position: relative; border: 2px solid #000000; border-radius: 8px; overflow: hidden; background-color: white;",
                       withSpinner(
                         div(style = "position: relative;",
                             # Height set to 500px to match sidebar container height
                             leafletOutput("map", height = "500px"),
                             div(id = "map-summary", textOutput("map_summary"))
                         )
                       )
                   )
            )
          ),
          
          # --- 3. BOTTOM PANEL ---
          fluidRow(
            column(12,
                   div(style = "margin-top: 15px; padding: 20px; border: 2px solid #000000; border-radius: 8px; background: #fafafa; color: black;",
                       h3("Selected Hybrid Zone Details"),
                       uiOutput("details_panel"),
                       br(),
                       plotOutput("cline_plot", height = "400px")
                   )
            )
          ),
          
          tags$script(HTML(js))
        )
    )
  })
  
  # 3. RESEARCH LOGIC (Nester Functions & Reactives) --------------------------
  
  format_citations = function(citation_str, doi_str) {
    if (is.na(citation_str) || !nzchar(citation_str) || citation_str == "na") return(tags$span("")) 
    cites = str_split(citation_str, fixed("|"))[[1]] %>% str_trim()
    dois  = if(!is.na(doi_str)) str_split(doi_str, fixed("|"))[[1]] %>% str_trim() else character(0)
    tagList(lapply(seq_along(cites), function(i) {
      has_doi = i <= length(dois) && !is.na(dois[i]) && nzchar(dois[i]) && dois[i] != "na"
      tagList(if (has_doi) tags$a(href = dois[i], target = "_blank", cites[i]) else cites[i],
              if (i < length(cites)) " | " else "")
    }))
  }
  
  selected_row = reactiveVal(NULL)
  
  # 1. FILTERED DATA REACTIVE
  filtered_data = reactive({
    req(auth())
    
    data = hz
    
    if (isTruthy(input$species_text)) {
      sp = tolower(str_squish(input$species_text))
      data = data %>% filter(
        str_detect(species1_clean, fixed(sp)) | 
          str_detect(species2_clean, fixed(sp)) | 
          str_detect(species1_common_clean, fixed(sp)) | 
          str_detect(species2_common_clean, fixed(sp))
      )
    }
    
    if (isTruthy(input$taxon_filter)) {
      if (input$taxon_filter != "All") {
        data = data %>% filter(taxon_category_clean == input$taxon_filter)
      }
    }
    
    if (isTruthy(input$continent_filter)) {
      if (input$continent_filter == "None / Open Water") {
        data = data %>% filter(is.na(continent))
      } else if (input$continent_filter != "All") {
        data = data %>% filter(continent == input$continent_filter)
      }
    }
    
    data
  })
  
  # 2. VISIBLE DATA REACTIVE
  visible_data = reactive({
    req(auth())
    data = filtered_data()
    bounds = input$map_bounds
    if (is.null(bounds)) return(data) 
    
    data %>% filter(
      latitude <= bounds$north, 
      latitude >= bounds$south, 
      longitude <= bounds$east, 
      longitude >= bounds$west
    )
  })
  
  # 3. MAP SUMMARY TEXT
  output$map_summary = renderText({
    req(auth())
    req(isTruthy(input$taxon_filter)) 
    
    data = visible_data()
    if (nrow(data) == 0) return("No hybrid zones visible")
    
    line1 = paste0(nrow(data), " hybrid zones visible")
    tax_counts = data %>% count(taxon_category_clean) %>% arrange(taxon_category_clean)
    line2 = paste(paste0(tax_counts$n, " ", tools::toTitleCase(tax_counts$taxon_category_clean)), collapse = " • ")
    paste0(line1, "\n", line2)
  })
  
  # 4. Map Output
  output$map = renderLeaflet({
    req(auth())
    
    leaflet(options = leafletOptions(
      worldCopyJump = FALSE, 
      preferCanvas = TRUE, 
      minZoom = 2, 
      maxBounds = list(c(-85, -180), c(85, 180))
    )) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"), 
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # 5. HIGHLIGHT POINT FUNCTION
  highlight_point = function(row) {
    target_color = pal(row$taxon_category_clean)
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addCircleMarkers(
        lng = row$longitude, 
        lat = row$latitude, 
        radius = 5,
        fillColor = target_color, 
        fillOpacity = 1.0,
        stroke = TRUE, 
        color = "#000000",
        weight = 2.5,
        opacity = 1.0,
        group = "highlight"
      )
  }
  
  # 6. MARKER OBSERVER
  observe({
    req(auth())
    req(input$map_center) 
    
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
          radius = 4,
          fillColor = ~pal(taxon_category_clean), 
          fillOpacity = 1.0, 
          stroke = TRUE, 
          color = "#000000",
          weight = 1.2,
          opacity = 1.0, 
          layerId = ~pt_id,
          label = ~paste0("<i>", tools::toTitleCase(species1_name), "</i> × <i>", tools::toTitleCase(species2_name), "</i>") %>% 
            lapply(htmltools::HTML)
        ) %>%
        addLegend("topleft", 
                  colors = unname(pal_colors), 
                  labels = tools::toTitleCase(names(pal_colors)), 
                  title = "Taxon Category", 
                  opacity = 1)
    }
  })
  
  output$results_list = renderUI({
    req(auth())
    data = filtered_data()
    sel = selected_row()
    sel_id = if (is.null(sel)) NA_integer_ else sel$id
    if (nrow(data) == 0) return(tags$em("No hybrid zones match the current filters."))
    
    lapply(seq_len(nrow(data)), function(i) {
      row = data[i, ]
      item_id = paste0("hz_item_", row$id)
      classes = "hz-item"
      if (!is.na(sel_id) && row$id == sel_id) classes = paste(classes, "hz-item-selected")
      
      meta_line = if (!is.na(row$continent)) {
        paste(row$taxon_category, "•", row$continent)
      } else {
        row$taxon_category
      }
      
      tags$div(id = item_id, class = classes, `data-pt-id` = row$pt_id, `data-row-id` = row$id,
               tags$div(class = "hz-item-title",
                        tags$span(style = "display: block;", paste(row$species1_common_name, "×", row$species2_common_name)),
                        tags$small(style = "color: #666; font-style: italic;", paste0("(", row$species1_name, " × ", row$species2_name, ")"))
               ),
               tags$div(class = "hz-item-meta", meta_line)
      )
    })
  })
  
  observeEvent(input$hz_list_click, {
    req(auth())
    pt_id = input$hz_list_click; data = filtered_data(); row = data[data$pt_id == pt_id, ]
    if (nrow(row) == 0) return(); row = row[1, ]
    selected_row(row); highlight_point(row)
    leafletProxy("map") %>% setView(lng = row$longitude, lat = row$latitude, zoom = 5)
  })
  
  observeEvent(input$map_marker_click, {
    req(auth())
    click = input$map_marker_click; data = filtered_data(); row = data[data$pt_id == click$id, ]
    if (nrow(row) == 0) return(); row = row[1, ]
    selected_row(row); highlight_point(row)
    leafletProxy("map") %>% setView(lng = row$longitude, lat = row$latitude, zoom = 5)
    session$sendCustomMessage("scrollToHzItem", list(id = paste0("hz_item_", row$id)))
  })
  
  get_cc_text <- function(license_url) {
    if (is.na(license_url) || !nzchar(license_url) || license_url == "na") return("Unknown License")
    clean_url <- tolower(gsub(" ", "", license_url))
    
    if (grepl("publicdomain|cc0", clean_url)) {
      return("CC0 / Public Domain")
    } else if (grepl("by-nc-nd", clean_url)) {
      return("CC BY-NC-ND")
    } else if (grepl("by-nc-sa", clean_url)) {
      return("CC BY-NC-SA")
    } else if (grepl("by-nc", clean_url)) {
      return("CC BY-NC")
    } else if (grepl("by-nd", clean_url)) {
      return("CC BY-ND")
    } else if (grepl("by-sa", clean_url)) {
      return("CC BY-SA")
    } else if (grepl("by", clean_url)) {
      return("CC BY")
    }
    
    return("View License")
  }
  
  output$details_panel = renderUI({
    req(auth())
    row = selected_row()
    if (is.null(row)) return(tags$em("Click a hybrid zone in the list or on the map to see details."))
    
    cont = ifelse(is.na(row$continent), "None / Open Water", row$continent)
    bg_color = paste0(pal(row$taxon_category_clean), "20")
    
    render_image_card <- function(title, img_url, user_name, inat_url, license_url, card_id) {
      has_img <- !is.na(img_url) && nzchar(img_url) && img_url != "na"
      
      has_license <- !is.na(license_url) && nzchar(license_url) && license_url != "na"
      display_license_text <- if(has_license) get_cc_text(license_url) else NULL
      
      div(style = "flex: 1; min-width: 200px; text-align: center; border: 1px solid #ddd; border-radius: 8px; padding: 10px; background: #ffffff; display: flex; flex-direction: column; justify-content: space-between;",
          tags$div(
            tags$strong(style = "display: block; margin-bottom: 8px; font-size: 1.1em;", title)
          ),
          
          tags$div(
            id = if(has_img) card_id else NULL,
            style = paste0(
              "height: 180px; display: flex; align-items: center; justify-content: center; margin-bottom: 8px; background-color: #eaeaea; border-radius: 4px; overflow: hidden;",
              if(has_img) "cursor: pointer; transition: transform 0.2s;" else ""
            ),
            onclick = if(has_img) sprintf("Shiny.setInputValue('zoom_image_click', '%s', {priority: 'event'})", img_url) else NULL,
            
            if (has_img) {
              tags$img(src = img_url, style = "max-width: 100%; max-height: 100%; object-fit: contain;")
            } else {
              tags$span(style = "color: #777; font-style: italic;", "No image available")
            }
          ),
          
          tags$div(style = "font-size: 0.9em; min-height: 38px; display: flex; flex-direction: column; justify-content: center;",
                   if (!is.na(user_name) && nzchar(user_name) && user_name != "na") {
                     tagList(
                       tags$div(
                         tags$span(style = "color: #555;", "Image credit: "),
                         if (!is.na(inat_url) && nzchar(inat_url) && inat_url != "na") {
                           tags$a(href = inat_url, target = "_blank", style = "font-weight: 600; text-decoration: underline;", user_name)
                         } else {
                           tags$span(style = "font-weight: 600;", user_name)
                         }
                       ),
                       if (has_license) {
                         tags$div(style = "font-size: 0.8em; margin-top: 2px;",
                                  tags$a(href = license_url, target = "_blank", style = "color: #666; text-decoration: underline;", display_license_text)
                         )
                       }
                     )
                   } else {
                     tags$span(style = "color: #999; font-style: italic;", "No credit metadata")
                   }
          )
      )
    }
    
    div(style = paste0("background-color:", bg_color, "; padding:16px; border-radius:10px;"),
        
        tags$h2(style = "margin-top: 0; margin-bottom: 5px;", tags$strong(paste(row$species1_common_name, "×", row$species2_common_name))),
        tags$h4(style = "margin-top: 0; margin-bottom: 20px; color: #555; font-style: italic;", paste0("(", row$species1_name, " × ", row$species2_name, ")")),
        
        div(style = "display: flex; flex-wrap: wrap; gap: 15px; margin-bottom: 20px;",
            render_image_card(
              title = htmltools::HTML(paste0("<i>", row$species1_name, "</i>")),
              img_url = row$sp1_image_url,
              user_name = row$sp1_image_user,
              inat_url = row$sp1_image_inat_url,
              license_url = row$sp1_image_cc_license,
              card_id = "sp1_card"
            ),
            render_image_card(
              title = htmltools::HTML(paste0("<i>", row$species1_name, "</i> × <i>", row$species2_name, "</i>")),
              img_url = row$hybrid_image_url,
              user_name = row$hybrid_image_credit,
              inat_url = row$hybrid_image_inat_url,
              license_url = row$hybrid_image_cc_license,
              card_id = "hybrid_card"
            ),
            render_image_card(
              title = htmltools::HTML(paste0("<i>", row$species2_name, "</i>")),
              img_url = row$sp2_image_url,
              user_name = row$sp2_image_user,
              inat_url = row$sp2_image_inat_url,
              license_url = row$sp2_image_cc_license,
              card_id = "sp2_card"
            )
        ),
        
        div(style = "border:1px solid #ddd; border-radius:6px; padding:12px; margin-bottom:12px; background: white;",
            tags$h4(tags$strong("Hybrid Zone Information")), tags$hr(),
            div(style = "display:grid; grid-template-columns: repeat(auto-fit,minmax(220px,1fr)); gap:10px;",
                div(strong("Taxon:"), br(), row$taxon_category), div(strong("Habitat:"), br(), row$habitat_type),
                div(strong("Latitude:"), br(), row$latitude), div(strong("Longitude:"), br(), row$longitude),
                div(strong("Continent:"), br(), cont), div(strong("Coordinates Citation:"), br(), format_citations(row$coordinates_citation, row$coordinates_doi)),
                if (!is.na(row$notes) && nzchar(row$notes) && row$notes != "na") div(strong("Notes:"), br(), row$notes))),
        
        div(style = "border:1px solid #ddd; border-radius:6px; padding:12px; margin-bottom:12px; background: white;",
            tags$h4(tags$strong("Cline Centers")), tags$small("All values in km"), tags$hr(),
            div(style = "display:grid; grid-template-columns: repeat(auto-fit,minmax(220px,1fr)); gap:10px;",
                div(strong("Pheno-Genomic:"), br(), row$new_pheno_geno_cline_center), div(strong("Genomic:"), br(), row$new_genomic_cline_center),
                div(strong("mtDNA:"), br(), row$new_mt_cline_center), div(strong("nDNA:"), br(), row$new_n_cline_center),
                div(strong("Phenotype:"), br(), row$new_pheno_cline_center), div(strong("Citation:"), br(), format_citations(row$center_citation, row$center_doi)))),
        
        div(style = "border:1px solid #ddd; border-radius:6px; padding:12px; background: white;",
            tags$h4(tags$strong("Cline Widths")), tags$small("All values in km"), tags$hr(),
            div(style = "display:grid; grid-template-columns: repeat(auto-fit,minmax(220px,1fr)); gap:10px;",
                div(strong("Pheno-Genomic:"), br(), row$new_pheno_geno_cline_width), div(strong("Genomic:"), br(), row$new_genomic_cline_width),
                div(strong("mtDNA:"), br(), row$new_mt_cline_width), div(strong("nDNA:"), br(), row$new_n_cline_width),
                div(strong("Phenotype:"), br(), row$new_pheno_cline_width),
                div(strong("mtDNA 95% CI:"), br(), if(!is.na(row$new_mt_width_uncertainty_lower_bound)) paste0(row$new_mt_width_uncertainty_lower_bound," – ", row$new_mt_width_uncertainty_upper_bound) else "NA"),
                div(strong("nDNA 95% CI:"), br(), if(!is.na(row$new_nuc_width_uncertainty_lower_bound)) paste0(row$new_nuc_width_uncertainty_lower_bound," – ", row$new_nuc_width_uncertainty_upper_bound) else "NA"),
                div(strong("Phenotype 95% CI:"), br(), if(!is.na(row$new_pheno_width_uncertainty_lower_bound)) paste0(row$new_pheno_width_uncertainty_lower_bound," – ", row$new_pheno_width_uncertainty_upper_bound) else "NA"),
                div(strong("Citation:"), br(), format_citations(row$width_citation, row$width_doi)))),
        
        div(style = "border:1px solid #ddd; border-radius:6px; padding:12px; margin-top:12px; background: white;",
            tags$h4(tags$strong("Generation Time & Dispersal")), tags$hr(),
            div(style = "display:grid; grid-template-columns: repeat(auto-fit,minmax(220px,1fr)); gap:10px;",
                div(strong("Generation Time (Years):"), br(), row$generation_time),
                div(strong("Generation Time Citation:"), br(), format_citations(row$generation_time_citation, row$generation_time_doi)),
                div(strong("Dispersal (km/√Gen):"), br(), row$dispersal_km_sqrtgen),
                div(strong("Male Dispersal (km/√Gen):"), br(), row$male_dispersal_km_sqrtgen),
                div(strong("Female Dispersal (km/√Gen):"), br(), row$female_dispersal_km_sqrtgen),
                div(strong("Dispersal Citation:"), br(), format_citations(row$dispersal_citation, row$dispersal_doi)),
                div(strong("Notes:"), br(), if(!is.na(row$gen_time_and_dispersal_note) && row$gen_time_and_dispersal_note != "na") row$gen_time_and_dispersal_note else "")))
    )
  })
  
  observeEvent(input$zoom_image_click, {
    showModal(modalDialog(
      title = NULL,
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l",
      tags$div(style = "text-align: center;",
               tags$img(src = input$zoom_image_click, style = "max-width: 100%; max-height: 75vh; object-fit: contain; border-radius: 4px;")
      )
    ))
  })
  
  output$cline_plot = renderPlot({
    req(auth())
    row = selected_row()
    if (is.null(row)) return(NULL)
    
    centers = c(row$new_pheno_geno_cline_center, row$new_genomic_cline_center, 
                row$new_mt_cline_center, row$new_n_cline_center, row$new_pheno_cline_center)
    widths  = c(row$new_pheno_geno_cline_width, row$new_genomic_cline_width, 
                row$new_mt_cline_width, row$new_n_cline_width, row$new_pheno_cline_width)
    
    width_lower = c(NA, NA, row$new_mt_width_uncertainty_lower_bound, 
                    row$new_nuc_width_uncertainty_lower_bound, row$new_pheno_width_uncertainty_lower_bound)
    width_upper = c(NA, NA, row$new_mt_width_uncertainty_upper_bound, 
                    row$new_nuc_width_uncertainty_upper_bound, row$new_pheno_width_uncertainty_upper_bound)
    
    labels = c("Pheno-Genomic", "Genomic", "mtDNA", "nDNA", "Phenotype")
    cols = c("#117733", "#333333", "#DDCC77", "#88CCEE", "#CC6677")
    
    valid = !(is.na(centers) | is.na(widths))
    
    if (sum(valid) == 0) {
      plot.new(); text(0.5, 0.5, "No cline available for this hybrid zone", cex = 2); return()
    }
    
    xmin = min(centers[valid] - 3 * widths[valid], na.rm = TRUE)
    xmax = max(centers[valid] + 3 * widths[valid], na.rm = TRUE)
    
    par(lend = 0, mar = c(6, 6, 5, 2) + 0.1) 
    
    plot(NA, xlim = c(xmin, xmax), ylim = c(0, 1), 
         xlab = "Transect distance (km, relative units)", 
         ylab = "Trait / allele frequency", 
         main = "Hybrid Zone Clines", 
         cex.lab = 1.8, cex.axis = 1.5, cex.main = 2.2, 
         xaxs = "i", yaxs = "i", yaxt = "n")
    
    mtext("Horizontal bars = estimated width", 
          side = 3, line = 0.5, cex = 1.2, font = 3, col = "gray30")
    
    axis(side = 2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), 
         las = 1, cex.axis = 1.5, tck = -0.02)
    
    offset_step = 0.06
    offset_index = 0
    
    for (i in seq_along(centers)) {
      c = centers[i]; w = widths[i]; if (is.na(c) || is.na(w)) next
      
      x = seq(xmin, xmax, length.out = 200)
      x_centered = x - mean(centers[valid])
      y = 1 / (1 + exp(-4 * (x_centered - (c - mean(centers[valid]))) / w))
      
      w_l = width_lower[i]; w_u = width_upper[i]
      if (!is.na(w_l) && !is.na(w_u)) {
        ci_u = 1 / (1 + exp(-4 * (x - c) / w_l))
        ci_l = 1 / (1 + exp(-4 * (x - c) / w_u))
        polygon(c(x, rev(x)), c(ci_u, rev(ci_l)), 
                col = adjustcolor(cols[i], alpha.f = 0.6), border = NA)
      }
      
      lines(x, y, lwd = 7.5, col = cols[i])
      
      y_off = 0.5 + (offset_index - (sum(valid) - 1) / 2) * offset_step
      segments(x0 = c - w/2, x1 = c + w/2, y0 = y_off, y1 = y_off, 
               col = adjustcolor(cols[i], alpha.f = 0.7), lwd = 7.5, lend = 1)
      
      offset_index = offset_index + 1
    }
    
    shaded_cols = adjustcolor(cols[valid], alpha.f = 0.6)
    
    legend("bottomright", 
           title = "Clines & 95% CIs",
           legend = labels[valid], 
           col = cols[valid],
           fill = shaded_cols,      
           border = cols[valid],    
           lty = 1, lwd = 5, 
           merge = TRUE,            
           cex = 1.4,               
           pt.cex = 5,              
           y.intersp = 1.4,         
           x.intersp = 1.2,         
           bty = "n")               
  })
  
  # DISABLED FOR NOW:
  # output$download_filtered_data = downloadHandler(
  #   filename = function() {
  #     taxon_tag = if(input$taxon_filter == "All") "AllTaxa" else input$taxon_filter
  #     region_tag = if(input$continent_filter == "All") "Global" else input$continent_filter
  #     clean_tag = function(x) gsub("[^[:alnum:]]", "", x)
  #     paste0("HZ_Export_", clean_tag(taxon_tag), "_", clean_tag(region_tag), "_", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     req(auth())
  #     export_data = filtered_data() %>% select(-id, -pt_id, -continent, -ends_with("_clean")) %>% rename_with(~ str_remove(., "^new_"), starts_with("new_"))
  #     write.csv(export_data, file, row.names = FALSE, na = "na")
  #   }
  # )
}

shinyApp(ui, server)