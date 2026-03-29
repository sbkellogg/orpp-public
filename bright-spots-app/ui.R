# =============================================================================
# ui.R — 2025 Attendance Bright Spots Shiny App
#
# Layout: bslib page_sidebar()
#   Sidebar  — all filter controls
#   Main     — tabsetPanel with Map and Table tabs
# =============================================================================

page_sidebar(
  title = tags$span(
    style = "color: white; font-weight: 700;",
    "2025 NC Attendance Bright Spots"
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    primary = NCDPI_NAVY,
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro")
  ),
  # Target only the title bar — leave sidebar and body colours untouched
  tags$style(HTML(paste0(
    ".navbar, .bslib-page-title { background-color: ",
    NCDPI_NAVY,
    " !important; }",
    ".navbar *, .bslib-page-title * { color: white !important; }",
    # Purple colour for the Table tab icon and label
    ".nav-item:nth-child(2) .nav-link, .nav-item:nth-child(2) .nav-link * { color: #922880 !important; }",
    ".nav-item:nth-child(2) .nav-link.active, .nav-item:nth-child(2) .nav-link.active * { color: #922880 !important; }"
  ))),

  # ── Sidebar ──────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 280,

    # Summary badge — updated reactively
    uiOutput("filter_summary"),

    hr(),

    # --- Decrease threshold ------------------------------------------------
    checkboxGroupInput(
      inputId = "threshold",
      label = "Decrease Threshold",
      choices = THRESHOLD_LABELS,
      selected = THRESHOLD_LABELS[1] # default: ≥50% decrease only
    ),

    hr(),

    # --- County ------------------------------------------------------------
    selectInput(
      inputId = "county",
      label = "County",
      choices = c("All" = "", app_county_choices),
      selected = "",
      multiple = FALSE
    ),

    # --- Grade band --------------------------------------------------------
    checkboxGroupInput(
      inputId = "grade_band",
      label = "Grade Band",
      choices = grade_band_choices,
      selected = grade_band_choices
    ),

    # --- Poverty -----------------------------------------------------------
    checkboxGroupInput(
      inputId = "poverty",
      label = "Poverty Designation",
      choices = poverty_choices,
      selected = poverty_choices
    ),

    # --- Enrollment range --------------------------------------------------
    sliderInput(
      inputId = "enrollment",
      label = "Enrollment",
      min = enroll_range[1],
      max = enroll_range[2],
      value = enroll_range,
      step = 10,
      sep = ","
    ),

    hr(),

    # Reset button
    actionButton(
      "reset_filters",
      "Reset Filters",
      class = "btn-outline-secondary btn-sm w-100"
    )
  ),

  # ── Main panel ───────────────────────────────────────────────────────────
  navset_card_underline(
    # --- Map tab -----------------------------------------------------------
    nav_panel(
      title = "Map",
      icon = icon("map"),
      leafletOutput("bright_spots_map", height = "650px")
    ),

    # --- Table tab ---------------------------------------------------------
    nav_panel(
      title = "Table",
      icon = icon("table"),
      div(
        style = "margin-bottom: 10px; display: flex; gap: 10px; align-items: center;",
        downloadButton(
          "download_csv",
          "Download CSV",
          class = "btn-sm",
          style = paste0(
            "background:",
            NCDPI_NAVY,
            ";",
            "color: white;",
            "border: none;",
            "font-weight: 600;"
          )
        ),
        textOutput("table_count", inline = TRUE) |>
          tagAppendAttributes(style = "color: #666; font-size: 13px;")
      ),
      reactableOutput("bright_spots_table")
    )
  )
)
