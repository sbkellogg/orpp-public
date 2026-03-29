# =============================================================================
# server.R — 2025 Attendance Bright Spots Shiny App
#
# Key reactive flow:
#   filtered_data()  ← all sidebar inputs
#       │
#       ├─► leafletProxy  (updates markers without full map redraw)
#       ├─► renderReactable (filtered table)
#       ├─► filter_summary badge
#       └─► table_count label / download handler
# =============================================================================

function(input, output, session) {
  # ── Reset button ───────────────────────────────────────────────────────────
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(
      session,
      "threshold",
      selected = THRESHOLD_LABELS[1]
    )
    updateSelectInput(session, "county", selected = "")
    updateCheckboxGroupInput(
      session,
      "grade_band",
      selected = grade_band_choices
    )
    updateCheckboxGroupInput(session, "poverty", selected = poverty_choices)
    updateSliderInput(session, "enrollment", value = enroll_range)
  })

  # ── Filtered dataset ───────────────────────────────────────────────────────
  filtered_data <- reactive({
    df <- bright_spot_candidates

    # Decrease threshold — derive from pct_decrease at runtime using the
    # DECREASE_THRESHOLDS list defined in global.R
    if (length(input$threshold) > 0) {
      selected_thresholds <- DECREASE_THRESHOLDS[
        THRESHOLD_LABELS %in% input$threshold
      ]
      df <- df |>
        filter(purrr::reduce(
          purrr::map(selected_thresholds, function(t) {
            pct_decrease >= t$min & pct_decrease < t$max
          }),
          `|`
        ))
    }

    # County (single select; "" means "All")
    if (!is.null(input$county) && input$county != "") {
      df <- df |> filter(county == input$county)
    }

    # Grade band
    if (length(input$grade_band) > 0) {
      df <- df |> filter(grade_band %in% input$grade_band)
    }

    # Poverty designation
    if (length(input$poverty) > 0) {
      df <- df |> filter(poverty %in% input$poverty)
    }

    # Enrollment range
    df <- df |>
      filter(den >= input$enrollment[1], den <= input$enrollment[2])

    df
  })

  # ── Filter summary badge ───────────────────────────────────────────────────
  output$filter_summary <- renderUI({
    n <- nrow(filtered_data())
    div(
      style = paste0(
        "background:",
        NCDPI_NAVY,
        ";",
        "color: white;",
        "border-radius: 6px;",
        "padding: 8px 12px;",
        "text-align: center;",
        "font-size: 13px;"
      ),
      strong(n),
      " schools"
    )
  })

  # ── Table row count label ──────────────────────────────────────────────────
  output$table_count <- renderText({
    paste0(nrow(filtered_data()), " schools")
  })

  # ── Initial map render ─────────────────────────────────────────────────────
  output$bright_spots_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        data = nc_county_boundaries,
        fillColor = "transparent",
        color = "#989a9cff",
        weight = 0.5,
        opacity = 0.5
      ) |>
      addPolygons(
        data = nc_state_boundary,
        color = "#989a9c72",
        weight = 0.5,
        opacity = 0.9
      ) |>
      addLegend(
        position = "bottomright",
        pal = grade_band_palette,
        values = factor(
          bright_spot_candidates$grade_band,
          levels = GRADE_BAND_LEVELS
        ),
        title = "Grade Band",
        opacity = 0.8
      ) |>
      setView(lng = -79.5, lat = 35.5, zoom = 7)
  })

  # ── Update map markers when filters change ─────────────────────────────────
  # Uses leafletProxy so county/state polygons and legend are not redrawn.
  observeEvent(filtered_data(), {
    df <- filtered_data()

    proxy <- leafletProxy("bright_spots_map") |>
      clearGroup("schools") # remove previous markers

    if (nrow(df) == 0) {
      return(proxy)
    }

    proxy |>
      addCircleMarkers(
        data = df,
        lng = ~longitude,
        lat = ~latitude,
        radius = 6,
        fillColor = ~ grade_band_palette(grade_band),
        color = "white",
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        opacity = 1,
        popup = make_school_popups(df),
        label = ~name,
        group = "schools"
      )
  })

  # ── Reactable table ────────────────────────────────────────────────────────
  output$bright_spots_table <- renderReactable({
    filtered_data() |>
      select(
        name,
        county,
        grade_band,
        poverty,
        den,
        p_peak,
        p_current,
        improvement_pp,
        pct_decrease
      ) |>
      arrange(desc(pct_decrease)) |>
      reactable(
        searchable = TRUE,
        filterable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        pagination = TRUE,
        defaultPageSize = 25,
        defaultSorted = list(pct_decrease = "desc"),
        style = list(
          fontFamily = "Source Sans Pro, Arial, sans-serif",
          fontSize = "14px"
        ),
        theme = ncdpi_table_theme,
        columns = list(
          name = colDef(
            name = "School",
            minWidth = 200,
            sticky = "left",
            filterMethod = JS(
              "function(rows, columnId, filterValue) {
                return rows.filter(row =>
                  row.values[columnId].toLowerCase()
                    .includes(filterValue.toLowerCase())
                );
              }"
            )
          ),
          county = colDef(
            name = "County",
            minWidth = 120,
            filterMethod = JS(
              "function(rows, columnId, filterValue) {
                return rows.filter(row =>
                  row.values[columnId].toLowerCase()
                    .includes(filterValue.toLowerCase())
                );
              }"
            )
          ),
          grade_band = colDef(
            name = "Grade Band",
            minWidth = 140,
            filterInput = JS(
              "function(column, state) {
                const options = ['', ...new Set(
                  state.data.map(row => row[column.id])
                )].sort();
                return React.createElement('select', {
                  onChange: e => column.setFilter(e.target.value || undefined),
                  style: { width: '100%', fontSize: '13px' }
                },
                options.map(opt =>
                  React.createElement('option', { value: opt, key: opt },
                    opt || 'All')
                ));
              }"
            )
          ),
          poverty = colDef(
            name = "Poverty",
            minWidth = 100,
            filterInput = JS(
              "function(column, state) {
                const options = ['', ...new Set(
                  state.data.map(row => row[column.id])
                )].sort();
                return React.createElement('select', {
                  onChange: e => column.setFilter(e.target.value || undefined),
                  style: { width: '100%', fontSize: '13px' }
                },
                options.map(opt =>
                  React.createElement('option', { value: opt, key: opt },
                    opt || 'All')
                ));
              }"
            )
          ),
          den = colDef(
            name = "Enrollment",
            format = colFormat(separators = TRUE),
            minWidth = 110,
            filterable = FALSE
          ),
          p_peak = colDef(
            name = "Peak Rate (2022)",
            format = colFormat(percent = TRUE, digits = 1),
            minWidth = 140,
            filterable = FALSE,
            style = list(color = "#e74c3c", fontWeight = "600")
          ),
          p_current = colDef(
            name = "Current Rate (2025)",
            format = colFormat(percent = TRUE, digits = 1),
            minWidth = 150,
            filterable = FALSE,
            style = list(color = "#27ae60", fontWeight = "600")
          ),
          improvement_pp = colDef(
            name = "Improvement (pp)",
            minWidth = 150,
            filterable = FALSE,
            cell = function(value) paste0(round(value, 1), " pp")
          ),
          pct_decrease = colDef(
            name = "% Decrease",
            minWidth = 130,
            filterable = FALSE,
            cell = function(value) {
              bar_width <- paste0(round(value * 100), "%")
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                div(
                  style = paste0(
                    "background:",
                    NCDPI_NAVY,
                    ";",
                    "width:",
                    bar_width,
                    ";",
                    "height: 14px; border-radius: 2px; min-width: 2px;"
                  )
                ),
                span(
                  style = "font-weight: 600;",
                  paste0(round(value * 100, 1), "%")
                )
              )
            }
          )
        )
      )
  })

  # ── CSV download ───────────────────────────────────────────────────────────
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("bright_spots_2025_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      filtered_data() |>
        select(
          School = name,
          County = county,
          `Grade Band` = grade_band,
          Poverty = poverty,
          Enrollment = den,
          `Peak Rate 2022 (%)` = p_peak,
          `Current Rate 2025 (%)` = p_current,
          `Improvement (pp)` = improvement_pp,
          `Pct Decrease (%)` = pct_decrease
        ) |>
        mutate(
          `Peak Rate 2022 (%)` = round(`Peak Rate 2022 (%)` * 100, 1),
          `Current Rate 2025 (%)` = round(`Current Rate 2025 (%)` * 100, 1),
          `Improvement (pp)` = round(`Improvement (pp)`, 1),
          `Pct Decrease (%)` = round(`Pct Decrease (%)` * 100, 1)
        ) |>
        write_csv(file)
    }
  )
}
