# app.R ----------------------------------------------------------------
# UK 2024 GE under AV — sub-block edition.
# Run: shiny::runApp(".")
# ----------------------------------------------------------------------

library(shiny)
library(leaflet)
library(dplyr)

source("R/irv_engine.R")
source("R/data_loader.R")
source("R/flow_helpers.R")
source("R/subblock_helpers.R")
source("R/sankey_helpers.R")
source("R/vote_share_helpers.R")

# networkD3 is needed for the Sankey diagram. We require it lazily so
# the app still works without it (Sankey panel just shows a message).
HAS_SANKEY <- requireNamespace("networkD3", quietly = TRUE)

PARTY_COLOURS <- c(
  Lab      = "#E4003B", Con      = "#0087DC", LD       = "#FAA61A",
  Reform   = "#12B6CF", Green    = "#02A95B", SNP      = "#FFF95D",
  PC       = "#005B54", DUP      = "#D46A4C", SF       = "#326760",
  SDLP     = "#2AA82C", UUP      = "#48A5EE", Alliance = "#F6CB2F",
  TUV      = "#0095B6", Other    = "#888888"
)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Source Sans Pro', system-ui, sans-serif; }
    .seat-pill { display:inline-block; padding:6px 12px; margin:3px;
                 border-radius: 16px; color:white; font-weight:600; }
    .origin-card { border:1px solid #ccc; border-radius:6px;
                   padding:10px 12px; margin-bottom:14px; background:#fafafa; }
    .subblock-row { display:flex; align-items:center; gap:8px;
                    margin-bottom:6px; }
    .subblock-row .share { width: 80px; flex: 0 0 auto; }
    .subblock-row .ranking { flex: 1 1 auto; }
    .subblock-row .remove-btn { flex: 0 0 auto; }
    .sum-note { font-size: 0.85em; color: #666; margin-top: 4px; }
    .sum-warn { color: #c00; font-weight: 600; }
    h3 { margin-top: 1.2em; }
  "))),
  titlePanel("UK 2024 under AV — sub-block edition"),
  p(em("Model the 2024 General Election as if it had used Australian-",
       "style full-preferential AV. For each region and each party, ",
       "define one or more voter sub-blocks — each with its own share ",
       "of that party's voters and its own full preference ranking.")),

  sidebarLayout(
    sidebarPanel(
      width = 5,
      radioButtons("edit_mode", "Editing:",
                   choices = c("Preferences (sub-blocks)" = "subblocks",
                               "1st-preference vote shares" = "shares"),
                   selected = "subblocks", inline = TRUE),
      selectInput("region_select", "Editing region:",
                  choices = REGIONS, selected = "London"),
      conditionalPanel(
        condition = "input.edit_mode == 'subblocks'",
        selectInput("origin_select", "Origin party:", choices = NULL),
        helpText("Each row is a sub-block of that party's 1st-preference ",
                 "voters. Share = fraction (auto-normalised). Ranking = ",
                 "comma-separated party codes from 2nd preference onward."),
        uiOutput("subblock_editor"),
        hr(),
        actionButton("copy_to_all", "Copy this region's sub-blocks to all GB regions",
                     icon = icon("clone")),
        br(), br(),
        actionButton("reset_all", "Reset all sub-blocks to defaults",
                     icon = icon("undo"))
      ),
      conditionalPanel(
        condition = "input.edit_mode == 'shares'",
        helpText("Set each party's regional 1st-preference vote share ",
                 "(as a percentage). The app rescales every constituency ",
                 "in the region to match the target, preserving within-",
                 "region variation. Defaults are the actual 2024 ",
                 "regional shares."),
        uiOutput("vote_share_editor"),
        actionButton("reset_shares", "Reset to actual 2024 shares",
                     icon = icon("undo"))
      ),
      hr(),
      actionButton("run_model", "Run the count", icon = icon("play"),
                   class = "btn-primary btn-lg")
    ),

    mainPanel(
      width = 7,
      radioButtons(
        "view_mode", NULL,
        choices = c("Actual 2024 result (FPTP)" = "fptp",
                    "Simulated AV result"      = "av"),
        selected = "fptp", inline = TRUE
      ),
      h3(textOutput("view_title", inline = TRUE)),
      uiOutput("seat_summary"),
      h3("Map"),
      leafletOutput("map", height = 500),
      tags$small(em("Click a constituency on the map to see its round-",
                    "by-round AV count below. (Click works once you've ",
                    "switched to the 'Simulated AV result' view and the ",
                    "polygon map is rendered.)")),
      h3(textOutput("detail_title", inline = TRUE)),
      uiOutput("detail_summary"),
      conditionalPanel(
        condition = "output.has_detail == true",
        if (HAS_SANKEY) networkD3::sankeyNetworkOutput("sankey", height = "400px")
        else            tags$em("Install the networkD3 package to see a Sankey diagram of the count: install.packages('networkD3')")
      ),
      h3("Comparison: FPTP vs AV"),
      tableOutput("comparison_table"),
      tags$small(em("Sub-block model: voters of a party are split into ",
                    "groups with distinct rankings — captures the real ",
                    "heterogeneity in how a party's supporters transfer. ",
                    "The AV view only updates after you click 'Run the count'."))
    )
  )
)

server <- function(input, output, session) {

  # Raw 2024 data — never mutated.
  raw_results <- reactive({ load_results() })

  # Baseline regional vote shares (the actual 2024 numbers).
  baseline_shares <- reactive({ compute_regional_shares(raw_results()) })

  # User-edited targets, initialised from baseline. NULL until first
  # baseline is read, then a named list keyed by region.
  vote_share_state <- reactiveVal(NULL)
  observe({
    if (is.null(vote_share_state())) vote_share_state(baseline_shares())
  })

  # Results after applying vote-share edits. When the targets equal
  # the baseline (user hasn't touched anything) this is the raw data;
  # otherwise it's rescaled.
  results_df <- reactive({
    targets <- vote_share_state()
    if (is.null(targets) ||
        shares_are_default(targets, baseline_shares())) {
      return(raw_results())
    }
    apply_regional_shares(raw_results(), targets)
  })

  region_lookup <- reactive({
    unique(results_df()[, c("constituency_id", "region")])
  })

  default_flows <- default_flows_by_region()
  subblocks_state <- reactiveVal(default_subblocks_by_region(default_flows))

  observeEvent(input$region_select, {
    region <- input$region_select
    parties <- rownames(default_flows[[region]])
    sel <- if (!is.null(input$origin_select) && input$origin_select %in% parties)
      input$origin_select else parties[1]
    updateSelectInput(session, "origin_select",
                      choices = parties, selected = sel)
  })

  # ---- Vote-share editor UI -----------------------------------------
  output$vote_share_editor <- renderUI({
    region <- input$region_select
    req(region)
    targets <- vote_share_state()
    if (is.null(targets)) return(NULL)
    region_targets <- targets[[region]]
    if (is.null(region_targets)) return(em("No data for this region."))

    parties <- names(region_targets)
    total_pct <- sum(region_targets) * 100
    sum_class <- if (abs(total_pct - 100) < 0.5) "sum-note" else "sum-note sum-warn"

    rows <- lapply(parties, function(p) {
      div(class = "subblock-row",
        div(class = "share",
            numericInput(
              inputId = paste0("vs_", region, "_", p),
              label = NULL,
              value = round(region_targets[[p]] * 100, 2),
              min = 0, max = 100, step = 0.1
            )),
        div(class = "ranking",
            tags$span(p, style = "padding-left: 4px;"))
      )
    })

    tagList(
      div(class = "origin-card",
        strong(paste0("1st-preference shares (%) for ", region)),
        br(), br(),
        tags$div(style = "display:flex; gap:8px; font-size:0.85em; color:#666; margin-bottom:4px;",
                 tags$div(style="width:80px;", "% share"),
                 tags$div(style="flex:1;", "Party")),
        tagList(rows),
        div(class = sum_class,
            sprintf("Sum: %.2f%% (will be normalised to 100%%)", total_pct))
      )
    )
  })

  # Sync vote-share UI -> state
  observe({
    region <- input$region_select
    req(region)
    targets <- vote_share_state()
    if (is.null(targets)) return()
    region_targets <- targets[[region]]
    if (is.null(region_targets)) return()
    parties <- names(region_targets)
    new_targets <- region_targets
    changed <- FALSE
    for (p in parties) {
      v <- input[[paste0("vs_", region, "_", p)]]
      if (!is.null(v) && !is.na(v)) {
        new_v <- v / 100
        if (!isTRUE(all.equal(unname(new_targets[[p]]), new_v))) {
          new_targets[[p]] <- new_v
          changed <- TRUE
        }
      }
    }
    if (changed) {
      targets[[region]] <- new_targets
      vote_share_state(targets)
    }
  })

  observeEvent(input$reset_shares, {
    vote_share_state(baseline_shares())
    showNotification("Vote shares reset to actual 2024 values.",
                     type = "message")
  })

  output$subblock_editor <- renderUI({
    region <- input$region_select
    origin <- input$origin_select
    req(region, origin)
    state <- subblocks_state()
    spec  <- state[[region]][[origin]]
    parties <- rownames(default_flows[[region]])
    other_parties <- setdiff(parties, origin)

    rows_ui <- lapply(seq_len(nrow(spec)), function(i) {
      div(class = "subblock-row",
        div(class = "share",
            numericInput(
              inputId = paste0("share_", region, "_", origin, "_", i),
              label = NULL, value = round(spec$share[i], 3),
              min = 0, max = 1, step = 0.01
            )),
        div(class = "ranking",
            textInput(
              inputId = paste0("rank_", region, "_", origin, "_", i),
              label = NULL,
              value = ranking_to_string(spec$ranking[[i]]),
              placeholder = "e.g. Green, LD, Con, Reform"
            )),
        div(class = "remove-btn",
            actionButton(
              inputId = paste0("rm_", region, "_", origin, "_", i),
              label = NULL, icon = icon("times"),
              class = "btn-sm btn-default"
            ))
      )
    })

    total_share <- sum(spec$share)
    sum_class <- if (abs(total_share - 1) < 0.01) "sum-note" else "sum-note sum-warn"
    sum_msg <- sprintf("Shares sum to %.3f (will be normalised to 1)", total_share)

    div(class = "origin-card",
      strong(paste0(origin, " voters in ", region)),
      br(), br(),
      tags$div(style = "display:flex; gap:8px; font-size:0.85em; color:#666; margin-bottom:4px;",
               tags$div(style="width:80px;", "Share"),
               tags$div(style="flex:1;", "Ranking (2nd → last)"),
               tags$div(style="width:38px;", "")),
      tagList(rows_ui),
      actionButton(
        inputId = paste0("add_", region, "_", origin),
        label = "Add sub-block", icon = icon("plus"),
        class = "btn-sm"
      ),
      div(class = sum_class, sum_msg),
      tags$small(em("Valid party codes: ",
                    paste(other_parties, collapse = ", ")))
    )
  })

  # Sync UI -> state for share/ranking changes.
  observe({
    region <- input$region_select
    origin <- input$origin_select
    req(region, origin)
    state <- subblocks_state()
    spec <- state[[region]][[origin]]
    parties <- rownames(default_flows[[region]])

    new_spec <- spec
    changed <- FALSE
    for (i in seq_len(nrow(spec))) {
      share_val <- input[[paste0("share_", region, "_", origin, "_", i)]]
      rank_val  <- input[[paste0("rank_", region, "_", origin, "_", i)]]
      if (!is.null(share_val) && !is.na(share_val)) {
        if (!isTRUE(all.equal(new_spec$share[i], share_val))) {
          new_spec$share[i] <- share_val; changed <- TRUE
        }
      }
      if (!is.null(rank_val)) {
        parsed <- ranking_from_string(rank_val)
        parsed <- parsed[parsed %in% parties & parsed != origin]
        if (!identical(new_spec$ranking[[i]], parsed)) {
          new_spec$ranking[[i]] <- parsed; changed <- TRUE
        }
      }
    }
    if (changed) {
      state[[region]][[origin]] <- new_spec
      subblocks_state(state)
    }
  })

  # Add button.
  observe({
    region <- input$region_select
    origin <- input$origin_select
    req(region, origin)
    parties <- rownames(default_flows[[region]])
    add_id <- paste0("add_", region, "_", origin)
    val <- input[[add_id]]
    if (is.null(val) || val == 0) return()
    prev_key <- paste0("__prev_", add_id)
    prev <- isolate(session$userData[[prev_key]] %||% 0)
    if (val > prev) {
      session$userData[[prev_key]] <- val
      isolate({
        state <- subblocks_state()
        state[[region]][[origin]] <- add_subblock_row(
          state[[region]][[origin]], parties, origin
        )
        subblocks_state(state)
      })
    }
  })

  # Remove buttons — scan all of them for this origin.
  observe({
    region <- input$region_select
    origin <- input$origin_select
    req(region, origin)
    state <- subblocks_state()
    spec <- state[[region]][[origin]]
    for (i in seq_len(nrow(spec))) {
      rm_id <- paste0("rm_", region, "_", origin, "_", i)
      val <- input[[rm_id]]
      if (is.null(val) || val == 0) next
      prev_key <- paste0("__prev_", rm_id)
      prev <- isolate(session$userData[[prev_key]] %||% 0)
      if (val > prev) {
        session$userData[[prev_key]] <- val
        isolate({
          new_state <- subblocks_state()
          new_state[[region]][[origin]] <- remove_subblock_row(
            new_state[[region]][[origin]], i
          )
          subblocks_state(new_state)
        })
        break
      }
    }
  })

  observeEvent(input$copy_to_all, {
    region <- input$region_select
    state <- subblocks_state()
    src <- state[[region]]
    for (r in REGIONS) if (r != "Northern Ireland") state[[r]] <- src
    subblocks_state(state)
    showNotification("Sub-blocks copied to all GB regions.", type = "message")
  })

  observeEvent(input$reset_all, {
    subblocks_state(default_subblocks_by_region(default_flows))
    showNotification("Sub-blocks reset to defaults.", type = "message")
  })

  model_output <- eventReactive(input$run_model, {
    withProgress(message = "Counting…", value = 0.2, {
      state <- subblocks_state()
      state <- lapply(state, function(region_spec) {
        lapply(region_spec, normalise_shares)
      })
      incProgress(0.2)
      out <- irv_count_all(
        results_df(), region_lookup(),
        subblocks_by_region = state,
        default_flows_by_region = default_flows
      )
      incProgress(0.6)
      out
    })
  }, ignoreNULL = FALSE)

  # The view shown on the map and in the seat-totals panel. Toggled
  # by input$view_mode. Both branches return a data frame with at
  # least constituency_id and winner.
  current_view <- reactive({
    if (input$view_mode == "fptp") {
      fp <- fptp_winners(results_df())
      data.frame(constituency_id = fp$constituency_id,
                 winner          = fp$fptp_winner,
                 stringsAsFactors = FALSE)
    } else {
      mo <- model_output()
      if (is.null(mo)) return(NULL)
      mo[, c("constituency_id", "winner")]
    }
  })

  output$view_title <- renderText({
    swung <- !is.null(vote_share_state()) &&
             !shares_are_default(vote_share_state(), baseline_shares())
    suffix <- if (swung) " (with edited vote shares)" else ""
    if (input$view_mode == "fptp")
      paste0("Seats won — FPTP on the data", suffix)
    else
      paste0("Seats won — AV simulation", suffix)
  })

  seat_totals <- reactive({
    cv <- current_view(); if (is.null(cv)) return(NULL)
    sort(table(cv$winner), decreasing = TRUE)
  })

  output$seat_summary <- renderUI({
    st <- seat_totals()
    if (is.null(st)) return(em("Click 'Run the count' to generate the AV simulation."))
    pills <- lapply(names(st), function(p) {
      col <- PARTY_COLOURS[p]; if (is.na(col)) col <- "#444"
      span(class = "seat-pill", style = paste0("background:", col, ";"),
           paste0(p, ": ", st[[p]]))
    })
    do.call(tagList, pills)
  })

  output$comparison_table <- renderTable({
    mo <- model_output()
    fptp <- fptp_winners(results_df())
    seats_fptp <- as.data.frame(table(fptp$fptp_winner), stringsAsFactors = FALSE)
    names(seats_fptp) <- c("Party", "FPTP seats")
    if (is.null(mo)) {
      seats_fptp$`AV seats` <- NA_integer_
      seats_fptp$Change <- NA_integer_
      return(seats_fptp[order(-seats_fptp$`FPTP seats`), ])
    }
    seats_av <- as.data.frame(table(mo$winner), stringsAsFactors = FALSE)
    names(seats_av) <- c("Party", "AV seats")
    out <- merge(seats_fptp, seats_av, by = "Party", all = TRUE)
    out[is.na(out)] <- 0
    out$Change <- out$`AV seats` - out$`FPTP seats`
    out[order(-out$`AV seats`), ]
  }, digits = 0)

  # ---- Click → detail ------------------------------------------------
  selected_constituency <- reactiveVal(NULL)

  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (is.null(click) || is.null(click$id)) return()
    selected_constituency(click$id)
  })

  detail_count <- reactive({
    cid <- selected_constituency()
    if (is.null(cid)) return(NULL)
    state <- lapply(subblocks_state(), function(rs) {
      lapply(rs, normalise_shares)
    })
    res <- irv_count_one(results_df(), region_lookup(), cid,
                         subblocks_by_region = state,
                         default_flows_by_region = default_flows)
    if (is.null(res)) return(NULL)
    con <- results_df()[results_df()$constituency_id == cid, ]
    region <- region_lookup()$region[region_lookup()$constituency_id == cid][1]
    v <- setNames(con$votes, con$party)
    df <- default_flows[[region]]
    if (!is.null(df)) {
      keep <- intersect(names(v), rownames(df))
      df <- df[keep, keep, drop = FALSE]
      v <- v[keep]
    }
    blocks <- build_blocks_from_subblocks(v, state[[region]] %||% list(), df)
    list(res = res, blocks = blocks, parties = names(v),
         con_name = con$constituency_name[1])
  })

  output$has_detail <- reactive({ !is.null(detail_count()) })
  outputOptions(output, "has_detail", suspendWhenHidden = FALSE)

  output$detail_title <- renderText({
    d <- detail_count()
    if (is.null(d)) "Constituency detail (click a polygon to view)"
    else paste0("Detail: ", d$con_name)
  })

  output$detail_summary <- renderUI({
    d <- detail_count()
    if (is.null(d)) return(em("Click a constituency on the map to see ",
                              "the round-by-round count."))
    elims <- if (length(d$res$eliminated) == 0) "none (won on first round)"
             else paste(d$res$eliminated, collapse = " → ")
    tagList(tags$p(
      tags$strong("Winner: "), d$res$winner, tags$br(),
      tags$strong("Rounds: "), nrow(d$res$rounds), tags$br(),
      tags$strong("Elimination order: "), elims, tags$br(),
      tags$strong("Final margin: "),
      format(round(d$res$final_margin), big.mark = ",")
    ))
  })

  if (HAS_SANKEY) {
    output$sankey <- networkD3::renderSankeyNetwork({
      d <- detail_count(); if (is.null(d)) return(NULL)
      sk <- sankey_from_irv(d$res, d$blocks, d$parties)
      colour_js <- paste0(
        "d3.scaleOrdinal().domain([",
        paste(shQuote(names(PARTY_COLOURS)), collapse = ","),
        "]).range([",
        paste(shQuote(unname(PARTY_COLOURS)), collapse = ","),
        "])"
      )
      networkD3::sankeyNetwork(
        Links = sk$links, Nodes = sk$nodes,
        Source = "source", Target = "target", Value = "value",
        NodeID = "name", NodeGroup = "party",
        colourScale = colour_js,
        fontSize = 12, nodeWidth = 18, nodePadding = 12
      )
    })
  }

  output$map <- renderLeaflet({
    cv <- current_view(); if (is.null(cv)) return(NULL)
    mo <- cv  # keep variable name `mo` below; semantics unchanged

    # Direct lookup: party name -> hex colour. Unknown parties get grey.
    # This bypasses leaflet::colorFactor, which silently mis-maps the
    # palette to factor levels by position rather than by name.
    colour_for <- function(party) {
      out <- unname(PARTY_COLOURS[party])
      out[is.na(out)] <- "#444444"
      out
    }

    # Prefer the baked RDS (fast, pre-simplified); fall back to raw files.
    shp <- load_boundaries()
    if (!is.null(shp)) {
      # Auto-detect the constituency-code column. Different boundary
      # sources use different names; try the common ones.
      candidate_cols <- c("PCON24CD", "PCON25CD", "pcon24cd", "pcon25cd",
                          "PCONCD", "PCON_CODE", "id", "ID", "CODE", "code")
      code_col <- intersect(candidate_cols, names(shp))[1]
      if (is.na(code_col)) {
        showNotification(
          paste0("GeoJSON has no recognised constituency-code column. ",
                 "Available columns: ",
                 paste(names(shp)[!names(shp) %in% c("geometry")],
                       collapse = ", "),
                 ". Edit the candidate_cols list in app.R."),
          type = "error", duration = NULL
        )
        return(leaflet() |> addProviderTiles(providers$OpenStreetMap.Mapnik))
      }
      # base::merge() strips the sf class — reattach it after merging.
      geom_col <- attr(shp, "sf_column")
      shp <- merge(shp, mo, by.x = code_col, by.y = "constituency_id",
                   all.x = TRUE)
      shp <- sf::st_as_sf(shp, sf_column_name = geom_col)
      n_matched <- sum(!is.na(shp$winner))
      if (n_matched == 0) {
        showNotification(
          paste0("Boundary file loaded (", nrow(shp), " features) but ",
                 "none of the codes match the loaded results. This ",
                 "usually means you're using the synthetic fixture — ",
                 "drop the real HCL 2024 CSV at data/hcl_2024.csv to ",
                 "see the choropleth."),
          type = "warning", duration = 10
        )
      }
      # Also detect a name column for hover labels.
      name_col <- intersect(c("PCON24NM", "PCON25NM", "pcon24nm", "pcon25nm",
                              "PCONNM", "name", "NAME"), names(shp))[1]
      label_text <- if (!is.na(name_col)) {
        paste0(shp[[name_col]], ": ", shp$winner)
      } else {
        paste0(shp[[code_col]], ": ", shp$winner)
      }
      leaflet(shp) |>
        addProviderTiles(providers$OpenStreetMap.Mapnik) |>
        addPolygons(fillColor = colour_for(shp$winner), fillOpacity = 0.85,
                    weight = 0.3, color = "#444",
                    label = label_text,
                    layerId = shp[[code_col]])
    } else {
      # Marker fallback: scatter around rough region centroids so the
      # map at least sits over the UK rather than a generic bounding box.
      region_centroids <- data.frame(
        region = c("North East", "North West", "Yorkshire and The Humber",
                   "East Midlands", "West Midlands", "East of England",
                   "London", "South East", "South West",
                   "Scotland", "Wales", "Northern Ireland"),
        lat = c(54.9, 53.7, 53.9, 52.9, 52.5, 52.2,
                51.5, 51.3, 50.9, 56.5, 52.4, 54.6),
        lng = c(-1.6, -2.7, -1.3, -0.9, -2.0, 0.5,
                -0.1, -0.7, -3.5, -4.2, -3.8, -6.7)
      )
      meta_join <- merge(mo, region_lookup(), by = "constituency_id",
                          all.x = TRUE)
      plot_df <- merge(meta_join, region_centroids, by = "region",
                        all.x = TRUE)
      set.seed(1)
      n <- nrow(plot_df)
      plot_df$lat <- plot_df$lat + runif(n, -0.5, 0.5)
      plot_df$lng <- plot_df$lng + runif(n, -0.8, 0.8)
      leaflet(plot_df) |>
        addProviderTiles(providers$OpenStreetMap.Mapnik) |>
        addCircleMarkers(lng = ~lng, lat = ~lat,
                         color = colour_for(plot_df$winner), radius = 4,
                         stroke = FALSE, fillOpacity = 0.85,
                         label = ~paste0(constituency_id, ": ", winner))
    }
  })
}

shinyApp(ui, server)
