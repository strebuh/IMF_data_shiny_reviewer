Sys.setenv(LANG = "en")

# Regions
groups <- c("all countries") #list("all countries")

# --- Base map ---
# base_map <- readRDS("dane/mapa_highchart_woj.RDS")


# APP: UI -----------------------------------------------------------

reviewer_ui <- function(id){
  ns <- NS(id)
  tags$table(
    tags$tr(
      tags$td(id = "left_panel",
                             
                       conditionalPanel(condition ="input.tabsetPanel_1  == 'Countries comparison'",
                                        uiOutput(ns("year_radar_UI"))),
                       
                       conditionalPanel(condition ="input.tabsetPanel_1  == 'Countries filter'",
                                        uiOutput(ns("year_filter_UI"))),
                       
                       selectInput(ns("continent"),
                                   label = "Continent",
                                   choices = c("All", "Asia", "Australia", "Europe", "North America", "South America"),
                                   selected = "All"),
                       
                       selectInput(ns("group"),
                                   label = "Group of countries",
                                   choices = groups,
                                   selected = groups,
                                   multiple = F),
                       
                       # Variables to show on radar plot
                       conditionalPanel(condition = "input.tabsetPanel_1  == 'Countries comparison'",
                                        uiOutput(ns("variables_radar_UI"))),
                       
                       # Countries for each year
                       conditionalPanel(condition = "input.tabsetPanel_1  == 'Countries comparison'",
                                        uiOutput(ns("year_countries_UI"))),
                       
                       # Variables sliders will be created for
                       conditionalPanel(condition ="input.tabsetPanel_1  == 'Countries filter'",
                                        uiOutput(ns("sliders_var_UI"))),
                       
                       # Variables the filtered table will consist of
                       conditionalPanel(condition = "input.tabsetPanel_1  == 'Countries filter'",
                                        uiOutput(ns("filter_dt_var_UI"))),
                       
                       # Sliders made for selected variables
                       conditionalPanel(condition ="input.tabsetPanel_1  == 'Countries filter'",
                                        uiOutput(ns("filter_sliders_UI"))),
                       
                       # Action button for 1st tab
                       conditionalPanel(condition ="input.tabsetPanel_1  == 'Countries comparison'",
                                        actionButton(ns("radarButton"), "Show output")),
                       
                       # Action button for 2nd tab
                       conditionalPanel(condition ="input.tabsetPanel_1  == 'Countries filter'",
                                        actionButton(ns("filterTableButton"), "Show output")),
                       
                       ),
      
      tags$td(id = "right_panel",
              fluidPage(
                tabsetPanel(
                  id = "tabsetPanel_1",
                  
                  tabPanel("Countries comparison",
                                          br(),
                                          h3(htmlOutput(ns("radar_plot_desc")), style = "text-align: justify; font-size: 16px; margin-left: 20px;  margin-right: 20px"),
                                          br(),
                                          plotlyOutput(ns("radar_plot"), height="400") %>%  shinycssloaders::withSpinner(color = col_nieb, type = 6),
                                          hr(),
                                          DT::dataTableOutput(ns("radar_table")) %>%  shinycssloaders::withSpinner(color = col_nieb, type = 6)
                                          ),
                                 
                  tabPanel("Countries filter",
                                          br(),
                                          DT::dataTableOutput(ns("filter_table")))
                                 
                                 )))
    )
  )
  }

# APP: SERVER -------------------------------------------------------

server_1 <- function(input, output, session){
  
  
  # ------------------------------------------------------------------------
  # --- Reactive reading data ---
  # --------------------------------------------------------------------------
  
  
  nominal_data <- reactive({
    fread("data/IMF_data_nominal.csv")
  })
  
  scaled_data <- reactive({
    fread("data/IMF_data_scaled.csv")
  })
  
  variables_list <-  reactive({
    names(nominal_data())[-c(1,2)]
  })
  
  
  # --------------------------------------------------------------------------
  # ---- UI Inputs ---- 
  # --------------------------------------------------------------------------
  
  
  # --------------------------------------------------------------------------
  # -- tab 1 - Radar plot and table --
  
  
  output$year_radar_UI <- renderUI({
    ns <- session$ns
    
    choices <- unique(nominal_data()$Year)
    choices <- sort(choices, decreasing = TRUE)
    selected <- year(Sys.Date())

    selectizeInput(ns("year_radar"),
                   label = "Year",
                   choices = choices,
                   selected = selected, 
                   options = list(maxItems = 4),
                   multiple = TRUE
                   )
  })
  
  # Create selectInputs of countries for each year selected
  output$year_countries_UI <- renderUI({
    ns <- session$ns
    req(input$year_radar, input$group)
    
    years <- input$year_radar
    filter_var_names <- input$filter_input_variables
    filter_variables <- nominal_data()[, ..filter_var_names]
    group <- input$group
    
    if(is.null(group)){
      countries <- "none"
    } else if (group=="all countries"){
      countries <- unique(nominal_data()$Country)
    } else {
      countries <- unique(nominal_data()[some_condition==T,]$Country)
    }
    
    # Create list of inputs, one for each year
    lapply(1:length(years), function(i) {
      selectizeInput(inputId = ns(paste0("countries_", i)),
                     label = paste("Countries -", years[i]),
                     choices = countries,
                     selected = c("Poland", "Hungary", "Argentina"), #sample(countries, 1),
                     options = list(maxItems = 4),
                     multiple = TRUE)
    })
  })
  
  
  output$variables_radar_UI <- renderUI({
    ns <- session$ns
    
    choices <- names(nominal_data())[-c(1:2)]
    selected <- choices[c(2, 6, 8)] #sample(choices, 3)
    selectInput(ns("variables_radar"),
                label = "Variables",
                choices = choices,
                selected = selected,
                multiple = TRUE)
  })
  
  
  # --------------------------------------------------------------------------
  # -- tab 2 - filtering countries based on variables --
  
  
  output$year_filter_UI <- renderUI({
    ns <- session$ns
    
    choices <- unique(nominal_data()$Year)
    selected <- choices[year(Sys.Date())]
    
    selectizeInput(ns("year_filter"),
                label = "Year",
                choices = choices,
                selected = selected, 
                options = list(maxItems = 5),
                multiple = FALSE
    )})
  
  
  output$sliders_var_UI <- renderUI({
    ns <- session$ns
    
    choices <- names(nominal_data())[-c(1:2)]
    selected <- sample(choices, 1)
    
    selectInput(ns("filter_input_variables"),
                label = "Sliders variables",
                choices = choices,
                selected = selected, 
                # options = list(maxItems = 4),
                multiple = TRUE
    )})
  
  
  output$filter_dt_var_UI <- renderUI({
    ns <- session$ns
    req(input$filter_input_variables)
    
    choices <- names(nominal_data())[-c(1:2)]
    selected <- unique(c(input$filter_input_variables, choices[c(2,4,6)])) # sample(choices, 3)
    
    selectizeInput(ns("variables_filter"),
                label = "Table variables",
                choices = choices,
                selected = selected,
                options = list(maxItems = 5),
                multiple = TRUE
    )})

  
  # Generate sliders for selected variables
  output$filter_sliders_UI <- renderUI({
    ns <- session$ns
    req(input$filter_input_variables)
    
    filter_var_names <- input$filter_input_variables
    filter_variables <- nominal_data()[, ..filter_var_names]
    
    # Create list of inputs, one for each variable
    lapply(1:length(input$filter_input_variables), function(i) {

      tmp_var <- filter_var_names[i]
      tmp_var_v <- unlist(filter_variables[,..tmp_var])

      # Statistics for starting values on sliders
      min_v <- floor(min(tmp_var_v, na.rm = T))
      max_v <- ceiling(max(tmp_var_v, na.rm = T))
      mean_v <- floor(mean(tmp_var_v, na.rm = T))
      sd_v <- sd(tmp_var_v, na.rm = T)

      sliderInput(inputId = ns(paste0("variable_", i)),
                  label = tmp_var, 
                  min = min_v,
                  max = max_v,
                  round = TRUE,
                  value = c(floor(mean_v + sd_v), ceiling(max_v)), 
                  step = 1)
    })
  })
  
  
  # --------------------------------------------------------------------------
  # --- OUTPUTS ---
  # --------------------------------------------------------------------------
  
  # --------------------------------------------------------------------------
  # -- tab 1 - Radar plot and table --
  
  
  get_filtered_table <- eventReactive(input$filterTableButton, {
    req(input$variables_filter, input$filter_input_variables, input[["variable_1"]])
    
    variables <- c("Country", input$filter_input_variables, input$variables_filter)
    countries <- unique(nominal_data()$Country)
    
    # List of extreme values for each slider variable
    var_lim <- lapply(1:length(input$filter_input_variables), function(x){
      c(input[[paste0("variable_", x)]][1], input[[paste0("variable_", x)]][2])
    })
    
    filtered_table <- nominal_data()[Year == input$year_filter,]
    
    # Find rows that meet criteria 
    filter_var_names <- input$filter_input_variables
    keep_rows <- sapply(1:length(input$filter_input_variables), function(x){
      var_ <- filter_var_names[x]
      between(filtered_table[, ..var_], var_lim[[x]][1], var_lim[[x]][2])
    })
    keep_rows[nrow(keep_rows), 1] <- T
    keep_rows <- unname(sapply(as.data.frame(t(keep_rows)), all))
    
    # Apply filters to table
    filtered_table <- filtered_table[keep_rows, ..variables]
    
    # Transform table
    final_dt <- setDT(as.data.frame(t(filtered_table)), keep.rownames = T)
    colnames(final_dt) <- c("Variable name", as.character(filtered_table$Country))
    final_dt[,`Variable name`:=names(filtered_table)]
    
    return(final_dt[-c(1,2),])
    })
  
  
    output$filter_table <- DT::renderDataTable({get_filtered_table()},
                                               rownames = FALSE,
                                               options = list(dom = 'Bfrtip',
                                                              columnDefs = list(
                                                                list(className = "dt-head-center dt-center", targets = "_all")), #
                                                              pageLength = 10),
                                               server = FALSE)

 
  # --------------------------------------------------------------------------
  # -- tab 2 - filtering countries based on variables --

  # Get selected coutntries for each selected year  
  get_countries_year <- reactive({
    years <- input$year_radar
    temp_list <- lapply(1:length(years), function(x){
      c(input[[paste0("countries_", x)]])
    })
    names(temp_list) <- as.character(years)
    return(temp_list)
  })
  
    
  get_radar_plot <- eventReactive(input$radarButton, {
    req(input$variables_radar, input$countries_1, input$year_radar)

    countries_year <- get_countries_year()
    years <- as.numeric(names(get_countries_year()))
    variables <- c("Country", "Year", input$variables_radar)
    selected_group <- input$group

    # Get data for selected countries and years 
    data_table <- lapply(years, function(x){
      as.data.frame(scaled_data()[Year %in% x &
                                    Country %in% countries_year[[as.character(x)]],
                                  ..variables])
      })
    data_table <- as.data.frame(rbindlist(data_table))

    # Substitute Inf values with 0
    if(ncol(data_table)==3){
      data_table[,3] <- ifelse(as.character(data_table[,3])=="Inf", 
                                               10, 
                                               ifelse(is.na(data_table[,3]), 
                                                      0, 
                                                      data_table[,3]))
    } else {
      data_table[,-c(1:2)] <- lapply(data_table[,-c(1:2)], function(x) {
        ifelse(as.character(x)=="Inf", 10, ifelse(is.na(x), 0, x)) 
      })
    }
    setDT(data_table)

    data_table[,c("Country_year", "Country", "Year"):=.(paste0(Country," - ",as.character(Year)), NULL, NULL)]
    data_table_col <- c("Country_year", input$variables_radar)
    data_table <- data_table[,..data_table_col]

    # Prepare radar plot
    fig <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    )
    for(c in data_table$Country_year) {
      curr_data <- unlist(data_table[Country_year==c,-1])
      fig <- fig %>%
        add_trace(
          r = curr_data,
          theta = input$variables_radar,
          name = c
        )
    }
    return(fig)
  })
  
  
  output$radar_plot<- renderPlotly({
    get_radar_plot()
  })

  
  # output$radar_table_desc <- renderText({
  #   req(input$year_radar)
  #   countries <- if(length(input$year_radar) > 1) paste(get_countries_year(), collapse = ", ") else get_countries_year()
  #   paste0( "Data for: ", unique(countries))
  #   })

  radar_table <- eventReactive(input$radarButton, {
    req(input$variables_radar, input$countries_1, input$group)
    
    countries_year <- get_countries_year()
    years <- as.numeric(names(countries_year))
    variables <- c("Country", "Year", input$variables_radar)
    group <- input$group
    
    data_table <- lapply(years, function(x){
      as.data.frame(nominal_data()[Year %in% x &
                                    Country %in% countries_year[[as.character(x)]],
                                  ..variables])
    })
    data_table <- rbindlist(data_table)
    setorder(data_table, Country, Year)
    return(data_table)
  })

  
  output$radar_table <- DT::renderDataTable({radar_table()},
                                              rownames = FALSE,
                                              options = list(dom = 'Bfrtip', 
                                                             columnDefs = list(
                                                               list(className = "dt-head-center dt-center", targets = "_all")), #
                                                             pageLength = 10),
                                              server = FALSE)
}
