box::use(
  shiny[
    enableBookmarking,
    bookmarkButton,
    moduleServer,
    NS,
    fluidRow,
    h4,
    observeEvent,
    reactiveValues,
    isolate,
    reactive,
    req,
    tagList,
    tags,
  ],
  shiny.semantic[
    selectInput,
    toggle,
    updateSelectInput,
  ],
  semantic.dashboard[
    icon,
    valueBox,
    dashboardPage,
    dashboardHeader,
    dashboardBody,
    dashboardSidebar,
    sidebarMenu,
    menuItem,
    box,
    tabItems,
    tabItem,
    infoBox,
  ],
  dplyr[
    bind_rows,
    filter,
    pull,
  ],
  here[
    here,
  ],
  waiter[
    spin_flower,
    useWaiter,
    waiter_show,
    waiter_hide,
  ],
)

box::use(
  app / view / nest_checks_table,
  app / view / summary_table,
  app / view / comparison_table,
  app / view / summary_map,
  app / view / summary_plot,
  app / view / comparison_plot,
  app / view / nest_checks_plot,
  app / logic / data_handler,
  app / logic / globals,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  dashboardPage(
    dashboardHeader(
      left = "NZPI Dashboard",
      logo_path = "static/nzpi-navy-logo.png",
      logo_align = "left",
      menu_button_label = ""
    ),
    dashboardSidebar(
      size = "thin",
      sidebarMenu(
        # can't be named. there is a bug in the package!
        # id = "main_sidebar",
        menuItem(
          tabName = "summaries",
          "Summaries",
          icon = icon("clipboard outline")
        ),
        menuItem(
          tabName = "nest_checks",
          "Nest Checks",
          icon = icon("boxes")
        ),
        menuItem(
          tabName = "comparison",
          "Comparisons",
          icon = icon("clone outline")
        )
      )
    ),
    dashboardBody(
      tabItems(

        # summaries ---------------------------------------------------------------
        tabItem(
          tabName = "summaries",
          fluidRow(
            box(
              title = "Filters",
              color = "grey",
              ribbon = T,
              selectInput(
                ns("summary_region"),
                label = "Region",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                ns("summary_area"),
                label = "Area",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                ns("summary_sites"),
                label = "Site",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                ns("summary_season"),
                label = "Season",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              toggle(ns("summary_absolute_values"),
                label = "Show counts",
                is_marked = FALSE
              )
            ),
            box(
              summary_plot$ui(ns("summary_plot")),
              title = "Plot",
              ribbon = F,
              collapsible = T
            )
          ),
          fluidRow(
            box(
              summary_table$ui(ns("summaries_table")),
              title = "Data",
              ribbon = F,
              collapsible = T
            ),
            box(
              summary_map$ui(ns("summary_map")),
              title = "Map",
              ribbon = F,
              collapsible = T
            )
          )
        ),

        # nest check --------------------------------------------------------------

        tabItem(
          tabName = "nest_checks",
          fluidRow(
            box(
              title = "Filters",
              color = "grey",
              ribbon = T,
              selectInput(
                ns("nest_checks_region"),
                label = "Regions",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                ns("nest_checks_area"),
                label = "Areas",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                ns("nest_checks_sites"),
                label = "Sites",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                ns("nest_checks_nests"),
                label = "Nests",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              )
            ),
            box(
              nest_checks_plot$ui(ns("nest_checks_plot")),
              title = "Image",
              ribbon = F,
              collapsible = T
            )
          ),
          fluidRow(
            box(
              nest_checks_table$ui(ns("nest_checks_table")),
              title = "Data",
              ribbon = F,
              collapsible = T
            ),
            box(
              summary_map$ui(ns("nest_checks_map")),
              title = "Map",
              ribbon = F,
              collapsible = T
            )
          )
        ),

        # comparison --------------------------------------------------------------

        tabItem(
          tabName = "comparison",
          fluidRow(
            box(
              title = "Filters",
              color = "grey",
              ribbon = T,
              selectInput(
                ns("comparison_region"),
                label = "Region",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                ns("comparison_area"),
                label = "Area",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                ns("comparison_sites"),
                label = "Site",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              toggle(ns("comparison_absolute_values"),
                label = "Show counts",
                is_marked = FALSE
              )
            ),
            box(
              comparison_plot$ui(ns("comparison_plot")),
              title = "Plot",
              ribbon = F,
              collapsible = T
            )
          ),
          fluidRow(
            box(
              comparison_table$ui(ns("comparison_table")),
              title = "Data",
              ribbon = F,
              collapsible = T
            ),
            box(
              summary_map$ui(ns("comparison_map")),
              title = "Map",
              ribbon = F,
              collapsible = T
            )
          )
        ),
        tags$head(
          tags$link(
            rel = "icon", 
            href = "static/favicon-96x96.png", 
            sizes = "any")
        ),
        useWaiter()
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # initialize --------------------------------------------------------------
    waiter_show(html = tagList(
      spin_flower(),
      h4("Penguins loading...")
    ) , color = "black")
    RV <- reactiveValues()

    nest_checks_data <- data_handler$load_nest_check_data()
    RV$nest_checks_data <- nest_checks_data

    nest_checks_region_choices <- nest_checks_data |>
      pull(Region) |>
      unique() |>
      sort()

    updateSelectInput(session,
      "nest_checks_region",
      choices = nest_checks_region_choices
    )

    # summary_data <- data_handler$load_data("nest_summary3")
    summary_mockup <- bind_rows(
      readRDS(here("rds", "mockup_data_2.rds")),
      readRDS(here("rds", "mockup_data_3.rds"))
      )
    
    summary_data <- summary_mockup
    comparison_data <- summary_mockup
    RV$summary_data <- summary_data
    RV$comparison_data <- comparison_data

    summary_region_choices <- summary_data |>
      pull(Region) |>
      unique() |>
      sort()

    updateSelectInput(session,
      "summary_region",
      choices = summary_region_choices
    )
    
    comparison_region_choices <- comparison_data |>
      pull(Region) |>
      unique() |>
      sort()
    
    updateSelectInput(session,
                      "comparison_region",
                      choices = comparison_region_choices
    )

    # module servers ----------------------------------------------------------

    summary_table$server(
      "summaries_table",
      reactive(
        RV$summary_data
      ),
      reactive(
        input$summary_absolute_values
      )
    )
    
    comparison_table$server(
      "comparison_table",
      reactive(
        RV$comparison_data
      ),
      reactive(
        input$comparison_absolute_values
      )
    )

    summary_map$server(
      "summary_map",
      reactive(
        RV$summary_data
      )
    )
    
    summary_map$server(
      "nest_checks_map",
      reactive(
        RV$nest_checks_data
      )
    )

    summary_plot$server(
      "summary_plot",
      reactive(
        RV$summary_data
      )
    )
    
    #comparison_plot$server(
     # "comparison_plot",
    #  reactive(
   #     RV$comparison_data
  #    )
 #   )
    
    comparison_table$server(
      "comparison_table",
      reactive(
        RV$comparison_data
      ),
      reactive(
        input$comparison_absolute_values
      )
    )
    
    summary_map$server(
      "comparison_map",
      reactive(
        RV$comparison_data
      )
    )
    
    comparison_plot$server(
      "comparison_plot",
      reactive(
        RV$comparison_data
      ),
      reactive(
        input$comparison_absolute_values
      )
    )

    nest_checks_table$server(
      "nest_checks_table",
      reactive(
        RV$nest_checks_data
      )
    )
    waiter_hide()
    
    nest_checks_plot$server(
      "nest_checks_plot",
      reactive(
        RV$nest_checks_data
      )
    )

    # Observers ---------------------------------------------------------------

    observeEvent(input$nest_checks_nests, {
      RV$nest_checks_data <- data_handler$update_nest_data(
        nest_checks_data,
        list(
          site_filter = input$nest_checks_sites,
          nest_filter = input$nest_checks_nests
        )
      )
    })

    observeEvent(input$summary_season, {
      RV$summary_data <- data_handler$update_summary_data(
        summary_data,
        list(
          area_filter = input$summary_area,
          site_filter = input$summary_sites,
          season_filter = input$summary_season
        )
      )
    })
    
    observeEvent(input$comparison_sites, {
      RV$comparison_data <- data_handler$update_comparison_data(
        summary_data,
        list(
          area_filter = input$comparison_area,
          site_filter = input$comparison_sites
        )
      )
    })


    # summary filters ---------------------------------------------------------

    observeEvent(input$summary_region, {
      updateSelectInput(session,
        "summary_area",
        choices = summary_data |>
          filter(Region %in% input$summary_region) |>
          pull(Area) |>
          unique() |>
          sort()
      )
    })

    observeEvent(input$summary_area, {
      updateSelectInput(session,
        "summary_sites",
        choices = summary_data |>
          filter(
            Region %in% input$summary_region,
            Area %in% input$summary_area
          ) |>
          pull(Site) |>
          unique() |>
          sort()
      )
    })

    observeEvent(input$summary_sites, {
      updateSelectInput(session,
        "summary_season",
        choices = summary_data |>
          filter(
            Region %in% input$summary_region,
            Area %in% input$summary_area,
            Site %in% input$summary_sites
          ) |>
          pull(year) |>
          unique() |>
          sort()
      )
    })

    # Nest check filters ------------------------------------------------------

    observeEvent(input$nest_checks_region, {
      updateSelectInput(session,
        "nest_checks_area",
        choices = nest_checks_data |>
          filter(Region %in% input$nest_checks_region) |>
          pull(Area) |>
          unique() |>
          sort()
      )
    })

    observeEvent(input$nest_checks_area, {
      updateSelectInput(session,
        "nest_checks_sites",
        choices = nest_checks_data |>
          filter(
            Region %in% input$nest_checks_region,
            Area %in% input$nest_checks_area
          ) |>
          pull(Site) |>
          unique() |>
          sort()
      )
    })

    observeEvent(input$nest_checks_sites, {
      updateSelectInput(session,
        "nest_checks_nests",
        choices = nest_checks_data |>
          filter(
            Region %in% input$nest_checks_region,
            Area %in% input$nest_checks_area,
            Site %in% input$nest_checks_sites
          ) |>
          pull(Nest) |>
          unique() |>
          sort()
      )
    })
    
    # comparison filters ---------------------------------------------------------
    
    observeEvent(input$comparison_region, {
      updateSelectInput(session,
                        "comparison_area",
                        choices = comparison_data |>
                          filter(Region %in% input$comparison_region) |>
                          pull(Area) |>
                          unique() |>
                          sort()
      )
    })
    
    observeEvent(input$comparison_area, {
      updateSelectInput(session,
                        "comparison_sites",
                        choices = comparison_data |>
                          filter(
                            Region %in% input$comparison_region,
                            Area %in% input$comparison_area
                          ) |>
                          pull(Site) |>
                          unique() |>
                          sort()
      )
    })
    
    observeEvent(input$comparison_sites, {
      updateSelectInput(session,
                        "comparison_season",
                        choices = comparison_data |>
                          filter(
                            Region %in% input$comparison_region,
                            Area %in% input$comparison_area,
                            Site %in% input$comparison_sites
                          ) |>
                          pull(year) |>
                          unique() |>
                          sort()
      )
    })
  })
}
