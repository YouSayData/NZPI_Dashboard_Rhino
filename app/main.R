box::use(
  shiny[
    enableBookmarking,
    bookmarkButton,
    moduleServer,
    NS,
    fluidRow,
    h4,
    observe,
    observeEvent,
    reactiveValues,
    isolate,
    reactive,
    reactiveValuesToList,
    req,
    tagList,
    tags,
  ],
  shiny.semantic[
    file_input,
    selectInput,
    button,
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
  shinymanager[
    check_credentials,
    secure_app,
    secure_server,
  ],
  dplyr[
    as_tibble,
    tibble,
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
  openxlsx[
    read.xlsx,
    write.xlsx,
  ],
  stringr[
    str_replace_all,
    str_to_lower,
  ],
  here[
    here,
  ],
  pins[
    board_folder,
    pin_read,
  ]
)

box::use(
  app / view / nest_checks_table,
  app / view / summary_table,
  app / view / comparison_table,
  app / view / summary_map,
  app / view / nest_checks_map,
  app / view / summary_plot,
  app / view / comparison_plot,
  app / view / nest_checks_plot,
  app / logic / data_handler,
  app / logic / globals,
)

#' @export
ui <- secure_app(dashboardPage(
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
        ),
        menuItem(
          tabName = "visualise_own",
          "Your Data",
          icon = icon("chart bar outline")
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
                "summary_region",
                label = "Region",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                "summary_area",
                label = "Area",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                "summary_sites",
                label = "Site",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                "summary_season",
                label = "Season",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              toggle("summary_absolute_values",
                label = "Show counts",
                is_marked = FALSE
              )
            ),
            box(
              summary_plot$ui("summary_plot"),
              title = "Plot",
              ribbon = F,
              collapsible = T
            )
          ),
          fluidRow(
            box(
              summary_table$ui("summaries_table"),
              title = "Data",
              ribbon = F,
              collapsible = T
            ),
            box(
              summary_map$ui("summary_map"),
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
                "nest_checks_region",
                label = "Regions",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                "nest_checks_area",
                label = "Areas",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                "nest_checks_sites",
                label = "Sites",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                "nest_checks_nests",
                label = "Nests",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              )
            ),
            box(
              nest_checks_plot$ui("nest_checks_plot"),
              title = "Image",
              ribbon = F,
              collapsible = T
            )
          ),
          fluidRow(
            box(
              nest_checks_table$ui("nest_checks_table"),
              title = "Data",
              ribbon = F,
              collapsible = T
            ),
            box(
              summary_map$ui("nest_checks_map"),
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
                "comparison_region",
                label = "Region",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                "comparison_area",
                label = "Area",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              selectInput(
                "comparison_sites",
                label = "Site",
                choices = "",
                selected = NULL,
                multiple = F,
                type = "search selection"
              ),
              toggle("comparison_absolute_values",
                label = "Show counts",
                is_marked = FALSE
              )
            ),
            box(
              comparison_plot$ui("comparison_plot"),
              title = "Plot",
              ribbon = F,
              collapsible = T
            )
          ),
          fluidRow(
            box(
              comparison_table$ui("comparison_table"),
              title = "Data",
              ribbon = F,
              collapsible = T
            ),
            box(
              summary_map$ui("comparison_map"),
              title = "Map",
              ribbon = F,
              collapsible = T
            )
          )
        ),

# Visualise own data ------------------------------------------------------

tabItem(
  tabName = "visualise_own",
  fluidRow(
    box(
      title = "Filters",
      color = "grey",
      ribbon = T,
      file_input(
        "own_data_file",
        label = "",
        accept = "xlsx",
        multiple = F
      ),
      button("visualise_button", "Visualise"),
      toggle("own_data_absolute_values",
             label = "Show counts",
             is_marked = FALSE
      )
    ),
    box(
      summary_plot$ui("own_data_plot"),
      title = "Plot",
      ribbon = F,
      collapsible = T
    )
  ),
  fluidRow(
    box(
      summary_table$ui("own_data_table"),
      title = "Data",
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
)

#' @export
server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials(
      {
        board <- board_folder(here("pins"))
        pin_read(board, "auth_pin")
      }
    )
  )
    # initialize --------------------------------------------------------------
    waiter_show(html = tagList(
      spin_flower(),
      h4("Penguins loading...")
    ) , color = "black")
  
  nest_checks_data <- data_handler$load_data("nest_checks")
  
  RV <- reactiveValues()
  
  observeEvent(res_auth$user, 
               {
                 board <- board_folder(here("pins"))
                 
                 access <- pin_read(board, "auth_pin") |> 
                   filter(user == res_auth$user) |> 
                   pull(access)
                 
                 RV$access <- access
                 if (access != "all") {
                   RV$nest_checks_data <- nest_checks_data |>
                       filter(
                         str_to_lower(Region) == access
                       )
                   } else {
                     RV$nest_checks_data <- nest_checks_data
                   }
                 RV$nest_checks_region_choices <- RV$nest_checks_data |>
                   pull(Region) |>
                   unique() |>
                   sort()
               })
     
     observeEvent(RV$nest_checks_region_choices, {
       updateSelectInput(session,
                         "nest_checks_region",
                         choices = RV$nest_checks_region_choices
       )
     })

    # summary_data <- data_handler$load_data("nest_summary3")
    summary_mockup <- bind_rows(
      readRDS(here("rds", "mockup_data_2.rds")),
      readRDS(here("rds", "mockup_data_3.rds"))
      )
    
    summary_data <- data_handler$load_summary_data("site_summary")
    comparison_data <- data_handler$load_summary_data("site_comparison")
    
    RV$summary_data <- summary_data
    RV$comparison_data <- comparison_data
    RV$own_data <- tibble()

    summary_region_choices <- summary_data |>
      pull(Region) |>
      unique() |>
      sort()

    updateSelectInput(session,
      "summary_region",
      choices = summary_region_choices,
      selected = "Wellington"
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
    
    nest_checks_map$server(
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
    
    summary_table$server(
      "own_data_table",
      reactive(
        RV$own_data
      ),
      reactive(
        input$own_data_absolute_values
      )
    )
    
    summary_plot$server(
      "own_data_plot",
      reactive(
        RV$own_data
      )
    )

    # Observers ---------------------------------------------------------------
    
    observeEvent(input$visualise_button, {
      req(input$own_data_file)
      data <- read.xlsx(input$own_data_file$datapath) |> 
        as_tibble()
      
      colnames(data) <- str_replace_all(colnames(data), "\\.", " ")

      RV$own_data <- data
    })

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
}
