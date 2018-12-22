ui <- dashboardPage(
  dashboardHeader(title = "Dashboard",
                  dropdownMenu(type = 'message'),
                  dropdownMenu(type = 'notification'),
                  dropdownMenu(type = 'task')),
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed; overflow: invisible; white-space: nowrap; width:230px;",
      menuItem("Rating Analysis", tabName = "tab1"))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tab1",
        fluidRow(
          box(plotlyOutput('Visual3'), width = 6),
          box(plotlyOutput('Visual4'), width = 6),
          box(uiOutput('ratingtext_input_ui'), width = 4),
          box(uiOutput('rating_input_ui'), width = 8),
          tabsetPanel(
            tabPanel(title = "Count",
                     box(plotOutput('Visual1'), width = 6),
                     box(plotlyOutput('Visual6'), width = 6),
                     box(plotlyOutput('Visual5'), width = 12)
                     ),
            tabPanel(title = "Avg Cost",
                     box(uiOutput('country_filter_ui'), width = NULL),
                     plotlyOutput('Visual2', height = 500)
                     )
            )
          )
        )
      )
    )
  )