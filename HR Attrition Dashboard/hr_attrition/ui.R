#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

header <- dashboardHeader(
    title = "HR Attrition",
    tags$li(class = "dropdown", 
            tags$a(href = "https://www.linkedin.com/in/nashrullah-66710014b/",
                   icon("linkedin"),
                   "My Profile",
                   target = "_blank"))
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Company Attrition Report",
                 tabName = "report",
                 icon = icon("file-invoice")),
        menuItem(text = "Important Features",
                 tabName = "important_features",
                 icon = icon("exclamation-triangle")),
        menuItem(text = "Attrition Segementation",
                 tabName = "attr_segementation",
                 icon = icon("podcast")),
        menuItem(text = "Dataset",
                 tabName = "dataset",
                 icon = icon("database"))
    )
)

body <- dashboardBody(
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #CF395C}")),
    tags$style(HTML(".irs-grid-text {font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}")),
    tags$style(HTML(".irs-grid-pol {display: none;}")),
    tags$style(HTML(".irs-max {font-family: 'arial'; color: white;}")),
    tags$style(HTML(".irs-min {font-family: 'arial'; color: white;}")),
    tabItems(
        tabItem(
            tabName = "report",
            fluidRow(
                valueBoxOutput("attr_rate", width = 6),
                valueBoxOutput("tot_employee", width = 6)
            ),
            fluidRow(
                box(background = "purple",
                    width = 12,
                    height = 450,
                    solidHeader = T,
                    box(width = 4,
                        plotlyOutput("department_ratio", height=400)),
                    box(width = 8,
                        plotlyOutput("attr_composition", height=400))),
                
                box(background = "purple",
                    width = 12,
                    height = 450,
                    solidHeader = T,
                    box(width = 9,
                        plotlyOutput("department_agg", height = 300)),
                    
                    box(width = 3,
                        h1("Select the aggregate column:"),
                        radioButtons(inputId = "agg_col",
                                     label = "",
                                     choices = c("Age Category", "Education", 
                                                 "Job Involvement", "Job Level", 
                                                 "Stock Option Level", 
                                                 "Years At Company Category",
                                                 "Years In Current Role Category"),
                                     selected = "Age Category"))
                )
            )
        ),
        tabItem(
            tabName = "important_features",
            fluidRow(
                
                box(background = "purple",
                    width = 8,
                    plotlyOutput("top_feature", height = 550)),
                
                column(width = 4,
                       box(
                           title = tags$span("Number of Features", style = "color: white;"),
                           background = "purple",
                           solidHeader = T,
                           width = 12,
                           sliderInput(inputId = "n_features", 
                                       label = tags$span("Select n most important features:", style = "color: white;"),
                                       min = 1,
                                       max = dim(employee)[2],
                                       value = 20)),
                       box(width = 12,
                           height = 280,
                           background = "purple",
                           h3("INSIGHT!", style="color:white"),
                           div(style = "text-align:justify",
                               p("By knowing the features that most influence attrition, we can delve deeper into these features to find out the causes of attrition in companies."),
                               br(), 
                               p(tags$b("Note:")), 
                               p(" The decision to leave a company is multivariate: There is no single cause. And not all employees leave for the same reasons.")
                               )
                           )
                       )
                )
            ),
        tabItem(
            tabName = "attr_segementation",
            fluidRow(
                tags$style(HTML("
        .tabbable > .nav > li > a {background-color: white;  color:black;}
        .tabbable > .nav > li[class=active]    > a {background-color: #CF395C; color:white}
  ")),
                column(width = 9,
                       box(
                           width = 12,
                           background = "purple",
                           checkboxGroupInput(inputId = "features", 
                                              label = tags$span("Select features:", style = "color: white; font-size:xx-large;"),
                                              choiceNames = checkbox.names(),
                                              choiceValues = X.colnames,
                                              selected = X.colnames,
                                              inline = T),
                           br(),
                           actionButton("updateSegment", "Update Segment!"),
                           br(),
                           br(),
                           tags$b("Note:"),
                           p("You must select at least 2 features to avoid errors!")
                       ),
                       
                       box(
                           width = 12,
                           background = "purple",
                           uiOutput("segmentation")),
                ),
                
                box(width = 3,
                    height = 420,
                    background = "purple",
                    h2("INSIGHT!", style="color:white"),
                    div(style = "text-align:justify",
                        p(
                            "Segmentation is the practice of grouping employees into 
              different groups based on certain general characteristics. 
              This process is very important when it comes to the quality of 
              your people analytics. If you have too many segments, you'll have 
              thousands of ways to view the same metrics. That, of course, leads 
              to lengthy reports, dashboards that require a lot of user 
              filtering, and / or fatigue-alerts. If you have too few 
              segments, you can fail to see meaningful insights."
                        ),
                        p(
                            "You can divide employee attrition rates into job types, years 
              of service, location, demographic segment, behavioral 
              segment, age segment, attitude segment, or other meaningful 
              segment."
                        ), 
                        p(
                            "Finally, you can plan advanced strategies that are more 
                targeted at each employee segment."
                        ),
                        br()
                    )
                )
            )
        ),
        tabItem(
            tabName = "dataset",
            dataTableOutput("datasets")
        )
    )
)

ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body,
    shinyDashboardThemes(
        theme = "flat_red"
    )
)

