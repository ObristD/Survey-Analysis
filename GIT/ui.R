sidebar <- dashboardSidebar(
    sidebarMenu(style = "position: fixed; overflow: visible;",
        menuItem("Upload", tabName = "upload", icon = icon("upload")),
        menuItem("Alle", tabName = "all", icon = icon("list")),
        menuItem("Aviatik", tabName = "AV", icon = icon("plane")),
        menuItem("Elektrotechnik", tabName = "ET", icon = icon("bolt")),
        menuItem("Energie- und Umwelttechnik", tabName = "EU", icon = icon("globe-americas")),
        menuItem("Informatik", tabName = "IT", icon = icon("laptop-code")),
        menuItem("Maschinentechnik", tabName = "MT", icon = icon("cogs")),
        menuItem("Systemtechnik", tabName = "ST", icon = icon("charging-station")),
        menuItem("Verkehrssysteme", tabName = "VS", icon = icon("subway")),
        menuItem("Wirtschaftsingenieurwesen", tabName = "WI", icon = icon("chart-line")),
        
        tags$footer(
            img(src="soe.png", height = 90),
            align = "right", style = "
              position:absolute;
              bottom: 1;
              padding: 20px;
              ")
    ))

body <- dashboardBody(
    tags$head(tags$style(HTML('
            /* logo */
            .skin-blue .main-header .logo {background-color: #0164A5;}
            /* logo when hovered */
            .skin-blue .main-header .logo:hover {background-color: #0164A5;}
            /* navbar (rest of the header) */
            .skin-blue .main-header .navbar {background-color: #0164A5;}
                                   '))),      
    fluidPage(
        tabItems(
            tabItem(
                tabName = "upload",
                fluidRow( 
                    box(width = 12,
                        fileInput("sfile", "CSV Datei einlesen",
                                  multiple = FALSE,
                                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                    )
                ),
                fluidRow(
                    box(width = 12,
                        div(style = 'overflow-x: scroll', DT::dataTableOutput("contents"))
                    )

                )
            ),
            eval_sgUI("all"),
            eval_sgUI("AV"),
            eval_sgUI("ET"),
            eval_sgUI("EU"),
            eval_sgUI("IT"),
            eval_sgUI("MT"),
            eval_sgUI("ST"),
            eval_sgUI("VS"),
            eval_sgUI("WI")
        ) 
    )
) 

shinyUI(
    dashboardPage(
        dashboardHeader(title = "Auswertung Infotag", titleWidth = 250),
        sidebar,
        body
    ))

