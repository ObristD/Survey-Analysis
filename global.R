library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(shinycssloaders)


#' @author Dario Obrist
#' @description returns subset of all rows in dataframe
#' where row \code{visited} contains the word \code{sg}
#' @param df (dataframe) to take subset from
#' @param sg (string) to subset
#' @return subset of the dataframe
get_subset <- function(df, sg){
  if (sg != "All"){
    return(df[grep(sg, df$visited), ])
  }
  else{
    return(df)
  }
}

#' @author Dario Obrist
#' @description geretates ggplot barchart based on \code{df} and \code{feature}
#' @param df (dataframe) to generate plot from
#' @param feature (string) feature to select for barchart
#' @param xticks (vector)scale_discrete limits if NULL alphabetical order is used
#' @param mlabels (vector) manual labels, if set the levels of the feature are set 
#' @param tsize (int) size or text in plot (lable and axis)
#' as the inputvector of \code{mlables} and \code{xticks} is set to mlabels
#' @return barplot
get_plt <- function(df, feature, xticks = NULL, mlabels = NULL, tsize = 15){
  df = df[df[, feature] != "", ]
  df[, feature] = factor(df[, feature])
  
  if (!is.null(mlabels)){
    levels(df[, feature]) <- mlabels
    xticks = mlabels
  }
  
  ggplot(df, aes_string(x = feature)) +
    geom_bar(fill = "#0064A8", na.rm = T) +
    labs(y = "Anzahl", x = "") +
    scale_x_discrete(limits = xticks) +
    theme(axis.text.x = element_text(size = tsize, angle = 30, hjust = 0),
          axis.text.y = element_text(size = tsize),
          axis.title.y = element_text(size = tsize), 
          axis.title.x = element_text(size = tsize))
}

#' @author Dario Obrist
#' @description geretates ggplot barchart based on \code{df} and \code{feature}
#' where the elements of the feature contain lists seperated with commas whose
#' elements are unlisted in otder to produce barchart
#' @param df (dataframe) to generate plot from
#' @param feature (string) feature to select for barchart
#' @param xticks (vector)scale_discrete limits if NULL alphabetical order is used
#' @param mlabels (vector) manual labels, if set the levels of the feature are set 
#' @param tsize (int) size or text in plot (lable and axis)
#' as the inputvector of \code{mlables} and \code{xticks} is set to mlabels
#' @return barplot
get_plt_agg <- function(df, feature = NULL, xticks = NULL, mlabels = NULL, tsize = 15){

  if (!is.null(feature)){
    df = data.frame(
      table(
        unlist(
          strsplit(as.character(
            df[, feature]), ", "
          )
        )
      )
    )
  }
  if (!is.null(mlabels)){
    levels(df$Var1) <- mlabels
    xticks = mlabels
  }
  ggplot(df, aes(x = Var1, y = Freq))+
    geom_bar(stat = "identity", fill = "#0064A8") +
    labs(y = "Anzahl", x = "") +
    scale_x_discrete(limits = xticks) +
    theme(axis.text.x = element_text(size = tsize, angle = 30, hjust = 0),
          axis.text.y = element_text(size = tsize),
          axis.title.y = element_text(size = tsize), 
          axis.title.x = element_text(size = tsize))
}

#' @author Dario Obrist
#' @description UI element of \code{eval_sg} module containing a 
#' infobox with the number of survey respunses per subgroup, barcharts, and tables
#' @param ns namespace for module, is to be wraped arround every UI element id!
eval_sgUI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    
    fluidRow(
      infoBoxOutput(ns("responses"))
    ),
    fluidRow(
      box(title = "Besuchte Praesentationen", 
          withSpinner(plotlyOutput(ns("visited")), type = 3, color.background = "white")
      ),
      box(title = "Interessierendes Studienmodell", 
          withSpinner(plotlyOutput(ns("Smodel")), type = 3, color.background = "white") 
      ),
      box(title = "Inwiefern hat der Infotag Ihren Erwartungen entsprochen?", 
          withSpinner(plotlyOutput(ns("expectations")), type = 3, color.background = "white")
      ),
      box(title = "Gesamteindruck Infotag", 
          withSpinner(plotlyOutput(ns("overall")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "Folgendes hat mit gefehlt", width = 12,
          withSpinner(DT::dataTableOutput(ns("missed")), type = 3, color.background = "white") 
      )
    ),
    fluidRow(
      box(title = "Bemerkungen",  width = 12,
          withSpinner(DT::dataTableOutput(ns("notes")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "Informationspraesentationen", 
          withSpinner(plotlyOutput(ns("pres_qual")), type = 3, color.background = "white")
      ),
      box(title = "Infomaterial", 
          withSpinner(plotlyOutput(ns("material")), type = 3, color.background = "white")
      ),
      box(title = "Aufbau Studium", 
          withSpinner(plotlyOutput(ns("contents")), type = 3, color.background = "white")
      ),
      box(title = "Karrieremoeglichkeiten", 
          withSpinner(plotlyOutput(ns("career")), type = 3, color.background = "white")
      ),
      box(title = "Forschungsinfo", 
          withSpinner(plotlyOutput(ns("projectinfo")), type = 3, color.background = "white")
      ),
      box(title = "Laborrundgaenge", 
          withSpinner(plotlyOutput(ns("labvisits")), type = 3, color.background = "white")
      ),
      box(title = "Gespraeche  Studiengangsleiter / Dozenten", 
          withSpinner(plotlyOutput(ns("talk_prof")), type = 3, color.background = "white")
      ),
      box(title = "Gespraeche Studierende / Absolventen", 
          withSpinner(plotlyOutput(ns("talk_stud")), type = 3, color.background = "white")
      ),
      box(title = "Gespraeche andere Teilnehmer", 
          withSpinner(plotlyOutput(ns("talk_other")), type = 3, color.background = "white")
      ),
      box(title = "Haben Sie sich schon fuer einen Studiengang entschieden?", 
          withSpinner(plotlyOutput(ns("desicion_yn")), type = 3, color.background = "white")
      ),
      box(title = "Falls ja fuer welchen Studiengang?", 
          withSpinner(plotlyOutput(ns("desicion")), type = 3, color.background = "white")
      ),
      box(title = "Geschlecht", 
          withSpinner(plotlyOutput(ns("gender")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "Was fehlt fuer Entscheid",  width = 12,
          withSpinner(DT::dataTableOutput(ns("desicion_missing")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "Haben Sie Infotage an anderen Hochschulen besucht oder vor dies zu tun?",
          withSpinner(plotlyOutput(ns("infodays_yn")), type = 3, color.background = "white")
      ),
      box(title = "Besuchte Infotage an anderen Hochschulen",
          withSpinner(plotlyOutput(ns("infodays_which")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "Was fehlt fuer Entscheid",  width = 12,
          withSpinner(DT::dataTableOutput(ns("infodays_other")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "Interesse Auslandaufenthalt", 
          withSpinner(plotlyOutput(ns("abroadsemester")), type = 3, color.background = "white")
      ),
      box(title = "Wunschland Auslandaufenthalt",
          withSpinner(plotlyOutput(ns("abroadsemester_where")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "Andere Orte Auslandsemester",  width = 12,
          withSpinner(DT::dataTableOutput(ns("abroadsemester_other")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "Wie haben Sie sich ueber Studium an der SoE informiert",
          withSpinner(plotlyOutput(ns("informed")), type = 3, color.background = "white")
      ),
      box(title = "Vorbildung",
          withSpinner(plotlyOutput(ns("education")), type = 3, color.background = "white")
      ),
      box(title = "Social Media Benutzung", 
          withSpinner(plotlyOutput(ns("socialmedia")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "Lehre / Lehrbetrieb",  width = 12,
          withSpinner(DT::dataTableOutput(ns("aprenticeship")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "E-Mail Adressen Reminder Anmeldeschluss",  width = 12,
          withSpinner(DT::dataTableOutput(ns("reminder_email")), type = 3, color.background = "white")
      )
    ),
    fluidRow(
      box(title = "Kontaktdaten Alle",  width = 12,
          withSpinner(DT::dataTableOutput(ns("email")), type = 3, color.background = "white")
      )
    )
  ) 
}

#' @author Dario Obrist
#' @description server element of \code{eval_sg} module
#' generating subset of dataset and calling plot functions defined above
#' and sending plots to UI
#' @param input input element of UI
#' @param output output element sending onjects to UI
#' @param session required element for modules to find the current namespace
#' @param dataset passed on dataset which is red in as .csv file from UI
#' @param studiengang subset condition 
eval_sg <- function(input, output, session, dataset, studiengang = "All"){
  data_subset = reactive(get_subset(dataset, sg = studiengang))
  
  output$responses <- renderInfoBox({
    infoBox(
      "Anzahl Antworten", nrow(data_subset()), icon = icon("list"),
      color = "blue"
    )
  })
  
  output$visited <- renderPlotly({
    get_plt_agg(data_subset(), 
                feature = "visited",
                xticks = c("Aviatik", "Elektrotechnik", "Energie- und Umwelttechnik",
                           "Informatik", "Maschinentechnik", "Systemtechnik",
                           "Verkehrssysteme", "Wirtschaftsingenieurwesen"))
  })
  
  output$Smodel <- renderPlotly({
    get_plt(data_subset(), 
            feature = "Smodel", 
            mlabels = c("PIBS", "Teilzeit", "Vollzeit"))
  })
  
  output$expectations <- renderPlotly({
    get_plt(data_subset(), 
            feature = "expectations", 
            mlabels = c("Alle Angaben erhalten", "Fast alle Angaben erhalten", "Zu wenig Angaben erhalten"))
  }
  )
  
  output$overall <- renderPlotly({
    get_plt(data_subset(), 
            feature = "overall", 
            xticks = c("Sehr gut", "Gut", "Weniger gut"))
  })
  
  output$missed <- DT::renderDataTable({
    out = data_subset()[, c("missed","gender", "lastname", "firstname", "email")]
    out[out$missed != "", ]
  })
  
  output$notes <- DT::renderDataTable({
    out = data_subset()[, c("notes","gender", "lastname", "firstname", "email")]
    out[out$notes != "", ]
  })
  
  output$pres_qual <- renderPlotly({
    get_plt(data_subset(), 
            feature = "pres_qual", 
            xticks = c("Eher unwichtig", "Wichtig", "Sehr wichtig"))
  })
  
  output$material <- renderPlotly({
    get_plt(data_subset(), 
            feature = "material", 
            xticks = c("Eher unwichtig", "Wichtig", "Sehr wichtig"))
  })
  
  output$contents <- renderPlotly({
    get_plt(data_subset(), 
            feature = "contents", 
            xticks = c("Eher unwichtig", "Wichtig", "Sehr wichtig"))
  })
  
  output$career <- renderPlotly({
    get_plt(data_subset(), 
            feature = "career", 
            xticks = c("Eher unwichtig", "Wichtig", "Sehr wichtig"))
  })
  
  output$projectinfo <- renderPlotly({
    get_plt(data_subset(), 
            feature = "projectinfo", 
            xticks = c("Eher unwichtig", "Wichtig", "Sehr wichtig"))
  })
  
  output$labvisits <- renderPlotly({
    get_plt(data_subset(), 
            feature = "labvisits", 
            xticks = c("Eher unwichtig", "Wichtig", "Sehr wichtig"))
  })
  
  output$talk_prof <- renderPlotly({
    get_plt(data_subset(), 
            feature = "talk_prof", 
            xticks = c("Eher unwichtig", "Wichtig", "Sehr wichtig"))
  })
  
  output$talk_stud <- renderPlotly({
    get_plt(data_subset(), 
            feature = "talk_stud", 
            xticks = c("Eher unwichtig", "Wichtig", "Sehr wichtig"))
  })
  
  output$talk_other <- renderPlotly({
    get_plt(data_subset(), 
            feature = "talk_other", 
            xticks = c("Eher unwichtig", "Wichtig", "Sehr wichtig"))
  })
  
  output$desicion_yn <- renderPlotly({
    temp_df = data.frame(Var1 = c("Ja", "Nein"), 
                         Freq = c(length(data_subset()$desicion)- sum(data_subset()$desicion != ""), 
                                  sum(data_subset()$desicion != "")))
    get_plt_agg(temp_df)
  })
  
  output$desicion <- renderPlotly({
    get_plt(data_subset(), 
            feature = "desicion", 
            xticks = c("Aviatik", "Elektrotechnik", "Energie- und Umwelttechnik",
                       "Informatik", "Maschinentechnik", "Systemtechnik",
                       "Verkehrssysteme", "Wirtschaftsingenieurwesen"))
  })
  
  output$gender <- renderPlotly({
    temp_df = data.frame(Var1 = c("Maenner", "Frauen"), 
                         Freq = c(sum(data_subset()$gender == "Herr"), 
                                  sum(data_subset()$gender == "Frau")))
    get_plt_agg(temp_df)
  })
  
  output$desicion_missing <- DT::renderDataTable({
    out = data_subset()[, c("desicion_missing", "gender", "lastname", "firstname", "email")]
    out[out$desicion_missing != "", ]
  })
 
  output$infodays_yn <- renderPlotly({
    get_plt(data_subset(), feature = "infodays_yn")
  })
  
  output$infodays_which <- renderPlotly({
    get_plt_agg(data_subset(), feature = "infodays_which")
  })
  
  output$infodays_other <- DT::renderDataTable({
    out = data_subset()[, c("infodays_other", "gender", "lastname", "firstname", "email")]
    out[out$infodays_other != "", ]
  })
  
  output$abroadsemester <- renderPlotly({
    get_plt(data_subset(), feature = "abroadsemester")
  })
  
  output$abroadsemester_where <- renderPlotly({
    get_plt_agg(data_subset(), feature = "abroadsemester_where", tsize = 9)
  })
  
  output$abroadsemester_other <- DT::renderDataTable({
    out = data_subset()[, c("abroadsemester_other", "gender", "lastname", "firstname", "email")]
    out[out$abroadsemester_other != "", ]
  })
  
  output$informed <- renderPlotly({
    get_plt_agg(data_subset(), feature = "informed", 
                mlabels = c("BIZ", "Bildungsmesse", "Bildungsportal", 
                            "Broschueren", "Veranstaltung Schule", "Webseite"))
  })
  
  output$education <- renderPlotly({
    get_plt_agg(data_subset(), feature = "education",
                mlabels = c("BMS", "HF", "Fachdiplom", "Gymnasium", 
                            "Lehre", "FH", "FH begonnen"))
  })
  
  output$socialmedia <- renderPlotly({
    get_plt_agg(data_subset(), feature = "socialmedia")
  })
  
  output$aprenticeship <- DT::renderDataTable({
    out = data_subset()[, c("aprenticeship", "employer", "gender", "lastname", "firstname", "email")]
    out[out$aprenticeship != "", ]
  })
  
  output$reminder_email <- DT::renderDataTable({
    out = data_subset()[data_subset()$reminder_yn == "Ja", ]
    out[, c("reminder_yn", "gender", "lastname", "firstname", "reminder_email")]
  })
  
  output$email <- DT::renderDataTable({
    out = data_subset()[, c("gender", "lastname", "firstname", "email")]
    out[out$email != "", ]
  })

}