shinyServer(function(input, output, session) {
    surveydata = reactive({
        req(input$sfile)
        tryCatch({
            # TODO uncomment encoding to deploy! (linux encoding)
            df <- read.csv(input$sfile$datapath, encoding = 'UTF-8', fileEncoding = 'ISO8859-1', 
                           header = T,
                           sep = ",",
                           quote = '"',
                           col.names = c("date", "visited", "Smodel", "expectations", "missed", "overall", 
                                         "notes", "pres_qual", "material", "contents", "career", "projectinfo",
                                         "labvisits", "talk_prof", "talk_stud", "talk_other", 
                                         "desicion", "desicion_missing", "infodays_yn", "infodays_which", 
                                         "infodays_other", "abroadsemester", "abroadsemester_where", 
                                         "abroadsemester_other", "informed", "informed_other", "socialmedia", "education", 
                                         "aprenticeship", "employer", "reminder_yn", "reminder_email",
                                         "gender", "lastname", "firstname", "email")) 
        },
            error = function(e){stop(safeError(e))}
        )
    })
    # ------------------------ Upload Tab -------------------
    output$contents <- DT::renderDataTable({
        surveydata()
    })
    # ------------------------ Studiengang Tabs -------------------
    callModule(eval_sg, "all", dataset = surveydata(), studiengang = "All")
    callModule(eval_sg, "AV", dataset = surveydata(), studiengang = "Aviatik")
    callModule(eval_sg, "ET", dataset = surveydata(), studiengang = "Elektrotechnik")
    callModule(eval_sg, "EU", dataset = surveydata(), studiengang = "Energie- und Umwelttechnik")
    callModule(eval_sg, "IT", dataset = surveydata(), studiengang = "Informatik")
    callModule(eval_sg, "MT", dataset = surveydata(), studiengang = "Maschinentechnik")
    callModule(eval_sg, "ST", dataset = surveydata(), studiengang = "Systemtechnik")
    callModule(eval_sg, "VS", dataset = surveydata(), studiengang = "Verkehrssysteme")
    callModule(eval_sg, "WI", dataset = surveydata(), studiengang = "Wirtschaftsingenieurwesen")
})

