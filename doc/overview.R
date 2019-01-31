## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("cssat/oliveRconnect")

## ---- eval = FALSE-------------------------------------------------------
#  oliveRconnect::post_auth_to_oliver(oliver_email = Sys.getenv("OLIVER_ID")
#                                     ,oliver_password = Sys.getenv("OLIVER_PW")
#                                     ,oliver_env = "production")
#  

## ---- eval = FALSE-------------------------------------------------------
#  oliveRconnect::get_csv_extract(extract_name = "fss_raw.csv")

## ---- eval = FALSE-------------------------------------------------------
#  get_org_info(c(1, 185))

## ---- eval = FALSE-------------------------------------------------------
#  get_org_info(c(1, 1000))

## ---- eval = FALSE-------------------------------------------------------
#  
#  library(shiny)
#  library(oliveRconnect)
#  library(dplyr)
#  library(highcharter)
#  
#  # Define UI for application that draws a barchart
#  ui <- fluidPage(
#  
#     # Application title
#     titlePanel("Family Support Services Timeliness Stats"),
#  
#     # Sidebar with a select input for provider
#     sidebarLayout(
#        sidebarPanel(
#          selectizeInput(inputId = "provider_select"
#                         ,label = "Provider"
#                         ,choices = NULL
#                         ,multiple = FALSE)
#        ),
#  
#        # Show a plot of the generated distribution
#        mainPanel(
#          highchartOutput('metricplot')
#  
#        )
#     )
#  )
#  
#  # Define server logic required to draw a histogram
#  server <- function(input, output, session) {
#  
#    values <- reactiveValues(authenticated = FALSE)
#  
#    # Return the UI for a modal dialog with data selection input. If 'failed'
#    # is TRUE, then display a message that the previous value was invalid.
#    loginModal <- function(failed = FALSE) {
#      modalDialog(title = "Enter Oliver Credentials",
#                  textInput("username", "Username:"),
#                  passwordInput("password", "Password:"),
#                  selectInput("environment", "Environment:"
#                              ,choices = c("staging", "production")
#                              ,selected = "production"),
#                  footer = tagList(
#                    actionButton("ok", "OK")
#                  )
#      )
#    }
#  
#    # Show modal when app is loaded
#    # This `observe` is suspended only with right user credentials
#  
#    obs1 <- observe({
#      showModal(loginModal())
#    })
#  
#    # When OK button is pressed, attempt to authenticate. If Oliver
#    # returns 200, remove the modal
#  
#    obs2 <- observe({
#      req(input$ok)
#      isolate({
#        Username <- input$username
#        Password <- input$password
#        Environment <- input$environment
#  
#      })
#  
#      post_auth_to_oliver(oliver_email = Username
#                          ,oliver_password = Password
#                          ,oliver_env = Environment)
#  
#      if (authentication_objects$session$status_code == 200) {
#        #Logged <<- TRUE
#        values$authenticated <- TRUE
#        obs1$suspend()
#        removeModal()
#  
#      } else {
#        values$authenticated <- FALSE
#      }
#    })
#  
#  
#    fss_raw <- reactive({
#      if (values$authenticated) {
#        dat_raw_timing <- get_csv_extract("fss_raw.csv") %>%
#          dplyr::filter(!is.na(service_confirmation)) %>%
#          dplyr::mutate(initial_contact_observed = ifelse(is.na(initial_contact), 0, 1)
#                        ,first_ftf_observed = ifelse(is.na(first_face_to_face), 0, 1)
#                        ,initial_contact = dplyr::coalesce(initial_contact
#                                                           ,first_face_to_face
#                                                           ,end_of_service
#                                                           ,lubridate::now())
#                        ,first_face_to_face = dplyr::coalesce(first_face_to_face
#                                                              ,end_of_service
#                                                              ,lubridate::now())
#                        ,hours_to_fc = difftime(lubridate::as_datetime(initial_contact)
#                                                ,lubridate::as_datetime(service_confirmation)
#                                                ,units = "hours")
#                        ,hours_to_fftf = difftime(lubridate::as_datetime(first_face_to_face)
#                                                  ,lubridate::as_datetime(initial_contact)
#                                                  ,units = "hours")
#                        ,hours_to_fftf = lubridate::as.duration(hours_to_fftf)
#                        ,hours_to_fc = lubridate::as.duration(hours_to_fc)
#          ) %>%
#          dplyr::select(oliver_referral_id, hours_to_fc, hours_to_fftf
#                        ,initial_contact_observed, first_ftf_observed
#                        ,org_id)
#      }
#      })
#  
#    org_list <- reactive({
#  
#      if (values$authenticated) {
#  
#        provider_vector <- fss_raw() %>%
#          select(org_id) %>%
#          distinct() %>%
#          .$org_id
#  
#        oliveRconnect::get_org_info(provider_vector)
#  
#      } else {
#  
#        c(`No Organization Returned` = 1)
#  
#      }
#  
#    })
#  
#    observeEvent(org_list(), {
#  
#      updateSelectizeInput(session = session
#                           ,inputId = "provider_select"
#                           ,choices = org_list())
#  
#    })
#  
#  
#    km_medians <- reactive({
#  
#      if (values$authenticated) {
#  
#        fit_fc <- survival::survfit(survival::Surv(time = hours_to_fc
#                                                   ,event = initial_contact_observed) ~ org_id
#                                    ,data = dat_raw_timing)
#        fit_fftf <- survival::survfit(survival::Surv(time = hours_to_fftf
#                                                     ,event = first_ftf_observed) ~ org_id
#                                      ,data = dat_raw_timing)
#        fit_fc <- survival::survfit(survival::Surv(time = hours_to_fc
#                                                   ,event = initial_contact_observed) ~ org_id,
#                                    data = fss_raw())
#        fit_fftf <- survival::survfit(survival::Surv(time = hours_to_fftf
#                                                     ,event = first_ftf_observed) ~ org_id,
#                                      data = fss_raw())
#        fit_list <- list(fc = fit_fc
#                         ,fftf = fit_fftf)
#        survminer::surv_median(fit = fit_list
#                               ,combine = TRUE) %>%
#          dplyr::mutate(median = lubridate::duration(num = median
#                                                     ,units = "second")/lubridate::dhours(1)
#                        ,strata = readr::parse_number(strata))
#  
#        } else {
#  
#        dplyr::data_frame(strata = 1, median = 0, id = c("fc", "fftf"))
#  
#        }
#  
#    })
#  
#    output$metricplot <- renderHighchart({
#  
#      highchart() %>%
#        hc_chart(type = 'column') %>%
#        hc_legend(enabled = FALSE) %>%
#        hc_xAxis(categories = c('First Contact', 'First Face-to-Face Contact')
#                 ,title = list(text = 'Measurement')) %>%
#        hc_yAxis(title = list(text = 'Median Hours to Event')) %>%
#        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
#        hc_add_series(data = c(round(filter(km_medians()
#                                            ,strata == input$provider_select, id == "fc")
#                                     %>% .$median, 2),
#                               round(filter(km_medians()
#                                            ,strata == input$provider_select, id == "fftf")
#                                     %>% .$median, 2))) %>%
#        hc_title(text = paste0('Provider Stats for '
#                               ,names(provider_labels[provider_labels == input$provider_select]))
#                 ,align = 'left') %>%
#        hc_add_theme(hc_theme_gridlight()) %>%
#        hc_colors(c('#d01010', '#d01010')) %>%
#        hc_tooltip(enabled = FALSE)
#  
#    })
#  
#  
#  }
#  
#  # Run the application
#  shinyApp(ui = ui, server = server)

