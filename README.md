# Getting Data From the Oliver API
## Joe Mienko


## Overview

`oliveRconnect` is a package providing a set of functions that help you connect to the Oliver api undergirding the Oliver service management solution [www.oliverservices.org](www.oliverservices.org). 

### Authentication Helper Functions 

* `check_for_200()` is a wrapper function for `httr::status_code()` which captures non-200 status codes. 
* `extract_poc_token()` extracts the "poc.t" cookie from an oliver authentication response.
* `return_subdomain()` stores the subdomain.domain combos for for all Oliver environments.
* `post_auth_to_oliver()` GETs a "poc.t" cookie and POSTs the cookie back to Oliver to return a session cookie.

### Functions to GET oliver data

* `get_csv_extract()` GETs one of four csv files available from the ppm section of the Oliver API. 
* `get_org_info()` GETs provider information from the organization section of the Oliver API. 

## Installation


```r
# install.packages("devtools")
devtools::install_github("cssat/oliveRconnect")
```

## Prerequisites 

In order to access the Oliver API, you will need to have an active Oliver account. To register for an Oliver account, follow the instructions at the following link: [https://www.oliverservices.org/register](https://www.oliverservices.org/register). At this time, most of the functions have been designed for a user with org_admin, super_admin privileges, or both sets of privileges. Once you have an Oliver username and password, it is best-practice to assign them to environment variables. The examples below assume you have assigned your Oliver username (i.e. email address) and password are assigned to environment variables named `OLIVER_ID` and `OLIVER_PW` respectively. 

## Authentication 

The code chunk shows how to pass these parameters to `post_auth_to_oliver()`. If successful, this function will return the message `oliver API authentication established`. This tells you that you have connected to Oliver and stored session cookies within your Global R environment. 


```r
oliveRconnect::post_auth_to_oliver(oliver_email = Sys.getenv("OLIVER_ID")
                                   ,oliver_password = Sys.getenv("OLIVER_PW")
                                   ,oliver_env = "production")
```

## Getting Data

The following two functions return data from the Oliver API. 

### CSV Result from Replica Database


```r
oliveRconnect::get_csv_extract(extract_name = "fss_raw.csv")
```

Other options for the `extract_name` parameter within `get_csv_extract()` include `fss_extract.csv`, `notes_extract.csv`, and `grassroots_diff.csv`.

### Parsed JSON Directly from the API

Once a user has been authenticated, you can also parse JSON directly from the API. An example function for doing so is `get_org_info()`. This function expects an integer vector of organization IDs. If the user has access to all of the organization IDs, the function will return a named vector of those IDs. 


```r
get_org_info(c(1, 185))
```

> Partners for Our Children Grassroots Therapy Group. LLC 

>                        1                           185 

If the user does not have access, the function will return as much information as is available. 


```r
get_org_info(c(1, 1000))
```

> Partners for Our Children         Org Not Available 

>                        1                        NA         

## Putting it Together

These individual functions can also be combined with Shiny to produce a visualization of some basic Quality Assurance statistics within the Combined In-Home Services domain. 


```r
library(shiny)
library(oliveRconnect)
library(dplyr)
library(quantmod)
library(xts)
library(TTR)
library(highcharter)
library(survminer)

# Define UI for application that draws a barchart
ui <- fluidPage(
  
  # Application title
  titlePanel("Family Support Services Timeliness Stats"),
  
  # Sidebar with a select input for provider
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "provider_select"
                     ,label = "Provider"
                     ,choices = NULL
                     ,multiple = FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      highchartOutput('metricplot')
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  loginModal <- function(failed = FALSE) {
    modalDialog(title = "Enter Oliver Credentials",
                textInput("username", "Username:"),
                passwordInput("password", "Password:"),
                selectInput("environment", "Environment:"
                            ,choices = c("staging", "production")
                            ,selected = "production"),
                footer = tagList(
                  actionButton("ok", "OK")
                )
    )
  }
  
  # Show modal when app is loaded  
  # This `observe` is suspended only with right user credentials
  
  obs1 <- observe({
    showModal(loginModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If Oliver
  # returns 200, remove the modal
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
      Environment <- input$environment
      
    })
    
    post_auth_to_oliver(oliver_email = Username
                        ,oliver_password = Password
                        ,oliver_env = Environment)
    
    if (authentication_objects$session$status_code == 200) {
      #Logged <<- TRUE
      values$authenticated <- TRUE
      obs1$suspend()
      removeModal()
      
    } else {
      values$authenticated <- FALSE
    }     
  })
  
  
  fss_raw <- reactive({
    if (values$authenticated) {
      get_csv_extract("fss_raw.csv") %>%
        dplyr::filter(!is.na(service_confirmation)) %>%
        dplyr::mutate(initial_contact_observed = ifelse(is.na(initial_contact), 0, 1)
                      ,first_ftf_observed = ifelse(is.na(first_face_to_face), 0, 1)
                      ,initial_contact = dplyr::coalesce(initial_contact
                                                         ,first_face_to_face
                                                         ,end_of_service
                                                         ,lubridate::now())
                      ,first_face_to_face = dplyr::coalesce(first_face_to_face
                                                            ,end_of_service
                                                            ,lubridate::now())
                      ,hours_to_fc = difftime(lubridate::as_datetime(initial_contact)
                                              ,lubridate::as_datetime(service_confirmation)
                                              ,units = "hours")
                      ,hours_to_fftf = difftime(lubridate::as_datetime(first_face_to_face)
                                                ,lubridate::as_datetime(initial_contact)
                                                ,units = "hours")
                      ,hours_to_fftf = lubridate::as.duration(hours_to_fftf)
                      ,hours_to_fc = lubridate::as.duration(hours_to_fc)
        ) %>%  
        dplyr::select(oliver_referral_id, hours_to_fc, hours_to_fftf
                      ,initial_contact_observed, first_ftf_observed
                      ,org_id) 
    } 
  })
  
  org_list <- reactive({
    
    if (values$authenticated) {
      
      provider_vector <- fss_raw() %>%
        select(org_id) %>%
        distinct() %>%
        .$org_id
      
      oliveRconnect::get_org_info(provider_vector)
      
    } else {
      
      c(`No Organization Returned` = 1)
      
    }
    
  })
  
  observeEvent(org_list(), {
    
    updateSelectizeInput(session = session
                         ,inputId = "provider_select"
                         ,choices = org_list())
    
  })
  
  
  km_medians <- reactive({
    
    if (values$authenticated) {

      fit_fc <- survival::survfit(survival::Surv(time = hours_to_fc
                                                 ,event = initial_contact_observed) ~ org_id, 
                                  data = fss_raw())
      fit_fftf <- survival::survfit(survival::Surv(time = hours_to_fftf
                                                   ,event = first_ftf_observed) ~ org_id, 
                                    data = fss_raw())
      fit_list <- list(fc = fit_fc
                       ,fftf = fit_fftf)
      survminer::surv_median(fit = fit_list
                             ,combine = TRUE) %>%
        dplyr::mutate(median = lubridate::duration(num = median
                                                   ,units = "second")/lubridate::dhours(1)
                      ,strata = readr::parse_number(strata)) 
      
    } else {
      
      dplyr::data_frame(strata = 1, median = 0, id = c("fc", "fftf"))
      
    }
    
  })
  
  output$metricplot <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = 'column') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = c('First Contact', 'First Face-to-Face Contact')
               ,title = list(text = 'Measurement')) %>%
      hc_yAxis(title = list(text = 'Median Hours to Event')) %>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      hc_add_series(data = c(round(filter(km_medians()
                                          ,strata == input$provider_select, id == "fc") 
                                   %>% .$median, 2),
                             round(filter(km_medians()
                                          ,strata == input$provider_select, id == "fftf") 
                                   %>% .$median, 2))) %>%
      hc_add_theme(hc_theme_gridlight()) %>%
      hc_colors(c('#d01010', '#d01010')) %>%
      hc_tooltip(enabled = FALSE)
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
```

