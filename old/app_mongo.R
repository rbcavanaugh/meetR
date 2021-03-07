# pull number of weeks from input

#'%!in%' <- function(x,y)!('%in%'(x,y))

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(tidyverse)
library(mongolite)
#library(googlesheets4)


jscode2 <- '
$(document).keyup(function(event) {
    if ($("#name").is(":focus") && (event.keyCode == 13)) {
        $("#go").click();
    }
});'

admin_name = "rob-admin"



#source("www/link.R")

cs = c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm")

ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    theme = shinytheme("paper"),
    titlePanel(uiOutput("welcome"), windowTitle = "meetR"),
    shinyjs::useShinyjs(),
    fluidRow(
             column(width = 12, style = "padding-left: 10%; padding-right: 10%; align:center;",
             tabsetPanel(id = "tabSwitch", 
                         tabPanel("Week 1",
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_1"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("Week 2",
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_2"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("Week 3",
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_3"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("Week 4",
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_4"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("check",
                                  tableOutput("checktable")
                                  )
                         )
             ),
                     fluidRow(
                     div(align = "center", style = "padding: 1%",
                         actionBttn("done", "Done!", color = "success", style = "pill")
                     )
                     )
                            
    )
)

server <- function(input, output, session) {
    create_buttons <- function(num){
        l = list()
        buttons = for(i in 1:num){
            l[[i]] = paste0('date', i, 'buttons')
        }
        return(unlist(l))
    }
    currentcount <- function(sel_week, sel_day, data_in, counts_in, ret = 1){
        cs = c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm")
        l = list()
        for (i in 1:10){
            l[[i]] = paste0(cs[i], " (", counts() %>%
                                dplyr::filter(week == sel_week, day == sel_day, selected == cs[i]) %>%
                                dplyr::select(n) %>%
                                pluck(1, .default = "0"), ")")
        }
        cs_new = unlist(l)
        if(ret == 1){
            return(cs_new[-c(choice() %>%
                                 dplyr::filter(week == sel_week, day == sel_day) %>%
                                 mutate(sel_num = match(selected, cs)) %>%
                                 dplyr::select(sel_num) %>%
                                 drop_na() %>%
                                 pluck(1,.default=50))])
        } else {
            return(cs_new)
        }
    }
    create_updateCheckboxGroupButtons <- function(button_id, choice_1, choice_2){ #dat1, dat2
        updateCheckboxGroupButtons(
            session = session, 
            inputId = button_id,
            disabledChoices = currentcount(sel_week=choice_1,sel_day=choice_2) #, data_in = get(dat1), counts_in = get(dat2)
        )
    }
    create_checkboxGroupButtons <- function(button_id, day_of, choice_1, choice_2, choice_3){ #, dat1, dat2
        column(width = 2,
               checkboxGroupButtons(
                   inputId = button_id,
                   label = HTML(day_of),
                   choices = currentcount(choice_1, choice_2, ret=choice_3), #data_in = get(dat1), counts_in = get(dat2)
                   direction = "vertical"
               )
        )
    }
    saveData <- function(data) {
      #Connect to the database
      db <- mongo(collection = collectionName,
                  url = sprintf(
                    "mongodb+srv://%s:%s@%s/%s",
                    options()$mongodb$username,
                    options()$mongodb$password,
                    options()$mongodb$host,
                    databaseName
                  ),
                  options = ssl_options(weak_cert_validation = TRUE,
                                        allow_invalid_hostname = TRUE))
      # Insert the data into the mongo collection as a data.frame
      data <- as_data_frame(data)
      db$insert(data)
    }
    newData <- function(data) {
      #Connect to the database
      db <- mongo(collection = collectionName,
                  url = sprintf(
                      "mongodb+srv://%s:%s@%s/%s",
                      options()$mongodb$username,
                      options()$mongodb$password,
                      options()$mongodb$host,
                      databaseName
                  ),
                  options = ssl_options(weak_cert_validation = TRUE,
                                        allow_invalid_hostname = TRUE))
      # Insert the data into the mongo collection as a data.frame
      data <- as_tibble(data)
      db$remove('{}')
      db$insert(data)
    }
    loadData <- function() {
      # Connect to the database
      db <- mongo(collection = collectionName,
                  url = sprintf(
                    "mongodb+srv://%s:%s@%s/%s",
                    options()$mongodb$username,
                    options()$mongodb$password,
                    options()$mongodb$host,
                    databaseName
                  ),
                  options = ssl_options(weak_cert_validation = TRUE))
      # Read all the entries
      data <- db$find()
      data
    }
     vars = reactive({
        week_var = 4
        formatted = tibble(
            days = rep(c('Monday', "Tuesday", "Wednesday",
                         "Thursday", "Friday", "Saturday", "Sunday"), week_var),
            first_date = ifelse(nrow(choice()>0),
                                rep(as.Date(unique(choice()$start)),week_var*7),
                                rep(as.Date(input$daterange), week_var*7)
                                ),
            add = seq(1,7*week_var,1),
            show_date = paste0(days, "<br/>", format(first_date+add-1, format="%b %d"))
        ) %>% filter(days != "Saturday", days != "Sunday")  %>% pull(show_date)
        
        vars = tibble(
            button_id = create_buttons(week_var*5),
            day_of = formatted,
            choice_1 = rep(seq(1,week_var,1), each = 5),
            choice_2 = rep(c('M', "T", "W", "TH", "F"),week_var),
            choice_3 = 0
        )
    })
        

    
    # Show the model on start up ...
    modal = modalDialog(
        tags$script(HTML(jscode2)),
        title = "Enter Name",
        textInput("name", ""),
        hidden(
            div(align = "center", id = "new",
                checkboxInput("newbox", "New Schedule"),
                div(id = "newmeeting",
                h3("Enter meeting info"),
                textInput("meetingname", h4("Meeting Name")),
                dateInput("daterange", "Select First Monday", value = Sys.Date()),
                radioButtons("num_weeks", "Number of Weeks", choices = c(2,3,4), inline = T, selected = 2)
                )
            )
        ),
        easyClose = F,size = "m",
        footer = tagList(
            actionButton("go", "Go!")
        )
    )
    showModal(modal)
    # remove modal with go. 
    observeEvent(input$go, {
        removeModal()
    })
    observeEvent(input$startover, {
        showModal(modal)
        shinyjs::reset("tabSwitch")
    })
    
    # check for admin
    output$admin_check <- reactive({
        input$name==admin_name
    })
    outputOptions(output, "admin_check", suspendWhenHidden = FALSE)
    
    start_end = reactive({
          choice() %>%
            select(weeks) %>%
            distinct() %>%
            pull(weeks)
    })
    
  
    
    observeEvent(input$done, {
        if(isTruthy(input$name == admin_name)){
            df = data() %>%
                mutate(user = "admin",
                       start = input$daterange,
                       weeks = input$num_weeks,
                       meetingname= input$meetingname)
            newData(df)
            Sys.sleep(.5)
            sendSweetAlert(
                session = session,
                title = "All Set!",
                text = "Your new meeting has been set",
                type = "success"
            )
        } else {
            df = data() %>%
                filter(user == input$name) %>%
                drop_na(selected) %>%
                mutate(start = NA,
                       end = NA,
                       meetingname=NA)
            saveData(df)
            Sys.sleep(.5)
        sendSweetAlert(
            session = session,
            title = "Thanks!",
            text = "Keep an eye out for a meeting invite",
            type = "success"
        )
        }
    })
    
    observeEvent(dat_in(), {
        show_weeks = ifelse(nrow(choice()>0),unique(choice()$weeks),4)
        if(show_weeks<3){
        hideTab(inputId = "tabSwitch", target = "Week 3")
        hideTab(inputId = "tabSwitch", target = "Week 4")
        } else if (show_weeks<4) {
        hideTab(inputId = "tabSwitch", target = "Week 4")
        } else {}
    })
    
    observeEvent(input$name,{
    req(input$name)
    if(isTruthy(input$name == admin_name)){
        shinyjs::show("admin")
        shinyjs::show("new")
    } else {
        shinyjs::hide("new")
        #hideTab(inputId = "tabSwitch", target = "check")
        }
    })
    
    output$welcome <- renderUI({
        req(input$name)
        h3(paste0("Welcome ", input$name, "!"), align="center")
    })
    
    observeEvent(input$newbox,{
        if(isTruthy(input$newbox)){
        shinyjs::enable("newmeeting")
        } else {
            shinyjs::disable("newmeeting")
            }
    })

############# mongo stuff #######
    
    # read sheet
    dat_in <- eventReactive(input$go,{
        if(isTruthy(input$new)){
            tibble(user=NA, week=NA, day=NA, selected=NA, start=NA, weeks=NA, meetingname=NA)
        }else{ 
            df = loadData() %>% #read_csv("available.csv") %>% #
                separate(selected, into = c("selected", "remove"), sep = -4) %>%
                select(-remove)
        }
    })
            ########### reactive data comes in here#########
            counts <- reactive({
                req(dat_in())
                dat_in()  %>% drop_na(selected) %>% count(week, day, selected)
            })
            
            choice <- reactive({
                req(dat_in())
                dat_in() %>%
                    filter(user == "admin")
            })
            ##################################################
            
    # overwrite sheet
    # check what table looks like. 
    output$checktable <- renderTable({
        choice()
    })
    
#### 10 pickeres ####
    
    output$week_buttons_1 <- renderUI({
        req(dat_in())
        vars() %>%
        filter(choice_1 == 1) %>%
        pmap(create_checkboxGroupButtons)
        
    })
    
    output$week_buttons_2 <- renderUI({
        req(dat_in())
        vars() %>%
            filter(choice_1 == 2) %>%
            pmap(create_checkboxGroupButtons)
    })
    outputOptions(output, "week_buttons_2", suspendWhenHidden = FALSE)
    
    output$week_buttons_3 <- renderUI({
        req(dat_in())
        vars() %>%
            filter(choice_1 == 3) %>%
            pmap(create_checkboxGroupButtons)
    })
    outputOptions(output, "week_buttons_3", suspendWhenHidden = FALSE)
    
    output$week_buttons_4 <- renderUI({
        req(dat_in())
        vars() %>%
            filter(choice_1 == 4) %>%
            pmap(create_checkboxGroupButtons)
    })
    outputOptions(output, "week_buttons_4", suspendWhenHidden = FALSE)

#### update pickers ####
    
    observeEvent(input$go,{
        if(isTruthy(input$name!=admin_name)){
            vars() %>%
                filter(choice_1 == 1) %>%
                select(button_id, choice_1, choice_2) %>%
                pmap(create_updateCheckboxGroupButtons)
        } else {}
    })
    
    observeEvent(input$tabSwitch,{
        if(isTruthy(input$name!=admin_name)){
            vars() %>%
            filter(choice_1 == 2) %>%
            select(button_id, choice_1, choice_2) %>%
            pmap(create_updateCheckboxGroupButtons)
     } else {}
        })
    
    observeEvent(input$tabSwitch,{
        if(isTruthy(input$name!=admin_name)){
            vars() %>%
                filter(choice_1 == 3) %>%
                select(button_id, choice_1, choice_2) %>%
                pmap(create_updateCheckboxGroupButtons)
        } else {}
    })
    
    observeEvent(input$tabSwitch,{
        if(isTruthy(input$name!=admin_name)){
            vars() %>%
                filter(choice_1 == 4) %>%
                select(button_id, choice_1, choice_2) %>%
                pmap(create_updateCheckboxGroupButtons)
        } else {}
    })
    
        

#### functions!!! #####
    # save picker data
    data = reactive({
        req(dat_in())
        bind_rows(
            list(
                m1=  bind_rows(
                    list(
                        user = input$name,
                        week = 1,
                        day = "M",
                        selected = input$date1buttons
                    )
                ), t1= bind_rows(     
                    list(
                        user = input$name,
                        week = 1,
                        day = "T",
                        selected = input$date2buttons
                    )
                ), w1=bind_rows(     
                    list(
                        user = input$name,
                        week = 1,
                        day = "W",
                        selected = input$date3buttons
                    )
                ), th1=bind_rows(     
                    list(
                        user = input$name,
                        week = 1,
                        day = "TH",
                        selected = input$date4buttons
                    )
                ), f1=bind_rows(     
                    list(
                        user = input$name,
                        week = 1,
                        day = "F",
                        selected = input$date5buttons
                    )
                ), m2=bind_rows( 
                    list(
                        user = input$name,
                        week = 2,
                        day = "M",
                        selected = input$date6buttons
                    )
                ), t2=bind_rows(     
                    list(
                        user = input$name,
                        week = 2,
                        day = "T",
                        selected = input$date7buttons
                    )
                ), w2=bind_rows(     
                    list(
                        user = input$name,
                        week = 2,
                        day = "W",
                        selected = input$date8buttons
                    )
                ), th2=bind_rows(     
                    list(
                        user = input$name,
                        week = 2,
                        day = "TH",
                        selected = input$date9buttons
                    )
                ), f2=bind_rows(     
                    list(
                        user = input$name,
                        week = 2,
                        day = "F",
                        selected = input$date10buttons
                    )
                ), m3=bind_rows( 
                    list(
                        user = input$name,
                        week = 3,
                        day = "M",
                        selected = input$date11buttons
                    )
                ), t3=bind_rows(     
                    list(
                        user = input$name,
                        week = 3,
                        day = "T",
                        selected = input$date12buttons
                    )
                ), w3=bind_rows(     
                    list(
                        user = input$name,
                        week = 3,
                        day = "W",
                        selected = input$date13buttons
                    )
                ), th3=bind_rows(     
                    list(
                        user = input$name,
                        week = 3,
                        day = "TH",
                        selected = input$date14buttons
                    )
                ), f3=bind_rows(     
                    list(
                        user = input$name,
                        week = 3,
                        day = "F",
                        selected = input$date15buttons
                    )
                ), m4=bind_rows( 
                    list(
                        user = input$name,
                        week = 4,
                        day = "M",
                        selected = input$date16buttons
                    )
                ), t4=bind_rows(     
                    list(
                        user = input$name,
                        week = 4,
                        day = "T",
                        selected = input$date17buttons
                    )
                ), w4=bind_rows(     
                    list(
                        user = input$name,
                        week = 4,
                        day = "W",
                        selected = input$date18buttons
                    )
                ), th4=bind_rows(     
                    list(
                        user = input$name,
                        week = 4,
                        day = "TH",
                        selected = input$date19buttons
                    )
                ), f4=bind_rows(     
                    list(
                        user = input$name,
                        week = 4,
                        day = "F",
                        selected = input$date20buttons
                    )
                )
            )
        )
        
    })
        
   
}
shiny::shinyApp(ui, server)










