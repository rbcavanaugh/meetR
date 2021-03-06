# fix top right button...just new action button and modal


'%!in%' <- function(x,y)!('%in%'(x,y))

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(tidyverse)
library(googlesheets4)

jscode2 <- '
$(document).keyup(function(event) {
    if ($("#name").is(":focus") && (event.keyCode == 13)) {
        $("#go").click();
    }
});'


source("www/link.R")
source("www/counts.R")

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
                                  fluidRow(uiOutput("week1")),
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_1"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("Week 2",
                                  fluidRow(uiOutput("week2")),
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_2"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("Week 3",
                                  fluidRow(uiOutput("week3")),
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_3"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("check",
                                  tableOutput("checktable")
                                  )
                         ),
                     fluidRow(align="center",br(),
                                      actionButton("save_user", "Save Meeting Times"),
                                      actionButton("startover", "Start Over"),
                              shinyjs::hidden(
                                  div(id = "admin", style = "padding: 2%",
                                          actionButton("adminupdate", "See only available"),
                                          actionButton("admin_set", "Set meeting info")
                                          )
                                  ),
                              
                              br(),
                              br()
                              
                     )
                     ),
                     fluidRow(
                     div(align = "center", style = "padding: 3%",
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
                   label = day_of,
                   choices = currentcount(choice_1, choice_2, ret=choice_3), #data_in = get(dat1), counts_in = get(dat2)
                   direction = "vertical"
               )
        )
    }
    weeks = 2
    buttons = create_buttons(weeks*5)
    vars = tibble(
        button_id = buttons,
        day_of = rep(c('Monday', "Tuesday", "Wednesday", "Thursday", "Friday"),weeks),
        choice_1 = rep(c(1,weeks), each = 5),
        choice_2 = rep(c('M', "T", "W", "TH", "F"),weeks),
        choice_3 = 0
    )
    
    # Show the model on start up ...
    modal = modalDialog(
        tags$script(HTML(jscode2)),
        title = "Enter Name",
        textInput("name", ""),
        hidden(
        checkboxInput("new", "New Schedule")
        ),
        easyClose = F,size = "s",
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
    })
    
    # check for admin
    output$admin_check <- reactive({
        input$name=="rob-admin"
    })
    outputOptions(output, "admin_check", suspendWhenHidden = FALSE)
    
    
    output$week1 <- renderUI({
        dat = choice() %>%
            select(start) %>%
            distinct() %>%
            pull(start)
        pr = paste0("Week of ", format(dat[1], format="%B %d, %Y"))
        div(style = "padding:1%;",
        h3(pr, align = "center")
        )
    })
    
    output$week2 <- renderUI({
        dat = choice() %>%
            select(end) %>%
            distinct() %>%
            pull(end)
            
        pr = paste0("Week of ", format(dat[1], format="%B %d, %Y"))
        div(style = "padding:1%;",
        h3(pr, align = "center")
        )
    })
    
    observeEvent(input$done, {
        sendSweetAlert(
            session = session,
            title = "Thanks!",
            text = "Keep an eye out for a meeting invite",
            type = "success"
        )
    })
    
    observeEvent(input$name, {
        if(weeks<3){
        hideTab(inputId = "tabSwitch", target = "Week 3")
        } else {}
    })
    
    observeEvent(input$name,{
    req(input$name)
    if(input$name == "rob-admin"){
        shinyjs::show("admin")
        shinyjs::show("new")
    } else {shinyjs::hide("new")}
    })
    
    output$welcome <- renderUI({
        req(input$name)
        h1(paste0("Welcome ", input$name, "!"), align="center")
    })
    
    
    observeEvent(input$admin_set,{
        showModal(modalDialog(
            div(align = "center",
                 title = "Enter meeting info",
                textInput("meetingname", h4("Meeting Name")),
                dateRangeInput("daterange", "Select First Monday and Second Monday"),
                actionButton("save_admin", "Create Meeting Times")
            ),
                easyClose = T, size = "m"
        ))
                })

############# sheets stuff #######
    
    # read sheet
    dat_in <- eventReactive(input$go,{
        if(isTruthy(input$new)){
            tibble(user=NA, week=NA, day=NA, selected=NA, start=NA, end=NA, meetingname=NA)
        }else{ 
            df = read_sheet(sheet = "available", ss = link) %>%
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
    observeEvent(input$save_admin,{
        df = data() %>%
            mutate(user = "admin",
                   start = input$daterange[1],
                   end = input$daterange[2],
                   meetingname= input$meetingname)
        sheet_write(data = df, ss = link, sheet = "available")
    })
    # append sheet
    observeEvent(input$save_user,{
        df = data() %>%
            filter(user == input$name) %>%
            drop_na(selected) %>%
            mutate(start = NA,
                   end = NA,
                   meetingname=NA)
        sheet_append(data = df, ss = link, sheet = "available")
    })
    # check what table looks like. 
    output$checktable <- renderTable({
        choice()
    })
    
    
    ####################### inputs! ##############3
    




    
#### 10 pickeres ####
    
    output$week_buttons_1 <- renderUI({
        req(dat_in())
        vars %>%
        filter(choice_1 == 1) %>%
        pmap(create_checkboxGroupButtons)
        
    })
    
    output$week_buttons_2 <- renderUI({
        req(dat_in())
        vars %>%
            filter(choice_1 == 2) %>%
            pmap(create_checkboxGroupButtons)
    })
    outputOptions(output, "week_buttons_2", suspendWhenHidden = FALSE)
    

#### update pickers ####
    
    observeEvent(input$go,{
        if(isTruthy(input$name!="rob-admin")){
            vars %>%
                filter(choice_1 == 1) %>%
                select(button_id, choice_1, choice_2) %>%
                pmap(create_updateCheckboxGroupButtons)
        } else {}
    })
    
    observeEvent(input$tabSwitch,{
        if(isTruthy(input$name!="rob-admin")){
        vars %>%
            filter(choice_1 == 2) %>%
            select(button_id, choice_1, choice_2) %>%
            pmap(create_updateCheckboxGroupButtons)
     } else {}
        })
    
    observeEvent(input$adminupdate,{
            vars %>%
                select(button_id, choice_1, choice_2) %>%
                pmap(create_updateCheckboxGroupButtons)
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
                )
            )
        )
        
    })
        
   
}
shiny::shinyApp(ui, server)










