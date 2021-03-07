# fix overwritten problem with multiple users...read in two separate sheets. 
# done category
# run github
# edit task choose with dropdown
# regular shiny for mobile friendly
# just enter name at start...probably no need for passwords



'%!in%' <- function(x,y)!('%in%'(x,y))

library(shiny)
library(shinydashboard)
library(shinyjs)
library(glue)
library(shinyauthr)
library(shinyWidgets)
library(lubridate)
library(tidyverse)
library(googlesheets4)

jscode <- '
shinyjs.init = function(){
$(document).keyup(function(event) {
    if ($("#login-password").is(":focus") && (event.keyCode == 13)) {
        $("#login-button").click();
    }
});
}'

jscode2 <- '
$(document).keyup(function(event) {
    if ($("#name").is(":focus") && (event.keyCode == 13)) {
        $("#go").click();
    }
});'


source("www/link.R")
source("www/counts.R")

user_base <- readRDS("user_base.rds")
#read_data <- read_sheet(link)
#data_list = lapply( split(read_data,read_data$name), as.list)



cs = c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm")



ui <- fluidPage(
    titlePanel("Welcome to the meeting app"),
    shinyjs::useShinyjs(),
    useShinydashboard(),
    extendShinyjs(text = jscode, functions = c()),
    #######################################################################
    shinyauthr::loginUI("login"),
    #######################################################################
    
    conditionalPanel(condition = "output.authed == true",
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
                     conditionalPanel(condition = "output.admin_check == true",
                                      column(width = 4,
                                      textInput("meetingname", "Name of Meeting")
                                      ),
                                      column(width = 4,
                                             dateRangeInput("daterange", "Select First Monday and Second Monday")
                                             ),
                                      column(width = 4,
                                      actionButton("save_admin", "Create Meeting Times")
                                      )
                     ),
                     conditionalPanel(condition = "output.admin_check == false", align = "center",
                                      actionButton("save_user", "Save Meeting Times")
                     )
                     ),
                     column(width = 12,
                     div(align = "center", style = "padding: 2%",
                     shinyauthr::logoutUI("logout", label = "Done!")
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
    # create_tabPanel <- function(title_in, week_buttons, header){
    #     tabPanel(title = title_in,
    #              fluidRow(uiOutput(header)),
    #              fluidRow(align = "center",
    #                       column(width = 1),
    #                       uiOutput(week_buttons),
    #                       column(width = 1)
    #              )
    #     )
    # }
    # create_weeks <- function(weeks){
    #     l = tibble()
    #     for (i in 1:weeks){
    #         l[i,1]=paste0("Week ", i)
    #         l[i,2]=paste0("week_buttons_",i)
    #         l[i,3]=paste0("week", i)
    #     }
    #     colnames(l) = c("title_in",  "week_buttons", "header")
    #     return(l)
    # }
    
    weeks = 2
    buttons = create_buttons(weeks*5)
    vars = tibble(
        button_id = buttons,
        day_of = rep(c('Monday', "Tuesday", "Wednesday", "Thursday", "Friday"),weeks),
        choice_1 = rep(c(1,weeks), each = 5),
        choice_2 = rep(c('M', "T", "W", "TH", "F"),weeks),
        choice_3 = 0
    )
 
    # output$tabs <- renderUI({
    #     create_weeks(weeks) %>%
    #         pmap(create_tabPanel)
    # })
    
    
    # login stuff
    # credentialing
    credentials <- callModule(shinyauthr::login, "login", 
                              data = user_base,
                              user_col = user,
                              pwd_col = password_hash,
                              sodium_hashed = TRUE,
                              log_out = reactive(logout_init()))
    logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
    # get user info from credentialing
    user_info <- reactiveValues()
    observeEvent(credentials()$user_auth,{
        req(credentials()$user_auth)
        user_info$info = credentials()$info
        user_info$name = credentials()$info$name
        user_info$other = user_base %>% filter(name != as.character(credentials()$info$name)) %>% dplyr::select(name)
    })
    # hide stuff unless authed
    output$authed <- reactive({
        credentials()$user_auth==TRUE
    })
    outputOptions(output, "authed", suspendWhenHidden = FALSE)
    
    # Show the model on start up ...
    observeEvent(dat_in(),{
        req(credentials()$user_auth)
    showModal(modalDialog(
        tags$script(HTML(jscode2)),
        title = "Enter Name",
        textInput("name", ""),
        easyClose = F,size = "s",
        footer = tagList(
            actionButton("go", "Go!")
        )
    ))
    })
    # remove modal with go. 
    observeEvent(input$go, {
        removeModal()
    })
    
    # check for admin
    output$admin_check <- reactive({
        req(credentials()$user_auth)
        credentials()$info$permissions=="admin"
    })
    outputOptions(output, "admin_check", suspendWhenHidden = FALSE)
    
    
    output$week1 <- renderUI({
        req(credentials()$user_auth)
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
        req(credentials()$user_auth)
        dat = choice() %>%
            select(end) %>%
            distinct() %>%
            pull(end)
            
        pr = paste0("Week of ", format(dat[1], format="%B %d, %Y"))
        div(style = "padding:1%;",
        h3(pr, align = "center")
        )
    })
    

############# sheets stuff #######
    
    # read sheet
    dat_in <- reactive({
        req(credentials()$user_auth)
        #df = read_sheet(sheet = "available", ss = link)
        df = read_csv('available.csv') %>%
            separate(selected, into = c("selected", "remove"), sep = -4) %>%
            select(-remove)
        df
    })
            ########### reactive data comes in here#########
            counts <- reactive({
                dat_in()  %>% drop_na(selected) %>% count(week, day, selected)
            })
            
            choice <- reactive({
                req(credentials()$user_auth)
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
        #sheet_write(data = df, ss = link, sheet = "available")
    })
    # append sheet
    observeEvent(input$save_user,{
        df = data() %>%
            filter(user == input$name) %>%
            drop_na(selected) %>%
            mutate(start = NA,
                   end = NA,
                   meetingname=NA)
        #sheet_append(data = df, ss = link, sheet = "available")
    })
    # check what table looks like. 
    output$checktable <- renderTable({
        choice()
    })
    
    
    ####################### inputs! ##############3
    




    
#### 10 pickeres ####
    
    output$week_buttons_1 <- renderUI({
        vars %>%
        filter(choice_1 == 1) %>%
        pmap(create_checkboxGroupButtons)
        
    })
    
    output$week_buttons_2 <- renderUI({
        vars %>%
            filter(choice_1 == 2) %>%
            pmap(create_checkboxGroupButtons)
    })
    outputOptions(output, "week_buttons_2", suspendWhenHidden = FALSE)
    

#### update pickers ####
    
    observeEvent(input$go,{
        if(isTruthy(credentials()$info$permissions=="standard")){
            vars %>%
                filter(choice_1 == 1) %>%
                select(button_id, choice_1, choice_2) %>%
                pmap(create_updateCheckboxGroupButtons)
        } else {}
    })
    
    observeEvent(input$tabSwitch,{
        if(isTruthy(credentials()$info$permissions=="standard")){
            
        vars %>%
            filter(choice_1 == 2) %>%
            select(button_id, choice_1, choice_2) %>%
            pmap(create_updateCheckboxGroupButtons)
     } else {}
        })
        

#### functions!!! #####
    # save picker data
    data = reactive({
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










