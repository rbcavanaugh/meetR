# add 1 week timeframe to meeting
# should probably make a non-public name of shiny app....
# and add github link to all....
# wait 5s after done then kill app
# fix done button for next. 
# cards for plot and switching button thing

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(tidyverse)
library(DT)
library(waiter)

jscode2 <- '
$(document).keyup(function(event) {
    if ($("#name").is(":focus") && (event.keyCode == 13)) {
        $("#go").click();
    }
});'

# source("www/link_sheets.R")
# source("www/mongo_functions.R")


ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    theme = shinytheme("paper"),
    div(id = "helpme", style = "float:right; position: top; margin-top: 10px; margin-right: 5px;",
        dropdownButton(
          icon = icon("info"),
          status = "default",
          p(icon("hand-pointer"), "Select the time slots you're available"),
          p(icon("users"), "The number in parentheses shows how many people have selected each time slot"),
          p(icon("bug"), "Contact rob with any issues: rob.cavanaugh@pitt.edu"),
          right = T,
          width = "300px"
        )),
    titlePanel(uiOutput("welcome"), windowTitle = "meetR"),
    shinyjs::useShinyjs(),
    fluidRow(
             column(width = 12, style = "padding-left: 10%; padding-right: 10%; align:center;",
             tabsetPanel(id = "tabSwitch", 
                         tabPanel("Week 1",br(),
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_1"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("Week 2",br(),
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_2"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("Week 3",br(),
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_3"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("Week 4",br(),
                                  fluidRow(align = "center",
                                           column(width = 1),
                                           uiOutput("week_buttons_4"),
                                           column(width = 1)
                                  )
                         ),
                         tabPanel("Results",br(),
                                  radioButtons("plot_table", "Plot or Table", choices = c("Plot", "Table"),
                                               selected = "Plot", inline = T),
                                  tabsetPanel(type ="hidden", id = "results_tabs",
                                    tabPanel("Plotpanel",
                                      div(align = "center",
                                          id = "result_plot",
                                          plotOutput("plot")
                                          )
                                    ),
                                    tabPanel("Tablepanel",
                                      div(
                                        align = "center",
                                        id = "result_table",
                                        DTOutput("result", width ="80%")
                                    ),
                                    br(),
                                    div(align = "right", 
                                        HTML("<p>To filter out a person, paste ^((?!name).)*$ into the attending 
                                       filter <br/> for example: ^((?!Rob).)*$ will filter out rows that contain Rob<p/>")
                                       )
                                    )
                                    )
                                    
                         )
             )
             )
    ),
    fluidRow(
      div(align = "center", style = "padding: 1%; z-index:0", 
          uiOutput("next_done")
      )
    )
)

server <- function(input, output, session) {

  
  
####  functions that I need to move to the R folder. ####
  
    create_updateCheckboxGroupButtons <- function(button_id, choice_1, choice_2){ 
        updateCheckboxGroupButtons(
            session = session, 
            inputId = button_id,
            disabledChoices = currentcount(sel_week=choice_1,sel_day=choice_2,
                                           choice = choice(), counts = counts(), halfhour_in = input$halfhour) 
        )
    }
    create_checkboxGroupButtons <- function(button_id, day_of, choice_1, choice_2, choice_3){ 
        column(width = 2,
               checkboxGroupButtons(
                   inputId = button_id,
                   label = HTML(day_of),
                   choices = currentcount(choice_1, choice_2, ret=choice_3,
                                          choice = choice(), counts =counts(), halfhour_in = input$halfhour), 
                   direction = "vertical"
                   )
        )
    }


    
####  A bunch of modal stuff ####
    
  values <- reactiveValues()

    modal = modalDialog(
        tags$script(HTML(jscode2)),
        title = "Enter your name to select available times",
        textInput("name", "Enter name:"),
        selectInput(inputId = "col_name", label = "Choose Schedule:", choices = properties$name),
        hidden(
            div(align = "center", id = "new",
                checkboxInput("newbox", "New Schedule"),
                div(id = "newmeeting",
                textInput("meetingname", h4("Meeting Name")),
                dateInput("daterange", "Select First Monday",daysofweekdisabled = c(0,2,3,4,5,6), autoclose = T),
                radioButtons("num_weeks", "Number of Weeks", choices = c(1,2,3,4), inline = T, selected = 4),
                checkboxInput("halfhour", "On the half hour?", value = F)
                ),
                actionButton("delete", "Delete selected meeting", icon = icon("trash-alt"))
            )
        ),
        
        
        easyClose = F,size = "m",
        footer = tagList(
          p("Beta-version of scheduling app. Contact Rob with issues: rob.cavanaugh@pitt.edu", style = "float:left;"),
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
      req(input$name)
        input$name==admin_name
    })
    outputOptions(output, "admin_check", suspendWhenHidden = FALSE)
    
    
    
#### reactuve data stuff ####
    
    
    
    vars = reactive({
      week_var = 4
      formatted = tibble(
        days = rep(c('Monday', "Tuesday", "Wednesday",
                     "Thursday", "Friday", "Saturday", "Sunday"), week_var),
        first_date = ifelse(nrow(choice()>0),
                            rep(as.character(unique(choice()$start)),week_var*7),
                            rep(as.character(input$daterange), week_var*7)
        ),
        add = seq(1,7*week_var,1),
        current_date = as.Date(first_date)+add-1) %>%
        mutate(show_date = paste0(days, "<br/>", format(current_date, format="%b %d"))) %>%
        filter(days != "Saturday", days != "Sunday")  %>%
        pull(show_date)
      
      vars = tibble(
        button_id = create_buttons(week_var*5),
        day_of = formatted,
        choice_1 = rep(seq(1,week_var,1), each = 5),
        choice_2 = rep(c('M', "T", "W", "TH", "F"),week_var),
        choice_3 = 0
      )
      
    })
    
    start_end = reactive({
          choice() %>%
            select(weeks) %>%
            distinct() %>%
            pull(weeks)
    })
    
    
    
#### Observers ####
    
    
  
    observeEvent(dat_in(), {
        show_weeks = ifelse(nrow(choice()>0),max(unique(choice()$weeks)),4)
        input_weeks = input$num_weeks
        if(show_weeks<2 || input_weeks <2){
          hideTab(inputId = "tabSwitch", target = "Week 2")
          hideTab(inputId = "tabSwitch", target = "Week 3")
          hideTab(inputId = "tabSwitch", target = "Week 4")
        } else if(show_weeks<3 || input_weeks <3){
        hideTab(inputId = "tabSwitch", target = "Week 3")
        hideTab(inputId = "tabSwitch", target = "Week 4")
        } else if (show_weeks<4 || input_weeks <4) {
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
        hideTab(inputId = "tabSwitch", target = "Results")
        }
    })
    
    observeEvent(input$newbox,{
        if(isTruthy(input$newbox)){
        shinyjs::enable("newmeeting")
        } else {
            shinyjs::disable("newmeeting")
            }
    })
    
    observeEvent(c(input$tabSwitch, input$go),{
      if(isTruthy(input$name!=admin_name)){
          max_week =  choice() %>% select(weeks) %>% distinct() %>% pull(weeks)
          max_week = paste0("Week ", max_week)
          if(isTruthy(max_week != input$tabSwitch)){
            shinyjs::disable("done")
          } else {
            shinyjs:: enable("done")
          }
      } else {
          if(isTruthy(input$tabSwitch == "Results")){
            shinyjs::hide("done")
          }
      }
      
    })
    
    
    observeEvent(input$nxt,{
      current = as.numeric(str_sub(input$tabSwitch, -1))
      new_week = paste0("Week ", current + 1)
      updateTabsetPanel(session, "tabSwitch",selected = new_week)
    })
    
    
#### Outputs ####
    
    output$welcome <- renderUI({
      req(input$name)
      h3(paste0("Welcome ", input$name, "!"), align="center")
    })
    
    output$next_done <- renderUI({
      if(isTruthy(input$name!=admin_name)){
        max_week =  choice() %>% select(weeks) %>% distinct() %>% pull(weeks)
        max_week = paste0("Week ", max_week)
        if(isTruthy(max_week != input$tabSwitch)){
          actionBttn("nxt", "Next Page!", color = "primary", style = "pill")
        } else {
          actionBttn("done", "Done!", color = "success", style = "pill")
        }
      } else {
        actionBttn("done", "Done!", color = "success", style = "pill")
      }
      
    })

    
#### sheets stuff ####
    
    # read sheet
    dat_in <- eventReactive(input$go,{
      
        if(isTruthy(input$newbox==T)){
            tmp = tibble(user=NA, week=NA, day=NA, halfhour=NA, selected=NA, start=NA, weeks=NA, meetingname=NA)
            googlesheets4::sheet_write(ss=link, sheet = input$meetingname, data = tmp)
            return(tmp)
            
        }else{ 
          x <- tryCatch({
              googlesheets4::read_sheet(ss = link, sheet = input$col_name) %>% #read_csv("available.csv") %>% #
                separate(selected, into = c("selected", "remove"), sep = -4) %>%
                select(-remove)
            },
            error = function(e){
              sendSweetAlert(
                session = session,
                title = "Gosh Darnit!",
                text = "Accessing the server didn't work, try refreshing the page. Otherwise, contact Rob",
                type = "error",
                btn_labels = NA,
                closeOnClickOutside = TRUE,
              )
            }
          )
          x
        }
     
      
    })
    
    dat_gone <- observeEvent(input$delete,{
      x <- tryCatch({
        googlesheets4::sheet_delete(ss = link, sheet = input$col_name) 
        values$properties <- googlesheets4::sheet_properties(ss=link)
        print(values$properties)
        updateSelectInput("col_name", session = session, choices = values$properties$name, selected = NULL)
        
      },
      error = function(e){
        sendSweetAlert(
          session = session,
          title = "Gosh Darnit!",
          text = "Accessing the server didn't work, try refreshing the page. Otherwise, contact Rob",
          type = "error",
          btn_labels = NA,
          closeOnClickOutside = TRUE,
        )
      }
      )
      x
      
    })
    
    
    observeEvent(input$done, {
      if(isTruthy(input$name == admin_name)){
        df = data() %>%
          mutate(user = "admin",
                 start = as.Date(input$daterange),
                 weeks = input$num_weeks,
                 meetingname= input$meetingname)
        print(df)
        googlesheets4::sheet_append(ss=link, data = df, sheet = input$meetingname)
        sendSweetAlert(
          session = session,
          title = "All Set!",
          text = "Your new meeting has been set",
          type = "success",
          btn_labels = NA,
          closeOnClickOutside = F,
        )
      } else {
        df = data() 
        if(isTruthy(nrow(df)>0)){
        df = df %>%
          filter(user == input$name) %>%
          drop_na(selected) %>%
          mutate(start = NA,
                 end = NA,
                 meetingname=NA,
                 user = paste(user, sample(1:100,1), sep = "_"))
        googlesheets4::sheet_append(ss=link, data = df, sheet = input$col_name)
        sendSweetAlert(
          session = session,
          title = "Thanks!",
          text = tags$span(
            tags$p("Keep an eye out for a meeting invite"),
            tags$p("(you can close this window now)")
          ),
          type = "success",
          btn_labels = NA,
          closeOnClickOutside = F,
          html = T
        )
        } else {
          sendSweetAlert(
            session = session,
            title = "Oops!",
            text = "You didn't select any times",
            type = "warning",
          )
        }
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
    
    observeEvent(c(input$go,input$tabSwitch),{
        if(isTruthy(input$name!=admin_name)){
          for(i in 1:4){
            vars() %>%
              filter(choice_1 == i) %>%
              select(button_id, choice_1, choice_2) %>%
              pmap(create_updateCheckboxGroupButtons)
          }
        } else {}
    })
    
 
        

#### functions!!! #####
    # save picker data
    data = reactive({
        req(dat_in())
      
      ms = c(1,6,11,16)
      ts = c(2,7,12,17)
      ws = c(3,8,13,18)
      ths = c(4,9,14,19)
      fs = c(5,10,15,20)
      
      l=list()
      
      for(i in 1:20){
        l[[i]] = bind_rows(
          list(user = input$name,
               week = ifelse(i<=5, 1,
                             ifelse(i<=10, 2,
                                    ifelse(i<=15, 3, 4))),
               day = ifelse(i %in% ms, "M",
                            ifelse(i %in% ts, "T",
                                   ifelse(i %in% ws, "W",
                                          ifelse(i %in% ths, "TH", "F")))),
               halfhour = ifelse(input$halfhour==TRUE,1,0),
               selected = eval(parse(text = paste0("input$date", i, "buttons")))
        )
        )
      }
      out = bind_rows(l)
      if(isTruthy(ncol(out) == 4)) {
        out$selected = "NA"
      }
      out
      
      })
    
    observeEvent(input$plot_table, {
      updateTabsetPanel(session, "results_tabs", selected = paste0(input$plot_table, "panel"))
    })
    
    output$plot <- renderPlot({
      validate(need(length(unique(dat_in()$user))>2, "Not enough responders to show plot"))
      var = vars() %>% select(week = choice_1, day = choice_2, day_of)
      dat_in() %>%
        left_join(var, by = c('week', 'day')) %>%
        drop_na(selected) %>%
        filter(user != "admin") %>%
        mutate(day_of = gsub("<br/>", " ", day_of),
               full_time = paste(day_of, selected, sep = ": ")) %>%
        add_count(full_time) %>%
        arrange(desc(n)) %>%
        select(user, full_time, n) %>%
        ggplot(aes(x = fct_reorder(full_time,n), y=1, fill = user)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label=user), position = position_stack(vjust = .5), size = 3.5) +
        coord_flip() +
        theme_minimal(base_size = 16) +
        theme(panel.grid = element_blank()) +
        ylab("Number of people available") +
        xlab(NULL)
    })
    
    output$result <- renderDT({
      attendees = dat_in() %>% select(user) %>% distinct() %>% pull(user)
      var = vars() %>% select(week = choice_1, day = choice_2, day_of)
      dat_in() %>%
        left_join(var, by = c('week', 'day')) %>%
        drop_na(selected) %>%
        mutate(day_of = gsub("<br/>", " ", day_of),
               attending = "attending") %>%
        add_count(selected, day_of) %>%
        select(day_of, selected, n, attending, user) %>%
        pivot_wider(names_from = attending, values_from = user) %>% #exploit annoying lost-cols feature :)
        arrange(desc(n)) %>%
        rowwise() %>%
        mutate(attending = toString((attending))) %>%
        rename(Day = day_of, Time = selected, "Number Available" = n, Attending = attending)
      
        
    }, options = list(dom = "tp",
                      search = list(regex = TRUE),
                      autoWidth = TRUE,
                      columnDefs = list(list(width = '20%', targets = list(1,2)),
                                        list(className = 'dt-center', targets = 1:2))
                      ), filter = "top", rownames=F)

   
}
shiny::shinyApp(ui, server)


