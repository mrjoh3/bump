library(shiny)
library(shiny.semantic)
library(shinyjs)
library(shinyalert)
library(randomNames)
library(purrr)
library(DBI)
library(pool)
library(glue)
library(dplyr)
library(dbplyr)
library(stringr)
library(hashids)
library(lubridate)


# setup db connection
con <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  dbname = "gisdata",
  host = 'intel.int.epa.vic.gov.au',
  user = "gisuser",
  password = keyring::key_get('RStudio Keyring Secrets', username = 'pg_gisuser')
)

# close db connection on exit
onStop(function() {
    cat('Closing Pool')
    pool::poolClose(con)
})

ACTIVITY_OPTIONS <- c('Coffee', 'Chat', 'Lunch', 'Walk')

## get a background image from unsplash

unsplash <- glue::glue("https://api.unsplash.com/photos/random?query=faces&orientation=portrait&client_id={Sys.getenv()[['unsplash_access']]}") %>%
  httr::GET() %>%
  content()

img_url <- unsplash$urls$full

## check / create required tables (this should only run once)

required_tbls <- c('bump_person', 'bump_scenario', 'bump_transact')

tbls <- dbListTables(con)

if (!all(required_tbls %in% tbls)) {
    
  bump_person <- randomNames::randomNames(10) %>%
      str_split(., ', ') %>%
      map_dfr(~ data.frame(id = hashids::encode(as.integer(Sys.time()), 
                                                hashid_settings(salt = glue('{tolower(.x[2])}.{tolower(.x[1])}@email.com'), min_length = 8)),
                           name_first = .x[2],
                           name_last = .x[1],
                           person = glue('{.x[2]} {.x[1]}'),
                           email = glue('{tolower(.x[2])}.{tolower(.x[1])}@email.com'),
                           bump_prefs = paste(sample(ACTIVITY_OPTIONS, 2, replace = FALSE), collapse = '|'),
                           status =  'fake',
                           timestamp = Sys.time()))
    
    bump_scenario <- data.frame(scenario = c('{person} is walking down the corridor in the opposite direction',
                                             'You just stepped out of the elevator and ran into {person}',
                                             '{person} was sitting alone at lunch you have stopped to say hi',
                                             'You and {person} have gone to make a coffee at the same time',
                                             'Walking from the station you can see {person} a few metres ahead',
                                             '{person} is blocking the stairs so you stop to chat'))
    
    bump_transact <- data.frame(bump_ee = as.character(NA),
                                bump_ed = as.character(NA),
                                activity = as.character(NA),
                                timestamp = as.POSIXct(NA))
    
    dbWriteTable(con, 'bump_person', bump_person, row.names = FALSE, overwrite = TRUE)
    dbCreateTable(con$fetch(), 'bump_transact', bump_transact)
    dbWriteTable(con, 'bump_scenario', bump_scenario, row.names = FALSE, overwrite = TRUE)
    
}

# function to validate email format
# https://www.r-bloggers.com/2012/07/validating-email-adresses-in-r/
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}


ui <- semanticPage(theme = 'darkly',
    title = "Bump",
    useShinyjs(),
    useShinyalert(),
    flow_layout(row_gap = "30px", cell_width = '100%',
        div(
          h1('Bump Me')
        ),
      split_layout(cell_widths = c("35%", "40%", "15%", "10%"), cell_args = "padding: 10px;", style = 'background-image: linear-gradient(to right, #222, #222, #2b3e50);',
                   div(tags$img(src = img_url, 
                                alt = "A New Face",
                                style = 'max-width: 120%; max-height: 120%;')),
                   div(style = "padding-top:50px;",
                     segment(style = 'margin:5px;border-filter: blur(3px);', # not working
                       form(
                           h4(class = "ui dividing header", "Inputs"),
                           field(
                               #tags$label("Are you a new User"),
                               shiny.semantic::toggle("new_user", "Are you a new User?", is_marked = FALSE)
                           ),
                           conditionalPanel('input.new_user',
                                            p('For new users please select all of the ways you wish to be contacted.',
                                              ' Then, once you are added to the system you can bump into someone.'),
                                            field(
                                              tags$label("New Options"),
                                              multiple_checkbox(
                                                "new_preferences", "", choices = ACTIVITY_OPTIONS, position = "inline"
                                              )
                                            ),
                                            field(
                                                tags$label("First Name"),
                                                text_input("name_first", value = "", type = "text", placeholder = "Enter Text...")
                                            ),
                                            field(
                                                tags$label("Last Name"),
                                                text_input(
                                                    "name_last", value = "", type = "text", placeholder = "Enter Text...")
                                            )),
                           hr(),
                           conditionalPanel('input.new_user == false',
                                            field(
                                              tags$label("Options"),
                                              multiple_radio(
                                                "preferences", "", choices = c('Coffee', 'Chat', 'Lunch', 'Walk'), position = "inline", type = 'toggleEach' 
                                              )
                                            )),
                           field(
                               tags$label("E-Mail"),
                               text_input("email", value = "", type = "email", placeholder = "Enter E-Mail")
                           ),
                           field(
                               #tags$label(),
                               action_button('submit', 'BUMP', width = '100%')
                           )))),
                   div(
                       h3('Instructions'),
                       p('Do you remember what is was like to bump into someone in the office? ',
                         'To see a familiar face walking from the train station or waiting for a coffee?'
                       )
                   ),
                   div()
      ),
      div(p('output area'),
          h1(textOutput('activity_who')),
          h1(textOutput('activity_what')))
    )

)

server <- function(input, output, session) {
    
    # import data
    people <<- dbReadTable(con, 'bump_person')
    scenarios <<- dbReadTable(con, 'bump_scenario')
    
    recent_transact <- tbl(con, 'bump_transact') %>%
      filter(timestamp > local(Sys.Date() - days(7))) %>%
      collect()
    
    observeEvent(input$submit, {
      
      scn <- sample(scenarios$scenario, 1)
      
      if (input$new_user) {
        
        # check if person already exists
        if (tolower(input$email) %in% people$email) {
          
          # modal looks like you already exist
          print('persons already exists')
          shinyalert::shinyalert('Hello Again', 'It looks like you have registered here before?')
          
        } else {
          
          ts <- Sys.time()
          
          new_person <- data.frame(id = hashids::encode(as.integer(ts), 
                                                    hashid_settings(salt = input$email, min_length = 8)),
                               name_first = as.character(input$name_first),
                               name_last = as.character(input$name_last),
                               person = glue::glue('{input$name_first} {input$name_last}'),
                               email = as.character(tolower(input$email)),
                               bump_prefs = as.character(paste(input$new_preferences, collapse = '|')),
                               status = 'active',
                               timestamp = ts)
          
          # only upload if email appears valid
          if (isValidEmail(input$email)){
            
            dbWriteTable(con, 'bump_person', new_person, append = TRUE, row.names = FALSE)
            
            updateTextInput(session, 'name_first', value = NA)
            updateTextInput(session, 'name_last', value = NA)
            updateTextInput(session, 'email', value = NA)
            reset('new_user')
            updateSelectInput(session, 'new_preferences', label = '', selected = NULL)
            
            
            ## render an example message
            # get fake person as bump_ed
            person <- people %>%
              filter(status == 'fake') %>%
              pull(person) %>%
              sample(., 1)
            
            output$activity_who <- renderText(glue::glue(scn))
            
            
          } else {
            
            # modal email does not appear to be correct format
            print('fix email address')
            shinyalert::shinyalert('Woops!', 'Please double check your email and try again')
            
          }
          
          
        }

        
        
      } else {
        
        # generate transaction and save to DB
        ## this is the bump_ed persons names, used in text templates
        
        bump_ee <- people %>%
          filter(email == input$email) 
        
        bump_ed <- people %>%
          filter(email != input$email) %>% # don't bump into yourself
          filter(!(id %in% recent_transact$bump_ed)) %>% # exclude users recently bumped
          dplyr::filter(grepl(input$preferences, bump_prefs)) %>% # bump into someone with your current interests
          sample_n(1)
        # bump_ed <- people %>%
        #   filter(email != 'matthew.johnson@epa.vic.gov.au') %>% # don't bump into yourself
        #   filter(!(id %in% recent_transact$bump_ed)) %>% # exclude users recently bumped
        #   dplyr::filter(grepl('Lunch', bump_prefs)) %>% # bump into someone with your current interests
        #   sample_n(1)
        
        if (nrow(bump_ed) > 0) {
          
          person <- bump_ed %>%
            pull(person)
          
          # maybe remove this activity is defined by the user (can user not select preference and be surprised???)
          # activity <- intersect(unlist(str_split(bump_ee$bump_prefs, '\\|')),
          #                       unlist(str_split(bump_ed$bump_prefs, '\\|'))) %>%
          #   sample(., 1)
          activity <- input$preferences
          # add option to better define activity bsed on chosen preference
          
          bump_transact <- data.frame(bump_ee = bump_ee %>% pull(id),
                                      bump_ed = bump_ed %>% pull(id),
                                      activity = activity,
                                      timestamp = Sys.time())
          
          output$activity_who <- renderText(glue::glue(scn))
          output$activity_what <- renderText(glue::glue("Why don't you invite them for {activity}"))
          
          dbWriteTable(con, 'bump_transact', bump_transact, append = TRUE, row.names = FALSE)
          
        } else {
          
          shinyalert::shinyalert('Oh No!', glue::glue('It looks like no one is available for a {input$preferences}'))
          
        }
        
      }


      
    })
    
    
    
}




shinyApp(ui, server)