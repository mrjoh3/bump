library(shiny)
library(shiny.semantic)
ui <- semanticPage(
    title = "Bump",
    flow_layout(row_gap = "30px", cell_width = '100%',
        div(
          h1('Bump Me'),
          p('Do you remember what is was like to bump into someone in the office? ',
            'To see a familiar face walking from the train station or waiting for a coffee?'
            )
        ),
      split_layout(cell_widths = c("15%", "50%", "20%", "15%"), cell_args = "padding: 10px;",
                   div(),
                   segment(
                       form(
                           h4(class = "ui dividing header", "Inputs"),
                           field(
                               tags$label("First Name"),
                               text_input("name_first", value = "", type = "text", placeholder = "Enter Text...")
                           ),
                           field(
                               tags$label("Last Name"),
                               text_input(
                                   "name_last", value = "", type = "text", placeholder = "Enter Text...")
                           ),
                           field(
                               tags$label("E-Mail"),
                               text_input("email_ex", value = "", type = "email", placeholder = "Enter E-Mail")
                           ),
                           field(
                               tags$label("Options"),
                               multiple_checkbox(
                                   "preferences", "", choices = c('Coffee', 'Chat', 'Lunch', 'Walk'), position = "inline"
                               )
                           ),
                           field(
                               #tags$label(),
                               action_button('submit', 'BUMP', width = '100%')
                           ))),
                   div(
                       h3('Instructions'),
                       p('loads of text')
                   ),
                   div()
      ),
      div(p('output area'))
    )

)

server <- function(input, output) {}




shinyApp(ui, server)