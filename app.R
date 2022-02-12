library(shiny)
library(shiny.semantic)
library(shiny.router)
library(tidyverse)
library(gt)
library(glue)

# Get data
population <- read_csv("https://gist.githubusercontent.com/jamie-ralph/7acaab327b7da827894d0bc4af455416/raw/22cac256db12aa63543045a719c3228aa63d7b5b/wales_population.csv") %>%
    filter(gender == "Persons",
           age_group == "All ages"
    )

mygrid <- grid_template(
    default = list(
        areas = rbind(
            c("title", "plot", "table"),
            c("intro", "plot", "table"),
            c("selection", "plot", "table"),
            c("user", "plot", "table"),
            c("user", "plot", "table")
        ),
        cols_width = c("300px", "500px", "300px"),
        rows_height = c("100px", "100px", "150px", "130px", "50px")
    ),
    mobile = list(
        areas = rbind(
            "title",
            "intro",
            "selection",
            "summary",
            "plot",
            "table",
            "user"
        ),
        rows_height = c("50px", "100px", "200px", "200px", "100px"),
        cols_width = c("100%")
    )
)

summary_grid <- grid(grid_template = grid_template(
    default = list(
        areas = rbind(
            c("tab"),
            c("sum")
        ),
        rows_height = c("55%", "45%"))),
    area_styles = list("tab" = "padding-top: 20px; padding-right: 20px", "sum" = "padding-top: 0px; padding-right: 20px"),
    tab = div(class = "ui raised segment", 
              div(gt_output("great_table"))),
    sum = uiOutput("summary"))

pageOne <- semanticPage(
    grid(
        mygrid,
        area_styles = list("plot" = "padding: 20px;"),
        title =
            h2(class = "ui header", "Mid-year population estimates in Wales"),
        intro = card(
            style = "border-radius: 0; width: 100%",
            div(class = "content",
                div(class = "description",
                    html(glue("This plot displays population data from
                       {a('StatsWales', href = 'https://statswales.gov.wales/Catalogue/Population-and-Migration/Population/Estimates/Local-Authority')}.
                       Select a local authority to see population changes over time."))))
        ),
        selection = 
            dropdown_input("choice",
            choices = sort(unique(population$area_name)),
            value = "Blaenau Gwent"),
        plot = div(class = "ui raised segment", plotOutput("linePlot")),
        user = card(
            style = "border-radius: 0; width: 100%; background: #efefef",
            div(class = "content", 
                div(class = "header", style = "padding-bottom: 10px", 
                    tags$img(src = "https://media-exp1.licdn.com/dms/image/C4D03AQEtb1ffLbF0Vg/profile-displayphoto-shrink_400_400/0/1643919380964?e=1649894400&v=beta&t=_2FcqCghH_ZjquQ4ptMkNQ_yx9xS8cvYDa7CDVTD4ko", 
                             class = "ui avatar image"), 
                    "Jamie Ralph", 
                    span(style = "color: #0099f9; font-size: 13px;", 
                         icon("linkedin"), 
                         tags$a(href = "https://www.linkedin.com/in/jamie-r-ralph/", style = "text-decoration: underline", "jamie-ralph"
                                )
                         )
                    )
            )
        ),
        table = summary_grid
    )
)

about <- div(
    p(glue("Hello and welcome {emo::ji('smile')}. This Shiny app
           displays some interesting data about my home country, Wales
           {emo::ji('dragon')}.")
    ),
    br(),
    html(glue("You can find the source code {a('here.', href = 'https://github.com/jamie-ralph/wales-population-app')}"))
)

                      
router <- make_router(
    route("/", pageOne),
    route("about", about)
)

ui <- shiny::fluidPage(
    theme = "css/main.css",
    tags$ul(
        tags$li(a(href = route_link("/"), "Visualisation")),
        tags$li(a(href = route_link("/about"), "About"))
    ),
    router$ui
)
    



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    router$server(input, output, session)

    output$linePlot <- renderPlot({
        
        population %>%
            filter(area_name %in% input$choice) %>%
            ggplot(mapping = aes(x = year, y = pop_estimate)) +
                geom_line() +
                labs(y = "Population estimate",
                     x = "Year",
                     title = paste0("Population estimates for ",
                                    input$choice)) +
                theme_bw()
    })
    
    output$summary <- renderUI({
        la_label <- input$choice
        
        hist_pop <- population %>%
            filter(year == 1991, area_name == input$choice) %>%
            pull(pop_estimate)
        
        current_pop <- population %>%
            filter(year == 2020, area_name == input$choice) %>%
            pull(pop_estimate)
        
        hist_change <- round(((current_pop / hist_pop) * 100) - 100, 1)
        
        if (hist_change > 0) {
            hist_change <- paste0("+", hist_change)
        }
        
        summary_text <- glue(
            "The latest population estimate for 
            {input$choice} is {format(current_pop, big.mark = ',')}. ",
            "In 1991, the population was {format(hist_pop, big.mark = ',')}. ",
            "This is a change of {hist_change}%."
        )
        
        div(card(div(class = "content", div(class = "header", "Summary"),
                     div(class = "meta", la_label),
                     div(class = "description", summary_text))))
    })
    
    pop_table <- reactive({
        population %>%
            filter(area_name %in% input$choice) %>%
            select(Year = year,
                   Population = pop_estimate) %>%
            arrange(desc(Year)) %>%
            top_n(5, Year) %>%
            gt() %>%
            fmt_number(columns = Population, decimals = 0) %>%
            opt_table_font(font = google_font("Poppins")) %>%
            data_color(
                columns = Population,
                colors = scales::col_numeric(
                    palette = c("white", "lightgray"),
                    domain = c(min(Population), max(Population))
                )
            )
    })
    
    output$great_table <- render_gt({
        expr = pop_table()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
