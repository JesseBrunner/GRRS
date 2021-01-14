library(shiny)
library(leaflet)
library(tidyverse)
library( mapview )
library(viridis)

# Read in data
grrs_map <- read_csv("GRRS_Map.csv")
meta_data <- read_csv("MetaData.csv")
template <- read_csv("GRRS_template.csv")

# Initial color pallete
pal <- colorFactor(palette = viridis(5)[-5],
                   grrs_map$populationType)


# User interface
ui <- fluidPage(title = "Global Ranavirus Reporting System",

                tabsetPanel(
                    tabPanel(title = "GRRS Map",
                             tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                             leafletOutput("map", width = "100%", height = "700")
                    ),
                    tabPanel( title = "Filter & download data",
                              selectInput("type", "Population types:",
                                          choices = c("Wild" = "wild",
                                                      "Production" = "production",
                                                      "Zoological" = "zoological"),
                                          selected = c("wild", "production", "zoological"),
                                          multiple = TRUE
                              ),
                              selectInput("taxa", "Taxa:",
                                          choices = c("Amphibians" = "amphibian",
                                                      "Reptiles" = "reptile",
                                                      "Fishes" = "fish"),
                                          selected = c("amphibian", "reptile", "fish"),
                                          multiple = TRUE
                              ),
                              selectInput("genera", "Genera: (select last)",
                                          choices = c(sort(unique(grrs_map$Genus)), "NA"),
                                          selected = c(sort(unique(grrs_map$Genus)), "NA"),
                                          multiple = TRUE
                              ),

                              downloadButton( outputId = "dl",
                                              label = "Download filtered data"
                              )
                              # ,
                              # downloadButton( outputId = "md",
                              #                 label = "Download metadata"
                              # )
                    ),
                    tabPanel(title = "About",
                             p("Global Ranavirus Reporting System was designed as a central, open-access, online database in which to report and track the occurrence of ranavirus presence or absence around the world."),
                             p("Please see the meta data or the manuscript in FACETS for more information about these records."),
                             downloadButton( outputId = "md",
                                             label = "Download metadata"),

                             h3("Acknowledgements"),
                             p("This project would not be possible without the support of many people. The original version of the GRRS was developed with the assistance of a team from the",
                               a(href = "https://www.ecohealthalliance.org/", "EcoHealth Alliance"),
                               "led by Andrew Huff and Russell Horton, and with the financial support of the U.S. Forest Service."),
                             p("The database was populated in large part by Amanda Duffus, Jesse Brunner, and many members of the Brunner lab, in particular Ana Trejo. We are also pleased to acknowledge the support of the Wildlife Disease Association for a small grant to support the data entry."),
                             h3("Citation"),
                             p("Please cite the GRRS as:"),
                             p("Brunner, J. L., D. H. Olson, M. J. Gray, D. L. Miller, and A. L. J. Duffus. in review. Global patterns of ranavirus reports in the GRRS. FACETS"),

                             h3("Add to or correct the GRRS"),
                             p("This is a work in progress. If you notice a record missing or incorrect, please let us know! First download and fill out to the best of your abilities the template spreadsheet (below). You may want to look at the metadata and the GRRS data for guidance. Then please email the spreadsheet to",
                               a(href = "mailto:jesse.brunner@wsu.edu?subject=GRRS", "Jesse Brunner."),
                               "We appreciate your help!"),
                             downloadButton( outputId = "template",
                                             label = "Download template"
                             )
                    )

                )

)

server <- function(input, output, session) {

    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        df <- grrs_map %>% filter(vertebrateClass %in% input$taxa &
                                      populationType %in% input$type)

        # Remake the genera list given the other choices
        updateSelectInput(session, "genera",
                          # label = paste("Select input label", length(x)),
                          choices = sort(unique(df$Genus)),
                          selected = sort(unique(df$Genus))
        )
        return(df)
    })
    # Reactive to subset data by genera (Do last, or is reset)
    filteredData2 <- reactive({
        filteredData() %>% filter(Genus %in% input$genera)
    })


    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    # colorpal <- reactive({
    #     colorFactor(palette = viridis(4)[-4], grrs_map$populationType)
    # })

    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).

        grrs_map %>%
            leaflet(data = . ) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            fitBounds(lng1 = min(grrs_map$long), lng2 = max(grrs_map$long),
                      lat1 = min(grrs_map$lat), lat2 = max(grrs_map$lat)) %>%
            # adding in all of the dots so we start with a full map
            addCircles(color = ~pal(populationType),
                       opacity = 1,
                       label = ~populationType,
                       popup = ~paste(studyTitle,  "<br/>",
                                      "Class:", vertebrateClass,  "<br/>",
                                      "Family:", Family,  "<br/>",
                                      "Genus:", "<i>", Genus,  "</i><br/>",
                                      "involved:", numInvolved,  "<br/>",
                                      "infected/tested:", totalAnimalsConfirmedInfected, "/", totalAnimalsTested, "<br/>",
                                      "# diseased:", totalAnimalsConfirmedDiseased)
            )

    })

    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        df <- filteredData2()
        # If missing input, return to avoid error later in function
        if (nrow(df) == 0) return(NULL)

        # pal <- colorpal()

        leafletProxy("map", data = df) %>%
            clearShapes() %>%
            addCircles(color = ~pal(populationType),
                       opacity = 1,
                       label = ~populationType,
                       popup = ~paste(studyTitle,  "<br/>",
                                      "Class:", vertebrateClass,  "<br/>",
                                      "Family:", Family,  "<br/>",
                                      "Genus:", "<i>", Genus,  "</i><br/>",
                                      "involved:", numInvolved,  "<br/>",
                                      "infected/tested:", totalAnimalsConfirmedInfected, "/", totalAnimalsTested, "<br/>",
                                      "# diseased:", totalAnimalsConfirmedDiseased)
            )
    })

    # Use a separate observer to recreate the legend as needed.
    observe({
        # pal <- colorpal()

        proxy <- leafletProxy("map", data = filteredData()) %>%
            clearControls() %>%
            addLegend(title = "Population type",
                      position = "bottomright",
                      pal = pal, opacity = 1,
                      values = ~populationType
            )
    })

    # Downloadable csv of selected dataset ----
    output$dl <- downloadHandler(
        filename = "GRRS_data.csv",
        content = function(file) {
            write.csv(filteredData(), file, row.names = FALSE)
        }
    )

    output$md <- downloadHandler(
        filename = "GRRS_metadata.csv",
        content = function(file) {
            write.csv(meta_data, file, row.names = FALSE)
        }
    )

    output$template <- downloadHandler(
        filename = "GRRS_template.csv",
        content = function(file) {
            write.csv(template, file, row.names = FALSE)
        }
    )


}

shinyApp(ui, server)