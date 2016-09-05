library(shiny)
library(tibble)
library(plotly)
library(igraph)

if ("porgat.txt" %in% dir() == FALSE) 
{
        download.file("http://bioinfo.uib.es/~joemiro/marvel/porgat.txt","porgat.txt")
        }


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session,clientData) {
   
        

        
        graph <- read_graph("porgat.txt",format="pajek")
        
        type <- vertex_attr(graph, "type")
        
        index <- which(type == "FALSE")
        
        labels <- vertex_attr(graph, "id")[index]
        
        search <- reactive({
                target_index <- which(grepl(as.character(input$query),labels,ignore.case = TRUE))
                search_results <- labels[target_index]   
                if (length(search_results) == 0) { search_results <- "No matching character. Try another name!"
                        } else search_results
        })
        
        s_options <- eventReactive(input$search_button,{
                
        s <- list()
        
        for (i in 1:length(search())) {
                
                s[[search()[i]]] <- search()[i]
        }
        
        s
        
        })
        
        observe({
        updateSelectInput(session, "character",
                          choices = s_options()
        )
        })
        
        selected_character <- reactive({
        
                shiny::validate(need(search() != "No matching character. Try another name!" , "Please select an existing character first!"),
                                need(input$character != "Search for Character" , "Please search for an existing character first!")
                                )
                input$character
                
        })
        
        output$plot <- eventReactive(input$select_button,{
                
        progress <- shiny::Progress$new()
        progress$set(message = "Creating Map View...", value = 0)
        on.exit(progress$close())
        
        graphing <- renderPlotly({ 
                
        target <- selected_character()       
                
        target_index <- which(labels == target)
        
        neighbours_index <- as.numeric(ego(graph, order=2, nodes = target_index,
                       mindist = 2)[[1]])
        
        neighbours_and_comics_index <- as.numeric(ego(graph, order=2, nodes = target_index,
                                           mindist = 1)[[1]])
        
        graph2 <- induced_subgraph(graph,neighbours_and_comics_index,impl = "copy_and_delete")
        
        degrees <- sqrt(degree(graph2))
        
        neighbours_and_comics_degrees <- tibble("vertex"= vertex_attr(graph2,"id"), "degree" = degrees)
        
        keep_index <- sapply(neighbours_and_comics_degrees[,1],FUN=function(f) {
                which(f %in% labels)
                
        }    )
        
        neighbours_degrees <- neighbours_and_comics_degrees[keep_index,]
        
        neighbours <- neighbours_degrees[[1]]
        
        target_edgelist <- tibble("A"=target,"B"=target,"size"=max(neighbours_degrees[[2]]))
        
        for (g in 1:length(neighbours)) {
        
        target_edgelist <- add_row(target_edgelist,"A"=target,"B"=neighbours[g],"size" = neighbours_degrees[[g,2]])
        
        }
        #removing duplicate entries
        if (length(which(duplicated(target_edgelist[[2]])) > 0)) {
        
        target_edgelist <- target_edgelist[-which(duplicated(target_edgelist[[2]])),]
        }
        
        graph <- graph_from_data_frame(target_edgelist)
        L <- layout_nicely(graph)
        vs <- V(graph)
        es <- as.data.frame(get.edgelist(graph))
        Ne <- length(es[1]$V1)
        Xn <- L[,1]
        Yn <- L[,2]
        
        size <- target_edgelist[[3]]
        
        txt <- list(
                family = "calibri",
                size = 10,
                color = toRGB("grey50"),
                opacity = 0.3
        )
        
        network <- plot_ly(type = "scatter", x = Xn, y = Yn, mode = "markers+text", 
                           text = names(vs), hoverinfo = "text",size = target_edgelist[[3]], textfont = txt)
        
        edge_shapes <- list()
        for(i in 1:Ne) {
                v0 <- es[i,]$V1
                v1 <- es[i,]$V2
                
                edge_shape = list(
                        type = "line",
                        line = list(color = "#890D0D", width = target_edgelist[[i,3]]),
                        x0 = Xn[match(v0,names(vs))],
                        y0 = Yn[match(v0,names(vs))],
                        x1 = Xn[match(v1,names(vs))],
                        y1 = Yn[match(v1,names(vs))],
                        opacity = target_edgelist[[i,3]] / 30
                )
                
                edge_shapes[[i]] <- edge_shape}
        
        network <- layout(
                network,
                hovermode = "closest",
                title = paste(target,"'s Relations",sep = ""),
                shapes = edge_shapes,
                xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
                yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
        )
        network
        
        
        })
        
        graphing()
        
        
        
        
        })

})

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
        tags$head(
                tags$style(HTML("
                                body {
                                background-color: #890D0D ;
                                font-family: Calibri;
                                color:
                                }

                                h2 {
                                color: white
                                }
                                .shiny-output-error-validation {
                                color: #FF9C0E;
                                font-size: 18px;
                                font-weight: bold
                                }

                                div#app_info.out.collapse.in {
                                min-height: 20px;
                                padding: 19px;
                                margin-bottom: 20px;
                                color: black;
                                background-color: #f5f5f5;
                                border: 1px solid #e3e3e3;
                                border-radius: 4px;
                                -webkit-box-shadow: inset 0 1px 1px rgba(0, 0, 0, .05);
                                box-shadow: inset 0 1px 1px rgba(0, 0, 0, .05);
                                }
                                
                                "))),
        
        # Application title
        titlePanel("Marvel Character Graph"),
        
        tags$body(HTML("

                        <div id='app_info' class='collapse out'>
                        <p>This app allows you to visualise the personal relationship network of comic book characters from the Marvel Universe.
                        <br>Fictional social graphs such as this behave in a similar fashion as real social networks and this app showcases how the immediate neighbours of an entity (in this case, a comic book character) can be visualised in order to gain insight as to who represent the most important connections for the subject node.
                        The data for the app was obtained from <b>Joe Miro</b>'s research project website - for more details about the underlying data, visit <a href='http://bioinfo.uib.es/~joemiro/marvel.html'>here</a>.
                        <br>In order to create the social graph, just type in a name of a Marvel character (can be partial name too) and click on the <b>Search Character</b> button.
                        A matching list of characters will appear in the <b>Select Match</b> dropdown menu. Choose the one you are interested in and then press <b>Choose Character</b>. 
                        A network graph will soon be constructed, showing you all the neighbours of the given character. These represent all the characters who had a major appearance in the same comic books as your chosen character.
                        At the centre of the network, you can see your chosen character, with its relations displayed by filled circle nodes. The size of these nodes, along with the width and opacity of the connecting link, shows the frequency with which the character appeared 
                        in the same comic book. The larger the node, the more important the given character is in the fictional life of your chosen super hero.
                        </p>
                        </div>
                        <button type='button' class='btn' data-toggle='collapse' style='float:left' data-target='#app_info'><span class='glyphicon glyphicon-collapse-down'></span> More Information</button>
                        <br/>
                        <br/>       
                       
                       ")),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        textInput(inputId = "query", "Search for Character Name",placeholder = "Spider-man"),
                        actionButton("search_button", "Search Character"),
                        selectInput(inputId = "character",label = "Select Match",selectize = FALSE, choices = c("Search for Character")),
                        actionButton("select_button", "Choose Character")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        plotlyOutput("plot",height = "1200px"),
                        width = 12
                        
                )
        )
))




shinyApp(ui = ui, server = server)


