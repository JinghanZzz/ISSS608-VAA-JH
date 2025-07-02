# app.R


library(shiny)
library(tidygraph)
library(ggraph)
library(jsonlite)
library(dplyr)
library(ggplot2)

library(pacman)
p_load(tidygraph, visNetwork, jsonlite, dplyr, rlang, igraph)

library(tidyverse)
library(plotly)
library(DT)
library(scales)
library(forcats)
library(visNetwork)
library(SmartEDA)
library(patchwork)


kg1 <- fromJSON("data/MC1_graph.json")
nodes1_tbl <- kg1$nodes
edges1_tbl <- kg1$links

id_map1 <- tibble(id = nodes1_tbl$id, index = seq_len(nrow(nodes1_tbl)))

edges1_tbl <- edges1_tbl %>%
  left_join(id_map1, by = c("source" = "id")) %>% rename(from = index) %>%
  left_join(id_map1, by = c("target" = "id")) %>% rename(to = index) %>%
  filter(!is.na(from), !is.na(to))

graph1 <- tbl_graph(nodes = nodes1_tbl, edges = edges1_tbl, directed = kg1$directed)


top3_names <- c("Kimberly Snyder", "Ping Tian", "Yang Zhao")


# ===== UI =====
ui <- navbarPage("Sailor Shift: Rise and Resonance",
                 
                 # --- Tab 1: Artist Explorer 
                 tabPanel("Artist Explorer",
                          fluidPage(
                            titlePanel("🎵 Artist Network Explorer"),
                            tabsetPanel(
                              tabPanel("Contribution Network",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("selected_artists", "Select Artists:",
                                                       choices = graph1 %>% activate(nodes) %>% as_tibble() %>% filter(`Node Type` == "Person") %>% pull(name),
                                                       selected = top3_names, multiple = TRUE
                                           ),
                                           checkboxGroupInput("edge_types", "Select Contribution Types:",
                                                              choices  = c("ComposerOf", "ProducerOf", "LyricistOf"),
                                                              selected = c("ComposerOf", "ProducerOf", "LyricistOf")
                                           )
                                         ),
                                         mainPanel(plotOutput("network_plot", height = "800px"))
                                       )
                              ),
                              tabPanel("Influence Network",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("artist_name", "Choose an Artist:",
                                                       choices = c(
                                                         top3_names,
                                                         setdiff(
                                                           graph1 %>% activate(nodes) %>% as_tibble() %>% filter(`Node Type` == "Person") %>% pull(name),
                                                           top3_names
                                                         )
                                                       ),
                                                       selected = top3_names[1]
                                           ),
                                           checkboxGroupInput("influence_types", "Select Influence Types:",
                                                              choices  = c("DirectlySamples","LyricalReferenceTo","CoverOf","InterpolatesFrom","InStyleOf"),
                                                              selected = c("DirectlySamples","LyricalReferenceTo","CoverOf","InterpolatesFrom","InStyleOf")
                                           )
                                         ),
                                         mainPanel(plotOutput("artist_plot", height = "800px"))
                                       )
                              ),
                              tabPanel("Next Rising Stars",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("rising_stars", "Select Rising Stars:",
                                                       choices = {
                                                         all_names <- nodes1_tbl %>% filter(`Node Type`=="Person") %>% pull(name)
                                                         pinned    <- c("Copper Canyon Ghosts","Beatrice Albright","Daniel O'Connell")
                                                         c(pinned, setdiff(sort(all_names), pinned))
                                                       },
                                                       selected = c("Copper Canyon Ghosts","Beatrice Albright","Daniel O'Connell"),
                                                       multiple = TRUE
                                           )
                                         ),
                                         mainPanel(plotOutput("rising_star_plot", height = "800px"))
                                       )
                              )
                            )
                          )
                 ),
                 
                 # --- Tab 2: Career Explorer 
                 tabPanel("Career Explorer",
                          fluidPage(
                            titlePanel("Sailor Shift Career Explorer"),
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("view2", "Select View:",
                                             choices = c("Influence"="influence", "Collaboration"="collab", "Community"="community"),
                                             selected = "influence"
                                ),
                                conditionalPanel("input.view2=='influence'",
                                                 checkboxGroupInput("inf_types", "Influence Type:",
                                                                    choices  = c("ComposerOf","ProducerOf","LyricistOf","CoverOf"),
                                                                    selected = c("ComposerOf","ProducerOf")
                                                 )
                                ),
                                conditionalPanel("input.view2=='collab'",
                                                 numericInput("depth", "Depth:", value=1, min=1, max=3)
                                ),
                                conditionalPanel("input.view2=='community'",
                                                 numericInput("com_depth", "Depth:", value=1, min=1, max=2)
                                ),
                                helpText("Click nodes for details")
                              ),
                              mainPanel(visNetworkOutput("network2", height="700px"))
                            )
                          )
                 ),
                 
                 # --- Tab 3: Influence Explorer 
                 tabPanel("Influence Explorer",
                          fluidPage(
                            titlePanel("\U1F30A Oceanus Folk Influence Explorer"),
                            tabsetPanel(
                              tabPanel("Influence Timeline",
                                       sidebarLayout(
                                         sidebarPanel(
                                           sliderInput("year_range", "Select Year Range:",
                                                       min=1990, max=2040, value=c(1990,2040), sep="", step=1
                                           ),
                                           numericInput("bin_size", "Heatmap Bin Size (years):", value=5, min=1, max=20)
                                         ),
                                         mainPanel(plotlyOutput("timeline_plot"))
                                       )
                              ),
                              tabPanel("Genre Impact",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("genre_select", "Select Genre(s):",
                                                       choices  = {
                                                         tmp <- as_tibble(kg1$nodes)
                                                         tmp %>% filter(`Node Type` %in% c("Song","Album")) %>% distinct(genre) %>% arrange(genre) %>% pull(genre)
                                                       },
                                                       selected = {
                                                         tmp <- as_tibble(kg1$nodes)
                                                         tmp %>% filter(`Node Type` %in% c("Song","Album")) %>% distinct(genre) %>% arrange(genre) %>% pull(genre)
                                                       },
                                                       multiple = TRUE
                                           ),
                                           selectInput("work_select", "Select Work(s):", choices=NULL, multiple=TRUE),
                                           selectInput("influence_type", "Influence Type:",
                                                       choices = tolower(c("InStyleOf","CoverOf","InterpolatesFrom","DirectlySamples","LyricalReferenceTo")),
                                                       selected = tolower(c("InStyleOf","CoverOf","InterpolatesFrom","DirectlySamples","LyricalReferenceTo")),
                                                       multiple = TRUE
                                           )
                                         ),
                                         mainPanel(visNetworkOutput("genre_impact_network", height="700px"))
                                       )
                              )
                            )
                          )
                 )
)



# ===== SERVER =====
server <- function(input, output, session) {
  
  # ----- Script1 Server -----
  # Contribution Network
  output$network_plot <- renderPlot({
    req(input$selected_artists, input$edge_types)
    
    artist_ids <- graph1 %>%
      activate(nodes) %>% as_tibble() %>%
      filter(name %in% input$selected_artists) %>% pull(id)
    
    graph_selected_edges <- graph1 %>%
      activate(edges) %>%
      filter(`Edge Type` %in% input$edge_types &
               (source %in% artist_ids | target %in% artist_ids))
    
    used_node_indices <- graph_selected_edges %>%
      activate(edges) %>% as_tibble() %>%
      select(from,to) %>% unlist() %>% unique()
    
    graph_selected <- graph_selected_edges %>%
      activate(nodes) %>% mutate(row_id=row_number()) %>%
      filter(row_id %in% used_node_indices) %>% select(-row_id) %>%
      activate(nodes) %>%
      mutate(
        degree = centrality_degree(),
        notable_flag   = ifelse(!is.na(notoriety_date),"Notable","Other"),
        release_year   = as.integer(release_date),
        label_with_year = case_when(
          `Node Type` %in% c("Song","Album") & !is.na(release_year) ~ paste0(name,"\n(",release_year,")"),
          TRUE ~ name
        ),
        size = case_when(
          `Node Type` == "Person" ~ degree*1.5,
          !is.na(notoriety_date)   ~ 6,
          TRUE                     ~ 2
        )
      )
    
    ggraph(graph_selected, layout="fr") +
      geom_edge_link(aes(color=`Edge Type`), alpha=0.6) +
      geom_node_point(aes(color=`Node Type`, size=size, shape=notable_flag)) +
      geom_node_text(aes(label=label_with_year), repel=TRUE, size=3) +
      scale_edge_color_manual(values=c(
        ComposerOf="#1f77b4", ProducerOf="#2ca02c", LyricistOf="#9467bd"
      )) +
      scale_color_manual(values=c(
        Person="steelblue", Song="gray40", Album="tomato"
      )) +
      scale_shape_manual(values=c(Notable=17, Other=16)) +
      theme_minimal(base_size=14) +
      labs(
        title    = "Artist Contribution Network",
        subtitle = paste("Artists:", paste(input$selected_artists, collapse=", ")),
        caption  = "▲ = notable work · Node size = influence degree"
      )
  })
  
  # Influence Network
  output$artist_plot <- renderPlot({
    req(input$artist_name)
    
    artist_id <- graph1 %>%
      activate(nodes) %>% as_tibble() %>%
      filter(name == input$artist_name) %>% pull(id)
    
    performed_work_ids <- graph1 %>%
      activate(edges) %>%
      filter(`Edge Type`=="PerformerOf", source==artist_id) %>%
      as_tibble() %>% pull(target)
    
    req(input$influence_types)
    
    graph_influence_edges <- graph1 %>%
      activate(edges) %>%
      filter(`Edge Type` %in% input$influence_types &
               (source %in% performed_work_ids | target %in% performed_work_ids))
    
    used_node_ids <- graph_influence_edges %>%
      activate(edges) %>% as_tibble() %>%
      select(from,to) %>% unlist() %>% unique()
    
    graph_artist_sub <- graph_influence_edges %>%
      activate(nodes) %>% mutate(row_id=row_number()) %>%
      filter(row_id %in% used_node_ids) %>% select(-row_id) %>%
      activate(nodes) %>%
      mutate(
        degree = centrality_degree(),
        notable_flag   = ifelse(!is.na(notoriety_date),"Notable","Other"),
        release_year   = as.integer(release_date),
        label_with_year = case_when(
          `Node Type` %in% c("Song","Album") & !is.na(release_year) ~ paste0(name,"\n(",release_year,")"),
          TRUE ~ name
        ),
        size = case_when(
          `Node Type` == "Person" ~ degree*1.5,
          !is.na(notoriety_date)   ~ 6,
          TRUE                     ~ 2
        )
      )
    
    ggraph(graph_artist_sub, layout="fr") +
      geom_edge_link(aes(color=`Edge Type`), alpha=0.6) +
      geom_node_point(aes(color=`Node Type`, size=size, shape=notable_flag)) +
      geom_node_text(aes(label=label_with_year), repel=TRUE, size=3) +
      scale_edge_color_manual(values=c(
        DirectlySamples="#1f77b4",
        LyricalReferenceTo="#9467bd",
        CoverOf="#ff7f0e",
        InterpolatesFrom="#2ca02c",
        InStyleOf="#8c564b"
      )) +
      scale_color_manual(values=c(
        Person="steelblue", Song="gray40", Album="tomato"
      )) +
      scale_shape_manual(values=c(Notable=17, Other=16)) +
      theme_minimal(base_size=14) +
      labs(
        title    = paste("Influence Network of", input$artist_name),
        subtitle = "Covers, interpolations, stylistic callbacks, and lyrical references",
        caption  = "▲ = notable work · Node size = influence degree"
      )
  })
  
  # Next Rising Stars
  output$rising_star_plot <- renderPlot({
    req(input$rising_stars)
    top3 <- input$rising_stars
    
    artist_song_edges <- edges1_tbl %>%
      filter(`Edge Type`=="PerformerOf") %>%
      semi_join(nodes1_tbl %>% filter(name %in% top3), by=c("source"="id")) %>%
      transmute(artist_id=source, song_id=target)
    
    artist_nodes <- nodes1_tbl %>% filter(name %in% top3) %>% mutate(label=name, type="Artist")
    song_nodes   <- nodes1_tbl %>% filter(id %in% artist_song_edges$song_id) %>% mutate(label=name, type="Song")
    
    all_nodes  <- bind_rows(artist_nodes, song_nodes) %>% mutate(graph_id=row_number())
    node_map   <- all_nodes %>% select(id, graph_id)
    edges_map  <- artist_song_edges %>%
      left_join(node_map, by=c("artist_id"="id")) %>% rename(from=graph_id) %>%
      left_join(node_map, by=c("song_id"="id"))   %>% rename(to  =graph_id) %>%
      select(from,to)
    
    song_graph <- tbl_graph(nodes=all_nodes, edges=edges_map, directed=FALSE)
    
    ggraph(song_graph, layout="fr") +
      geom_edge_link(color="gray80", alpha=0.6) +
      geom_node_point(aes(color=type, shape=type, size=type)) +
      geom_node_text(aes(label=label), repel=TRUE, size=3.2) +
      scale_color_manual(values=c(Artist="#D55E00", Song="steelblue")) +
      scale_shape_manual(values=c(Artist=17, Song=16)) +
      scale_size_manual(values=c(Artist=6, Song=3)) +
      labs(
        title    = "Top 3 Rising Stars and Their Performed Works",
        subtitle = "Network visualization of artist-song relationships",
        caption  = "Source: Oceanus Folk Archive | Chart by FU YILIN"
      ) +
      theme_void() +
      theme(
        plot.title    = element_text(face="bold", size=16, hjust=0.5),
        plot.subtitle = element_text(size=11, hjust=0.5),
        legend.position="bottom"
      )
  })
  
  
  # ----- Script2 Server -----

  kg2 <- fromJSON("data/MC1_graph.json", simplifyDataFrame = TRUE)
  nodes2_df <- as_tibble(kg2$nodes)
  edges2_df <- as_tibble(kg2$links)
  
  id_map2 <- tibble(id = nodes2_df$id, index = seq_len(nrow(nodes2_df)))
  edges2_df <- edges2_df %>%
    left_join(id_map2, by = c("source" = "id")) %>% rename(from = index) %>%
    left_join(id_map2, by = c("target" = "id")) %>% rename(to   = index) %>%
    filter(!is.na(from), !is.na(to))
  
  graph2_tbl <- tbl_graph(nodes = nodes2_df, edges = edges2_df, directed = kg2$directed)
  sailor_idx <- which(nodes2_df$name == "Sailor Shift")
  
  extract_subgraph2 <- function(graph, expr) {
    g_sub <- graph %>% activate(edges) %>% filter(!!enquo(expr))
    used <- g_sub %>% activate(edges) %>% as_tibble() %>% select(from,to) %>% unlist() %>% unique()
    g_sub %>% activate(nodes) %>% mutate(.row=row_number()) %>% filter(.row %in% used)
  }
  get_influence2 <- function(types) {
    works <- graph2_tbl %>% activate(edges) %>% as_tibble() %>%
      filter(`Edge Type`=="PerformerOf", from==sailor_idx) %>% pull(to) %>% unique()
    focus <- unique(c(sailor_idx, works))
    extract_subgraph2(graph2_tbl, (`Edge Type` %in% types) & (to %in% focus))
  }
  get_collab2 <- function(depth) {
    ig  <- as.igraph(graph2_tbl)
    d   <- distances(ig, v=sailor_idx, to=V(ig), mode="all")
    valid <- which(d <= depth)
    extract_subgraph2(graph2_tbl,
                      (`Edge Type` %in% c("PerformerOf","ComposerOf","ProducerOf","LyricistOf","CoverOf")) &
                        (from %in% valid & to %in% valid)
    )
  }
  get_community2 <- function(depth=1) {
    folk_idx <- graph2_tbl %>% activate(nodes) %>% as_tibble() %>% mutate(.row=row_number()) %>%
      filter(genre=="Oceanus Folk") %>% pull(.row)
    base_sub <- extract_subgraph2(graph2_tbl,
                                  (`Edge Type`=="PerformerOf") & (from==sailor_idx & to %in% folk_idx)
    )
    if(depth==1) return(base_sub)
    creative_types <- c("ComposerOf","ProducerOf","LyricistOf")
    work_ids <- base_sub %>% activate(nodes) %>% as_tibble() %>%
      mutate(.row=row_number()) %>% filter(`Node Type` %in% c("Album","Song")) %>% pull(.row)
    creative_sub <- extract_subgraph2(graph2_tbl,
                                      (`Edge Type` %in% creative_types) & (to %in% work_ids)
    )
    edges_all <- bind_rows(
      base_sub   %>% activate(edges)%>% as_tibble(),
      creative_sub%>% activate(edges)%>% as_tibble()
    )
    nodes_all <- bind_rows(
      base_sub   %>% activate(nodes)%>% as_tibble(),
      creative_sub%>% activate(nodes)%>% as_tibble()
    ) %>% distinct(name, .keep_all=TRUE) %>% mutate(.row=row_number())
    tbl_graph(nodes=nodes_all, edges=edges_all, directed=kg2$directed)
  }
  
  output$network2 <- renderVisNetwork({
    subg2 <- switch(input$view2,
                    influence = get_influence2(input$inf_types),
                    collab    = get_collab2(input$depth),
                    community = get_community2(input$com_depth)
    )
    vn <- toVisNetworkData(subg2)
    nodes_v <- vn$nodes %>% rename(name=label) %>%
      mutate(id=as.character(id), label=name, group=`Node Type`)
    edges_v <- vn$edges %>% mutate(
      label = as.character(`Edge Type`),
      title = paste0("Edge Type: ", label)
    )
    visNetwork(nodes_v, edges_v, height="700px", width="100%") %>%
      visEdges(arrows="to") %>%
      visOptions(
        highlightNearest=list(enabled=TRUE, degree=1, hover=TRUE),
        nodesIdSelection=TRUE,
        selectedBy="group"
      ) %>%
      visLegend() %>%
      visLayout(randomSeed=2025) %>%
      visInteraction(navigationButtons=TRUE)
  })
  
  
  # ----- Script3 Server -----

  kg3 <- fromJSON("data/MC1_graph.json")
  gk_nodes <- as_tibble(kg3$nodes) %>%
    mutate(
      release_date   = as.integer(release_date),
      written_date   = as.integer(written_date),
      notoriety_date = as.integer(notoriety_date)
    )
  gk_edges <- as_tibble(kg3$links)
  
  id_map3 <- tibble(id = gk_nodes$id, index = seq_len(nrow(gk_nodes)))
  edges3_tbl <- gk_edges %>%
    left_join(id_map3, by = c("source" = "id")) %>% rename(from = index) %>%
    left_join(id_map3, by = c("target" = "id")) %>% rename(to = index) %>%
    filter(!is.na(from), !is.na(to))
  
  nodes3_tbl <- gk_nodes
  edges3_tbl <- edges3_tbl %>% mutate(`Edge Type` = tolower(`Edge Type`))
  
  influence_types3 <- c("instyleof","coverof","interpolatesfrom","directlysamples","lyricalreferenceto")
  
  all_genres <- nodes3_tbl %>%
    filter(`Node Type` %in% c("Song","Album")) %>%
    distinct(genre) %>% arrange(genre) %>% pull(genre)
  
  all_years <- nodes3_tbl %>% filter(!is.na(release_date)) %>% pull(release_date)
  min_year3 <- min(all_years, na.rm=TRUE)
  max_year3 <- max(all_years, na.rm=TRUE)
  
  # Influence Timeline
  output$timeline_plot <- renderPlotly({
    df <- nodes3_tbl %>%
      filter(genre=="Oceanus Folk", `Node Type` %in% c("Song","Album"),
             !is.na(release_date),
             between(release_date, input$year_range[1], input$year_range[2])) %>%
      count(release_date, `Node Type`, name="count") %>%
      pivot_wider(names_from=`Node Type`, values_from=count, values_fill=0) %>%
      mutate(
        total   = Song + Album,
        year_bin = floor(release_date / input$bin_size) * input$bin_size
      )
    
    heatmap_df <- df %>%
      group_by(year_bin) %>%
      summarise(bin_total = sum(total), .groups="drop") %>%
      arrange(year_bin)
    
    p <- ggplot() +
      geom_tile(data = heatmap_df,
                aes(x = year_bin + input$bin_size/2,
                    y = -5,
                    fill = bin_total,
                    text = paste0("Period: ", year_bin, "–", year_bin+input$bin_size-1,
                                  "<br>Total: ", bin_total)
                ),
                width  = input$bin_size, height = 3
      ) +
      geom_line(data=df, aes(x=release_date, y=total, group=1), linewidth=1) +
      geom_point(data=df,
                 aes(x=release_date, y=total,
                     text = paste0("Year: ", release_date, "<br>Total: ", total)
                 ),
                 size=2
      ) +
      scale_fill_gradient(low="#a8dadc", high="#2f4b7c") +
      labs(
        title="Oceanus Folk Releases by Year with Heatmap",
        x="Release Year", y="Total Count"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip="text")
  })
  

  observe({
    edge_graphdata <- edges3_tbl %>%
      left_join(nodes3_tbl %>% select(id, genre_to=genre), by=c("to"="id"))
    of_publications <- nodes3_tbl %>%
      filter(`Node Type` %in% c("Song","Album"), genre=="Oceanus Folk") %>%
      pull(id)
    of_outgoing_edges <- edge_graphdata %>%
      filter(from %in% of_publications, `Edge Type` %in% input$influence_type)
    target_metadata <- of_outgoing_edges %>%
      left_join(nodes3_tbl %>% select(id, name_target=name, genre_target=genre, type_target=`Node Type`),
                by=c("to"="id")) %>%
      filter(genre_target %in% input$genre_select) %>%
      distinct(to, name_target, genre_target, type_target)
    work_choices <- setNames(as.character(target_metadata$to), target_metadata$name_target)
    updateSelectInput(session, "work_select", choices=work_choices, selected=work_choices)
  })
  
  # Genre Impact Network
  output$genre_impact_network <- renderVisNetwork({
    edge_graphdata <- edges3_tbl %>%
      left_join(nodes3_tbl %>% select(id, genre_to=genre), by=c("to"="id"))
    of_publications <- nodes3_tbl %>%
      filter(`Node Type` %in% c("Song","Album"), genre=="Oceanus Folk") %>% pull(id)
    of_outgoing_edges <- edge_graphdata %>%
      filter(from %in% of_publications, `Edge Type` %in% input$influence_type)
    target_metadata <- of_outgoing_edges %>%
      left_join(nodes3_tbl %>% select(id, name_target=name, genre_target=genre, type_target=`Node Type`),
                by=c("to"="id")) %>%
      filter(genre_target %in% input$genre_select) %>%
      distinct(to, name_target, genre_target, type_target)
    

    if(!is.null(input$work_select)) {
      of_outgoing_edges <- of_outgoing_edges %>% filter(to %in% as.numeric(input$work_select))
      target_metadata    <- target_metadata    %>% filter(to %in% as.numeric(input$work_select))
    }
    
    node_of <- tibble(
      id    = "Oceanus Folk",
      label = "Oceanus Folk",
      group = "Genre",
      title = "Genre node for Oceanus Folk\nAggregated influence hub"
    )
    target_graphdata <- target_metadata %>%
      transmute(
        id    = as.character(to),
        label = name_target,
        group = type_target,
        title = paste0("Genre: ", genre_target)
      )
    other_genres <- target_metadata %>%
      filter(!is.na(genre_target), genre_target!="Oceanus Folk") %>%
      distinct(genre_target) %>%
      transmute(
        id    = genre_target,
        label = genre_target,
        group = "Genre",
        title = paste0("Genre node: ", genre_target)
      )
    node_graphdata <- bind_rows(node_of, target_graphdata, other_genres) %>%
      distinct(id, .keep_all=TRUE)
    
    edge_type_colors <- c(
      instyleof="#1f77b4", coverof="#ff7f0e", interpolatesfrom="#2ca02c",
      directlysamples="#d62728", lyricalreferenceto="#9467bd"
    )
    edge_of_to_target <- of_outgoing_edges %>%
      transmute(
        from  = "Oceanus Folk",
        to    = as.character(to),
        title = paste0("Edge Type: ", `Edge Type`),
        color = edge_type_colors[as.character(`Edge Type`)],
        group = as.character(`Edge Type`)
      )
    edge_work_to_genre <- target_metadata %>%
      filter(!is.na(genre_target), genre_target!="Oceanus Folk") %>%
      transmute(
        from  = as.character(to),
        to    = genre_target,
        title = "Belongs to genre",
        color = "#bdbdbd",
        group = "BelongsToGenre"
      )
    edge_graphdata_final <- bind_rows(edge_of_to_target, edge_work_to_genre) %>%
      distinct(from, to, .keep_all=TRUE)
    

    target_meta <- edge_graphdata_final %>% count(to, name="n_incoming")
    node_graphdata <- node_graphdata %>%
      left_join(target_meta, by=c("id"="to")) %>%
      mutate(size = case_when(id=="Oceanus Folk" ~ 60,
                              TRUE ~ rescale(n_incoming, to=c(20,50), na.rm=TRUE))) %>%
      replace_na(list(size=20))
    
    nodes_vis <- node_graphdata %>%
      mutate(
        display_name  = label,
        display_genre = case_when(
          group=="Work"  ~ str_extract(title, "(?<=Genre: ).*"),
          group=="Genre" ~ "Genre node",
          TRUE           ~ NA_character_
        ),
        label = display_name,
        title = paste0(
          "<b>", display_name, "</b><br>",
          "Type: ", group, "<br>",
          ifelse(!is.na(display_genre), paste0("Genre: ", display_genre, "<br>"), "")
        ),
        color = case_when(
          group=="Genre"  ~ "skyblue",
          group=="Work"   ~ "lightgreen",
          group=="Person" ~ "yellow",
          TRUE            ~ "gray"
        ),
        font_size = ifelse(group=="Genre", 20, 0)
      ) %>%
      mutate(font = purrr::map(font_size, ~ list(size=.x)))
    
    visNetwork(nodes=nodes_vis, edges=edge_graphdata_final, width="100%", height="700px") %>%
      visNodes(borderWidth=2, borderWidthSelected=4, shadow=TRUE,
               color=list(border="black", highlight=list(border="black", background="orange"))) %>%
      visEdges(arrows="to", smooth=FALSE) %>%
      visOptions(highlightNearest=TRUE, nodesIdSelection=list(enabled=TRUE, useLabels=TRUE)) %>%
      visInteraction(zoomView=TRUE) %>%
      visLayout(randomSeed=1234) %>%
      visGroups(groupname="Genre",  color="skyblue") %>%
      visGroups(groupname="Work",   color="lightgreen") %>%
      visGroups(groupname="Person", color="yellow") %>%
      visLegend(useGroups=TRUE,
                addEdges=data.frame(label=c("InStyleOf","CoverOf","InterpolatesFrom",
                                            "DirectlySamples","LyricalReferenceTo","Belongs to genre"),
                                    color=c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#bdbdbd"),
                                    arrows=rep("to",6)),
                position="right",
                main="Node/Edge Type & Size Meaning") %>%
      visPhysics(solver="forceAtlas2Based",
                 forceAtlas2Based=list(gravitationalConstant=-50),
                 stabilization=list(enabled=TRUE, iterations=100))
  })
  
}


shinyApp(ui, server)
