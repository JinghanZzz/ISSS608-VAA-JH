library(shiny)
library(tidygraph)
library(ggraph)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Load and prepare data
kg <- fromJSON("data/MC1_graph.json")
nodes_tbl <- kg$nodes
edges_tbl <- kg$links

# Map source/target to tidygraph row indices
id_map <- tibble(id = nodes_tbl$id, index = seq_len(nrow(nodes_tbl)))

edges_tbl <- edges_tbl %>%
  left_join(id_map, by = c("source" = "id")) %>% rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>% rename(to = index) %>%
  filter(!is.na(from), !is.na(to))

graph <- tbl_graph(nodes = nodes_tbl, edges = edges_tbl, directed = kg$directed)

# Define top 3 artists
top3_names <- c("Kimberly Snyder", "Ping Tian", "Yang Zhao")


# ==== UI ====
ui <- fluidPage(
  titlePanel("🎵 Artist Network Explorer"),
  tabsetPanel(
    tabPanel("Contribution Network",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_artists", "Select Artists:",
                             choices = graph %>% activate(nodes) %>%
                               as_tibble() %>% filter(`Node Type` == "Person") %>% pull(name),
                             selected = top3_names,
                             multiple = TRUE),
                 checkboxGroupInput("edge_types", "Select Contribution Types:",
                                    choices = c("ComposerOf", "ProducerOf", "LyricistOf"),
                                    selected = c("ComposerOf", "ProducerOf", "LyricistOf"))
               ),
               mainPanel(plotOutput("network_plot", height = "800px"))
             )
    ),
    tabPanel("Influence Network",
             sidebarLayout(
               sidebarPanel(
                 selectInput("artist_name", "Choose an Artist:",
                             choices = c(top3_names,
                                         setdiff(
                                           graph %>% activate(nodes) %>%
                                             as_tibble() %>% filter(`Node Type` == "Person") %>% pull(name),
                                           top3_names
                                         )),
                             selected = top3_names[1]  # Preselect one of the three
                 ),
                 checkboxGroupInput("influence_types", "Select Influence Types:",
                                    choices = c("DirectlySamples", "LyricalReferenceTo", "CoverOf", "InterpolatesFrom", "InStyleOf"),
                                    selected = c("DirectlySamples", "LyricalReferenceTo", "CoverOf", "InterpolatesFrom", "InStyleOf"))
               ),
               mainPanel(plotOutput("artist_plot", height = "800px"))
             )
    ),
    tabPanel("Next Rising Stars",
             sidebarLayout(
               sidebarPanel(
                 selectInput("rising_stars", "Select Rising Stars:",
                             choices = {
                               all_names <- nodes_tbl %>%
                                 filter(`Node Type` == "Person") %>%
                                 pull(name)
                               pinned <- c("Copper Canyon Ghosts", "Beatrice Albright", "Daniel O'Connell")
                               c(pinned, setdiff(sort(all_names), pinned))
                             },
                             selected = c("Copper Canyon Ghosts", "Beatrice Albright", "Daniel O'Connell"),
                             multiple = TRUE
                 )
               ),
               mainPanel(
                 plotOutput("rising_star_plot", height = "800px")
               )
             )
    )
  ) 
)   

# ==== SERVER ====
server <- function(input, output, session) {
  
  # --- Contribution Network ---
  output$network_plot <- renderPlot({
    req(input$selected_artists, input$edge_types)
    
    artist_ids <- graph %>%
      activate(nodes) %>% as_tibble() %>%
      filter(name %in% input$selected_artists) %>%
      pull(id)
    
    graph_selected_edges <- graph %>%
      activate(edges) %>%
      filter(`Edge Type` %in% input$edge_types &
               (source %in% artist_ids | target %in% artist_ids))
    
    used_node_indices <- graph_selected_edges %>%
      activate(edges) %>% as_tibble() %>%
      select(from, to) %>% unlist() %>% unique()
    
    graph_selected <- graph_selected_edges %>%
      activate(nodes) %>% mutate(row_id = row_number()) %>%
      filter(row_id %in% used_node_indices) %>% select(-row_id) %>%
      activate(nodes) %>%
      mutate(
        degree = centrality_degree(),
        notable_flag = ifelse(!is.na(notoriety_date), "Notable", "Other"),
        release_year = as.integer(release_date),
        label_with_year = case_when(
          `Node Type` %in% c("Song", "Album") & !is.na(release_year) ~ paste0(name, "\n(", release_year, ")"),
          TRUE ~ name
        ),
        size = case_when(
          `Node Type` == "Person" ~ degree * 1.5,
          !is.na(notoriety_date) ~ 6,
          TRUE ~ 2
        )
      )
    
    ggraph(graph_selected, layout = "fr") +
      geom_edge_link(aes(color = `Edge Type`), alpha = 0.6) +
      geom_node_point(aes(color = `Node Type`, size = size, shape = notable_flag)) +
      geom_node_text(aes(label = label_with_year), repel = TRUE, size = 3) +
      scale_edge_color_manual(values = c(
        ComposerOf = "#1f77b4",
        ProducerOf = "#2ca02c",
        LyricistOf = "#9467bd"
      )) +
      scale_color_manual(values = c(
        Person = "steelblue",
        Song = "gray40",
        Album = "tomato"
      )) +
      scale_shape_manual(values = c("Notable" = 17, "Other" = 16)) +
      theme_minimal(base_size = 14) +
      labs(
        title = "Artist Contribution Network",
        subtitle = paste("Artists:", paste(input$selected_artists, collapse = ", ")),
        caption = "▲ = notable work · Node size = influence degree"
      )
  })
  
  # --- Influence Network ---
  output$artist_plot <- renderPlot({
    req(input$artist_name)
    
    artist_id <- graph %>%
      activate(nodes) %>% as_tibble() %>%
      filter(name == input$artist_name) %>% pull(id)
    
    performed_work_ids <- graph %>%
      activate(edges) %>%
      filter(`Edge Type` == "PerformerOf", source == artist_id) %>%
      as_tibble() %>% pull(target)
    
    influence_types <- input$influence_types
    req(influence_types)
    
    graph_influence_edges <- graph %>%
      activate(edges) %>%
      filter(`Edge Type` %in% influence_types,
             source %in% performed_work_ids | target %in% performed_work_ids)
    
    used_node_ids <- graph_influence_edges %>%
      activate(edges) %>% as_tibble() %>%
      select(from, to) %>% unlist() %>% unique()
    
    graph_artist_sub <- graph_influence_edges %>%
      activate(nodes) %>% mutate(row_id = row_number()) %>%
      filter(row_id %in% used_node_ids) %>% select(-row_id) %>%
      activate(nodes) %>%
      mutate(
        degree = centrality_degree(),
        notable_flag = ifelse(!is.na(notoriety_date), "Notable", "Other"),
        release_year = as.integer(release_date),
        label_with_year = case_when(
          `Node Type` %in% c("Song", "Album") & !is.na(release_year) ~ paste0(name, "\n(", release_year, ")"),
          TRUE ~ name
        ),
        size = case_when(
          `Node Type` == "Person" ~ degree * 1.5,
          !is.na(notoriety_date) ~ 6,
          TRUE ~ 2
        )
      )
    
    ggraph(graph_artist_sub, layout = "fr") +
      geom_edge_link(aes(color = `Edge Type`), alpha = 0.6) +
      geom_node_point(aes(color = `Node Type`, size = size, shape = notable_flag)) +
      geom_node_text(aes(label = label_with_year), repel = TRUE, size = 3) +
      scale_edge_color_manual(values = c(
        DirectlySamples = "#1f77b4",
        LyricalReferenceTo = "#9467bd",
        CoverOf = "#ff7f0e",
        InterpolatesFrom = "#2ca02c",
        InStyleOf = "#8c564b"
      )) +
      scale_color_manual(values = c(
        Person = "steelblue",
        Song = "gray40",
        Album = "tomato"
      )) +
      scale_shape_manual(values = c("Notable" = 17, "Other" = 16)) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Influence Network of", input$artist_name),
        subtitle = "Covers, interpolations, stylistic callbacks, and lyrical references",
        caption = "▲ = notable work · Node size = influence degree"
      )
  })
  
  # --- Rising Stars ---
  output$rising_star_plot <- renderPlot({
    req(input$rising_stars)
    top3_names <- input$rising_stars
    
    # Step 2: Get performer edges for top 3
    artist_song_edges <- edges_tbl %>%
      filter(`Edge Type` == "PerformerOf") %>%
      semi_join(nodes_tbl %>% filter(name %in% top3_names), by = c("source" = "id")) %>%
      select(artist_id = source, song_id = target)
    
    # Step 3: Extract nodes
    artist_nodes <- nodes_tbl %>%
      filter(name %in% top3_names) %>%
      mutate(label = name, type = "Artist")
    
    song_nodes <- nodes_tbl %>%
      filter(id %in% artist_song_edges$song_id) %>%
      mutate(label = name, type = "Song")
    
    # Step 4: Combine and re-index nodes
    all_nodes <- bind_rows(artist_nodes, song_nodes) %>%
      mutate(graph_id = row_number())
    
    # Step 5: Map original IDs to graph IDs
    node_map <- all_nodes %>% select(id, graph_id)
    
    edges_mapped <- artist_song_edges %>%
      left_join(node_map, by = c("artist_id" = "id")) %>%
      rename(from_id = graph_id) %>%
      left_join(node_map, by = c("song_id" = "id")) %>%
      rename(to_id = graph_id) %>%
      select(from = from_id, to = to_id)
    
    # Step 6: Create graph and plot
    song_graph <- tbl_graph(nodes = all_nodes, edges = edges_mapped, directed = FALSE)
    
    ggraph(song_graph, layout = "fr") +
      geom_edge_link(color = "gray80", alpha = 0.6) +
      geom_node_point(aes(color = type, shape = type, size = type)) +
      geom_node_text(aes(label = label), repel = TRUE, size = 3.2) +
      scale_color_manual(values = c("Artist" = "#D55E00", "Song" = "steelblue")) +
      scale_shape_manual(values = c("Artist" = 17, "Song" = 16)) +
      scale_size_manual(values = c("Artist" = 6, "Song" = 3)) +
      labs(
        title = "Top 3 Rising Stars and Their Performed Works",
        subtitle = "Network visualization of artist-song relationships",
        caption = "Source: Oceanus Folk Archive | Chart by FU YILIN"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        legend.position = "bottom"
      )
  })
}
# ==== Run App ====
shinyApp(ui = ui, server = server)