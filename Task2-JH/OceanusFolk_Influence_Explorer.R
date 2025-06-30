# 🌊 Oceanus Folk Influence Explorer
# Shiny App

library(shiny)
library(tidyverse)
library(jsonlite)
library(tidygraph)
library(ggraph)
library(plotly)
library(DT)
library(scales)
library(forcats)
library(visNetwork)
library(SmartEDA)
library(patchwork)

# ---- Data Preparation ----
# 1. 读取数据
kg <- fromJSON("data/MC1_graph.json")

# 2. year-like字段转为整数
gk_nodes <- as_tibble(kg$nodes) %>%
  mutate(
    release_date   = as.integer(release_date),
    written_date   = as.integer(written_date),
    notoriety_date = as.integer(notoriety_date)
  )

# 3. 提取nodes和edges
gk_edges <- as_tibble(kg$links)

# 4. id映射（为from/to做准备）
id_map <- tibble(id = gk_nodes$id, index = seq_len(nrow(gk_nodes)))

# 5. 将source/target映射为from/to
edges_tbl <- gk_edges %>%
  left_join(id_map, by = c("source" = "id")) %>%
  rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>%
  rename(to = index)

# 6. 过滤无效边
edges_tbl <- edges_tbl %>% filter(!is.na(from), !is.na(to))

# 7. nodes_tbl
nodes_tbl <- gk_nodes

# 8. Edge Type字段全小写
edges_tbl <- edges_tbl %>% mutate(`Edge Type` = tolower(`Edge Type`))

# 9. influence_types全小写
influence_types <- c("instyleof", "coverof", "interpolatesfrom", "directlysamples", "lyricalreferenceto")

# All genres
all_genres <- nodes_tbl %>% filter(`Node Type` %in% c("Song", "Album")) %>% distinct(genre) %>% arrange(genre) %>% pull(genre)

# All years
all_years <- nodes_tbl %>% filter(!is.na(release_date)) %>% pull(release_date)
min_year <- min(all_years, na.rm=TRUE)
max_year <- max(all_years, na.rm=TRUE)

# ---- UI ----
ui <- fluidPage(
  titlePanel("\U1F30A Oceanus Folk Influence Explorer"),
  tabsetPanel(
    tabPanel("Influence Timeline",
      sidebarLayout(
        sidebarPanel(
          sliderInput("year_range", "Select Year Range:",
                      min = min_year, max = max_year,
                      value = c(min_year, max_year), sep = ""),
          numericInput("bin_size", "Heatmap Bin Size (years):", value = 5, min = 1, max = 20)
        ),
        mainPanel(
          plotlyOutput("timeline_plot")
        )
      )
    ),
    tabPanel("Genre Impact",
      sidebarLayout(
        sidebarPanel(
          selectInput("genre_select", "Select Genre(s):", choices = all_genres, selected = all_genres, multiple = TRUE),
          selectInput("work_select", "Select Work(s):", choices = NULL, selected = NULL, multiple = TRUE),
          selectInput("influence_type", "Influence Type:", choices = influence_types, selected = influence_types, multiple = TRUE)
        ),
        mainPanel(
          visNetworkOutput("genre_impact_network", height = "700px")
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # --- Influence Timeline ---
  output$timeline_plot <- renderPlotly({
    df <- nodes_tbl %>%
      filter(genre == "Oceanus Folk", `Node Type` %in% c("Song", "Album"),
             !is.na(release_date),
             between(release_date, input$year_range[1], input$year_range[2])) %>%
      count(release_date, `Node Type`, name = "count") %>%
      pivot_wider(names_from = `Node Type`, values_from = count, values_fill = 0) %>%
      mutate(total = Song + Album,
             year_bin = floor(release_date / input$bin_size) * input$bin_size)

    heatmap_df <- df %>%
      group_by(year_bin) %>%
      summarise(bin_total = sum(total), Album = sum(Album), Song = sum(Song), .groups = "drop") %>%
      arrange(year_bin) %>%
      mutate(change = bin_total - lag(bin_total),
             hover_text = paste0("Period: ", year_bin, "–", year_bin + input$bin_size - 1,
                                 "<br>Total: ", bin_total,
                                 "<br>Change: ", ifelse(is.na(change), "NA", ifelse(change > 0, paste0("+", change), change))))

    p <- ggplot() +
      geom_tile(data = heatmap_df,
                aes(x = year_bin + input$bin_size/2, y = -5, fill = bin_total, text = hover_text),
                width = input$bin_size, height = 3) +
      geom_line(data = df, aes(x = release_date, y = total, group = 1), color = "#2f4b7c", linewidth = 1) +
      geom_point(data = df, aes(x = release_date, y = total, text = paste0("Year: ", release_date, "<br>Total: ", total, "<br>Album: ", Album, "<br>Song: ", Song)), color = "#2f4b7c", size = 2) +
      scale_fill_gradient(low = "#a8dadc", high = "#2f4b7c") +
      labs(title = "Oceanus Folk Releases by Year with Heatmap", x = "Release Year", y = "Total Count", fill = paste(input$bin_size, "Year Total")) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  # --- Panel2: 动态work_select下拉 ---
  observe({
    edge_graphdata <- edges_tbl %>%
      left_join(nodes_tbl %>% select(id, genre_from = genre), by = c("from" = "id")) %>%
      left_join(nodes_tbl %>% select(id, type_from = `Node Type`), by = c("from" = "id")) %>%
      left_join(nodes_tbl %>% select(id, type_to = `Node Type`), by = c("to" = "id"))
    of_publications <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album"), genre == "Oceanus Folk") %>%
      pull(id)
    of_outgoing_edges <- edge_graphdata %>%
      filter(from %in% of_publications, `Edge Type` %in% input$influence_type)
    target_metadata <- of_outgoing_edges %>%
      left_join(nodes_tbl %>%
                  select(id, name_target = name, genre_target = genre, type_target = `Node Type`),
                by = c("to" = "id")) %>%
      filter(genre_target %in% input$genre_select) %>%
      distinct(to, name_target, genre_target, type_target)
    work_choices <- setNames(as.character(target_metadata$to), target_metadata$name_target)
    updateSelectInput(session, "work_select", choices = work_choices, selected = work_choices)
  })

  # --- Panel2: network点击节点联动select ---
  observeEvent(input$genre_impact_network_selected, {
    sel <- input$genre_impact_network_selected
    if (is.null(sel)) return()
    # 获取当前所有节点信息
    edge_graphdata <- edges_tbl %>%
      left_join(nodes_tbl %>% select(id, genre_from = genre), by = c("from" = "id")) %>%
      left_join(nodes_tbl %>% select(id, type_from = `Node Type`), by = c("from" = "id")) %>%
      left_join(nodes_tbl %>% select(id, type_to = `Node Type`), by = c("to" = "id"))
    of_publications <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album"), genre == "Oceanus Folk") %>%
      pull(id)
    of_outgoing_edges <- edge_graphdata %>%
      filter(from %in% of_publications, `Edge Type` %in% input$influence_type)
    target_metadata <- of_outgoing_edges %>%
      left_join(nodes_tbl %>%
                  select(id, name_target = name, genre_target = genre, type_target = `Node Type`),
                by = c("to" = "id")) %>%
      filter(genre_target %in% input$genre_select) %>%
      distinct(to, name_target, genre_target, type_target)
    # 判断点击的是genre还是work
    if (sel %in% input$genre_select) {
      updateSelectInput(session, "genre_select", selected = sel)
    } else if (sel %in% as.character(target_metadata$to)) {
      updateSelectInput(session, "work_select", selected = sel)
    }
  })

  # --- Genre Impact (Panel 2: 2b.1 Transform Genre to Virtual Node) ---
  output$genre_impact_network <- renderVisNetwork({
    library(purrr)
    library(stringr)
    edge_graphdata <- edges_tbl %>%
      left_join(nodes_tbl %>% select(id, genre_from = genre), by = c("from" = "id")) %>%
      left_join(nodes_tbl %>% select(id, type_from = `Node Type`), by = c("from" = "id")) %>%
      left_join(nodes_tbl %>% select(id, type_to = `Node Type`), by = c("to" = "id"))

    of_publications <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album"), genre == "Oceanus Folk") %>%
      pull(id)

    of_outgoing_edges <- edge_graphdata %>%
      filter(from %in% of_publications, `Edge Type` %in% input$influence_type)

    target_metadata <- of_outgoing_edges %>%
      left_join(nodes_tbl %>%
                  select(id, name_target = name, genre_target = genre, type_target = `Node Type`),
                by = c("to" = "id")) %>%
      filter(genre_target %in% input$genre_select) %>%
      distinct(to, name_target, genre_target, type_target)

    # work_select筛选
    selected_works <- input$work_select
    if (!is.null(selected_works)) {
      target_metadata <- target_metadata %>% filter(as.character(to) %in% selected_works)
      of_outgoing_edges <- of_outgoing_edges %>% filter(to %in% as.numeric(selected_works))
    }

    node_of <- tibble(
      id    = "Oceanus Folk",
      label = "Oceanus Folk",
      group = "Genre",
      title = "Genre node for Oceanus Folk<br>Aggregated influence hub"
    )

    target_graphdata <- target_metadata %>%
      transmute(
        id    = as.character(to),
        label = name_target,
        group = type_target,
        title = paste0("Genre: ", genre_target)
      )

    edge_of_to_target <- of_outgoing_edges %>%
      filter(to %in% target_metadata$to) %>%
      transmute(
        from  = "Oceanus Folk",
        to    = as.character(to),
        title = paste0("Edge Type: ", `Edge Type`)
      )

    other_genres <- target_metadata %>%
      filter(!is.na(genre_target), genre_target != "Oceanus Folk") %>%
      distinct(genre_target) %>%
      transmute(
        id = genre_target,
        label = genre_target,
        group = "Genre",
        title = paste0("Genre node: ", genre_target)
      )

    edge_work_to_genre <- target_metadata %>%
      filter(!is.na(genre_target), genre_target != "Oceanus Folk") %>%
      transmute(
        from = as.character(to),
        to   = genre_target,
        title = "Belongs to genre"
      )

    node_graphdata <- bind_rows(node_of, target_graphdata, other_genres) %>%
      distinct(id, .keep_all = TRUE)

    edge_graphdata_final <- bind_rows(edge_of_to_target, edge_work_to_genre) %>%
      distinct(from, to, .keep_all = TRUE)

    target_meta <- edge_graphdata_final %>%
      count(to, name = "n_incoming")
    node_graphdata <- node_graphdata %>%
      left_join(target_meta, by = c("id" = "to")) %>%
      mutate(size = case_when(
        id == "Oceanus Folk" ~ 60,
        TRUE ~ rescale(n_incoming, to = c(20, 50), na.rm = TRUE))
      ) %>%
      replace_na(list(size = 20))

    nodes_vis <- node_graphdata %>%
      mutate(
        group = case_when(
          group %in% c("Song", "Album") ~ "Work",
          TRUE ~ group
        ),
        display_name = label,
        display_genre = case_when(
          group == "Work" ~ str_extract(title, "(?<=Genre: ).*"),
          group == "Genre" ~ "Genre node",
          TRUE ~ NA_character_
        ),
        label = display_name,
        title = paste0(
          "<b>", display_name, "</b><br>",
          "Type: ", group, "<br>",
          ifelse(!is.na(display_genre), paste0("Genre: ", display_genre, "<br>"), "")
        ),
        color = case_when(
          group == "Genre"  ~ "skyblue",
          group == "Work"   ~ "lightgreen",
          group == "Person" ~ "yellow",
          TRUE              ~ "gray"
        ),
        font_size = ifelse(group == "Genre", 20, 0)
      ) %>%
      mutate(font = purrr::map(font_size, ~ list(size = .x)))

    edges_vis <- edge_graphdata_final

    visNetwork(nodes = nodes_vis, edges = edges_vis, width = "100%", height = "700px") %>%
      visNodes(
        borderWidth = 2,
        borderWidthSelected = 4,
        shadow = TRUE,
        color = list(
          border = "black",
          highlight = list(border = "black", background = "orange")
        )
      ) %>%
      visEdges(arrows = "to", smooth = FALSE) %>%
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
      ) %>%
      visInteraction(zoomView = TRUE) %>%
      visLayout(randomSeed = 1234) %>%
      visGroups(groupname = "Genre",  color = "skyblue") %>%
      visGroups(groupname = "Work",   color = "lightgreen") %>%
      visGroups(groupname = "Person", color = "yellow") %>%
      visLegend(useGroups = TRUE, position = "right", main = "Node Type") %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -50),
        stabilization = list(enabled = TRUE, iterations = 100)
      )
  })

  # --- Top Artists Influenced Network (Panel 3) ---
  output$top_artist_network <- renderVisNetwork({
    # Step 0: 筛选有contribute to notable works的top artist
    notable_works <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album"), notable == TRUE) %>%
      pull(id)
    contrib_types <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
    direct_contributors <- edges_tbl %>%
      filter(`Edge Type` %in% contrib_types, to %in% notable_works) %>%
      pull(from)
    group_ids <- nodes_tbl %>%
      filter(`Node Type` == "Music Group") %>%
      pull(id)
    group_contributors <- edges_tbl %>%
      filter(`Edge Type` %in% contrib_types, from %in% group_ids, to %in% notable_works) %>%
      pull(from)
    group_members <- edges_tbl %>%
      filter(`Edge Type` == "MemberOf", to %in% group_contributors) %>%
      pull(from)
    all_top_artist_ids <- unique(c(direct_contributors, group_members))
    person_ids <- nodes_tbl %>%
      filter(`Node Type` == "Person") %>%
      pull(id)
    top_artist_ids <- intersect(all_top_artist_ids, person_ids)

    # Step 1: enrich edge metadata
    edge_graphdata <- edges_tbl %>%
      left_join(nodes_tbl %>% select(id, genre_from = genre), by = c("from" = "id")) %>%
      left_join(nodes_tbl %>% select(id, type_from = `Node Type`), by = c("from" = "id")) %>%
      left_join(nodes_tbl %>% select(id, type_to = `Node Type`), by = c("to" = "id"))

    # Step 2: get all Oceanus Folk publications (Song or Album)
    of_publications <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album"), genre == "Oceanus Folk") %>%
      pull(id)

    # Step 3: get edges representing influence from OF works to others
    influence_edge_types <- c("InStyleOf", "CoverOf", "InterpolatesFrom",
                              "DirectlySamples", "LyricalReferenceTo")
    of_outgoing_edges <- edge_graphdata %>%
      filter(from %in% of_publications, `Edge Type` %in% influence_edge_types)

    # Step 4: get metadata for influenced works
    influenced_work_metadata <- of_outgoing_edges %>%
      left_join(nodes_tbl %>%
                  select(id, name_work = name, type_work = `Node Type`),
                by = c("to" = "id")) %>%
      distinct(to, name_work, type_work)

    # Step 5: get people who are linked to这些works
    person_link_types <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
    work_to_person_edges <- edge_graphdata %>%
      filter(to %in% influenced_work_metadata$to, `Edge Type` %in% person_link_types) %>%
      left_join(nodes_tbl %>% filter(`Node Type` == "Person") %>% select(id, name_person = name), by = c("from" = "id")) %>%
      transmute(
        from = as.character(to),        # from: song
        to   = as.character(from),      # to: person
        title = paste0("Role: ", `Edge Type`),
        name_person
      ) %>%
      filter(!is.na(name_person), to %in% top_artist_ids) %>%  # 只保留top artist
      distinct(from, to, title, name_person)

    # Step 6: define nodes
    node_of <- tibble(
      id = "Oceanus Folk",
      label = "Oceanus Folk",
      group = "Genre",
      title = "Genre node: Oceanus Folk"
    )
    node_work <- nodes_tbl %>%
      filter(id %in% influenced_work_metadata$to) %>%
      transmute(
        id = as.character(id),
        label = name,
        group = `Node Type`,  # original type: Song or Album
        title = paste0("Influenced Work: ", name)
      )
    node_person <- work_to_person_edges %>%
      distinct(to, name_person) %>%
      transmute(
        id = to,
        label = name_person,
        group = "Person",
        title = paste0("Artist: ", name_person)
      )
    # Step 7: define edges
    edge_of_to_work <- of_outgoing_edges %>%
      transmute(
        from = "Oceanus Folk",
        to   = as.character(to),
        title = paste0("Influence: ", `Edge Type`)
      )
    edge_work_to_person <- work_to_person_edges %>%
      select(from, to, title)
    # Step 8: assemble full node table with only connected nodes
    node_graphdata_full <- bind_rows(node_of, node_work, node_person) %>%
      distinct(id, .keep_all = TRUE)
    used_node_ids <- unique(c(edge_of_to_work$from, edge_of_to_work$to,
                              edge_work_to_person$from, edge_work_to_person$to))
    node_graphdata <- node_graphdata_full %>%
      filter(id %in% used_node_ids) %>%
      distinct(id, .keep_all = TRUE)
    # Step 9: compute in-degree (number of incoming edges)
    edge_graphdata_final <- bind_rows(edge_of_to_work, edge_work_to_person) %>%
      distinct(from, to, .keep_all = TRUE)
    target_meta <- edge_graphdata_final %>%
      count(to, name = "n_incoming")
    node_graphdata <- node_graphdata %>%
      mutate(id = as.character(id)) %>%
      left_join(target_meta, by = c("id" = "to")) %>%
      mutate(size = case_when(
        id == "Oceanus Folk" ~ 60,
        TRUE ~ rescale(n_incoming, to = c(20, 50), na.rm = TRUE))
      ) %>%
      replace_na(list(size = 20))
    nodes_vis <- node_graphdata %>%
      mutate(
        group = case_when(
          group %in% c("Song", "Album") ~ "Work",
          TRUE ~ group
        ),
        font_size = ifelse(group == "Genre", 20, 15),
        color = case_when(
          group == "Genre"  ~ "skyblue",
          group == "Work"   ~ "lightgreen",
          group == "Person" ~ "yellow",
          TRUE              ~ "gray"
        )
      ) %>%
      mutate(font = purrr::map(font_size, ~ list(size = .x)))
    edges_vis <- edge_graphdata_final
    visNetwork(nodes = nodes_vis, edges = edges_vis, width = "100%", height = "700px") %>%
      visNodes(
        borderWidth = 2,
        borderWidthSelected = 4,
        shadow = TRUE,
        color = list(
          border = "black",
          highlight = list(border = "black", background = "orange")
        )
      ) %>%
      visEdges(arrows = "to", smooth = FALSE) %>%
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
      ) %>%
      visInteraction(zoomView = TRUE, dragNodes = TRUE) %>%
      visLayout(randomSeed = 2025) %>%
      visGroups(groupname = "Genre",  color = "skyblue") %>%
      visGroups(groupname = "Work",   color = "lightgreen") %>%
      visGroups(groupname = "Person", color = "yellow") %>%
      visLegend(
        useGroups = TRUE,
        position = "right",
        main = "Node Type & Size Meaning"
      ) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -50),
        stabilization = list(enabled = TRUE, iterations = 100)
      )
  })

  # --- Top Artists Influenced (Panel 3: only top artists with notable works) ---
  output$top_artist_bar <- renderPlotly({
    # 1. 找出所有notable works
    notable_works <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album"), notable == TRUE) %>%
      pull(id)
    # 2. 找出直接contribute的artist
    contrib_types <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
    direct_contributors <- edges_tbl %>%
      filter(`Edge Type` %in% contrib_types, to %in% notable_works) %>%
      pull(from)
    # 3. group成员
    group_ids <- nodes_tbl %>%
      filter(`Node Type` == "Music Group") %>%
      pull(id)
    group_contributors <- edges_tbl %>%
      filter(`Edge Type` %in% contrib_types, from %in% group_ids, to %in% notable_works) %>%
      pull(from)
    group_members <- edges_tbl %>%
      filter(`Edge Type` == "MemberOf", to %in% group_contributors) %>%
      pull(from)
    all_top_artist_ids <- unique(c(direct_contributors, group_members))
    person_ids <- nodes_tbl %>%
      filter(`Node Type` == "Person") %>%
      pull(id)
    top_artist_ids <- intersect(all_top_artist_ids, person_ids)

    # 受影响作品
    of_publications <- nodes_tbl %>% filter(`Node Type` %in% c("Song", "Album"), genre == "Oceanus Folk") %>% pull(id)
    of_outgoing_edges <- edges_tbl %>% filter(from %in% of_publications, `Edge Type` %in% influence_types)
    influenced_work_metadata <- of_outgoing_edges %>%
      left_join(nodes_tbl %>% select(id, name, `Node Type`), by = c("to" = "id")) %>%
      distinct(to, name, `Node Type`)
    person_link_types <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
    work_to_person_edges <- edges_tbl %>%
      filter(to %in% influenced_work_metadata$to, `Edge Type` %in% person_link_types) %>%
      left_join(nodes_tbl %>% filter(`Node Type` == "Person") %>% select(id, name_person = name), by = c("from" = "id")) %>%
      transmute(
        from = as.character(to),
        to   = as.character(from),
        name_person
      ) %>%
      filter(!is.na(name_person), to %in% top_artist_ids) %>%
      distinct(from, to, name_person)
    person_indegree_tbl <- work_to_person_edges %>%
      count(to, name = "in_degree") %>%
      left_join(nodes_tbl %>% filter(`Node Type` == "Person") %>% select(id, name_person = name), by = c("to" = "id")) %>%
      select(id = to, name_person, in_degree) %>%
      arrange(desc(in_degree)) %>%
      slice_head(n = input$top_n)
    p <- ggplot(person_indegree_tbl, aes(x = fct_reorder(name_person, in_degree), y = in_degree)) +
      geom_col(fill = "#FFA500") +
      coord_flip() +
      labs(title = "Top Influenced Artists", x = "Artist", y = "Number of Influenced Works") +
      theme_minimal()
    ggplotly(p)
  })
  output$top_artist_table <- renderDT({
    # 1. 找出所有notable works
    notable_works <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album"), notable == TRUE) %>%
      pull(id)
    # 2. 找出直接contribute的artist
    contrib_types <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
    direct_contributors <- edges_tbl %>%
      filter(`Edge Type` %in% contrib_types, to %in% notable_works) %>%
      pull(from)
    # 3. group成员
    group_ids <- nodes_tbl %>%
      filter(`Node Type` == "Music Group") %>%
      pull(id)
    group_contributors <- edges_tbl %>%
      filter(`Edge Type` %in% contrib_types, from %in% group_ids, to %in% notable_works) %>%
      pull(from)
    group_members <- edges_tbl %>%
      filter(`Edge Type` == "MemberOf", to %in% group_contributors) %>%
      pull(from)
    all_top_artist_ids <- unique(c(direct_contributors, group_members))
    person_ids <- nodes_tbl %>%
      filter(`Node Type` == "Person") %>%
      pull(id)
    top_artist_ids <- intersect(all_top_artist_ids, person_ids)

    # 受影响作品
    of_publications <- nodes_tbl %>% filter(`Node Type` %in% c("Song", "Album"), genre == "Oceanus Folk") %>% pull(id)
    of_outgoing_edges <- edges_tbl %>% filter(from %in% of_publications, `Edge Type` %in% influence_types)
    influenced_work_metadata <- of_outgoing_edges %>%
      left_join(nodes_tbl %>% select(id, name, `Node Type`), by = c("to" = "id")) %>%
      distinct(to, name, `Node Type`)
    person_link_types <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")
    work_to_person_edges <- edges_tbl %>%
      filter(to %in% influenced_work_metadata$to, `Edge Type` %in% person_link_types) %>%
      left_join(nodes_tbl %>% filter(`Node Type` == "Person") %>% select(id, name_person = name), by = c("from" = "id")) %>%
      transmute(
        from = as.character(to),
        to   = as.character(from),
        name_person
      ) %>%
      filter(!is.na(name_person), to %in% top_artist_ids) %>%
      distinct(from, to, name_person)
    person_indegree_tbl <- work_to_person_edges %>%
      count(to, name = "in_degree") %>%
      left_join(nodes_tbl %>% filter(`Node Type` == "Person") %>% select(id, name_person = name), by = c("to" = "id")) %>%
      select(id = to, name_person, in_degree) %>%
      arrange(desc(in_degree)) %>%
      slice_head(n = input$top_n)
    person_indegree_tbl
  })
}

# ---- Run App ----
shinyApp(ui, server) 