ui <- fluidPage(
  titlePanel("🌊 Oceanus Folk Influence Explorer"),
  tabsetPanel(
    
    # Tab 1: Influence Timeline
    tabPanel("Influence Timeline",
             sidebarLayout(
               sidebarPanel(
                 # 你可以添加年份选择器、滑块或其他控件
                 sliderInput("year_range", "Select Year Range:",
                             min = 1990, max = 2030,
                             value = c(2000, 2025), step = 1, sep = "")
               ),
               mainPanel(
                 plotOutput("timeline_plot", height = "700px")
               )
             )
    ),
    
    # Tab 2: Genre Impact
    tabPanel("Genre Impact",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("influence_types_genre", "Influence Types:",
                                    choices = c("InStyleOf", "CoverOf", "InterpolatesFrom", "DirectlySamples", "LyricalReferenceTo"),
                                    selected = c("InStyleOf", "CoverOf", "InterpolatesFrom"))
               ),
               mainPanel(
                 plotOutput("genre_plot", height = "700px")
               )
             )
    ),
    
    # Tab 3: Top Influenced Artists
    tabPanel("Top Influenced Artists",
             sidebarLayout(
               sidebarPanel(
                 numericInput("top_n", "Number of Top Influenced Artists:", value = 10, min = 1, max = 50)
               ),
               mainPanel(
                 plotOutput("artist_plot", height = "700px")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Tab 1: Timeline Plot
  output$timeline_plot <- renderPlot({
    # 你可用 Oceanus Folk 的歌曲/专辑 + 年份分布来画趋势线图或条形图
    plot(1, 1, main = "Timeline Plot Placeholder")  # 用你的 ggraph/ggplot 替换
  })
  
  # Tab 2: Genre Plot
  output$genre_plot <- renderPlot({
    # 按 genre 统计被 Oceanus Folk 影响的数量（分影响类型）
    plot(1, 1, main = "Genre Impact Placeholder")
  })
  
  # Tab 3: Artist Plot
  output$artist_plot <- renderPlot({
    # 找出最常被 Oceanus Folk 影响的 artist，并展示条形图或子图网络
    plot(1, 1, main = "Top Influenced Artists Placeholder")
  })
}

shinyApp(ui, server)
