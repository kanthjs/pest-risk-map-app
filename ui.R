ui <- fluidPage(
    tags$head(
        # Import Google Font 'Mitr'
        tags$link(
            rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=Mitr&display=swap"
        ),
        tags$style(HTML(
            "
      body { font-family: 'Mitr', sans-serif; }
      h2, h4 { font-family: 'Mitr', sans-serif; }
      .well { background-color: #f8f9fa; }
      .stats-box {
        background: white;
        padding: 15px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 15px;
      }
      .high-risk { color: #d73027; font-weight: bold; }
      .medium-risk { color: #fdae61; font-weight: bold; }
      .low-risk { color: #a6d96a; font-weight: bold; }
      .no-risk { color: #1a9641; font-weight: bold; }
    "
        ))
    ),

    titlePanel("ระบบแสดงดัชนีความเสี่ยงศัตรูข้าว (Pest Risk Index)"),

    sidebarLayout(
        sidebarPanel(
            h4("ตัวเลือกการแสดงผล"),

            selectInput(
                "pest",
                "เลือกศัตรูข้าว:",
                choices = NULL
            ),

            selectInput(
                "month",
                "เลือกเดือน:",
                choices = NULL
            ),

            hr(),

            h4("สถิติภาพรวม"),
            div(class = "stats-box", uiOutput("stats_summary")),

            br(),

            helpText(
                tags$ul(
                    tags$li("สีแดง (4) = ความเสี่ยงสูงมาก (60-100%)"),
                    tags$li("สีส้ม (3) = ความเสี่ยงสูง (30-60%)"),
                    tags$li("สีเหลืองส้ม (2) = ความเสี่ยงปานกลาง (10-30%)"),
                    tags$li("สีเขียวอ่อน (1) = ความเสี่ยงต่ำ (1-10%)"),
                    tags$li("สีเขียวเข้ม (0) = ไม่มีความเสี่ยง (0%)")
                )
            )
        ),

        mainPanel(
            tabsetPanel(
                tabPanel(
                    "แผนที่",
                    leafletOutput("map", height = 600),
                ),

                tabPanel(
                    "จังหวัดเสี่ยงสูง",
                    br(),
                    plotlyOutput("province_ranking")
                )
            )
        )
    )
)
