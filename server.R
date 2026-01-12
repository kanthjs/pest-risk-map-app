server <- function(input, output, session) {
    # Load data within the server function to handle errors gracefully
    data <- tryCatch(load_data(), error = function(e) {
        shiny::showNotification(
            paste("เกิดข้อผิดพลาดในการโหลดข้อมูล:", e$message),
            type = "error",
            duration = NULL
        )
        NULL
    })

    # Stop the app if data loading failed, and update inputs if it succeeded
    if (is.null(data)) {
        shiny::showNotification(
            "ไม่สามารถเริ่มต้นแอปพลิเคชันได้เนื่องจากข้อมูลไม่พร้อมใช้งาน",
            type = "error",
            duration = NULL
        )
        return()
    } else {
        updateSelectInput(
            session,
            "pest",
            choices = data$pest_choices,
            selected = data$pest_choices[1]
        )
        updateSelectInput(
            session,
            "month",
            choices = data$month_choices,
            selected = data$month_choices[1]
        )
    }

    # Reactive data selection for the main map
    data_sel <- reactive({
        req(input$pest, input$month)
        data$pest_long |>
            filter(pest == input$pest, month == input$month)
    })

    # Statistics summary
    output$stats_summary <- renderUI({
        df <- data_sel()

        total_provinces <- length(unique(df$province))
        high_risk <- sum(df$risk_index >= 3, na.rm = TRUE)
        medium_risk <- sum(df$risk_index == 2, na.rm = TRUE)
        low_risk <- sum(df$risk_index == 1, na.rm = TRUE)
        no_risk <- sum(df$risk_index == 0, na.rm = TRUE)
        avg_risk <- mean(df$risk_index, na.rm = TRUE)

        HTML(paste0(
            "<b>ศัตรูข้าว:</b> ",
            input$pest,
            "<br>",
            "<b>เดือน:</b> ",
            input$month,
            "<br>",
            "<b>จังหวัดทั้งหมด:</b> ",
            total_provinces,
            "<br>",
            "<span class='high-risk'>เสี่ยงสูง (3-4):</span> ",
            high_risk,
            " จังหวัด<br>",
            "<span class='medium-risk'>เสี่ยงปานกลาง (2):</span> ",
            medium_risk,
            " จังหวัด<br>",
            "<span class='low-risk'>เสี่ยงต่ำ (1):</span> ",
            low_risk,
            " จังหวัด<br>",
            "<span class='no-risk'>ไม่มีความเสี่ยง (0):</span> ",
            no_risk,
            " จังหวัด<br>",
            "<b>ค่าเฉลี่ยความเสี่ยง:</b> ",
            round(avg_risk, 2)
        ))
    })

    # --- Map Rendering ---
    output$map <- renderLeaflet({
        leaflet() |>
            addProviderTiles(
                providers$CartoDB.Positron,
                layerId = "base_tiles"
            ) |>
            setView(lng = 100.5, lat = 13.7, zoom = 6) |>
            setMaxBounds(lng1 = 97, lat1 = 5, lng2 = 106, lat2 = 21)
    })

    # Use a proxy to update the main map without redrawing it
    observe({
        df <- data_sel()
        map_df <- data$thai_sf |>
            left_join(df, by = c("pro_th" = "province"))

        pal <- colorFactor(
            palette = c("#1a9641", "#a6d96a", "#fee08b", "#f46d43", "#d73027"),
            domain = factor(0:4),
            na.color = "#cccccc"
        )

        leafletProxy("map", data = map_df) |>
            clearShapes() |>
            addPolygons(
                fillColor = ~ pal(factor(risk_index)),
                fillOpacity = 0.85,
                color = "#ffffff",
                weight = 1,
                label = ~ paste0(
                    pro_th,
                    " : Risk = ",
                    ifelse(is.na(risk_index), "ไม่มีข้อมูล", risk_index)
                ),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"
                ),
                layerId = ~pro_th, # ทำให้แต่ละจังหวัดสามารถคลิกได้
                highlightOptions = highlightOptions(
                    weight = 3,
                    color = "#2c3e50",
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                )
            ) |>
            clearControls() |>
            addLegend(
                "bottomright",
                pal = pal,
                values = factor(0:4),
                title = paste(
                    "<b>Risk Index</b><br>",
                    input$pest,
                    "<br>",
                    input$month
                ),
                opacity = 1
            )
    })

    # Province ranking
    output$province_ranking <- renderPlotly({
        ranking <- data_sel() |>
            arrange(desc(risk_index)) |>
            head(15) |>
            mutate(province = factor(province, levels = rev(unique(province))))

        p <- ggplot(
            ranking,
            aes(x = risk_index, y = province, fill = factor(risk_index))
        ) +
            geom_col() +
            scale_fill_manual(
                values = c(
                    "0" = "#1a9641",
                    "1" = "#a6d96a",
                    "2" = "#fee08b",
                    "3" = "#f46d43",
                    "4" = "#d73027"
                )
            ) +
            labs(
                title = paste(
                    "15 จังหวัดเสี่ยงสูงสุด:",
                    input$pest,
                    "-",
                    input$month
                ),
                x = "ดัชนีความเสี่ยง",
                y = ""
            ) +
            theme_minimal() +
            theme(legend.position = "none")

        ggplotly(p) |>
            layout(hoverlabel = list(bgcolor = "white"))
    })
}
