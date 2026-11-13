# =============================================================
# Data 1 tab : Industrial accident rate by workplace size & industry
# - data1_ui("d1")      : 화면(UI) 부분
# - data1_server("d1")  : 계산(server) 부분
# 팀 전체 앱(app.R)에서 source("data1_tab.R") 로 불러서 씀
# =============================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# ---- 1. 데이터 불러오기 (앱 시작할 때 한 번만) ----------------------
rate <- read.csv("rate_final.csv", fileEncoding = "UTF-8-BOM")
rate$size_available <- rate$size_available %in% c(TRUE, "True", "TRUE")

size_levels <- c("<5", "5 to 9", "10 to 19", "20 to 29", "30 to 49",
                 "50 to 99", "100 to 299", "300 to 499", "500 to 999", "1,000+")
rate$size_en     <- factor(rate$size_en, levels = c(size_levels, "All sizes (no size breakdown)"))
rate$industry_en <- factor(rate$industry_en, levels = unique(rate$industry_en))

major_choices <- unique(rate$major_industry_en)

# ---- 2. UI ---------------------------------------------------------
data1_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 290,
      sliderInput(ns("years"), "Year", min = 2020, max = 2024,
                  value = c(2020, 2024), step = 1, sep = ""),
      checkboxGroupInput(ns("major"), "Major industry",
                         choices = major_choices, selected = major_choices),
      selectizeInput(ns("industry"), "Industry (leave empty = all)",
                     choices = NULL, multiple = TRUE),
      checkboxGroupInput(ns("size"), "Workplace size (workers)",
                         choices = size_levels, selected = size_levels),
      radioButtons(ns("group"), "Compare by",
                   choices = c("Workplace size" = "size_en",
                               "Industry" = "industry_en",
                               "Year" = "year")),
      checkboxInput(ns("drop_warn"), "Exclude warning cells", value = TRUE),
      checkboxInput(ns("keep_unstable"), "Include unstable cells", value = TRUE)
    ),

    # 질문
    card(
      card_header("Question"),
      p(strong("Does the industrial accident rate differ by workplace size and industry?")),
      p("Rate = injured workers / workers × 1,000 (per 1,000 workers). ",
        "Accidents only (occupational disease excluded), 2020–2024.")
    ),

    # 숫자 요약 (value box 5개)
    layout_columns(
      fill = FALSE,
      value_box("Injured workers", textOutput(ns("vb_cases"))),
      value_box("Workers",         textOutput(ns("vb_workers"))),
      value_box("Accident rate (‰)", textOutput(ns("vb_rate")),
                textOutput(ns("vb_rate_stable"))),
      value_box("Mean of industry rates",   textOutput(ns("vb_mean"))),
      value_box("Median of industry rates", textOutput(ns("vb_median")))
    ),

    # 그래프 + 핵심 결과 문장
    card(
      full_screen = TRUE,
      card_header("Accident rate by group"),
      plotOutput(ns("plot"), height = "420px"),
      uiOutput(ns("finding"))
    ),

    # 표
    card(
      card_header("Summary table"),
      tableOutput(ns("table"))
    ),

    # 왜 중요한가 / 다음 분석 / 주의사항
    card(
      card_header("Why it matters & next step"),
      p(strong("Why it matters: "),
        "Counting injured workers alone makes large industries stand out just because ",
        "they employ more people. The rate shows where risk is actually concentrated, ",
        "which helps decide where prevention support should go first."),
      p(strong("Next analysis: "),
        "For high-rate groups (especially small workplaces), look at which accident types ",
        "occur most and at workers' age and length of employment."),
      tags$small(
        tags$ul(
          tags$li("Construction has yearly totals only (no size breakdown), so it is dropped ",
                  "when you compare by size or select only some sizes."),
          tags$li("Unstable = fewer than 10 injured or fewer than 1,000 workers. ",
                  "The rate excluding them is shown under the main rate."),
          tags$li("Warning cells (transport/storage/communication; government <5) count people in ",
                  "the numerator who are not in the denominator, so their rates may be overstated.")
        )
      )
    )
  )
}

# ---- 3. Server -----------------------------------------------------
data1_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # 대업종을 바꾸면 세부 업종 목록도 바뀌게
    observe({
      inds <- rate |>
        filter(major_industry_en %in% input$major) |>
        pull(industry_en) |> unique() |> as.character()
      updateSelectizeInput(session, "industry", choices = inds,
                           selected = intersect(isolate(input$industry), inds))
    })

    # 필터 적용 (불안정 칸 처리 전)
    filtered <- reactive({
      d <- rate |>
        filter(year >= input$years[1], year <= input$years[2],
               major_industry_en %in% input$major,
               workers > 0)
      if (length(input$industry) > 0) d <- d |> filter(industry_en %in% input$industry)

      # 규모로 비교하거나 일부 규모만 고르면 건설업(규모 없음)은 빠짐
      all_sizes <- length(input$size) == length(size_levels)
      if (!all_sizes || input$group == "size_en") d <- d |> filter(size_available)
      d <- d |> filter(!size_available | size_en %in% input$size)

      if (input$drop_warn) d <- d |> filter(warning_level_en == "No warning")
      d
    })

    # 불안정 칸 포함 여부 반영
    used <- reactive({
      d <- filtered()
      if (!input$keep_unstable) d <- d |> filter(status_en != "Unstable")
      validate(need(nrow(d) > 0, "No data for this selection. Try other filters."))
      d
    })

    # 업종별 재해율 (평균, 중앙값 계산용)
    ind_rates <- reactive({
      used() |>
        group_by(industry_en) |>
        summarise(r = sum(cases) / sum(workers) * 1000, .groups = "drop")
    })

    fmt <- function(x, d = 0) format(round(x, d), big.mark = ",", nsmall = d)

    output$vb_cases   <- renderText(fmt(sum(used()$cases)))
    output$vb_workers <- renderText(fmt(sum(used()$workers)))
    output$vb_rate    <- renderText(fmt(sum(used()$cases) / sum(used()$workers) * 1000, 2))
    output$vb_rate_stable <- renderText({
      s <- filtered() |> filter(status_en != "Unstable")
      paste0("Excl. unstable: ", fmt(sum(s$cases) / sum(s$workers) * 1000, 2))
    })
    output$vb_mean   <- renderText(fmt(mean(ind_rates()$r), 2))
    output$vb_median <- renderText(fmt(median(ind_rates()$r), 2))

    # 그룹별 요약
    summ <- reactive({
      used() |>
        group_by(group = .data[[input$group]]) |>
        summarise(injured = sum(cases), workers = sum(workers), .groups = "drop") |>
        mutate(rate = injured / workers * 1000,
               share_workers = workers / sum(workers) * 100,
               share_injured = injured / sum(injured) * 100)
    })
# 그래프 폭: 화면이 좁아도 최소 600px로 그림
    plot_w <- function() {
      w <- session$clientData[[paste0("output_", session$ns("plot"), "_width")]]
      if (is.null(w) || w < 600) 600 else w
    }
    output$plot <- renderPlot({
      s <- summ()
      overall <- sum(s$injured) / sum(s$workers) * 1000
      if (input$group == "year") {
        ggplot(s, aes(group, rate)) +
          geom_line(linewidth = 1, colour = "#2c7fb8") +
          geom_point(size = 3, colour = "#2c7fb8") +
          scale_x_continuous(breaks = s$group) +
          labs(x = "Year", y = "Accidents per 1,000 workers")
      } else {
        p <- ggplot(s, aes(group, rate)) +
          geom_col(fill = "#2c7fb8") +
          geom_hline(yintercept = overall, linetype = "dashed") +
          labs(x = NULL, y = "Accidents per 1,000 workers",
               caption = "Dashed line = overall rate of the selection")
        if (input$group == "industry_en") {
          p + geom_text(aes(label = round(rate, 2)), hjust = -0.2, size = 3.5) +
            coord_flip()
        } else {
          p + geom_text(aes(label = round(rate, 2)), vjust = -0.4, size = 3.5)
        }
      }
    }, res = 96, width = plot_w)

    # 규모별로 볼 때 자동으로 나오는 핵심 결과 문장
    output$finding <- renderUI({
      s <- summ()
      if (input$group != "size_en" || !all(c("<5", "1,000+") %in% s$group)) return(NULL)
      a <- s[s$group == "<5", ]; b <- s[s$group == "1,000+", ]
      p(strong("Result: "),
        sprintf("Workplaces with <5 workers have %.2f accidents per 1,000 workers vs %.2f for 1,000+ (%.1f times). ",
                a$rate, b$rate, a$rate / b$rate),
        sprintf("They employ %.1f%% of workers but account for %.1f%% of injured workers.",
                a$share_workers, a$share_injured))
    })

    output$table <- renderTable({
      summ() |>
        mutate(group = as.character(group),
               injured = fmt(injured), workers = fmt(workers)) |>
        rename(Group = group, `Injured` = injured, `Workers` = workers,
               `Rate (per 1,000)` = rate, `Share of workers (%)` = share_workers,
               `Share of injured (%)` = share_injured)
    }, digits = 2, striped = TRUE)
  })
}
