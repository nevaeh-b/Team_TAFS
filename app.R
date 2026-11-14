# Project 2 template: the tabs and the inputs only. Everything else, and the look of the app, is yours.
library(shiny)
source("data1_tab.R")   # Data 1 tab (UI + server)
source("global.R")
source("shoe_search.R")
source("explain_pca.R")
source("our_shoes.R")

# Example: read one photo as 4,096 numbers (64 x 64 grayscale, row by row, 0 = black to 255 = white)
# library(magick)
# img <- image_read("shoes/0001.png")
# x <- as.numeric(t(matrix(as.integer(image_data(img, channels = "gray")), 64, 64)))

ui <- navbarPage("TODO: TAFS",
  theme = bslib::bs_theme(version = 5),   # needed for the Data 1 cards and value boxes
  shoe_search_ui,
  explain_pca_ui,
  our_shoes_ui,
  
  tabPanel("Data 1",
    data1_ui("d1")
  ),
  tabPanel("AI",
    h4("Ethics statement"),
    p("We understand the rules on data and AI use for this project, including following the license and privacy rules",
      "of every dataset we used, understanding all of our code, and knowing that presenting AI output as our own work may be",
      "academic misconduct. We certify that we have followed all of them. Any violation is our responsibility,",
      "and we agree to receive an F in this course if we have not."),
    p("Signed: TODO (every member)"),
    h4("What AI did and what we did"),
    tableOutput("ai_table"),
    h4("Reflection"),
    tableOutput("ai_reflection")
  )
)

server <- function(input, output, session) {
  output$result <- renderText("TODO")

  search <- shoe_search_server(input, output, session)
  
  # Explain PCA
  explain_pca_server(
    input, output, session,
    selected_shoe = search$query_vec,
    catalogue_id = search$catalogue_id
  )
  
  # Our Shoes
  our_shoes_server(
    input, output,
    K_now = search$K_now
  )
  
  # Data 1 tab
  data1_server("d1")

  # AI tab: fill in the two tables. Keep the columns.
  output$ai_table <- renderTable(data.frame(
    Tab           = c("Shoe Search", "Explain PCA", "Our Shoes", "Data 1"),
    `What AI did` = c("TODO", "TODO", "TODO", "TODO"),
    `What we did` = c("TODO", "TODO", "TODO", "TODO"),
    check.names = FALSE))
  output$ai_reflection <- renderTable(data.frame(
    Member     = c("박채빈", "유나안", "임서원", "정예은", "조윤서"),
    Reflection = c("빅데이터를 전처리하면서 크기가 작은 데이터를 크게 고려를 안했었는데 어떠한 영향이 있을 수도 있는지 경각심을 일깨워줌. python으로 전처리 한 걸 R에서 다루기 쉽도록 변환하는 걸 부탁했는데 확인해보니 범위가 날짜로 변형되어 있었다. 만약 적은 양의 데이터가 아니고, 수많은 데이터 중간에 오류가 있었다면 찾기 힘들었을 것 같고 간단한 일이라고 해서 너무 AI에 의존하거나 믿지 말고 검사하는 자세(단계)가 필요한 것 같다. ",
                   "Shiny 앱 제작과 배포 과정에서 AI 도움을 많이 받음. 배포 에러(파일 이름 오류, 그래프 출력 오류 등)가 날 때마다 에러 메시지를 보여주고 원인과 수정 방법을 물어 해결함. 혼자 했으면 오래 걸렸을 문제들을 빠르게 고칠 수 있어 시간 단축에 큰 도움이 됨. 다만 AI가 주소를 추측해 알려주거나 다른 폴더를 잡는 등 틀린 경우도 있어서, 코드 실행 위치와 결과는 직접 확인해야 한다는 점을 배움.", 
                   "TODO", "TODO", "TODO"))) #TODO: one or two paragraphs: what AI helped with, where it failed, what you learned.
}

shinyApp(ui, server)
