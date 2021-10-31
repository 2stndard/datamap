#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)
library(plotly)
library(DT)
plot_height <- 0

########### 연구데이터 로딩

df_연구 <- read_excel('../연구데이터 조사표(최종).xlsx', skip = 2, na = '-', sheet = '통합', col_types = c(rep('text',26)), col_names = F)


colnames(df_연구) <- c('기관명', '과제명', '연구책임자', '조사명', '조사설명', '분석분야', '분석분야_기타', '분석대상', '분석대상_기타', '조사방법', '조사방법_기타', '설문대상', '설문대상_기타', '설문규모', '설문규모_기타', '설문지공개', '설문지공개_기타', '조사회수', '조사회수_기타', '응답자기본정보항목수', '응답자기본정보항목수_기타', '원본데이터포함여부', '원본데이터포함여부_기타', '다년조사', '다년조사_기타', '승인통계여부', '승인통계여부_기타')

df_연구$기관명 <- as.factor(df_연구$기관명)
df_연구$과제명 <- as.factor(df_연구$과제명)
df_연구$분석분야 <- as.factor(df_연구$분석분야)
df_연구$분석대상 <- as.factor(df_연구$분석대상)
df_연구$조사방법 <- as.factor(df_연구$조사방법)
df_연구$설문대상 <- as.factor(df_연구$설문대상)
df_연구$설문규모 <- as.factor(df_연구$설문규모)
df_연구$설문지공개 <- as.factor(df_연구$설문지공개)
df_연구$조사회수 <- as.factor(df_연구$조사회수)
df_연구$응답자기본정보항목수 <- as.factor(df_연구$응답자기본정보항목수)
df_연구$원본데이터포함여부 <- as.factor(df_연구$원본데이터포함여부)
df_연구$다년조사 <- as.factor(df_연구$다년조사)
df_연구$승인통계여부 <- as.factor(df_연구$승인통계여부)

df_연구$분석분야 <- fct_relevel(df_연구$분석분야, '교육전반', '유아분야', '초중등교육', '고등교육', '평생교육', '기타')

df_연구$설문규모 <- fct_relevel(df_연구$설문규모, '~19', '20~99', '100~499', '500~999', '1000~')

df_연구$설문대상 <- fct_relevel(df_연구$설문대상, '대국민', '정부부처, 교육청', '연구자 및 전문가', '학교행정가(교장, 교감, 보직교원 등)', '교원', '학생', '학부모', '전문가, 행정가 및 교원', '기타')

############# 수탁데이터 로딩

df_수탁 <- read_excel('../수탁데이터 조사표(최종).xlsx', skip = 2, na = '-', sheet = '조사표(작성양식)',  col_names = F)

colnames(df_수탁) <- c('연번', '기관명', '시스템명', '데이터명', '관리부서', '데이터목적', '데이터설명', '세부영역', '주업무목적', '주업무목적_기타', '대상학교급', '대상학교급_기타', '데이터저장단위', '데이터저장단위_기타', '공개범위', '공개범위_기타', '데이터입력범위', '데이터입력범위_기타', '데이터갱신주기', '데이터갱신주기_기타', '시작년도', '시작년도_기타', '데이터구축방법', '데이터구축방법_기타', '데이터형태', '데이터형태_기타', '데이터저장방법', '데이터저장방법_기타', '공개대상', '공개대상_기타', '고유식별정보보유', '고유식별정보보유_기타', '문의처')

df_수탁 <- df_수탁 |> 
  filter(!is.na(주업무목적))

df_수탁$세부영역 <- as.factor(df_수탁$세부영역)
df_수탁$주업무목적 <- as.factor(df_수탁$주업무목적)
df_수탁$대상학교급 <- as.factor(df_수탁$대상학교급)
df_수탁$데이터저장단위 <- as.factor(df_수탁$데이터저장단위)
df_수탁$공개범위 <- as.factor(df_수탁$공개범위)
df_수탁$데이터입력범위 <- as.factor(df_수탁$데이터입력범위)
df_수탁$데이터갱신주기 <- as.factor(df_수탁$데이터갱신주기)
df_수탁$시작년도 <- as.factor(df_수탁$시작년도)
df_수탁$데이터구축방법 <- as.factor(df_수탁$데이터구축방법)
df_수탁$데이터형태 <- as.factor(df_수탁$데이터형태)
df_수탁$데이터저장방법 <- as.factor(df_수탁$데이터저장방법)
df_수탁$공개대상 <- as.factor(df_수탁$공개대상)
df_수탁$고유식별정보보유 <- as.factor(df_수탁$고유식별정보보유)

df_수탁$대상학교급 <- fct_relevel(df_수탁$대상학교급, '교육전반', '유초중등교육', '고등교육', '평생교육')

df_수탁$세부영역 <- fct_relevel(df_수탁$세부영역, '기관영역(학교)', '교원(강사)정보영역', '학급(학과)정보영역', '학생정보영역', '학부모정보영역', '교육과정(강좌)운영영역', '학업성취영역', '시설기자재영역', '예결산영역', '교육지원영역', '학생역량영역', '교원역량영역', '기타')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h1('교육 데이터맵 서비스 프로토타입', align = 'center')),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Select",
                        label = "연구/수탁 선택",
                        choice = c('연구데이터', '수탁데이터'), 
                        selected = '연구데이터'
                        
            ), 
            selectInput(inputId = "XAxis",
                        label = 'X축',
                        choice = NULL
            ), 
            selectInput(inputId = "YAxis",
                        label = 'Y축',
                        choice =NULL
            ), 
            selectInput(inputId = "Facet",
                        label = '분할',
                        choice = NULL
            )
            
        ),
        # Show a plot of the generated distribution
        mainPanel(
          textOutput('text'),
          plotOutput('Plot', click = 'click', inline = FALSE),
          DTOutput('df')
        )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  DIV <- reactive({
    if(input$Select == '연구데이터') {
      selection <- c('분석분야', '분석대상', '조사방법', '설문대상', '설문규모', '설문지공개', '조사회수', '응답자기본정보항목수', '원본데이터포함여부', '다년조사', '승인통계여부')

      updateSelectInput(session,
                        inputId = "XAxis",
                        choice = selection)

      updateSelectInput(session,
                        inputId = "YAxis",
                        choice = selection)

      updateSelectInput(session,
                        inputId = "Facet",
                        choice = selection)
      
      as.data.frame(df_연구)
  
    } else {
    selection <-  c('세부영역', '주업무목적', '대상학교급', '데이터저장단위', '공개범위', '데이터입력범위', '데이터갱신주기', '시작년도',  '데이터구축방법', '데이터형태', '데이터저장방법', '공개대상', '고유식별정보보유', '문의처')
      
    updateSelectInput(session,
                      inputId = "XAxis",
                      choice = selection,
                      selected = '세부영역')
    
    updateSelectInput(session,
                      inputId = "YAxis",
                      choice = selection,
                      selected = '주업무목적')
    
    updateSelectInput(session,
                      inputId = "Facet",
                      choice = selection,
                      selected = '대상학교급')

    as.data.frame(df_수탁)   
    }
  })

# observe({
#      plot_height <- ceiling(nlevels(DIV()[, input$Facet])/2) * 100
#    })
#   
#   output$text <- renderText({
#     plot_hight <- ceiling(nlevels(DIV()[, input$Facet])/2) * 100
#     print(plot_height)
#   })  

    output$df <- renderDT({

    pos_x <- round(input$click$x)
    fct_level1 <- levels(DIV()[, input$XAxis])

    pos_y <- round(input$click$y)
    fct_level2 <- levels(DIV()[, input$YAxis])
    
    if (input$Select == '연구데이터') {
      DIV() |>
        filter(get(input$XAxis) == fct_level1[pos_x], 
               get(input$YAxis) == fct_level2[pos_y],
               get(input$Facet) == input$click$panelvar1) |>
        select('기관명', '과제명', '연구책임자', '조사명', '조사설명')
    }
    else {
      DIV() |>
        filter(get(input$XAxis) == fct_level1[pos_x],
               get(input$YAxis) == fct_level2[pos_y],
               get(input$Facet) == input$click$panelvar1) |>
        select('기관명', '시스템명', '데이터명', '관리부서', '데이터목적', '데이터설명')
    }
    
  })
  
##observe({
  output$Plot <- renderPlot({
    DIV() |> 
      group_by_(input$XAxis, input$YAxis, input$Facet) |>
      count() |>
      group_by_(input$Facet) |>
      mutate(nn = sum(n)) |>
      ggplot(aes_string(x = input$XAxis, y = input$YAxis)) +
      geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
      geom_text(aes(label = n), color = 'white') + 
      facet_wrap(~get(input$Facet), ncol = 2) +
      labs(fill = '전체사례수') +
      theme(axis.text.x = element_text(angle = -90, hjust = 0), 
            axis.text = element_text(size = 15),
            legend.position = 'bottom')
    
    })
##})

}

# Run the application 
shinyApp(ui = ui, server = server)
