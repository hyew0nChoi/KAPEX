library(timevis)
library(RSQLite)
library(DT)
library(stringi)
library(dplyr)
library(data.table)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plotly)
library(keyring)
library(shinymanager)
library(plyr)
library(shinyhelper)
library(formattable)
library(psych)
library(stringr)
library(extrafont)
library(lubridate)


# -------------------------------------------------------------------------

# load('KAPEX0620.RData')

if(exists("db_list") == FALSE){
  db_list <<- data.table()
}

if(exists("db_list2") == FALSE){
  db_list2 <<- data.table()
}

if(exists("DT_db_list2") == FALSE){
  DT_db_list2 <<- data.table()
}

if(exists("DT_db_list") == FALSE){
  DT_db_list <<- data.table()
}


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  title = 'KAPEX',
  id = "mains",
  inverse=TRUE,
  
  
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  
  
  
  # css 적용 -------------------------------------------------------------------
  
  
  tags$head(tags$style("
                       /* navbar 탭 바 flex 지정 */
                       .nav.navbar-nav.shiny-tab-input.shiny-bound-input{display: flex; flex-direction: row; justify-content: space-around; margin-left: -250px;}
                       /* db_select 탭 바 hide */
                       #DB_select {display : none;}
                       /* 코드관리 그룹 사이즈 설정 */
                       #code_group {height: 600px; width: 300px; font-size: 15px; }
                       /* 코드관리 테이블 줄바꿈 제거 */
                       #code_monitor table {white-space: nowrap;}
                       /* 코드관리 search 버튼 색상 */
                       #code_search_bt {background: #1f4394; color : #ffffff;} 
                       /* 신규등록 버튼 색상 */
                       #add_db {background: #1f4394; color : #ffffff;} 
                       /* DB조회 search 버튼 색상 */
                       #db_search2 {background: #1f4394; color : #ffffff;} 
                       /* DB조회 search 버튼 색상 */
                       #db_search {background: #1f4394; color : #ffffff;}
                       /* input 사이즈 width 100% */
                       .form-group.shiny-input-container{width:100%}
                       /* tier dropdownbutton 설정*/
                       .btn.btn-default.action-button.btn-circle.dropdown-toggle.shiny-bound-input {vertical-align: -3px;padding:0px; height:39px; width:21px; border-color:#ccc0;} 
                       /* 양식 다운로드 width */
                       #ta_form {width:130px}
                       #in_form {width:130px}
                       #out_form {width:130px}
                       /* 코드관리 테이블 줄바꿈 제거 */
                       #code_monitor table {white-space: nowrap;}
                       /* search hide */
                       .dataTables_filter{display:none;}
                       /* 미리보기 monitor 줄바꿈 제거 */
                       #modalpreview .dt-center sorting{white-space: nowrap;}
                       
                       ")),
  
  uiOutput('style_tag'),
  
  fluidRow(
    
    column(2),
    
    column(8,
           
           ## home logo
           
           fluidRow(
             
             column(4),
             
             column(4, align = "center",
                    div(id = "home_logo",
                        
                        style= "margin-left: -31px;margin-top:28px; margin-bottom: 5px",
                        
                        tags$img(src = "KAPEX/logo.png", align = 'center'))),
             
             column(4, style = 'padding:0px;',
                    div(style = "display : inline-block; ",
                        actionButton(style = 'margin-left:189px; margin-top:22px; width:120px;',
                                     inputId = "user_manual", label = "사용자 매뉴얼")),
                    div(style = "display : inline-block; ",
                        actionButton(style = 'margin-top:22px;width:120px;',
                                     inputId = "system_management_bt", label = "시스템 관리"))
             )
             
           ),
           
           br(),
           
           # tab구성 -------------------------------------------------------------------
           
           navbarPage(
             
             title = NULL,
             
             id = "inTabset",
             selected = 'home',
             
             
             # ~ tab : Home ---------------------
             
             tabPanel(id = 'home',
                      
                      title = 'HOME',
                      
                      value = 'home',
                      
                      fluidRow()
                      
             ),
             
             
             
             # ~ tab : KAPEX 소개  ------------------
             
             tabPanel(id = 'system_info',
                      title = 'KAPEX 소개',
                      value = 'system_info',
                      
                      # tags$head(tags$style( HTML(' system_info .tab-content {margin-top:-50px;}'))),
                      column(12, style = 'padding:0px;',
                             wellPanel(style = "background :#ffffff; padding: 45px; margin-left: -15px; margin-right: -15px;",
                                       
                                       fluidRow(id = 'info_1',
                                                h2(style = 'margin-top:0px; margin-left: 23px;','KAPEX 소개')),
                                       
                                       fluidRow(id = 'info_2',
                                                column(5,
                                                       id = 'info_2_1',
                                                       tags$img(src = "KAPEX/kosem_img.PNG")),
                                                
                                                column(7,
                                                       id = 'info_2_2',
                                                       style = 'margin-left: -40px;',
                                                       br(),
                                                       
                                                       h4("'한국형 초미세먼지 국민노출 모델", br(), "(Korea Simulation Exposure Model for PM (KoSEM-PM)'"),
                                                       br(),
                                                       tags$p('우리나라 국민의 초미세먼지 노출 수준을 파악하고 노출과 관련된 인자를 평가하기 위한 모델이다. 통계청에서 실시한 시간활동패턴 조사 자료를 활용하며 국내에서 3계절동안 다양한 정소에서 실측된 초미세먼지 농도 측정 자료를 활용하여 초미세먼지 노출을 계산하는 모델이다.'),
                                                       br(),
                                                       tags$p('각각의 장소에서 특정시간대에 발생되는 초미세먼지 농도는 외기농도의 영향뿐 아니라 실내활동에 의해 영향을 받기 때문에 실측자료에서 제공되는 분포를 이용하여 국민이 각 시간대별로 거주하고 행동하는 패턴을 적용하여 국민개인노출의 분포를 도출하는 것이다.'),
                                                       br(),
                                                       tags$p('국민개인노출정보는 국민의 개인노출에 미치는 인자를 파악하고 고노출군의 관리에 필요한 정책 도출에 활용될 수 있다. 또 개인형 KOSEM은 개인정보를 제공하면 특정 개인의 초미세먼지 개인노출의 문포를 파악하고 이에 영향을 미치는 인자를 파악할 수 있으며 초미세먼지 저감에 필요한 행동요령의 제공도 가능하다.'),
                                                       br(),
                                                       tags$p(align = 'right', '2018.06.08.', br(), '서울대학교 생활환경시스템 연구실', br(), '이기영 교수')
                                                )
                                                
                                       ) 
                             ) 
                      )
             ),
             
             
             # ~ tab : KAPEX DB  ------------------
             
             navbarMenu('KAPEX DB',
                        
                        # ~~ KAPEX DB : DB 소개 ----------------------------------
                        
                        tabPanel(title = 'DB 소개',
                                 value = 'db',
                                 id = 'db',
                                 
                                 
                                 column(12, style = 'padding:0px;',
                                        
                                        
                                        wellPanel(style = "background :#ffffff; padding: 45px; margin-left: -15px; margin-right: -15px;",
                                                  fluidRow(
                                                    
                                                    h2(style = 'margin-top:0px;','DB 소개'),
                                                    
                                                    tags$hr(style="border-color: #555; margin-bottom: 0px; ")
                                                    
                                                  ),
                                                  
                                                  
                                                  fluidRow(style = 'margin: 0px;',
                                                           tabsetPanel(type = "tabs",
                                                                       id = 'DB_select',
                                                                       
                                                                       
                                                                       tabPanel(title = '목록',
                                                                                id = 'DB_select_1',
                                                                                
                                                                                fluidRow(id = 'img',
                                                                                         
                                                                                         column(6,
                                                                                                tags$img(id = 'bg_db01',
                                                                                                         src = 'KAPEX_CSS/images/bg_db01.gif'),
                                                                                                column(6,
                                                                                                       tags$img(id = 'in_png',
                                                                                                                style = 'margin-top: -165px; margin-left: 75px;',
                                                                                                                src = 'KAPEX_CSS/images/ico_db01.png')),
                                                                                                column(6,
                                                                                                       tags$img(id = 'out_png',
                                                                                                                style = 'margin-top: -165px',
                                                                                                                src = 'KAPEX_CSS/images/ico_db02.png'))
                                                                                         ),
                                                                                         column(6,
                                                                                                tags$img(id = 'bg_db02',
                                                                                                         src = 'KAPEX_CSS/images/bg_db02.gif'),
                                                                                                
                                                                                                tags$img(id = 'ta_png',
                                                                                                         style = 'margin-top: -165px; margin-left: 196px;',
                                                                                                         src = 'KAPEX_CSS/images/ico_db03.png'))
                                                                                ),
                                                                                
                                                                                
                                                                                fluidRow(id = 'db_2',
                                                                                         
                                                                                         column(6,
                                                                                                id = 'db_2_1',
                                                                                                style = 'padding-left: 0px;',
                                                                                                wellPanel(
                                                                                                  h5('· 실내 : 주생활 공간, 기타장소, 이동수단을 대상으로 
                                                                                      CO, O3, PM2.5, 온도, 습도에 대해 1분 단위로'), 
                                                                                                  h5(HTML('&nbsp;'),'측정한 데이터로, KAPEX 과제에서 생산한 자료를 기본으로 제공합니다.'),
                                                                                                  br(),
                                                                                                  h5('· 실외 : CO, O3, PM10, PM2.5를 대상으로 1시간 단위로 
                                                                                      외기 농도를 측정한 정보로서, 환경부'), 
                                                                                                  h5(HTML('&nbsp;'),'에어코리아 홈페이지 공개 자료를 기본으로 제공합니다.')
                                                                                                )),
                                                                                         
                                                                                         column(6,
                                                                                                id = 'db_2_2',
                                                                                                style = 'padding-right: 0px;',
                                                                                                wellPanel(
                                                                                                  h5('· 시간활동 : 하루 24시간을 10분 단위로 나누어 조사대상자가 
                                                                                      머문 장소와 이동수단을 조사한'), 
                                                                                                  h5(HTML('&nbsp;'), '자료로서, 본 시스템에서는 KAPEX 과제 생산 자료와 통계청(2019) 조사 자료를 기본으로'),
                                                                                                  h5(HTML('&nbsp;'),'제공합니다.')
                                                                                                )
                                                                                         )),
                                                                                
                                                                                fluidRow(id = 'db_3',
                                                                                         h5('* KAPEX: Development of Korea Air pollutants Personal EXposure model.'), 
                                                                                         h5(HTML('&nbsp;'), '공기오염물질의 실내외 시공간적 변화에 따른 노출량 평가 및 추정 기술 개발 (2021.04~2025.12)'))
                                                                       )
                                                                       
                                                                       
                                                           )
                                                  )
                                                  
                                                  
                                                  
                                                  
                                        )
                                 )
                        ),
                        
                        
                        # ~~ KAPEX DB : DB 조회 ----------------------------------
                        
                        
                        tabPanel(title = 'DB 조회',
                                 value = 'db_stat',
                                 id = 'db_stat',
                                 
                                 
                                 column(12, style = 'padding:0px;',
                                        
                                        
                                        wellPanel(style = "background :#ffffff; padding: 45px; margin-left: -15px; margin-right: -15px;",
                                                  fluidRow(id = 'id_db_stat1',
                                                           
                                                           h2(style = 'margin-top:0px;','DB 조회'),
                                                           
                                                           tags$hr(style="border-color: #555; margin-bottom: 0px; ")
                                                           
                                                  ),
                                                  
                                                  fluidRow(id = 'db_1',
                                                           wellPanel(
                                                             fluidRow(h5(
                                                               '본 메뉴는 모니터링 DB와 노출계수 DB 정보를 확인할 수 있는 기능을 제공합니다.'))
                                                           )),
                                                  ## DB 조회 monitor
                                                  
                                                  fluidRow(id = 'dbdb1',
                                                           div(style = "display : inline-block; margin-left: 25px;",
                                                               h5('DB 구분')),
                                                           div(style = "display : inline-block; margin-left: 20px; width: 12%;",
                                                               pickerInput(inputId = 'sel2', label = '', choices = c('전체', '시간활동', '실내농도', '실외농도'), selected = '전체')),
                                                           div(style = "display : inline-block; margin-left: 25px;", h5('자료원명')),
                                                           div(style = "display : inline-block; margin-left: 20px; width:25%;",
                                                               textInput(inputId = 'search2', label = '')),
                                                           div(style = "display : inline-block; margin-left: 25px;", h5('수집년도')),
                                                           div(style = "display : inline-block; margin-left: 20px; width: 10%;",
                                                               pickerInput(inputId = 'sel_date', label = '', 
                                                                           choices = if(nrow(db_list) == 0){
                                                                             NULL
                                                                           } else {
                                                                             c('전체', min(substr(db_list$수집년도, 1, 4) %>% as.numeric()):max(substr(db_list$수집년도, 6, 9) %>% as.numeric()))
                                                                           },
                                                                           selected = NULL)),
                                                           div(style = "display : inline-block; margin-left: 25px;", h5('수행기관')),
                                                           div(style = "display : inline-block; margin-left: 20px; width: 10%;",
                                                               pickerInput(inputId = 'sel_admin2', label = '', 
                                                                           choices = if(nrow(db_list) == 0){
                                                                             NULL
                                                                           } else {
                                                                             c('전체', db_list$수행기관)
                                                                           },
                                                                           selected = NULL)),
                                                           div(style = "display : inline-block; ",
                                                               actionButton(style = 'margin-left: 25px; width: 100px; ',
                                                                            inputId = "db_search2", label = "검색")),
                                                           
                                                           
                                                           
                                                           tags$hr(),
                                                           
                                                           fluidRow(id = 'dbdb2',
                                                                    div(style = "display : inline-block;  ", h5('총 ')), 
                                                                    div(style = "display : inline-block; color:red;", textOutput('monitordb2_num')), 
                                                                    div(style = "display : inline-block; ", h5(' 건 검색되었습니다.')),
                                                                    
                                                                    div(style = "display : inline-block; float : right;",
                                                                        downloadButton(
                                                                          outputId = "list_download", label = "목록 다운로드"))
                                                                    
                                                           ),
                                                           
                                                           fluidRow(id = 'dbdb3',
                                                                    DTOutput("monitor_db2")
                                                           )       
                                                  ),
                                                  
                                                  
                                                  
                                                  
                                                  
                                        )
                                        
                                 )
                        )
             ),
             
             
             
             ## 노출평가 tab
             
             tabPanel(title = '노출평가', 
                      value = 'assessment',
                      
                      ## .. 노출평가 탭 img_ready         
                      column(12, style = 'padding:0px;',
                             wellPanel(style = "background :#ffffff; padding: 45px; margin-left: -15px; margin-right: -15px;",
                                       fluidRow(
                                         align = "center",
                                         tags$img(src = 'KAPEX/img_ready.jpg')
                                       )
                                       
                                       
                                       
                             )
                      )
                      
                      
             ),
             
             
             
             tabPanel(id = 'system_management',
                      title = '시스템 관리', 
                      value = 'system_management',
                      
                      br(),
                      br(),
                      
                      column(12,
                             
                             fluidRow(align = 'center',
                                      div(
                                        id = "login-basic", 
                                        style ="width: 500px; max-width: 100%; margin: 0 auto;",
                                        
                                        ## 로그인 ui -------------------------------------------
                                        
                                        div(
                                          h4(class = "text-center", "MEMBER LOGIN"),
                                          p(class = "text-center", 
                                            tags$small("KAPEX에 오신 것을 환영합니다.")
                                          ),
                                          
                                          textInput(
                                            inputId  = "ti_user_name_basic", 
                                            label = '',
                                            placeholder = "아이디"
                                          ),
                                          
                                          passwordInput(
                                            inputId     = "ti_password_basic", 
                                            label = '',
                                            placeholder = "비밀번호"
                                          ), 
                                          
                                          div(
                                            class = "text-center",
                                            actionButton(
                                              inputId = "ab_login_button_basic", 
                                              label = "로그인",
                                              class = "btn-primary"
                                            )
                                          )
                                          
                                          
                                          
                                        )
                                      ),
                                      
                                      uiOutput(outputId = "display_content_basic")
                                      
                             )
                             
                      )
                      
                      
             )
             
           )        
    ),
    
    column(1),
    
    column(1,
           div(style="display : inline-block; padding:6px; margin-top:15px; margin-left:50px;",
               actionBttn(inputId = 'close_button', icon =  icon("power-off"), style = "jelly")))
  ),
  
  
  fluidRow(
    class = 'footer',
    # align = "center",
    
    style = "left:0px; right:0px; bottom:0px; position:fixed; cursor:inherit; z-index: 10000; height : 90px;",
    
    wellPanel(
      style = "border-bottom: 1px solid #CCC; background :#22242A; color : white; height : 90px",
      
      align="center",
      
      ## 환경부 GIF 
      div(style="display: inline-block;",
          tags$img(src = "KAPEX/logo_footer01.gif")),
      HTML('&nbsp;'),
      HTML('&nbsp;'),
      HTML('&nbsp;'),
      HTML('&nbsp;'),
      HTML('&nbsp;'),
      
      div(style="display : inline-block; vertical-align: middle;",
          h6(align="left",
             HTML('서울특별시 관악구 관악로 1 서울대학교 보건대학원
                     <br/> <br/> COPYRIGHTⒸ BY MINISTRY OF ENVIRONMENT. ALL RIGHTS RESERVED.')
          )
      ),
      
      HTML('&nbsp;'),
      HTML('&nbsp;'),
      HTML('&nbsp;'),
      HTML('&nbsp;'),
      
      ## 환경부 GIF
      div(style="display: inline-block;",
          tags$img(src = "KAPEX/logo_footer02.gif"))
      
    )
    
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", type="text/css", href = "KAPEX_CSS/base.css")
  )
)


server <- function(input, output, session) { 
  
  # TEST --------------------------------------------------------------------
  
  output$test <- renderText({
    
    input$file_upload
    
  })
  
  # 시스템 관리 ------------------------------------------------  
  
  # 시스템 관리 : onclick ----------------------------------------------------------
  onclick('system_management_bt',{
    updateTabItems(session, 'inTabset',
                   selected = 'system_management')
    
  })
  
  
  # ~~ 로그인 server ---------------------
  user_base_basic_tbl <- tibble(
    user_name = "hwchoi",
    password  = "cheminet"
  )
  
  # check credentials vs tibble
  validate_password_basic <- eventReactive(input$ab_login_button_basic, {
    
    validate <- FALSE
    
    if (input$ti_user_name_basic == user_base_basic_tbl$user_name &&
        input$ti_password_basic == user_base_basic_tbl$password)
    {validate <- TRUE} else {
      showModal(
        modalDialog(
          '암호를 다시 입력하세요'
          
        ))
      
    }
  })
  
  # hide form 
  observeEvent(validate_password_basic(), {
    shinyjs::hide(id = "login-basic")
  })
  
  # ~~ 로그인 변경 render ui ----------------
  
  output$display_content_basic <- renderUI({
    
    req(validate_password_basic())
    
    tabsetPanel(type = "tabs",
                id = 'manage_tab',
                
                ## 시스템관리 tab 구성 ----------------------------------        
                
                
                ## ~ 코드관리 ----------------------------------        
                
                
                tabPanel(title = '코드 관리',
                         id = 'code_manage',
                         
                         column(3, style = 'padding:0px;',
                                
                                fluidRow(align = 'left',
                                         id = 'code_mg1',
                                         selectInput('code_group', 
                                                     choices = c('전체', column_def$MODULE_DESC %>% unique()), 
                                                     label = '',
                                                     selected = '전체',
                                                     multiple = FALSE, 
                                                     selectize = FALSE, size = 4)
                                         
                                )
                         ),
                         
                         column(9, 
                                
                                fluidRow(align = 'left',
                                         id = 'code_mg2',
                                         div(style = "display : inline-block; ", h5("코드명")),
                                         div(style = "display : inline-block; margin-left: 30px; width:720px;",
                                             textInput(inputId = 'code_search', label = '', placeholder = '코드 대표명, 국문 코드명, 영문 코드명을 입력하세요.')),
                                         div(style = "display : inline-block; ",
                                             actionButton(style = 'margin-left: 3px; width: 115px; margin-top: -4px;',
                                                          inputId = "code_search_bt", label = "검색"))
                                         
                                ),
                                
                                br(),
                                fluidRow(id = 'code_mg3',
                                         align = 'left',
                                         
                                         div(style = "display : inline-block;  ", h5('총 ')), 
                                         div(style = "display : inline-block; color:red;", textOutput('code_num')), 
                                         div(style = "display : inline-block; ", h5(' 건 검색되었습니다.')),
                                         
                                         br(),
                                         
                                         div(style = "margin-top: 25px;", 
                                             DTOutput("code_monitor"))
                                )
                         )
                         
                         ## 코드 관리 파일 
                         
                ),
                
                ## ~ DB관리 ----------------------------------  
                tabPanel(title = 'DB 관리',
                         id = 'db_manage',
                         
                         column(12, style = 'padding:0px;',
                                
                                fluidRow(
                                  id = 'monitordb_1',
                                  
                                  
                                  div(style = "display : inline-block;",
                                      h5('DB 구분')),
                                  div(style = "display : inline-block; margin-left: 20px; width: 12%;",
                                      pickerInput(inputId = 'sel1', label = '', choices = c('전체', '시간활동', '실내농도', '실외농도'), selected = '전체')),
                                  div(style = "display : inline-block; margin-left: 25px;", h5('자료원명')),
                                  div(style = "display : inline-block; margin-left: 20px; width:25%;",
                                      textInput(inputId = 'search', label = '')),
                                  #수정
                                  div(style = "display : inline-block; margin-left: 25px;", h5('수집년도')),
                                  div(style = "display : inline-block; margin-left: 20px; width: 10%;",
                                      pickerInput(inputId = 'sel_date2', label = '', 
                                                  choices = if(nrow(db_list) == 0){
                                                    NULL
                                                  } else {
                                                    c('전체', min(substr(db_list$수집년도, 1, 4) %>% as.numeric()):max(substr(db_list$수집년도, 6, 9) %>% as.numeric()))
                                                  },
                                                  selected = NULL)),
                                  div(style = "display : inline-block; margin-left: 25px;", h5('수행기관')),
                                  div(style = "display : inline-block; margin-left: 20px; width: 10%;",
                                      pickerInput(inputId = 'sel_admin', label = '', 
                                                  choices = if(nrow(db_list) == 0){
                                                    NULL
                                                  } else {
                                                    c('전체', db_list$수행기관)
                                                  },
                                                  selected = NULL)),
                                  div(style = "display : inline-block; margin-left: 25px; ",
                                      actionButton(style = 'width: 100px; ',
                                                   inputId = "db_search", label = "검색")),
                                  
                                  
                                  tags$hr(),
                                  
                                  fluidRow(id = 'mdb_2',
                                           div(style = "display : inline-block; margin-left: -900px;  ", h5('총 ')), 
                                           div(style = "display : inline-block; color:red;", textOutput('monitordb_num')), 
                                           div(style = "display : inline-block; ", h5(' 건 검색되었습니다.'))
                                  ),
                                  
                                  
                                  div(style = "display : inline-block; margin-left: 5px; float : right;",
                                      downloadButton(
                                        outputId = "list_download2", label = "목록 다운로드")),
                                  div(style = "display : inline-block;  float : right;",
                                      actionButton(style = 'margin-left: 3px; width: 100px; ',
                                                   inputId = "add_db", label = "신규등록")),
                                  
                                  fluidRow(
                                    id = 'monitordb_2',
                                    DTOutput("monitor_db")
                                  )
                                  
                                )
                                
                                
                         )
                         
                         
                )
    )
    
  })
  
  # 코드관리  -----------------------------------------------------------------
  
  # ~ 코드관리 : datatable  -----------------------------------------------------------------
  
  output$code_monitor <- renderDT({
    input$code_group
    input$code_search_bt
    
    code_monitor_dt %>% datatable(escape = F, options = list(lengthChange = FALSE,
                                                             info = FALSE))
  })
  
  
  observeEvent(input$code_group, {
    if(input$code_group == '전체'){
      
      code_monitor_dt <<- column_def %>%
        select(MODULE, CD, CD_DESC_RPRS, CD_DESC_EN, DEL_YN)
      
    } else {
      
      code_monitor_dt <<- column_def %>% filter(MODULE_DESC == input$code_group) %>%
        select(MODULE, CD, CD_DESC_RPRS, CD_DESC_EN, DEL_YN)
    }
    
    updateTextInput(session, "code_search", value = "", placeholder = '코드 대표명, 영문 코드명을 입력하세요.' )
    db_nrow <<- ""
  })
  
  # ~ 코드관리 : search 버튼  -----------------------------------------------------------------
  
  observeEvent(input$code_search_bt, {
    if(input$code_group == '전체'){
      
      code_monitor_dt2 <<- column_def %>%
        select(MODULE, CD, CD_DESC_RPRS, CD_DESC_EN, DEL_YN)
      
    } else {
      
      code_monitor_dt2 <<- column_def %>% filter(MODULE_DESC == input$code_group) %>%
        select(MODULE, CD, CD_DESC_RPRS, CD_DESC_EN, DEL_YN)
    }
    
    code_monitor_dt <<- code_monitor_dt2[lapply(code_monitor_dt2, function(x) {grep(pattern = input$code_search, x = x, ignore.case = T)}) %>% unlist() %>% unique(), ]
    
  })  
  
  
  output$code_num <- renderText({
    input$code_group
    input$code_search_bt
    
    
    length(lapply(code_monitor_dt, function(x) {grep(pattern = db_nrow, x = x, ignore.case = T)}) %>% unlist() %>% unique())
    
  })
  
  # ~ 코드관리 : 상세보기 ------------------------------- 
  observeEvent(input$CODEMAIN, {
    
    showModal(
      modalDialog(
        title = '코드관리 상세보기',
        DTOutput("col_def"),
        
        size = "m",
        easyClose = TRUE,
        
        footer =  tagList(modalButton("확인"))
        
      )
    )
  })
  
  output$col_def <- renderDT({
    input$CODEMAIN
    
    coldef1 <-  column_def[input$CODEMAIN[1], 1:16]
    coldef1$CD_DESC_RPRS <- column_def$REAL_CD_DESC_RPRS[input$CODEMAIN[1]]
    coldef1 %>% t() %>% datatable(options = list(pageLength = 16, 
                                                 dom = 'ft,',
                                                 headerCallback = JS(
                                                   "function(thead, data, start, end, display){",
                                                   "  $(thead).remove();",
                                                   "}")
    ))
  })
  
  # DB 관리 ------------------------------
  
  # ~ DB 관리 : 신규 등록 팝업창 ----------------------------------------------------------------
  
  observeEvent(input$add_db, {
    
    dataset$data <- NULL
    
    showModal(
      modalDialog(
        title = '자료원 등록',
        
        
        fluidRow(id = 'add_db_1',
                 column(2, align="center",
                        h5(style = 'margin-top:28px;', "자료원명")),
                 column(10,
                        div(style="display : inline-block; width: 96%;", 
                            textInput(inputId = 'DB_NAME', label = '', value = ''))
                        
                 )
                 
        ),
        
        fluidRow(id = 'add_db_2',
                 
                 column(2, align="center",
                        h5(style = 'margin-top:28px;', "DB 구분")),
                 column(4,
                        div(style="display : inline-block; width : 90%;", 
                            pickerInput(inputId = 'DB_TYPE', label = '', choices = c('시간활동', '실내농도', '실외농도')))),
                 
                 column(2, align="center",
                        h5(style = 'margin-top:28px;',"수집방법")),
                 column(4,
                        div(style="display : inline-block; width : 90%;", 
                            pickerInput(inputId = 'DB_COLLECTION', label = '', 
                                        choices = c('실측', '예측', '조사'))))
        ),
        
        
        
        fluidRow(id = 'add_db_3',
                 column(2, align="center",
                        h5(style = 'margin-top:28px;',"수행기관")),
                 column(4,
                        div(style="display : inline-block;  width : 90%;", 
                            pickerInput(inputId = 'DB_SOURCE', label = '', 
                                        choices = c('K-STAT', '통계청')))),
                 
                 column(2, align="center",
                        h5(style = 'margin-top:28px;',"수집기간")),
                 column(4,
                        div(style="display : inline-block; width : 90%;", 
                            dateRangeInput("DB_DATE", "",
                                           start = "2019-01-01", 
                                           end = as.character(Sys.Date()))
                        )
                 )
                 
        ),
        
        fluidRow(id = 'add_db_4',
                 
                 column(2, align="center",
                        h5(style = 'margin-top:28px;',"조사지역")),
                 column(4,
                        div(style="display : inline-block; width : 90%;", 
                            textInput(inputId = 'DB_DEPARTMENT', label = '', value = '')
                        )
                 ),
                 
                 column(2, align="center",
                        div(style="display : inline-block; margin-top:28px", h5("단계")),
                        div(id = 'dropdown1',
                            style="display : inline-block; margin-top: -3px;",
                            dropdownButton(
                              DTOutput("info_tier"),
                              circle = TRUE,
                              icon = fontawesome::fa("info-circle"),
                              tooltip = tooltipOptions(title = "Click"),
                              margin = '20px',
                              width = '750px'
                            )
                        )),
                 column(4,
                        div(style="display : inline-block; width : 90%;",
                            pickerInput(inputId = 'DB_STEP', label = '',
                                        choices = c('1단계', '2단계', '3단계', '4단계', '5단계'))
                        ))
                 
                 
        ),
        
        br(),
        
        fluidRow(id = 'add_db_5',
                 column(2, align="center",
                        h5(style = 'margin-top:10px;', "양식 다운로드")),
                 column(10,
                        div(style="display : inline-block;", 
                            downloadButton(outputId = 'ta_form', label = '시간활동' )),
                        div(style="display : inline-block;", 
                            downloadButton(outputId = 'in_form', label = '실내농도')),
                        div(style="display : inline-block;", 
                            downloadButton(outputId = 'out_form', label = '실외농도'))
                 )
        ),
        
        br(),
        
        fluidRow(id = 'add_db_6',
                 column(2, align="center",
                        h5(style = 'margin-top:28px;', "자료 업로드")),
                 column(10,
                        div(style="display : inline-block; ", 
                            fileInput("file_upload", "", width = '96%'))
                 )
        ),
        
        fluidRow(id = 'add_db_7',
                 column(2, align="center",
                        h5(style = 'margin-top:28px;', "비고")),
                 column(10,
                        div(style="display : inline-block; width: 96%;", 
                            textAreaInput("reference", "", value = "",  height = '100px', resize = 'none'))
                 )
        ),
        
        
        # verbatimTextOutput('test'),
        
        size = "l",
        footer = tagList(actionButton(inputId = "add_db_ok", "저장"),
                         actionButton(inputId = "add_db_no", "닫기")),
        easyClose = TRUE
        
      )
    )
    # disable('DB_ID')
  })
  
  
  
  
  # ~~ 신규 등록 : DB구분 변경 시  ------------------------------------------------
  
  
  observeEvent(input$DB_TYPE,{
    
    if(input$DB_TYPE == '시간활동'){
      updatePickerInput(session, 'DB_SOURCE', choices = c("K-STAT", "통계청(2019)"))
    }else if(input$DB_TYPE == '실내농도'){
      updatePickerInput(session, 'DB_SOURCE', choices = c("서울대학교", "서경대학교"))
    }else if(input$DB_TYPE == '실외농도'){
      updatePickerInput(session, 'DB_SOURCE', choices = c("에어코리아", "대구가톨릭대학교"))
    }
  })
  
  
  # ~~ 신규 등록 : 단계 info ------------------------------------------------ 
  
  output$info_tier <- renderDT({
    tier %>% datatable(options = list(searching = FALSE,
                                      lengthChange = FALSE,
                                      info = FALSE,
                                      dom = 'ft'),
                       rownames = FALSE)
  })
  
  # ~~ 신규 등록 : no ------------------------------------------------ 
  
  observeEvent(input$add_db_no, {
    removeModal()
  })
  
  
  # ~~ 신규 등록 : ok --------------------------------------------------------------------
  
  observeEvent(input$add_db_ok, {
    
    if(input$DB_NAME == '' | is.null(dataset$data)){
      
      showModal(
        modalDialog(
          title = '잘못된 등록방법입니다.',
          h5('자료원명을 작성하고 자료를 업로드 해주세요.'),
          
          footer =  tagList(modalButton("확인")),
          easyClose = TRUE
          
        )
      )
      
    } else {
      
      CODE_NUM <- if(
        nrow(db_list) == 0){
        paste0("KA", sprintf("%04d", 1))
      } else {
        paste0("KA", sprintf("%04d", max(as.numeric(gsub("[^0-9]", "", db_list$자료원ID)))+1))
      }
      
      # data_set <- data.frame()
      data_set <<- dataset$data
      
      ## 측정시간 형식 
      posixt <- which(lapply(data_set, class) %>% data.frame() %>% .[1,] %>% unlist() == 'POSIXct')
      
      if(length(posixt) == 0){
        
      } else{
        
        data_set[,posixt] <<- lapply(data_set[,posixt], function(x) as.POSIXct(x, format = "%d-%b-%Y %H:%M:%OS")) %>% data.frame()
        
        data_set[,posixt] <<- lapply(data_set[,posixt], function(x) as.character(x)) %>% data.frame()
        
        if('측정시간' %in% colnames(data_set[,posixt])){
          data_set[,which(colnames(data_set) == '측정시간')] <<-
            substr(data_set[,which(colnames(data_set) == '측정시간')] %>% unlist(), 12, 16) %>% data.frame()
        }
        
      }
      
      ## colname
      if(input$DB_TYPE == '실내농도'){
        data_set <<- left_join(data_set, indoor_codebook,
                               by = c('측정순서 ID' = 'INDOOR_MEASURE_POINT_ID'))
        
      }else if(input$DB_TYPE == '실외농도'){
        data_set <<- left_join(data_set, outdoor_codebook,
                               by = c('측정소코드' = 'OUTDOOR_MEASURE_POINT_ID'))
        
      }
      
      con <- dbConnect(SQLite(), dbname = "K_APEX.db")
      dbWriteTable(con, paste0(input$DB_TYPE, "_", CODE_NUM), data_set)
      dbDisconnect(con)
      
      
      
      one_db <- data.table("자료원ID" = CODE_NUM,
                           "DB구분" = input$DB_TYPE,
                           "자료원명" = input$DB_NAME,
                           "수집방법" = input$DB_COLLECTION,
                           "수집년도" = paste0(substr(input$DB_DATE[1], 1, 4), '~', substr(input$DB_DATE[2], 1, 4)),
                           "수행기관" = input$DB_SOURCE,
                           "찐_자료원명" = input$DB_NAME,
                           "수집기간" = paste0(input$DB_DATE[1], '~', input$DB_DATE[2]),
                           "조사지역" = input$DB_DEPARTMENT,
                           "단계" = input$DB_STEP,
                           "비고" = input$reference
      )
      
      
      db_list <<- bind_rows(db_list, one_db)
      
      db_list$자료원명 <<- paste0('<span><a href="javascript:void(0)" onmousedown="',
                              'Shiny.onInputChange(\'DTClick\',[', as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))), ',Math.random()]);',
                              'event.preventDefault(); event.stopPropagation(); return false;"><font color="blue"><b>', db_list$찐_자료원명, '</b></font></a></span>')
      
      db_list2 <<- db_list
      
      db_list2$자료원명 <<- paste0('<span><a href="javascript:void(0)" onmousedown="',
                               'Shiny.onInputChange(\'DTClick2\',[', as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))), ',Math.random()]);',
                               'event.preventDefault(); event.stopPropagation(); return false;"><font color="blue"><b>', db_list$찐_자료원명, '</b></font></a></span>')
      
      removeModal()
      
      showModal(
        modalDialog(
          title = '미리보기',
          
          br(),
          
          h4('데이터는 총 ', comma(nrow(dataset$data), format = 'd'), '건 입니다.'),
          br(),
          h5('본 화면은 미리보기 기능으로, 30건의 데이터만 확인하실 수 있습니다.'),
          h5('전체 DB를 확인하시려면 본 화면을 닫으신 후, 목록에서 해당 자료원을 클릭하세요.'),
          br(), br(),
          div(style = 'white-space : nowrap;',
              DTOutput("modalpreview")),
          
          size = "l",
          easyClose = TRUE,
          footer = actionButton("add_db_no", "닫기")
        )
      )
      
      
      updatePickerInput(session = session, inputId = 'sel_date',
                        choices = c('전체', min(substr(db_list$수집년도, 1, 4) %>% as.numeric()):max(substr(db_list$수집년도, 6, 9) %>% as.numeric()))
                        
      )
      
      updatePickerInput(session = session, inputId = 'sel_date2',
                        choices = c('전체', min(substr(db_list$수집년도, 1, 4) %>% as.numeric()):max(substr(db_list$수집년도, 6, 9) %>% as.numeric()))
                        
      )
      
      updatePickerInput(session = session, inputId = 'sel_admin',
                        choices = c('전체', db_list$수행기관)
                        
      )
      
      updatePickerInput(session = session, inputId = 'sel_admin2',
                        choices = c('전체', db_list$수행기관)
                        
      )
      
      
    }
    
  })
  
  
  # ~ DB 관리 : 파일 업로드 dataset ---------------------------------------------------------------------------- 
  
  
  dataset <- reactiveValues(data = NULL)
  
  observe({
    
    inFile <- input$file_upload
    
    dataset$data <- if(is.null(inFile)) {
      return(NULL)
    } else if (identical(grep("xlsx",inFile$name) == 1, logical(0)) == FALSE) {
      readxl::read_xlsx(inFile$datapath)
    } else {
      read.csv(inFile$datapath)
    }
    
    
  })
  
  
  
  # ~ DB 관리 : 미리보기 datatable -----------------------------------
  
  output$modalpreview <- renderDT({
    input$add_db_ok
    input$DB_TYPE
    
    # data_preview <- data.frame()
    data_preview <<- dataset$data 
    
    posixt <- which(lapply(data_preview, class) %>% data.frame() %>% .[1,] %>% unlist() == 'POSIXct')
    
    if(length(posixt) == 0){
      
    } else{
      
      data_preview[,posixt] <- lapply(data_preview[,posixt], function(x) as.POSIXct(x, format = "%d-%b-%Y %H:%M:%OS")) %>% data.frame()
      
      data_preview[,posixt] <- lapply(data_preview[,posixt], function(x) as.character(x)) %>% data.frame()
      
      if('측정시간' %in% colnames(data_preview[,posixt])){
        data_preview[,which(colnames(data_preview) == '측정시간')] <-
          substr(data_preview[,which(colnames(data_preview) == '측정시간')] %>% unlist(), 12, 16) %>% data.frame()
      }
      
    }
    
    data_preview %>% 
      head(30) %>% 
      datatable(
        options = list(lengthChange = FALSE,
                       scrollX = TRUE,
                       autoWidth = TRUE,
                       columnDefs = list(
                         list(width = '80px', targets = "_all"),
                         list(className = "dt-center", targets = "_all")
                       )
        )
      )
    
    
  })
  
  
  # ~ DB 관리 : 모니터링 datatable  -----------------------------------------------------------------
  
  output$monitor_db <- renderDT({
    input$del_ok
    input$add_db_ok
    input$db_search
    
    if(nrow(db_list) == 0){
      
    }else{
      
      if(input$db_search == '0'){
        
        DT_db_list <<- db_list[, c(2:7)]
        
        DT_db_list %>% datatable(options=list(columnDefs = list(list(visible=FALSE, targets=c(6))),
                                              info = FALSE,
                                              lengthChange = FALSE),
                                 rownames = TRUE,
                                 selection ="none",
                                 escape = FALSE)
        
      } else {
        
        DT_db_list %>% datatable(options=list(columnDefs = list(list(visible=FALSE, targets=c(6))),
                                              info = FALSE,
                                              lengthChange = FALSE),
                                 rownames = TRUE,
                                 selection ="none",
                                 escape = FALSE)
      }
    }
    
  })
  
  # ~ DB 관리 : search 버튼 -------------------------------------------------------------------------
  
  observeEvent(input$db_search, {
    if(nrow(db_list) == 0){
      
      showModal(
        modalDialog(
          title = '검색 할 DB가 없습니다.',
          
          size = "s",
          easyClose = TRUE,
          footer = actionButton("add_db_no", "닫기")
        )
      )
      
    } else {
      if(input$sel1 == '전체'){
        
        DT_db_list <<- db_list[, c(2:7)]
        
        if(input$sel_date2 == '전체'){
          
          if(input$search == ''){
            ## 전체 & 자료원명 x
            
          } else {
            ## 전체 & 자료원명 o
            
            DT_db_list <<- DT_db_list[grep(pattern = input$search, DT_db_list$자료원명, ignore.case = T) %>% unlist() %>% unique(), ]
            
          }
          
        }else {
          
          if(input$search == ''){
            
            DT_db_list <<- DT_db_list[(substr(DT_db_list$수집년도, 1, 4) <= input$sel_date2) & (substr(DT_db_list$수집년도, 6, 9) >= input$sel_date2), ]
            
          }else{
            
            DT_db_list <<- DT_db_list[(substr(DT_db_list$수집년도, 1, 4) <= input$sel_date2) & (substr(DT_db_list$수집년도, 6, 9) >= input$sel_date2), ]
            DT_db_list <<- DT_db_list[grep(pattern = input$search, DT_db_list$자료원명, ignore.case = T) %>% unlist() %>% unique(), ]
            
          }
        }
        
      } else{
        
        DT_db_list <<- db_list[, c(2:7)] %>% filter(`DB구분` == input$sel1)
        
        if(input$sel_date2 == '전체'){
          
          
          if(input$search == ''){
            ## 전체 & 자료원명 x
            
          } else {
            ## 전체 & 자료원명 o
            
            DT_db_list <<- DT_db_list[grep(pattern = input$search, DT_db_list$자료원명, ignore.case = T) %>% unlist() %>% unique(), ]          
          }
          
        }else {
          
          if(input$search == ''){
            
            DT_db_list <<- DT_db_list[(substr(DT_db_list$수집년도, 1, 4) <= input$sel_date2) & (substr(DT_db_list$수집년도, 6, 9) >= input$sel_date2), ]
            
          }else{
            
            DT_db_list <<- DT_db_list[(substr(DT_db_list$수집년도, 1, 4) <= input$sel_date2) & (substr(DT_db_list$수집년도, 6, 9) >= input$sel_date2), ]
            DT_db_list <<- DT_db_list[grep(pattern = input$search, DT_db_list$자료원명, ignore.case = T) %>% unlist() %>% unique(), ]
            
          }
          
          
        }
      }
      
      if(input$sel_admin == '전체'){
        
      } else {
        DT_db_list <<- DT_db_list %>% filter(`수행기관` == input$sel_admin)
      }
    } 
  })
  
  
  # ~ DB 관리 : 검색어 count -------------------------------------------------------------------------
  
  output$monitordb_num <- renderText({
    input$sel1
    input$db_search
    input$del_ok
    input$add_db_ok
    
    length(lapply(DT_db_list, function(x) {grep(pattern = db_nrow, x = x, ignore.case = T)}) %>% unlist() %>% unique())
    
  })
  
  
  
  # ~ DB 관리 : click 팝업창  -----------------------------------------------------------------
  
  
  observeEvent(input$DTClick, {
    
    con <- dbConnect(SQLite(), dbname = "K_APEX.db")
    click_db <<- dbReadTable(con,  paste0(db_list$DB구분[which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick[1])], "_", db_list$자료원ID[which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick[1])]))
    dbDisconnect(con)
    
    db_num <<- which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick[1])
    
    if(db_list[db_num,2] == '실내농도' | db_list[db_num,2] == '실외농도'){
      
      if(db_list[db_num,2] == '실내농도'){
        
        colnames(click_db) <<- c(in_col, indoor_codebook_colname[-1])
        
      }else if(db_list[db_num,2] == '실외농도'){
        
        colnames(click_db) <<- c(out_col, outdoor_codebook_colname[-1])
      }   
      
      showModal(
        modalDialog(
          title = 'DB 정보 확인',
          
          div(style="display : inline-block; float : right", actionButton(inputId = "del_db", "삭제")),
          
          br(),
          br(),
          
          div(DTOutput("modal_name")),
          
          br(),
          
          tabsetPanel(
            tabPanel(id = 'basic_info',
                     "기본정보", 
                     br(),
                     DTOutput("modal_info"),
                     fluidRow(
                       div(style="display : inline-block;", h5("1) ")),
                       div(style="display : inline-block;", h5("단계")),
                       div(style="display : inline-block; margin-top: -3px;",
                           id = 'dropdown2',
                           dropdownButton(
                             DTOutput("info_tier2"),
                             circle = TRUE,
                             icon = fontawesome::fa("info-circle"),
                             tooltip = tooltipOptions(title = "Click"),
                             margin = '20px',
                             width = '750px'
                           ))
                     ),
                     fluidRow(id = 'text',
                              div(style="display : inline-block;", h5("2) ")),
                              div(style = "display : inline-block;", textOutput('tooltip'))
                     )
            ), 
            tabPanel(id = 'measure_info',
                     "측정인자", 
                     br(),
                     div(style = "display : inline-block;  ", h5('총 ')), 
                     div(style = "display : inline-block; color:red;", textOutput('ex_measure_num')), 
                     div(style = "display : inline-block; ", h5(' 건 검색되었습니다.')),
                     DTOutput("modal_measure")), 
            tabPanel(id = 'detail_info',
                     "세부항목", 
                     br(),
                     div(style = "display : inline-block;  ", h5('총 ')), 
                     div(style = "display : inline-block; color:red;", textOutput('ex_detail_num')), 
                     div(style = "display : inline-block; ", h5(' 건 검색되었습니다.')),
                     DTOutput("modal_detail")),
            tabPanel(id = 'raw_data',
                     "원자료", 
                     br(),
                     div(style = ' white-space : nowrap;',
                         DTOutput("modalContent")))
            
          ),
          
          size = "l",
          easyClose = TRUE,
          footer = actionButton("add_db_no", "목록")
        )
      )
      
    }else if(db_list[db_num,2] == '시간활동'){
      
      colnames(click_db) <<- ta_col
      
      showModal(
        modalDialog(
          title = 'DB 정보 확인',
          
          div(style="display : inline-block; float : right", actionButton(inputId = "del_db", "삭제")),
          
          br(),
          br(),
          
          div(DTOutput("modal_name")),
          
          br(),
          
          tabsetPanel(
            
            tabPanel(id = 'basic_info',
                     "기본정보", 
                     br(),
                     DTOutput("modal_info"),
                     fluidRow(
                       div(style="display : inline-block;", h5("1) ")),
                       div(style="display : inline-block;", h5("단계")),
                       div(style="display : inline-block; margin-top: -3px;",
                           id = 'dropdown2',
                           dropdownButton(
                             DTOutput("info_tier2"),
                             circle = TRUE,
                             icon = fontawesome::fa("info-circle"),
                             tooltip = tooltipOptions(title = "Click"),
                             margin = '20px',
                             width = '750px'
                           ))
                     ),
                     fluidRow(
                       div(style="display : inline-block;", h5("2) ")),
                       div(style = "display : inline-block;", textOutput('tooltip'))
                     )
            ), 
            tabPanel(id = 'detail_info',
                     "세부항목", 
                     br(),
                     div(style = "display : inline-block;  ", h5('총 ')), 
                     div(style = "display : inline-block; color:red;", textOutput('time_detail_num')), 
                     div(style = "display : inline-block; ", h5(' 건 검색되었습니다.')),
                     DTOutput("modal_detail")),
            tabPanel(id = 'raw_data',
                     "원자료", 
                     br(),
                     div(style = ' white-space : nowrap;',
                         DTOutput("modalContent")))
            
          ),
          
          size = "l",
          easyClose = TRUE,
          footer = actionButton("add_db_no", "닫기")
        )
      )
      
    }
    
  })
  
  
  output$modalContent <- renderDT({
    input$DTClick
    
    click_db %>% datatable(options = list(
      scrollX = TRUE,
      columnDefs = list(list(width = '500px', targets = "_all")))) 
    
  })
  
  # ~ DB 관리 : 삭제 --------------------------------------------------------------
  
  observeEvent(input$del_db, {
    showModal(
      
      modalDialog(
        title = paste0(db_list$찐_자료원명[which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick[1])], "를 삭제하시겠습니까?"),
        
        
        size = "m",
        easyClose = FALSE,
        footer = tagList(actionButton(inputId = "del_ok", "예"),                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
                         actionButton(inputId = "del_no", "아니오"))
      )
    )
    
    
  })
  
  observeEvent(input$del_ok, {
    
    
    con <- dbConnect(SQLite(), dbname = "K_APEX.db")
    
    dbRemoveTable(con, paste0(db_list$DB구분[which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick[1])], "_", db_list$자료원ID[which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick[1])]))
    
    dbDisconnect(con)
    
    db_list <<- db_list[-which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick[1]), ]
    db_list2 <<- db_list2[-which(as.numeric(substr(db_list2$자료원ID, 3, nchar(db_list2$자료원ID))) == input$DTClick[1]), ]
    DT_db_list <<- db_list[-which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick[1]), ]
    DT_db_list2 <<- db_list2[-which(as.numeric(substr(db_list2$자료원ID, 3, nchar(db_list2$자료원ID))) == input$DTClick[1]), ]
    
    removeModal()
    
    if(nrow(db_list) == 0){
      
      updatePickerInput(session = session, inputId = 'sel_date',
                        choices = c(''))
      
      updatePickerInput(session = session, inputId = 'sel_date2',
                        choices = c(''))
      
      updatePickerInput(session = session, inputId = 'sel_admin',
                        choices = c(''))
      
      updatePickerInput(session = session, inputId = 'sel_admin2',
                        choices = c(''))
      
    } else {
      
      updatePickerInput(session = session, inputId = 'sel_date',
                        choices = c('전체', min(substr(db_list$수집년도, 1, 4) %>% as.numeric()):max(substr(db_list$수집년도, 6, 9) %>% as.numeric()))
                        
      )
      
      updatePickerInput(session = session, inputId = 'sel_date2',
                        choices = c('전체', min(substr(db_list$수집년도, 1, 4) %>% as.numeric()):max(substr(db_list$수집년도, 6, 9) %>% as.numeric()))
                        
      )
      
      updatePickerInput(session = session, inputId = 'sel_admin',
                        choices = c('전체', db_list$수행기관)
                        
      )
      
      updatePickerInput(session = session, inputId = 'sel_admin2',
                        choices = c('전체', db_list$수행기관)
                        
      )
      
    }
    
  })
  
  
  
  observeEvent(input$del_no, {
    removeModal()
  })
  
  # ~~ 신규 등록 : 양식 다운로드 -----------------------------------------------
  
  output$in_form <- downloadHandler(
    filename = function() {
      paste("실내농도_업로드양식.xlsx", sep = "")
    },
    
    content = function(file) {
      writexl::write_xlsx(list(
        "실내농도" = in_form1
      ), file)
    }
    
  )
  
  output$out_form <- downloadHandler(
    filename = function() {
      paste("실외농도_업로드양식.xlsx", sep = "")
    },
    
    content = function(file) {
      writexl::write_xlsx(list(
        "실외농도" = out_form1
      ), file)
    }
    
  )
  
  output$ta_form <- downloadHandler(
    filename = function() {
      paste("시간활동_업로드양식.xlsx", sep = "")
    },
    
    content = function(file) {
      writexl::write_xlsx(list(
        "시간활동" = ta_form1,
        "코드북" = ta_form2
      ), file)
    }
    
  )
  
  # DB 조회 ----------------------------
  
  
  # ~ DB 조회 : 모니터링 datatable --------------------------------------------------
  
  
  output$monitor_db2 <- renderDT({
    input$del_ok
    input$add_db_ok
    input$db_search2
    
    
    if(nrow(db_list) == 0){
      
    }else{
      
      if(input$db_search2 == '0'){
        
        DT_db_list2 <<- db_list2[, c(2:7)]
        
        DT_db_list2 %>% datatable(options=list(columnDefs = list(list(visible=FALSE, targets=c(6))),
                                               info = FALSE,
                                               lengthChange = FALSE),
                                  rownames = TRUE,
                                  selection ="none",
                                  escape = FALSE)
        
      } else {
        
        DT_db_list2 %>% datatable(options=list(columnDefs = list(list(visible=FALSE, targets=c(6))),
                                               info = FALSE,
                                               lengthChange = FALSE),
                                  rownames = TRUE,
                                  selection ="none",
                                  escape = FALSE)
      }
    }
    
  })
  
  
  # ~ DB 조회 : search 버튼 -------------------------------------------------------------------------
  
  observeEvent(input$db_search2, {
    if(nrow(db_list) == 0){
      
      showModal(
        modalDialog(
          title = '검색 할 DB가 없습니다.',
          
          size = "s",
          easyClose = TRUE,
          footer = actionButton("add_db_no", "닫기")
        )
      )
      
    } else {
      
      if(input$sel2 == '전체'){
        
        DT_db_list2 <<- db_list2[, c(2:7)] 
        
        if(input$sel_date == '전체'){
          
          if(input$search2 == ''){
            ## 전체 & 자료원명 x  
            
          } else {
            ## 전체 & 자료원명 o
            
            DT_db_list2 <<- DT_db_list2[grep(pattern = input$search2, DT_db_list2$자료원명, ignore.case = T) %>% unlist() %>% unique(), ]
          }
          
        }else {
          
          if(input$search2 == ''){
            
            DT_db_list2 <<- DT_db_list2[(substr(DT_db_list2$수집년도, 1, 4) <= input$sel_date) & (substr(DT_db_list2$수집년도, 6, 9) >= input$sel_date), ]          
          }else{
            
            DT_db_list2 <<- DT_db_list2[(substr(DT_db_list2$수집년도, 1, 4) <= input$sel_date) & (substr(DT_db_list2$수집년도, 6, 9) >= input$sel_date), ]
            DT_db_list2 <<- DT_db_list2[grep(pattern = input$search2, DT_db_list2$자료원명, ignore.case = T) %>% unlist() %>% unique(), ]
            
          }
        } 
        
      } else{
        
        DT_db_list2 <<- db_list2[, c(2:7)] %>% filter(`DB구분` == input$sel2)
        
        if(input$sel_date == '전체'){
          
          
          if(input$search2 == ''){
            ## 전체 & 자료원명 x  
            
          } else {
            ## 전체 & 자료원명 o
            
            DT_db_list2 <<- DT_db_list2[grep(pattern = input$search2, DT_db_list2$자료원명, ignore.case = T) %>% unlist() %>% unique(), ]
            
          }
          
        }else {
          
          if(input$search2 == ''){
            
            DT_db_list2 <<- DT_db_list2[(substr(DT_db_list2$수집년도, 1, 4) <= input$sel_date) & (substr(DT_db_list2$수집년도, 6, 9) >= input$sel_date), ]
            
          }else{
            
            DT_db_list2 <<- DT_db_list2[(substr(DT_db_list2$수집년도, 1, 4) <= input$sel_date) & (substr(DT_db_list2$수집년도, 6, 9) >= input$sel_date), ]
            DT_db_list2 <<- DT_db_list2[grep(pattern = input$search2, DT_db_list2$자료원명, ignore.case = T) %>% unlist() %>% unique(), ]
            
          }
          
          
        } 
      }
      
      if(input$sel_admin2 == '전체'){
        
      } else {
        DT_db_list2 <<- DT_db_list2 %>% filter(`수행기관` == input$sel_admin2)
        
      }
    } 
  })
  
  
  # ~ DB 조회 : 검색어 count -------------------------------------------------------------------------
  
  output$monitordb2_num <- renderText({
    input$sel2
    input$db_search2
    input$del_ok
    input$add_db_ok
    
    length(lapply(DT_db_list2, function(x) {grep(pattern = db_nrow, x = x, ignore.case = T)}) %>% unlist() %>% unique())
    
  })
  
  
  
  # ~ DB 조회 : click 팝업창  -----------------------------------------------------------------
  
  observeEvent(input$DTClick2, {
    
    con <- dbConnect(SQLite(), dbname = "K_APEX.db")
    click_db <<- dbReadTable(con,  paste0(db_list$DB구분[which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick2[1])], "_", db_list$자료원ID[which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick2[1])]))
    dbDisconnect(con)
    
    db_num <<- which(as.numeric(substr(db_list$자료원ID, 3, nchar(db_list$자료원ID))) == input$DTClick2[1])
    
    if(db_list[db_num,2] == '실내농도' | db_list[db_num,2] == '실외농도'){
      
      if(db_list[db_num,2] == '실내농도'){
        
        colnames(click_db) <<- c(in_col, indoor_codebook_colname[-1])
        
      }else if(db_list[db_num,2] == '실외농도'){
        
        colnames(click_db) <<- c(out_col, outdoor_codebook_colname[-1])
      }   
      
      showModal(
        modalDialog(
          title = 'DB 정보 확인',
          
          br(),
          br(),
          
          div(DTOutput("modal_name")),
          
          br(),
          
          tabsetPanel(
            tabPanel("기본정보", 
                     br(),
                     DTOutput("modal_info"),
                     fluidRow(
                       div(style="display : inline-block;", h5("1) ")),
                       div(style="display : inline-block;", h5("단계")),
                       div(style="display : inline-block; margin-top: -3px;",
                           id = 'dropdown2',
                           dropdownButton(
                             DTOutput("info_tier2"),
                             circle = TRUE,
                             icon = fontawesome::fa("info-circle"),
                             tooltip = tooltipOptions(title = "Click"),
                             margin = '20px',
                             width = '750px'
                           ))
                     ),
                     fluidRow(
                       div(style="display : inline-block;", h5("2) ")),
                       div(style = "display : inline-block;", textOutput('tooltip'))
                     )
            ), 
            tabPanel("측정인자", 
                     br(),
                     div(style = "display : inline-block;  ", h5('총 ')), 
                     div(style = "display : inline-block; color:red;", textOutput('ex_measure_num')), 
                     div(style = "display : inline-block; ", h5(' 건 검색되었습니다.')),
                     DTOutput("modal_measure")), 
            tabPanel("세부항목", 
                     br(),
                     div(style = "display : inline-block;  ", h5('총 ')), 
                     div(style = "display : inline-block; color:red;", textOutput('ex_detail_num')), 
                     div(style = "display : inline-block; ", h5(' 건 검색되었습니다.')),
                     DTOutput("modal_detail"))
            
          ),
          
          size = "l",
          easyClose = TRUE,
          footer = actionButton("add_db_no", "목록")
        )
      )
      
    }else if(db_list[db_num,2] == '시간활동'){
      
      colnames(click_db) <<- ta_col
      
      showModal(
        modalDialog(
          title = 'DB 정보 확인',
          
          br(),
          br(),
          
          div(DTOutput("modal_name")),
          
          br(),
          
          tabsetPanel(
            tabPanel("기본정보", 
                     br(),
                     DTOutput("modal_info"),
                     fluidRow(
                       div(style="display : inline-block;", h5("1) ")),
                       div(style="display : inline-block;", h5("단계")),
                       div(style="display : inline-block; margin-top: -3px;",
                           id = 'dropdown2',
                           dropdownButton(
                             DTOutput("info_tier2"),
                             circle = TRUE,
                             icon = fontawesome::fa("info-circle"),
                             tooltip = tooltipOptions(title = "Click"),
                             margin = '20px',
                             width = '750px'
                           ))
                     ),
                     fluidRow(
                       div(style="display : inline-block;", h5("2) ")),
                       div(style = "display : inline-block;", textOutput('tooltip'))
                     )
            ), 
            tabPanel("세부항목", 
                     br(),
                     div(style = "display : inline-block;  ", h5('총 ')), 
                     div(style = "display : inline-block; color:red;", textOutput('time_detail_num')), 
                     div(style = "display : inline-block; ", h5(' 건 검색되었습니다.')),
                     DTOutput("modal_detail"))
            
          ),
          
          size = "l",
          easyClose = TRUE,
          footer = actionButton("add_db_no", "목록")
        )
      )
      
    }
    
  })
  
  
  # ~~ DB click 팝업창 : 자료원명 -----------------------------------------------------------------
  
  output$modal_name  <- renderDT({
    input$DTClick2
    input$DTClick
    
    data.table('자료원명',
               db_list$찐_자료원명[db_num]) %>%
      datatable(class = 'cell-border stripe',
                options = list(
                  columnDefs = list(list(width = '100px', targets = c(1))),
                  searching = FALSE,
                  lengthChange = FALSE,
                  info = FALSE,
                  dom = 'ft',
                  headerCallback = JS(
                    "function(thead, data, start, end, display){",
                    "  $(thead).remove();",
                    "}")
                ),
                rownames = FALSE) %>%
      formatStyle(c(1), backgroundColor = "#f9f9f9") %>%
      formatStyle(c(2), backgroundColor = "#ffffff")
  })
  
  # ~~ DB click 팝업창 : 기본정보 -----------------------------------------------------------------
  
  output$modal_info  <- renderDT({
    input$DTClick2
    input$DTClick
    
    # DB구분, 수집방법, 수집기관, 수집기간, 수행부서, 단계
    basic_dt <- db_list[db_num, c(2,4,6,8,9,10)]
    colnames(basic_dt)[6] <- '단계    1)'
    
    if(db_list[db_num,2] == '실내농도'){
      data.table(basic_dt,
                 '모니터링 건수' = comma((as.numeric(click_db[,1] %>% unique() %>% length()))*5, format = 'd'),
                 '측정소 수' = comma(click_db[,1] %>% unique() %>% length(), format = 'd'),
                 '측정인자 수' = 5,
                 '세부항목 건수    2)' = click_db[,11] %>% unique() %>% length(),
                 db_list[db_num, 11]) %>% t() %>%
        datatable(class = 'cell-border stripe',
                  options = list(
                    pageLength = 11,
                    searching = FALSE,
                    lengthChange = FALSE,
                    info = FALSE,
                    dom = 'ft',
                    headerCallback = JS(
                      "function(thead, data, start, end, display){",
                      "  $(thead).remove();",
                      "}")
                  )
        ) %>%
        formatStyle(c(0), backgroundColor = "#f9f9f9") %>%
        formatStyle(c(1), backgroundColor = "#ffffff")
      
    } else if(db_list[db_num,2] == '실외농도'){
      
      data.table(basic_dt,
                 '모니터링 건수' = comma((as.numeric(click_db[,1] %>% unique() %>% length()))*6, format = 'd'),
                 '측정소 수' = comma(click_db[,1] %>% unique() %>% length(), format = 'd'),
                 '측정인자 수' = 6,
                 '세부항목 건수    2)' = click_db[,11] %>% unique() %>% length(),
                 db_list[db_num, 11]) %>% t() %>%
        datatable(class = 'cell-border stripe',
                  options = list(
                    pageLength = 11,
                    searching = FALSE,
                    lengthChange = FALSE,
                    info = FALSE,
                    dom = 'ft',
                    headerCallback = JS(
                      "function(thead, data, start, end, display){",
                      "  $(thead).remove();",
                      "}")
                  )
        ) %>%
        formatStyle(c(0), backgroundColor = "#f9f9f9") %>%
        formatStyle(c(1), backgroundColor = "#ffffff")
      
    } else if(db_list[db_num,2] == '시간활동'){
      
      data.table(basic_dt,
                 '모니터링 건수' = comma((click_db[,1] %>% unique() %>% length())*144, format = 'd'),
                 '응답자 수' = comma(click_db[,1] %>% unique() %>% length(), format = 'd'),
                 '측정인자 수' = '-',
                 '세부항목 건수    2)' = click_db[,7] %>% unique() %>% length(),
                 db_list[db_num, 11]) %>% t() %>%
        datatable(class = 'cell-border stripe',
                  options = list(
                    pageLength = 11,
                    searching = FALSE,
                    lengthChange = FALSE,
                    info = FALSE,
                    dom = 'ft',
                    headerCallback = JS(
                      "function(thead, data, start, end, display){",
                      "  $(thead).remove();",
                      "}")
                  )
        ) %>%
        formatStyle(c(0), backgroundColor = "#f9f9f9") %>%
        formatStyle(c(1), backgroundColor = "#ffffff")
    }
    
    
  })
  
  # ~~~ 기본정보 : 세부항목 tootip-----------------------------------------------------------------
  
  output$tooltip   <- renderText({
    input$DTClick2
    input$DTClick
    
    if(db_list[db_num,2] == '실내농도'){
      '세부항목: 측정 대상 장소/이동수단 건수'
    }else if (db_list[db_num,2] == '실외농도'){
      '세부항목: 측정 대상 지역(시군구)'
    }else if (db_list[db_num,2] == '시간활동'){
      '세부항목: 조사 대상 지역(시군구)'
    }
    
  })
  
  # ~~ 기본정보 : 단계 info_tier2 tootip ------------------------------------------------ 
  
  output$info_tier2 <- renderDT({
    tier %>% datatable(options = list(searching = FALSE,
                                      lengthChange = FALSE,
                                      info = FALSE,
                                      dom = 'ft'),
                       rownames = FALSE)
  })
  
  
  # ~~ DB click 팝업창 : 측정인자 -----------------------------------------------------------------
  
  output$modal_measure  <- renderDT({
    input$DTClick2
    input$DTClick
    
    if(db_list[db_num,2] == '실내농도'){
      
      output$ex_measure_num  <- renderText({ 5 })
      
      measure_in[,6] <- colSums(!is.na(click_db[,c(4:8)]))
      
      measure_in %>% datatable(options = list(searching = FALSE,
                                              lengthChange = FALSE,
                                              info = FALSE,
                                              dom = 'ft'),
                               rownames = FALSE) %>% 
        formatCurrency(6,currency = "", interval = 3, mark = ",", digits = 0)
      
    }else if(db_list[db_num,2] == '실외농도'){
      
      output$ex_measure_num  <- renderText({ 6 })
      
      measure_out[,6] <- colSums(!is.na(click_db[,c(4:9)]))
      
      measure_out %>% datatable(options = list(searching = FALSE,
                                               lengthChange = FALSE,
                                               info = FALSE,
                                               dom = 'ft'),
                                rownames = FALSE) %>% 
        formatCurrency(6,currency = "", interval = 3, mark = ",", digits = 0)
    }
    
  })
  
  # ~~ DB click 팝업창 : 세부항목 -----------------------------------------------------------------
  
  output$modal_detail  <- renderDT({
    input$DTClick2
    input$DTClick
    
    if(db_list[db_num,2] == '실외농도' | db_list[db_num,2] == '시간활동'){
      
      
      
      detail_dt <- left_join(data.frame("No." = 1:length(click_db['시군구 코드'] %>% unique() %>% unlist()),
                                        'CD' = sort(click_db['시군구 코드'] %>% unique() %>% unlist())) ,
                             column_def %>% filter(MODULE_DESC == '시군구코드') %>% select(CD, CD_DESC_KO))
      
      detail_dt <- left_join(detail_dt, data.frame(table(click_db['시군구 코드'])),
                             by = c('CD' = 'Var1'))
      
      
      detail_dt %>% datatable(options = list(info = FALSE,
                                             lengthChange = FALSE),
                              rownames = FALSE,
                              colnames=c("No.", "시군구 코드", "시군구 명", '구축현황(건)')) %>% 
        formatCurrency(4, currency = "", interval = 3, mark = ",", digits = 0)
      
      
      
      
    }else if(db_list[db_num,2] == '실내농도'){
      
      
      
      detail_dt <- left_join(data.frame("No." = 1:length(click_db['장소/이동수단 코드'] %>% unique() %>% unlist()),
                                        'CD' = click_db['장소/이동수단 코드'] %>% unique() %>% unlist()),
                             column_def %>% filter(MODULE_DESC == '장소/이동수단') %>% select(CD, CD_DESC_KO))
      
      detail_dt <- left_join(detail_dt, data.frame(table(click_db['장소/이동수단 코드'])),
                             by = c('CD' = 'Var1'))
      
      detail_dt %>% datatable(options = list(info = FALSE,
                                             lengthChange = FALSE),
                              rownames = FALSE,
                              colnames=c("No.", "장소/이동수단코드", "장소/이동수단명", '구축현황(건)')) %>% 
        formatCurrency(4, currency = "", interval = 3, mark = ",", digits = 0)
      
      
      # all_x <- click_db[,1:17]
      #
      # where <- column_def %>% filter(MODULE_DESC == '장소/이동수단') %>% .[,c(3,23)]
      #
      # for(i in (where$CD %>% as.character())){
      #
      #   x <- rowSums(click_db[, 18:161] == i, na.rm = T) %>% data.frame()
      #   all_x <- cbind(all_x, x)
      # }
      #
      # colnames(all_x) <- c(ta_col[1:17], (where$REAL_CD_DESC_RPRS %>% as.character()))
      #
      # # all_x$na <- 144-rowSums(all_x[,18:39])
      # all_x[,18:39] <- all_x[,18:39]*10
      # click_db <<- all_x
      
    }
    
  })
  
  # ~~~ 세부항목 : 실외 / 실내 count -----------------------------------------------------------------
  
  output$ex_detail_num   <- renderText({
    input$DTClick2
    input$DTClick
    
    if(db_list[db_num,2] == '실외농도'){
      
      length(click_db['시군구 코드'] %>% unique() %>% unlist())
      
    }else if (db_list[db_num,2] == '실내농도'){
      
      length(click_db['장소/이동수단 코드'] %>% unique() %>% unlist())
    }
    
  })
  
  # ~~~ 세부항목 : 시간 count -----------------------------------------------------------------
  
  output$time_detail_num  <- renderText({
    input$DTClick2
    input$DTClick
    
    length(click_db['시군구 코드'] %>% unique() %>% unlist())
    
  })
  
  
  # ~ DB 조회: 목록 다운로드 ----------------------------------------------------------
  
  
  output$list_download <- downloadHandler(
    filename = function() {
      paste("DB 목록.xlsx", sep = "")
    }, 
    
    content = function(file) {
      
      DT_db_list2[,2] <- DT_db_list2[,6]
      
      writexl::write_xlsx(DT_db_list2[,-6], path = file)
    }
    
  )
  
  output$list_download2 <- downloadHandler(
    filename = function() {
      paste("DB 목록.xlsx", sep = "")
    }, 
    
    content = function(file) {
      
      DT_db_list[,2] <- DT_db_list[,6]
      
      writexl::write_xlsx(DT_db_list[,-6], path = file)
    }
    
  )
  
  
  # 로고 onclick  --------------------------------------------------------------
  
  onclick("home_logo",{
    updateTabsetPanel(session, "inTabset",
                      selected = "home")
  })
  
  # UI 변경 -------------------------------------------------------------------
  
  output$style_tag <- renderUI({
    input$inTabset
    
    if(input$inTabset=='home'){
      
      return(tags$style(HTML('body{overflow:auto; background-image: url("KAPEX/main_visual01.jpg") ; background-repeat: no-repeat;}')))
      
    } else {
      
      return(tags$style(HTML('body{overflow:auto; background-image: url("KAPEX_CSS/images/base.jpg") ; background-repeat: no-repeat;}')))
      
    }
    
  })
  
  # 시스템 종료 ------------------------------------------------------------------
  
  onclick("close_button",{
    
    showModal(
      modalDialog(
        title = '시스템 종료','종료하시겠습니까?',
        
        size = 's',
        footer =  tagList(actionButton(inputId = 'close_btn','확인'),
                          actionButton(inputId = 'add_db_no','취소')),
        easyClose = TRUE
        
      )
    )
    
  })
  
  onclick('close_btn',{
    
    stopApp()
    js$closeWindow()
    
  })
  
  
}

# save.image('0620.RData')
runApp(list(ui = ui, server = server), host="192.168.0.91", port=8088, launch.browser = TRUE)
# runApp(list(ui = ui, server = server), launch.browser = TRUE)

