# Rainbow
rainbow project

환자 상태 분석툴
재활환자들을 대상으로 설문조사를 통해 얻은 삶의 질 지표와  실험을 통해 측정한 환자의 활동량을 환자들의 나이군, 질병, 기간별로 비교하는 툴이다.
환자의 삶의 질 지표는 EQ-5D test와 SF-36 test를 통해서 설문조사한 것이고 환자의 활동량은 환자들에게 스마트밴드 착용을 통해 측정한 특정 기간 동안의 걸음수로 나타낼 것이다. 

상단 탭 별 설명
Tab1. 종합지수(All)
사용목적 : 분석툴을 통해서 설문조사 내  세부 항목의 평균  지표, 스마트밴드를 통해 특정 기간 동안의 평균걸음수, 그리고 그 차이를 환자들의 나이군, 질병 별로 비교한다
사용방법
:‘Please select x-axis’에서는 가로축으로 보고 싶은 변수를 선택한다.
:x축은 age(나이대), Illness(질병군별) 중에서 고른다.
bar, line indexes 는 x축 기준에서 비교하고 싶은 건강 지표를
 bar 그래프(왼쪽 축으로 비교)와 line 그래프(오른쪽 축으로 비교)를 선택하는 란이다
  1.eq5d :  x축 그룹별 평균 EQ-5D 지표
  2.stride : x축 그룹별 평균 보행수 지표
  3.pr: x축 그룹별 평균 SF-36 PH()지표
  4.mr :  x축 그룹별 평균 SF-36 MR(심리적 제한) 지표
  5.vt :  x축 그룹별 평균 SF-36 VT(활력) 지표
  6.gh :x축 그룹별 평균 SF-36 GH(일반건강) 지표
:Period, from-to를 통해 bar indexes 와 lines indexes를 원하는 기간 동안의 총 평균 수치를 확인할 수 있다.

Tab2. 월 별 수치 비교(comparing value along to month)
사용목적 : 실험 시작 후 환자들의 특정 기간 동안의 평균 수치를 개월의 경과에 따라 비교한다
사용방법
:실험을 개월의 경과에 따라 기록하여 월마다  측정했다고 가정했을 때 월 별로 실험군의  평균 측정 수치를 비교하는 그래프이다.
:두 달을 각각 select month, select month2를 통해 선택하고 비교하고 싶은 지표를 index에서 선택하면 막대그래프를 통해 실험군의 월 별 평균 측정 수치가 어떤지 보여준다.
Tab3. 개인 지수 비교(Individual Strides)
사용목적 : 환자 개개인의 특정 기간 동안의 수치들을 개월의 경과에 따라 비교한다.
사용방법
:Period, from-to로 원하는 기간을 선택한다
:특정 개인을 Name에서 선택한 후, 원하는 지표를 index에서 선택하여 원하는 기간 동안의 수치를 비교할 수 있다.


Rstudio 설치

: 분석툴을 프로그래밍, 수정, shinyapps에 deploy하기 위해서 컴퓨터에 Rstudio가 설치되어 있어야 한다.
: https://www.rstudio.com/products/rstudio/download/ 에서 컴퓨터 버전에 따라 Rstudio program을 설치할 수 있다.


만든 프로그램을 shinyapps.io에 deploy하는 방법

: 만든 분석툴이 Rstudio에서만 되는 것이 아니라 웹에서 주소만 알면 툴을 사용할 수 있도록 하는 단계이다.
  1. http://www.shinyapps.io 에 접속하여 회원가입을 한다.
  2. 그 후 http://account.shinyapps.io 와 같이 앱의 주소를 설정한다.(account 부분에 자신이 원하는 이름을 넣으면 된다)
  3. 자신이 Rstudio에서 콘솔창에 다음과 같은 명령어들을 입력한다.
    install.packages(‘devtools’)
    devtools::install_github(‘rstudio/shinyapps’)
    shinyapps::setAccoutInfo(name=‘위에서 설정한 계정명’
                                           token=‘~’
                                           secret=‘~’)
      //shinyapps 홈페이지 안내창에서 name, token, secret에 할당되는 것들을 알려준다.
    library(shinyapps)
    shinyapps::deployApp(‘path/to/your/app’)
      //여기서 path/to/your/app이란 앱으로 만들고 싶은 server.R, ui.R 파일이 있는 폴더 경로를 입력하면 된다.    
      //보통 경로의 마지막 이름을 가지고 앱이름을 설정하는데, 마지막 이름이 짧다면 deploy 에러가 난다.
        그 경우 shinyapps::deployApp(‘path/to/your/app’, appname=‘~’) 으로 따로 앱이름을 설정해주면 된다.
  4. 한 번 shinyapps.io에 계정이 생성된 이후에는 rstudio에서 publish(스크립트창 우측 상단 run App 우측의 파란 아이콘) 하면 동일한 계정으로 deploy 된다.(단, 이 때 appname이 4글자 미만, 64글자 초과일 경우 validation error가 나므로 기준에 맞게 앱이름을 작성한다)

github에서 클론 생성 

:깃허브에서 데스크탑에 앱에 대한 내용을 클로닝하는 방법을 설명한다.
  1. github.com에 들어가 계정을 생성한 후 https://github.com/dsshin999/Rainbow를 인터넷 웹페이지에 검색하여 사용할 분석툴이      저장되어 있는 페이지로 간다. 
  2. 오른쪽 하단에 "Clone in Desktop" 을 클릭하여 github, repository 에 저장된 내용을 자신의 컴퓨터와 원격으로 공유할 있는       준비를 한다. 
  3. Github window/Mac 이 설치되어 있지 않는 경우 다운로드를 받는다. 
  4. Github 설치후 github계정을 등록하고 원하느 repository의 url 을 아까 "Clone in Desktop" 상단에 있는 
    "https://github.com/dsshin999/Rainbow.git" 를 복사하여 clone의 경로로 설정해주고 PC에 원격으로 공유할 수 있는 폴더를         생성, 그 경로를 목적지로 설정하면 Github저장소와 PC의 저장소가 연결이 된다.
  5. Github 프로그램의 PULL버튼 또는 Synch버튼으로 github.com에 있는 내용을 공유한다.

Rstudio 실행방법

: 깃허브에서 클로닝해서 데스크탑에 클로닝한 패키지에서 앱을 실행하는 방법을 설명한다.
  1. Rstudio를 실행하고 우측 하단 박스에서 files에서 클로닝한 폴더를 연다.
  2. 폴더에서 ~.Rproj라고 쓰여진 파일을 클릭하면 컴퓨터 Rstudio에 해당 프로젝트가 생성된다.
  3. R 폴더에 들어가 server.R 파일 혹은 ui.R 파일을 연다.
  4. 좌측 상단에 스크립트 창이 생기고 스크립트창의 우측 상단에 초록색 삼각형과 Run App 버튼이 있다.
    - Run App 하부 메뉴를 여는 작은 검은 삼각형을 클릭하면 앱을 어디에서 열 것인지 설정할 수 있다.
    - Run in Window : Rstudio에서 새 창을 열어 앱 실행
    - Run in Viewer Pane : 좌측 하단 Viewer창에서 실행
    - Run in External : 인터넷 창을 열어 즉, Rstudio 외부에서 실행
  5. 앱을 중지하고 싶으면 Rstudio의 좌측 하단 콘솔 창 우측 상단의 빨간 팔각형 stop버튼을 누른다
    - Run in Window로 실행한 경우 창을 닫으면 중지된다.
