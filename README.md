# Rainbow
rainbow project

환자 상태 분석툴
: 재활환자들을 대상으로 설문조사를 통해 얻은 삶의 질 지표와  실험을 통해 측정한 환자의 활동량을 환자들의 나이군, 질병, 기간별로 비교하는 툴이다.
: 환자의 삶의 질 지표는 EQ-5D test와 SF-36 test를 통해서 설문조사한 것이고 환자의 활동량은 환자들에게 스마트밴드 착용을 통해 측정한 특정 기간 동안의 걸음수로 나타낼 것이다. 

상단 탭 별 설명
Tab1. 종합지수(All)
- 사용목적 : 분석툴을 통해서 설문조사의 전후 세부 항목, 전후 지표간 차이 와 스마트밴드 착용 전의 걸음수와 착용 후 특정 기간 동안의 걸음수, 그리고 그 차이를 환자들의 나이군, 질병 별로 비교한다
- 사용방법
: ‘Please select x-axis’에서는 가로축으로 보고 싶은 변수를 선택한다.
: x축은 age(나이대), Illness(질병군별) 중에서 고른다.
: indexes는 설정된 x축 기준에서 비교하고 싶은 건강 지표를 고르는 란이다
  - aver_bf.eq : (스마트밴드) 착용전 x축 그룹별 평균 EQ-5D 지표
  - aver_bf.gh : (스마트밴드) 착용전 x축 그룹별 평균 SF-36 GH(일반건강) 지표
  - aver_bf.mr : (스마트밴드) 착용전 x축 그룹별 평균 SF-36 MR(심리적 제한) 지표
  - aver_bf.vt : (스마트밴드) 착용전 x축 그룹별 평균 SF-36 VT(활력) 지표
  - aver_bf.st : (스마트밴드) 착용전 x축 그룹별 평균 보행수 지표
  - aver_af.eq : (스마트밴드) 착용후 x축 그룹별  평균 EQ-5D 지표
  - aver_af.gh : (스마트밴드) 착용후 x축 그룹별  평균 SF-36 GH(일반건강) 지표
  - aver_af.mr : (스마트밴드) 착용후 x축 그룹별  평균 SF-36 MR(심리적 제한) 지표
  - aver_af.vt : (스마트밴드) 착용후 x축 그룹별  평균 SF-36 VT(활력) 지표
  - aver_af.st : (스마트밴드) 착용후 x축 그룹별  평균 보행수 지표
  - age : 나이(y축 지표로 의미 없다)
  - char : 실험군인지 대조군인지 구분하는 기준(y축 지표로 의미없다)
  - diff.st : 스마트밴드 착용 전후의 보행수 차이를 x축 그룹별로 평균낸 값
  - diff.eq : 스마트밴드 착용 전후의 EQ-5D 값 차이를 x축 그룹별로 평균낸 값
  - diff.mr : 스마트밴드 착용 전후의 SF-36 MR(심리적 제한) 값 차이를 x축 그룹별로 평균낸 값
: Seperation은 None, Char로 구분된다.
  - 기본 값이 None이며 이 경우 비교군과 대조군이 구분없이 한꺼번에 그래프에 표시된다.
  - Char를 선택하면 비교군과 대조군 그래프를 비교할 수 있다.

Tab2. 평균 걸음수 비교(Strides along to month)
- 사용목적 : 실험 시작 후 환자들의 특정 기간동안의 평균 걸음수를 개월의 경과에 따라 비교한다
- 사용방법
: 실험을 12개월 동안 진행했으며 달마다 보행수를 측정했다고 가정했을 때 달별로 실험군의 보행수 평균을 비교하는 그래프이다.
: 두 달을 각각 선택하면 막대그래프로 실험군의 각 달별 보행수 평균이 어떤지 보여준다.

Tab3. 개인 걸음수(Individual Strides)
- 사용목적 : 환자 개개인의 특정 기간동안의 걸음수를 개월의 경과에 따라 비교한다.
- 사용방법
: 특정 개인의 보행수를 달 별로 비교할 수 있다.
: 두 달을 각각 선택하고, 사람 이름을 선택하면 막대그래프로 그 사람의 특정 두 달의 보행수를 보여준다.


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
