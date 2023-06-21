# 금융통계 기말과제
웹 스크래핑 및 tidyverse를 사용한 R의 탐색적 데이터 분석 및 비교 주식 시장 분석 



## 삼성전자, 카카오, SK하이닉스, KT, 에스오일, 기아차의 날짜별 종가, 시가, 고가, 저가, 거래량, 외국인소진율 웹 스크랩핑
새로운 열 'Stock'을 만들고 각 주식의 영어명칭을 주입
이후 Master_Data 에 통합


## 월별 가격 시각화

 - ggplot 패키지를 사용하여 주식의 월별 및 일별 가격 패턴을 살펴 보았습니다. 이를 위해서는 주식별로 마스터 데이터 프레임을 그룹화했습니다.
 - 거래량을 100000으로 나누고 종가를 100으로 나눠주었습니다 
 - 이후 날짜를 년, 월, 일로 나누고 월별 종가 가격대를 점 산점도로 한 그래프로 통합하여 gif 형태로 나타냈습니다

![Price_Range](https://github.com/surlyban/my_repo2/assets/86463268/9f69f065-a180-4b36-9dd0-d6a7f800f6ce)


 - 이후 날짜를 기준으로 일일 종가 가격의 변화율을 점 산점도로 한 그래프에 통합된 형태로 나타냈습니다.


![일일 종가 가격](https://github.com/surlyban/my_repo2/assets/86463268/e18b9a5f-d3f4-4ce1-9fd9-2a366e5563c4)


## 거래량과 종가 사이의 관계 알아보기

 - 일반적으로 특정 날짜에 주가가 너무 급등 또는 급락하면 거래량이 증가하는 추세를 보입니다. 이 매개 변수는 예측 모델에 중요하고 생각되었습니다. 
 - 따라서 데이터에서 이 둘 사이의 관계를 파악하기 위한 분석도 해주었습니다.
   

![Quantity_Price](https://github.com/surlyban/my_repo2/assets/86463268/d6af9c68-9ac3-4c8e-9a8b-f63480421c83)


 - 주가의 추세를 파악할 수 있지만 월별 가격에서 명확하게 알 수 있는 것은 많지 않았습니다. 주가는 대체적으로 특정 시점에 큰 변동성을 보여주기도 하였습니다. 


## 시가 대비 고가 편차의 밀집도 분포 

 - 주간 단위로 가격이 어느 방향으로 얼마나 편중되는지 이해하기 위해 시가와 고가의 밀도 분포를 살펴 보았습니다.
 - 이를 통해 일일 거래에서 모든 주식의 가격 범위를 알 수 있습니다.


 - 이를 위해 mutate 함수를 사용하여 고가와 시가의 차이가 있는 새 열을 추가합니다. 
 - mutate 함수를 사용하여 낮은 가격과 높은 가격의 차이로 다른 새 열을 추가합니다. 
 - tidyverse 패키지의 "tq_transmute()" 함수를 사용하여 차이의 주간 평균을 계산합니다. 
 - ggplot에서 점 분포로 두 밀집도에 대한 그래프로 시각화했습니다.
   

![High_Low_distribution](https://github.com/surlyban/my_repo2/assets/86463268/cc425c88-2058-48e1-b814-0bea51a12901)

### 자동 상관관계 지연 관찰하기
 
 - 지연 연산자(백시프트 연산자라고도 함)는 "지연된" 값이 실제 시계열과 정렬되도록 시계열을 이동(오프셋)하는 함수입니다. 
 - 시차는 원하는 단위만큼 이동할 수 있으며, 단순히 백시프트의 길이를 제어합니다.

 - 여기서 "k"는 지연으로 표시됩니다. 180일 기간의 지연을 보고 주식이 어떻게 움직이는지 살펴 보겠습니다.

 - 계산 단계는 다음과 같습니다.

 - k 지연 기간 정의
 - 지연 기간에 대한 열 만들기
 - 지연에 대한 새 데이터 프레임을 만들어 주식별로 데이터를 그룹화합니다.
 - 새 데이터 프레임에 tq_mutate() 함수를 사용하여 lag.xts를 적용합니다.
 - 자동 보정 적용
