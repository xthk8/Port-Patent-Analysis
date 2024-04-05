# install.packages("tm")            # 텍스트 데이터의 로딩, 처리, 변환, 분석 및 시각화 등 수행
# install.packages("stringi")       # 문자열 처리 기능, ICU(International Components for Unicode) 라이브러리 기반 문자열 검색, 교체, 정렬, 인코딩 변환 등 수행 
# install.packages("topicmodels" )  # LDA 등의 모델을 사용하여 숨겨진 토픽 발견 및 분석에 유용 
# install.packages("igraph")        # 네트워크 분석(SNA)과 그래프 이론 -> 복잡한 네트워크 구조를 생성, 시각화, 분석 
# install.packages("lda")           # LDA 토픽 모델링에 초점을 맞춘 패키지
                                  # 다량의 텍스트 데이터 내에서 주제를 모델링하고 이해
# install.packages("lsa")           # (Latent Semantic Analysis), 잠재 의미 분석(LSA)를 적용하여 텍스트 데이터의 패턴과 구조를 분석
                                  # 문서 간의 유사성을 파악하거나, 텍스트 데이터의 차원을 축소
# install.packages("textstem")      # 텍스트 전처리 과정 중 하나인 어간 추출(stemming)과 표제어 추출(lemmatization)을 수행
                                  # 단어의 기본 형태를 찾아내어 텍스트 데이터의 분석효율화
# install.packages("readxl")        # Excel 파일(.xlsx, .xls)을 R로 불러오는 기능을 제공
# install.packages("writexl")       # R에서 생성한 데이터 프레임을 Excel 파일로 저장

library(tm)
library(stringi)
library(topicmodels)
library(igraph)
library(lda)
library(cluster)
library(NLP)
library(NbClust)
library(grid)
library(MASS)
library(slam)
library(lsa)
library(textstem)
library(readxl)
library(writexl)
library(dplyr)

setwd("C:/Users/USER/Desktop/DKU/2024-1 경영공학종합설계") # 컴퓨터 주소
summary_READ <- read_excel("C:/Users/USER/Desktop/DKU/2024-1 경영공학종합설계/항만 특허_(20030702-20230701)_미국.xls")
Data <- summary_READ[15]  # 열 이름을 변수에 저장 (데이터프레임)
head(Data)

Data=as.matrix(Data) #transform to matrix

#* doc_id =c(2303:1) # 셀 개수 변경 (2303~1의 숫자를 역순으로 저장)
doc_id =c(35410:1)
colnames(Data)=c("text") #열이름 바꾸기
Data= data.frame(doc_id = doc_id, text = Data , stringsAsFactors = FALSE) 
# stringsAsFactors = FALSE 옵션 : 문자열 데이터가 팩터(factor)로 자동 변환되는 것을 방지



### 텍스트 데이터 전처리
Data.cor=Corpus(DataframeSource(Data))  # Corpus 생성
# Corpus : 텍스트 분석을 위해 조직된 텍스트 모음
inspect(Data.cor[1])  # 문서 검사, 코퍼스 내 특정 문서의 내용을 표시
options(max.print = 9999999)  # R 콘솔에 표시할 수 있는 최대 출력량 설정



### 데이터 분석

# tm_map 함수
# 코퍼스의 각 문서에 대해 지정된 변환(transformations) 또는 정제(cleaning) 작업을 수행할 수 있게 함
# 기본 문법 : tm_map(x, FUN, ...)

#소문자변경
Data.cor <- tm_map(Data.cor, tolower)

#문장부호(구두점)제거
Data.cor <- tm_map(Data.cor, removePunctuation)

#숫자제거
Data.cor <- tm_map(Data.cor, removeNumbers)

#공백제거:tm_map()
Data.cor<-tm_map(Data.cor, stripWhitespace)

#띄어쓰기와 시제 제거
Data.cor <- tm_map(Data.cor, removeWords, stopwords("english"))

#Lemmatization (표제어 추출)
#lemmatize_words 함수 : 주어진 텍스트 데이터의 각 단어를 그 기본 형태(표제어)로 변환
Data.cor <- lemmatize_words(Data.cor)

# 불필요한 단어 제거
# 1. 제거할 단어 목록을 excludes 변수에 할당
excludes <- c('one', 'first', 'second', stopwords('english'))
# 2. tm_map 함수와 removeWords 옵션을 사용하여 excludes에 명시된 단어 제거
Data.cor <- tm_map(Data.cor, removeWords, excludes)
# 3. 추가적인 단어 정제
mywords<- c("device","least","output","cell","method","value")  # 제거 단어 설정
Data.cor<- tm_map(Data.cor, removeWords, mywords)

head(Data.cor)

### 키워드 추출

# 1. DocumentTermMatrix함수 -> 문서-용어 행렬(DTM) 생성
# control 옵션을 통해 가중치 계산 (TF-IDF) & 두 글자 이상의 단어 대상
kwdDTM_TF = DocumentTermMatrix(Data.cor, control=list(wordLengths=c(2,Inf), weighting = function(x) weightTfIdf(x, normalize = TRUE)))

# 2. 행렬 변환 (행 : 문서, 열 : 단어)
tdm_tf.matrix <- as.matrix(kwdTDM_TF)

# 3. 단어 빈도 계산 : 행렬의 각 열(단어)에 대한 합계를 계산
# = 각 단어의 총 TF-IDF 가중치를 의미
word.count.tf <- rowSums(tdm_tf.matrix)

# 4. 단어 순서 정렬 : 계산된 단어의 TF-IDF 가중치 합계를 기준으로 내림차순으로 정렬
# 가장 가중치가 높은 단어부터 순서를 매김
word.order.tf <- order(word.count.tf,decreasing=T)

# 5. 상위 300개 키워드 추출 
top_300keyword <- rownames(tdm_tf.matrix)[word.order.tf[1:300]]

# 6. 상위 300개 키워드의 빈도(TF-IDF 가중치) 행렬
freq.word <- tdm_tf.matrix[word.order.tf[1:300], ]

# 상위 300개 키워드 확인
freq.word

#이후 불용어 추가하면서 Lemmatization부터 top_300keyword까지 반복

# write.csv(freq.word, "C:/Users/USER/Desktop/DKU/2024-1 경영공학종합설계/freq_word.csv")
