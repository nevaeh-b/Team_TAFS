# P2 TeamTAFS (Part 1)

## 실행 방법
1. 교수님 zip의 shoes/, selfcheck/, shoes_meta.csv를 data/ 폴더 안에 넣기
2. data/ourshoes/ 폴더를 만들고 팀 사진 넣기 (영어이름_Category_번호.png)
3. 패키지 설치: install.packages(c("shiny", "png", "jpeg", "ggplot2"))
4. RStudio에서 app.R을 열고 Run App

## 폴더 모양
```
P2/
  app.R
  data/
    shoes/
    selfcheck/
    shoes_meta.csv
    ourshoes/
```

## 주의
- 신발 사진은 수업 용도로만 쓰므로 GitHub에 올리지 않음
- 앱을 처음 켤 때 PCA 계산 때문에 30초 정도 걸림
