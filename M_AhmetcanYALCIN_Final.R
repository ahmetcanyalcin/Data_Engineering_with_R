
#                      ********* IVA 501  - Final Sinavi *********


#SORU - 1

c1=c("ali", "nadire", "huseyin", NA, "kadir")
c2=c(NA, "F","M" , "F", "M")
c3=c(15000, NA, NA, 20000, 12645)
df<- data.frame(c1,c2,c3)

naLokasyonlari <- function(dfr) {
  dfr[!is.na(df)] <- FALSE
  dfr[is.na(df)] <- TRUE
  for(i in 1:ncol(dfr)){
    dfr[, i] <- as.logical(dfr[, i])
  }
  return (dfr)
}

naLokasyonlari(df)

#    c1    c2    c3
#1 FALSE  TRUE FALSE
#2 FALSE FALSE  TRUE
#3 FALSE FALSE  TRUE
#4  TRUE FALSE FALSE
#5 FALSE FALSE FALSE



#SORU - 2

df2<- c(-5, 5, 10, -15, 7)
organizeEt <- function(inp) {
  out <- c(inp[inp<0], inp[inp>=0])
  return (out)
}
organizeEt(df2)

# -5 -15   5  10   7


# SORU - 3 


inp <- c(3, 10, 3, 11, 4, 5, 6, 7, 8, 12)
ardisikEnUzun <- function(inp) {
  ardisikUzunluklar = c()
  ardisikBaslangicIndeksleri = c() 
  j = 1
  start = 1
  for(i in 1:(length(inp)-1)){
    if((inp[i] + 1) == inp[i+1]){
      j=j+1
    } else{
      ardisikBaslangicIndeksleri = append(ardisikBaslangicIndeksleri, start)
      ardisikUzunluklar = append(ardisikUzunluklar, j)
      start = i+1
      j=0
    }
    i=i+1
  }
  
  indeks = which.max(ardisikUzunluklar)
  baslangic = ardisikBaslangicIndeksleri[indeks]
  uzunluk = ardisikUzunluklar[indeks]
  indexler <- c(baslangic-1, baslangic+uzunluk-1)
  return (indexler)
}
ardisikEnUzun(inp)





# SORU - 4 


x1 <- c(-5, 5, 10, -15, 7)
x2 <- c(-50, 50, 100, -150, 70)
df4 <- data.frame(x1, x2) 

yenidenAdlandir <- function(dfr) {
  newCols = paste("c", c(1:ncol(df4)), sep = "")
  colnames(dfr) <- newCols
  return (dfr)
}
yenidenAdlandir(df4)

#    c1   c2
# 1  -5  -50
# 2   5   50
# 3  10  100
# 4 -15 -150
# 5   7   70



# SORU - 5 


ardisikEnUzun <- function(inp) {
  ardisikUzunluklar = c()
  ardisikBaslangicIndeksleri = c() 
  j = 1
  start = 1
  for(i in 1:(length(inp)-1)){
    if((inp[i] + 1) == inp[i+1] && i <length(inp)-1){
      j=j+1
    } else{
      ardisikBaslangicIndeksleri = append(ardisikBaslangicIndeksleri, start)
      if(i == (length(inp)-1) && (inp[i] + 1) == inp[i+1]){
        j=j+1
      }
      
      ardisikUzunluklar = append(ardisikUzunluklar, j)
      start = i+1
      j=1
    }
    i=i+1
  }
  
  indeks = which.max(ardisikUzunluklar)
  baslangic = ardisikBaslangicIndeksleri[indeks]
  uzunluk = ardisikUzunluklar[indeks]
  bitis = baslangic+uzunluk-1
  if(baslangic == bitis){
    return ("Ardisik Yok")
  }
  
  indexler <- c(baslangic, bitis)
  return (indexler)
}

inp <- c(1, 2, 3, 4,   1, 2, 3, 4, 5,    1, 2, 3)
ardisikEnUzun(inp)
inp <- c(1, 3, 3)
ardisikEnUzun(inp)
inp <- c(1, 3, 3, 4, 5, 5)
ardisikEnUzun(inp)
inp <- c(1, 3, 3, 4, 5, 6)
ardisikEnUzun(inp)