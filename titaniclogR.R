library(xlsx)
titanic <- read.xlsx("C:\\Users\\Hi\\Desktop\\Titanic\\Titanic\\Titanic Survival Data.xlsx",header= T,sheetIndex = 1)

tit1 <- titanic

for (i in 1:891){
  if(str_detect(tit1$name[i],"Mrs.") == T){
    tit1$namcl[i] <- 1 
  }else if(str_detect(tit1$name[i],"Master.") == T){
    tit1$namcl[i] <- 2 
  }else if(str_detect(tit1$name[i],"Miss.") == T){
    tit1$namcl[i] <- 3
  }else{
    tit1$namcl[i] <- 0
  }
  next
}

tit1$age0 <- 0
tit1$age1 <- 0
tit1$age2 <- 0
tit1$age3 <- 0

for ( i in 1:891){
  if (tit1$namcl[i] == 0){
    tit1$age0 [i] = tit1$age[i]
  }else if (tit1$namcl[i] == 1){
    tit1$age1 [i] = tit1$age[i]
  }else if (tit1$namcl[i] == 2){
    tit1$age2 [i] = tit1$age[i]
  }else{
    tit1$age3 [i] = tit1$age[i]
  }
  next
}

for (i in 14:17){
  tit1[,i][tit1[,i] == 0] <- NA
}

for ( i in 1:891){
  mean0 <- mean(tit1$age0,na.rm = T)
  mean1 <- mean(tit1$age1,na.rm = T)
  mean2 <- mean(tit1$age2,na.rm = T)
  mean3 <- mean(tit1$age3,na.rm = T)
}

for (i in 1:891){
  if (is.na(tit1$age[i]) == T){
    if (tit1$namcl[i] == 0){
      tit1$age[i] <- 33
    } else if (tit1$namcl[i] == 1){
      tit1$age[i] <- 36
    } else if (tit1$namcl[i] == 2){
      tit1$age[i] <- 5
    }else{
      tit1$age[i] <- 22
    }
  }else{
    next
  }  
}


set.seed(25)
tind <- createDataPartition(tit1$Survived,p=0.7,list = F,times = 1)
tit1train <- tit1[tind,]
tit1test <- tit1[-tind,]

#MOdel1

tit1$Survived <- factor(tit1$Survived)
tit1$sibsp <- factor(tit1$sibsp)

lrm2 <- glm(Survived ~pclass+sex+age+sibsp+parch+namcl-1, data = tit1train, family = "binomial")
summary(lrm2)
tit1test$pprob <- predict(lrm2,tit1test[,-12],type = "response")
tit1test$predval <- ifelse(tit1test$pprob >=0.5,1,0)

#HL test
hoslem.test(lrm2$y,lrm2$fitted.values)

#COncordance
bruteforce(lrm2)
OptimisedConc(lrm2)

#Classification table
CT <-CrossTable(tit1test$Survived,tit1test$predval)
CT_val <- as.vector(CT$t)
acc1 <-(CT_val[1]+CT_val[4])/nrow(tit1test)

#ROC Plot
rocplot(lrm2)

#read test file of kaggle
test_kag <- read.csv("C:\\Users\\Hi\\Desktop\\Titanic\\Titanic\\test.csv",header = T)
test_kag$sibsp <- as.factor(test_kag$sibsp)

for (i in 1:418){
  if(str_detect(test_kag$name[i],"Mrs.") == T){
    test_kag$namcl[i] <- 1 
  }else if(str_detect(test_kag$name[i],"Master.") == T){
    test_kag$namcl[i] <- 2 
  }else if(str_detect(test_kag$name[i],"Miss.") == T){
    test_kag$namcl[i] <- 3
  }else{
    test_kag$namcl[i] <- 0
  }
  next
}

test_kag$age0 <- 0
test_kag$age1 <- 0
test_kag$age2 <- 0
test_kag$age3 <- 0

for ( i in 1:418){
  if (test_kag$namcl[i] == 0){
    test_kag$age0 [i] = test_kag$age[i]
  }else if (test_kag$namcl[i] == 1){
    test_kag$age1 [i] = test_kag$age[i]
  }else if (test_kag$namcl[i] == 2){
    test_kag$age2 [i] = test_kag$age[i]
  }else{
    test_kag$age3 [i] = test_kag$age[i]
  }
  next
}

for (i in 13:16){
  test_kag[,i][test_kag[,i] == 0] <- NA
}

for ( i in 1:418){
  Tmean0 <- mean(test_kag$age0,na.rm = T)
  Tmean1 <- mean(test_kag$age1,na.rm = T)
  Tmean2 <- mean(test_kag$age2,na.rm = T)
  Tmean3 <- mean(test_kag$age3,na.rm = T)
}

for (i in 1:418){
  if (is.na(test_kag$age[i]) == T){
    if (test_kag$namcl[i] == 0){
      test_kag$age[i] <- 33
    } else if (test_kag$namcl[i] == 1){
      test_kag$age[i] <- 37
    } else if (test_kag$namcl[i] == 2){
      test_kag$age[i] <- 4
    }else{
      test_kag$age[i] <- 22
    }
  }else{
    next
  }  
}

test_kag$sibsp[361] <- 5
test_kag$pprob <- predict(lrm2,test_kag,type = "response")
test_kag$predval <- ifelse(test_kag$pprob >=0.5,1,0)

submission <- as.data.frame(cbind(test_kag[1],test_kag$predval))
colnames(submission) <- c("PassengerId","Survived")

write.csv(submission,"C:\\Users\\Hi\\Desktop\\Titanic\\Titanic\\sub3.csv")
