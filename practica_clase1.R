colnames(heart2)[1] <- "age"

heart2$sex <- factor(heart2$sex, levels=c(1, 0), labels = c("H", "M"))

heart2$sex

plus40 <- heart2[which(heart2$age > 40),]

plus40male <- heart2[which(heart2$age > 40 & heart2$sex == "H"),]

sexFreq <- table(heart2$sex)

cpFreq <- table(heart2$cp)

cholInPlus45 <- heart2[which(heart2$age > 45 & heart2$sex == "H"),c(2, 5)]

pie(table(heart2$exang))