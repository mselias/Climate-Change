## Cleaning Data Set ##
First <- New[which(New$Year == 2014),]
Second <- New[which(New$Year == 2016),]
Third <- New[which(New$Year == 2017),]

Data1 <- First[which(First$Hour == 13 | First$Hour == 14 | First$Hour == 15),]
Data1_2 <- First[which(First$Hour == 13 | First$Hour == 14),]
Data1_3 <- First[which(First$Hour == 13),]

Data2 <- Second[which(Second$Hour == 13 | Second$Hour == 14 | Second$Hour == 15),]
Data2_2 <- Second[which(Second$Hour == 13 | Second$Hour == 14),]
Data2_3 <- Second[which(Second$Hour == 13),]

Data3 <- Third[which(Third$Hour == 13 | Third$Hour == 14 | Third$Hour == 15),]
Data3_2 <- Third[which(Third$Hour == 13 | Third$Hour == 14),]
Data3_3 <- Third[which(Third$Hour == 13),]

## Time Series Analysis ##

## 2014 Year
A.TEMP1 <- Data1$Ts
TEMP1 <- HoltWinters(A.TEMP1, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP1)

A.TEMP1_2 <- Data1_2$Ts
TEMP2 <- HoltWinters(A.TEMP1_2, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP2)


##### This is the one we will use #####
A.TEMP1_3 <- Data1_3$Ts
TEMP3 <- HoltWinters(A.TEMP1_3, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP3, main = "Holt-Winters (2014 Data)", ylab = "Surface Temperature")
summary(TEMP3)
Forecast1 <- forecast(TEMP3, h=10)
plot(Forecast1, main = "Holt-Winters (2014 Data)", ylab = "Surface Temperature")

A.TEMP1_3_a <- Data1_3$Ta
TEMP3_a <- HoltWinters(A.TEMP1_3_a, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP3_a, main = "Holt-Winters (2014 Data)", ylab = "Air Temperature")
summary(TEMP3_a)
Forecast1 <- forecast(TEMP3, h=10)
plot(Forecast1, main = "Holt-Winters (2014 Data)", ylab = "Air Temperature")
##### This is the one we will use #####


Press1 <- Data1$BP
Late1 <- HoltWinters(Press1, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(Late1)



## 2016 Year
A.TEMP2 <- Data2$Ts
TEMP4 <- HoltWinters(A.TEMP2, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP4)

A.TEMP2_2 <- Data2_2$Ts
TEMP5 <- HoltWinters(A.TEMP2_2, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP5)


##### This is the one we will use #####
A.TEMP2_3 <- Data2_3$Ts
TEMP6 <- HoltWinters(A.TEMP2_3, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP6, main = "Holt-Winters (2016 Data)", ylab = "Surface Temperature")
summary(TEMP6)
Forecast2 <- forecast(TEMP6, h=10)
plot(Forecast2, main = "Holt-Winters (2016 Data)", ylab = "Surface Temperature")

A.TEMP2_3_a <- Data2_3$Ta
TEMP6_a <- HoltWinters(A.TEMP2_3_a, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP6, main = "Holt-Winters (2016 Data)", ylab = "Air Temperature")
summary(TEMP6_a)
Forecast2 <- forecast(TEMP6, h=10)
plot(Forecast2, main = "Holt-Winters (2016 Data)", ylab = "Air Temperature")
##### This is the one we will use #####



Press2 <- Data2$BP
Late2 <- HoltWinters(Press2, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(Late2)



## 2017 Year
A.TEMP3 <- Data3$Ts
TEMP7 <- HoltWinters(A.TEMP3, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP7)

A.TEMP3_2 <- Data3_2$Ts
TEMP8 <- HoltWinters(A.TEMP3_2, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP8)


##### This is the one we will use #####
A.TEMP3_3 <- Data3_3$Ts
TEMP9 <- HoltWinters(A.TEMP3_3, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP9, main = "Holt-Winters (2017 Data)", ylab = "Surface Temperature")
summary(TEMP9)
Forecast3 <- forecast(TEMP9, h=10)
plot(Forecast3, main = "Holt-Winters (2017 Data)", ylab = "Surface Temperature" )

A.TEMP3_3_a <- Data3_3$Ta
TEMP9_a <- HoltWinters(A.TEMP3_3_a, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(TEMP9_a, main = "Holt-Winters (2017 Data)", ylab = "Air Temperature")
summary(TEMP9_a)
Forecast3 <- forecast(TEMP9, h=10)
plot(Forecast3, main = "Holt-Winters (2017 Data)", ylab = "Air Temperature" )
##### This is the one we will use #####



Press3 <- Data3$BP
Late3 <- HoltWinters(Press3, start.periods = 2,seasonal = c("additive"), alpha = NULL, gamma = FALSE, beta = NULL)
plot(Late3)