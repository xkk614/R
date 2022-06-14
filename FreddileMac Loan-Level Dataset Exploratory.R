### Reading Data, Explore data
Loan_data = read.table("sample_orig_2020.txt", header = F, sep = "|")
head(Loan_data)
dim(Loan_data)
colnames(Loan_data) <- c("Credit Score", "First Payment Date", "First Time Homebuyer", "Maturity Date", "MSA", "Mi%", "No. of Units", "Occupancy Status", "CLTV", "DTI Ratio", "UPB", "LTV", "Interest Rate", "Channel", "PPM", "Amortization Type", "Property State", "Property Type", "Postal Code", "Loan Sequence Number", "Loan Purpose", "Loan Term", "No. of Borrowers", "Seller Name", "Service Name", "sp conf flg", "Seq Number", "Program Indicator", "Harp Indicator", "Valuation Method", "I/O Indicator")
head(Loan_data)
Loandata2 <- Loan_data[Loan_data$`Credit Score` != 9999, ]
loan_data3 <- Loan_data[Loan_data$`First Time Homebuyer`!=9, ]
dim(loan_data3)

####### Signel variable exploration 

#ciredit score 
Loan_data$`Credit Score`
range(Loan_data$`Credit Score`)
range(Loandata2$`Credit Score`)
hist(Loan_data$`Credit Score`)
hist(table(Loan_data$`Credit Score`))
pie(table(Loan_data$`Credit Score`))

Loan_data$CreditScrLvl <- "Not avaliable"
Loan_data$CreditScrLvl[Loan_data$`Credit Score`>= 800 & Loan_data$`Credit Score`<9998] <- "Exceptional"
Loan_data$CreditScrLvl[Loan_data$`Credit Score`<800 & Loan_data$`Credit Score` >=740] <- "Very Good"
Loan_data$CreditScrLvl[Loan_data$`Credit Score`<740 & Loan_data$`Credit Score` >=670] <- "Good"  ### add colums for differnt credit score levels 
Loan_data$CreditScrLvl[Loan_data$`Credit Score`< 670] <- "Fair"
max(Loan_data$`Credit Score`
min(Loan_data$`Credit Score`)
sort(table(Loan_data$CreditScrLvl), decreasing = TRUE)              ##### #Table 1 

pie(table(Loan_data$CreditScrLvl),main = "Credit Score Level",     
    col = c("forestgreen", "red", "gold", "grey", "chartreuse"))    ##### Figure 1 

table(Loan_data$CreditScrLvl)                                      
barplot(table(Loan_data$CreditScrLvl), main = "Credit Score Level",
        xlab = "frequency", ylab = "Standings",
        col = c("forestgreen", "red", "gold", "grey", "chartreuse"))

#Interst rate
range(Loan_data$`Interest Rate`)
hist(Loan_data$`Interest Rate`, breaks = 20, 
     main = "Interest Rate", 
     xlab = "interest Rate %", ylab = "Number of people")           ###### Figure 2 
abline(v=median(Loan_data$`Interest Rate`), col="red")
abline(v=mean(Loan_data$`Interest Rate`), col="blue")
Average_interest <- mean(Loan_data$`Interest Rate`)
boxplot(Loan_data$`Interest Rate`)
median(Loan_data$`Interest Rate`)
#Property type
Loan_data$`Property Type`
barplot(table(Loan_data$`Property Type`), col= c("yellow", "red","orange", "blue", "green"), 
        main = "# of Property Type",
        xlab = "Property type", ylab = "number of type")             ######Figure 4
pie(table(Loan_data$`Property Type`), col= c("yellow", "red","orange", "blue", "green"),
    main = "# of Property Type")
table(Loan_data$`Property Type`)                                     ###table 2
#Loan Purpose
barplot(table(Loan_data$`Loan Purpose`), col= c("Red", "Green", "Blue"), 
        main = "Loan Purpose", 
        xlab = "loan Purpose", ylab = "No. of people")             

pie(table(Loan_data$`Loan Purpose`),
    col= c("Red", "Green", "Blue"),
    main = "Loan Purpose")                                           #### #Figure 3 

#DIT ratio
hist(Loan_data$`DTI Ratio`, breaks = 20, main = "Debt to income Ratio", 
     xlab = "DTI Ritio%", ylab = "No. of loans")
abline(v=mean(Loan_data$`DTI Ratio`), col="red")
Loan_data$`DTI Ratio`[1:5]

range(Loan_data$`DTI Ratio`)


#no. of unit
hist(Loan_data$`No. of Units`)
sort(table(Loan_data$`No. of Units`), decreasing = TRUE)

#property state 
barplot(table(Loan_data$`Property State`), las = 2,#explore property States
        cex.names = 0.5)
sort(Loan_data$`Property State`)
sort(table(Loan_data$`Property State`), decreasing = TRUE)
length(unique(Loan_data$`Property State`))


#######multi variable analysis 

## credit score vs interest rate 
plot(Loan_data$`Credit Score`, Loan_data$`Interest Rate`,
     xlim = c(600,850), ylim = c(1,7),
     pch=20, col="black", cex=1)
reg_line <- lm(Loan_data$`Credit Score`~Loan_data$`Interest Rate`)
abline(reg_line, col = "red")

boxplot(Loan_data$`Interest Rate`~ Loan_data$CreditScrLvl, data = Loan_data, 
        main = "Credit score and Interest Rate",
        xlab = "CreditScore Level", ylab = "Interest Rate")        #####Figure 5

Exceptional_Ir <- mean(Loan_data$`Interest Rate`[Loan_data$CreditScrLvl == "Exceptional"])
Verygood_Ir <- mean(Loan_data$`Interest Rate`[Loan_data$CreditScrLvl == "Very Good"])
Good_Ir <- mean(Loan_data$`Interest Rate`[Loan_data$CreditScrLvl == "Good"])
Fair_Ir <- mean(Loan_data$`Interest Rate`[Loan_data$CreditScrLvl == "Fair"])

avg_ir <- data.frame(CreditScrLvl = c("exceptional", "VeryGood", "Good", "Fair"),
                    Avg_IR = c(Exceptional_Ir, Verygood_Ir, Good_Ir, Fair_Ir))  #table 3
avg_ir


##### Interst rate vs Loan purpose
boxplot(Loan_data$`Interest Rate`~ Loan_data$`Loan Purpose`, data = Loan_data,
        main = "Interest Rate and Loan Purpose",
        xlab = "loan Purpose", ylab = "Interest Rate")            #######figure 6

##Interest rate vs loan term 
plot(Loan_data$`Loan Term`, Loan_data$`Interest Rate`, xlim = c(90,360), ylim = c(1,7),
     pch=20, col="black", cex=1,
     xlab = "Loan Term", ylab = "Interest Rate",
     main = "Interest Rate vs Loan Term")
reg_line <- lm(Loan_data$`Interest Rate`~Loan_data$`Loan Term`)     ########figure 7 
abline(reg_line, col = "red")
range(Loan_data$`Interest Rate`)
range(Loan_data$`Loan Term`)

######property type vs interst rate 
boxplot(Loan_data$`Interest Rate`~ Loan_data$`Property Type`, data = Loan_data,
        main = ' Property type and Interst rate',
        xlab = "Property type", ylab = "Interest Rate")            ########Figure 8

#### First time home ber vs Credit score 
table(loan_data3$`First Time Homebuyer`)
table(loan_data3$CreditScrLvl)
homebuyer_table <- table(loan_data3$`First Time Homebuyer`, loan_data3$CreditScrLvl)
barplot(table(loan_data3$`First Time Homebuyer`, loan_data3$CreditScrLvl),
        xlab = "Credit Score level", ylab = "First time home buyer",
        main = "First Time home buyer and Credit score level",
        legend = "topleft", rownames(homebuyer_table),
        col = c("red", "chartreuse"))                               ########Figure 9 
table(loan_data3$`First Time Homebuyer`, loan_data3$CreditScrLvl)   ########table 4 
#### property type vs ciredit score
prptyp_table <- table(Loan_data$CreditScrLvl, Loan_data$`Property Type`)
barplot(table(Loan_data$CreditScrLvl, Loan_data$`Property Type`),
        xlab = "Property type", ylab = "Number of loans",
        main = "property type and Credit score Level", 
        legend= "topleft", rownames(prptyp_table),
        col = c("forestgreen", "red", "gold", "grey", "chartreuse"))  ########Figure 10 
table(Loan_data$CreditScrLvl, Loan_data$`Property Type`)             #########Table 5
barplot(table(Loan_data$`Interest Rate`, Loan_data$CreditScrLvl, Loan_data$`Property Type`))





####### variables that examed but not in the report

####### DIT vs ciredit score
plot(Loan_data$`Credit Score`, Loan_data$`DTI Ratio`, xlim = c(600,850), ylim = c(0,60),
     pch=20, col="black", cex=1)
reg_line <- lm(Loan_data$`Credit Score`~Loan_data$`DTI Ratio`)
abline(reg_line, col = "red")
cor(Loan_data$`Credit Score`,Loan_data$`DTI Ratio`)




#### heatmap

colums <- c("Credit Score","DTI Ratio", "LTV", "Interest Rate", "Loan Term")
Cor_mat <- cor(Loan_data[, colums])
library(pheatmap)
pheatmap(Cor_mat, cluster_cols = F, cluster_rows = F, 
         display_numbers = Cor_mat)
