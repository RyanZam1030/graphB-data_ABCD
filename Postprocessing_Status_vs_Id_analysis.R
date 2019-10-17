####################################################################################################
##### Clears Working Environment / Install packages
####################################################################################################

rm(list=ls())

##### Installing neccessary dependecies. Only need to install once on local computer 
install.packages(c("readr","corrgram","ggplot2","grid"))

##### Importing Libraries 
##### Library for reading files 
library(readr)
##### Library for plots. 
library(corrgram)
library(ggplot2)
library(grid)

####################################################################################################
##### This is the Dataset that has patients and present Diagnosis with all rows and Column NAs
####################################################################################################

Interview_1_subset <- read.csv("~/Documents/Project/ABCD/Interview_1_All_Present_Diagnosis_subset.csv", 
                               stringsAsFactors=FALSE)

####################################################################################################
##### Assigning the dataset with a generic variable name
####################################################################################################

interview = Interview_1_subset
dim(interview)
View(interview)

presentDiags =  c("ksads_1_843_p", "ksads_1_840_p", "ksads_1_846_p",
                  "ksads_2_835_p", "ksads_2_836_p", "ksads_2_831_p", "ksads_2_832_p",
                  "ksads_2_830_p", "ksads_2_838_p", "ksads_3_848_p", "ksads_4_851_p",
                  "ksads_4_826_p", "ksads_4_828_p", "ksads_4_849_p", "ksads_5_857_p",
                  "ksads_6_859_p", "ksads_7_861_p", "ksads_8_863_p", "ksads_9_867_p",
                  "ksads_10_869_p", "ksads_11_917_p", "ksads_12_927_p", "ksads_12_925_p",
                  "ksads_13_938_p", "ksads_13_929_p", "ksads_13_932_p", "ksads_13_935_p",
                  "ksads_14_853_p", "ksads_14_856_p", "ksads_15_901_p", "ksads_16_897_p",
                  "ksads_16_898_p", "ksads_17_904_p", "ksads_19_895_p", "ksads_19_891_p",
                  "ksads_20_893_p", "ksads_20_874_p", "ksads_20_872_p", "ksads_20_889_p",
                  "ksads_20_878_p", "ksads_20_877_p", "ksads_20_875_p", "ksads_20_876_p",
                  "ksads_20_879_p", "ksads_20_873_p", "ksads_20_871_p", "ksads_21_921_p",
                  "ksads_22_969_p", "ksads_23_954_p", "ksads_23_952_p", "ksads_23_955_p", 
                  "ksads_23_951_p", "ksads_23_953_p", "ksads_24_967_p", "ksads_25_865_p")

##### Uncomment to shorten present diagnosis names 
##### To comment/uncomment blocks of code: Highlight all then press Ctrl + Shift + C 
newPresentDiags = c("PDD", "MDD", "UDD", "Bi2H", "Bi2D", "Bi1D", "Bi1H", "Bi1EM",
                    "UbiRD", "DMDD", "USOPD", "Hal", "Del", "APS", "PD", "Agor",
                    "SepAD", "SocAD", "SP", "GAD", "OCD", "Encop", "Enur",  "BED",
                    "ANBE", "ANRS", "BulN", "ADHD", "UADHD", "ODD", "CDCh", "CDAd",
                    "UTD", "UARD", "AUD", "USRD", "SUDCoc", "SUDAmp", "SUD", "IUD",
                    "PCPUD", "OUD", "OHUD", "ODUD", "SHAUD", "CUD", "PTSD", "SlP",
                    "SA", "IA", "NSIB", "PAISB", "ASA", "HIB", "SM")

colnames(interview) = c(colnames(interview[,c(1,2,3)]), newPresentDiags)

##### Making a dataframe with summury frequency counts
rowCounts = nrow(interview)
columnCounts = ncol(interview)
onesCounts = colSums(interview[,4:columnCounts], na.rm=TRUE)
NACounts = apply(interview[,4:columnCounts],2, function(x) sum(length(which(is.na(x)))))
totalCounts = rowCounts
zeroCounts = totalCounts - onesCounts - NACounts 
freqdf = data.frame(rbind(onesCounts, zeroCounts, NACounts, totalCounts))
freqdf



####################################################################################################
##### Making the Diagnositc Counts bar plot for all present diagnositics
####################################################################################################

##### Run to center all main titles for plots
theme_update(plot.title = element_text(hjust = 0.5))

##### Making levels for plot coloring 
df = t(freqdf)
bins <- cut(as.numeric(df[,1]), breaks = seq(0, 1100, 100),include.lowest = T)
groups = cut(as.numeric(df[,1]), breaks = seq(0, 1100, 365),include.lowest = T)
df = cbind(df, bins, groups)
df[,6][df[,6] == 1] = "Low"
df[,6][df[,6] == 2] = "Mid"
df[,6][df[,6] == 3] = "High"
Levels = as.factor(df[,6]) 

g1 = qplot(x = as.numeric(df[,1]), fill = factor(Levels), geom = "histogram", binwidth = 100, 
           main = "Diagnostic Counts by Levels",
           xlab = " Number of endorsed diagnosis", 
           ylab = "Number of frequency counts",
           col=I("black"))
g1  + scale_fill_manual(values = c("green", "red", "yellow"), name = "Levels") 



####################################################################################################
##### Reading in the Status_vs_Id csv file
####################################################################################################

##### The Path will need to change to where ever the status _vs_IDs csv files are located
Interview_1_0_status <- read.csv("~/Desktop/Interview_1_0.csv", stringsAsFactors=FALSE)

##### Removing the first column of unneccary indicies
Interview_1_status = Interview_1_0_status[,-1]


####################################################################################################
##### Reading in Interview Users.csv for mapping

##### Mapping the users to and status
##### Path might need to change
Interview_1_users <- read.csv("~/Documents/Project/ABCD/Interview_1_users.csv", stringsAsFactors=FALSE)

##### Both the row numbers should be equal
dim(Interview_1_users)
dim(Interview_1_status)

Node_Status_Mapping = Interview_1_users
Node_Status_Mapping$Status = Interview_1_status$X..C0[match(Interview_1_users$Node.ID, Interview_1_status$Vert.ID)]
View(Node_Status_Mapping)

existing = match(presentDiags, Node_Status_Mapping$User.ID)
Node_Status_Mapping$User.ID[na.omit(existing)] = newPresentDiags[which(!is.na(existing))]
View(Node_Status_Mapping)

###### Putting the names of the diagnosis into a vector 
diagID  = Node_Status_Mapping$User.ID[1:51]

####################################################################################################
###### Plotting the Status_vs_ID
plot(Interview_1_status$Vert.ID, Interview_1_status$X..C0,
     main = "Status vs IDs", col = "blue",
     xlab = "Vert ID", ylab = "Status")


####################################################################################################
###### Plotting the patient/child status vs Id
diag_vertID = Interview_1_status$Vert.ID[1:length(diagID)]
diag_status = Interview_1_status$X..C0[1:length(diagID)]
diagdf = data.frame(diag_vertID, diag_status)

plot(diag_vertID, diag_status, ylim = c(0, 20), 
     main = "Diagnosis Status", col = "blue",
     xlab = "Diagnosis ID", ylab = "Status")

yintercept1 = c(mean(diag_status), 
               mean(diag_status) + sd(diag_status), 
               mean(diag_status) - sd(diag_status), 
               mean(diag_status) + 2*sd(diag_status), 
               mean(diag_status) - 2*sd(diag_status))

ggplot() + geom_point(aes(x = diag_vertID, y = diag_status), colour="red", shape=1) + 
geom_hline(yintercept = yintercept1) + 
geom_text(aes(x = 0, y = yintercept1[1], label = paste(expression(paste(mu)))), vjust = -0.5, parse = TRUE) + 
geom_text(aes(x = 0, y = yintercept1[2], label = paste("+",expression(sigma),sep="")), vjust = -0.5, parse = TRUE) + 
geom_text(aes(x = 0, y = yintercept1[3], label = paste("-",expression(sigma),sep="")), vjust = -0.5, parse = TRUE) + 
#geom_text(aes(x = 0, y = yintercept[4], label = paste("-","2",expression(sigma),sep=" ")), vjust = -0.5, parse = TRUE) + 
#geom_text(aes(x = 0, y = yintercept[5], label = paste("-2",expression(sigma),sep="")), vjust = -0.5, parse = TRUE) + 
labs(x = "Diagnostic ID", y = "Status", title = "Status of Diagnoistic IDs")


####################################################################################################
##### Plotting the Child Status_vs_ID by color
child_vertID = Interview_1_0_status$Vert.ID[(length(diagID)+1):nrow(Interview_1_status)]
child_status = Interview_1_0_status$X..C0[(length(diagID)+1):nrow(Interview_1_status)]
childdf = data.frame(child_vertID,child_status)

yintercept = c(mean(child_status), 
               mean(child_status) + sd(child_status), 
               mean(child_status) - sd(child_status), 
               mean(child_status) + 2*sd(child_status), 
               mean(child_status) - 2*sd(child_status))

ggplot() + geom_point(aes(x = child_vertID, y = child_status), colour="blue", shape=1) + 
geom_hline(yintercept = yintercept) + 
geom_text(aes(x = 0, y = yintercept[1], label = paste(expression(paste(mu)))), vjust = -0.5, parse = TRUE) + 
geom_text(aes(x = 0, y = yintercept[2], label = paste("+",expression(sigma),sep="")), vjust = -0.5, parse = TRUE) + 
geom_text(aes(x = 0, y = yintercept[3], label = paste("-",expression(sigma),sep="")), vjust = -0.5, parse = TRUE) + 
#geom_text(aes(x = 0, y = yintercept[4], label = paste("-","2",expression(sigma),sep=" ")), vjust = -0.5, parse = TRUE) + 
#geom_text(aes(x = 0, y = yintercept[5], label = paste("-2",expression(sigma),sep="")), vjust = -0.5, parse = TRUE) + 
labs(x = "Patient ID", y = "Status", title = "Status of Patients IDs")


####################################################################################################
##### Plotting the All Status_vs_ID by color
plot(child_vertID, child_status, xlim = c(0,13000),
     ylim = c(0,100), col = "blue",
     main = "Patient and Diagnosis Status vs ID", 
     xlab = "VertID", ylab = "Status %")
points(diag_vertID, diag_status)
points(diag_vertID, diag_status, pch = 3, col = 2)

ggplot() +  geom_point(aes(x = diag_vertID, y = diag_status), colour="red", shape=1) +
geom_point(aes(x = child_vertID, y = child_status), colour="blue", shape=1) +
labs(x = "Node ID", y = "Status", title = "Patient and Diagnosis Status vs IDs")






####################################################################################################
##### Removing any rows and columns with All NA values and writing subset for analysis
####################################################################################################
# Removing rows
interview = interview[rowSums(is.na(interview[,4:length(presentDiags)])) < (ncol(interview[,4:length(presentDiags)])-1),]
dim(interview)
#Removing columns
interview = interview[,colSums(is.na(interview)) < (nrow(interview)-1)]
dim(interview)

interview$Status = Node_Status_Mapping$Status[(length(diagID)+1):dim(Node_Status_Mapping)[1]]
View(interview)
tail(interview)





####################################################################################################
##### Finding out how many subjects have more than one diagnosis. 
####################################################################################################
interview$Counts = rowSums(interview[,4:(ncol(interview)-1)], na.rm = TRUE) 
max(interview$Counts)
head(interview)

####################################################################################################
##### Plotting the Patient Status Ramped by number of diagnosis patients endorse

f1 = factor(interview$Counts)
ggplot(interview, mapping=aes(x = seq_along(interview$Status), y = interview$Status, color = f1)) + 
geom_point(size = 3, shape = 1) + scale_y_continuous(breaks = seq(50, 100, by = 10)) + 
scale_colour_hue(l=50) + 
labs(x = "Patient ID", 
     y = "Status", 
     color = "Diag Counts", 
     title = "Status of Subjects Ramped by number of diagnosis endorsed")


lmsumm = summary(lm(interview$Status~f1))
rsq = lmsumm$r.squared

grob <- grobTree(textGrob(bquote(R^{2}==~.(rsq)), x=.60,  y=0.55, hjust=0,
                          gp=gpar(col="red", fontsize=20, fontface="italic")))

q = qplot(x = interview$Counts, y = interview$Status, fill = f1, geom = "boxplot",
          main = "Status of Subjects Ramped by number of diagnosis endorsed",
          xlab = " Number of endorsed diagnosis", 
          ylab = "Status Values",
          col=I("black"))
q  + scale_fill_discrete(name = "Counts") + 
  geom_smooth(method = "lm", color="red", aes(group=1)) + 
  annotation_custom(grob)


####################################################################################################
##### Ploting the peopole who have at least one diagnosis. 
####################################################################################################

Count_nonzeros = interview[!interview$Counts == 0,]
Count_nonzeros[is.na(Count_nonzeros)]  =  0

f2 = factor(Count_nonzeros$Counts)
ggplot(Count_nonzeros, mapping=aes(x = seq_along(Count_nonzeros$Status), y = Count_nonzeros$Status, color = f2)) + 
geom_point(size = 3, shape = 1) + scale_y_continuous(breaks = seq(50, 100, by = 10)) + 
scale_colour_hue(l=50) + 
labs(x = "Patient ID", 
     y = "Status", 
     color = "Diag Counts", 
     title = "Status of Subjects Ramped by number of diagnosis endorsed")

lmsumm = summary(lm(Count_nonzeros$Status~f2))
rsq2 = lmsumm$r.squared

grob <- grobTree(textGrob(bquote(R^{2}==~.(rsq2)), x=.60,  y=0.55, hjust=0,
                          gp=gpar(col="red", fontsize=20, fontface="italic")))

q = qplot(x = Count_nonzeros$Counts, y = Count_nonzeros$Status, fill = f2, geom = "boxplot",
          main = "Status of Subjects Ramped by number of diagnosis endorsed",
          xlab = " Number of endorsed diagnosis", 
          ylab = "Status Values",
          col=I("black"))
q  + scale_fill_discrete(name = "Counts") + 
  geom_smooth(method = "lm", color="red", aes(group=1)) + 
  annotation_custom(grob)


################################################################################################
##### Making a dataframe with summury frequency counts
nrowCounts = nrow(Count_nonzeros)
ncolumnCounts = ncol(Count_nonzeros)
nonesCounts = colSums(Count_nonzeros[,4:(ncolumnCounts-2)], na.rm=TRUE)
nNACounts = apply(Count_nonzeros[,4:(ncolumnCounts-2)],2, function(x) sum(length(which(is.na(x)))))
ntotalCounts = nrowCounts
nzeroCounts = ntotalCounts - nonesCounts - nNACounts 
nfreqdf = data.frame(rbind(nonesCounts, nzeroCounts, nNACounts, ntotalCounts))
nfreqdf


wdf = t(nfreqdf)
wdf = df[!df[,2] == 0,]
bins <- cut(as.numeric(wdf[,1]), breaks = seq(0, 1100, 100),include.lowest = T)
groups = cut(as.numeric(wdf[,1]), breaks = seq(0, 1100, 365),include.lowest = T)
wdf = cbind(wdf, bins, groups)
wdf[,6][wdf[,6] == 1] = "Low"
wdf[,6][wdf[,6] == 2] = "Mid"
wdf[,6][wdf[,6] == 3] = "High"
Levels = wdf[,6] 

matchings = rbind(colnames(Count_nonzeros[(4:(ncol(Count_nonzeros)-2))]),Levels)
redG = names(matchings[2,][matchings[2,] == "Low"])
yellowG = names(matchings[2,][matchings[2,] == "Mid"])
greenG = names(matchings[2,][matchings[2,] == "High"])

# One level
lowfredDiags = Count_nonzeros[,c("Status",paste(redG))]
max(rowSums(lowfredDiags[,-1]))
plot(lowfredDiags$Status)

midfredDiags = Count_nonzeros[,c("Status",paste(yellowG))]
plot(midfredDiags$Status)

highfredDiags = Count_nonzeros[,c("Status",paste(greenG))]
max(rowSums(highfredDiags[,-1]))
plot(highfredDiags$Status)

# One level
# Here are all the people who have a high rank diagnosis 
rs = rowSums(highfredDiags[,2:4])
highfredDiagsL = list()
for(i in 1:length(rs)){
  if(rs[i] > 0){
    highfredDiagsL[i] = 1
  } else{
    highfredDiagsL[i] = 0
  }
}
highfredDiags$indicator = as.numeric(highfredDiagsL)

f3 = factor(highfredDiags$indicator)
ggplot(highfredDiags, mapping=aes(x = seq_along(highfredDiags$Status), y = highfredDiags$Status, color = f3)) + 
  geom_point(size = 3, shape = 1) + scale_y_continuous(breaks = seq(50, 100, by = 10)) + scale_colour_hue(l=50) + scale_colour_manual(values = c("black","green")) +
  labs(x = "Patients with Diagnosis", y = "Status", color = "Indicator", title = "Patietnt Status colored by high frequency diagnois")

# Here are all the people who have a low rank diagnosis
rs = rowSums(lowfredDiags[,2:ncol(lowfredDiags)])
lowfredDiagsL = list()
for(i in 1:length(rs)){
  if(rs[i] > 0){
    lowfredDiagsL[i] = 1
  } else{
    lowfredDiagsL[i] = 0
  }
}
lowfredDiags$indicator = as.numeric(lowfredDiagsL)

f4 = factor(lowfredDiags$indicator)
ggplot(lowfredDiags, mapping=aes(x = seq_along(lowfredDiags$Status), y = lowfredDiags$Status, color = f4)) + 
  geom_point(size = 3, shape = 1) + scale_y_continuous(breaks = seq(50, 100, by = 10)) + scale_colour_hue(l=50) + scale_colour_manual(values = c("black", "red")) +
  labs(x = "Patients with Diagnosis", y = "Status", color = "Indicator", title = "Patietnt Status colored by low frequency diagnois")

# Here are all the people who have a mid diagnosis
f5 = factor(midfredDiags$ODD)
ggplot(midfredDiags, mapping=aes(x = seq_along(midfredDiags$Status), y = midfredDiags$Status, color = f5)) + 
  geom_point(size = 3, shape = 1) + scale_y_continuous(breaks = seq(70, 100, by = 10)) + scale_colour_hue(l=50) + scale_colour_manual(values = c("black", "yellow")) +
  labs(x = "Patients with Diagnosis", y = "Status", color = "Indicator", title = "Patietnt Status colored by mid frequency diagnois")

# Here are all the people who have ADHD
f6 = factor(Count_nonzeros$ADHD)
ggplot(Count_nonzeros, mapping=aes(x = seq_along(Count_nonzeros$Status), y = Count_nonzeros$Status, color = f6)) + 
  geom_point(size = 3, shape = 1) + scale_y_continuous(breaks = seq(50, 100, by = 10)) + scale_colour_hue(l=50) + scale_colour_manual(values = c("black", "green")) +
  labs(x = "Patients with Diagnosis", y = "Status", color = "Indicator", title = "Patietnt Status colored by ADHD diagnois")

# Here are all the people who Do have UADHD
f7 = factor(Count_nonzeros$UADHD)
ggplot(Count_nonzeros, mapping=aes(x = seq_along(Count_nonzeros$Status), y = Count_nonzeros$Status, color = f7)) + 
  geom_point(size = 3, shape = 1) + scale_y_continuous(breaks = seq(50, 100, by = 10)) + scale_colour_hue(l=50) + scale_colour_manual(values = c("black", "yellow")) +
  labs(x = "Patients with Diagnosis", y = "Status", color = "Indicator", title = "Patietnt Status colored by UADHD diagnois")


f8 = factor(Count_nonzeros$gender)
ggplot(Count_nonzeros, mapping=aes(x = seq_along(Count_nonzeros$Status), y = Count_nonzeros$Status, color = f8)) + 
  geom_point(size = 3, shape = 1) + scale_y_continuous(breaks = seq(50, 100, by = 10)) + scale_colour_hue(l=50) + scale_colour_manual(values = c("blue", "red", "green")) +
  labs(x = "Patients with Diagnosis", y = "Status", color = "Indicator", title = "Patietnt Status colored by gender")


length(f8)
f9 = f8[!f8==0]
length(f9)
sexStatus = Count_nonzeros[!Count_nonzeros[,3] == 0, ]
summary(lm(sexStatus$Status~f9))

maleStatus = Count_nonzeros[Count_nonzeros[,3] == 'M', ]$Status
femaleStatus = Count_nonzeros[Count_nonzeros[,3] == 'F', ]$Status
plot(maleStatus, ylim = c(75, 95))
plot(femaleStatus, ylim = c(75, 95))


##### Making levels for plot coloring 
sdf = sexStatus[,c(2,3,(ncol(sexStatus)-1))]
min(sdf[1])
max(sdf[1])

g1 = qplot(x = as.numeric(sdf[,3]), fill = factor(sexStatus$gender), geom = "histogram", binwidth = 1, 
           main = "Stautus Counts by Gender",
           xlab = "Status", 
           ylab = "Number of frequency counts",
           col=I("black"))
g1  + scale_fill_manual(values = c("green", "red"), name = "Gender") 

g2 = qplot(x = as.numeric(sdf[,1]), fill = factor(sexStatus$gender), geom = "histogram", binwidth = 1, 
           main = "Age Counts by Gender",
           xlab = "Status", 
           ylab = "Number of frequency counts",
           col=I("black"))
g2  + scale_fill_manual(values = c("green", "red"), name = "Gender") 


##### Not done with this one yet
sdf$interview_age = as.factor(sdf$interview_age)
sdf$gender = as.factor(sdf$gender)

ggplot(data = sdf, aes(x = interview_age, y = Status, fill = gender)) + geom_boxplot() +
labs(title = "Patietnt Status colored by Age")

 
       