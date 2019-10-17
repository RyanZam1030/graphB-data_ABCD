####################################################################################################
########## First I want to upload the raw dataset
####################################################################################################
abcd_ksad01 <- read.delim("~/Downloads/Diags/abcd_ksad01.txt", na.strings= "")
dim(abcd_ksad01)
head(abcd_ksad01)
tail(abcd_ksad01)

####################################################################################################
########## Second I want to sort the dataset by subject ID
####################################################################################################
sort_abcd_ksad01 = abcd_ksad01[order(abcd_ksad01$eventname),]
View(sort_abcd_ksad01)
dim(sort_abcd_ksad01)
head(sort_abcd_ksad01)
tail(sort_abcd_ksad01)

####################################################################################################
########## Removing the last row because it is an unneccessary row
####################################################################################################
sort_abcd_ksad01 = sort_abcd_ksad01[1:((dim(sort_abcd_ksad01)[1])-1),]
dim(sort_abcd_ksad01)
head(sort_abcd_ksad01)
tail(sort_abcd_ksad01)


####################################################################################################
########## Seperate dataset by interview 
####################################################################################################
interviews = split(sort_abcd_ksad01, as.factor(sort_abcd_ksad01$eventname))

interview1 = interviews$baseline_year_1_arm_1
View(interview1)
dim(interview1)
head(interview1)
class(interview1)

interview1.5 = interviews$`1_year_follow_up_y_arm_1`
dim(interview1.5)
head(interview1.5)
class(interview1.5)
View(interview1.5)

####################################################################################################
########## Making the master list of diagnosis and subject IDs
####################################################################################################
diagnosis = colnames(interview1[11:((dim(interview1)[2])-3)])
diagnosisIDs = 0:(length(diagnosis)-1)
diagnosisMat = cbind(diagnosisIDs, diagnosis)

subjects = as.character(interview1[,5])
UserIDs = 1000:(length(subjects)+999)
subjectMat = cbind(UserIDs, subjects)

workMat = rbind(diagnosisMat, subjectMat)
colnames(workMat) = c("Node ID", "User ID")
#colnames(workMat) = c("Node_ID", "User_ID")

dataset_Type = "MasterList"
wpath = getwd()
#wpath = paste(wpath,"Documents","Project", "ABCD", sep = "/")

write.csv(workMat, file = paste(wpath, paste(dataset_Type, "users.csv",sep = "_"), sep = "/"), row.names=FALSE)
print(paste("Your csv file is located in the", wpath, "directory"))
