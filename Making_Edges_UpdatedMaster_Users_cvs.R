####################################################################################################
##### Clears Working Environment 
rm(list=ls())

####################################################################################################
########## First I want to upload the raw dataset
####################################################################################################
abcd_ksad01 <- read.delim("~/Downloads/Diags/abcd_ksad01.txt", na.strings= "")
dim(abcd_ksad01)
View(abcd_ksad01)

####################################################################################################
########## Second I want to sort the dataset by subject ID
####################################################################################################
sort_abcd_ksad01 = abcd_ksad01[order(abcd_ksad01$eventname),]
dim(sort_abcd_ksad01)
View(sort_abcd_ksad01)

####################################################################################################
########## Removing the last row because it is an unneccessary row
####################################################################################################
sort_abcd_ksad01 = sort_abcd_ksad01[1:((dim(sort_abcd_ksad01)[1])-1),]
dim(sort_abcd_ksad01)

####################################################################################################
########## Seperate dataset by interview 
####################################################################################################
interviews = split(sort_abcd_ksad01, as.factor(sort_abcd_ksad01$eventname))

interview1 = interviews$baseline_year_1_arm_1
dim(interview1)

interview1.5 = interviews$`1_year_follow_up_y_arm_1`
dim(interview1.5)


####################################################################################################
########## Steps for setting up preprocess datasets
####################################################################################################

##### Reading in the Master users csv file
# Might need to include the updated master_users list 
userID_MasterList = read.csv("~/Documents/Project/ABCD/MasterList_users.csv")
dim(userID_MasterList)
View(userID_MasterList)

# These are the diagnostics of interest we will be using for analysis
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


####################################################################################################
# Step 1. Select the dataset that you want by interview type, diagnosis, specified qualities
####################################################################################################
## These need to be filled in change datset_num from 1 to 1.5 and workData to interview 1 to interview1.5
dataset_num = 1
workData = interview1
diagnosis_of_interest = presentDiags
dataset_Type = "Interview"

# Should be the same for all interviews
subset_workData = workData[,c(4,7,8,11:(ncol(workData)))]
View(subset_workData)

# This selects of the present diagnosis. Could be used to select specific diagnosis
present_workData = cbind(subset_workData[,1:3],subset_workData[,presentDiags])

# Gets your current working directory 
wpath = getwd()
# Uncomment to set up working directory
#wpath = paste(wpath,"Documents","Project", "ABCD", sep = "/")
write.csv(present_workData, file = paste(wpath, paste("Interview", dataset_num, "All_Present_Diagnosis_subset.csv",sep = "_"), sep = "/"), row.names=FALSE)
print(paste("Your csv file is located in the", wpath, "directory"))


####################################################################################################
# Step 2. Remove any rows and columns with All NA values and writing subset for analysis
####################################################################################################
# Removing rows
present_workData = present_workData[rowSums(is.na(present_workData[,4:length(presentDiags)])) < (ncol(present_workData[,4:length(presentDiags)])-1),]
dim(present_workData)
#Removing columns
present_workData = present_workData[,colSums(is.na(present_workData)) < (nrow(present_workData)-1)]
dim(present_workData)
View(present_workData)

####################################################################################################
# Step 3. Make the users.csv
####################################################################################################
uMat = matrix(data = NA, nrow = nrow(present_workData) + (ncol(present_workData) - 3), ncol = 2)
colnames(uMat) = c("Node ID", "User ID")
#colnames(uMat) = c("Node_ID", "User_ID")
uMat[,2] = c(colnames(present_workData[,-c(1,2,3)]), as.character(present_workData[,1]))
uMat[,1] = userID_MasterList$Node.ID[match(uMat[,2],userID_MasterList$User.ID)]
uMat = uMat[order(as.numeric(uMat[,1])),]
View(uMat)

###### Performed these next couple of steps becuase the users in interview 1.5 had new ids
# Checking to see if there are any new User ID not on Master list
# tail(uMat, n=10)
# colnames(userID_MasterList) = c("Node ID", "User ID")
# newIds = subset(uMat,is.na(uMat[,1]))
# newIds[1:nrow(newIds), 1] = (tail(userID_MasterList[,1], 1) + 1):(tail(userID_MasterList[,1], 1) + nrow(newIds))  
# updated_userID_MasterList = rbind(userID_MasterList, newIds)
# dim(updated_userID_MasterList)
# write.csv(updated_userID_MasterList, file = paste(wpath, paste("updated_MasterList_from_dataset", dataset_num, "users.csv",sep = "_"), sep = "/"), row.names=FALSE)
# print(paste("Your csv file is located in the", wpath, "directory"))
# 
# # Redo-ing uMat
# userID_MasterList = read.csv("~/Documents/Project/ABCD/updated_MasterList_from_dataset_1.5_users.csv")
# uMat[,2] = c(colnames(present_workData[,-c(1,2,3)]), as.character(present_workData[,1]))
# uMat[,1] = userID_MasterList$Node.ID[match(uMat[,2],userID_MasterList$User.ID)]
# uMat = uMat[order(as.numeric(uMat[,1])),]

write.csv(uMat, file = paste(wpath, paste(dataset_Type, dataset_num, "users.csv",sep = "_"), sep = "/"), row.names=FALSE)
print(paste("Your csv file is located in the", wpath, "directory"))

####################################################################################################
# Step 4. Make the edges.csv
####################################################################################################
present_workData$sortingVar = userID_MasterList$Node.ID[match(present_workData[,1], userID_MasterList$User.ID)]
present_workData = present_workData[order(as.numeric(present_workData[,"sortingVar"])),]
present_workData = present_workData[,-(dim(present_workData)[2])]


eMat = matrix(data = NA, nrow = dim(present_workData)[1] * (dim(present_workData)[2] - 3), ncol = 3)
colnames(eMat) = c("From Node ID", "To Node ID", "Edge Weight")
#colnames(eMat) = c("From_Node_ID", "To_Node_ID", "Edge_Weight")
eMat[,1] = rep(as.character(present_workData[,1]), each = (dim(present_workData)[2] - 3))
eMat[,1] = userID_MasterList$Node.ID[match(eMat[,1], userID_MasterList$User.ID)]
eMat[,2] = rep(colnames(present_workData[4:(dim(present_workData)[2])]), dim(present_workData)[1])
eMat[,2] = userID_MasterList$Node.ID[match(eMat[,2], userID_MasterList$User.ID)]
eMat[,3] = as.numeric(t(present_workData[,-c(1,2,3)]))
eMat[,3][eMat[,3]==0] = -1  
dim(eMat)
eMat = eMat[complete.cases(eMat),]
dim(eMat)
write.csv(eMat, file = paste(wpath, paste(dataset_Type, dataset_num, "edges.csv", sep = "_"), sep = "/"), row.names=FALSE)
print(paste("Your csv file is located in the", wpath, "directory"))

####################################################################################################
# Make a list of names for present diagnosis, past diagnosis, and ect 
