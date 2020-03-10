#install.packages("expss")
#install.packages('data.table')
#install.packages("dplyr")
library(expss) #for count_if function
library(dplyr)

getwd()
setwd("~/Documents/BCAC_QC/Core/summ_stat_files") #Set working directory

# READ DATA----------------------------------------------------------------------------------------------------------------
file1<- "AHS_core.csv"    # Enter filename
file2<- "PLCO_core.csv"    # Enter filename
file3<- "PBCS_core.csv"    # Enter filename
file4<- "USRT_core.csv"    # Enter filename

read_data1 <- read.csv(file1)
read_data2 <- read.csv(file2)
read_data3 <- read.csv(file3)
read_data4 <- read.csv(file4)

data1 <- data.frame(read_data1)
data2 <- data.frame(read_data2)
data3 <- data.frame(read_data3)
data4 <- data.frame(read_data4)

#Define key value pairs for each variable name to get it's corresponding number---------------------------------------------------
# ie. > status[[1]]
#[1] "case"

studyType_name= c("sporadic", "familial", "other", "control", "DK")
studyType_num= c(0, 1, 2, 777, 888)
status_name= c("case", "control")
status_num= c(1,0)
eth_name= c("European", "Hispanic", "African", "Asian", "South East Asian", "Other", "DK")
eth_num = c(1,2,3,4,5,6,888)
fHist_name= c("no","yes", "DK")
fHist_num= c(0, 1, 888)
# fhnumber_name= c( "1", "2", "3", "4", "5", "DK") ##-------------FIX add more than 5??
# fhnumber_num= c( 1,2, 3, 4, 5, 888)
ER_name= c("negative", "positive", "DK", "blank")
ER_num= c(0, 1,888, NA)

studyType1 <- setNames(as.list(studyType_num), studyType_name)
status1 <- setNames(as.list(status_num), status_name)
ethnicityClass1 <- setNames(as.list(eth_num), eth_name)
fHist1 <- setNames(as.list(fHist_num), fHist_name)
fhnumber1 <- setNames(as.list(fhnumber_num), fhnumber_name)
ER1 <- setNames(as.list(ER_num), ER_name)


# make ageCols function to fill age columns--------------------------------------------------------------------------------------------------------

ageCols = function(data, studyType2, status2, ethnicityClass2, ageRange){
  row= c()
  
  for (age in ageRange){
    findages<-nrow(filter(data, studyType==studyType2 & status ==status2 & ethnicityClass==ethnicityClass2, ageInt==age))
    row<- append(row,findages)
    row<-(sum(row))
  }
  return(row)
}
colnames(data1)
twent <- ageCols(data1, 777, 1, 1, 20:30 )

### MAKE SUMMARY STATISTICS DATAFRAME USING make.DF FUNCTION---------------------------------------------------------------------------------------

make.DF = function(data_x){
  
  #MAKE NEW DATAFRAME (newdata): of COLUMNS WE NEED FROM THE CORE DATA
  
  columns <- c("studyType", "study", "status" , "ethnicityClass", "ageInt","famHist", "fhnumber", "ER_statusIndex")
  newdata <- data_x[columns]
  
  ## Define 3 subclasses: study, status, ethnicity 
  studyType <- character()
  status <- character()
  ethnicityClass <- character()
  
  #3 for loops to make first 3 columns
  
  for (a in eth_name){
    for (b in status_name){
      for (c in studyType_name){
        
        studyType <- c(studyType, c)
        status <- c(status,b)
        ethnicityClass <- c(ethnicityClass,a)
        
        ############## MAKE NEW DATAFRAME for consortia and study(df1)
        summData <- data.frame(studyType, status, ethnicityClass)
        
        # Add consortia and study columns to this  new data frame
        summData$consortium = "BCAC"
        summData$study = newdata$study[[1]]   #********grabbing the first row in study, can only add dataframes by study
        df1<- summData[,c(4,5,1:3)]
        
        ############## MAKE NEW EMPTY DATAFRAME for sum columns (df2)
        df2<-data.frame(matrix(nrow= nrow(df1),ncol=15))
        colnames(df2)<-c("age20:29", "age30:39", "age40:49", "age50:59", "age60:69", "age70:79", "age80:89", "age90:99", "ER_statusIndex_pos", "ER_statusIndex_neg", "ER_statusIndex_DK", "ER_statusIndex_NA", "famHist_yes", "famHist_no", "famHist_DK")
        
        ##### COMBINE BOTH DATA FRAMES INTO 1 (df)
        df<- cbind(df1,df2)
        
      }
    }
  } 
  return(df)}

#Add data to empty dataframes using addData function----------------------------------------------------------------------------------------------------------------------------------------

add.Data <- function (data_x, df_x){
  
  #MAKE NEW DATAFRAME (newdata): of COLUMNS WE NEED FROM THE CORE DATA
  
  columns <- c("studyType", "study", "status" , "ethnicityClass", "ageInt","famHist", "fhnumber", "ER_statusIndex")
  newdata <- data_x[columns]
  
  ERrow= 1
  while(ERrow<71){
    
    for (a in eth_name){
      for (b in status_name){
        for (c in studyType_name){
          
          #-----add age data (20-109? or 99?)

          age20s<- ageCols(newdata, studyType1[[c]], status1[[b]], ethnicityClass1[[a]], ageRange = 20:29 )
          df_x[ERrow,6]<- age20s

          age30s<- ageCols(newdata,studyType1[[c]], status1[[b]], ethnicityClass1[[a]], ageRange = 30:39 )
          df_x[ERrow,7]<- age30s

          age40s<- ageCols(newdata,studyType1[[c]], status1[[b]], ethnicityClass1[[a]], ageRange = 40:49 )
          df_x[ERrow,8]<- age40s

          age50s<- ageCols(newdata,studyType1[[c]], status1[[b]], ethnicityClass1[[a]], ageRange = 50:59 )
          df_x[ERrow,9]<- age50s

          age60s<- ageCols(newdata,studyType1[[c]], status1[[b]], ethnicityClass1[[a]], ageRange = 60:69 )
          df_x[ERrow,10]<- age60s

          age70s<- ageCols(newdata,studyType1[[c]], status1[[b]], ethnicityClass1[[a]], ageRange = 70:79 )
          df_x[ERrow,11]<- age70s

          age80s<- ageCols(newdata,studyType1[[c]], status1[[b]], ethnicityClass1[[a]], ageRange = 80:89 )
          df_x[ERrow,12]<- age80s

          age90s<- ageCols(newdata,studyType1[[c]], status1[[b]], ethnicityClass1[[a]], ageRange = 90:109 )
          df_x[ERrow,13]<- age90s
          
          #-----add ER (positive, negative, don't know, blank) data
          
          ER_yes<-nrow(filter(data_x, studyType==studyType1[[c]] & status ==status1[[b]] & ethnicityClass==ethnicityClass1[[a]], ER_statusIndex ==1 ))
          df_x[ERrow,14]<- ER_yes
          
          ER_no<-nrow(filter(data_x, studyType==studyType1[[c]] & status ==status1[[b]] & ethnicityClass==ethnicityClass1[[a]], ER_statusIndex ==0 ))
          df_x[ERrow,15]<- ER_no
          
          ER_DK<-nrow(filter(data_x, studyType==studyType1[[c]] & status ==status1[[b]] & ethnicityClass==ethnicityClass1[[a]], ER_statusIndex ==888 ))
          df_x[ERrow,16]<- ER_DK
          
          ER_NA<-nrow(filter(data_x, studyType==studyType1[[c]] & status ==status1[[b]] & ethnicityClass==ethnicityClass1[[a]], ER_statusIndex== is.na(ER_statusIndex )))
          df_x[ERrow,17]<- ER_NA
          
          #-----add family history data (yes, no, don't know)
          
          fHist_yes<-nrow(filter(data_x, studyType==studyType1[[c]] & status ==status1[[b]] & ethnicityClass==ethnicityClass1[[a]], famHist ==1 ))
          df_x[ERrow,18]<- fHist_yes
          
          fHist_no<-nrow(filter(data_x, studyType==studyType1[[c]] & status ==status1[[b]] & ethnicityClass==ethnicityClass1[[a]], famHist==0 ))
          df_x[ERrow,19]<- fHist_no
          
          fHist_DK<-nrow(filter(data_x, studyType==studyType1[[c]] & status ==status1[[b]] & ethnicityClass==ethnicityClass1[[a]], famHist ==888 ))
          df_x[ERrow,20]<- fHist_DK
          
          ERrow = ERrow+1
          
        }
      }
    }
  }
  return(df_x)}

# # read in multiple files -------------------------------------------------------------------------------------------------------------------------------------
# # https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
# temp = list.files(pattern="*.csv")
# myfiles = lapply(temp, read.delim)
# 
# #make 4 df from 4 files --------------------------------------------------------------------------------------------------------------------------------------
# #https://stackoverflow.com/questions/29402528/append-data-frames-together-in-a-for-loop/29419402
# datalist = list()
# 
# for (i in 1:length(temp)){
#   
#   i <- data.frame(read.csv(temp[[i]]))
#   df<-make.DF(data_x= i)
#   df<-add.Data(data_x= i, df_x= df)
#   df$i<- i
#   datalist[[i]]<- df
#   #df <- rbind(df)
#   #df_list = append(df_list, df)
# }

df1<-make.DF(data_x= data1)
df1<-add.Data(data_x= data1, df_x= df1)

df2<-make.DF(data_x= data2)
df2<-add.Data(data_x= data2, df_x= df2)

df3<-make.DF(data_x= data3)
df3<-add.Data(data_x= data3, df_x= df3)

df4<-make.DF(data_x= data4)
df4<-add.Data(data_x= data4, df_x= df4)


## combine all studies (df 1-4) using r bind
df <- rbind(df1, df2, df3, df4)
### SAVE DATA AS TXT OR CSV (CSV saves ages 30:40 as hours 30:40:00)
#txt----------------------write.table(df, "summary_statistics_AHS", sep = ",")

write.csv(df, "summary_statistics_AHS_core14.csv", row.names = FALSE)

