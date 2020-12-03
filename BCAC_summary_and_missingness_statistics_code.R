# VERSION: 1.0
# LAST UPDATED: DECEMBER 2, 2020
# AUTHOR: LORENA SANDOVAL
# EMAIL: lorena.sandoval@nih.gov
#--------------------------------------------------------------------------------------------
# CONFLUENCE SUMMARY STATISTICS - BCAC

# PURPOSE: TO GENERATE CSV FILE THAT CONTAINS COUNT DATA OF CONFLUENCE CORE VARIABLES THAT ARE PRESENTED
#          ON THE CONFLUENCE DATA PLATFORM'S (https://episphere.github.io/confluence/?#) PUBLIC LANDING PAGE
#          AND THE DATA EXPLORE PAGE.

# INPUT REQUIREMENTS: Match variables by letter and casing:"study" , "status",  "sex" , "ageInt" , "fhscore" , "fhnumber" , "famHist" , "ER_statusIndex", "Genotyping_chip", "ethnicityClass", "studyDesign"

# Input options for studyDesign:

# Input for studyDesign:
# Here is the update values for StudyDesign:
# 1) "Nested case-control": a case-control study nested in a prospective cohort study
# 2) "Population-based case-control studies": eligible cases are all (or a random sample of) cases occurring in a geographically defined population during a specified period, and controls are a random sample of the same source population as cases, recruited during the same period
# 3) "Hospital-based case-control": eligible cases are all cases diagnosed in a given hospital or hospitals during a specified period of time,and controls are either population-based controls selected from the catchment area of cases during the same period of time OR, for example, hospital-based controls, with appropriate selection criteria 
# 4) "Mixed": all other studies with cases and controls (e.g. selected cases, blood donor controls)
# 5) "Case-series": studies including only patients without controls

# DETAILED EXPLANATION OF SUMMARY STATISTICS 

#[1a] Install packages if not already installed and load their corresponding libraries
#[2a] Make sure data input requirements are met
#[3a] Create READ.DATA function to preprocess data removing whitespace, rounding "ageInt" decimals down, make list of study names, make all case types to 1 and correct "FALSE" in the "sex" column
#[4a] Create READ.AGE function used in READ.DATA function
#[5a] Create MAKE.DF function to create an empty dataframe
#[6a] Create ADD.DATA function to add summary data to empty dataframe
#[7a] Create MAKE.SUMMSTAT function to call above functions
#[8a] SPecify box ids and df names to run in MAKE.SUMMSTAT, then combine new and old summary statistics files using rowbind (rbind)
#[9a] Upload the combined file to box using the box file ID of the file to be replaced
#--------------------------------------------------------------------------------------------
# CONFLUENCE MISSINGNESS STATISTICS - BCAC

# PURPOSE: TO GENERATE CSV FILE THAT CONTAINS BINARY DATA OF PRESENT/ABSENT CONFLUENCE CORE VARIABLES THAT ARE
#          DISPLAYED ON THE CONFLUENCE DATA PLATFORM'S (https://episphere.github.io/confluence/?#) DATA EXPLORE PAGE.

# INPUT REQUIREMENTS: Match variables by letter and casing: "status","ageInt", "sex","ethnicityClass","famHist","fhscore","ER_statusIndex"

# DETAILED EXPLANATION OF MISSINGNESS STATISTICS:

#[1b] Install packages if not already installed and call their libraries
#[2b] Create MAKE.MISS.STAT function

#--------------------------------------------------------------------------------------------
# SAVE CONFLUENCE BCAC SUMMARY AND MISSINGNESS STATISTICS DIRECTLY TO BOX USING BOXR

#[1c] Call summary or missingness statistics functions to create summary and or missingness output dataframes
#[2c] If adding new studies to the current statistics, used old_df to refer to the previous statistics file and combine with new df1, df2, etc
#[3c] If making statistics file from completely new data, use df1, df2, etc and combine them using r bind

#--------------------------------------------------------------------------------------------
###################################### BEGIN SUMMARY STATISTICS CODE ########################
#[1a]
install.packages("expss")
install.packages(c("boxr", "base", "usethis"))
install.packages("dplyr")
library(boxr)
library(expss) #count_if function
library(dplyr)
library(devtools)
library(tibble)


#[3a] Function to read box IDs
READ.DATA= function(box_id){
 
          # Read box id 
          data = box_read(box_id)
          
          # Preprocess data: Removing whitespace, rounding "ageInt" decimals down, and correcting "F" turning into FALSE in the "sex" column
          # Fix:  F turns to FALSE in R
          data$sex[data$sex=="FALSE"]<-"F"  
          
          # Remove leading or trailing white space from each column using the following "trim function
          trim <- function (x) gsub("^\\s+|\\s+$", "", x) 
          data$study<- trim(data$study)
          data$status<- trim(data$status)
          data$sex<- trim(data$sex)
          data$ageInt<- trim(data$ageInt)
          data$fhscore<- trim(data$fhscore)
          data$fhnumber<- trim(data$fhnumber)
          data$famHist<- trim(data$famHist)
          data$ER_statusIndex<- trim(data$ER_statusIndex)
          data$Genotyping_chip<- trim(data$Genotyping_chip)
          data$ethnicityClass<- trim(data$ethnicityClass)
          
          # Round ageInt numbers down
          data$ageInt<-floor(as.numeric(data$ageInt))
          
          # Get study names into a list
          study_name<- factor(data$study)
          study_name= levels(study_name)
          
          # Convert all case types to 1
          data$status[data$status!=0] <-1
          return(data)
      }


#[4a] Function used ADD.DATA function to read fill ages columns ---------------------------------------------------------------------------------------
READ.AGE = function(data, chip2, study2, status2, ethnicityClass2, sex2, ageRange){
  row= c()
  for (age in ageRange){
    findages<-nrow(filter(data, Genotyping_chip==chip2, study== study2,status ==status2 ,ethnicityClass==ethnicityClass2,sex==sex2, ageInt==age))
    row<- append(row,findages)
    row<-(sum(row))
  }
  return(row)
}


#[5a] Function to make empty dataframe ---------------------------------------------------------------------------------------
MAKE.DF = function(data){
  
  # Define function variables

  study_name = factor(data$study)
  study_name = levels(study_name)
  
  data$status[data$status!=0] <-1
  
  chip_name= c("Confluence chip","Other chip")
  chip_num= c(1,0)
  studyDesign_name= c("Nested case-control","Population-based case-control","Hospital-based case-control","Mixed","Case-series")
  studyDesign_num= c(1,2,3,4,5,6)
  status_name= c("case", "control")
  status_num= c(1,0)
  eth_name= c("European", "Hispanic", "African", "Asian", "South East Asian", "Other", "don't know")
  eth_num = c(1,2,3,4,5,6,888)
  fHist_name= c("no","yes", "don't know")
  fHist_num= c(0, 1, 888)
  ER_name= c("negative", "positive", "don't know", "blank")
  ER_num= c(0, 1,888, NA)
  sex_name= c("female", "male", "unknown")
  sex_num= c("F", "M", "U")
  chip1 <- setNames(as.list(chip_num), chip_name)
  #studyDesign1 <- setNames(as.list(studyDesign_name), studyDesign_num) #reverse from rest
  status1 <- setNames(as.list(status_num), status_name)
  ethnicityClass1 <- setNames(as.list(eth_num), eth_name)
  fHist1 <- setNames(as.list(fHist_num), fHist_name)
  ER1 <- setNames(as.list(ER_num), ER_name)
  sex1 <- setNames(as.list(sex_num), sex_name)
  ## Define subclasses: status ,study, chip, ethnicity
  sex <- character()
  status <- character()
  ethnicityClass <- character()
  study <- character()
  chip<- character()
  
  #5 for loops to make first 7 columns
  for (d in chip_name){
    for (e in study_name){
      for (a in eth_name){
        for (b in status_name){
          for (c in sex_name){
            
            
            # make DATAFRAME for consortia,studyDesign, study,ethnicityClass,status, statusTotal 
            chip <- c(chip, d) # column 1
            study <- c(study, e)     # column 2
            ethnicityClass <- c(ethnicityClass,a) # column 3
            sex <- c(sex,c) # column 4
            status <- c(status,b) # column 5
            
            df1 <- data.frame(chip, study, ethnicityClass,sex, status)
            df1$statusTotal = ""  # column 6
            df1$consortium = "BCAC" # column 7
            df1$studyDesign =""  # column 8
            df1<- df1[,c(7,1,2,8,3:6)] #rearrange the columns
            
            # make EMPTY DATAFRAME (df2) for variables totals
            df2<-data.frame(matrix(nrow= nrow(df1),ncol=16))
            colnames(df2)<-c("age10:19","age20:29", "age30:39", "age40:49", "age50:59", "age60:69", "age70:79", "age80:89", "age90:99", "ageDK", 
                             "ER_statusIndex_pos", "ER_statusIndex_neg", "ER_statusIndex_DK", "famHist_yes", "famHist_no", "famHist_DK" 
            ) # "Genotyping_chip_no", "Genotyping_chip_yes") # "ER_statusIndex_NA",
            
            # COMBINE BOTH DATA FRAMES INTO 1 (df)
            df<- cbind(df1,df2)
          }
        }
      }
    }
  }
  return(df)}

#[6a] FUnction to fill in empty dataframe----------------------------------------------------------------------------------------------------------
ADD.DATA <- function (data, df){
        row= 1
        study_name = factor(data$study)
        study_name = levels(study_name)
        chip_name= c("Confluence chip","Other chip")
        chip_num= c(1,0)
        # add studyDesign later on. Will manually add for now. 
        studyDesign_name= c("Nested case-control","Population-based case-control","Hospital-based case-control","Mixed","Case-series")
        studyDesign_num= c(1,2,3,4,5)
        status_name= c("case", "control")
        status_num= c(1,0)
        eth_name= c("European", "Hispanic", "African", "Asian", "South East Asian", "Other", "don't know")
        eth_num = c(1,2,3,4,5,6,888)
        fHist_name= c("no","yes", "don't know")
        fHist_num= c(0, 1, 888)
        ER_name= c("negative", "positive", "don't know", "blank")
        ER_num= c(0, 1,888, NA)
        sex_name= c("female", "male", "unknown")
        sex_num= c("F", "M", "U")
        
        chip1 <- setNames(as.list(chip_num), chip_name)
        studyDesign1 <- setNames(as.list(studyDesign_name), studyDesign_num) #reverse from rest
        status1 <- setNames(as.list(status_num), status_name)
        ethnicityClass1 <- setNames(as.list(eth_num), eth_name)
        fHist1 <- setNames(as.list(fHist_num), fHist_name)
        ER1 <- setNames(as.list(ER_num), ER_name)
        sex1 <- setNames(as.list(sex_num), sex_name)
        
        ## Define subclasses: status ,study, chip, ethnicity
        sex <- character()
        status <- character()
        ethnicityClass <- character()
        study <- character()
        chip<- character()
        study_name = factor(data$study)
        study_name = levels(study_name)
        
        while(row<28){
          for (d in chip_name){
            for (e in study_name){
              for (a in eth_name){
                for (b in status_name){
                  for (c in sex_name){
                    
                    #-----add studyDesign rows
                    st=filter(data, study==e,studyDesign !=777,studyDesign !=888)
                    st1=levels(factor(st$studyDesign))
                    st2= paste(st1,sep="")
                    # if(length(st1)==1){
                    #        st2= paste(st1,sep="")
                    #   }else if(length(st1>1)){
                    #        st2= paste(555,sep="")
                    #   }else {paste(666, sep = "")}
                    
                    df[row,4]<- studyDesign1[[st2]]
                    
                    #-----add status Totals
                    statusTotal<- nrow(filter(data,Genotyping_chip==chip1[[d]],study==e,status ==status1[[b]], ethnicityClass==ethnicityClass1[[a]], sex==sex1[[c]]))
                    df[row,8]<- statusTotal
                    
                    #-----add age data
                    age10s<- READ.AGE(data, chip1[[d]], e, status1[[b]], ethnicityClass1[[a]], sex1[[c]], ageRange = 10:19 )
                    df[row,9]<- age10s
                    age20s<- READ.AGE(data, chip1[[d]], e, status1[[b]], ethnicityClass1[[a]], sex1[[c]], ageRange = 20:29 )
                    df[row,10]<- age20s
                    age30s<- READ.AGE(data, chip1[[d]],e,status1[[b]], ethnicityClass1[[a]], sex1[[c]],ageRange = 30:39 )
                    df[row,11]<- age30s
                    age40s<- READ.AGE(data, chip1[[d]],e,status1[[b]], ethnicityClass1[[a]], sex1[[c]],ageRange = 40:49 )
                    df[row,12]<- age40s
                    age50s<- READ.AGE(data, chip1[[d]],e,status1[[b]], ethnicityClass1[[a]], sex1[[c]],ageRange = 50:59 )
                    df[row,13]<- age50s
                    age60s<- READ.AGE(data, chip1[[d]],e , status1[[b]], ethnicityClass1[[a]], sex1[[c]],ageRange = 60:69 )
                    df[row,14]<- age60s
                    age70s<- READ.AGE(data, chip1[[d]],e,status1[[b]], ethnicityClass1[[a]], sex1[[c]],ageRange = 70:79 )
                    df[row,15]<- age70s
                    age80s<- READ.AGE(data, chip1[[d]],e,status1[[b]], ethnicityClass1[[a]], sex1[[c]],ageRange = 80:89 )
                    df[row,16]<- age80s
                    age90s<- READ.AGE(data, chip1[[d]],e,status1[[b]], ethnicityClass1[[a]], sex1[[c]],ageRange = 90:99 )
                    df[row,17]<- age90s
                    ageDK<- nrow(filter(data, Genotyping_chip==chip1[[d]],study==e, status ==status1[[b]], ethnicityClass==ethnicityClass1[[a]],sex==sex1[[c]],  ageInt ==888 ))
                    df[row,18]<- ageDK
                    
                    #-----add ER data (positive, negative, don't know, blank)
                    ER_pos<-nrow(filter(data, Genotyping_chip==chip1[[d]],study==e, status ==status1[[b]],ethnicityClass==ethnicityClass1[[a]],sex==sex1[[c]],  ER_statusIndex ==1 ))
                    df[row,19]<- ER_pos
                    ER_neg<-nrow(filter(data, Genotyping_chip==chip1[[d]],study==e, status ==status1[[b]],ethnicityClass==ethnicityClass1[[a]],sex==sex1[[c]],  ER_statusIndex ==0 ))
                    df[row,20]<- ER_neg
                    ER_DK<-nrow(filter(data, Genotyping_chip==chip1[[d]],study==e, status ==status1[[b]],ethnicityClass==ethnicityClass1[[a]],sex==sex1[[c]],  ER_statusIndex ==888 ))
                    df[row,21]<- ER_DK
                    # ER_NA<-nrow(filter(data, Genotyping_chip==chip1[[d]],study==e, status ==status1[[b]] & ethnicityClass==ethnicityClass1[[a]],sex==sex1[[c]],  is.na(ER_statusIndex )))
                    # df[row,20]<- ER_NA
                    #-----add family history data (yes, no, don't know)
                    fHist_yes<-nrow(filter(data, Genotyping_chip==chip1[[d]],study==e, status ==status1[[b]], ethnicityClass==ethnicityClass1[[a]],sex==sex1[[c]],  famHist ==1 ))
                    df[row,22]<- fHist_yes
                    fHist_no<-nrow(filter(data, Genotyping_chip==chip1[[d]],study==e, status ==status1[[b]] ,ethnicityClass==ethnicityClass1[[a]],sex==sex1[[c]],  famHist==0 ))
                    df[row,23]<- fHist_no
                    fHist_DK<-nrow(filter(data, Genotyping_chip==chip1[[d]],study==e,  status ==status1[[b]], ethnicityClass==ethnicityClass1[[a]],sex==sex1[[c]],  famHist ==888 ))
                    df[row,24]<- fHist_DK
                    
                    #print(row)
                    row = row+1
            }
          }
        }
      }
    }
  }
  return(df)}

#[7a] Function to run Summary statistics 

MAKE.SUMMSTAT = function(box_id){
                    
                    data = READ.DATA(box_id)
                    empty_df = MAKE.DF(data) # uses READ.AGE function
                    df = ADD.DATA(data, empty_df)
                    return(df)
                    }
                    
###################################### END SUMMARY STATISTICS CODE ##################################################
###################################### BEGIN MISSINGNESS STATISTICS CODE ############################################

#[1]------------------------------------------------------------------------
install.packages("fastDummies")
install.packages("stringi")
install.packages(c("boxr", "base", "usethis"))
install.packages("tidyverse")
install.packages("tibble")
install.packages("dplyr")
library(fastDummies)
library(stringi)
library(boxr)
library(tidyverse)
library(tibble) #add_column
library(dplyr) #select

#[2]------------------------------------------------------------------------


MAKE.MISS.STAT <-function(box_id){
              data <- box_read(box_id)
              #trim leading and trailing white space from sex column
              trim <- function (x) gsub("^\\s+|\\s+$", "", x)
              data$sex<- trim(data$sex)
              data$sex[data$sex=="FALSE"]<-"F"  #fix when F turns into FALSE
              
              # Replace numbers with word definitions from confluence data dictionary
              data$status[data$status==0] <-"control"
              data$status[data$status==1] <-"case"
              data$status[data$status==2] <-"case"
              data$status[data$status==3] <-"case"
              data$status[data$status==9] <-"missing" #check with Montse (BCAC CIMBA) , drop 9? ask Jean, add to QAQC?
              
              #If input has blanks, replace with 888
              data$ethnicityClass[is.na(data$ethnicityClass)]<- 888
              data$ethnicityClass[data$ethnicityClass==1] <-"European"
              data$ethnicityClass[data$ethnicityClass==2] <-"Hispanic" 
              data$ethnicityClass[data$ethnicityClass==3] <-"African"
              data$ethnicityClass[data$ethnicityClass==4] <-"Asian Subcontinent"
              data$ethnicityClass[data$ethnicityClass==5] <-"South-East Asian"
              data$ethnicityClass[data$ethnicityClass==6] <-"Other"
              data$ethnicityClass[data$ethnicityClass==888] <-"missing" 
              
              data$famHist[data$famHist==0] <-"Data available"
              data$famHist[data$famHist==1] <-"Data available"
              data$famHist[data$famHist==888] <-"missing"
              
              data$fhscore[data$fhscore!=888] <-"Data available"
              data$fhscore[data$fhscore == 888] <-"missing"
              
              data$fhnumber[data$fhnumber!=888] <-"Data available"
              data$fhnumber[data$fhnumber==888] <-"missing"
              
              #If status is control ER should be NA
              data$ER_statusIndex[data$status==0]<- NA
              data$ER_statusIndex[data$ER_statusIndex==0] <-"Data available"
              data$ER_statusIndex[data$ER_statusIndex==1] <-"Data available"
              data$ER_statusIndex[data$ER_statusIndex==888] <-"missing"
              data$ER_statusIndex[is.na(data$ER_statusIndex)] <-"missing"
              
              data$sex[data$sex=="M"] <-"Data available"
              data$sex[data$sex=="F"] <-"Data available"
              data$sex[data$sex=="U"] <- "missing" 
              
              data$ageInt[data$ageInt!=888]<- "Data available"
              data$ageInt[data$ageInt==888] <-"missing"
              
              # risk factor broad categories-------------------------------
              data$Lifestyle<- "missing"
              idx = which(data$"eduCat"!=888 | !is.na(data$"eduComments") |
                            data$"alcoholFreq" !=888|data$"alcoholCum" !=888 |
                            data$"smokingEver" !=888| data$"SmoRegStartAge"!=888)
              data$Lifestyle[idx] ="Data available"
              #-------------------------------
              data$"Reproductive history"<- "missing"
              idx = which(data$"AgeMenarche"!=888 | data$"mensAgeLast"!= 888 |
                            data$"mensRsn" !=888 |data$"MenoStat" !=888 |
                            data$"parous" !=888 | data$"parity"!=888|
                            data$"AgeFFTP" !=888 |data$"lastChildAge" !=888 |
                            data$"breastfed" !=888 |data$"breastMos" !=888) 
              data$"Reproductive history"[idx] ="Data available"
              #-------------------------------
              data$Anthropometry<- "missing"
              idx = which(data$"weight"!=888 |data$"height" !=888 |data$"BMI" !=888) 
              data$Anthropometry[idx]<- "Data available"
              #-------------------------------
              data$"Hormone use"<- "missing"
              idx = which(data$"OCEver"!=888 |data$"OCCurrent" !=888 |data$"OCMo" !=888|
                          data$"HRTEver"!=888 |data$"HRTCurrent" !=888 |data$"EPEver" !=888| 
                          data$"EPCurrent"!=888 |data$"EPMo" !=888 |data$"ECurrent" !=888| 
                          data$"EMo"!=888)
              data$"Hormone use"[idx]<- "Data available"
              #-------------------------------
              data$"Detailed family history"<- "missing"
              idx = which(data$"sisters"!=888 |data$"brCancerSis" !=888 |data$"ovCancerSis" !=888|
                          data$"daughters"!=888 |data$"brCancerDau" !=888 |data$"ovCancerDau" !=888| 
                          data$"brCancerMom"!=888 |data$"ovCancerMom" !=888 |data$"brCancerDad" !=888| 
                          data$"FHisFstBC"!=888 |data$"FHisFstBCNr" !=888 |data$"FHisSecBC" !=888| 
                          data$"FHisSecBCNr"!=888 |data$"FHisFstOC" !=888 |data$"FHisFstOCNr" !=888|                             
                          data$"HRTEver"!=888 |data$"HRTCurrent" !=888 |data$"EPEver" !=888| 
                          data$"FHisSecOC"!=888 |data$"FHisSecOCNr" !=888 |data$"fam1grBC50" !=888| 
                          data$"fam1grOC50"!=888)
              data$"Detailed family history"<- "Data available"
              #-------------------------------
              data$"Benign breast disease"<- "missing"
              idx = which(data$"Biopsies_number"!=888 |data$"BBD_history" !=888 |data$"BBD_number" !=888|
                            data$"BBD_type1"!=888)
              data$"Benign breast disease"<- "Data available"
              #-------------------------------
              data$"Mode of detection"<- "missing"
              idx = which(data$"Screen_ever"!=888 |data$"Last_screen_year" !=888 |data$"Last_screen_month" !=888|
                            data$"Detection_screen"!=888 | data$"Detection_detailed"!=888)
              data$"Mode of detection"<- "Data available"
              #------------------------------------------------------------------------ 
              
              variables<- c("Lifestyle","Reproductive history","Anthropometry","Hormone use",
                            "Detailed family history","Benign breast disease","Mode of detection",
                            "status","ageInt", "sex","ethnicityClass","famHist",
                            "fhscore","ER_statusIndex")
              variables2<- colnames(data)
              matches<-character()
              for (i in variables2){
                for (e in variables){
                  if (i==e){
                    #matches[[i]]
                    print(i)
                    matches <- append(matches,i)
                  }
                }
              }
              newdata= data[matches]
              
              # Make binary dummy columns (1s,0s)------------------------------------------------
              results <- fastDummies::dummy_cols(newdata, remove_selected_columns = TRUE)
              
              
              # Remove "missing" columns---------------------------------------------------
              results<-results[, -grep("missing$", colnames(results))] 
              print("Missingness results column names")
              print(colnames(results))
              
              # Add column with zeros if variable has all missings
              library(tibble)
              cols <- c("fhscore_Data available" = 0, "famHist_Data available"=0, "ER_statusIndex_Data available"=0, 
                        "sex_Data available"=0, "ageInt_Data available"=0,
                        "Lifestyle_Data available"=0,"Anthropometry_Data available"=0,"Detailed family history_Data available"=0,
                        "Benign breast disease_Data available"=0,"Hormone use_Data available"=0,"Mode of detection_Data available"=0)
              
              results = add_column(results, !!!cols[setdiff(names(cols), names(results))])
              
              # Rename filtering variables

              results = results %>% 
                rename(
                  Sex="sex_Data available",
                  Age="ageInt_Data available",
                  ER_status="ER_statusIndex_Data available",
                  FamHist="famHist_Data available",
                  Fhscore="fhscore_Data available" ,
                  Lifestyle= "Lifestyle_Data available",
                  Anthropometry= "Anthropometry_Data available",
                  "Detailed family history"= "Detailed family history_Data available",
                  "Benign breast disease"="Benign breast disease_Data available",
                  "Hormone use"="Hormone use_Data available",
                  "Mode of detection"="Mode of detection_Data available"
                )
              
              
              # Use library(tibble) to add ethnicities not in data
              cols <- c("ethnicityClass_European" = 0,"ethnicityClass_Hispanic" = 0,
                        "ethnicityClass_African" = 0,"ethnicityClass_Asian Subcontinent"=0,
                        "ethnicityClass_South-East Asian"=0,"ethnicityClass_Other"=0)
              results<-add_column(results, !!!cols[setdiff(names(cols), names(results))])
              
              #Add "Consortia" column with consortia name as rows
              results$Consortia<- "BCAC"
              colnames(results)
              
              #Add study column
              results$study<- data$study
              return(results)
  }

###################################### END MISSINGNESS STATISTICS CODE ###################

############################### BEGIN BOX UPLOAD OF  SUMMARY STATISTICS ##################

box_id1= "732212467224"

#box_id2=

# Authenticate user access through boxr
box_auth(client_id = "627lww8un9twnoa8f9rjvldf7kb56q1m" , client_secret = "gSKdYKLd65aQpZGrq9x4QVUNnn5C8qqm") 
old_df_summ_stat = box_read(690962125479) # old summstat file, DO NOT CHANGE

# Make one or more summary statistics dataframes and combine with other new files or with the old file
df1_summ_stat = MAKE.SUMMSTAT(box_id1) # new data - CHANGE BOX ID

df_final_summ_stat = rbind(df1_summ_stat)# old_df_summ_stat,df2_summ_stat,df3_summ_stat) # COMBINE DFs IF MORE THAN ONE or adding to old summary statistics file

# Save dataframe locally as csv
write.csv(df_final_summ_stat, "BCAC_Summary_Statistics.csv", row.names = FALSE) # change name

#[9a] Upload the combined file to box using the box file ID of the file to be replaced
box_ul(109395301106, file="BCAC_Summary_Statistics.csv", pb = options()$boxr.progress,description = NULL) # UPLOAD AS NEW BCAC_Summary_statistics.csv - DON'T CHANGE
      

############################### BEGIN BOX UPLOAD OF MISSINGNESS STATISTICS ##################

# Make one or more missingness statistics dataframes and combine with other dataframses from new files or with the old missingness file
df_final_miss_stat = MAKE.MISS.STAT(box_id1) # new data - CHANGE BOX ID

#Save the missingness statistics results locally as csv
write.csv(df_final_miss_stat,"BCAC_Missingness_Statistics.csv",row.names = FALSE)

#[9a] Upload the file to box using the box folder ID of the BCAC summary and missingness statistics
box_ul(109395301106, file="BCAC_Missingness_Statistics.csv", pb = options()$boxr.progress,description = NULL) # UPLOAD AS NEW BCAC_missingness_statistics.csv - DON'T CHANGE
