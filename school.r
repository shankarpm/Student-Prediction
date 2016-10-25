## Name: Shankar Mohanakrishnan : Language - R
######Solution for Requirement A  - Start ################################
library(dplyr)
###Read all the files in the following variables 
demog <- read.table("NYDoESchool\\demographics.txt", sep="\t",header=TRUE)
school <- read.table("NYDoESchool\\schools.txt", sep="\t",header=TRUE)
scores <- read.table("NYDoESchool\\scores.txt", sep="\t",header=TRUE)
 
#we sort all the data by the Student Ids
demog <- demog %>% arrange(StudentID)
school <- school %>% arrange(StudentID)
scores <- scores %>% arrange(StudentID)

#since all the subject comes in difference case words, we convert all to upper case for grouping by subjects
scores$subject <- toupper(scores$subject)

# since there are multiple rows with same subjects with score for the same student , we group by student and subject and calculate the mean score
scores <- scores %>% group_by(StudentID,subject) %>% summarise(score = mean(score)) %>% as.data.frame()
# we round the score value to integers
scores$score <- round(scores$score,0)

# we do a inner join for all the 3 datasets based on studentID and merge all the data into a single dataset based on student ID,
# we filter out data where students dont have data for their scores
finaldata <- merge(demog, school, by = "StudentID")
finaldata <- merge(finaldata, scores, by = "StudentID")
   
#------------------------------------------------------------------------------------------------#
#we filter all the  pass students in math class and score > 65 for all schools by grouping school
math.scores65 <- finaldata %>% filter(subject == 'MATH', score > 65) %>% group_by(School) %>% summarise(math.scores65 = n()) %>% as.data.frame

#we filter all the students in math class in all schools by grouping school
all_math.scores <- finaldata %>% filter(subject == 'MATH') %>% group_by(School) %>% summarise(math.scores = n()) %>% as.data.frame

#we calculate the pass rate percent from the above score / total
math.scores_pct <- math.scores65
math.scores_pct[1,2] = round((math.scores65[1,2] / all_math.scores[1,2]) * 100)
math.scores_pct[2,2] = round((math.scores65[2,2] / all_math.scores[2,2]) * 100)
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
#we filter all the  pass students in ELA class and score > 65 for all schools by grouping school
ela.scores65 <- finaldata %>% filter(subject == 'ELA', score > 65) %>% group_by(School) %>% summarise(ela.scores65 = n()) %>% as.data.frame

#we filter all the students in ELA class in all schools by grouping school
all_ela.scores <- finaldata %>% filter(subject == 'ELA') %>% group_by(School) %>% summarise(ela.scores = n()) %>% as.data.frame

#we calculate the pass rate percent from the above score / total
ela.scores_pct <- ela.scores65
ela.scores_pct[1,2] = round((ela.scores65[1,2] / all_ela.scores[1,2]) * 100)
ela.scores_pct[2,2] = round((ela.scores65[2,2] / all_ela.scores[2,2]) * 100)
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
#we filter all the  pass ELL students in math class and score > 65 for all schools by grouping school
ell.Math.Studscores65 <- finaldata %>% filter(subject == 'MATH',ELL %in% c('A', 'E', 'I','O','U'),
                                              score > 65) %>% group_by(School) %>% summarise(ell.Math.Studscores65 = n()) %>% as.data.frame

#we filter all the ELL students in Math class in all schools by grouping school
ell.Math.Studscores <- finaldata %>% filter(subject == 'MATH',ELL %in% c('A', 'E', 'I','O','U')) %>% group_by(School) %>% summarise(ell.Math.Studscores = n()) %>% as.data.frame

#we calculate the pass rate percent from the above score / total
ell.Math.Studscores_pct <- ell.Math.Studscores65
ell.Math.Studscores_pct[1,2] = round((ell.Math.Studscores65[1,2] / ell.Math.Studscores[1,2]) * 100)
ell.Math.Studscores_pct[2,2] = round((ell.Math.Studscores65[2,2] / ell.Math.Studscores[2,2]) * 100)
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
#we filter all the  pass ELL students in ELA class and score > 65 for all schools by grouping school
ell.ELA.Studscores65 <- finaldata %>% filter(subject == 'ELA',ELL %in% c('A', 'E', 'I','O','U'), score > 65)  %>%  group_by(School) %>% summarise(ell.ELA.Studscores65 = n()) %>% as.data.frame

#we filter all the ELL students in ELA class in all schools by grouping school
ell.ELA.Studscores <- finaldata %>% filter(subject == 'ELA',ELL %in% c('A', 'E', 'I','O','U'))  %>%   group_by(School) %>% summarise(ell.ELA.Studscores = n()) %>% as.data.frame

#we calculate the pass rate percent from the above score / total
ell.ELA.Studscores_pct <- ell.ELA.Studscores65
ell.ELA.Studscores_pct[1,2] = round((ell.ELA.Studscores65[1,2] / ell.ELA.Studscores[1,2]) * 100)
ell.ELA.Studscores_pct[2,2] = round((ell.ELA.Studscores65[2,2] / ell.ELA.Studscores[2,2]) * 100)
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
#we filter all the  pass Sepcial Ed students in Math class and score > 65 for all schools by grouping school
SE.Math.studscores65 <- finaldata %>% filter(subject == 'MATH',Special_Ed == 'Y', score > 65) %>% group_by(School) %>% summarise(SE.Math.studscores65 = n()) %>% as.data.frame

#we filter all the Sepcial Ed students in Math class in all schools by grouping school
SE.Math.studscores <- finaldata %>% filter(subject == 'MATH',Special_Ed == 'Y') %>% group_by(School) %>% summarise(SE.Math.studscores = n()) %>% as.data.frame

#we calculate the pass rate percent from the above score / total
SE.Math.studscores_pct <- SE.Math.studscores65
SE.Math.studscores_pct[1,2] = round((SE.Math.studscores65[1,2] / SE.Math.studscores[1,2]) * 100)
SE.Math.studscores_pct[2,2] = round((SE.Math.studscores65[2,2] / SE.Math.studscores[2,2]) * 100)
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
#we filter all the  pass Sepcial Ed students in ELA class and score > 65 for all schools by grouping school
SE.ELA.studscores65 <- finaldata %>% filter(subject == 'ELA',Special_Ed == 'Y', score > 65) %>% group_by(School) %>% summarise(SE.ELA.studscores65 = n()) %>% as.data.frame

#we filter all the Sepcial Ed students in ELA class in all schools by grouping school
SE.ELA.studscores <- finaldata %>% filter(subject == 'ELA',Special_Ed == 'Y') %>% group_by(School) %>% summarise(SE.ELA.studscores = n()) %>% as.data.frame

#we calculate the pass rate percent from the above score / total
SE.ELA.studscores_pct <- SE.ELA.studscores65
SE.ELA.studscores_pct[1,2] = round((SE.ELA.studscores65[1,2] / SE.ELA.studscores[1,2]) * 100)
SE.ELA.studscores_pct[2,2] = round((SE.ELA.studscores65[2,2] / SE.ELA.studscores[2,2]) * 100)
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#we filter all the  pass Sepcial Ed and ELL students in Math class and score > 65 for all schools by grouping school
ell.SE.math.studscores65 <- finaldata %>% filter(subject == 'MATH', ELL %in% c('A', 'E', 'I','O','U'), Special_Ed == 'Y', score > 65) %>% 
                                  group_by(School) %>% summarise(ell.SE.math.studscores65 = n()) %>% as.data.frame

#we filter all the Sepcial Ed and ELL students in Math class in all schools by grouping school
ell.SE.math.studscores <- finaldata %>% filter(subject == 'MATH', ELL %in% c('A', 'E', 'I','O','U'), Special_Ed == 'Y') %>% 
  group_by(School) %>% summarise(ell.SE.math.studscores = n()) %>% as.data.frame

#we calculate the pass rate percent from the above score / total
ell.SE.math.studscores_pct <- ell.SE.math.studscores65
ell.SE.math.studscores_pct[1,2] = round((ell.SE.math.studscores65[1,2] / ell.SE.math.studscores[1,2]) * 100)
ell.SE.math.studscores_pct[2,2] = round((ell.SE.math.studscores65[2,2] / ell.SE.math.studscores[2,2]) * 100)
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
#we filter all the  pass Sepcial Ed and ELL students in ELA class and score > 65 for all schools by grouping school
ell.SE.ela.studscores65 <- finaldata %>% filter(subject == 'ELA', ELL %in% c('A', 'E', 'I','O','U'), Special_Ed == 'Y', score > 65) %>% 
                                  group_by(School) %>% summarise(ell.SE.ela.studscores65 = n()) %>% as.data.frame

#we filter all the Sepcial Ed and ELL students in ELA class in all schools by grouping school
ell.SE.ela.studscores <- finaldata %>% filter(subject == 'ELA', ELL %in% c('A', 'E', 'I','O','U'), Special_Ed == 'Y') %>% 
  group_by(School) %>% summarise(ell.SE.ela.studscores = n()) %>% as.data.frame

#we calculate the pass rate percent from the above score / total
ell.SE.ela.studscores_pct <- ell.SE.ela.studscores65
ell.SE.ela.studscores_pct[1,2] = round((ell.SE.ela.studscores65[1,2] / ell.SE.ela.studscores[1,2]) * 100)
ell.SE.ela.studscores_pct[2,2] = round((ell.SE.ela.studscores65[2,2] / ell.SE.ela.studscores[2,2]) * 100)
#------------------------------------------------------------------------------------------------#

#We merge all the above calculated percent rates into a single table by merging based on School
passRates <-  merge(math.scores_pct,ela.scores_pct, by = "School")  %>% merge(ell.Math.Studscores_pct, by = "School") %>% 
  merge(ell.ELA.Studscores_pct, by = "School") %>% merge(SE.Math.studscores_pct, by = "School") %>% 
  merge(SE.ELA.studscores_pct, by = "School")  %>% merge(ell.SE.math.studscores_pct, by = "School") %>% 
  merge(ell.SE.ela.studscores_pct, by = "School")
colnames(passRates) <- c("School","math.scores_pct","ela.scores_pct","ell.Math.Studscores_pct","ell.ELA.Studscores_pct",
                        "SE.Math.studscores_pct","SE.ELA.studscores_pct","ell.SE.math.studscores_pct","ell.SE.ela.studscores_pct")
#this prints the solution for Requirement A
print(passRates)
######Solution for Requirement A  - End ################################

########Solution for Requirement B  - Start################################################# 

# we filter the student scores data for subjects in Math and ELA
filterscores <- subset(scores,subject %in% c('MATH','ELA'))
  
 #convert the above ROWS with subject and score to COLUMNS and values
finalMeanScore <- reshape(filterscores,direction = "wide", idvar="StudentID", timevar="subject")
# we rename the columns
colnames(finalMeanScore) = c('StudentID', 'score_ELA' , 'score_Math')
 
#we do a left outer join with school and above calulated scores data
 finalMeanScore <- merge(x =school, y = finalMeanScore, by = "StudentID" , all.x = TRUE)
 
finalMeanScore$pass_Math = ''

# we create a new column pass_Math based on 1 and 0 for students who score more than 65 in subject Math
finalMeanScore[!is.na(finalMeanScore$score_Math) & finalMeanScore$score_Math >=65,'pass_Math'] = 1
finalMeanScore[!is.na(finalMeanScore$score_Math) & finalMeanScore$score_Math < 65,'pass_Math'] = 0

# we create a new column pass_ELA based on 1 and 0 for students who score more than 65 in subject ELA
finalMeanScore$pass_ELA = ''
finalMeanScore[!is.na(finalMeanScore$score_ELA) & finalMeanScore$score_ELA >=65,'pass_ELA'] = 1
finalMeanScore[!is.na(finalMeanScore$score_ELA) & finalMeanScore$score_ELA < 65,'pass_ELA'] = 0

  # we do a inner join for the above data with demographic based on student ID
  finalMeanScore <- merge(finalMeanScore, demog, by = "StudentID")
 
  # we create a new column IsELL based on 1 and 0 for students who are ELL
finalMeanScore <- finalMeanScore %>%  mutate(IsELL = ifelse(finalMeanScore$ELL %in% c('A', 'E', 'I','O','U'),1,0))  

# we create a new column IsSpecialEd based on 1 and 0 for students who are Special edution
finalMeanScore <- finalMeanScore %>%  mutate(IsSpecialEd = ifelse(finalMeanScore$Special_Ed == 'Y',1,0))  

# we rename the columns
finalMeanScore <-  finalMeanScore[,c('StudentID','School', 'score_Math', 'score_ELA' ,'pass_Math', 'pass_ELA',   'IsELL' ,'IsSpecialEd')]

# we save the data
write.table(finalMeanScore, 'NYDoESchool\\studentOutput.txt', sep = "\t",row.names = FALSE)
write.csv(finalMeanScore, 'NYDoESchool\\studentOutput.csv',row.names = FALSE)
########Solution for Requirement B  - End################################################# 





















#head(finalMeanScore) 
#diffIds1 <- (!demog$StudentID %in% school$StudentID)
#diffIds2 <- (!school$StudentID %in% demog$StudentID)

#diff1 <- demog[diffIds1,]#250027 in demo but not in schoool
#diff2 <- school[diffIds2,] #250018 in school but not in dem
#nrow(diff1) # 1269
#nrow(diff2) # 1269
#head(diff1)
#head(diff2)
 

# head(kk)
# kk <- scores %>% group_by(StudentID) %>% summarise(cnt = n()) %>% arrange(desc(cnt)) %>% as.data.frame()
# kk %>% arrange(desc(cnt))
# subset(kk,cnt ==2)

#kk <- scores %>% group_by(StudentID) %>% summarise( cnt = n()) %>% as.data.frame()  #subset(kk, cnt == 2) #head(kk)

#finalMeanScore <- finalMeanScore %>%  mutate(score_Math = ifelse(is.na(score_Math),'',score_Math))
#finalMeanScore <- finalMeanScore %>%  mutate(score_ELA = ifelse(is.na(score_ELA),'',score_ELA))
#subset(finalMeanScore , score_ELA < 65) %>% head
#finalMeanScore <- finalMeanScore %>%  mutate(pass_Math = ifelse(finalMeanScore$score_Math >= 65,1,0))
#finalMeanScore <- finalMeanScore %>%  mutate(pass_ELA = ifelse(finalMeanScore$score_ELA >= 65,1,0))
#finalMeanScore$pass_Math <- ifelse( !is.na(finalMeanScore$score_Math) & finalMeanScore$score_Math >=65, 1,0 ) 
#subset(finalMeanScore , pass_Math > 0 & pass_ELA > 0 & IsELL >0 & IsSpecialEd >0) %>% head
