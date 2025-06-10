library(tidyverse)
library(ggplot2)
# Part A: Data Preparation ----


# 1. Load dataset and remove rows with missing Remarks or Passed values
data <- read_csv('student_performance.csv') 
data %>% view()

data1 <- data[data1$Passed != "nan" & data1$Remarks != "nan", ]
data1 %>% View()


# 2. Create a new column Total_Score (sum of three subject scores)
data1 <- data1 %>% mutate(Total_Score = English_Score + Science_Score + Math_Score) 
data1 %>% View()

# 3. Create a new column Performance_Level
data1 <- data1 %>% mutate(Performance_Level = ifelse(Total_Score >= 240, "Excelent", 
                                     ifelse(Total_Score >= 200 ,"Good",
                                     ifelse(Total_Score >= 150 , "Average","Poor"))))
  
# Part B: Data Analysis ----


# 1. Count how many students fall into each Performance_Level. 
data1 %>%  count(Performance_Level) 

# 2. Compute the average Study_Hours_Per_Week and Attendance_Percentage for each performance level.
data1 %>% select(Study_Hours_Per_Week, Attendance_Percentage, Performance_Level) %>% 
  group_by(Performance_Level) %>% 
  summarise(Average_Study_Hours_Per_Week = mean(Study_Hours_Per_Week),Average_Attendance_Percentage = mean(Attendance_Percentage)) 

# 3. Group by Gender and School_Type, and compute: 
#     o Mean Total_Score 
#     o Pass percentage (Passed == "Yes")

data1 %>% select(Gender, School_Type, Total_Score, Passed) %>% 
  group_by(Gender, School_Type) %>% 
  summarise(Mean_Total_Score = mean(Total_Score), Pass_Percentage = mean(Passed == "Yes") * 100) %>% 
  View()

# 4. Find the top 5 students with the highest Study_Hours_Per_Week who did not pass. 
data1 %>% filter(Passed == "No") %>% arrange(desc(Study_Hours_Per_Week)) %>% head(5) 


# Part C: Conditional and Logical Operations ----

# 1. Create a new column Study_Efficiency: 
#     Study_Efficiency = Total_Score / Study_Hours_Per_Week 
#     Filter students with Study_Efficiency < 10 and Passed == "Yes". 

data1 <- data1 %>%  mutate(Study_Efficiency = Total_Score / Study_Hours_Per_Week) %>% 
  filter(Study_Efficiency < 10 & Passed == "Yes" ) 


# 2. Add a column Eligible_for_Scholarship: 
#     o TRUE if Total_Score ≥ 230 and Attendance_Percentage > 90, else FALSE 
data1 %>% mutate(Eligible_for_Scholarship = ifelse((Total_Score > 230 | Total_Score == 230) & Attendance_Percentage > 90, TRUE, FALSE)) %>% 
  View()



# Part D: Data Visualization ----
#   Using ggplot2: 

# 1. Bar chart showing number of students in each Performance_Level 
data1 %>% group_by(Performance_Level) %>% summarise(Number_of_Students = n() )%>% ggplot(aes(x=Performance_Level, y=Number_of_Students)) + 
  geom_bar(stat = "identity")

# 2. Boxplot comparing Total_Score across Gender 
data1 %>% group_by(Gender) %>% summarise(Total_Score) %>% ggplot(mapping=aes(x=Gender, y=Total_Score))+geom_s

# 3. Scatter plot of Study_Hours_Per_Week vs Total_Score, colored by Passed 
data1 %>% ggplot(aes(x = Study_Hours_Per_Week, y = Total_Score, color = Passed)) + geom_point()

# 4. Line chart showing average Total_Score by Attendance_Percentage bins (use cut() to bin attendance) 



# Questions: ----

#  Are high study hours always linked to passing? 
# Do school type and gender impact overall performance? 
# Which factors best predict scholarship eligibility? 