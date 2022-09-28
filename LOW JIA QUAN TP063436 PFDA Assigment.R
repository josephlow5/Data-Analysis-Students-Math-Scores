#Low Jia Quan
#TP063436

#Install and Load Packages
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("dplyr")
install.packages("scales")
install.packages("waffle")
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(waffle)


#Read CSV
StudentData = read.csv("D:\\Users\\Joseph\\Documents\\APU\\Sem 3\\PFDA\\Assignment\\PFDA Assignment\\student.csv")
View(StudentData)
summary(StudentData)
any(is.na(StudentData))

# Adding a column called average
average = (StudentData$G1+StudentData$G2+StudentData$G3)/3
average
StudentData = cbind(StudentData,average)

# Adding a column called Talc
Talc = (StudentData$Dalc+StudentData$Walc)/2
Talc
StudentData = cbind(StudentData,Talc)
summary(StudentData)
View(StudentData)


#Question 1: What are the factors that affect the student study time?

# Analysis 1: Relationship between Student's home to school travel time with student's study time
ggplot(StudentData,aes(x=studytime,y=traveltime))+
  geom_jitter()+
  labs(x="Student's study time",y="Student's home to school travel time",
       title="Student's home to school travel time against student's study time")

# Analysis 2: Relationship between Extra-curriculum activities with student's study time
ggplot(StudentData,aes(x=studytime,fill=activities))+
  geom_bar(position="dodge")+
  scale_fill_discrete(name="Extra-curriculum activities")+
  labs(x="Student's study time",y="Student count",title="Extra-curriculum activities against student's study time")

# Analysis 3: Relationship between student's willingness to take higher education with student's study time
ggplot(StudentData,aes(x=studytime,fill=higher))+
  geom_bar(position = position_dodge(preserve = "single"))+
  scale_fill_discrete(name="Wants to take higher education")+
  labs(x="Student's study time",y="Student count",
       title="Students who wants to take higher education against student's study time")

# Analysis 4: Relationship between Student's Internet access with student's study time
ggplot(StudentData,aes(x=studytime,fill=internet))+
  geom_bar(position="dodge")+
  scale_fill_discrete(name="Internet access")+
  labs(x="Student's study time",y="Student count",title="Internet access against student's study time")

# Analysis 5: Relationship between Student's free time after school with student's study time
ggplot(StudentData,aes(x=studytime,y=freetime))+
  geom_jitter()+
  labs(x="Student's study time",y="Student's free time after school",
       title="Student's free time after school against student's study time")

# Analysis 6: Relationship between Student's Time going out with friends with student's study time
ggplot(StudentData,aes(x=studytime,y=goout))+
  geom_jitter(color="purple")+
  labs(x="Student's study time",y="Time going out with friends",
       title="Time going out with friends against student's study time")

# Analysis 7: Relationship between Student's having extra paid classes against student's study time
ggplot(StudentData,aes(x=studytime,fill=paid))+
  geom_bar(position="dodge")+
  scale_fill_discrete(name="Extra paid classes")+
  labs(x="Student's study time",y="Student count",title="Extra paid classes against student's study time")



#Question 2: What are the factors that affect student's number of school absences?

# Analysis 1: Relationship between Student's school with number of school absences
ggplot(StudentData,aes(x=school,y=absences))+
  geom_jitter()+
  labs(x="Student's school",y="Number of school absences",
       title="Student's school against number of school absences")

# Analysis 2: Relationship between Travel time with number of school absences
ggplot(StudentData,aes(absences))+
  geom_density(aes(fill=factor(traveltime)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Number of school absences Grouped by Home to school travel time",
       x="Number of school absences",
       fill="# Travel time")

#Analysis 3: Relationship between Reason of choosing school with number of school absences
ggplot(StudentData,aes(reason,absences))+
  geom_violin(aes(fill=reason))+
  labs(title="Reason of choosing school against number of school absences", 
       x="Reason of choosing school",
       fill="# Reason")

#Analysis 4: Relationship between School extra educational support with number of school absences
ggplot(StudentData,aes(x=schoolsup,y=absences))+
  geom_point(aes(color=schoolsup)) +
  facet_wrap(~schoolsup, scales = "free_x") +
  labs(title="School extra educational support against number of school absences", 
       x="School extra educational support",
       y="Number of school absences")

#Analysis 5: Relationship between Extra-curricular activities with number of school absences
ggplot(StudentData,aes(x=activities,y=absences))+
  geom_boxplot(aes(color=activities), show.legend=F)+
  labs(title="Extra-curricular activities against number of school absences", 
       x="Extra-curricular activities",
       y="Number of school absences")



#Question 3: What are the factors that will affect a student's average performance?

#Analysis 1: Relationship between Mother Education Level with Student's Average Grade
ggplot(StudentData,aes(x=average,y=Medu))+
  geom_point(aes(color=Medu, size=average)) + 
  labs(x="Student's Average Grade",y="Mother Education Level",title="Mother Education Level against Average")

#Analysis 2: Relationship between Parent Education Level with Student's Average Grade
Pedu = StudentData$Medu + StudentData$Fedu
ggplot(StudentData,aes(x=average,y=Pedu))+
  geom_point(aes(color=Pedu, size=average)) + 
  labs(x="Student's Average Grade",y="Parent Education Level",title="Parent Education Level against Average")

#Analysis 3: Relationship between Student's Weekly Study Time with Student's Average Grade
ggplot(StudentData,aes(x=average,y=studytime))+
  geom_point(color=average, size=3) + 
  geom_segment(aes(x=average, 
                   xend=average, 
                   y=0, 
                   yend=studytime)) +
  labs(x="Student's Average Grade",y="Student's Weekly Study Time",
       title="Student's Weekly Study Time against Average")

#Analysis 4: Relationship between Student's Extra Paid Classes with Student's Average Grade
ggplot(StudentData, aes(average)) + scale_fill_brewer(palette = "Spectral")+
  geom_histogram(aes(fill=paid), 
                   binwidth = .1, 
                   col="black", 
                   size=.1)+
  labs(title="Histogram", 
       x = "Student's Average Grade",
       subtitle="Student's Extra Paid Classes against Student's Average Grade")  

#Analysis 5: Relationship between Student's Extra-curricular Activities with Student's Average Grade
ggplot(StudentData,aes(x=average,fill=activities))+
  geom_bar(position="dodge")+
  scale_fill_discrete(name="Extra-curricular Activities")+
  labs(x="Student's Average Grade",y="Student count",
       title="Student's Extra-curricular Activities against Student's Average Grade")

#Analysis 6: Relationship between Student's Health Status with Student's Average Grade
ggplot(StudentData, aes(average))+
  geom_density(aes(fill=factor(health)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Student's Health Status against Student's Average Grade",
       x="Student's Average Grade",
       fill="# Student's Health Status")

#Analysis 7: Relationship between Student's Number of School Absences with Student's Average Grade
ggplot(StudentData, aes(average, absences))+
  geom_count(color="tomato3", show.legend=F)+
  labs(x="Student's Average Grade",y="Student's Number of School Absences",
       title="Student's Number of School Absences against Student's Average Grade")

#Analysis 8: Relationship between Student's Romantic Relationship with Student's Average Grade
ggplot(StudentData,aes(x=average,fill=romantic))+
  geom_density(aes(fill=factor(romantic)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Student's Romantic Relationship against Student's Average Grade",
       x="Student's Average Grade",
       fill="# Having a romantic relationship")

#Analysis 9: Relationship between Student's Age with Student's Average Grade
ggplot(StudentData,aes(x=age,y=average))+
  geom_jitter(col="blue",width = .5, size=StudentData$average/12) + 
  labs(x="Student's Age",y="Student's Average Grade",title="Student's Age against Student's Average Grade")



#Question 4: What is the effect of alcohol consumption to students?

#Analysis 1: Relationship between Workday alcohol consumption and Weekend alcohol consumption
ggplot(StudentData,aes(x=Dalc,y=Walc))+
  geom_jitter() + 
  geom_smooth(formula = y ~ x,method="lm", se=F) +
  labs(x="Workday alcohol consumption",y="Weekend Alcohol Consumption",
       title="Workday alcohol consumption against Weekend Alcohol Consumption")

#Analysis 2: Relationship between Student's age with Student's Average Alcohol Consumption
ggplot(StudentData,aes(age,Talc))+
  geom_jitter() + 
  labs(title="Relationship between Student's age with Student's Average Alcohol Consumption", 
       x="Student's age")

#Analysis 3: Relationship between Student's sex with Student's Average Alcohol Consumption
ggplot(StudentData,aes(sex,Talc))+
  geom_violin(aes(fill=sex))+
  labs(title="Relationship between Student's sex with Student's Average Alcohol Consumption", 
       x="Student's sex",
       fill="# Average Alcohol Consumption")

#Analysis 4: Relationship between Student going out with friends with Student's Average Alcohol Consumption
ggplot(StudentData,aes(goout,Talc))+
  geom_violin(aes(fill=factor(goout)))+
  labs(title="Relationship between Student going out with friends with Student's Average Alcohol Consumption", 
       x="Student going out with friends",
       fill="# Average Alcohol Consumption")

#Analysis 5: Relationship between Student's health with Student's Average Alcohol Consumption
ggplot(StudentData,aes(health,Talc))+
  geom_count(aes(fill=factor(health)))+
  labs(title="Relationship between Student's health with Student's Average Alcohol Consumption", 
       x="Student's health",
       fill="# Average Alcohol Consumption")

#Analysis 6: Relationship between Student's Average Alcohol Consumption with Student's Average Grade
ggplot(StudentData,aes(x=Talc,y=average))+
  geom_jitter(aes(col=factor(Talc)),width = .5, size=StudentData$average/12) + 
  labs(x="Student's Average Alcohol Consumption",y="Student's Average Grade",
       title="Student's Average Alcohol Consumption against Student's Average Grade")

#Analysis 7: Relationship between Student's Average Alcohol Consumption with Student's Number of School Absences
ggplot(StudentData,aes(x=Talc,y=absences))+
  geom_jitter(aes(col=factor(Talc)),width = .5, size=StudentData$absences/12) + 
  labs(x="Student's Average Alcohol Consumption",y="Student's Number of School Absences",
       title="Student's Average Alcohol Consumption against Student's Number of School Absences")



#Question 5: Do student Perform Better in Specific Condition?

#Analysis 1: Do students perform better in final grade?
#Find the distributions of Average grade of first and second period with final grade.
FirstTwoGradesAvg = (StudentData$G1 + StudentData$G2)/2
StudentData = cbind(StudentData,FirstTwoGradesAvg)
summary(StudentData)
grades_compare = subset(StudentData,select=c(FirstTwoGradesAvg,G3))
all_grades_summary = 
  grades_compare %>%
  group_by(FirstTwoGradesAvg,G3) %>%
  summarize(n=n())

ggplot(all_grades_summary,aes(FirstTwoGradesAvg,G3))+
  geom_tile(aes(fill=n))+
  scale_fill_gradient(low="red", high="green") +
  geom_text(aes(label=n))+
  theme_minimal()+
  labs(x="Average grade of first and second period",
       y="Final Grade",
       title = "Distribution Heatmap", 
       subtitle="Average grade of first and second period against Final Grade", 
       fill="Number of Students")

#Analysis 2: Will one school have much better student performance than another?
par(mfrow=c(1,2))
GP_Excellent = nrow(StudentData[StudentData$school=="GP" & StudentData$average > median(StudentData$average),])
GP_Excellent
GP_Normal = nrow(StudentData[StudentData$school=="GP",])-GP_Excellent
GP_Normal
pie(c(GP_Excellent,GP_Normal), c('Excellent Students','Normal Students'), 
    main="Gabriel Pereira Performance", col=rainbow(2))

MS_Excellent = nrow(StudentData[StudentData$school=="MS" & StudentData$average > median(StudentData$average),])
MS_Excellent
MS_Normal = nrow(StudentData[StudentData$school=="MS",])-MS_Excellent
MS_Normal
pie(c(MS_Excellent,MS_Normal), c('Excellent Students',' Normal Students'), 
    main="Mousinho da Silveira Performance", col=rainbow(4))

#Analysis 3: Will one gender perform better than another in both school?
GP_Excellent_Male = nrow(StudentData[StudentData$school=="GP" & StudentData$sex == "M" & 
                                       StudentData$average > median(StudentData$average),])
GP_Excellent_Female = nrow(StudentData[StudentData$school=="GP" & StudentData$sex == "F" & 
                                         StudentData$average > median(StudentData$average),])
MS_Excellent_Male = nrow(StudentData[StudentData$school=="MS" & StudentData$sex == "M" & 
                                       StudentData$average > median(StudentData$average),])
MS_Excellent_Female = nrow(StudentData[StudentData$school=="MS" & StudentData$sex == "F" & 
                                         StudentData$average > median(StudentData$average),])
GP_Excellent_Male
GP_Excellent_Female
MS_Excellent_Male
MS_Excellent_Female
waffle(c("Excellent GP Male"=GP_Excellent_Male,"Excellent GP Female"=GP_Excellent_Female,
         "Excellent MS Male"=MS_Excellent_Male,"Excellent MS Female"=MS_Excellent_Female)/10,
       rows = 8,
       title="Excellent Students from Gabriel Pereira and 
       Mousinho da Silveira Grouped by Gender",
       xlab="1 sqaure represent 10 people")

