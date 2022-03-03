#load dplyr
install.packages("dplyr")
library('dplyr')

#load test data 
X_test_data <- read.table(file.path(getwd(),"UCI HAR Dataset","test","X_test.txt"))
y_test_data <- read.table(file.path(getwd(),"UCI HAR Dataset","test","y_test.txt"))
subject_test_data <- read.table(file.path(getwd(),"UCI HAR Dataset","test","subject_test.txt"))

#load train data
X_train_data <- read.table(file.path(getwd(),"UCI HAR Dataset","train","X_train.txt"))
y_train_data <- read.table(file.path(getwd(),"UCI HAR Dataset","train","y_train.txt"))
subject_train_data <- read.table(file.path(getwd(),"UCI HAR Dataset","train","subject_train.txt"))

#load features data + activity labels
features <- read.table(file.path(getwd(),"UCI HAR Dataset","features.txt"))
activity_labels <- read.table(file.path(getwd(),"UCI HAR Dataset","activity_labels.txt"))

#add test column names
colnames(X_test_data)=features[,2]
colnames(y_test_data)='activity_code'
colnames(subject_test_data)='subject'

#add train column names
colnames(X_train_data)=features[,2]
colnames(y_train_data)='activity_code'
colnames(subject_train_data)='subject'

#add activity_labels column names
colnames(activity_labels) = c('activity_code','activity')

#merge test data componenents using a column bind
merged_test <- cbind(X_test_data,y_test_data,subject_test_data)

#merge train data components using a column bind
merged_train <- cbind(X_train_data,y_train_data,subject_train_data)

#merge test and train data using a row bind
merged_data <- rbind(merged_test,merged_train)

#extract only measurements on the mean and standard deviation
vector_of_strings <- c("mean\\(\\)","std\\(\\)","activity_code","subject")
matchExpression <- paste(vector_of_strings,collapse = "|")
filtered_data <- (merged_data %>% select(matches(matchExpression)))

#now left join our merged data with activity_labels on activity code to name the activities descriptively
labelled_data <- left_join(filtered_data,activity_labels,by = 'activity_code')
labelled_data <- subset(labelled_data,select = -activity_code)
labelled_data

#now group by activty and subject then calculate means
avg_data <- (labelled_data %>%
                 group_by(activity,subject)%>%
                 summarise(across(everything(),list(mean))))

#convert to dataframe
df_data <- as.data.frame(avg_data)

#order the data by subject and plac esubject column first to make data more intuitive to read
ordered_data <- arrange(df_data,subject)
tidy_data <- (ordered_data %>% relocate(subject))

#write data to .txt file
write.table(tidy_data,file = "tidydata.txt",row.name = FALSE)

