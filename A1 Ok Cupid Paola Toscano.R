#A1 OkCupud EDA assignment
#By: Paola Toscano Corredor


# loading requried libraries for EDA
library(radiant.data)
library(dplyr)
library(DataExplorer)
library(DescTools)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(maps)
library(mapdata)
#install.packages("DescTools")
#install.packages("radiant.data")
#install.packages('DataExplorer')
#install.packages('mapdata')

# Set WD
setwd("~/Hult_Visualizing-Analyzing-Data-with-R/DD1_Case_Info/A1_OKCupid")

# storing the files into respective objects
profiles <- read.csv(file="profiles.csv", header=TRUE,
                     stringsAsFactors=FALSE)

address <- read.csv(file="addr.csv", header=TRUE,
                    stringsAsFactors=FALSE)

Latlon <- read.csv(file="LatLon.csv", header=TRUE,
                   stringsAsFactors=FALSE)

SharedCensus2010 <- read.csv(file="sharedCensus2010Vars.csv", header=TRUE,
                       stringsAsFactors=FALSE)

#print the data
print(profiles)
print(address)
print(Latlon)
print(SharedCensus2010)

#head of the data
head(profiles)
head(address)
head(Latlon)
head(SharedCensus2010)

# Set WD
setwd("~/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

profiles_revised <- subset(profiles[, (names(profiles) %in% c("body_type", "education", "location", "orientation", "sex", "smokes", "status", "age", "height"))])
profiles_revised                                       

###############################
#######  General EDA    #######
###############################

#Profiles csv file
#Overall structure  & dimensions of the data
str(profiles_revised)

#gives dimentionality
dim(profiles_revised)
class(profiles_revised)

# Classes for each column
sapply(profiles_revised, class)

# Tail
tail(profiles_revised)

#names of the columns
names(profiles_revised)

# count of nulls
View(colSums(is.na(profiles_revised)))

#summary
summary(profiles_revised)

#filtering the data
#profiles.subset <- filter(profiles, height>=55 & height <=80)

##############################################################
###########  Cleaning the interested columns  ###############
##############################################################


#COLUMN - body type
unique(profiles$body_type)

#Normalizing the data
#I'm grouping the different type of data in the body type column
profiles_revised$body_type <- replace(profiles_revised$body_type, profiles$body_type == "a little extra", "overweight")
profiles_revised$body_type <- replace(profiles_revised$body_type, profiles$body_type == "skinny", "thin")
profiles_revised$body_type <- replace(profiles_revised$body_type, profiles$body_type == "jacked", "fit")
profiles_revised$body_type <- replace(profiles_revised$body_type, profiles$body_type == "athletic", "fit")
profiles_revised$body_type <- replace(profiles_revised$body_type, profiles$body_type == "full figured", "overweight")
profiles_revised$body_type <- replace(profiles_revised$body_type, profiles$body_type == "rather not say", "others")
profiles_revised$body_type <- replace(profiles_revised$body_type, profiles$body_type == "used up", "others")

# Filter out NA values and store in a new data frame
#The NA is the more frecuent value, I will filter NA and store the data in a new dataframe to find the mode value
profiles_revised.subset <- filter(profiles_revised, !is.na(body_type))
profiles_revised1 <- profiles_revised.subset
mode_body_type <- Mode(profiles_revised1$body_type)
mode_body_type

#Replacing the NA
profiles_revised$body_type[is.na(profiles_revised$body_type)] <- mode_body_type

#unique values after cleaning the body type column
unique(profiles_revised$body_type)


#COLUMN - education 
#replacing the value t
# Substitute columns to collage in education 
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "working on college/university", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "graduated from college/university", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "graduated from two-year college/university", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "working on med school/university", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "med school/university", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "college/university/university", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "college/university", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "graduated from med school", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "working on two-year college", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "two-year college", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "graduated from two-year college", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "working on med school", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "med school", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "graduated from law school", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "working on law school", "university")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "law school", "university")

#space camp
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "space camp", "space camp")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "working on space camp", "space camp")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "graduated from space camp", "space camp")

# Substitute columns to masters in education 
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "masters program", "masters program")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "graduated from masters program", "masters program")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "working on masters program", "masters program")


# Substitute columns to high school in education 
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "working on high school", "high school")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "graduated from high school", "high school")

#replacing the 'dropped' condition
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "dropped out of college/university", "dropped")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "dropped out of med school", "dropped")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "dropped out of two-year college", "dropped")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "dropped out of two-year college", "dropped")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "dropped out of high school", "dropped")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "dropped out of space camp", "dropped")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "dropped out of ph.d program", "dropped")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "dropped out of masters program", "dropped")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "dropped out of law school", "dropped")

# Substitute columns to ph.d program in education
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "graduated from ph.d program", "ph.d program")
profiles_revised$education <- replace(profiles_revised$education, profiles_revised$education == "working on ph.d program", "ph.d program")
unique(profiles_revised$education)

#taking the NA out
profiles_revised.subset <- filter(profiles_revised, !is.na(education))
profiles_revised2 <- profiles_revised.subset
mode_education <- Mode(profiles_revised2$education)
mode_education

profiles_revised$education[is.na(profiles_revised$education)] <- mode_education
unique(profiles_revised$education)

#-----------------------------------------

#COLUMN - height
#first know the data that contains the column
unique(profiles_revised$height)

#finding the outliers of the height values
colors <- c("#3366CC", "#DC3912")
boxplot(profiles_revised$height, outline = TRUE, col = colors, main = "Height")

#min and max og the height
min_height <- min(profiles_revised$height, na.rm = TRUE)
min_height
max_height <- max(profiles_revised$height, na.rm = TRUE)
max_height

#the principal outliers are less than 60 and above 80 inches. However will be considered the values less than 55 and above 80
#to clean the data. First, calculate the average
mean_height <- mean(profiles_revised$height, na.rm = TRUE)
mean_height
median_height <- median(profiles_revised$height, na.rm = TRUE)
median_height
mode_height <- Mode(profiles_revised$height)
mode_height

#replacing values for the median
profiles_revised$height[profiles_revised$height > 80] <- median_height
profiles_revised$height[profiles_revised$height < 55] <- median_height

#looking to the null values 
sum(is.na(profiles_revised$height))

#removing the NA values
profiles_revised <- filter(profiles_revised, !is.na(height))
sum(is.na(profiles_revised$height))

#verifying if the outliers were removed for the column
boxplot(profiles_revised$height, outline = TRUE, col = colors, main = "Height")

print(profiles_revised)
#---------------------------------------------------------
#COLUMN - location
location <- separate(profiles_revised, col = location, into = c("city", "state"), sep = ", ")

# print resulting dataframe
print(location)


#---------------------------------------------------------
#COLUMN - age
#first know the unique values of the column
unique(profiles_revised$age)

#what is the min and the max values of the column
min_age <- min(profiles_revised$age, na.rm = TRUE)
min_age
max_age <- max(profiles_revised$age, na.rm = TRUE)
max_age

#In a Histogram see how the values are distributed 
ggplot(profiles_revised, aes(x = age)) +
  geom_histogram(binwidth = 5, color = "black", fill = "yellow") +
  stat_bin(aes(label = ..count..), binwidth = 5, geom = "text", vjust = -0.5, color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

#now a boxplot to see the where are the outliers
boxplot(profiles_revised$age, outline = TRUE, col = "pink", main = "Age")


#taking out the outliers from the age colum. For this, i will assume that the outliers are above 65 years old
#however it is better to create a new column 
# creating a new column "age group" to store the age groups
profiles_revised$age_group <- ""

# define the age ranges and the corresponding age groups
conditions <- list(
  profiles_revised$age >= 18 & profiles_revised$age <= 34,
  profiles_revised$age >= 35 & profiles_revised$age <= 44,
  profiles_revised$age >= 45 & profiles_revised$age <= 54,
  profiles_revised$age >= 55 & profiles_revised$age <= 64,
  profiles_revised$age >= 65
)
age_groups <- c(
  "Young adult",
  "Early middle-aged adult",
  "Late middle-aged adult",
  "Young older adult",
  "Older person"
)

# Assign each age value to its corresponding age group.
profiles_revised$age_group <- cut(profiles_revised$age, breaks = c(18, 35, 45, 55, 65, Inf), labels = age_groups)
profiles_revised$age_group

#removing NA values
profiles_revised <- filter(profiles_revised, !is.na(age_group))
sum(is.na(profiles_revised$age_group))

#dropping the 'age' column
new_profiles <- subset(profiles_revised, select = -age)
new_profiles

#----------------------------------------------------
#COLUMN - orientation
unique(new_profiles$orientation)

#----------------------------------------------------
#COLUMN - gender
unique(new_profiles$sex)

new_profiles$sex <- replace(new_profiles$sex, new_profiles$sex == "m", "Male")
new_profiles$sex <- replace(new_profiles$sex, new_profiles$sex == "f", "Female")

# Calcular los porcentajes totales de cada valor en la columna 'sex'
sex_percentages <- prop.table(table(new_profiles$sex)) * 100
sex_percentages


#----------------------------------------------------
#COLUMN - smokes
unique(new_profiles$smokes)

# creating a new column "age group" to store the age groups
new_profiles$smokes <- replace(new_profiles$smokes, new_profiles$smokes == "when drinking", "sometimes")
new_profiles$smokes <- replace(new_profiles$smokes, new_profiles$smokes == "trying to quit", "sometimes")

#Using the mode to take out the NA 
new_profiles.subset <- filter(new_profiles, !is.na(smokes))
new_profiles1 <- new_profiles.subset
mode_smokes <- Mode(new_profiles1$smokes)
mode_smokes

new_profiles$smokes[is.na(new_profiles$smokes)] <- mode_smokes

unique(new_profiles$smokes)

# creating a histogram of the "smokes" column
ggplot(new_profiles, aes(x = smokes, fill = smokes)) + 
  geom_bar(color = "black", alpha = 0.8) +
  ggtitle("Distribution of a Smokers") + 
  xlab("Smoking Status") + 
  ylab("Frequency") +
  scale_fill_manual(values = c("#FF00FF", "#FFC0CB", "#FF69B4"))


#-----------------------------------------------------------
#COLUMN - status
unique(new_profiles$status)

#bar chart to see the distribution of the data in the column
ggplot(new_profiles, aes(x = status, fill = status)) + 
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  ggtitle("Distribution of Status") + 
  xlab("Status") + 
  ylab("Count")

#creating different groups to normalize the data
new_profiles$status <- replace(new_profiles$status, new_profiles$status == "available", "single")
new_profiles$status <- replace(new_profiles$status, new_profiles$status == "seeing someone", "in a relationship")
new_profiles$status <- replace(new_profiles$status, new_profiles$status == "married", "in a relationship")

#getting rid of the 'unknown' values since are only 10 out of 59 thousand by filtering the column
new_profiles_filtered <- new_profiles %>% 
  filter(status != "unknown")

#ggplot of the filtered data
ggplot(new_profiles, aes(x = status, fill = status)) + 
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  ggtitle("Distribution of Status Normalized") + 
  xlab("Status") + 
  ylab("Count")

#-------------------------------------------------------------------------------------------------
###########################
### Analysis and graphs ###
###########################

# 1. education and sex
#Aggregate the columns education and sex
education_age_count <- aggregate(. ~ education + sex, data = new_profiles_filtered, FUN = length)

#here I'm calling the columns
colnames(education_age_count) <- c("Education", "Sex", "Count")

#Scatterplot for the graph
ggplot(education_age_count, aes(x = Sex, y = Education, size = Count, color = Education)) +
  geom_point() +
  geom_text(aes(label = Count), size = 5, vjust = -1, color = "black") +
  labs(title = "Relationship between Sex and Education Level",
       x = "Sex",
       y = "Education Level",
       size = "Quantity") +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Education Level") +
  scale_size_continuous(range = c(2, 10)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))


#--------------------------------------------------------------------------------------
# 2. Relationship between age and gender
# frequency table
age_gender_tab <- table(new_profiles_filtered$age, new_profiles_filtered$sex)

#converting the table in a new data frame
age_gender_df <- as.data.frame(age_gender_tab)

#renaming the columns
colnames(age_gender_df) <- c("Age", "Gender", "Count")

#bar chart
ggplot(age_gender_df, aes(x=Age, y=Count, fill=Gender)) +
  geom_bar(position="dodge", stat="identity", color='black') +
  geom_text(aes(label=Count), position=position_dodge(width=0.9), vjust=-0.5, size=4, fontface="bold", colour = "black") +
  labs(x="Age", y="Count", title="Age and Sex Distribution") +
  scale_fill_manual(values=c("#FF00FF", "#fef0d9")) +
  theme_minimal() +
  theme(text=element_text(size=10, color="black", family="Arial"),
        plot.title=element_text(hjust=0.5, size=18, face="bold"),
        axis.title=element_text(size=10, face="bold"),
        legend.title=element_blank(),
        legend.text=element_text(size=10, face="bold"))

#------------------------------------------------------------------------------------------------
# 3. Relationship between status and gender
# Aggregate count of status and gender
status_gender_count <- aggregate(. ~ status + sex, data = new_profiles_filtered, FUN = length)

# Rename columns
colnames(status_gender_count) <- c("Status", "Gender", "Count")

# Create bar chart
ggplot(status_gender_count, aes(x = Status, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Relationship between gender and relationship status",
       x = "Relationship status",
       y = "Number of users",
       fill = "Gender") +
  geom_text(aes(label = Count, y = Count, group = Gender), 
            position = position_dodge(width = 0.9), 
            size = 5, fontface = "bold", 
            vjust = -0.5) +
  scale_fill_manual(values = c("#FF00FF", "#fef0d9"), name = "Gender") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"))

#------------------------------------------------------------------------------------------------

#3.Relationship between body type and age

#average age by body type
body_age_mean <- profiles_revised %>% group_by(`body_type`) %>% summarise(mean_age = mean(age))
mean(profiles_revised$age)
# scatterplot
ggplot(body_age_mean, aes(x = mean_age, y = `body_type`)) +
  geom_point(size = 3, color = "#FF00FF") +
  geom_text(aes(label = round(mean_age, 1)), vjust = -1) +
  labs(title = "Average Age by Body Type",
       x = "Mean Age",
       y = "Body Type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12))


#bar chart of the different bosy types by age group

body_age_count <- new_profiles_filtered %>%
  count(age_group, `body_type`) %>%
  arrange(`body_type`, age_group)

#ggplot that show the relation in a bar chart
ggplot(body_age_count, aes(x = age_group, y = n, fill = `body_type`)) +
  geom_col() +
  labs(title = "Body Type Counts by Age Group",
       x = "Age",
       y = "Count",
       fill = "Body Type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  coord_flip()

#-----------------------------------------------------------------------------
# 4. Relationship between orientation and status
# Count the number of people in each combination of orientation and status
orientation_status_count <- new_profiles_filtered %>% 
  group_by(orientation, status) %>% 
  summarise(count = n()) %>% 
  ungroup()

# Stacked bar chart
ggplot(orientation_status_count, aes(x = orientation, y = count, fill = `status`)) +
  geom_col(color = "black", width = 0.8) +
  scale_fill_manual(values = c("#FF00FF", "#fef0d9")) +
  labs(title = "Relationship between Orientation and Status",
       x = "Orientation",
       y = "Count",
       fill = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))

#-----------------------------------------------------------------------------
# 5. Relationship between height and gender
# Average height by gender
ggplot(new_profiles_filtered, aes(x = sex, y = height)) +
  geom_point(alpha = 0.5) +
  stat_summary(fun.y = mean, geom = "point", size = 6, color = "#FF00FF", 
               show.legend = TRUE, 
               aes(label = round(..y.., 1))) +
  labs(title = "Relationship between Sex and Height",
       x = "Sex",
       y = "Height") +
  theme_minimal()


#mean of height into a dataframe
height_mean <- aggregate(height ~ sex, data = new_profiles_filtered, FUN = mean)
#using a bar chart
ggplot(height_mean, aes(x = sex, y = height)) +
  geom_bar(stat = "identity", fill = "#FF00FF", color = "black", width = 0.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", size = 3, color = "brown", show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "line", size = 1, color = "blue", show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "text", size = 4, color = "black", aes(label = round(..y..,2)),
               vjust = -1) +
  labs(title = "Average Height by Sex",
       x = "Sex",
       y = "Height (inches)",
       caption = "Data source: new_profiles_filtered") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
#-----------------------------------------------------------------------------
# 6. Relationship between height and age group
# Average height by age
ggplot(new_profiles_filtered, aes(x = age_group, y = height)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#FF00FF", color = "black", alpha = 0.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", size = 3, color = "brown", show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "text", size = 4, color = "black", 
               aes(label = round(..y.., 2)), vjust = -1) +
  labs(title = "Height by Age Group",
       x = "Age Group",
       y = "Height (inches)",
       caption = "Data source: new_profiles_filtered") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 9, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#---------------------------------------------------------------------------
# Location
# Get the state count data
state_counts <- location %>% 
  count(state)

# Join the state count data with the map data
state_counts_map <- map_data("state") %>% 
  left_join(state_counts, by = c("region" = "state"))

# Create the choropleth map
ggplot(state_counts_map, aes(fill = n)) +
  geom_map(map = state_counts_map, aes(map_id = region), color = "black") +
  scale_fill_gradient(low = "#fef0d9", high = "#FF00FF", na.value = "grey90", guide = "legend") +
  expand_limits(x = state_counts_map$long, y = state_counts_map$lat) +
  labs(title = "Distribution of Users by State",
       fill = "Number of Users",
       caption = "Data source: location") +
  theme_void()


# location by sex
# create a subset of the data with non-missing gender and count the number of users by state and gender
state_gender_count <- location %>% 
  filter(!is.na(sex)) %>% 
  group_by(state, sex) %>% 
  summarise(count = n()) %>% 
  ungroup()

# filter out the states with no gender or 0 count
state_gender_count_filtered <- state_gender_count %>% 
  filter(state != "", count > 0)

#new dataframe with just California data
ca_gender_count <- state_gender_count_filtered %>% 
  filter(state == "california")

# plot the count of users by gender for California as a stacked bar chart
ggplot(ca_gender_count, aes(x = sex, y = count, fill = sex)) +
  geom_bar(stat = "identity", color='black') +
  geom_text(aes(label = count), vjust = -0.5) +
  scale_fill_manual(values = c("#FF00FF", "#fef0d9")) +
  labs(title = "Count of Users by Gender in California",
       x = "Gender",
       y = "Count",
       fill = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
