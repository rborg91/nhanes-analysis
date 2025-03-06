# Load the packages
library(NHANES)
library(tidyverse)
library(ggplot2)

# Load the NHANES dataset into your environment
data(NHANES)

# Create new database only selecting relevant fields
nhanes_clean <- NHANES |>
  select (ID, Gender, Age, Education,
          HHIncome, Height,
          BMI, BPSysAve, BPDiaAve,
          TotChol, Diabetes, Depressed,
          SleepHrsNight, PhysActiveDays,
          Alcohol12PlusYr, Smoke100)

# Filter to those aged 18 years or over
nhanes_clean <- nhanes_clean |> 
  filter(Age >= 18)

# Remove rows with N/A values in all fields
nhanes_clean <- nhanes_clean |>
  filter(!is.na(ID) & !is.na(Gender) & !is.na(Age) & !is.na(Education) &
           !is.na(HHIncome) & !is.na(BMI) & !is.na(BPSysAve) & 
           !is.na(TotChol) & !is.na(Diabetes) & !is.na(Depressed) & 
           !is.na(SleepHrsNight) & !is.na(PhysActiveDays) & 
           !is.na(Alcohol12PlusYr) & !is.na(Smoke100))

# Create new column that categorises BMI
# and places column after BMI
nhanes_clean <- nhanes_clean |> 
  mutate(BMI_Category = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI < 25 ~ "Normal",
    BMI < 30 ~ "Overweight",
    BMI >= 30 ~ "Obese",
    TRUE ~ NA_character_
  )) |>
  relocate(BMI_Category, .after = BMI)

# Convert new BMI column to a factor type and order levels
# to correct order
nhanes_clean$BMI_Category <- factor(nhanes_clean$BMI_Category, 
                                    levels = c("Underweight", "Normal", "Overweight", "Obese"))

# Create new column that categorises Household income
# into broader brackets
nhanes_clean <- nhanes_clean |> 
  mutate(HHIncomeV2 = case_when(
    HHIncome %in% c(" 0-4999", " 5000-9999", "10000-14999", "15000-19999", "20000-24999") ~ "0-24999",
    HHIncome %in% c("25000-34999", "35000-44999", "45000-54999", "55000-64999") ~ "25000-64999",
    HHIncome %in% c("65000-74999", "75000-99999") ~ "65000-99999",
    HHIncome == "more 99999" ~ "100000+",
    TRUE ~ NA_character_
  )) |>
  relocate(HHIncomeV2, .after = HHIncome)

# Convert new HHIncome column to a factor type and order levels
# to correct order
nhanes_clean$HHIncomeV2 <- factor(nhanes_clean$HHIncomeV2, 
                                  levels = c("0-24999", "25000-64999", "65000-99999", "100000+"))

# Count rows in each dataset, therefore showing how many
# of the original rows were removed from the above filters
nrow(NHANES)
nrow(nhanes_clean)

# Save the cleaned dataset to a csv file
write.csv(nhanes_clean, "nhanes_clean_dataset.csv", row.names = FALSE)

# Explore the distributions and means/medians of the various fields
# in the cleaned dataset
summary(nhanes_clean)

# Look at distribution of BMI
ggplot(nhanes_clean, aes(x = BMI)) +
  geom_histogram(binwidth = 1,
                 fill = "lightblue",
                 color = "black") +
  labs(title = "BMI Distribution",
       x = "BMI",
       y = "Count") +
  theme_minimal()

# Look at distribution of total cholesterol
ggplot(nhanes_clean, aes(x = TotChol)) +
  geom_histogram(binwidth = 0.5,
                 fill = "lightblue",
                 color = "black") +
  labs(title = "Total Cholesterol Distribution",
       x = "Total Cholesterol",
       y = "Count") +
  theme_minimal()

# Compare BMI between genders
ggplot(nhanes_clean, aes(x = BMI, y = Gender)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "BMI - Comparison Between Genders",
       x = "BMI",
       y = "Gender") +
  theme_minimal()

# Compare BMI among different education levels
ggplot(nhanes_clean, aes(x = BMI, y = Education)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "BMI - Comparison Among Different Education Levels",
       x = "BMI",
       y = "Education Level") +
  theme_minimal()

# Compare BMI among different income levels
ggplot(nhanes_clean, aes(x = BMI, y = HHIncomeV2)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "BMI - Comparison Between Household Income Brackets",
       x = "BMI",
       y = "Household Income Bracket") +
  theme_minimal()

# Compare BMI between having diabetes or not
ggplot(nhanes_clean, aes(x = BMI, y = Diabetes)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "BMI - Comparison of Diabetes Status",
       x = "BMI",
       y = "Diabetes Status") +
  theme_minimal()

# Compare BMI between frequency of depression
ggplot(nhanes_clean, aes(x = BMI, y = Depressed)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "BMI - Comparison of Frequency of Depression",
       x = "BMI",
       y = "Depression Status") +
  theme_minimal()

# Compare BMI between drinking status
ggplot(nhanes_clean, aes(x = BMI, y = Alcohol12PlusYr)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "BMI - Comparison of Alcohol Status",
       x = "BMI",
       y = "12 drink consumption in any one year") +
  theme_minimal()

# Compare BMI between smoking status
ggplot(nhanes_clean, aes(x = BMI, y = Smoke100)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "BMI - Comparison of Smoking Status",
       x = "BMI",
       y = "Smoked >=100 cigarettes in lifetime") +
  theme_minimal()

# Relationship between BMI and TotChol
# showing a trend line of this between the genders
ggplot(nhanes_clean, aes(x = BMI, y = TotChol)) +
  geom_point(color= "blue", size = 2) +
  geom_smooth(aes(color = Gender), method = "lm", se = FALSE) +
  labs(title = "BMI vs Total Cholesterol",
       x = "BMI",
       y = "Total Cholesterol") +
  theme_minimal()

# Correlation and p-value for BMI vs Total Cholesterol
cor.test(nhanes_clean$BMI, nhanes_clean$TotChol, method = "pearson")

# Correlation and p-value for BMI vs Total Cholesterol (Males)
cor.test(subset(nhanes_clean, Gender == "male")$BMI, subset(nhanes_clean, Gender == "male")$TotChol, method = "pearson")

# Correlation and p-value for BMI vs Total Cholesterol (Females)
cor.test(subset(nhanes_clean, Gender == "female")$BMI, subset(nhanes_clean, Gender == "female")$TotChol, method = "pearson")

# Relationship between BMI and Systolic Blood Pressure
ggplot(nhanes_clean, aes(x = BMI, y = BPSysAve)) +
  geom_point(color = "blue",
             size = 2) +
  geom_smooth(method = "lm",
              color = "red",
              se = TRUE) +
  labs(title = "BMI vs Systolic Blood Pressure",
       x = "BMI",
       y = "Systolic BP") +
  theme_minimal()

# Correlation and p-value for BMI vs Systolic Blood Pressure
cor.test(nhanes_clean$BMI, nhanes_clean$BPSysAve, method = "pearson")

# Relationship between BMI and Diastolic Blood Pressure
ggplot(nhanes_clean, aes(x = BMI, y = BPDiaAve)) +
  geom_point(color = "blue",
             size = 2) +
  geom_smooth(method = "lm",
              color = "red",
              se = TRUE) +
  labs(title = "BMI vs Diastolic Blood Pressure",
       x = "BMI",
       y = "Diastolic BP") +
  theme_minimal()

# Correlation and p-value for BMI vs Diastolic Blood Pressure
cor.test(nhanes_clean$BMI, nhanes_clean$BPDiaAve, method = "pearson")

# Relationship between BMI and Hours Slept per Night
ggplot(nhanes_clean, aes(x = BMI, y = SleepHrsNight)) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "BMI vs Hours Slept Per Night",
       x = "BMI",
       y = "Sleep Hours") +
  scale_y_continuous(breaks = seq(2, 12, by = 1),
                     limits = c(2, 12)) +
  theme_minimal()

# Correlation and p-value for BMI vs Hours Slept per Night
cor.test(nhanes_clean$BMI, nhanes_clean$SleepHrsNight, method = "pearson")

# Relationship between BMI and days of physical activity
ggplot(nhanes_clean, aes(x = BMI, y = PhysActiveDays)) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "BMI vs Days Physically Active",
       x = "BMI",
       y = "No of Physical Active Days") +
  scale_y_continuous(breaks = seq(1, 7, by = 1),
                     limits = c(1, 7)) +
  theme_minimal()

# Correlation and p-value for BMI vs Days Physically Active
cor.test(nhanes_clean$BMI, nhanes_clean$PhysActiveDays, method = "pearson")


# Can also look at the relationship between days of physical activity
# and other health conditions

# Relationship between Physical Activity and Diabetes
ggplot(nhanes_clean, aes(x = PhysActiveDays, y = Diabetes)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "Physical Activity vs Diabetes",
       x = "Days Physically Active",
       y = "Diabetes Status") +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  theme_minimal()

# Relationship between Physical Activity and Depression
ggplot(nhanes_clean, aes(x = PhysActiveDays, y = Depressed)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "Physical Activity vs Depression",
       x = "Days Physically Active",
       y = "Depression Frequency") +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  theme_minimal()

# Relationship between Physical Activity and Total Cholesterol
ggplot(nhanes_clean, aes(x = PhysActiveDays, y = TotChol)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Physical Activity vs Total Cholesterol",
       x = "Days Physically Active",
       y = "Total Cholesterol") +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  theme_minimal()

# Correlation test for Physical Activity vs Total Cholesterol
cor.test(nhanes_clean$PhysActiveDays, nhanes_clean$TotChol, method = "pearson")


# Can also look at the relationship between average sleep hours
# and other health conditions

# Relationship between Sleep and Diabetes
ggplot(nhanes_clean, aes(x = SleepHrsNight, y = Diabetes)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "Sleep vs Diabetes",
       x = "Hours Slept Per Night",
       y = "Diabetes Status") +
  scale_x_continuous(breaks = seq(2, 12, by = 1)) +
  theme_minimal()

# Relationship between Sleep and Depression
ggplot(nhanes_clean, aes(x = SleepHrsNight, y = Depressed)) +
  geom_boxplot(fill = "lightblue",
               color = "black",
               width = 0.5) +
  labs(title = "Sleep vs Depression",
       x = "Hours Slept Per Night",
       y = "Depression Frequency") +
  scale_x_continuous(breaks = seq(2, 12, by = 1)) +
  theme_minimal()

# Relationship between Sleep and Total Cholesterol
ggplot(nhanes_clean, aes(x = SleepHrsNight, y = TotChol)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Sleep vs Total Cholesterol",
       x = "Hours Slept Per Night",
       y = "Total Cholesterol") +
  scale_x_continuous(breaks = seq(2, 12, by = 1)) +
  theme_minimal()

# Correlation test for Sleep vs Total Cholesterol
cor.test(nhanes_clean$SleepHrsNight, nhanes_clean$TotChol, method = "pearson")