

# Load necessary libraries
library(haven)        # To read Stata files
library(tidyverse)
library(labelled)
library(ggpubr)
library(ggplot2) # For visualization
library(dplyr)  # For data manipulation
library(oaxaca) # For Blinder-Oaxaca decomposition



# Step 1: Load and merge data
# Add year labels and combine datasets
getwd()
setwd("E:/")
bdhs_2014 <- read_dta("BDHS_14.dta") %>%
  filter(V012 >= 15 & V012 <= 49) %>%  # Age range 15-49
  mutate(
    year = 2014,
    weight = V005 / 1000000,  # Scaling weight variable
    Division = to_factor(V024),
    Residence = to_factor(V025),
    Wealth_index = to_factor(V190),
    Respondent_Education=to_factor(V106),
    Sex_of_householdhead=V151,
    Wealth_index = to_factor(V190),
    Husbands_education_level=to_factor(V701),
    Age_at_first_birth=age_at_birth,
    Antenatal=anc_y_n,
    Household_size=household_size,
    Decision=Decision,
    Birth_Order=birth_order,
    Respondent_working_Status=respondent_working_status,
    Husbands_working_status=husbands_working_status
  )


bdhs_2017 <- read_dta("BDHS_17_18.dta") %>%
  filter(V012 >= 15 & V012 <= 49) %>%  # Age range 15-49
  mutate(
    year = 2017,
    weight =V005 / 1000000,  # Scaling weight variable
    Division = to_factor(V024),
    Residence = to_factor(V025),
    Wealth_index = to_factor(V190),
    Respondent_Education=to_factor(V106),
    Sex_of_householdhead=V151,
    Wealth_index = to_factor(V190),
    Husbands_education_level=to_factor(V701),
    Age_at_first_birth=age_at_birth,
    Antenatal=anc_y_n,
    Household_size=household_size,
    Decision=Decision,
    Birth_Order=birth_order,
    Respondent_working_Status=respondent_working_status,
    Husbands_working_status=husbands_working_status
    
  )

bdhs_2022 <- read_dta("BDHS_22.dta") %>%
  filter(V012 >= 15 & V012 <= 49) %>%  # Age range 15-49
  mutate(
    year = 2022,
    weight = V005 / 1000000,  # Scaling weight variable
    Division = to_factor(V024),
    Residence = to_factor(V025),
    Wealth_index = to_factor(V190),
    Respondent_Education=to_factor(V106),
    Sex_of_householdhead=V151,
    Wealth_index = to_factor(V190),
    Husbands_education_level=to_factor(V701),
    Age_at_first_birth=age_at_birth,
    Antenatal=anc_y_n,
    Household_size=household_size,
    Decision=Decision,
    Birth_Order=birth_order,
    Respondent_working_Status=respondent_working_status,
    Husbands_working_status=husbands_working_status
    
  )

# Combine datasets
bdhs <- bind_rows(bdhs_2014, bdhs_2017, bdhs_2022)

########################
# Blinder-Oaxaca decomposition

# Decompose the difference between 2014 and 2017
# Recode 'year' into a binary variable
bdhs_14_17 <- bdhs %>%
  filter(year %in% c(2014, 2017) ) %>% 
  mutate(year_binary = ifelse(year == 2017, 0, 1))  # 1 for 2014, 0 for 2017



dummy_division <- model.matrix(~ as.factor(bdhs_14_17$V024) - 1,
                               data = bdhs_14_17)

d1 <- as.tibble(dummy_division)


dummy_wi <- model.matrix(~ as.factor(bdhs_14_17$V190) - 1,
                         data = bdhs_14_17)

d2 <- as.tibble(dummy_wi)


dummy_res_edu <- model.matrix(~ as.factor(bdhs_14_17$V106) - 1,
                         data = bdhs_14_17)

d3 <- as.tibble(dummy_res_edu)


dummy_hus_edu <- model.matrix(~ as.factor(bdhs_14_17$V701) - 1,
                         data = bdhs_14_17)

d4 <- as.tibble(dummy_hus_edu)

# Remove rows with missing values in the specified columns
bdhs_14_17 <- bdhs_14_17 %>%
  mutate(
    poorest = d2$`as.factor(bdhs_14_17$v190)1`,
    poorer = d2$`as.factor(bdhs_14_17$v190)2`,
    middle = d2$`as.factor(bdhs_14_17$v190)3`,
    richer = d2$`as.factor(bdhs_14_17$v190)4`,
    richest = d2$`as.factor(bdhs_14_17$v190)5`,
    barisal = d1$`as.factor(bdhs_14_17$v024)1`,
    chittagong = d1$`as.factor(bdhs_14_17$v024)2`,
    dhaka = d1$`as.factor(bdhs_14_17$v024)3`,
    khulna = d1$`as.factor(bdhs_14_17$v024)4`, # ref
    mymensing = d1$`as.factor(bdhs_14_17$v024)5`,
    rajshahi = d1$`as.factor(bdhs_14_17$v024)6`,
    rangpur = d1$`as.factor(bdhs_14_17$v024)7`,
    sylhet = d1$`as.factor(bdhs_14_17$v024)8` ,
    no_education = d3$`as.factor(bdhs_14_17$V106)1`,
    primary = d3$`as.factor(bdhs_14_17$V106)2`,
    secondary = d3$`as.factor(bdhs_14_17$V106)3`,
    higher = d3$`as.factor(bdhs_14_17$V106)4`,
    no_education = d4$`as.factor(bdhs_14_17$V701)1`,
    primary = d4$`as.factor(bdhs_14_17$V701)2`,
    secondary = d4$`as.factor(bdhs_14_17$V701)3`,
    higher = d4$`as.factor(bdhs_14_17$V701)4`
  )  


decomp <- oaxaca(
  Antenatal ~ to_factor(Sex_of_householdhead) + to_factor(Age_at_first_birth) + to_factor(Decision) + to_factor(Birth_Order) + to_factor(Respondent_working_Status)+to_factor(Husbands_working_status)
     + to_factor(Husbands_education_level) + to_factor(Respondent_Education) + to_factor(V024) + to_factor(V190) +| year_binary,
  data = bdhs_14_17,
  R = 50
)





f1_14_17 <- plot(decomp, decomposition = "twofold",
                 group.weight = -1, components = c("explained"))

f2_14_17 <- plot(decomp, components = c("endowments"))



summary(decomp)

decomp$twofold$overall

decomp$n

decomp$y






# Extract explained coefficients
explained_contributions <- decomp$twofold$variables[[1]][, "coef(explained)"]

# Get variable names
variable_names <- rownames(decomp$twofold$variables[[1]])

# Combine into a data frame
explained_df <- data.frame(
  Variable = variable_names,
  Contribution = explained_contributions
)

# View the data frame
print(explained_df)


# Rank variables by absolute contribution
ranked_contributions14_17 <- explained_df %>%
  arrange(desc(Contribution))

# View ranked contributions
print(ranked_contributions14_17)






# Decompose the difference between 2017 and 2022
# Recode 'year' into a binary variable
bdhs_17_22 <- bdhs %>%
  filter(year %in% c(2017, 2022) ) %>% 
  mutate(year_binary = ifelse(year == 2022, 0, 1))

length(bdhs_17_22$year_binary)
View(bdhs_2017)
View(bdhs_2022)
dummy_division <- model.matrix(~ as.factor(bdhs_17_22$V024) - 1,
                               data = bdhs_17_22)
d1 <- as.tibble(dummy_division)


dummy_wi <- model.matrix(~ as.factor(bdhs_17_22$V190) - 1,
                         data = bdhs_17_22)

d2 <- as.tibble(dummy_wi)

dummy_res_edu <- model.matrix(~ as.factor(bdhs_17_22$V106) - 1,
                              data = bdhs_17_22)
d3 <- as.tibble(dummy_res_edu)


dummy_hus_edu <- model.matrix(~ as.factor(bdhs_17_22$V701) - 1,
                              data = bdhs_17_22)

d4 <- as.tibble(dummy_hus_edu)

# Remove rows with missing values in the specified columns
bdhs_17_22 <- bdhs_17_22 %>%
  mutate(
    poorest = d2$`as.factor(bdhs_17_22$v190)1`,
    poorer = d2$`as.factor(bdhs_17_22$v190)2`,
    middle = d2$`as.factor(bdhs_17_22$v190)3`,
    richer = d2$`as.factor(bdhs_17_22$v190)4`,
    richest = d2$`as.factor(bdhs_17_22$v190)5`,
    barisal = d1$`as.factor(bdhs_17_22$v024)1`,
    chittagong = d1$`as.factor(bdhs_17_22$v024)2`,
    dhaka = d1$`as.factor(bdhs_17_22$v024)3`,
    khulna = d1$`as.factor(bdhs_17_22$v024)4`, # ref
    mymensing = d1$`as.factor(bdhs_17_22$v024)5`,
    rajshahi = d1$`as.factor(bdhs_17_22$v024)6`,
    rangpur = d1$`as.factor(bdhs_17_22$v024)7`,
    sylhet = d1$`as.factor(bdhs_17_22$v024)8` ,
    no_education = d3$`as.factor(bdhs_17_22$V106)1`,
    primary = d3$`as.factor(bdhs_17_22$V106)2`,
    secondary = d3$`as.factor(bdhs_17_22$V106)3`,
    higher = d3$`as.factor(bdhs_17_22$V106)4`,
    no_education = d4$`as.factor(bdhs_17_22$V701)1`,
    primary = d4$`as.factor(bdhs_17_22$V701)2`,
    secondary = d4$`as.factor(bdhs_17_22$V701)3`,
    higher = d4$`as.factor(bdhs_17_22$V701)4`
  )
levels(bdhs_17_22$Respondent_working_Status)

levels(bdhs_17_22$Husbands_education_level)
table(is.na(bdhs_17_22$Husbands_education_level))
decomp <- oaxaca(
  Antenatal ~ to_factor(Sex_of_householdhead)+ to_factor(Age_at_first_birth) + to_factor(Decision)+
    + to_factor(Birth_Order)+to_factor(Respondent_working_Status)+to_factor(Husbands_working_status)+ to_factor(V701)+ + to_factor(V106) + to_factor(V024) + to_factor(V190)| year_binary,
  data = bdhs_17_22,
  R = 50
)

str(bdhs_2022)


f1_17_22 <- plot(decomp, decomposition = "twofold",
     group.weight = -1, components = c("explained"))

f2_17_22 <- plot(decomp, components = c("endowments"))

summary(decomp)

decomp$twofold$overall

decomp$n

decomp$y







# Extract explained coefficients
explained_contributions <- decomp$twofold$variables[[1]][, "coef(explained)"]

# Get variable names
variable_names <- rownames(decomp$twofold$variables[[1]])

# Combine into a data frame
explained_df <- data.frame(
  Variable = variable_names,
  Contribution = explained_contributions
)

# View the data frame
print(explained_df)


# Rank variables by absolute contribution
ranked_contributions17_22 <- explained_df %>%
  arrange(desc(Contribution))

# View ranked contributions
print(ranked_contributions17_22)


# Plot contributions
ggplot(ranked_contributions, aes(x = reorder(Variable, Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Explained Contributions to Changes in UWB",
    x = "Variable",
    y = "Contribution"
  ) +
  theme_minimal()



######################################


figure <- ggarrange(f1_17_22, f2_17_22,
                    labels = c("Two fold", "Endowment"),
                    ncol = 2, nrow = 1)
figure

