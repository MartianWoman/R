#install.packages("readxl")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("data.table")
#install.packages("ggpubr")
library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(ggpubr)

#__ Read the titrations data: has to include calculated titratable acidity
meow <- read_excel("r_raw.xlsx")
meow <- select(meow, "Treatment", "Time", "Period","H+")
meow <- meow %>%
  mutate(`H+` = as.numeric(gsub(",", ".", as.character(`H+`))))

#___________________________  Part 1 - Raw values at dusk and dawn separately

#__ Plot raw values as grid
ggplot(meow, aes(x = Treatment, y = `H+`, fill=Treatment), ) +
  geom_boxplot()+
  labs(x="Treatment", y="H⁺ (mmol/kg FW)", title="Raw values of H+") +
  facet_grid(Time ~ Period) + 
  ggtitle("Nominal values")

#__ Statistic analysis

# Pull groups for weeks and treatments
raw_control_w1_en <- meow %>%
  filter(Treatment == "control", Period =="week 1", Time =="end of night") %>%
  pull(`H+`)
raw_control_w2_en <- meow %>%
  filter(Treatment == "control", Period =="week 2", Time =="end of night") %>%
  pull(`H+`)
raw_control_w1_ed <- meow %>%
  filter(Treatment == "control", Period =="week 1", Time =="end of day") %>%
  pull(`H+`)
raw_control_w2_ed <- meow %>%
  filter(Treatment == "control", Period =="week 2", Time =="end of day") %>%
  pull(`H+`)

raw_heat_w1_en<-meow %>%
  filter(Treatment == "heat", Period =="week 1", Time =="end of night") %>%
  pull(`H+`)
raw_heat_w2_en<-meow %>%
  filter(Treatment == "heat", Period =="week 2", Time =="end of night") %>%
  pull(`H+`)
raw_heat_w1_ed<-meow %>%
  filter(Treatment == "heat", Period =="week 1", Time =="end of day") %>%
  pull(`H+`)
raw_heat_w2_ed<-meow %>%
  filter(Treatment == "heat", Period =="week 2", Time =="end of day") %>%
  pull(`H+`)

raw_salt_w1_en<-meow %>%
  filter(Treatment == "salt", Period =="week 1", Time =="end of night") %>%
  pull(`H+`)
raw_salt_w2_en<-meow %>%
  filter(Treatment == "salt", Period =="week 2", Time =="end of night") %>%
  pull(`H+`)
raw_salt_w1_ed<-meow %>%
  filter(Treatment == "salt", Period =="week 1", Time =="end of day") %>%
  pull(`H+`)
raw_salt_w2_ed<-meow %>%
  filter(Treatment == "salt", Period =="week 2", Time =="end of day") %>%
  pull(`H+`)

raw_salt_heat_w1_en<-meow %>%
  filter(Treatment == "salt+heat", Period =="week 1", Time =="end of night") %>%
  pull(`H+`)
raw_salt_heat_w2_en<-meow %>%
  filter(Treatment == "salt+heat", Period =="week 2", Time =="end of night") %>%
  pull(`H+`)
raw_salt_heat_w1_ed<-meow %>%
  filter(Treatment == "salt+heat", Period =="week 1", Time =="end of day") %>%
  pull(`H+`)
raw_salt_heat_w2_ed<-meow %>%
  filter(Treatment == "salt+heat", Period =="week 2", Time =="end of day") %>%
  pull(`H+`)

# Automatic test type choise
choose_test <- function(control, treatment, group_name) {
  p_norm_control <- shapiro.test(control)$p.value
  p_norm_treatment <- shapiro.test(treatment)$p.value
  
  if (p_norm_control < 0.05 | p_norm_treatment < 0.05) {
    test_result <- wilcox.test(control, treatment, alternative = "two.sided", exact = FALSE)
    test_type <- "Wilcoxon"
    test_stat <- test_result$statistic
  } else {
    test_result <- t.test(control, treatment, alternative = "two.sided", var.equal = FALSE)
    test_type <- "Welch"
    test_stat <- test_result$statistic
  }
  p_value <- test_result$p.value
  significant <- ifelse(p_value < 0.05, "Yes", "No")
  
  return(data.frame(
    Comparison = group_name,
    Test = test_type,
    Statistic = test_stat,
    P_Value = p_value,
    Significant = significant
  ))
}

# Testing if data has normal distribution in each group
shapiro.test(raw_control_w1_en)
shapiro.test(raw_control_w2_en)
shapiro.test(raw_control_w1_ed)
shapiro.test(raw_control_w2_ed)

shapiro.test(raw_heat_w1_en)
shapiro.test(raw_heat_w2_en)
shapiro.test(raw_heat_w1_ed)
shapiro.test(raw_heat_w2_ed)

shapiro.test(raw_salt_w1_en)
shapiro.test(raw_salt_w2_en)
shapiro.test(raw_salt_w1_ed)
shapiro.test(raw_salt_w2_ed)

shapiro.test(raw_salt_heat_w1_en)
shapiro.test(raw_salt_heat_w2_en)
shapiro.test(raw_salt_heat_w1_ed)
shapiro.test(raw_salt_heat_w2_ed)


# Table of results
test_results <- bind_rows(
  choose_test(raw_control_w1_en, raw_heat_w1_en, "Week 1: EN Heat"),
  choose_test(raw_control_w2_en, raw_heat_w2_en, "Week 2: EN Heat"),
  choose_test(raw_control_w1_ed, raw_heat_w1_ed, "Week 1: ED Heat"),
  choose_test(raw_control_w2_ed, raw_heat_w2_ed, "Week 2: ED Heat"),
  choose_test(raw_control_w1_en, raw_salt_w1_en, "Week 1: EN Salt"),
  choose_test(raw_control_w2_en, raw_salt_w2_en, "Week 2: EN Salt"),
  choose_test(raw_control_w1_ed, raw_salt_w1_ed, "Week 1: ED Salt"),
  choose_test(raw_control_w2_ed, raw_salt_w2_ed, "Week 2: ED Salt"),
  choose_test(raw_control_w1_en, raw_salt_heat_w1_en, "Week 1: EN Salt + Heat"),
  choose_test(raw_control_w2_en, raw_salt_heat_w2_en, "Week 2: EN Salt + Heat"),
  choose_test(raw_control_w1_ed, raw_salt_heat_w1_ed, "Week 1: ED Salt + Heat"),
  choose_test(raw_control_w2_ed, raw_salt_heat_w2_ed, "Week 2: ED Salt + Heat")
)
print(test_results)

# Assign common p values as asteriscs
test_results <- test_results %>%
  mutate(Significance = case_when(
    P_Value < 0.001 ~ "***",
    P_Value < 0.01  ~ "**",
    P_Value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Test results as treatments and weeks
comparisons_list <- list(
  c("control", "heat"),
  c("control", "salt"),
  c("control", "salt+heat")
)

#__ Plot with significance above boxplots as grid
ggplot(meow, aes(x = Treatment, y = `H+`, fill = Treatment)) +
  geom_boxplot()+
  stat_compare_means(comparisons = comparisons_list, method = "t.test", label = "p.signif", 
                     hide.ns = TRUE) +
  labs(x="Treatment", y="H⁺ (mmol/kg FW)", title="Raw values of H+") +
  facet_grid(Time ~ Period) + 
  labs(x = "", y = expression(mu * "mol H"^"+" ~ "g"^-1 ~ " FW"), 
       title = "Acid accumulation (raw values)")


#___________________________  Part 2 - working with acidity differences detween dawn and dusk (delta H)

#__ Import data without outliers (positive outliers removed manually)
deltas <- read_excel("r_delta.xlsx")
deltas2 <- select(deltas, "Treatment", "Period","delta")

# Removing negative outliers
deltas2<-deltas%>%
  filter(delta>=-4)

#__ Plot delta values as grid
ggplot(deltas2, aes(x = Treatment, y = delta, fill=Treatment)) +
  geom_boxplot()+
  labs(x="Treatment", y="H⁺ (mmol/kg FW)") +
  facet_grid(. ~ Period) + 
  ggtitle("Night acid accumulation")

#__ Statistic analysis

# Pull groups for weeks and treatments
control_w1 <- deltas2 %>%
  filter(Treatment == "control", Period =="week 1") %>%
  pull(delta)
control_w2 <- deltas2 %>%
  filter(Treatment == "control", Period =="week 2") %>%
  pull(delta)

heat_w1<-deltas2 %>%
  filter(Treatment == "heat", Period =="week 1") %>%
  pull(delta)
heat_w2<-deltas2 %>%
  filter(Treatment == "heat", Period =="week 2") %>%
  pull(delta)

salt_w1<-deltas2 %>%
  filter(Treatment == "salt", Period =="week 1") %>%
  pull(delta)
salt_w2<-deltas2 %>%
  filter(Treatment == "salt", Period =="week 2") %>%
  pull(delta)

salt_heat_w1<-deltas2 %>%
  filter(Treatment == "salt+heat", Period =="week 1") %>%
  pull(delta)
salt_heat_w2<-deltas2 %>%
  filter(Treatment == "salt+heat", Period =="week 2") %>%
  pull(delta)

# Automatic test type choise
choose_test <- function(control, treatment, group_name) {
  p_norm_control <- shapiro.test(control)$p.value
  p_norm_treatment <- shapiro.test(treatment)$p.value
  
  if (p_norm_control < 0.05 | p_norm_treatment < 0.05) {
    test_result <- wilcox.test(control, treatment, alternative = "two.sided", exact = FALSE)
    test_type <- "Wilcoxon rank-sum"
    test_stat <- test_result$statistic
  } else {
    test_result <- t.test(control, treatment, alternative = "two.sided", var.equal = FALSE)
    test_type <- "Welch’s t-test"
    test_stat <- test_result$statistic
  }
  p_value <- test_result$p.value
  significant <- ifelse(p_value < 0.05, "Yes", "No")
  
  return(data.frame(
    Comparison = group_name,
    Test = test_type,
    Statistic = test_stat,
    P_Value = p_value,
    Significant = significant
  ))
}

# Testing if data has normal distribution in each group
shapiro.test(control_w1)
shapiro.test(control_w2)
shapiro.test(heat_w1)
shapiro.test(heat_w2)
shapiro.test(salt_w1)
shapiro.test(salt_w2)
shapiro.test(salt_heat_w1)
shapiro.test(salt_heat_w2)

# Table of results
test_results <- bind_rows(
  choose_test(control_w1, heat_w1, "Week 1: Control vs Heat"),
  choose_test(control_w1, salt_w1, "Week 1: Control vs Salt"),
  choose_test(control_w1, salt_heat_w1, "Week 1: Control vs Salt+Heat"),
  choose_test(control_w2, heat_w2, "Week 2: Control vs Heat"),
  choose_test(control_w2, salt_w2, "Week 2: Control vs Salt"),
  choose_test(control_w2, salt_heat_w2, "Week 2: Control vs Salt+Heat")
)
print(test_results)

# Assign common p values as asteriscs
test_results <- test_results %>%
  mutate(Significance = case_when(
    P_Value < 0.001 ~ "***",
    P_Value < 0.01  ~ "**",
    P_Value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Test results as treatments and weeks
comparisons_list <- list(
  c("control", "heat"),
  c("control", "salt"),
  c("control", "salt+heat")
)

#__ Plot with significance above boxplots as grid
ggplot(deltas2, aes(x = Treatment, y = delta, fill = Treatment)) +
  geom_boxplot() +
  stat_compare_means(comparisons = comparisons_list, method = "t.test", label = "p.signif", hide.ns = TRUE) +
  facet_grid(. ~ Period) +
  labs(x = "", y = expression(mu * "mol H"^"+" ~ "g"^-1 ~ " FW"), title = "Night acid accumulation")
