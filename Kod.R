# ---- 1) Start ----

# Paket
library(tidyverse)

# Läs in data (välj fil manuellt)
data <- read.csv(file.choose())


# ---- 2) Kolla datat snabbt ----

# Första rader
head(data)

# Struktur
str(data)

# Sammanfattning
summary(data)

# Dimension
dim(data)


# ---- 3) Datastädning ----

# Tar bort mellanslag
data$plan_type <- trimws(data$plan_type)

# Saknade värden: bmi = medel
data$bmi[is.na(data$bmi)] <- mean(data$bmi, na.rm = TRUE)

# annual_checkups = median
data$annual_checkups[is.na(data$annual_checkups)] <- median(data$annual_checkups, na.rm = TRUE)

# Gör text till lower case + trim
data$smoker <- tolower(trimws(data$smoker))
data$region <- tolower(trimws(data$region))
data$plan_type <- tolower(trimws(data$plan_type))
data$exercise_level <- tolower(trimws(data$exercise_level))

# Tomma exercise_level -> unknown
data$exercise_level[data$exercise_level == ""] <- "unknown"

# Kategoriska variabler till faktor
data$sex <- as.factor(data$sex)
data$region <- as.factor(data$region)
data$smoker <- as.factor(data$smoker)
data$chronic_condition <- as.factor(data$chronic_condition)
data$exercise_level <- as.factor(data$exercise_level)
data$plan_type <- as.factor(data$plan_type)

# Snabb kontroll
unique(data$smoker)
unique(data$region)


# ---- 4) Nya variabler ----

# BMI-kategorier
data$bmi_category <- cut(data$bmi,
                         breaks = c(0, 18.5, 25, 30, Inf),
                         labels = c("Underweight", "Normal", "Overweight", "Obese"))

# En enkel riskpoäng
data$risk_score <- data$prior_accidents + data$prior_claims


# ---- 5) Diagram ----

# Enkel gemensam stil
nice_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

# Histogram: charges
p_hist <- ggplot(data, aes(x = charges)) +
  geom_histogram(bins = 30, fill = "#2C7FB8", color = "white", alpha = 0.9) +
  labs(
    title = "Fördelning av försäkringskostnader",
    subtitle = "hur charges är fördelad",
    x = "Kostnad (charges)",
    y = "Antal personer"
  ) +
  nice_theme
print(p_hist)

# Boxplot: smoker vs charges
p_box_smoker <- ggplot(data, aes(x = smoker, y = charges)) +
  geom_boxplot(aes(fill = smoker), alpha = 0.85, outlier.alpha = 0.5) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title = "Kostnader för rökare vs icke-rökare",
    x = "Rökstatus",
    y = "Kostnad (charges)"
  ) +
  nice_theme
print(p_box_smoker)

# Scatter: age vs charges
p_scatter_age <- ggplot(data, aes(x = age, y = charges)) +
  geom_point(alpha = 0.45, color = "#1F78B4") +
  geom_smooth(method = "lm", se = TRUE, color = "#E31A1C", linewidth = 1) +
  labs(
    title = "Samband mellan ålder och försäkringskostnad",
    subtitle = "Punkter med linjär trendlinje",
    x = "Ålder",
    y = "Kostnad (charges)"
  ) +
  nice_theme
print(p_scatter_age)

# Boxplot: bmi_category vs charges
p_box_bmi <- ggplot(data, aes(x = bmi_category, y = charges)) +
  geom_boxplot(aes(fill = bmi_category), alpha = 0.85, outlier.alpha = 0.5) +
  scale_fill_brewer(palette = "Pastel1", guide = "none") +
  labs(
    title = "Kostnader per BMI-kategori",
    x = "BMI-kategori",
    y = "Kostnad (charges)"
  ) +
  nice_theme
print(p_box_bmi)


# ---- 6) Regression ----

# Modell 1
model1 <- lm(charges ~ age + bmi + smoker + children, data = data)
summary(model1)

# Modell 2
model2 <- lm(charges ~ age + bmi + smoker + risk_score + exercise_level, data = data)
summary(model2)


# ---- 7) Enkel jämförelse av modeller ----

# Tar ut R²
sum_model1 <- summary(model1)
sum_model2 <- summary(model2)

r2_model1 <- sum_model1$r.squared
r2_model2 <- sum_model2$r.squared

cat("\nJämförelse av R²:\n")
cat("Modell 1 (age + bmi + smoker + children):", round(r2_model1, 4), "\n")
cat("Modell 2 (age + bmi + smoker + risk_score + exercise_level):", round(r2_model2, 4), "\n")

# Enkel tolkning
if (r2_model2 > r2_model1) {
  cat("Modell 2 har högre förklaringsgrad (R²).\n")
} else if (r2_model2 < r2_model1) {
  cat("Modell 1 har högre förklaringsgrad (R²).\n")
} else {
  cat("Båda modellerna har samma R².\n")
}

# Tips: kolla p-värden i summary(model1/model2)


# ---- 8) Spara filer ----

# Skapa output-mapp om den saknas
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Spara diagram som PNG
ggsave(file.path(output_dir, "01_histogram_charges.png"), p_hist,
       width = 9, height = 6, dpi = 300)
ggsave(file.path(output_dir, "02_boxplot_smoker_charges.png"), p_box_smoker,
       width = 9, height = 6, dpi = 300)
ggsave(file.path(output_dir, "03_scatter_age_charges.png"), p_scatter_age,
       width = 9, height = 6, dpi = 300)
ggsave(file.path(output_dir, "04_boxplot_bmi_category_charges.png"), p_box_bmi,
       width = 9, height = 6, dpi = 300)

# Spara modellresultat i textfil
sink(file.path(output_dir, "modellresultat.txt"))
cat("Analys av försäkringskostnader\n\n")
cat("R² Modell 1:", round(r2_model1, 4), "\n")
cat("R² Modell 2:", round(r2_model2, 4), "\n\n")
cat("Summary Modell 1:\n")
print(sum_model1)
cat("\nSummary Modell 2:\n")
print(sum_model2)
sink()

cat("\nFiler sparade i mappen 'output':\n")
cat("- 4 diagram (PNG)\n")
cat("- modellresultat.txt med modellresultat\n")