library(dplyr)
#mengimport data dari web
library(readr)
pef_1 <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/refs/heads/main/pefc2.csv")
smoking <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/smoking.csv")

#mempelajari struktur data
str(pef_1)
str(smoking)
summary(pef_1)
summary(smoking)

#melihat distribusi data dengan histogram
hist(pef_1$age)
hist(pef_1$height)

#check pidlink yang unik
n_distinct(pef_1$pidlink)
n_distinct(smoking$pidlink)

#Find duplicated pidlink
pef_1 %>%
  count(pidlink) %>%
  filter(n>1)

smoking %>%
  count(pidlink) %>%
  filter(n>1)

#combining dataset (menggabungkan data)
# menggabungkan variables dengan common variable
smk_pef_c_ij = inner_join(smoking, pef_1, by = "pidlink")
View(smk_pef_c_ij)

#menghapus duplicate variable
smk_pef_c_ij <- smk_pef_c_ij %>% select(-...1.y)

#memeriksa distribusi data
library(ggplot2)
summary(smk_pef_c_ij)
hist(smk_pef_c_ij$age)
ggplot(data = smk_pef_c_ij, aes(x = age)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age",
       x = "Age",
       y = "Frequency") +
  theme_minimal()
hist(smk_pef_c_ij$height)
ggplot(data = smk_pef_c_ij, aes(x = height)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of height",
       x = "Height",
       y = "Frequency") +
  theme_minimal()
hist(smk_pef_c_ij$pef)
ggplot(data = smk_pef_c_ij, aes(x = pef)) +
  geom_histogram(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Peak Expiratory Flow",
       x = "PEF",
       y = "Frequency") +
  theme_minimal()
str(smk_pef_c_ij$age)


#Analisis Deskriptif data
library(dplyr)
tbl_smkpef <- smk_pef_c_ij %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_height = mean(height, na.rm = TRUE),
    sd_height = sd(height, na.rm = TRUE),
    mean_pef = mean(pef, na.rm = TRUE),
    sd_pef = sd(pef, na.rm = TRUE)
  )

print(tbl_smkpef)

#distribusi variabel kategorik
table_sex <- table(smk_pef_c_ij$sex)
table_asthma <- table(smk_pef_c_ij$Asthma)
table_smoking <- table(smk_pef_c_ij$smoking)

print(table_sex)
print(table_asthma)
print(table_smoking)

#hubungan dengan grafik
library(ggplot2)

#hubungan PEF dengan usia dan tinggi badan
ggplot(smk_pef_c_ij, aes(x = age, y = pef)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Hubungan antara PEF dan Usia", x = "Usia (tahun)", y = "PEF") +
  theme_minimal()

ggplot(smk_pef_c_ij, aes(x = height, y = pef)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Hubungan antara PEF dan Tinggi Badan", x = "Tinggi Badan (cm)", y = "PEF") +
  theme_minimal()

#Hubungan PEF dengan sex, asthma, smoking
ggplot(smk_pef_c_ij, aes(x = sex, y = pef, fill = sex)) +
  geom_boxplot() +
  labs(title = "Distribusi PEF Berdasarkan Jenis Kelamin", x = "Jenis Kelamin", y = "PEF") +
  theme_minimal()

ggplot(smk_pef_c_ij, aes(x = Asthma, y = pef, fill = Asthma)) +
  geom_boxplot() +
  labs(title = "Distribusi PEF Berdasarkan Kondisi Asma", x = "Kondisi Asma", y = "PEF") +
  theme_minimal()

ggplot(smk_pef_c_ij, aes(x = smoking, y = pef, fill = smoking)) +
  geom_boxplot() +
  labs(title = "Distribusi PEF Berdasarkan Kebiasaan Merokok", x = "Kebiasaan Merokok", y = "PEF") +
  theme_minimal()


#fit linear regression model
library(lmtest)
library(lme4)
mod_pef <- lm(pef ~ age + height + sex + Asthma + smoking, data = smk_pef_c_ij)
summary(mod_pef)
smk_pef_c_ij$residuals <- residuals(mod_pef)
smk_pef_c_ij$fitted <- fitted(mod_pef)

#Visualisasi model
library(ggplot2)

# Scatter plot dengan garis regresi
ggplot(data = smk_pef_c_ij, aes(x = age, y = pef)) +
  geom_point(alpha = 0.6, color = "blue") +  # Titik data
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +  # Garis regresi
  labs(title = "Hubungan antara Age dan PEF",
       x = "Age (Usia)",
       y = "PEF (Fungsi Paru)") +
  theme_minimal()

ggplot(data = smk_pef_c_ij, aes(x = age, y = pef, color = sex)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Hubungan antara Age dan PEF dengan Sex sebagai Faktor",
       x = "Age (Usia)",
       y = "PEF (Fungsi Paru)") +
  theme_minimal()

ggplot(data = smk_pef_c_ij, aes(x = height, y = pef)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  facet_wrap(~ smoking) +
  labs(title = "Hubungan antara Height dan PEF berdasarkan Kebiasaan Merokok",
       x = "Height (Tinggi Badan)",
       y = "PEF (Fungsi Paru)") +
  theme_minimal()

ggplot(data = smk_pef_c_ij, aes(x = age, y = pef)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  facet_wrap(~ smoking) +
  labs(title = "Hubungan antara Usia dan PEF berdasarkan Kebiasaan Merokok",
       x = "age (Usia)",
       y = "PEF (Fungsi Paru)") +
  theme_minimal()

ggplot(data = smk_pef_c_ij, aes(x = age, y = pef)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  facet_wrap(~ Asthma) +
  labs(title = "Hubungan antara Usia dan PEF berdasarkan Riwayat Asthma",
       x = "Age (Usia)",
       y = "PEF (Fungsi Paru)") +
  theme_minimal()

# Analisis diagnostik regresi
# Pemeriksaan residuals (Normalitas)
qqnorm(residuals(mod_pef))
qqline(residuals(mod_pef))
resid_mod_pef <- resid(mod_pef)
ks.test(resid_mod_pef, "pnorm", mean = mean(resid_mod_pef), sd = sd(resid_mod_pef))

#Pemeriksaan homoskedastisitas
plot(mod_pef, which = 1)
library(lmtest)
bptest(mod_pef)      #uji Breusch-Pagan

#Pemeriksaan multikolinearitas
library(car)
vif(mod_pef)

#pemeriksaan autokorelasi menggunakan durbin-watson test
dwtest(mod_pef)

#identifikasi outliers
plot(rstandard(mod_pef), main = "Standardized Residuals")
abline(h = c(-2, 2), col = "red", lty = 2)

plot(mod_pef, which = 4)  # Cook's Distance

plot(hatvalues(mod_pef), main = "Leverage Values")
abline(h = 2 * mean(hatvalues(mod_pef)), col = "red", lty = 2)

#transformasi data
smk_pef_c_ij$pef_log <- log(smk_pef_c_ij$pef)

#model linear regression akhir
mod_logpef <- lm(pef_log ~ age + height + sex + Asthma + smoking, data = smk_pef_c_ij)
summary(mod_logpef)

library(robustbase)
mod_robpef <- lmrob(pef ~ age + height + sex + Asthma + smoking, data = smk_pef_c_ij)
summary(mod_robpef)
