library(tidyverse)
library(readr)
library(psych)
library(reshape2)
library(plm)
library(ggfortify)
library(quantreg)
library(knitr)
library(kableExtra)
library(Hmisc)
library(gridExtra)


# DATA IMPORTS: CO2 emissions and EPS dataset ------------------------------

# World Bank "CO2 and Greenhouse Gas Emissions" dataset (https://data360.worldbank.org/en/dataset/OWID_CB)
emiss <- read_csv("WB_co2emissions.csv")

# OECD data on Environmental Policy Stringency indicator (https://data-explorer.oecd.org/vis?tenant=archive&df[ds]=DisseminateArchiveDMZ&df[id]=DF_EPS&df[ag]=OECD&dq=.&pd=%2C&to[TIME_PERIOD]=false)
eps <- read_csv("OECD_EPS.csv")

# merging the 2 datasets
colnames(eps) <- c("iso_code", "country", "year", "EPSvalue")
ds <- inner_join(emiss, eps, join_by(iso_code, country, year))

unique(ds$country) # list of countries (36) for which data is available

# calculating logarithmic variables
ds$logGDP <- log(ds$gdp)
ds$logEnCons <- log(ds$primary_energy_consumption)
ds$logEmiss <- log(ds$co2)

# calculating gdp per capita
ds$gdp_pct <- ds$gdp / ds$population
ds <- ds |> relocate(gdp_pct, .after = "gdp")

# calculating devations (substracting mean of x from each value of gdp)
ds$logc <- ds$logGDP - mean(ds$logGDP)
ds$pctc <- ds$gdp_pct - mean(ds$gdp_pct)



# DATA EXPLORATION and VISUALISATION ------------------------------------------

length(unique(ds$country)) # 36 countries
colSums(is.na(ds)) # some data missing
ds$country[is.na(ds$trade_co2_share)]
ds$country[is.na(ds$consumption_co2)]
# the missing data are for Iceland and Norway, but only for some specific categ.

table(ds$country)
summary(ds)
describe(ds) # with the package psych

# Histograms
hist(ds$gdp_pct, main = "Histogram of GDP per capita", xlab = "GDPpct", col = "skyblue")
hist(ds$EPSvalue, main = "Histogram of EPS", xlab = "Value", col = "skyblue")

# Density plot
ggplot(ds, aes(x = co2_per_capita)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Data", x = "Value", y = "Density") +
  theme_minimal()

# Boxplot
boxplot(ds$gdp_pct ~ ds$iso_code, main = "GDP per capita by Country", xlab = "Country", ylab = "GDPpct",las = 2)
boxplot(ds$co2_per_capita ~ ds$iso_code, main = "CO2 Emissions per capita by Country", xlab = "Country", ylab = "CO2pct",las = 2)
boxplot(ds$energy_per_capita~ ds$iso_code, main = "Energy consumption per capita", xlab= "Country", ylab = "Enpct", las=2)

# Scatterplots: energy consumption and growth
country_means <- ds %>%
  group_by(iso_code, country) %>%
  summarise(
    mean_energy = mean(primary_energy_consumption, na.rm = TRUE),
    mean_gdp = mean(gdp, na.rm = TRUE),
    mean_gdppct = mean(gdp_pct, na.rm = TRUE),
    mean_co2pct = mean(co2_per_capita, na.rm = TRUE),
    consumpco2_pergdp = mean(consumption_co2_per_gdp, na.rm = TRUE),
    mean_EPS = mean(EPSvalue, na.rm=TRUE)) 
ggplot(ds, aes(x = primary_energy_consumption, y = gdp)) +
  geom_point() +
  geom_text(data = country_means, aes(x = mean_energy, y = mean_gdp, label = iso_code), 
            size = 3, color = "blue") +
  labs(title = "Energy consumption and Growth", x = "Primary energy cons.", y = "GDP per capita")

# Loess scatterplots (EPS and emissions)
e <- ggplot(ds, aes(x = EPSvalue, y = logEmiss)) +
  geom_point(color = "blue",alpha=0.25, size = 1) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "red")+
  labs(title="EPS and emissions (log scale)",x = "EPSvalue", y = "Log(Co2)") +
  theme(plot.title= element_text(hjust=0.5))
f <- ggplot(ds, aes(x = EPSvalue, y = co2_per_capita)) +
  geom_point(color = "blue",alpha=0.25, size = 1) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "red")+
  labs(title = "EPS and emissions (per capita)", x = "EPSvalue", y = "co2 per capita")+
  theme(plot.title= element_text(hjust=0.5))
grid.arrange(e,f,ncol=2)

# Loess scatterplots (inverted U-shaped EKCs)
a <- ggplot(ds, aes(x = gdp_pct, y = co2_per_capita)) +
  geom_point(size = 1, col="grey") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "blue") +
  ggtitle("CO2 per capita vs GDP per capita") +
  theme(plot.title = element_text(hjust = 0.5))
b <- ggplot(ds, aes(x = log(gdp_pct), y = log(co2_per_capita))) +
  geom_point(size = 1, col="grey") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "blue") +
  ggtitle("Log CO2 per capita vs Log GDP per capita") +
  theme(plot.title = element_text(hjust = 0.5))
c <- ggplot(ds, aes(x = energy_per_capita, y = co2_per_capita)) +
  geom_point(size = 1, col="grey") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "blue") +
  ggtitle("Energy per capita vs GDP per capita") +
  theme(plot.title = element_text(hjust = 0.5))
d <- ggplot(ds, aes(x = log(energy_per_capita), y = co2_per_capita)) +
  geom_point(size = 1, col="grey") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "blue") +
  ggtitle("Log energy per capita vs Log GDP per capita") +
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(a,b,c,d, ncol=2)

# interactive scatterplots of gdp and co2, per country
Countries <- unique(ds$country)
for (i in Countries) {
  p <- ggplot(ds[ds$country == i, ], aes(x = gdp, y = co2)) +
    geom_point() +
    geom_text(aes(label = year), vjust = -1, hjust = 1) +
    labs(title = i) +
    theme(plot.title = element_text(hjust=0.1)) +
    coord_cartesian(clip = "off")
  print(p)
  cat("Country", i, "\n")
  ans <- readline(prompt = "Press [Enter] to continue or type 'q' to quit: ")
  if (tolower(ans) == "q") {
    cat("Loop interrupted by user.\n")
    break
  }
}

# Correlations and heatmaps
vars <- c("gdp", "co2", "primary_energy_consumption", "population", "EPSvalue")
rc <- rcorr(as.matrix(ds[, vars]), type = "pearson")
cor_matrix <- rc$r
p_matrix <- rc$P
cor_melt <- melt(cor_matrix)
p_melt <- melt(p_matrix)
cor_melt$p_value <- p_melt$value
cor_melt$significance <- cut(
  cor_melt$p_value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, 1),
  labels = c("***", "**", "*", ".", ""))
cor_melt <- cor_melt[as.numeric(factor(cor_melt$Var1, levels = vars)) >
                       as.numeric(factor(cor_melt$Var2, levels = vars)), ]
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(value, 2), significance)), size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.4))

# Time series
ggplot(ds_selected, aes(x = year, y = co2_per_capita, color = country)) +
  geom_line() +
  labs(title = "CO2 emissions per capita Over Time by Country", x = "Year", y = "CO2pct")

# EPS value over the years
EPSyearlymeans <- ds |> 
  group_by(year) |> 
  summarise(EPS_yearlymeans = mean(EPSvalue, na.rm = TRUE))
ggplot(ds, aes(x = year, y = EPSvalue)) +
  geom_point() +
  geom_line(data = EPSyearlymeans, aes(x = year, y = EPS_yearlymeans), color = "blue", size = 1) +
  labs(title = "EPS value over the years", x = "Year", y = "EPSvalue") +
  theme(plot.title = element_text(hjust = 0.4))

# Share global co2
ggplot(ds, aes(x = country, y = share_global_co2, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Share of Global CO2 Emissions by Country", x = "Country", y = "Share of Global CO2")

# consumptionco2_pergdp
ggplot(ds_selected, aes(x = year, y = consumption_co2_per_gdp, color = country, group = country)) +
  geom_line() +
  labs(title = "Consumption of co2 per unit of GDP over time", x = "Year", y = "Co2 per GDP") +
  theme_minimal()
co2gdp <- ds |> group_by(country) |> summarise(mean_consco2gdp = mean(consumption_co2_per_gdp, na.rm = TRUE))
co2gdp <- na.omit(co2gdp)
co2gdp$mean_consco2gdp <- sort(co2gdp$mean_consco2gdp, decreasing = TRUE)
co2gdp

# co2 emissions and EPS
ggplot(ds_selected, aes(x = consumption_co2_per_gdp, y = EPSvalue, color = country)) +
  geom_line() +
  geom_text(data = country_means_sel, aes(x = consumpco2_pergdp, y = mean_EPS, label = iso_code), 
            size = 3, color = "blue") +
  labs(title = "Emissions and Policy Stringency", x = "Consumption of co2 per unit of gdp", y = "EPS")


# code for LaTeX table
variables <- c("co2", "EPSvalue", "gdp", "gdp_pct","primary_energy_consumption", "energy_per_capita")
summary_table <- ds |> reframe(
  Mean = sapply(across(all_of(variables), mean, na.rm = TRUE), round, 2),
  Median = sapply(across(all_of(variables), median, na.rm = TRUE), round, 2),
  `Standard Deviation` = sapply(across(all_of(variables), sd, na.rm = TRUE), round, 2),
  Minimum = sapply(across(all_of(variables), min, na.rm = TRUE), round, 2),
  Maximum = sapply(across(all_of(variables), max, na.rm = TRUE), round, 2),
  Observations = sapply(across(all_of(variables), ~sum(!is.na(.x))), as.numeric)) |> 
  t() |> 
  as.data.frame()
colnames(summary_table) <- colnames(ds[variables])
summary_table <- cbind(Statistic = rownames(summary_table), summary_table)
rownames(summary_table) <- NULL
kable(summary_table, format = "latex", booktabs = TRUE, digits = 2, align = "c") %>%
  kable_styling(latex_options = c("hold_position", "striped", "scale_down")) %>%
  add_header_above(c(" " = 1, "Variables" = ncol(summary_table)))



# Linear-log simple Regression ------------------------------------------------

co2_EPS <- lm(co2 ~ EPSvalue, ds)
summary(co2_EPS) # not significant

co2_EPS_GDP <- lm(co2 ~ EPSvalue + gdp, ds)
summary(co2_EPS_GDP) # significant, but difficult to interpret without % changes

co2_EPS_lnGDP <- lm(log(co2) ~ EPSvalue + log(gdp), ds)
summary(co2_EPS_lnGDP) # easier interpretation: 1-unit increase in EPS -> -21%

co2_EPS_lnGDP_lnEnCons <- lm(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption), ds)
summary(co2_EPS_lnGDP_lnEnCons) # EPS effect changed to -9% (still significant)

co2_EPS_lnGDP_lnEnCons_lnPop <- lm(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption) + log(population), ds)
summary(co2_EPS_lnGDP_lnEnCons_lnPop) #controlling for population, EPS becomes nonsignificant

ds2 <- na.omit(ds) #omitting Iceland and some years for Norway
unique(ds2$country)
table(ds2$country)

co2_EPS_lnGDP_lnEnCons_lnTrade <- lm(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption) + trade_co2, ds2)
summary(co2_EPS_lnGDP_lnEnCons_lnTrade) #still -9%

co2_EPS_lnGDP_lnEnCons_GDPpct <- lm(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption) + gdp_pct, ds)
summary(co2_EPS_lnGDP_lnEnCons_GDPpct) #adding GDPpct, EPS becomes insignificant

co2_EPS_lnGDP_lnEnCons_EnConspct <- lm(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption) + energy_per_capita, ds)
summary(co2_EPS_lnGDP_lnEnCons_EnConspct) #adding energy_per_capita, same



# Fixed Effects --------------------------------------------------

#country FE
co2_controls_locfe <- plm(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption), ds, index="country", model="within")
summary(co2_controls_locfe) # EPS effect: -5%

#time-variant FE
co2_controls_timefe <- plm(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption), ds, index="year", model="within")
summary(co2_controls_timefe) # EPS effect: -7.6% 

#Location and Time Fixed Effects 

co2_controls_bothfe <- plm(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption), ds, index=c("country","year"),
                     effect="twoways", model="within")
summary(co2_controls_bothfe) #EPS effect: -1.5%


# Quantile Regression -----------------------------------------------------

taus <- seq(0.1,0.9,by=0.1)
quantreg <- rq(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption),data=ds,taus)
summary(quantreg)

#quantile reg and fixed effect ?



# Analysis of Residuals ---------------------------------------------------

plot(co2_EPS_lnGDP_lnEnCons)

res_timefe <- residuals(co2_controls_timefe)
hist(res_timefe)
qqnorm(res_timefe)
qqline(res_timefe, col = "red")

plot(residuals(co2_controls_locfe))



# SubDatasets -------------------------------------------------------------

OECD_ISOs <- c(
  "AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU",
  "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE",
  "CHE", "TUR", "GBR", "USA"
)
ds_OECD <- ds[ds$iso_code %in% OECD_ISOs,]
ds_nonOECD <- ds[!ds$iso_code %in% OECD_ISOs, ]
# unique(ds_OECD$country)
# unique(ds_nonOECD$country)

co2_EPS_lnGDP_lnEnCons_OECD <- lm(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption), ds_OECD)
summary(co2_EPS_lnGDP_lnEnCons_OECD)
plot(co2_EPS_lnGDP_lnEnCons_OECD)

co2_controls_bothfe_OECD <- plm(log(co2) ~ EPSvalue + log(gdp) + log(primary_energy_consumption), ds_OECD, index="year",model="within")
summary(co2_controls_bothfe_OECD)
qqnorm(residuals(co2_controls_bothfe_OECD))
qqline(residuals(co2_controls_bothfe_OECD),col="red")

ggplot(ds, aes(x = logEnCons, y = logEmiss)) +
  geom_point()

# country selection
sel <- c("Australia", "Brazil", "Canada", "China", "Germany", "India",
         "Indonesia", "Italy","Japan", "Mexico","Netherlands", "Norway",
         "Russia", "South Africa", "South Korea", "Spain", "Sweden",
         "Turkey","United Kingdom", "United States")

ds_selected <- ds |> filter(country %in% sel)
country_means_sel <- country_means |> filter(country %in% sel)
ggplot(ds_selected, aes(x = gdp_pct, y = co2_per_capita, color = country)) +
  geom_point() +
  geom_text(data = country_means_sel, aes(x = mean_gdppct, y = mean_co2pct, label = iso_code), 
            size = 3, color = "blue") +
  labs(title = "GDP per Capita vs CO2 per Capita by Country", x = "GDP per Capita", y = "CO2 per Capita")

ds_sel <- ds |> 
  filter(!iso_code %in% c("LUX", "ISL") & !year %in% 2020)



# Other Regressions

#simple linear reg

simple_logEmiss <- lm(logEmiss ~ poly(logGDP,2) + EPSvalue, ds)
summary(simple_logEmiss)
simple_logEmiss3 <- lm(logEmiss ~ poly(logGDP,2)+ logEnCons + EPSvalue, ds)
summary(simple_logEmiss3)

simple_co2pct <- lm(co2_per_capita ~ poly(gdp_pct,2) + EPSvalue, ds)
summary(simple_co2pct)
simple_co2pct3 <- lm(co2_per_capita ~ poly(gdp_pct,2)+ energy_per_capita + EPSvalue,ds)
summary(simple_co2pct3)

# Fixed effects

logEmiss_cFE <- plm(logEmiss ~ poly(logGDP,2) + EPSvalue, data = ds,
                 index = "country", model = "within", na.action = na.omit)
summary(logEmiss_cFE) 
co2pct_cFE <- plm(co2_per_capita ~ poly(gdp_pct,2) + energy_per_capita +EPSvalue, data = ds,
                    index = "country", model = "within", na.action = na.omit)
summary(co2pct_cFE) 


logEmiss_tFE <- plm(logEmiss ~ poly(logGDP,2) + EPSvalue, data = ds,
                  index = "year", model = "within", na.action = na.omit)
summary(logEmiss_tFE) 
co2pct_tFE <- plm(co2_per_capita ~ poly(gdp_pct,2) + energy_per_capita + EPSvalue, data = ds,
                    index = "year", model = "within", na.action = na.omit)
summary(co2pct_tFE) 


logEmiss_bothFE <- plm(logEmiss ~ poly(logGDP,2) + EPSvalue, data = ds,
                  index = c("country", "year"),
                  effect = "twoways", model = "within", na.action = na.omit)
summary(logEmiss_bothFE) 
co2pct_bothFE <- plm(co2_per_capita ~ poly(gdp_pct,2) + energy_per_capita +EPSvalue, data = ds,
                       index = c("country", "year"),
                       effect = "twoways", model = "within", na.action = na.omit)
summary(co2pct_bothFE)



# Tables ------------------------------------------------------------------

# install.packages("stargazer")
library(stargazer)

stargazer(simple_logEmiss, logEmiss_cFE, logEmiss_tFE, logEmiss_bothFE, type = "text",
          title = "Results (Variables: aggregate)", omit="Constant",
          dep.var.caption = "Dependent Variable: log Co2 Emissions",
          model.names = FALSE, 
          dep.var.labels = "", 
          column.labels = c("-linear Reg-","-Country FE-","-Time FE-", "-Both FE-"),
          covariate.labels = c("log GDP","log GDP (squared)","EPS value"),
          omit.stat = c("f", "ser"),
          no.space = TRUE)

stargazer(simple_co2pct, simple_co2pct3, co2pct_cFE, co2pct_tFE, co2pct_bothFE, type = "text",
          title = "Results (Variables: per capita)", 
          dep.var.caption = "Dependent Variable: Co2 Emissions per capita",
          omit= "Constant",
          model.names = FALSE,
          dep.var.labels = "", 
          column.labels = c("-linear Reg-","-Country FE-","-Time FE-", "-Both FE-"),
          column.separate = c(2,1,1,1),
          covariate.labels = c("GDP per capita","GDP per capita (squared)","energy per capita","EPS value"),
          omit.stat = c("f", "ser"), 
          no.space = TRUE)
