# in this example data, we will explore the relationship between wing length and total body length in two species of damselflies: Ischnura elegans and Enallagma cyathigerum. The data has been collected from 20 locations across 2 years for Ischnura, and 13 populations from 1 year (2021)

#The goal of this code is to illustrate how the random effect works. We run a model without a random effect, then with 1 random effect (Locale), then finally with 2 random effects that are nested to one another (Locale and Year)

# setwd("~/Dropbox/Teaching/BIOS15/2025/example code and data")

library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)

library(here)

damsel_data <- read.csv(here("Exercises", "MixedModels", "Data", "damselfly_averages.csv"))

#Wing-body allometry

ggplot(data = damsel_data, aes(x = TotalLength, y = WingLength))+
	geom_point(alpha = 0.5)+
	theme_classic()

ggplot(data = damsel_data, aes(x = TotalLength, y = WingLength, color = Species))+
	geom_point(alpha = 0.5)+
	theme_classic()+
	theme(legend.position = "top")

ggplot(data = damsel_data, aes(x = TotalLength, y = WingLength, color = interaction(Sex, Species)))+
	geom_point(alpha = 0.1)+
	geom_smooth(method = "lm")+
	theme_classic()+
	theme(legend.position = "top")+
	guides(color=guide_legend(ncol=2))

#It seems that sex and species are important variable affecting the allometric intercepts, which indicate wing length for a given total body length. Now we want to construct a random mixed effect model aiming to estiamte allometric slopes and intercepts, controlling for the effect of year and locale

model_lm <- lm(WingLength ~ TotalLength*Sex*Species, data = damsel_data)
summary(model_lm)

model_lm2 <- lm(WingLength ~ TotalLength + Sex*Species, data = damsel_data)

summary(model_lm2)

# Evaluate the total length
model_Locale <- lmer(WingLength ~ TotalLength + Sex*Species + (1|Locale), data = damsel_data)

# About 5% variation coming from location
summary(model_Locale)

# interested in the wing length body length relationship. Year is a nested random effect
# (every year we go to the same locations to sample). No interaction between wing length and total length,
# interaction between sex and species
# All slopes are the same, but we have different intercepts
model_LocaleYear <- lmer(WingLength ~ TotalLength + Sex*Species + (1|Year/Locale), data = damsel_data)

# See the "Random effects" section of the summary
summary(model_LocaleYear)

model_LocaleYear2 <- lmer(WingLength ~ TotalLength + Sex*Species + (1|Year/Locale), data = damsel_data)

summary(model_LocaleYear2)
coef(summary(model_LocaleYear2))

ggplot(data = damsel_data, aes(x = TotalLength, y = WingLength, color = interaction(Sex, Species)))+
	geom_point(alpha = 0.1)+
 	geom_abline(intercept = coef(summary(model_LocaleYear2))[1,1], slope = coef(summary(model_LocaleYear2))[2,1], lty = 1, color = scales::hue_pal()(4)[1])+#female Enallagma
 	 	geom_abline(intercept = coef(summary(model_LocaleYear2))[1,1]+coef(summary(model_LocaleYear2))[3,1], slope = coef(summary(model_LocaleYear2))[2,1], lty = 1, color = scales::hue_pal()(4)[2])+#male Enallagma
 	 	geom_abline(intercept = coef(summary(model_LocaleYear2))[1,1]+coef(summary(model_LocaleYear2))[4,1], slope = coef(summary(model_LocaleYear2))[2,1], lty = 1, color = scales::hue_pal()(4)[3])+#female Ischnura
 	 	geom_abline(intercept = coef(summary(model_LocaleYear2))[1,1]+coef(summary(model_LocaleYear2))[3,1]+coef(summary(model_LocaleYear2))[5,1]+coef(summary(model_LocaleYear2))[3,1], slope = coef(summary(model_LocaleYear2))[2,1], lty = 1, color = scales::hue_pal()(4)[4])+#male Ischnura
	theme_classic()+
	#geom_smooth(method = "lm")+
	theme(legend.position = "top")+
	guides(color=guide_legend(ncol=2))