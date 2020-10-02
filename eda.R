library(tidyverse)
library(GGally)
##########################################################

velocity <- read_csv("velocity.csv")
v_movement <- read_csv("v-movement.csv")
h_movement <- read_csv("h-movement.csv")
pitch_type <- read_csv("pitch_type.csv")
pitch_value <- read_csv("pitch_value.csv")
basic <- read_csv("basic.csv")

all <- left_join(velocity,v_movement,by=c("Name","Season","Team","IP","playerid"))
all <- left_join(all,h_movement,by=c("Name","Season","Team","IP","playerid"))
all <- left_join(all,pitch_value,by=c("Name","Season","Team","IP","playerid"))
all <- left_join(all,pitch_type,by=c("Name","Season","Team","IP","playerid"))
all <- left_join(all,basic,by=c("Name","Season","Team","IP","playerid"))
save(all, file = "all.Rda")

length(all)

# glimpse(all)
model <- lm(WAR~`K/9`,all %>% filter(vFA > 80))
summary(model)
ggplot(data=all,aes(x=`K/9`,y=WAR))+
  geom_point()+
  geom_smooth(method="lm")


# We must handle missing data before proceeding with 
# model selection and analysis

library(VIM)
aggr(all)
# Remove variables where over 20% of data is missing
all_sigVars <- all[ lapply( all, function(x) sum(is.na(x)) / length(x) ) < 0.2 ]
aggr(all_sigVars)
# We now have data for 4 pitches: 
# fastball, sinker, changeup, and curveball

# Make variables with percentages real numeric proportions
all_sigVars <- all_sigVars %>% 
  mutate(`FA%` = as.numeric(str_sub(`FA%`, end = -3))/100,
         `SI%` = as.numeric(str_sub(`SI%`, end = -3))/100,
         `CH%` = as.numeric(str_sub(`CH%`, end = -3))/100,
         `CU%` = as.numeric(str_sub(`CU%`, end = -3))/100,
         `LOB%` = as.numeric(str_sub(`LOB%`, end = -3))/100,
         `GB%` = as.numeric(str_sub(`GB%`, end = -3))/100,
         `HR/FB` = as.numeric(str_sub(`HR/FB`, end = -3))/100)

library(mice)
# This next line of code gets rid of "%" in certain variable names.
# Mice does not seem to be able to run without those removed
all_sigVars <- data.frame(all_sigVars)
all_noNa <- complete(mice(data=all_sigVars, m=1, method="mean"))
aggr(all_noNa)

# ggscatmat on just a couple variables
test <- na_if(all_noNa[c(1:27,29:44)],0) %>% 
  na.omit()
ggscatmat(test, columns = 5:8)

# Add a variable that measures verticle, horizontal, and 
# hypotenuseic distance between fastmall movement and curveball movement
all_noNa <- all_noNa %>% 
  mutate(FA.CU.diff.X = abs(FA.X - CU.X),
         FA.CU.diff.Z = abs(FA.Z - CU.Z),
         FA.CU.DIFF = sqrt(FA.CU.diff.X^2 + FA.CU.diff.Z^2))

# All missing data is handled through imuptation, we can now
# proceed with model selection and analysis
base <- lm(WAR~1,all_noNa)
model <- lm(WAR~vFA+vSI+vCH+vCU+FA.Z+SI.Z+CH.Z+
              CU.Z+FA.X+SI.X+CH.X+CU.X+
              wFA.C+wSI.C+wCH.C+wCU.C+FA.+
              SI.+CH.+CU.+FA.CU.diff.X+FA.CU.diff.Z+
              FA.CU.DIFF+K.9+BB.9+HR.9+BABIP+
              LOB.+HR.FB+ERA+FIP+xFIP,all_noNa)

# model <- lm(WAR~vFA+vSI+vCH+vCU+FA.Z+SI.Z+CH.Z+
#               CU.Z+FA.X+SI.X+CH.X+CU.X+wFA.C+wSI.C+
#               wCH.C+wCU.C+FA.+SI.+CH.+CU.,all_noNa)

summary(model)
step(model, scope = list(upper = model, lower = base), direction = "backward")#, trace = FALSE)
step(base, scope = list(upper = model, lower = base), direction = "forward")#, trace = FALSE)
step(base, scope = list(upper = model, lower = base), direction = "both")#, trace = FALSE)

# Backward gave us the best adjusted r^2 anmd lowest AIC
summary(
  lm(formula = WAR ~ vFA + vCU + FA.Z + SI.Z + SI.X + CH.X + wCH.C + 
       wCU.C + CH. + FA.CU.DIFF + K.9 + BB.9 + HR.9 + LOB. + HR.FB + 
       xFIP, data = all_noNa)
)
summary(
  lm(formula = WAR ~ vFA + FA.Z + vCH + SI.X + CH.Z + CH. + CU.X + 
       FA.CU.DIFF + CH.X + SI. + FA. + SI.Z, data = all_noNa)
)
summary(
  lm(formula = WAR ~ vFA + FA.Z + vCH + SI.X + CH.Z + CH. + CU.X + 
       FA.CU.DIFF + CH.X + SI. + FA. + SI.Z, data = all_noNa)
)

# Try to predict innings pitched per game
# We only want to look at starting pitchers
# Filter GS/G > .8
starters <- all_noNa %>% 
  filter(GS/G == 1) %>% 
  mutate(IP.GS = IP / GS)

# How good is IP/GS at predicting WAR?
summary(lm(WAR~IP.GS,starters))
ggplot(data=starters,aes(x=IP.GS,y=WAR))+
  geom_point()+
  geom_smooth(method="lm")
# Very good, R^2 = 41.26%

# Now, let's try to predict a pitcher's IP/GP
base <- lm(IP.GS~1,starters)
model <- lm(IP.GS~vFA+vSI+vCH+vCU+FA.Z+SI.Z+CH.Z+
              CU.Z+FA.X+SI.X+CH.X+CU.X+wFA.C+wSI.C+
              wCH.C+wCU.C+FA.+SI.+CH.+CU.,starters)

summary(model)
step(model, scope = list(upper = model, lower = base), direction = "backward", trace = FALSE)

summary(
  lm(formula = IP.GS ~ vFA + vSI + vCH + vCU + FA.Z + CU.Z + FA.X + 
       CH.X + CU.X + wFA.C + wSI.C + wCH.C + wCU.C + FA. + CH. + 
       CU., data = starters)
)
