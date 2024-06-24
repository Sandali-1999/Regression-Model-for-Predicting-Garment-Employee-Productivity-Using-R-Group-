getwd()

data=read.csv("C:/Users/I C Sathsara/Desktop/GWP.csv")
data

#setwd("C:\\Users\\I C Sathsara\\Desktop")

#install.packages("tidyverse")
library(tidyverse)
glimpse(GWP)

names(GWP)

unique(GWP$quarter)
unique(GWP$department)
unique(GWP$day)

#Categorical variables
GWP$quarter<-as.factor(GWP$quarter)
GWP$department<-as.factor(GWP$department)
GWP$day<-as.factor(GWP$day)

levels(GWP$quarter)
levels(GWP$department)
levels(GWP$day)

#Rename categories in the department variable.
levels(GWP$department) <- list(sweing="sweing", finishing="finishing", finishing="finishing ")
unique(GWP$department)

#Finding missing values.
colSums(is.na(GWP))

#Replace the missing values with zero
GWP$wip[is.na(GWP$wip)] <- 0

#Find is there any duplication.
sum(duplicated(GWP))#if this zero we can say no duplication.


#Notice that team number is categorical variable.It is just a mention number
#for a team.So we do not consider that variable .

######Forward selection###############
attach(GWP)
#cor(actual_productivity,idle_men)
#null
lm_null=lm(actual_productivity~1)
anova(lm_null)
summary(lm_null)

lm_q=lm(actual_productivity~quarter) #1
anova(lm_q)

lm_smv=lm(actual_productivity~smv) #2
anova(lm_smv)

lm_dep=lm(actual_productivity~department) #3
anova(lm_dep)

lm_target=lm(actual_productivity~targeted_productivity)#4
anova(lm_target)

lm_wip=lm(actual_productivity~wip) #5
anova(lm_wip)

lm_ov=lm(actual_productivity~over_time)#6
anova(lm_ov)

lm_inc=lm(actual_productivity~incentive)#7
anova(lm_inc)

lm_t=lm(actual_productivity~idle_time) #8
anova(lm_t)

lm_men=lm(actual_productivity~idle_men) #9
anova(lm_men)

lm_style=lm(actual_productivity~no_of_style_change) #10
anova(lm_style)

lm_wo=lm(actual_productivity~no_of_workers) #11
anova(lm_wo)

lm_day=lm(actual_productivity~day) #12
anova(lm_day)


#Targeted_productivity entered into model

lm_tq=lm(actual_productivity~targeted_productivity+quarter) #1
anova(lm_tq)

lm_tsmv=lm(actual_productivity~targeted_productivity+smv)#2
anova(lm_tsmv)

lm_tdep=lm(actual_productivity~targeted_productivity+department) #3
anova(lm_tdep)

lm_twip=lm(actual_productivity~targeted_productivity+wip)#5
anova(lm_wip)

lm_tover=lm(actual_productivity~targeted_productivity+over_time) #6
anova(lm_tover)

lm_tinc=lm(actual_productivity~targeted_productivity+incentive)#7
anova(lm_tinc)

lm_ttime=lm(actual_productivity~targeted_productivity+idle_time)#8
anova(lm_ttime)

lm_tmen=lm(actual_productivity~targeted_productivity+idle_men)#9
anova(lm_tmen)

lm_tchange=lm(actual_productivity~targeted_productivity+no_of_style_change)#10
anova(lm_tchange)

lm_twork=lm(actual_productivity~targeted_productivity+no_of_workers)#11
anova(lm_twork)

lm_tday=lm(actual_productivity~targeted_productivity+day)#12
anova(lm_tday)

#add idle_men to the model

lm_tmeq=lm(actual_productivity~targeted_productivity+idle_men+quarter)#1
anova(lm_tmeq)

lm_tmsmv=lm(actual_productivity~targeted_productivity+idle_men+smv)#2
anova(lm_tmsmv)

lm_tmdep=lm(actual_productivity~targeted_productivity+idle_men+department)#3
anova(lm_tmdep)

lm_tmwip=lm(actual_productivity~targeted_productivity+idle_men+wip)#5
anova(lm_tmwip)

lm_tmov=lm(actual_productivity~targeted_productivity+idle_men+over_time)#6
anova(lm_tmov)

lm_tminc=lm(actual_productivity~targeted_productivity+idle_men+incentive)#7
anova(lm_tminc)

lm_tmtime=lm(actual_productivity~targeted_productivity+idle_men+idle_time)#8
anova(lm_tmtime)

lm_tmchange=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change)#10
anova(lm_tmchange)

lm_tmwork=lm(actual_productivity~targeted_productivity+idle_men+no_of_workers)#11
anova(lm_tmwork)

lm_tmday=lm(actual_productivity~targeted_productivity+idle_men+day)#12
anova(lm_tmday)


#add no_of_style_change to the model
lm_tmchq=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter)#1
anova(lm_tmchq)

lm_tmchsmv=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+smv)#2
anova(lm_tmchsmv)

lm_tmchdept=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+department)#3
anova(lm_tmchdept)

lm_tmchwip=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+wip)#5
anova(lm_tmchwip)

lm_tmchov=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+over_time)#6
anova(lm_tmchov)

lm_tmchinc=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+incentive)#7
anova(lm_tmchinc)

lm_tmchtime=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+idle_time)#8
anova(lm_tmchtime)

lm_tmchwork=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+no_of_workers)#11
anova(lm_tmchwork)

lm_tmchday=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+day)#12
anova(lm_tmchday)

#add quarter to the model
lm_tmchqsmv=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+smv)#2
anova(lm_tmchqsmv)

lm_tmchqdept=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+department)#3
anova(lm_tmchqdept)

lm_tmchqwip=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+wip)#5
anova(lm_tmchqwip)

lm_tmchqov=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+over_time)#6
anova(lm_tmchqov)

lm_tmchqinc=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive)#7
anova(lm_tmchqinc)

lm_tmchqtime=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+idle_time)#8
anova(lm_tmchqtime)

lm_tmchqwork=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+no_of_workers)#11
anova(lm_tmchqwork)

lm_tmchqday=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+day)#12
anova(lm_tmchqday)

#Add incentive to the model

lm_tmchqincsmv=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv)#2
anova(lm_tmchqincsmv)

lm_tmchqincdept=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+department)#3
anova(lm_tmchqincdept)

lm_tmchqincwip=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+wip)#5
anova(lm_tmchqincwip)

lm_tmchqincov=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+over_time)#6
anova(lm_tmchqincov)

lm_tmchqinctime=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+idle_time)#8
anova(lm_tmchqinctime)

lm_tmchqincwork=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+no_of_workers)#11
anova(lm_tmchqincwork)

lm_tmchqincday=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+day)#12
anova(lm_tmchqincday)


#Add smv to the model
lm_tmchqincsmvdept=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+department)#3
anova(lm_tmchqincsmvdept)

lm_tmchqincsmvwip=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+wip)#5
anova(lm_tmchqincsmvwip)

lm_tmchqincsmvov=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+over_time)#6
anova(lm_tmchqincsmvov)

lm_tmchqincsmvtime=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+idle_time)#8
anova(lm_tmchqincsmvtime)

lm_tmchqincsmvwork=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers)#11
anova(lm_tmchqincsmvwork)

lm_tmchqincsmvday=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+day)#12
anova(lm_tmchqincsmvday)


#Add no_of_workers to the model
lm_tmchqincsmvworkdept=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers+department)#3
anova(lm_tmchqincsmvworkdept)

lm_tmchqincsmvworkwip=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers+wip)#5
anova(lm_tmchqincsmvworkwip)

lm_tmchqincsmvworkov=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers+over_time)#6
anova(lm_tmchqincsmvworkov)

lm_tmchqincsmvworktime=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers+idle_time)#8
anova(lm_tmchqincsmvworktime)

lm_tmchqincsmvworkday=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers+day)#9
anova(lm_tmchqincsmvworkday)

#Add department to the model

lm_tmchqincsmvworkdeptwip=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers+department+wip)#5
anova(lm_tmchqincsmvworkdeptwip)

lm_tmchqincsmvworkdeptov=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers+department+over_time)#6
anova(lm_tmchqincsmvworkdeptov)

lm_tmchqincsmvworkdepttime=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers+department+idle_time)#8
anova(lm_tmchqincsmvworkdepttime)

lm_tmchqincsmvworkdeptday=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers+department+day)#9
anova(lm_tmchqincsmvworkdeptday)

#final model
lm_tmchqincsmvworkdept=lm(actual_productivity~targeted_productivity+idle_men+no_of_style_change+quarter+incentive+smv+no_of_workers+department)#3
anova(lm_tmchqincsmvworkdept)
summary(lm_tmchqincsmvworkdept)

plot(lm_tmchqincsmvworkdept)


#vif(lm_tmchqincsmvworkdept)
#install.packages("carTools")


#############################################################################
lm_try1=lm(actual_productivity~targeted_productivity)
summary(lm_try1) #R^2=17.77%

lm_try2=lm(actual_productivity~targeted_productivity+smv+no_of_workers+no_of_workers*smv)
summary(lm_try2) #R^2=20.97%

lm_try3=lm(actual_productivity~over_time+no_of_workers+no_of_workers*over_time)
summary(lm_try3) #R^2=0.44%

lm_try4=lm(actual_productivity~over_time+smv+smv*over_time)
summary(lm_try4) #R^2=1.733%

##############################################################################

#backward elimination considering R square values

lm_full=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers+day)
summary(lm_full)
anova(lm_full)

 #dropping predictors

#dropping quarter
lm_nq=lm(actual_productivity~department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_nq)
#dropping department
lm_nd=lm(actual_productivity~quarter+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_nd)
#dropping targeted_productivity
lm_np=lm(actual_productivity~quarter+department+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_np)
#dropping smv
lm_ns=lm(actual_productivity~quarter+department+targeted_productivity+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_ns)
#dropping wip
lm_nw=lm(actual_productivity~quarter+department+targeted_productivity+smv+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_nw)
#dropping over time
lm_no=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_no)
#incentive
lm_ni=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_ni)
#idle time
lm_nt=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_men+no_of_style_change+no_of_workers)
summary(lm_nt)
#idle men
lm_nm=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+no_of_style_change+no_of_workers)
summary(lm_nm)
#no of style change
lm_nsc=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_workers)
summary(lm_nsc)
#no of workers
lm_nw=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change)
summary(lm_nw)
#dropping day
lm_nd=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_nd)

#the model without day lead to an improvement in adjusted R square

lm_nd=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_nd)
#drop quarter 
lm_ndq=lm(actual_productivity~department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_ndq)

#drop department
lm_ndd=lm(actual_productivity~quarter+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_ndd)

#drop targeted productivity
lm_ndp=lm(actual_productivity~quarter+department+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_ndp)

#drop smv
lm_nds=lm(actual_productivity~quarter+department+targeted_productivity+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_nds)

#drop wip
lm_ndw=lm(actual_productivity~quarter+department+targeted_productivity+smv+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_ndw)

#drop over time
lm_ndo=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_ndo)

#drop incentive
lm_ndi=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_ndi)

#drop idle time
lm_ndt=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_men+no_of_style_change+no_of_workers)
summary(lm_ndt)

#drop idle men
lm_ndm=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+no_of_style_change+no_of_workers)
summary(lm_ndm)

#drop no of style change
lm_ndc=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_workers)
summary(lm_ndc)

#drop no of workers
lm_ndw=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change)
summary(lm_ndw)

#not any model improve adjusted R square 
#Therefore final model using Backward elimination R square appraoch is:
lm_nd=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_nd)
plot(lm_nd)


hist(GWP$actual_productivity)

lm_full=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers+day)
summary(lm_full)

#Backward elimination  p value approach
lm_full=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers+day)
anova(lm_full)
summary(lm_full)
#remove day
lm_nd=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers)
summary(lm_nd)
anova(lm_nd)

#remove idle_time
lm_withouttime=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_men+no_of_style_change+no_of_workers)
summary(lm_withouttime)

#remove wip
lm_withoutwip=lm(actual_productivity~quarter+department+targeted_productivity+smv+over_time+incentive+idle_men+no_of_style_change+no_of_workers)
summary(lm_withoutwip)

#without overtime
lm_withoutov=lm(actual_productivity~quarter+department+targeted_productivity+smv+incentive+idle_men+no_of_style_change+no_of_workers)
summary(lm_withoutov)

#without incentive
lm_withoutinc=lm(actual_productivity~quarter+department+targeted_productivity+smv+idle_men+no_of_style_change+no_of_workers)
summary(lm_withoutinc)
anova(lm_withoutinc)

#No any p value>alpha .Therefore stop
#final model from backward eliminaton is:
lm_withoutinc=lm(actual_productivity~quarter+department+targeted_productivity+smv+idle_men+no_of_style_change+no_of_workers)


######################################################################################################


#sql=log(actual_productivity,2)
#lm_sc=lm(sql~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers+day)
#summary(lm_sc)


#Stepwise selection

library(MASS)

#with two way interactions

lm_full1=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers+day+quarter*department+quarter*day+quarter*targeted_productivity+quarter*smv+quarter*wip+quarter*over_time+quarter*incentive+quarter*idle_time+quarter*idle_men+quarter*no_of_style_change+quarter*no_of_workers+department*day+department*targeted_productivity+department*smv+department*wip+department*over_time+department*incentive+department*idle_time+department*idle_men+department*no_of_style_change+department*no_of_workers+day*targeted_productivity+day*smv+day*wip+day*over_time+day*incentive+day*idle_time+day*idle_men+day*no_of_style_change+day*no_of_workers+targeted_productivity*smv+targeted_productivity*wip+targeted_productivity*over_time+targeted_productivity*incentive+targeted_productivity*idle_time+targeted_productivity*idle_men+targeted_productivity*no_of_style_change+targeted_productivity*no_of_workers+smv*wip+smv*over_time+smv*incentive+smv*idle_time+smv*idle_men+smv*no_of_style_change+smv*no_of_workers+wip*over_time+wip*incentive+wip*idle_time+wip*idle_men+wip*no_of_style_change+wip*no_of_workers+over_time*incentive+over_time*idle_time+over_time*idle_men+over_time*no_of_style_change+over_time*no_of_workers+incentive*idle_time+incentive*idle_men+incentive*no_of_style_change+incentive*no_of_workers+idle_time*idle_men+idle_time*no_of_style_change+idle_time*no_of_workers+idle_men*no_of_style_change+idle_men*no_of_workers+no_of_style_change*no_of_workers)
step_model=stepAIC(lm_full1, direction = "both", trace = FALSE)
summary(step_model)

back_model=stepAIC(lm_full1, direction = "backward", trace = FALSE)
summary(back_model)

forward_model=stepAIC(lm_full1, direction = "forward", trace = FALSE)
summary(forward_model)

plot(step_model)
plot(back_model)
plot(forward_model)


#Step model is better

#with three way interactions

lm_full2=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers+day+quarter*department+quarter*day+quarter*targeted_productivity+quarter*smv+quarter*wip+quarter*over_time+quarter*incentive+quarter*idle_time+quarter*idle_men+quarter*no_of_style_change+quarter*no_of_workers+department*day+department*targeted_productivity+department*smv+department*wip+department*over_time+department*incentive+department*idle_time+department*idle_men+department*no_of_style_change+department*no_of_workers+day*targeted_productivity+day*smv+day*wip+day*over_time+day*incentive+day*idle_time+day*idle_men+day*no_of_style_change+day*no_of_workers+targeted_productivity*smv+targeted_productivity*wip+targeted_productivity*over_time+targeted_productivity*incentive+targeted_productivity*idle_time+targeted_productivity*idle_men+targeted_productivity*no_of_style_change+targeted_productivity*no_of_workers+smv*wip+smv*over_time+smv*incentive+smv*idle_time+smv*idle_men+smv*no_of_style_change+smv*no_of_workers+wip*over_time+wip*incentive+wip*idle_time+wip*idle_men+wip*no_of_style_change+wip*no_of_workers+over_time*incentive+over_time*idle_time+over_time*idle_men+over_time*no_of_style_change+over_time*no_of_workers+incentive*idle_time+incentive*idle_men+incentive*no_of_style_change+incentive*no_of_workers+idle_time*idle_men+idle_time*no_of_style_change+idle_time*no_of_workers+idle_men*no_of_style_change+idle_men*no_of_workers+no_of_style_change*no_of_workers+quarter*department*day+quarter*department*targeted_productivity+quarter*department*smv+ quarter*department*wip+quarter*department*over_time+quarter*department*incentive+quarter*department*idle_time+quarter*department*idle_men+quarter*department*no_of_style_change+quarter*department*no_of_workers+quarter*day* targeted_productivity+ quarter*day* smv+quarter*day* wip+quarter*day* over_time+quarter*day* incentive+quarter*day* idle_time+quarter*day* idle_men+quarter*day* no_of_style_change+quarter*day* no_of_workers+ quarter*targeted_productivity* smv+quarter*targeted_productivity* wip+quarter*targeted_productivity* over_time+quarter*targeted_productivity* incentive+quarter*targeted_productivity* idle_time+quarter*targeted_productivity* idle_men+quarter*targeted_productivity* no_of_style_change+quarter*targeted_productivity* no_of_workers+ quarter*smv* wip+quarter*smv* over_time+quarter*smv* incentive+quarter*smv* idle_time+quarter*smv* idle_men+quarter*smv* no_of_style_change+quarter*smv* no_of_workers+ quarter*wip *over_time+quarter*wip *incentive+quarter*wip *idle_time+quarter*wip *idle_men+quarter*wip *no_of_style_change+quarter*wip *no_of_workers+ quarter*over_time* incentive+quarter*over_time* idle_time+quarter*over_time* idle_men+quarter*over_time* no_of_style_change+quarter*over_time* no_of_workers+ quarter*incentive* idle_time+quarter*incentive* idle_men+quarter*incentive* no_of_style_change+quarter*incentive* no_of_workers  +  quarter*idle_time*  idle_time +  quarter*idle_time*  idle_men+quarter*idle_time*  no_of_style_change+quarter*idle_time*  no_of_workers+ quarter*idle_men  *no_of_style_change+quarter*idle_men*no_of_workers+ quarter*no_of_style_change*no_of_workers) 
stepmodel3=stepAIC(lm_full2, direction = "both", trace = FALSE)
summary(stepmodel3)

lm_full=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers+day)
stepmodelno3=stepAIC(lm_full, direction = "both", trace = FALSE)
summary(stepmodelno3)


###### Checking the assumptions of the MLR model #######

#01.Linearity between the response variable and predictors 
#01.Scatterplot of residuals vs fitted values

# Calculate standardized residuals
std_resid <- rstandard(step_model)
# Create a data frame containing fitted values and standardized residuals
data <- data.frame(Fitted = fitted(step_model), Standardized_Residuals = std_resid)
# Create a scatterplot
ggplot(data, aes(x = Fitted, y = Standardized_Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Fitted Values vs. Standardized Residuals Scatterplot")


install.packages("stats")
library(stats)

# Perform the runs test
runs_test_result <- runs.test(residuals)

# Print the results
cat("Runs Test Results:\n")
cat("Test Statistic:", runs_test_result$statistic, "\n")
cat("P-value:", runs_test_result$p.value, "\n")
cat("Conclusion:", ifelse(runs_test_result$p.value < 0.05, "Reject randomness/null hypothesis", "Fail to reject randomness/null hypothesis"), "\n")



