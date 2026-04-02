# ---------------------------
# R version: 3.5.2
# Code: 4/06/2019
# Title: 'Descriptive and Substantive Representation in Congress: Evidence from 80,000 Congressional Inquiries'
# Authors: Kenneth Lowande, Melinda Richtie, and Erinn Lauterbach
# Summary: Replication code for producing all tables and figures. 
# Please report errors to: lowande@umich.edu
# ---------------------------

cat("\014")
rm(list=ls())
# install.packages(c('cem','ggplot2','foreach','doBy','doMC','stargazer','MASS','sandwich','lmtest','doRNG'))
# foreach/cem installation + Mac will require https://www.xquartz.org/

source('cluster-se.R')
source('split-del.R')

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("MASS", "cem", "foreach", "doMC", "doRNG", 
         "tidyverse", "ggplot2", "stargazer",
         "patchwork", "showtext", "ggtext", "xtable",
         "purrr", "systemfonts",
         "ggfortify", "usmap"),  pkgTest)

showtext_auto()

all_fonts <- system_fonts()

font_table <- all_fonts %>%
  dplyr::group_by(family) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup()

purrr::walk2(font_table$family, font_table$path, function(fam, path) {
  try(font_add(fam, regular = path), silent = TRUE)
})

showtext_auto()


SVU <- theme_minimal(base_family = "Tahoma", base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(margin = margin(t = 10, b = 10), 
                                  face = "bold", size = rel(1.7)),
        plot.subtitle = element_text(face = "plain", size = rel(1.3),
                                     color = "grey40"),
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", 
                                  size = rel(1.1), hjust = 0.5),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10, 
                                                    b = 10), hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 10,
                                                    l = 8), hjust = 0.5),
        strip.background = element_rect(fill = "grey90", color = NA),
        panel.border = element_rect(color = "grey90", fill = NA),
        palette.colour.discrete = function(n)
          scales::pal_viridis(option = "D")(n),
        palette.fill.discrete = function(n) 
          scales::pal_viridis(option = "D")(n),
        palette.colour.continuous = scales::pal_viridis(option = "D"),
        palette.fill.continuous = scales::pal_viridis(option = "D"))

# ---------------------------
# PRINT ARTICLE
# ---------------------------

# Figure 1: Descriptive Representation in Congress
load('f1-house.Rdata')
load('f1-senate.Rdata')
clrs <- c('#d21a1d','#0000b3','#ABABAB')
f1a=ggplot(house, aes(x=cong,y=no,color=ident)) + geom_line(size=1.2) + geom_point() + 
  labs(x="Congress",y="Representatives", title = "House of Representatives") +
    SVU + 
  scale_colour_manual(values=clrs) + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), legend.position="none")
f1b=ggplot(senate, aes(x=cong,y=no,color=ident)) + 
  geom_line(size=1.2) + geom_point() + 
  labs(x="Congress",y="Representatives", title = "Senate") +
    SVU + scale_colour_manual(values=clrs) + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), legend.title=element_blank())
representation <- f1a + f1b

# ---------------------------

# Table 1: Data Coverage for Legislator Interventions; summary table created manually from FOIA records.
# ---------------------------

# Figure 2: Representatives and the Represented
load('f2-reps.Rdata') # House member and district characteristics in 108-111th Congress
b <- summaryBy(vet.any ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # veteran legislators
c <- summaryBy(vet.perc ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # veteran population
d <- summaryBy(female ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # female legislators
e <- summaryBy(white.pop ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # racial/ethnic minority population
f <- summaryBy(white ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # racial/ethnic minority legislators
g <- summaryBy(tw.ideo.mean ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # ideology
load('f2-usa.Rdata') # US House district map (limited to continguous states)
f2a=ggplot() + geom_map(data = b, aes(map_id = GEO_ID, fill = vet.any), map = usa.f) +
  SVU + # My theme settings for fonts, etc.
  labs(title = "Veteran Representation") +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#FFFFFF', mid = '#C0C0C0', high = '#000000', midpoint=0.2) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) 
f2b=ggplot() + geom_map(data = c, aes(map_id = GEO_ID, fill = vet.perc), map = usa.f) +
  SVU +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#FFFFFF', mid = '#C0C0C0', high = '#000000', midpoint=0.06) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(title = "Veteran Populations")

f2c=ggplot() + geom_map(data = d, aes(map_id = GEO_ID, fill = female), map = usa.f) +
  SVU +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#FFFFFF', mid = '#C0C0C0', high = '#000000', midpoint=0.2) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(title = "Female Representatives")
f2d=ggplot() + geom_map(data = g, aes(map_id = GEO_ID, fill = tw.ideo.mean), map = usa.f) +
  SVU +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#FFFFFF', mid = '#C0C0C0', high = '#000000', midpoint=-0.25) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(title = "Mean Constituent Ideologies",
       subtitle = "(Tausanovich and Warsaw Scale)")
f2e=ggplot() + geom_map(data = f, aes(map_id = GEO_ID, fill = white), map = usa.f) +
  SVU +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#000000', mid = '#C0C0C0', high = '#FFFFFF', midpoint=0.6) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(title = "Non-White Representatives")
f2f=ggplot() + geom_map(data = e, aes(map_id = GEO_ID, fill = white.pop), map = usa.f) +
  SVU +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#000000', mid = '#808080', high = '#FFFFFF', midpoint=0.55) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(title = "Non-White Constituents")


vet_maps <- f2a + f2b
women_maps <- f2c + f2d
minority_maps <- f2e + f2f

getwd()

# ---------------------------
load('t2-b10-main.Rdata')


glimpse(main)
# Table 2: Representation Differences Across Members with Military Service
# note: summary table created manually from the following commands
main.v1=splitdel(main,7) # cutting df to split delegations, 7 = Any veteran
main.v2=splitdel(main,8) # cutting df to split delegations, 8 = non-reservists
t2.1 <- t.test(main$vet.all.d~main$vet.any) # row 1
t2.2 <- t.test(main.v1$vet.all.d~main.v1$vet.any) # row 2
t2.3 <- t.test(main$vet.all.d~main$vet.mainbranch) # row 3
t2.4 <- t.test(main.v2$vet.all.d~main.v2$vet.mainbranch) # row 4

# - Grabbing Estimates
t2.1est <- round(t2.1$estimate, 2)
t2.2est <- round(t2.2$estimate, 2)
t2.3est <- round(t2.3$estimate, 2)
t2.4est <- round(t2.4$estimate, 2)
# - Grabbing Confidence Interval Values
t2.1ci <- paste(round(abs(t2.1$conf.int[1:2]), 2), collapse = ", ")
t2.2ci <- paste(round(abs(t2.2$conf.int[1:2]), 2), collapse = ", ")
t2.3ci <- paste(round(abs(t2.3$conf.int[1:2]), 2), collapse = ", ")
t2.4ci <- paste(round(abs(t2.4$conf.int[1:2]), 2), collapse = ", ")

df_table2 <- data.frame(
  `Any Veteran` = c(nrow(main), nrow(main.v1), nrow(main), nrow(main.v2)),
  Nonveterans = c(t2.1est[1],t2.2est[1],t2.3est[1],t2.4est[1]),
  Veterans = c(t2.1est[2],t2.2est[2],t2.3est[2],t2.4est[2]),
  `Difference (95% CI)` = c(t2.1ci, t2.2ci,t2.3ci,t2.4ci))
table_2 <- xtable(df_table2, align="llccc")
print(table_2, type = "latex", include.rownames=FALSE)
# ---------------------------

# Table 3: Military Service and Veterans Representation

vars=c('vet.10.12.d','chamber','cong','vet.any','vets.exp')
main.v0=main[complete.cases(main[,vars]),]
main.v0$idno=as.character(main.v0$idno)

# -- Any Vets, all data
v1=lm(vet.all.d~chamber+dwnom1+as.factor(cong)+
        vet.any+log(vets.exp),data=main) 
v1c=cl(main,v1,main$idno) 
# -- Non-reservists, all data
v2=lm(vet.all.d~chamber+dwnom1+as.factor(cong)+
        vet.mainbranch+log(vets.exp),data=main)
v2c=cl(main,v2,main$idno)
# -- Any vets, complete-case data from 110th, 112th congresses
v3=lm(vet.10.12.d~chamber+dwnom1+cong+
        vet.any+log(vets.exp),data=main.v0)
v3c=cl(main.v0,v3,main.v0$idno)
# -- Non-reservists, complete-case data from 110th, 112th congresses
v4=lm(vet.10.12.d~chamber+dwnom1+as.factor(cong)+
        vet.mainbranch+log(vets.exp),data=main.v0)
v4c=cl(main.v0,v4,main.v0$idno)

v_val1 <- round(summary(v1)$adj.r.squared, 2)
v_val2 <- round(summary(v2)$adj.r.squared, 2)
v_val3 <- round(summary(v3)$adj.r.squared, 2)
v_val4 <- round(summary(v4)$adj.r.squared, 2)

stargazer(v1c,v2c,v3c,v4c,digits=3,
          add.lines = list(c("N", nobs(v1), nobs(v2), nobs(v3), nobs(v4)),
                      c("Adjusted $R^2$", v_val1, v_val2, v_val3, v_val4)),
          column.sep.width = "20pt",
          type = "latex")
# ---------------------------

# Table 4: Representation Differences Across Gender
# note: summary table created manually from the following commands
main.w=splitdel(main,6)
t4.1 <- t.test(main$fem.all.d~main$female) # row 1
t4.2 <- t.test(main.w$fem.all.d~main.w$female) # row 2

# -- Estimates
t4.1est <- round(t4.1$estimate, 2)
t4.2est <- round(t4.2$estimate, 2)
# -- Confidence Interval Numbers
t4.1ci <- paste(round(abs(t4.1$conf.int[1:2]), 2), collapse = ", ")
t4.2ci <- paste(round(abs(t4.2$conf.int[1:2]), 2), collapse = ", ")
# -- Constructing the table:
df_table4 <- data.frame(
  Delete = c(nrow(main), nrow(main.w)),
  Male = c(t4.1est[1],t4.2est[1]),
  Female = c(t4.1est[2],t4.2est[2]),
  `Difference (95% CI)` = c(t4.1ci, t4.2ci))
table_4 <- xtable(df_table4, align="llccc")
print(table_4, type = "latex", include.rownames=FALSE)
# ---------------------------

# Table 5: Gender and Women’s Representation
# -- With Commonspace DWNOMINATE Scores
w1=lm(fem.all.d~chamber+dwnom1+as.factor(cong)+
        female,data=main)
w1c=cl(main,w1,main$idno)
# -- With District Mean Ideology
w2=lm(fem.all.d~chamber+tw.ideo.mean+as.factor(cong)+
        female,data=main)
w2c=cl(main,w2,main$idno)
# - Grabbing R-squared's
w_val1 <- round(summary(w1)$adj.r.squared, 2)
w_val2 <- round(summary(w2)$adj.r.squared, 2)

stargazer(w1c,w2c,digits=3,
          add.lines = list(c("N", nobs(w1), nobs(w2)),
                           c("Adjusted $R^2$", w_val1, w_val2)),
          column.sep.width = "20pt",
          type = "latex")
# ---------------------------

# Table 6: Race/Ethnicity and Minority Representation
vars=c('race.ni.12.d','chamber','cong','tw.ideo.mean','nonwhite','white.pop')
main.r0=main[complete.cases(main[,vars]),]
main.r0$idno=as.character(main.r0$idno)

# -- Race Representation with District Mean Ideology
r1=lm(race.ni.d~chamber+as.factor(cong)+tw.ideo.mean+
        nonwhite,data=main)
r1c=cl(main,r1,main$idno)
# -- Race Representation with proportion of district population White
r2=lm(race.ni.d~chamber+as.factor(cong)+white.pop+
        nonwhite,data=main)
r2c=cl(main,r2,main$idno)
# -- District Mean Ideology, complete-case data from 110th, 112th congresses
r3=lm(race.ni.12.d~chamber+as.factor(cong)+tw.ideo.mean+
        nonwhite,data=main.r0)
r3c=cl(main.r0,r3,main.r0$idno)
# -- Proportion of population White, complete-case data from 110th, 112th congresses
r4=lm(race.ni.12.d~chamber+as.factor(cong)+white.pop+
        nonwhite,data=main.r0)
r4c=cl(main.r0,r4,main.r0$idno)

# - Grabbing R-squared's
r_val1 <- round(summary(r1)$adj.r.squared, 2)
r_val2 <- round(summary(r2)$adj.r.squared, 2)
r_val3 <- round(summary(r3)$adj.r.squared, 2)
r_val4 <- round(summary(r4)$adj.r.squared, 2)

stargazer(r1c,r2c,r3c,r4c,digits=3,
          add.lines = list(c("N", nobs(r1), nobs(r2), nobs(r3), nobs(r4)),
                           c("Adjusted $R^2$", r_val1, r_val2, r_val3, r_val4)),
          column.sep.width = "20pt",
          type = "latex")
# ---------------------------
# SHELLY'S TWIST
# ---------------------------


# Table 7: Women for Racial/Ethnic Minorities -----------------------------
# District Ideology Means
w_r1=lm(race.ni.d~chamber+as.factor(cong)+tw.ideo.mean+
        female, data=main)
w_r1c=cl(main,w_r1,main$idno)
# - White Population Prop
w_r2=lm(race.ni.d~chamber+as.factor(cong)+white.pop+
        female,data=main)
w_r2c=cl(main,w_r2,main$idno)
#Complete cases control
vars=c('race.ni.12.d','chamber','cong','tw.ideo.mean','nonwhite', 'female', 'white.pop')
main.w_r0=main[complete.cases(main[,vars]),]
main.w_r0$idno=as.character(main.r0$idno)
# Ideology means (110-112 congresses)
w_r3=lm(race.ni.12.d~chamber+as.factor(cong)+tw.ideo.mean+
        female,data=main.r0)
w_r3c=cl(main.r0,w_r3,main.r0$idno)
# Prop White District (110-112 congresses)
w_r4=lm(race.ni.12.d~chamber+as.factor(cong)+white.pop+
        female,data=main.r0)
w_r4c=cl(main.r0,w_r4,main.r0$idno)

wr_val1 <- round(summary(w_r1)$adj.r.squared, 2)
wr_val2 <- round(summary(w_r2)$adj.r.squared, 2)
wr_val3 <- round(summary(w_r3)$adj.r.squared, 2)
wr_val4 <- round(summary(w_r4)$adj.r.squared, 2)

stargazer(w_r1c,w_r2c,w_r3c,w_r4c,digits=3,
          add.lines = list(c("N", nobs(w_r1), nobs(w_r2), nobs(w_r3), nobs(w_r4)),
                           c("Adjusted $R^2$", wr_val1, wr_val2, wr_val3, wr_val4)),
          column.sep.width = "20pt",
          type = "latex")

# Table 8: Racial/Ethnic Minorities for Women -----------------------------
# - DW NOMINATE Score
r_w1=lm(fem.all.d~chamber+dwnom1+as.factor(cong)+
        nonwhite,data=main)
r_w1c=cl(main,r_w1,main$idno)
# - District Mean Ideology
r_w2=lm(fem.all.d~chamber+tw.ideo.mean+as.factor(cong)+
        nonwhite,data=main)
r_w2c=cl(main,r_w2,main$idno)

rw_val1 <- round(summary(r_w1)$adj.r.squared, 2)
rw_val2 <- round(summary(r_w2)$adj.r.squared, 2)

stargazer(r_w1c,r_w2c, digits=3,
          add.lines = list(c("N", nobs(r_w1), nobs(r_w2)),
                           c("Adjusted $R^2$", rw_val1, rw_val2)),
          column.sep.width = "20pt",
          type = "latex")
# Logit ----------------------------------------------------------
# - Veterans (any branch)
logit_vet.any <- glm(r.vet.10.12.d ~ chamber + dwnom1 + as.factor(cong) + vet.any + log(vets.exp),
                data = main.v0,
                family = binomial(link = "logit"))
logit_va_c=cl(main.v0,logit_vet.any,main.v0$idno)
# - Veterans (non-reservists)
logit_vet.mainbranch <- glm(r.vet.10.12.d ~ chamber + dwnom1 + as.factor(cong) + vet.mainbranch + log(vets.exp),
                     data = main.v0,
                     family = binomial(link = "logit"))
logit_vm_c=cl(main.v0,logit_vet.mainbranch,main.v0$idno)
# Odds Ratios
or_vet.any <- exp(coef(logit_vet.any))
or_vet.main <- exp(coef(logit_vet.mainbranch))
# Standard Errors adjusted for Odds Ratios
se_vet.any <- logit_va_c[, "Std. Error"]* or_vet.any
se_vet.mainbranch <- logit_vm_c[, "Std. Error"]*or_vet.main

# - Women
logit_women <- glm(r.fem.all.d ~ chamber + dwnom1 + as.factor(cong) + 
                     female + tw.ideo.mean,
                data = main,
                family = binomial(link = "logit"))
logit_w_c=cl(main,logit_women,main$idno)
logit_w_c
# Odds Ratios and Standard Errors adjusted for Odds Ratios
or_women <- exp(coef(logit_w_c))
se_women <- logit_w_c[, "Std. Error"]*or_women

# - Minorities
logit_minorities <- glm(r.race.ni.12.d ~ chamber + as.factor(cong) + dwnom1 +
                          tw.ideo.mean + white.pop + nonwhite,
                data = main.r0,
                family = binomial(link = "logit"))
logit_r_c=cl(main.r0,logit_minorities,main.r0$idno)
# Odds Ratios and Standard Errors adjusted for Odds Ratios
or_minorities <- exp(coef(logit_r_c))
se_minorities <- logit_r_c[, "Std. Error"]*or_minorities
# - Veteran's Stargazer Table
stargazer(logit_va_c, logit_vm_c,digits=2,
          type = "latex",
          coef = list(or_vet.any, or_vet.main),
          se = list(
            se_vet.any,
            se_vet.mainbranch
          ),
          keep.stat = c("ll", "n")
          )
# - Women's Stargazer Table
stargazer(logit_w_c, digits=2,
          type = "latex",
          coef = list(or_women),
          se = list(se_women),
          keep.stat = c("ll", "n")
          )
# - Minorities Stargazer Table
stargazer(logit_r_c,digits=2,
          type = "latex",
          coef = list(or_minorities),
          se = list(se_minorities),
          keep.stat = c("ll", "n")
          )
# Maps of Inquiries -------------------------------------------------------
main.10=main[main$cong==110,]
main.11=main[main$cong==111,]
main.12=main[main$cong==112,]

state_fem_10 <- main.10 %>%
  group_by(state.abbrev) %>%
  summarise(
    pct_fem_advocacy = mean(r.fem.all.d, na.rm = TRUE),
    n_legislators = n()
  ) %>%
  mutate(state = state.abbrev)

state_race_10 <- main.10 %>%
  group_by(state.abbrev) %>%
  summarise(
    pct_min_advocacy = mean(r.race.ni.d, na.rm = TRUE),
    n_legislators = n()
  ) %>%
  mutate(state = state.abbrev)

state_vet_10 <- main.10 %>%
  group_by(state.abbrev) %>%
  summarise(
    pct_vet_advocacy = mean(r.vet.all.d, na.rm = TRUE),
    n_legislators = n()
  ) %>%
  mutate(state = state.abbrev)

state_fem_11 <- main.11 %>%
  group_by(state.abbrev) %>%
  summarise(
    pct_fem_advocacy = mean(r.fem.all.d, na.rm = TRUE),
    n_legislators = n()
  ) %>%
  mutate(state = state.abbrev)

state_race_11 <- main.11 %>%
  group_by(state.abbrev) %>%
  summarise(
    pct_min_advocacy = mean(r.race.ni.d, na.rm = TRUE),
    n_legislators = n()
  ) %>%
  mutate(state = state.abbrev)

state_vet_11 <- main.11 %>%
  group_by(state.abbrev) %>%
  summarise(
    pct_vet_advocacy = mean(r.vet.all.d, na.rm = TRUE),
    n_legislators = n()
  )%>%
  mutate(state = state.abbrev)

state_fem_12 <- main.12 %>%
  group_by(state.abbrev) %>%
  summarise(
    pct_fem_advocacy = mean(r.fem.all.d, na.rm = TRUE),
    n_legislators = n()
  )%>%
  mutate(state = state.abbrev)

state_race_12 <- main.12 %>%
  group_by(state.abbrev) %>%
  summarise(
    pct_min_advocacy = mean(r.race.ni.d, na.rm = TRUE),
    n_legislators = n()
  )%>%
  mutate(state = state.abbrev)

state_vet_12 <- main.12 %>%
  group_by(state.abbrev) %>%
  summarise(
    pct_vet_advocacy = mean(r.vet.all.d, na.rm = TRUE),
    n_legislators = n()
  )%>%
  mutate(state = state.abbrev)

pf_110<- plot_usmap(data = state_fem_10, values = 'pct_fem_advocacy', color = "black") +
  SVU +
  scale_fill_continuous(limits = c(0, 1),
                        low = "white", 
                        high = "#7A2E2E") +
  labs(title = "110th Congress") +
  theme(plot.title = element_text(hjust = 0.5),
    legend.position = "none", panel.background=element_blank(),
    panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

pf_111<- plot_usmap(data = state_fem_11, values = 'pct_fem_advocacy', color = "black") +
  SVU +
  scale_fill_continuous(limits = c(0, 1),
                        low = "white", 
                        high = "#7A2E2E") +
  labs(title = "111th Congress") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", panel.background=element_blank(),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

pf_112<- plot_usmap(data = state_fem_12, values = 'pct_fem_advocacy', color = "black") +
  SVU +
  scale_fill_continuous(limits = c(0, 1),
                        low = "white", 
                        high = "#7A2E2E", 
                        name = "Percent Representatives") +
  labs(title = "112th Congress") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right", panel.background=element_blank(),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

womens_rep_maps <- pf_110 + pf_111 + pf_112
ggsave("womens_SVU_maps.pdf", womens_rep_maps,
       width = 17, height = 7, units = "in")

pv_110<- plot_usmap(data = state_vet_10, values = 'pct_vet_advocacy', color = "black") +
  SVU +
  scale_fill_continuous(limits = c(0, 1),
                        low = "white", 
                        high = "#2E4F7A") +
  labs(title = "110th Congress") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none", panel.background=element_blank(),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

pv_111<- plot_usmap(data = state_vet_11, values = 'pct_vet_advocacy', color = "black") +
  SVU +
  scale_fill_continuous(limits = c(0, 1),
                        low = "white", 
                        high = "#2E4F7A") +
  labs(title = "111th Congress") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", panel.background=element_blank(),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

pv_112<- plot_usmap(data = state_vet_12, values = 'pct_vet_advocacy', color = "black") +
  SVU +
  scale_fill_continuous(limits = c(0, 1),
                        low = "white", 
                        high = "#2E4F7A", 
                        name = "Percent Representatives") +
  labs(title = "112th Congress") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right", panel.background=element_blank(),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

pv_110 + pv_111 + pv_112
vets_rep_maps <- pv_110 + pv_111 + pv_112
ggsave("vets_SVU_maps.pdf", vets_rep_maps,
       width = 17, height = 7, units = "in")

pr_110<- plot_usmap(data = state_race_10, values = 'pct_min_advocacy', color = "black") +
  SVU +
  scale_fill_continuous(limits = c(0, 1),
                        low = "white", 
                        high = "#2E6F40") +
  labs(title = "110th Congress") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none", panel.background=element_blank(),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

pr_111<- plot_usmap(data = state_race_11, values = 'pct_min_advocacy', color = "black") +
  SVU +
  scale_fill_continuous(limits = c(0, 1),
                        low = "white", 
                        high = "#2E6F40") +
  labs(title = "111th Congress") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", panel.background=element_blank(),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

pr_112<- plot_usmap(data = state_race_12, values = 'pct_min_advocacy', color = "black") +
  SVU +
  scale_fill_continuous(limits = c(0, 1),
                        low = "white", 
                        high = "#2E6F40", 
                        name = "Percent Representatives") +
  labs(title = "112th Congress") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right", panel.background=element_blank(),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

pr_110 + pr_111 + pr_112
minority_rep_maps <- pr_110 + pr_111 + pr_112
ggsave("minority_SVU_maps.pdf", minority_rep_maps,
       width = 17, height = 7, units = "in")
# Variable table for LateX ------------------------------------------------

row_1 <- c("vet.all.d", "dichotomous, 1 if member contacted on behalf of
veterans at least once")
row_2 <- c("vet.10.12.d", "dichotomous, 1 if member contacted on behalf
of veterans at least once in 110-112 Congress and complete agency coverage")
row_3 <- c("fem.all.d", "dichotomous, 1 if member contacted on behalf of
women at least once")
row_4 <- c("race.ni.d", "dichotomous, 1 if member contacted on behalf of
racial/ethnic minorities at least once")
row_5 <- c("race.ni.12.d", "dichotomous, 1 if member contacted on behalf
of racial/ethnic minorities at least once in 110-112
Congress and complete agency coverage")

row_6 <- c("vets.exp", "numeric, veteran-related expenditures in
district")
row_7 <- c("chamber", "factor, Chamber")
row_8 <- c("dwnom1", "continuous, Commonspace DWNOMINATE score")
row_9 <- c("cong", "numeric, Congress number")
row_10 <- c("tw.ideo.mean", "mean constituent ideology from Tausanovitch
            and Warshaw")
row_11 <- c("white.pop", "percent white population from American
Community Survey")

row_12 <- c("female", "dichotomous, 1 if female legislator")
row_13 <- c("vet.any", "dichotomous, 1 if military service background
legislator")
row_14 <- c("vet.mainbranch", "dichotomous, 1 if military service
background legislator who served in something other than
only reserves/national guard")
row_15 <- c("nonwhite", "dichotomous, 1 if racial/ethnic minority
background legislator")

variables <- rbind(row_1, row_2, row_3, row_4, row_5,
                   row_6, row_7, row_8, row_9, row_10, row_11,
                   row_12, row_13, row_14, row_15)
variables <- as.data.frame(variables, stringsAsFactors = FALSE)
colnames(variables) <- c("Variable", "Description")
rownames(variables) <- NULL

print(
  xtable(variables, include.rownames = FALSE),
  align = c("l", "p{4cm}", "p{9cm}"),
  include.colnames = TRUE,
  sanitize.text.function = identity,
  booktabs = TRUE,
  add.to.row = list(
    pos = list(5, 11),
    command = c("\\addlinespace[0.5em]\n", "\\addlinespace[0.5em]\n")
  ))












# Their Functions ---------------------------------------------------------
# split senate delegations
splitdel=function(data,cat){
  temp=data[data$chamber=='Senate',]
  temp=temp[order(temp$cong,temp$state),]
  temp$spl=0
  for (i in 1:(nrow(temp)-1)) {
    if (temp$state[i]==temp$state[i+1]) {
      if (temp[i,cat]!=temp[i+1,cat]) {
        temp$spl[i]=1
        temp$spl[i+1]=1
      } else {next}
    } else {next}
  }
  temp=temp[temp$spl==1,]
  return(temp)
}

# cluster-robust standard errors
cl   <- function(dat,fm,cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  uj <- uj[!is.na(uj[,1]),]
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) 
}
# ---------------------------
# SUPPLEMENTARY INFORMATION
# ---------------------------

# Table B1: Military Service and Veterans Representation (112th Congress)
main.12=main[main$cong==112,]
main.12$idno=as.character(main.12$idno)
v5=lm(vet.12.d~chamber+dwnom1+
        vet.any+log(vets.exp),data=main.12)
v5c=cl(main.12,v5,main.12$idno)
v6=lm(vet.12.d~chamber+dwnom1+
        vet.mainbranch+log(vets.exp),data=main.12)
v6c=cl(main.12,v6,main.12$idno)
v7=glm.nb(vet.12~chamber+dwnom1+
            vet.any+log(vets.exp),data=main.12)
v7c=cl(main.12,v7,main.12$idno)
v8=glm.nb(vet.12~chamber+dwnom1+
            vet.mainbranch+log(vets.exp),data=main.12)
v8c=cl(main.12,v8,main.12$idno)
stargazer(v5c,v6c,v7c,v8c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B2: Representation Differences Across Members with Military Service (Matching)
# note: summary table created manually from the following commands
th <- levels(main$cong)
house <- levels(main$chamber)
main$log.vets.exp=log(main$vets.exp)
dt.na=main[is.na(main$vet.all.d)==F,c(13,14,7,4,5,28)] # row 1
imbalance(group=dt.na$vet.any,data=dt.na[,c(1,4:6)])
mat1 <-  cem('vet.any',dt.na,cutpoints=list(log.vets.exp=10,tw.ideo.mean=10),
             drop='vet.all.d',grouping=list(cong=th,chamber=house),keep.all=T)
est1 <- att(mat1, vet.all.d ~ vet.any, data = dt.na)
dt.na=main[is.na(main$vet.all.d)==F,c(13,15,7,4,5,28)] # row 2
imbalance(group=dt.na$vet.any,data=dt.na[,c(1,4:6)])
mat2 <-  cem('vet.any',dt.na,cutpoints=list(log.vets.exp=10),
             drop='vet.10.12.d',grouping=list(chamber=house),keep.all=T)
est2 <- att(mat2, vet.10.12.d ~ vet.any, data = dt.na)
dt.na=main[is.na(main$vet.all.d)==F,c(13,14,8,4,5,28)] # row 3
imbalance(group=dt.na$vet.mainbranch,data=dt.na[,c(1,4:6)])
mat3 <-  cem('vet.mainbranch',dt.na,cutpoints=list(log.vets.exp=10,tw.ideo.mean=10),
             drop='vet.all.d',grouping=list(cong=th,chamber=house),keep.all=T)
est3 <- att(mat3, vet.all.d ~ vet.mainbranch, data = dt.na)
dt.na=main[is.na(main$vet.all.d)==F,c(13,15,8,4,5,28)] # row 4
imbalance(group=dt.na$vet.mainbranch,data=dt.na[,c(1,4:6)])
mat4 <-  cem('vet.mainbranch',dt.na,cutpoints=list(log.vets.exp=10,tw.ideo.mean=10),
             drop='vet.10.12.d',grouping=list(chamber=house),keep.all=T)
est4 <- att(mat4, vet.10.12.d ~ vet.mainbranch, data = dt.na)
# ---------------------------

# Table B3: Military Service and Veterans Representation (Additional Controls)
main$idno=as.character(main$idno)
v9=lm(vet.10.12.d~chamber+dwnom1+as.factor(cong)+
        vet.any+log(vets.exp)+poverty+seniority,data=main)
v9c=cl(main,v9,main$idno[is.na(main$vet.10.12.d)==F])
v10=lm(vet.10.12.d~chamber+dwnom1+as.factor(cong)+
         vet.mainbranch+log(vets.exp)+poverty+seniority,data=main)
v10c=cl(main,v10,main$idno[is.na(main$vet.10.12.d)==F])
v11=lm(vet.10.12.d~chamber+dwnom1+as.factor(cong)+as.factor(state.abbrev)+
         vet.any+log(vets.exp)+poverty+seniority,data=main)
v11c=cl(main,v11,main$idno[is.na(main$vet.10.12.d)==F])
v12=lm(vet.10.12.d~chamber+dwnom1+as.factor(cong)+as.factor(state.abbrev)+
         vet.mainbranch+log(vets.exp)+poverty+seniority,data=main)
v12c=cl(main,v12,main$idno[is.na(main$vet.10.12.d)==F])
stargazer(v9c,v10c,v11c,v12c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B4: Military Service and Veterans Representation (Alternative Dependent Variable)
v13=lm(r.vet.all.d~chamber+dwnom1+as.factor(cong)+
        vet.any+log(vets.exp),data=main)
v13c=cl(main,v13,main$idno)
v14=lm(r.vet.all.d~chamber+dwnom1+as.factor(cong)+
         vet.mainbranch+log(vets.exp),data=main)
v14c=cl(main,v14,main$idno)
v15=lm(r.vet.10.12.d~chamber+dwnom1+cong+
         vet.any+log(vets.exp),data=main)
v15c=cl(main,v15,main$idno[is.na(main$r.vet.10.12.d)==F])
v16=lm(r.vet.10.12.d~chamber+dwnom1+as.factor(cong)+
         vet.mainbranch+log(vets.exp),data=main)
v16c=cl(main,v16,main$idno[is.na(main$r.vet.10.12.d)==F])
stargazer(v13c,v14c,v15c,v16c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B5: Representation Differences Across Genders (Matching)
# note: summary table created manually from the following commands
dt.wna=main[,c(11,6,4,5,16)] # row 1
imbalance(group=dt.wna$female,data=dt.wna[,c(1,3:4)])
mat5 <-  cem('female',dt.wna,cutpoints=list(dwnom1=10),
             drop='fem.all.d',grouping=list(cong=th,chamber=house),keep.all=T)
est5 <- att(mat5, fem.all.d ~ female, data = dt.wna)
dt.wna=main[,c(13,4,5,16,6)] # row 2
imbalance(group=dt.wna$female,data=dt.wna[,c(1:3)])
mat6 <-  cem('female',dt.wna,cutpoints=list(tw.ideo.mean=10),
             drop='fem.all.d',grouping=list(cong=th,chamber=house),keep.all=T)
est6 <- att(mat6, fem.all.d ~ female, data = dt.wna)
# ---------------------------

# Table B6: Gender and Women’s Representation (Additional Controls)
w3=lm(fem.all.d~chamber+dwnom1+as.factor(cong)+
        female+poverty+seniority,data=main)
w3c=cl(main,w3,main$idno)
w4=lm(fem.all.d~chamber+tw.ideo.mean+as.factor(cong)+
        female+poverty+seniority,data=main)
w4c=cl(main,w4,main$idno)
w5=lm(fem.all.d~chamber+dwnom1+as.factor(cong)+as.factor(state.abbrev)+
        female+poverty+seniority,data=main)
w5c=cl(main,w5,main$idno)
w6=lm(fem.all.d~chamber+tw.ideo.mean+as.factor(cong)+as.factor(state.abbrev)+
        female+poverty+seniority,data=main)
w6c=cl(main,w6,main$idno)
stargazer(w3c,w4c,w5c,w6c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B7: Gender and Women’s Representation (Alternative Dependent Variable)
w7=lm(r.fem.all.d~chamber+dwnom1+as.factor(cong)+
        female,data=main)
w7c=cl(main,w7,main$idno)
w8=lm(r.fem.all.d~chamber+tw.ideo.mean+as.factor(cong)+
        female,data=main)
w8c=cl(main,w8,main$idno)
stargazer(w7c,w8c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B8: Representation Differences Across Race/Ethnicity (Matching)
# note: summary table created manually from the following commands
dt.rna=main[,c(4,5,10,17,9)] # row 1
imbalance(group=dt.rna$nonwhite,data=dt.rna[,c(1:3)]) 
mat7 <-  cem('nonwhite',dt.rna,cutpoints=list(white.pop=10),
             drop='race.ni.d',grouping=list(cong=th,chamber=house),keep.all=T)
est7 <- att(mat7, race.ni.d ~ nonwhite, data = dt.rna)
dt.rna=main[,c(4,5,13,18,9)]  # row 2
imbalance(group=dt.rna$nonwhite,data=dt.rna[,c(1:3)]) 
mat8 <-  cem('nonwhite',dt.rna,cutpoints=list(tw.ideo.mean=10),
             drop='race.ni.12.d',grouping=list(cong=th,chamber=house),keep.all=T)
est8 <- att(mat8, race.ni.12.d ~ nonwhite, data = dt.rna)
# ---------------------------

# Table B9: Race/Ethnicity and Minority Representation (Additional Controls)
r5=lm(race.ni.d~chamber+as.factor(cong)+tw.ideo.mean+
        nonwhite+poverty+seniority,data=main)
r5c=cl(main,r5,main$idno)
r6=lm(race.ni.12.d~chamber+as.factor(cong)+tw.ideo.mean+
        nonwhite+poverty+seniority,data=main)
r6c=cl(main,r6,main$idno[is.na(main$race.ni.12.d)==F])
r7=lm(race.ni.d~chamber+as.factor(cong)+white.pop+
        nonwhite+poverty+seniority,data=main)
r7c=cl(main,r7,main$idno)
r8=lm(race.ni.12.d~chamber+as.factor(cong)+white.pop+
        nonwhite+poverty+seniority,data=main)
r8c=cl(main,r8,main$idno[is.na(main$race.ni.12.d)==F])
r9=lm(race.ni.d~chamber+as.factor(cong)+tw.ideo.mean+as.factor(state.abbrev)+
        nonwhite+poverty+seniority,data=main)
r9c=cl(main,r9,main$idno)
r10=lm(race.ni.12.d~chamber+as.factor(cong)+tw.ideo.mean+as.factor(state.abbrev)+
         nonwhite+poverty+seniority,data=main)
r10c=cl(main,r10,main$idno[is.na(main$race.ni.12.d)==F])
r11=lm(race.ni.d~chamber+as.factor(cong)+white.pop+as.factor(state.abbrev)+
         nonwhite+poverty+seniority,data=main)
r11c=cl(main,r11,main$idno)
r12=lm(race.ni.12.d~chamber+as.factor(cong)+white.pop+as.factor(state.abbrev)+
         nonwhite+poverty+seniority,data=main)
r12c=cl(main,r12,main$idno[is.na(main$race.ni.12.d)==F])
stargazer(r5c,r6,r7c,r8c,r9c,r10c,r11c,r12c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B10: Race/Ethnicity and Minority Representation (Alternative Dependent Variable)
r13=lm(r.race.ni.d~chamber+as.factor(cong)+tw.ideo.mean+
        nonwhite,data=main)
r13c=cl(main,r13,main$idno)
r14=lm(r.race.ni.d~chamber+as.factor(cong)+white.pop+
        nonwhite,data=main)
r14c=cl(main,r14,main$idno)
r15=lm(r.race.ni.12.d~chamber+as.factor(cong)+tw.ideo.mean+
         nonwhite,data=main)
r15c=cl(main,r15,main$idno[is.na(main$r.race.ni.12.d)==F])
r16=lm(r.race.ni.12.d~chamber+as.factor(cong)+white.pop+
        nonwhite,data=main)
r16c=cl(main,r16,main$idno[is.na(main$r.race.ni.12.d)==F])
stargazer(r13c,r14c,r15c,r16c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Figure C1: Detecting Meaningful Effects in Congress
# Note, loads stored simulation results unless otherwise specified.

#rm(list=ls())
#load('fc1-districts.Rdata')
#registerDoMC(cores=detectCores()-1)

# Figure C1(a): Women
#N <- nrow(districts)
#effects <- seq(0.01,0.45,by=0.02)
#iter <- 1:1000
#sim <- data.frame()
#x <- data.frame(iter=numeric(1),effects=numeric(1),est=numeric(1),se=numeric(1))
#sim=foreach(t=1:length(effects),.combine=rbind,.options.RNG=1990) %dorng% {
#  for (j in 1:length(iter)) {
#    Y <- 1:N
#    for (i in Y) {
#      B1 <- effects[t]
#      B2 <- 0.1
#      model <- B1*districts$female[i] - B2*districts$tw.ideo.mean[i] + rnorm(1,0,1)
#      if (model<0) {model=0} else if (model>1) {model=1} else {model=model}
#      Y[i] <- rbinom(1,1,prob=model)
#    }
#    pp <- cbind(districts[,c(2,4,5,7:9)],Y)
#    th <- levels(pp$cong)
#    house <- levels(pp$chamber)
#    mat <-  cem('female',pp,cutpoints=list(white.pop=10,vet.perc=10,tw.ideo.mean=10),
#                drop='Y',grouping=list(cong=th,chamber=house),keep.all=T)
#    est <- att(mat, Y ~ female, data = pp)
#    x[1,1] <- iter[j]
#    x[1,2] <- effects[t]
#    x[1,3] <- est$att.model[1,2]
#    x[1,4] <- est$att.model[2,2]
#    sim=rbind(sim,x)
#  }
# return(sim)
#}
load('sim-1.Rdata')
sim$pos <- 0
sim$pos[sim$est>0] <- 1
sim$sig <- 0
sim$sig[which((sim$est-(1.96*sim$se))>0)] <- 1
sims=summaryBy(pos + sig + est ~ effects, data=sim, FUN=mean, keep.names = T)
fc1a=ggplot(sims) + geom_line(size=1.2,aes(x=effects/0.45,y=pos,color='Positive')) + 
  geom_line(size=1.2,aes(x=effects/0.45,y=sig,color='Significant\n(p<0.05)')) + 
  labs(x="Simulated Effect Size (SD)",y="Proportion Accurate") + scale_y_continuous(limits=c(0, 1)) +
  theme_bw() + theme(legend.title=element_blank())

# Figure C1(b): Racial/Ethnic Minorities
#sim <- data.frame()
#x <- data.frame(iter=numeric(1),effects=numeric(1),est=numeric(1),se=numeric(1))
#sim=foreach(t=1:length(effects),.combine=rbind,.options.RNG=1990) %dorng% {
#  for (j in 1:length(iter)) {
#    Y <- 1:N
#    for (i in Y) {
#      B1 <- effects[t]
#      B2 <- 0.1
#      model <- B1*districts$minority[i] - B2*districts$white.pop[i] + rnorm(1,0,1)
#      if (model<0) {model=0} else if (model>1) {model=1} else {model=model}
#      Y[i] <- rbinom(1,1,prob=model)
#    }
#    pp <- cbind(districts[,c(2,4,6,7:9)],Y)
#    th <- levels(pp$cong)
#    house <- levels(pp$chamber)
#    mat <-  cem('minority',pp,cutpoints=list(white.pop=10,vet.perc=10,tw.ideo.mean=10),
#                drop='Y',grouping=list(cong=th,chamber=house),keep.all=T)
#    est <- att(mat, Y ~ minority, data = pp)
#    x[1,1] <- iter[j]
#    x[1,2] <- effects[t]
#    x[1,3] <- est$att.model[1,2]
#    x[1,4] <- est$att.model[2,2]
#    sim <- rbind(sim,x)
#  }
# return(sim)
#}
load('sim-2.Rdata')
sim$pos <- 0
sim$pos[sim$est>0] <- 1
sim$sig <- 0
sim$sig[which((sim$est-(1.96*sim$se))>0)] <- 1
sims <- summaryBy(pos + sig + est ~ effects, data=sim, FUN=mean, keep.names = T)
fc1b=ggplot(sims) + geom_line(size=1.2,aes(x=effects/0.45,y=pos,color='Positive')) + 
  geom_line(size=1.2,aes(x=effects/0.45,y=sig,color='Significant\n(p<0.05)')) + 
  labs(x="Simulated Effect Size (SD)",y="Proportion Accurate") + scale_y_continuous(limits=c(0, 1)) +
  theme_bw() + theme(legend.title=element_blank())

# Figure C1(c): Veterans
#sim <- data.frame()
#x <- data.frame(iter=numeric(1),effects=numeric(1),est=numeric(1),se=numeric(1))
#sim=foreach(t=1:length(effects),.combine=rbind,.options.RNG=1990) %dorng% {
#  for (j in 1:length(iter)) {
#    Y <- 1:N
#    for (i in Y) {
#      B1 <- effects[t]
#      B2 <- 0.1
#      model <- B1*districts$vet.any[i] - B2*districts$vet.perc[i] + rnorm(1,0,1)
#      if (model<0) {model=0} else if (model>1) {model=1} else {model=model}
#      Y[i] <- rbinom(1,1,prob=model)
#    }
#    pp <- cbind(districts[,c(2,4,10,7:9)],Y)
#    th <- levels(pp$cong)
#    house <- levels(pp$chamber)
#    mat <-  cem('vet.any',pp,cutpoints=list(white.pop=10,vet.perc=10,tw.ideo.mean=10),
#                drop='Y',grouping=list(cong=th,chamber=house),keep.all=T)
#    est <- att(mat, Y ~ vet.any, data = pp)
#    x[1,1] <- iter[j]
#    x[1,2] <- effects[t]
#    x[1,3] <- est$att.model[1,2]
#    x[1,4] <- est$att.model[2,2]
#    sim <- rbind(sim,x)
#  }
# return(sim)
#}
load('sim-3.Rdata')
sim$pos <- 0
sim$pos[sim$est>0] <- 1
sim$sig <- 0
sim$sig[which((sim$est-(1.96*sim$se))>0)] <- 1
sims=summaryBy(pos + sig + est ~ effects, data=sim, FUN=mean, keep.names = T)
fc1c=ggplot(sims) + geom_line(size=1.2,aes(x=effects/0.45,y=pos,color='Positive')) + 
  geom_line(size=1.2,aes(x=effects/0.45,y=sig,color='Significant\n(p<0.05)')) + 
  labs(x="Simulated Effect Size (SD)",y="Proportion Accurate") + scale_y_continuous(limits=c(0, 1)) +
  theme_bw() + theme(legend.title=element_blank())
# ---------------------------

t_race <- t.test(main$race.ni.d~main$nonwhite)
# mean in group White: 0.27, group Nonwhite: 0.36