library(latex2exp)
library(ggplot2)
library(patchwork)

setwd("/")
load("/TableData.Rdata") 

urbanData = AggrDat
urbanData = subset(AggrDat, select = -c(uniq, Dupl))



## Analyze data.

# Population-area relationship
areaData <- read.csv("/Supplementary Information 1.csv")
summary(lm(areaData$log.A ~ areaData$log.N))


# Scaling of urban hierarchy and village sizes with respect to largest settlement
urbanData$Rural = log10(10^urbanData$Pop - 10^urbanData$Cap)
urbanData[which(urbanData$Rural=='-Inf'),]$Rural = NA
summary(lm(Rural-log10(SL-1)~Cap, data=urbanData[which(urbanData$SL>1),]))



# Scaling of socioeconomic output Y with N
urbanData$Energy = log10(urbanData$Agri * 15 * 100) 
urbanData[which(urbanData$Energy=='-Inf'),]$Energy = NA
urbanData[which(urbanData$Energy!='NA'),]$Energy = urbanData[which(urbanData$Energy!='NA'),]$Energy + urbanData[which(urbanData$Energy!='NA'),]$Terr
summary(lm(Energy~Cap, data=urbanData))


# Scaling of c_1NI with N, where c_1 = 1.
summary(lm(log10(Agri)+Cap~Cap, data=urbanData[which(urbanData$Agri > 0),]))


# Scaling of infrastructural length with energy consumption in contemporary societies
large_scale_infra1 <- read.csv("/Roadways-Energy.csv")
summary(lm(Energy~Infra, data=large_scale_infra1))
large_scale_infra2 <- read.csv("/Railways-Energy.csv")
summary(lm(log_total_E~log_rail_lines_km, data=large_scale_infra2))


# Scaling of urbanization rate
summary(lm(Cap~Pop, data=urbanData[which((urbanData$Pop > urbanData$Cap + .3)),]))


# Scaling of A_P and I with N_P
summary(lm(Terr~Pop, data=urbanData))
summary(lm(log10(Agri)~Pop, data=urbanData[which(scalingData$Agri>0),]))




# Figure 1

fit1 <- lm(log.A ~ log.N, data=areaData)
slope1 <- round(coef(fit1)[2], 2)
intercept1 <- round(coef(fit1)[1], 2)
r21 <- round(summary(fit1)$adj.r.squared, 2)

fig1 = ggplot(data=areaData, aes(x = log.N, y = log.A)) +  
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5, y = 4, hjust = 0.5,
           label = paste0("italic(log~A) == ", intercept1, " + ", slope1, 
                          " * italic(log~N) ~~ ~~ italic(R^2) == ", r21),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  xlab(TeX("log Population size of the largest settlement, $N$")) +
  ylab(TeX("log $A$ (ha)")) +
  theme_minimal()



# Figure 2

fit2 <- lm(Energy ~ Cap, data = urbanData)
slope2 <- round(coef(fit2)[2], 2)
intercept2 <- round(coef(fit2)[1], 2)
r22 <- round(summary(fit2)$adj.r.squared, 2)

fig2 = ggplot(data=urbanData, aes(x = Cap, y = Energy)) +  
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 4, y = 6.5, hjust = 0.5,
           label = paste0("italic(log~Y) == ", intercept2, " + ", slope2, 
                          " * italic(log~N) ~~ ~~ italic(R^2) == ", r22),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  xlab(TeX("log Population size of the largest settlement, $N$")) +
  ylab(TeX("log $Y$ (GJ/y)")) +
  theme_minimal()


# Figure 3

fit3 <- lm(Cap ~ Pop, data = urbanData[which((urbanData$Pop > urbanData$Cap + .3)),])
slope3 <- round(coef(fit3)[2], 2)
intercept3 <- round(coef(fit3)[1], 2)
r23 <- round(summary(fit3)$adj.r.squared, 2)

fig3 = ggplot(data=urbanData[which((urbanData$Pop > urbanData$Cap + .3)),], aes(x = Cap, y = Energy)) +  
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 4, y = 6.5, hjust = 0.5,
           label = paste0("italic(log~N) == ", intercept3, " + ", slope3, 
                          " * italic(log~Np) ~~ ~~ italic(R^2) == ", r23),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  ylab(TeX("log $N$")) +
  xlab(TeX("log Polity population size, $N_P$")) +
  theme_minimal()


fig1 / fig2 / fig3 + plot_annotation(tag_levels = 'A')
