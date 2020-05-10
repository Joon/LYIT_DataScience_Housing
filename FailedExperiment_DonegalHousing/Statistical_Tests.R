# Load Dataset
dl_accomodation <- read.csv("Data/Analyze_data.csv")

# Investigate required sample sizes
#install.packages("pwr")
library(pwr)
p.out <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
                    sig.level = 0.05,
                    power = 0.80)
plot(p.out)

t.out <-pwr.t.test(n = 10, d = 0.2, type = "paired", sig.level = 0.05,)

plot(t.out)

t.anova <-pwr.anova.test(k = 2, n = 10, f = 0.2, sig.level = 0.05)

plot(t.anova)
