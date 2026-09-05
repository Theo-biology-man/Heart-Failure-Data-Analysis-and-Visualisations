Heart Failure Data Analysis and Visualisation
================
Theo Richardson
2026-09-01

**Heart Failure Data Analysis and Visualisations**

This project presents and analyses data from 299 heart failure patients
to understand factors contributing to mortality in the style of a
scientific journal article

**The effect of the number of risk factors on death by heart failure**

The effect of compounding risk factors on incidences of death was
assessed by totaling the number of risk factors in each patient and
calculating what percentage of that group died as the groups varied in
sizes.

Figure 1 demonstrates that those in the group with a combination of
three risk factors, out of smoking, hypertension, anemia and diabetes,
saw the highest percentage of deaths (48.78%) within their group with an
increase over both those with two (31.07%) and one (26.27%) risk
factors.

Despite this, the second highest percentage (37.14%) was shown by the
group with zero risk factors, while no patients with four factors were
deceased. This indicates using combination of risk factors as a model
for predicting mortality in heart failure patients is not informative.

``` r
#packages 

library(readxl)
library(dplyr)
library(ggplot2)
library(nortest)
library(gridExtra)

Heart_Failure <- read_xlsx("Data/Heart failure.xlsx", sheet = 2)


#mutate table

Heart_Failure <- Heart_Failure %>%
  mutate(num_factors = rowSums(select(.,
                                      diabetes, 
                                      high_blood_pressure, 
                                      anaemia, 
                                      smoking)))

#patient count for each factor

total_counts <- Heart_Failure %>% 
  group_by(num_factors) %>%
  summarise(total = n())

#death count for each factor
death_counts <- Heart_Failure %>%
  filter(DEATH_EVENT == 1) %>%
  group_by(num_factors) %>%
  summarise(deaths = n())

#joining tables together 
merged_counts <- left_join(total_counts, death_counts, by = "num_factors")

merged_counts <- merged_counts %>%
  mutate(
    deaths = ifelse(is.na(deaths), 0, deaths),
    percentage_of_deaths = (deaths/total) * 100
    )



#figure for proportion of people in group who are deceased

ggplot(merged_counts, aes(x = factor(num_factors), 
                          y = percentage_of_deaths, 
                          fill = factor(num_factors))) +
  geom_bar(stat = "identity", colour = "black", width = 0.5, 
           na.rm = FALSE, show.legend = FALSE) +
  scale_fill_manual(values = c("0" = "#228B22",
                               "1" = "#FFD700",
                               "2" = "lightblue3",
                               "3" = "purple",
                               "4" = "orange")) +
  
  labs(x = "Number of risk factors",
       y = "Percentage of patients who died (%)") +
  theme(axis.line = element_line(size = 0.5))
```

<figure>
<img
src="Figures/Figure 1.png"
alt="Figure 1: The number of categorical risk factors in each patient was totalled and each patient was separated into groups based on their risk factor numbers. This included smoking, anaemia, diabetes and hypertension. The percentage of this group that was deceased during their follow up period was calculated. Those with 3 risk factors (n = 41) showed the highest percentage however those with 1 (n = 118) and 2 (n = 103) risk factors showed a lower percentage than those with 0 (n = 35) risk factors. No patients with more 4 risk factors were deceased in this dataset" />
<figcaption aria-hidden="true">Figure 1: The number of categorical risk
factors in each patient was totalled and each patient was separated into
groups based on their risk factor numbers. This included smoking,
anaemia, diabetes and hypertension. The percentage of this group that
was deceased during their follow up period was calculated. Those with 3
risk factors (<em>n = 41</em>) showed the highest percentage however
those with 1 (<em>n = 118</em>) and 2 (<em>n = 103</em>) risk factors
showed a lower percentage than those with 0 (<em>n = 35</em>) risk
factors. No patients with more 4 risk factors were deceased in this
dataset</figcaption>
</figure>

**The effects of age on mortality in heart failure patients**

Figure 2 demonstrates that patients who had died during the follow-up
period tended to be older, with the median age of the deceased group
being 65 years compared with 60 years in the alive group. Neither group
followed a normal distribution (Anderson-Darling test: p \< 0.05 for
both groups) therefore a non-parametric Mann Whitney U test was used to
compare age between the two groups.

The Mann Whitney U test identified a significant difference in age
between alive and deceased patients (W = 7127, p \< 0.001). This
suggests that age will be one of the main factors contributing to
mortality in this data set.

``` r
#filtering the data for stats test and labeling for graph

alive_patients <- Heart_Failure %>%
  filter( DEATH_EVENT == "0")

dead_patients <- Heart_Failure %>%
  filter( DEATH_EVENT == "1")

Heart_Failure$DEATH_EVENT <- factor(Heart_Failure$DEATH_EVENT, 
                                    levels = c(0, 1),
                                labels = c("Alive", "Dead"))

#boxplot presenting distribution of ages across alive and dead patients

ggplot(data = Heart_Failure, aes(x = DEATH_EVENT, 
                                 y = age, 
                                 fill = DEATH_EVENT)) +
  geom_boxplot(width = 0.3, 
               show.legend = FALSE, 
               outlier.shape = NA ) +
  geom_jitter(width = 0.1, alpha = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c("Alive" = "#228B22", 
                               "Dead" = "#FFD700")) +
    labs(x = "",
       y = "Age of patient") +
  theme(axis.line = element_line(size = 0.5))
```

<figure>
<img
src="Figures/Figure 2.png"
alt="Figure 2: The distibution of age among patients who were either alive (n = 203) or deceased (n = 96) filter their follow up period. The age of patients was significantly higher (Mann Whitney U test: U = 7127, p &lt; 0.001) in the group of patients who were deceased after their follow up period." />
<figcaption aria-hidden="true">Figure 2: The distibution of age among
patients who were either alive (<em>n = 203</em>) or deceased (<em>n =
96</em>) filter their follow up period. The age of patients was
significantly higher (Mann Whitney U test: U = 7127, <em>p</em> &lt;
0.001) in the group of patients who were deceased after their follow up
period.</figcaption>
</figure>

``` r
##comparison of medians

median(alive_patients$age)
```

    ## [1] 60

``` r
median(dead_patients$age)
```

    ## [1] 65

``` r
#test for normality

ad.test(alive_patients$age)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  alive_patients$age
    ## A = 0.96207, p-value = 0.01497

``` r
ad.test(dead_patients$age)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  dead_patients$age
    ## A = 0.76031, p-value = 0.04638

``` r
#wilcoxon rank sum test
wilcox.test(age ~ DEATH_EVENT, data = Heart_Failure)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  age by DEATH_EVENT
    ## W = 7121, p-value = 0.0001668
    ## alternative hypothesis: true location shift is not equal to 0

**Effects blood related risk factors on mortality in heart failure
patients**

Four blood related factors which are used to predict mortality by heart
failure were analysed to understand how they differed between alive and
deceased patients.

Visually Figure 3 (a) and (c) demonstrate little difference in the
distribution of their respective factors however in (b) and (d) we can
observe that patients who were deceased after their follow up period
tended to have a higher serum creatinine level and a lower serum sodium
level. Figure 3 (b), (c) and (d) do display some extreme outliers
demonstrating there is still a very wide range of values for these blood
related factors in heart failure patients.

Most of the groups did not follow a normal distribution
(Anderson-Darling test: *p* \< 0.05 for all groups except for the
platelet number in the deceased groups, *p* = 0.05703) and therefore a
non-parametric Mann-Whitney U test was performed.

A significant difference was found between the medians of the
log<sub>10</sub> of serum creatinine levels (W = 5298.5 *p* \< 0.001)
and the log<sub>10</sub> of serum sodium levels (W = 12262 *p* \< 0.001)
for the deceased and alive groups. No significant difference was found,
however, between the medians of the log<sub>10</sub> of the CPK levels
(W = 9460, *p* = 0.684) and the log<sub>10</sub> of the platelet levels
(W = 10300, *p* = 0.4256) for the deceased and alive groups.

From this we can infer that higher levels of serum creatinine and lower
levels of serum sodium are associated with mortality in this dataset
whereas CPK and platelets levels have little effect.

``` r
#series of four box plots for blood related factors

#creatine phosphokinase boxplot

p1 <- ggplot(data = Heart_Failure, aes(
  x = DEATH_EVENT, 
  y = log10(creatinine_phosphokinase), 
  fill = DEATH_EVENT)) +
  geom_boxplot(width = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Alive" = "#228B22", 
    "Dead" = "#FFD700")) +
  theme() +
  labs(
    title = "(a)",
    x = "",
    y = "Log10(CPK levels) (g/L)"
  ) +
    theme(axis.line = element_line(size = 0.3), axis.title.y = element_text(size = 8))

#serum creatinine boxplot

p2 <- ggplot(data = Heart_Failure, aes(
  x = DEATH_EVENT, 
  y = log10(serum_creatinine), 
  fill = DEATH_EVENT)) +
  geom_boxplot(width = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c("Alive" = "#228B22", "Dead" =   "#FFD700")) +
  labs(
    title = "(b)",
    x = "",
    y = "Log10(serum creatinine levels) (mg/dL)"
  ) +
  theme(axis.line = element_line(size = 0.3), 
        axis.title.y = element_text(size = 8))

#platelet boxplot

p3 <- ggplot(data = Heart_Failure, aes(
  x = DEATH_EVENT, 
  y = log10(platelets), 
  fill =  DEATH_EVENT)) +
  geom_boxplot(width = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Alive" = "#228B22", 
    "Dead" = "#FFD700")) +
  labs(
    title = "(c)",
    x = "",
    y = "Log10(platelet levels) (kiloplatelets/mL)"
  ) +
  theme(axis.line = element_line(size = 0.3), 
        axis.title.y = element_text(size = 8))

#serum sodium boxplot

p4 <- ggplot(data = Heart_Failure, aes(
  x = DEATH_EVENT, 
  y = log10(serum_sodium), 
  fill = DEATH_EVENT)) +
  geom_boxplot(width = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Alive" = "#228B22",
    "Dead" = "#FFD700")) +
  labs(
    title = "(d)",
    x = "",
    y = "Log10(serum sodium levels) (mEq/L)"
  ) +
  theme(axis.line = element_line(size = 0.3), 
        axis.title.y = element_text(size = 8))

#arranging graphs into grid of four for figure 3

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
```

<figure>
<img
src="Figures/Figure 3.png"
alt="Figure 3: (a): The distribution of the Log10 of CPK levels in alive and deceased heart failure patients. There was no significant difference between the medians of both groups (Mann Whitney U test: U = 9380, p = 0.684). (b): The distribution of the Log10 of serum creatinine levels in alive and deceased heart failure patients.The median serum creatinine levels in deceased patients was signifcantly higher than the median in alive patients (Mann-Whitney U test: U = 5280.5 p &gt; 0.001). (c): The distribution of the Log10 of platelet levels in alive and deceased heart failure patients. There was no significant difference between the medians of both groups (Mann Whitney U test: U = 10156, p = 0.4256. (d): The distribution of the Log10 of serum sodium levels in alive and deceased heart failure patients. The median serum sodium levels in deceased patients was signifcantly lower than the median in alive patients (Mann Whitney U test: U = p &gt; 0.001)" />
<figcaption aria-hidden="true">Figure 3: (a): The distribution of the
Log10 of CPK levels in alive and deceased heart failure patients. There
was no significant difference between the medians of both groups (Mann
Whitney U test: U = 9380, <em>p</em> = 0.684). (b): The distribution of
the Log10 of serum creatinine levels in alive and deceased heart failure
patients.The median serum creatinine levels in deceased patients was
signifcantly higher than the median in alive patients (Mann-Whitney U
test: U = 5280.5 <em>p</em> &gt; 0.001). (c): The distribution of the
Log10 of platelet levels in alive and deceased heart failure patients.
There was no significant difference between the medians of both groups
(Mann Whitney U test: U = 10156, <em>p</em> = 0.4256. (d): The
distribution of the Log10 of serum sodium levels in alive and deceased
heart failure patients. The median serum sodium levels in deceased
patients was signifcantly lower than the median in alive patients (Mann
Whitney U test: U = <em>p</em> &gt; 0.001)</figcaption>
</figure>

``` r
#normality tests for serum creatine, serum sodium, platelet count and creatine phosphokinase levels

ad.test(alive_patients$serum_creatinine)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  alive_patients$serum_creatinine
    ## A = 21.974, p-value < 2.2e-16

``` r
ad.test(dead_patients$serum_creatinine)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  dead_patients$serum_creatinine
    ## A = 10.81, p-value < 2.2e-16

``` r
ad.test(alive_patients$serum_sodium)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  alive_patients$serum_sodium
    ## A = 2.3668, p-value = 5.215e-06

``` r
ad.test(dead_patients$serum_sodium)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  dead_patients$serum_sodium
    ## A = 0.94699, p-value = 0.01593

``` r
ad.test(alive_patients$platelets)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  alive_patients$platelets
    ## A = 4.969, p-value = 2.492e-12

``` r
ad.test(dead_patients$platelets)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  dead_patients$platelets
    ## A = 0.72419, p-value = 0.05703

``` r
ad.test(alive_patients$creatinine_phosphokinase)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  alive_patients$creatinine_phosphokinase
    ## A = 22.495, p-value < 2.2e-16

``` r
ad.test(dead_patients$creatinine_phosphokinase)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  dead_patients$creatinine_phosphokinase
    ## A = 18.168, p-value < 2.2e-16

``` r
#Wilcoxon rank sum tests



wilcox.test(platelets ~ DEATH_EVENT, data = Heart_Failure)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  platelets by DEATH_EVENT
    ## W = 10300, p-value = 0.4256
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(creatinine_phosphokinase ~ DEATH_EVENT, data = Heart_Failure)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  creatinine_phosphokinase by DEATH_EVENT
    ## W = 9460, p-value = 0.684
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(serum_creatinine ~ DEATH_EVENT, data = Heart_Failure)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  serum_creatinine by DEATH_EVENT
    ## W = 5298, p-value = 1.581e-10
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(serum_sodium ~ DEATH_EVENT, data = Heart_Failure)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  serum_sodium by DEATH_EVENT
    ## W = 12262, p-value = 0.0002928
    ## alternative hypothesis: true location shift is not equal to 0

**The effects of smoking on a patients ejection fraction**

The effect of smoking on the patients ejection fraction was investigated
as a persons ejection fraction can be used to predict a patients risk of
death if it falls outside a healthy range.

Figure 4 shows that a similar distribution of ejection fraction was
observed between smokers and non-smokers with most values being
distributed between approximately within the 25% and 50% range.

Mann Whitney U analysis was used as the ejection fraction data does not
follow a normal distribution (Anderson-Darling test: *p* \> 0.001 for
ejection fraction in both smoker and non-smoker group).

This found no significant difference was found between medians
(Mann-Whitney U test: U = 10602, *p* = 0.2158) suggesting smoking was
not significantly associated with ejection fraction in this dataset.

``` r
#more labeling for graph and subsequent data filtering

Heart_Failure$smoking <- factor(Heart_Failure$smoking, levels = c(0, 1),
                                labels = c("Non-smoker", "Smoker"))

#smoking violin plot

ggplot(data = Heart_Failure, aes(
  x = smoking, 
  y = ejection_fraction, 
  fill = smoking , 
  colour = smoking)) +
  geom_violin(trim = FALSE, show.legend = FALSE, alpha = 0.2) +
  labs( x = "", 
        y = "Ejection Fraction (%)") + 
  scale_colour_manual(values = c(
    "Non-smoker" = "black", 
    "Smoker" = "black")) +
  scale_fill_manual(values = c(
    "Non-smoker" = "#228B22", 
    "Smoker" = "#FFD700")) +
  geom_boxplot(width = 0.1, 
               alpha = 0.5, 
               show.legend = FALSE) +
  theme(axis.line = element_line(size = 0.3))
```

<figure>
<img
src="Figures/Figure 4.png"
alt="Figure 4: The difference in ejection fraction among heart failure patients who smoke (n = 96) and those who do not smoke (n= 203). There was no significant difference between the median ejection fraction of smokers and non-smokers (Mann-Whitney U test: U = 10602, p = 0.2158)." />
<figcaption aria-hidden="true">Figure 4: The difference in ejection
fraction among heart failure patients who smoke (<em>n = 96</em>) and
those who do not smoke (<em>n= 203</em>). There was no significant
difference between the median ejection fraction of smokers and
non-smokers (Mann-Whitney U test: U = 10602, <em>p</em> =
0.2158).</figcaption>
</figure>

``` r
#filtering data to obtain the median 

nonsmokers <- Heart_Failure %>%
  filter( smoking == "Non-smoker")

smokers <- Heart_Failure %>%
  filter( smoking == "Smoker")

#comparison of the medians of the two groups 

median(nonsmokers$ejection_fraction)
```

    ## [1] 38

``` r
median(smokers$ejection_fraction)
```

    ## [1] 35

``` r
#test for normality 

ad.test(nonsmokers$ejection_fraction)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  nonsmokers$ejection_fraction
    ## A = 3.8079, p-value = 1.597e-09

``` r
ad.test(smokers$ejection_fraction)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  smokers$ejection_fraction
    ## A = 2.0907, p-value = 2.356e-05

``` r
#Wilcoxon rank sum test 

wilcox.test(ejection_fraction ~ smoking, data = Heart_Failure)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  ejection_fraction by smoking
    ## W = 10602, p-value = 0.2158
    ## alternative hypothesis: true location shift is not equal to 0

It can be proposed that patients could have several conditions
contributing to poor heart health such as diabetes and anaemia, but the
patients age will be one of the main factors contributing to mortality.
Additionally, a person having serum sodium levels lower and serum
creatinine levels higher than the healthy range may increase incidents
of death in heart failure patients. Furthermore, limited evidence has
been found to suggest smoking has any effect on the volume of blood
leaving the heart after every contraction.
