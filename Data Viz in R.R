#Data Visualization in R using GGPLOT2

#Install the MosaicData package from CRAN
install.packages("mosaicData")
mosaicData::
#Calling the CPS85 dataset from the mosaicdata package
data(CPS85, package="mosaicData")
View(CPS85)
dim(CPS85)



#Using ggplot2 for data visualization

library(ggplot2) 
require(ggplot2)
ggplot(data = CPS85,
       mapping = aes(x = exper, y = wage))

#geom_point allows you to plot your data
#U can deal with outlier by removing it or changing the value by using the avr, mean or mode.

ggplot(data = CPS85,
       mapping = aes(x = exper, y = wage)) +
  geom_point()
#U can deal with outlier by removing it or changing the value by using the avr, mean or mode. since there is a point outside 40, u can filter to ,40
library(dplyr)
plotdata <- filter(CPS85, wage < 40)
dim(plotdata)
ggplot(data = plotdata,
       mapping = aes(x = exper, y = wage)) +
  geom_point()

ggplot(data = plotdata,
       mapping = aes(x = exper, y = wage)) +
  geom_point(color = "blue",
             alpha = .6,
             size = 4,
             pch=20)
#geom smooth is used to add a trendline to carry out regression analysis
ggplot(data = plotdata,
       mapping = aes(x = exper, y = wage)) +
  geom_point(color = "cornflowerblue",
             alpha = .7,
             size = 3, pch = 8) +
  geom_smooth(method = "lm", lty=1, color="red")
#To determine the number of males and females or dividing a dataset
table(CPS85$sex)
summary(CPS85$sex)
#grouping
ggplot(data = plotdata,
       mapping = aes(x = exper, 
                     y = wage,
                     color = sex)) +
  geom_point(alpha = .7,
             size = 3) +
  geom_smooth(method = "lm", 
              se = TRUE, 
              size = 1.5)
#example 2
ggplot(data = plotdata, mapping = aes(x = exper,  y = wage,
                     color = married)) +
  geom_point(alpha = .7,
             size = 3) +
  geom_smooth(method = "lm", 
              se = TRUE, 
              size = 1.5, lty=1)

#Scaling
library(scales)
ggplot(data = plotdata,
       mapping = aes(x = exper, 
                     y = wage,
                     color = sex)) +
  geom_point(alpha = .7,
             size = 3) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1.5) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", 
                                "cornflowerblue"), alpha("indianred3", alpha=.4), alpha("cornflowerblue", alpha=.2))
#to apply to every sector/ break down the table into various graph
ggplot(data = plotdata,
       mapping = aes(x = exper, 
                     y = wage,
                     color = sex)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", 
              se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", 
                                "cornflowerblue")) +
  facet_wrap(~sector)


#Labels

ggplot(data = plotdata,
       mapping = aes(x = exper, 
                     y = wage,
                     color = sex)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", 
              se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", 
                                "cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender")


#Themes

ggplot(data = plotdata,
       mapping = aes(x = exper, 
                     y = wage,
                     color = sex)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", 
              se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", 
                                "cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender")+
  theme_dark()

#To get a curve chart, polynomial is introduced
myplot <- ggplot(plotdata,
       aes(x = exper, 
           y = wage,
           color = sex)) +
  geom_point(alpha = .7,
             size = 3) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x,2),
              se = FALSE, size = 1.5)+ scale_color_manual(values = c("indianred3", "cornflowerblue"))

myplot


#Univariate Graphs
data(Marriage, package = "mosaicData")

#Categorical Analysis
#Barchart

View(Marriage)

#To know more about the data(Marriage)
?Marriage
library(ggplot2)
library(dplyr)
ggplot(Marriage, aes(x = race)) + 
  geom_bar()

table(Marriage$race)


ggplot(Marriage, aes(x = race)) + 
  geom_bar(fill = "blue",
           color = "Red") +
  labs(x = "Race", 
       y = "Frequency", 
       title = "Participants by race")

#To divide the bar chart into race of different colors
ggplot(Marriage, aes(x = race, 
                     fill = race)) + 
  geom_bar() +
  labs(x = "Race", 
       y = "Frequency", 
       title = "Participants by race")

#To choose a color you prefer for your chat
ggplot(Marriage, aes(x = race, 
                     fill = race)) + 
  geom_bar(fill=c("blue","red","grey","yellow")) +
  labs(x = "Race", 
       y = "Frequency", 
       title = "Participants by race")


# plot the distribution as percentages
ggplot(Marriage, 
       aes(x = race, 
           y = ..count.. / sum(..count..))) + 
  geom_bar() +
  labs(x = "Race", 
       y = "Percent", 
       title  = "Participants by race") +
  scale_y_continuous(labels = scales::percent)

#Sorting Categories
plotdata1 <- Marriage %>%
  count(race)
table(Marriage$race)

View(plotdata1)
View(Marriage)
#Plotting sorted data
ggplot(plotdata1, 
       aes(x = reorder(race, n), 
           y = n)) + 
  geom_bar(stat = "identity") +
  labs(x = "Race", 
       y = "Frequency", 
       title  = "Participants by race")

#Labeling bins

ggplot(plotdata1, 
       aes(x = reorder(race, n), 
           y = n)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), 
            vjust=-.5) +
  labs(x = "Race", 
       y = "Frequency", 
       title  = "Participants by race")

library(dplyr)
library(scales)
plotdata2 <- Marriage %>%
  count(race) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))
View(plotdata2)
# plot the bars as percentages, 
# in decending order with bar labels
ggplot(plotdata2, 
       aes(x = reorder(race, -pct),
           y = pct)) + 
  geom_bar(stat = "identity", fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Race", 
       y = "Percent", 
       title  = "Participants by race")

#Overlapping label

ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() +
  labs(x = "Officiate",
       y = "Frequency",
       title = "Marriages by officiate")

#1 Flipping the coordination of the chart

# horizontal bar chart
ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate") +
  coord_flip()

#2 bar chart with rotated labels
ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))

#3 bar chart with staggered labels
pms <- paste0(c("", "\n"), 
               levels(Marriage$officialTitle))
pms
ggplot(Marriage, 
       aes(x=factor(officialTitle, 
                    labels = pms))) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate")

#Pie Chart
# create a basic ggplot2 pie chart
plotdata3 <- Marriage %>%
  count(race) %>%
  arrange(desc(race)) %>% 
  mutate(prop = round(n*100/sum(n), 1), 
         lab.ypos = cumsum(prop) - 0.5 * prop)

  
View(plotdata3)

ggplot(plotdata3, 
       aes(x = "", 
           y = prop, 
           fill = race)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void()

# create a pie chart with slice labels
plotdata4 <- Marriage %>%
  count(race) %>%
  arrange(desc(race)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)


plotdata4$label <- paste0(plotdata4$race, "\n",
                         round(plotdata4$prop), "%")
View(plotdata4)
ggplot(plotdata4, 
       aes(x = "", 
           y = prop, 
           fill = race)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Participants by race")


#Treemap
#An alternative to a pie chart is a tree map. 
# Unlike pie charts, it can handle categorical 
# variables that have many levels.

library(treemapify)
plotdata5 <- Marriage %>%
  count(officialTitle)

ggplot(plotdata5, 
       aes(fill = officialTitle, 
           area = n)) +
  geom_treemap() + 
  labs(title = "Marriages by officiate")

# create a treemap with tile labels
ggplot(plotdata5, 
       aes(fill = officialTitle, 
           area = n, 
           label = officialTitle)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Marriages by officiate") +
  theme(legend.position = "none")

##Quantitative

#Histogram

# plot the age distribution using a histogram
ggplot(Marriage, aes(x = age)) +
  geom_histogram() + 
  labs(title = "Participants by age",
       x = "Age")

# plot the histogram with blue bars and white borders
ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white") + 
  labs(title="Participants by age",
       x = "Age")

# plot the histogram with 20 bins
ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 20) + 
  labs(title="Participants by age", 
       subtitle = "number of bins = 20",
       x = "Age")

# plot the histogram with a binwidth of 5
ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 5) + 
  labs(title="Participants by age", 
       subtitle = "binwidth = 5 years",
       x = "Age")

# plot the histogram with percentages on the y-axis
library(scales)
ggplot(Marriage, 
       aes(x = age, 
           y= ..count.. / sum(..count..))) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 5) + 
  labs(title="Participants by age", 
       y = "Percent",
       x = "Age") +
  scale_y_continuous(labels = scales::percent)

#Kernel Density plot

ggplot(Marriage, aes(x = age)) +
  geom_density() + 
  labs(title = "Participants by age")

# Create a kernel density plot of age
ggplot(Marriage, aes(x = age)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Participants by age")
# default bandwidth for the age variable
bw.nrd0(Marriage$age)

ggplot(Marriage, aes(x = age)) +
  geom_density(fill = "deepskyblue", 
               bw = 5.1819 ) + 
  labs(title = "Participants by age",
       subtitle = "bandwidth = 1")

#Dot Chart

# plot the age distribution using a dotplot
ggplot(Marriage, aes(x = age)) +
  geom_dotplot() + 
  labs(title = "Participants by age",
       y = "Proportion",
       x = "Age")


# Plot ages as a dot plot using 
# gold dots with black borders
ggplot(Marriage, aes(x = age)) +
  geom_dotplot(fill = "gold", 
               color = "black") + 
  labs(title = "Participants by age",
       y = "Proportion",
       x = "Age")

###Bivariate Graphs
#Bivariate graphs display the relationship between two variables. 
#The type of graph will depend on the measurement level of 
#the variables (categorical or quantitative).

#Categorical

#Stacked bar chart

#Let's plot the relationship between automobile class and drive type 
#(front-wheel, rear-wheel, or 4-wheel drive) for the automobiles in the 
#Fuel economy dataset.

data(mpg, package="ggplot2")
ggplot2::mpg
# stacked bar chart
?mpg
View(mpg)
table(mpg$drv)

table(mpg$class)

ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = "stack")

# grouped bar plot
ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = "dodge")

# grouped bar plot preserving zero count bars
ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = position_dodge(preserve = "single"))

# bar plot, with each bar representing 100%
ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")

# bar plot, with each bar representing 100%, 
# reordered bars, and better labels and colors
library(scales)
ggplot(mpg, 
       aes(x = factor(class,
                      levels = c("2seater", "subcompact", 
                                 "compact", "midsize", 
                                 "minivan", "suv", "pickup")),
           fill = factor(drv, 
                         levels = c("f", "r", "4"),
                         labels = c("front-wheel", 
                                    "rear-wheel", 
                                    "4-wheel")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Percent", 
       fill = "Drive Train",
       x = "Class",
       title = "Automobile Drive by Class") +
  theme_minimal()

# change the order the levels for the categorical variable "class"
mpg$class = factor(mpg$class,
                   levels = c("2seater", "subcompact", 
                              "compact", "midsize", 
                              "minivan", "suv", "pickup"))

library(dplyr)
plotdata5 <- mpg %>%
  group_by(class, drv) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
View(plotdata5)

# create segmented bar chart
# adding labels to each segment

ggplot(plotdata5, 
       aes(x = factor(class,
                      levels = c("2seater", "subcompact", 
                                 "compact", "midsize", 
                                 "minivan", "suv", "pickup")),
           y = pct,
           fill = factor(drv, 
                         levels = c("f", "r", "4"),
                         labels = c("front-wheel", 
                                    "rear-wheel", 
                                    "4-wheel")))) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  geom_text(aes(label = lbl), 
            size = 3, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", 
       fill = "Drive Train",
       x = "Class",
       title = "Automobile Drive by Class") +
  theme_minimal()

###Quantitative vs. Quantitative
#The relationship between two quantitative variables is 
#typically displayed using scatterplots and line graphs.

###Scatterplot
#The simplest display of two quantitative variables is a scatterplot, 
#with each variable represented on an axis. For example, using the Salaries dataset, 
#we can plot experience (yrs.since.phd) vs. academic salary (salary) for college professors.

data(Salaries, package="carData")
View(Salaries)

# simple scatterplot
ggplot(Salaries, 
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point()

# enhanced scatter plot
ggplot(Salaries, 
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(50000, 250000)) +
  scale_x_continuous(breaks = seq(0, 60, 10), 
                     limits=c(0, 60)) + 
  labs(x = "Years Since PhD",
       y = "",
       title = "Experience vs. Salary",
       subtitle = "9-month salary for 2008-2009")
library(mice)
#Adding best line of fit

# scatterplot with linear fit line
ggplot(Salaries,
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point(color= "red") +
  geom_smooth(method = "lm", color = "green")

# scatterplot with quadratic line of best fit
ggplot(Salaries, 
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")
# scatterplot with loess smoothed line
ggplot(Salaries, 
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point(color= "steelblue") +
  geom_smooth(color = "tomato")

# scatterplot with loess smoothed line 
# and better labeling and color
ggplot(Salaries, 
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(size = 1.5,
              color = "darkgrey") +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(50000, 250000)) +
  scale_x_continuous(breaks = seq(0, 60, 10), 
                     limits = c(0, 60)) + 
  labs(x = "Years Since PhD",
       y = "",
       title = "Experience vs. Salary",
       subtitle = "9-month salary for 2008-2009") +
  theme_minimal()


### Line plot
#When one of the two variables represents time, 
#a line plot can be an effective method of displaying 
#relationship. For example, the code below displays the 
#relationship between time (year) and life expectancy 
# (lifeExp) in the United States between 1952 and 2007. 
# The data comes from the gapminder dataset.

install.packages("gapminder")
data(gapminder, package="gapminder")
View(gapminder)
# Select US cases
library(dplyr)
plotdata6 <- filter(gapminder, 
                   country == "United States")
View(plotdata6)
# simple line plot
ggplot(plotdata6, 
       aes(x = year, 
           y = lifeExp)) +
  geom_line() 


# line plot with points
# and improved labeling
ggplot(plotdata6, 
       aes(x = year, 
           y = lifeExp)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3,
             color = "steelblue") +
  labs(y = "Life Expectancy (years)", 
       x = "Year",
       title = "Life expectancy changes over time",
       subtitle = "United States (1952-2007)",
       caption = "Source: http://www.gapminder.org/data/")


## Bar chart (on summary statistics)
#You can also use bar charts to display other summary 
# statistics (e.g., means or medians) on a quantitative 
# variable for each level of a categorical variable.

data(Salaries, package="carData") 

# calculate mean salary for each rank
library(dplyr)
plotdata7 <- Salaries %>%
  group_by(rank) %>%
  summarize(mean_salary = mean(salary))

# plot mean salaries
ggplot(plotdata7, 
       aes(x = rank, 
           y = mean_salary)) +
  geom_bar(stat = "identity")


# plot mean salaries in a more attractive fashion
library(scales)
ggplot(plotdata7, 
       aes(x = factor(rank,
                      labels = c("Assistant\nProfessor",
                                 "Associate\nProfessor",
                                 "Full\nProfessor")), 
           y = mean_salary)) +
  geom_bar(stat = "identity", 
           fill = "cornflowerblue") +
  geom_text(aes(label = dollar(mean_salary)), 
            vjust = -0.25) +
  scale_y_continuous(breaks = seq(0, 130000, 20000), 
                     label = dollar) +
  labs(title = "Mean Salary by Rank", 
       subtitle = "9-month academic salary for 2008-2009",
       x = "",
       y = "")


# One limitation of such plots is that they do not display 
# the distribution of the data - only the summary statistic 
# for each group. The plots below correct this limitation to 
# some extent.

###Grouped kernel density plots
# One can compare groups on a numeric 
# variable by superimposing kernel density plots in a 
# single graph.

# plot the distribution of salaries 
# by rank using kernel density plots
ggplot(Salaries, 
       aes(x = salary, 
           fill = rank)) +
  geom_density(alpha = 0.4) +
  labs(title = "Salary distribution by rank")


###Box plots
# A boxplot displays the 25th percentile, median, and 
# 75th percentile of a distribution. The whiskers (vertical 
# lines) capture roughly 99% of a normal distribution, and 
# observations outside this range are plotted as points 
# representing outliers.
# Side-by-side box plots are very useful for comparing groups 
# (i.e., the levels of a categorical variable) on a numerical
# variable.

# plot the distribution of salaries by rank using boxplots
ggplot(Salaries, 
       aes(x = rank, 
           y = salary)) +
  geom_boxplot() +
  labs(title = "Salary distribution by rank")

# plot the distribution of salaries by rank using boxplots
ggplot(Salaries, aes(x = rank, 
                     y = salary)) +
  geom_boxplot(notch = TRUE, 
               fill = "cornflowerblue", 
               alpha = .7) +
  labs(title = "Salary distribution by rank")


###Violin plots
#Violin plots are similar to kernel density plots, but are 
#mirrored and rotated 90 degrees.

# plot the distribution of salaries 
# by rank using violin plots
ggplot(Salaries, 
       aes(x = rank,
           y = salary)) +
  geom_violin() +
  labs(title = "Salary distribution by rank")


# plot the distribution using violin and boxplots
ggplot(Salaries, 
       aes(x = rank, 
           y = salary)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) + 
  labs(title = "Salary distribution by rank")


###Ridgeline plots
# A ridgeline plot (also called a joyplot) displays the 
# distribution of a quantitative variable for several groups.
# They're similar to kernel density plots with vertical 
# faceting, but take up less room. Ridgeline plots are 
# created with the ggridges package.


library(ggplot2)
library(ggridges)
View(mpg)
ggplot(mpg, 
       aes(x = cty, 
           y = class, 
           fill = class)) +
  geom_density_ridges() + 
  geom_density_ridges(alpha = 0.4) +
  theme_ridges() +
  labs("Highway mileage by auto class") +
  theme(legend.position = "none")


###Mean/SEM plots
# A popular method for comparing groups on a numeric 
# variable is the mean plot with error bars. 
# Error bars can represent standard deviations, 
# standard error of the mean, or confidence intervals.


# calculate means, standard deviations,
# standard errors, and 95% confidence 
# intervals by rank
library(dplyr)
plotdata8 <- Salaries %>%
  group_by(rank) %>%
  summarize(n = n(),
            mean = mean(salary),
            sd = sd(salary),
            se = sd / sqrt(n),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))

ggplot(plotdata8, 
       aes(x = rank, 
           y = mean, 
           group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1)

# Although we plotted error bars representing the standard 
# error, we could have plotted standard deviations or 95% 
# confidence intervals. Simply replace se with sd or error 
# in the aes option.
# 
# We can use the same technique to compare salary across 
# rank and sex. (Technically, this is not bivariate since 
#                we're plotting rank, sex, and salary, but 
#                it seems to fit here)

# calculate means and standard errors by rank and sex
plotdata9 <- Salaries %>%
  group_by(rank, sex) %>%
  summarize(n = n(),
            mean = mean(salary),
            sd = sd(salary),
            se = sd/sqrt(n))

# plot the means and standard errors by sex
ggplot(plotdata9, aes(x = rank,
                     y = mean, 
                     group=sex, 
                     color=sex)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin  =mean - se, 
                    ymax = mean+se), 
                width = .1)

# Unfortunately, the error bars overlap. We can dodge 
# the horizontal positions a bit to overcome this.

# plot the means and standard errors by sex (dodged)
pd <- position_dodge(0.2)
ggplot(plotdata9, 
       aes(x = rank, 
           y = mean, 
           group=sex, 
           color=sex)) +
  geom_point(position = pd, 
             size = 3) +
  geom_line(position = pd,
            size = 1) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1, 
                position= pd)

# Finally, lets add some options to make the graph more 
# attractive.

# improved means/standard error plot
pd <- position_dodge(0.2)
ggplot(plotdata9, 
       aes(x = factor(rank, 
                      labels = c("Assistant\nProfessor",
                                 "Associate\nProfessor",
                                 "Full\nProfessor")), 
           y = mean, 
           group=sex, 
           color=sex)) +
  geom_point(position=pd, 
             size = 3) +
  geom_line(position = pd, 
            size = 1) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1, 
                position = pd, 
                size = 1) +
  scale_y_continuous(label = scales::dollar) +
  scale_color_brewer(palette="Set1") +
  theme_minimal() +
  labs(title = "Mean salary by rank and sex",
       subtitle = "(mean +/- standard error)",
       x = "", 
       y = "",
       color = "Gender")
###Strip plots
# The relationship between a grouping variable and a numeric 
# variable can be displayed with a scatter plot. For example

# plot the distribution of salaries 
# by rank using strip plots
ggplot(Salaries, 
       aes(y = rank, 
           x = salary)) +
  geom_point() + 
  labs(title = "Salary distribution by rank")


# plot the distribution of salaries
# by rank using jittering
ggplot(Salaries, 
       aes(y = rank, 
           x = salary)) +
  geom_jitter() + 
  labs(title = "Salary distribution by rank")

#It is easier to compare groups if we use color.

# plot the distribution of salaries 
# by rank using jittering
library(scales)
ggplot(Salaries, 
       aes(y = factor(rank,
                      labels = c("Assistant\nProfessor",
                                 "Associate\nProfessor",
                                 "Full\nProfessor")), 
           x = salary, 
           color = rank)) +
  geom_jitter(alpha = 0.7,
              size = 1.5) + 
  scale_x_continuous(label = dollar) +
  labs(title = "Academic Salary by Rank", 
       subtitle = "9-month salary for 2008-2009",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none")

# The option legend.position = "none" is used to suppress 
# the legend (which is not needed here). Jittered plots works
# well when the number of points in not overly large.

