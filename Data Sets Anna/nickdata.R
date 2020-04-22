if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()




nickfemale <- read_csv("Data Sets Anna/nickfemale.csv")

t.test(nickfemale$HF_diff, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

t.test(nickfemale$HJ_diff, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)
