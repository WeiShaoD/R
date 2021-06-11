Hello <-c(1,2,3)
mt <- mtcars
plot(mt)

library(readr)
library(ggplot2)
library(dplyr)
setwd("/Users/davidwei/R/dataset/datasets")
confirmed_cases_worldwide <- read_csv("confirmed_cases_worldwide.csv")
confirmed_cases_worldwide

library(testthat)
install.packages('IRkernel.testthat') 
library(IRkernel.testthat)

soln_confirmed_cases_worldwide <- read_csv("confirmed_cases_worldwide.csv")

run_tests({
    test_that("readr is loaded", {
        expect_true(
            "readr" %in% .packages(), 
            info = "Did you load the `readr` package?"
        )
    })
    test_that("ggplot2 is loaded", {
        expect_true(
            "ggplot2" %in% .packages(), 
            info = "Did you load the `ggplot2` package?"
        )
    })
    test_that("dplyr is loaded", {
        expect_true(
            "dplyr" %in% .packages(), 
            info = "Did you load the `dplyr` package?"
        )
    })
    
    test_that("confirmed_cases_worldwide is a data.frame", {
        expect_s3_class(
            confirmed_cases_worldwide,
            "data.frame",
        )
    })
    test_that("confirmed_cases_worldwide has the correct column", {
        expect_identical(
            colnames(confirmed_cases_worldwide),
            colnames(soln_confirmed_cases_worldwide), 
            info = "The column names of the `confirmed_cases_worldwide` data frame do not correspond with the ones in the CSV file: `\"datasets/confirmed_cases_worldwide.csv\"`."
        ) 
    })
    test_that("has the correct data", {
        expect_equal(
            confirmed_cases_worldwide,
            soln_confirmed_cases_worldwide, 
            info = "The data of the `confirmed_cases_worldwide` data frame do not correspond with data in the CSV file: \"datasets/confirmed_cases_worldwide.csv\"."
        )
    })
})

ggplot(confirmed_cases_worldwide, aes(date, cum_cases)) +
  geom_line() +
  ylab("Cumulative confirmed cases")

confirmed_cases_china_vs_world <- read_csv("confirmed_cases_china_vs_world.csv")
glimpse(confirmed_cases_china_vs_world)

plt_cum_confirmed_cases_china_vs_world <- ggplot(confirmed_cases_china_vs_world) +
  geom_line(aes(date, cum_cases, color = is_china)) +
  ylab("Cumulative confirmed cases")

# See the plot
plt_cum_confirmed_cases_china_vs_world

soln_confirmed_cases_china_vs_world <- read_csv("confirmed_cases_china_vs_world.csv")

who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))

# Using who_events, add vertical dashed lines with an xintercept at date
# and text at date, labeled by event, and at 100000 on the y-axis
plt_cum_confirmed_cases_china_vs_world +
  geom_vline(aes(xintercept = date), data = who_events, linetype = "dashed") +
  geom_text(aes(date, label = event), data = who_events, y = 1e5)

#Filter for China, from Feb 15
china_after_feb15 <- confirmed_cases_china_vs_world %>%
  filter(is_china == "China", date >= "2020-02-15")
# Using china_after_feb15, draw a line plot cum_cases vs. date
# Add a smooth trend line using linear regression, no error bars
ggplot(china_after_feb15, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")

# Filter confirmed_cases_china_vs_world for not China
not_china <- confirmed_cases_china_vs_world %>%
  filter(is_china == "Not China")

# Using not_china, draw a line plot cum_cases vs. date
# Add a smooth trend line using linear regression, no error bars
plt_not_china_trend_lin <- ggplot(not_china, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")

  B3 <- c(1,3,5)
B3
