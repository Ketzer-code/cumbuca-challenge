# importing libraries
library(nycflights13)
library(tidyverse)
library(tidylog)
library(Information)

#checking nas on the delays
flights %>%
    filter(is.na(arr_delay) | is.na(dep_delay)) %>%
    count()

# checking if there are any delays with NA where
# arrival and departure times are available
flights %>%
    filter(
        is.na(arr_delay)
        & !is.na(arr_time)
        & !is.na(sched_arr_time)
        & !is.na(dep_delay)
        ) %>%
    count()

flights %>%
    filter(
        is.na(dep_delay)
        & !is.na(dep_time)
        & !is.na(sched_dep_time)
        & !is.na(arr_delay)
    ) %>%
    count()

### ---- QUESTION 1 ---- ###

# less than 1% of delays can be properly treated,
# best to follow without them - creating function
# to generate summary table

create_summary <- function(df, var) {
    df_summarized <- df %>%
                         mutate(
                            mean_var = mean({{var}}),
                            sd_var = sd({{var}}),
                            p25 = quantile({{var}}, probs = c(.25)),
                            p50 = median({{var}}),
                            p90 = quantile({{var}}, probs = c(.9)),
                            p99 = quantile({{var}}, probs = c(.99))
                        )
}

# creating delay and day of flight variables
# for dataframe
flights <- flights %>%
            mutate(
                total_delay = dep_delay + arr_delay,
                flight_day_month = lubridate::ymd(str_sub(time_hour, 1, 10)),
                is_delayed = ifelse(total_delay > 0, 1, 0),
                month = strftime(flight_day_month, "%m"),
                weekday = strftime(flight_day_month, "%u"),
                week = strftime(flight_day_month, "%U")
            )

# creating summary for each day of the year
delay_per_day <- flights %>%
                    filter(!is.na(total_delay)) %>%
                    group_by(flight_day_month) %>%
                    create_summary(total_delay) %>%
                    distinct(flight_day_month, .keep_all = T)

# checking mean avg delay per month
delay_per_day %>%
    ggplot(aes(month, mean_var)) +
    geom_boxplot()

delay_per_day %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        mean_avg_delay = mean(mean_var),
        sd_avg_delay = sd(mean_var),
        cv_avg_delay = sd_avg_delay / mean_avg_delay * 100
    ) %>%
    ggplot(aes(month, mean_avg_delay)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean_avg_delay - sd_avg_delay, ymax = mean_avg_delay + sd_avg_delay)) +
    geom_text(aes(label = round(mean_avg_delay, 2)), vjust = - 1)

# standard deviation outweights mean by
# a lot, in all months, checking in how
# many days the deviation is larger than the
# mean
delay_per_day %>%
    filter(sd_var > mean_var) %>%
    count()

# analyzing percentiles 90 and 99 by
# month, to see how extreme cases behave
V

# veryfing if there are specific
# weekdays or weeks of the year that concentrate extreme cases

## REFACTOR THIS ##
delay_per_day %>%
    ungroup() %>%
    group_by(weekday) %>%
    summarise(
        total_days = n(),
        p90_ext_delays = sum(p90 > 217),
        p99_ext_delays = sum(p99 > 471),
        p90_pct_ext_delays = p90_ext_delays / total_days,
        p99_pct_ext_delays = p99_ext_delays / total_days
    ) %>%
    view(.)

delay_per_day %>%
    ungroup() %>%
    group_by(week) %>%
    summarise(
        total_days = n(),
        p90_ext_delays = sum(p90 > 217),
        p99_ext_delays = sum(p99 > 471),
        p90_pct_ext_delays = p90_ext_delays / total_days,
        p99_pct_ext_delays = p99_ext_delays / total_days
    ) %>%
    view(.)

# counting flights, delays and early arrivals per month
flights %>%
    dplyr::select(month, is_delayed) %>%
    table(.) %>%
    addmargins(.)

# some weeks and days concentrate more delays
# than others. lastly, analyzing percentiles 25 and 50
delay_per_day %>%
    pull(p25) %>%
    quantile(., probs = seq(0, 1, by = 0.1))

delay_per_day %>%
    pull(p50) %>%
    quantile(., probs = seq(0, 1, by = 0.1))

### ---- QUESTION 2 ---- ###

# checking if there is a particular airline that has a 
# higher percentage of delays compared to the total
`delays_per_airline <- flights %>%
                            filter(!is.na(total_delay)) %>%
                            inner_join(airlines, by = "carrier") %>%
                            dplyr::select(name, is_delayed) %>%
                            table(.)`

## REFACTOR THIS ##

prop_delays_airline <- delays_per_airline %>%
                        prop.table(margin = 1) %>%
                        as.data.frame(.) %>%
                        pivot_wider(names_from = is_delayed, values_from = Freq) %>%
                        rename(N = `0`, Y = `1`) %>%
                        mutate(early_arrivals_delays_ratio = N / Y) %>%
                        arrange(., early_arrivals_delays_ratio) %>%
                        rbind(
                            flights %>%
                                dplyr::select(is_delayed) %>%
                                table(.) %>%
                                prop.table(.) %>%
                                as.data.frame(.) %>%
                                mutate(name = "Total") %>%
                                pivot_wider(., names_from = ., values_from = Freq) %>%
                                rename(N = `0`, Y = `1`) %>%
                                mutate(early_arrivals_delays_ratio = N / Y)
                        )


# creating airline groups based on distribution
# of delays and non delays

## REFACTOR THIS ##
airline_groups <- flights %>%
                    inner_join(airlines, by = "carrier") %>%
                    mutate(group = case_when(
                        name %in% c("Frontier Airlines Inc.", "AirTran Airways Corporation") ~ 1,
                        name %in% c("Southwest Airlines Co.", "ExpressJet Airlines Inc.", "Mesa Airlines Inc.") ~ 2,
                        name %in% c("JetBlue Airways", "Envoy Air", "United Air Lines Inc.", "Endeavor Air Inc.") ~ 3,
                        name %in% c("American Airlines Inc.", "US Airways Inc.", "Delta Air Lines Inc", "Delta Air Lines Inc.", "Virgin America") ~ 4,
                        name %in% c("SkyWest Airlines Inc.", "Alaska Airlines Inc.") ~ 5,
                        TRUE ~ 6
                    )) 

## REFACTOR THIS ##
airline_groups %>%
    dplyr::select(group, is_delayed) %>%
    table(.) %>%
    prop.table(margin = 1) %>%
    as.data.frame(.) %>%
    pivot_wider(names_from = is_delayed, values_from = Freq) %>%
    rename(N = `0`, Y = `1`) %>%
    mutate(early_arrivals_delays_ratio = N / Y) %>% 
    arrange(., early_arrivals_delays_ratio) %>%
    rbind(
        flights %>%
            dplyr::select(is_delayed) %>%
            table(.) %>%
            prop.table(.) %>%
            as.data.frame(.) %>%
            mutate(group = "Total") %>%
            pivot_wider(., names_from = ., values_from = Freq) %>%
            rename(N = `0`, Y = `1`) %>%
            mutate(early_arrivals_delays_ratio = N / Y)
    )

# calculating iv statistic to verify
# if variables can discriminate target variable
airline_groups %>%
    dplyr::select(group, is_delayed) %>%
    filter(!is.na(is_delayed)) %>%
    create_infotables(., y = "is_delayed", bins = 6)
  
### ---- QUESTION 3 ---- ###

# preparing dataset
flights <- flights %>%
            left_join(planes %>% dplyr::select(manufacturer, tailnum), by = "tailnum")

# most adequate analysis is anova - since sample is big,
# analyzing if there is evidence for equal variance between groups
flights %>%
    filter(!is.na(manufacturer)) %>%
    ggplot(aes(manufacturer, distance)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))

# clearly suggests for unequal variance, outliers are also
# a problem for anova - substituting extreme values by p05
# and p95 per group and replotting

manufacturer_anova_df <- flights %>%
                            filter(!is.na(manufacturer.x)) %>%
                            dplyr::select(manufacturer.x, distance) %>%
                            group_by(manufacturer.x) %>%
                            mutate(
                                p05 = quantile(distance, probs = c(.05)),
                                p95 = quantile(distance, probs = c(.95)),
                                distance = case_when(
                                    distance < p05 ~ p05,
                                    distance > p95 ~ p95,
                                    TRUE ~ distance
                                )
                                )

manufacturer_anova_df %>%
    ggplot(aes(manufacturer, distance)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# improvement on outliers, plot still suggest difference between
# distance for manufacturers - checking observations per manufacturer
manufacturer_anova_df %>%
    summarise(count = n()) %>%
    view()

# some manufacturers dont have enough observations to assume
# normality, mainting only ones with > 30
manufacturer_anova_df <- manufacturer_anova_df %>%
                            mutate(count = n()) %>%
                            filter(count > 30)

# testing
oneway.test(distance ~ manufacturer, 
    data = manufacturer_anova_df,
    var.equal = FALSE
)

# repeating the same process for companies
flights %>%
    inner_join(airlines, by = "carrier") %>%
    dplyr::rename(company = name) %>%
    ggplot(aes(company, distance)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# plot does not suggest any meaningful outliers, but unequal variance - checking
# obs per company
flights %>%
    inner_join(airlines, by = "carrier") %>%
    dplyr::rename(company = name) %>%
    group_by(company) %>%
    summarise(count = n())

flights <- flights %>%
            inner_join(airlines, by = "carrier")

# every company has at least 30 obs. testing
oneway.test(distance ~ name,
    data = flights,
    var.equal = FALSE
)
# some companies have 0 variance, treating dataframe
# accordingly

company_anova_df <- flights %>%
                    group_by(name) %>%
                    mutate(var_distance = var(distance)) %>%
                    filter(var_distance > 0)

oneway.test(distance ~ name,
    data = company_anova_df,
    var.equal = FALSE
)

