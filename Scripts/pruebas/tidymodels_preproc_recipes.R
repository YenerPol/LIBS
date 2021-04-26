# 1. Built the model (lr_mod),
# 2. Created a preprocessing recipe (flights_rec),
# 3. Bundled the model and recipe (flights_wflow), and
# 4. Trained our workflow using a single call to fit().

# ----- THE NEW YORK CITY FLIGHT DATA ----

library(tidymodels)      # for the recipes package, along with the rest of tidymodels

# Helper packages
library(nycflights13)    # for flight data
library(skimr)           # for variable summaries

set.seed(123)

flight_data <- 
        flights %>% 
        mutate(
                # Convert the arrival delay to a factor
                arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
                arr_delay = factor(arr_delay),
                # We will use the date (not date-time) in the recipe below
                date = as.Date(time_hour)
        ) %>% 
        # Include the weather data
        inner_join(weather, by = c("origin", "time_hour")) %>% 
        # Only retain the specific columns we will use
        select(dep_time, flight, origin, dest, air_time, distance, 
               carrier, date, arr_delay, time_hour) %>% 
        # Exclude missing data
        na.omit() %>% 
        # For creating models, it is better to have qualitative columns
        # encoded as factors (instead of character strings)
        mutate_if(is.character, as.factor)

flight_data %>% 
        count(arr_delay) %>% 
        mutate(prop = n/sum(n))

glimpse(flight_data)

flight_data %>% 
        skimr::skim(dest, carrier) 

# ----- DATA SPLITTING ----

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(555)
# Put 3/4 of the data into the training set 
data_split <- initial_split(flight_data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# ----- CREATE RECIPE AND ROLES -----
        # esto es de parsnip
flights_rec <- 
        recipe(arr_delay ~ ., data = train_data) %>% 
        # the recipe to keep these two variables but not use them as either outcomes or predictors
        update_role(flight, time_hour, new_role = "ID") 

summary(flights_rec)

# ----- CREATE FEATURES -----

flight_data %>% 
        distinct(date) %>% 
        mutate(numeric_date = as.numeric(date)) 

flights_rec <- 
        recipe(arr_delay ~ ., data = train_data) %>% 
        update_role(flight, time_hour, new_role = "ID") %>% 
        step_date(date, features = c("dow", "month")) %>% 
        step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
        step_rm(date) %>% 
        #Create dummy variables for all of the factor or character columns unless they are outcomes
        step_dummy(all_nominal(), -all_outcomes()) %>% 
        step_zv(all_predictors())

# ----- FIT A MODEL WITH A RECIPE ------

lr_mod <- logistic_reg() %>%
        set_engine("glm")

flights_wflow <- 
        ##  workflows package from tidymodels to bundle our parsnip model (lr_mod) 
        ##  with our recipe (flights_rec).
        workflow() %>% 
        add_model(lr_mod) %>% 
        add_recipe(flights_rec)


flights_fit <- 
        # single function that can be used to prepare the recipe and train the model 
        # from the resulting predictors:
        flights_wflow %>% 
        fit(data = train_data)

# ----- USE A TRAINED WORKFLOW TO PREDICT ------

predict(flights_fit, test_data)

# if we want the predicted class probabilities for each flight instead

flights_pred <- 
        predict(flights_fit, test_data, type = "prob") %>% 
        bind_cols(test_data %>% select(arr_delay, time_hour, flight))

flights_pred %>% 
        roc_curve(truth = arr_delay, .pred_late) %>% 
        autoplot()

flights_pred %>% 
        roc_auc(truth = arr_delay, .pred_late)
