source('local_laplace.R')

# Example using simulated data ----------------------------------------------------------------

#Possible questions in a given study
age_group <- c('18-25', '25-35', '36-45', '46-55', '56-65', '66-75', '75+')
race <- c('Alaska Native', 'Asian', 'Black', 'Hispanice',
          'Pacific Islander', 'White', 'Other', 'Not Answered')
how_do_u_feel <- c('Good', 'Not well')
fatigue <- c('yes', 'no')
temperature <- c('yes', 'no')
cough <- c('yes', 'no')
chills <- c('yes', 'no')
congestion <- c('yes', 'no')
loss_taste_smell <- c('yes', 'no')
headache <- c('yes', 'no')
muscle_pain <- c('yes', 'no')
tight_chest <- c('yes', 'no')
covid_virus_test <- c('yes', 'no')
covid_ant_test <- c('yes', 'no')
influenza_test <- c('yes', 'no')
past_unc_covid <- c('yes', 'no', 'I dont know')
exposed <- c('yes', 'no', 'probably', 'unlikely')
household_symptom <- c('yes', 'no')
parent <- c('yes', 'no')
cancelled_appointment <- c('yes', 'no')
sleep <- c('< 5', '5-6', '7-8', '9-10', '11+')
diabetes <-  c('yes', 'no')
hypertension <-  c('yes', 'no')
cardiovascular <-  c('yes', 'no')
asthma <-  c('yes', 'no')
allergies <-  c('yes', 'no')
lung_disease <-  c('yes', 'no')
kidney_disease <-  c('yes', 'no')
cancer <-  c('yes', 'no')
immunodeficiency <-  c('yes', 'no')
auto_immune <-  c('yes', 'no')
pregnant <-  c('yes', 'no')
smoked <- c('yes', 'used to', 'never')
frontline_industry <- c('yes', 'no')
left_home <- c('work', 'exercise', 'another', 'no')
social_distance <- c('yes', 'no')
mask <- c('yes', 'no')
contact <- c('0', '1', '>1')



## E.g., create Study 1 questions: symptoms
# Need grid of all possible answer combinations
unique_combn_s1 <- expand.grid(age_group, race, how_do_u_feel, fatigue, temperature,
                               cough, chills, congestion, loss_taste_smell, headache, muscle_pain,
                               tight_chest)


## E.g., Study 2 questions: precautions
unique_combn_s2 <- expand.grid(age_group, how_do_u_feel, household_symptom,
                               left_home, social_distance, mask, contact)


# Construct simulated data
# Total number of respondents
n  <- 100

# We will randomly assign respondents to study 1 or study 2

assignments <- sample(c(rep(1, n/2), rep(2, n/2)), n, replace = FALSE)

# Let's focus on study 2 for now
# Sample some completely random responses (in practice we think the data will have more structure)

responses_s2 <- apply(unique_combn_s2, 2, function(i) sample(i, replace = TRUE, n/2))

# Now we will make these responses differentially private

# First create private one hot encoding
one_hot_list <- lapply(1:nrow(responses_s2), function(row) oneHot(responses_s2[row,], unique_combn = unique_combn_s2))

# Then add random DP noise to each on the device

epsilon <- 0.5 # privacy parameter
one_hot_dp <- deviceDP(one_hot_list, epsilon = epsilon, n = nrow(responses_s2))

# Now we can aggregate the individual DP responses
dp_data <- dpHistogram(one_hot_dp, unique_combn_s2)

# So now we have a differentially private data set

dp_data

# (Then we could do the same thing for study 1)