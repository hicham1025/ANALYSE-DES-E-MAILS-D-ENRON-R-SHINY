load("Enron.RData")

ls()

str(employeelist)

str(message)
str(recipientinfo)
str(referenceinfo)

library(dplyr)

getwd()

load("Enron.RData")

message %>%
  group_by(sender)

message %>%
  group_by(sender) %>%
  summarise(total_sent = n())

message %>%
  group_by(sender) %>%
  summarise(total_sent = n()) %>%
  arrange(desc(total_sent))


message %>%
  group_by(sender) %>%
  summarise(total_sent = n()) %>%
  arrange(desc(total_sent)) %>%
  head(10)

top_senders <- message %>%
  group_by(sender) %>%
  summarise(total_sent = n()) %>%
  arrange(desc(total_sent)) %>%
  head(10)

top_senders_named <- left_join(
  top_senders,
  employeelist,
  by = c("sender" = "Email_id")
)

top_senders_named


message_with_roles <- left_join(
  message,
  employeelist,
  by = c("sender" = "Email_id")
)

emails_by_role <- message_with_roles %>%
  group_by(status) %>%
  summarise(total_sent = n()) %>%
  arrange(desc(total_sent))

emails_by_role

install.packages("ggplot2")
library(ggplot2)
ggplot(data = emails_by_role, aes(x = status, y = total_sent)) +
  geom_col()

ggplot(emails_by_role, aes(x = reorder(status, total_sent), y = total_sent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Emails Sent by Job Role",
       x = "Job Role",
       y = "Number of Emails")


na_senders <- message_with_roles %>%
  filter(is.na(status)) %>%
  group_by(sender) %>%
  summarise(total_sent = n()) %>%
  arrange(desc(total_sent))

head(na_senders, 10)

na_senders

library(tidyr)

employee_emails <- employeelist %>%
  pivot_longer(cols = starts_with("Email"), 
               names_to = "which_email", 
               values_to = "email") %>%
  filter(email != "")

message_with_roles_expanded <- left_join(
  message,
  employee_emails,
  by = c("sender" = "email")
)
emails_by_role_full <- message_with_roles_expanded %>%
  group_by(status) %>%
  summarise(total_sent = n()) %>%
  arrange(desc(total_sent))

ggplot(emails_by_role_full, aes(x = reorder(status, total_sent), y = total_sent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Emails Sent by Job Role (Expanded Matching)",
       x = "Job Role",
       y = "Number of Emails")


emails_by_role_full <- emails_by_role_full %>%
  mutate(status = ifelse(is.na(status), "Unknown", status))

emails_by_role_full <- emails_by_role_full %>%
  mutate(status = as.character(status)) %>%   # convert to text
  mutate(status = ifelse(is.na(status), "Unknown", status))

ggplot(emails_by_role_full, aes(x = reorder(status, total_sent), y = total_sent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Emails Sent by Job Role (Expanded Matching)",
       x = "Job Role",
       y = "Number of Emails")




emails_by_role_full <- message_with_roles_expanded %>%
  group_by(status) %>%
  summarise(total_sent = n()) %>%
  arrange(desc(total_sent)) %>%
  mutate(status = as.character(status)) %>%             
  mutate(status = ifelse(is.na(status), "Unknown", status))  # Replace NA


ggplot(emails_by_role_full, aes(x = reorder(status, total_sent), y = total_sent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Emails Sent by Job Role (Expanded Matching)",
       x = "Job Role",
       y = "Number of Emails")

emails_by_day <- message %>%
  group_by(date) %>%
  summarise(total_sent = n())

head(emails_by_day)

library(ggplot2)

ggplot(emails_by_day, aes(x = date, y = total_sent)) +
  geom_line(color = "blue") +
  labs(title = "Email Activity Over Time",
       x = "Date",
       y = "Number of Emails")

range(message$date, na.rm = TRUE)

message_clean <- message %>%
  filter(date >= as.Date("1990-01-01"))

emails_by_day <- message_clean %>%
  group_by(date) %>%
  summarise(total_sent = n())

ggplot(emails_by_day, aes(x = date, y = total_sent)) +
  geom_line(color = "blue") +
  labs(title = "Email Activity Over Time (Cleaned Dates)",
       x = "Date",
       y = "Number of Emails")

sum(message$date < as.Date("1990-01-01"))

message_clean <- message %>%
  filter(date >= as.Date("1999-01-01") & date <= as.Date("2002-12-30"))

emails_by_day <- message_clean %>%
  group_by(date) %>%
  summarise(total_sent = n())

ggplot(emails_by_day, aes(x = date, y = total_sent)) +
  geom_line(color = "blue") +
  labs(title = "Email Activity (1999–2002)",
       x = "Date",
       y = "Number of Emails")

str(recipientinfo)

recipients_per_message <- recipientinfo %>%
  group_by(mid) %>%
  summarise(recipient_count = n())

head(recipients_per_message)

ggplot(recipients_per_message, aes(x = recipient_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Number of Recipients per Message",
       x = "Number of Recipients",
       y = "Count of Messages")

recipients_per_message_filtered <- recipients_per_message %>%
  filter(recipient_count <= 30)

ggplot(recipients_per_message_filtered, aes(x = recipient_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Recipients per Message (Filtered ≤ 50)",
       x = "Number of Recipients",
       y = "Count of Messages")

recipients_per_message %>%
  arrange(desc(recipient_count)) %>%
  head(10)

recipient_type_counts <- recipientinfo %>%
  group_by(rtype) %>%
  summarise(count = n())

recipient_type_counts

ggplot(recipient_type_counts, aes(x = rtype, y = count)) +
  geom_col(fill = "steelblue") +
  labs(title = "Recipient Types (TO, CC, BCC)",
       x = "Recipient Type",
       y = "Number of Recipients")

str(referenceinfo)

references_per_message <- referenceinfo %>%
  group_by(mid) %>%
  summarise(reference_count = n())


head(references_per_message)

ggplot(references_per_message, aes(x = reference_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of References per Message",
       x = "Number of References",
       y = "Count of Messages")

library(tidytext)

library(dplyr)
library(ggplot2)
library(stringr)
library(wordcloud)
library(tm)

install.packages(c("tidytext", "tm", "wordcloud"))


subject_words <- message %>%
  select(subject) %>%
  filter(!is.na(subject)) %>%
  mutate(subject = str_to_lower(subject)) %>%
  unnest_tokens(word, subject) %>%
  filter(!word %in% stopwords("en")) %>%  # remove common stop words
  count(word, sort = TRUE)


top_words <- subject_words %>% 
  slice_max(order_by = n, n = 20)

ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Words in Email Subjects",
       x = "Word",
       y = "Frequency")


filtered_data <- reactive({
  req(input$date_range)
  message %>%
    filter(date >= input$date_range[1], date <= input$date_range[2])
})

emails_by_role_filtered <- reactive({
  filtered_data() %>%
    left_join(employeeList, by = c("sender" = "Email_id")) %>%
    group_by(status) %>%
    summarise(total_sent = n()) %>%
    mutate(status = ifelse(is.na(status), "Unknown", status)) %>%
    arrange(desc(total_sent))
})


