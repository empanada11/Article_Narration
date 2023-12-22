#### Packages ####
library(readr)
library(tidyverse)
library(plotly)
library(readxl)
#library(rnaturalearth)
library(ggplot2)
library(leaflet)
library(sf)
library(maps)


#### Read Data ####
#### > History Data ####
athlete_events <- read_csv("athlete_events.csv")
noc_regions <- read_csv("noc_regions.csv")
#### > Tokyo ####
gender_tokyo <- read_excel('EntriesGender.xlsx')
#### > TEAMS EVENTS ####
teams_tokyo <- read_excel('Teams.xlsx')
#### > Gender Inequality Index ####
map_gender <- read_csv('Gender_Inequality_Index.csv')
#### > Gender Olympics ####
map_tokyo <- read_csv('df_countries_1.csv')
map_tokyo1 <- read_csv('df_countries_2.csv')
map_tokyo <- map_tokyo %>% 
  bind_rows(map_tokyo1)

## muss leider hier noch manuell einlesen
map_tokyo2 <- data.frame(
  Country = c("Armenia", "Belgium", "Botswana", "Canada", "China", "Germany", 
              "Hungary", "Netherlands", "Serbia", "South Africa", "Tunisia",
              "Ukraine", "Uzbekistan"),  # Replace with your country names
  Total_Men = c(14, 68, 8, 148, 125, 254, 81, 113, 43, 115, 34, 66, 37),
  Total_Women = c(3, 55, 5, 233, 281, 171, 88, 165, 44, 64, 29, 89, 30)# Replace with the corresponding values
)
map_tokyo <- map_tokyo %>% 
  bind_rows(map_tokyo2)

# ROC to Russia
map_tokyo[map_tokyo=="Russian Olympic Committee athletes"] <- "Russia"
map_tokyo[map_tokyo=="Great Britain"] <- "United Kingdom"



#### Subset only the summer olympics ####
summer_olympics <- athlete_events %>% 
  filter(Season == "Summer")

#### Woman over the time ####
# Filter the data for female participants in the Summer Olympics
female_summer_olympics <- athlete_events %>%
  filter(Season == "Summer", Sex == "F")

# Aggregate data by year
annual_participation <- female_summer_olympics %>%
  group_by(Year) %>%
  summarise(Count = n())


#### PLOT BEGINNT ####
####> M vs. F over Years ####
# Preparing data for Female participants
female_data <- athlete_events %>%
  filter(Season == "Summer", Sex == "F") %>%
  group_by(Year) %>%
  summarise(FemaleCount = n())

# Preparing data for Male participants
male_data <- athlete_events %>%
  filter(Season == "Summer", Sex == "M") %>%
  group_by(Year) %>%
  summarise(MaleCount = -n()) # Negative count for downward bars

# Get the count of female and male from Tokyo
# Calculate the total number of females and males
total_gen_tokyo <- gender_tokyo %>%
  summarise(FemaleCount = sum(as.numeric(Female)), MaleCount = sum(as.numeric(Male))) %>% 
  mutate(Year = 2020)
total_gen_tokyo$MaleCount <- -total_gen_tokyo$MaleCount


# Combine the datasets
combined_data <- merge(female_data, male_data, by = "Year")
# Now, add the 'total_gen_tokyo' row to 'combined_data'
combined_data <- combined_data %>%
  bind_rows(total_gen_tokyo)

# Create the plot
plot_gender_participation <- plot_ly(data = combined_data, x = ~Year,
                                     hoverinfo = "text",
                                     hovertext = ~paste("Year: ", Year,
                                                        "<br>Female: ", FemaleCount,
                                                        "<br>Male: ", -MaleCount)) %>%
  add_bars(y = ~FemaleCount, name = "Female", marker = list(color = '#BB29BB')) %>%
  add_bars(y = ~MaleCount, name = "Male", marker = list(color = '#0A2472')) %>%
  layout(xaxis = list(title = "Year"),
         yaxis = list(title = "Number of Participants", 
                      range = c(-8000, 8000)),  # Set explicit y-axis limits
         barmode = 'overlay',
         showlegend = FALSE)


#### Woman Peace ####
# clean the map_gender Dataset so only GII
map_gender <- map_gender[c("Country", "Human_development", "GII")]
# Example: Modify specific country names in the 'Country' column
map_gender <- map_gender %>%
  mutate(Country = case_when(
    Country == "United States" ~ "USA",
    Country == "Russian Federation" ~ "Russia",
    Country == "United Kingdom" ~ "UK",
    Country == "Czechia" ~ "Czech Republic",
    Country == "Syrian Arab Republic" ~ "Syria",
    Country == "Lao People's Democratic Republic" ~ "Laos",
    Country == "Viet Nam" ~ "Vietnam",
    # Add more conditions as needed
    TRUE ~ Country  # Leaves all other values unchanged
  ))

# get the world data
# Get world map data
world_map <- map_data("world")
merged_data <- world_map %>% 
  left_join(map_gender, by = c("region" = "Country"))
merged_data_df <- as.data.frame(merged_data)


# GGPLOT2
# Assuming merged_data is your merged dataset
base_plot <- ggplot(merged_data_df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = GII, 
                   text = paste("Country:", region, "<br>GII:", GII)), color = NA) +
  scale_fill_viridis_c(option = "A", direction = -1, na.value = "grey50", guide = "colorbar") +
  labs(fill = "GII") +
  theme_minimal()

# Convert to interactive plotly object
interactive_map_GII <- ggplotly(base_plot, tooltip = "text")

# Show the interactive map
# interactive_map_GII

#### Gender Map ####
map_tokyo <- map_tokyo %>%
  mutate(Country = case_when(
    Country == "United States" ~ "USA",
    Country == "Russian Federation" ~ "Russia",
    Country == "United Kingdom" ~ "UK",
    Country == "Czechia" ~ "Czech Republic",
    Country == "Syrian Arab Republic" ~ "Syria",
    Country == "Lao People's Democratic Republic" ~ "Laos",
    Country == "Viet Nam" ~ "Vietnam",
    # Add more conditions as needed
    TRUE ~ Country  # Leaves all other values unchanged
  ))

# get the world data
map_tokyo_GII <- map_tokyo %>% 
  left_join(map_gender, by = c("Country" = "Country"))
world_map <- map_data("world")
merged_data_tokyo <- world_map %>% 
  left_join(map_tokyo_GII, by = c("region" = "Country"))
merged_data_tokyo.df <- as.data.frame(merged_data_tokyo)


fill_color <- "#BB29BB"  # You can choose any color you prefer
  

# Create the ggplot plot
base_plot <- ggplot(merged_data_tokyo) +
  geom_sf(aes(fill = Total_Women), color = NA) +
  scale_fill_gradient(low = "white", high = fill_color, na.value = "grey50") +
  labs(
    fill = "Woman") +
  theme_minimal()

# make it interactive
# Create the ggplot plot with hover text
base_plot <- ggplot(merged_data_tokyo) +
  geom_polygon(aes(x = long, y = lat, 
                   group = group, fill = Total_Women, text = paste("Country:", region, 
                                               "<br>Total Women:", Total_Women,
                                               "<br>Total Men:", Total_Men,
                                               "<br>GII-Index:", GII)),
          color = NA) +
  scale_fill_gradient(low = "pink", high = fill_color, na.value = "grey50") +
  labs(fill = "Total Women") +
  theme_minimal()


# Convert to interactive plotly object
interactive_map_equality <- ggplotly(base_plot, tooltip = "text")

# Show the interactive map
interactive_map_equality

#### > Ratio Gender Map ####

# get the world data
# get the world data
map_tokyo <- map_tokyo %>% 
  mutate(ratio_women = Total_Women / (Total_Men + Total_Women))
map_tokyo_GII.r <- map_tokyo %>% 
  left_join(map_gender, by = c("Country" = "Country"))
merged_data_tokyo_ratio <- world_map %>% 
  left_join(map_tokyo_GII.r, by = c("region" = "Country"))
merged_data_tokyo_ratio.df <- as.data.frame(merged_data_tokyo_ratio)

fill_color <- "#BB29BB"  # You can choose any color you prefer
  

# Create the ggplot plot
ggplot(merged_data_tokyo_ratio) +
  geom_polygon(aes(x = long, y = lat, 
                   group = group, fill = ratio_women), color = NA) +
  scale_fill_gradient(low = "pink", high = fill_color, na.value = "grey50") +
  labs(
    fill = "Woman") +
  theme_minimal()

# make it interactive
# Base ggplot object with reversed color scale
base_plot_ratio <- ggplot(merged_data_tokyo_ratio) +
  geom_polygon(aes(x = long, y = lat, 
                   group = group, fill = ratio_women, text = paste("Country:", region, 
                                               "<br>Women Ratio:", 
                                               sprintf("%.2f", ratio_women))),
          color = NA) +
  scale_fill_gradient(low = "pink", high = fill_color, na.value = "grey50") +
  labs(fill = "ratio_women") +
  theme_minimal()

# Convert to interactive plotly object
interactive_map_equality_ratio <- ggplotly(base_plot_ratio, tooltip = "text")

# Show the interactive map
# interactive_map_equality_ratio


#### > is there any connection ####

# Calculate the correlation
correlation_test <- cor.test(merged_data_tokyo_ratio$ratio_women, merged_data_tokyo_ratio$GII, method = "pearson")

# Print the results
print(correlation_test)

#### Pie Chart Gender ####
# Calculate the sum of female and male athletes
# Convert 'Female' and 'Male' to numeric
gender_tokyo$Female <- as.numeric(gender_tokyo$Female)
gender_tokyo$Male <- as.numeric(gender_tokyo$Male)
gender_tokyo$Total <- as.numeric(gender_tokyo$Total)

# Calculate the total count for Female and Male
total_female <- sum(gender_tokyo$Female)
total_male <- sum(gender_tokyo$Male)

# Create a data frame for plotting
gender_totals <- data.frame(
  Gender = c("Female", "Male"),
  Count = c(total_female, total_male)
)

# Calculate the percentages
gender_totals$Percentage <- gender_totals$Count / sum(gender_totals$Count) * 100

# Calculate the percentages and round to whole numbers
gender_totals$Percentage <- round(gender_totals$Count / sum(gender_totals$Count) * 100)

# Create the text labels with percentages followed by a percent sign
gender_totals$Text <- paste0(gender_totals$Percentage, "%")

# Update the plot_ly function to use the new text labels
pie_gender <- plot_ly(gender_totals, labels = ~Gender, values = ~Count, 
                      type = 'pie', 
                      hoverinfo = 'label+percent', 
                      textinfo = 'none',  # No text inside the slices
                      marker = list(colors = c('#BB29BB', '#0A2472')),
                      insidetextorientation = 'radial') %>%
  layout(
    title = "Tokyo 2020",
    showlegend = FALSE,
    legend = list(orientation = 'v', x = 1, y = -0.1, xanchor = 'right', yanchor = 'bottom')
  )
# Render the plot
# pie_gender

#### Disciplines Gender ####
# Calculate the difference between Male and Female counts
gender_tokyo$Gender_Difference = gender_tokyo$Male - gender_tokyo$Female

# Order by Gender Difference to find Male-dominated (positive difference)
male_dominated <- gender_tokyo[order(-gender_tokyo$Gender_Difference), ]

# Order by Gender Difference to find Female-dominated (negative difference)
female_dominated <- gender_tokyo[order(gender_tokyo$Gender_Difference), ]

# Display the top Male-dominated disciplines
cat("Top Male-Dominated Disciplines:\n")
head(male_dominated)

# Display the top Female-dominated disciplines
cat("\nTop Female-Dominated Disciplines:\n")
head(female_dominated)

#### > Plot ####
# Calculate gender ratio
gender_tokyo <- gender_tokyo %>% 
  mutate(Ratio = (Male - Female) / Total)

# Prepare the data for plotting
gender_tokyo_long <- gender_tokyo %>% 
  gather(key = "Gender", value = "Count", Female, Male) %>% 
  arrange(Total)

# Plot
discipline_gender <- ggplot(gender_tokyo_long, aes(x = reorder(Discipline, Ratio), y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +  # Flip coordinates for horizontal bars
  scale_fill_manual(values = c("Female" = "#BB29BB", "Male" = "#0A2472")) +
  labs(x = "Discipline", y = "Count", fill = "Gender") +
  theme_minimal() +
  theme(legend.position = "bottom")


#### PLOT DISCIPLINE EVENT ####
events_aggregated <- teams_tokyo %>%
  group_by(Discipline) %>%
  summarise(Events = list(unique(Event)))

# Merge with gender_tokyo
gender_tokyo_events <- gender_tokyo %>%
  left_join(events_aggregated, by = "Discipline")

gender_tokyo_events <- gender_tokyo_events %>% 
  mutate(Ratio = (Female) / Total)

# Prepare the data for plotting
gender_tokyo_events_long <- gender_tokyo_events %>% 
  gather(key = "Gender", value = "Count", Female, Male) %>% 
  arrange(Total)

# Plot
# Convert the list of events into a string for each discipline
gender_tokyo_events_long <- gender_tokyo_events_long %>%
  mutate(Events_Text = sapply(Events, function(e) paste(e, collapse = "\n")))

# Modify the ggplot with tooltip text
discipline_event <- ggplot(gender_tokyo_events_long, aes(x = reorder(Discipline, Ratio), 
                                                         y = Count, fill = Gender,
                                                         text = paste("Discipline:", Discipline,
                                                                      "<br>Events:", Events_Text))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  scale_fill_manual(values = c("Female" = "#BB29BB", "Male" = "#0A2472")) +
  labs(x = "Discipline", y = "Count", fill = "Gender") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convert to interactive plotly object
interactive_discipline_event <- ggplotly(discipline_event, tooltip = "text")

# Show the interactive map
# interactive_discipline_event


#### CHATGPT PROMPT DISCIPLINE ####
# Prepare the data for plotting
# Convert the list of events into a string for each discipline
events_aggregated <- teams_tokyo %>%
  group_by(Discipline) %>%
  summarise(Events = list(unique(Event)))

# Merge with gender_tokyo
gender_tokyo_events <- gender_tokyo %>%
  left_join(events_aggregated, by = "Discipline")

gender_tokyo_events <- gender_tokyo_events %>% 
  mutate(Ratio = (Female) / Total)

# Prepare the data for plotting
gender_tokyo_events_long <- gender_tokyo_events %>% 
  gather(key = "Gender", value = "Count", Female, Male) %>% 
  arrange(Total)

# Plot
# Convert the list of events into a string for each discipline
gender_tokyo_events_long <- gender_tokyo_events_long %>%
  mutate(Events_Text = sapply(Events, function(e) paste(e, collapse = "\n")))

gender_tokyo_events_long <- gender_tokyo_events_long %>%
  mutate(Events_Text = sapply(Events, function(e) paste(e, collapse = "\n")))

gender_tokyo_events_long <- gender_tokyo_events_long%>%
  mutate(Tooltip_Text = paste(
    "<br>Events:<br>", Events_Text,
    "<br>Total Athletes:", Total,
    "<br>Gender Ratio (Female):", sprintf("%.2f", Ratio)))



# Modified plot to emphasize gender disparity
discipline_event <- ggplot(gender_tokyo_events_long, aes(x = reorder(Discipline, Ratio), 
                                                         y = Count, fill = Gender,
                                                         text = Tooltip_Text)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  scale_fill_manual(values = c("Female" = "#BB29BB", "Male" = "#0A2472")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Discipline", y = "Count", fill = "Gender") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convert to interactive plotly object with enhanced tooltips
interactive_discipline_event <- ggplotly(discipline_event, tooltip = "text")

# # Show the interactive plot
# interactive_discipline_event

# Women Dominated: Modified plot to emphasize gender disparity
discipline_event_women <- gender_tokyo_events_long %>% 
  filter(Ratio > 0.5) %>% 
  ggplot(aes(x = reorder(Discipline, Ratio), 
             y = Count, fill = Gender,
             text = Tooltip_Text)) + # 'aes()' is for mappings, data is specified before 'ggplot()'
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  scale_fill_manual(values = c("Female" = "#BB29BB", "Male" = "#0A2472")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Discipline", y = "Count", fill = "Gender") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Convert to interactive plotly object with enhanced tooltips
interactive_discipline_event_women <- ggplotly(discipline_event_women, tooltip = "text")

# # Show the interactive plot
#interactive_discipline_event_women

# Men Dominated: Modified plot to emphasize gender disparity
discipline_event_men <- gender_tokyo_events_long %>% 
  filter(Ratio < 0.41) %>% 
  ggplot(aes(x = reorder(Discipline, Ratio), 
             y = Count, fill = Gender,
             text = Tooltip_Text)) + # 'aes()' is for mappings, data is specified before 'ggplot()'
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  scale_fill_manual(values = c("Female" = "#BB29BB", "Male" = "#0A2472")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Discipline", y = "Count", fill = "Gender") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Convert to interactive plotly object with enhanced tooltips
interactive_discipline_event_men <- ggplotly(discipline_event_men, tooltip = "text")

# # Show the interactive plot
#interactive_discipline_event_men



# Get world map data
world_map <- map_data("world")

# Assuming map_tokyo_GII is your data frame with gender data
# Merge your data with the world map data
merged_data <- merge(world_map, map_tokyo_GII, by.x = "region", by.y = "Country")

# Create the plot
ggplot(merged_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Total_Women)) +
  scale_fill_gradient(low = "white", high = "#BB29BB", na.value = "grey50") +
  labs(fill = "Total Women") +
  theme_minimal()























