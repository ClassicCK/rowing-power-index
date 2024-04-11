library(reactable)
library(htmltools)
library(showtext)
library(webshot)
library(dplyr)

# Assuming db is your database connection
db <- dbConnect(SQLite(), dbname = "rpi.db")

# Run the Elo rating update process
RPI(db)

rpi_results_query <- "
SELECT 
  Teams.TeamName, Teams.League, Teams.Division, Teams.Country, 
  TeamRPI.RPIDate, TeamRPI.RPI  
FROM TeamRPI
JOIN Teams ON TeamRPI.TeamName = Teams.TeamName
ORDER BY TeamRPI.RPIDate DESC, TeamRPI.RPI DESC;"

rpi_results <- dbGetQuery(db, rpi_results_query)

# Step 1: Identify the most recent and second most recent dates for each team
rpi_dates <- rpi_results %>%
  arrange(TeamName, desc(RPIDate)) %>%
  group_by(TeamName) %>%
  mutate(RankDate = row_number()) %>%
  ungroup()

# Step 2: Determine the most recent date overall
latest_date <- "2024-04-07" #max(rpi_dates$RPIDate) # can also manually set
second_date <- "2024-03-30" #unique(rpi_dates$RPIDate)[2] # can also manually set

# Step 3: Separate the data into two datasets for the most recent and second most recent competitions
latest_rpi <- rpi_dates %>%
  filter(RPIDate == latest_date) %>%
  arrange(desc(RPIDate)) %>%
  group_by(TeamName) %>%
  slice(1) %>% 
  ungroup()

second_latest_rpi <- rpi_dates %>%
  filter(RPIDate == second_date) %>%
  arrange(desc(RPIDate)) %>%
  group_by(TeamName) %>%
  slice(1) %>% 
  ungroup()

# Calculate rankings for the most recent date
latest_rpi <- latest_rpi %>%
  mutate(Rank = rank(-RPI, ties.method = "first"))

# Calculate rankings for the second most recent date
second_latest_rpi <- second_latest_rpi %>%
  mutate(Rank = rank(-RPI, ties.method = "first"))

# Merge the latest rankings with the second latest to calculate rank change
rank_change <- latest_rpi %>%
  select(TeamName, LatestRank = Rank, RPI = RPI) %>%
  left_join(second_latest_rpi %>% select(TeamName, PreviousRank = Rank), by = "TeamName") %>%
  mutate(RankChange = PreviousRank - LatestRank) %>%
  left_join(rpi_results %>% select(TeamName, Country = Country, Division = Division), by = "TeamName")

ranks <- unique(rank_change)

######## Calculate Chances for Different Races #########
stotesbury <- ranks %>%
  filter(Country == "USA", Division == "Scholastic", !(TeamName %in% c("New Trier",
                                                                        "Belen Jesuit",
                                                                        "Loyola Academy",
                                                                        "Deerfield",
                                                                        "Central Catholic",
                                                                        "Pinecrest",
                                                                        "Lake Brantley",
                                                                        "St. Ignatius (OH)",
                                                                        "Mel High",
                                                                        "Berkeley",
                                                                        "Great Bay",
                                                                        "Ransom Everglades",
                                                                        "Westford Academy",
                                                                        "Shrewsbury (MA)",
                                                                        "Arlington Belmont",
                                                                        "Boston Latin",
                                                                        "Bolles",
                                                                        "McCallie")))

youths <- ranks %>%
  filter(Country == "USA")

schools <- ranks %>%
  filter(Country == "GB")

henley <- ranks

# Now you can run the overall simulation using the updated functions
Stotesbury <- simulate_stotesbury(stotesbury, 100000)[,c("TeamName", "Champion_Prob")]
Youths <- simulate_youths(youths, 100000)[,c("TeamName", "Champion_Prob")]
Schools <- simulate_schools(schools, 10000)[,c("TeamName", "Champion_Prob")]
Henley <- simulate_henley(henley, 100000)[,c("TeamName", "Champion_Prob")]

ranks <- left_join(ranks, Stotesbury, by = "TeamName")
ranks <- left_join(ranks, Youths, by = "TeamName")
ranks <- left_join(ranks, Schools, by = "TeamName")
ranks <- left_join(ranks, Henley, by = "TeamName")

ranks[is.na(ranks)] <- 0

colnames(ranks)[8:11] <- c("Stotesbury", "Youths", "Schools", "Henley")

##### Generate Table #####
championship_cols <- c("Stotesbury", "Youths", "Schools", "Henley")

forecasts <- ranks[,c("LatestRank", "Country", "RankChange", "TeamName", "RPI", "Division", championship_cols)]

# Your forecasts data frame should have the RPI values and the range should be known
low_rpi <- min(forecasts$RPI, na.rm = TRUE)
high_rpi <- max(forecasts$RPI, na.rm = TRUE)

#knockout_pct_color <- make_color_pal(c("#ffffff","#FFC0CB", "#FFAEC9", "#FF8EB4", "#E683D9", "#9B5FC6"), bias = 2)
knockout_pct_color <- make_color_pal(c("#ffffff","#FAEAFF", "#AE7FFF", "#8A5FDB", "#6740B7"), bias = 2)

# Table
tbl <- reactable(
  forecasts,
  pagination = FALSE,
  defaultSorted = "LatestRank",
  defaultSortOrder = "asc",
  defaultColGroup = colGroup(headerVAlign = "bottom"),
  columnGroups = list(
  colGroup(name = "Probability of Winning Championship", columns = championship_cols)
  ),
  defaultColDef = colDef(
    vAlign = "center",
    headerVAlign = "bottom",
    class = "cell",
    headerClass = "header"
  ),
  columns = list(
    LatestRank = colDef(
      header = "Rank",
      defaultSortOrder = "asc",
      maxWidth = 75,
      align = "right",
      class = "cell group", headerStyle = list(fontWeight = 700),
      cell = function(value) { format(round(value), nsmall = 0) }),
    TeamName = colDef(
      header = "Crew",
      defaultSortOrder = "asc",
      headerStyle = list(fontWeight = 700),
      minWidth = 375,
      cell = function(value, index) {
        country_code <- forecasts$Country[index]
        img_src <- knitr::image_uri(sprintf("images/%s.png", country_code))
        change <- forecasts$RankChange[index]
        change_symbol <- if (change > 0) "▲" else if (change < 0) "▼" else ""
        change_color <- if (change > 0) "green" else if (change < 0) "red" else "white"
        
        div(
          class = "team-name",
          img(src = img_src, class = "team-flag", alt = paste(country_code, "flag"), style = list(height = "16px", marginRight = "10px")),
          span(class = "team-name", value),
          span(style = list(color = change_color, marginLeft = "10px"),
               sprintf("%s %d", change_symbol, abs(change)))
        )
      }
    ),
    RPI = colDef(
      header = "RPI",
      headerStyle = list(fontWeight = 700),
      align = "left",
      minWidth = 75,
      class = "cell group",
      cell = function(value) { format(round(value), nsmall = 0) }
    ),
    Division = colDef(
      header = "Category",
      headerStyle = list(fontWeight = 700),
      align = "left",
      minWidth = 175,
      class = "cell group"),
    Country = colDef(show = FALSE),
    RankChange = colDef(show = FALSE),
  Stotesbury = champion_column(name = "Stotesbury Cup", class = "border-left"),
  Youths = champion_column(name = "Youth Nationals"),
  Schools = champion_column(name = "National Schoolboys"),
  Henley = champion_column(name = "Henley")),
  style = list(fontFamily = 'Roboto Mono'),
  showSortIcon = TRUE,
  borderless = TRUE,
  class = "standings-table"
)

# Define the additional subheading text with bold terms
subheading_text <- div(
  p(HTML("<strong>RPI</strong> ratings are an expanded ELO ranking system that estimates a crew's strength based on each team's recent regatta results, adapted for multi-team events, and is updated weekly."))
)

update_timestamp <- div(
  id = "last-updated",
  class = "update-timestamp",
  "UPDATED: " # JavaScript will fill in the rest
)

# Define the additional caption content
caption_content <- div(
  style = list(
    display = "flex",
    justifyContent = "space-between", # Distribute space evenly between items
    width = "100%",  # Make sure the container spans the full width
    fontSize = "0.875rem",
    fontFamily = "'Roboto Mono', monospace",
    marginTop = "4rem",  # Adjust as necessary for spacing from the table
    marginBottom = "1rem" # Adjust as necessary for bottom spacing
  ),
  div("April 8, 2024 Forecasts", style = list(flex = "1")),
  div("Twitter/X: @ClassicCK", style = list(flex = "1", textAlign = "right")),
  div(HTML("&copy; 2024 Christopher L. Kilner, PhD"), style = list(flex = "1", textAlign = "center"))
)

html_content <- tagList(
  tags$head(
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var now = new Date();
        var dateString = 'UPDATED ' + now.toLocaleDateString('en-US', { year: 'numeric', month: 'long', day: 'numeric' }) + ', AT ' + now.toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit' });
        document.getElementById('last-updated').textContent = dateString;
      });
    ")),
    tags$link(href = "https://fonts.googleapis.com/css?family=Rubik:400,700|Roboto+Mono&display=fallback", rel = "stylesheet"),
    tags$style(HTML("
    body {
    padding: 0 20px;
    }
    
.standings {
  font-family: Rubik, sans-serif;
  font-size: 0.875rem;
}

.update-timestamp {
  text-align: center;
  font-size: 0.875rem;
  margin-top: 1rem;
  margin-bottom: 1rem;
  font-family: 'Roboto Mono', monospace;
  color: #666;
}

.title {
  margin-top: 2rem;
  margin-bottom: 1.125rem;
  font-size: 1rem;
}

.title h1 {
  font-size: 3rem;
  font-weight: 600;
  align: center;
}

.title h2 {
  font-size: 1.25rem;
  font-weight: 600;
}

.subtitle {
  font-size: 1.125rem;
  font-weight: 400;
  align: center;
}

.standings-table {
  margin-bottom: 1.25rem;
}

.header {
  border-bottom-color: #555;
    font-size: 0.8125rem;
  font-weight: 400;
  text-transform: uppercase;
}

/* Highlight headers when sorting */
  .header:hover,
.header:focus,
.header[aria-sort=ascending],
.header[aria-sort=descending] {
  background-color: transparent;
}

.border-left {
  border-left: 2px solid #555;
}

/* Use box-shadow to create row borders that appear behind vertical borders */
  .cell {
    box-shadow: inset 0 -1px 0 rgba(0, 0, 0, 0.15);
  }

.group-last .cell {
  box-shadow: inset 0 -2px 0 #555;
}

.team {
  display: flex;
  align-items: center;
}

.team-flag {
  height: 1.3rem;
  border: 1px solid #f0f0f0;
}

.team-name {
  margin-left: 0.5rem;
  font-size: 1.125rem;
  font-weight: 700;
}

.team-record {
  margin-left: 0.35rem;
  color: hsl(0, 0%, 45%);
  font-size: 0.8125rem;
}

.group {
  font-size: 1.1875rem;
}

.number {
  font-family: Roboto Mono, monospace;
  font-size: 1rem;
  white-space: pre;
}
"))
  ),
  tags$body(
    div(
      class = "standings",
      update_timestamp, # This line inserts the timestamp
      tags$div(class = "title", h1("Global Boys Varsity 8+ Rankings"), align = "center"),
      subheading <- div(class = "subtitle",
                        p("How 197 international rowing teams compare by Rowing Power Index, updated after each regatta.", align = "center")
      ),
      tbl, # Your table goes here
      subheading_text, # Insert the subheading text here
      caption_content # Add the caption container here, which was defined earlier
    )
  )
)


# Save the HTML content to a file
save_html(html_content, file = "index.html")
