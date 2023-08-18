# NHL Team and Player Valuation
# Laken Rivet

# load necessary libraries
library(tidyverse)
library(ggrepel)
library(rvest)
library(reshape2)
library(openxlsx)
library(lpSolve)

# turn off scientific notation for larger numbers
options(scipen = 999)

# PART ONE - RELATIONSHIP BETWEEN SPENDING AND WINS

# read in cash payroll csv (from https://www.spotrac.com/nhl/cash/)
cashpayroll <- read.csv(file = "raw_data/NHL_Cash_Payroll.csv", header = TRUE)

# read in salary cap payroll csv (from https://www.spotrac.com/nhl/cap/)
cappayroll <- read.csv(file = "raw_data/NHL_Cap_Space.csv", header = TRUE)

# join data sets together 
nhl_financials <- full_join(x = cappayroll, y = cashpayroll, by = c("Team", "Year"))

# split record into wins, losses, OT losses, and points
nhl_financials <- nhl_financials %>%
  separate_wider_regex(Record, patterns = c(W = "[[:digit:]]+", "-", L = "[[:digit:]]+", "-", OT_L = "[[:digit:]]+", " \\(", P = "[[:digit:]]+", "\\)"))

# convert W, L, OT_L, and P into numeric
nhl_financials$W <- as.numeric(nhl_financials$W)
nhl_financials$L <- as.numeric(nhl_financials$L)
nhl_financials$OT_L <- as.numeric(nhl_financials$OT_L)
nhl_financials$P <- as.numeric(nhl_financials$P)

# create total cap and total cash variables in millions
nhl_financials$Total_Cap_M <- nhl_financials$Total_Cap / 1000000
nhl_financials$Total_Cash_M <- nhl_financials$Total_Cash / 1000000

# write out financials df into csv for use elsewhere
write.csv(nhl_financials, "processed_data/nhl_financials.csv")

# check to see what's the most correlated - 
# salary cap or cash payroll
# wins or points
cor.test(nhl_financials$W, nhl_financials$Total_Cap)
cor.test(nhl_financials$W, nhl_financials$Total_Cash)
cor.test(nhl_financials$P, nhl_financials$Total_Cap)
cor.test(nhl_financials$P, nhl_financials$Total_Cash)

# cash payroll and wins have the highest correlation (0.57) 

# calculate rank correlation coefficient (checking for non-linear,
# monotone relationship)
cor.test(nhl_financials$W, nhl_financials$Total_Cash, method = "spearman", exact = FALSE)

# evaluate linear nature of relationship with regression
lin <- lm(W ~ Total_Cash_M, data = nhl_financials)
summary(lin)

# evaluate quadratic relationship of model
quad <- lm(W ~ Total_Cash_M + I(Total_Cash_M^2), data = nhl_financials)
summary(quad)

# create list of red wings teams to highlight in graphic
highlight <- nhl_financials %>%
  filter(Team == "Detroit Red Wings")

# create visualization showing relationship between wins and cash payroll
wins_cash <- ggplot(nhl_financials, aes(x = Total_Cash_M, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +
  labs(title = "Cash Payroll vs Wins", x = "Cash Payroll (millions)", y = "Wins") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
  geom_point(data = highlight, color = "red") +
  geom_label_repel(data = highlight, color = "red", aes(label = paste(Team, Year)))

# display visualization
wins_cash

# save visualization
ggsave(file = "output/cash_wins.pdf")

# comparing Red Wings spending with other organizations
# calculating percent changes in cash payroll and salary cap year to year
# for whole league
yearly_change <- nhl_financials %>%
  group_by(Year) %>%
  summarise(mean_cash = mean(Total_Cash_M),
            mean_cap = mean(Total_Cap_M)) %>%
  mutate(cash_pct_change = (mean_cash /lag(mean_cash) - 1) * 100) %>%
  mutate(cap_pct_change = (mean_cap /lag(mean_cap) - 1) * 100)

yearly_change

# calculating percent changes in cash payroll and salary cap year to year
# for Red Wings
highlight <- highlight %>%
  arrange(P) %>%
  mutate(cash_pct_change = (Total_Cash_M /lag(Total_Cash_M) - 1) * 100) %>%
  mutate(cap_pct_change = (Total_Cap_M /lag(Total_Cap_M) - 1) * 100)

highlight %>% 
  select(Year, Total_Cash_M, Total_Cap_M, cash_pct_change, cap_pct_change) %>%
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Total')))

# PART TWO - CURRENT AND FUTURE VALUATION

# create list of teams to iterate through website urls
list_of_teams <- c("new-york-rangers", 
                   "toronto-maple-leafs",
                   "montreal-canadiens",
                   "chicago-blackhawks",
                   "boston-bruins",
                   "los-angeles-kings",
                   "edmonton-oilers",
                   "philadelphia-flyers",
                   "washington-capitals",
                   "seattle-kraken",
                   "detroit-red-wings",
                   "new-york-islanders",
                   "vancouver-canucks",
                   "tampa-bay-lightning",
                   "pittsburgh-penguins",
                   "vegas-golden-knights",
                   "new-jersey-devils",
                   "dallas-stars",
                   "st-louis-blues",
                   "colorado-avalanche",
                   "calgary-flames",
                   "minnesota-wild",
                   "nashville-predators",
                   "ottawa-senators",
                   "san-jose-sharks",
                   "anaheim-ducks",
                   "winnipeg-jets",
                   "carolina-hurricanes",
                   "columbus-blue-jackets",
                   "buffalo-sabres",
                   "florida-panthers",
                   "arizona-coyotes")

# create unique url for each team
target_pages = paste0('https://www.forbes.com/teams/', list_of_teams)

# create data frame to be appended in for loop
team_worth = data.frame()

# create for loop to read and scrape unique url for each team then append
# to data frame with all team information (forbes.com)
for (i in target_pages) {
  # reading url
  url <- read_html(i)
  # scraping team name
  team <- url %>%
    html_nodes("h1") %>%
    html_text()
  # scraping values
  values <- url %>% 
    html_nodes(".profile-row__value-text") %>% 
    html_text
  # scraping names of values
  value_columns <- url %>% 
    html_nodes(".profile-row__type-title") %>% 
    html_text()
  # reformatting names of values to be column names
  value_columns <- gsub(" ", "_", value_columns)
  # create new row with values and team name to add to df
  new_row <- c(values, team[1])
  # add new row to df
  team_worth <- rbind(team_worth, new_row)
  # add "Team" to list of scraped column names
  value_columns <- append(value_columns, "Team")
  # naming data frame columns with scraped column names
  colnames(team_worth) = c(value_columns)
}

# write for loop to convert team value to numeric values, then create new
# column that puts all team values in millions
for (i in 1:nrow(team_worth)) {
  if (grepl("M", team_worth$Team_Value[i]) == FALSE) {
    team_worth$Team_Value_M[i] <- parse_number(team_worth$Team_Value[i]) * 1000
    team_worth$Team_Value[i] <- parse_number(team_worth$Team_Value[i]) * 1000000000
  }
  else if (grepl("M", team_worth$Team_Value[i]) == TRUE) {
    team_worth$Team_Value_M[i] <- parse_number(team_worth$Team_Value[i])
    team_worth$Team_Value[i] <- parse_number(team_worth$Team_Value[i]) * 1000000
  }
  else {
    team_worth$Team_Value[i] <- "Error"
  }
}

# convert team value columns into numerics
team_worth$Team_Value <- as.numeric(team_worth$Team_Value)
team_worth$Team_Value_M <- as.numeric(team_worth$Team_Value_M)

# leafs and blue jackets have two entries for year purchased and price paid
# reducing those entries to the most recent
team_worth$Year_Purchased[team_worth$Team == "#29 Columbus Blue Jackets"] <- "2012"
team_worth$Price_Paid[team_worth$Team == "#29 Columbus Blue Jackets"] <- "$173M"
team_worth$Year_Purchased[team_worth$Team == "#2 Toronto Maple Leafs"] <- "2012"
team_worth$Price_Paid[team_worth$Team == "#2 Toronto Maple Leafs"] <- "$1000M"

# split team column into team and rank
team_worth <- team_worth %>%
  mutate(colsplit(team_worth$Team," ",c("Rank","Team")))

# remove # sign from rank
team_worth$Rank <- parse_number(team_worth$Rank)

# clean operating income variable
team_worth <- team_worth %>%
  mutate(Operating_Income = parse_number(Operating_Income)) %>%
  rename(Operating_Income_M = Operating_Income)

# clean price paid variable
team_worth <- team_worth %>%
  mutate(Price_Paid = parse_number(Price_Paid)) %>%
  rename(Price_Paid_M = Price_Paid)

# clean revenue variable
team_worth <- team_worth %>%
  mutate(Revenue = parse_number(Revenue)) %>%
  rename(Revenue_M = Revenue)

# clean player expenses variable
team_worth <- team_worth %>%
  mutate(Player_Expenses = parse_number(Player_Expenses)) %>%
  rename(Player_Expenses_M = Player_Expenses)

# clean gate receipts variable
team_worth <- team_worth %>%
  mutate(Gate_Receipts = parse_number(Gate_Receipts)) %>%
  rename(Gate_Receipts_M = Gate_Receipts)

# clean metro area population variable
team_worth <- team_worth %>%
  mutate(Metro_Area_Population = parse_number(Metro_Area_Population)) %>%
  rename(Metro_Area_Population_M = Metro_Area_Population)

# convert championships, year purchased, and wins to player cost ratio to numerics
team_worth$Championships <- as.numeric(team_worth$Championships)
team_worth$Year_Purchased <- as.numeric(team_worth$Year_Purchased)
team_worth$`Wins-to-player_cost_ratio` <- as.numeric(team_worth$`Wins-to-player_cost_ratio`)

# fix debt/value ratio column
team_worth <- team_worth %>%
  mutate(`Debt/Value` = parse_number(`Debt/Value`) / 100)
  
# fix revenue per fan column
team_worth <- team_worth %>%
  mutate(Revenue_per_Fan = parse_number(Revenue_per_Fan)) %>%
  rename(Revenue_per_Fan_dollars = Revenue_per_Fan)

# remove white spaces from start of owners
team_worth <- team_worth %>%
  mutate(`Owner(s)` = str_trim(`Owner(s)`))

# write cleaned dataframe to csv for usage later
write.csv(team_worth, "processed_data/team_valuations_2022.csv")

# filter only red wings team worth data to highlight in visualization
redwings <- team_worth %>%
  filter(Team == "Detroit Red Wings")

# create plot that shows team value versus operating income across the league
value_op_income <- ggplot(team_worth, aes(x = Team_Value_M, y = Operating_Income_M)) +
  geom_point() +
  geom_hline(yintercept = mean(team_worth$Operating_Income_M), color = "blue") +
  geom_vline(xintercept = mean(team_worth$Team_Value_M), color = "blue") +
  labs(title = "Team Value vs Operating Income (millions)", x = "Team Value", y = "Operating Income") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) +
  geom_point(data = redwings, color = "red") +
  geom_label_repel(data = redwings, color = "red", aes(label = paste(Team)))

# display visualization
value_op_income

# save visualization
ggsave(file = "output/value_op_income.pdf")

# what team's value has increased the most since purchase?
team_worth %>%
  select(c("Team", "Price_Paid_M", "Team_Value_M")) %>%
  mutate(New = Team_Value_M - Price_Paid_M) %>%
  arrange(New)

# create cash-flow analysis for 1.5B sales price, no cost control
outcomea <- data.frame(index_year = c(0,1,2,3,4,5,5),
                       season_event = c("Purchase","22-23","23-24","24-25","25-26","26-27","Sale"),
                       cash_in_M = c(0, 200, 215, 230, 245, 260, 1500),
                       cash_out_M = c(1030, 260, 270, 280, 290, 300, 0))

outcomea <- outcomea %>%
  mutate(cash_in_out_M = cash_in_M - cash_out_M,
         discount_in_out_M = cash_in_out_M/(1+0.08)^index_year)

# create cash-flow analysis for 1.5B sales price, with cost control
outcomeb <- data.frame(index_year = c(0,1,2,3,4,5,5),
                       season_event = c("Purchase","22-23","23-24","24-25","25-26","26-27","Sale"),
                       cash_in_M = c(0, 200, 215, 230, 245, 260, 1500),
                       cash_out_M = c(1030, 225, 225, 225, 225, 225, 0))

outcomeb <- outcomeb %>%
  mutate(cash_in_out_M = cash_in_M - cash_out_M,
         discount_in_out_M = cash_in_out_M/(1+0.08)^index_year)

# create cash-flow analysis for 2B sales price, with cost control
outcomec <- data.frame(index_year = c(0,1,2,3,4,5,5),
                       season_event = c("Purchase","22-23","23-24","24-25","25-26","26-27","Sale"),
                       cash_in_M = c(0, 200, 215, 230, 245, 260, 2000),
                       cash_out_M = c(1030, 225, 225, 225, 225, 225, 0))

outcomec <- outcomec %>%
  mutate(cash_in_out_M = cash_in_M - cash_out_M,
         discount_in_out_M = cash_in_out_M/(1+0.08)^index_year)

# write different outcomes to different sheets in excel file for report
dataset_names <- list('A' = outcomea, 'B' = outcomeb, 'C' = outcomec)
write.xlsx(dataset_names, file = "output/discounted_cash_flow.xlsx")

# PART THREE - PLAYER CONTRIBUTIONS AND VALUE

# grabbing player performance and salary data for all active NHL players
# from capfriendly.com

# identify target pages
target_pages <- c("https://www.capfriendly.com/browse/active/2023?stats-season=2023")

# because website only shows 50 players at a time, have to iterate through 32
# different webpages starting at 2
target_pages <- append(target_pages, paste0("https://www.capfriendly.com/browse/active/2023?stats-season=2023&pg=", seq(2,32,1)))

# create data frame to append with player information
active_players <- data.frame()

# write another for loop to crawl through pages and scrape player data
for (i in target_pages) {
  # reading url
  url <- read_html(i)
  # scraping column names
  vars <- url %>%
    html_nodes("th") %>%
    html_text
  # scraping values
  values <- url %>%
    html_nodes("td") %>%
    html_text
  # create for loop that splits values into individual player columns
  for (i in seq(1,51,1)) {
    # divide values into list containing 23 entries (a single player's stats)
    player <- split(values, ceiling(seq_along(values)/23))
    # grab each player's stats and put into new row
    new_row <- t(data.frame(player[i]))
    # append new row to active players data frame
    active_players <- rbind(active_players, new_row)
  }
}

# name columns of active player data frame
colnames(active_players) = c(vars)

# filter goalies from active players due to different stats
goalies <- active_players %>%
  filter(TOI == "-") %>%
  # remove unrelated stat columns (for skaters only)
  select(-c(seq(7,14,1))) %>%
  # clean name column
  separate(PLAYER, c("X", "PLAYER"), "\\d\\. ") %>%
  select(-c(1)) %>%
  # drop players with no games played this season
  filter(GP != "0")

# columns to convert to numeric in goalies
colstoconvert <- c(3, seq(6,11,1), 14, 15)

# convert specified columns to numeric
goalies[ , colstoconvert] <- apply(goalies[ ,colstoconvert], 2,
                                   function(x) parse_number(x))

# filter skaters from active players due to different stats
skaters <- active_players %>%
  filter(W == "-") %>%
  # remove unrelated stat columns (for goalies only)
  select(-c(seq(15,19,1))) %>%
  # clean name column
  separate(PLAYER, c("X", "PLAYER"), "\\d\\. ") %>%
  select(-c(1)) %>%
  # drop players with no time on ice recorded
  filter(TOI != "") %>%
  # separate time on ice into minutes and seconds
  separate(TOI, c("MINUTES", "SECONDS"), ":")

# columns to convert to numeric in skaters
colstoconvert <- c(3, seq(6,15,1), 18, 19)

# convert specified columns to numeric
skaters[ , colstoconvert] <- apply(skaters[ ,colstoconvert], 2,
                                   function(x) parse_number(x))

# convert minutes and seconds back to time on ice
skaters <- skaters %>%
  # calculate toi in seconds
  mutate(TOI_S = (MINUTES*60) + SECONDS) %>%
  # calculate toi in minutes
  mutate(TOI_M = TOI_S / 60) %>%
  # drop minutes and seconds column
  select(-c(14,15))

# create subset of skaters of defense men
dmen <- skaters[grepl("D", skaters$POS), ]
# create subset of skaters of forwards
fwds <- skaters[!grepl("D", skaters$POS), ]

# write active player data to different sheets in excel file for use elsewhere
dataset_names <- list('Forwards' = fwds, 'Defensemen' = dmen, 'Goalies' = goalies)
write.xlsx(dataset_names, file = "processed_data/active_players.xlsx")

# correlations for forwards
cor.test(fwds$P, fwds$SALARY)
cor.test(fwds$P, fwds$`CAP HIT`)
cor.test(fwds$`P/GP`, fwds$SALARY)
cor.test(fwds$`P/GP`, fwds$`CAP HIT`)

# correlations for defense men
cor.test(dmen$P, dmen$SALARY)
cor.test(dmen$P, dmen$`CAP HIT`)
cor.test(dmen$`P/GP`, dmen$SALARY)
cor.test(dmen$`P/GP`, dmen$`CAP HIT`)

# correlations for goalies
cor.test(goalies$GAA, goalies$SALARY)
cor.test(goalies$GAA, goalies$`CAP HIT`)
cor.test(goalies$`Sv%`, goalies$SALARY)
cor.test(goalies$`Sv%`, goalies$`CAP HIT`)

# revise goalies for more than 10 games played
go2 <- goalies[goalies$GP > 10, ]

# new correlations for goalies
cor.test(go2$GAA, go2$SALARY)
cor.test(go2$GAA, go2$`CAP HIT`)
cor.test(go2$`Sv%`, go2$SALARY)
cor.test(go2$`Sv%`, go2$`CAP HIT`)

# create new dfs that highlight wings fwds, dmen, and goalies
detdmen <- dmen[dmen$TEAM == "DET", ]
detfwds <- fwds[fwds$TEAM == "DET", ]
detgoals <- goalies[goalies$TEAM == "DET", ]

# create plot that shows P/GP and cap hit across the league
fwd_pgp_caphit <- ggplot(fwds, aes(x = `CAP HIT` / 1000000, y = `P/GP`)) +
  geom_point() +
  geom_hline(yintercept = mean(fwds$`P/GP`), color = "blue") +
  geom_vline(xintercept = mean((fwds$`CAP HIT`)/1000000), color = "blue") +
  labs(title = "Points/Games Played vs Cap Hit - Forwards", x = "Cap Hit (millions)", y = "Points/Games Played") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) +
  geom_point(data = detfwds, color = "red") +
  geom_label_repel(data = detfwds[detfwds$`P/GP` < mean(fwds$`P/GP`) & detfwds$`CAP HIT` > mean(fwds$`CAP HIT`), ],
                   color = "red", aes(label = paste(PLAYER)))

# display visualization
fwd_pgp_caphit

# save visualization
ggsave(filename = "output/fwd_pgp_caphit.pdf")

# create plot that shows P/GP and cap hit across the league
def_pgp_caphit <- ggplot(dmen, aes(x = `CAP HIT` / 1000000, y = `P/GP`)) +
  geom_point() +
  geom_hline(yintercept = mean(dmen$`P/GP`), color = "blue") +
  geom_vline(xintercept = mean((dmen$`CAP HIT`)/1000000), color = "blue") +
  labs(title = "Points/Games Played vs Cap Hit - Defensemen", x = "Cap Hit (millions)", y = "Points/Games Played") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) +
  geom_point(data = detdmen, color = "red") +
  geom_label_repel(data = detdmen[detdmen$`P/GP` < mean(dmen$`P/GP`) & detdmen$`CAP HIT` > mean(dmen$`CAP HIT`), ],
                   color = "red", aes(label = paste(PLAYER)))

# display visualization
def_pgp_caphit

# save visualization
ggsave(filename = "output/def_pgp_caphit.pdf")

# create plot that shows P/GP and cap hit across the league
goal_gaa_salary <- ggplot(goalies, aes(x = SALARY / 1000000, y = GAA)) +
  geom_point() +
  geom_hline(yintercept = mean(goalies$GAA), color = "blue") +
  geom_vline(xintercept = mean((goalies$SALARY)/1000000), color = "blue") +
  labs(title = "Salary vs Goals Against Average", x = "Salary (millions)", y = "Goals Against Average") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) +
  geom_point(data = detgoals, color = "red") +
  geom_label_repel(data = detgoals, color = "red", aes(label = paste(PLAYER)))

# display visualization
goal_gaa_salary

# save visualization
ggsave(filename = "output/goal_gaa_salary.pdf")

# group by teams to look at average P/GP and and cap hit for forwards
team_fwds <- aggregate(cbind(fwds$`P/GP`, fwds$`CAP HIT`) ~ fwds$TEAM, FUN = mean)
team_fwds <- team_fwds %>%
  rename(TEAM = `fwds$TEAM`, P_GP = V1, CAP_HIT = V2) %>%
  filter(TEAM != "-")

# create plot that shows TEAM P/GP and cap hit across the league
team_fwd_pgp_caphit <- ggplot(team_fwds, aes(x = CAP_HIT / 1000000, y = P_GP)) +
  geom_point() +
  geom_hline(yintercept = mean(team_fwds$P_GP), color = "blue") +
  geom_vline(xintercept = mean((team_fwds$CAP_HIT)/1000000), color = "blue") +
  labs(title = "Avg. Points/Games Played vs Avg. Cap Hit - Forwards", x = "Avg. Cap Hit (millions)", y = "Avg. Points/Games Played") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) +
  geom_point(data = team_fwds[team_fwds$TEAM == "DET", ], color = "red") +
  geom_label_repel(data = team_fwds[team_fwds$TEAM == "DET", ],
                   color = "red", aes(label = paste("Detroit Red Wings")))

# display visualization
team_fwd_pgp_caphit

# save visualization
ggsave(filename = "output/team_fwd_pgp_caphit.pdf")

# group by teams to look at average P/GP and and cap hit for dmen
team_dmen <- aggregate(cbind(dmen$`P/GP`, dmen$`CAP HIT`) ~ dmen$TEAM, FUN = mean)
team_dmen <- team_dmen %>%
  rename(TEAM = `dmen$TEAM`, P_GP = V1, CAP_HIT = V2) %>%
  filter(TEAM != "-")

# create plot that shows TEAM P/GP and cap hit across the league
team_dmen_pgp_caphit <- ggplot(team_dmen, aes(x = CAP_HIT / 1000000, y = P_GP)) +
  geom_point() +
  geom_hline(yintercept = mean(team_dmen$P_GP), color = "blue") +
  geom_vline(xintercept = mean((team_dmen$CAP_HIT)/1000000), color = "blue") +
  labs(title = "Avg. Points/Games Played vs Avg. Cap Hit - Defensemen", x = "Avg. Cap Hit (millions)", y = "Avg. Points/Games Played") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) +
  geom_point(data = team_dmen[team_dmen$TEAM == "DET", ], color = "red") +
  geom_label_repel(data = team_dmen[team_dmen$TEAM == "DET", ],
                   color = "red", aes(label = paste("Detroit Red Wings")))

# display visualization
team_dmen_pgp_caphit

# save visualization
ggsave(filename = "output/team_dmen_pgp_caphit.pdf")

# group by teams to look at average P/GP and and cap hit for forwards
team_goalies <- aggregate(cbind(goalies$GAA, goalies$SALARY) ~ goalies$TEAM, FUN = mean)
team_goalies <- team_goalies %>%
  rename(TEAM = `goalies$TEAM`, GAA = V1, SALARY = V2) %>%
  filter(TEAM != "-")

# create plot that shows P/GP and cap hit across the league
team_goal_gaa_salary <- ggplot(team_goalies, aes(x = SALARY / 1000000, y = GAA)) +
  geom_point() +
  geom_hline(yintercept = mean(team_goalies$GAA), color = "blue") +
  geom_vline(xintercept = mean((team_goalies$SALARY)/1000000), color = "blue") +
  labs(title = "Avg. Salary vs Avg. Goals Against Average", x = "Avg. Salary (millions)", y = "Avg. Goals Against Average") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.25)) +
  geom_point(data = team_goalies[team_goalies$TEAM == "DET", ], color = "red") +
  geom_label_repel(data = team_goalies[team_goalies$TEAM == "DET", ], color = "red", aes(label = paste("Detroit Red Wings")))

# display visualization
team_goal_gaa_salary

# save visualization
ggsave(filename = "output/team_goal_gaa_salary.pdf")

# create detroit roster table and write to excel file for use elsewhere
dataset_names <- list('Forwards' = detfwds, 'Defensemen' = detdmen, 'Goalies' = detgoals)
write.xlsx(dataset_names, file = "processed_data/det_players.xlsx")

# PART FOUR - OPTIMIZING THE ROSTER

# web pages to identify free agents next season
target_pages <- c("https://www.capfriendly.com/browse/free-agents/2024/caphit/all/all/ufa?stats-season=2023")

# because website only shows 50 players at a time, have to iterate through 7
# different webpages starting at 2
target_pages <- append(target_pages, paste0("https://www.capfriendly.com/browse/free-agents/2024/caphit/all/all/ufa?stats-season=2023&pg=", seq(2,7,1)))

# create list to append with player names
optimize_players <- list()

# write another for loop to crawl through pages and scrape player free agent names
for (i in target_pages) {
  # reading url
  url <- read_html(i)
  # scraping player names only
  player_name <- url %>%
    html_nodes(".left") %>%
    html_text
  # for loop to go through names and split into individual entries in list
  for (i in seq(1, length(player_name), 1)) {
    # divide values into list containing 23 entries (a single player's stats)
    player <- split(player_name, ceiling(seq_along(player_name)/1))
    optimize_players[[length(optimize_players) + 1]] <- player[[i]]
  }
}

# take out of list format for use
optimize_players <- unlist(optimize_players)

# clean player names
optimize_players <- str_split_i(optimize_players, "\\d\\. ", 2)

# cross reference free agents with active players, that have nhl records
# for last season
activefreeagents <- fwds[fwds$PLAYER %in% c(optimize_players), ] 
activefreeagents <- rbind(activefreeagents, dmen[dmen$PLAYER %in% c(optimize_players), ] )

activefreeagents <- activefreeagents %>%
  mutate(Defense = case_when(grepl("D", POS) ~ 1, !grepl("D", POS) ~ 0)) %>%
  mutate(Forward = case_when(grepl("D", POS) ~ 0, !grepl("D", POS) ~ 1)) %>%
  mutate(PlayerID = seq(1, nrow(activefreeagents), 1)) %>%
  filter(GP >= 41 & CLAUSE == "")

opt_prob <- activefreeagents[, c("PLAYER", "PlayerID", "P/GP", "CAP HIT", "Defense", "Forward")]

# set up arguments needed for solving the knapsack problem
# objective is to maximize the points per game played
# objective = opt_prob$P/GP
# direction = "max"

# one dmen only, one fwd only, and 8 million cap hit
# const.rhs = c(1, 1, 1, 1, 8)  
# const.dir = c("<=", ">=", "<=", ">=", "<=")  

# this is a knapsack problem so we have binary integers only
# int.vec = 1:188
# all.bin = TRUE  

# set up the constraint matrix using data for dmen, fwds, and salaries
constraint_matrix <- as.matrix(rbind(opt_prob$Defense,
                                     opt_prob$Defense,
                                     opt_prob$Forward,
                                     opt_prob$Forward,
                                     opt_prob$`CAP HIT`))
dimnames(constraint_matrix) <- 
  list(c("OneDmenMax",
         "OneDmenMin",
         "OneFwdMax",
         "OneFwdMin",
         "CapHitMax"),
       opt_prob$PlayerID)

# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix,
     objective = opt_prob$`P/GP`,
     direction = "max",
     const.rhs = c(1, 1, 1, 1, 8000000),
     const.dir = c("<=", ">=", "<=", ">=", "<=")  ,
     int.vec = 1:188, all.bin = TRUE)

# examine the structure of resulting list object 
cat("\n\nStructure of Mathematical Programming Object\n") 
print(str(knapsack_object))

# show the solution
cat("\n\nBest Set of Free Agents\n")
print(opt_prob[as.logical(knapsack_object$solution),])

# show the points per period maximum
cat("\n\nMaximum Points per Games Played:", knapsack_object$objval, "\n")

# compare old and suggested new players
comparefwds <- fwds %>% 
  filter(PLAYER ==  "Pius Suter" | PLAYER == "Max Domi")
comparedmen <- dmen %>%
  filter(PLAYER == "Ben Chiarot" | PLAYER == "Erik Gustafsson")
compare <- rbind(comparedmen, comparefwds)

# write to excel file to make a table for report
write.xlsx(compare, file = "output/compare_players.xlsx")

