## Analyzing match statistics and team performances in 5 seasons (2015/16 to 2019/20) of the Premier League
##
## AUTHOR: TARA NGUYEN
## Part of final project for UCLA Extension course:
## Introduction to Data Science
## Started on December 4th, 2020

########## DATA IMPORT AND CLEANING ##########

library(readr)   ## for function read_csv()
matchstats <- read_csv('matchstats.csv')
matchstats
names(matchstats)
summary(matchstats)
colSums(is.na(matchstats))

## replace NAs

matchstats$scoredfirst[is.na(matchstats$scoredfirst)] <- 'neither'

## turn some variables into factors

matchstats$season <- as.factor(matchstats$season)
matchstats$fulltime_res <- factor(matchstats$fulltime_res,
	levels = c('home_win', 'draw', 'away_win'))
matchstats$scoredfirst <- factor(matchstats$scoredfirst,
	levels = c('home', 'neither', 'away'))
matchstats$halftime_res <- factor(matchstats$halftime_res,
	levels = c('home_lead', 'draw', 'away_lead'))
summary(matchstats)

########## DATA TRANSFORMATION ##########

## full-time and half-time results

ft_res <- c('Home win', 'Draw', 'Away win')
ht_res <- c('Home lead', 'Draw', 'Away lead')

## which team scored first

scfst <- c('Home team', 'Neither', 'Away team')

##### MATCH RECORDS #####

## most home goals, most away goals
sorthomegoals <- matchstats[order(matchstats$homegoals), c(1:4, 5)]
tail(sorthomegoals)
tail(matchstats[order(matchstats$awaygoals), c(1:4, 6)])

## draws with the most goals

tail(sorthomegoals[sorthomegoals$fulltime_res == 'draw', ], 15)

## biggest home wins, biggest away wins

goaldiff <- matchstats$homegoals - matchstats$awaygoals
tail(matchstats[order(goaldiff), c(1:3, 5, 6)])
head(matchstats[order(goaldiff), c(1:3, 5, 6)])

## highest goal difference at half-time

tail(matchstats[order(matchstats$halftime_goaldiff), c(1:4, 9)])

## biggest comebacks after trailing at half-time

comeback <- cbind(matchstats[, c(1:3, 5, 6, 9)], goaldiff)
comeback <- comeback[order(comeback$halftime_goaldiff), ]

home_cb <- subset(comeback, halftime_goaldiff < 0 & goaldiff > 0)
home_cb_goals <- home_cb$goaldiff - home_cb$halftime_goaldiff
tail(home_cb[order(abs(home_cb$halftime_goaldiff), home_cb_goals), ])

away_cb <- subset(comeback, halftime_goaldiff > 0 & goaldiff < 0)
away_cb_goals <- away_cb$halftime_goaldiff - away_cb$goaldiff
tail(away_cb[order(away_cb$halftime_goaldiff, away_cb_goals), ])

## most unbalanced possession

possessdiff <- matchstats$homepossession - matchstats$awaypossession
tail(matchstats[order(abs(possessdiff)), c(1:4, 12, 13)])

## wins with the lowest possession

sorthomepossess <- matchstats[order(matchstats$homepossession), c(1:4, 12)]
head(sorthomepossess[sorthomepossess$fulltime_res == 'home_win', ])
sortawaypossess <- matchstats[order(matchstats$awaypossession), c(1:4, 13)]
head(sortawaypossess[sortawaypossess$fulltime_res == 'away_win', ])

## losses with the highest possession

tail(sorthomepossess[sorthomepossess$fulltime_res == 'away_win', ])
tail(sortawaypossess[sortawaypossess$fulltime_res == 'home_win', ])

## most/least passes by both teams combined

totalpasses <- matchstats$homepasses + matchstats$awaypasses
passes_subset <- cbind(matchstats[, c(1:4, 14, 15)], totalpasses)
tail(passes_subset[order(totalpasses), ])
head(passes_subset[order(totalpasses), ])

## biggest difference in number of accurate passes

homeaccpasses <- matchstats$homepasses * matchstats$homepass_acc
awayaccpasses <- matchstats$awaypasses * matchstats$awaypass_acc
accpassdiff <- abs(homeaccpasses - awayaccpasses)
accpass_subset <- cbind(matchstats[, c(1:4)], homeaccpasses, awayaccpasses,
	accpassdiff)
tail(accpass_subset[order(accpassdiff), ])

## most/least shots by both teams combined

totalshots <- matchstats$homeshots + matchstats$awayshots
shots_subset <- cbind(matchstats[, c(1:4, 18, 19)], totalshots)
tail(shots_subset[order(totalshots), ])
head(shots_subset[order(totalshots), ])

## wins with the least shots on target

homeshots_ot <- matchstats$homeshots * matchstats$homeontarget
homesot_subset <- cbind(matchstats[, 1:4], homeshots_ot)
homesot_subset <- homesot_subset[order(homeshots_ot), ]
head(homesot_subset[homesot_subset$fulltime_res == 'home_win', ])

awayshots_ot <- matchstats$awayshots * matchstats$awayontarget
awaysot_subset <- cbind(matchstats[, 1:4], awayshots_ot)
awaysot_subset <- awaysot_subset[order(awayshots_ot), ]
head(awaysot_subset[awaysot_subset$fulltime_res == 'away_win', ])

## losses with the most shots on target

tail(homesot_subset[homesot_subset$fulltime_res == 'away_win', ])
tail(awaysot_subset[awaysot_subset$fulltime_res == 'home_win', ])

## biggest difference in number of shots on target

sotdiff <- abs(homeshots_ot - awayshots_ot)
sot_subset <- cbind(matchstats[, 1:4], homeshots_ot, awayshots_ot, sotdiff)
tail(sot_subset[order(sot_subset$sotdiff), ])

## goalless draws with the highest number of shots on target

totalsot <- homeshots_ot + awayshots_ot
nogoal_subset <- cbind(matchstats[, c(1:3, 7)], homeshots_ot, awayshots_ot, 
	totalsot)
nogoal_subset <- subset(nogoal_subset, scoredfirst == 'neither',
	select = c(1:3, 5:7))
tail(nogoal_subset[order(nogoal_subset$totalsot), ])

## most/least defensive plays by both teams combined

totaldefense <- matchstats$homedefense + matchstats$awaydefense
defense_subset <- cbind(matchstats[, c(1:4, 24, 25)], totaldefense)
tail(defense_subset[order(totaldefense), ])
head(defense_subset[order(totaldefense), ])

## most/least unsportsmanlike plays by both teams combined

totalbadplays <- matchstats$homebadplays + matchstats$awaybadplays
badplays_subset <- cbind(matchstats[, c(1:4, 26, 27)], totalbadplays)
tail(badplays_subset[order(totalbadplays), ])
head(badplays_subset[order(totalbadplays), ])

##### CONTIGENCY TABLES ACROSS ALL MATCHES #####

## full-time results, half-time results, and which team scored first

(ft_res_tab <- table(matchstats$fulltime_res))
(ht_res_tab <- table(matchstats$halftime_res))
(scfst_tab <- table(matchstats$scoredfirst))

## full-time results by half-time results

(ht_ft_tab <- xtabs(~ halftime_res + fulltime_res, matchstats))
apply(ht_ft_tab, 1, prop.table)   ## proportions

## full-time results by who scored first

(scfst_ft_tab <- xtabs(~ scoredfirst + fulltime_res, matchstats))
apply(scfst_ft_tab, 1, prop.table)   ## proportions

## full-time results by goal difference at half-time, by home goals, and by away goals

(htgd_ft_tab <- xtabs(~ halftime_goaldiff + fulltime_res, matchstats))
(homegoals_ft_tab <- xtabs(~ homegoals + fulltime_res, matchstats))
(awaygoals_ft_tab <- xtabs(~ awaygoals + fulltime_res, matchstats))

## number of unique formations

(n_formations_unique <- 
	length(unique(c(matchstats$homeformation, matchstats$awayformation))))
	## 33

## number of times each formation was used

(homefm_tab <- sort(table(matchstats$homeformation), decreasing = T))
(awayfm_tab <- sort(table(matchstats$awayformation), decreasing = T))

## number of home wins, draws, and away wins grouped by formation
## (only the most used formations included)

ft_form_tab <- xtabs(~ homeformation + fulltime_res + awayformation, 
	matchstats)
(ft_form_tab <- ft_form_tab[names(homefm_tab[1:4]), , 
	names(awayfm_tab[1:4])])
apply(ft_form_tab, c(1, 3), prop.table)   ## proportions

##### PER-SEASON AND PER-MATCH AVERAGES AT TEAM LEVEL #####

n_teams_per_season <- 20

## all teams

(teams <- sort(unique(c(matchstats$hometeam, matchstats$awayteam))))

## abbreviate team names

teams_abbr <- toupper(substr(teams, 1, 3))
teams_abbr[which(teams_abbr == 'MAN')] <- c('MCT', 'MU')
teams_abbr[which(teams_abbr == 'WES')] <- c('WBA', 'WHU')
teams_abbr

## number of unique teams in all seasons

(n_teams_unique <- length(teams))   ## 29

## number of home/away games each team played across all seasons

n_homegames_byteam <- table(matchstats$hometeam)

## number of seasons each team played in

(n_seasons_byteam <- n_homegames_byteam / (n_teams_per_season - 1))

## teams that played in all 5 seasons

n_seasons_byteam[n_seasons_byteam == 5]
sum(n_seasons_byteam == 5)   ## 13

## season-average number of wins/draws/losses

home_res_tab <- xtabs(~ hometeam + fulltime_res, matchstats)
home_res_seasonavg <- home_res_tab / as.vector(n_seasons_byteam)
head(home_res_seasonavg)

away_res_tab <- xtabs(~ awayteam + fulltime_res, matchstats)
away_res_seasonavg <- away_res_tab / as.vector(n_seasons_byteam)
head(away_res_seasonavg)

## season-average number of times each team scored/conceded first, grouped by full-time results

home_sf_res_tab <- xtabs(~ hometeam + scoredfirst + fulltime_res, 
	matchstats)
home_sf_res_seasonavg <- home_sf_res_tab / as.vector(n_seasons_byteam)
head(home_sf_res_seasonavg[, , 2])

away_sf_res_tab <- xtabs(~ awayteam + scoredfirst + fulltime_res, 
	matchstats)
away_sf_res_seasonavg <- away_sf_res_tab / as.vector(n_seasons_byteam)
head(away_sf_res_seasonavg[, , 2])

## season-average number of wins after conceding first

concf_win_seasonavg <- rbind(home_sf_res_seasonavg[, 'away', 'home_win'], 
	away_sf_res_seasonavg[, 'home', 'away_win'])
rownames(concf_win_seasonavg) <- c('home_win', 'away_win')
concf_win_seasonavg

## season-average number of half-time leads/draws/trails, grouped by full-time results

home_ht_ft_tab <- xtabs(~ hometeam + halftime_res + fulltime_res,
	matchstats)
colnames(home_ht_ft_tab) <- c('lead', 'draw', 'trail')
home_ht_ft_seasonavg <- home_ht_ft_tab / as.vector(n_seasons_byteam)
head(home_ht_ft_seasonavg[, , 1])

away_ht_ft_tab <- xtabs(~ awayteam + halftime_res + fulltime_res,
	matchstats)
colnames(away_ht_ft_tab) <- c('trail', 'draw', 'lead')
away_ht_ft_seasonavg <- away_ht_ft_tab / as.vector(n_seasons_byteam)
head(away_ht_ft_seasonavg[, , 1])

## season-average number of wins after trailing at half-time

httrail_win_seasonavg <- rbind(home_ht_ft_seasonavg[, 'trail', 'home_win'],
	away_ht_ft_seasonavg[, 'trail', 'away_win'])
rownames(httrail_win_seasonavg) <- c('home_win', 'away_win')
httrail_win_seasonavg

## season-average number of clean sheets

(cleansh_seasonavg <- (xtabs(~ hometeam + awaygoals, matchstats)[, 1] +
	xtabs(~ awayteam + homegoals, matchstats)[, 1]) / n_seasons_byteam)

## match-average stats

home_matchavg <- aggregate(cbind(homegoals, awaygoals, halftime_goaldiff, 
	homepossession, homepasses, homepass_acc, homeshots, homeontarget, 
	homesaves, homedefense, homebadplays) ~ hometeam, matchstats, mean)
colnames(home_matchavg) <- c('team', 'goalsscored', 'goalsconceded', 
	'ht_goaldiff', 'possession', 'passes', 'pass_acc', 'shots', 'ontarget', 
	'saves', 'defense', 'badplays')

away_matchavg <- aggregate(cbind(homegoals, awaygoals, halftime_goaldiff, 
	awaypossession, awaypasses, awaypass_acc, awayshots, awayontarget, 
	awaysaves, awaydefense, awaybadplays) ~ awayteam, matchstats, mean)
colnames(away_matchavg) <- c('team', 'goalsconceded', 'goalsscored', 
	'ht_goaldiff', 'possession', 'passes', 'pass_acc', 'shots', 'ontarget', 
	'saves', 'defense', 'badplays')

matchavg <- cbind(home_matchavg[, 1:3], away_matchavg[, 3:2], 
	(home_matchavg[, 2] + away_matchavg[, 3]) / 2,
	(home_matchavg[, 3] + away_matchavg[, 2]) / 2)
matchavg <- cbind(matchavg,
	(home_matchavg[, -(1:3)] + away_matchavg[, -(1:3)]) / 2)
colnames(matchavg)
colnames(matchavg)[2:7] <- c('homescored', 'homeconceded', 'awayscored', 
	'awayconceded', 'totalscored', 'totalconceded')
matchavg$ft_goaldiff <- with(matchavg, 
	(homescored + awayscored - homeconceded - awayconceded) / 2)
colnames(matchavg)
matchavg <- matchavg[, c(1:7, 17, 8:16)]
format(head(matchavg), digits = 4)

##### SEASON-END STATS AT TEAM LEVEL #####

seasons <- levels(matchstats$season)

## teams in each season

library(reshape2)   ## for function melt()

teams_byseason <- t(aggregate(hometeam ~ season, matchstats, 
	function(x) sort(unique(x))))
teams_byseason <- teams_byseason[-1, ]
colnames(teams_byseason) <- seasons
teams_byseason <- melt(teams_byseason, varnames = c('', 'season'),
	value.name = 'team')
teams_byseason <- teams_byseason[, -1]
teams_byseason[c(1:5, 21:25), ]

## season-total number of wins/draws/losses

homewin_byseason <- aggregate(fulltime_res ~ hometeam + season, matchstats, 
	function(x) sum(x == 'home_win'))
awaywin_byseason <- aggregate(fulltime_res ~ awayteam + season, matchstats, 
	function(x) sum(x == 'away_win'))
win_byseason <- cbind(teams_byseason,
	homewin_byseason[, 3] + awaywin_byseason[, 3])
colnames(win_byseason)[3] <- 'win'

homedraw_byseason <- aggregate(fulltime_res ~ hometeam + season, matchstats, 
	function(x) sum(x == 'draw'))
awaydraw_byseason <- aggregate(fulltime_res ~ awayteam + season, matchstats, 
	function(x) sum(x == 'draw'))
draw_byseason <- cbind(teams_byseason,
	homedraw_byseason[, 3] + awaydraw_byseason[, 3])
colnames(draw_byseason)[3] <- 'draw'

homeloss_byseason <- aggregate(fulltime_res ~ hometeam + season, matchstats, 
	function(x) sum(x == 'away_win'))
awayloss_byseason <- aggregate(fulltime_res ~ awayteam + season, matchstats, 
	function(x) sum(x == 'home_win'))
loss_byseason <- cbind(teams_byseason,
	homeloss_byseason[, 3] + awayloss_byseason[, 3])
colnames(loss_byseason)[3] <- 'loss'

res_byseason <- cbind(win_byseason, draw_byseason[, 3], loss_byseason[, 3])
colnames(res_byseason)[4:5] <- c('draw', 'loss')
head(res_byseason)

## season-total number of points earned (3 for each win, 1 for each draw)

pts_byseason <- cbind(res_byseason[, 1:2],
	res_byseason$win * 3 + res_byseason$draw)
colnames(pts_byseason)[3] <- 'points'
head(pts_byseason)

## season-total number of goals scored/conceded

homesc_byseason <- aggregate(homegoals ~ hometeam + season, matchstats, sum)
awaysc_byseason <- aggregate(awaygoals ~ awayteam + season, matchstats, sum)
scored_byseason <- cbind(teams_byseason,
	homesc_byseason[, 3] + awaysc_byseason[, 3])
colnames(scored_byseason)[3] <- 'goalsscored'
head(scored_byseason)

homeconc_byseason <- aggregate(awaygoals ~ hometeam + season, matchstats, 
	sum)
awayconc_byseason <- aggregate(homegoals ~ awayteam + season, matchstats, 
	sum)
conceded_byseason <- cbind(teams_byseason,
	homeconc_byseason[, 3] + awayconc_byseason[, 3])
colnames(conceded_byseason)[3] <- 'goalsconceded'
head(conceded_byseason)

## season-total goal difference

goaldiff_byseason <- cbind(teams_byseason,
	scored_byseason[3] - conceded_byseason[3])
colnames(goaldiff_byseason)[3] <- 'goaldiff'
head(goaldiff_byseason)

##### TEAM RANKINGS #####

## rankings at the end of each season

ranktab_byseason <- cbind(res_byseason, pts_byseason[3], scored_byseason[3], 
	conceded_byseason[3])

## season-total goal difference
ranktab_byseason$goaldiff <- 
	ranktab_byseason$goalsscored - ranktab_byseason$goalsconceded

## ranks
ranktab_byseason <- ranktab_byseason[with(ranktab_byseason, 
	order(season, points, goaldiff, goalsscored, goalsconceded, 
	decreasing = c(F, rep(T, 4)))), ]
ranktab_byseason$rank <- rep(1:n_teams_per_season, length(seasons))
ncol <- ncol(ranktab_byseason)
ranktab_byseason <- ranktab_byseason[, c(ncol, 1:(ncol-1))]
head(ranktab_byseason)
tail(ranktab_byseason)

## rankings across all seasons

ranktab_allseasons <- cbind(n_homegames_byteam * 2, 
	aggregate(cbind(win, draw, loss, points, goalsscored, goalsconceded, 
		goaldiff) ~ team, ranktab_byseason, mean)[, -1])
head(ranktab_allseasons)
colnames(ranktab_allseasons)[1:2] <- c('team', 'matches')

## ranks
ranktab_allseasons <- ranktab_allseasons[with(ranktab_allseasons, 
	order(points, goaldiff, goalsscored, decreasing = T)), ]
ranktab_allseasons$rank <- 1:nrow(ranktab_allseasons)
ncol <- ncol(ranktab_allseasons)
ranktab_allseasons <- ranktab_allseasons[, c(ncol, 1:(ncol-1))]
head(ranktab_allseasons)   ## teams with the highest average points/season
tail(ranktab_allseasons)   ## teams with the lowest average points/season

##### TEAM RECORDS #####

## most/least wins/draws/losses in a season

tail(ranktab_byseason[order(ranktab_byseason$win), ])
head(ranktab_byseason[order(ranktab_byseason$win), ])
tail(ranktab_byseason[order(ranktab_byseason$draw), ])
head(ranktab_byseason[order(ranktab_byseason$draw), ])
tail(ranktab_byseason[order(ranktab_byseason$loss), ])
head(ranktab_byseason[order(ranktab_byseason$loss), ])

## most/least total wins/draws/losses per season

tail(ranktab_allseasons[order(ranktab_allseasons$win), ])
head(ranktab_allseasons[order(ranktab_allseasons$win), ])
tail(ranktab_allseasons[order(ranktab_allseasons$draw), ])
head(ranktab_allseasons[order(ranktab_allseasons$draw), ])
tail(ranktab_allseasons[order(ranktab_allseasons$loss), ])
head(ranktab_allseasons[order(ranktab_allseasons$loss), ])

## most/least points in a season

tail(ranktab_byseason[order(ranktab_byseason$points), ])
head(ranktab_byseason[order(ranktab_byseason$points), ])

## most season-average wins after conceding first

tail(sort(home_sf_res_seasonavg[, 'away', 'home_win']))
tail(sort(away_sf_res_seasonavg[, 'home', 'away_win']))

## most season-average losses after scoring first

tail(sort(home_sf_res_seasonavg[, 'home', 'away_win']))
tail(sort(away_sf_res_seasonavg[, 'away', 'home_win']))

## most season-average wins after trailing at half-time

tail(sort(home_ht_ft_seasonavg[, 'trail', 'home_win']))
tail(sort(away_ht_ft_seasonavg[, 'trail', 'away_win']))

## most season-average losses after leading at half-time

tail(sort(home_ht_ft_seasonavg[, 'lead', 'away_win']))
tail(sort(away_ht_ft_seasonavg[, 'lead', 'home_win']))

## most clean sheets per season

tail(sort(cleansh_seasonavg))

## most/least goals scored/conceded in a season

tail(ranktab_byseason[order(ranktab_byseason$goalsscored), ])
head(ranktab_byseason[order(ranktab_byseason$goalsscored), ])
tail(ranktab_byseason[order(ranktab_byseason$goalsconceded), ])
head(ranktab_byseason[order(ranktab_byseason$goalsconceded), ])

## highest/lowest goal difference in a season

tail(ranktab_byseason[order(ranktab_byseason$goaldiff), ])
head(ranktab_byseason[order(ranktab_byseason$goaldiff), ])

########## DATA VISUALIZATION ##########

## function for getting colors for plots
## n: number of colors needed
## i: index of the color palette listed in hcl.pals('qualitative')
## i = 1: "Pastel 1"
## i = 2: "Dark 2"
## i = 3: "Dark 3"
## i = 4: "Set 2"
## i = 5: "Set 3"
## i = 6: "Warm"
## i = 7: "Cold"
## i = 8: "Harmonic"
## i = 9: "Dynamic"
## i > 9: the list of palettes gets recycled
## alpha: color transparency; a single number or a vector of numbers between 0 and 1

getcol <- function(n, i, alpha = NULL) {
	if (i %% 9 != 0) {
		i <- i %% 9
	}
	hcl.colors(n, hcl.pals('qualitative')[i])
}

## colors for plots of home team vs. away team

col_hva <- getcol(3, 9)[c(1, 3)]

##### PLOTS OF CONTIGENCY TABLES #####

## function for saving plots as png files
## name: a descriptive name for the file (without the .png extension)
## w, h: width and height (in pixels) of the image; default to 500 each

saveaspng <- function(name, w = 700, h = 480) {
	filename <- paste0('epldat5seasons/Plots/', name, '.png')
	png(filename, w, h)
}

saveaspng('fulltime-halftime-results')
par(mfrow = c(1, 3))
barplot(prop.table(ft_res_tab), ylim = c(0, .5), main = 'Full-Time Results', 
	names.arg = ft_res, ylab = 'Proportion', col = getcol(3, 9))
barplot(prop.table(ht_res_tab), ylim = c(0, .5), main = 'Half-Time Results', 
	names.arg = ht_res, ylab = 'Proportion', col = getcol(3, 9))
spineplot(ht_ft_tab, main = 'Full-Time Results by Half-Time Results',
	xlab = 'Half-time result', ylab = 'Full-time result',
	xaxlabels = ht_res, yaxlabels = ft_res, col = getcol(3, 9))
par(mfrow = c(1, 1))
dev.off()

saveaspng('fulltime-results-scoredfirst', h = 550)
par(mfrow = c(1, 2))
barplot(scfst_tab, main = 'Which Team Scored First', names.arg = scfst,
	ylab = 'Number of matches', col = getcol(3, 9))
spineplot(scfst_ft_tab, main = 'Full-Time Results by Who Scored First',
	xlab = 'Which team scored first', ylab = 'Full-time result',
	xaxlabels = scfst, yaxlabels = ft_res, col = getcol(3, 9))
par(mfrow = c(1, 1))
dev.off()

saveaspng('fulltime-results-halftime-goaldiff')
spineplot(htgd_ft_tab, col = getcol(3, 9), ylab = 'Full-time result',
	main = 'Full-Time Results by Half-Time Goal Difference',
	xlab = 'Goal difference at half-time', yaxlabels = ft_res)
dev.off()

spineplot(homegoals_ft_tab, col = getcol(3, 9), ylab = 'Full-time result',
	main = 'Full-Time Results by Home Goals', yaxlabels = ft_res,
	xlab = 'Number of goals scored by the home team')

spineplot(awaygoals_ft_tab, col = getcol(3, 9), ylab = 'Full-time result', 
	main = 'Full-Time Results by Away Goals', yaxlabels = ft_res,
	xlab = 'Number of goals scored by the away team')

saveaspng('formations', w = 1000)
par(mfrow = c(1, 4))
for (i in 1:4) {
	spineplot(ft_form_tab[, , i], col = getcol(3, 9), yaxlabels = ft_res,
		xlab = 'Home team formation', ylab = 'Full-time result')
	mtext(paste('Away team formation:', names(awayfm_tab)[i]), cex = .8)
}
par(mfrow = c(1, 1))
## add title
text <- 'Full-Time Results by Combinations of Home Team Formation and Away Team Formation'
mtext(text, line = 2.5, font = 2, cex = 1.2)
dev.off()

##### PLOTS OF NUMERIC VARIABLES #####

xnames <- c('Home team', 'Away team')

saveaspng('numericvars', w = 600)
par(mfrow = c(3, 3))
with(matchstats, boxplot(homegoals, awaygoals, col = col_hva,
	main = 'Home Goals and Away Goals', names = xnames,
	ylab = 'Number of goals', boxwex = .6, medlwd = 2, whisklwd = .5, 
	staplewex = .3, outcex = .5))
with(matchstats, boxplot(homepossession, awaypossession, col = col_hva,
	main = 'Home Possession and\nAway Possession', names = xnames,
	ylab = 'Proportion of possession', boxwex = .6, medlwd = 2, 
	whisklwd = .5, staplewex = .3, outcex = .5))
with(matchstats, boxplot(homepasses, awaypasses, col = col_hva,
	main = 'Home Passes and Away Passes', names = xnames,
	ylab = 'Number of passes', boxwex = .6, medlwd = 2, whisklwd = .5, 
	staplewex = .3, outcex = .5))
with(matchstats, boxplot(homepass_acc, awaypass_acc, col = col_hva,
	main = 'Passing Accuracy', names = xnames, boxwex = .6, medlwd = 2, 
	whisklwd = .5, staplewex = .3, outcex = .5))
with(matchstats, boxplot(homeshots, awayshots, col = col_hva,
	main = 'Home Shots and Away Shots', names = xnames,
	ylab = 'Number of shots', boxwex = .6, medlwd = 2, whisklwd = .5, 
	staplewex = .3, outcex = .5))
with(matchstats, boxplot(homeontarget, awayontarget, col = col_hva,
	main = 'Proportion of Shots on Target', names = xnames, boxwex = .6, 
	medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5))
with(matchstats, boxplot(homesaves, awaysaves, col = col_hva,
	main = 'Proportion of Shots on Target\nSaved by the Goalkeeper',
	names = xnames, boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, 
	outcex = .5))
with(matchstats, boxplot(homedefense, awaydefense, col = col_hva,
	main = 'Number of defensive Plays', names = xnames, boxwex = .6, 
	medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5))
with(matchstats, boxplot(homebadplays, awaybadplays, col = col_hva,
	main = 'Number of Unsportsmanlike Plays', names = xnames, boxwex = .6, 
	medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5))
par(mfrow = c(1, 1))
dev.off()

## visualization of correlation matrix

library(corrplot)   ## for function corrplot()

saveaspng('correlation-matrix', w = 480)
corrplot(cor(matchstats[, c(5:6, 9, 12:27)]), method = 'color', 
	title = 'Correlation Matrix', type = 'upper', diag = F, 
	mar = c(0, 0, 4, 0), tl.col = 1, cl.pos = 'b', cl.ratio = .1)
dev.off()

## pairwise scatterplot matrix, color-coded by full-time results

saveaspng('scatterplot-matrix')
plot(matchstats[, c(5:6, 9, 12, 20:23)], col = matchstats$fulltime_res, 
	main = 'Pairwise Scatterplot Matrix', pch = 20, cex = .8,
	lower.panel = NULL)
## add legend
legend('left', ft_res, title = 'Full-time result', col = 1:3, pch = 20, 
	cex = .8, inset = .1)
dev.off()

##### PLOTS OF PER-SEASON AND PER-MATCH AVERAGES #####

saveaspng('team-seasonavg-win-draw-loss', w = 1680)
spineplot(res_tab_byteam[, c('wintotal', 'drawtotal', 'losstotal')],
	main = paste('Season-Average Numbers of Wins, Draws, and Losses,',
	'Grouped by Team'), xlab = 'Team', ylab = 'Full-time result',
	xaxlabels = teams_abbr, yaxlabels = c('Win', 'Draw', 'Loss'),
	col = getcol(3, 9))
dev.off()

saveaspng('team-seasonavg-win-concfirst', w = 1280)
barplot(concf_win_seasonavg, col = getcol(2, 9), names.arg = teams_abbr,
	main = paste('Season-Average Number of Times A Team Won',
	'After Conceding First'), ylim = c(0, max(concf_win_seasonavg) * 1.1), 
	ylab = 'Number of matches', xlab = 'Team',
	legend.text = c('Home win', 'Away win'), args.legend = list(
	x = 'topleft', inset = c(.15, .05), horiz = T))
dev.off()

saveaspng('team-seasonavg-win-trailatht', w = 1280)
barplot(httrail_win_seasonavg, col = getcol(2, 9), names.arg = teams_abbr,
	main = paste('Season-Average Number of Times A Team Won',
	'After Trailing at Half-Time'), ylab = 'Number of matches',
	xlab = 'Team', legend.text = c('Home win', 'Away win'), 
	args.legend = list(x = 'top', inset = .1, horiz = T))
dev.off()

## PLOTS OF NUMERIC VARIABLES

saveaspng('team-matchavg-goals-homevsaway', w = 600)
par(mfrow = c(1, 2))

with(matchavg, boxplot(homescored, awayscored, col = col_hva,
	main = 'Average Number of Goals Scored\nat Home and Away Per Match', 
	cex.axis = .8, names = c('Home', 'Away'), ylab = 'Number of goals', 
	boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5))
## add mean points
points(colMeans(matchavg[, -1])[c('homescored', 'awayscored')], pch = 23,
	bg = 'red')

with(matchavg, boxplot(homeconceded, awayconceded, col = col_hva,
	main = 'Average Number of Goals Conceded\nat Home and Away Per Match', 
	cex.axis = .8, names = c('Home', 'Away'), ylab = 'Number of goals', 
	boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5))
## add mean points
points(colMeans(matchavg[, -1])[c('homeconceded', 'awayconceded')], 
	pch = 23, bg = 'red')

par(mfrow = c(1, 1))

## add explanation
mtext('The red diamond shapes denote the mean points', side = 1, line = 3.5)
dev.off()

par(mfrow = c(2, 4))
boxplot(matchavg$possession, main = 'Proportion of Ball Possession',
	col = 3, boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, 
	outcex = .5)
boxplot(matchavg$passes, main = 'Number of Passes', col = 3, 
	boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5)
boxplot(matchavg$pass_acc, main = 'Passing Accuracy', col = 3, 
	boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5)
boxplot(matchavg$shots, main = 'Number of Shots', col = 3, 
	boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5)
boxplot(matchavg$ontarget, main = 'Proportion of Shots on Target', 
	col = 3, boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, 
	outcex = .5)
boxplot(matchavg$ontarget, col = 3,
	main = 'Proportion of Shots on Target\nSaved by the Goalkeeper', 
	boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5)
boxplot(matchavg$saves, main = 'Number of Defensive Plays', col = 3, 
	boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5)
boxplot(matchavg$badplays, main = 'Number of Unsportsmanlike Plays', 
	col = 3, boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, 
	outcex = .5)
par(mfrow = c(1, 1))

saveaspng('team-matchavg-possession', 1280)
barplot(matchavg$possession, col = getcol(n_teams_unique, 1),
	main = 'Average Proportion of Ball Possession Per Match', ylab = 'Team',
	names.arg = teams_abbr, ylim = c(0, max(matchavg$possession) * 1.1))
dev.off()

saveaspng('team-matchavg-passes', 1280)
barplot(matchavg$passes, col = getcol(n_teams_unique, 1),
	main = 'Average Number of Passes Per Match', ylab = 'Team',
	names.arg = teams_abbr, ylim = c(0, max(matchavg$passes) * 1.1))
dev.off()

saveaspng('team-matchavg-pass_acc', 1280)
barplot(matchavg$pass_acc, col = getcol(n_teams_unique, 1),
	main = 'Average Passing Accuracy Per Match', ylab = 'Team',
	names.arg = teams_abbr, ylim = c(0, max(matchavg$pass_acc) * 1.1))
dev.off()

saveaspng('team-matchavg-shots', 1280)
barplot(matchavg$shots, col = getcol(n_teams_unique, 1),
	main = 'Average Number of Shots Per Match', ylab = 'Team',
	names.arg = teams_abbr, ylim = c(0, max(matchavg$shots) * 1.1))
dev.off()

saveaspng('team-matchavg-ontarget', 1280)
barplot(matchavg$ontarget, col = getcol(n_teams_unique, 1),
	main = 'Average Proportion of Shots on Target Per Match', ylab = 'Team', 
	names.arg = teams_abbr, ylim = c(0, max(matchavg$ontarget) * 1.1))
dev.off()

saveaspng('team-matchavg-saves', 1280)
barplot(matchavg$saves, col = getcol(n_teams_unique, 1),
	main = paste('Average Proportion of Shots on Target Saved by',
	'the Goalkeeper Per Match'), ylab = 'Team', names.arg = teams_abbr, 
	ylim = c(0, max(matchavg$saves) * 1.1))
dev.off()

saveaspng('team-matchavg-defense', 1280)
barplot(matchavg$defense, col = getcol(n_teams_unique, 1),
	main = 'Average Number of Defensive Plays Per Match', ylab = 'Team', 
	names.arg = teams_abbr, ylim = c(0, max(matchavg$defense) * 1.1))
dev.off()

saveaspng('team-matchavg-badplays', 1280)
barplot(matchavg$badplays, col = getcol(n_teams_unique, 1), ylab = 'Team',
	main = 'Average Number of Unsportsmanlike Plays Per Match',
	names.arg = teams_abbr, ylim = c(0, max(matchavg$badplays) * 1.1))
dev.off()

########## STATISTICAL ANALYSES ##########

##### CLASSIFICATION OF FULL-TIME RESULTS USING RANDOM FORESTS #####

## split data into training set (70%) and test set (30%)
## select only variables that are going to be in the model

nrow <- nrow(matchstats)
ntrain <- round(nrow * .7)
set.seed(264)   ## for reproducible results
index <- sample(nrow, ntrain)
(trainset <- matchstats[index, c(4:7, 9, 12, 20:23)])
(testset <- matchstats[-index, c(4:7, 9, 12, 20:23)])

library(randomForest)   ## to use random forest algorith

(rf <- randomForest(fulltime_res ~ ., trainset, importance = T))
## Number of trees: 500
## No. of variables tried at each split: 3
## OOB estimate of error rate: .45%

## plot error rates vs. number of trees

saveaspng('randomforest-error-vs-ntree')
plot(rf, main = paste('Error Rates as a Function of the Number of Trees',
	'in the Random Forest'))
## add legend
legend('topright', c('Out-of-bag sample', 'Home-win class', 'Draw class', 
	'Away-win class'), lty = 1:4, col = 1:4)
dev.off()

## use model rf to predict full-time results in the test set

pred <- predict(rf, testset)

## confusion matrix of predicted values and actual values in test set

table(pred, testset$fulltime_res)

## number of misclassifcations and error rate in the test set

(n_misclass <- sum(testset$fulltime_res != pred))   ## 2
(mean(testset$fulltime_res != pred) * 100)   ## .35%

## fine-tune the model by varying the number of variables tried at each split

testset_err_rate_new <- c()
oob_error <- c()
for (i in 1:(ncol(trainset) - 1)) {
	rf_new <- randomForest(fulltime_res ~ ., trainset, mtry = i,
		importance = T)
	
	## new predicted values for the test set
	pred_new <- predict(rf_new, testset)
	
	## new misclassification rate in the test set
	testset_err_rate_new[i] <- mean(testset$fulltime_res != pred_new)

	## new out-of-bag error estimate
	oob_error[i] <- rf_new$err.rate[rf_new$ntree, 1]
}

## plot new error rates vs. number of variables tried at each split

saveaspng('randomforest-finetune')
par(mfrow = c(1, 2))
plot(testset_err_rate_new, type = 'b', col = 3, 
	xlab = 'Number of variables', ylab = 'Error rate in the test set')
plot(oob_error, type = 'b', col = 4, lty = 2, pch = 20, 
	xlab = 'Number of variables', ylab = 'Out-of-bag error estimate')
## add title
title <- paste('Error Rates in the Test Set and Out-of-Bag Error Estimates',
	'as a Function of\nthe Number of Variables Tried at Each Split')
par(mfrow = c(1, 1))
mtext(title, line = 1, font = 2, cex = 1.2)
dev.off()

## importance matrix for the original model (rf)

importance <- importance(rf)
importance[order(importance[, 4], importance[, 5], decreasing = T), ]

## variable importance plot

saveaspng('randomforest-varimportance')
varImpPlot(rf,
	main = 'Variable Importance in the Random Forest')
dev.off()

## how many times each variable was used in the random forest

use_counts <- varUsed(rf)
names(use_counts) <- colnames(trainset)[-1]
sort(use_counts, decreasing = T)

## partial dependence plots for the three most important variables

## get the coordinates to find out the limits on the axes
pdp <- vector('list', 5)
pdp[[1]] <- partialPlot(rf, as.data.frame(trainset), homegoals, 'home_win', 
	plot = F)
pdp[[2]] <- partialPlot(rf, as.data.frame(trainset), homegoals, 'draw',
	plot = F)
pdp[[3]] <- partialPlot(rf, as.data.frame(trainset), homegoals, 'away_win', 
	plot = F)
pdp[[4]] <- partialPlot(rf, as.data.frame(trainset), awaygoals, 'home_win', 
	plot = F)
pdp[[5]] <- partialPlot(rf, as.data.frame(trainset), awaygoals, 'away_win', 
	plot = F)

xlim <- range(sapply(pdp, function(x) range(x$x)))
ylim <- range(sapply(pdp, function(x) range(x$y))) * c(1.1, 1.5)

## draw the plots

saveaspng('randomforest-partialdependence-numeric')
plot(pdp[[1]], type = 'b', col = 3, lty = 2, xlim = xlim, ylim = ylim,
	main = paste('Partial Dependence of Full-Time Results on the Numbers',
		'of Home Goals and Away Goals'),
	xlab = 'Number of goals', ylab = 'Partial dependence')
lines(pdp[[2]], type = 'b', col = 3, lty = 3)
lines(pdp[[3]], type = 'b', col = 3, lty = 4)
lines(pdp[[4]], type = 'b', col = 4, lty = 2, pch = 0)
lines(pdp[[5]], type = 'b', col = 4, lty = 4, pch = 0)
## add legends
legend('topleft', c('Home win', 'Draw', 'Away win'), col = 3, lty = 2:4,
	pch = 1, title = 'Dependence on home goals', box.lwd = .5)
legend('topright', c('Home win', 'Away win'), col = 4, lty = c(2, 4),
	pch = 0, title = 'Dependence on away goals', box.lwd = .5)
## add horizontal line at y = 0
abline(h = 0, col = 'red', lty = 5, lwd = .5)
dev.off()

saveaspng('randomforest-partialdependence-scfst')
classes <- levels(trainset$fulltime_res)
par(mfrow = c(1, 3))
for (i in seq_along(classes)) {
	pdp_sf <- partialPlot(rf, as.data.frame(trainset), scoredfirst,
		classes[i], plot = F)
	barplot(pdp_sf$y, col = getcol(3, 9)[i], names.arg = pdp_sf$x,
		ylab = 'Partial dependence', legend.text = ft_res[i],
		args.legend = list(x = i %% 3 + 2))
}
par(mfrow = c(1, 1))
## add title and label
mtext('Partial Dependence of Full-Time Results on Which Team Scored First',
	line = 2.5, font = 2, adj = .5, cex = 1.2)
mtext('Which team scored first', side = 1, line = 3.5, cex = .8)
dev.off()