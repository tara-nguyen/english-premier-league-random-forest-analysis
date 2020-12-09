## Scraping match statistics in 5 seasons (2015/16 to 2019/20) of the Premier League directly from webpages
##
## AUTHOR: TARA NGUYEN
## Part of final project for UCLA Extension course:
## Introduction to Data Science
## Completed in December 2020

n_teams_per_season <- 20
n_games_per_season <- n_teams_per_season * (n_teams_per_season - 1)

## list of urls

parent_url <- 'https://fbref.com'
fixturelist_urlmiddle1 <- '/en/comps/9'
fixturelist_urlmiddle2 <- c('/1467/schedule/2015-2016',
	'/1526/schedule/2016-2017', '/1631/schedule/2017-2018',
	'/1889/schedule/2018-2019', '/3232/schedule/2019-2020')
fixturelist_urlend <- '-Premier-League-Scores-and-Fixtures'
fixturelist_urlfull <- paste0(parent_url, fixturelist_urlmiddle1,
	fixturelist_urlmiddle2, fixturelist_urlend)

## list of seasons

seasons <- unlist(regmatches(fixturelist_urlmiddle2, 
	regexec('20(.{7})$', fixturelist_urlmiddle2)))
seasons <- seasons[seq(1, length(seasons), 2)]
seasons <- gsub('-20', '/', seasons)
n_seasons <- length(seasons)

## read webpages for fixture list

readcon <- vector('list', n_seasons)
for (i in 1:n_seasons) {
	con <- url(fixturelist_urlfull[i], 'r')
	readcon[[i]] <- readLines(con)
	close(con)
}

## function for obtaining match statistics from webpages

getmatchstats <- function(i, j, dat) {
	## get url link for match report
	
	match_url <- unlist(regmatches(readcon[[i]], 
		regexec('/en/matches/(.{8})/(.+?)Premier-League', readcon[[i]])))
	match_url <- grep('/', match_url, value = T)[j]
	
	## read webpage for match report and get match stats
	
	con <- url(paste0(parent_url, match_url), 'r')
	readcon <- readLines(con)
	close(con)
	
	rowindex <- n_games_per_season * (i-1) + j   ## row in data frame
	
	## teams
	
	title <- grep('<title>', readcon, value = T)
	title_split1 <- unlist(strsplit(title, '>'))[2]
	title_split2 <- unlist(strsplit(title_split1, ' vs. '))
	teams <- unlist(strsplit(title_split2, ' Match'))[1:2]
	dat$hometeam[rowindex] <- teams[1]
	dat$awayteam[rowindex] <- teams[2]
	
	## result, number of home goals, and number of away goals
	## at full time
	
	score <- grep('color:#777', readcon, value = T)
	score <- unlist(regmatches(score, regexec('[0-9]:[0-9]', score)))
	ftscore <- score[length(score)]
	ftscore <- as.numeric(unlist(strsplit(ftscore, ':')))
	dat$fulltime_res[rowindex] <- 
		ifelse(ftscore[1] > ftscore[2], 'home_win',
		ifelse(ftscore[1] == ftscore[2], 'draw', 'away_win'))
	dat$homegoals[rowindex] <- ftscore[1]
	dat$awaygoals[rowindex] <- ftscore[2]
	
	## which team scored first
	
	if (sum(ftscore) > 0) {
		scoredfirst <- unlist(regmatches(score, regexec('1:0|0:1', score)))
		scoredfirst <- as.numeric(strsplit(scoredfirst, ':')[[1]])
		scoredfirst <- which(scoredfirst == 1)
		if (scoredfirst == 1) {
			dat$scoredfirst[rowindex] <- 'home'
		} else {
			dat$scoredfirst[rowindex] <- 'away'
		}
	}
	
	## result and goal difference at half-time
	
	minute <- grep('&rsquor;$', readcon, value = T)
	minute <- unlist(regmatches(minute, regexec('[0-9]{1,2}', minute)))
	if (sum(minute < 46) > 0) {
		htscore <- unlist(strsplit(score[max(which(minute < 46))], ':'))
		htscore <- as.numeric(htscore)
	} else {
		htscore <- c(0, 0)
	}
	dat$halftime_res[rowindex] <- 
		ifelse(htscore[1] > htscore[2], 'home_lead',
		ifelse(htscore[1] == htscore[2], 'draw', 'away_lead'))
	dat$halftime_goaldiff[rowindex] <- htscore[1] - htscore[2]
	
	## team formation
	
	formation <- unlist(regmatches(readcon, 
		regexec('[(]([0-9]-){1,}[0-9](.+?)[)]', readcon)))[c(1, 4)]
	formation <- unlist(strsplit(formation, '[()]'))[c(2, 4)]
	dat$homeformation[rowindex] <- formation[1]
	dat$awayformation[rowindex] <- formation[2]
	
	## possession proportion
	
	possession <- readcon[grep('<th colspan="2">Possession', readcon):
		(grep('<th colspan="2">Passing', readcon)-1)]
	possession <- grep('<strong>', possession, value = T)
	possession <- unlist(regmatches(possession, 
		regexec('([0-9]{1,3})%', possession)))[c(2, 4)]
	possession <- as.numeric(possession)
	dat$homepossession[rowindex] <- possession[1] / 100
	dat$awaypossession[rowindex] <- possession[2] / 100
	
	## number of passes and passing accuracy
	
	passing <- readcon[grep('<th colspan="2">Passing', readcon):
		(grep('<th colspan="2">Shots on Target', readcon)-1)]
	passing <- unlist(regmatches(passing, 
		regexec('[0-9]{1,} of [0-9]{1,}', passing)))
	passing <- as.numeric(unlist(strsplit(passing, ' of ')))
	dat$homepasses[rowindex] <- passing[2]
	if (passing[2] > 0) {
		dat$homepass_acc[rowindex] <- passing[1] / passing[2]
	} else {
		dat$homepass_acc[rowindex] <- 0
	}
	dat$awaypasses[rowindex] <- passing[4]
	if (passing[4] > 0) {
		dat$awaypass_acc[rowindex] <- passing[3] / passing[4]
	} else {
		dat$awaypass_acc[rowindex] <- 0
	}
	
	## number of shots and proportion of shots on target
	
	shots <- readcon[grep('<th colspan="2">Shots on Target', readcon):
		(grep('<th colspan="2">Saves', readcon)-1)]
	shots <- unlist(regmatches(shots, 
		regexec('[0-9]{1,} of [0-9]{1,}', shots)))
	shots <- as.numeric(unlist(strsplit(shots, ' of ')))
	dat$homeshots[rowindex] <- shots[2]
	if (shots[2] > 0) {
		dat$homeontarget[rowindex] <- shots[1] / shots[2]
	} else {
		dat$homeontarget[rowindex] <- 0
	}
	dat$awayshots[rowindex] <- shots[4]
	if (shots[4] > 0) {
		dat$awayontarget[rowindex] <- shots[3] / shots[4]
	} else {
		dat$awayontarget[rowindex] <- 0
	}
	
	## proportion of shots on target saved by the goalkeeper
	
	saves <- readcon[grep('<th colspan="2">Saves', readcon):
		(grep('<th colspan="2">Cards', readcon)-1)]
	saves <- unlist(regmatches(saves, 
		regexec('[0-9]{1,} of [0-9]{1,}', saves)))
	saves <- as.numeric(unlist(strsplit(saves, ' of '))[c(1, 3)])
	if (shots[3] > 0) {
		dat$homesaves[rowindex] <- saves[1] / shots[3]
	} else {
		dat$homesaves[rowindex] <- 0
	}
	if (shots[1] > 0) {
		dat$awaysaves[rowindex] <- saves[2] / shots[1]
	} else {
		dat$awaysaves[rowindex] <- 0
	}
	
	## numbers of tackles, interceptions, and clearances
	
	tackles <- unlist(regmatches(readcon, 
		regexec('[0-9]{1,}(.+?)Tackles\\1[0-9]{1,}', readcon)))
	tackles <- unlist(strsplit(tackles, 
		'</div><div>Tackles</div><div>'))[1:2]
	tackles <- as.numeric(tackles)
	
	interceptions <- unlist(regmatches(readcon, 
		regexec('[0-9]{1,}(.+?)Interceptions\\1[0-9]{1,}', readcon)))
	interceptions <- unlist(strsplit(interceptions, 
		'</div><div>Interceptions</div><div>'))[1:2]
	interceptions <- as.numeric(interceptions)
	
	clearances <- unlist(regmatches(readcon, 
		regexec('[0-9]{1,}(.+?)Clearances\\1[0-9]{1,}', readcon)))
	clearances <- unlist(strsplit(clearances, 
		'</div><div>Clearances</div><div>'))[1:2]
	clearances <- as.numeric(clearances)
	
	## defense is defined as the sum of the numbers of tackles,
	## interceptions, and clearances
	
	dat$homedefense[rowindex] <- tackles[1] + interceptions[1] +
		clearances[1]
	dat$awaydefense[rowindex] <- tackles[2] + interceptions[2] +
		clearances[2]
	
	## number of fouls
	
	fouls <- unlist(regmatches(readcon, 
		regexec('[0-9]{1,}(.+?)Fouls\\1[0-9]{1,}', readcon)))
	fouls <- unlist(strsplit(fouls, '</div><div>Fouls</div><div>'))[1:2]
	fouls <- as.numeric(fouls)
	
	## numbers of cards (i.e., bookings for fouls)
		
	cards <- grep('class="cards"', readcon, value = T)
	yellowcards <- strsplit(cards, 'yellow')
	yellowcards <- sapply(yellowcards, function(x) length(x) - 1)
	doubleyellowcards <- strsplit(cards, 'yellow_red')
	doubleyellowcards <- sapply(doubleyellowcards, 
		function(x) length(x) - 1)
	redcards <- strsplit(cards, 'class="red')
	redcards <- sapply(redcards, function(x) length(x) - 1)
	
	## badplays is defined by the following formula:
	## fouls + yellowcards + doubleyellowcards * 3 + redcards * 3
	
	dat$homebadplays[rowindex] <- fouls[1] + yellowcards[1] +
		doubleyellowcards[1] * 3 + redcards[1] * 3
	dat$awaybadplays[rowindex] <- fouls[2] + yellowcards[2] +
		doubleyellowcards[2] * 3 + redcards[2] * 3

	return(dat)
}

## data frame of match stats

matchstats <- data.frame(season = rep(seasons, each = n_games_per_season),
	hometeam = NA, awayteam = NA,
	fulltime_res = NA, 
	homegoals = NA, awaygoals = NA, scoredfirst = NA,
	halftime_res = NA, halftime_goaldiff = NA,
	homeformation = NA, awayformation = NA,
	homepossession = NA, awaypossession = NA,
	homepasses = NA, awaypasses = NA,
	homepass_acc = NA, awaypass_acc = NA,
	homeshots = NA, awayshots = NA,
	homeontarget = NA, awayontarget = NA,
	homesaves = NA, awaysaves = NA,
	homedefense = NA, awaydefense = NA,
	homebadplays = NA, awaybadplays = NA)
nrow(matchstats) == n_games_per_season * n_seasons   ## TRUE

# ## either read all webpages at once

# for (i in seq_along(fixturelist_urlfull)) {
	# for (j in 1:n_games_per_season) {
		# matchstats <- getmatchstats(i, j, matchstats)
	# }
# }

## or read a few webpages at a time to avoid overworking the computer

i <- 1; j1 <- 1; incre <- 19   ## run this line only once

## keep running this for loop until matchstats has been filled out completely
## (i.e., until i == length(fixturelist_urlfull) & j == n_games_per_season)
for (j in j1:(j1+incre)) {
	matchstats <- getmatchstats(i, j, matchstats)
	if (j == j1 + incre) {
		j1 <- j1 + incre + 1
		if (j1 > n_games_per_season) {
			j1 <- 1; i <- i + 1
		}
	}
}
i; j; j1

matchstats2 <- matchstats   ## save a copy just in case something went wrong
## if something did go wrong, revert matchstats to before the last for loop
# matchstats <- matchstats2

## fix weird characters

matchstats$homeformation <- gsub('&#9670;', '-diamond', 
	matchstats$homeformation)
matchstats$awayformation <- gsub('&#9670;', '-diamond', 
	matchstats$awayformation)

## check for missing values
## there are supposed to be NAs in the scoredfirst column for games without goals

for (col in 1:ncol(matchstats)) {
	is_na <- is.na(matchstats[, col])
	if (sum(is_na) > 0) {
		print(colnames(matchstats)[col])   ## "scoredfirst"
		print(matchstats[which(is_na), c('homegoals', 'awaygoals')])
	}
}

## write to csv file

write.csv(matchstats, row.names = F, file = 'matchstats.csv')