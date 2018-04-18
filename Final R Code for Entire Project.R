################################
## WRITING PROJECT FINAL CODE ##
################################

#########################################################################
### Scraping Function - Largely From Carson Sievert's Scrape Function ###
#########################################################################

collapse_obs2 <- function(x) {
  val <- collapse_obs(x)
  if (is.list(val)) {
    return(val)
  } else {
    li <- list(val)
    names(li) <- unique(names(x))
    return(li)
  }
}

merged <- function(x, y, ...){
  dat <- merge(x=x, y=y, sort=FALSE, ...)
  dat[] <- lapply(dat, function(x) as.character(x))
  return(dat)
}

# Take a matrix and turn into data frame and turn relevant columns into numerics
format.table <- function(dat, name) {
  nums <- NULL
  switch(name,
         game = nums <- c("venue_id", "scheduled_innings", "away_team_id", "away_league_id", "home_team_id",
                          "home_league_id", "away_games_back_wildcard", "away_win",  "away_loss", "home_win",
                          "home_loss", "inning", "outs", "away_team_runs","home_team_runs", "away_team_hits",
                          "home_team_hits", "away_team_errors", "home_team_errors"),
         player = nums <- c("id", "num", "avg", "hr", "rbi", "bat_order", "wins", "losses", "era"),
         coach = nums <- c("id", "num"),
         umpire = nums <- "id",
         hip = nums <- c("x", "y", "batter", "pitcher", "inning"),
         action = nums <- c("b", "s", "o", "player", "pitch", "inning"),
         atbat = nums <- c("pitcher", "batter", "num", "b", "s", "o", "inning"),
         pitch = nums <- c("id", "x", "y", "start_speed", "end_speed", "sz_top", "sz_bot", "pfx_x", "pfx_z", "px",
                           "pz", "x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az", "type_confidence",
                           "zone", "nasty", "spin_dir", "spin_rate", "inning", "num", "on_1b", "on_2b", "on_3b"),
         po = nums <- c("inning", "num"),
         runner = nums <- c("id", "inning", "num"))
  #For some reason, records are sometimes duplicated, remove them!
  dat <- data.frame(dat[!duplicated(dat),], stringsAsFactors=FALSE)
  nms <- names(dat)
  numz <- nums[nums %in% nms] #error handling (just in case one of the columns doesn't exist)
  for (i in numz) dat[, i] <- suppressWarnings(as.numeric(dat[, i]))
  if ("game" %in% name) {
    dat$url_scoreboard <- dat$url
    dat$url <- paste0(gsub("miniscoreboard.xml", "", dat$url), "gid_", dat$gameday_link, "/inning/inning_all.xml")
    # These fields only show up for suspended games...I don't think they're worth tracking...
    dat <- dat[, !names(dat) %in% c("runner_on_base_status", "runner_on_1b")]
  } else { #create a 'gameday_link' column for easier linking of tables
    if (length(grep("^url$", names(dat)))) dat$gameday_link <- sub("/.*", "", sub(".*gid", "gid", dat$url))
  }
  return(dat)
}

# Add columns with relevant pitch count to the 'pitch' table.
# @param dat 'pitch' matrix/df
# @return returns the original matrix/df with the proper pitch count column appended.
appendPitchCount <- function(dat) {
  if (any(!c("type", "gameday_link", "num") %in% colnames(dat))){
    warning("Count column couldn't be created")
    return(dat)
  }
  balls <- as.numeric(dat[,"type"] %in% "B")
  strikes <- as.numeric(dat[,"type"] %in% "S")
  pre.idx <- paste(dat[,"gameday_link"], dat[,"num"])
  idx <- factor(pre.idx, levels=unique(pre.idx))
  cum.balls <- unlist(tapply(balls, INDEX=idx, function(x){ n <- length(x); pmin(cumsum(c(0, x[-n])), 3) }))
  cum.strikes <- unlist(tapply(strikes, INDEX=idx, function(x) { n <- length(x); pmin(cumsum(c(0, x[-n])), 2) }))
  count <- paste(cum.balls, cum.strikes, sep = "-")
  return(cbind(dat, count))
}

# Add columns with relevant pitch count to the 'pitch' table.
# @param dat 'pitch' matrix/df
# @return returns the original matrix/df with the proper pitch count column appended.
appendDate <- function(dat) {
  if (!"gameday_link" %in% colnames(dat)){
    warning("'date' column couldn't be created")
    return(dat)
  }
  return(cbind(dat, date = substr(dat[,"gameday_link"], 5, 14)))
}

enhanced.scrape <- function (start, end, game.ids, suffix = "inning/inning_all.xml", 
                             connect, ...) 
{
  if (!missing(connect)) {
    if (!requireNamespace("DBI")) 
      warning("You will need the DBI package to write tables to a database.")
    fieldz <- plyr::try_default(DBI::dbListFields(connect, 
                                                  "atbat"), NULL, quiet = TRUE)
    if (!"date" %in% fieldz && !is.null(fieldz)) {
      msg <- "An 'atbat' table without the 'date' column was detected\n"
      if (!requireNamespace("dplyr") || packageVersion("dplyr") < 
          0.2) {
        message(msg, "To automatically append 'date', please install/update the dplyr and DBI packages \n", 
                "More details are discussed here -- \n", "http://baseballwithr.wordpress.com/2014/04/13/modifying-and-querying-a-pitchfx-database-with-dplyr/")
      }
      else {
        message(msg, "A 'date' column will now be appended. Please be patient.")
        new.col <- if ("SQLiteConnection" %in% class(connect)) {
          if ("gameday_link" %in% fieldz) 
            "SUBSTR(gameday_link, 15, -10)"
          else "SUBSTR(url, 80, -10)"
        }
        else {
          if ("gameday_link" %in% fieldz) 
            "SUBSTR(gameday_link, 5, 10)"
          else "SUBSTR(url, 70, 10)"
        }
        res <- DBI::dbSendQuery(connect, paste("CREATE TABLE atbat_temp AS SELECT *,", 
                                               new.col, "AS date FROM atbat"))
        DBI::dbRemoveTable(connect, name = "atbat")
        DBI::dbSendQuery(connect, "ALTER TABLE atbat_temp RENAME TO atbat")
      }
    }
  }
  message("If file names don't print right away, please be patient.")
  valid.suffix <- c("inning/inning_all.xml", "inning/inning_hit.xml", 
                    "miniscoreboard.xml", "players.xml")
  if (!all(suffix %in% valid.suffix)) {
    warning("Currently supported file suffix are: 'inning/inning_all.xml', 'inning/inning_hit.xml', 'miniscoreboard.xml', and 'players.xml'")
    Sys.sleep(5)
  }
  if (missing(game.ids)) {
    gameDir <- makeUrls(start = start, end = end)
  }
  else {
    if (!all(grepl("gid_", game.ids))) 
      warning("Any Game IDs supplied to the gids option should be of the form gid_YYYY_MM_DD_xxxmlb_zzzmlb_1")
    gameDir <- makeUrls(gids = game.ids)
  }
  fields = NULL
  env2 <- environment()
  data(fields, package = "pitchRx", envir = env2)
  if (any(grepl("miniscoreboard.xml", suffix))) {
    dayDir <- unique(gsub("/gid_.*", "", gameDir))
    scoreboards <- paste0(dayDir, "/miniscoreboard.xml")
    obs <- XML2Obs(scoreboards, as.equiv = TRUE, url.map = FALSE, 
                   ...)
    illegal <- paste0("games//game//", c("review", "home_probable_pitcher", 
                                         "away_probable_pitcher"))
    obs <- obs[!names(obs) %in% illegal]
    nms <- names(obs)
    nms <- gsub("^games//game//game_media//media$", "media", 
                nms)
    nms <- gsub("^games//game$", "game", nms)
    obs <- setNames(obs, nms)
    game.idx <- grep("^games$", nms)
    if (length(game.idx) > 0) 
      obs <- obs[-game.idx]
    tables <- collapse_obs2(obs)
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], 
                                                         name = i)
    if (!missing(connect)) {
      for (i in names(tables)) export(connect, name = i, 
                                      value = tables[[i]], template = fields[[i]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc()
    }
  }
  if (any(grepl("players.xml", suffix))) {
    player.files <- paste0(gameDir, "/players.xml")
    obs <- XML2Obs(player.files, as.equiv = TRUE, url.map = FALSE, 
                   ...)
    obs <- add_key(obs, parent = "game//team", recycle = "id", 
                   key.name = "name_abbrev", quiet = TRUE)
    obs <- add_key(obs, parent = "game//team", recycle = "type", 
                   quiet = TRUE)
    obs <- add_key(obs, parent = "game//team", recycle = "name", 
                   quiet = TRUE)
    nms <- names(obs)
    nms <- gsub("^game//team//player$", "player", nms)
    nms <- gsub("^game//team//coach$", "coach", nms)
    nms <- gsub("^game//umpires//umpire$", "umpire", nms)
    obs <- setNames(obs, nms)
    game.idx <- grep("game", nms)
    if (length(game.idx) > 0) 
      obs <- obs[-game.idx]
    if (exists("tables")) {
      tables <- c(tables, collapse_obs2(obs))
    }
    else {
      tables <- collapse_obs2(obs)
    }
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], 
                                                         name = i)
    if (!missing(connect)) {
      for (i in names(tables)) export(connect, name = i, 
                                      value = tables[[i]], template = fields[[i]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc()
    }
  }
  if (any(grepl("inning/inning_hit.xml", suffix))) {
    inning.files <- paste0(gameDir, "/inning/inning_hit.xml")
    obs <- XML2Obs(inning.files, as.equiv = TRUE, url.map = FALSE, 
                   ...)
    if (exists("tables")) {
      tables <- c(tables, collapse_obs2(obs))
    }
    else {
      tables <- collapse_obs2(obs)
    }
    names(tables) <- sub("^hitchart//hip$", "hip", names(tables))
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], 
                                                         name = i)
    if (!missing(connect)) {
      for (i in names(tables)) export(connect, name = i, 
                                      value = tables[[i]], template = fields[[i]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc()
    }
  }
  if (any(grepl("inning/inning_all.xml", suffix))) {
    inning.files <- paste0(gameDir, "/inning/inning_all.xml")
    n.files <- length(inning.files)
    cap <- min(200, n.files)
    if (n.files > cap && missing(connect)) {
      warning("play-by-play data for just the first 200 games will be returned (even though you've asked for", 
              n.files, ")", "If you want/need more, please consider using the 'connect' argument.")
    }
    n.loops <- ceiling(n.files/cap)
    for (i in seq_len(n.loops)) {
      inning.filez <- inning.files[seq(1, cap) + (i - 1) * 
                                     cap]
      inning.filez <- inning.filez[!is.na(inning.filez)]
      obs <- XML2Obs(inning.filez, as.equiv = TRUE, url.map = FALSE, 
                     ...)
      obs <- re_name(obs, equiv = c("game//inning//top//atbat//pitch", 
                                    "game//inning//bottom//atbat//pitch"), diff.name = "inning_side", 
                     quiet = TRUE)
      obs <- re_name(obs, equiv = c("game//inning//top//atbat//runner", 
                                    "game//inning//bottom//atbat//runner"), diff.name = "inning_side", 
                     quiet = TRUE)
      obs <- re_name(obs, equiv = c("game//inning//top//atbat//po", 
                                    "game//inning//bottom//atbat//po"), diff.name = "inning_side", 
                     quiet = TRUE)
      obs <- re_name(obs, equiv = c("game//inning//top//atbat", 
                                    "game//inning//bottom//atbat"), diff.name = "inning_side", 
                     quiet = TRUE)
      obs <- re_name(obs, equiv = c("game//inning//top//action", 
                                    "game//inning//bottom//action"), diff.name = "inning_side", 
                     quiet = TRUE)
      obs <- add_key(obs, parent = "game//inning", recycle = "num", 
                     key.name = "inning", quiet = TRUE)
      obs <- add_key(obs, parent = "game//inning", recycle = "next", 
                     key.name = "next_", quiet = TRUE)
      names(obs) <- sub("^game//inning//action$", "game//inning//atbat//action", 
                        names(obs))
      obs <- add_key(obs, parent = "game//inning//atbat", 
                     recycle = "num", quiet = TRUE)
      obs <- add_key(obs, parent = "game//inning//atbat", recycle = "pitcher", quiet = T)
      obs <- add_key(obs, parent = "game//inning//atbat", recycle = "batter", quiet = T)
      obs <- add_key(obs, parent = "game//inning//atbat", recycle = "stand", quiet = T)
      obs <- add_key(obs, parent = "game//inning//atbat", recycle = "b_height", quiet = T)
      obs <- add_key(obs, parent = "game//inning//atbat", recycle = "p_throws", quiet = T)
      nms <- names(obs)
      rm.idx <- c(grep("^game$", nms), grep("^game//inning$", 
                                            nms))
      if (length(rm.idx) > 0) 
        obs <- obs[-rm.idx]
      if (exists("tables")) {
        tables <- c(tables, collapse_obs2(obs))
      }
      else {
        tables <- collapse_obs2(obs)
      }
      rm(obs)
      gc()
      tab.nms <- names(tables)
      tab.nms <- sub("^game//inning//atbat$", "atbat", 
                     tab.nms)
      tab.nms <- sub("^game//inning//atbat//action$", "action", 
                     tab.nms)
      tab.nms <- sub("^game//inning//atbat//po$", "po", 
                     tab.nms)
      tab.nms <- sub("^game//inning//atbat//runner$", "runner", 
                     tab.nms)
      tab.nms <- sub("^game//inning//atbat//pitch$", "pitch", 
                     tab.nms)
      tables <- setNames(tables, tab.nms)
      scrape.env <- environment()
      data(players, package = "pitchRx", envir = scrape.env)
      players$id <- as.character(players$id)
      colnames(tables[["atbat"]]) <- sub("^batter$", "id", 
                                         colnames(tables[["atbat"]]))
      tables[["atbat"]] <- merged(x = tables[["atbat"]], 
                                  y = players, by = "id", all.x = TRUE)
      colnames(tables[["atbat"]]) <- sub("^id$", "batter", 
                                         colnames(tables[["atbat"]]))
      colnames(tables[["atbat"]]) <- sub("^full_name$", 
                                         "batter_name", colnames(tables[["atbat"]]))
      colnames(tables[["atbat"]]) <- sub("^pitcher$", "id", 
                                         colnames(tables[["atbat"]]))
      tables[["atbat"]] <- merged(x = tables[["atbat"]], 
                                  y = players, by = "id", all.x = TRUE)
      colnames(tables[["atbat"]]) <- sub("^id$", "pitcher", 
                                         colnames(tables[["atbat"]]))
      colnames(tables[["atbat"]]) <- sub("^full_name$", 
                                         "pitcher_name", colnames(tables[["atbat"]]))
      colnames(tables[["atbat"]]) <- sub("^des", "atbat_des", 
                                         colnames(tables[["atbat"]]))
      for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], 
                                                           name = i)
      tables[["pitch"]] <- appendPitchCount(tables[["pitch"]])
      tables[["atbat"]] <- appendDate(tables[["atbat"]])
      if (!missing(connect)) {
        for (i in names(tables)) export(connect, name = i, 
                                        value = tables[[i]], template = fields[[i]])
        rm(tables)
        message("Collecting garbage")
        gc()
      }
    }
  }
  if (exists("tables")) {
    return(tables)
  }
  else {
    return(NULL)
  }
}

##################################
### DATA SCRAPING AND CLEANING ###
##################################

########################
##### load packages ####
########################
setwd("~/School Work/Writing Project")
pack <- c("dplyr", "pitchRx", "DBI", "XML2R", "readr", "MASS", "ggplot2", "spatstat", "lattice", "car", "dummies", "mnormt")
suppressMessages(suppressWarnings(lapply(pack, require, character.only = TRUE, quietly = T)))
data(players, package = "pitchRx") #link playerID to player names

#######################################
#### create/connect to external db ####
#######################################
#db <- src_sqlite("baseball_dat_2008-08-01_2014-09-16", create = F)  ##old database - missing vars
db2 <- src_sqlite("baseball_dat_2008-08-01_2014_08_14", create = F)
db3 <- src_sqlite("player_dat_2008-08-01_2014-08-16", create = F)
#enhanced.scrape(start = "2008-08-01", end = "2017-10-31", connect = db2$con)
#enhanced.scrape(start = "2008-08-01", end = "2014-08-16", suffix = "players.xml", connect = db3$con)

#############################
## pull batter information ##
#############################
players[which(players$full_name == "Clayton Kershaw"),] #477132
pitch <- tbl(db2, "pitch")
kershaw <- collect(filter(pitch, pitcher == "477132"))
player <- tbl(db3, "player")
player.df <- collect(player, n = Inf)
kershaw$batter <- as.numeric(kershaw$batter)
player.df$batter <- as.numeric(player.df$id)
player.df <- dplyr::select(player.df, -id, -num, -url, -type)
kershaw <- inner_join(kershaw, player.df, by = c("batter", "gameday_link"))
kershaw$gameday_link <- factor(kershaw$gameday_link)

###########################
## Remove All Star Games ##
###########################
gids <- unique(kershaw$gameday_link)
all.star1 <- grep("aasmlb_nasmlb_1", gids, value = T)
all.star2 <- grep("nasmlb_aasmlb_1", gids, value = T)
all.star <- c(all.star1, all.star2)
gids.keep <- gids[-which(gids %in% all.star)]
kershaw <- filter(kershaw, gameday_link  %in% gids.keep)
kershaw$gameday_link <- as.factor(kershaw$gameday_link)


##############################
## pull catcher information ##
##############################
key <- unique(kershaw$gameday_link)
kershaw.games <- which(player.df$gameday_link %in% key)
kershaw.games.df <- player.df[kershaw.games,]
catcher.key <- which(kershaw.games.df$game_position == "C")
catcher.df <- kershaw.games.df[catcher.key,]

pitch.key <- which(kershaw.games.df$game_position == "P")
pitch.test.df <- kershaw.games.df[pitch.key,]
pitcher.id <- pitch.test.df$batter
catcher.df$pitcher_id <- pitcher.id
kershaw.catchers.df <- filter(catcher.df, pitcher.id == "477132")

temp <- dplyr::select(kershaw.catchers.df, first, last, gameday_link, pitcher_id)
key.df <- data.frame(gameday_link = key)
match.df <- full_join(temp, key.df)

##fix it
temp2 <- subset(catcher.df, gameday_link %in% c(match.df$gameday_link[nrow(temp)+1:nrow(match.df)])) 
temp3 <- subset(temp2, team_abbrev %in% c("LAD"))
temp4 <- dplyr::select(temp3, first, last, gameday_link)
temp4$pitcher_id <- rep("477132", nrow(temp4))

#final answer
kershaw.catchers <- rbind(match.df[1:nrow(kershaw.catchers.df),], temp4)
catchers <- unique(kershaw.catchers[,1:2])
full.names <- NULL
for(i in 1:nrow(catchers)){
  full.names[i] <- paste(catchers[i,1], catchers[i,2])
}
catchers$full_name <- full.names
test2 <- subset(players, full_name %in% catchers$full_name)

#identify Ramon Hernandez
#subset(kershaw.catchers, last == "Hernandez")
ramon.id <- scrape(game.ids = "gid_2013_05_31_lanmlb_colmlb_1", suffix = "players.xml")
ramon.id.df <- ramon.id$player
ramon.id.num <- subset(ramon.id.df, first == "Ramon" & last == "Hernandez")$id

#match catcher ids to gids
'%!in%' <- function(x,y)!('%in%'(x,y))
remove <- which(test2$full_name == "Ramon Hernandez" & test2$id %!in% ramon.id.num)
test3 <- test2[-c(remove), ]
names(test3) <- c("catcher_id", "full_name")
full_names <- NULL
for(i in 1:nrow(kershaw.catchers)){
  full_names[i] <- paste(kershaw.catchers[i,1], kershaw.catchers[i,2])
}
kershaw.catchers$full_name <- full_names
catcher.id.df <- full_join(kershaw.catchers, test3, by = "full_name")
names(catcher.id.df)[5] <- "catcher_full_name"
catcher.id.df2 <- dplyr::select(catcher.id.df, gameday_link, catcher_full_name, catcher_id)

#add catchers to kershaw
kershaw <- full_join(kershaw, catcher.id.df2, by = "gameday_link")

#factor variables
kershaw$count <- as.factor(kershaw$count)
kershaw$type <- as.factor(kershaw$type)
kershaw$b_height <- factor(kershaw$b_height,levels(factor(kershaw$b_height))[c(3:6,1:2,7:14)])
kershaw$pitch_type <- as.factor(kershaw$pitch_type)
kershaw$gameday_link <- as.factor(kershaw$gameday_link)
game_ids <- unique(kershaw$gameday_link)
kershaw$catcher_id <- as.factor(kershaw$catcher_id)

##remove atbats with NA, IN, UN
test <- summarize(group_by(kershaw, gameday_link), total = n())
kershaw.drop <- data.frame(matrix(0, 1, ncol(kershaw)))
names(kershaw.drop) <- names(kershaw)
for(i in 1:length(game_ids)){
  temp.df <- subset(kershaw, gameday_link == game_ids[i])
  drop <- unique(filter(temp.df, is.na(pitch_type) | pitch_type == "UN" | pitch_type == "IN")$num)
  if(is.empty(drop)){
    removed.df <- temp.df
  } else{removed.df <- temp.df[-c(which(temp.df$num %in% drop)),]}
  kershaw.drop <- as.tbl(rbind(kershaw.drop, removed.df))
}
kershaw.drop <- kershaw.drop[-1,]
kershaw.drop$pitch_type <- factor(kershaw.drop$pitch_type)
kershaw <- kershaw.drop

#create onbase variable - binary
onbase <- apply(kershaw[,43:45], 1, FUN = function(x) {!is.na(x)})
kershaw$onbase <- as.numeric(apply(onbase, 2, any))
kershaw$onbase <- as.factor(kershaw$onbase)

#create onbase.mult variable - multicategory
onbase.function <- function(X){
  ##function to return combination of runners on base
  x1 <- ifelse(!is.na(X[,1]), 1, NA)
  x2 <- ifelse(!is.na(X[,2]), 2, NA)
  x3 <- ifelse(!is.na(X[,3]), 3, NA)
  cbind(x1, x2, x3)
  
}

#previous pitch variable
game_ids <- unique(kershaw$gameday_link)
prev_pitch <- NULL
for(i in 1:length(game_ids)){
  temp.dat <- subset(kershaw, kershaw$gameday_link == game_ids[i])
  temp.dat$num <- as.factor(temp.dat$num)
  at_bats <- unique(temp.dat$num)
  one_game <- NULL
  for(j in 1:length(at_bats)){
    temp.dat2 <- subset(temp.dat, num == at_bats[j])
    pitches <- as.character(temp.dat2$pitch_type)
    pitches <- c("None", pitches)
    pitches <- pitches[-length(pitches)]
    one_game <- c(one_game, pitches)
  }
  prev_pitch <- c(prev_pitch, one_game)
}
kershaw$prev_pitch <- as.factor(prev_pitch)
kershaw$prev_pitch <- relevel(kershaw$prev_pitch, ref = "None")

prev_pitch_type <- NULL
for(i in 1:length(game_ids)){
  temp.dat <- subset(kershaw, kershaw$gameday_link == game_ids[i])
  temp.dat$num <- as.factor(temp.dat$num)
  at_bats <- unique(temp.dat$num)
  one_game <- NULL
  for(j in 1:length(at_bats)){
    temp.dat2 <- subset(temp.dat, num == at_bats[j])
    pitches <- as.character(temp.dat2$type)
    pitches <- c("None", pitches)
    pitches <- pitches[-length(pitches)]
    one_game <- c(one_game, pitches)
  }
  prev_pitch_type <- c(prev_pitch_type, one_game)
}
kershaw$prev_pitch_type <- as.factor(prev_pitch_type)

write.csv(kershaw, file = "kershaw_red.csv", row.names = F)

###########
### EDA ###
###########

kershaw <- read.csv("~/School Work/Writing Project/kershaw_red.csv")

kershaw$count <- factor(kershaw$count, levels = c("0-0", "1-0", "2-0", "3-0", "0-1", "1-1", "2-1", "3-1", "0-2", "1-2", "2-2", "3-2"))

ggplot(kershaw, aes(pitch_type)) + geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) + facet_wrap( ~ count) + ylab("relative frequencies")
ggplot(kershaw, aes(pitch_type)) + geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) + facet_wrap( ~ prev_pitch) + ylab("relative frequencies")

#########################################
### SAMPLER FUNCTIONS AND MCMC CHAINS ###
#########################################

kershaw <- read.csv("~/School Work/Writing Project/kershaw_red.csv")
kershaw$prev_pitch <- relevel(kershaw$prev_pitch, ref = "None")
pack <- c("dplyr", "pitchRx", "DBI", "XML2R", "readr", "MASS", "ggplot2", "spatstat", "lattice", "car", "dummies", "mnormt", "BayesMultReg")
suppressMessages(suppressWarnings(lapply(pack, require, character.only = TRUE, quietly = T)))

#create training and test datasets
set.seed(20938)
ids <- sort(sample(1:nrow(kershaw), 15000))
kershaw.training <- kershaw[ids,]
kershaw.cross <- kershaw[-ids,]
kershaw.cross$
  
bayes.mlogit.hier.thin <- function(formula, baseline = NULL, data, num.mcmc = 10000, step.size = NULL, silent = T, mu0 = 0,
                                     Lambda0 = 100, eta0 = 1, S0 = 1, burnin = 1, ini.beta = 0, ini.theta = 0, ini.Sigma = 1,
                                     ini.beta.mean = 0, ini.beta.var = 1, ini.theta.mean = 0, ini.theta.var = 1, seed = NULL,
                                     num.chains = 1, hierarchy = NULL, thin = 1, export = F){
    #seed
    set.seed(seed)
    
    #thinning
    if(num.mcmc %% thin != 0){stop("Number of iterations must be divisible by the thinning parameter.")}
    
    #model
    formula <- as.formula(formula)
    mf <- model.frame(formula = formula, data = data)
    X <- model.matrix(formula, data = data)
    y <- dummies::dummy(model.response(mf))
    colnames(y) <- as.vector(sort(unique(model.response(mf))))
    
    #hierarchy
    hier.id <- which(colnames(data) == hierarchy)
    if(!is.factor(data[,hier.id])) {stop("Variable used to define hierarchy must be a factor.")}
    m <- length(levels(data[,hier.id]))
    
    #separate datasets for each level of hierarchy
    df.list <- split(data, f = data[,hier.id])
    
    X.list <- lapply(df.list, FUN = function(x){model.matrix(formula, data = x)})
    y.list <- lapply(df.list, FUN = function(x){mf <- model.frame(formula = formula, data = x); dummies::dummy(model.response(mf))})
    
    #setup sampler
    k <- dim(y)[2]
    p <- k*dim(X)[2]
    theta.mcmc.list <- lapply(rep(0, num.chains), FUN = function(x) {matrix(x, num.mcmc/thin + 1, p)})
    Sigma.mcmc.list <- lapply(rep(0, num.chains), FUN = function(x) {matrix(x, nrow = num.mcmc/thin + 1, ncol = p*(p+1)/2)})
    Beta.mcmc.list <- lapply(rep(0, num.chains), FUN = function(x) {array(x, dim = c(num.mcmc/thin + 1, p, m))})
    
    #priors - mean
    if(length(mu0 == 1)){
      mu0 <- rep(mu0, p)
    } else{
      if(length(m0 == p)){
        mu0 <- mu0
      } else{
        stop("Prior mean format not understood; specify as vector of prior means of length p or a single number representing the prior mean of each beta.")
      }
    }
    
    if(is.matrix(Lambda0)){
      Lambda0 <- Lambda0
    } else{
      if(length(Lambda0) == 1){
        Lambda0 <- Lambda0 * diag(p)
      } else{
        stop("Prior variance format not understood; specify as a p x p matrix or a single number representing the prior variance of the betas")
      }
    }
    if(dim(Lambda0)[2] != p){
      stop("Dimension of prior variance matrix does not match number of groups.")
    }
    if(dim(Lambda0)[1] != p){
      stop("Dimension of prior variance matrix does not match number of groups.")
    }
    Lambda0.inv <- solve(Lambda0)
    
    #priors - variance
    if(length(eta0) != 1) {stop("Degrees of freedom parameter of Inverse Wishart must be of length 1.")}
    if(eta0 < p) {stop(bquote('Degrees of freedom parameter of Inverse Wishart must be greater than dimension of S0 = '~.(p)))}
    
    if(length(S0 == 1)){
      S0 <- S0 * diag(p)
    } else{
      if(is.matrix(S0)){
        S0 <- S0
      }
    }
    if(dim(S0)[2] != p){
      stop("Dimension of prior scale matrix does not match number of regressors.")
    }
    if(dim(S0)[1] != p){
      stop("Dimension of prior scale matrix does not match number of regressors.")
    }
    
    #baseline response category
    if(is.null(baseline)){
      baseline <- 1
    } else{
      if(is.character(baseline)){
        baseline <- which(colnames(y) == baseline)
      } else{
        if(length(baseline) == 1){
          baseline <- baseline
        } else{
          stop("Format of baseline category not understood; specify as a level of the response or numeric column reference.")
        }
      }
    }
    
    #define sets of betas by variable
    X.attr <- attributes(X)$assign
    col.ref.var <- rep(X.attr, k)
    #accept.ratio <- array(0, dim = c(num.mcmc, length(unique(col.ref.var)), m))
    blocks <- unique(col.ref.var) + 1
    
    #step size
    if(is.null(step.size)){
      step.size <- rep(.01, length(blocks))
    } else{
      if(length(step.size) == 1){
        step.size <- rep(step.size, length(blocks))
      } else{
        if(length(step.size) != length(blocks)){
          stop("Step size must have length of one or equal to the number of blocks in the Gibbs sampler.")
        } else{
          step.size <- step.size
        }
      }
    }
    
    #label variables
    resp <- rep(colnames(y), each = p/k)
    ex <- rep(colnames(X), k)
    names <- paste(resp, rep("-", p), ex)
    
    #initalize MCMC
    if(all(ini.beta == 0)){
      NULL
    } else{
      if(all(ini.beta == "random")){
        for(q in 1:num.chains){
          Beta.mcmc.list[[q]][1,,] <- rnorm(p, ini.beta.mean, ini.beta.var)
        }
      } else{
        if(length(ini.beta) == m*p){
          for(q in 1:num.chains){
            Beta.mcmc.list[[q]][1,,] <- ini.beta
          }
        } else{
          stop("Initialization parameters not understood. Should be either 0 to initialize the MCMC at 0s or random to initialize at a random starting location.")
        }
      }
    }
    
    if(all(ini.theta == 0)){
      NULL
    } else {
      if(all(ini.theta == "random")){
        for(q in 1:num.chains){
          theta.mcmc.list[[q]][1,] <- rnorm(p, ini.theta.mean, ini.theta.var)
        }
      } else{
        if(length(ini.theta) == p){
          for(q in 1:num.chains){
            theta.mcmc.list[[q]][1,] <- ini.theta
          }
        } else{
          stop("Initialization parameters not understood. Should be either 0 to initialize the MCMC at 0s or random to initialize at a random starting location.")
        }
      }
    }
    
    if(all(ini.Sigma == 1)){
      for(q in 1:num.chains){
        tmp.Sigma <- diag(p)
        up.tri <- gdata::upperTriangle(tmp.Sigma)
        varcov.diag <- diag(tmp.Sigma)
        
        Sigma.mcmc.list[[q]][1,1:(p*(p-1)/2)]  <- up.tri
        Sigma.mcmc.list[[q]][1,(p*(p-1)/2 + 1):(p*(p+1)/2)] <- varcov.diag
      }
    } else{
      if(length(ini.Sigma) == p*(p+1)/2){
        for(q in 1:num.chains){
          Sigma.mcmc.list[[q]][1,1:(p*(p-1)/2)]  <- ini.Sigma[1:(p*(p-1)/2)]
          Sigma.mcmc.list[[q]][1,(p*(p-1)/2 + 1):(p*(p+1)/2)] <- ini.Sigma[(p*(p-1)/2 + 1):(p*(p+1)/2)]
        }
      }
    }
    
    
    for(b in 1:num.chains){
      message('Beginning sampler. Please be patient.')
      
      Beta.mcmc <- Beta.mcmc.list[[b]]
      theta.mcmc <- theta.mcmc.list[[b]]
      Sigma.mcmc <- Sigma.mcmc.list[[b]]
      
      #thinning
      for(i in 2:num.mcmc){
        if(i == 2){
          tmp.Beta.mcmc <- array(0, dim = c(thin+1, dim(Beta.mcmc)[2], dim(Beta.mcmc)[3]))
          tmp.Beta.mcmc[2,,] <- Beta.mcmc[floor(i/thin)+1,,] 
          
          tmp.theta.mcmc <- matrix(0, thin+1, dim(theta.mcmc)[2])
          tmp.theta.mcmc[2,] <- theta.mcmc[floor(i/thin)+1,]
          
          tmp.Sigma.mcmc <- matrix(0, thin+1, dim(Sigma.mcmc)[2])
          tmp.Sigma.mcmc[2,] <- Sigma.mcmc[floor(i/thin)+1,] 
        }
        if(i %% thin == 1){
          tmp.Beta.mcmc <-  array(0, dim = c(thin+1, dim(Beta.mcmc)[2], dim(Beta.mcmc)[3]))
          tmp.Beta.mcmc[1,,] <- Beta.mcmc[floor(i/thin)+1,,] 
          
          tmp.theta.mcmc <- matrix(0, thin+1, dim(theta.mcmc)[2])
          tmp.theta.mcmc[1,] <- theta.mcmc[floor(i/thin)+1,]
          
          tmp.Sigma.mcmc <- matrix(0, thin+1, dim(Sigma.mcmc)[2])
          tmp.Sigma.mcmc[1,] <- Sigma.mcmc[floor(i/thin)+1,] 
        }
        
        if(i %% thin != 0){
          ndx <- i %% thin #make sure this only goes 1:9
        } else{
          ndx <- thin
        }
        
        #sample theta conditional on Sigma, beta
        Sigma <- diag(tmp.Sigma.mcmc[ndx,(p*(p-1)/2 + 1):(p*(p+1)/2)])
        gdata::upperTriangle(Sigma) <- tmp.Sigma.mcmc[ndx,1:(p*(p-1)/2)]
        gdata::lowerTriangle(Sigma, byrow = T) <- tmp.Sigma.mcmc[ndx,1:(p*(p-1)/2)]
        Sigma.inv <- solve(Sigma)
        beta.bar <- rowMeans(tmp.Beta.mcmc[ndx,,])
        
        Lambda.m <- solve(Lambda0.inv + m*Sigma.inv)
        mu.m <- Lambda.m %*% (Lambda0.inv %*% matrix(mu0,p,1) + m*Sigma.inv %*% matrix(beta.bar, p, 1))
        tmp.theta.mcmc[ndx+1,] <- mnormt::rmnorm(1, mu.m, Lambda.m)
        
        #sample Sigma conditional on theta, beta
        tmp.s.theta <- sapply(c(1:m), FUN = function(x){tmp.Beta.mcmc[ndx,,x] - tmp.theta.mcmc[ndx+1,]})
        tmp <- array(0, dim = c(p,p,m))
        for(n in 1:dim(tmp.s.theta)[2]){
          tmp[,,n] <- tmp.s.theta[,n] %*% t(tmp.s.theta[,n])
        }
        S.theta <- apply(tmp, c(1:2), sum)
        Sn <- solve(S0 + S.theta)
        
        Sigma <- round(MCMCpack::riwish(eta0 + m, Sn), 6)
        if(!isSymmetric(Sigma)){
          Sigma <- round(MCMCpack::riwish(eta0 + m, Sn), 6)
        }
        
        tmp.Sigma.mcmc[ndx+1,1:(p*(p-1)/2)] <- gdata::upperTriangle(Sigma)
        tmp.Sigma.mcmc[ndx+1,(p*(p-1)/2 + 1):(p*(p+1)/2)] <- diag(Sigma)
        
        for(m in 1:m){
          for(j in 1:length(blocks)){
            y <- y.list[[m]]
            X <- X.list[[m]]
            Beta.star <- tmp.Beta.mcmc[ndx,,m] + ifelse(col.ref.var == (j-1), 1, 0) * mnormt::rmnorm(1, 0, step.size[j]*diag(p))
            
            #compute r
            Beta.current <- matrix(tmp.Beta.mcmc[ndx,,m], p/k, k)
            Beta.current <- Beta.current - Beta.current[,baseline]
            tmp <- exp(X %*% Beta.current)
            pi.current <- tmp / rowSums(tmp)
            
            Beta.star <- matrix(Beta.star, p/k, k)
            Beta.star <- Beta.star - Beta.star[,baseline]
            tmp <- exp(X %*% Beta.star)
            pi.star <- tmp / rowSums(tmp)
            
            log.p.current <- sum((y * log(pi.current))) + mnormt::dmnorm(x = as.numeric(Beta.current), mean = tmp.theta.mcmc[ndx+1,], varcov = Sigma, log = T)
            log.p.star <- sum((y * log(pi.star))) + mnormt::dmnorm(x = as.numeric(Beta.star), mean = tmp.theta.mcmc[ndx+1,], varcov = Sigma, log = T)
            
            log.r <- log.p.star - log.p.current
            
            if(j < length(blocks)){
              if (log(runif(1)) < log.r){
                tmp.Beta.mcmc[ndx,,m] <- Beta.star
                #accept.ratio[i,j,m] <- 1
              } else{
                tmp.Beta.mcmc[ndx,,m] <- tmp.Beta.mcmc[ndx,,m]
              }
            } else{
              if (log(runif(1)) < log.r){
                tmp.Beta.mcmc[ndx+1,,m] <- Beta.star
                #accept.ratio[i,length(blocks), m] <- 1
              } else{
                tmp.Beta.mcmc[ndx+1,,m] <- tmp.Beta.mcmc[ndx,,m]
              }
            }
          }
          
          if(i %% thin == 0){
            Beta.mcmc[floor(i/thin)+1,,] <- tmp.Beta.mcmc[thin,,]
            
            theta.mcmc[floor(i/thin)+1,] <- tmp.theta.mcmc[thin,]
            
            Sigma.mcmc[floor(i/thin)+1,] <- tmp.Sigma.mcmc[thin,]
          }
          
        }
        
        if(silent == F){
          #cat(bquote(.(round(i/(num.mcmc), 5)*100)), "% through chain", bquote(.(b)), "\n")
          if (i %% 500 == 0) {
            message('Progress: ', bquote(.(i)), 'th iteration of chain ', bquote(.(b)))
            message(bquote(.(round(i/(num.mcmc), 5)*100)),'% through chain ',bquote(.(b)))
          }
        }
        
        #export periodically
        if(export == T){
          if(i %% (num.mcmc/10) == 0){
            options(scipen = 999)
            name <- as.character(paste('Chain ', bquote(.(b)), ' - Betas - Iteration ', bquote(.(i)), '.csv', sep = ""))
            name2 <- as.character(paste('Chain ', bquote(.(b)), ' - Theta - Iteration ', bquote(.(i)), '.csv', sep = ""))
            name3 <- as.character(paste('Chain ', bquote(.(b)), ' - Sigma - Iteration ', bquote(.(i)), '.csv', sep = ""))
            options(scipen = 0)
            write.csv(data.frame(Beta.mcmc), file = bquote(.(name)), row.names = F)
            write.csv(data.frame(theta.mcmc), file = bquote(.(name2)), row.names = F)
            write.csv(data.frame(Sigma.mcmc), file = bquote(.(name3)), row.names = F)
          }
        }
      }
      
      Beta.mcmc <- Beta.mcmc[(burnin/thin):(num.mcmc/thin + 1),,]
      dimnames(Beta.mcmc)[[3]] <- levels(data[,hier.id])
      dimnames(Beta.mcmc)[[2]] <- names
      
      Beta.mcmc.list[[b]] <- Beta.mcmc
      #mcmc <- coda::mcmc(Beta.mcmc, start = burnin, end = num.mcmc)
      
      #mcmc.list[[b]] <- mcmc
      theta.mcmc.list[[b]] <- theta.mcmc[(burnin/thin):(num.mcmc/thin + 1),]
      Sigma.mcmc.list[[b]] <- Sigma.mcmc[(burnin/thin):(num.mcmc/thin + 1),]
    }
    #mcmc.list <- coda::as.mcmc.list(mcmc.list)
    #theta.mcmc.list
    #accept.ratio.mat <- colMeans(accept.ratio)
    #colnames(accept.ratio.mat) <- levels(data[,hier.id])
    #rownames(accept.ratio.mat) <- c("intercept", names(mf)[-1])
    list(Beta.mcmc.list, theta.mcmc.list, Sigma.mcmc.list)
  }

#chain 1
start <- Sys.time()
#fit model
setwd("~/School Work/Writing Project/Feb 20th/Sampler - Mar 13/Chain 1 - Thinned Sampler")
hier.mod <- bayes.mlogit.hier.thin(formula = pitch_type ~ count + prev_pitch, baseline = "FF", data = kershaw.training,
                                   num.mcmc = 1000000, silent = F, mu0 = 0, step.size = c(.0005, .0005, .0007), Lambda0 = 100,
                                   eta0 = 64, S0 = 1, burnin = 1, ini.beta = "random", ini.theta = 0, ini.Sigma = 1,
                                   ini.beta.mean = 0, ini.beta.var = 1, ini.theta.mean = 0, ini.theta.var = 1, seed = NULL,
                                   num.chains = 1, hierarchy = "catcher_full_name", thin = 10, export = T)
end <- Sys.time()
end - start

#chain 2
start <- Sys.time()
#fit model
setwd("~/School Work/Writing Project/Feb 20th/Sampler - Mar 13/Chain 2 - Thinned Sampler")
hier.mod <- bayes.mlogit.hier.thin(formula = pitch_type ~ count + prev_pitch, baseline = "FF", data = kershaw.training,
                                   num.mcmc = 1000000, silent = F, mu0 = 0, step.size = c(.0005, .0005, .0007), Lambda0 = 100,
                                   eta0 = 64, S0 = 1, burnin = 1, ini.beta = "random", ini.theta = 0, ini.Sigma = 1,
                                   ini.beta.mean = 0, ini.beta.var = 1, ini.theta.mean = 0, ini.theta.var = 1, seed = NULL,
                                   num.chains = 1, hierarchy = "catcher_full_name", thin = 10, export = T)
end <- Sys.time()
end - start

#chain 3
start <- Sys.time()
#fit model
setwd("~/School Work/Writing Project/Feb 20th/Sampler - Mar 13/Chain 3 - Thinned Sampler")
hier.mod <- bayes.mlogit.hier.thin(formula = pitch_type ~ count + prev_pitch, baseline = "FF", data = kershaw.training,
                                   num.mcmc = 1000000, silent = F, mu0 = 0, step.size = c(.0005, .0005, .0007), Lambda0 = 100,
                                   eta0 = 64, S0 = 1, burnin = 1, ini.beta = "random", ini.theta = 0, ini.Sigma = 1,
                                   ini.beta.mean = 0, ini.beta.var = 1, ini.theta.mean = 0, ini.theta.var = 1, seed = NULL,
                                   num.chains = 1, hierarchy = "catcher_full_name", thin = 10, export = T)
end <- Sys.time()
end - start

#non-hier
bayes.mlogit.thin <- function(formula, baseline = NULL, data, num.mcmc = 10000, step.size = NULL, b0 = 0, bvar0 = 100, silent = T,
                              burnin = 1, ini.beta = 0, ini.mean = 0, ini.var = 1, seed = NULL, num.chains = 1, thin, export = F){
  
  #seed
  set.seed(seed)
  
  #thinning
  if(num.mcmc %% thin != 0){stop("Number of iterations must be divisible by the thinning parameter.")}
  
  #model
  formula <- as.formula(formula)
  mf <- model.frame(formula = formula, data = data)
  X <- model.matrix(formula, data = data)
  y <- dummies::dummy(model.response(mf))
  colnames(y) <- as.vector(sort(unique(model.response(mf))))
  
  #setup sampler
  k <- dim(y)[2]
  p <- k*dim(X)[2]
  Beta.mcmc.list <- lapply(rep(0, num.chains), FUN = function(x) {matrix(x, num.mcmc/thin+1, p)})
  
  #prior variance
  if(is.matrix(bvar0)){
    beta.prior.var <- bvar0
  } else{
    if(length(bvar0) == 1){
      beta.prior.var <- bvar0 * diag(p)
    } else{
      stop("Prior variance format not understood; specify as a p x p matrix or a single number representing the prior variance of each beta.")
    }
  }
  if(dim(beta.prior.var)[2] != p){
    stop("Dimension of prior variance matrix does not match dimension of beta vector.")
  }
  if(dim(beta.prior.var)[1] != p){
    stop("Dimension of prior variance matrix does not match dimension of beta vector.")
  }
  
  #prior mean
  if(length(b0) == 1){
    b0 <- b0
  } else{
    if(length(b0) == p){
      b0 <- b0
    } else{
      stop("Prior mean format not understood; specify as vector of prior means of length p or a single number representing the prior mean of each beta.")
    }
  }
  
  #baseline response category
  if(is.null(baseline)){
    baseline <- 1
  } else{
    if(is.character(baseline)){
      baseline <- which(colnames(y) == baseline)
    } else{
      if(length(baseline) == 1){
        baseline <- baseline
      } else{
        stop("Format of baseline category not understood; specify as a level of the response or numeric column reference.")
      }
    }
  }
  
  #define sets of betas by variable
  X.attr <- attributes(X)$assign
  col.ref.var <- rep(X.attr, k)
  #accept.ratio <- matrix(0, num.mcmc, length(unique(col.ref.var)))
  blocks <- unique(col.ref.var) + 1
  
  #step size
  if(is.null(step.size)){
    step.size <- rep(.01, length(blocks))
  } else{
    if(length(step.size) == 1){
      step.size <- rep(step.size, length(blocks))
    } else{
      if(length(step.size) != length(blocks)){
        stop("Step size must have length of one or equal to the number of blocks in the Gibbs sampler.")
      } else{
        step.size <- step.size
      }
    }
  }
  
  #label variables
  resp <- rep(colnames(y), each = p/k)
  ex <- rep(colnames(X), k)
  names <- paste(resp, rep("-", p), ex)
  
  #initalize MCMC
  if(ini.beta == 0){
    NULL
  } else{
    if(ini.beta == "random"){
      for(q in 1:num.chains){
        Beta.mcmc.list[[q]][1,] <- rnorm(p, ini.mean, ini.var)
      }
    } else{
      stop("Initialization parameters not understood. Should be either 0 to initialize the MCMC at 0s or random to initialize at a random starting location.")
    }
  }
  
  #sampler
  mcmc.list <- NULL
  for(b in 1:num.chains){
    message('Beginning sampler. Please be patient.')
    
    Beta.mcmc <- Beta.mcmc.list[[b]]
    
    for(i in 2:num.mcmc){
      
      if(i == 2){
        tmp.Beta.mcmc <- matrix(0, nrow = thin+1, ncol = dim(Beta.mcmc)[2])
        tmp.Beta.mcmc[2,] <- Beta.mcmc[floor(i/thin)+1,] 
      }
      if(i %% thin == 1){
        tmp.Beta.mcmc <-  matrix(0, nrow = thin+1, ncol = dim(Beta.mcmc)[2])
        tmp.Beta.mcmc[1,] <- Beta.mcmc[floor(i/thin)+1,] 
      }
      
      if(i %% thin != 0){
        ndx <- i %% thin #make sure this only goes 1:9
      } else{
        ndx <- thin
      }
      
      for(j in 1:length(blocks)){
        Beta.star <- tmp.Beta.mcmc[ndx,] + ifelse(col.ref.var == (j-1), 1, 0) * mnormt::rmnorm(1, 0, step.size[j]*diag(p))
        
        #compute r
        Beta.current <- matrix(tmp.Beta.mcmc[ndx,], p/k, k)
        Beta.current <- Beta.current - Beta.current[,baseline]
        tmp <- exp(X %*% Beta.current)
        pi.current <- tmp / rowSums(tmp)
        
        
        Beta.star <- matrix(Beta.star, p/k, k)
        Beta.star <- Beta.star - Beta.star[,baseline]
        tmp <- exp(X %*% Beta.star)
        pi.star <- tmp / rowSums(tmp)
        
        log.p.current <- sum((y * log(pi.current))) + mnormt::dmnorm(as.numeric(Beta.current), 0, beta.prior.var, log = T)
        log.p.star <- sum((y * log(pi.star))) + mnormt::dmnorm(as.numeric(Beta.star), 0, beta.prior.var, log = T)
        
        log.r <- log.p.star - log.p.current
        
        if(j < length(blocks)){
          if (log(runif(1)) < log.r){
            tmp.Beta.mcmc[ndx,] <- Beta.star
            #accept.ratio[i,j] <- 1
          } else{
            tmp.Beta.mcmc[ndx,] <- tmp.Beta.mcmc[ndx,]
          }
        } else{
          if (log(runif(1)) < log.r){
            tmp.Beta.mcmc[ndx+1,] <- Beta.star
            #accept.ratio[i,length(blocks)] <- 1
          } else{
            tmp.Beta.mcmc[ndx+1,] <- tmp.Beta.mcmc[ndx,]
          }
        }
      }
      
      if(i %% thin == 0){
        Beta.mcmc[floor(i/thin)+1,] <- tmp.Beta.mcmc[thin,]
      }
      
      if(silent == F){
        #cat(bquote(.(round(i/(num.mcmc), 5)*100)), "% through chain", bquote(.(b)), "\n")
        if (i %% 500 == 0) {
          message('Progress: ', bquote(.(i)), 'th iteration of chain ', bquote(.(b)))
          message(bquote(.(round(i/(num.mcmc), 5)*100)),'% through chain ',bquote(.(b)))
        }
      }
    }
    
    Beta.mcmc <- Beta.mcmc[burnin:(num.mcmc/thin + 1),]
    colnames(Beta.mcmc) <- names
    mcmc <- coda::mcmc(Beta.mcmc, start = burnin, end = num.mcmc, thin = thin)
    
    mcmc.list[[b]] <- mcmc
  }
  mcmc.list <- coda::as.mcmc.list(mcmc.list)
}

test <- bayes.mlogit.thin(formula = pitch_type ~ count + prev_pitch, baseline = "FF",
                          step.size = c(.0005, .0005, .0007), num.mcmc = 1000000, bvar0 = 9, b0 = 0,
                          data = kershaw.training, silent = F, burnin = 1, num.chains = 3, ini.beta = "random", thin = 10)

chain1.df <- data.frame(test[[1]])
chain2.df <- data.frame(test[[2]])
chain3.df <- data.frame(test[[3]])

setwd("~/School Work/Writing Project/Feb 20th/Sampler - Mar 13/Nonhierarchical")

write.csv(chain1.df, file = "Chain 1.csv", row.names = F)
write.csv(chain2.df, file = "Chain 2.csv", row.names = F)
write.csv(chain3.df, file = "Chain 3.csv", row.names = F)

###########################################
### CALCULATE CONDITIONAL BAYES FACTORS ###
###########################################

pi.calc <- function(beta.vect, nresp, X){
  beta.mat <- matrix(beta.vect, length(beta.vect)/nresp, nresp)
  tmp <- exp(X %*% beta.mat)
  tmp/rowSums(tmp)
}
dmultlog.mat <- function(x, prob){
  size <- sum(x)
  if(is.null(dim(prob))){
    rows <- 1
  } else{
    rows <- dim(prob)[1]
  }
  
  norm.num <- lgamma(size + 1)
  norm.denom <- lgamma(x + 1)
  norm.con <- norm.num - sum(norm.denom)
  
  x.vect <- rep(x, rows)
  
  tmp <- matrix(x.vect, rows, length(x), byrow = T)
  norm.con + rowSums(tmp*log(prob))
}

# GET DATA
{
  kershaw <- read.csv("~/School Work/Writing Project/kershaw_red.csv")
  kershaw$prev_pitch <- relevel(kershaw$prev_pitch, ref = "None")
  pack <- c("dplyr", "pitchRx", "DBI", "XML2R", "readr", "MASS", "ggplot2", "spatstat", "lattice", "car", "dummies", "mnormt", "BayesMultReg", "coda")
  suppressMessages(suppressWarnings(lapply(pack, require, character.only = TRUE, quietly = T)))
  
  #create training and test datasets
  set.seed(20938)
  ids <- sort(sample(1:nrow(kershaw), 15000))
  kershaw.training <- kershaw[ids,]
  kershaw.cross <- kershaw[-ids,]
  
  kershaw.cross.red <- dplyr::select(kershaw.cross, pitch_type, count, prev_pitch, catcher_full_name)
  
  chain1.betas <- read.csv("~/School Work/Writing Project/Feb 20th/Sampler - Mar 13/Chain 1 - Thinned Sampler/Chain 1 - Betas - Iteration 1000000.csv")
  chain2.betas <- read.csv("~/School Work/Writing Project/Feb 20th/Sampler - Mar 13/Chain 2 - Thinned Sampler/Chain 2 - Betas - Iteration 1000000.csv")
  chain3.betas <- read.csv("~/School Work/Writing Project/Feb 20th/Sampler - Mar 13/Chain 3 - Thinned Sampler/Chain 3 - Betas - Iteration 1000000.csv")
  chain1.non <- read.csv("~/School Work/Writing Project/Feb 20th/Sampler - Mar 13/Nonhierarchical/Chain 1.csv")
  chain2.non <- read.csv("~/School Work/Writing Project/Feb 20th/Sampler - Mar 13/Nonhierarchical/Chain 2.csv")
  chain3.non <- read.csv("~/School Work/Writing Project/Feb 20th/Sampler - Mar 13/Nonhierarchical/Chain 3.csv")
  
  mf <- model.frame(pitch_type ~ count + prev_pitch, data = kershaw.cross.red)
  X <- model.matrix(pitch_type ~ count + prev_pitch, data = kershaw.cross.red)
  resp <- dummies::dummy(model.response(mf))
  colnames(resp) <- as.vector(sort(unique(model.response(mf))))
  
  thin <- seq(10000, 100000, by = 9)
  non.hier.avg <- (as.matrix(chain1.non)[thin,] + as.matrix(chain2.non)[thin,] + as.matrix(chain3.non)[thin,])/3
  
  betas.chain1 <- array(unname(unlist(c((chain1.betas)))), dim = c(nrow(chain1.betas), 64, 11))
  betas.chain2 <- array(unname(unlist(c((chain2.betas)))), dim = c(nrow(chain2.betas), 64, 11))
  betas.chain3 <- array(unname(unlist(c((chain3.betas)))), dim = c(nrow(chain3.betas), 64, 11))
  avg.betas.array <- (betas.chain1[thin,,] + betas.chain2[thin,,] + betas.chain3[thin,,])/3
  
  get.names <- bayes.mlogit(formula = pitch_type ~ count + prev_pitch, baseline = "FF",
                            step.size = c(.0005, .0005, .0007), num.mcmc = 100, bvar0 = 9, b0 = 0,
                            data = kershaw.training, silent = F, burnin = 1, num.chains = 1, ini.beta = "random")
  names <- colnames(get.names[[1]])
  
  dimnames(non.hier.avg)[[2]] <- names
  dimnames(avg.betas.array)[[2]] <- names
}

y <- kershaw.cross.red %>% group_by(count, prev_pitch) %>% summarise(freq = n())
X <- model.matrix(~ count + prev_pitch, data = y)

# GET PI'S
{
  #non-hierarchical
  pis <- array(0, dim = c(dim(non.hier.avg)[1],4,dim(X)[1]))
  for(i in 1:dim(non.hier.avg)[1]){
    for(j in 1:dim(X)[1]){
      pis[i,,j] <- pi.calc(non.hier.avg[i,], nresp = 4, X = X[j,]) 
    }
  }
  dimnames(pis) <- list(NULL, levels(kershaw$pitch_type), paste(y$count, y$prev_pitch))
  
  #hierarchical
  hier.pis.list <- list(NULL)
  for(m in 1:length(levels(kershaw.cross.red$catcher_full_name))){
    hier.pis <- array(0, dim = c(dim(avg.betas.array)[1],4,dim(X)[1]))
    for(i in 1:dim(avg.betas.array)[1]){
      for(j in 1:dim(X)[1]){
        hier.pis[i,,j] <- pi.calc(avg.betas.array[i,,m], nresp = 4, X = X[j,]) 
      }
    }
    dimnames(hier.pis) <- list(NULL, levels(kershaw$pitch_type), paste(y$count, y$prev_pitch))
    hier.pis.list[[m]] <- hier.pis
    print(bquote(.(m)))
  }
}

# STRUCTURE PI'S IN PROPER ORDER
{
  X <- model.matrix(pitch_type ~ count + prev_pitch, data = kershaw.cross.red)
  design.id <- rep(0, dim(X)[1])
  design.id[unname(which(rowSums(X) == 1))] <- 1
  
  X.order <- model.matrix(~ count + prev_pitch, data = y)
  
  check.equal <- function(x, y){
    isTRUE(all.equal(y, x, check.attributes = FALSE))
  }
  unname(which(apply(X, 1, check.equal, y=X.order[1,])))
  
  for(i in 1:dim(X.order)[1]){
    tmp <- unname(which(apply(X, 1, check.equal, y=X.order[i,])))
    design.id[tmp] <- i
  }
  catchers <- kershaw.cross.red$catcher_full_name
  
  catcher.id <- rep(0, length(catchers))
  for(i in 1:length(catchers)){
    catcher.id[i] <- which(catchers[i] == levels(kershaw.cross.red$catcher_full_name))
  }
}


# CALCULATE BF
bf <- matrix(0, dim(resp)[1], 2)
colnames(bf) <- c("non", "hier")
for(i in 1:dim(resp)[1]){
  id <- design.id[i]
  catch <- catcher.id[i]
  tmp.resp <- unname(resp[i,])
  tmp.mat <- matrix(rep(tmp.resp, dim(pis)[1]), nrow = dim(pis)[1], ncol = 4, byrow = T)
  
  non <- sum(colMeans(tmp.mat * log(pis[,,id])))
  hier <- sum(colMeans(tmp.mat * log(hier.pis.list[[catch]][,,id])))
  
  bf[i,] <- c(non, hier)
  
  if(i %% 500 == 0){
    message(bquote(.(i)), ' iteration of ', bquote(.(dim(resp)[1])))
  }
}

logbf <- colSums(bf)['hier'] - colSums(bf)['non']

#################
### 2016 NLDS ###
#################

#1024 - 1101

nlds2016 <- enhanced.scrape(start = "2016-10-13", end = "2016-10-13")
pitches <- nlds2016$pitch
nldsker <- subset(pitches, pitcher == 477132)
## 1-2 count, prev pitch was slider, fouls off slider, strikes out on curve
## runners on 1, 2, 2 outs, down 4-3
## strikes out on slider

lapply(hier.pis.list, FUN = function(x){colMeans(x[,,21])})[[1]]
nldsker[,c('pitch_type', 'count')]

########################
### SIMULATION STUDY ###
########################

bayes.mlogit.hier <- function(formula, baseline = NULL, data, num.mcmc = 10000, step.size = NULL, silent = T, mu0 = 0,
                              Lambda0 = 100, eta0 = 1, S0 = 1, burnin = 1, ini.beta = 0, ini.theta = 0, ini.Sigma = 1,
                              ini.beta.mean = 0, ini.beta.var = 1, ini.theta.mean = 0, ini.theta.var = 1, seed = NULL,
                              num.chains = 1, hierarchy = NULL){
  #seed
  set.seed(seed)
  
  #model
  formula <- as.formula(formula)
  mf <- model.frame(formula = formula, data = data)
  X <- model.matrix(formula, data = data)
  y <- dummies::dummy(model.response(mf))
  colnames(y) <- as.vector(sort(unique(model.response(mf))))
  
  #hierarchy
  hier.id <- which(colnames(data) == hierarchy)
  if(!is.factor(data[,hier.id])) {stop("Variable used to define hierarchy must be a factor.")}
  m <- length(levels(data[,hier.id]))
  
  #separate datasets for each level of hierarchy
  df.list <- split(data, f = data[,hier.id])
  
  X.list <- lapply(df.list, FUN = function(x){model.matrix(formula, data = x)})
  y.list <- lapply(df.list, FUN = function(x){mf <- model.frame(formula = formula, data = x); dummies::dummy(model.response(mf))})
  
  #setup sampler
  k <- dim(y)[2]
  p <- k*dim(X)[2]
  theta.mcmc.list <- lapply(rep(0, num.chains), FUN = function(x) {matrix(x, num.mcmc, p)})
  Sigma.mcmc.list <- lapply(rep(0, num.chains), FUN = function(x) {matrix(x, nrow = num.mcmc, ncol = p*(p+1)/2)})
  Beta.mcmc.list <- lapply(rep(0, num.chains), FUN = function(x) {array(x, dim = c(num.mcmc, p, m))})
  
  #priors - mean
  if(length(mu0 == 1)){
    mu0 <- rep(mu0, p)
  } else{
    if(length(m0 == p)){
      mu0 <- mu0
    } else{
      stop("Prior mean format not understood; specify as vector of prior means of length p or a single number representing the prior mean of each beta.")
    }
  }
  
  if(is.matrix(Lambda0)){
    Lambda0 <- Lambda0
  } else{
    if(length(Lambda0) == 1){
      Lambda0 <- Lambda0 * diag(p)
    } else{
      stop("Prior variance format not understood; specify as a p x p matrix or a single number representing the prior variance of the betas")
    }
  }
  if(dim(Lambda0)[2] != p){
    stop("Dimension of prior variance matrix does not match number of groups.")
  }
  if(dim(Lambda0)[1] != p){
    stop("Dimension of prior variance matrix does not match number of groups.")
  }
  Lambda0.inv <- solve(Lambda0)
  
  #priors - variance
  if(length(eta0) != 1) {stop("Degrees of freedom parameter of Inverse Wishart must be of length 1.")}
  if(eta0 < p) {stop(bquote('Degrees of freedom parameter of Inverse Wishart must be greater than dimension of S0 = '~.(p)))}
  
  if(length(S0 == 1)){
    S0 <- S0 * diag(p)
  } else{
    if(is.matrix(S0)){
      S0 <- S0
    }
  }
  if(dim(S0)[2] != p){
    stop("Dimension of prior scale matrix does not match number of regressors.")
  }
  if(dim(S0)[1] != p){
    stop("Dimension of prior scale matrix does not match number of regressors.")
  }
  
  #baseline response category
  if(is.null(baseline)){
    baseline <- 1
  } else{
    if(is.character(baseline)){
      baseline <- which(colnames(y) == baseline)
    } else{
      if(length(baseline) == 1){
        baseline <- baseline
      } else{
        stop("Format of baseline category not understood; specify as a level of the response or numeric column reference.")
      }
    }
  }
  
  #define sets of betas by variable
  X.attr <- attributes(X)$assign
  col.ref.var <- rep(X.attr, k)
  accept.ratio <- array(0, dim = c(num.mcmc, length(unique(col.ref.var)), m))
  blocks <- unique(col.ref.var) + 1
  
  #step size
  if(is.null(step.size)){
    step.size <- rep(.01, length(blocks))
  } else{
    if(length(step.size) == 1){
      step.size <- rep(step.size, length(blocks))
    } else{
      if(length(step.size) != length(blocks)){
        stop("Step size must have length of one or equal to the number of blocks in the Gibbs sampler.")
      } else{
        step.size <- step.size
      }
    }
  }
  
  #label variables
  resp <- rep(colnames(y), each = p/k)
  ex <- rep(colnames(X), k)
  names <- paste(resp, rep("-", p), ex)
  
  #initalize MCMC
  if(all(ini.beta == 0)){
    NULL
  } else{
    if(all(ini.beta == "random")){
      for(q in 1:num.chains){
        Beta.mcmc.list[[q]][1,,] <- rnorm(p, ini.beta.mean, ini.beta.var)
      }
    } else{
      if(length(ini.beta) == m*p){
        for(q in 1:num.chains){
          Beta.mcmc.list[[q]][1,,] <- ini.beta
        }
      } else{
        stop("Initialization parameters not understood. Should be either 0 to initialize the MCMC at 0s or random to initialize at a random starting location.")
      }
    }
  }
  
  if(all(ini.theta == 0)){
    NULL
  } else {
    if(all(ini.theta == "random")){
      for(q in 1:num.chains){
        theta.mcmc.list[[q]][1,] <- rnorm(p, ini.theta.mean, ini.theta.var)
      }
    } else{
      if(length(ini.theta) == p){
        for(q in 1:num.chains){
          theta.mcmc.list[[q]][1,] <- ini.theta
        }
      } else{
        stop("Initialization parameters not understood. Should be either 0 to initialize the MCMC at 0s or random to initialize at a random starting location.")
      }
    }
  }
  
  if(all(ini.Sigma == 1)){
    for(q in 1:num.chains){
      tmp.Sigma <- diag(p)
      up.tri <- gdata::upperTriangle(tmp.Sigma)
      varcov.diag <- diag(tmp.Sigma)
      
      Sigma.mcmc.list[[q]][1,1:(p*(p-1)/2)]  <- up.tri
      Sigma.mcmc.list[[q]][1,(p*(p-1)/2 + 1):(p*(p+1)/2)] <- varcov.diag
    }
  } else{
    if(length(ini.Sigma) == p*(p+1)/2){
      for(q in 1:num.chains){
        Sigma.mcmc.list[[q]][1,1:(p*(p-1)/2)]  <- ini.Sigma[1:(p*(p-1)/2)]
        Sigma.mcmc.list[[q]][1,(p*(p-1)/2 + 1):(p*(p+1)/2)] <- ini.Sigma[(p*(p-1)/2 + 1):(p*(p+1)/2)]
      }
    }
  }
  
  
  for(b in 1:num.chains){
    message('Beginning sampler. Please be patient.')
    
    Beta.mcmc <- Beta.mcmc.list[[b]]
    theta.mcmc <- theta.mcmc.list[[b]]
    Sigma.mcmc <- Sigma.mcmc.list[[b]]
    
    for(i in 2:num.mcmc){
      
      #sample theta conditional on Sigma, beta
      Sigma <- diag(Sigma.mcmc[i-1,(p*(p-1)/2 + 1):(p*(p+1)/2)])
      gdata::upperTriangle(Sigma) <- Sigma.mcmc[i-1,1:(p*(p-1)/2)]
      gdata::lowerTriangle(Sigma, byrow = T) <- Sigma.mcmc[i-1,1:(p*(p-1)/2)]
      #Sigma <- matrix(Sigma.mcmc[i-1,], p, p)
      Sigma.inv <- solve(Sigma)
      beta.bar <- rowMeans(Beta.mcmc[i-1,,])
      
      Lambda.m <- solve(Lambda0.inv + m*Sigma.inv)
      mu.m <- Lambda.m %*% (Lambda0.inv %*% matrix(mu0,p,1) + m*Sigma.inv %*% matrix(beta.bar, p, 1))
      theta.mcmc[i,] <- mnormt::rmnorm(1, mu.m, Lambda.m)
      
      #sample Sigma conditional on theta, beta
      tmp.s.theta <- sapply(c(1:m), FUN = function(x){Beta.mcmc[i-1,,x] - theta.mcmc[i,]})
      tmp <- array(0, dim = c(p,p,m))
      for(n in 1:dim(tmp.s.theta)[2]){
        tmp[,,n] <- tmp.s.theta[,n] %*% t(tmp.s.theta[,n])
      }
      S.theta <- apply(tmp, c(1:2), sum)
      Sn <- solve(S0 + S.theta)
      
      Sigma <- round(MCMCpack::riwish(eta0 + m, Sn), 6)
      
      Sigma.mcmc[i,1:(p*(p-1)/2)] <- gdata::upperTriangle(Sigma)
      Sigma.mcmc[i,(p*(p-1)/2 + 1):(p*(p+1)/2)] <- diag(Sigma)
      
      for(m in 1:m){
        for(j in 1:length(blocks)){
          y <- y.list[[m]]
          X <- X.list[[m]]
          Beta.star <- Beta.mcmc[i-1,,m] + ifelse(col.ref.var == (j-1), 1, 0) * mnormt::rmnorm(1, 0, step.size[j]*diag(p))
          
          #compute r
          Beta.current <- matrix(Beta.mcmc[i-1,,m], p/k, k)
          Beta.current <- Beta.current - Beta.current[,baseline]
          tmp <- exp(X %*% Beta.current)
          pi.current <- tmp / rowSums(tmp)
          
          Beta.star <- matrix(Beta.star, p/k, k)
          Beta.star <- Beta.star - Beta.star[,baseline]
          tmp <- exp(X %*% Beta.star)
          pi.star <- tmp / rowSums(tmp)
          
          log.p.current <- sum((y * log(pi.current))) + mnormt::dmnorm(x = as.numeric(Beta.current), mean = theta.mcmc[i,], varcov = Sigma, log = T)
          log.p.star <- sum((y * log(pi.star))) + mnormt::dmnorm(x = as.numeric(Beta.star), mean = theta.mcmc[i,], varcov = Sigma, log = T)
          
          log.r <- log.p.star - log.p.current
          
          if(j < length(blocks)){
            if (log(runif(1)) < log.r){
              Beta.mcmc[i-1,,m] <- Beta.star
              accept.ratio[i,j,m] <- 1
            } else{
              Beta.mcmc[i-1,,m] <- Beta.mcmc[i-1,,m]
            }
          } else{
            if (log(runif(1)) < log.r){
              Beta.mcmc[i,,m] <- Beta.star
              accept.ratio[i,length(blocks), m] <- 1
            } else{
              Beta.mcmc[i,,m] <- Beta.mcmc[i-1,,m]
            }
          }
        }
      }
      
      if(silent == F){
        #cat(bquote(.(round(i/(num.mcmc), 5)*100)), "% through chain", bquote(.(b)), "\n")
        if (i %% 500 == 0) {
          message('Progress: ', bquote(.(i)), 'th iteration of chain ', bquote(.(b)))
          message(bquote(.(round(i/(num.mcmc), 5)*100)),'% through chain ',bquote(.(b)))
        }
      }
    }
    
    Beta.mcmc <- Beta.mcmc[burnin:num.mcmc,,]
    dimnames(Beta.mcmc)[[3]] <- levels(data[,hier.id])
    dimnames(Beta.mcmc)[[2]] <- names
    
    Beta.mcmc.list[[b]] <- Beta.mcmc
    #mcmc <- coda::mcmc(Beta.mcmc, start = burnin, end = num.mcmc)
    
    #mcmc.list[[b]] <- mcmc
    theta.mcmc.list[[b]] <- theta.mcmc[burnin:num.mcmc,]
    Sigma.mcmc.list[[b]] <- Sigma.mcmc[burnin:num.mcmc,]
  }
  #mcmc.list <- coda::as.mcmc.list(mcmc.list)
  #theta.mcmc.list
  accept.ratio.mat <- colMeans(accept.ratio)
  colnames(accept.ratio.mat) <- levels(data[,hier.id])
  rownames(accept.ratio.mat) <- c("intercept", names(mf)[-1])
  list(Beta.mcmc.list, theta.mcmc.list, Sigma.mcmc.list, accept.ratio.mat)
}
bayes.mlogit <- function(formula, baseline = NULL, data, num.mcmc = 10000, step.size = NULL, b0 = 0, bvar0 = 100, silent = T,
                         burnin = 1, ini.beta = 0, ini.mean = 0, ini.var = 1, seed = NULL, num.chains = 1){
  
  #seed
  set.seed(seed)
  
  #model
  formula <- as.formula(formula)
  mf <- model.frame(formula = formula, data = data)
  X <- model.matrix(formula, data = data)
  y <- dummies::dummy(model.response(mf))
  colnames(y) <- as.vector(sort(unique(model.response(mf))))
  
  #setup sampler
  k <- dim(y)[2]
  p <- k*dim(X)[2]
  Beta.mcmc.list <- lapply(rep(0, num.chains), FUN = function(x) {matrix(x, num.mcmc, p)})
  
  #prior variance
  if(is.matrix(bvar0)){
    beta.prior.var <- bvar0
  } else{
    if(length(bvar0) == 1){
      beta.prior.var <- bvar0 * diag(p)
    } else{
      stop("Prior variance format not understood; specify as a p x p matrix or a single number representing the prior variance of each beta.")
    }
  }
  if(dim(beta.prior.var)[2] != p){
    stop("Dimension of prior variance matrix does not match dimension of beta vector.")
  }
  if(dim(beta.prior.var)[1] != p){
    stop("Dimension of prior variance matrix does not match dimension of beta vector.")
  }
  
  #prior mean
  if(length(b0) == 1){
    b0 <- b0
  } else{
    if(length(b0) == p){
      b0 <- b0
    } else{
      stop("Prior mean format not understood; specify as vector of prior means of length p or a single number representing the prior mean of each beta.")
    }
  }
  
  #baseline response category
  if(is.null(baseline)){
    baseline <- 1
  } else{
    if(is.character(baseline)){
      baseline <- which(colnames(y) == baseline)
    } else{
      if(length(baseline) == 1){
        baseline <- baseline
      } else{
        stop("Format of baseline category not understood; specify as a level of the response or numeric column reference.")
      }
    }
  }
  
  #define sets of betas by variable
  X.attr <- attributes(X)$assign
  col.ref.var <- rep(X.attr, k)
  accept.ratio <- matrix(0, num.mcmc, length(unique(col.ref.var)))
  blocks <- unique(col.ref.var) + 1
  
  #step size
  if(is.null(step.size)){
    step.size <- rep(.01, length(blocks))
  } else{
    if(length(step.size) == 1){
      step.size <- rep(step.size, length(blocks))
    } else{
      if(length(step.size) != length(blocks)){
        stop("Step size must have length of one or equal to the number of blocks in the Gibbs sampler.")
      } else{
        step.size <- step.size
      }
    }
  }
  
  #label variables
  resp <- rep(colnames(y), each = p/k)
  ex <- rep(colnames(X), k)
  names <- paste(resp, rep("-", p), ex)
  
  #initalize MCMC
  if(ini.beta == 0){
    NULL
  } else{
    if(ini.beta == "random"){
      for(q in 1:num.chains){
        Beta.mcmc.list[[q]][1,] <- rnorm(p, ini.mean, ini.var)
      }
    } else{
      stop("Initialization parameters not understood. Should be either 0 to initialize the MCMC at 0s or random to initialize at a random starting location.")
    }
  }
  
  #sampler
  mcmc.list <- NULL
  for(b in 1:num.chains){
    message('Beginning sampler. Please be patient.')
    
    Beta.mcmc <- Beta.mcmc.list[[b]]
    
    for(i in 2:num.mcmc){
      
      for(j in 1:length(blocks)){
        Beta.star <- Beta.mcmc[i-1,] + ifelse(col.ref.var == (j-1), 1, 0) * mnormt::rmnorm(1, 0, step.size[j]*diag(p))
        
        #compute r
        Beta.current <- matrix(Beta.mcmc[i-1,], p/k, k)
        Beta.current <- Beta.current - Beta.current[,baseline]
        tmp <- exp(X %*% Beta.current)
        pi.current <- tmp / rowSums(tmp)
        
        
        Beta.star <- matrix(Beta.star, p/k, k)
        Beta.star <- Beta.star - Beta.star[,baseline]
        tmp <- exp(X %*% Beta.star)
        pi.star <- tmp / rowSums(tmp)
        
        log.p.current <- sum((y * log(pi.current))) + mnormt::dmnorm(as.numeric(Beta.current), 0, beta.prior.var, log = T)
        log.p.star <- sum((y * log(pi.star))) + mnormt::dmnorm(as.numeric(Beta.star), 0, beta.prior.var, log = T)
        
        log.r <- log.p.star - log.p.current
        
        if(j < length(blocks)){
          if (log(runif(1)) < log.r){
            Beta.mcmc[i-1,] <- Beta.star
            accept.ratio[i,j] <- 1
          } else{
            Beta.mcmc[i-1,] <- Beta.mcmc[i-1,]
          }
        } else{
          if (log(runif(1)) < log.r){
            Beta.mcmc[i,] <- Beta.star
            accept.ratio[i,length(blocks)] <- 1
          } else{
            Beta.mcmc[i,] <- Beta.mcmc[i-1,]
          }
        }
      }
      
      if(silent == F){
        #cat(bquote(.(round(i/(num.mcmc), 5)*100)), "% through chain", bquote(.(b)), "\n")
        if (i %% 500 == 0) {
          message('Progress: ', bquote(.(i)), 'th iteration of chain ', bquote(.(b)))
          message(bquote(.(round(i/(num.mcmc), 5)*100)),'% through chain ',bquote(.(b)))
        }
      }
    }
    
    Beta.mcmc <- Beta.mcmc[burnin:num.mcmc,]
    colnames(Beta.mcmc) <- names
    mcmc <- coda::mcmc(Beta.mcmc, start = burnin, end = num.mcmc)
    
    mcmc.list[[b]] <- mcmc
  }
  mcmc.list <- coda::as.mcmc.list(mcmc.list)
}
pi.calc <- function(beta.vect, nresp, X){
  beta.mat <- matrix(beta.vect, length(beta.vect)/nresp, nresp)
  tmp <- exp(X %*% beta.mat)
  tmp/rowSums(tmp)
}
dmultlog.mat <- function(x, prob){
  size <- sum(x)
  norm.num <- lgamma(size + 1)
  norm.denom <- lgamma(x + 1)
  norm.con <- norm.num - sum(norm.denom)
  tmp <- NULL
  for(i in 1:(dim(prob)[1])){
    tmp[i] <- norm.con + sum(x * log(prob[i,]))
  }
  tmp
}
pack <- c("dplyr", "pitchRx", "DBI", "XML2R", "readr", "MASS", "ggplot2", "spatstat", "lattice", "car", "dummies", "mnormt", "BayesMultReg", "coda")
suppressMessages(suppressWarnings(lapply(pack, require, character.only = TRUE, quietly = T)))

#cond.bf <- rep(0, 50)
for(z in 1:50){
  
  ## CREATE DATA
  {
    #set.seed(239012)
    theta.true <- sample(c(-1:1), size = 9, replace = T)
    Beta.true <- mnormt::rmnorm(3, theta.true, diag(9))
    
    g1.beta.mat.true <- matrix(Beta.true[1,], 3, 3)
    g2.beta.mat.true <- matrix(Beta.true[2,], 3, 3)
    g3.beta.mat.true <- matrix(Beta.true[3,], 3, 3)
    
    ## group 1
    y <- rep(0, 300)
    str <- c(rep("low", 100), rep("med", 100), rep("high", 100))
    str <- factor(str, levels = c("low", "med", "high"))
    heat <- c(rep("cold", 150), rep("hot", 150))
    test.df <- data.frame(y, str, heat)
    
    X <- model.matrix(~ str, data = test.df)
    tmp <- exp(X%*%g1.beta.mat.true)
    pi.true <- tmp/rowSums(tmp)
    
    resp <- t(sapply(1:nrow(pi.true), FUN = function(x){rmultinom(1, 1, prob = pi.true[x,])}))
    temp.resp <- resp * cbind(rep(1,300), rep(2, 300), rep(3, 300))
    resp.vect <- rowSums(temp.resp)
    cat.resp.vect<- ifelse(resp.vect == 1, "none", ifelse(resp.vect == "2", "partial", "full"))
    cat.resp.vect <- factor(cat.resp.vect, levels = c("none", "partial", "full"))
    test.df$y <- cat.resp.vect
    group1.df <- test.df
    
    ## group 2
    y <- rep(0, 300)
    str <- c(rep("low", 100), rep("med", 100), rep("high", 100))
    str <- factor(str, levels = c("low", "med", "high"))
    heat <- c(rep("cold", 150), rep("hot", 150))
    test.df <- data.frame(y, str, heat)
    
    X <- model.matrix(~ str, data = test.df)
    tmp <- exp(X%*%g2.beta.mat.true)
    pi.true2 <- tmp/rowSums(tmp)
    
    resp <- t(sapply(1:nrow(pi.true2), FUN = function(x){rmultinom(1, 1, prob = pi.true2[x,])}))
    temp.resp <- resp * cbind(rep(1,300), rep(2, 300), rep(3, 300))
    resp.vect <- rowSums(temp.resp)
    cat.resp.vect<- ifelse(resp.vect == 1, "none", ifelse(resp.vect == "2", "partial", "full"))
    cat.resp.vect <- factor(cat.resp.vect, levels = c("none", "partial", "full"))
    test.df$y <- cat.resp.vect
    group2.df <- test.df
    
    ## group 3
    y <- rep(0, 300)
    str <- c(rep("low", 100), rep("med", 100), rep("high", 100))
    str <- factor(str, levels = c("low", "med", "high"))
    heat <- c(rep("cold", 150), rep("hot", 150))
    test.df <- data.frame(y, str, heat)
    
    X <- model.matrix(~ str, data = test.df)
    tmp <- exp(X%*%g3.beta.mat.true)
    pi.true3 <- tmp/rowSums(tmp)
    
    resp <- t(sapply(1:nrow(pi.true3), FUN = function(x){rmultinom(1, 1, prob = pi.true3[x,])}))
    temp.resp <- resp * cbind(rep(1,300), rep(2, 300), rep(3, 300))
    resp.vect <- rowSums(temp.resp)
    cat.resp.vect<- ifelse(resp.vect == 1, "none", ifelse(resp.vect == "2", "partial", "full"))
    cat.resp.vect <- factor(cat.resp.vect, levels = c("none", "partial", "full"))
    test.df$y <- cat.resp.vect
    group3.df <- test.df
    
    df <- rbind(group1.df, group2.df, group3.df)
    df$group <- c(rep(1,300), rep(2,300), rep(3,300))
    df$group <- as.factor(df$group)
    
    g1.beta.mat.true;g2.beta.mat.true;g3.beta.mat.true
    unique(pi.true);unique(pi.true2);unique(pi.true3)
  }
  
  ##Run modelS
  hier.mod <- bayes.mlogit.hier(formula = y ~ str, baseline = "full", data = df, num.mcmc = 50000, silent = F, mu0 = 0,
                                Lambda0 = 100, eta0 = 10, S0 = 1, burnin = 1, ini.beta = "random", ini.theta = 0, ini.Sigma = 1,
                                ini.beta.mean = 0, ini.beta.var = 1, ini.theta.mean = 0, ini.theta.var = 1, seed = NULL,
                                num.chains = 1, hierarchy = "group")
  Beta.mcmc <- hier.mod[[1]][[1]]
  
  
  #ignore hierarchy
  ig.hier.mod <- bayes.mlogit(formula = y ~ str, data = test.df, baseline = "full", num.mcmc = 50000,
                              b0 = 0, bvar0 = 9, ini.beta = "random", num.chains = 1, silent = F)
  ig.hier.mcmc <- as.matrix(ig.hier.mod[[1]])
  
  
  ##Compare models
  #new data to validate on
  {
    ## group 1
    y <- rep(0, 300)
    str <- c(rep("low", 100), rep("med", 100), rep("high", 100))
    str <- factor(str, levels = c("low", "med", "high"))
    heat <- c(rep("cold", 150), rep("hot", 150))
    test.df <- data.frame(y, str, heat)
    
    X <- model.matrix(~ str, data = test.df)
    #tmp <- exp(X%*%g1.beta.mat.true)
    #pi.true <- tmp/rowSums(tmp)
    
    resp <- t(sapply(1:nrow(pi.true), FUN = function(x){rmultinom(1, 1, prob = pi.true[x,])}))
    temp.resp <- resp * cbind(rep(1,300), rep(2, 300), rep(3, 300))
    resp.vect <- rowSums(temp.resp)
    cat.resp.vect<- ifelse(resp.vect == 1, "none", ifelse(resp.vect == "2", "partial", "full"))
    cat.resp.vect <- factor(cat.resp.vect, levels = c("none", "partial", "full"))
    test.df$y <- cat.resp.vect
    group1.val.df <- test.df
    
    ## group 2
    y <- rep(0, 300)
    str <- c(rep("low", 100), rep("med", 100), rep("high", 100))
    str <- factor(str, levels = c("low", "med", "high"))
    heat <- c(rep("cold", 150), rep("hot", 150))
    test.df <- data.frame(y, str, heat)
    
    X <- model.matrix(~ str, data = test.df)
    #tmp <- exp(X%*%g2.beta.mat.true)
    #pi.true2 <- tmp/rowSums(tmp)
    
    resp <- t(sapply(1:nrow(pi.true2), FUN = function(x){rmultinom(1, 1, prob = pi.true2[x,])}))
    temp.resp <- resp * cbind(rep(1,300), rep(2, 300), rep(3, 300))
    resp.vect <- rowSums(temp.resp)
    cat.resp.vect<- ifelse(resp.vect == 1, "none", ifelse(resp.vect == "2", "partial", "full"))
    cat.resp.vect <- factor(cat.resp.vect, levels = c("none", "partial", "full"))
    test.df$y <- cat.resp.vect
    group2.val.df <- test.df
    
    ## group 3
    y <- rep(0, 300)
    str <- c(rep("low", 100), rep("med", 100), rep("high", 100))
    str <- factor(str, levels = c("low", "med", "high"))
    heat <- c(rep("cold", 150), rep("hot", 150))
    test.df <- data.frame(y, str, heat)
    
    X <- model.matrix(~ str, data = test.df)
    #tmp <- exp(X%*%g3.beta.mat.true)
    #pi.true3 <- tmp/rowSums(tmp)
    
    resp <- t(sapply(1:nrow(pi.true3), FUN = function(x){rmultinom(1, 1, prob = pi.true3[x,])}))
    temp.resp <- resp * cbind(rep(1,300), rep(2, 300), rep(3, 300))
    resp.vect <- rowSums(temp.resp)
    cat.resp.vect<- ifelse(resp.vect == 1, "none", ifelse(resp.vect == "2", "partial", "full"))
    cat.resp.vect <- factor(cat.resp.vect, levels = c("none", "partial", "full"))
    test.df$y <- cat.resp.vect
    group3.val.df <- test.df
    
    df.val <- rbind(group1.val.df, group2.val.df, group3.val.df)
    df.val$group <- c(rep(1,300), rep(2,300), rep(3,300))
    df.val$group <- as.factor(df.val$group)
  }
  
  # BAYES FACTOR
  mf <- model.frame(y ~ str, data = df)
  X <- model.matrix(y ~ str, data = df)
  resp <- dummies::dummy(model.response(mf))
  colnames(resp) <- as.vector(sort(unique(model.response(mf))))
  
  y <- df %>% group_by(str) %>% summarise(freq = n())
  X <- model.matrix(~ str, data = y)
  
  # GET PI'S
  {
    #non-hierarchical
    pis <- array(0, dim = c(dim(ig.hier.mcmc)[1],3,dim(X)[1]))
    for(i in 1:dim(ig.hier.mcmc)[1]){
      for(j in 1:dim(X)[1]){
        pis[i,,j] <- pi.calc(ig.hier.mcmc[i,], nresp = 3, X = X[j,]) 
      }
    }
    dimnames(pis) <- list(NULL, levels(df$y), paste(y$str))
    
    #hierarchical
    hier.pis.list <- list(NULL)
    for(m in 1:length(levels(df$group))){
      hier.pis <- array(0, dim = c(dim(Beta.mcmc)[1],3,dim(X)[1]))
      for(i in 1:dim(Beta.mcmc)[1]){
        for(j in 1:dim(X)[1]){
          hier.pis[i,,j] <- pi.calc(Beta.mcmc[i,,m], nresp = 3, X = X[j,]) 
        }
      }
      dimnames(hier.pis) <- list(NULL, levels(df$y), paste(y$str))
      hier.pis.list[[m]] <- hier.pis
    }
  }
  
  # STRUCTURE PI'S IN PROPER ORDER
  {
    X <- model.matrix(y ~ str, data = df)
    design.id <- rep(0, dim(X)[1])
    X.order <- model.matrix(~ str, data = y)
    
    check.equal <- function(x, y){
      isTRUE(all.equal(y, x, check.attributes = FALSE))
    }
    unname(which(apply(X, 1, check.equal, y=X.order[1,])))
    
    for(i in 1:dim(X.order)[1]){
      tmp <- unname(which(apply(X, 1, check.equal, y=X.order[i,])))
      design.id[tmp] <- i
    }
    groups <- df$group
    
    groups.id <- rep(0, length(groups))
    for(i in 1:length(groups)){
      groups.id[i] <- which(groups[i] == levels(df$group))
    }
  }
  
  # CALCULATE BF
  bf <- matrix(0, dim(resp)[1], 2)
  colnames(bf) <- c("non", "hier")
  for(i in 1:dim(resp)[1]){
    id <- design.id[i]
    grou <- groups.id[i]
    tmp.resp <- unname(resp[i,])
    tmp.mat <- matrix(rep(tmp.resp, dim(pis)[1]), nrow = dim(pis)[1], ncol = 3, byrow = T)
    
    non <- sum(colMeans(tmp.mat * log(pis[,,id])))
    hier <- sum(colMeans(tmp.mat * log(hier.pis.list[[grou]][,,id])))
    
    bf[i,] <- c(non, hier)
  }
  
  logbf <- colSums(bf)['hier'] - colSums(bf)['non']
  
  message('z = ', bquote(.(z)))
  cond.bf[z] <- logbf
}

write.csv(data.frame(lbf = cond.bf), file = "sim_cond_lbfs.csv", row.names = F)

with(sim_cond_lbfs, boxplot(2*lbf, horizontal = T, main = "2log(BF) for Hierarchical Multinomial Data", xlab = "2log(BF)"))