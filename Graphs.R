library('ggthemes')

goldose <- closer_match_history_info[[1]]$
  info$
  frames$
  participantFrames$
  `1`$
  currentGold
goldose

hist(goldose)
hist(goldose, breaks = 20, main = "Neeh")

tim <- closer_match_history_info[[1]]$
  info$
  frames$
  timestamp /
  1000 /
  60 #minutes
tim
# %>%
# second()

neehh <- tibble(goldose, tim)

p <- ggplot(neehh, aes(x = tim, y = goldose)) +
  geom_line() +
  xlab("")

p

game1 <- closer_match_history_info[[1]]
timestampApi <- game1$info$frames$timestamp

participant1 <- game1$info$frames$participantFrames$`1`

participant1$minionsKilled
participant1$currentGold
participant1$totalGold
participant1 %>% names()
timestampApi <- game1$info$frames$timestamp / 1000 / 60

timestampApi

gameParticipantInfo <- function(player) {
  xp <- player$xp
  crG <- player$currentGold
  ms <- player$minionsKilled
  totalGold <- player$totalGold

  return(player$participantId <- tibble(xp, crG, ms, totalGold))
}

playersInfo <- game1$info$frames$participantFrames

playersInfoApplied <- lapply(playersInfo, gameParticipantInfo)

playersInfoApplied$`1`
#   (p <- ggplot(playersInfoApplied$`1`,aes(x = timestampApi)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(vars(xp, crG, ms)))
#   labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents"))
ggplot(playersInfoApplied$`1`, aes(timestampApi, crG)) +
  geom_line() +
  facet_wrap(vars(xp)) +
  theme_dark()
#Rnd - not working right now

(goldOvertime <- ggplot(playersInfoApplied$`1`, aes(x = timestampApi, y = crG)) +
  geom_line() +
  xlab(""))

(xpOverTime <- ggplot(playersInfoApplied$`1`, aes(x = timestampApi, y = xp)) +
  geom_line() +
  xlab(""))




# Graph tutorial - https://youtu.be/HPJn1CMvtmI
# ggplot(data = BOD, mapping = aes(x = Time, y = demand)) +
#   geom_line(color = "blue") +
#   geom_point(size = 5, color = "pink") +
#   geom_smooth()
#
# CO2 %>%
#   ggplot(aes(conc, uptake, color = Treatment)) +
#   geom_point(size = 3, alpha = 0.5) +
#   geom_smooth(method = lm, se = F) +
#   facet_wrap(~Type) +
#   labs(title = "Concentration of co2") +
#   theme_dark()
#
# CO2 %>%
#   ggplot(aes(Treatment, uptake)) +
#   geom_boxplot() +
#   geom_point(alpha = 0.5, aes(size = conc, color = Plant)) +
#   facet_wrap(~Type) +
#   coord_flip() +
#   theme_bw() +
#   labs(title = "Chilled ~ Non-chilled")
#
# mpg %>%
#   filter(cty < 25) %>%
#   ggplot(aes(displ, cty)) +
#   geom_point(aes(color = drv, size = trans), alpha = 0.5) +
#   geom_smooth(method = lm) +
#   facet_wrap(~year, nrow = 1) +
#   labs(x = "Eng size",
#        y = "MPG in the city",
#        title = "Fuel ef")+
#   theme_classic()

playersInfoApplied$`1`

playersCurrentGoldOverTimePointXpMS <- function (plInfo){
  Graph <- plInfo %>%
    ggplot(aes(crG, timestampApi)) +
    geom_point(aes(color = ms, size = xp), alpha = 0.5) +
    geom_smooth(method = lm) +
    theme_calc()

  return(plInfo <-  list(Graph))
}

GoverTime <- lapply(playersInfoApplied,playersCurrentGoldOverTimePointXpMS)

GoverTime

