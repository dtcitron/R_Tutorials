source("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/Multipatch_data_transform.R")

patch.human.populations = c(302, 0, 0, 0, 0, 0, 0)

directory = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/BI_Macro_Calibration/patch_2083"

human.pathogen.path <- paste0(directory, "/HumanPathogen_Run0.csv")
human.move.path <- paste0(directory, "/HumanMove_Run0.csv")
t4 <- SIP.Conversion.Curves(human.pathogen.path, human.move.path, patch.human.populations, 1825)
ggplot(data = t4) +
  geom_line(mapping = aes(x = time, y = N, color = status)) +
  facet_wrap(~location, ncol = 3, labeller = label_parsed) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff", "#00ff1d"),
                     breaks = c("I", "S", "P"),
                     labels = c("Infected (PR)", "Susceptible", "Protected")) +
  xlim(0,730) + ylim(0,300) +
  xlab("Time") + ylab("N")

# And only for location 2
ggplot(data = t4[location == 2]) +
  geom_line(mapping = aes(x = time, y = N, color = status)) +
  facet_wrap(~location, ncol = 3, labeller = label_parsed) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff", "#00ff1d"),
                     breaks = c("I", "S", "P"),
                     labels = c("Infected (PR)", "Susceptible", "Protected")) +
  xlim(0,730) + ylim(0,300) +
  xlab("Time") + ylab("N")
