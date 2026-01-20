length_DF <- aggregate(length_DF$length, by = list(year = length_DF$year), FUN = mean, na.rm = TRUE)
names(length_DF)[2] <- "length"


plt <- ggplot() +
  geom_point(data = length_DF, aes(x = year, y = length)) +
  labs(x = "year", y = "Observed length (cm)") +
  theme_classic(base_size = 15) +
  theme(legend.position = "bottom",
        legend.margin = margin(c(1,1,1,1)))
ggsave("figures/test_FoF_v1.png", width = 25, height = 20, unit = "cm")