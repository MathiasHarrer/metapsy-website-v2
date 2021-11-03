---
# Title, summary, and page position.
linktitle: "Visualisierung"
weight: 6
date: "2021-01-25"

# Page metadata.
title: "Visualisierung der Studienergebnisse"
type: book  # Do not modify.
---

<style>
code{
  color: #2a7792;
}
.hljs{
  font-size: 16px
}
.minih{
  font-size: 1px;
  margin: 0px 0px 0px 0px;
}

.highlight {
    position: relative;
}
.highlight pre {
    padding: 15px;
}
.highlight-copy-btn {
    position: absolute;
    top: 7px;
    right: 7px;
    border: 0;
    border-radius: 4px;
    padding: 5px;
    font-size: 0.7em;
    line-height: 1.8;
    color: #fff;
    background-color: #777;
    min-width: 55px;
    text-align: center;
}
.highlight-copy-btn:hover {
    background-color: #666;
}
</style>

---



## Foliensatz {.minih}

<iframe src="https://drive.google.com/file/d/117bRqbY9wjSy8M7jakcYDjBxtYTMldmF/preview" width="640" height="480" allow="autoplay"></iframe>


<br></br>

## Praxis-Teil

---


### Datenvorbereitung

{{< highlight R >}}
library(ggplot2)
library(see)
library(tidyverse)

# Speichere Anzahl an Personen und Anzahl an
# Imputationssets
m = length(implist)
n = nrow(implist[[1]])
mn = m*n

# Fasse Imputationslist zu einem großen data frame zusammen
do.call(rbind, implist) %>%
  mutate(m = rep(1:m, each = n),
         id = 1:mn) -> data

# Bringe Daten des primären Endpunktes ins Long-Format
data %>%
  select(id, m, group, pss.0,
         pss.1, pss.2) %>%
  pivot_longer(-c(id, group, m),
               names_to = "time",
               names_prefix = "pss.",
               values_to = "pss") -> plot.data

# Jitter hinzufügen (vermeidet Overplotting)
jitter1 = runif(nrow(plot.data), -.05, .05)
jitter2 = runif(nrow(plot.data), -.5, .5)

within(plot.data, {
  time = as.numeric(time)
  group = as.factor(group)
  time.jitter = time + jitter1
  pss.jitter = pss + jitter2
}) -> plot.data


# Splitte Daten nach Zeitpunkten
plot.data %>%
  filter(time != 2) -> plot.data.prepost

plot.data.prepost %>%
  filter(time == 0) -> plot.data.pre

plot.data.prepost %>%
  filter(time == 1) -> plot.data.post

plot.data %>%
  filter(time == 2) -> plot.data.fu
{{< / highlight >}}


<br>

### Generiere Plot (Baseline und Post-Test)

{{< highlight R >}}
ggplot(plot.data.prepost, aes(x = as.factor(time), y = pss, color = group)) +
  geom_blank() +
  geom_point(aes(x = time.jitter+1, y = pss.jitter), alpha = .05) +
  geom_line(aes(group = id, x = time.jitter+1, y = pss.jitter), alpha = .05) +
  geom_violinhalf(data = plot.data.pre,
                  mapping = aes(group = interaction(as.factor(m), group, time), fill = group),
                  size = 0.1, flip = TRUE, position = position_nudge(x = -0.1), alpha = .01) +
  geom_violinhalf(data = plot.data.post,
                  mapping = aes(group = interaction(as.factor(m), group, time), fill = group),
                  size = 0.1, position = position_nudge(x = 0.1), alpha = .01) +
  geom_pointrange(data = plot.data.means %>% filter(time == 0),
                  mapping = aes(x = time+0.925, y = pss, color = group,
                                ymin = lower, ymax = upper), position = position_dodge(0.03)) +
  geom_pointrange(data = plot.data.means %>% filter(time == 1),
                  mapping = aes(x = time+1.075, y = pss, color = group,
                                ymin = lower, ymax = upper), position = position_dodge(0.03)) +
  ylim(c(0, 45)) +
  theme_minimal() +
  guides(fill = guide_legend(title="Gruppe"),
         color = guide_legend(title="Gruppe")) +
  scale_color_manual(labels = c("Kontrolle", "Intervention"), values = c("gray", "dodgerblue")) +
  scale_fill_manual(labels = c("Kontrolle", "Intervention"), values = c("gray", "dodgerblue")) +
  scale_x_discrete(labels = c("Baseline", "Post-Test")) +
  ylab("Perceived Stress (PSS-10 Skala)") +
  xlab("Messzeitpunkt")
{{< / highlight >}}

<br>

### Generiere Plot (Baseline, Post-Test und Follow-up)

{{< highlight R >}}
ggplot(plot.data, aes(x = as.factor(time), y = pss, color = group)) +
  geom_blank() +
  geom_point(aes(x = time.jitter+1, y = pss.jitter), alpha = .05) +
  geom_line(aes(group = id, x = time.jitter+1, y = pss.jitter), alpha = .05) +
  geom_violinhalf(data = plot.data.pre,
                  mapping = aes(group = interaction(as.factor(m), group, time), fill = group),
                  size = 0.1, flip = TRUE, position = position_nudge(x = -0.1), alpha = .01) +
  geom_violinhalf(data = plot.data.fu,
                  mapping = aes(group = interaction(as.factor(m), group, time), fill = group),
                  size = 0.1, position = position_nudge(x = 0.1), alpha = .01) +
  geom_pointrange(data = plot.data.means %>% filter(time == 0),
                  mapping = aes(x = time+0.925, y = pss, color = group,
                                ymin = lower, ymax = upper), position = position_dodge(0.03)) +
  geom_pointrange(data = plot.data.means %>% filter(time == 2),
                  mapping = aes(x = time+1.075, y = pss, color = group,
                                ymin = lower, ymax = upper), position = position_dodge(0.03)) +
  ylim(c(0, 45)) +
  theme_minimal() +
  guides(fill = guide_legend(title="Gruppe"),
         color = guide_legend(title="Gruppe")) +
  scale_color_manual(labels = c("Kontrolle", "Intervention"), values = c("gray", "dodgerblue")) +
  scale_fill_manual(labels = c("Kontrolle", "Intervention"), values = c("gray", "dodgerblue")) +
  scale_x_discrete(labels = c("Baseline", "Post-Test", "Follow-Up")) +
  ylab("Perceived Stress (PSS-10 Skala)") +
  xlab("Messzeitpunkt")
{{< / highlight >}}


<style>
h1 {color: #2a7792;}
</style>