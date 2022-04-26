---
title: "RBNY Data Investigation"
author: "Jason Whitelaw"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: TRUE
---

# R Setup




```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
## ✓ tibble  3.1.6     ✓ dplyr   1.0.8
## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
## ✓ readr   2.1.1     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(tidymodels)
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 0.2.0 ──
```

```
## ✓ broom        0.7.12     ✓ rsample      0.1.1 
## ✓ dials        0.1.1      ✓ tune         0.2.0 
## ✓ infer        1.0.0      ✓ workflows    0.2.6 
## ✓ modeldata    0.1.1      ✓ workflowsets 0.2.1 
## ✓ parsnip      0.2.1      ✓ yardstick    0.0.9 
## ✓ recipes      0.2.0
```

```
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## x scales::discard() masks purrr::discard()
## x dplyr::filter()   masks stats::filter()
## x recipes::fixed()  masks stringr::fixed()
## x dplyr::lag()      masks stats::lag()
## x yardstick::spec() masks readr::spec()
## x recipes::step()   masks stats::step()
## • Use suppressPackageStartupMessages() to eliminate package startup messages
```


```r
rbny_data <- read_csv("Dataset_For_Intern_Test.csv")
```

```
## Rows: 6956 Columns: 41
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): footedness, player_position
## dbl (39): player_id, team_id, match_id, minutes, touches, team_possession_pe...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

# Data Manipulation

## Compute Stats per 90


```r
rbny_data_clean <- rbny_data %>%
  mutate_each(funs(per_90 = (./minutes)*90), -player_id, -team_id, -match_id, -footedness, -player_position, -minutes, -team_possession_percentage, -xg_per_shot)
```

```
## Warning: `funs()` was deprecated in dplyr 0.8.0.
## Please use a list of either functions or lambdas: 
## 
##   # Simple named list: 
##   list(mean = mean, median = median)
## 
##   # Auto named with `tibble::lst()`: 
##   tibble::lst(mean, median)
## 
##   # Using lambdas
##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

```
## Warning: `mutate_each_()` was deprecated in dplyr 0.7.0.
## Please use `across()` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

## Add Position Categories


```r
rbny_data_clean <- rbny_data_clean %>%
  mutate(player_position_broad = case_when(player_position == "Right Center Back" | player_position == "Center Back" | player_position == "Left Center Back" ~ "CB/RCB/LCB", player_position == "Left Wing Back" | player_position == "Right Wing Back" ~ "RWB/LWB", player_position == "Left Back" | player_position == "Right Back" ~ "RB/LB", player_position == "Center Midfield" | player_position == "Right Center Midfield" | player_position == "Left Center Midfield" ~ "CM/RCM/LCM", player_position == "Left Midfield" | player_position == "Right Midfield" ~ "RM/LM", player_position == "Center Defensive Midfield" | player_position == "Left Defensive Midfield" | player_position == "Right Defensive Midfield" ~ "CDM/RDM/LDM", player_position == "Center Attacking Midfield" | player_position == "Right Attacking Midfield" | player_position == "Left Attacking Midfield" ~ "CAM/RAM/LAM", player_position == "Center Forward" | player_position == "Left Center Forward" | player_position == "Right Center Forward" ~ "CF/RCF/LCF", player_position == "Left Wing" | player_position == "Right Wing" ~ "RW/LW"))
```

## Compute Non-Penalty Goal Differential


```r
rbny_data_clean <- rbny_data_clean %>%
  group_by(match_id, team_id) %>%
  mutate(team_non_penalty_goals = sum(non_penalty_goals)) %>%
  ungroup() %>%
  group_by(match_id) %>%
  mutate(opp_team_non_penalty_goals = sum(non_penalty_goals) - team_non_penalty_goals) %>%
  mutate(team_gd = team_non_penalty_goals - opp_team_non_penalty_goals) %>%
  ungroup() 
```

## Normalize Data


```r
rbny_data_regression <- rbny_data_clean %>%
  select(ends_with("per_90"), team_gd, player_position_broad) %>%
  group_by(player_position_broad) %>%
  mutate_each(funs((.-min(.))/(max(.)-min(.)))) %>%
  ungroup()
```

## Perform Linear Regression


```r
rbny_data_CB_RCB_LCB <- rbny_data_regression %>%
  filter(player_position_broad == "CB/RCB/LCB") %>%
  select(-player_position_broad)

rbny_data_RWB_LWB <- rbny_data_regression %>%
  filter(player_position_broad == "RWB/LWB") %>%
  select(-player_position_broad)

rbny_data_RB_LB <- rbny_data_regression %>%
  filter(player_position_broad == "RB/LB") %>%
  select(-player_position_broad)

rbny_data_CM_RCM_LCM <- rbny_data_regression %>%
  filter(player_position_broad == "CM/RCM/LCM") %>%
  select(-player_position_broad)

rbny_data_RM_LM <- rbny_data_regression %>%
  filter(player_position_broad == "RM/LM") %>%
  select(-player_position_broad)

rbny_data_CDM_RDM_LDM <- rbny_data_regression %>%
  filter(player_position_broad == "CDM/RDM/LDM") %>%
  select(-player_position_broad)

rbny_data_CAM_RAM_LAM <- rbny_data_regression %>%
  filter(player_position_broad == "CAM/RAM/LAM") %>%
  select(-player_position_broad)

rbny_data_CF_RCF_LCF <- rbny_data_regression %>%
  filter(player_position_broad == "CF/RCF/LCF") %>%
  select(-player_position_broad)

rbny_data_RW_LW <- rbny_data_regression %>%
  filter(player_position_broad == "RW/LW") %>%
  select(-player_position_broad)
```


```r
lm_spec <- 
    linear_reg() %>% 
    set_engine(engine = 'lm') %>% 
    set_mode('regression')
```


```r
CB_RCB_LCB_mod <- fit(lm_spec, team_gd ~ ., data=rbny_data_CB_RCB_LCB)

RB_LB_mod <- fit(lm_spec, team_gd ~ ., data=rbny_data_RB_LB)

RWB_LWB_mod <- fit(lm_spec, team_gd ~ ., data=rbny_data_RWB_LWB)

CM_RCM_LCM_mod <- fit(lm_spec, team_gd ~ ., data=rbny_data_CM_RCM_LCM)

RM_LM_mod <- fit(lm_spec, team_gd ~ ., data=rbny_data_RM_LM)

CDM_RDM_LDM_mod <- fit(lm_spec, team_gd ~ ., data=rbny_data_CDM_RDM_LDM)

CAM_RAM_LAM_mod <- fit(lm_spec, team_gd ~ ., data=rbny_data_CAM_RAM_LAM)

CF_RCF_LCF_mod <- fit(lm_spec, team_gd ~ ., data=rbny_data_CF_RCF_LCF)

RW_LW_mod <- fit(lm_spec, team_gd ~ ., data=rbny_data_RW_LW)
```

## Find Significant Coefficients and Compute Weighted Average Value


```r
CB_RCB_LCB_mod %>% tidy() %>% filter(p.value<.05) %>%
  filter(term!="(Intercept)") %>%
  mutate(weighted_average_value = estimate*1/sum(estimate))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["weighted_average_value"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"open_play_assists_per_90","2":"0.16036340","3":"0.05930066","4":"2.704243","5":"6.933999e-03","6":"0.9564470"},{"1":"non_penalty_goals_per_90","2":"0.40115194","3":"0.18220670","4":"2.201631","5":"2.786388e-02","6":"2.3925694"},{"1":"open_play_key_passes_per_90","2":"-0.51340185","3":"0.24394813","4":"-2.104553","5":"3.551887e-02","6":"-3.0620556"},{"1":"open_play_xg_buildup_per_90","2":"0.37432150","3":"0.05553515","4":"6.740262","5":"2.356044e-11","6":"2.2325460"},{"1":"turnovers_per_90","2":"-0.27057792","3":"0.12656740","4":"-2.137817","5":"3.271484e-02","6":"-1.6137936"},{"1":"clearances_per_90","2":"0.10525003","3":"0.04695728","4":"2.241400","5":"2.516585e-02","6":"0.6277372"},{"1":"aerials_per_90","2":"-0.08944135","3":"0.04097935","4":"-2.182596","5":"2.924133e-02","6":"-0.5334504"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
RB_LB_mod %>% tidy() %>% filter(p.value<.05) %>%
  filter(term!="(Intercept)") %>%
  mutate(weighted_average_value = estimate*1/sum(estimate))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["weighted_average_value"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"open_play_assists_per_90","2":"0.1714640","3":"0.04622863","4":"3.709044","5":"2.238504e-04","6":"0.1334756"},{"1":"forward_passes_per_90","2":"0.1648768","3":"0.06393167","4":"2.578954","5":"1.010491e-02","6":"0.1283478"},{"1":"expected_assists_per_90","2":"0.1519754","3":"0.06679032","4":"2.275411","5":"2.317071e-02","6":"0.1183048"},{"1":"open_play_xg_buildup_per_90","2":"0.4703774","3":"0.09485195","4":"4.959069","5":"8.818988e-07","6":"0.3661637"},{"1":"counterpressure_duration_total_per_90","2":"0.3259160","3":"0.16152083","4":"2.017795","5":"4.397848e-02","6":"0.2537082"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
RWB_LWB_mod %>% tidy() %>% filter(p.value<.05) %>%
  filter(term!="(Intercept)") %>%
  mutate(weighted_average_value = estimate*1/sum(estimate))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["weighted_average_value"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"open_play_assists_per_90","2":"0.3177152","3":"0.11249965","4":"2.824144","5":"0.004921238","6":"0.5778550"},{"1":"non_penalty_goals_per_90","2":"0.1623686","3":"0.07751158","4":"2.094766","5":"0.036671414","6":"0.2953133"},{"1":"open_play_xg_buildup_per_90","2":"0.1901602","3":"0.07414467","4":"2.564718","5":"0.010603373","6":"0.3458601"},{"1":"dispossessions_per_90","2":"-0.1204258","3":"0.05055366","4":"-2.382138","5":"0.017568241","6":"-0.2190284"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
CM_RCM_LCM_mod %>% tidy() %>% filter(p.value<.05) %>%
  filter(term!="(Intercept)") %>%
  mutate(weighted_average_value = estimate*1/sum(estimate))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["weighted_average_value"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"open_play_assists_per_90","2":"0.2323143","3":"0.07942516","4":"2.924946","5":"3.602600e-03","6":"3.654176"},{"1":"non_penalty_goals_per_90","2":"0.3285177","3":"0.12676067","4":"2.591637","5":"9.833001e-03","6":"5.167401"},{"1":"forward_passes_per_90","2":"-0.1761208","3":"0.07754564","4":"-2.271189","5":"2.356241e-02","6":"-2.770283"},{"1":"sideways_passes_per_90","2":"-0.4816392","3":"0.13303182","4":"-3.620481","5":"3.242173e-04","6":"-7.575919"},{"1":"open_play_xg_buildup_per_90","2":"0.3164302","3":"0.07078459","4":"4.470326","5":"9.683441e-06","6":"4.977272"},{"1":"turnovers_per_90","2":"-0.1559272","3":"0.04765193","4":"-3.272211","5":"1.141309e-03","6":"-2.452648"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
RM_LM_mod %>% tidy() %>% filter(p.value<.05) %>%
  filter(term!="(Intercept)") %>%
  mutate(weighted_average_value = estimate*1/sum(estimate))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["weighted_average_value"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"open_play_assists_per_90","2":"0.2602045","3":"0.1020117","4":"2.550732","5":"0.01201836","6":"1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
CDM_RDM_LDM_mod %>% tidy() %>% filter(p.value<.05) %>%
  filter(term!="(Intercept)") %>%
  mutate(weighted_average_value = estimate*1/sum(estimate))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["weighted_average_value"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"open_play_assists_per_90","2":"0.1628371","3":"0.07398882","4":"2.200834","5":"2.796301e-02","6":"0.5595563"},{"1":"non_penalty_goals_per_90","2":"0.3204331","3":"0.10961655","4":"2.923219","5":"3.538022e-03","6":"1.1011026"},{"1":"forward_passes_per_90","2":"-0.1872089","3":"0.06759934","4":"-2.769389","5":"5.714266e-03","6":"-0.6433048"},{"1":"sideways_passes_per_90","2":"-0.4410553","3":"0.17345301","4":"-2.542794","5":"1.113840e-02","6":"-1.5155959"},{"1":"open_play_xg_buildup_per_90","2":"0.2188197","3":"0.04786489","4":"4.571612","5":"5.410090e-06","6":"0.7519290"},{"1":"interceptions_per_90","2":"0.1055592","3":"0.04642198","4":"2.273905","5":"2.317116e-02","6":"0.3627324"},{"1":"ball_recoveries_per_90","2":"0.1116262","3":"0.05109301","4":"2.184764","5":"2.912477e-02","6":"0.3835804"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
CAM_RAM_LAM_mod %>% tidy() %>% filter(p.value<.05) %>%
  filter(term!="(Intercept)") %>%
  mutate(weighted_average_value = estimate*1/sum(estimate))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["weighted_average_value"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"open_play_assists_per_90","2":"0.3948565","3":"0.09475860","4":"4.166973","5":"3.718844e-05","6":"0.3336212"},{"1":"non_penalty_goals_per_90","2":"0.2937345","3":"0.13245652","4":"2.217592","5":"2.709398e-02","6":"0.2481814"},{"1":"non_penalty_shots_per_90","2":"-0.2489556","3":"0.09278595","4":"-2.683117","5":"7.569451e-03","6":"-0.2103469"},{"1":"open_play_shots_per_90","2":"0.2691087","3":"0.09670963","4":"2.782646","5":"5.624298e-03","6":"0.2273746"},{"1":"open_play_xg_buildup_per_90","2":"0.3396226","3":"0.10292027","4":"3.299861","5":"1.046175e-03","6":"0.2869531"},{"1":"interceptions_per_90","2":"0.1351807","3":"0.06176131","4":"2.188761","5":"2.914032e-02","6":"0.1142166"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
CF_RCF_LCF_mod %>% tidy() %>% filter(p.value<.05) %>%
  filter(term!="(Intercept)") %>%
  mutate(weighted_average_value = estimate*1/sum(estimate))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["weighted_average_value"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"touches_per_90","2":"0.6041027","3":"0.13888152","4":"4.349770","5":"1.499238e-05","6":"0.8891045"},{"1":"open_play_assists_per_90","2":"0.1905646","3":"0.05972650","4":"3.190620","5":"1.462881e-03","6":"0.2804686"},{"1":"non_penalty_goals_per_90","2":"0.2960672","3":"0.05705737","4":"5.188938","5":"2.549658e-07","6":"0.4357449"},{"1":"non_penalty_shots_per_90","2":"-0.4605002","3":"0.21964316","4":"-2.096584","5":"3.627557e-02","6":"-0.6777537"},{"1":"passes_per_90","2":"-0.4858987","3":"0.13428715","4":"-3.618356","5":"3.109796e-04","6":"-0.7151346"},{"1":"non_penalty_shots_on_target_per_90","2":"0.1085437","3":"0.04572605","4":"2.373783","5":"1.779075e-02","6":"0.1597522"},{"1":"aggressive_actions_per_90","2":"0.4265715","3":"0.18104858","4":"2.356116","5":"1.865499e-02","6":"0.6278182"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
RW_LW_mod %>% tidy() %>% filter(p.value<.05) %>%
  filter(term!="(Intercept)") %>%
  mutate(weighted_average_value = estimate*1/sum(estimate))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["weighted_average_value"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"open_play_assists_per_90","2":"0.2435972","3":"0.04581402","4":"5.317088","5":"1.317842e-07","6":"0.3995673"},{"1":"non_penalty_goals_per_90","2":"0.4912388","3":"0.10372170","4":"4.736123","5":"2.514148e-06","6":"0.8057686"},{"1":"open_play_key_passes_per_90","2":"-0.1827042","3":"0.07418166","4":"-2.462929","5":"1.395902e-02","6":"-0.2996858"},{"1":"open_play_xg_buildup_per_90","2":"0.1575647","3":"0.04271950","4":"3.688356","5":"2.386532e-04","6":"0.2584501"},{"1":"counterpressures_per_90","2":"-0.1594195","3":"0.07718182","4":"-2.065506","5":"3.914879e-02","6":"-0.2614924"},{"1":"op_half_ball_recoveries_per_90","2":"-0.1272253","3":"0.05186545","4":"-2.452988","5":"1.434823e-02","6":"-0.2086850"},{"1":"ball_recoveries_per_90","2":"0.1866007","3":"0.05547239","4":"3.363849","5":"7.996955e-04","6":"0.3060772"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## Comptue Final Player Score


```r
rbny_data_final <- rbny_data %>%
  mutate(player_position_broad = case_when(player_position == "Right Center Back" | player_position == "Center Back" | player_position == "Left Center Back" ~ "CB/RCB/LCB", player_position == "Left Wing Back" | player_position == "Right Wing Back" ~ "RWB/LWB", player_position == "Left Back" | player_position == "Right Back" ~ "RB/LB", player_position == "Center Midfield" | player_position == "Right Center Midfield" | player_position == "Left Center Midfield" ~ "CM/RCM/LCM", player_position == "Left Midfield" | player_position == "Right Midfield" ~ "RM/LM", player_position == "Center Defensive Midfield" | player_position == "Left Defensive Midfield" | player_position == "Right Defensive Midfield" ~ "CDM/RDM/LDM", player_position == "Center Attacking Midfield" | player_position == "Right Attacking Midfield" | player_position == "Left Attacking Midfield" ~ "CAM/RAM/LAM", player_position == "Center Forward" | player_position == "Left Center Forward" | player_position == "Right Center Forward" ~ "CF/RCF/LCF", player_position == "Left Wing" | player_position == "Right Wing" ~ "RW/LW"))
```


```r
rbny_data_final <- rbny_data_final %>%
  group_by(player_id, player_position_broad) %>%
  mutate_each(funs(per_90 = (sum(.)/sum(minutes))*90), -team_id, -match_id, -footedness, -player_position, -minutes, -team_possession_percentage, -xg_per_shot) %>%
  ungroup()
```


```r
rbny_data_final <- rbny_data_final %>%
  select(ends_with("per_90"), player_id, player_position_broad, minutes) %>%
  unique() %>%
  group_by(player_position_broad) %>%
  mutate_each(funs((.-min(.))/(max(.)-min(.))), -player_id, -minutes) %>%
  ungroup()
```


```r
rbny_data_final <- rbny_data_final %>%
  mutate(player_score = case_when(player_position_broad == "CB/RCB/LCB" ~ (open_play_assists_per_90*0.9564470 + non_penalty_goals_per_90*2.3925694 + open_play_key_passes_per_90*-3.0620556 + open_play_xg_buildup_per_90*2.2325460 + turnovers_per_90*-1.6137936 + clearances_per_90*0.6277372 + aerials_per_90*-0.5334504)/7, player_position_broad == "RB/LB" ~ (open_play_assists_per_90*0.1334756 + forward_passes_per_90*0.1283478 + expected_assists_per_90*0.1183048 + open_play_xg_buildup_per_90*0.3661637 + counterpressure_duration_total_per_90*0.2537082)/5, player_position_broad == "RWB/LWB" ~ (open_play_assists_per_90*0.5778550 + non_penalty_goals_per_90*0.2953133 + open_play_xg_buildup_per_90*0.3458601 + dispossessions_per_90*-0.2190284)/4, player_position_broad == "CM/RCM/LCM" ~ (open_play_assists_per_90*3.654176 + non_penalty_goals_per_90*5.167401 + forward_passes_per_90*-2.770283 + sideways_passes_per_90*-7.575919 + open_play_xg_buildup_per_90*4.977272 + turnovers_per_90*-2.452648)/6, player_position_broad == "RM/LM" ~ (open_play_assists_per_90*1)/1, player_position_broad == "CDM/RDM/LDM" ~ (open_play_assists_per_90*0.5595563 + non_penalty_goals_per_90*1.1011026 + forward_passes_per_90*-0.6433048 + sideways_passes_per_90*-1.5155959 + open_play_xg_buildup_per_90*0.7519290 + interceptions_per_90*0.3627324 + ball_recoveries_per_90*0.3835804)/7, player_position_broad == "CAM/RAM/LAM" ~ (open_play_assists_per_90*0.3336212 + non_penalty_goals_per_90*0.2481814 + non_penalty_shots_per_90*-0.2103469 + open_play_shots_per_90*0.2273746 + open_play_xg_buildup_per_90*0.2869531 + interceptions_per_90*0.1142166)/6, player_position_broad == "CF/RCF/LCF" ~ (touches_per_90*0.8891045 + open_play_assists_per_90*0.2804686 + non_penalty_goals_per_90*0.4357449 + non_penalty_shots_per_90*-0.6777537 + passes_per_90*-0.7151346 + non_penalty_shots_on_target_per_90*0.1597522 + aggressive_actions_per_90*0.6278182)/7, player_position_broad == "RW/LW" ~ (open_play_assists_per_90*0.3995673 + non_penalty_goals_per_90*0.8057686 + open_play_key_passes_per_90*-0.2996858 + open_play_xg_buildup_per_90*0.2584501 + counterpressures_per_90*-0.2614924 + op_half_ball_recoveries_per_90*-0.2086850 + ball_recoveries_per_90*0.3060772)/7))
```


```r
rbny_data_final <- rbny_data_final %>%
  select(player_id, player_score, minutes, player_position_broad) %>%
  group_by(player_id, player_position_broad) %>%
  mutate(total_minutes = sum(minutes)) %>%
  ungroup() %>%
  select(player_id, player_score, total_minutes) %>%
  unique() %>%
  group_by(player_id) %>%
  mutate(weighted_player_score = player_score*(total_minutes/sum(total_minutes))) %>%
  mutate(final_player_score = sum(weighted_player_score)/n()) %>%
  ungroup() %>%
  select(player_id, final_player_score) %>%
  unique()
```

# My Top 5 Players


```r
rbny_data_final %>%
  arrange(desc(final_player_score)) %>%
  top_n(final_player_score, n = 5)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["player_id"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["final_player_score"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"296505","2":"0.11772541"},{"1":"180105","2":"0.10318366"},{"1":"307705","2":"0.08249046"},{"1":"22105","2":"0.07058995"},{"1":"206105","2":"0.06872875"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

# Summary and Analysis

When deciding how to select my top 5 players that fit the Red Bull philosophy, I wanted to be as objective as possible, and not have my biases influence the final result. While I could pick out certain statistics that I believe make a successful Red Bulls player, at the end of the day, I believe the goal of the Red Bulls is to be a successful organization, and the players help facilitate that. For that reason, I chose to model goal differential as a function of all the provided statistics. Using my model, I could see which statistics impacted goal differential the most at each position, to find the most impact players on the pitch at their respective positions. I believe these players would fit the Red Bull philosophy the most as they are contributing to create a successful organization.

## Methodology
* Compute all the statistics per 90 minutes.
* Group the players by similar position into 9 categories (CB/RCB/LCB, RB/LB, RWB/LWB, CM/RCM/LCM, RM/LM, CDM/RDM/LDM, CAM/RAM/LAM, CF/RCF/LCF, RW/LW).
* Compute non-penalty goal differential for each game using the non_penalty_goals and match_id data.
* Normalize all per 90 statistics.
* Perform linear regression for each position group, where the outcome variable is non-penalty goal differential and the predictors are all the normalized per 90 statistics.
* Find all predictors with p-values less than 0.05.
* Compute a weighted_average_value from the estimates that can be used to compute a final weighted average of the relevant predictors.
* Compute final player score using a weighted average of relevant predictor estimates times per 90 statistics, weighted by the number of minutes played at the position category.

## Future Steps

While this is one way to find the top 5 players in the data set, there are future steps that could be taken to improve upon this method. Firstly, linear regression assumes a linear relationship between all predictor variables. Additionally, the model could be overfit to the data set. Future work could include using different models, for example GAMs or Splines which don't imply a linear relationship, and using cross-validation which helps prevent overfitting.
