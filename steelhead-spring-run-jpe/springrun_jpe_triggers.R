library(knitr)
library(kableExtra)
library(lubridate)
library(data.table)
library(tidyverse)
library(scales)
library(viridis)
library(forcats)
library(reshape2)
library(png)
library(dataRetrieval)
library(rerddap)
library(plotly)
library(marked)
library(lubridate)
library(busdater)
library(janitor)
library(officer)
library(flextable)

# 1) your URLs, in order WY 2025 → 2018
spring_urls <- c(
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086641_386.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086688_891.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086696_521.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149443_627.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149458_112.html",  # duplicate?
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149481_432.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149491_216.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149503_26.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149533_767.html", #17
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149591_244.html", #16
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149612_13.html", #14
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149653_972.html" #13
)

late_urls <- c(
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086753_625.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086761_928.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086768_958.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086778_313.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086786_833.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086794_80.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086801_114.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747086809_530.html",
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149731_841.html", #17
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149751_672.html", #16
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149763_294.html", #15
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149773_744.html", #14
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149792_513.html", #13
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149834_748.html", #12
  "https://www.cbr.washington.edu/sacramento/tmp/deltacwttable_1747149843_633.html" #11
)

# 2) helper to build a tibble of WY, run, html → csv
build_index <- function(urls, run_type) {
  tibble(
    WY       = seq(2025, 2011, by = -1)[seq_along(urls)],
    run      = run_type,
    html_url = urls,
    csv_url  = sub("\\.html$", ".csv", html_url)
  )
}

idx <- bind_rows(
  build_index(spring_urls,   "Spring"),
  build_index(late_urls,     "Late-Fall")
)

# 3) pull every CSV into a list-column
all_data <- idx %>%
  mutate(
    data = map(csv_url, ~ read_csv(.x, show_col_types = FALSE))
  ) %>%
  unnest(data)

# 4) filter for the two hatcheries & select your columns
target <- all_data %>%
  filter(
    `CWT Tag Race` %in% c("Spring", "Late-Fall"),
    Hatchery        %in% c("Coleman NFH", "Feather River Hatchery")
  ) %>%
  select(
    WY,
    run = `CWT Tag Race`,
    Hatchery,
    `CWT Number Released`
  ) %>%
  arrange(run, WY)

# 5) view
print(target)

#target$no_no_CWT <- target$`CWT Number Released`*(1-(target$`% CWT Marked of Total Number Released`/100))
#target$actual_no <- target$`CWT Number Released` + target$no_no_CWT

# ── Load WY types early ───────────────────────────────────────────────────────
wy_types <- read_csv(
  'https://www.cbr.washington.edu/sacramento/data/php/rpt/hci.php?sc=1&outputFormat=csv&classification=Official',
  show_col_types = FALSE
) %>%
  clean_names() %>%
  filter(basin == "Sacramento Valley") %>%
  select(wy, water_year_type = code) %>%
  mutate(wy = as.integer(wy)) %>%
  # add WY 2025 as Above Normal (AN)
  bind_rows(
    tibble(wy = 2025, water_year_type = "AN")
  ) %>%
  arrange(wy)


# ── Base survival lookup ──────────────────────────────────────────────────────
surv_lookup <- tibble(
  Hatchery     = c(rep("Feather River Hatchery",6), rep("Coleman NFH",3)),
  run          = c(rep("Spring",6), rep("Late-Fall",3)),
  WY           = c(2025,2024,2023, 2021,2020,2019, 2021,2020,2019),
  survival_est = c(40.5,30.8,40.6, 49.4,26.8,28.6, 14.3,60.4,23.0)
) %>%
  # join in WY type
  left_join(wy_types, by = c("WY" = "wy"))

# A) Build surv_lookup_filled from your explicit estimates
surv_lookup_filled <- surv_lookup %>%
  # just carry survival_est straight into surv_fill
  mutate(surv_fill = survival_est) %>%
  select(Hatchery, run, WY, surv_fill)

# B) Now build target2, joining in those known values, then filling
target2 <- target %>%
  # 1) attach WY_type only if you still need it downstream (optional)
  left_join(wy_types, by = c("WY" = "wy")) %>%
  # 2) bring in the known surv_fill 
  left_join(surv_lookup_filled, by = c("Hatchery","run","WY")) %>%
  # 3) for any hatchery without its own year‐specific surv_fill,
  #    replace NA with that hatchery’s mean
  group_by(Hatchery) %>%
  mutate(
    surv_fill = if_else(
      is.na(surv_fill),
      round(mean(surv_fill, na.rm = TRUE), 1),
      surv_fill
    ),
    # 4) compute JPE
    JPE = round((surv_fill/100) * as.numeric(`CWT Number Released`))
  ) %>%
  ungroup()

# C) Finally, your annual totals and thresholds
target3 <- target2 %>%
  group_by(WY) %>%
  summarize(
    JPE_total = sum(JPE, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    JPE_0.5pct  = round(JPE_total * 0.005),
    JPE_0.75pct = round(JPE_total * 0.0075),
    JPE_1pct    = round(JPE_total * 0.01)
  )

print(target3)

# ── 1) Read the single “juv_loss_detail” CSV ────────────────────────────────
loss_all <- read_csv(
  "https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3At&dnaOnly=no&age=no",
  show_col_types = FALSE
) %>%
  clean_names()

# ── 2) Clean names & parse columns ──────────────────────────────────────────
salmon_filt <- loss_all %>%
  clean_names() %>%
  mutate(
    date_str    = str_trim(str_extract(sample_time, "^[^ ]+")),
    parsed_date = parse_date_time(date_str, orders = c("mdy","ymd"), exact = FALSE),
    # **convert to Date** here
    parsed_date = as_date(parsed_date),
    WY          = if_else(month(parsed_date) >= 10,
                          year(parsed_date) + 1,
                          year(parsed_date)),
    adipose_clip= toupper(str_trim(adipose_clip)),
    cwt_race    = toupper(str_trim(cwt_race)),
    loss        = as.numeric(loss),
    hatchery    = cwt_hatch
  ) %>%
  filter(
    !is.na(parsed_date),
    (hatchery=="CNFH" & str_detect(cwt_race,"(?i)^late[- ]?fall$")) |
      (hatchery=="FRH"  & str_detect(cwt_race,"(?i)^spring$"))
  )

# ── 3) Your annual JPE totals & thresholds (from earlier) ──────────────────
# target3 must be a tibble with WY, JPE_total, JPE_0.5pct, JPE_0.75pct, JPE_1pct
# e.g.:
# target3 <- tibble(WY=2019:2025, JPE_total=..., JPE_0.5pct=..., ...)

# ── 4) Join JPE thresholds & compute cumulative‐loss ────────────────────────
salmon_cum <- salmon_filt %>%
  left_join(target3, by = "WY") %>%
  filter(JPE_total > 0) %>%
  arrange(WY, parsed_date) %>%
  group_by(WY) %>%
  mutate(
    cum_loss    = cumsum(loss),
    thresh_0.5  = JPE_0.5pct,
    thresh_0.75 = JPE_0.75pct,
    thresh_1    = JPE_1pct
  ) %>%
  ungroup()


# 1) Compute “day_of_wy” for every detection date
salmon_cum2 <- salmon_cum %>%
  mutate(
    # water‐year start is Oct 1 of WY-1
    wy_start    = as_date(sprintf("%d-10-01", WY - 1)),
    day_of_wy   = as.integer(parsed_date - wy_start)
  )

# 2) Summary table for flextable with reframe()
summary_tbl <- salmon_cum2 %>%
  group_by(WY) %>%
  summarise(
    JPE             = unique(JPE_total),
    Total_Loss      = max(cum_loss, na.rm = TRUE),
    Proportion_Loss = round(100 * Total_Loss / JPE, 2),
    
    `50% Trigger` = if (any(cum_loss >= thresh_0.5)) {
      format(min(parsed_date[cum_loss >= thresh_0.5]), "%b %d")
    } else {
      "-"
    },
    
    `75% Trigger` = if (any(cum_loss >= thresh_0.75)) {
      format(min(parsed_date[cum_loss >= thresh_0.75]), "%b %d")
    } else {
      "-"
    },
    
    `100% Trigger` = if (any(cum_loss >= thresh_1)) {
      format(min(parsed_date[cum_loss >= thresh_1]), "%b %d")
    } else {
      "-"
    },
    
    .groups = "drop"
  ) %>%
  arrange(WY) %>%
  mutate(WY = as.character(WY))

# 1) Define breaks every 30 days from 0 to 240
x_breaks <- seq(0, 240, by = 30)

# 2) Create 9 labels for Oct–Jun
x_labels <- c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun")

# 2) melt thresholds into a single aesthetic for legend
plot_df <- salmon_cum2 %>%
  select(WY, day_of_wy, cum_loss, thresh_0.5, thresh_0.75, thresh_1) %>%
  pivot_longer(
    cols = starts_with("thresh_"),
    names_to  = "threshold",
    values_to = "yint"
  ) %>%
  mutate(
    threshold = recode(threshold,
                       thresh_0.5  = " 50%",
                       thresh_0.75 = " 75%",
                       thresh_1    = "100%")
  )

# 3) Re‑plot with matching breaks & labels
sr_loss <- ggplot(salmon_cum2, aes(x = day_of_wy, y = cum_loss)) +
  geom_line() +
  geom_hline(aes(yintercept = thresh_0.5),  linetype = "dashed")  +
  geom_hline(aes(yintercept = thresh_0.75), linetype = "dotdash") +
  geom_hline(aes(yintercept = thresh_1),    linetype = "dotted")  +
  facet_wrap(~WY, scales = "fixed") +
  scale_x_continuous(
    limits = c(0, 240),
    breaks = x_breaks,
    labels = x_labels
  ) +
  geom_hline(
    data = plot_df %>% distinct(WY, threshold, yint),
    aes(yintercept = yint, linetype = threshold),
    color = "grey40", size = 0.5
  ) +
  scale_linetype_manual(
    name   = "Thresholds",
    values = c("100%" = "solid", " 75%" = "dotdash", " 50%" = "dashed"),
    guide  = guide_legend(reverse = TRUE, keywidth = unit(2, "lines"))
  ) +
  labs(
    x = NULL,
    y = "Cumulative Loss",
    title = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major     = element_line(color = "grey90"),
    panel.grid.minor     = element_blank(),
    strip.background     = element_rect(fill = "grey80", color = NA),
    strip.text           = element_text(face = "bold"),
    legend.position      = "bottom",
    legend.title.align   = 0.5,
    axis.text.x          = element_text(size = 9),
    axis.text.y          = element_text(size = 9)
  )

sr_loss
