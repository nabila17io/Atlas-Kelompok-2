
# ============================================================
# DAY 4–6 — THEME, PALET WARNA, GALERI GRAFIK, DAN PATCHWORK
# ============================================================

# ------------------------------------------------------------
# DAY 4 — THEME TIM DAN PALET WARNA
# ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Membaca data bersih hasil Day 2–3
data <- readRDS(
  "data/bersih/data_tpt_kemiskinan_sulsel.rds"
)

# Palet warna tim
palet_tim <- c(
  Kemiskinan = "#6BAED6",
  TPT = "#F4A261"
)

palet_tahun <- c(
  "2023" = "#8ECAE6",
  "2024" = "#F6BD60",
  "2025" = "#A8DADC"
)

# Fungsi theme_tim()
theme_tim <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 15,
        colour = "#355C7D",
        margin = margin(b = 8)
      ),
      axis.title = element_text(
        face = "bold",
        colour = "#333333"
      ),
      axis.text = element_text(
        colour = "#333333"
      ),
      panel.grid.major = element_line(
        colour = "#E5E7EB",
        linewidth = 0.4
      ),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.background = element_rect(
        fill = "white",
        colour = "#BFC5CC",
        linewidth = 0.8
      ),
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      plot.margin = margin(10, 10, 10, 10)
    )
}


# ------------------------------------------------------------
# DAY 5 — MENYIAPKAN DATA UNTUK GALERI
# ------------------------------------------------------------

data_long <- data |>
  select(
    kode_wilayah,
    kabupaten_kota,
    tahun,
    kemiskinan_maret_p0_persen,
    tpt_agustus_persen
  ) |>
  pivot_longer(
    cols = c(
      kemiskinan_maret_p0_persen,
      tpt_agustus_persen
    ),
    names_to = "indikator",
    values_to = "nilai"
  ) |>
  mutate(
    indikator = recode(
      indikator,
      kemiskinan_maret_p0_persen = "Kemiskinan",
      tpt_agustus_persen = "TPT"
    ),
    tahun = as.character(tahun)
  )


# ------------------------------------------------------------
# GRAFIK 1 — DISTRIBUSI KEMISKINAN DAN TPT
# ------------------------------------------------------------

grafik_1 <- ggplot(
  data_long,
  aes(
    x = indikator,
    y = nilai,
    fill = indikator
  )
) +
  geom_boxplot(
    width = 0.5,
    alpha = 0.75,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.55,
    size = 2
  ) +
  scale_fill_manual(
    values = palet_tim
  ) +
  labs(
    title = "Sebaran Kemiskinan dan TPT",
    x = NULL,
    y = "Persentase (%)"
  ) +
  theme_tim()

print(grafik_1)


# ------------------------------------------------------------
# GRAFIK 2 — TREN TPT 24 KABUPATEN/KOTA
# ------------------------------------------------------------

grafik_2 <- ggplot(
  data,
  aes(
    x = tahun,
    y = tpt_agustus_persen,
    group = 1
  )
) +
  geom_line(
    colour = "#6BAED6",
    linewidth = 0.8
  ) +
  geom_point(
    colour = "#F4A261",
    size = 2
  ) +
  facet_wrap(
    ~ kabupaten_kota,
    ncol = 4
  ) +
  labs(
    title = "Tren TPT 24 Kabupaten/Kota",
    x = "Tahun",
    y = "TPT (%)"
  ) +
  theme_tim(base_size = 10) +
  theme(
    legend.position = "none",
    strip.text = element_text(
      size = 8,
      face = "bold",
      colour = "#333333"
    ),
    strip.background = element_rect(
      fill = "#F3F6F8",
      colour = "#BFC5CC",
      linewidth = 0.5
    ),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 9),
    panel.spacing = unit(0.8, "lines")
  )

print(grafik_2)


# ------------------------------------------------------------
# GRAFIK 3 — HUBUNGAN KEMISKINAN DAN TPT
# ------------------------------------------------------------

grafik_3 <- ggplot(
  data,
  aes(
    x = kemiskinan_maret_p0_persen,
    y = tpt_agustus_persen
  )
) +
  geom_point(
    colour = "#6BAED6",
    alpha = 0.70,
    size = 2.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "#F4A261",
    fill = "#F4A261",
    alpha = 0.20
  ) +
  labs(
    title = "Hubungan Kemiskinan dan TPT",
    x = "Kemiskinan (%)",
    y = "TPT (%)"
  ) +
  theme_tim()

print(grafik_3)


# ------------------------------------------------------------
# GRAFIK 4 — DISTRIBUSI TPT ANTAR TAHUN
# ------------------------------------------------------------

grafik_4 <- ggplot(
  data,
  aes(
    x = factor(tahun),
    y = tpt_agustus_persen,
    fill = factor(tahun)
  )
) +
  geom_boxplot(
    width = 0.55,
    alpha = 0.75,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.50,
    size = 2
  ) +
  scale_fill_manual(
    values = palet_tahun
  ) +
  labs(
    title = "Distribusi TPT Antar Tahun",
    x = "Tahun",
    y = "TPT (%)"
  ) +
  theme_tim()

print(grafik_4)


# ------------------------------------------------------------
# DAY 6 — MULTIPANEL PATCHWORK
# ------------------------------------------------------------

grafik_2_panel <- grafik_2 +
  labs(title = NULL) +
  theme(
    plot.margin = margin(6, 6, 6, 6),
    strip.text = element_text(size = 7),
    axis.text.x = element_text(size = 6.5),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 8),
    panel.spacing = unit(0.5, "lines")
  )

grafik_5 <- (
  grafik_1 | grafik_3
) /
  grafik_4 /
  grafik_2_panel +
  plot_layout(
    heights = c(1, 1, 2.4)
  )

print(grafik_5)


# ------------------------------------------------------------
# MENYIMPAN HASIL GALERI
# ------------------------------------------------------------

dir.create(
  "keluaran/galeri",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  "keluaran/galeri/grafik_1_distribusi.png",
  grafik_1,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  "keluaran/galeri/grafik_2_tren_24_kabkota.png",
  grafik_2,
  width = 12,
  height = 13,
  dpi = 300,
  bg = "white"
)

ggsave(
  "keluaran/galeri/grafik_3_hubungan.png",
  grafik_3,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  "keluaran/galeri/grafik_4_tpt_antar_tahun.png",
  grafik_4,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  "keluaran/galeri/grafik_5_multipanel.png",
  grafik_5,
  width = 14,
  height = 15,
  dpi = 300,
  bg = "white"
)

galeri_grafik <- list(
  grafik_1 = grafik_1,
  grafik_2 = grafik_2,
  grafik_3 = grafik_3,
  grafik_4 = grafik_4,
  grafik_5 = grafik_5
)

saveRDS(
  galeri_grafik,
  "keluaran/galeri/galeri_grafik.rds"
)

# ============================================================
# SELESAI DAY 4–6
# ============================================================