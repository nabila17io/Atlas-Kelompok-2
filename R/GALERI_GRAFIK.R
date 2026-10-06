# ============================================================
# GALERI GRAFIK STATISTIK
# KEMISKINAN DAN TPT SULAWESI SELATAN
# PERIODE 2023-2025
# ============================================================


# ============================================================
# 1. PACKAGE
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)


# ============================================================
# 2. MEMBACA DATA
# ============================================================

data <- readRDS(
  "C:/Users/User/Downloads/data_utama/data_bersih.rds"
)


# ============================================================
# 3. PALET WARNA
# ============================================================

warna_kemiskinan <- "#8B5A2B"
warna_tpt <- "#D2A679"


# ============================================================
# 4. TEMA GRAFIK
# ============================================================

tema_tim <- theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      color = "#8B5A2B"
    ),
    
    plot.subtitle = element_text(
      size = 9
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    
    legend.position = "top"
  )


# ============================================================
# 5. MENYIAPKAN DATA
# ============================================================

data_long <- data %>%
  select(
    kabupaten_kota,
    tahun,
    kemiskinan,
    pengangguran
  ) %>%
  pivot_longer(
    cols = c(
      kemiskinan,
      pengangguran
    ),
    names_to = "indikator",
    values_to = "nilai"
  )

data_long$indikator <- recode(
  data_long$indikator,
  kemiskinan = "Kemiskinan",
  pengangguran = "TPT"
)


# ============================================================
# GRAFIK 1
# SEBARAN KEMISKINAN DAN TPT
# ============================================================

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
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    size = 2,
    alpha = 0.6
  ) +
  scale_fill_manual(
    values = c(
      "Kemiskinan" = warna_kemiskinan,
      "TPT" = warna_tpt
    )
  ) +
  labs(
    title = "Sebaran Kemiskinan dan TPT",
    x = NULL,
    y = "Persentase (%)"
  ) +
  tema_tim

print(grafik_1)


# ============================================================
# GRAFIK 2
# SEBARAN KEMISKINAN PER TAHUN
# VIOLIN
# ============================================================

grafik_2 <- ggplot(
  data,
  aes(
    x = factor(tahun),
    y = kemiskinan
  )
) +
  geom_violin(
    fill = "#F5EBDD",
    color = warna_kemiskinan,
    linewidth = 0.8
  ) +
  geom_boxplot(
    width = 0.15,
    fill = "white"
  ) +
  geom_jitter(
    width = 0.08,
    size = 2,
    alpha = 0.7
  ) +
  labs(
    title = "Sebaran Kemiskinan",
    subtitle = "Violin = sebaran, kotak = median dan kuartil",
    x = "Tahun",
    y = "Tingkat kemiskinan (%)"
  ) +
  tema_tim +
  theme(
    plot.title = element_text(
      size = 13
    ),
    plot.subtitle = element_text(
      size = 8
    )
  )

print(grafik_2)


# ============================================================
# GRAFIK 3
# SEBARAN TPT PER TAHUN
# BOXPLOT
# ============================================================

grafik_3 <- ggplot(
  data,
  aes(
    x = factor(tahun),
    y = pengangguran
  )
) +
  geom_boxplot(
    width = 0.5,
    fill = "#F5EBDD",
    color = warna_tpt,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.08,
    size = 2,
    alpha = 0.7
  ) +
  labs(
    title = "Sebaran TPT",
    subtitle = "Kotak = median dan kuartil",
    x = "Tahun",
    y = "TPT (%)"
  ) +
  tema_tim +
  theme(
    plot.title = element_text(
      size = 13
    ),
    plot.subtitle = element_text(
      size = 8
    )
  )

print(grafik_3)


# ============================================================
# GRAFIK 4
# TREN KEMISKINAN 24 KABUPATEN/KOTA
# ============================================================

grafik_4 <- ggplot(
  data,
  aes(
    x = tahun,
    y = kemiskinan,
    group = kabupaten_kota
  )
) +
  geom_line(
    color = warna_kemiskinan,
    linewidth = 0.8
  ) +
  geom_point(
    color = warna_kemiskinan,
    size = 2
  ) +
  facet_wrap(
    ~ kabupaten_kota,
    ncol = 6
  ) +
  labs(
    title = "Tren Kemiskinan 24 Kabupaten/Kota",
    subtitle = "Periode 2023-2025",
    x = "Tahun",
    y = "Kemiskinan (%)"
  ) +
  tema_tim +
  theme(
    strip.text = element_text(
      size = 7,
      face = "bold"
    ),
    axis.text = element_text(
      size = 7
    ),
    axis.title = element_text(
      size = 9
    ),
    panel.spacing = unit(
      0.7,
      "lines"
    )
  )

print(grafik_4)


# ============================================================
# GRAFIK 5
# TREN TPT 24 KABUPATEN/KOTA
# ============================================================

grafik_5 <- ggplot(
  data,
  aes(
    x = tahun,
    y = pengangguran,
    group = kabupaten_kota
  )
) +
  geom_line(
    color = warna_tpt,
    linewidth = 0.8
  ) +
  geom_point(
    color = warna_tpt,
    size = 2
  ) +
  facet_wrap(
    ~ kabupaten_kota,
    ncol = 6
  ) +
  labs(
    title = "Tren TPT 24 Kabupaten/Kota",
    subtitle = "Periode 2023-2025",
    x = "Tahun",
    y = "TPT (%)"
  ) +
  tema_tim +
  theme(
    strip.text = element_text(
      size = 7,
      face = "bold"
    ),
    axis.text = element_text(
      size = 7
    ),
    axis.title = element_text(
      size = 9
    ),
    panel.spacing = unit(
      0.7,
      "lines"
    )
  )

print(grafik_5)


# ============================================================
# GRAFIK 6
# HUBUNGAN TPT DAN KEMISKINAN
# ============================================================

grafik_6 <- ggplot(
  data,
  aes(
    x = pengangguran,
    y = kemiskinan
  )
) +
  geom_point(
    color = warna_tpt,
    size = 2.5,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#1A1A1A",
    fill = "#D2A679"
  ) +
  labs(
    title = "Hubungan TPT dan Kemiskinan",
    subtitle = "Garis regresi dan selang kepercayaan 95%",
    x = "TPT (%)",
    y = "Tingkat kemiskinan (%)"
  ) +
  tema_tim

print(grafik_6)


# ============================================================
# GRAFIK 7
# PERINGKAT KEMISKINAN TAHUN 2025
# ============================================================

data_2025 <- data %>%
  filter(
    tahun == 2025
  ) %>%
  arrange(
    kemiskinan
  )

grafik_7 <- ggplot(
  data_2025,
  aes(
    x = kemiskinan,
    y = reorder(
      kabupaten_kota,
      kemiskinan
    )
  )
) +
  geom_segment(
    aes(
      x = 0,
      xend = kemiskinan,
      yend = reorder(
        kabupaten_kota,
        kemiskinan
      )
    ),
    color = "#D2A679"
  ) +
  geom_point(
    color = warna_kemiskinan,
    size = 3
  ) +
  labs(
    title = "Peringkat Kemiskinan 2025",
    subtitle = "24 kabupaten/kota berdasarkan tingkat kemiskinan",
    x = "Tingkat kemiskinan (%)",
    y = NULL
  ) +
  tema_tim +
  theme(
    axis.text.y = element_text(
      size = 8
    ),
    plot.title = element_text(
      size = 14
    ),
    plot.subtitle = element_text(
      size = 9
    )
  )

print(grafik_7)


# ============================================================
# GRAFIK 8
# KOMPOSISI MULTIPANEL
# ============================================================
# Grafik 8 hanya berisi:
# - Sebaran Kemiskinan
# - Sebaran TPT
# - Hubungan TPT dan Kemiskinan
# - Tren Kemiskinan 24 wilayah
# - Tren TPT 24 wilayah
#
# Grafik 7 tidak dimasukkan ke Grafik 8
# ============================================================


bagian_atas <- (
  grafik_2 |
    grafik_3 |
    grafik_6
) +
  plot_layout(
    widths = c(
      1,
      1,
      1.25
    )
  )


bagian_bawah <- (
  grafik_4 |
    grafik_5
) +
  plot_layout(
    widths = c(
      1,
      1
    )
  )


grafik_8 <- (
  bagian_atas /
    bagian_bawah
) +
  plot_layout(
    heights = c(
      1,
      1.8
    )
  ) +
  plot_annotation(
    title = "Kemiskinan dan TPT di 24 Kabupaten/Kota Sulawesi Selatan",
    subtitle = "Periode 2023-2025",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        color = "#8B5A2B",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 11,
        hjust = 0.5
      )
    )
  )

print(grafik_8)


# ============================================================
# 6. MEMBUAT FOLDER GALLERY GRAFIK
# ============================================================

folder_output <- "C:/Users/User/Downloads/data_utama/gallery_grafik"

dir.create(
  folder_output,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 7. MENYIMPAN SEMUA GRAFIK
# ============================================================

ggsave(
  paste0(
    folder_output,
    "/01_sebaran_kemiskinan_tpt.png"
  ),
  grafik_1,
  width = 8,
  height = 6,
  dpi = 300
)


ggsave(
  paste0(
    folder_output,
    "/02_sebaran_kemiskinan.png"
  ),
  grafik_2,
  width = 8,
  height = 6,
  dpi = 300
)


ggsave(
  paste0(
    folder_output,
    "/03_sebaran_tpt.png"
  ),
  grafik_3,
  width = 8,
  height = 6,
  dpi = 300
)


ggsave(
  paste0(
    folder_output,
    "/04_tren_kemiskinan.png"
  ),
  grafik_4,
  width = 12,
  height = 9,
  dpi = 300
)


ggsave(
  paste0(
    folder_output,
    "/05_tren_tpt.png"
  ),
  grafik_5,
  width = 12,
  height = 9,
  dpi = 300
)


ggsave(
  paste0(
    folder_output,
    "/06_hubungan_tpt_kemiskinan.png"
  ),
  grafik_6,
  width = 8,
  height = 6,
  dpi = 300
)


ggsave(
  paste0(
    folder_output,
    "/07_peringkat_kemiskinan_2025.png"
  ),
  grafik_7,
  width = 9,
  height = 10,
  dpi = 300
)


ggsave(
  paste0(
    folder_output,
    "/08_komposisi_patchwork.png"
  ),
  grafik_8,
  width = 18,
  height = 16,
  dpi = 300,
  bg = "white"
)


# ============================================================
# SELESAI
# ============================================================