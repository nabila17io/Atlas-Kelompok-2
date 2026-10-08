# ============================================================
# CHECKPOINT 3
# LINEUP 20 PANEL
# HUBUNGAN KEMISKINAN DAN TPT
# SULAWESI SELATAN TAHUN 2025
# ============================================================


# ============================================================
# 1. PACKAGE
# ============================================================
library(tidyverse)
library(ggplot2)
library(nullabor)


# ============================================================
# 2. BACA DATA BERSIH
# ============================================================
data <- readRDS(
  "data/bersih/data_tpt_kemiskinan_sulsel.rds"
)


# ============================================================
# 3. PALET TEMA TIM
# COKLAT - PUTIH - HITAM
# ============================================================
warna_tim <- c(
  coklat       = "#8B5A2B",
  coklat_tua   = "#4E2E12",
  coklat_muda  = "#D2A679",
  coklat_pucat = "#F5EBDD",
  hitam        = "#1A1A1A",
  abu          = "#8C8C8C",
  putih        = "#FFFFFF"
)


# ============================================================
# 4. MEMBUAT FOLDER OUTPUT
# ============================================================
dir.create(
  "keluaran/lineup",
  showWarnings = FALSE,
  recursive = TRUE
)


# ============================================================
# 5. MENYIAPKAN DATA LINEUP TAHUN 2025
# ============================================================
data_lineup <- data %>%
  filter(tahun == 2025) %>%
  select(
    kode_wilayah,
    kabupaten_kota,
    jenis,
    kemiskinan_maret_p0_persen,
    tpt_agustus_persen
  ) %>%
  drop_na(
    kemiskinan_maret_p0_persen,
    tpt_agustus_persen
  )


# ============================================================
# 6. MEMASTIKAN ADA 24 WILAYAH
# ============================================================
stopifnot(
  nrow(data_lineup) == 24
)

# ============================================================
# 7. MEMBUAT LINEUP
# 1 DATA ASLI + 19 DATA PERMUTASI
# ============================================================
set.seed(2026)

lineup_hasil <- nullabor::lineup(
  method = nullabor::null_permute(
    "tpt_agustus_persen"
  ),
  true = data_lineup,
  n = 20
)


# ============================================================
# 8. MENYIMPAN POSISI DATA ASLI
# ============================================================
posisi_asli <- attr(
  lineup_hasil,
  "pos"
)

# ============================================================
# 9. MENYIAPKAN DATA UNTUK GRAFIK
# ============================================================
lineup_display <- lineup_hasil

attr(
  lineup_display,
  "pos"
) <- NULL

lineup_display$.sample <- as.integer(
  lineup_display$.sample
)

# ============================================================
# 10. SIMPAN DATA LINEUP
# ============================================================
saveRDS(
  lineup_display,
  "keluaran/lineup/lineup_20_display_2025.rds"
)

saveRDS(
  list(
    pos = posisi_asli,
    tahun = 2025,
    jumlah_panel = 20,
    jumlah_pengamatan = 24
  ),
  "keluaran/lineup/kunci_lineup_2025.rds"
)

# ============================================================
# 11. WARNA LINEUP
# ============================================================
warna_kabupaten <- warna_tim["coklat"]

warna_kota <- warna_tim["coklat_muda"]

warna_garis <- warna_tim["coklat_tua"]

warna_pita <- warna_tim["coklat_pucat"]

warna_grid <- warna_tim["abu"]

warna_border <- warna_tim["coklat_muda"]

warna_teks <- warna_tim["hitam"]

warna_latar <- warna_tim["putih"]

# ============================================================
# 12. GRAFIK LINEUP 20 PANEL
# ============================================================
p_lineup <- ggplot(
  lineup_display,
  aes(
    x = kemiskinan_maret_p0_persen,
    y = tpt_agustus_persen
  )
) +
  
# ----------------------------------------------------------
# GARIS REGRESI + SK 95%
# ----------------------------------------------------------
geom_smooth(
  method = "lm",
  formula = y ~ x,
  se = TRUE,
  colour = unname(warna_garis),
  fill = unname(warna_pita),
  linewidth = 0.45,
  alpha = 0.8
) +
  
# ----------------------------------------------------------
# TITIK WILAYAH
# Kabupaten = coklat
# Kota = coklat muda
# ----------------------------------------------------------
geom_point(
  aes(
    shape = jenis,
    colour = jenis
  ),
  size = 1.1,
  stroke = 0.4
) +
  
# ----------------------------------------------------------
# 20 PANEL
# 5 KOLOM x 4 BARIS
# ----------------------------------------------------------
facet_wrap(
  ~ .sample,
  ncol = 5,
  nrow = 4
) +
  
# ----------------------------------------------------------
# BENTUK TITIK
# ----------------------------------------------------------
scale_shape_manual(
  values = c(
    Kabupaten = 16,
    Kota = 15
  )
) +
  
# ----------------------------------------------------------
# WARNA TITIK
# ----------------------------------------------------------
scale_colour_manual(
  values = c(
    Kabupaten = unname(warna_kabupaten),
    Kota = unname(warna_kota)
  )
) +
  
# ----------------------------------------------------------
# LABEL
# ----------------------------------------------------------

labs(
  title = "Lineup 20 Panel: Hubungan Kemiskinan dan TPT",
  subtitle = "Sulawesi Selatan, Tahun 2025",
  x = "Kemiskinan (%)",
  y = "TPT (%)",
  shape = "Jenis wilayah",
  colour = "Jenis wilayah"
) +
  
# ----------------------------------------------------------
# TEMA COKLAT - PUTIH - HITAM
# ----------------------------------------------------------
theme_minimal(
  base_size = 9
) +
  
  theme(
    
    # Judul
    plot.title = element_text(
      face = "bold",
      size = 13,
      colour = warna_teks,
      hjust = 0.5
    ),
    
    # Subjudul
    plot.subtitle = element_text(
      size = 9,
      colour = warna_tim["coklat_tua"],
      hjust = 0.5
    ),
    
    # Nomor panel
    strip.text = element_text(
      face = "bold",
      size = 8,
      colour = warna_teks
    ),
    
    # Kotak nomor panel
    strip.background = element_rect(
      fill = warna_latar,
      colour = warna_border,
      linewidth = 0.4
    ),
    
    # Legenda
    legend.position = "top",
    
    legend.title = element_text(
      size = 8,
      colour = warna_teks
    ),
    
    legend.text = element_text(
      size = 8,
      colour = warna_teks
    ),
    
    # Grid
    panel.grid.major = element_line(
      colour = warna_grid,
      linewidth = 0.25
    ),
    
    panel.grid.minor = element_blank(),
    
    # Border panel
    panel.border = element_rect(
      colour = warna_border,
      fill = NA,
      linewidth = 0.35
    ),
    
    # Angka sumbu
    axis.text = element_text(
      size = 6,
      colour = warna_teks
    ),
    
    # Judul sumbu
    axis.title = element_text(
      size = 8,
      face = "bold",
      colour = warna_teks
    ),
    
    # Jarak antar panel
    panel.spacing = grid::unit(
      0.15,
      "lines"
    ),
    
    # Margin
    plot.margin = margin(
      8, 8, 8, 8
    ),
    
    # Background putih
    plot.background = element_rect(
      fill = warna_latar,
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = warna_latar,
      colour = NA
    )
  )

# ============================================================
# 13. TAMPILKAN LINEUP
# ============================================================
p_lineup

# ============================================================
# 14. SIMPAN GAMBAR LINEUP
# ============================================================
ggsave(
  filename = "keluaran/lineup/lineup_20_panel_2025.png",
  plot = p_lineup,
  width = 13,
  height = 7,
  dpi = 300,
  bg = unname(warna_latar)
)


# ============================================================
# 15. MENGHITUNG KEKUATAN HUBUNGAN SETIAP PANEL
# ============================================================
skor_panel <- lineup_display %>%
  group_by(.sample) %>%
  summarise(
    korelasi = cor(
      kemiskinan_maret_p0_persen,
      tpt_agustus_persen
    ),
    .groups = "drop"
  ) %>%
  mutate(
    kekuatan_hubungan = abs(korelasi)
  ) %>%
  arrange(
    desc(kekuatan_hubungan)
  )

# ============================================================
# 16. PANEL DENGAN HUBUNGAN PALING KUAT
# ============================================================
panel_hubungan_terkuat <- skor_panel %>%
  slice(1)

# ============================================================
# 17. TAMPILKAN HASIL
# ============================================================
panel_hubungan_terkuat


# ============================================================
# 18. TAMPILKAN NOMOR PANEL SAJA
# ============================================================
panel_hubungan_terkuat$.sample

