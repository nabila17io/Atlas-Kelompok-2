# ============================================================
# SK Bootstrap 95% (B = 1.999)
# Titik estimasi + garis galat yang diurutkan
# ============================================================
setwd("C:/Users/ASUS/Documents")

library(tidyverse)

# ============================================================
# 1. BACA DATA BERSIH
# ============================================================
berkas_data <- "data/bersih/data_tpt_kemiskinan_sulsel.rds"
stopifnot(file.exists(berkas_data))

data_tpt <- readRDS(berkas_data)
stopifnot(is.data.frame(data_tpt))

kolom <- c(
  "kabupaten_kota", "jenis", "tahun",
  "kemiskinan_maret_p0_persen", "tpt_agustus_persen"
)
stopifnot(all(kolom %in% names(data_tpt)))

data_tpt <- data_tpt %>%
  mutate(tahun = as.integer(as.character(tahun)))

# ============================================================
# 2. PALET WARNA TIM
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
# 3. FOLDER OUTPUT
# ============================================================
dir.create("keluaran/lineup", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 4. DATA 2023-2025 UNTUK BOOTSTRAP
# ============================================================
data_boot <- data_tpt %>%
  filter(tahun %in% 2023:2025) %>%
  select(
    kabupaten_kota, jenis, tahun,
    kemiskinan_maret_p0_persen, tpt_agustus_persen
  ) %>%
  pivot_longer(
    cols      = c(kemiskinan_maret_p0_persen, tpt_agustus_persen),
    names_to  = "indikator",
    values_to = "nilai"
  ) %>%
  mutate(
    indikator = case_when(
      indikator == "kemiskinan_maret_p0_persen" ~ "Kemiskinan (%)",
      indikator == "tpt_agustus_persen"         ~ "TPT (%)"
    )
  ) %>%
  drop_na(nilai)

stopifnot(nrow(data_boot) > 0)

# pengecekan: tiap indikator seharusnya 72 baris (24 wilayah x 3 tahun)
print(count(data_boot, indikator, name = "baris"))
print(unique(data_boot$jenis))

# ============================================================
# 5. FUNGSI SK BOOTSTRAP 95% (PERSENTIL, B = 1.999)
# ============================================================
hitung_sk_boot <- function(x, B = 1999, level = 0.95) {
  n <- length(x)
  rata_boot <- replicate(B, mean(sample(x, n, replace = TRUE)))
  a <- (1 - level) / 2
  tibble(
    n        = n,
    estimasi = mean(x),
    bawah    = unname(quantile(rata_boot, a)),
    atas     = unname(quantile(rata_boot, 1 - a))
  )
}

# format angka 2 desimal dengan koma (9,11)
fmt_angka <- function(x) {
  formatC(x, format = "f", digits = 2, decimal.mark = ",")
}

# ============================================================
# 6. METODE autoplot() UNTUK OBJEK sk_bootstrap
# ============================================================
autoplot.sk_bootstrap <- function(object, judul = NULL,
                                  pilih_tahun = NULL,
                                  rapat = FALSE, ...) {
  d <- as_tibble(object)
  ada_tahun <- "tahun" %in% names(d)
  
  if (ada_tahun && !is.null(pilih_tahun)) {
    d <- d %>% filter(tahun %in% pilih_tahun)
  }
  
  daftar_tahun <- if (ada_tahun) sort(unique(d$tahun)) else NULL
  
  d <- d %>%
    mutate(
      kunci = forcats::fct_reorder(
        paste(indikator, if (ada_tahun) tahun else "", kelompok,
              sep = "___"),
        estimasi
      )
    )
  
  if (is.null(judul)) judul <- "SK Bootstrap 95%"
  
  keterangan <- if (is.null(daftar_tahun)) {
    "2023-2025"
  } else if (length(daftar_tahun) == 1) {
    paste("tahun", daftar_tahun)
  } else {
    "per tahun, 2023-2025"
  }
  
  p <- ggplot(d, aes(y = kunci)) +
    geom_segment(
      aes(x = bawah, xend = atas, yend = kunci),
      linewidth = 0.6,
      colour    = warna_tim[["coklat_tua"]]
    ) +
    geom_point(
      aes(x = estimasi),
      size   = 2.4,
      colour = warna_tim[["coklat"]]
    )
  
  if (rapat) {
    p <- p +
      geom_text(
        aes(x = atas, label = fmt_angka(estimasi)),
        hjust  = -0.25,
        size   = 2.8,
        colour = warna_tim[["hitam"]]
      )
  } else {
    p <- p +
      geom_text(
        aes(x = estimasi, label = fmt_angka(estimasi)),
        vjust    = -1.1,
        size     = 3.2,
        fontface = "bold",
        colour   = warna_tim[["hitam"]]
      )
  }
  
  p <- p +
    scale_x_continuous(expand = expansion(mult = c(0.12, 0.22))) +
    scale_y_discrete(
      labels = function(x) sub("^.*___", "", x),
      expand = expansion(add = 0.8)
    ) +
    labs(
      title    = judul,
      subtitle = paste0(
        "Sulawesi Selatan, ", keterangan,
        " (B = 1.999, persentil)"
      ),
      x        = "Rata-rata (%) dan SK 95%",
      y        = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(
        hjust = 0.5, colour = warna_tim[["coklat_tua"]]
      ),
      strip.text       = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(
        colour = warna_tim[["coklat_muda"]], fill = NA
      ),
      plot.background  = element_rect(fill = "white", colour = NA)
    )
  
  if (ada_tahun && length(daftar_tahun) > 1) {
    p + facet_wrap(
      vars(indikator, tahun),
      scales = "free",
      ncol   = length(daftar_tahun)
    )
  } else {
    p + facet_wrap(~ indikator, scales = "free")
  }
}

# ============================================================
# 7. SK BOOTSTRAP PER KELOMPOK PER TAHUN
#    (21 kabupaten, 3 kota)
# ============================================================
set.seed(2026)

boot_tahun <- data_boot %>%
  mutate(kelompok = jenis) %>%
  group_by(indikator, tahun, kelompok) %>%
  group_modify(~ hitung_sk_boot(.x$nilai)) %>%
  ungroup()

class(boot_tahun) <- c("sk_bootstrap", class(boot_tahun))

print(boot_tahun, n = Inf)

# ============================================================
# 8. GRAFIK TAHUN 2023
# ============================================================
p_boot_2023 <- autoplot(
  boot_tahun, pilih_tahun = 2023,
  judul = "SK Bootstrap 95% per Kelompok Wilayah"
)
print(p_boot_2023)

ggsave(
  "keluaran/lineup/sk_boot_2023.png",
  p_boot_2023, width = 9, height = 4, dpi = 300, bg = "white"
)

# ============================================================
# 9. GRAFIK TAHUN 2024
# ============================================================
p_boot_2024 <- autoplot(
  boot_tahun, pilih_tahun = 2024,
  judul = "SK Bootstrap 95% per Kelompok Wilayah"
)
print(p_boot_2024)

ggsave(
  "keluaran/lineup/sk_boot_2024.png",
  p_boot_2024, width = 9, height = 4, dpi = 300, bg = "white"
)

# ============================================================
# 10. GRAFIK TAHUN 2025
# ============================================================
p_boot_2025 <- autoplot(
  boot_tahun, pilih_tahun = 2025,
  judul = "SK Bootstrap 95% per Kelompok Wilayah"
)
print(p_boot_2025)

ggsave(
  "keluaran/lineup/sk_boot_2025.png",
  p_boot_2025, width = 9, height = 4, dpi = 300, bg = "white"
)

# ============================================================
# 11. GRAFIK 3 TAHUN DIGABUNG DALAM SATU GAMBAR
#     baris 1 = Kemiskinan, baris 2 = TPT, kolom = tahun
# ============================================================
p_boot_per_tahun <- autoplot(
  boot_tahun,
  judul = "SK Bootstrap 95% per Kelompok Wilayah, per Tahun",
  rapat = TRUE
)
print(p_boot_per_tahun)

ggsave(
  "keluaran/lineup/sk_boot_per_tahun_gabungan.png",
  p_boot_per_tahun, width = 13, height = 6, dpi = 300, bg = "white"
)

# ============================================================
# 12. SK BOOTSTRAP GABUNGAN 2023-2025 PER KABUPATEN/KOTA
#     catatan: dengan n = 3 tahun, SK hanya rentang min-maks
# ============================================================
set.seed(2026)

boot_wilayah <- data_boot %>%
  mutate(kelompok = kabupaten_kota) %>%
  group_by(indikator, kelompok, jenis) %>%
  group_modify(~ hitung_sk_boot(.x$nilai)) %>%
  ungroup()

class(boot_wilayah) <- c("sk_bootstrap", class(boot_wilayah))
print(boot_wilayah, n = Inf)

p_boot_wilayah <- autoplot(
  boot_wilayah,
  judul = "SK Bootstrap 95% per Kabupaten/Kota",
  rapat = TRUE
)
print(p_boot_wilayah)

ggsave(
  "keluaran/lineup/sk_boot_wilayah.png",
  p_boot_wilayah, width = 11, height = 8, dpi = 300, bg = "white"
)

# ============================================================
# 13. SK BOOTSTRAP KORELASI KEMISKINAN - TPT (DATA 2025)
# ============================================================
data_2025 <- data_tpt %>%
  filter(tahun == 2025) %>%
  select(kemiskinan_maret_p0_persen, tpt_agustus_persen) %>%
  drop_na()

set.seed(2026)

korelasi_boot <- replicate(1999, {
  idx <- sample(nrow(data_2025), replace = TRUE)
  cor(
    data_2025$kemiskinan_maret_p0_persen[idx],
    data_2025$tpt_agustus_persen[idx]
  )
})

korelasi_asli <- cor(
  data_2025$kemiskinan_maret_p0_persen,
  data_2025$tpt_agustus_persen
)

sk_korelasi <- quantile(korelasi_boot, c(0.025, 0.975), names = FALSE)

korelasi_asli
sk_korelasi

