# ==============================================================================
# CHECKPOINT 3
# 24 PROFIL KABUPATEN/KOTA SULAWESI SELATAN
#
# OUTPUT:
# 1. 24 grafik profil terpisah
# 2. Semua grafik langsung tampil di RStudio
# 3. Semua grafik otomatis tersimpan sebagai PNG
# 4. Semua grafik tersimpan dalam objek profil_24
#
# DATA:
# data_bersih.rds
#
# LOKASI PROYEK:
# C:/Users/User/Downloads/data_komstat
# ==============================================================================


# ==============================================================================
# 1. PACKAGE
# ==============================================================================

library(tidyverse)
library(patchwork)


# ==============================================================================
# 2. LOKASI PROYEK
# ==============================================================================

ROOT_DIR <- "C:/Users/User/Downloads/data_komstat"

setwd(ROOT_DIR)


# ==============================================================================
# 3. MEMBACA DATA BERSIH
# ==============================================================================

PATH_DATA <- file.path(
  ROOT_DIR,
  "data_bersih.rds"
)


# Jika tidak ada di folder utama,
# cari otomatis di dalam subfolder
if (!file.exists(PATH_DATA)) {
  
  hasil_cari <- list.files(
    ROOT_DIR,
    pattern = "^data_bersih\\.rds$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(hasil_cari) > 0) {
    PATH_DATA <- hasil_cari[1]
  }
}


# Cek file
if (!file.exists(PATH_DATA)) {
  
  stop(
    "ERROR: data_bersih.rds tidak ditemukan.\n",
    "Pastikan file berada di:\n",
    ROOT_DIR
  )
}


# Baca data
data_bersih <- readRDS(PATH_DATA) |>
  as_tibble()


# ==============================================================================
# 4. CEK STRUKTUR DATA
# ==============================================================================

kolom_wajib <- c(
  "kabupaten_kota",
  "tahun",
  "kemiskinan",
  "pengangguran"
)


kolom_hilang <- setdiff(
  kolom_wajib,
  names(data_bersih)
)


if (length(kolom_hilang) > 0) {
  
  stop(
    "Kolom berikut belum tersedia dalam data_bersih.rds:\n",
    paste(
      kolom_hilang,
      collapse = ", "
    )
  )
}


# ==============================================================================
# 5. PALET WARNA TIM
# ==============================================================================

warna_tim <- c(
  
  # Warna terang untuk grafik
  coklat_sangat_muda = "#E8D6C3",
  
  coklat_pucat       = "#F5EBDD",
  
  coklat_muda        = "#D2A679",
  
  coklat             = "#8B5A2B",
  
  # Warna teks
  coklat_tua         = "#4E2E12",
  
  hitam              = "#1A1A1A",
  
  abu                = "#8C8C8C",
  
  putih              = "#FFFFFF"
)


# ==============================================================================
# 6. WARNA KHUSUS GRAFIK
# ==============================================================================

warna_grafik_1 <- warna_tim["coklat_muda"]

warna_grafik_2 <- warna_tim["coklat_sangat_muda"]

warna_bar <- warna_tim["coklat_muda"]

warna_bar_pilih <- warna_tim["coklat"]

warna_teks <- warna_tim["coklat_tua"]


# ==============================================================================
# 7. TEMA GRAFIK
# ==============================================================================

theme_tim <- function(base_size = 11) {
  
  theme_minimal(
    base_size = base_size
  ) +
    
    theme(
      
      # ------------------------------------------------------------------------
      # BACKGROUND
      # ------------------------------------------------------------------------
      
      plot.background =
        element_rect(
          fill = warna_tim["putih"],
          colour = NA
        ),
      
      panel.background =
        element_rect(
          fill = warna_tim["putih"],
          colour = NA
        ),
      
      
      # ------------------------------------------------------------------------
      # JUDUL
      # ------------------------------------------------------------------------
      
      plot.title =
        element_text(
          face = "bold",
          size = 14,
          colour = warna_tim["coklat_tua"],
          margin =
            margin(
              bottom = 6
            )
        ),
      
      plot.subtitle =
        element_text(
          size = 9,
          colour = warna_tim["coklat_tua"]
        ),
      
      plot.caption =
        element_text(
          size = 8,
          colour = warna_tim["abu"]
        ),
      
      
      # ------------------------------------------------------------------------
      # SUMBU
      # ------------------------------------------------------------------------
      
      axis.title =
        element_text(
          colour = warna_tim["coklat_tua"],
          face = "bold"
        ),
      
      axis.text =
        element_text(
          colour = warna_tim["coklat_tua"]
        ),
      
      
      # ------------------------------------------------------------------------
      # GRID
      # ------------------------------------------------------------------------
      
      panel.grid.major =
        element_line(
          colour = warna_tim["coklat_pucat"],
          linewidth = 0.45
        ),
      
      panel.grid.minor =
        element_blank(),
      
      
      # ------------------------------------------------------------------------
      # LEGEND
      # ------------------------------------------------------------------------
      
      legend.position =
        "bottom",
      
      legend.text =
        element_text(
          colour = warna_tim["coklat_tua"]
        ),
      
      legend.title =
        element_text(
          colour = warna_tim["coklat_tua"],
          face = "bold"
        )
    )
}


# ==============================================================================
# 8. FOLDER OUTPUT
# ==============================================================================

DIR_OUTPUT <- file.path(
  ROOT_DIR,
  "keluaran",
  "profil_24"
)


dir.create(
  DIR_OUTPUT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 9. CEK JUMLAH KABUPATEN/KOTA
# ==============================================================================

daftar_kab <- data_bersih |>
  
  distinct(
    kabupaten_kota
  ) |>
  
  arrange(
    kabupaten_kota
  ) |>
  
  pull(
    kabupaten_kota
  )


cat("\n")
cat("============================================================\n")
cat("CHECKPOINT 3 - PROFIL KABUPATEN/KOTA\n")
cat("============================================================\n")

cat(
  "Jumlah kabupaten/kota:",
  length(daftar_kab),
  "\n\n"
)

print(daftar_kab)


if (length(daftar_kab) != 24) {
  
  warning(
    "\nPERHATIAN!\n",
    "Data saat ini berisi ",
    length(daftar_kab),
    " kabupaten/kota.\n",
    "Seharusnya 24 kabupaten/kota."
  )
}


# ==============================================================================
# 10. TAHUN TERSEDIA
# ==============================================================================

tahun_tersedia <- sort(
  unique(
    data_bersih$tahun
  )
)


TAHUN_FOKUS <- max(
  tahun_tersedia,
  na.rm = TRUE
)


cat(
  "\nTahun yang tersedia:",
  paste(
    tahun_tersedia,
    collapse = ", "
  ),
  "\n"
)

cat(
  "Tahun fokus:",
  TAHUN_FOKUS,
  "\n"
)


# ==============================================================================
# 11. FUNGSI NAMA FILE
# ==============================================================================

buat_nama_file <- function(x) {
  
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(
      "[^a-zA-Z0-9]+",
      "_"
    ) |>
    stringr::str_replace_all(
      "^_|_$",
      ""
    )
}


# ==============================================================================
# 12. FUNGSI GRAFIK TREN KEMISKINAN
# ==============================================================================

buat_grafik_kemiskinan <- function(
    data,
    kab
) {
  
  data_kab <- data |>
    
    filter(
      kabupaten_kota == kab
    )
  
  
  ggplot(
    data_kab,
    aes(
      x = tahun,
      y = kemiskinan
    )
  ) +
    
    # Garis tren
    geom_line(
      colour = warna_grafik_1,
      linewidth = 1.3
    ) +
    
    # Titik
    geom_point(
      colour = warna_grafik_1,
      fill = warna_tim["coklat_pucat"],
      shape = 21,
      size = 3.5,
      stroke = 1
    ) +
    
    # Nilai
    geom_text(
      aes(
        label =
          sprintf(
            "%.2f",
            kemiskinan
          )
      ),
      vjust = -1,
      colour = warna_teks,
      size = 3
    ) +
    
    scale_x_continuous(
      breaks = tahun_tersedia
    ) +
    
    labs(
      title =
        "Tren Kemiskinan",
      
      subtitle =
        "Perkembangan persentase kemiskinan",
      
      x =
        "Tahun",
      
      y =
        "Kemiskinan (%)"
    ) +
    
    theme_tim()
}


# ==============================================================================
# 13. FUNGSI GRAFIK TREN TPT
# ==============================================================================

buat_grafik_tpt <- function(
    data,
    kab
) {
  
  data_kab <- data |>
    
    filter(
      kabupaten_kota == kab
    )
  
  
  ggplot(
    data_kab,
    aes(
      x = tahun,
      y = pengangguran
    )
  ) +
    
    geom_line(
      colour = warna_grafik_2,
      linewidth = 1.3
    ) +
    
    geom_point(
      colour = warna_grafik_2,
      fill = warna_tim["coklat_pucat"],
      shape = 21,
      size = 3.5,
      stroke = 1
    ) +
    
    geom_text(
      aes(
        label =
          sprintf(
            "%.2f",
            pengangguran
          )
      ),
      vjust = -1,
      colour = warna_teks,
      size = 3
    ) +
    
    scale_x_continuous(
      breaks = tahun_tersedia
    ) +
    
    labs(
      title =
        "Tren Tingkat Pengangguran",
      
      subtitle =
        "Perkembangan TPT dari tahun ke tahun",
      
      x =
        "Tahun",
      
      y =
        "TPT (%)"
    ) +
    
    theme_tim()
}


# ==============================================================================
# 14. FUNGSI GRAFIK PERBANDINGAN KEMISKINAN
# ==============================================================================

buat_grafik_perbandingan <- function(
    data,
    kab,
    tahun_fokus
) {
  
  
  data_tahun <- data |>
    
    filter(
      tahun == tahun_fokus
    ) |>
    
    arrange(
      kemiskinan
    ) |>
    
    mutate(
      
      wilayah =
        factor(
          kabupaten_kota,
          levels =
            kabupaten_kota
        ),
      
      status =
        if_else(
          kabupaten_kota == kab,
          "Kabupaten/Kota",
          "Wilayah lainnya"
        )
    )
  
  
  ggplot(
    data_tahun,
    aes(
      x = kemiskinan,
      y = wilayah,
      fill = status
    )
  ) +
    
    geom_col(
      width = 0.65
    ) +
    
    scale_fill_manual(
      values = c(
        
        "Kabupaten/Kota" =
          warna_bar_pilih,
        
        "Wilayah lainnya" =
          warna_bar
      )
    ) +
    
    labs(
      
      title =
        paste(
          "Kemiskinan Tahun",
          tahun_fokus
        ),
      
      subtitle =
        paste(
          "Posisi",
          kab,
          "dibandingkan wilayah lainnya"
        ),
      
      x =
        "Kemiskinan (%)",
      
      y =
        NULL,
      
      fill =
        NULL
    ) +
    
    theme_tim() +
    
    theme(
      
      axis.text.y =
        element_text(
          size = 7
        ),
      
      legend.position =
        "bottom"
    )
}


# ==============================================================================
# 15. LIST UNTUK MENYIMPAN 24 PROFIL
# ==============================================================================

profil_24 <- list()


# ==============================================================================
# 16. LOOP 24 KABUPATEN/KOTA
# ==============================================================================

for (i in seq_along(daftar_kab)) {
  
  
  # ---------------------------------------------------------------------------
  # NAMA WILAYAH
  # ---------------------------------------------------------------------------
  
  kab <- daftar_kab[i]
  
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat(
    "PROFIL ",
    i,
    " DARI ",
    length(daftar_kab),
    "\n"
  )
  cat(
    kab,
    "\n"
  )
  cat("------------------------------------------------------------\n")
  
  
  # ---------------------------------------------------------------------------
  # GRAFIK 1: KEMISKINAN
  # ---------------------------------------------------------------------------
  
  grafik_kemiskinan <- buat_grafik_kemiskinan(
    data = data_bersih,
    kab = kab
  )
  
  
  # ---------------------------------------------------------------------------
  # GRAFIK 2: TPT
  # ---------------------------------------------------------------------------
  
  grafik_tpt <- buat_grafik_tpt(
    data = data_bersih,
    kab = kab
  )
  
  
  # ---------------------------------------------------------------------------
  # GRAFIK 3: PERBANDINGAN
  # ---------------------------------------------------------------------------
  
  grafik_perbandingan <- buat_grafik_perbandingan(
    data = data_bersih,
    kab = kab,
    tahun_fokus = TAHUN_FOKUS
  )
  
  
  # ---------------------------------------------------------------------------
  # GABUNGKAN
  # ---------------------------------------------------------------------------
  
  profil <- (
    
    (grafik_kemiskinan /
       grafik_tpt) |
      
      grafik_perbandingan
    
  ) +
    
    plot_layout(
      widths = c(
        1.05,
        1
      )
    ) +
    
    plot_annotation(
      
      title =
        kab,
      
      subtitle =
        paste(
          "Profil Statistik Kabupaten/Kota Sulawesi Selatan |",
          "Tahun fokus",
          TAHUN_FOKUS
        ),
      
      caption =
        "Sumber: data_bersih.rds | Diolah menggunakan R",
      
      theme =
        theme(
          
          plot.title =
            element_text(
              face = "bold",
              size = 19,
              colour =
                warna_tim["coklat_tua"],
              hjust = 0
            ),
          
          plot.subtitle =
            element_text(
              size = 10,
              colour =
                warna_tim["coklat_tua"],
              hjust = 0
            ),
          
          plot.caption =
            element_text(
              size = 8,
              colour =
                warna_tim["abu"]
            )
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # SIMPAN KE LIST R
  # ---------------------------------------------------------------------------
  
  profil_24[[kab]] <- profil
  
  
  # ---------------------------------------------------------------------------
  # TAMPILKAN DI RSTUDIO
  # ---------------------------------------------------------------------------
  
  print(profil)
  
  
  # ---------------------------------------------------------------------------
  # NAMA FILE
  # ---------------------------------------------------------------------------
  
  nama_file <- paste0(
    "profil_",
    buat_nama_file(kab),
    ".png"
  )
  
  
  path_png <- file.path(
    DIR_OUTPUT,
    nama_file
  )
  
  
  # ---------------------------------------------------------------------------
  # SIMPAN FILE PNG
  # ---------------------------------------------------------------------------
  
  ggsave(
    
    filename =
      path_png,
    
    plot =
      profil,
    
    width =
      12,
    
    height =
      8,
    
    dpi =
      300,
    
    bg =
      warna_tim["putih"]
  )
  
  
  # ---------------------------------------------------------------------------
  # INFORMASI PROSES
  # ---------------------------------------------------------------------------
  
  cat(
    "✓ Grafik tampil di RStudio\n"
  )
  
  cat(
    "✓ Grafik tersimpan:\n",
    path_png,
    "\n"
  )
}


# ==============================================================================
# 17. MANIFEST 24 PROFIL
# ==============================================================================

manifest_24 <- tibble(
  
  nomor =
    seq_along(daftar_kab),
  
  kabupaten_kota =
    daftar_kab,
  
  file_png =
    file.path(
      
      DIR_OUTPUT,
      
      paste0(
        "profil_",
        buat_nama_file(
          daftar_kab
        ),
        ".png"
      )
    )
)


write_csv(
  
  manifest_24,
  
  file.path(
    DIR_OUTPUT,
    "manifest_24_profil.csv"
  )
)


# ==============================================================================
# 18. SIMPAN SEMUA OBJEK GRAFIK
# ==============================================================================

saveRDS(
  
  profil_24,
  
  file.path(
    DIR_OUTPUT,
    "profil_24.rds"
  )
)


# ==============================================================================
# 19. INFORMASI AKHIR
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("CHECKPOINT 3 SELESAI\n")
cat("============================================================\n")

cat(
  "Jumlah profil yang dibuat:",
  length(profil_24),
  "\n"
)

cat(
  "Folder output:\n",
  DIR_OUTPUT,
  "\n"
)

cat(
  "File PNG: 24 profil terpisah\n"
)

cat(
  "Objek R: profil_24\n"
)

cat(
  "Objek R juga disimpan sebagai: profil_24.rds\n"
)

cat("============================================================\n")