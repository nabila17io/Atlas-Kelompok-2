# ==============================================================================
# CHECKPOINT 3
# UJI BUTA WARNA PETA TPT SULAWESI SELATAN
#
# OUTPUT:
# 1. Penglihatan normal
# 2. Deuteranopia (simulasi)
# 3. Protanopia (simulasi)
#
# KETIGA PETA DITAMPILKAN BERSAMA DALAM 1 OUTPUT
#
# DATA:
# data_bersih.rds
#
# VARIABEL:
# pengangguran = Tingkat Pengangguran Terbuka (TPT)
#
# TAHUN:
# 2025
#
# LOKASI:
# C:/Users/User/Downloads/data_komstat
# ==============================================================================


# ==============================================================================
# 1. PACKAGE
# ==============================================================================

library(tidyverse)
library(sf)
library(colorspace)
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


# Jika tidak ditemukan di folder utama,
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
    paste0(
      "\nERROR: data_bersih.rds tidak ditemukan.\n\n",
      "Pastikan file berada di:\n",
      ROOT_DIR
    )
  )
}


# Membaca data

data_bersih <- readRDS(
  PATH_DATA
) |>
  as_tibble()


# ==============================================================================
# 4. CEK DATA
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("DATA BERSIH\n")
cat("============================================================\n")

cat(
  "Lokasi data : ",
  PATH_DATA,
  "\n"
)

cat(
  "Jumlah baris: ",
  nrow(data_bersih),
  "\n"
)

cat("\nKolom data:\n")

print(
  names(data_bersih)
)


# ==============================================================================
# 5. KOLOM YANG DIGUNAKAN
# ==============================================================================

kolom_wajib <- c(
  "kabupaten_kota",
  "tahun",
  "pengangguran"
)


kolom_hilang <- setdiff(
  kolom_wajib,
  names(data_bersih)
)


if (length(kolom_hilang) > 0) {
  
  stop(
    paste0(
      "\nKolom berikut tidak ditemukan:\n",
      paste(
        kolom_hilang,
        collapse = ", "
      )
    )
  )
}


# ==============================================================================
# 6. PENGATURAN VARIABEL
# ==============================================================================

VAR_UTAMA <- "pengangguran"

LABEL_VAR <- "Tingkat\nPengangguran\nTerbuka (%)"


# Tahun yang digunakan

TAHUN_FOKUS <- 2025


# Cek apakah tahun 2025 tersedia

if (
  !TAHUN_FOKUS %in% data_bersih$tahun
) {
  
  stop(
    paste0(
      "\nData tahun ",
      TAHUN_FOKUS,
      " tidak ditemukan dalam data_bersih.rds."
    )
  )
}


# ==============================================================================
# 7. PALET WARNA TIM
# ==============================================================================

warna_tim <- c(
  
  coklat       = "#8B5A2B",
  
  coklat_tua   = "#4E2E12",
  
  coklat_muda  = "#D2A679",
  
  coklat_pucat = "#F5EBDD",
  
  hitam        = "#1A1A1A",
  
  abu          = "#8C8C8C",
  
  putih        = "#FFFFFF"
  
)


# ==============================================================================
# 8. PALET KOROPLET
# ==============================================================================

# 5 kelas dari terang ke gelap:
#
# Kelas 1 = coklat pucat
# Kelas 2 = coklat muda
# Kelas 3 = coklat
# Kelas 4 = coklat tua
# Kelas 5 = hitam
#
# Semakin gelap berarti TPT semakin tinggi.

PALET_PETA <- c(
  
  warna_tim["coklat_pucat"],
  
  warna_tim["coklat_muda"],
  
  warna_tim["coklat"],
  
  warna_tim["coklat_tua"],
  
  warna_tim["hitam"]
  
)


N_KELAS <- 5


# ==============================================================================
# 9. FOLDER OUTPUT
# ==============================================================================

DIR_OUTPUT <- file.path(
  
  ROOT_DIR,
  
  "keluaran",
  
  "peta_koroplet"
  
)


dir.create(
  
  DIR_OUTPUT,
  
  recursive = TRUE,
  
  showWarnings = FALSE
  
)


# ==============================================================================
# 10. MENCARI FILE BATAS WILAYAH
# ==============================================================================

FILE_GEO <- list.files(
  
  ROOT_DIR,
  
  pattern = "\\.(geojson|gpkg|shp)$",
  
  recursive = TRUE,
  
  full.names = TRUE,
  
  ignore.case = TRUE
  
)


if (length(FILE_GEO) == 0) {
  
  stop(
    paste0(
      "\nFile batas wilayah tidak ditemukan.\n\n",
      "Simpan file .geojson / .gpkg / .shp ",
      "di dalam folder:\n",
      ROOT_DIR
    )
  )
}


# Cari file ADM2 jika tersedia

INDEX_ADM2 <- grep(
  "ADM2|adm2",
  FILE_GEO,
  ignore.case = TRUE
)


if (length(INDEX_ADM2) > 0) {
  
  PATH_GEO <- FILE_GEO[
    INDEX_ADM2[1]
  ]
  
} else {
  
  PATH_GEO <- FILE_GEO[1]
  
}


cat("\n")
cat("File batas wilayah:\n")
cat(PATH_GEO, "\n")


# ==============================================================================
# 11. MEMBACA BATAS WILAYAH
# ==============================================================================

geo <- st_read(
  PATH_GEO,
  quiet = TRUE
)


geo <- st_make_valid(
  geo
)


# ==============================================================================
# 12. NAMA KOLOM WILAYAH
# ==============================================================================

cat("\n")
cat("Kolom pada file peta:\n")

print(
  names(geo)
)


# Nama kolom geoBoundaries

KOLOM_NAMA_GEO <- "shapeName"


if (
  !KOLOM_NAMA_GEO %in% names(geo)
) {
  
  stop(
    paste0(
      "\nKolom 'shapeName' tidak ditemukan.\n",
      "Periksa nama kolom wilayah pada file peta."
    )
  )
}


# ==============================================================================
# 13. ALIAS NAMA WILAYAH
# ==============================================================================

alias <- c(
  
  "sidenreng rappang" =
    "sidrap",
  
  "pangkajene dan kepulauan" =
    "pangkep",
  
  "pangkajene kepulauan" =
    "pangkep",
  
  "pangkajene and islands" =
    "pangkep",
  
  "kepulauan selayar" =
    "selayar",
  
  "selayar islands" =
    "selayar",
  
  "pare pare" =
    "parepare",
  
  "north toraja" =
    "toraja utara",
  
  "north luwu" =
    "luwu utara",
  
  "east luwu" =
    "luwu timur",
  
  "ujung pandang" =
    "makassar"
  
)


# ==============================================================================
# 14. FUNGSI STANDARDISASI NAMA
# ==============================================================================

kunci_wilayah <- function(x) {
  
  k <- x |>
    
    str_to_lower() |>
    
    str_replace_all(
      "[^a-z]",
      " "
    ) |>
    
    str_remove_all(
      "\\b(kabupaten|kab|kota|city|regency)\\b"
    ) |>
    
    str_squish()
  
  
  # Menggunakan alias
  
  k <- ifelse(
    
    k %in% names(alias),
    
    unname(
      alias[k]
    ),
    
    k
    
  )
  
  
  # Hilangkan spasi
  
  k <- str_remove_all(
    k,
    " "
  )
  
  
  return(k)
  
}


# ==============================================================================
# 15. KUNCI DATA
# ==============================================================================

data_kunci <- data_bersih |>
  
  mutate(
    
    kunci =
      kunci_wilayah(
        kabupaten_kota
      )
    
  )


# ==============================================================================
# 16. KUNCI PETA
# ==============================================================================

geo <- geo |>
  
  mutate(
    
    kunci =
      kunci_wilayah(
        .data[[KOLOM_NAMA_GEO]]
      )
    
  )


# ==============================================================================
# 17. PILIH 24 WILAYAH SULAWESI SELATAN
# ==============================================================================

geo_sulsel <- geo |>
  
  filter(
    
    kunci %in%
      unique(
        data_kunci$kunci
      )
    
  )


cat("\n")
cat(
  "Jumlah wilayah yang ditemukan pada peta: ",
  nrow(geo_sulsel),
  "\n"
)


# ==============================================================================
# 18. CEK KESESUAIAN NAMA WILAYAH
# ==============================================================================

wilayah_data <- unique(
  data_kunci$kunci
)


wilayah_peta <- unique(
  geo_sulsel$kunci
)


wilayah_tidak_cocok <- setdiff(
  
  wilayah_data,
  
  wilayah_peta
  
)


if (
  length(wilayah_tidak_cocok) > 0
) {
  
  cat("\nWilayah yang belum cocok:\n")
  
  print(
    wilayah_tidak_cocok
  )
  
  stop(
    "\nNama wilayah pada data dan peta belum seluruhnya cocok."
  )
}


# ==============================================================================
# 19. DATA TPT TAHUN 2025
# ==============================================================================

data_tpt_2025 <- data_kunci |>
  
  filter(
    tahun == TAHUN_FOKUS
  )


# ==============================================================================
# 20. MEMBUAT KELAS KUANTIL
# ==============================================================================

BRK <- quantile(
  
  data_tpt_2025[[VAR_UTAMA]],
  
  probs =
    seq(
      0,
      1,
      length.out =
        N_KELAS + 1
    ),
  
  na.rm =
    TRUE
  
) |>
  
  as.numeric()


# Pembulatan satu desimal

BRK <- round(
  BRK,
  1
)


# Batas bawah

BRK[1] <-
  floor(
    BRK[1] * 10
  ) / 10


# Batas atas

BRK[length(BRK)] <-
  ceiling(
    BRK[length(BRK)] * 10
  ) / 10


# Cek batas kelas

if (
  anyDuplicated(BRK) > 0
) {
  
  stop(
    paste0(
      "\nBatas kelas kuantil ada yang sama.\n",
      "Kurangi jumlah kelas atau periksa data TPT."
    )
  )
  
}


# ==============================================================================
# 21. LABEL KELAS
# ==============================================================================

LAB <- sprintf(
  
  "%.2f - %.2f",
  
  head(BRK, -1),
  
  tail(BRK, -1)
  
)


# ==============================================================================
# 22. TABEL KELAS
# ==============================================================================

tabel_kelas <- tibble(
  
  kelas =
    1:N_KELAS,
  
  interval =
    LAB,
  
  warna =
    PALET_PETA
  
)


cat("\n")
cat("============================================================\n")
cat("KELAS TPT 2025\n")
cat("============================================================\n")

print(
  tabel_kelas
)


# ==============================================================================
# 23. GABUNGKAN DATA TPT DENGAN PETA
# ==============================================================================

peta_tpt <- geo_sulsel |>
  
  select(
    kunci,
    geometry
  ) |>
  
  left_join(
    
    data_tpt_2025,
    
    by = "kunci"
    
  ) |>
  
  mutate(
    
    kelas =
      cut(
        
        .data[[VAR_UTAMA]],
        
        breaks =
          BRK,
        
        labels =
          LAB,
        
        include.lowest =
          TRUE
        
      )
    
  )


# ==============================================================================
# 24. CEK JUMLAH WILAYAH
# ==============================================================================

cat("\n")
cat(
  "Jumlah wilayah pada peta TPT 2025: ",
  nrow(peta_tpt),
  "\n"
)


# ==============================================================================
# 25. FUNGSI MEMBUAT PETA
# ==============================================================================

buat_peta <- function(
    
  data_peta,
  
  palet,
  
  judul_panel
  
) {
  
  ggplot(
    data_peta
  ) +
    
    geom_sf(
      
      aes(
        fill = kelas
      ),
      
      colour =
        warna_tim["putih"],
      
      linewidth =
        0.35
      
    ) +
    
    
    scale_fill_manual(
      
      values =
        setNames(
          palet,
          LAB
        ),
      
      drop =
        FALSE,
      
      name =
        LABEL_VAR
      
    ) +
    
    
    labs(
      
      title =
        judul_panel
      
    ) +
    
    
    coord_sf(
      
      expand =
        FALSE
      
    ) +
    
    
    theme_minimal(
      
      base_size =
        10
      
    ) +
    
    
    theme(
      
      # ------------------------------------------------------------------------
      # BACKGROUND
      # ------------------------------------------------------------------------
      
      plot.background =
        element_rect(
          
          fill =
            warna_tim["putih"],
          
          colour =
            NA
          
        ),
      
      
      panel.background =
        element_rect(
          
          fill =
            warna_tim["putih"],
          
          colour =
            NA
          
        ),
      
      
      # ------------------------------------------------------------------------
      # GRID DAN SUMBU
      # ------------------------------------------------------------------------
      
      panel.grid =
        element_blank(),
      
      axis.text =
        element_blank(),
      
      axis.title =
        element_blank(),
      
      axis.ticks =
        element_blank(),
      
      
      # ------------------------------------------------------------------------
      # JUDUL PANEL
      # ------------------------------------------------------------------------
      
      plot.title =
        element_text(
          
          face =
            "bold",
          
          size =
            11,
          
          colour =
            warna_tim["coklat_tua"],
          
          hjust =
            0.5
          
        ),
      
      
      # ------------------------------------------------------------------------
      # LEGENDA
      # ------------------------------------------------------------------------
      
      legend.position =
        "right",
      
      legend.title =
        element_text(
          
          face =
            "bold",
          
          size =
            8,
          
          colour =
            warna_tim["coklat_tua"]
          
        ),
      
      legend.text =
        element_text(
          
          size =
            7,
          
          colour =
            warna_tim["coklat_tua"]
          
        ),
      
      legend.key.height =
        unit(
          0.45,
          "cm"
        ),
      
      legend.key.width =
        unit(
          0.45,
          "cm"
        )
      
    )
  
}


# ==============================================================================
# 26. PALET SIMULASI BUTA WARNA
# ==============================================================================

# Peta normal menggunakan persis palet tim.

PALET_NORMAL <- PALET_PETA


# Simulasi deuteranopia

PALET_DEUTAN <- colorspace::deutan(
  PALET_PETA
)


# Simulasi protanopia

PALET_PROTAN <- colorspace::protan(
  PALET_PETA
)


# ==============================================================================
# 27. PETA 1 — PENGLIHATAN NORMAL
# ==============================================================================

p_normal <- buat_peta(
  
  data_peta =
    peta_tpt,
  
  palet =
    PALET_NORMAL,
  
  judul_panel =
    "Penglihatan normal"
  
)


# ==============================================================================
# 28. PETA 2 — DEUTERANOPIA
# ==============================================================================

p_deutan <- buat_peta(
  
  data_peta =
    peta_tpt,
  
  palet =
    PALET_DEUTAN,
  
  judul_panel =
    "Deuteranopia (simulasi)"
  
)


# ==============================================================================
# 29. PETA 3 — PROTANOPIA
# ==============================================================================

p_protan <- buat_peta(
  
  data_peta =
    peta_tpt,
  
  palet =
    PALET_PROTAN,
  
  judul_panel =
    "Protanopia (simulasi)"
  
)


# ==============================================================================
# 30. GABUNGKAN MENJADI 3 PANEL
# ==============================================================================

peta_3_panel <- (
  
  p_normal +
    
    p_deutan +
    
    p_protan
  
) +
  
  plot_layout(
    ncol = 3,
    guides = "keep"
  ) +
  
  plot_annotation(
    
    title =
      paste(
        "Uji Buta Warna: Peta TPT",
        TAHUN_FOKUS
      ),
    
    theme =
      theme(
        
        plot.title =
          element_text(
            
            face =
              "bold",
            
            size =
              18,
            
            colour =
              warna_tim["coklat_tua"],
            
            hjust =
              0
            
          )
        
      )
    
  )


# ==============================================================================
# 31. TAMPILKAN LANGSUNG DI RSTUDIO
# ==============================================================================

print(
  peta_3_panel
)


# ==============================================================================
# 32. SIMPAN OUTPUT 3 PANEL
# ==============================================================================

FILE_OUTPUT <- file.path(
  
  DIR_OUTPUT,
  
  paste0(
    "uji_buta_warna_peta_TPT_",
    TAHUN_FOKUS,
    ".png"
  )
  
)


ggsave(
  
  filename =
    FILE_OUTPUT,
  
  plot =
    peta_3_panel,
  
  width =
    16,
  
  height =
    7,
  
  dpi =
    300,
  
  bg =
    warna_tim["putih"]
  
)


# ==============================================================================
# 33. SIMPAN PETA NORMAL SECARA TERPISAH
# ==============================================================================

ggsave(
  
  filename =
    file.path(
      
      DIR_OUTPUT,
      
      paste0(
        "peta_TPT_normal_",
        TAHUN_FOKUS,
        ".png"
      )
      
    ),
  
  plot =
    p_normal,
  
  width =
    8,
  
  height =
    7,
  
  dpi =
    300,
  
  bg =
    warna_tim["putih"]
  
)


# ==============================================================================
# 34. SIMPAN PETA DEUTERANOPIA SECARA TERPISAH
# ==============================================================================

ggsave(
  
  filename =
    file.path(
      
      DIR_OUTPUT,
      
      paste0(
        "peta_TPT_deuteranopia_",
        TAHUN_FOKUS,
        ".png"
      )
      
    ),
  
  plot =
    p_deutan,
  
  width =
    8,
  
  height =
    7,
  
  dpi =
    300,
  
  bg =
    warna_tim["putih"]
  
)


# ==============================================================================
# 35. SIMPAN PETA PROTANOPIA SECARA TERPISAH
# ==============================================================================

ggsave(
  
  filename =
    file.path(
      
      DIR_OUTPUT,
      
      paste0(
        "peta_TPT_protanopia_",
        TAHUN_FOKUS,
        ".png"
      )
      
    ),
  
  plot =
    p_protan,
  
  width =
    8,
  
  height =
    7,
  
  dpi =
    300,
  
  bg =
    warna_tim["putih"]
  
)


# ==============================================================================
# 36. SIMPAN DATA PETA
# ==============================================================================

saveRDS(
  
  peta_tpt,
  
  file.path(
    
    DIR_OUTPUT,
    
    paste0(
      "peta_TPT_",
      TAHUN_FOKUS,
      ".rds"
    )
    
  )
  
)


# ==============================================================================
# 37. SIMPAN INFORMASI PALET
# ==============================================================================

hasil_palet <- tibble(
  
  kelas =
    LAB,
  
  warna_normal =
    PALET_NORMAL,
  
  warna_deuteranopia =
    PALET_DEUTAN,
  
  warna_protanopia =
    PALET_PROTAN
  
)


write_csv(
  
  hasil_palet,
  
  file.path(
    
    DIR_OUTPUT,
    
    paste0(
      "hasil_uji_buta_warna_TPT_",
      TAHUN_FOKUS,
      ".csv"
    )
    
  )
  
)


# ==============================================================================
# 38. SIMPAN TABEL KELAS
# ==============================================================================

write_csv(
  
  tabel_kelas,
  
  file.path(
    
    DIR_OUTPUT,
    
    paste0(
      "kelas_TPT_",
      TAHUN_FOKUS,
      ".csv"
    )
    
  )
  
)


# ==============================================================================
# 39. INFORMASI HASIL
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("CHECKPOINT 3 - UJI BUTA WARNA SELESAI\n")
cat("============================================================\n")

cat(
  "Variabel       : Tingkat Pengangguran Terbuka (TPT)\n"
)

cat(
  "Tahun          : ",
  TAHUN_FOKUS,
  "\n"
)

cat(
  "Jumlah wilayah : ",
  nrow(peta_tpt),
  "\n"
)

cat(
  "Jumlah kelas   : ",
  N_KELAS,
  "\n"
)

cat("\n")
cat("PALET NORMAL:\n")

print(
  PALET_NORMAL
)

cat("\n")
cat("OUTPUT UTAMA:\n")

cat(
  FILE_OUTPUT,
  "\n"
)

cat("\n")
cat(
  "Peta 3 panel telah ditampilkan langsung di RStudio.\n"
)

cat("============================================================\n")