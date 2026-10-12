# ============================================================
# R/autoplot.R
# Metode S3 autoplot() untuk objek "sk_bootstrap"
# (SK bootstrap persentil: titik estimasi + garis galat terurut)
#
# Isi berkas:
#   1. hitung_sk_boot()          : hitung SK bootstrap untuk satu vektor
#   2. sk_bootstrap()            : konstruktor + validasi objek
#   3. print.sk_bootstrap()      : ringkasan objek
#   4. autoplot.sk_bootstrap()   : metode autoplot()
#
# Paket: ggplot2, dplyr, tibble, forcats
# ============================================================

library(ggplot2)   # autoplot() adalah generik milik ggplot2
library(dplyr)

# palet bawaan (dapat diganti lewat argumen `warna`)
warna_sk_bawaan <- c(
  coklat       = "#8B5A2B",
  coklat_tua   = "#4E2E12",
  coklat_muda  = "#D2A679",
  coklat_pucat = "#F5EBDD",
  hitam        = "#1A1A1A",
  abu          = "#8C8C8C",
  putih        = "#FFFFFF"
)

# format angka 2 desimal dengan koma, contoh 9,11
fmt_angka <- function(x) {
  formatC(x, format = "f", digits = 2, decimal.mark = ",")
}

# pengganti %||% agar tidak bergantung pada versi R/rlang
atau <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------
# 1. HITUNG SK BOOTSTRAP
# ------------------------------------------------------------
#' Selang kepercayaan bootstrap persentil untuk rata-rata
#'
#' @param x vektor numerik (NA dibuang).
#' @param B jumlah replikasi bootstrap. Tugas mensyaratkan B >= 1.999.
#' @param level tingkat kepercayaan, default 0,95.
#' @return tibble satu baris: `n`, `estimasi`, `bawah`, `atas`.
#' @examples
#' set.seed(1)
#' hitung_sk_boot(c(2.1, 3.4, 5.0, 4.2))
hitung_sk_boot <- function(x, B = 1999, level = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) stop("`x` harus berisi minimal 2 nilai bukan-NA.", call. = FALSE)
  rata_boot <- replicate(B, mean(sample(x, n, replace = TRUE)))
  a <- (1 - level) / 2
  tibble::tibble(
    n        = n,
    estimasi = mean(x),
    bawah    = unname(quantile(rata_boot, a)),
    atas     = unname(quantile(rata_boot, 1 - a))
  )
}

# ------------------------------------------------------------
# 2. KONSTRUKTOR
# ------------------------------------------------------------
#' Buat objek kelas `sk_bootstrap`
#'
#' @param data data frame dengan kolom wajib `indikator`, `kelompok`,
#'   `estimasi`, `bawah`, `atas`. Kolom `tahun` bersifat opsional
#'   (bila ada, grafik dipecah per tahun).
#' @param B jumlah replikasi yang dipakai (disimpan untuk subjudul grafik).
#' @param level tingkat kepercayaan (disimpan untuk subjudul grafik).
#' @return objek kelas `c("sk_bootstrap", "tbl_df", "tbl", "data.frame")`.
#' @examples
#' d <- data.frame(indikator = "TPT (%)", kelompok = c("Kabupaten", "Kota"),
#'                 estimasi = c(3, 6), bawah = c(2.5, 4.8), atas = c(3.6, 7.1))
#' sk_bootstrap(d)
sk_bootstrap <- function(data, B = 1999, level = 0.95) {
  if (!is.data.frame(data)) stop("`data` harus berupa data frame.", call. = FALSE)
  perlu  <- c("indikator", "kelompok", "estimasi", "bawah", "atas")
  hilang <- setdiff(perlu, names(data))
  if (length(hilang) > 0) {
    stop("Kolom wajib belum ada: ", paste(hilang, collapse = ", "), call. = FALSE)
  }
  if (!all(vapply(data[c("estimasi", "bawah", "atas")], is.numeric, logical(1)))) {
    stop("Kolom `estimasi`, `bawah`, dan `atas` harus numerik.", call. = FALSE)
  }
  if (any(data$bawah > data$atas, na.rm = TRUE)) {
    stop("Ada baris dengan batas bawah > batas atas.", call. = FALSE)
  }
  x <- tibble::as_tibble(dplyr::ungroup(data))
  attr(x, "B")     <- B
  attr(x, "level") <- level
  class(x) <- c("sk_bootstrap", class(x))
  x
}

# ------------------------------------------------------------
# 3. PRINT
# ------------------------------------------------------------
#' @export
print.sk_bootstrap <- function(x, ...) {
  B     <- atau(attr(x, "B"), NA)
  level <- atau(attr(x, "level"), 0.95)
  cat("<sk_bootstrap> SK ", level * 100, "% persentil | B = ",
      format(B, big.mark = "."), " | ", nrow(x), " baris\n", sep = "")
  NextMethod()
  invisible(x)
}

# ------------------------------------------------------------
# 4. METODE autoplot()
# ------------------------------------------------------------
#' Grafik SK bootstrap: titik estimasi dan garis galat yang diurutkan
#'
#' @param object objek `sk_bootstrap`.
#' @param judul judul grafik (default "SK Bootstrap 95%").
#' @param pilih_tahun vektor tahun yang ditampilkan; NULL = semua tahun.
#'   Hanya berlaku bila objek punya kolom `tahun`.
#' @param rapat `FALSE`: angka di atas titik (cocok untuk sedikit baris);
#'   `TRUE`: angka di ujung kanan garis (cocok untuk banyak baris).
#' @param warna vektor bernama warna; default `warna_sk_bawaan`.
#'   Nama yang dipakai: `coklat`, `coklat_tua`, `coklat_muda`, `hitam`.
#' @param ... tidak dipakai.
#' @return objek `ggplot`. Panel dipecah per indikator (dan per tahun bila
#'   lebih dari satu tahun ditampilkan); garis diurutkan menurut estimasi.
#' @examples
#' \dontrun{
#' autoplot(boot_tahun, pilih_tahun = 2024)
#' autoplot(boot_wilayah, judul = "Per kabupaten/kota", rapat = TRUE)
#' }
#' @export
autoplot.sk_bootstrap <- function(object, judul = NULL, pilih_tahun = NULL,
                                  rapat = FALSE, warna = warna_sk_bawaan, ...) {
  d <- tibble::as_tibble(as.data.frame(object))
  B     <- atau(attr(object, "B"), 1999)
  level <- atau(attr(object, "level"), 0.95)
  ada_tahun <- "tahun" %in% names(d)

  if (ada_tahun && !is.null(pilih_tahun)) {
    tidak_ada <- setdiff(pilih_tahun, d$tahun)
    if (length(tidak_ada) > 0) {
      stop("Tahun tidak ada di data: ", paste(tidak_ada, collapse = ", "),
           call. = FALSE)
    }
    d <- dplyr::filter(d, tahun %in% pilih_tahun)
  }

  daftar_tahun <- if (ada_tahun) sort(unique(d$tahun)) else NULL

  # kunci unik per panel agar urutan estimasi tidak bercampur antarpanel
  d <- d |>
    dplyr::mutate(
      kunci = forcats::fct_reorder(
        paste(indikator, if (ada_tahun) tahun else "", kelompok, sep = "___"),
        estimasi
      )
    )

  if (is.null(judul)) judul <- paste0("SK Bootstrap ", level * 100, "%")

  keterangan <- if (is.null(daftar_tahun)) {
    "2023-2025"
  } else if (length(daftar_tahun) == 1) {
    paste("tahun", daftar_tahun)
  } else {
    "per tahun, 2023-2025"
  }

  p <- ggplot(d, aes(y = kunci)) +
    geom_segment(aes(x = bawah, xend = atas, yend = kunci),
                 linewidth = 0.6, colour = warna[["coklat_tua"]]) +
    geom_point(aes(x = estimasi), size = 2.4, colour = warna[["coklat"]])

  if (rapat) {
    p <- p + geom_text(aes(x = atas, label = fmt_angka(estimasi)),
                       hjust = -0.25, size = 2.8, colour = warna[["hitam"]])
  } else {
    p <- p + geom_text(aes(x = estimasi, label = fmt_angka(estimasi)),
                       vjust = -1.1, size = 3.2, fontface = "bold",
                       colour = warna[["hitam"]])
  }

  p <- p +
    scale_x_continuous(expand = expansion(mult = c(0.12, 0.22))) +
    scale_y_discrete(labels = function(x) sub("^.*___", "", x),
                     expand = expansion(add = 0.8)) +
    labs(
      title    = judul,
      subtitle = paste0("Sulawesi Selatan, ", keterangan,
                        " (B = ", format(B, big.mark = "."), ", persentil)"),
      x        = paste0("Rata-rata (%) dan SK ", level * 100, "%"),
      y        = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(hjust = 0.5, colour = warna[["coklat_tua"]]),
      strip.text       = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = warna[["coklat_muda"]], fill = NA),
      plot.background  = element_rect(fill = "white", colour = NA)
    )

  if (ada_tahun && length(daftar_tahun) > 1) {
    p + facet_wrap(vars(indikator, tahun), scales = "free",
                   ncol = length(daftar_tahun))
  } else {
    p + facet_wrap(~ indikator, scales = "free")
  }
}

# ============================================================
# CONTOH PEMAKAIAN (jalankan setelah data_boot dibuat, seperti di Bootstrap.R)
# ============================================================
# source("R/autoplot.R")
#
# set.seed(2026)
# boot_tahun <- data_boot |>
#   mutate(kelompok = jenis) |>
#   group_by(indikator, tahun, kelompok) |>
#   group_modify(~ hitung_sk_boot(.x$nilai, B = 1999)) |>
#   ungroup() |>
#   sk_bootstrap(B = 1999)
#
# boot_tahun                                   # print.sk_bootstrap
# autoplot(boot_tahun, pilih_tahun = 2024)     # satu tahun
# autoplot(boot_tahun, rapat = TRUE)           # 3 tahun sekaligus
#
# Bukti dispatch berjalan:
# inherits(boot_tahun, "sk_bootstrap")          # TRUE
# exists("autoplot.sk_bootstrap")               # TRUE
