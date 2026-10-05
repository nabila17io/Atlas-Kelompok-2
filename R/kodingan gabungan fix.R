setwd("C:/Users/User/Downloads/data_utama")
try(graphics.off(), silent = TRUE)

paket <- c("readxl", "dplyr", "tidyr", "readr", "sf", "jsonlite", "ggplot2")
baru  <- paket[!sapply(paket, requireNamespace, quietly = TRUE)]
if (length(baru)) install.packages(baru)
invisible(lapply(paket, library, character.only = TRUE))
suppressMessages(sf_use_s2(FALSE))
options(timeout = 900)

for (d in c("data/mentah", "data/bersih", "peta", "keluaran/makeover"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# fungsi penyeragam nama wilayah
kunci <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("^(kabupaten|kota|kab\\.|kab)\\s+", "", x)
  x <- gsub("[^a-z]", "", x)
  alias <- c(pangkajenedankepulauan = "pangkep", pangkajenekepulauan = "pangkep",
             pangkajene = "pangkep", sidenrengrappang = "sidrap",
             sidenreng = "sidrap", selayar = "kepulauanselayar")
  ifelse(x %in% names(alias), unname(alias[x]), x)
}

tema <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#1F4E79"),
        legend.position = "top", panel.grid.minor = element_blank())

simpan <- function(p, nama, folder, w, h)
  ggsave(file.path(folder, nama), p, width = w, height = h, dpi = 300, bg = "white")

# ---------------------------------------------------------------
# 1. baca data excel
# ---------------------------------------------------------------
berkas <- list.files(pattern = "\\.xlsx$")
berkas <- berkas[!grepl("^~\\$", berkas)]
judul  <- sapply(berkas, function(f)
  paste(unlist(read_excel(f, range = "B1", col_names = FALSE,
                          col_types = "text")), collapse = " "))
f_miskin <- berkas[grepl("miskin", judul, ignore.case = TRUE)]
f_tpt    <- berkas[grepl("pengangguran", judul, ignore.case = TRUE)]
stopifnot(length(f_miskin) == 1, length(f_tpt) == 1)

baca <- function(path, nama) {
  read_excel(path, skip = 2,
             col_names = c("kabupaten_kota", "2023", "2024", "2025"),
             col_types = c("text", "numeric", "numeric", "numeric")) |>
    filter(!is.na(kabupaten_kota), kabupaten_kota != "Catatan") |>
    pivot_longer(-kabupaten_kota, names_to = "tahun", values_to = nama) |>
    mutate(tahun = as.integer(tahun))
}

data_excel <- inner_join(baca(f_miskin, "kemiskinan"),
                         baca(f_tpt, "pengangguran"),
                         by = c("kabupaten_kota", "tahun")) |>
  mutate(kunci = kunci(kabupaten_kota)) |>
  filter(kunci != "sulawesiselatan")

# ---------------------------------------------------------------
# 2. kode wilayah
# ---------------------------------------------------------------
kode <- data.frame(
  kode_wilayah = c("7301","7302","7303","7304","7305","7306","7307","7308",
                   "7309","7310","7311","7312","7313","7314","7315","7316",
                   "7317","7318","7322","7325","7326","7371","7372","7373"),
  kabupaten_kota = c("Kepulauan Selayar","Bulukumba","Bantaeng","Jeneponto",
                     "Takalar","Gowa","Sinjai","Maros","Pangkep","Barru","Bone",
                     "Soppeng","Wajo","Sidrap","Pinrang","Enrekang","Luwu",
                     "Tana Toraja","Luwu Utara","Luwu Timur","Toraja Utara",
                     "Makassar","Pare Pare","Palopo"))
kode$kunci <- kunci(kode$kabupaten_kota)

tabel_gabungan <- data_excel |>
  select(-kabupaten_kota) |>
  left_join(kode, by = "kunci") |>
  mutate(jenis = ifelse(as.integer(substr(kode_wilayah, 3, 4)) >= 71,
                        "Kota", "Kabupaten")) |>
  select(kode_wilayah, kabupaten_kota, jenis, tahun, kemiskinan, pengangguran) |>
  arrange(kode_wilayah, tahun) |>
  as.data.frame()

tabel_lebar <- pivot_wider(tabel_gabungan, names_from = tahun,
                           values_from = c(kemiskinan, pengangguran))

# ---------------------------------------------------------------
# 3. pembersihan dan pemeriksaan data
# ---------------------------------------------------------------
data_raw <- tabel_gabungan
data <- data_raw |>
  rename(tpt_agustus_persen = pengangguran,
         kemiskinan_maret_p0_persen = kemiskinan) |>
  mutate(tahun = as.integer(tahun))

dim(data)
n_distinct(data$kabupaten_kota)
sort(unique(data$tahun))
colSums(is.na(data))
data |> count(kabupaten_kota, tahun) |> filter(n > 1)
summary(data$tpt_agustus_persen)
summary(data$kemiskinan_maret_p0_persen)

stopifnot(
  nrow(data) == 72,
  n_distinct(data$kabupaten_kota) == 24,
  setequal(data$tahun, 2023:2025),
  !anyNA(data),
  data$tpt_agustus_persen >= 0, data$tpt_agustus_persen <= 100,
  data$kemiskinan_maret_p0_persen >= 0, data$kemiskinan_maret_p0_persen <= 100
)

write_csv(data_raw, "data/mentah/data_kelompok_2.csv")
saveRDS(data, "data/bersih/data_tpt_kemiskinan_sulsel.rds")
saveRDS(tabel_gabungan, "data_bersih.rds")
saveRDS(tabel_lebar, "data_bersih_lebar.rds")
saveRDS(kode, "kode_wilayah.rds")

View(tabel_gabungan)
View(tabel_lebar)
View(kode[, c("kode_wilayah", "kabupaten_kota")])

# ---------------------------------------------------------------
# 4. batas peta
# ---------------------------------------------------------------
file_peta <- "geoBoundaries-IDN-ADM2.geojson"
peta_ok   <- function() file.exists(file_peta) && file.size(file_peta) > 500000

if (!peta_ok()) {
  meta <- tryCatch(
    fromJSON("https://www.geoboundaries.org/api/current/gbOpen/IDN/ADM2/"),
    error = function(e) NULL)
  gh <- "https://github.com/wmgeolab/geoBoundaries/raw/main/releaseData/gbOpen/IDN/ADM2/"
  urls <- c(meta$simplifiedGeometryGeoJSON, meta$gjDownloadURL,
            paste0(gh, "geoBoundaries-IDN-ADM2_simplified.geojson"),
            paste0(gh, "geoBoundaries-IDN-ADM2.geojson"))
  for (u in urls) {
    if (peta_ok()) break
    try(suppressWarnings(download.file(u, file_peta, mode = "wb", quiet = TRUE)),
        silent = TRUE)
    if (!peta_ok() && file.exists(file_peta)) file.remove(file_peta)
  }
}

batas_idn <- st_read(file_peta, quiet = TRUE) |> st_make_valid()

pusat <- st_coordinates(st_centroid(st_geometry(batas_idn)))
sul <- batas_idn[pusat[, 1] > 117.5 & pusat[, 1] < 122.5 &
                   pusat[, 2] > -8.2 & pusat[, 2] < -1.5, ]
sul$kunci <- kunci(sul$shapeName)

# cocok tepat dulu, sisanya cocok mirip (selisih maksimal 2 huruf)
idx <- match(kode$kunci, sul$kunci)
for (i in which(is.na(idx))) {
  d <- adist(kode$kunci[i], sul$kunci)[1, ]
  d[na.omit(idx)] <- Inf
  if (min(d) <= 2) idx[i] <- which.min(d)
}
ada <- !is.na(idx)

batas <- sul[idx[ada], ] |>
  mutate(kode_wilayah = kode$kode_wilayah[ada]) |>
  left_join(kode[, c("kode_wilayah", "kabupaten_kota")], by = "kode_wilayah") |>
  select(kode_wilayah, kabupaten_kota, nama_geoboundaries = shapeName)

nrow(batas)
setdiff(kode$kabupaten_kota, batas$kabupaten_kota)

data_peta <- batas |>
  select(kode_wilayah) |>
  left_join(tabel_gabungan, by = "kode_wilayah")
saveRDS(batas, "batas_sulsel.rds")
saveRDS(data_peta, "data_peta.rds")
View(st_drop_geometry(batas))

# ---------------------------------------------------------------
# 5. plot peta
# ---------------------------------------------------------------
plot_batas <- ggplot(batas) +
  geom_sf(aes(fill = kabupaten_kota), colour = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = kode_wilayah), size = 2.6, check_overlap = TRUE) +
  scale_fill_hue(l = 80, c = 60, guide = "none") +
  labs(title = "Batas Kabupaten/Kota Sulawesi Selatan",
       subtitle = "Label = kode wilayah", caption = "Sumber: geoBoundaries") +
  tema
print(plot_batas)
simpan(plot_batas, "peta_batas_sulsel.png", "peta", 7, 9)

peta_gabungan <- function(indikator, judul, legenda)
  ggplot(data_peta) +
  geom_sf(aes(fill = {{ indikator }}), colour = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "turbo",
                       guide = guide_colourbar(barwidth = 15, barheight = 0.8)) +
  facet_wrap(~tahun, nrow = 1) +
  labs(title = judul, fill = legenda,
       caption = "Sumber: BPS Provinsi Sulawesi Selatan; batas: geoBoundaries") +
  tema

g1 <- peta_gabungan(pengangguran, "Tingkat Pengangguran Terbuka 2023-2025", "tpt")
g2 <- peta_gabungan(kemiskinan, "Persentase Penduduk Miskin 2023-2025", "miskin (%)")
print(g1)
print(g2)
simpan(g1, "peta_tpt_2023_2025.png", "peta", 14, 6)
simpan(g2, "peta_kemiskinan_2023_2025.png", "peta", 14, 6)

# ---------------------------------------------------------------
# 6. makeover tpt 2024 (before disimpan manual di keluaran/makeover/TPT_before.png)
# ---------------------------------------------------------------
rata_tpt <- mean(data$tpt_agustus_persen[data$tahun == 2024])

data_makeover <- data |>
  filter(tahun == 2024) |>
  arrange(desc(tpt_agustus_persen)) |>
  mutate(kategori = ifelse(tpt_agustus_persen >= rata_tpt,
                           "Di atas rata-rata", "Di bawah rata-rata"))
stopifnot(nrow(data_makeover) == 24)

grafik_after <- ggplot(data_makeover,
                       aes(reorder(kabupaten_kota, tpt_agustus_persen),
                           tpt_agustus_persen, fill = kategori)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = rata_tpt, linetype = "dashed",
             colour = "#333333", linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", tpt_agustus_persen)),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_manual(values = c("Di atas rata-rata" = "#D55E00",
                               "Di bawah rata-rata" = "#0072B2"), name = NULL) +
  scale_y_continuous(limits = c(0, max(data_makeover$tpt_agustus_persen) * 1.12),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Tingkat Pengangguran Terbuka",
       subtitle = paste0("Kabupaten/Kota di Sulawesi Selatan, 2024. ",
                         "Garis putus-putus = rata-rata 24 wilayah (",
                         sprintf("%.2f", rata_tpt), "%)"),
       x = NULL, y = "TPT (%)", caption = "Sumber data: BPS") +
  tema + theme(panel.grid.major.y = element_blank())
print(grafik_after)
simpan(grafik_after, "TPT_after.png", "keluaran/makeover", 10, 8)

write_csv(data_makeover, "keluaran/makeover/data_makeover_2024.csv")
saveRDS(data_makeover, "keluaran/makeover/data_makeover_2024.rds")
View(data_makeover)