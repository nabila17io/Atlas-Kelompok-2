setwd("C:/Users/User/Downloads/data_utama")
try(graphics.off(), silent = TRUE)

paket <- c("readxl", "dplyr", "tidyr", "sf", "jsonlite", "ggplot2")
baru  <- paket[!sapply(paket, requireNamespace, quietly = TRUE)]
if (length(baru)) install.packages(baru)
invisible(lapply(paket, library, character.only = TRUE))
suppressMessages(sf_use_s2(FALSE))
options(timeout = 900)

dir.create("peta", showWarnings = FALSE)

kunci <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("^(kabupaten|kota|kab\\.|kab)\\s+", "", x)
  x <- gsub("[^a-z]", "", x)
  alias <- c(pangkajenedankepulauan = "pangkep", pangkajenekepulauan = "pangkep",
             pangkajene = "pangkep", sidenrengrappang = "sidrap",
             sidenreng = "sidrap", selayar = "kepulauanselayar")
  ifelse(x %in% names(alias), unname(alias[x]), x)
}

# ---------------------------------------------------------------
# data excel dan kode wilayah
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

stopifnot(nrow(tabel_gabungan) == 72, !anyNA(tabel_gabungan$kode_wilayah))

# ---------------------------------------------------------------
# batas peta
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
saveRDS(batas, "batas_sulsel.rds")

# ---------------------------------------------------------------
# plot batas wilayah + kode wilayah
# ---------------------------------------------------------------
plot_batas <- ggplot(batas) +
  geom_sf(aes(fill = kabupaten_kota), colour = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = kode_wilayah), size = 2.6, check_overlap = TRUE) +
  scale_fill_hue(l = 80, c = 60, guide = "none") +
  labs(title = "Batas Kabupaten/Kota Sulawesi Selatan",
       subtitle = "Label = kode wilayah", caption = "Sumber: geoBoundaries") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#1F4E79"))
print(plot_batas)
ggsave("peta/peta_batas_sulsel.png", plot_batas,
       width = 7, height = 9, dpi = 300, bg = "white")

View(tabel_gabungan)
View(tabel_lebar)
View(kode[, c("kode_wilayah", "kabupaten_kota")])
View(st_drop_geometry(batas))