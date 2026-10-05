setwd("C:/Users/User/Downloads/data_utama")

paket <- c("readxl", "dplyr", "tidyr", "readr")
baru  <- paket[!sapply(paket, requireNamespace, quietly = TRUE)]
if (length(baru)) install.packages(baru)
invisible(lapply(paket, library, character.only = TRUE))

for (d in c("data/mentah", "data/bersih"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

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

# ---------------------------------------------------------------
# pembersihan data
# ---------------------------------------------------------------
data_raw <- tabel_gabungan
data <- data_raw |>
  rename(tpt_agustus_persen = pengangguran,
         kemiskinan_maret_p0_persen = kemiskinan) |>
  mutate(kode_wilayah = as.character(kode_wilayah),
         kabupaten_kota = as.character(kabupaten_kota),
         tahun = as.integer(tahun))

# ---------------------------------------------------------------
# pemeriksaan data
# ---------------------------------------------------------------
dim(data)
n_distinct(data$kabupaten_kota)
sort(unique(data$tahun))
colSums(is.na(data))
data[!complete.cases(data), ]
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

# ---------------------------------------------------------------
# simpan
# ---------------------------------------------------------------
write_csv(data_raw, "data/mentah/data_kelompok_2.csv")
saveRDS(data, "data/bersih/data_tpt_kemiskinan_sulsel.rds")

View(data_raw)
View(data)