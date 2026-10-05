setwd("C:/Users/User/Downloads/data_utama")
try(graphics.off(), silent = TRUE)

paket <- c("readxl", "dplyr", "tidyr", "readr", "ggplot2")
baru  <- paket[!sapply(paket, requireNamespace, quietly = TRUE)]
if (length(baru)) install.packages(baru)
invisible(lapply(paket, library, character.only = TRUE))

dir.create("keluaran/makeover", recursive = TRUE, showWarnings = FALSE)

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

data <- data_excel |>
  select(-kabupaten_kota) |>
  left_join(kode, by = "kunci") |>
  mutate(jenis = ifelse(as.integer(substr(kode_wilayah, 3, 4)) >= 71,
                        "Kota", "Kabupaten")) |>
  select(kode_wilayah, kabupaten_kota, jenis, tahun,
         kemiskinan_maret_p0_persen = kemiskinan,
         tpt_agustus_persen = pengangguran) |>
  arrange(kode_wilayah, tahun) |>
  as.data.frame()

stopifnot(nrow(data) == 72, !anyNA(data))

# ---------------------------------------------------------------
# makeover tpt 2024 (before disimpan manual di keluaran/makeover/TPT_before.png)
# ---------------------------------------------------------------
rata_tpt <- mean(data$tpt_agustus_persen[data$tahun == 2024])

data_makeover <- data |>
  filter(tahun == 2024) |>
  arrange(desc(tpt_agustus_persen)) |>
  mutate(kategori = ifelse(tpt_agustus_persen >= rata_tpt,
                           "Di atas rata-rata", "Di bawah rata-rata"))

nrow(data_makeover)
n_distinct(data_makeover$kabupaten_kota)
data_makeover |> select(kabupaten_kota, tahun, tpt_agustus_persen, kategori)
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
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = "#1F4E79"),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
print(grafik_after)
ggsave("keluaran/makeover/TPT_after.png", grafik_after,
       width = 10, height = 8, dpi = 300, bg = "white")

write_csv(data_makeover, "keluaran/makeover/data_makeover_2024.csv")
saveRDS(data_makeover, "keluaran/makeover/data_makeover_2024.rds")
View(data_makeover)