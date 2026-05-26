
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readxl,
  tidyverse,
  lubridate,
  tseries,
  forecast,
  ggplot2,
  scales
)

xls_yolu <- "İl ve aya göre evlenmeler.xls"

ham_veri <- read_excel(
  xls_yolu,
  skip      = 4,
  col_names = c("yil", "il", "toplam",
                "oca","sub","mar","nis","may","haz",
                "tem","agu","eyl","eki","kas","ara"),
  col_types = "text"
)

turkiye <- ham_veri %>%
  filter(grepl("Türkiye|Turkey", il, ignore.case = TRUE)) %>%
  mutate(
    yil    = as.integer(gsub("\\(r\\)|\\(p\\)", "", yil)),
    toplam = as.numeric(gsub("\\.", "", toplam)),
    across(oca:ara, ~ as.numeric(gsub("\\.", "", .x)))
  ) %>%
  filter(!is.na(yil)) %>%
  arrange(yil)

cat("=== VERİ ÖZET ===\n")
cat("Yıl aralığı  :", min(turkiye$yil), "-", max(turkiye$yil), "\n")
cat("Yıl sayısı   :", nrow(turkiye), "\n")
cat("Toplam evlenme (son yıl):", tail(turkiye$toplam, 1), "\n\n")

ay_sirasi <- c("oca","sub","mar","nis","may","haz",
               "tem","agu","eyl","eki","kas","ara")

df <- turkiye %>%
  select(yil, all_of(ay_sirasi)) %>%
  pivot_longer(cols = all_of(ay_sirasi),
               names_to  = "ay_kisa",
               values_to = "evlenme") %>%
  mutate(
    ay_no = match(ay_kisa, ay_sirasi),
    tarih = as.Date(paste(yil, ay_no, "01", sep = "-"))
  ) %>%
  arrange(tarih) %>%
  filter(!is.na(evlenme))

cat("Aylık gözlem sayısı:", nrow(df), "\n")
cat("Başlangıç           :", format(min(df$tarih), "%Y-%m"), "\n")
cat("Bitiş               :", format(max(df$tarih), "%Y-%m"), "\n\n")
print(tail(df %>% select(tarih, evlenme), 12))

ts_evlenme <- ts(
  df$evlenme,
  start     = c(min(df$yil), 1),
  frequency = 12
)

p1 <- ggplot(df, aes(x = tarih, y = evlenme)) +
  geom_line(color = "
  geom_smooth(method = "loess", se = FALSE,
              color = "
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Türkiye Aylık Evlenme Sayısı (2001-2025)",
    subtitle = "TÜİK | İl ve aya göre evlenmeler",
    x = NULL, y = "Evlenme Sayısı",
    caption  = "Kesikli mavi = LOESS trend | Kaynak: TÜİK"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p1)
ggsave("evlenme_zaman_serisi.png", p1, width = 13, height = 5, dpi = 150)

ay_isimleri <- c("Oca","Şub","Mar","Nis","May","Haz",
                 "Tem","Ağu","Eyl","Eki","Kas","Ara")

p2 <- df %>%
  mutate(ay_f = factor(month(tarih), levels = 1:12, labels = ay_isimleri)) %>%
  ggplot(aes(x = ay_f, y = evlenme, fill = ay_f)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.5) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "C") +
  labs(
    title    = "Aylara Göre Evlenme Dağılımı",
    subtitle = "Her kutuda 2001-2025 yıllarına ait değerler",
    x = "Ay", y = "Evlenme Sayısı"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p2)
ggsave("evlenme_mevsimsel_kutu.png", p2, width = 11, height = 5, dpi = 150)

p3 <- turkiye %>%
  ggplot(aes(x = yil, y = toplam)) +
  geom_col(fill = "
  geom_text(aes(label = format(toplam, big.mark = ".")),
            vjust = -0.3, size = 2.8) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Yıllık Toplam Evlenme Sayısı (2001-2025)",
    x = "Yıl", y = "Evlenme Sayısı"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p3)
ggsave("evlenme_yillik_toplam.png", p3, width = 12, height = 5, dpi = 150)

cat("\n=== DURAĞANLIK ANALİZİ ===\n")

adf_sonuc  <- adf.test(ts_evlenme, alternative = "stationary")
kpss_sonuc <- kpss.test(ts_evlenme)

cat(sprintf("ADF  p-değeri : %.4f → %s\n", adf_sonuc$p.value,
            ifelse(adf_sonuc$p.value < 0.05, "DURAĞAN ✓", "DURAĞAN DEĞİL ✗")))
cat(sprintf("KPSS p-değeri : %.4f → %s\n", kpss_sonuc$p.value,
            ifelse(kpss_sonuc$p.value > 0.05, "DURAĞAN ✓", "DURAĞAN DEĞİL ✗")))

png("evlenme_acf_pacf.png", width = 1200, height = 500, res = 150)
par(mfrow = c(1, 2))
acf(ts_evlenme,  lag.max = 48, main = "ACF - Evlenme Serisi")
pacf(ts_evlenme, lag.max = 48, main = "PACF - Evlenme Serisi")
dev.off()

save(df, turkiye, ts_evlenme, file = "evlenme_data.RData")
cat("\n✓ 01_data_prep.R tamamlandı.\n")
cat("Çıktılar: evlenme_zaman_serisi.png | evlenme_mevsimsel_kutu.png | evlenme_yillik_toplam.png\n")
