
library(forecast)
library(ggplot2)
library(dplyr)
library(scales)

if (!exists("ts_evlenme")) {
  load("evlenme_data.RData")
  cat("✓ Veri yüklendi.\n")
}

n_test  <- 12
n_train <- length(ts_evlenme) - n_test
ts_train <- window(ts_evlenme, end   = time(ts_evlenme)[n_train])
ts_test  <- window(ts_evlenme, start = time(ts_evlenme)[n_train + 1])
gercek   <- as.numeric(ts_test)

cat("=== OTOMATİK ETS ===\n")
ets_auto <- ets(ts_train, model = "ZZZ")
cat("Seçilen model:", ets_auto$method, "\n")
cat("alpha:", round(ets_auto$par["alpha"], 4), "\n")
if ("beta"  %in% names(ets_auto$par))
  cat("beta :", round(ets_auto$par["beta"],  4), "\n")
if ("gamma" %in% names(ets_auto$par))
  cat("gamma:", round(ets_auto$par["gamma"], 4), "\n")

hw_add <- HoltWinters(ts_train, seasonal = "additive")
hw_add_fc <- forecast(hw_add, h = n_test, level = c(80, 95))

hesapla <- function(gercek, tahmin, ad) {
  data.frame(
    Model = ad,
    MAE   = round(mean(abs(gercek - tahmin))),
    RMSE  = round(sqrt(mean((gercek - tahmin)^2))),
    MAPE  = round(mean(abs((gercek - tahmin) / gercek)) * 100, 2)
  )
}

ets_fc_test <- forecast(ets_auto, h = n_test)
ets_tahmin  <- as.numeric(ets_fc_test$mean)
hw_tahmin   <- as.numeric(hw_add_fc$mean)

cat("\n=== PERFORMANS ===\n")
perf <- rbind(
  hesapla(gercek, ets_tahmin, "ETS (Otomatik)"),
  hesapla(gercek, hw_tahmin,  "Holt-Winters (Additive)")
)
print(perf, row.names = FALSE)

cat("\n=== MEVSİMSEL AYRIŞIM ===\n")

decomp <- stl(ts_evlenme, s.window = "periodic", robust = TRUE)

png("ets_stl_dekomposizyon.png", width = 1200, height = 900, res = 150)
plot(decomp,
     main = "TÜİK Evlenme Serisi - STL Ayrıştırması (Trend + Mevsimsel + Artık)")
dev.off()

ay_isimleri <- c("Oca","Şub","Mar","Nis","May","Haz",
                 "Tem","Ağu","Eyl","Eki","Kas","Ara")

mevsim_df <- data.frame(
  ay   = factor(ay_isimleri, levels = ay_isimleri),
  etki = tapply(decomp$time.series[,"seasonal"], cycle(ts_evlenme), mean)
)

cat("\nAylık mevsimsel etki (evlenme sayısı farkı):\n")
print(mevsim_df %>% arrange(desc(etki)))

p_mevsim <- ggplot(mevsim_df, aes(x = ay, y = etki, fill = etki > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_text(aes(label = format(round(etki), big.mark = ".")),
            vjust = ifelse(mevsim_df$etki > 0, -0.3, 1.2), size = 3) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Aylık Mevsimsel Etki - Evlenme Sayısı",
    subtitle = "Kırmızı = o ay evlenme sayısı ortalamadan fazla | Mavi = az",
    x = "Ay", y = "Mevsimsel Etki (kişi)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p_mevsim)
ggsave("ets_mevsimsel_etki.png", p_mevsim, width = 10, height = 5, dpi = 150)

ets_final  <- ets(ts_evlenme, model = "ZZZ")
hw_final   <- HoltWinters(ts_evlenme, seasonal = "additive")

tahmin_ets <- forecast(ets_final, h = 1, level = c(80, 95))
tahmin_hw  <- forecast(hw_final,  h = 1, level = c(80, 95))

son_tarih    <- max(df$tarih)
tahmin_tarih <- seq(son_tarih, by = "month", length.out = 2)[2]

cat(sprintf("\n★ ETS tahmini (%s)         : %.0f evlenme\n",
            format(tahmin_tarih, "%Y-%m"), as.numeric(tahmin_ets$mean)))
cat(sprintf("  80%% GA: [%.0f , %.0f]\n",
            as.numeric(tahmin_ets$lower[,"80%"]),
            as.numeric(tahmin_ets$upper[,"80%"])))

cat(sprintf("\n★ Holt-Winters tahmini (%s): %.0f evlenme\n",
            format(tahmin_tarih, "%Y-%m"), as.numeric(tahmin_hw$mean)))

test_tarihler <- tail(df$tarih, n_test)
son_3yil      <- df %>% filter(tarih >= max(tarih) %m-% years(3))

tahmin_plot <- data.frame(
  tarih  = test_tarihler,
  gercek = gercek,
  ets    = ets_tahmin,
  hw     = hw_tahmin
)

p_ets <- ggplot() +
  geom_line(data = son_3yil,
            aes(x = tarih, y = evlenme),
            color = "gray50", linewidth = 0.8) +
  geom_line(data = tahmin_plot,
            aes(x = tarih, y = gercek),
            color = "
  geom_line(data = tahmin_plot,
            aes(x = tarih, y = ets, color = "ETS"),
            linewidth = 1, linetype = "dashed") +
  geom_line(data = tahmin_plot,
            aes(x = tarih, y = hw, color = "Holt-Winters"),
            linewidth = 1, linetype = "dotdash") +
  geom_point(aes(x = tahmin_tarih,
                 y = as.numeric(tahmin_ets$mean), color = "ETS"),
             size = 4, shape = 18) +
  geom_point(aes(x = tahmin_tarih,
                 y = as.numeric(tahmin_hw$mean), color = "Holt-Winters"),
             size = 4, shape = 17) +
  scale_color_manual(values = c("ETS" = "
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title   = "ETS / Holt-Winters Evlenme Tahmini",
    subtitle= paste("Seçilen ETS:", ets_auto$method),
    x = NULL, y = "Aylık Evlenme Sayısı",
    color = "Model",
    caption = "Şekil = 1 ay sonrası tahmin"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_ets)
ggsave("ets_tahmin.png", p_ets, width = 13, height = 6, dpi = 150)

ets_sonuclar <- list(
  model_ets    = ets_final,
  model_hw     = hw_final,
  tahmin_ets   = tahmin_ets,
  tahmin_hw    = tahmin_hw,
  perf         = perf,
  tahmin_deger = as.numeric(tahmin_ets$mean)
)
save(ets_sonuclar, file = "ets_sonuclar.RData")
cat("\n✓ 03_ets_model.R tamamlandı → ets_tahmin.png, ets_mevsimsel_etki.png\n")
