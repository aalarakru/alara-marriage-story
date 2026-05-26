
library(forecast)
library(ggplot2)
library(dplyr)
library(scales)

if (!exists("ts_evlenme")) load("evlenme_data.RData")
if (!exists("arima_sonuclar")) load("arima_sonuclar.RData")
if (!exists("ets_sonuclar"))   load("ets_sonuclar.RData")

n_test  <- 12
n_train <- length(ts_evlenme) - n_test
ts_train <- window(ts_evlenme, end   = time(ts_evlenme)[n_train])
ts_test  <- window(ts_evlenme, start = time(ts_evlenme)[n_train + 1])
gercek   <- as.numeric(ts_test)

arima_m  <- auto.arima(ts_train, seasonal = TRUE,
                        stepwise = FALSE, approximation = FALSE, ic = "aicc")
ets_m    <- ets(ts_train, model = "ZZZ")
hw_m     <- HoltWinters(ts_train, seasonal = "additive")
naive_m  <- naive(ts_train, h = n_test)
snaive_m <- snaive(ts_train, h = n_test)

arima_t  <- as.numeric(forecast(arima_m,  h = n_test)$mean)
ets_t    <- as.numeric(forecast(ets_m,    h = n_test)$mean)
hw_t     <- as.numeric(forecast(hw_m,     h = n_test)$mean)
naive_t  <- as.numeric(naive_m$mean)
snaive_t <- as.numeric(snaive_m$mean)

hesapla <- function(g, t, ad) {
  data.frame(
    Model  = ad,
    MAE    = round(mean(abs(g - t))),
    RMSE   = round(sqrt(mean((g - t)^2))),
    MAPE   = round(mean(abs((g - t) / g)) * 100, 2),
    TheilU = round(sqrt(mean((g - t)^2)) /
                   sqrt(mean((g - naive_t)^2) + 1e-6), 3)
  )
}

perf_df <- rbind(
  hesapla(gercek, arima_t,  "ARIMA"),
  hesapla(gercek, ets_t,    "ETS (Otomatik)"),
  hesapla(gercek, hw_t,     "Holt-Winters"),
  hesapla(gercek, naive_t,  "Naive (Benchmark)"),
  hesapla(gercek, snaive_t, "Seasonal Naive")
) %>% arrange(RMSE)

cat("=== MODEL KARŞILAŞTIRMASI ===\n")
print(perf_df, row.names = FALSE)
cat("\nEn iyi model:", perf_df$Model[1],
    "| TheilU < 1 = Naive'den iyi\n")

aktif <- perf_df %>% filter(Model %in% c("ARIMA","ETS (Otomatik)","Holt-Winters"))
agirl <- (1 / aktif$RMSE) / sum(1 / aktif$RMSE)

cat("\nEnsemble ağırlıkları:\n")
for (i in seq_along(aktif$Model))
  cat(sprintf("  %-22s: %.1f%%\n", aktif$Model[i], agirl[i]*100))

ens_t <- arima_t * agirl[aktif$Model == "ARIMA"] +
         ets_t   * agirl[aktif$Model == "ETS (Otomatik)"] +
         hw_t    * agirl[aktif$Model == "Holt-Winters"]

ens_perf <- hesapla(gercek, ens_t, "Ensemble")
cat("\nEnsemble:", sprintf("MAE=%.0f | RMSE=%.0f | MAPE=%.1f%%\n",
                            ens_perf$MAE, ens_perf$RMSE, ens_perf$MAPE))

arima_f <- auto.arima(ts_evlenme, seasonal = TRUE,
                       stepwise = FALSE, approximation = FALSE, ic = "aicc")
ets_f   <- ets(ts_evlenme, model = "ZZZ")
hw_f    <- HoltWinters(ts_evlenme, seasonal = "additive")

t_arima    <- as.numeric(forecast(arima_f, h = 1)$mean)
t_ets      <- as.numeric(forecast(ets_f,   h = 1)$mean)
t_hw       <- as.numeric(forecast(hw_f,    h = 1)$mean)
t_ensemble <- t_arima * agirl[1] + t_ets * agirl[2] + t_hw * agirl[3]

son_tarih    <- max(df$tarih)
tahmin_tarih <- seq(son_tarih, by = "month", length.out = 2)[2]

son_tahmin <- data.frame(
  Model  = c("ARIMA", "ETS", "Holt-Winters", "Ensemble ★"),
  Tahmin = round(c(t_arima, t_ets, t_hw, t_ensemble))
)

cat(sprintf("\n=== %s TAHMİNLERİ ===\n", format(tahmin_tarih, "%Y-%m")))
print(son_tahmin, row.names = FALSE)

test_tarihler <- tail(df$tarih, n_test)
son_3yil      <- df %>% filter(tarih >= max(tarih) %m-% years(3))

plot_long <- bind_rows(
  data.frame(tarih=test_tarihler, deger=arima_t,  model="ARIMA"),
  data.frame(tarih=test_tarihler, deger=ets_t,    model="ETS"),
  data.frame(tarih=test_tarihler, deger=hw_t,     model="Holt-Winters"),
  data.frame(tarih=test_tarihler, deger=ens_t,    model="Ensemble")
)

nokta_df <- data.frame(
  tarih  = tahmin_tarih,
  Tahmin = c(t_arima, t_ets, t_hw, t_ensemble),
  model  = c("ARIMA","ETS","Holt-Winters","Ensemble")
)

p_main <- ggplot() +
  geom_line(data = son_3yil,
            aes(x = tarih, y = evlenme),
            color = "gray60", linewidth = 0.7) +
  geom_line(data = data.frame(tarih=test_tarihler, deger=gercek),
            aes(x = tarih, y = deger),
            color = "black", linewidth = 1.2) +
  geom_line(data = plot_long,
            aes(x = tarih, y = deger, color = model),
            linewidth = 0.9) +
  geom_point(data = nokta_df,
             aes(x = tarih, y = Tahmin, color = model),
             size = 4) +
  geom_text(data = nokta_df,
            aes(x = tarih, y = Tahmin,
                label = format(Tahmin, big.mark="."), color = model),
            nudge_x = 15, fontface = "bold", size = 3.2) +
  scale_color_manual(values = c(
    "ARIMA"        = "
    "ETS"          = "
    "Holt-Winters" = "
    "Ensemble"     = "
  )) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title   = "Evlenme Tahmin Modelleri — Karşılaştırma",
    subtitle= sprintf("Ensemble tahmini (%s): %s evlenme",
                      format(tahmin_tarih, "%Y-%m"),
                      format(round(t_ensemble), big.mark=".")),
    x = NULL, y = "Aylık Evlenme Sayısı",
    color = "Model",
    caption= "Siyah: Gerçek (test) | Noktalar: sonraki ay tahmini | Kaynak: TÜİK"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_main)
ggsave("model_karsilastirma.png", p_main, width = 14, height = 7, dpi = 150)

tum_perf <- rbind(perf_df, ens_perf) %>%
  mutate(en_iyi = RMSE == min(RMSE),
         Model  = factor(Model, levels = rev(Model)))

p_rmse <- ggplot(tum_perf, aes(x = Model, y = RMSE, fill = en_iyi)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = format(RMSE, big.mark=".")),
            hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("FALSE"="
  coord_flip() +
  labs(title = "Model Karşılaştırması — RMSE (Düşük = İyi)",
       x = NULL, y = "RMSE (evlenme sayısı)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold"),
        panel.grid.major.y = element_blank())

print(p_rmse)
ggsave("model_rmse.png", p_rmse, width = 10, height = 5, dpi = 150)

cat("\n", strrep("=",60), "\n")
cat("      TÜİK EVLENme TAHMİN RAPORU — Alara Kuru\n")
cat(strrep("=",60), "\n")
cat(sprintf("Veri     : İl ve aya göre evlenmeler, 2001-2025\n"))
cat(sprintf("Gözlem   : %d aylık\n", length(ts_evlenme)))
cat(sprintf("Son veri : %s — %s evlenme\n",
            format(max(df$tarih),"%Y-%m"),
            format(tail(df$evlenme,1), big.mark=".")))
cat(strrep("-",60), "\n")
cat("MODEL PERFORMANSI (Test - Son 12 Ay):\n")
for (i in 1:nrow(perf_df)) {
  flag <- if(i==1) " ← EN İYİ" else ""
  cat(sprintf("  %-22s RMSE:%7s | MAPE:%.1f%%%s\n",
              perf_df$Model[i],
              format(perf_df$RMSE[i], big.mark="."),
              perf_df$MAPE[i], flag))
}
cat(strrep("-",60), "\n")
cat(sprintf("TAHMİN (%s):\n", format(tahmin_tarih,"%Y-%m")))
for (i in 1:nrow(son_tahmin))
  cat(sprintf("  %-20s: %s evlenme\n",
              son_tahmin$Model[i],
              format(son_tahmin$Tahmin[i], big.mark=".")))
cat(strrep("=",60), "\n")

save(list=ls(), file="tum_sonuclar.RData")
cat("\n✓ 04_compare_forecast.R tamamlandı!\n")
