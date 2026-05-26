
library(forecast)
library(ggplot2)
library(dplyr)

if (!exists("ts_evlenme")) {
  load("evlenme_data.RData")
  cat("✓ Veri yüklendi.\n")
}

n_test  <- 12
n_train <- length(ts_evlenme) - n_test

ts_train <- window(ts_evlenme, end   = time(ts_evlenme)[n_train])
ts_test  <- window(ts_evlenme, start = time(ts_evlenme)[n_train + 1])
gercek   <- as.numeric(ts_test)

cat(sprintf("Eğitim: %d ay | Test: %d ay\n", n_train, n_test))

cat("\n=== AUTO.ARIMA MODEL SEÇİMİ ===\n")

arima_model <- auto.arima(
  ts_train,
  seasonal      = TRUE,
  stepwise      = FALSE,
  approximation = FALSE,
  ic            = "aicc",
  trace         = TRUE
)

cat("\n=== SEÇİLEN MODEL ===\n")
print(summary(arima_model))

cat("\n=== ARTIK ANALİZİ ===\n")

lb  <- Box.test(residuals(arima_model), lag = 24, type = "Ljung-Box")
sw  <- shapiro.test(residuals(arima_model))

cat(sprintf("Ljung-Box p : %.4f → %s\n", lb$p.value,
            ifelse(lb$p.value > 0.05, "✓ Otokorelasyon yok", "⚠ Otokorelasyon var")))
cat(sprintf("Shapiro-Wilk p : %.4f → %s\n", sw$p.value,
            ifelse(sw$p.value > 0.05, "✓ Normal dağılım", "⚠ Normal değil")))

png("arima_artik.png", width = 1400, height = 900, res = 150)
checkresiduals(arima_model)
dev.off()

arima_fc_test <- forecast(arima_model, h = n_test, level = c(80, 95))
tahmin_test   <- as.numeric(arima_fc_test$mean)

mae_a  <- mean(abs(gercek - tahmin_test))
rmse_a <- sqrt(mean((gercek - tahmin_test)^2))
mape_a <- mean(abs((gercek - tahmin_test) / gercek)) * 100

cat(sprintf("\nTest Seti  →  MAE: %.0f | RMSE: %.0f | MAPE: %.2f%%\n",
            mae_a, rmse_a, mape_a))

arima_final   <- auto.arima(ts_evlenme, seasonal = TRUE,
                              stepwise = FALSE, approximation = FALSE, ic = "aicc")
tahmin_1ay    <- forecast(arima_final, h = 1, level = c(80, 95))

son_tarih     <- max(df$tarih)
tahmin_tarih  <- seq(son_tarih, by = "month", length.out = 2)[2]

cat(sprintf("\n★ Sonraki ay tahmini (%s): %.0f evlenme\n",
            format(tahmin_tarih, "%Y-%m"),
            as.numeric(tahmin_1ay$mean)))
cat(sprintf("  80%% GA: [%.0f , %.0f]\n",
            as.numeric(tahmin_1ay$lower[,"80%"]),
            as.numeric(tahmin_1ay$upper[,"80%"])))
cat(sprintf("  95%% GA: [%.0f , %.0f]\n",
            as.numeric(tahmin_1ay$lower[,"95%"]),
            as.numeric(tahmin_1ay$upper[,"95%"])))

test_tarihler <- tail(df$tarih, n_test)

tahmin_df <- data.frame(
  tarih  = test_tarihler,
  gercek = gercek,
  tahmin = tahmin_test,
  alt80  = as.numeric(arima_fc_test$lower[,"80%"]),
  ust80  = as.numeric(arima_fc_test$upper[,"80%"]),
  alt95  = as.numeric(arima_fc_test$lower[,"95%"]),
  ust95  = as.numeric(arima_fc_test$upper[,"95%"])
)

son_3yil <- df %>% filter(tarih >= max(tarih) %m-% years(3))

p_arima <- ggplot() +
  geom_line(data = son_3yil,
            aes(x = tarih, y = evlenme),
            color = "gray50", linewidth = 0.8) +
  geom_ribbon(data = tahmin_df,
              aes(x = tarih, ymin = alt95, ymax = ust95),
              fill = "
  geom_ribbon(data = tahmin_df,
              aes(x = tarih, ymin = alt80, ymax = ust80),
              fill = "
  geom_line(data = tahmin_df,
            aes(x = tarih, y = tahmin),
            color = "
  geom_line(data = tahmin_df,
            aes(x = tarih, y = gercek),
            color = "
  geom_point(aes(x = tahmin_tarih,
                 y = as.numeric(tahmin_1ay$mean)),
             color = "
  geom_text(aes(x = tahmin_tarih,
                y = as.numeric(tahmin_1ay$mean),
                label = paste0(format(round(as.numeric(tahmin_1ay$mean)), big.mark="."), "\nevlenme")),
            nudge_x = 20, color = "
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = paste("ARIMA Evlenme Tahmini |", as.character(arima_final)),
    subtitle = sprintf("Test MAE: %.0f | RMSE: %.0f | MAPE: %.1f%%",
                       mae_a, rmse_a, mape_a),
    x = NULL, y = "Aylık Evlenme Sayısı",
    caption  = "Mavi: Gerçek | Kırmızı kesikli: ARIMA | Şekil: Sonraki ay tahmini"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p_arima)
ggsave("arima_tahmin.png", p_arima, width = 13, height = 6, dpi = 150)

arima_sonuclar <- list(
  model        = arima_final,
  tahmin_1ay   = tahmin_1ay,
  mae          = mae_a,
  rmse         = rmse_a,
  mape         = mape_a,
  tahmin_deger = as.numeric(tahmin_1ay$mean)
)
save(arima_sonuclar, file = "arima_sonuclar.RData")
cat("\n✓ 02_arima_model.R tamamlandı → arima_tahmin.png\n")
