# Corrección Metodológica: Script 1 - Cálculo de Ponderaciones del Alquiler

## El Problema Identificado

### ¿Qué Calcula Actualmente el Script 1?

**Líneas 75-82 (cálculo nacional):**

```r
peso_alquiler_anual <- df_completo %>%
  filter(!is.na(PESO_ALQ) & PESO_ALQ >= 0) %>%
  group_by(ANOENC) %>%
  summarise(
    n_observaciones = n(),
    peso_alquiler_medio = weighted.mean(PESO_ALQ, FACTOR, na.rm = TRUE),
    ...
  )
```

Esto computa:
$$\bar{w}_{alquiler} = \frac{\sum_i FACTOR_i \cdot \left(\frac{GASTO\_ALQUILER_i}{GASTO\_SIN\_IMP_i}\right)}{\sum_i FACTOR_i}$$

**= Proporción media del presupuesto que los hogares dedican al alquiler** (media de ratios)

### ¿Qué Necesitan Realmente las Ponderaciones del IPC?

$$w_{alquiler}^{IPC} = \frac{\sum_i FACTOR_i \cdot GASTO\_ALQUILER_i}{\sum_i FACTOR_i \cdot GASTO\_SIN\_IMP_i}$$

**= Proporción del gasto agregado total que va a alquiler** (ratio de totales)

## ¿Por Qué Importa Esta Diferencia?

Los dos enfoques **solo coinciden** si la proporción de alquiler está incorrelacionada con el gasto total del hogar. En la práctica:

- **Hogares de menores ingresos**: Suelen gastar 40-50% de su presupuesto en alquiler
- **Hogares de mayores ingresos**: Suelen gastar 15-25% de su presupuesto en alquiler

### El Sesgo Resultante

Si usas la **media de ratios**:

- Das el mismo "peso representativo" a cada tipo de hogar
- **Sobrestimas** la ponderación del alquiler porque no consideras que los hogares con mayor gasto contribuyen más al gasto agregado total
- Cuando fuerzas que la cesta sume 1000 en el Script 2, comprimes demasiado el resto de categorías

### Ejemplo Ilustrativo

Imagina solo 2 hogares inquilinos (simplificado):

| Hogar | Gasto Total | Gasto Alquiler | % Alquiler | FACTOR |
|-------|-------------|----------------|------------|---------|
| A (bajo ingreso) | 1,000€ | 400€ | 40% | 1,000 |
| B (alto ingreso) | 3,000€ | 600€ | 20% | 1,000 |

**Media de ratios (método actual):**

- (40% × 1,000 + 20% × 1,000) / 2,000 = **30%**

**Ratio de totales (método correcto IPC):**

- (400€ × 1,000 + 600€ × 1,000) / (1,000€ × 1,000 + 3,000€ × 1,000) = 1,000,000€ / 4,000,000€ = **25%**

La diferencia: **5 puntos porcentuales** de sobrestimación.

---

## 🔧 CORRECCIONES NECESARIAS EN SCRIPT 1

### 1️⃣ Corrección para Cálculo Nacional

**REEMPLAZAR las líneas 75-87** (sección "peso_alquiler_anual"):

```r
# ❌ MÉTODO ACTUAL (INCORRECTO para propósitos del IPC)
peso_alquiler_anual <- df_completo %>%
  filter(!is.na(PESO_ALQ) & PESO_ALQ >= 0) %>%
  group_by(ANOENC) %>%
  summarise(
    n_observaciones = n(),
    peso_alquiler_medio = weighted.mean(PESO_ALQ, FACTOR, na.rm = TRUE),
    peso_total_ponderado = sum(PESO_ALQ * FACTOR, na.rm = TRUE),
    suma_factores = sum(FACTOR, na.rm = TRUE)
  ) %>%
  mutate(
    peso_alquiler_medio = round(peso_alquiler_medio, 3),
  ) %>%
  arrange(ANOENC)
```

**POR ESTE CÓDIGO CORREGIDO:**

```r
# ✅ MÉTODO CORREGIDO (ratio de totales - consistente con metodología IPC)
peso_alquiler_anual <- df_completo %>%
  filter(!is.na(GASTO_ALQUILER) & !is.na(GASTO_SIN_IMP) &
         GASTO_ALQUILER >= 0 & GASTO_SIN_IMP > 0) %>%
  group_by(ANOENC) %>%
  summarise(
    n_observaciones = n(),
    # Ratio de totales: gasto agregado en alquiler / gasto agregado total
    gasto_total_alquiler_ponderado = sum(GASTO_ALQUILER * FACTOR, na.rm = TRUE),
    gasto_total_sin_imp_ponderado = sum(GASTO_SIN_IMP * FACTOR, na.rm = TRUE),
    peso_alquiler_medio = gasto_total_alquiler_ponderado / gasto_total_sin_imp_ponderado,

    # OPCIONAL: Mantener cálculo antiguo para comparación
    peso_alquiler_medio_OLD = weighted.mean(GASTO_ALQUILER / GASTO_SIN_IMP, FACTOR, na.rm = TRUE),
    diferencia_metodos = peso_alquiler_medio_OLD - peso_alquiler_medio
  ) %>%
  mutate(
    peso_alquiler_medio = round(peso_alquiler_medio, 3),
  ) %>%
  arrange(ANOENC)
```

### 2️⃣ Corrección para Cálculo por CCAA

**REEMPLAZAR las líneas 92-105** (sección "peso_alquiler_anual_ccaa"):

```r
# ❌ MÉTODO ACTUAL (INCORRECTO)
peso_alquiler_anual_ccaa <- df_completo %>%
  filter(!is.na(PESO_ALQ) & PESO_ALQ >= 0) %>%
  group_by(ANOENC, CCAA) %>%
  summarise(
    n_observaciones = n(),
    peso_alquiler_medio = weighted.mean(PESO_ALQ, FACTOR, na.rm = TRUE),
    peso_total_ponderado = sum(PESO_ALQ * FACTOR, na.rm = TRUE),
    suma_factores = sum(FACTOR, na.rm = TRUE)
  ) %>%
  mutate(
    peso_alquiler_medio = round(peso_alquiler_medio, 3),
  ) %>%
  arrange(CCAA, ANOENC)
```

**POR ESTE CÓDIGO CORREGIDO:**

```r
# ✅ MÉTODO CORREGIDO (ratio de totales - consistente con metodología IPC)
peso_alquiler_anual_ccaa <- df_completo %>%
  filter(!is.na(GASTO_ALQUILER) & !is.na(GASTO_SIN_IMP) &
         GASTO_ALQUILER >= 0 & GASTO_SIN_IMP > 0) %>%
  group_by(ANOENC, CCAA) %>%
  summarise(
    n_observaciones = n(),
    # Ratio de totales por CCAA
    gasto_total_alquiler_ponderado = sum(GASTO_ALQUILER * FACTOR, na.rm = TRUE),
    gasto_total_sin_imp_ponderado = sum(GASTO_SIN_IMP * FACTOR, na.rm = TRUE),
    peso_alquiler_medio = gasto_total_alquiler_ponderado / gasto_total_sin_imp_ponderado,
    .groups = 'drop'
  ) %>%
  mutate(
    peso_alquiler_medio = round(peso_alquiler_medio, 3)
  ) %>%
  arrange(CCAA, ANOENC)
```

---

## 📊 Diagnóstico: ¿Cuánto Importa en Tus Datos?

**AÑADIR ESTE CÓDIGO** después de las correcciones para verificar el impacto:

```r
###################### DIAGNÓSTICO: Comparación de Métodos ####

# Comparar ambos métodos para ver la magnitud del sesgo
comparacion_metodos <- df_completo %>%
  filter(!is.na(GASTO_ALQUILER) & !is.na(GASTO_SIN_IMP) &
         GASTO_ALQUILER >= 0 & GASTO_SIN_IMP > 0) %>%
  group_by(ANOENC) %>%
  summarise(
    # Método 1: Media de ratios (lo que tenías antes)
    metodo_1_media_ratios = weighted.mean(GASTO_ALQUILER / GASTO_SIN_IMP, FACTOR),

    # Método 2: Ratio de totales (lo que necesitas)
    metodo_2_ratio_totales = sum(GASTO_ALQUILER * FACTOR) / sum(GASTO_SIN_IMP * FACTOR),

    # Diferencias
    diferencia_absoluta = metodo_1_media_ratios - metodo_2_ratio_totales,
    diferencia_relativa_pct = (metodo_1_media_ratios / metodo_2_ratio_totales - 1) * 100,

    n_hogares = n()
  )

print("=== COMPARACIÓN DE MÉTODOS ===")
print(comparacion_metodos)

# Verificar correlación entre proporción de alquiler y gasto total
# (Si es negativa, confirma el sesgo)
correlacion_test <- df_completo %>%
  filter(!is.na(GASTO_ALQUILER) & !is.na(GASTO_SIN_IMP) &
         GASTO_ALQUILER >= 0 & GASTO_SIN_IMP > 0) %>%
  mutate(proporcion_alquiler = GASTO_ALQUILER / GASTO_SIN_IMP) %>%
  summarise(
    correlacion = cor(proporcion_alquiler, GASTO_SIN_IMP,
                     method = "pearson", use = "complete.obs")
  )

cat("\n=== CORRELACIÓN ENTRE % ALQUILER Y GASTO TOTAL ===\n")
cat("Correlación:", round(correlacion_test$correlacion, 3), "\n")
cat("Interpretación:\n")
cat("  • Negativa = hogares de menor gasto tienen mayor % de alquiler\n")
cat("  • Esto confirma que el método 'media de ratios' sobrestima la ponderación\n\n")
```

---

## 🎯 Resumen Ejecutivo

### El Problema

Tu script calcula la **media de las proporciones individuales de alquiler**, cuando las ponderaciones del IPC requieren la **proporción del gasto agregado total**.

### El Impacto Esperado

- Probablemente sobrestimas la ponderación del alquiler en **2-5 puntos porcentuales**
- Ejemplo: si el ratio correcto es 25%, tu método actual podría dar 27-30%
- Esto comprime artificialmente el resto de categorías en el Script 2

### La Solución

Cambiar de:

```r
weighted.mean(PESO_ALQ, FACTOR)  # ❌ Media de ratios
```

A:

```r
sum(GASTO_ALQUILER * FACTOR) / sum(GASTO_SIN_IMP * FACTOR)  # ✅ Ratio de totales
```

### Próximos Pasos

1. ✅ Aplicar las correcciones en las líneas indicadas
2. ✅ Ejecutar el código de diagnóstico para verificar la magnitud del sesgo
3. ✅ Revisar que las nuevas ponderaciones sumen correctamente en Script 2
4. ✅ Documentar la diferencia con la metodología anterior

---

## 📚 Referencias Metodológicas

Esta corrección alinea tu cálculo con la metodología estándar de índices de precios tipo Laspeyres, donde las ponderaciones representan **participaciones en el gasto agregado** de la población objetivo, no promedios de participaciones individuales.

Para más detalles, consultar:

- Manual del IPC del INE (sección sobre cálculo de ponderaciones)
- ILO Consumer Price Index Manual (capítulo sobre weights construction)
