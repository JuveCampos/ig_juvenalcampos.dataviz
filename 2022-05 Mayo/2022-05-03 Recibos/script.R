datos_desde_base <- function(base){
  iva <- base*0.16
  subtotal <- base + iva
  retencion_isr <- base*0.10
  retencion_iva <- base*0.106667
  neto <- subtotal -
    (retencion_isr +
       retencion_iva)
  return(
    list(
      "IVA: " = iva,
      "Subtotal: " = subtotal,
      "Retención ISR: " = retencion_isr,
      "Retención IVA: " = retencion_iva,
      "Neto: " = neto
    )
  )
}

datos_desde_base(base = 1500)
