FUNCTION ZABAP_EGITIM_FMAL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_KKENAR) TYPE  INT4
*"     VALUE(IV_UKENAR) TYPE  INT4 OPTIONAL
*"     VALUE(IV_YUKSEK) TYPE  INT4 OPTIONAL
*"     VALUE(IV_SEC) TYPE  ZABAP_EGITIM_FMCV_DE
*"  EXPORTING
*"     REFERENCE(EV_ALAN) TYPE  INT4
*"  CHANGING
*"     REFERENCE(CV_EQ) TYPE  CHAR20 OPTIONAL
*"  EXCEPTIONS
*"      CV_EQ
*"----------------------------------------------------------------------
IF iv_sec CS 'D' AND iv_kkenar > iv_ukenar.
RAISE cv_eq.
ENDIF.
cv_eq = 'Değiştirilmiş Mesaj'.
CASE iv_sec.
  WHEN 'K'.
    ev_alan = iv_kkenar * iv_kkenar.
  WHEN 'D'.
    ev_alan = iv_kkenar * iv_ukenar.
  WHEN 'U'.
    ev_alan = ( iv_kkenar * iv_yuksek ) / 2.
ENDCASE.


ENDFUNCTION.