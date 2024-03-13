FUNCTION zabap_egitim_fmcv.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_EDGE1) TYPE  INT4
*"     VALUE(IV_EDGE2) TYPE  INT4 OPTIONAL
*"     VALUE(IV_EDGE3) TYPE  INT4 OPTIONAL
*"     VALUE(IV_CHO) TYPE  ZABAP_EGITIM_FMCV_DE
*"  EXPORTING
*"     REFERENCE(IV_CEVRE) TYPE  INT4
*"  CHANGING
*"     REFERENCE(CV_EQ) TYPE  CHAR20 OPTIONAL
*"  EXCEPTIONS
*"      CV_EQ
*"----------------------------------------------------------------------
  CASE iv_cho.
    WHEN 'K'.
      iv_cevre = iv_edge1 * 4.
    WHEN 'D'.
      IF iv_edge1 > iv_edge2 .
        RAISE cv_eq.

        cv_eq = 'Değiştirilmiş Mesaj'.
      ELSE.
        iv_cevre = ( 2 * iv_edge1 ) + ( 2 * iv_edge2 ).
      ENDIF.
    WHEN 'U'.
      IF iv_edge1 > iv_edge2 .
        RAISE cv_eq.

        cv_eq = 'Değiştirilmiş Mesaj'.
      ELSE.
        iv_cevre = iv_edge1 + iv_edge2 + iv_edge3.
      ENDIF.

  ENDCASE.


ENDFUNCTION.