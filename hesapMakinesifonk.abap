*&---------------------------------------------------------------------*
*& Report ZABAP_EGITIM_FDENEME
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*



REPORT zabap_egitim_fdeneme.


DATA: gv_result TYPE int4,
      gv_mes    TYPE char20,
      gv_text   TYPE char100.

START-OF-SELECTION.

  gv_mes = 'İşlem 1'.

  SELECTION-SCREEN BEGIN OF  BLOCK bl1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_num1 TYPE int4,
              p_num2 TYPE int4.

  SELECTION-SCREEN END OF BLOCK bl1.

  SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_cbx1 AS CHECKBOX USER-COMMAND cb1,
              p_cbx2 AS CHECKBOX USER-COMMAND cb2,
              p_cbx3 AS CHECKBOX USER-COMMAND cb3,
              p_cbx4 AS CHECKBOX USER-COMMAND cb4.

  SELECTION-SCREEN END OF BLOCK bl2.




AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'CB1'.
      p_cbx1 = abap_true.
      CLEAR:p_cbx2,p_cbx3,p_cbx4.
    WHEN 'CB2'.
      p_cbx2 = abap_true.
      CLEAR:p_cbx1,p_cbx3,p_cbx4.
    WHEN 'CB3'.
      p_cbx3 = abap_true.
      CLEAR:p_cbx1,p_cbx2,p_cbx4.
    WHEN 'CB4'.
      p_cbx4 = abap_true.
      CLEAR:p_cbx2,p_cbx3,p_cbx1.
  ENDCASE.

  CASE 'X'.
    WHEN p_cbx1.

      CALL FUNCTION 'ZABAP_EGITIM_FMDORT'
        EXPORTING
          iv_num3       = p_num1
          iv_num4       = p_num2
          iv_sec        = '+'
       IMPORTING
         EV_RES        = gv_result
       CHANGING
         CV_HEL        = gv_mes
       EXCEPTIONS
         CV_HEL        = 1
         OTHERS        = 2
                .
      IF sy-subrc EQ 0.
        gv_text = 'Toplamları :' && gv_result.
       ELSEIF sy-subrc EQ 1.
         MESSAGE 'Birinci sayı ikinci sayıya eşit olamaz' TYPE 'E' DISPLAY LIKE 'I'.
       ENDIF.

*      CALL FUNCTION 'ZABAP_EGITIM_FDMODUL'
*        EXPORTING
*          iv_num1         = p_num1
*          iv_num2         = p_num2
*        IMPORTING
*          ev_sonuc        = gv_result
*        CHANGING
*          cv_mess         = gv_mes
*        EXCEPTIONS
*          check_parameter = 1
*          OTHERS          = 2.
*      IF sy-subrc EQ 0.
** Implement suitable error handling here
*        gv_text = 'Toplamları :' && gv_result.
*      ELSEIF sy-subrc EQ 1.
*        MESSAGE 'İkinci sayı birinci sayıdan küçük olamaz' TYPE 'E' DISPLAY LIKE 'I'.
*      ENDIF.
    WHEN p_cbx2.

      CALL FUNCTION 'ZABAP_EGITIM_FMDORT'
        EXPORTING
          iv_num3       = p_num1
          iv_num4       = p_num2
          iv_sec        = '-'
       IMPORTING
         EV_RES        = gv_result
       CHANGING
         CV_HEL        = gv_mes
       EXCEPTIONS
         CV_HEL        = 1
         OTHERS        = 2
                .
      IF sy-subrc EQ 0.
        gv_text = 'Çıkarımları :' && gv_result.
       ELSEIF sy-subrc EQ 1.
         MESSAGE 'Birinci sayı ikinci sayıya eşit olamaz' TYPE 'E' DISPLAY LIKE 'I'.
       ENDIF.


    WHEN p_cbx3.
    CALL FUNCTION 'ZABAP_EGITIM_FMDORT'
                       EXPORTING
                         iv_num3       = p_num1
                         iv_num4       = p_num2
                         iv_sec        = '*'
                      IMPORTING
                        EV_RES        = gv_result
                      CHANGING
                        CV_HEL        = gv_mes
                      EXCEPTIONS
                        CV_HEL        = 1
                        OTHERS        = 2
                               .
                     IF sy-subrc EQ 0.
                     gv_text = 'Çarpımları :' && gv_result.
* Implement suitable error handling here
                     ELSEIF sy-subrc EQ 1.
                     MESSAGE 'Sayılar birbirne eşit olamaz' TYPE 'E' DISPLAY LIKE 'I'.
                     ENDIF.
    WHEN p_cbx4.
      CALL FUNCTION 'ZABAP_EGITIM_FMDORT'
                       EXPORTING
                         iv_num3       = p_num1
                         iv_num4       = p_num2
                         iv_sec        = '/'
                      IMPORTING
                        EV_RES        = gv_result
                      CHANGING
                        CV_HEL        = gv_mes
                      EXCEPTIONS
                        CV_HEL        = 1
                        OTHERS        = 2
                               .
                     IF sy-subrc EQ 0.
                     gv_text = 'Bölümleri :' && gv_result.
                     ELSEIF sy-subrc EQ 0.
                     MESSAGE: 'İki sayı birbirine eşit olamaz' TYPE 'E' DISPLAY LIKE 'I'.
* Implement suitable error handling here
                     ENDIF.
  ENDCASE.

END-OF-SELECTION.
  WRITE: gv_text.