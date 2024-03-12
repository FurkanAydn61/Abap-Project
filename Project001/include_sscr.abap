*&---------------------------------------------------------------------*
*& Include          ZABAP_EGITIM_DEMO_PROJECT_SSCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_cbox1 AS CHECKBOX USER-COMMAND cm1,
            p_cbox2 AS CHECKBOX USER-COMMAND cm2,
            p_cbox3 AS CHECKBOX USER-COMMAND cm3.

SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-002.

 PARAMETERS: p_rad1 RADIOBUTTON GROUP gr1 USER-COMMAND rb1,
             p_rad2 RADIOBUTTON GROUP gr1.

SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE TEXT-003.

  PARAMETERS: p_num1 TYPE i MODIF ID m1,
              p_num2 TYPE i MODIF ID m1,
              p_num3 TYPE i MODIF ID m1.

SELECTION-SCREEN END OF BLOCK bl3.

AT SELECTION-SCREEN OUTPUT.

LOOP AT SCREEN.
 CASE 'X'.
 	WHEN p_cbox1.
    IF screen-name CS 'P_NUM2' OR screen-name CS 'P_NUM3'.
        screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
 	WHEN p_cbox2.
    IF screen-name CS 'P_NUM3'.
       screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
 	WHEN p_cbox3.
    IF p_rad1 EQ abap_true AND screen-name CS 'P_NUM2'.
       screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
 ENDCASE.
 ENDLOOP.

 AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'CM1'.
       p_cbox1 = abap_true.
      CLEAR: p_cbox2, p_cbox3.
    WHEN 'CM2'.
       p_cbox2 = abap_true.
      CLEAR: p_cbox1, p_cbox3.

    WHEN 'CM3'.
      p_cbox3 = abap_true.
      CLEAR: p_cbox2, p_cbox1.
  ENDCASE.

  CASE 'X'.
    WHEN p_cbox1.
      PERFORM at_selection_output_kare USING p_num1.
*      IF p_rad1 EQ abap_true.
*      gv_alan = p_num1 * p_num1.
*      ELSEIF p_rad2 EQ abap_true.
*      gv_cevre = p_num1 * 4.
*      ENDIF.
    WHEN p_cbox2 .
      PERFORM at_selection_output_ddort USING p_num1
                                              p_num2.
*      IF p_rad1 EQ abap_true.
*        gv_alan = p_num1 * p_num2.
*      ELSEIF p_rad2 EQ abap_true.
*      gv_cevre = ( 2 * p_num1 ) + ( 2 * p_num2 ).
*      ENDIF.
    WHEN p_cbox3.
      PERFORM at_selection_output_ucg USING p_num1
                                            p_num2
                                            p_num3.
*      IF p_rad1 EQ abap_true.
*      gv_alan = ( p_num1 * p_num3 ) / 2.
*      ELSEIF p_rad2 EQ abap_true.
*      gv_cevre = p_num1 + p_num2 + p_num3.
*      ENDIF.
  ENDCASE.

  START-OF-SELECTION.

WRITE: 'Şeklin alanı :' , gv_alan.
WRITE:/ 'Şeklin çevresi :', gv_cevre.