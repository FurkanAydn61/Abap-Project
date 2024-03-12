*&---------------------------------------------------------------------*
*& Include          ZABAP_EGITIM_DEMO_PROJECT_FFRM
*&---------------------------------------------------------------------*

FORM at_selection_output_kare USING p_num1.
  CASE 'X'.
    WHEN p_rad1.
      gv_alan = p_num1 * p_num1.
    WHEN p_rad2.
      gv_cevre = p_num1 * 4.
    WHEN OTHERS.
  ENDCASE.
  ENDFORM.

FORM at_selection_output_ddort USING p_num1
                                     p_num2.
  CASE 'X'.
    WHEN p_rad1.
      gv_alan = p_num1 * p_num2.
    WHEN p_rad2.
      gv_cevre = ( 2 * p_num1 ) + ( 2 * p_num2 ).
  ENDCASE.
  ENDFORM.

FORM at_selection_output_ucg USING p_num1
                                   p_num2
                                   p_num3.
  CASE 'X'.
    WHEN p_rad1.
      gv_alan = ( p_num1 * p_num3 ) / 2.
    WHEN p_rad2.
      gv_cevre = p_num1 + p_num2 + p_num3.
     ENDCASE.
  ENDFORM.