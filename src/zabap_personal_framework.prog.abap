REPORT zabap_personal_framework MESSAGE-ID zabap_pers_framework.

INCLUDE zabap_personal_framework_top.
INCLUDE zabap_personal_framework_scr.
INCLUDE zabap_personal_framework_f01.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    IF p_log EQ abap_true.
      CLEAR:  p_fname,
              p_header.

      IF screen-group1 = 'T2'.
        screen-invisible = 1.
        screen-input = 0.
        screen-active = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ELSEIF p_export EQ abap_true.
      CLEAR:  p_fname,
              p_header.

      p_fname = gv_file_name.

      IF screen-group1 = 'T1'.
        screen-invisible = 1.
        screen-input = 0.
        screen-active = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDIF.

  ENDLOOP.

START-OF-SELECTION.

  SELECT *
    FROM spfli
    INTO TABLE gt_alv_print.

END-OF-SELECTION.

  IF p_log EQ 'X'.
    PERFORM f_print_alv USING gt_alv_print.
  ELSEIF p_export EQ 'X'.
    PERFORM f_save_file USING gt_alv_print[].
  ENDIF.
