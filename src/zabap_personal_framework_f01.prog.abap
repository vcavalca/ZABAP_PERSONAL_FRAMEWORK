*&---------------------------------------------------------------------*
*&  Include           ZABAP_PERSONAL_FRAMEWORK_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  FORM  F_PRINT_ALV                                                  *
*&---------------------------------------------------------------------*
*&  Routine for assembling and displaying the ALV                      *
*&---------------------------------------------------------------------*
*&  -->gt_print_alv  Internal Table to print ALV Report                *
*&---------------------------------------------------------------------*
FORM f_print_alv USING p_in_gt_alv_print TYPE table.

  IF p_in_gt_alv_print[] IS NOT INITIAL.                "Condition to check if the external table is not empty

    PERFORM f_header USING p_in_gt_alv_print[].         "Assemble the ALV header by passing the table as a parameter
    PERFORM f_set_layout.
    PERFORM set_field USING p_in_gt_alv_print[].        "Assemble the ALV fields by passing the table as a parameter

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = sy-repid              "System variable (program name).
        i_callback_user_command = 'F_USER_COMMAND'      "Calls the "HOTSPOT" function
        i_callback_top_of_page  = 'F_TOP_PAGE'          "Structure to mount the header
        i_background_id         = 'ZHRST_EFD_BK_WHITE'  "Add a white background to the header
        is_layout               = ty_layout             "Structure with layout details.
        it_fieldcat             = ty_fieldcat_col       "Table with the columns to be printed.
        i_save                  = 'A'                   "Layouts can be saved (buttons for changing the layout appear).
      TABLES
        t_outtab                = p_in_gt_alv_print     "Table with the data to be printed.
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.
    IF sy-subrc IS NOT INITIAL.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE s003 DISPLAY LIKE 'E'.                "Message (Program error occurred while displaying ALV grid.)
        WHEN OTHERS.
          MESSAGE s004 DISPLAY LIKE 'E'.                "Message (An error occurred while displaying ALV grid.)
      ENDCASE.

    ENDIF.

  ELSE.

    MESSAGE s005 DISPLAY LIKE 'W'.                      "Message for when the ALV is empty (The list is empty. No data to display.).

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM  F_HEADER                                                     *
*&---------------------------------------------------------------------*
*&  Make the ALV header                                                *
*&---------------------------------------------------------------------*
*&  -->gt_print_alv  Internal Table to print ALV Report                *
*&---------------------------------------------------------------------*
FORM f_header USING p_in_gt_alv_print TYPE table.

  DATA: vl_data(10),
        vl_hora(10).

  CLEAR ty_watop.
  ty_watop-typ  = 'H'.                                "H = Big, prominent | S = Small | A = Average with italics
  ty_watop-info = sy-title.

  APPEND ty_watop TO ty_top.                          "Title shown in the ALV header

  CLEAR ty_watop.

  ty_watop-typ  = 'S'.
  CONCATENATE text-h01 sy-uname                       "Message to show user in header (User...........................:)
    INTO ty_watop-info
      SEPARATED BY space.

  APPEND ty_watop TO ty_top.                          "Shows the user who generated the report

  CLEAR ty_watop.

  ty_watop-typ  = 'S'.

  WRITE sy-datum TO vl_data USING EDIT MASK '__/__/____'.
  WRITE sy-uzeit TO vl_hora USING EDIT MASK '__:__'.

  CONCATENATE text-h02 vl_data '-'  vl_hora           "Message to show day and houra in header (Date - Time................:).
    INTO ty_watop-info
      SEPARATED BY space.

  APPEND ty_watop TO ty_top.                          "Shows the date and time that the report was generated

  CLEAR ty_watop.

  ty_watop-typ  = 'S'.

  DATA: lv_count TYPE string,
        wn_qtd   TYPE i.
  DESCRIBE TABLE p_in_gt_alv_print LINES wn_qtd.
  lv_count = wn_qtd.

  CONCATENATE text-h03 lv_count                       "Text to show the number of lines (Number of records......:).
    INTO ty_watop-info
      SEPARATED BY space.

  APPEND ty_watop TO ty_top.                          "New field that counts lines in the ALV report

  CLEAR ty_watop.

  ty_watop-typ  = 'S'.

  CONCATENATE '@' sy-datum(4) 'HRST Brasil.' text-h04 "Text for copyright (All rights reserved.)
    INTO ty_watop-info
    SEPARATED BY space.

  APPEND ty_watop TO ty_top.                          "All rights reserved message field

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM  F_TOP_PAGE                                                   *
*&---------------------------------------------------------------------*
*&  Defines and place the logo in the ALV header                       *
*&---------------------------------------------------------------------*
FORM f_top_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = ty_top
      i_logo             = 'ZHRST_EFD_LOGO'. "Place the logo in the header
ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM  F_SET_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*&  Optimization and zebra lines                                       *
*&---------------------------------------------------------------------*
FORM f_set_layout.
  ty_layout-zebra             = 'X'.  " Zebra lines
  ty_layout-colwidth_optimize = 'X'.  " Automatically optimize column widths
ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM  F_SET_FIELD                                                  *
*&---------------------------------------------------------------------*
*&  Assembles columns with column names dynamically                    *
*&---------------------------------------------------------------------*
*&  -->p_in_gt_alv_print  Internal Table to print ALV Report           *
*&---------------------------------------------------------------------*
FORM set_field USING p_in_gt_alv_print TYPE table.

  "Method of dynamically assembling ALV columns
  DATA: table_descr  TYPE REF TO cl_abap_tabledescr,
        struct_descr TYPE REF TO cl_abap_structdescr,
        columns      TYPE abap_compdescr_tab.

  FIELD-SYMBOLS <column> LIKE LINE OF columns.
  FIELD-SYMBOLS <fs_row> TYPE any.
  FIELD-SYMBOLS <fs_field> TYPE any.

  DATA lv_column TYPE abap_compname.
  DATA lv_has_data TYPE abap_bool.

  table_descr ?= cl_abap_typedescr=>describe_by_data( p_in_gt_alv_print ).
  struct_descr ?= table_descr->get_table_line_type( ).
  columns = struct_descr->components.
  LOOP AT columns ASSIGNING <column>.
    lv_column = <column>-name.

    "Initialize to 'false'
    lv_has_data = abap_false.

    "Check if there are any non-initial values in the column
    LOOP AT p_in_gt_alv_print ASSIGNING <fs_row>.
      ASSIGN COMPONENT lv_column OF STRUCTURE <fs_row> TO <fs_field>.
      IF <fs_field> IS NOT INITIAL.
        lv_has_data = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    DATA(lv_no_out) = ' '.
    IF p_no_out EQ abap_true.
      "Set no_out based on the presence of data
      IF lv_has_data = abap_false.
        lv_no_out = 'X'.
      ENDIF.
    ENDIF.


    PERFORM f_set_column USING  <column>-name 'P_IN_GT_ALV_PRINT'   lv_column    ' '  ' '  '80'  ' '  'L'  ' ' ' ' lv_no_out. "PERFORM's unique call to make the columns using the technical names of the columns.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM f_set_column                                                  *
*&---------------------------------------------------------------------*
*&  Assemble the columns with the data                                 *
*&---------------------------------------------------------------------*
*&  -->p_fieldname      Internal table field                           *
*&  -->p_tabname        Internal table                                 *
*&  -->p_texto          Column name/text                               *
*&  -->p_ref_fieldname  Reference field                                *
*&  -->p_ref_tabname    Reference table                                *
*&  -->p_outputlen      Column width                                   *
*&  -->p_emphasize      Color an entire column                         *
*&  -->p_just                                                          *
*&  -->p_do_sum                                                        *
*&  -->p_icon                                                          *
*&  -->p_no_out                                                        *
*&---------------------------------------------------------------------*
FORM f_set_column USING p_fieldname
                        p_tabname
                        p_text
                        p_ref_fieldname
                        p_ref_tabname
                        p_outputlen
                        p_emphasize
                        p_just
                        p_do_sum
                        p_icon
                        p_no_out.

  "Checks the column name with the name of the table information fields
  LOOP AT gt_table_info INTO DATA(lwa_mount_alv) WHERE fieldname = p_fieldname.
    p_text = lwa_mount_alv-fieldtext.
  ENDLOOP.

  ADD 1 TO vg_nrcol.
  ty_fieldcat-col_pos       = vg_nrcol.
  ty_fieldcat-fieldname     = p_fieldname.
  ty_fieldcat-tabname       = p_tabname.
  ty_fieldcat-seltext_l     = p_text.
  ty_fieldcat-ref_fieldname = p_ref_fieldname.
  ty_fieldcat-ref_tabname   = p_ref_tabname.
  ty_fieldcat-outputlen     = p_outputlen.
  ty_fieldcat-emphasize     = p_emphasize.
  ty_fieldcat-just          = p_just.
  ty_fieldcat-do_sum        = p_do_sum.
  ty_fieldcat-icon          = p_icon.
  ty_fieldcat-no_out        = p_no_out.

  APPEND ty_fieldcat TO ty_fieldcat_col.           " Inserts row into internal table TY_FIELDCAT_COL.

ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_USER_COMMAND                                               *
*&---------------------------------------------------------------------*
*&  Handle Double Click on ALV                                         *
*&---------------------------------------------------------------------*
*&  -->r_ucomm                                                         *
*&  -->rs_selfield                                                     *
*&---------------------------------------------------------------------*
FORM f_user_command USING
      r_ucomm LIKE sy-ucomm
     rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&IC1'.
      READ TABLE gt_alv_print INTO DATA(lwa_on_hold) INDEX rs_selfield-tabindex. "Read the table according to the double-clicked row.
      IF sy-subrc IS INITIAL.
        SELECT * FROM sflight
          WHERE carrid = @lwa_on_hold-carrid
          AND connid = @lwa_on_hold-connid INTO TABLE @DATA(lt_sflight). "Read the table according to the data from the line that was selected in SPFLI
        IF sy-subrc IS INITIAL.

          "Function to show the table in a new window when double-clicking on the ALV line.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              i_structure_name      = 'SFLIGHT'
              i_grid_title          = sy-title
              i_screen_start_column = 10
              i_screen_start_line   = 20
              i_screen_end_column   = 100
              i_screen_end_line     = 40
            TABLES
              t_outtab              = lt_sflight
            EXCEPTIONS
              program_error         = 1
              OTHERS                = 2.
          IF sy-subrc IS NOT INITIAL.

            CASE sy-subrc.
              WHEN 1.
                MESSAGE s006 DISPLAY LIKE 'E'. "Message (Program error occurred while displaying ALV grid.)
              WHEN OTHERS.
                MESSAGE s007 DISPLAY LIKE 'E'. "Message (An error occurred while displaying ALV grid.)
            ENDCASE.

          ENDIF.

        ELSE.
          MESSAGE s008 DISPLAY LIKE 'W'.       "Message if there is no data in SFLIGHT (There are no fields to show.).
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM  F_SAVE_FILE                                                  *
*&---------------------------------------------------------------------*
*&  Routine to save the file                                           *
*&---------------------------------------------------------------------*
*&  -->gt_in_table  Internal table that will save                      *
*&---------------------------------------------------------------------*
FORM f_save_file USING gt_in_table TYPE table.

  DATA: lv_full_file_name TYPE string,
        lwa_line_data     TYPE char1000_idoc,
        lt_file_data      TYPE TABLE OF char1000_idoc,
        lv_connid         TYPE string,
        lv_fltime         TYPE string,
        lv_deptime        TYPE string,
        lv_arrtime        TYPE string,
        lv_distance       TYPE string,
        lv_period         TYPE string.

  CONSTANTS:  lv_server TYPE c LENGTH 1 VALUE 'S',
              lv_local  TYPE c LENGTH 1 VALUE 'L'.

  "Check if the internal table is not empty
  IF gt_in_table IS INITIAL.
    MESSAGE s009 DISPLAY LIKE 'E'. "Message (Error: Internal table is empty. No data to save.)
    RETURN.
  ENDIF.

  IF p_header EQ abap_true.
    IF gt_dfies_tab IS NOT INITIAL.
      LOOP AT gt_dfies_tab INTO DATA(lwa_dfies_tab).
        IF lwa_line_data IS INITIAL.
          lwa_line_data = lwa_dfies_tab-fieldname.
        ELSE.
          CONCATENATE lwa_line_data lwa_dfies_tab-fieldname INTO lwa_line_data SEPARATED BY ';'.
        ENDIF.
      ENDLOOP.
      APPEND lwa_line_data TO lt_file_data.
      CLEAR lwa_line_data.
    ENDIF.
  ENDIF.

  LOOP AT gt_alv_print INTO DATA(lwa_in_table).

    CLEAR:  lwa_line_data,
            lv_connid,
            lv_fltime,
            lv_deptime,
            lv_arrtime,
            lv_distance,
            lv_period.

    lv_connid   = lwa_in_table-connid.
    lv_fltime   = lwa_in_table-fltime.
    lv_deptime  = lwa_in_table-deptime.
    lv_arrtime  = lwa_in_table-arrtime.
    lv_distance = lwa_in_table-distance.
    lv_period   = lwa_in_table-period.

    CONDENSE: lv_connid,
              lv_fltime,
              lv_deptime,
              lv_arrtime,
              lv_distance,
              lv_period.

    CONCATENATE lwa_in_table-mandt
                lwa_in_table-carrid
                lv_connid
                lwa_in_table-countryfr
                lwa_in_table-cityfrom
                lwa_in_table-airpfrom
                lwa_in_table-countryto
                lwa_in_table-cityto
                lwa_in_table-airpto
                lv_fltime
                lv_deptime
                lv_arrtime
                lv_distance
                lwa_in_table-distid
                lwa_in_table-fltype
                lv_period INTO lwa_line_data SEPARATED BY ';'.

    APPEND lwa_line_data TO lt_file_data.
  ENDLOOP.

  "Check the save destination
  IF p_server EQ abap_true.
    "Generate full file name
    PERFORM f_get_file_name USING lv_server CHANGING lv_full_file_name.
    "Save to server
    PERFORM f_save_file_to_server USING lv_full_file_name lt_file_data[].
  ELSEIF p_local EQ abap_true.
    "Save locally
    PERFORM f_save_file_to_local USING lt_file_data[].
  ELSE.
    "Invalid save location
    MESSAGE s010 DISPLAY LIKE 'E'. "Message (Error: Invalid save location indicator.)
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM  F_SAVE_FILE_TO_SERVER                                        *
*&---------------------------------------------------------------------*
*&  Routine to save on the server                                      *
*&---------------------------------------------------------------------*
*&  -->p_in_full_file_name  Full file name                             *
*&  -->gt_in_file_content   Internal table that will save              *
*&---------------------------------------------------------------------*
FORM f_save_file_to_server
  USING
    p_in_full_file_name TYPE string
    gt_in_file_content  TYPE table.

  DATA: lv_line TYPE string.

  "Open the file on the server for writing
  OPEN DATASET p_in_full_file_name FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE s011 DISPLAY LIKE 'E'. "Message (Error: Could not open file on the server. Check file path and permissions.)
    RETURN.
  ENDIF.

  "Write the content to the file
  LOOP AT gt_in_file_content INTO lv_line.
    TRANSFER lv_line TO p_in_full_file_name.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s012 DISPLAY LIKE 'E'. "Message (Error: Failed to write data to file on server.)
      EXIT.
    ENDIF.
  ENDLOOP.

  "Close the file
  CLOSE DATASET p_in_full_file_name.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE s013 DISPLAY LIKE 'E'.  "Message (Error: Could not close the file on the server.)
  ELSE.
    MESSAGE s014 DISPLAY LIKE 'S'. "Message (File saved successfully on the server.)
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM  F_SAVE_FILE_TO_LOCAL                                         *
*&---------------------------------------------------------------------*
*&  Routine to save on the server                                      *
*&---------------------------------------------------------------------*
*&  -->gt_in_file_content   Internal table that will save              *
*&---------------------------------------------------------------------*
FORM f_save_file_to_local
  USING
    gt_in_file_content  TYPE table.

  DATA: lv_standard_path TYPE string,
        lv_file_name     TYPE string,
        lv_separator     TYPE c,
        lv_dummy_path    TYPE string,
        rv_result        TYPE string.

  "Get directory separator depending on operating system
  cl_gui_frontend_services=>get_file_separator(
    CHANGING
      file_separator = lv_separator
    EXCEPTIONS
      not_supported_by_gui = 1
      error_no_gui         = 2
      cntl_error           = 3
      OTHERS               = 4 ).

  IF sy-subrc <> 0.
    lv_separator = '/'.
  ENDIF.

  "Get default download path
  cl_gui_frontend_services=>get_upload_download_path(
      CHANGING
        download_path        = lv_standard_path
        upload_path          = lv_dummy_path
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Allow user to select directory to save file
  cl_gui_frontend_services=>directory_browse(
    EXPORTING
      initial_folder       = lv_standard_path
    CHANGING
      selected_folder      = rv_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Generate full file name
  PERFORM f_get_file_name USING 'L' CHANGING lv_file_name.

  "Concatenate path and file name
  CONCATENATE rv_result lv_separator lv_file_name INTO rv_result.

  "Local download function
  cl_gui_frontend_services=>gui_download(
                    EXPORTING
                      filename = rv_result
                    CHANGING
                      data_tab = gt_in_file_content ).

  IF sy-subrc = 0.
    MESSAGE s015 DISPLAY LIKE 'S'.  "Message (File saved successfully on local machine.)
  ELSE.
    MESSAGE s016 DISPLAY LIKE 'E'.  "Message (Error saving file to local machine.)
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM  F_GET_FILE_NAME                                              *
*&---------------------------------------------------------------------*
*&  Routine to generate the file name                                  *
*&---------------------------------------------------------------------*
*&  -->p_in_file_save_type Indicates whether saving to server or local *
*&  <--p_out_result        Internal table that will save               *
*&---------------------------------------------------------------------*
FORM f_get_file_name
  USING
    p_in_file_save_type TYPE char1
  CHANGING
    p_out_result        TYPE string.

  CONSTANTS lv_separator TYPE c LENGTH 1 VALUE '_'.

  DATA: lv_date_time      TYPE string,
        lv_file_extension TYPE string,
        lv_file_name      TYPE string.

  "Getting the current date and time
  lv_date_time = sy-datum && sy-uzeit.

  IF p_csv EQ abap_true.
    lv_file_extension = '.CSV'.
  ELSEIF p_txt EQ abap_true.
    lv_file_extension = '.TXT'.
  ENDIF.

  IF p_fname IS INITIAL.
    lv_file_name = gv_file_name.
  ELSE.
    lv_file_name = p_fname.
  ENDIF.

  IF p_in_file_save_type EQ 'S'.
    "Concatenates path, filename, separator, date/time and extension
    CONCATENATE gv_server_file_path lv_file_name lv_separator lv_date_time lv_file_extension INTO p_out_result.
  ELSEIF p_in_file_save_type EQ 'L'.
    "Concatenates file name, separator, date/time and extension
    CONCATENATE lv_file_name lv_separator lv_date_time lv_file_extension INTO p_out_result.
  ENDIF.

ENDFORM.
