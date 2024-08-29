*&---------------------------------------------------------------------*
*&  Include           ZABAP_PERSONAL_FRAMEWORK_TOP
*&---------------------------------------------------------------------*
DATA: gt_alv_print TYPE TABLE OF spfli.

*&---------------------------------------------------------------------*
*** Save File Variables                                                *
*&---------------------------------------------------------------------*
CONSTANTS: gv_file_name        TYPE string VALUE 'DEFAULT_FILE',        "File name
           gv_server_file_path TYPE string VALUE '/usr/sap/trans/tmp/'. "File path.

*&---------------------------------------------------------------------*
*** Alv Structures                                                     *
*&---------------------------------------------------------------------*

"Structure to store the table name (Table name is in case it is necessary to get more than one table), field name and field description.
TYPES:  BEGIN OF ty_table_info,
          tabname   TYPE dfies-tabname,
          fieldname TYPE dfies-fieldname,
          fieldtext TYPE dfies-fieldtext,
        END OF ty_table_info.

"ALV Global Tables
DATA: gt_table_info TYPE TABLE OF ty_table_info,
      gt_dfies_tab  LIKE TABLE OF dfies.

"ALV Global WorkAreas
DATA: gwa_table_info  TYPE ty_table_info,
      ty_layout       TYPE slis_layout_alv,
      ty_top          TYPE slis_t_listheader,
      ty_watop        TYPE slis_listheader,
      ty_fieldcat_col TYPE slis_t_fieldcat_alv,
      ty_fieldcat     TYPE slis_fieldcat_alv.

"ALV Global Variables
DATA vg_nrcol(4) TYPE c.

"ALV Global Constants
CONSTANTS gc_tabname TYPE tabname VALUE 'SPFLI'. "Change the table name(VALUE) to get the desired descriptions.

"Function to search for descriptions of table fields according to the user's language.
CALL FUNCTION 'DDIF_FIELDINFO_GET'
  EXPORTING
    tabname        = gc_tabname
    langu          = sy-langu
  TABLES
    dfies_tab      = gt_dfies_tab
  EXCEPTIONS
    not_found      = 1
    internal_error = 2
    OTHERS         = 3.

IF sy-subrc IS INITIAL.

  "Loop to fetch table name, field name and field description.
  LOOP AT gt_dfies_tab INTO DATA(lwa_dfies_tab).
    gwa_table_info-tabname = lwa_dfies_tab-tabname.
    gwa_table_info-fieldname = lwa_dfies_tab-fieldname.
    gwa_table_info-fieldtext = lwa_dfies_tab-fieldtext.
    APPEND gwa_table_info TO gt_table_info.
  ENDLOOP.

ELSE.

  CASE sy-subrc.
    WHEN 1.
      MESSAGE s000 DISPLAY LIKE 'E'. " Message (Table field descriptions not found.)
    WHEN 2.
      MESSAGE s001 DISPLAY LIKE 'E'. " Message (Internal error occurred.)
    WHEN OTHERS.
      MESSAGE s002 DISPLAY LIKE 'E'. " Message (An error occurred while retrieving field descriptions.)
  ENDCASE.

ENDIF.
