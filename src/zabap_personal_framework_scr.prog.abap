*&---------------------------------------------------------------------*
*&  Include           ZABAP_PERSONAL_FRAMEWORK_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_log RADIOBUTTON GROUP b1 DEFAULT 'X' USER-COMMAND go.
SELECTION-SCREEN COMMENT (18) text-002 FOR FIELD p_log.

PARAMETERS p_export RADIOBUTTON GROUP b1.
SELECTION-SCREEN COMMENT (18) text-003 FOR FIELD p_export.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.

"Additional options block, which only appears when selecting p_log
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text-005 FOR FIELD p_csv MODIF ID t1.
PARAMETERS p_no_out TYPE xfeld MODIF ID t1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

"Additional options block, which only appears when selecting p_export
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-006.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text-007 FOR FIELD p_fname MODIF ID t2.
PARAMETERS p_fname TYPE string MODIF ID t2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text-011 FOR FIELD p_csv MODIF ID t2.
PARAMETERS p_csv RADIOBUTTON GROUP g1 MODIF ID t2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT (20) text-012 FOR FIELD p_csv MODIF ID t2.
PARAMETERS p_txt RADIOBUTTON GROUP g1 MODIF ID t2.
SELECTION-SCREEN COMMENT (20) text-013 FOR FIELD p_csv MODIF ID t2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text-014 FOR FIELD p_server MODIF ID t2.
PARAMETERS p_server RADIOBUTTON GROUP g2 MODIF ID t2.
SELECTION-SCREEN COMMENT (20) text-015 FOR FIELD p_server MODIF ID t2.
PARAMETERS p_local RADIOBUTTON GROUP g2 MODIF ID t2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT (20) text-016 FOR FIELD p_local MODIF ID t2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text-008 FOR FIELD p_header MODIF ID t2.
PARAMETERS p_header TYPE xfeld MODIF ID t2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b3.
