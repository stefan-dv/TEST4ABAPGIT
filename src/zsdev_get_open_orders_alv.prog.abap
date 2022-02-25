*&---------------------------------------------------------------------*
*& Report zsdev_get_alv
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdev_get_open_orders_alv.
DATA: lt_outtab TYPE STANDARD TABLE OF alv_t_t2.

DATA: BEGIN OF object_tab OCCURS 1.
        INCLUDE STRUCTURE rihaufk_list AS rihaufk_list.

* Start of Note 2721584
* this include has been moved to the top
* manually added fields has to be at the end of the structure
* because it can cause inconsistencies during excel export
* SAP Note 2417613
 "  INCLUDE STRUCTURE eams_s_order_mcall.
* SAP Note 2417613
* End of Note 2721584
DATA:  ppsid LIKE viaufkst-ppsid.
DATA:  igewrk LIKE viaufkst-gewrk.          "Int Verant.Arbeitsplatz
DATA:  aufpt  LIKE viaufkst-aufpt.          "Int Netzwerknummer
DATA:  aplzt  LIKE viaufkst-aplzt.          "Int Netzwerkvorgang
DATA:  adrnr_iloa  LIKE viaufkst-adrnr_iloa."Adresse Bezugsobjekt
DATA:  tplnr_int   LIKE viaufkst-tplnr.     "T.Platz int. Format
DATA:  no_disp    LIKE viaufkst-no_disp.    "Dispokennzeichen Datenb.
DATA:  selected,
       lights,
       pm_selected TYPE pm_selected,
***********************************************
***  Customer Connect EAM 2014, IR 7135, note 2079318
***  count lines in ALV list reports
       row_count TYPE sy-tabix.
***********************************************
DATA END OF object_tab.


*FIELD-SYMBOLS: <lt_outtab> LIKE lt_outtab.
*FIELD-SYMBOLS: <lt_outtab> LIKE object_tab.
FIELD-SYMBOLS: <lt_outtab> TYPE STANDARD TABLE.
DATA lo_data TYPE REF TO DATA.

" Let know the model
 cl_salv_bs_runtime_info=>set(
  EXPORTING
    display  = abap_false
    metadata = abap_false
    DATA     = abap_true
).


SUBMIT RIAUFK20 USING SELECTION-SET 'SDEV SHORT'
  AND RETURN.

TRY.
    " get data from SALV model
    cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING
            r_data = lo_data
    ).
    BREAK-POINT.
    ASSIGN lo_data->* to <lt_outtab>.
    BREAK-POINT.
    data: csv_converted_table type TRUXS_T_TEXT_DATA.
    call function 'SAP_CONVERT_TO_CSV_FORMAT'
*      EXPORTING

*        i_line_header        =
*        i_filename           =
*        i_appl_keep          = space
      TABLES
        i_tab_sap_data       = <lt_outtab>
      CHANGING
        i_tab_converted_data = csv_converted_table
      EXCEPTIONS
        conversion_failed    = 1
        others               = 2
      .
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  CATCH cx_salv_bs_sc_runtime_info.
ENDTRY.
