CLASS zsdev_my_first_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zsdev_my_first_class IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    out->display(
*      EXPORTING
*        data   =
*        name   =
*      RECEIVING
*        output =
    ).
  ENDMETHOD.

ENDCLASS.
