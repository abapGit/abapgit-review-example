CLASS zcx_abapgit_review DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    CONSTANTS: BEGIN OF read_ags_merge_request,
      msgid TYPE symsgid VALUE 'ZABAPGIT_REVIEW',
      msgno TYPE symsgno VALUE '000',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF read_ags_merge_request.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ABAPGIT_REVIEW IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
