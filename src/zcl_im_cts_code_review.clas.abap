CLASS zcl_im_cts_code_review DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ex_cts_request_check .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_IM_CTS_CODE_REVIEW IMPLEMENTATION.


  METHOD if_ex_cts_request_check~check_before_add_objects.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_changing_owner.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_creation.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_release.
    NEW zcl_review( )->release( request ).
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_release_slin.
    RETURN.
  ENDMETHOD.
ENDCLASS.
