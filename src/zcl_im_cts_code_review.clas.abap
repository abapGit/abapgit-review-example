CLASS zcl_im_cts_code_review DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ex_cts_request_check .
  PROTECTED SECTION.
    METHODS release_workbench.
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
    CONSTANTS lc_workbench TYPE trfunction VALUE 'K'.
    IF type = lc_workbench.
      release_workbench( ).
    ENDIF.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_release_slin.
    RETURN.
  ENDMETHOD.


  METHOD release_workbench.
* todo
  ENDMETHOD.
ENDCLASS.
