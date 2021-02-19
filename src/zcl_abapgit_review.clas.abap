CLASS zcl_abapgit_review DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS release
      IMPORTING
        !request TYPE trkorr
      RAISING
        cx_static_check .
  PROTECTED SECTION.

    METHODS determine_branch_name
      IMPORTING
        !iv_request           TYPE trkorr
      RETURNING
        VALUE(rv_branch_name) TYPE string .
    METHODS create_http_client
      RETURNING
        VALUE(ri_client) TYPE REF TO if_http_client
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_REVIEW IMPLEMENTATION.


  METHOD create_http_client.

    DATA lv_url TYPE string VALUE 'https://api.github.com'.
    DATA lo_proxy_configuration TYPE REF TO zcl_abapgit_proxy_config.

    ri_client = zcl_abapgit_exit=>get_instance( )->create_http_client( lv_url ).
    IF ri_client IS INITIAL.
      lo_proxy_configuration = NEW #( ).
      cl_http_client=>create_by_url(
        EXPORTING
          url           = lv_url
          ssl_id        = zcl_abapgit_exit=>get_instance( )->get_ssl_id( )
          proxy_host    = lo_proxy_configuration->get_proxy_url( lv_url )
          proxy_service = lo_proxy_configuration->get_proxy_port( lv_url )
        IMPORTING
          client        = ri_client ).
    ENDIF.

  ENDMETHOD.


  METHOD determine_branch_name.

    SELECT SINGLE trkorr, strkorr FROM e070 INTO @DATA(ls_e070) WHERE trkorr = @iv_request.
    ASSERT sy-subrc = 0.
    IF ls_e070-strkorr IS NOT INITIAL.
      rv_branch_name = ls_e070-strkorr.
    ELSE.
      rv_branch_name = ls_e070-trkorr.
    ENDIF.

  ENDMETHOD.


  METHOD release.

    DATA(li_github) = CAST zif_githubcom( NEW zcl_githubcom( create_http_client( ) ) ).

    DATA(lt_pulls) = li_github->pulls_list(
      owner = 'abapGit'
      repo  = 'abapGit' ).
    BREAK-POINT.

    LOOP AT lt_pulls INTO DATA(ls_pull).
      WRITE / ls_pull-head-ref.
    ENDLOOP.

* todo

  ENDMETHOD.
ENDCLASS.
