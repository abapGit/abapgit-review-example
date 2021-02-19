CLASS zcl_abapgit_review DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS release
      IMPORTING
        !iv_trkorr TYPE trkorr
      RAISING
        cx_static_check .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_tadir,
        pgmid    TYPE tadir-pgmid,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
      END OF ty_tadir .
    TYPES:
      ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH EMPTY KEY .

    METHODS create_branch_if_missing
      IMPORTING
        !io_repo        TYPE REF TO zcl_abapgit_repo_online
        !iv_branch_name TYPE string
      RAISING
        cx_static_check .
    METHODS create_http_client
      RETURNING
        VALUE(ri_client) TYPE REF TO if_http_client
      RAISING
        zcx_abapgit_exception .
    METHODS create_pr_if_missing
      IMPORTING
        !io_repo        TYPE REF TO zcl_abapgit_repo_online
        !iv_branch_name TYPE string
      RAISING
        cx_static_check .
    METHODS create_pull_request
      IMPORTING
        !iv_url         TYPE string
        !iv_branch_name TYPE string
      RAISING
        cx_static_check .
    METHODS determine_branch_name
      IMPORTING
        !iv_trkorr            TYPE trkorr
      RETURNING
        VALUE(rv_branch_name) TYPE string .
    METHODS find_abapgit_repo
      IMPORTING
        !iv_trkorr     TYPE trkorr
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo_online .
    METHODS find_request
      IMPORTING
        !iv_trkorr        TYPE trkorr
      RETURNING
        VALUE(rv_request) TYPE trkorr .
    METHODS list_objects
      IMPORTING
        !iv_request     TYPE trkorr
      RETURNING
        VALUE(rt_tadir) TYPE ty_tadir_tt .
    METHODS release_request
      IMPORTING
        !iv_request TYPE trkorr
      RAISING
        cx_static_check .
    METHODS release_task
      IMPORTING
        !iv_task TYPE trkorr
      RAISING
        cx_static_check .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_REVIEW IMPLEMENTATION.


  METHOD create_branch_if_missing.

    DATA(lv_full_name) = |{ zif_abapgit_definitions=>c_git_branch-heads_prefix }{ iv_branch_name }|.
    DATA(lo_branches) = zcl_abapgit_git_transport=>branches( io_repo->get_url( ) ).
    TRY.
        lo_branches->find_by_name( lv_full_name ).
      CATCH zcx_abapgit_exception.
* branch not found, create it, based on HEAD
        DATA(ls_head) = lo_branches->find_by_name( lo_branches->get_head_symref( ) ).
        io_repo->create_branch(
          iv_name = lv_full_name
          iv_from = ls_head-sha1 ).
    ENDTRY.

  ENDMETHOD.


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


  METHOD create_pr_if_missing.

* todo, there must be commits on the branch for it to be possible to create the PR?

    DATA(lt_pulls) = zcl_abapgit_pr_enumerator=>new( io_repo )->get_pulls( ).

    IF NOT line_exists( lt_pulls[ head_branch = iv_branch_name ] ).
      create_pull_request(
        iv_url         = io_repo->get_url( )
        iv_branch_name = iv_branch_name ).
    ENDIF.

  ENDMETHOD.


  METHOD create_pull_request.

* todo, add more allowed characters,
    FIND REGEX 'github.com/(\d\w)+/(\d\w)+' IN iv_url SUBMATCHES DATA(lv_owner) DATA(lv_repo).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(li_github) = CAST zif_githubcom( NEW zcl_githubcom( create_http_client( ) ) ).
    DATA(ls_created) = li_github->pulls_create(
      owner = lv_owner
      repo  = lv_repo
      body  = VALUE #( ) ). " todo

  ENDMETHOD.


  METHOD determine_branch_name.

    rv_branch_name = find_request( iv_trkorr ).

  ENDMETHOD.


  METHOD find_abapgit_repo.

    DATA(lt_objects) = list_objects( find_request( iv_trkorr ) ).
    BREAK-POINT.

* todo

  ENDMETHOD.


  METHOD find_request.

    SELECT SINGLE trkorr, strkorr FROM e070 INTO @DATA(ls_e070) WHERE trkorr = @iv_trkorr.
    ASSERT sy-subrc = 0.
    IF ls_e070-strkorr IS NOT INITIAL.
      rv_request = ls_e070-strkorr.
    ELSE.
      rv_request = ls_e070-trkorr.
    ENDIF.

  ENDMETHOD.


  METHOD list_objects.

    SELECT trkorr FROM e070
      INTO TABLE @DATA(lt_e070)
      WHERE strkorr = @iv_request
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT pgmid, object, obj_name FROM e071
      INTO TABLE @DATA(lt_e071)
      FOR ALL ENTRIES IN @lt_e070
      WHERE trkorr = @lt_e070-trkorr.
    SORT lt_e071 BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_e071 COMPARING pgmid object obj_name.

    LOOP AT lt_e071 INTO DATA(ls_list).
      IF ls_list-pgmid = 'R3TR'.
        APPEND CORRESPONDING #( ls_list ) TO rt_tadir.
      ELSE.
        DATA(ls_tadir) = cl_wb_object_type=>get_tadir_from_limu(
          p_object   = ls_list-object
          p_obj_name = ls_list-obj_name ).

        APPEND CORRESPONDING #( ls_tadir ) TO rt_tadir.
      ENDIF.
    ENDLOOP.

    SORT rt_tadir BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM rt_tadir COMPARING pgmid object obj_name.

  ENDMETHOD.


  METHOD release.

    CONSTANTS lc_workbench TYPE e070-trfunction VALUE 'K'.
    CONSTANTS lc_development TYPE e070-trfunction VALUE 'S'.

    SELECT SINGLE strkorr, trfunction
      FROM e070
      INTO @DATA(ls_e070)
      WHERE trkorr = @iv_trkorr.
    IF sy-subrc <> 0
        OR ( ls_e070-trfunction <> lc_workbench
        AND ls_e070-trfunction <> lc_development ).
      RETURN.
    ENDIF.

    IF ls_e070-strkorr IS NOT INITIAL.
      release_task( iv_trkorr ).
    ELSE.
      release_request( iv_trkorr ).
    ENDIF.

  ENDMETHOD.


  METHOD release_request.

* todo, call github api to check if PR is merged

  ENDMETHOD.


  METHOD release_task.

    DATA(lo_repo) = find_abapgit_repo( iv_task ).
    IF lo_repo IS INITIAL OR lo_repo->get_url( ) NP '*github.com*'.
      RETURN.
    ENDIF.

    DATA(lv_branch_name) = determine_branch_name( iv_task ).

    create_branch_if_missing(
      io_repo        = lo_repo
      iv_branch_name = lv_branch_name ).

* push changes to branch
* todo

    create_pr_if_missing(
      io_repo        = lo_repo
      iv_branch_name = lv_branch_name ).

  ENDMETHOD.
ENDCLASS.
