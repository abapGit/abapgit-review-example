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

    METHODS push_changes
      IMPORTING
        !io_repo        TYPE REF TO zcl_abapgit_repo_online
        !it_objects     TYPE ty_tadir_tt
        !iv_branch_name TYPE string
      RAISING
        zcx_abapgit_exception .
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
        !it_tadir      TYPE ty_tadir_tt
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo_online
      RAISING
        cx_static_check .
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

* todo, there must be commits on the branch for it to be possible to create the PR

    DATA(lt_pulls) = zcl_abapgit_pr_enumerator=>new( io_repo )->get_pulls( ).

    IF NOT line_exists( lt_pulls[ head_branch = iv_branch_name ] ).
      create_pull_request(
        iv_url         = io_repo->get_url( )
        iv_branch_name = iv_branch_name ).
    ENDIF.

  ENDMETHOD.


  METHOD create_pull_request.

* todo, add more allowed characters,
    FIND REGEX 'https:\/\/github\.com\/([\d\w]+)\/([-\d\w]+)(\.git)?' IN iv_url SUBMATCHES DATA(lv_owner) DATA(lv_repo).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(li_github) = CAST zif_githubcom( NEW zcl_githubcom( create_http_client( ) ) ).
    DATA(ls_created) = li_github->pulls_create(
      owner = lv_owner
      repo  = lv_repo
      body  = VALUE #(
        title                 = 'title'
        head                  = iv_branch_name
        base                  = 'master' " todo
        maintainer_can_modify = abap_true
        draft                 = abap_false
        issue                 = cl_abap_math=>max_int4
        body                  = 'body text' ) ).

  ENDMETHOD.


  METHOD determine_branch_name.

    rv_branch_name = find_request( iv_trkorr ).

  ENDMETHOD.


  METHOD find_abapgit_repo.

    IF lines( it_tadir ) = 0.
      RETURN.
    ENDIF.

    SELECT DISTINCT devclass FROM tadir INTO TABLE @DATA(lt_packages)
      FOR ALL ENTRIES IN @it_tadir
      WHERE pgmid = @it_tadir-pgmid
      AND object = @it_tadir-object
      AND obj_name = @it_tadir-obj_name.

    DATA(lt_repos) = zcl_abapgit_repo_srv=>get_instance( )->list( ).
    LOOP AT lt_packages INTO DATA(lv_package).
      DATA(lt_supers) = zcl_abapgit_factory=>get_sap_package( lv_package-devclass )->list_superpackages( ).
      LOOP AT lt_supers INTO DATA(lv_super).
        LOOP AT lt_repos INTO DATA(lo_repo).
          IF lo_repo->is_offline( ) = abap_true.
            CONTINUE.
          ELSEIF lo_repo->get_package( ) = lv_super.
            IF ro_repo IS INITIAL.
              ro_repo ?= lo_repo.
            ELSEIF ro_repo->get_package( ) = lo_repo->get_package( ).
              CONTINUE.
            ELSE.
              ASSERT 0 = 'the transport spans multiple repos'.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

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


  METHOD push_changes.

    DATA(ls_files) = zcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).
    DATA(lt_file_status) = zcl_abapgit_file_status=>status( io_repo ).

    DATA(lo_stage) = NEW zcl_abapgit_stage( ).

    LOOP AT lt_file_status ASSIGNING FIELD-SYMBOL(<ls_status>)
        WHERE lstate <> zif_abapgit_definitions=>c_state-unchanged.

* the object must be part of the task's request
      READ TABLE it_objects WITH KEY object = <ls_status>-obj_type obj_name = <ls_status>-obj_name TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE <ls_status>-lstate.
        WHEN zif_abapgit_definitions=>c_state-modified OR zif_abapgit_definitions=>c_state-added.
          DATA(lv_data) = ls_files-local[ file-filename = <ls_status>-filename
                                          file-path     = <ls_status>-path ]-file-data.
          lo_stage->add( iv_path     = <ls_status>-path
                         iv_filename = <ls_status>-filename
                         iv_data     = lv_data ).
        WHEN zif_abapgit_definitions=>c_state-deleted.
          lo_stage->rm( iv_path     = <ls_status>-path
                        iv_filename = <ls_status>-filename ).
      ENDCASE.
    ENDLOOP.

    IF lo_stage->count( ) > 0.
      DATA(lv_current) = io_repo->get_selected_branch( ).
      ASSERT lv_current IS NOT INITIAL.
      io_repo->select_branch( |refs/heads/{ iv_branch_name }| ).
      io_repo->push(
        is_comment = VALUE #(
          committer = VALUE #( name = 'name' email = 'name@localhost' )
          author    = VALUE #( name = 'name' email = 'name@localhost' )
          comment   = 'automatic push')
        io_stage   = lo_stage ).
      io_repo->select_branch( lv_current ).
    ENDIF.

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

    DATA(lt_objects) = list_objects( find_request( iv_task ) ).
    IF lines( lt_objects ) = 0.
      RETURN.
    ENDIF.

    DATA(lo_repo) = find_abapgit_repo( lt_objects ).
    IF lo_repo IS INITIAL OR lo_repo->get_url( ) NP '*github.com*'.
      RETURN.
    ENDIF.

    DATA(lv_branch_name) = determine_branch_name( iv_task ).

    create_branch_if_missing(
      io_repo        = lo_repo
      iv_branch_name = lv_branch_name ).

    push_changes(
      io_repo        = lo_repo
      it_objects     = lt_objects
      iv_branch_name = lv_branch_name ).

    create_pr_if_missing(
      io_repo        = lo_repo
      iv_branch_name = lv_branch_name ).

  ENDMETHOD.
ENDCLASS.
