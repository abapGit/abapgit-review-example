CLASS zcl_abapgit_review DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_pull_url,
             package TYPE devclass,
             url     TYPE string,
           END OF ty_pull_url.
    TYPES: tt_pull_urls TYPE STANDARD TABLE OF ty_pull_url WITH DEFAULT KEY.
    TYPES: tt_repos TYPE STANDARD TABLE OF REF TO zcl_abapgit_repo.
    TYPES: tt_packages TYPE STANDARD TABLE OF devclass.

    METHODS release
      IMPORTING
        !iv_trkorr TYPE trkorr
      RAISING
        cx_static_check .
    METHODS find_pull_request_urls
      IMPORTING
        !iv_trkorr       TYPE trkorr
      EXPORTING
        et_pull_requests TYPE tt_pull_urls
        et_packages      TYPE tt_packages
      RAISING
        cx_static_check.
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_tadir,
        pgmid    TYPE tadir-pgmid,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
      END OF ty_tadir .
    TYPES:
      ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_repo,
        package TYPE devclass,
        repo    TYPE REF TO zcl_abapgit_repo,
      END OF ty_repo.

    CONSTANTS gc_workbench TYPE e070-trfunction VALUE 'K' ##NO_TEXT.
    CONSTANTS gc_development TYPE e070-trfunction VALUE 'S' ##NO_TEXT.

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
        !iv_request     TYPE trkorr
      RAISING
        cx_static_check .
    METHODS create_pull_request
      IMPORTING
        !iv_url         TYPE string
        !iv_branch_name TYPE string
        !iv_request     TYPE trkorr
        !iv_base        TYPE string
      RAISING
        cx_static_check .
    METHODS find_abapgit_repo
      IMPORTING
        !it_tadir      TYPE ty_tadir_tt
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo_online
      RAISING
        cx_static_check .
    METHODS find_abapgit_repos
      IMPORTING
        !iv_trkorr TYPE trkorr
      EXPORTING
        et_repos   TYPE tt_repos
      RAISING
        cx_static_check.
    METHODS find_pull_requests
      IMPORTING
                it_repos         TYPE tt_repos
                iv_branch_name   TYPE string
      RETURNING VALUE(rt_result) TYPE tt_pull_urls
      RAISING   cx_static_check.
    METHODS find_ags_merge_requests
      IMPORTING
        io_repo        TYPE REF TO zcl_abapgit_repo_online
        iv_branch_name TYPE string
      CHANGING
        ct_result      TYPE tt_pull_urls
      RAISING
        zcx_abapgit_review.
    METHODS find_github_pull_requests
      IMPORTING
        io_repo        TYPE REF TO zcl_abapgit_repo_online
        iv_branch_name TYPE string
      CHANGING
        ct_result      TYPE tt_pull_urls
      RAISING
        cx_static_check.
    METHODS list_objects
      IMPORTING
        !iv_request     TYPE trkorr
      RETURNING
        VALUE(rt_tadir) TYPE ty_tadir_tt .
    METHODS push_changes
      IMPORTING
        !io_repo        TYPE REF TO zcl_abapgit_repo_online
        !iv_task        TYPE trkorr
        !it_objects     TYPE ty_tadir_tt
        !iv_branch_name TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS release_request
      IMPORTING
        !iv_request TYPE trkorr
        !io_repo    TYPE REF TO zcl_abapgit_repo_online
      RAISING
        cx_static_check .
    METHODS release_task
      IMPORTING
        !iv_task    TYPE trkorr
        !io_repo    TYPE REF TO zcl_abapgit_repo_online
        !it_objects TYPE ty_tadir_tt
        !iv_request TYPE trkorr
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

    DATA(lo_branches) = zcl_abapgit_git_transport=>branches( io_repo->get_url( ) ).
    DATA(ls_head) = lo_branches->find_by_name( lo_branches->get_head_symref( ) ).

    SELECT SINGLE @abap_true FROM zagr_created_prs
      INTO @DATA(lv_created)
      WHERE trkorr = @iv_request.                         "#EC CI_SUBRC
    IF lv_created = abap_false.
      create_pull_request(
        iv_url         = io_repo->get_url( )
        iv_request     = iv_request
        iv_base        = ls_head-display_name
        iv_branch_name = iv_branch_name ).
    ENDIF.

  ENDMETHOD.


  METHOD create_pull_request.

    FIND REGEX 'https:\/\/github\.com\/([-\d\w]+)\/([-\d\w]+)(\.git)?'
      IN iv_url SUBMATCHES DATA(lv_owner) DATA(lv_repo).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(li_github) = CAST zif_githubcom( NEW zcl_githubcom( create_http_client( ) ) ).
    DATA(ls_created) = li_github->pulls_create(
      owner = lv_owner
      repo  = lv_repo
      body  = VALUE #(
        title                 = |{ sy-sysid } - { iv_request }|
        head                  = iv_branch_name
        base                  = iv_base
        maintainer_can_modify = abap_true
        draft                 = abap_false
        issue                 = cl_abap_math=>max_int4
        body                  = |{ sy-sysid } - { iv_request }| ) ).

    IF ls_created-number IS NOT INITIAL.
      DATA(ls_pr) = VALUE zagr_created_prs(
        trkorr = iv_request
        pr     = ls_created-number
        url    = ls_created-html_url ).
      INSERT zagr_created_prs FROM @ls_pr.
      ASSERT sy-subrc = 0.
    ENDIF.

  ENDMETHOD.


  METHOD find_abapgit_repo.

    IF lines( it_tadir ) = 0.
      RETURN.
    ENDIF.

    SELECT DISTINCT devclass FROM tadir INTO TABLE @DATA(lt_packages)
      FOR ALL ENTRIES IN @it_tadir
      WHERE pgmid = @it_tadir-pgmid
      AND object = @it_tadir-object
      AND obj_name = @it_tadir-obj_name.                  "#EC CI_SUBRC

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


  METHOD find_abapgit_repos.
    DATA: lt_repos TYPE HASHED TABLE OF ty_repo WITH UNIQUE KEY package.

    DATA(lt_objects) = list_objects( iv_trkorr ).
    IF lt_objects IS INITIAL.
      RETURN.
    ENDIF.

    SELECT DISTINCT devclass FROM tadir INTO TABLE @DATA(lt_packages)
      FOR ALL ENTRIES IN @lt_objects
      WHERE pgmid = @lt_objects-pgmid
      AND object = @lt_objects-object
      AND obj_name = @lt_objects-obj_name.                "#EC CI_SUBRC

    DATA(lt_abapgit_repos) = zcl_abapgit_repo_srv=>get_instance( )->list( ).
    LOOP AT lt_packages INTO DATA(lv_package).
      DATA(lt_supers) = zcl_abapgit_factory=>get_sap_package( lv_package-devclass )->list_superpackages( ).
      LOOP AT lt_supers INTO DATA(lv_super).
        LOOP AT lt_abapgit_repos INTO DATA(lo_abapgit_repo).
          IF lo_abapgit_repo->is_offline( ) = abap_true.
            CONTINUE.
          ELSEIF lo_abapgit_repo->get_package( ) = lv_super.
            INSERT VALUE #(
              package = lo_abapgit_repo->get_package( )
              repo = lo_abapgit_repo ) INTO TABLE lt_repos.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    et_repos = VALUE #( FOR ls_repo IN lt_repos ( ls_repo-repo ) ).

  ENDMETHOD.


  METHOD find_ags_merge_requests.
    DATA: lv_query_url   TYPE string,
          lt_merge_requests TYPE zagr_ags_merge_req_tt.

    FIND REGEX '(http|https):\/\/([\.\d\w]+)\/sap\/zabapgitserver\/git\/([-\d\w]+)(\.git)?'
      IN io_repo->get_url( ) SUBMATCHES DATA(lv_protocol) DATA(lv_host) DATA(lv_repo_name).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_query_url = |{ lv_protocol }://{ lv_host }/sap/zabapgitserver/rest/| &&
      |find_merge_requests/{ lv_repo_name }/{ iv_branch_name }|.
    TRY.
        cl_http_client=>create_by_url( EXPORTING url = lv_query_url
          IMPORTING client = DATA(lo_http_client) ).
        DATA(lo_http_rest_client) = NEW cl_rest_http_client( lo_http_client ).
        lo_http_rest_client->if_rest_resource~get( ).
        DATA(lv_response) = lo_http_rest_client->if_rest_client~get_response_entity( )->get_binary_data( ).
        CALL TRANSFORMATION id
          SOURCE XML lv_response
          RESULT data = lt_merge_requests.

        LOOP AT lt_merge_requests REFERENCE INTO DATA(lr_merge_req).
          DATA(lv_merge_request_url) = |{ lv_protocol }://{ lv_host }/sap/zabapgitserver/| &&
            |{ lv_repo_name }/merge_request/{ lr_merge_req->*-id }|.
          INSERT VALUE #( package = io_repo->get_package( ) url = lv_merge_request_url )
            INTO TABLE ct_result.
        ENDLOOP.
      CATCH cx_transformation_error INTO DATA(lo_transformation_error).
        RAISE EXCEPTION TYPE zcx_abapgit_review
          EXPORTING
            textid   = zcx_abapgit_review=>read_ags_merge_request
            previous = lo_transformation_error.
      CATCH cx_rest_client_exception INTO DATA(lo_http_exception).
        RAISE EXCEPTION TYPE zcx_abapgit_review
          EXPORTING
            textid   = zcx_abapgit_review=>read_ags_merge_request
            previous = lo_http_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD find_github_pull_requests.

    FIND REGEX 'https:\/\/github\.com\/([-\d\w]+)\/([-\d\w]+)(\.git)?'
      IN io_repo->get_url( ) SUBMATCHES DATA(lv_owner) DATA(lv_repo).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(li_github) = CAST zif_githubcom( NEW zcl_githubcom( create_http_client( ) ) ).
    DATA(lt_pr) = li_github->pulls_list(
      owner       = lv_owner
      repo        = lv_repo
      head        = |{ lv_owner }:{ iv_branch_name }| ).
    LOOP AT lt_pr REFERENCE INTO DATA(lr_pr).
      INSERT VALUE #( package = io_repo->get_package( ) url = lr_pr->*-html_url )
        INTO TABLE ct_result.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_pull_requests.

    LOOP AT it_repos INTO DATA(lo_repo).
      find_github_pull_requests( EXPORTING
        io_repo = CAST zcl_abapgit_repo_online( lo_repo )
        iv_branch_name = iv_branch_name
        CHANGING ct_result = rt_result ).
      find_ags_merge_requests( EXPORTING
        io_repo = CAST zcl_abapgit_repo_online( lo_repo )
        iv_branch_name = iv_branch_name
        CHANGING ct_result = rt_result ).
    ENDLOOP.

  ENDMETHOD.


  METHOD find_pull_request_urls.

    find_abapgit_repos(
      EXPORTING
        iv_trkorr = iv_trkorr
      IMPORTING
        et_repos = DATA(lt_repos) ).
    et_pull_requests = find_pull_requests(
      it_repos = lt_repos
      iv_branch_name = CONV string( iv_trkorr ) ).
    et_packages = VALUE #( FOR lo_repo IN lt_repos ( lo_repo->get_package( ) ) ).

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
      WHERE trkorr = @lt_e070-trkorr.                     "#EC CI_SUBRC
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

    io_repo->select_branch( |refs/heads/{ iv_branch_name }| ).

    DATA(ls_files) = zcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).
    DATA(lt_file_status) = zcl_abapgit_file_status=>status( io_repo ).

    DATA(lo_stage) = NEW zcl_abapgit_stage( ).

    LOOP AT lt_file_status ASSIGNING FIELD-SYMBOL(<ls_status>)
        WHERE match <> abap_true AND obj_type IS NOT INITIAL.
* the object must be part of the task's request
      IF NOT line_exists( it_objects[ object = <ls_status>-obj_type obj_name = <ls_status>-obj_name ] ).
        CONTINUE.
      ENDIF.

      CASE <ls_status>-lstate.
        WHEN zif_abapgit_definitions=>c_state-deleted.
          lo_stage->rm( iv_path     = <ls_status>-path
                        iv_filename = <ls_status>-filename ).
        WHEN OTHERS.
          DATA(lv_data) = ls_files-local[ file-filename = <ls_status>-filename
                                          file-path     = <ls_status>-path ]-file-data.
          lo_stage->add( iv_path     = <ls_status>-path
                         iv_filename = <ls_status>-filename
                         iv_data     = lv_data ).
      ENDCASE.
    ENDLOOP.

    IF lo_stage->count( ) > 0.
      io_repo->push(
        is_comment = VALUE #(
          committer = VALUE #( name = 'name' email = 'name@localhost' )
          author    = VALUE #( name = 'name' email = 'name@localhost' )
          comment   = iv_task )
        io_stage   = lo_stage ).
    ENDIF.

  ENDMETHOD.


  METHOD release.

    SELECT SINGLE strkorr, trfunction FROM e070 INTO @DATA(ls_e070) WHERE trkorr = @iv_trkorr.
    IF sy-subrc <> 0 OR ( ls_e070-trfunction <> gc_workbench AND ls_e070-trfunction <> gc_development ).
      RETURN.
    ENDIF.

    IF ls_e070-strkorr IS NOT INITIAL.
      DATA(lv_request) = ls_e070-strkorr.
    ELSE.
      lv_request = iv_trkorr.
    ENDIF.

    DATA(lt_objects) = list_objects( lv_request ).
    IF lines( lt_objects ) = 0.
      RETURN.
    ENDIF.

    DATA(lo_repo) = find_abapgit_repo( lt_objects ).
    IF lo_repo IS INITIAL OR lo_repo->get_url( ) NP '*github.com*'.
      RETURN.
    ENDIF.

    IF ls_e070-strkorr IS NOT INITIAL.
      release_task(
        iv_task    = iv_trkorr
        it_objects = lt_objects
        iv_request = lv_request
        io_repo    = lo_repo ).
    ELSE.
      release_request(
        iv_request = iv_trkorr
        io_repo    = lo_repo ).
    ENDIF.

  ENDMETHOD.


  METHOD release_request.

* call github api to check if PR is merged

    SELECT SINGLE pr FROM zagr_created_prs INTO @DATA(lv_pr)
      WHERE trkorr = @iv_request.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    FIND REGEX 'https:\/\/github\.com\/([-\d\w]+)\/([-\d\w]+)(\.git)?'
      IN io_repo->get_url( ) SUBMATCHES DATA(lv_owner) DATA(lv_repo).
    ASSERT sy-subrc = 0.

    DATA(li_github) = CAST zif_githubcom( NEW zcl_githubcom( create_http_client( ) ) ).
    DATA(ls_pr) = li_github->pulls_get(
      owner       = lv_owner
      repo        = lv_repo
      pull_number = lv_pr ).

    IF ls_pr-state = 'open'.
      zcx_abapgit_exception=>raise( |PR #{ lv_pr } must be merged before the request can be released| ).
    ENDIF.

  ENDMETHOD.


  METHOD release_task.

    DATA(lv_branch_name) = |{ iv_request }|.

    create_branch_if_missing(
      io_repo        = io_repo
      iv_branch_name = lv_branch_name ).

    push_changes(
      io_repo        = io_repo
      it_objects     = it_objects
      iv_task        = iv_task
      iv_branch_name = lv_branch_name ).

    create_pr_if_missing(
      io_repo        = io_repo
      iv_request     = iv_request
      iv_branch_name = lv_branch_name ).

  ENDMETHOD.
ENDCLASS.
