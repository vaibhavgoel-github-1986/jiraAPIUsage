CLASS zcl_utm_create_jira_issues DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_utm_create_jira_issues.

    DATA:
      gt_config       TYPE STANDARD TABLE OF tvarvc,
      gt_dedup_status TYPE RANGE OF string.

    DATA:
      go_issues_api      TYPE REF TO zif_jira_issues,
      go_user_search_api TYPE REF TO zif_jira_user_search,
      go_utility         TYPE REF TO zif_jira_utils.

    CONSTANTS:
      gc_failure TYPE string VALUE 'Assertion Failure'.

    METHODS constructor
      IMPORTING
        VALUE(io_issues_api)      TYPE REF TO zif_jira_issues OPTIONAL
        VALUE(io_user_search_api) TYPE REF TO zif_jira_user_search OPTIONAL
        VALUE(io_utility)         TYPE REF TO zif_jira_utils OPTIONAL
        VALUE(io_adf_builder)     TYPE REF TO zcl_jira_adf_builder OPTIONAL
      RAISING
        zcx_jira_exceptions.

  PROTECTED SECTION.
    METHODS:
      create_payload
        IMPORTING
          VALUE(is_object)  TYPE zif_utm_create_jira_issues=>ts_object
        EXPORTING
          es_create_request TYPE zif_jira_issues=>ts_create_request
        RAISING
          zcx_jira_exceptions,

      get_last_change_by
        IMPORTING
          VALUE(iv_obj_name)       TYPE sobj_name
        RETURNING
          VALUE(rv_last_change_by) TYPE syuname
        RAISING
          zcx_jira_exceptions,

      create_description_adf
        IMPORTING
          VALUE(is_object) TYPE zif_utm_create_jira_issues=>ts_object
        EXPORTING
          es_doc           TYPE zif_jira_adf_types=>ts_document,

      create_notes_adf
        IMPORTING
          VALUE(is_object) TYPE zif_utm_create_jira_issues=>ts_object
        EXPORTING
          es_doc           TYPE zif_jira_adf_types=>ts_document,

      create_acceptance_adf
        IMPORTING
          VALUE(is_object) TYPE zif_utm_create_jira_issues=>ts_object
        EXPORTING
          es_doc           TYPE zif_jira_adf_types=>ts_document,

      map_obj_type
        IMPORTING
          VALUE(iv_obj_type) TYPE trobjtype
        RETURNING
          VALUE(rv_text)     TYPE string,

      create_text_node
        IMPORTING
          VALUE(iv_text)   TYPE string
          VALUE(iv_strong) TYPE abap_bool DEFAULT abap_false
          VALUE(iv_color)  TYPE string OPTIONAL
        RETURNING
          VALUE(ro_node)   TYPE REF TO zcl_jira_adf_builder,

      create_list_item
        IMPORTING
          VALUE(iv_text1) TYPE string OPTIONAL
          VALUE(iv_text2) TYPE string OPTIONAL
        RETURNING
          VALUE(ro_node)  TYPE REF TO zcl_jira_adf_builder,

      create_panel_node
        IMPORTING
          VALUE(is_object) TYPE zif_utm_create_jira_issues=>ts_object
        RETURNING
          VALUE(ro_node)   TYPE REF TO zcl_jira_adf_builder,

      create_table_node
        IMPORTING
          VALUE(is_object) TYPE zif_utm_create_jira_issues=>ts_object
        RETURNING
          VALUE(ro_node)   TYPE REF TO zcl_jira_adf_builder,

      create_table_header_row
        IMPORTING
          VALUE(iv_heading)   TYPE string
          VALUE(iv_col_width) TYPE i DEFAULT 250
        RETURNING
          VALUE(ro_node)      TYPE REF TO zcl_jira_adf_builder,

      create_table_cell_row
        IMPORTING
          VALUE(iv_cell_value) TYPE string
          VALUE(iv_col_width)  TYPE i DEFAULT 250
        RETURNING
          VALUE(ro_node)       TYPE REF TO zcl_jira_adf_builder,

      compress_msg
        IMPORTING
          VALUE(iv_msg) TYPE string
        RETURNING
          VALUE(rv_msg) TYPE string,

      modify_utm_logs
        IMPORTING
          VALUE(it_logs)  TYPE zif_utm_create_jira_issues=>tt_logs
        RETURNING
          VALUE(rv_error) TYPE abap_bool.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_UTM_CREATE_JIRA_ISSUES IMPLEMENTATION.


  METHOD constructor.

    CLEAR:
      gt_config,
      gt_dedup_status,
      go_issues_api.

*- Dependency Injection
    IF io_issues_api IS INITIAL.
      go_issues_api = NEW zcl_jira_issues( ).
    ELSE.
      go_issues_api = io_issues_api.
    ENDIF.

    IF io_user_search_api IS INITIAL.
      go_user_search_api = NEW zcl_jira_user_search( ).
    ELSE.
      go_user_search_api = io_user_search_api.
    ENDIF.

    IF io_utility IS INITIAL.
      go_utility = NEW zcl_jira_utility( ).
    ELSE.
      go_utility = io_utility.
    ENDIF.

*- Fetch Jira Config
    SELECT *
     FROM tvarvc
     INTO TABLE gt_config
     WHERE name LIKE '%JIRA%'.
    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_jira_exceptions
        EXPORTING
          code   = 404
          reason = 'Not Found'
          msgv1  = 'Jira Config Not Found'.
    ELSE.
*-   Creating Ranges for Dedup Check Status
      gt_dedup_status = VALUE #( FOR ls_config IN gt_config
                                WHERE ( name = 'ZJIRA_DEDUP_CHECK_STATUS' )
                                  ( sign = ls_config-sign
                                    option = ls_config-opti
                                    low = ls_config-low ) ).
      IF gt_dedup_status IS INITIAL.
        RAISE EXCEPTION TYPE zcx_jira_exceptions
          EXPORTING
            code   = 404
            reason = 'Not Found'
            msgv1  = 'Dedup Check Status Config Not Found'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_utm_create_jira_issues~create_jira_issues.

    CLEAR: et_logs.

    LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<ls_object>).

*-   Skip if No Failure
      IF <ls_object>-test_classes[ 1 ]-state-issue-has_failure = abap_false.
        CONTINUE.
      ENDIF.

*-   Dedup Check
      me->zif_utm_create_jira_issues~dedup_check(
        EXPORTING
          is_object = <ls_object>
        IMPORTING
          et_logs   = DATA(lt_logs)
        RECEIVING
          rv_skip   = DATA(lv_skip)
      ).

      IF lv_skip EQ abap_true.
        APPEND LINES OF lt_logs TO et_logs.
        CLEAR: lt_logs.
        CONTINUE.
      ENDIF.

      TRY.
*-       Create Payload Structure
          create_payload(
            EXPORTING
              is_object  = <ls_object>
             IMPORTING
               es_create_request = DATA(ls_payload)
          ).
        CATCH zcx_jira_exceptions INTO DATA(lo_exception).
          et_logs = VALUE #( BASE et_logs
                     ( devclass = <ls_object>-package
                       obj_type = <ls_object>-obj_type
                       obj_name = <ls_object>-obj_name
                       created_on = sy-datum
                       created_at = sy-uzeit
                       changed_on = sy-datum
                       changed_at = sy-uzeit
                       changed_by = sy-uname
                       issue_status = zif_jira_issues=>gc_bug_status-error
                       message = lo_exception->get_text( ) ) ).
          CONTINUE.
      ENDTRY.

      TRY.
*-       Create Jira Issue
          go_issues_api->create_issue(
            EXPORTING
              is_request  = ls_payload
             IMPORTING
               es_response = DATA(ls_response)
          ).

          et_logs = VALUE #( BASE et_logs
                     ( devclass = <ls_object>-package
                       obj_type = <ls_object>-obj_type
                       obj_name = <ls_object>-obj_name
                       created_on = sy-datum
                       created_at = sy-uzeit
                       changed_on = sy-datum
                       changed_at = sy-uzeit
                       changed_by = sy-uname
                       issue_assignee = get_last_change_by( <ls_object>-obj_name )
                       issue_key = ls_response-key
                       issue_status = zif_jira_issues=>gc_bug_status-to_do
                       message = |New Issue was created successfully|
                     ) ).

        CATCH zcx_jira_exceptions INTO lo_exception.
          et_logs = VALUE #( BASE et_logs
                     ( devclass = <ls_object>-package
                       obj_type = <ls_object>-obj_type
                       obj_name = <ls_object>-obj_name
                       created_on = sy-datum
                       created_at = sy-uzeit
                       changed_on = sy-datum
                       changed_at = sy-uzeit
                       changed_by = sy-uname
                       issue_status = zif_jira_issues=>gc_bug_status-error
                       message = lo_exception->get_text( ) ) ).
          CONTINUE.
      ENDTRY.

    ENDLOOP.

*- Update the Logs
    modify_utm_logs( et_logs ).

  ENDMETHOD.


  METHOD create_payload.

    DATA: lv_due_date TYPE sydatum.

    DATA: ls_fields TYPE zif_jira_issues=>ts_fields.

    CLEAR: es_create_request.

*- Priority
    ls_fields-priority-id = gt_config[ name = 'ZJIRA_PRIORITY' ]-low.

*- Assignee Account ID
    TRY.
        ls_fields-assignee-account_id =
          go_user_search_api->get_jira_account_id(
             get_last_change_by( is_object-obj_name ) ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.

*- Parent Issue ID
    ls_fields-parent-key = gt_config[ name = 'ZJIRA_PARENT_ISSUE' ]-low.


*- Component ID
    ls_fields-components = VALUE #( (
                     id = gt_config[ name = 'ZJIRA_COMPONENT_ID' ]-low ) ).

*- Summary Text (Title)
    ls_fields-summary = |Unit Test Failure: { sy-sysid }({ sy-mandt })|.
    DATA(lv_summmary) = | { map_obj_type( is_object-obj_type ) } { is_object-obj_name }|.
    CONCATENATE ls_fields-summary lv_summmary
     INTO ls_fields-summary SEPARATED BY ','.

*- Severity ID
    ls_fields-customfield_10043-id = gt_config[ name = 'ZJIRA_SEVERITY' ]-low.

*- Scrum Team ID
    ls_fields-customfield_10035-id = gt_config[ name = 'ZJIRA_SCRUM_TEAM' ]-low.

*- Reporter Account ID
    TRY.
        ls_fields-reporter-account_id =
         go_user_search_api->get_jira_account_id(
         CONV #( gt_config[ name = 'ZJIRA_REPORTER_USERID' ]-low ) ).
      CATCH zcx_jira_exceptions INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.

*- Issue Type
    ls_fields-issuetype-id =  gt_config[ name = 'ZJIRA_ISSUE_TYPE' ]-low.

*- Project ID
    ls_fields-project-id =  gt_config[ name = 'ZJIRA_PROJECT_ID' ]-low.

*- Labels
    ls_fields-labels =  VALUE #( ( gt_config[ name = 'ZJIRA_LABEL' ]-low ) ).

*- Notes
    create_notes_adf(
      EXPORTING
        is_object = is_object
      IMPORTING
        es_doc      = ls_fields-customfield_10040 ).

*- Body Content
    create_description_adf(
      EXPORTING
        is_object = is_object
       IMPORTING
         es_doc    = ls_fields-description ).

*- Acceptance Criteria
    create_acceptance_adf(
      EXPORTING
        is_object = is_object
      IMPORTING
        es_doc    = ls_fields-customfield_10038 ).

*- Exporting Payload
    es_create_request-fields = ls_fields.

  ENDMETHOD.


  METHOD get_last_change_by.

    DATA: lv_obj_name TYPE string.

    lv_obj_name = |%{ iv_obj_name }%|.

    SELECT
      a~trkorr,
      b~as4user
    FROM e071 AS a
     INNER JOIN e070 AS b
      ON a~trkorr = b~trkorr
    INTO TABLE @DATA(lt_data)
     WHERE a~obj_name LIKE @lv_obj_name
    ORDER BY b~as4date DESCENDING,
             b~as4time DESCENDING.
    IF sy-subrc IS INITIAL.
      rv_last_change_by = lt_data[ 1 ]-as4user.
    ELSE.
      RAISE EXCEPTION TYPE zcx_jira_exceptions
        EXPORTING
          code   = 404
          reason = 'Not Found'
          msgv1  = 'Last Change By User Not Found'.
    ENDIF.

  ENDMETHOD.


  METHOD create_notes_adf.

    DATA: lv_text TYPE string.

    CLEAR: es_doc.

    lv_text = |{ sy-sysid }/{ sy-mandt }|.
    DATA(lv_text_2) = | { map_obj_type( is_object-obj_type ) } { is_object-obj_name }|.

    CONCATENATE lv_text lv_text_2
     INTO lv_text SEPARATED BY ','.

    DATA(lo_text) = create_text_node( lv_text ).

    DATA(lo_paragraph) = NEW zcl_jira_adf_builder(
         iv_type  = zif_jira_adf_types=>gc_nodes-paragraph )->add_content( lo_text ).

    es_doc-version = 1.
    es_doc-type = zif_jira_adf_types=>gc_doc.
    es_doc-content = VALUE #( ( lo_paragraph->get_node_data( ) ) ).

  ENDMETHOD.


  METHOD create_description_adf.

    DATA: lt_nodes TYPE zif_jira_adf_types=>tt_nodes.

    CLEAR: es_doc.

*- Warning Panel
    DATA(ls_panel_node) = create_panel_node( is_object )->get_node_data( ).
    APPEND ls_panel_node TO lt_nodes.

*- Table for Test Methods
    DATA(ls_table_node) = create_table_node( is_object  )->get_node_data( ).
    APPEND ls_table_node TO lt_nodes.

*- Final ADF Document
    es_doc-version = 1.
    es_doc-type = zif_jira_adf_types=>gc_doc.
    es_doc-content = lt_nodes.

  ENDMETHOD.


  METHOD map_obj_type.

    CASE iv_obj_type.
      WHEN 'CLAS'.
        rv_text = 'Class'.

      WHEN 'PROG'.
        rv_text = 'Program'.

      WHEN 'FUGR'.
        rv_text = 'Function'.
    ENDCASE.

  ENDMETHOD.


  METHOD create_panel_node.

    DATA(lv_obj_name) = |{ map_obj_type( is_object-obj_type ) }: { is_object-obj_name }|.
    DATA(lv_test_class) = |Test Class: { is_object-test_classes[ 1 ]-name }|.
    DATA(lv_text) = |{ lv_obj_name }, { lv_test_class }|.

    DATA(lo_text) = create_text_node(
                      iv_text   = lv_text
                      iv_strong = abap_true
                      iv_color  = zif_jira_adf_types=>gc_colors-orange ).

    DATA(lo_paragraph) = NEW zcl_jira_adf_builder(
      iv_type  = zif_jira_adf_types=>gc_nodes-paragraph )->add_content( lo_text ).

*- Warning Panel
    DATA(lo_panel) = NEW zcl_jira_adf_builder(
      iv_type = zif_jira_adf_types=>gc_nodes-panel
      is_attrs = VALUE #( panel_type = zif_jira_adf_types=>gc_panel_types-warning )
       )->add_content( lo_paragraph ).

    ro_node = lo_panel.

  ENDMETHOD.


  METHOD create_table_node.

*-  Creating the Table
    DATA(lo_table) = NEW zcl_jira_adf_builder(
       iv_type  =  zif_jira_adf_types=>gc_nodes-table
       is_attrs = VALUE #(
                    is_number_column_enabled = abap_false
                    layout = zif_jira_adf_types=>gc_table_layouts-default  ) ).

*- Heading Row
    DATA(lo_table_header_row) = NEW zcl_jira_adf_builder(
         iv_type = zif_jira_adf_types=>gc_nodes-table_row
       )->add_content(
         create_table_header_row( 'Test Method Name' )
       )->add_content( create_table_header_row( 'Error Message' ) ).

*- Add the Heading Row
    lo_table->add_content( lo_table_header_row ).

*- Adding Test Methods
    LOOP AT is_object-test_classes[ 1 ]-test_methods
     ASSIGNING FIELD-SYMBOL(<ls_test_method>).

      IF <ls_test_method>-alert-kind EQ gc_failure.
*-    Table Cell Rows
        DATA(lo_cell_row) = NEW zcl_jira_adf_builder(
          iv_type = zif_jira_adf_types=>gc_nodes-table_row ).

        lo_cell_row->add_content( create_table_cell_row( <ls_test_method>-name ) ).
        lo_cell_row->add_content( create_table_cell_row(
          compress_msg( <ls_test_method>-alert-description ) ) ).

        lo_table->add_content( lo_cell_row ).

        CLEAR: lo_cell_row.
      ENDIF.
    ENDLOOP.

    ro_node = lo_table.

  ENDMETHOD.


  METHOD create_table_header_row.

    DATA(lo_text) = create_text_node(
                      iv_text   = iv_heading
                      iv_strong = abap_true ).

    DATA(lo_paragraph) = NEW zcl_jira_adf_builder(
         iv_type  = zif_jira_adf_types=>gc_nodes-paragraph
       )->add_content( lo_text ).

    DATA(lo_table_header) = NEW zcl_jira_adf_builder(
        iv_type = zif_jira_adf_types=>gc_nodes-table_header
        is_attrs = VALUE #(
                     colwidth = VALUE #( ( iv_col_width ) ) )
       )->add_content( lo_paragraph ).

    ro_node = lo_table_header.

  ENDMETHOD.


  METHOD create_table_cell_row.

    DATA(lo_text) = create_text_node( iv_cell_value ).

    DATA(lo_paragraph) = NEW zcl_jira_adf_builder(
      iv_type  = zif_jira_adf_types=>gc_nodes-paragraph )->add_content( lo_text ).

    DATA(lo_table_cell) = NEW zcl_jira_adf_builder(
        iv_type = zif_jira_adf_types=>gc_nodes-table_cell
        is_attrs = VALUE #(
                     colwidth = VALUE #( ( iv_col_width ) ) )
       )->add_content( lo_paragraph ).

    ro_node = lo_table_cell.

  ENDMETHOD.


  METHOD create_text_node.

    DATA(lo_text) = NEW zcl_jira_adf_builder(
        iv_type  = zif_jira_adf_types=>gc_nodes-text
        iv_text  = iv_text ).

    IF iv_strong = abap_true.
      lo_text->add_marks(
            VALUE #( type = zif_jira_adf_types=>gc_mark_types-strong ) ).
    ENDIF.

    IF iv_color IS NOT INITIAL.
      lo_text->add_marks(
         VALUE #( type = zif_jira_adf_types=>gc_mark_types-text_color
                  attrs-color = zif_jira_adf_types=>gc_colors-orange ) ).
    ENDIF.

    ro_node = lo_text.

  ENDMETHOD.


  METHOD compress_msg.

    rv_msg = iv_msg.

    DATA(lv_pattern) = |Critical Assertion Error: |.

    REPLACE lv_pattern IN rv_msg WITH space.
    REPLACE ALL OCCURRENCES OF '''' IN rv_msg WITH space.

  ENDMETHOD.


  METHOD modify_utm_logs.

    IF it_logs IS NOT INITIAL.
*-   Modify the Logs
      MODIFY zdt_utm_logs FROM TABLE it_logs.
      IF sy-subrc IS NOT INITIAL.
        rv_error = abap_true.
      ELSE.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_utm_create_jira_issues~print_logs.

    LOOP AT it_logs ASSIGNING FIELD-SYMBOL(<ls_log>).
      WRITE:/ |Object: { <ls_log>-obj_type }|,
            / |Object: { <ls_log>-obj_name }|,
            / |Created On: { <ls_log>-created_on }|,
            / |Issue Key: { <ls_log>-issue_key }|,
            / |Message: { <ls_log>-message }|.

      SKIP 1.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_list_item.

    DATA(lo_text1) = create_text_node(
                      iv_text   = iv_text1
                      iv_strong = abap_true ).

    DATA(lo_text2) = create_text_node(
                      iv_text   = iv_text2
                      iv_strong = abap_true ).

    DATA(lo_paragraph) = NEW zcl_jira_adf_builder(
      iv_type  = zif_jira_adf_types=>gc_nodes-paragraph
      )->add_content( lo_text1
      )->add_content( lo_text2 ).

    DATA(lo_list_item) = NEW zcl_jira_adf_builder(
      iv_type  = zif_jira_adf_types=>gc_nodes-list_item )->add_content( lo_paragraph ).

    ro_node = lo_list_item.

  ENDMETHOD.


  METHOD create_acceptance_adf.

    DATA: lt_nodes TYPE zif_jira_adf_types=>tt_nodes.

    CLEAR: es_doc.

    DATA(lo_bullet_item) = NEW zcl_jira_adf_builder(
      iv_type  = zif_jira_adf_types=>gc_nodes-bulletlist ).

    DATA(lo_list_item1) = create_list_item(
                            iv_text1 = 'Unit Tests: '
                            iv_text2 = CONV #( TEXT-001 )
                          ).

    DATA(lo_list_item2) = create_list_item(
                     iv_text1 = 'Code Coverage: '
                     iv_text2 = CONV #( TEXT-002 )
                   ).

    lo_bullet_item->add_content( lo_list_item1 ).
    lo_bullet_item->add_content( lo_list_item2 ).

    APPEND lo_bullet_item->get_node_data( ) TO lt_nodes.

*- Final ADF Document
    es_doc-version = 1.
    es_doc-type = zif_jira_adf_types=>gc_doc.
    es_doc-content = lt_nodes.

  ENDMETHOD.


  METHOD zif_utm_create_jira_issues~bulk_upd_issue_status.

    DATA: lv_jql  TYPE string,
          lv_flag TYPE abap_bool,
          lv_msg  TYPE string.

*- Fetch all the Issues, which are not in Done Status
    SELECT *
     FROM zdt_utm_logs
    INTO TABLE @DATA(lt_issues)
    WHERE issue_key NE ''.
    IF NOT sy-subrc IS INITIAL.
      RETURN.
    ENDIF.

    DESCRIBE TABLE lt_issues LINES DATA(lv_count).

*- Create JQL Query String
    LOOP AT lt_issues ASSIGNING FIELD-SYMBOL(<ls_issue>).
      DATA(lv_key) = |'{ <ls_issue>-issue_key }'|.

      IF sy-tabix = 1.
        lv_jql = |key in ({ lv_key }|.

        IF lv_count = 1.
          lv_jql = |{ lv_jql })|.
        ENDIF.
      ELSEIF sy-tabix = lv_count. "Last Row
        lv_jql = |{ lv_jql }, { lv_key })|.
      ELSE.
        lv_jql = |{ lv_jql }, { lv_key }|.
      ENDIF.

      CLEAR: lv_key.
    ENDLOOP.

    TRY.
*-     Get all the issues status
        go_issues_api->search_issue_by_jql(
          EXPORTING
            is_query_params = VALUE #(
                                jql = lv_jql
                                max_results = lv_count
                                fields = 'status,assignee' )
          IMPORTING
            es_response     = DATA(ls_response)
        ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.

    DATA(lt_issue_upd) = lt_issues.

    LOOP AT lt_issue_upd ASSIGNING <ls_issue>.
      CLEAR: lv_msg,
             lv_flag.

*-   Get the Issue details from the Response
      READ TABLE ls_response-issues INTO DATA(ls_issue_data)
       WITH KEY key = <ls_issue>-issue_key.
      IF sy-subrc IS INITIAL.
*-     Extract Assignee's User ID from the Email Address
        SPLIT ls_issue_data-fields-assignee-email_address AT '@'
         INTO DATA(lv_user_id) DATA(lv_domain).
        IF sy-subrc IS INITIAL.
          DATA(lv_assignee) = to_upper( lv_user_id ).
        ENDIF.

        IF <ls_issue>-issue_status NE ls_issue_data-fields-status-id
       AND <ls_issue>-issue_assignee NE lv_assignee.
          lv_msg = |Issue Status & Assignee were updated|.
          lv_flag = abap_true.
        ELSEIF <ls_issue>-issue_status NE ls_issue_data-fields-status-id
           AND <ls_issue>-issue_assignee EQ lv_assignee.
          lv_msg = |Issue Status was updated|.
          lv_flag = abap_true.
        ELSEIF <ls_issue>-issue_status EQ ls_issue_data-fields-status-id
           AND <ls_issue>-issue_assignee NE lv_assignee.
          lv_msg = |Issue Assignee was updated|.
          lv_flag = abap_true.
        ELSEIF <ls_issue>-issue_status EQ ls_issue_data-fields-status-id
           AND <ls_issue>-issue_assignee EQ lv_assignee.
          lv_flag = abap_false.
        ENDIF.

*-     Skip if No Changes
        IF lv_flag EQ abap_false.
          CONTINUE.
        ENDIF.

        <ls_issue>-issue_assignee = lv_assignee.
        <ls_issue>-issue_status = ls_issue_data-fields-status-id.
        <ls_issue>-changed_on = sy-datum.
        <ls_issue>-changed_at = sy-uzeit.
        <ls_issue>-changed_by = sy-uname.
        <ls_issue>-message = lv_msg.
      ENDIF.
    ENDLOOP.

*- Update the Table
    IF lt_issue_upd NE lt_issues.
      modify_utm_logs( lt_issue_upd ).

      WRITE:/ |Bulk Issues Status Update Logs:|.

      me->zif_utm_create_jira_issues~print_logs( lt_issue_upd ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_utm_create_jira_issues~dedup_check.

*- Check if any Issue is already Open
    SELECT *
     FROM zdt_utm_logs
    INTO TABLE @et_logs UP TO 1 ROWS
    WHERE devclass = @is_object-package
      AND obj_type = @is_object-obj_type
      AND obj_name = @is_object-obj_name
     ORDER BY created_on DESCENDING.
    IF sy-subrc IS INITIAL.
      ASSIGN et_logs[ 1 ] TO FIELD-SYMBOL(<ls_log>).

*-   If NOT in these Status - Issue Should be created
      IF <ls_log>-issue_status NOT IN gt_dedup_status[].
        <ls_log>-changed_on = sy-datum.
        <ls_log>-changed_at = sy-uzeit.
        <ls_log>-changed_by = sy-uname.
        <ls_log>-message = |Dedup Check: Issue creation skipped|.

        rv_skip = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
