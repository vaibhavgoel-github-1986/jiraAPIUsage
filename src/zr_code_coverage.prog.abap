*&---------------------------------------------------------------------*
*& Report ZVG_COVERAGE_API
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_code_coverage MESSAGE-ID s_aucv_runner.

INCLUDE zuscm_code_coverage_top.

DATA: gv_query TYPE string.

DATA: gt_coverage TYPE STANDARD TABLE OF zscmd_coverage.

DATA: gt_tadir TYPE STANDARD TABLE OF tadir.

DATA: gt_objects_hash TYPE HASHED TABLE OF ty_program
       WITH UNIQUE KEY obj_name obj_type.

DATA: go_coverage_api TYPE REF TO zif_code_coverage_api.

DATA: gt_mdg_coverage TYPE STANDARD TABLE OF zscmd_coverage.

AT SELECTION-SCREEN OUTPUT.
  PERFORM at_selection_screen_output.

AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

START-OF-SELECTION.

  CLEAR: gv_query,
         gt_mdg_coverage,
         gt_objects_hash,
         gt_coverage,
         go_coverage_api.

*- Code Coverage API
  go_coverage_api = NEW zcl_code_coverage_api( ).

  DATA(go_fetch_data) = NEW lcl_fetch_data( ).

  DATA(lv_start_date) = sy-datum.
  DATA(lv_start_time) = sy-uzeit.

*- Fetch TADIR Objects
  go_fetch_data->select_objects( ).

*- Get Complete Coverage
  go_coverage_api->get_units_coverage(
    EXPORTING
      it_program_keys  = CORRESPONDING sabp_t_tadir_keys( go_fetch_data->gt_programs )
    IMPORTING
      et_coverage      = DATA(lt_stmt_coverage)
      et_aunit_results = DATA(lt_aunit_results) ).

*- Converting tto Hashed Table
  gt_objects_hash = go_fetch_data->gt_programs.

  IF cb_prod EQ abap_true.

    DATA:
      lt_query  TYPE STANDARD TABLE OF rfc_db_opt,
      lt_fields TYPE STANDARD TABLE OF rfc_db_fld,
      lt_data   TYPE STANDARD TABLE OF tab512.

    CLEAR:
     gt_tadir.

    lt_query = VALUE #( ( |devclass LIKE 'Z%' AND| )
                        ( |(object = 'CLAS' OR object = 'PROG' OR object = 'FUGR')| ) ).

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION 'TMSSUP@PHA.DOMAIN_DHA'
      EXPORTING
        query_table          = 'TADIR'
      TABLES
        options              = lt_query                 " Selection entries, "WHERE clauses" (in)
        fields               = lt_fields                 " Names (in) and structure (out) of fields read
        data                 = lt_data                 " Data read (out)
      EXCEPTIONS
        table_not_available  = 1                " QUERY_TABLE not active in Dictionary
        table_without_data   = 2                " QUERY_TABLE is name of structure
        option_not_valid     = 3                " Selection entries (e.g. syntax) incorrect
        field_not_valid      = 4                " Field to be read not in table
        not_authorized       = 5                " User not authorized to access QUERY_TABLE
        data_buffer_exceeded = 6                " Selected fields do not fit into structure DATA
        OTHERS               = 7.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_data INTO DATA(ls_data).
        APPEND INITIAL LINE TO gt_tadir
         ASSIGNING FIELD-SYMBOL(<ls_tadir>).
        <ls_tadir> = ls_data.
        UNASSIGN <ls_tadir>.
      ENDLOOP.
    ELSE.
      MESSAGE 'RFC Connection failed' TYPE 'S'
       DISPLAY LIKE 'I'.
    ENDIF.
  ENDIF.

*- For Binary Search
  SORT lt_aunit_results BY type name.

*- Building Output Table
  LOOP AT lt_stmt_coverage ASSIGNING FIELD-SYMBOL(<ls_coverage>).

*- Filter out
    IF cb_prod EQ abap_true AND gt_tadir IS NOT INITIAL.
      READ TABLE gt_tadir TRANSPORTING NO FIELDS
       WITH KEY object = <ls_coverage>-type
                obj_name = <ls_coverage>-name.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    APPEND INITIAL LINE TO gt_coverage ASSIGNING FIELD-SYMBOL(<ls_output>).
    TRY.
        <ls_output> = CORRESPONDING #( gt_objects_hash[ obj_type = <ls_coverage>-type
                                                        obj_name = <ls_coverage>-name ] ).
        <ls_output> = CORRESPONDING #( BASE ( <ls_output> ) <ls_coverage> ).

*-     AUNIT Results
        READ TABLE lt_aunit_results INTO DATA(ls_aunit_result)
         WITH KEY type = <ls_coverage>-type
                  name = <ls_coverage>-name BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <ls_output>-total_test_cases = ls_aunit_result-total_test_cases.
          <ls_output>-failure_cnt = ls_aunit_result-failure_cnt.
        ENDIF.

        <ls_output>-last_run_date = lv_start_date.
        <ls_output>-last_run_time = lv_start_time.
      CATCH cx_sy_itab_line_not_found.
        CONTINUE.
    ENDTRY.
    UNASSIGN <ls_output>.
  ENDLOOP.

*- Ignoring objects which are not needed in UTM Coverage
  DELETE gt_coverage WHERE total IS INITIAL.

  IF cb_upd EQ abap_true.
*- Clear the table
    SELECT *
     FROM zscmd_coverage
    INTO TABLE @DATA(lt_delete).
    IF sy-subrc IS INITIAL.
      DELETE zscmd_coverage FROM TABLE lt_delete.
    ENDIF.

    INSERT zscmd_coverage FROM TABLE gt_coverage.
    IF sy-subrc IS INITIAL.
      MESSAGE 'Coverage DB was updated successfully'
       TYPE 'S'.

      COMMIT WORK.
    ELSE.
      MESSAGE 'Coverage DB updation failed'
       TYPE 'E'.
    ENDIF.
  ENDIF.

  IF sy-batch IS INITIAL.
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = if_salv_c_bool_sap=>true
          IMPORTING
             r_salv_table = DATA(lo_salv_table)
          CHANGING
             t_table      = gt_coverage ).
      CATCH cx_salv_msg INTO DATA(lex_exception).
        DATA(lv_message) = lex_exception->get_text( ).
    ENDTRY.

    DATA(lo_columns) = lo_salv_table->get_columns( ).
    lo_columns->set_optimize( 'X' ).
    lo_salv_table->get_functions( )->set_all( abap_true ).

    DATA(lo_layout) = lo_salv_table->get_layout( ).
    lo_layout->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    lo_layout->set_default( abap_true ).
    lo_salv_table->display( ).
  ENDIF.



FORM at_selection_screen_output .

  PERFORM sub_mode.

ENDFORM.

FORM sub_mode.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'DVC'.
        IF b_devc = abap_true.
          screen-active = 1.
          screen-invisible = 0.
        ELSE.
          screen-active = 0.
          screen-invisible = 1.
        ENDIF.
      WHEN 'OBJ'.
        IF b_obj = abap_true.
          screen-active = 1.
          screen-invisible = 0.
        ELSE.
          screen-active = 0.
          screen-invisible = 1.
        ENDIF.
      WHEN 'XCL' OR 'XC2'.
        IF p_excl = abap_true AND b_devc = abap_true.
          screen-active = 1.
          screen-invisible = 0.
          IF p_packr = abap_false AND screen-group1 = 'XC2'.
            screen-active = 0.
            screen-invisible = 1.
          ENDIF.
        ELSE.
          screen-active = 0.
          screen-invisible = 1.
        ENDIF.

      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

FORM at_selection_screen .

  IF sy-ucomm = 'ONLI' OR sy-ucomm = 'SJOB'.
    IF b_devc = abap_true.
      IF so_devc[] IS NOT INITIAL.
        p_selcl = p_selfg = p_selprg = abap_true.
      ELSE.
        MESSAGE e002. "check devc not initial
      ENDIF.
    ENDIF.

    IF b_obj = abap_true.
      IF so_class[] IS INITIAL AND
         so_fugr[]  IS INITIAL AND
         so_prog[]  IS INITIAL.
        MESSAGE e003. "check devc not initial
      ENDIF.

      CLEAR: p_selcl,
             p_selfg,
             p_selprg.

      IF so_class[] IS NOT INITIAL.
        p_selcl = abap_true.
      ENDIF.

      IF so_fugr[]  IS NOT INITIAL.
        p_selfg = abap_true.
      ENDIF.

      IF so_prog[]  IS NOT INITIAL.
        p_selprg = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
