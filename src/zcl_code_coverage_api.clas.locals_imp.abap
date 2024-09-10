*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_units IMPLEMENTATION.

  METHOD get_statement_coverage.

    CONSTANTS: lc_limit_on_duration_category TYPE saunit_d_allowed_rt_duration VALUE 36.
    CONSTANTS: lc_limit_on_risk_level        TYPE saunit_d_allowed_risk_level  VALUE 11.

    DATA: ls_converted_key TYPE cl_aucv_task=>ty_object_directory_element.
    DATA: lt_converted_keys TYPE cl_aucv_task=>ty_object_directory_elements.

    CLEAR: et_coverage.

    DATA: lt_program_keys TYPE sabp_t_tadir_keys.

    "Incoming keys
    APPEND LINES OF it_program_keys TO lt_program_keys.

    "Filter out for programs with unit tests
    get_programs_with_tests( CHANGING ct_program_keys = lt_program_keys ).

    "Get the listener
    DATA(lr_listener) = cl_saunit_gui_service=>create_listener( ).

    TRY.
        "Get AUCV Task
        DATA(lr_task) =
          cl_aucv_task=>create(
            EXPORTING i_listener = lr_listener
                      i_measure_coverage = iv_with_coverage
                      i_max_risk_level = lc_limit_on_risk_level
                      i_max_duration_category = lc_limit_on_duration_category  ).

        "Convert Program Keys to AUCV format
        LOOP AT lt_program_keys ASSIGNING FIELD-SYMBOL(<fs_program_key>).
          ls_converted_key-object = <fs_program_key>-obj_type.
          ls_converted_key-obj_name = <fs_program_key>-obj_name.
          INSERT ls_converted_key INTO TABLE lt_converted_keys.
          CLEAR: ls_converted_key.
        ENDLOOP.

        "For the keys get unit test
        lr_task->add_associated_unit_tests( lt_converted_keys ).

        "Run the tests
        lr_task->run( if_aunit_task=>c_run_mode-catch_short_dump ).

        "Get result.
        mr_aunit_result = lr_listener->get_result_after_end_of_task( ).
      CATCH cx_root INTO DATA(lo_exception).
        MESSAGE 'CX_ROOT Exception was raised' TYPE 'I'.
    ENDTRY.

    "Is coverage requested
    IF iv_with_coverage EQ abap_true.
      TRY.
          measure_coverage(
            EXPORTING
              it_program_keys = it_program_keys
              ir_coverage_measurement = lr_task->get_coverage_measurement( ) ).
        CATCH cx_scv_execution_error.
      ENDTRY.

      get_coverage_results(
       IMPORTING
         et_results = et_coverage
      ).
    ENDIF.

  ENDMETHOD.

  METHOD measure_coverage.

    DATA: lv_object_type TYPE trobjtype.

    DATA: lr_coverage_result_provider TYPE REF TO lcl_coverage.

    FIELD-SYMBOLS: <fs_program_key> TYPE sabp_s_tadir_key.

    "Coverage for each unit separately
    LOOP AT it_program_keys ASSIGNING <fs_program_key>.

      lv_object_type = <fs_program_key>-obj_type.

      "Get main program name for TADIR
      DATA(lv_program_name) =
        cl_aunit_prog_info=>tadir_to_progname(
          obj_name = <fs_program_key>-obj_name
          obj_type = <fs_program_key>-obj_type ).

      "Call coverage class
      TRY.
          lr_coverage_result_provider =
            NEW lcl_coverage(
              iv_program_name         = lv_program_name
              iv_object_type          = lv_object_type
              iv_coverage_measurement = ir_coverage_measurement ).

          "Hold on to coverage resukts
          APPEND lr_coverage_result_provider TO mt_coverage_result_provider.
          CLEAR: lr_coverage_result_provider.
        CATCH cx_root. "into failure ##catch_All ##needed.
          CLEAR: lr_coverage_result_provider.
      ENDTRY.

      CLEAR: lv_program_name,
             lv_object_type.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_abapunit_results.

    DATA: lv_task_data    TYPE if_saunit_internal_result_type=>ty_s_task.

    DATA: ls_result  TYPE ty_s_alert_total.
    DATA: lt_results TYPE ty_t_alert_totals.

    FIELD-SYMBOLS: <fs_result>           TYPE ty_s_alert_total.
    FIELD-SYMBOLS: <fs_total_on_task>    TYPE ty_s_alert_total.
    FIELD-SYMBOLS: <fs_total_on_program> TYPE ty_s_alert_total.
    FIELD-SYMBOLS: <fs_total_on_class>   TYPE ty_s_alert_total.
    FIELD-SYMBOLS: <fs_total_on_method>  TYPE ty_s_alert_total.
    FIELD-SYMBOLS: <fs_program>          TYPE ty_s_prog_data.
    FIELD-SYMBOLS: <fs_class>            TYPE ty_s_clas_data.
    FIELD-SYMBOLS: <fs_method>           TYPE ty_s_meth_data.
    FIELD-SYMBOLS: <fs_alerts_by_index>  TYPE ty_s_alerts_by_index.
    FIELD-SYMBOLS: <fs_infos_by_index>   TYPE ty_s_extd_infos_by_index.
    FIELD-SYMBOLS: <fs_alert>            TYPE ty_s_alert_data.
    FIELD-SYMBOLS: <fs_units>            TYPE ty_s_units.

    DEFINE mac_add_alerts_to_total.
      LOOP AT lv_task_data-alerts_by_indicies[]
        ASSIGNING <fs_alerts_by_index>
        WHERE
          program_ndx = &1-program_ndx AND
          class_ndx =   &1-class_ndx   AND
          method_ndx =  &1-method_ndx.

        LOOP AT <fs_alerts_by_index>-alerts ASSIGNING <fs_alert>.
          ADD 1 TO &2-cnt_alert.
          CASE <fs_alert>-kind.
            WHEN gc_alert_kind-error.
              ADD 1 TO &2-cnt_error.
            WHEN gc_alert_kind-failure.
              ADD 1 TO &2-cnt_failure.
            WHEN gc_alert_kind-warning.
              ADD 1 TO &2-cnt_warning.
            WHEN gc_alert_kind-shortdump.
              ADD 1 TO &2-cnt_shortdump.
            WHEN gc_alert_kind-execution_event.
              " do not count
            WHEN OTHERS.
              ASSERT 1 = 2. " yuk horrible
          ENDCASE.
          CASE <fs_alert>-level.
            WHEN gc_alert_level-information.
              ADD 1 TO &2-cnt_info.
            WHEN gc_alert_level-critical.
              ADD 1 TO &2-cnt_critical.
            WHEN gc_alert_level-fatal.
              ADD 1 TO &2-cnt_fatal.
            WHEN OTHERS. " gc_Alert_Level-Tolerable.
              ADD 1 TO &2-cnt_tolerable.
          ENDCASE.
        ENDLOOP.
       ENDLOOP.
    end-of-definition.

    DEFINE mac_add_infos_to_total.
      LOOP AT lv_task_data-extd_infos_by_indicies[]
        ASSIGNING <fs_infos_by_index>
          WHERE program_ndx = &1-program_ndx
            AND class_ndx = &1-class_ndx
            AND method_ndx = &1-method_ndx.
          ADD 1 TO &2-cnt_extended.
          ADD 1 TO &2-cnt_info.
       ENDLOOP.
    end-of-definition.

    DATA:
      total_for_error TYPE ty_s_alert_total.

    DEFINE mac_clone_if_not_initial.

      IF ( 0 = &1-cnt_extended AND 0 = &1-cnt_alert ).
        "Nothing to do
*        return.
      ELSE.
      total_for_error = &1.
      CASE &1-degree.
        WHEN gc_info_degree-task.
          total_for_error-degree = gc_info_degree-task_only.
        WHEN gc_info_degree-program.
          total_for_error-degree = gc_info_degree-program_only.
        WHEN gc_info_degree-class.
          total_for_error-degree = gc_info_degree-class_only.
        WHEN OTHERS.
          RETURN.
      ENDCASE.
      INSERT total_for_error INTO TABLE lt_results. "result.
      ENDIF.
    end-of-definition.

    DEFINE mac_sum_counters.
      ADD &1-cnt_alert     TO  &2-cnt_alert.
      ADD &1-cnt_extended  TO  &2-cnt_extended.

      ADD &1-cnt_error     TO  &2-cnt_error.
      ADD &1-cnt_failure   TO  &2-cnt_failure.
      ADD &1-cnt_shortdump TO  &2-cnt_shortdump.
      ADD &1-cnt_warning   TO  &2-cnt_warning.

      ADD &1-cnt_critical  TO  &2-cnt_critical.
      ADD &1-cnt_fatal     TO  &2-cnt_fatal.
      ADD &1-cnt_tolerable TO  &2-cnt_tolerable.
      ADD &1-cnt_info      TO  &2-cnt_info.
    end-of-definition.

    DATA: lv_total_test_cases TYPE i.

    "Type cast the result
    DATA(lr_aunit_result_casted) = CAST cl_saunit_internal_result( mr_aunit_result ).
    lv_task_data = lr_aunit_result_casted->f_task_data.

    "Create task entry
*    INSERT INITIAL LINE INTO TABLE lt_results ASSIGNING <fs_total_on_task>.
*    <fs_total_on_task>-degree = gc_info_degree-task.
*
*    "Calculate the totals
*    mac_add_alerts_to_total ls_result <fs_total_on_task>.
*    mac_add_infos_to_total  ls_result <fs_total_on_task>.
*    mac_clone_if_not_initial <fs_total_on_task>.

    "Build results by --> Program
    LOOP AT lv_task_data-programs[] ASSIGNING <fs_program>.

*- Begin 774129
      CLEAR: lt_results,
             lv_total_test_cases.

      INSERT INITIAL LINE INTO TABLE lt_results ASSIGNING <fs_total_on_task>.
      <fs_total_on_task>-degree = gc_info_degree-task.

      "Calculate the totals
      mac_add_alerts_to_total ls_result <fs_total_on_task>.
      mac_add_infos_to_total  ls_result <fs_total_on_task>.
      mac_clone_if_not_initial <fs_total_on_task>.
*- End 774129

      ADD 1 TO ls_result-program_ndx.
      ls_result-class_ndx = 0.
      ls_result-method_ndx = 0.

      " create program entry.
      INSERT ls_result INTO TABLE lt_results ASSIGNING <fs_total_on_program>.
      <fs_total_on_program>-degree = gc_info_degree-program.

      "program name..
      <fs_total_on_program>-program = <fs_program>-info-name.

      "Calculate the totals
      mac_add_alerts_to_total ls_result <fs_total_on_program>.
      mac_add_infos_to_total  ls_result <fs_total_on_program>.
      mac_clone_if_not_initial <fs_total_on_program>.

      "Group by Test classes of the program
      LOOP AT <fs_program>-classes ASSIGNING <fs_class>.
        ADD 1 TO ls_result-class_ndx.
        ls_result-method_ndx = 0.

        " create class entry.
        INSERT ls_result INTO TABLE lt_results ASSIGNING <fs_total_on_class>.
        <fs_total_on_class>-degree = gc_info_degree-class.

        "Class name
        <fs_total_on_class>-program = <fs_program>-info-name.
        <fs_total_on_class>-class = <fs_class>-info-name.

        "Calculate the totals
        mac_add_alerts_to_total ls_result <fs_total_on_class>.
        mac_add_infos_to_total  ls_result <fs_total_on_class>.
        mac_clone_if_not_initial <fs_total_on_class>.

        "Group by methods of the test classes
        LOOP AT <fs_class>-methods ASSIGNING <fs_method>.
          ADD 1 TO lv_total_test_cases.
          ADD 1 TO ls_result-method_ndx.

          " create method entry
          INSERT ls_result INTO TABLE lt_results ASSIGNING <fs_total_on_method>.
          <fs_total_on_method>-degree = gc_info_degree-method.

          "Method name..
          <fs_total_on_method>-program = <fs_program>-info-name.
          <fs_total_on_method>-class = <fs_class>-info-name.
          <fs_total_on_method>-method = <fs_method>-info-name.

          "Calculate the totals
          mac_add_alerts_to_total ls_result <fs_total_on_method>.
          mac_add_infos_to_total  ls_result <fs_total_on_method>.
          mac_sum_counters <fs_total_on_method> <fs_total_on_class>.
        ENDLOOP.

        "Totals by class
        mac_sum_counters <fs_total_on_class> <fs_total_on_program>.
      ENDLOOP.

      "Totals by program
      mac_sum_counters <fs_total_on_program> <fs_total_on_task>.

*-   Begin 774129
      READ TABLE lt_results ASSIGNING <fs_result>
       WITH KEY program = <fs_program>-info-name.
      IF sy-subrc IS INITIAL.
        APPEND INITIAL LINE TO et_aunit_results ASSIGNING FIELD-SYMBOL(<fs_results>).
        <fs_results>-name = <fs_program>-info-key-obj_name.
        <fs_results>-type = <fs_program>-info-key-obj_type.
        <fs_results>-total_test_cases = lv_total_test_cases.
        <fs_results>-failure_cnt = <fs_result>-cnt_error +
                                   <fs_result>-cnt_failure.
        UNASSIGN <fs_results>.
      ENDIF.
*-   End 774129

    ENDLOOP.

    "Return ABAP Units results
*    LOOP AT lt_results ASSIGNING <fs_result>.
*      APPEND INITIAL LINE TO et_aunit_results ASSIGNING FIELD-SYMBOL(<fs_results>).
*      MOVE-CORRESPONDING <fs_result> TO <fs_results>.
*    ENDLOOP.

  ENDMETHOD.

  METHOD get_coverage_results.

    DATA: lv_type TYPE trobjtype.

    DATA: lr_coverage_result TYPE REF TO if_scv_result.
    DATA: lr_root_node TYPE REF TO if_scv_result_node.

    DATA: lr_coverage_result_provider TYPE REF TO lcl_coverage.

    DATA: lt_children TYPE if_scv_result_node=>tab,
          ls_child    TYPE REF TO if_scv_result_node.

    FIELD-SYMBOLS: <fs_coverage> TYPE zif_code_coverage_api=>gty_s_coverage.

    CLEAR: et_results.

    LOOP AT mt_coverage_result_provider INTO lr_coverage_result_provider.

      CLEAR: lv_type,
             lt_children.
*             mt_coverage.

      UNASSIGN: <fs_coverage>.

      TRY.
          lr_coverage_result = lr_coverage_result_provider->if_aucv_cvrg_rslt_provider~build_coverage_result( ).
        CATCH cx_scv_call_error INTO DATA(lcx_cx_scv_call_error).
          DATA(lv_error_msg) = lcx_cx_scv_call_error->if_message~get_text( ).
*-       Populate the data with some combination to indicate obj is in syntax error
          APPEND INITIAL LINE TO mt_coverage ASSIGNING <fs_coverage>.
          <fs_coverage>-name =  cl_aunit_prog_info=>convert_program_name_2_tadir(
                                  lr_coverage_result_provider->gv_program_name )-obj_name.
          <fs_coverage>-type = lr_coverage_result_provider->gv_object_type.
          <fs_coverage>-total = 1.
          <fs_coverage>-executed = 1.
          <fs_coverage>-notexecuted = 1.
          <fs_coverage>-percentage = 1.
          CONTINUE.
        CATCH cx_scv_execution_error INTO DATA(lcx_exe_error).
          lv_error_msg = lcx_exe_error->if_message~get_text( ).
*-       Populate the data with some combination to indicate obj is in syntax error
          APPEND INITIAL LINE TO mt_coverage ASSIGNING <fs_coverage>.
          <fs_coverage>-name =  cl_aunit_prog_info=>convert_program_name_2_tadir(
                                  lr_coverage_result_provider->gv_program_name )-obj_name.
          <fs_coverage>-type = lr_coverage_result_provider->gv_object_type.
          <fs_coverage>-total = 1.
          <fs_coverage>-executed = 1.
          <fs_coverage>-notexecuted = 1.
          <fs_coverage>-percentage = 1.
          CONTINUE.
      ENDTRY.

      "root node in coverage_result..
      lr_root_node = lr_coverage_result->get_root_node( ).

*      Read recursively..
*       calculate_coverage_recursively( lr_root_node ).

      IF lr_root_node->has_children( ).
        lt_children = lr_root_node->get_children( ).

        LOOP AT lt_children INTO ls_child.
          DATA(lv_lcl_name) = cl_aunit_prog_info=>convert_program_name_2_tadir( program_name = ls_child->name )-obj_name.
          IF lv_lcl_name EQ 'LCL_TEST_SEAM_HELPER'.
            DATA(lv_lcl_total) = ls_child->get_coverage( ce_scv_coverage_type=>statement )->get_total( ).
            DATA(lv_lcl_executed) = ls_child->get_coverage( ce_scv_coverage_type=>statement )->get_executed( ).
            DATA(lv_lcl_notexecuted) = ls_child->get_coverage( ce_scv_coverage_type=>statement )->get_not_executed( ).
          ENDIF.
          CLEAR: lv_lcl_name.
        ENDLOOP.
      ENDIF.

*    Build coverage for all children
      APPEND INITIAL LINE TO mt_coverage ASSIGNING <fs_coverage>.
      <fs_coverage>-name =  cl_aunit_prog_info=>convert_program_name_2_tadir( program_name = lr_root_node->name )-obj_name.
      <fs_coverage>-type = lr_coverage_result_provider->gv_object_type.
      <fs_coverage>-total = lr_root_node->get_coverage( ce_scv_coverage_type=>statement )->get_total( ) - lv_lcl_total.
      <fs_coverage>-executed = lr_root_node->get_coverage( ce_scv_coverage_type=>statement )->get_executed( ) - lv_lcl_executed.
      <fs_coverage>-notexecuted = lr_root_node->get_coverage( ce_scv_coverage_type=>statement )->get_not_executed( ) - lv_lcl_notexecuted.
*      <fs_coverage>-percentage = lr_root_node->get_coverage( ce_scv_coverage_type=>statement )->get_percentage( ).
      <fs_coverage>-percentage = ( <fs_coverage>-executed / <fs_coverage>-total ) * 100.

**-   Read recursively..
*      calculate_coverage_recursively( lr_root_node ).
*
*      IF lr_coverage_result_provider->gv_object_type EQ 'CLAS'.
*        lv_type = 'GLOB'.
*      ELSE.
*        lv_type = lr_coverage_result_provider->gv_object_type.
*      ENDIF.
*
**-   To ignore any Local Classes, just take the Global Class Coverage
*      READ TABLE mt_coverage INTO DATA(ls_cov)
*        WITH KEY type = lv_type.
*      IF sy-subrc IS INITIAL.
*        APPEND INITIAL LINE TO et_results ASSIGNING FIELD-SYMBOL(<ls_results>).
*        <ls_results>-name =  ls_cov-name.
*        <ls_results>-type = lr_coverage_result_provider->gv_object_type.
*        <ls_results>-total = ls_cov-total.
*        <ls_results>-executed = ls_cov-executed.
*        <ls_results>-notexecuted = ls_cov-notexecuted.
*        <ls_results>-percentage = ls_cov-percentage.
*      ENDIF.

      CLEAR: lv_lcl_name,
             lv_lcl_total,
             lv_lcl_executed,
             lv_lcl_notexecuted.

    ENDLOOP.

    "Return coverage results
    APPEND LINES OF mt_coverage TO et_results.

    CLEAR: mt_coverage.

  ENDMETHOD.

  METHOD get_programs_with_tests.

    DATA: ls_prog_info TYPE if_aunit_prog_info_types=>ty_s_program.

    FIELD-SYMBOLS: <fs_program_key> TYPE sabp_s_tadir_key.

    "Only programs with test cases
    LOOP AT ct_program_keys ASSIGNING <fs_program_key>.

      ls_prog_info = cl_aunit_prog_info=>get_program_info(
        obj_name = <fs_program_key>-obj_name
        obj_type = <fs_program_key>-obj_type
        allow_commit = abap_true
        skip_class_info = abap_true ).

      IF ( abap_true EQ ls_prog_info-has_tests ).
        CONTINUE.
      ELSE.
        CLEAR: <fs_program_key>.
      ENDIF.

    ENDLOOP.

    "Get rid of initial lines
    DELETE ct_program_keys WHERE table_line IS INITIAL.

  ENDMETHOD.

  METHOD calculate_coverage_recursively.

    FIELD-SYMBOLS: <fs_coverage> TYPE zif_code_coverage_api=>gty_s_coverage.

    "Build coverage for all children
    APPEND INITIAL LINE TO mt_coverage ASSIGNING <fs_coverage>.
    <fs_coverage>-name =  cl_aunit_prog_info=>convert_program_name_2_tadir( program_name = lr_node->name )-obj_name.

*    <fs_coverage>-name = lr_node->name.
    <fs_coverage>-type = lr_node->subtype.
    <fs_coverage>-total = lr_node->get_coverage( ce_scv_coverage_type=>statement )->get_total( ).
    <fs_coverage>-executed = lr_node->get_coverage( ce_scv_coverage_type=>statement )->get_executed( ).
    <fs_coverage>-notexecuted = lr_node->get_coverage( ce_scv_coverage_type=>statement )->get_not_executed( ).
*    <fs_coverage>-percentage = | { lr_node->get_coverage( ce_scv_coverage_type=>statement )->get_percentage( ) DECIMALS = 2 }% |.
    <fs_coverage>-percentage = lr_node->get_coverage( ce_scv_coverage_type=>statement )->get_percentage( ).

    IF lr_node->has_children( ).
      DATA: children TYPE if_scv_result_node=>tab,
            child    TYPE REF TO if_scv_result_node.

      children = lr_node->get_children( ).
      LOOP AT children INTO child.
        calculate_coverage_recursively( child ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_coverage IMPLEMENTATION.

  METHOD constructor.

    me->go_coverage_measurement = iv_coverage_measurement.
    me->gv_program_name = iv_program_name.
    me->gv_object_type = iv_object_type.

  ENDMETHOD.

  METHOD if_aucv_cvrg_rslt_provider~build_coverage_result.

    IF ( me->go_coverage_result IS BOUND ).
      result = me->go_coverage_result.
      RETURN.
    ENDIF.

    DATA(cov_factory) = cl_scv_coverage_api=>get_factory( ).

    me->go_coverage_result =
      me->go_coverage_measurement->build_program_result( me->gv_program_name ).

    CLEAR me->go_coverage_measurement.

    result = me->go_coverage_result.

  ENDMETHOD.

ENDCLASS.
