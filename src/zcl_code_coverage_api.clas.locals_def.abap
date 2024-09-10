
CLASS lcl_coverage DEFINITION DEFERRED.

CLASS lcl_units DEFINITION.

  PUBLIC SECTION.
    TYPES:
      ty_icon_long TYPE string,
      ty_e_degree  TYPE int1.

    TYPES:
      BEGIN OF ty_s_alert_total,
        cnt_fatal     TYPE i,       "total of fatal alerts"
        cnt_critical  TYPE i,       "total of critical alerts"
        cnt_tolerable TYPE i,       "total of tolerable alerts"
        cnt_info      TYPE i,       "total of info items"

        "counters per kind"
        cnt_failure   TYPE i,       "total of failed assertions"
        cnt_warning   TYPE i,       "total of warnings"
        cnt_error     TYPE i,       "total of runtime errors"
        cnt_shortdump TYPE i,       "total of short dumps"
        cnt_alert     TYPE i,       "total of any problem"
        cnt_extended  TYPE i,

        degree        TYPE ty_e_degree,
        program_ndx   TYPE i,
        class_ndx     TYPE i,
        method_ndx    TYPE i,

        program       TYPE string,
        class         TYPE string,
        method        TYPE string,

      END OF ty_s_alert_total,

      ty_t_alert_totals TYPE STANDARD TABLE OF ty_s_alert_total
        WITH NON-UNIQUE KEY program_ndx class_ndx method_ndx.

    TYPES:
      BEGIN OF ty_s_units,
        program       TYPE string,
        class         TYPE string,
        method        TYPE string,
        cnt_failure   TYPE string,
        cnt_warning   TYPE string,
        cnt_error     TYPE string,
        cnt_shortdump TYPE string,
        cnt_alert     TYPE string,
      END OF ty_S_units,

      ty_t_units TYPE STANDARD TABLE OF ty_s_units
       WITH EMPTY KEY.

    TYPES:
      ty_s_task_data           TYPE if_saunit_internal_result_type=>ty_s_task,
      ty_s_prog_data           TYPE if_saunit_internal_result_type=>ty_s_program,
      ty_s_clas_data           TYPE if_saunit_internal_result_type=>ty_s_class,
      ty_s_meth_data           TYPE if_saunit_internal_result_type=>ty_s_method,
      ty_t_prog_data           TYPE if_saunit_internal_result_type=>ty_t_programs,
      ty_t_clas_data           TYPE if_saunit_internal_result_type=>ty_t_classes,
      ty_t_meth_data           TYPE if_saunit_internal_result_type=>ty_t_methods,
      ty_s_alert_data          TYPE if_saunit_internal_result_type=>ty_s_alert,
      ty_s_alerts_by_index     TYPE if_saunit_internal_result_type=>ty_s_alerts_by_index,
      ty_s_extd_infos_by_index TYPE if_saunit_internal_result_type=>ty_s_extd_infos_by_index.

    CONSTANTS:
      BEGIN OF gc_info_degree,
        initial      TYPE ty_e_degree VALUE 0,
        task         TYPE ty_e_degree VALUE 1,
        task_only    TYPE ty_e_degree VALUE 11,
        program      TYPE ty_e_degree VALUE 2,
        program_only TYPE ty_e_degree VALUE 21,
        class        TYPE ty_e_degree VALUE 4,
        class_only   TYPE ty_e_degree VALUE 41,
        method       TYPE ty_e_degree VALUE 6,
        other        TYPE ty_e_degree VALUE 8,
      END OF gc_info_degree.

    CONSTANTS:
      BEGIN OF gc_alert_kind,
        error           TYPE saunit_d_alert_kind
                VALUE if_saunit_internal_rt_v3=>c_alert_kind-error,
        failure         TYPE saunit_d_alert_kind
                VALUE if_saunit_internal_rt_v3=>c_alert_kind-failure,
        warning         TYPE saunit_d_alert_kind
                 VALUE if_saunit_internal_rt_v3=>c_alert_kind-warning,
        shortdump       TYPE saunit_d_alert_kind
                  VALUE if_saunit_internal_rt_v3=>c_alert_kind-shortdump,
        execution_event TYPE saunit_d_alert_kind
                         VALUE if_saunit_internal_rt_v3=>c_alert_kind-execution_event,
        extended_info   TYPE saunit_d_alert_kind
                     VALUE if_saunit_internal_rt_v3=>c_alert_kind-extended_info,
      END OF gc_alert_kind.

    CONSTANTS:
      BEGIN OF gc_alert_level,
        critical    TYPE saunit_d_alert_level
                 VALUE if_saunit_internal_rt_v3=>c_alert_level-critical,
        fatal       TYPE saunit_d_alert_level
                 VALUE if_saunit_internal_rt_v3=>c_alert_level-fatal,
        tolerable   TYPE saunit_d_alert_level
                 VALUE if_saunit_internal_rt_v3=>c_alert_level-tolerable,
        information TYPE saunit_d_alert_level
                 VALUE if_saunit_internal_rt_v3=>c_alert_level-information,
      END OF gc_alert_level.

    DATA: mt_keys TYPE sabp_t_tadir_keys.

    DATA: mt_coverage TYPE zif_code_coverage_api=>gty_t_coverage.
    DATA: mt_coverage_result_provider TYPE TABLE OF REF TO lcl_coverage.

    DATA: mr_aunit_result TYPE REF TO if_saunit_internal_result.

    METHODS:
      get_programs_with_tests
        CHANGING
          ct_program_keys TYPE sabp_t_tadir_keys,

      get_statement_coverage
        IMPORTING
          iv_with_coverage TYPE abap_bool DEFAULT abap_True
          it_program_keys  TYPE sabp_t_tadir_keys
        EXPORTING
          et_coverage      TYPE zif_code_coverage_api=>gty_t_coverage,

      measure_coverage
        IMPORTING
                  it_program_keys         TYPE sabp_t_tadir_keys
                  ir_coverage_measurement TYPE REF TO if_scv_measurement
        RAISING   cx_scv_execution_error,

      calculate_coverage_recursively
        IMPORTING
          lr_node TYPE REF TO if_scv_result_node,

      get_abapunit_results
        EXPORTING
          et_aunit_results TYPE zif_code_coverage_api=>gty_t_units,

      get_coverage_results
        EXPORTING
          et_results TYPE zif_code_coverage_api=>gty_t_coverage.

ENDCLASS.

CLASS lcl_coverage DEFINITION.

  PUBLIC SECTION.

    INTERFACES if_aucv_cvrg_rslt_provider.

    METHODS:
      constructor
        IMPORTING
          iv_coverage_measurement TYPE REF TO if_scv_measurement
          iv_program_name         TYPE progname
          iv_object_type          TYPE   trobjtype.

    DATA:
      go_coverage_measurement TYPE REF TO if_scv_measurement,
      gv_program_name         TYPE progname,
      gv_object_type          TYPE sobj_name,
      go_coverage_result      TYPE REF TO if_scv_result.

ENDCLASS.
