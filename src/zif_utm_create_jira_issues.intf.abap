INTERFACE zif_utm_create_jira_issues
  PUBLIC .

  TYPES:
    BEGIN OF ts_context,
      package          TYPE string,
      program          TYPE string,
      obj_type         TYPE string,
      obj_name         TYPE string,
      test_class       TYPE string,
      test_method      TYPE string,
      adt_resource_uri TYPE string,
    END OF ts_context,

    BEGIN OF ts_alert,
      context     TYPE ts_context,
      kind        TYPE string,
      description TYPE string,
      level       TYPE char6,
      apply_zebra TYPE abap_bool,
    END OF ts_alert,

    tt_alerts TYPE STANDARD TABLE OF ts_alert WITH DEFAULT KEY,

    BEGIN OF ts_test_method,
      name  TYPE string,
      alert TYPE ts_alert,
      BEGIN OF state,
        has_been_started TYPE abap_bool,
        has_been_skipped TYPE abap_bool,
      END OF state,
    END OF ts_test_method,
    tt_methods TYPE STANDARD TABLE OF ts_test_method WITH NON-UNIQUE KEY name,

    BEGIN OF ts_test_class,
      name         TYPE string,
      handle       TYPE REF TO if_aunit_test_class_handle,
      test_methods TYPE tt_methods,
      BEGIN OF state,
        has_been_started           TYPE abap_bool,
        BEGIN OF issue,
          has_been_skipped TYPE abap_bool,
          has_rt_failure   TYPE abap_bool,
          has_timeout      TYPE abap_bool,
          has_failure      TYPE abap_bool,
        END OF issue,
        count_exec_methods         TYPE i,
        count_skipped_methods      TYPE i,
        count_skipped_over_methods TYPE i,
        count_no_permission        TYPE i,
      END OF state,
    END OF ts_test_class,

    tt_test_classes TYPE STANDARD TABLE OF ts_test_class WITH NON-UNIQUE KEY name,

    BEGIN OF ts_object,
      name             TYPE progname,
      obj_type         TYPE tadir-object,
      obj_name         TYPE tadir-obj_name,
      package          TYPE tadir-devclass,
      adt_resource_uri TYPE string,
      test_classes     TYPE tt_test_classes,
      is_permitted     TYPE abap_bool,
      BEGIN OF state,
        has_been_started TYPE abap_bool,
        has_issue        TYPE abap_bool,
      END OF state,
    END OF ts_object,

    tt_objects TYPE STANDARD TABLE OF ts_object WITH NON-UNIQUE KEY obj_name obj_type
      WITH UNIQUE SORTED KEY sorted COMPONENTS obj_name obj_type,

    tt_logs    TYPE STANDARD TABLE OF zdt_utm_logs.

  METHODS:
    create_jira_issues
      IMPORTING
        VALUE(it_objects) TYPE tt_objects
      EXPORTING
        et_logs           TYPE tt_logs,

    dedup_check
      IMPORTING
        VALUE(is_object) TYPE ts_object
      EXPORTING
        et_logs          TYPE tt_logs
      RETURNING
        VALUE(rv_skip)   TYPE abap_bool,

    bulk_upd_issue_status
      RAISING
        zcx_jira_exceptions,

    print_logs
      IMPORTING
        VALUE(it_logs) TYPE tt_logs.

ENDINTERFACE.
