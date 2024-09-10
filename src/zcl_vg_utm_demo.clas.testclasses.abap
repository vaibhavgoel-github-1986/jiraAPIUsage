*"* use this source file for your ABAP unit test classes

CLASS ltcl_utm_demo DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      lo_cut TYPE REF TO zcl_vg_utm_demo.  "class under test

    CLASS-DATA:
      lo_osql_test TYPE REF TO if_osql_test_environment.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.

    METHODS: setup.
    METHODS: teardown.
    METHODS: test_get_data_pos FOR TESTING.
    METHODS: test_get_data_neg FOR TESTING.
    METHODS: test_process_logic FOR TESTING.

ENDCLASS.       "ltcl_Test


CLASS ltcl_utm_demo IMPLEMENTATION.

  METHOD class_setup.

    lo_osql_test = cl_osql_test_environment=>create(
                    i_dependency_list = VALUE #( ( 'VBAK' ) ) ).

  ENDMETHOD.


  METHOD class_teardown.

    lo_osql_test->destroy( ).

  ENDMETHOD.


  METHOD setup.

    CREATE OBJECT lo_cut.

  ENDMETHOD.


  METHOD teardown.

    lo_osql_test->clear_doubles( ).

  ENDMETHOD.


  METHOD test_get_data_pos.

    DATA:
      lt_insert_data TYPE zcl_vg_utm_demo=>gtt_vbak.

*- Mock Data
    lt_insert_data = VALUE #( ( vbeln = '5000001161' auart = 'ZGSO' netwr = '10.00' ) ).

    lo_osql_test->insert_test_data(
      EXPORTING
        i_data             = lt_insert_data
    ).

*- Call the Method under Test
    lo_cut->get_data(
      EXPORTING
        iv_vbeln = '5000001161'
      IMPORTING
        et_data  = DATA(lt_data)
      RECEIVING
        rv_error = DATA(lv_error)
    ).

*- Verify Expected Result
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = abap_false
        exp                  = lv_error
        msg                  = 'Data fetching failed'
    ).

  ENDMETHOD.

  METHOD test_get_data_neg.

    DATA:
      lt_insert_data TYPE zcl_vg_utm_demo=>gtt_vbak.

* lt_insert_data = VALUE #( ( vbeln = '5000001161' auart = 'ZGSO' netwr = '10.00' ) ).

*- Blank Data
    lo_osql_test->insert_test_data(
      EXPORTING
        i_data             = lt_insert_data
    ).

*- Call the Method under Test
    lo_cut->get_data(
      EXPORTING
        iv_vbeln = '5000001161'
      IMPORTING
        et_data  = DATA(lt_data)
      RECEIVING
        rv_error = DATA(lv_error)
    ).

*- Verify Expected Result
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = abap_true
        exp                  = lv_error
        msg                  = 'Data fetching failed'
    ).

  ENDMETHOD.

  METHOD test_process_logic.

    DATA:
      lt_insert_data TYPE zcl_vg_utm_demo=>gtt_vbak.

*- Mock Data
    lt_insert_data = VALUE #( ( vbeln = '5000001161' auart = 'ZGSO' netwr = '10.00' ) ).

    lo_osql_test->insert_test_data(
      EXPORTING
        i_data             = lt_insert_data
    ).

    lo_cut->process_logic(
     EXPORTING
       iv_vbeln = '5000001161'
     IMPORTING
       et_processed_data = DATA(lt_processed_data)
    ).

    IF lt_processed_data IS NOT INITIAL.
      DATA(lv_netwr) = lt_processed_data[ 1 ]-netwr.
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = '10.01'
      exp   = lv_netwr
      msg   = 'Net Value is not as expected'
    ).

  ENDMETHOD.

ENDCLASS.
