"Name: \TY:CL_SAUNIT_LEGACY_CI_CHECK\ME:RUN\SE:END\EI
ENHANCEMENT 0 ZENH_TEST_IMPL.

    DATA:
      checksumcalculator TYPE REF TO cl_saunit_legacy_identity,
      checksum           TYPE i.

*- Call the Code Coverage API
    DATA(lo_coverage_api) = NEW zcl_code_coverage_api( ).

    lo_coverage_api->zif_code_coverage_api~get_units_coverage(
      EXPORTING
        it_program_keys  = VALUE #( ( obj_name = object_name obj_type = object_type ) )
      IMPORTING
        et_coverage      = DATA(lt_coverage)
        et_aunit_results = DATA(lt_results)
    ).

    IF lt_coverage IS INITIAL.
      RETURN.
    ELSE.
      DATA(lv_percent) = lt_coverage[ 1 ]-percentage.

      IF lv_percent < 90.

        checksumcalculator = cl_saunit_legacy_identity=>get_calculator( ).

        checksum = checksumcalculator->calculate_checksum(
                     EXPORTING
                       i_test_class_name   = test_classes[ 1 ]->get_absolute_class_name( )
                       i_header            = VALUE #( id = 'CO2'
                                    params = VALUE #( ( |Code Coverage % is equal to { lv_percent }| ) ) ) ).

*-     Error
        inform(
           p_sub_obj_type = 'PROG'
           p_sub_obj_name = test_classes[ 1 ]->get_program_name( )
           p_checksum_1 =   checksum
           p_line =         1
           p_kind =         c_note
           p_test =         me->myname
           p_code =         c_Rule_Id-internal
           p_param_1 =      |Code Coverage is only { lv_percent }%| ).
      ENDIF.
    ENDIF.

ENDENHANCEMENT.
