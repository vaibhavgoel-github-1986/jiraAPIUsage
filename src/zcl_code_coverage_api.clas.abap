class ZCL_CODE_COVERAGE_API definition
  public
  create public .

public section.

  interfaces ZIF_CODE_COVERAGE_API .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CODE_COVERAGE_API IMPLEMENTATION.


  METHOD ZIF_CODE_COVERAGE_API~GET_UNITS_COVERAGE.

    CLEAR: et_coverage,
           et_aunit_results.

    DATA(lo_units) = NEW lcl_units( ).

    lo_units->get_statement_coverage(
      EXPORTING
        it_program_keys  = it_program_keys
       IMPORTING
         et_coverage     = et_coverage ).

    lo_units->get_abapunit_results(
      IMPORTING
        et_aunit_results = et_aunit_results ).

  ENDMETHOD.
ENDCLASS.
