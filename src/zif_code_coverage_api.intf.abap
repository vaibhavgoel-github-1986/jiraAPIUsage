interface ZIF_CODE_COVERAGE_API
  public .


  types:
    BEGIN OF gty_s_coverage,
      type        TYPE trobjtype,
      name        TYPE sobj_name,
      total       TYPE cva_coverage_total,
      executed    TYPE cva_coverage_executed,
      notexecuted TYPE zscm_coverage_nonexec,
      percentage  TYPE zscm_coverage_percentage,
    END OF gty_s_coverage .
  types:
    BEGIN OF gty_s_units,
      type             TYPE trobjtype,
      name             TYPE sobj_name,
      total_test_cases TYPE i,
      failure_cnt      TYPE i,
    END OF gty_s_units .
  types:
    gty_t_coverage TYPE STANDARD TABLE OF gty_s_coverage
       WITH NON-UNIQUE KEY name .
  types:
    gty_t_units    TYPE STANDARD TABLE OF gty_s_units
       WITH EMPTY KEY .

  methods GET_UNITS_COVERAGE
    importing
      !IT_PROGRAM_KEYS type SABP_T_TADIR_KEYS
    exporting
      !ET_COVERAGE type GTY_T_COVERAGE
      !ET_AUNIT_RESULTS type GTY_T_UNITS .
endinterface.
