*&---------------------------------------------------------------------*
*& Include          ZUSCM_CODE_COVERAGE_TOP
*&---------------------------------------------------------------------*

TABLES:
  tdevc,
  seoaliases,
  tlibg,
  sscrfields.

TYPES:
  ty_packages TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY,

  BEGIN OF ty_context,
    package          TYPE string,
    program          TYPE string,
    obj_type         TYPE string,
    obj_name         TYPE string,
    test_class       TYPE string,
    test_method      TYPE string,
    adt_resource_uri TYPE string,
  END OF ty_context,

  ty_alert_level TYPE c LENGTH 6,

  BEGIN OF ty_alert,
    context     TYPE ty_context,
    kind        TYPE string,
    description TYPE string,
    level       TYPE ty_alert_level,
    apply_zebra TYPE abap_bool,
  END OF ty_alert,

  ty_alerts TYPE STANDARD TABLE OF ty_alert WITH DEFAULT KEY,

  BEGIN OF ty_test_method,
    name  TYPE string,
    alert TYPE ty_alert,
    BEGIN OF state,
      has_been_started TYPE abap_bool,
      has_been_skipped TYPE abap_bool,
    END OF state,
  END OF ty_test_method,
  ty_methods TYPE STANDARD TABLE OF ty_test_method WITH NON-UNIQUE KEY name,

  BEGIN OF ty_test_class,
    name         TYPE string,
    handle       TYPE REF TO if_aunit_test_class_handle,
    test_methods TYPE ty_methods,
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
  END OF ty_test_class,

  ty_test_classes TYPE STANDARD TABLE OF ty_test_class WITH NON-UNIQUE KEY name,

  BEGIN OF ty_program,
    name             TYPE progname,
    obj_type         TYPE tadir-object,
    obj_name         TYPE tadir-obj_name,
    devclass         TYPE tadir-devclass,
    author           TYPE responsibl,
    adt_resource_uri TYPE string,
    test_classes     TYPE ty_test_classes,
    is_permitted     TYPE abap_bool,
    BEGIN OF state,
      has_been_started TYPE abap_bool,
      has_issue        TYPE abap_bool,
    END OF state,
    created_on       TYPE sydatum,
  END OF ty_program,

  ty_programs TYPE STANDARD TABLE OF ty_program." WITH NON-UNIQUE KEY obj_name obj_type
   "  WITH UNIQUE SORTED KEY sorted COMPONENTS obj_name obj_type.

DATA:
  gv_prog TYPE s_aucv_d_prog_other.

SELECTION-SCREEN BEGIN OF BLOCK objs WITH FRAME TITLE TEXT-000.

  SELECTION-SCREEN BEGIN OF BLOCK header.
    PARAMETERS:
      b_devc TYPE s_aucv_d_select_by_devc RADIOBUTTON GROUP head DEFAULT 'X' USER-COMMAND uc_devc,
      b_obj  TYPE s_aucv_d_select_by_prog RADIOBUTTON GROUP head.
  SELECTION-SCREEN END OF BLOCK header.

* ----------------- via Package -----------------
  SELECTION-SCREEN BEGIN OF BLOCK devc WITH FRAME TITLE TEXT-001.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(26)
        TEXT-dvc FOR FIELD so_devc MODIF ID dvc.
      SELECT-OPTIONS:
         so_devc    FOR tdevc-devclass NO INTERVALS MODIF ID dvc. "default 'willy'
    SELECTION-SCREEN END OF LINE.

    PARAMETERS:
      p_packr TYPE s_aucv_d_with_subdevc AS CHECKBOX DEFAULT 'X' MODIF ID dvc
              USER-COMMAND uc_packr,
      p_excl  TYPE s_aucv_d_exclude_obj AS CHECKBOX DEFAULT ' ' MODIF ID dvc
              USER-COMMAND uc_excl.

    SELECTION-SCREEN BEGIN OF BLOCK excl WITH FRAME TITLE TEXT-007.
      SELECT-OPTIONS:
        so_ndevc FOR tdevc-devclass     NO INTERVALS MODIF ID xc2,
        so_ncl   FOR seoaliases-clsname NO INTERVALS MODIF ID xcl,
        so_nfg   FOR tlibg-area         NO INTERVALS MODIF ID xcl,
        so_npr   FOR gv_prog            NO INTERVALS MODIF ID xcl.
    SELECTION-SCREEN END OF BLOCK excl.

  SELECTION-SCREEN END OF BLOCK devc.

* ----------------- via Object Name -----------------
  SELECTION-SCREEN BEGIN OF BLOCK obj WITH FRAME TITLE TEXT-002.

    DATA:
      p_selcl                TYPE abap_bool,
      p_selfg                TYPE abap_bool,
      p_selprg               TYPE abap_bool,
      g_adt_uri_is_supported TYPE abap_bool.

    SELECT-OPTIONS:
      so_class FOR seoaliases-clsname NO INTERVALS MODIF ID obj,
      so_fugr FOR tlibg-area          NO INTERVALS MODIF ID obj,
      so_prog FOR gv_prog             NO INTERVALS MODIF ID obj.

  SELECTION-SCREEN END OF BLOCK obj.

  SELECTION-SCREEN BEGIN OF BLOCK db WITH FRAME TITLE TEXT-009.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(26)
        TEXT-upd FOR FIELD cb_upd.
      PARAMETERS: cb_upd AS CHECKBOX DEFAULT abap_true.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(26)
        TEXT-prd FOR FIELD cb_prod.
      PARAMETERS: cb_prod AS CHECKBOX DEFAULT abap_true.
    SELECTION-SCREEN END OF LINE.

*    SELECTION-SCREEN BEGIN OF LINE.
*      SELECTION-SCREEN COMMENT 1(26)
*        TEXT-bqt FOR FIELD cb_bq.
*      PARAMETERS: cb_bq AS CHECKBOX DEFAULT abap_false.
*    SELECTION-SCREEN END OF LINE.
*
*    SELECTION-SCREEN BEGIN OF LINE.
*      SELECTION-SCREEN COMMENT 1(26)
*        TEXT-adh FOR FIELD cb_adhoc.
*      PARAMETERS: cb_adhoc AS CHECKBOX DEFAULT abap_false.
*    SELECTION-SCREEN END OF LINE.
*
*    SELECTION-SCREEN BEGIN OF LINE.
*      SELECTION-SCREEN COMMENT 1(26)
*        TEXT-mdg FOR FIELD cb_no_md.
*      PARAMETERS: cb_no_md AS CHECKBOX DEFAULT abap_false.
*    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK db.

SELECTION-SCREEN END OF BLOCK objs.

*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01 NO INTERVALS.
*  PARAMETERS: p_rfc TYPE string DEFAULT 'RD7CLNT200' OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_fetch_data DEFINITION.

  PUBLIC SECTION.

    DATA:
      gt_programs TYPE ty_programs.

    METHODS:
      select_objects,
      select_objects_by_pkg,
      select_objects_by_type,
      select_objects_by_pkg_and_type
        IMPORTING i_packages TYPE ty_packages,
      settle_objects_after_selection.

ENDCLASS.

CLASS lcl_fetch_data IMPLEMENTATION.

  METHOD select_objects_by_pkg.

    DATA:
      package           TYPE devclass,
      selected_packages TYPE ty_packages,
      expanded_packages TYPE cl_pak_package_queries=>tt_subpackage_info,
      overall_packages  TYPE ty_packages.

    SELECT devclass FROM tdevc
      INTO TABLE selected_packages
      WHERE
        devclass IN so_devc.                            "#EC CI_GENBUFF

    IF abap_false EQ  p_packr. " no sub packages.
      overall_packages = selected_packages.
    ELSE.
      overall_packages = selected_packages.
      LOOP AT selected_packages INTO package.
        CLEAR expanded_packages.
        cl_pak_package_queries=>get_all_subpackages(
          EXPORTING
            im_package                    = package
          IMPORTING
            et_subpackages                = expanded_packages
          EXCEPTIONS
            OTHERS                        = 7 ).
        IF ( abap_true EQ p_excl AND so_ndevc IS NOT INITIAL ).
          DELETE expanded_packages
            WHERE table_line IN so_ndevc.               "#EC CI_SORTSEQ
        ENDIF.
        APPEND LINES OF expanded_packages TO overall_packages.
      ENDLOOP.
    ENDIF.

    SORT overall_packages.
    DELETE ADJACENT DUPLICATES FROM overall_packages.
    IF ( overall_packages IS NOT INITIAL ).
      select_objects_by_pkg_and_type( overall_packages ).
    ENDIF.

  ENDMETHOD.


  METHOD select_objects_by_pkg_and_type.

    DATA:
      tab_rng_obj_name TYPE RANGE OF tadir-obj_name,
      package          TYPE devclass.

    ASSERT i_packages IS NOT INITIAL.

    IF abap_true EQ p_excl AND so_ncl IS NOT INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass
        author
        created_on
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->gt_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'CLAS' AND
          obj_name NOT IN so_ncl[] AND
          delflag EQ abap_false.

    ELSE.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass
        author
        created_on
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->gt_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'CLAS' AND
          delflag EQ abap_false.
    ENDIF.

    IF abap_true EQ p_excl AND so_nfg IS NOT INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass
        author
        created_on
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->gt_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'FUGR' AND
          obj_name NOT IN so_nfg[] AND
          delflag EQ abap_false.
    ELSE.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass
        author
        created_on
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->gt_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'FUGR' AND
          delflag EQ abap_false.
    ENDIF.

    IF abap_true EQ p_excl AND so_npr IS NOT INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass
        author
        created_on
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->gt_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'PROG' AND
          obj_name NOT IN so_npr[] AND
          delflag EQ abap_false.
    ELSE.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass
        author
        created_on
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->gt_programs
        FOR ALL ENTRIES IN i_packages
        WHERE
          devclass = i_packages-table_line AND
          pgmid    = 'R3TR' AND
          object   = 'PROG' AND
          delflag EQ abap_false.
    ENDIF.

    DELETE me->gt_programs WHERE obj_type = 'CLAS' AND obj_name CS '='.

  ENDMETHOD.

  METHOD select_objects_by_type.

    IF NOT p_selcl IS INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass
        author
        created_on
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->gt_programs
        WHERE
          pgmid = 'R3TR' AND
          object = 'CLAS' AND
          obj_name IN so_class AND
          delflag EQ abap_false.
    ENDIF.

    IF NOT p_selprg IS INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass
        author
        created_on
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->gt_programs
        WHERE
          pgmid = 'R3TR' AND
          object = 'PROG' AND
          obj_name IN so_prog AND
          delflag EQ abap_false.
    ENDIF.

    IF NOT p_selfg IS INITIAL.
      SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object AS obj_type                              "#EC CI_GENBUFF
        obj_name
        devclass
        author
        created_on
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE me->gt_programs
        WHERE
          pgmid = 'R3TR' AND
          object = 'FUGR' AND
          obj_name  IN so_fugr AND
          delflag EQ abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD settle_objects_after_selection.

    IF ( p_selcl IS INITIAL ).
      DELETE me->gt_programs
        WHERE
          obj_type = 'CLAS'.
    ELSE.
      DELETE me->gt_programs
        WHERE
          obj_type = 'CLAS' AND
          obj_name NOT IN so_class.
    ENDIF.

    IF ( p_selprg IS INITIAL ).
      DELETE me->gt_programs
        WHERE
          obj_type = 'PROG'.
    ELSE.
      DELETE me->gt_programs
        WHERE
          obj_type = 'PROG' AND
          obj_name NOT IN so_prog.
    ENDIF.

    IF ( p_selfg IS INITIAL ).
      DELETE me->gt_programs
        WHERE
          obj_type = 'FUGR'.
    ELSE.
      DELETE me->gt_programs
        WHERE
          obj_type = 'FUGR' AND
          obj_name NOT IN so_fugr.
    ENDIF.

  ENDMETHOD.

  METHOD select_objects.

    DATA:
      type_of_include TYPE trdir-subc.

    FIELD-SYMBOLS:
      <program>  TYPE ty_program.

    CONSTANTS:
      BEGIN OF c_include_type,
        stand_alone TYPE trdir-subc  VALUE 'I',
      END OF c_include_type.

    IF b_devc = abap_true.
      select_objects_by_pkg( ).
    ELSEIF b_obj = abap_true.
      select_objects_by_type( ).
    ENDIF.

*- Fetch the list of Objects maintained in the exclusion list
*    SELECT *
*      FROM zscmd_cov_except
*     INTO TABLE @DATA(lt_exclude).
*    IF sy-subrc IS INITIAL.
*      p_excl = abap_true.   "Set the Parameter as true
*
*      LOOP AT lt_exclude ASSIGNING FIELD-SYMBOL(<ls_exclude>).
*        IF <ls_exclude>-devclass IS INITIAL OR
*           <ls_exclude>-obj_name IS INITIAL OR
*          <ls_exclude>-obj_type IS INITIAL.
*          CONTINUE.
*        ENDIF.
*
*        CASE <ls_exclude>-obj_type.
*          WHEN 'CLAS'.
*            APPEND INITIAL LINE TO so_ncl[] ASSIGNING FIELD-SYMBOL(<ls_ncl>).
*            <ls_ncl>-sign = 'I'. <ls_ncl>-option = 'EQ'.
*            <ls_ncl>-low = <ls_exclude>-obj_name.
*            UNASSIGN <ls_ncl>.
*
*          WHEN 'PROG'.
*            APPEND INITIAL LINE TO so_npr[] ASSIGNING FIELD-SYMBOL(<ls_npr>).
*            <ls_npr>-sign = 'I'. <ls_npr>-option = 'EQ'.
*            <ls_npr>-low = <ls_exclude>-obj_name.
*            UNASSIGN <ls_npr>.
*
*          WHEN 'FUGR'.
*            APPEND INITIAL LINE TO so_nfg[] ASSIGNING FIELD-SYMBOL(<ls_nfg>).
*            <ls_nfg>-sign = 'I'. <ls_nfg>-option = 'EQ'.
*            <ls_nfg>-low = <ls_exclude>-obj_name.
*            UNASSIGN <ls_nfg>.
*        ENDCASE.
*      ENDLOOP.
*    ENDIF.

    " excluded
    IF b_devc = abap_true AND p_excl = abap_true.
      IF NOT so_ndevc[] IS INITIAL.
        DELETE me->gt_programs
           WHERE devclass IN so_ndevc.
      ENDIF.
      IF NOT so_ncl[] IS INITIAL.
        DELETE me->gt_programs
           WHERE obj_type = 'CLAS' AND obj_name IN so_ncl.
      ENDIF.
      IF NOT so_nfg[] IS INITIAL.
        DELETE me->gt_programs
           WHERE obj_type = 'FUGR' AND obj_name IN so_nfg.
      ENDIF.
      IF NOT so_npr[] IS INITIAL.
        DELETE me->gt_programs
           WHERE obj_type = 'PROG' AND obj_name IN so_npr.
      ENDIF.
    ENDIF.

    LOOP AT me->gt_programs ASSIGNING <program>.

      IF ( <program>-obj_type = 'PROG' ).
        " no includes
        SELECT SINGLE subc FROM trdir
          INTO type_of_include
          WHERE
            name = <program>-obj_name AND
            subc <> c_include_type-stand_alone.
        IF ( 0 NE sy-subrc ).
          CONTINUE.
        ENDIF.
      ENDIF.

      <program>-name  = cl_aunit_prog_info=>tadir_to_progname(
         obj_type = <program>-obj_type
         obj_name = <program>-obj_name ).

    ENDLOOP.

    DELETE me->gt_programs WHERE name IS INITIAL.

    SORT me->gt_programs BY devclass obj_type obj_name.

    LOOP AT me->gt_programs ASSIGNING FIELD-SYMBOL(<ls_progams>).
      CASE <ls_progams>-obj_type.
        WHEN 'CLAS'.
          DATA(lv_class_name) = <ls_progams>-obj_name.

          SPLIT lv_class_name AT '_' INTO TABLE DATA(lt_split).
          IF lt_split IS NOT INITIAL.
            DESCRIBE TABLE lt_split LINES DATA(lv_count).
            IF lt_split[ lv_count ] EQ 'DPC'
            OR lt_split[ lv_count ] EQ 'MPC'.
              CLEAR <ls_progams>.
            ENDIF.
          ENDIF.

        WHEN 'PROG'.
          IF <ls_progams>-obj_name+0(4) EQ 'SAPM'.
            CLEAR <ls_progams>.
          ENDIF.
      ENDCASE.

      CLEAR: lv_class_name, lt_split, lv_count.
    ENDLOOP.

    DELETE me->gt_programs WHERE table_line IS INITIAL.

  ENDMETHOD.

ENDCLASS.
