CLASS zcl_code_coverage_check DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_program
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    METHODS constructor .

    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA f_text_tool TYPE REF TO if_aunit_text_description .
    DATA f_count_all_programs TYPE int4 VALUE 0 ##NO_TEXT.
    DATA f_count_tested_programs TYPE int4 VALUE 0 ##NO_TEXT.
    DATA f_duration_setting TYPE saunit_s_duration_setting .
    DATA f_duration_check_option TYPE saunit_d_duration_check_option .
    DATA f_allowed_risk_level TYPE saunit_d_allowed_risk_level .
    DATA f_allowed_duration_category TYPE saunit_d_allowed_rt_duration .
    DATA f_effective_risk_level TYPE saunit_d_allowed_risk_level .
    DATA f_effective_duration_setting TYPE saunit_s_duration_setting .
    CONSTANTS:
      BEGIN OF c_duration_option,
        no_limit      TYPE int1 VALUE 1,
        default_limit TYPE int1 VALUE 2,
        custom_limit  TYPE int1 VALUE 3,
      END OF c_duration_option .
    DATA f_risk_categories TYPE saunit_t_categories .
    DATA f_duration_categories TYPE saunit_t_categories .
    DATA f_min_code_coverage TYPE int3 VALUE 90 ##NO_TEXT.
ENDCLASS.



CLASS ZCL_CODE_COVERAGE_CHECK IMPLEMENTATION.


  METHOD constructor.

    DATA:
      sci_message TYPE scimessage.

    super->constructor( ).

    me->description = 'Code Coverage Check'.                "#EC NOTEXT
    me->category =    'ZCL_CUSTOM_ATC_TEMPLATE'.       "#EC NOTEXT
    me->version =     '0001'.                               "#EC NOTEXT
    me->position =    1.

    me->has_attributes = abap_true.                               " has dialog
    me->has_documentation = abap_true.                            " has docu
    me->attributes_ok  = abap_true.                               " dialog is optional
    me->has_display_consolidation = abap_false.                    " call consolidate_For_Display
    me->uses_checksum = abap_true.
    me->check_scope_enabled = abap_false.

    add_obj_type( 'CLAS' ).
    add_obj_type( 'FUGR' ).
    add_obj_type( 'PROG' ).

    me->f_text_tool = NEW cl_aunit_factory( )->get_text_converter(
                            language = 'E' ).

    DEFINE declare_message.
      CLEAR sci_message.
      sci_message-test = me->myname.
      sci_message-code = &1.
      sci_message-kind = cl_ci_test_root=>&2.
      sci_message-text = &3.
      sci_message-pcom = cl_ci_test_root=>c_exceptn_imposibl.
      INSERT sci_message INTO TABLE me->scimessages[].
    end-of-definition.

    DATA(lv_text) = |Code Coverage is &1%|.

    declare_message:
      'Critical'  c_note lv_text. ##no_Text.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.
    DATA:
      max_risk_level TYPE saunit_d_allowed_risk_level,
      do_cancel      TYPE abap_bool,
      txt_message    TYPE c LENGTH 120,
      scr_attribs    TYPE sci_atttab,
      scr_attrib     LIKE LINE OF scr_attribs.

    DEFINE mac_fill_attrib.
      CLEAR scr_attrib.
      GET REFERENCE OF &1 INTO scr_attrib-ref.
      scr_attrib-text = &2.
      scr_attrib-kind = &3.
      scr_attrib-obligatory = &4.
      INSERT scr_attrib INTO TABLE scr_attribs[].
    end-of-definition.

    mac_fill_attrib  ''                       'Minimum Required Code Coverage'   'G' ''.
    mac_fill_attrib  me->f_min_code_coverage  'Code Coverage %'                    ''  ''.

    " perform query dialog
    CLEAR txt_message.

    do_cancel = cl_ci_query_attributes=>generic(
      p_name       = myname
      p_title      = TEXT-001
      p_attributes = scr_attribs[]
      p_display    = p_display
      p_message    = txt_message ).

    IF ( abap_true EQ do_cancel ).
      RETURN.
    ENDIF.

    me->attributes_ok = abap_true.

  ENDMETHOD.


  METHOD run.

    DATA:
      checksumcalculator TYPE REF TO cl_saunit_legacy_identity,
      checksum           TYPE i.

    " precheck for correct program type & name
    CASE object_type.
      WHEN 'CLAS' OR 'FUGR' OR 'PROG'.
        " okay.
      WHEN OTHERS.
        RETURN.  " leave method
    ENDCASE.

    IF ( object_name IS INITIAL )." OR me->f_risk_categories IS INITIAL ).
      RETURN.
    ENDIF.

    ADD 1 TO me->f_count_all_programs.

    " check whether program contains tests
    DATA(test_classes) = NEW cl_aunit_factory( )->get_test_class_handles(
                              obj_name = object_name obj_type = object_type ).
    IF ( test_classes IS INITIAL ).
      RETURN.
    ENDIF.

    " do the test - retry if there a temporary problems - block further
    " execution upon permanent errors
    ADD 1 TO me->f_count_tested_programs.

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

      IF lv_percent < me->f_min_code_coverage.

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
           p_code =         'Critical'
           p_param_1 =      CONV #( lv_percent ) ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
       min_code_coverage =          me->f_min_code_coverage
    TO DATA BUFFER p_attributes
    COMPRESSION ON.

  ENDMETHOD.


  METHOD put_attributes.

    IF ( p_attributes IS NOT INITIAL ).
      IMPORT
        min_code_coverage =          me->f_min_code_coverage
      FROM DATA BUFFER p_attributes.
    ELSE.
      me->f_min_code_coverage = 90.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
