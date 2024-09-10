class ZCL_VG_UTM_3 definition
  public
  create public .

public section.

  types:
    gtt_vbak TYPE STANDARD TABLE OF vbak .

  methods GET_DATA
    importing
      !IV_VBELN type VBELN_VA optional
    exporting
      !ET_DATA type GTT_VBAK
    returning
      value(RV_ERROR) type CHAR1 .
  methods PROCESS_LOGIC
    importing
      !IV_VBELN type VBELN_VA optional
    exporting
      !ET_PROCESSED_DATA type GTT_VBAK .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_VG_UTM_3 IMPLEMENTATION.


  METHOD GET_DATA.

    CLEAR:
     et_data.

    SELECT *
     FROM vbak INTO TABLE et_data.
    IF NOT sy-subrc IS INITIAL.
      rv_error = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD PROCESS_LOGIC.

    CLEAR:
     et_processed_data.

    get_data(
      EXPORTING
        iv_vbeln = '5000001161'
      IMPORTING
        et_data  = DATA(lt_data)
      RECEIVING
        rv_error = DATA(lv_error)
    ).
    IF lv_error EQ abap_true.
      RETURN.
    ENDIF.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
      IF <lfs_data>-auart EQ 'ZGSO'.
        <lfs_data>-netwr = <lfs_data>-netwr + '0.02'.
      ENDIF.
    ENDLOOP.

    et_processed_data = lt_data.

  ENDMETHOD.
ENDCLASS.
