class ZCL_CUSTOM_ATC_TEMPLATE definition
  public
  inheriting from CL_CI_CATEGORY_ROOT
  create public .

public section.

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CUSTOM_ATC_TEMPLATE IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    me->description = 'Custom ATC Checks'.
    me->category = 'CL_CI_CATEGORY_TOP'.
    me->position = '999'.

  ENDMETHOD.
ENDCLASS.
