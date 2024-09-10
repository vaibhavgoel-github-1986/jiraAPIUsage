
 " for each message the documentation of an according class attribute
 " is maintained in transaction SE61. Please take care when changing
 " the literals.
 constants:
    begin of c_Rule_Id,
      tolerable         type sci_Errc  value 'Tolerable'  ##no_Text,
      fatal             type sci_Errc  value 'Fatal'      ##no_Text,
      critical          type sci_Errc  value 'Critical'   ##no_Text,
      info              type sci_Errc  value 'Info'       ##no_Text,
      internal          type sci_Errc  value 'Internal'   ##no_Text,
      timeout           type sci_Errc  value 'Timeout'    ##no_Text,
      aborted           type sci_Errc  value 'Aborted'    ##no_Text,
      statistics        type sci_Errc  value 'Statistics' ##no_Text,
    end of c_Rule_Id.

class meta_Data definition.

  public section.
    class-methods:
      get_Messages
        importing i_Test_Name type csequence
        changing  p_Messages type scimessages.

endclass.

class zcl_code_coverage_check definition local friends meta_Data.

class meta_Data implementation.

  method get_Messages.
    data:
      sci_Message type scimessage.

    define declare_Message.
      clear sci_Message.
      sci_Message-test = i_Test_Name.
      sci_Message-code = &1.
      sci_Message-kind = cl_Ci_Test_Root=>&2.
      sci_Message-text = &3.
      sci_Message-pcom = cl_Ci_Test_Root=>c_Exceptn_Imposibl.
      insert sci_Message into table p_Messages.
    end-of-definition.

    declare_Message:
      c_Rule_Id-aborted   c_Note      'No test execution - missing prerequisites (CL_ABAP_UNIT_ASSERT=>ABORT)' ##no_Text,
      c_Rule_Id-internal  c_Note      'No test execution - test driver issue' ##no_Text,
      c_Rule_Id-timeout   c_Warning   'Timeout - duration exceeds upper limit' ##no_Text,
      c_Rule_Id-info      c_Note      'Information' ##no_Text,
      c_Rule_Id-tolerable c_Warning   'Tolerable alert' ##no_Text,
      c_Rule_Id-critical  c_Error     'Critical alert' ##no_Text,
      c_Rule_Id-fatal     c_Error     'Fatal alert' ##no_Text.

  endmethod.

endclass.


class buffer_Service definition.

  public section.
    types:
      begin of ty_Stack_Entry,
        include_Name type string,
        line_Number  type i,
        description type string,
      end of ty_Stack_Entry,
      ty_Stack_Entries type standard table of ty_Stack_Entry.

      class-data:
        fg_Details type string_Table,
        fg_Stack_Entry type ty_Stack_Entry,
        fg_Stack_Entries type ty_Stack_Entries.

    class-methods:
      init_From_Memento importing memento type xstring,
      get_Memento returning value(result) type xstring.

endclass.

class buffer_Service implementation.


  method init_From_Memento.
    try.
      call transformation id
        source   xml memento
        result   stack_Entries = fg_Stack_Entries
                 details = fg_Details.
    catch cx_Transformation_Error.
      clear fg_Stack_Entries.
      clear fg_Details.
    endtry.

  endmethod.


  method get_Memento.
    data: details_Limited type string_Table.
    details_Limited = fg_Details.
    delete details_Limited from 8.
    call transformation id
      options xml_Header = 'no'
      source  stack_Entries = fg_Stack_Entries
              details =     details_Limited
      result  xml !result.

  endmethod.

endclass.


class text_Handle_Factory definition.

  public section.
    interfaces: if_Satc_Ci_Text_Handle_Factory.

endclass.


class text_Handle_Factory implementation.

  method if_Satc_Ci_Text_Handle_Factory~create_Text_Handle.
    data(text_Handle) = new cl_Satc_Ac_Text_Handle_Common( ).
    if ( ci_Result-param1 is not initial ).
      text_Handle->add_String( ci_Result-param1 ).
    endif.

    buffer_Service=>init_From_Memento( ci_Result-detail ).
    text_Handle->add_Strings( buffer_Service=>fg_Details ).
    loop at buffer_Service=>fg_Stack_Entries into buffer_Service=>fg_Stack_Entry.
      text_Handle->add_Code_Location(
        i_Include_Name = buffer_Service=>fg_Stack_Entry-include_Name
        i_Line_Nbr = buffer_Service=>fg_Stack_Entry-line_Number
        i_Description = buffer_Service=>fg_Stack_Entry-description ).
    endloop.

    result = text_Handle.

  endmethod.

endclass.
