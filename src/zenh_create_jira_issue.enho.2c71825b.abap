"Name: \PR:RS_AUCV_RUNNER\TY:EMAIL_LISTENER\ME:SEND_EMAIL\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_CREATE_JIRA_ISSUE.

  DATA: lo_create_issues TYPE REF TO zif_utm_create_jira_issues.

*- Create Object for the API
  lo_create_issues = NEW zcl_utm_create_jira_issues( ).

  TRY.
*-   Bulk Update of Issues Status
      lo_create_issues->bulk_upd_issue_status( ).
    CATCH zcx_jira_exceptions INTO DATA(lo_exception).
      WRITE:/ lo_exception->get_text( ).
      RETURN.
  ENDTRY.

*- Create Jira Issues
  lo_create_issues->create_jira_issues(
    EXPORTING
      it_objects = me->f_programs
     IMPORTING
       et_logs    = DATA(lt_logs) ).

*- For Spool Logs
  WRITE:/ |Create Jira Issues Logs:|.
  lo_create_issues->print_logs( lt_logs ).

  RETURN.  "No need to trigger emails

ENDENHANCEMENT.
