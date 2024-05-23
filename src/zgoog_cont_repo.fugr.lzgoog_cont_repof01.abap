*----------------------------------------------------------------------*
***INCLUDE LZGOOG_CONT_REPOF01.
*----------------------------------------------------------------------*

FORM f_new_entry.

  IF zgoog_cont_repo-keyname IS INITIAL.
    MESSAGE 'Please provide a valid Google Client Key Name' TYPE 'E'.
  ELSEIF zgoog_cont_repo-bucket IS INITIAL.
    MESSAGE 'Please provide a valid Storage bucket name' TYPE 'E'.
  ENDIF.

ENDFORM.
