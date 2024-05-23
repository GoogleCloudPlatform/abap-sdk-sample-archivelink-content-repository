*"* local types for public class ZGOOG_CL_CONTENT_REPO_GCS
*"* use this source file for any type declarations (class
*"* definitions, interfaces or data types) you need for method
*"* implementation or private method's signature of
*"* public ZGOOG_CL_CONTENT_REPO_GCS

CLASS cl_abap_char_utilities DEFINITION LOAD.
CLASS cl_http_client DEFINITION LOAD.

TYPES:
  BEGIN OF ltyp_s_error,
    status_code         TYPE i,
    status_text         TYPE string,
    msgid               LIKE sy-msgid,
    msgty               LIKE sy-msgty,
    msgno               LIKE sy-msgno,
    msgv1               LIKE sy-msgv1,
    msgv2               LIKE sy-msgv2,
    msgv3               LIKE sy-msgv3,
    msgv4               LIKE sy-msgv4,
    set_authenticate(1) TYPE c,
  END OF ltyp_s_error.

TYPES:
  BEGIN OF ltyp_s_parameter,
    name  TYPE string,
    value TYPE string,
    usage TYPE i,
  END OF ltyp_s_parameter,

  ltyp_t_parameters TYPE STANDARD TABLE OF ltyp_s_parameter.
