**********************************************************************
*  Copyright 2024 Google LLC                                         *
*                                                                    *
*  Licensed under the Apache License, Version 2.0 (the "License");   *
*  you may not use this file except in compliance with the License.  *
*  You may obtain a copy of the License at                           *
*      https://www.apache.org/licenses/LICENSE-2.0                   *
*  Unless required by applicable law or agreed to in writing,        *
*  software distributed under the License is distributed on an       *
*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      *
*  either express or implied.                                        *
*  See the License for the specific language governing permissions   *
*  and limitations under the License.                                *
**********************************************************************
CLASS zgoog_cl_content_repo_gcs DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS scmst .

    INTERFACES if_http_extension .
private section.

  types:
*"* private components of class ZGOOG_CL_CONTENT_REPO_GCS
*"* do not include other source files here!!!
    BEGIN OF gtyp_s_comp_data,
        comp_id   TYPE string,
        mime_type TYPE string,
        data      TYPE xstring,
      END OF gtyp_s_comp_data .
  types GTYP_S_REPO_CONFIG type ZGOOG_CONT_REPO .
  types:
    gtyp_t_attr_result TYPE STANDARD TABLE OF scms_reatr WITH DEFAULT KEY .

  data GV_DOCPROT type STRING .
  data GV_NL type STRING .
  data GV_SP type STRING .
  data GV_MANDT type SYMANDT .
  data GV_BODY type STRING .
  data GS_ERROR type LTYP_S_ERROR .
  data GO_SERVER type ref to IF_HTTP_SERVER .
  data GV_MODE type STRING .
  data GV_COMMAND type STRING .
  data GT_PARAMETERS type LTYP_T_PARAMETERS .
  data GV_CONTREP type STRING .
  data GV_CREP_TYPE type SCMS_CRTYP .
  data GS_REPO_CONFIG type GTYP_S_REPO_CONFIG .
  constants C_PARAMETER_UNKNOWN type I value 0 ##NO_TEXT.
  constants C_PARAMETER_USED type I value 1 ##NO_TEXT.
  data GV_PARAMETER_SEC_USED type I value 2 ##NO_TEXT.
  constants C_PARAMETER_MISSING type I value 3 ##NO_TEXT.
  class-data:
    gv_cd_mode TYPE c LENGTH 1 value ' ' ##NO_TEXT.

  methods LOAD_GOOG_REPO_CONFIG
    importing
      !IV_CONTREP type SAEARCHIVI .
  methods INIT .
  methods PARSE_URI .
  methods URL_HEX_DECODE
    importing
      !IV_VALUE type STRING
    returning
      value(RV_RESULT) type STRING .
  methods BODY_ADD_FIELD
    importing
      !IV_NAME type STRING
      !IV_VALUE type STRING .
  methods BODY_PUT_FIELD
    importing
      !IV_NAME type STRING
      !IV_VALUE type STRING .
  methods SYS_ERROR_SET .
  methods REPORT_ERROR .
  methods GET_PARAMETER
    importing
      !IV_NAME type STRING
      !IV_DEFAULT type STRING optional
      !IV_MANDATORY type C default SPACE
      !IV_RAW_MODE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_VALUE) type STRING .
  methods GET_PARAMETER_B
    importing
      !IV_NAME type STRING
      !IV_DEFAULT type I optional
      !IV_MANDATORY type C default SPACE
    returning
      value(RV_VALUE) type SY-DATAR .
  methods GET_PARAMETER_I
    importing
      !IV_NAME type STRING
      !IV_DEFAULT type I optional
      !IV_MANDATORY type C default SPACE
    returning
      value(RV_VALUE) type I .
  methods GET_PARAMETER_C
    importing
      !IV_NAME type STRING
      !IV_DEFAULT type STRING optional
      !IV_MANDATORY type C default SPACE
      !IV_RAW_MODE type ABAP_BOOL default ABAP_FALSE
    exporting
      !EV_VALUE_S type STRING
      !EV_VALUE_C type C
      !ER_VALUE type ref to DATA .
  methods CHECK_PARAMETER_CONSISTENCE .
  methods CHECK_EXPIRATION
    importing
      !IV_EXPIRATION type STRING .
  methods CHECK_AUTHORITY
    importing
      value(IV_READ) type I default 0
      value(IV_WRITE) type I default 0 .
  methods VERIFY_SIGNATURE
    importing
      !IV_SECKEY type STRING
      !IV_MESSAGE type STRING
      !IV_CONTREP type STRING
      !IV_AUTHID type STRING .
  methods CHECK_SIGNATURE
    importing
      !IV_DOCPROT type STRING
      value(IV_CONTREP) type STRING optional
    returning
      value(RV_SIGNATURE) type SY-DATAR .
  methods PROCESS_INFO .
  methods PROCESS_GET .
  methods PROCESS_DOCGET .
  methods PROCESS_CREATE .
  methods PROCESS_CREATE_POST .
  methods PROCESS_MCREATE_POST .
  methods PROCESS_APPEND .
  methods PROCESS_UPDATE .
  methods PROCESS_UPDATE_POST .
  methods PROCESS_DELETE .
  methods PROCESS_SEARCH .
  methods PROCESS_ATTRSEARCH .
  methods PROCESS_PUTCERT .
  methods PROCESS_SERVERINFO .
  methods PROCESS_ADMINCONTREP .
  methods PROCESS_REP_CONFIGGET .
  methods PROCESS_REP_STATGET .
  methods PROCESS_REP_CERTGET .
  methods PROCESS_REP_CERTSET .
  class-methods FORMAT_TIMESTAMP
    importing
      !IV_TIMESTAMP type N
    exporting
      !EV_DATE type STRING
      !EV_TIME type STRING .
  class-methods FORMAT_STATUS
    importing
      !IV_STATUS type C
    returning
      value(RV_STATUS_TEXT) type STRING .
  class-methods FORMAT_DATE_AND_TIME
    importing
      !IV_DATE_IN type D
      !IV_TIME_IN type T
    exporting
      !EV_DATE type STRING
      !EV_TIME type STRING .
  methods SET_ERROR
    importing
      !IV_CODE type I
      !IV_TEXT type STRING .
  methods FIND_CODEPAGE
    importing
      !IV_DATA_BUFFER type XSTRING
    returning
      value(RV_CODEPAGE) type ABAP_ENCODING .
  methods TRANSLATE
    importing
      !IV_BUFFER type XSTRING
      !IV_CODEPAGE type ABAP_ENCODING
    changing
      !CV_CLINE type STRING .
  methods URL_HEX_DECODE_X
    importing
      !IV_IN type CSEQUENCE
    returning
      value(RV_OUT) type XSTRING .
  methods CONVERT_UPPER_LOWER
    importing
      !IV_PATTERN type XSTRING
      !IV_ENCODING type ABAP_ENCODING
    exporting
      !EV_UPPER type XSTRING
      !EV_LOWER type XSTRING .
  methods SET_LOCALE_FOR_CODEPAGE
    importing
      !IV_CODEPAGE type CPCODEPAGE .
  methods GET_LANGUAGE_FOR_CODEPAGE
    importing
      !IV_CODEPAGE type CPCODEPAGE
    returning
      value(RV_LANGUAGE) type SY-LANGU .
  methods DOC_SEARCH
    importing
      value(IV_CREP_ID) type C default SPACE
      value(IV_DOC_ID) type C
      value(IV_COMP_ID) type C default SPACE
      value(IV_SEARCH_TEXT) type C default SPACE
      value(IV_OFFSET) type I default 0
      value(IV_TO_OFFSET) type I default -1
      value(IV_CASE_SENSITIVE) type C default SPACE
      value(IV_NUM_RESULTS) type I default 1
      value(IV_SIGNATURE) type C default 'X'
      value(IV_SECURITY) type C
      value(IV_RAW_PATTERN) type C default SPACE
    exporting
      !ET_RESULT type STANDARD TABLE .
  methods SEARCH_BINARY_2
    importing
      !IV_BUFFER type XSTRING
      !IV_PATTERN type XSTRING
      !IV_PATTERN2 type XSTRING
    changing
      !CV_POS type I .
  methods SEARCH_BINARY
    importing
      !IV_BUFFER type XSTRING
      !IV_PATTERN type XSTRING
    changing
      !CV_POS type I .
  methods ATTR_SEARCH
    importing
      value(IV_CREP_ID) type C default SPACE
      value(IV_DOC_ID) type C
      value(IV_FROM_OFFSET) type I default 0
      value(IV_TO_OFFSET) type I default -1
      value(IV_CASE_SENSITIVE) type C default SPACE
      value(IV_NUM_RESULTS) type I default 1
      value(IV_SIGNATURE) type C default 'X'
      value(IV_SECURITY) type C
      value(IV_PATTERN) type C default SPACE
    exporting
      !ET_RESULT type GTYP_T_ATTR_RESULT .
  methods ATTR_SEARCH_INNER
    importing
      !IV_FROM type I
      !IV_TO type I
      !IV_CASE_SENSITIVE type C
      !IV_REVERSE type C
      !IV_DATA_BUFFER type XSTRING
      !IV_DESCR_BUFFER type XSTRING
      !IV_PATTERN type C
    changing
      !CT_RESULT type GTYP_T_ATTR_RESULT .
  methods CONVERT_STR_TO_TIME
    importing
      !IV_DATETIME type STRING
    returning
      value(RV_UTC_TIME) type SDOK_CRTST .
ENDCLASS.



CLASS ZGOOG_CL_CONTENT_REPO_GCS IMPLEMENTATION.


  METHOD attr_search.
    TYPES:
      BEGIN OF lty_off,
        pos TYPE i,
      END OF lty_off,

      ltt_off TYPE STANDARD TABLE OF lty_off.

    DATA: lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk.

    DATA: lv_p_bucket       TYPE string,
          lv_case_sensitive TYPE c LENGTH 1,
          lv_codepage       TYPE abap_encoding,
          lv_offset         TYPE i,
          lv_max_offset     TYPE i,
          lv_pattern        TYPE xstring,
          lv_upper          TYPE xstring,
          lv_lower          TYPE xstring,
          lv_pos            TYPE i,
          lv_p_object       TYPE string,
          lv_ret_code       TYPE i,
          lv_err_text       TYPE string,
          lv_data_buffer    TYPE xstring,
          lv_descr_buffer   TYPE xstring,
          lv_msg            TYPE string,
          lv_from           TYPE i,
          lv_to             TYPE i,
          lv_reverse        TYPE c LENGTH 1.

    DATA: ls_off            TYPE lty_off.

    DATA: lt_off            TYPE ltt_off.

    load_goog_repo_config( iv_crep_id ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_p_bucket = gs_repo_config.

    TRY.
        CREATE OBJECT lo_gcs EXPORTING iv_key_name = gs_repo_config-keyname.

        lo_gcs->add_common_qparam( iv_name  = 'alt'
                                   iv_value = 'media' ).

        lv_p_object = iv_doc_id && '/' && iv_doc_id && '-' && 'data'.

        lo_gcs->get_objects( EXPORTING iv_p_bucket = lv_p_bucket
                                       iv_p_object = lv_p_object
                             IMPORTING ev_ret_code = lv_ret_code
                                       ev_err_text = lv_err_text
                                       es_raw      = lv_data_buffer ).

        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

        lv_p_object = iv_doc_id && '/' && iv_doc_id && '-' && 'descr'.

        lo_gcs->get_objects( EXPORTING iv_p_bucket = lv_p_bucket
                                       iv_p_object = lv_p_object
                             IMPORTING ev_ret_code = lv_ret_code
                                       ev_err_text = lv_err_text
                                       es_raw      = lv_descr_buffer ).

        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_msg = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = 400
                   iv_text = lv_msg ).
        RETURN.
    ENDTRY.

    IF iv_from_offset > 0 AND iv_from_offset <= iv_to_offset OR iv_to_offset < 0.
      lv_from = iv_from_offset.
      lv_to   = lv_offset.
      lv_reverse = space.
    ELSE.
      lv_from = lv_offset.
      lv_to   = iv_from_offset.
      lv_reverse = 'X'.
    ENDIF.

    attr_search_inner( EXPORTING iv_from           = lv_from
                                 iv_to             = lv_to
                                 iv_case_sensitive = iv_case_sensitive
                                 iv_reverse        = lv_reverse
                                 iv_data_buffer    = lv_data_buffer
                                 iv_descr_buffer   = lv_descr_buffer
                                 iv_pattern        = iv_pattern
                       CHANGING  ct_result         = et_result ).
  ENDMETHOD.


  METHOD attr_search_inner.
    TYPES:
      BEGIN OF lty_search,
        pos    TYPE i,
        length TYPE i,
        value  TYPE string,
        lower  TYPE xstring,
        upper  TYPE xstring,
      END OF lty_search.

    CONSTANTS lc_dain    TYPE x LENGTH 4 VALUE '4441494E'.
    CONSTANTS lc_newline TYPE x LENGTH 1 VALUE '0A'.

    DATA lv_pattern    TYPE string.
    DATA lv_part       TYPE string.
    DATA lv_pos        TYPE string.
    DATA lv_length     TYPE string.
    DATA lv_value      TYPE string.
    DATA lv_subrc      TYPE sy-subrc.
    DATA ls_search     TYPE lty_search.
    DATA lt_search_tab TYPE STANDARD TABLE OF lty_search.
    DATA lv_encoding   TYPE abap_encoding.
    DATA lv_buffer     TYPE xstring.
    DATA lv_line       TYPE xstring.
    DATA lv_cline      TYPE string.
    DATA lv_offset     TYPE i.
    DATA lv_line_len   TYPE i.
    DATA lv_rest       TYPE xstring.
    DATA lv_off        TYPE string.
    DATA lv_len        TYPE string.
    DATA lv_found      TYPE abap_bool.
    DATA lv_rest2      TYPE xstring.
    DATA lv_search_pos TYPE i.
    DATA ls_result     TYPE scms_reatr.

    lv_pattern = iv_pattern.

    WHILE lv_pattern IS NOT INITIAL.
      SPLIT lv_pattern AT '#' INTO lv_part lv_pattern.
      SPLIT lv_part AT '+' INTO lv_pos lv_length lv_value.
      IF NOT lv_pos CO '0123456789' OR NOT lv_length CO '01234567890'.
        lv_subrc = 1.
        EXIT.
      ENDIF.
      ls_search-pos    = lv_pos.
      ls_search-length = lv_length.
      ls_search-value  = lv_value.
      APPEND ls_search TO lt_search_tab.
    ENDWHILE.

    lv_encoding = '1100'.

    lv_buffer = iv_data_buffer.
    SPLIT lv_buffer AT lc_newline INTO lv_line lv_buffer IN BYTE MODE.
    translate( EXPORTING iv_buffer   = lv_line
                         iv_codepage = lv_encoding
               CHANGING  cv_cline    = lv_cline ).

    " Get codepage information from data file
    IF lv_cline CP 'V0200*'.  " new ALF format
      WHILE lv_buffer IS NOT INITIAL.
        SPLIT lv_buffer AT lc_newline INTO lv_line lv_buffer IN BYTE MODE.
        translate( EXPORTING iv_buffer   = lv_line
                             iv_codepage = lv_encoding
                   CHANGING  cv_cline    = lv_cline ).
        IF lv_cline NP '#**'.
          CONTINUE.
        ENDIF.
        IF lv_cline CP '#*CODEPAGE=*'.
          SPLIT lv_cline AT '=' INTO lv_cline lv_part.
          IF lv_part CO '0123456789'.
            lv_encoding = lv_part.
          ENDIF.
          EXIT.
        ENDIF.
      ENDWHILE.

    ENDIF.

    LOOP AT lt_search_tab INTO ls_search.
      lv_line = url_hex_decode_x( ls_search-value ).

      IF iv_case_sensitive = 'X'.
        ls_search-upper = lv_line.
        ls_search-lower = lv_line.
      ELSE.
        convert_upper_lower( EXPORTING iv_pattern  = lv_line
                                       iv_encoding = lv_encoding
                             IMPORTING ev_upper    = ls_search-upper
                                       ev_lower    = ls_search-lower ).
      ENDIF.
      MODIFY lt_search_tab FROM ls_search.
    ENDLOOP.

    lv_offset = 0.
    lv_buffer = iv_descr_buffer.

    WHILE lv_buffer IS NOT INITIAL.
      SPLIT lv_buffer AT lc_newline INTO lv_line lv_buffer IN BYTE MODE.
      lv_line_len = xstrlen( lv_line ).

      SPLIT lv_line AT lc_dain INTO lv_line lv_rest IN BYTE MODE.
      translate( EXPORTING iv_buffer   = lv_line
                           iv_codepage = lv_encoding
                 CHANGING  cv_cline    = lv_cline ).
      CONDENSE lv_cline.
      SPLIT lv_cline AT ' ' INTO lv_off lv_len.
      IF lv_off CO '0123456789' AND lv_len CO '0123456789'.
        IF ( lv_off >= iv_from OR iv_from < 0 ) AND ( lv_off <= iv_to OR iv_to < 0 ).
          lv_found = abap_true.
          LOOP AT lt_search_tab INTO ls_search.
            lv_rest2 = lv_rest+ls_search-pos(ls_search-length).
            IF ls_search-upper <> ls_search-lower.
              search_binary_2( EXPORTING iv_buffer   = lv_rest2
                                         iv_pattern  = ls_search-lower
                                         iv_pattern2 = ls_search-upper
                               CHANGING  cv_pos      = lv_search_pos ).
            ELSE.
              search_binary( EXPORTING iv_buffer  = lv_rest2
                                       iv_pattern = ls_search-lower
                             CHANGING  cv_pos     = lv_search_pos ).
            ENDIF.
            IF lv_search_pos < 0.
              lv_found = abap_false.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF lv_found = abap_true.
            ls_result-pos    = lv_off.
            ls_result-length = lv_len.
            IF iv_reverse = 'X'.
              INSERT ls_result INTO ct_result INDEX 1.
            ELSE.
              APPEND ls_result TO ct_result.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      lv_offset = lv_offset + lv_line_len + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD body_add_field.
    CONCATENATE gv_body iv_name '="' iv_value '";' INTO gv_body.
  ENDMETHOD.


  METHOD body_put_field.
    CONCATENATE gv_body iv_name '=' iv_value gv_nl INTO gv_body.
  ENDMETHOD.


  METHOD check_authority.

    DATA: lv_udata       TYPE uslogond,
          lv_status_text TYPE string,
          lv_seckey      TYPE string,
          lv_signature   TYPE sy-datar.

    DATA: lt_r_crep_table  TYPE RANGE OF tabname.

    DATA: ls_r_crep_table LIKE LINE OF lt_r_crep_table.

    DEFINE raise_error.
      sy-msgid = &1.
      sy-msgty = &2.
      sy-msgno = &3.
      sy-msgv1 = &4.
      sy-msgv2 = &5.
      sy-msgv3 = &6.
      sy-msgv4 = &7.
    END-OF-DEFINITION.

    " Populate range with list of CREP tables for authorization check
    REFRESH lt_r_crep_table.
    ls_r_crep_table-sign   = 'I'.
    ls_r_crep_table-option = 'EQ'.
    ls_r_crep_table-low    = 'CREP'.
    APPEND ls_r_crep_table TO lt_r_crep_table.

    ls_r_crep_table-sign   = 'I'.
    ls_r_crep_table-option = 'EQ'.
    ls_r_crep_table-low    = 'CREPDESCR'.
    APPEND ls_r_crep_table TO lt_r_crep_table.

    ls_r_crep_table-sign   = 'I'.
    ls_r_crep_table-option = 'EQ'.
    ls_r_crep_table-low    = 'CREPDOCSP'.
    APPEND ls_r_crep_table TO lt_r_crep_table.

    LOOP AT lt_r_crep_table INTO ls_r_crep_table.
      IF iv_read <> 0 OR iv_write <> 0.
        CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
          EXPORTING
            view_action                    = 'S'
            view_name                      = ls_r_crep_table-low
            no_warning_for_clientindep     = 'X'
          EXCEPTIONS
            invalid_action                 = 1
            no_authority                   = 2
            no_clientindependent_authority = 3
            table_not_found                = 4
            no_linedependent_authority     = 5
            OTHERS                         = 6.
        IF sy-subrc <> 0.
          raise_error 'CMS' 'E' '023' space space space space.
          sy-subrc = 2.
        ENDIF.

      ENDIF.

      IF sy-subrc <> 0 OR iv_write = 0.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
        EXPORTING
          view_action                    = 'U'
          view_name                      = ls_r_crep_table-low
          no_warning_for_clientindep     = 'X'
        EXCEPTIONS
          invalid_action                 = 1
          no_authority                   = 2
          no_clientindependent_authority = 3
          table_not_found                = 4
          no_linedependent_authority     = 5
          OTHERS                         = 6.

      IF sy-subrc <> 0.
        " Implement suitable error handling here
        raise_error 'CMS' 'E' '019' space space space space.
        sy-subrc = 1.
      ELSE.

        CALL FUNCTION 'SUSR_USER_LOGONDATA_GET'
          EXPORTING
            user_name           = sy-uname
          IMPORTING
            user_logondata      = lv_udata
          EXCEPTIONS
            user_name_not_exist = 1
            OTHERS              = 2.
        IF sy-subrc = 0.
          IF lv_udata-ustyp = 'S'.
            lv_status_text
              = 'Service user should not be used'(001).
            sy-subrc = '1'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0.

      CALL FUNCTION 'SUSR_USER_LOGONDATA_GET'
        EXPORTING
          user_name           = sy-uname
        IMPORTING
          user_logondata      = lv_udata
        EXCEPTIONS
          user_name_not_exist = 1
          OTHERS              = 2.
      IF sy-subrc = 0.
        IF lv_udata-ustyp = 'S'.
          lv_status_text
            = 'Service user should not be used'(001).
          sy-subrc = '1'.
        ENDIF.
      ENDIF.
    ENDIF.

    " Avoid anonymous access
    IF go_server = go_server.
      lv_status_text
        = 'Explicit logon required'(002).
      sy-subrc = 1.
    ENDIF.

    IF sy-subrc <> 0.

      lv_seckey = get_parameter( iv_name = 'secKey' ).

      IF lv_seckey IS NOT INITIAL.
        get_parameter( iv_name      = 'authId'
                       iv_mandatory = 'S' ).
        get_parameter( iv_name      = 'expiration'
                       iv_mandatory = 'S' ).
      ENDIF.

      lv_signature = check_signature( iv_docprot = ''
                                      iv_contrep = '' ).
      IF lv_signature = 'X'.
        sy-subrc = 0.
      ELSE.
        sy-subrc = 1.
      ENDIF.

    ENDIF.

    IF sy-subrc = 0.
      CLEAR gs_error.
    ENDIF.

    IF sy-subrc <> 0.
      IF lv_status_text IS NOT INITIAL.
        CLEAR gs_error.
        gs_error-status_text = lv_status_text.
      ENDIF.

      gs_error-status_code      = 401.
      gs_error-set_authenticate = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD check_expiration.

    DATA: lv_len        TYPE i,
          lv_dummy      TYPE string ##NEEDED,
          lv_time       TYPE timestamp,
          lv_expiration TYPE timestamp.

    lv_len = strlen( iv_expiration ).
    IF NOT iv_expiration CO '0123456789' OR lv_len <> 14.
      MESSAGE e185(cms) WITH 'expiration' INTO lv_dummy.
      sys_error_set( ).
      gs_error-status_code = 401.
      EXIT.
    ENDIF.

    GET TIME STAMP FIELD lv_time.

    lv_expiration = iv_expiration.
    IF lv_expiration < lv_time.
      MESSAGE e184(cms) INTO lv_dummy.
      sys_error_set( ).
      gs_error-status_code = 401.
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD check_parameter_consistence.

    DATA: lv_operation TYPE string,
          lv_command   TYPE string,
          lv_dummy     TYPE string ##NEEDED.

    DATA: ls_param     TYPE ltyp_s_parameter.

    LOOP AT gt_parameters INTO ls_param WHERE usage = c_parameter_missing. "#EC CI_STDSEQ
      lv_operation = get_parameter( iv_name = 'operation' ).
      IF lv_operation IS NOT INITIAL.
        CONCATENATE gv_command '&operation=' lv_operation INTO lv_command ##NO_TEXT.
      ELSE.
        lv_command = gv_command.
      ENDIF.
      MESSAGE e181(cms) WITH lv_command ls_param-name INTO lv_dummy.
      sys_error_set( ).
      gs_error-status_code = 400.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_signature.

    DATA: lv_message    TYPE string,
          lv_seckey     TYPE string,
          lv_amode      TYPE string,
          lv_authid     TYPE string,
          lv_expiration TYPE string,
          lv_contrep    TYPE string,
          lv_dummy      TYPE string ##NEEDED,
          lv_nosig      TYPE c LENGTH 1.

    DATA: ls_param      TYPE ltyp_s_parameter.

    CLEAR rv_signature.

    LOOP AT gt_parameters INTO ls_param.
      IF ls_param-usage = gv_parameter_sec_used.
        CONCATENATE lv_message ls_param-value INTO lv_message.
      ENDIF.
      IF ls_param-name = 'secKey'.
        lv_seckey = ls_param-value.
      ELSEIF ls_param-name = 'accessMode'.
        lv_amode = url_hex_decode( iv_value = ls_param-value ).
      ELSEIF ls_param-name = 'authId'.
        lv_authid = url_hex_decode( iv_value = ls_param-value ).
      ELSEIF ls_param-name = 'expiration'.
        lv_expiration = url_hex_decode( iv_value = ls_param-value ).
      ELSEIF ls_param-name = 'contRep'.
        lv_contrep = url_hex_decode( iv_value = ls_param-value ).
      ENDIF.
    ENDLOOP.

    IF iv_contrep IS SUPPLIED.
      lv_contrep = iv_contrep.
    ENDIF.

    IF lv_seckey IS NOT INITIAL.
      check_expiration( iv_expiration = lv_expiration ).
      IF gs_error IS NOT INITIAL.
        EXIT.
      ENDIF.

      IF NOT lv_amode CA iv_docprot AND iv_docprot <> ' '.
        MESSAGE e182(cms) WITH lv_amode gv_command iv_docprot INTO lv_dummy.
        sys_error_set( ).
        gs_error-status_code = 401.
        EXIT.
      ENDIF.

      IF lv_contrep <> space.
        SELECT SINGLE http_nosig FROM crep_http
          INTO lv_nosig
          WHERE crep_id = lv_contrep.

        IF sy-subrc <> 0 AND gv_crep_type <> scmst_crtyp_rfc.
          MESSAGE e002(cms) WITH lv_contrep INTO lv_dummy.
          " Content Repository &1 doesn't exist
          sys_error_set( ).
          gs_error-status_code = '400'.
          EXIT.
        ENDIF.

        IF lv_nosig = 'X'.
          rv_signature = space.
          EXIT.
        ENDIF.

      ENDIF.

      verify_signature( iv_seckey  = lv_seckey
                        iv_message = lv_message
                        iv_contrep = lv_contrep
                        iv_authid  = lv_authid ).
      IF gs_error IS INITIAL.
        rv_signature = 'X'.
      ENDIF.
    ELSE.
      " If Seckey is initial check if signature is required for the repository.
      IF lv_contrep <> space.
        " Repository Type HTTP
        SELECT SINGLE http_nosig FROM crep_http
          INTO lv_nosig
          WHERE crep_id = lv_contrep.
        IF sy-subrc <> 0 AND gv_crep_type <> scmst_crtyp_rfc.
          MESSAGE e002(cms) WITH lv_contrep INTO lv_dummy.
          " Content Repository &1 existiert nicht
          sys_error_set( ).
          gs_error-status_code = '400'.
          EXIT.
        ENDIF.

        IF lv_nosig = 'X'.
          EXIT.
        ELSE.
          " Error the signature is required and it'f not passed thus report an error.
          MESSAGE e109(cms) WITH lv_contrep INTO lv_dummy.
          sys_error_set( ).
          gs_error-status_code = 401.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD convert_upper_lower.
    DATA lv_lg       TYPE tcp0c-langu.
    DATA lv_c        TYPE tcp0c-country.
    DATA lv_m        TYPE tcp0c-modifier.
    DATA lv_codepage TYPE cpcodepage.
    DATA lo_convin   TYPE REF TO cl_abap_conv_in_ce.
    DATA lv_text     TYPE string.
    DATA lo_convout  TYPE REF TO cl_abap_conv_out_ce.

    " Get current setting
    GET LOCALE LANGUAGE lv_lg COUNTRY lv_c MODIFIER lv_m.

    lv_codepage = iv_encoding.
    set_locale_for_codepage( lv_codepage ).

    TRY.
        lo_convin = cl_abap_conv_in_ce=>create( encoding = iv_encoding
                                                input    = iv_pattern ).

        lo_convin->read( IMPORTING data = lv_text ).

        lo_convout = cl_abap_conv_out_ce=>create( encoding = iv_encoding ).

        lv_text = to_lower( lv_text ).
        lo_convout->write( data = lv_text ).
        ev_lower = lo_convout->get_buffer( ).
        lo_convout->reset( ).

        lv_text = to_upper( lv_text ).
        lo_convout->write( data = lv_text ).
        ev_upper = lo_convout->get_buffer( ).

      CATCH cx_root.
        CLEAR: ev_upper,
               ev_lower.

    ENDTRY.

    " Restore setting
    SET LOCALE LANGUAGE lv_lg COUNTRY lv_c MODIFIER lv_m.
  ENDMETHOD.


  METHOD doc_search.
    TYPES:
      BEGIN OF lty_off,
        pos TYPE i,
      END OF lty_off,

      ltt_off TYPE STANDARD TABLE OF lty_off.

    DATA: lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk.

    DATA: lv_p_bucket   TYPE string,
          lv_p_object   TYPE string,
          lv_ret_code   TYPE i,
          lv_err_text   TYPE string,
          lv_buffer     TYPE xstring,
          lv_msg        TYPE string,
          lv_codepage   TYPE abap_encoding,
          lv_pattern    TYPE xstring,
          lv_upper      TYPE xstring,
          lv_lower      TYPE xstring,
          lv_max_offset TYPE i,
          lv_offset     TYPE i,
          lv_pos        TYPE i,
          ls_off        TYPE lty_off,
          lt_off        TYPE ltt_off,
          lv_cnt        TYPE i,
          lv_pos1       TYPE char20.

    load_goog_repo_config( iv_crep_id ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_p_bucket = gs_repo_config-bucket.

    lv_p_object = iv_doc_id && '/' && iv_doc_id && '-' && iv_comp_id.

    TRY.
        CREATE OBJECT lo_gcs EXPORTING iv_key_name = gs_repo_config-keyname.

        lo_gcs->add_common_qparam( iv_name  = 'alt'
                                   iv_value = 'media' ).

        lo_gcs->get_objects( EXPORTING iv_p_bucket = lv_p_bucket
                                       iv_p_object = lv_p_object
                             IMPORTING ev_ret_code = lv_ret_code
                                       ev_err_text = lv_err_text
                                       es_raw      = lv_buffer ).

        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_msg = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = 400
                   iv_text = lv_msg ).
        RETURN.
    ENDTRY.

    lv_codepage = find_codepage( lv_buffer ).

    IF iv_search_text IS INITIAL.
      lv_pattern = url_hex_decode_x( iv_raw_pattern ).
    ELSE.
      lv_pattern = iv_search_text.
    ENDIF.

    IF iv_case_sensitive = space.

      convert_upper_lower( EXPORTING iv_pattern  = lv_pattern
                                     iv_encoding = lv_codepage
                           IMPORTING ev_upper    = lv_upper
                                     ev_lower    = lv_lower ).

      IF     lv_pattern = lv_upper
         AND lv_pattern = lv_lower.
        iv_case_sensitive = 'X'.
      ENDIF.
    ELSE.
      lv_upper = lv_pattern.
      lv_lower = lv_pattern.
    ENDIF.

    lv_max_offset = xstrlen( lv_buffer ) - 1.

    IF iv_offset < 0 OR iv_offset > lv_max_offset.
      iv_offset = lv_max_offset.
    ENDIF.

    IF iv_to_offset < 0 OR iv_to_offset > lv_max_offset.
      iv_to_offset = lv_max_offset.
    ENDIF.

    IF iv_offset < iv_to_offset.
      lv_offset = iv_offset.
      lv_max_offset = iv_to_offset.
    ELSE.
      lv_offset = iv_to_offset.
      lv_max_offset = iv_offset.
    ENDIF.

    " Do search
    SHIFT lv_buffer LEFT BY lv_offset PLACES IN BYTE MODE.
    WHILE lv_buffer IS NOT INITIAL AND lv_offset <= lv_max_offset.
      IF iv_case_sensitive = space.
        search_binary_2( EXPORTING iv_buffer   = lv_buffer
                                   iv_pattern  = lv_lower
                                   iv_pattern2 = lv_upper
                         CHANGING  cv_pos      = lv_pos ).

      ELSE.
        search_binary( EXPORTING iv_buffer  = lv_buffer
                                 iv_pattern = lv_pattern
                       CHANGING  cv_pos     = lv_pos ).
      ENDIF.
      IF lv_pos >= 0.
        ls_off-pos = lv_offset + lv_pos.
        IF ls_off-pos >= lv_offset AND ls_off-pos <= lv_max_offset.
          APPEND ls_off TO lt_off.
        ENDIF.
        lv_pos = lv_pos + 1.
        SHIFT lv_buffer LEFT BY lv_pos PLACES IN BYTE MODE.
        lv_offset = lv_offset + lv_pos.
      ELSE.
        CLEAR lv_buffer.
      ENDIF.
    ENDWHILE.

    " Reduce hitlist according to the request

    DESCRIBE TABLE lt_off LINES lv_cnt.
    WHILE lv_cnt > iv_num_results.
      IF iv_offset < iv_to_offset.
        DELETE lt_off INDEX lv_cnt.
      ELSE.
        DELETE lt_off INDEX 1.
      ENDIF.
      lv_cnt = lv_cnt - 1.
    ENDWHILE.

    " Fill result
    CLEAR et_result[].
    LOOP AT lt_off INTO ls_off.

      lv_pos1 = ls_off-pos.
      IF iv_offset < iv_to_offset.
        APPEND lv_pos1 TO et_result.
      ELSE.
        INSERT lv_pos1 INTO et_result INDEX 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD find_codepage.
    CONSTANTS lc_newline TYPE x LENGTH 1 VALUE '0A'.

    DATA lv_buffer TYPE xstring.
    DATA lv_line   TYPE xstring.
    DATA lv_cline  TYPE string.
    DATA lv_part   TYPE string.

    lv_buffer = iv_data_buffer.
    SPLIT lv_buffer AT lc_newline INTO lv_line lv_buffer IN BYTE MODE.
    translate( EXPORTING iv_buffer   = lv_line
                         iv_codepage = '1100'
               CHANGING  cv_cline    = lv_cline ).

    rv_codepage = '1100'.

    " Get codepage information from data file
    IF lv_cline CP 'V0200*'.  " new ALF format
      WHILE lv_buffer IS NOT INITIAL.
        SPLIT lv_buffer AT lc_newline INTO lv_line lv_buffer IN BYTE MODE.
        translate( EXPORTING iv_buffer   = lv_line
                             iv_codepage = '1100'
                   CHANGING  cv_cline    = lv_cline ).

        IF lv_cline NP '#**'.
          CONTINUE.
        ENDIF.
        IF lv_cline CP '#*CODEPAGE=*'.
          SPLIT lv_cline AT '=' INTO lv_cline lv_part.
          IF lv_part CO '0123456789'.
            rv_codepage = lv_part.
          ENDIF.
          EXIT.
        ENDIF.
      ENDWHILE.

    ENDIF.
  ENDMETHOD.


  METHOD format_date_and_time.

    CONCATENATE iv_date_in(4) '-' iv_date_in+4(2) '-' iv_date_in+6(2) INTO ev_date.
    CONCATENATE iv_time_in(2) ':' iv_time_in+2(2) ':' iv_time_in+4(2) INTO ev_time.

  ENDMETHOD.


  METHOD format_status.

    CASE iv_status.
      WHEN '0'. rv_status_text = 'online'.
      WHEN '1'. rv_status_text = 'offline'.
      WHEN '2'. rv_status_text = 'unknown'.
    ENDCASE.

  ENDMETHOD.


  METHOD format_timestamp.

    DATA: lv_tst  TYPE timestamp,
          lv_date TYPE d,
          lv_time TYPE t.

    lv_tst = iv_timestamp.

    CONVERT TIME STAMP lv_tst TIME ZONE 'UTC   '
            INTO DATE lv_date TIME lv_time.

    CONCATENATE lv_date(4) '-' lv_date+4(2) '-' lv_date+6(2) INTO ev_date.
    CONCATENATE lv_time(2) ':' lv_time+2(2) ':' lv_time+4(2) INTO ev_time.

  ENDMETHOD.


  METHOD get_language_for_codepage.
    DATA lv_codepage TYPE cpcodepage.
    DATA lv_langs    TYPE string.
    DATA lv_lang     TYPE c LENGTH 1.

    CALL FUNCTION 'SCP_CODEPAGE_FOR_LANGUAGE'
      EXPORTING
        language    = sy-langu
      IMPORTING
        codepage    = lv_codepage
      EXCEPTIONS
        no_codepage = 1
        OTHERS      = 2.
    IF sy-subrc = 0 AND lv_codepage = '4110'.
      " Unicode fits all
      rv_language = sy-langu.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SCP_ALLOWED_LANGUAGES'
      IMPORTING
        languages = lv_langs.

    CLEAR rv_language.
    WHILE lv_langs IS NOT INITIAL.
      lv_lang = lv_langs(1).
      CALL FUNCTION 'SCP_CODEPAGE_FOR_LANGUAGE'
        EXPORTING
          language    = lv_lang
        IMPORTING
          codepage    = lv_codepage
        EXCEPTIONS ##FM_SUBRC_OK
          no_codepage = 1
          OTHERS      = 2.
      IF sy-subrc = 0 AND iv_codepage = lv_codepage.
        rv_language = lv_lang.
        EXIT.
      ENDIF.
      SHIFT lv_langs LEFT BY 1 PLACES.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_parameter.

    DATA ls_param TYPE ltyp_s_parameter.
    DATA lv_index TYPE i.

    READ TABLE gt_parameters INTO ls_param
      WITH KEY name = iv_name.                           "#EC CI_STDSEQ
    IF sy-subrc = 0.
      lv_index = sy-tabix.
      IF iv_raw_mode = abap_true.
        rv_value = ls_param-value.
      ELSE.
        rv_value = url_hex_decode( iv_value = ls_param-value ).
      ENDIF.
      IF iv_mandatory = 'S' OR iv_mandatory = 's'.
        ls_param-usage = gv_parameter_sec_used.
      ELSE.
        ls_param-usage = c_parameter_used.
      ENDIF.
      MODIFY gt_parameters INDEX lv_index FROM ls_param.
    ELSE.
      rv_value = iv_default.
      IF iv_mandatory = 'X' OR iv_mandatory = 'S'.
        ls_param-name  = iv_name.
        ls_param-usage = c_parameter_missing.
        APPEND ls_param TO gt_parameters.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_parameter_b.
    DATA lv_default TYPE string.
    DATA lv_value   TYPE string.

    lv_default = iv_default.

    lv_value = get_parameter( iv_name      = iv_name
                              iv_default   = lv_default
                              iv_mandatory = iv_mandatory ).

    IF lv_value = '1'.
      rv_value = 'X'.
    ELSEIF lv_value = '0'.
      rv_value = ' '.
    ELSE.
      gs_error-status_code = 400.
      EXIT.
    ENDIF.
  ENDMETHOD.


  METHOD get_parameter_c.
    DATA lv_len TYPE i.

    FIELD-SYMBOLS <lv_char> TYPE c.

    ev_value_s = get_parameter( iv_name      = iv_name
                                iv_default   = iv_default
                                iv_mandatory = iv_mandatory
                                iv_raw_mode  = iv_raw_mode ).

    ev_value_c = ev_value_s.
    IF ev_value_c <> ev_value_s.
      " Error handling
    ENDIF.

    lv_len = strlen( ev_value_s ).
    IF lv_len = 0.
      lv_len = 1.
    ENDIF.

    CREATE DATA er_value TYPE c LENGTH lv_len.
    ASSIGN er_value->* TO <lv_char>.
    <lv_char> = ev_value_s.
  ENDMETHOD.


  METHOD get_parameter_i.
    DATA lv_default TYPE string.
    DATA lv_value   TYPE string.

    lv_default = iv_default.

    lv_value = get_parameter( iv_name      = iv_name
                              iv_default   = lv_default
                              iv_mandatory = iv_mandatory ).

    IF NOT lv_value CO '0123456789- '.
      gs_error-status_code = 400.
      EXIT.
    ENDIF.

    rv_value = lv_value.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.

    go_server = server.
    go_server->response->if_http_entity~suppress_content_type( ).
    init( ).

    parse_uri( ).

    IF gs_error IS NOT INITIAL.
      report_error( ).
      EXIT.
    ENDIF.

    CASE gv_mode.
      WHEN 'GET'.
        CASE gv_command.
          WHEN 'info'. process_info( ).
          WHEN 'get'. process_get( ).
          WHEN 'docGet'. process_docget( ).
          WHEN 'delete'. process_delete( ).
          WHEN 'search'. process_search( ).
          WHEN 'attrSearch'. process_attrsearch( ).
          WHEN 'serverInfo'. process_serverinfo( ).
          WHEN 'adminContRep'. process_admincontrep( ).
          WHEN 'getCert'. process_rep_certget( ).
          WHEN OTHERS. gs_error-status_code = 400.
        ENDCASE.

      WHEN 'HEAD'.
        CASE gv_command.
          WHEN 'get'. process_get( ).
          WHEN OTHERS. gs_error-status_code = 400.
        ENDCASE.

      WHEN 'PUT'.
        CASE gv_command.
          WHEN 'create'. process_create( ).
          WHEN 'append'. process_append( ).
          WHEN 'update'. process_update( ).
          WHEN 'putCert'. process_putcert( ).
          WHEN 'adminContRep'. process_admincontrep( ).
          WHEN OTHERS. gs_error-status_code = 400.
        ENDCASE.

      WHEN 'POST'.
        CASE gv_command.
          WHEN 'create'. process_create_post( ).
          WHEN 'mCreate'. process_mcreate_post( ).
          WHEN 'update'. process_update_post( ).
          WHEN OTHERS. gs_error-status_code = 400.
        ENDCASE.

      WHEN OTHERS. gs_error-status_code = 400.
    ENDCASE.

    IF gs_error IS NOT INITIAL.
      report_error( ).
      EXIT.
    ENDIF.

    IF gv_body IS NOT INITIAL.
      go_server->response->set_cdata( data = gv_body ).
    ENDIF.

    if_http_extension~flow_rc = if_http_extension=>co_flow_ok_others_opt.

  ENDMETHOD.


  METHOD init.
    " Default document protection used if docProt is not passed
    gv_docprot = 'crud'.

    " String containing CRLF, used to separate lines
    gv_nl = cl_abap_char_utilities=>cr_lf.

    " Single space, used as delimiter
    gv_sp = ' X'.
    gv_sp = gv_sp(1).
  ENDMETHOD.


  METHOD load_goog_repo_config.
    SELECT SINGLE * FROM zgoog_cont_repo
      INTO gs_repo_config
      WHERE archive_id = iv_contrep.
    IF sy-subrc <> 0.

      set_error( iv_code = 400
                 iv_text = 'Configuration missing in table ZGOOG_CONT_REPO for the Content Repository ID' ) ##NO_TEXT.

    ENDIF.
  ENDMETHOD.


  METHOD parse_uri.

    DATA: lo_scms_scan_obj  TYPE REF TO cl_scms_virus_scan_info.

    DATA: lv_uri            TYPE string,
          lv_script         TYPE string,
          lv_rest           TYPE string,
          lv_path           TYPE string,
          lv_dummy          TYPE string ##NEEDED,
          lv_extension      TYPE string,
          lv_ext1           TYPE string ##NEEDED,
          lv_ext2           TYPE string ##NEEDED,
          lv_ext3           TYPE string ##NEEDED,
          lv_scanperformed1 TYPE string,
          lv_index          TYPE i,
          lv_scanperformed  TYPE c LENGTH 1.

    DATA: ls_parameter TYPE ltyp_s_parameter,
          ls_param     TYPE ltyp_s_parameter.

    gv_mode = go_server->request->get_header_field( '~REQUEST_METHOD' ).

    lv_uri    = go_server->request->get_header_field( '~REQUEST_URI' ).
    lv_script = go_server->request->get_header_field( '~SCRIPT_NAME' ).

    CLEAR: gv_command,
           gt_parameters[].

    SPLIT lv_uri AT '?' INTO lv_path lv_rest.

    SPLIT lv_path AT lv_script INTO lv_dummy lv_extension.
    SPLIT lv_extension AT '/' INTO lv_dummy lv_ext1 lv_ext2 lv_ext3.

    IF lv_ext1 IS INITIAL.
      gv_mandt = sy-mandt.
    ELSE.
      gv_mandt = lv_ext1.
    ENDIF.

    ls_parameter-usage = c_parameter_unknown.

    SPLIT lv_rest AT '&' INTO gv_command lv_rest.
    WHILE lv_rest IS NOT INITIAL.
      SPLIT lv_rest AT '&' INTO ls_parameter-name lv_rest.
      SPLIT ls_parameter-name AT '='
            INTO ls_parameter-name ls_parameter-value.
      ls_parameter-name = url_hex_decode( iv_value = ls_parameter-name ).
      APPEND ls_parameter TO gt_parameters.
    ENDWHILE.

    READ TABLE gt_parameters WITH KEY name = 'contRep' INTO ls_parameter. "#EC CI_STDSEQ
    IF sy-subrc = 0.
      gv_contrep = url_hex_decode( iv_value = ls_parameter-value ).
      SELECT SINGLE crep_type FROM crep
        INTO gv_crep_type
        WHERE crep_id = gv_contrep.                       "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD process_admincontrep.

    DATA lv_operation TYPE string.

    lv_operation = get_parameter( iv_name      = 'operation'
                                  iv_mandatory = 'S' ).
    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    CASE gv_mode.
      WHEN 'GET'.
        CASE lv_operation.
          WHEN 'configGet'. process_rep_configget( ).
          WHEN 'certGet'. process_rep_certget( ).
          WHEN 'statGet'. process_rep_statget( ).
          WHEN OTHERS. gs_error-status_code = 400.
        ENDCASE.
      WHEN 'PUT'.
        CASE lv_operation.
          WHEN 'certSet'. process_rep_certset( ).
          WHEN OTHERS. gs_error-status_code = 400.
        ENDCASE.
    ENDCASE.

  ENDMETHOD.


  METHOD process_append.

    DATA: lo_contrep       TYPE REF TO data,
          lo_docid         TYPE REF TO data,
          lo_compid        TYPE REF TO data,
          lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_entity        TYPE REF TO if_http_entity,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk.

    DATA: lv_pversion         TYPE string ##NEEDED,
          lv_seckey           TYPE string,
          lv_smand            TYPE c LENGTH 1,
          lv_signature        TYPE c LENGTH 1,
          lv_data             TYPE xstring,
          lv_mimetype         TYPE string,
          lv_compsize         TYPE i,
          lt_bin              TYPE STANDARD TABLE OF sdokcntbin,
          lv_p_bucket         TYPE string,
          lv_object_name      TYPE string,
          lv_object_apnd_name TYPE string.

    DATA: ls_input_object  TYPE /goog/cl_storage_v1=>ty_013,
          ls_output_object TYPE /goog/cl_storage_v1=>ty_013,
          ls_source_obj    TYPE /goog/cl_storage_v1=>ty_067,
          ls_compose_inp   TYPE /goog/cl_storage_v1=>ty_006.

    DATA: lt_source_obj TYPE /goog/cl_storage_v1=>ty_t_067.

    FIELD-SYMBOLS: <lv_contrep> TYPE c,
                   <lv_docid>   TYPE c,
                   <lv_compid>  TYPE c.

    get_parameter_c( EXPORTING iv_name      = 'contRep'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_docid ).
    get_parameter_c( EXPORTING iv_name      = 'compId'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_compid ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.
    ASSIGN lo_compid->*  TO <lv_compid>.

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'u' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lo_entity = go_server->request.
    lv_data = lo_entity->get_data( ).

    load_goog_repo_config( <lv_contrep> ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.
    lv_mimetype = lo_entity->get_header_field( name = 'Content-Type' ) ##NO_TEXT.

    lv_p_bucket = gs_repo_config-bucket.
    TRY.

        CREATE OBJECT lo_gcs
          EXPORTING
            iv_key_name = gs_repo_config-keyname.

        FIELD-SYMBOLS <ls_comp_data> TYPE gtyp_s_comp_data.

        lv_object_name = <lv_docid> && '/' && <lv_docid> && '-' && <lv_compid>.
        lv_object_apnd_name = lv_object_name && 'append' ##NO_TEXT.

        DATA: lv_ret_code TYPE i.
        DATA: lv_err_text TYPE string.
        lo_gcs->insert_objects( EXPORTING iv_q_name       = lv_object_apnd_name
                                          iv_p_bucket     = lv_p_bucket
                                          is_input        = ls_input_object
                                          is_data         = lv_data
                                          iv_content_type = lv_mimetype
                                IMPORTING es_output       = ls_output_object
                                          ev_ret_code     = lv_ret_code
                                          ev_err_text     = lv_err_text ).
        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

        ls_source_obj-name = lv_object_name.
        APPEND ls_source_obj TO lt_source_obj.
        ls_source_obj-name = lv_object_apnd_name.
        APPEND ls_source_obj TO lt_source_obj.

        APPEND LINES OF lt_source_obj TO ls_compose_inp-source_objects.

        lv_object_name = cl_http_utility=>escape_url( unescaped = lv_object_name ).

        DATA: ls_op TYPE /goog/cl_storage_v1=>ty_013.
        lo_gcs->compose_objects( EXPORTING iv_p_destination_bucket = lv_p_bucket
                                           iv_p_destination_object = lv_object_name
                                           is_input                = ls_compose_inp
                                 IMPORTING es_output               = ls_op
                                           ev_ret_code             = lv_ret_code
                                           ev_err_text             = lv_err_text ).
        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

        lv_object_apnd_name = cl_http_utility=>escape_url( unescaped = lv_object_apnd_name ).
        lo_gcs->delete_objects( EXPORTING iv_p_bucket = lv_p_bucket
                                          iv_p_object = lv_object_apnd_name
                                IMPORTING ev_ret_code = lv_ret_code
                                          ev_err_text = lv_err_text ).

      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_err_text = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = lv_ret_code
                   iv_text = lv_err_text ).
        RETURN.
    ENDTRY.

    go_server->response->set_status( code   = 200
                                     reason = 'ok' ).
  ENDMETHOD.


  METHOD process_attrsearch.
    TYPES lty_result TYPE c LENGTH 20.

    DATA:lo_contrep TYPE REF TO data,
         lo_docid   TYPE REF TO data,
         lo_pattern TYPE REF TO data.

    DATA: lv_fromoffset    TYPE i,
          lv_tooffset      TYPE i,
          lv_numresults    TYPE i,
          lv_casesens      TYPE string,
          lv_pversion      TYPE string ##NEEDED,
          lv_seckey        TYPE string,
          lv_smand         TYPE c LENGTH 1,
          lv_signature     TYPE c LENGTH 1,
          lv_casesensitive TYPE c LENGTH 1,
          lv_pos           TYPE string,
          lv_length        TYPE string,
          lv_cnt           TYPE i.

    DATA: ls_result TYPE scms_reatr.

    DATA: lt_results       TYPE STANDARD TABLE OF scms_reatr.

    FIELD-SYMBOLS: <lv_contrep> TYPE c,
                   <lv_docid>   TYPE c,
                   <lv_pattern> TYPE c.

    get_parameter_c( EXPORTING iv_name      = 'contRep'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_docid ).
    get_parameter_c( EXPORTING iv_name      = 'pattern'
                               iv_mandatory = 'X'
                               iv_raw_mode  = 'X'
                     IMPORTING er_value     = lo_pattern ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.
    ASSIGN lo_pattern->* TO <lv_pattern>.

    lv_fromoffset = get_parameter_i( iv_name    = 'fromOffset'
                                     iv_default = 0 ).
    lv_tooffset = get_parameter_i( iv_name    = 'toOffset'
                                   iv_default = -1 ).
    lv_numresults = get_parameter_i( iv_name    = 'numResults'
                                     iv_default = 1 ).

    lv_casesens = get_parameter( iv_name    = 'caseSensitive'
                                 iv_default = 'n' ).
    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'r' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF lv_casesens = 'n'.
      lv_casesensitive = ' '.
    ELSEIF lv_casesens = 'y'.
      lv_casesensitive = 'X'.
    ELSE.
      gs_error-status_code = 400.
    ENDIF.

    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    attr_search(
      EXPORTING
        iv_crep_id        = <lv_contrep>
        iv_doc_id         = <lv_docid>
        iv_from_offset    = lv_fromoffset
        iv_to_offset      = lv_tooffset
        iv_case_sensitive = lv_casesensitive
        iv_num_results    = lv_numresults
        iv_signature      = lv_signature
        iv_security       = ' '
        iv_pattern        = <lv_pattern>
      IMPORTING
        et_result         = lt_results
    ).

    DESCRIBE TABLE lt_results LINES lv_cnt.
    gv_body = lv_cnt.
    CONDENSE gv_body NO-GAPS.
    CONCATENATE gv_body ';' INTO gv_body.

    LOOP AT lt_results INTO ls_result.
      lv_pos = ls_result-pos.
      lv_length = ls_result-length.
      CONDENSE lv_pos NO-GAPS.
      CONDENSE lv_length NO-GAPS.
      CONCATENATE gv_body lv_pos ';' lv_length ';' INTO gv_body.
    ENDLOOP.

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).
  ENDMETHOD.


  METHOD process_create.

    DATA: lo_contrep       TYPE REF TO data,
          lo_docid         TYPE REF TO data,
          lo_compid        TYPE REF TO data,
          lo_docprot       TYPE REF TO data,
          lo_entity        TYPE REF TO if_http_entity,
          lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk.

    DATA: lv_compid      TYPE string,
          lv_pversion    TYPE string ##NEEDED,
          lv_seckey      TYPE string,
          lv_smand       TYPE c LENGTH 1,
          lv_signature   TYPE c LENGTH 1,
          lv_data        TYPE xstring,
          lv_mimetype    TYPE string,
          lv_p_bucket    TYPE string,
          lv_object_name TYPE string,
          lv_ret_code    TYPE i,
          lv_msg         TYPE string,
          lv_err_text    TYPE string.

    DATA: ls_info          TYPE scms_acinf,
          ls_input_object  TYPE /goog/cl_storage_v1=>ty_013,
          ls_output_object TYPE /goog/cl_storage_v1=>ty_013.

    DATA: lt_bin   TYPE STANDARD TABLE OF sdokcntbin,
          lt_infos TYPE STANDARD TABLE OF scms_acinf.

    FIELD-SYMBOLS: <lv_contrep> TYPE c,
                   <lv_docid>   TYPE c,
                   <lv_compid>  TYPE c,
                   <lv_docprot> TYPE c,
                   <ls_info>    TYPE scms_acinf.

    get_parameter_c( EXPORTING iv_name      = 'contRep'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_docid ).
    get_parameter_c( EXPORTING iv_name      = 'compId'
                               iv_mandatory = 'S'
                     IMPORTING ev_value_s   = lv_compid
                               er_value     = lo_compid ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.
    ASSIGN lo_compid->*  TO <lv_compid>.

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    get_parameter_c( EXPORTING iv_name      = 'docProt'
                               iv_default   = gv_docprot
                               iv_mandatory = 's'
                     IMPORTING er_value     = lo_docprot ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.
    ASSIGN lo_compid->*  TO <lv_compid>.
    ASSIGN lo_docprot->* TO <lv_docprot>.

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'c' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    load_goog_repo_config( <lv_contrep> ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lo_entity = go_server->request.
    lv_data = lo_entity->get_data( ).
    lv_mimetype = lo_entity->get_header_field( name = 'Content-Type' ) ##NO_TEXT.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_data
*       APPEND_TO_TABLE = ' '
      IMPORTING
        output_length = ls_info-comp_size
      TABLES
        binary_tab    = lt_bin.

    ls_info-comp_id    = lv_compid.
    ls_info-mimetype   = lv_mimetype.
    ls_info-binary_flg = 'X'.
    ls_info-first_line = 1.
    DESCRIBE TABLE lt_bin LINES ls_info-last_line.

    APPEND ls_info TO lt_infos.

    lv_p_bucket = gs_repo_config-bucket.

    TRY.
        CREATE OBJECT lo_gcs
          EXPORTING
            iv_key_name = gs_repo_config-keyname.

        lv_object_name = <lv_docid> && '/' && <lv_docid>.

        GET REFERENCE OF lt_infos INTO ls_input_object-metadata.

        lo_gcs->insert_objects( EXPORTING iv_q_name       = lv_object_name
                                          iv_p_bucket     = lv_p_bucket
                                          is_input        = ls_input_object
                                          iv_content_type = 'text/plain'
                                IMPORTING es_output       = ls_output_object
                                          ev_ret_code     = lv_ret_code
                                          ev_err_text     = lv_err_text ).
        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

        lv_object_name = <lv_docid> && '/' && <lv_docid> && '-' && <lv_compid>.

        READ TABLE lt_infos ASSIGNING <ls_info>
          WITH KEY comp_id = <lv_compid>.                "#EC CI_STDSEQ
        IF sy-subrc = 0.
          GET REFERENCE OF <ls_info> INTO ls_input_object-metadata.
        ENDIF.

        lo_gcs->insert_objects( EXPORTING iv_q_name       = lv_object_name
                                          iv_p_bucket     = lv_p_bucket
                                          is_input        = ls_input_object
                                          is_data         = lv_data
                                          iv_content_type = lv_mimetype
                                IMPORTING es_output       = ls_output_object
                                          ev_ret_code     = lv_ret_code
                                          ev_err_text     = lv_err_text ).
        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_msg = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = 400
                   iv_text = lv_msg ).
        RETURN.
    ENDTRY.

    go_server->response->set_status( code   = 201
                                     reason = 'created' ).

  ENDMETHOD.


  METHOD process_create_post.

    DATA: lo_contrep       TYPE REF TO data,
          lo_docid         TYPE REF TO data,
          lo_docprot       TYPE REF TO data,
          lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk,
          lo_entity        TYPE REF TO if_http_entity.

    DATA: lv_pversion    TYPE string ##NEEDED,
          lv_seckey      TYPE string,
          lv_smand       TYPE c LENGTH 1,
          lv_signature   TYPE c LENGTH 1,
          lv_count       TYPE i,
          lv_mimetype    TYPE string,
          lv_compid      TYPE string,
          lv_data        TYPE xstring,
          lv_p_bucket    TYPE string,
          lv_object_name TYPE string,
          lv_ret_code    TYPE i,
          lv_err_text    TYPE string,
          lv_msg         TYPE string.

    DATA: ls_input_object    TYPE /goog/cl_storage_v1=>ty_013,
          ls_output_object   TYPE /goog/cl_storage_v1=>ty_013,
          ls_comp_data       TYPE gtyp_s_comp_data,
          ls_info            TYPE scms_acinf,
          ls_object_list     TYPE /goog/cl_storage_v1=>ty_083,
          ls_err_resp        TYPE /goog/err_resp,
          ls_managed_folders TYPE /goog/cl_storage_v1=>ty_082.

    DATA: lt_bin       TYPE STANDARD TABLE OF sdokcntbin,
          lt_infos     TYPE HASHED TABLE OF scms_acinf WITH UNIQUE KEY comp_id,
          lt_comp_data TYPE STANDARD TABLE OF gtyp_s_comp_data.

    FIELD-SYMBOLS: <lv_contrep>   TYPE c,
                   <lv_docid>     TYPE c,
                   <lv_docprot>   TYPE c,
                   <ls_comp_data> TYPE gtyp_s_comp_data,
                   <ls_info>      TYPE scms_acinf.

    get_parameter_c( EXPORTING iv_name      = 'contRep'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_docid ).

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    get_parameter_c( EXPORTING iv_name      = 'docProt'
                               iv_default   = gv_docprot
                               iv_mandatory = 's'
                     IMPORTING er_value     = lo_docprot ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.
    ASSIGN lo_docprot->* TO <lv_docprot>.

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'c' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    load_goog_repo_config( <lv_contrep> ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_count = go_server->request->num_multiparts( ).

    DO lv_count TIMES.
      lo_entity = go_server->request->get_multipart( index = sy-index ).
      lv_mimetype = lo_entity->get_header_field( name = 'Content-Type' ) ##NO_TEXT.
      lv_compid = lo_entity->get_header_field( name = 'X-compId' ) ##NO_TEXT.

      IF lv_compid IS INITIAL.  " workarround
        CONTINUE.
      ENDIF.

      lv_data = lo_entity->get_data( ).

      DESCRIBE TABLE lt_bin LINES ls_info-first_line.
      ls_info-first_line = ls_info-first_line + 1.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer          = lv_data
          append_to_table = 'X'
        IMPORTING
          output_length   = ls_info-comp_size
        TABLES
          binary_tab      = lt_bin.

      ls_info-comp_id    = lv_compid.
      ls_info-mimetype   = lv_mimetype.
      ls_info-binary_flg = 'X'.
      DESCRIBE TABLE lt_bin LINES ls_info-last_line.

      INSERT ls_info INTO TABLE lt_infos.

      ls_comp_data-comp_id   = lv_compid.
      ls_comp_data-mime_type = lv_mimetype.
      ls_comp_data-data      = lv_data.

      APPEND ls_comp_data TO lt_comp_data.
      CLEAR ls_comp_data.
    ENDDO.

    lv_p_bucket = gs_repo_config-bucket.

    TRY.
        CREATE OBJECT lo_gcs
          EXPORTING
            iv_key_name = gs_repo_config-keyname.

        lv_object_name = <lv_docid> && '/' && <lv_docid>.

        GET REFERENCE OF lt_infos INTO ls_input_object-metadata.

        lo_gcs->insert_objects( EXPORTING iv_q_name       = lv_object_name
                                          iv_p_bucket     = lv_p_bucket
                                          is_input        = ls_input_object
                                          iv_content_type = 'text/plain'
                                IMPORTING es_output       = ls_output_object
                                          ev_ret_code     = lv_ret_code
                                          ev_err_text     = lv_err_text ).
        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

        LOOP AT lt_comp_data ASSIGNING <ls_comp_data>.
          lv_object_name = <lv_docid> && '/' && <lv_docid> && '-' && <ls_comp_data>-comp_id.

          READ TABLE lt_infos ASSIGNING <ls_info>
               WITH KEY comp_id = <ls_comp_data>-comp_id.
          IF sy-subrc = 0.
            GET REFERENCE OF <ls_info> INTO ls_input_object-metadata.
          ENDIF.

          lo_gcs->insert_objects( EXPORTING iv_q_name       = lv_object_name
                                            iv_p_bucket     = lv_p_bucket
                                            is_input        = ls_input_object
                                            is_data         = <ls_comp_data>-data
                                            iv_content_type = <ls_comp_data>-mime_type
                                  IMPORTING es_output       = ls_output_object
                                            ev_ret_code     = lv_ret_code
                                            ev_err_text     = lv_err_text ).
          IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
            set_error( iv_code = lv_ret_code
                       iv_text = lv_err_text ).
            RETURN.
          ENDIF.

        ENDLOOP.
      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_msg = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = 400
                   iv_text = lv_msg ).
        RETURN.
    ENDTRY.

    go_server->response->set_status( code   = 201
                                     reason = 'created' ).
  ENDMETHOD.


  METHOD process_delete.

    DATA: lo_contrep       TYPE REF TO data,
          lo_docid         TYPE REF TO data,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk,
          lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_compid        TYPE REF TO data.

    DATA: lv_pversion       TYPE string ##NEEDED,
          lv_seckey         TYPE string,
          lv_smand          TYPE c LENGTH 1,
          lv_signature      TYPE c LENGTH 1,
          lv_p_bucket       TYPE string,
          lv_object_name    TYPE string,
          lv_ret_code       TYPE i,
          lv_err_text       TYPE string,
          lv_managed_folder TYPE string,

          lv_p_object       TYPE string,
          lv_delete_error   TYPE flag,
          lv_msg            TYPE string.

    DATA: ls_object_list    TYPE /goog/cl_storage_v1=>ty_016.

    FIELD-SYMBOLS: <lv_contrep> TYPE c,
                   <lv_docid>   TYPE c,
                   <lv_compid>  TYPE c,
                   <ls_item>    TYPE /goog/cl_storage_v1=>ty_013.

    get_parameter_c( EXPORTING iv_name      = 'contRep '
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_docid ).
    get_parameter_c( EXPORTING iv_name      = 'compId'
                               iv_mandatory = 's'
                     IMPORTING er_value     = lo_compid ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.
    ASSIGN lo_compid->*  TO <lv_compid>.

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'd' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    load_goog_repo_config( <lv_contrep> ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_p_bucket = gs_repo_config-bucket.

    TRY.
        CREATE OBJECT lo_gcs
          EXPORTING
            iv_key_name = gs_repo_config-keyname.

        IF <lv_compid> IS NOT INITIAL.

          lv_object_name = <lv_docid> && '/' && <lv_docid> && '-' && <lv_compid>.

          lv_object_name = cl_http_utility=>escape_url( lv_object_name ).

          lo_gcs->delete_objects( EXPORTING iv_p_bucket = lv_p_bucket
                                            iv_p_object = lv_object_name
                                  IMPORTING ev_ret_code = lv_ret_code
                                            ev_err_text = lv_err_text ).
        ELSE.

          lv_managed_folder = <lv_docid> && '/'.

          lo_gcs->list_objects( EXPORTING iv_q_prefix = lv_managed_folder
                                          iv_p_bucket = lv_p_bucket
                                IMPORTING es_output   = ls_object_list
                                          ev_ret_code = lv_ret_code
                                          ev_err_text = lv_err_text ).

          IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
            set_error( iv_code = lv_ret_code
                       iv_text = lv_err_text ).
            RETURN.
          ENDIF.

          LOOP AT ls_object_list-items ASSIGNING <ls_item>.
            lv_p_object = cl_http_utility=>escape_url( <ls_item>-name ).
            lo_gcs->delete_objects( EXPORTING iv_p_bucket = lv_p_bucket
                                              iv_p_object = lv_p_object
                                    IMPORTING ev_ret_code = lv_ret_code
                                              ev_err_text = lv_err_text ).

            IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
              lv_delete_error = abap_true.
              set_error( iv_code = lv_ret_code
                         iv_text = lv_err_text ).
              RETURN.
            ENDIF.
          ENDLOOP.
          IF sy-subrc = 4.
*            set_error( iv_code = '404'
*                       iv_text = 'Empty folder' ) ##NO_TEXT.
            "Delete the folder
            lo_gcs->delete_objects( EXPORTING iv_p_bucket = lv_p_bucket
                                              iv_p_object = lv_managed_folder
                                    IMPORTING ev_ret_code = lv_ret_code
                                              ev_err_text = lv_err_text ).
            IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
              lv_delete_error = abap_true.
              set_error( iv_code = lv_ret_code
                         iv_text = lv_err_text ).
              RETURN.
            ENDIF.
          ENDIF.

          IF lv_delete_error IS NOT INITIAL.
            RETURN.
          ENDIF.
        ENDIF.

        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_msg = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = 400
                   iv_text = lv_msg ).
        RETURN.

    ENDTRY.

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).

  ENDMETHOD.


  METHOD process_docget.
    TYPES:
      BEGIN OF lty_comp_data,
        comp_id TYPE sdok_filnm,
        data    TYPE xstring,
      END OF lty_comp_data,

      ltt_comp_data TYPE HASHED TABLE OF lty_comp_data WITH UNIQUE KEY comp_id.

    DATA: lo_contrep       TYPE REF TO data,
          lo_docid         TYPE REF TO data,
          lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_struc         TYPE REF TO cl_abap_structdescr,
          lo_entity        TYPE REF TO if_http_entity,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk.

    DATA: lv_contrep      TYPE string,
          lv_docid        TYPE string,
          lv_pversion     TYPE string,
          lv_seckey       TYPE string,
          lv_smand        TYPE c LENGTH 1,
          lv_signature    TYPE c LENGTH 1,
          lv_q_prefix     TYPE string,
          lv_data         TYPE xstring,
          lv_msg          TYPE string,
          lv_status       TYPE string,
          lv_inumcomps    TYPE i,
          lv_numcomps     TYPE string,
          lv_size         TYPE string,
          lv_mimetype     TYPE string,
          lv_compid       TYPE string,
          lv_p_bucket     TYPE string,
          lv_ret_code     TYPE i,
          lv_err_text     TYPE string,
          lv_crea_date    TYPE d,
          lv_crea_time    TYPE t,
          lv_chng_date    TYPE d,
          lv_chng_time    TYPE t,
          lv_dummy        TYPE string,
          lv_timec        TYPE string,
          lv_datec        TYPE string,
          lv_timem        TYPE string,
          lv_datem        TYPE string,
          lv_doc_status   TYPE scms_docst,
          lv_time_updated TYPE string,
          lv_time_created TYPE string,
          lv_bucket       TYPE string.

    DATA: ls_object_list   TYPE /goog/cl_storage_v1=>ty_016,
          ls_err_resp      TYPE /goog/err_resp,
          ls_output_object TYPE /goog/cl_storage_v1=>ty_013,
          ls_component     TYPE REF TO abap_compdescr,
          ls_metadata      TYPE scms_acinf,
          ls_info          TYPE scms_acinf,
          ls_comp_data     TYPE lty_comp_data,
          ls_data          TYPE xstring.

    DATA: lt_infos     TYPE STANDARD TABLE OF scms_acinf,
          lt_comp_data TYPE ltt_comp_data,
          lt_txt       TYPE STANDARD TABLE OF sdokcntasc,
          lt_bin       TYPE STANDARD TABLE OF sdokcntbin.

    FIELD-SYMBOLS: <lv_contrep>       TYPE c,
                   <lv_docid>         TYPE c,
                   <ls_metadata_comp> TYPE any,
                   <ls_source>        TYPE any,
                   <ls_target>        TYPE any,
                   <ls_source_val>    TYPE any,
                   <ls_comp_data>     TYPE lty_comp_data.

    get_parameter_c( EXPORTING iv_name      = 'contRep '
                               iv_mandatory = 'S'
                     IMPORTING ev_value_s   = lv_contrep
                               er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING ev_value_s   = lv_docid
                               er_value     = lo_docid ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'r' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    load_goog_repo_config( <lv_contrep> ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT lo_gcs EXPORTING iv_key_name = gs_repo_config-keyname.

        lv_q_prefix = <lv_docid>.
        lv_p_bucket = gs_repo_config-bucket.

        lo_gcs->list_objects( EXPORTING iv_q_prefix = lv_q_prefix
                                        iv_p_bucket = lv_p_bucket
                              IMPORTING es_output   = ls_object_list
                                        ev_ret_code = lv_ret_code
                                        ev_err_text = lv_err_text
                                        es_err_resp = ls_err_resp ).
        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ELSEIF ls_object_list-items IS INITIAL.
          set_error( iv_code = 404
                     iv_text = 'Document not found in the repository' ) ##NO_TEXT.
          RETURN.
        ELSE.

          FIELD-SYMBOLS: <ls_item> TYPE /goog/cl_storage_v1=>ty_013.
          LOOP AT ls_object_list-items ASSIGNING <ls_item>.
            lv_bucket = gs_repo_config-bucket.

            lo_gcs->add_common_qparam( iv_name  = 'alt'
                                       iv_value = 'media' ).

            lo_gcs->get_objects( EXPORTING iv_p_bucket = lv_bucket
                                           iv_p_object = <ls_item>-name
                                 IMPORTING es_output   = ls_output_object
                                           ev_ret_code = lv_ret_code
                                           ev_err_text = lv_err_text
                                           es_err_resp = ls_err_resp
                                           es_raw      = lv_data ).

            IF <ls_item>-name = <lv_docid> && '/' && <lv_docid>.
              lo_gcs->clear_all_common_qparams( ).

              lo_gcs->get_objects( EXPORTING iv_p_bucket = lv_bucket
                                             iv_p_object = <ls_item>-name
                                   IMPORTING es_output   = ls_output_object
                                             ev_ret_code = lv_ret_code
                                             ev_err_text = lv_err_text
                                             es_err_resp = ls_err_resp
                                             es_raw      = ls_data ).

              TRY.
                  lv_timec = substring( val = ls_output_object-time_created
                                        off = 11
                                        len = 8 ).
                  lv_datec = substring( val = ls_output_object-time_created
                                        off = 0
                                        len = 10 ).
                  lv_timem = substring( val = ls_output_object-updated
                                        off = 11
                                        len = 8 ).
                  lv_datem = substring( val = ls_output_object-updated
                                        off = 0
                                        len = 10 ).
                CATCH cx_sy_range_out_of_bounds.
                  " Do nothing
              ENDTRY.
              lv_doc_status = 0.

            ELSE.
              lo_gcs->clear_all_common_qparams( ).

              lo_gcs->get_objects( EXPORTING iv_p_bucket = lv_bucket
                                             iv_p_object = <ls_item>-name
                                   IMPORTING es_output   = ls_output_object
                                             ev_ret_code = lv_ret_code
                                             ev_err_text = lv_err_text
                                             es_err_resp = ls_err_resp
                                             es_raw      = ls_data ).

              IF ls_output_object-metadata IS BOUND.
                ASSIGN ls_output_object-metadata->* TO <ls_metadata_comp>.

                lo_struc ?= cl_abap_structdescr=>describe_by_data( <ls_metadata_comp> ).

                LOOP AT lo_struc->components REFERENCE INTO ls_component.

                  ASSIGN COMPONENT ls_component->name OF STRUCTURE <ls_metadata_comp> TO <ls_source>.
                  IF sy-subrc IS NOT INITIAL.
                    CONTINUE.
                  ENDIF.

                  ASSIGN COMPONENT ls_component->name OF STRUCTURE ls_metadata TO <ls_target>.
                  IF sy-subrc IS NOT INITIAL.
                    CONTINUE.
                  ENDIF.

                  ASSIGN <ls_source>->* TO <ls_source_val>.
                  <ls_target> = <ls_source_val>.

                ENDLOOP.
              ENDIF.

              MOVE-CORRESPONDING ls_metadata TO ls_info.
              TRY.
                  lv_time_created = ls_output_object-time_created(19).
                  ls_info-crea_time  = convert_str_to_time( lv_time_created ).

                  lv_time_updated = ls_output_object-updated(19).
                  ls_info-chng_time  = convert_str_to_time( lv_time_updated ).

                  APPEND ls_info TO lt_infos.
                CATCH cx_sy_range_out_of_bounds.
                  " Do nothing
              ENDTRY.

              ls_comp_data-comp_id = ls_info-comp_id.
              ls_comp_data-data    = lv_data.
              INSERT ls_comp_data INTO TABLE lt_comp_data.

            ENDIF.

          ENDLOOP.

        ENDIF.

      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_msg = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = 400
                   iv_text = lv_msg ).
        RETURN.

    ENDTRY.

    lv_status = format_status( iv_status = lv_doc_status ).

    lo_entity = go_server->response.
    DESCRIBE TABLE lt_infos LINES lv_inumcomps.
    lv_numcomps = lv_inumcomps.

    lo_entity->set_header_field( name  = 'Content-Type' ##NO_TEXT
                                 value = 'multipart/form-data' ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-dateC'
                                 value = lv_datec ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-timeC'
                                 value = lv_timec ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-dateM'
                                 value = lv_datec ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-timeM'
                                 value = lv_timec ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-numComps'
                                 value = lv_numcomps ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-contRep'
                                 value = lv_contrep ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-docId'
                                 value = lv_docid ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-docStatus'
                                 value = lv_status ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-pVersion'
                                 value = lv_pversion ) ##NO_TEXT.

    " Add new header field to disable auto sniffing by IE
    lo_entity->set_header_field( name  = 'X-Content-Type-Options'
                                 value = 'nosniff' ) ##NO_TEXT.

    LOOP AT lt_infos INTO ls_info.

      format_timestamp( EXPORTING iv_timestamp = ls_info-crea_time
                        IMPORTING ev_date      = lv_datec
                                  ev_time      = lv_timec ).
      format_timestamp( EXPORTING iv_timestamp = ls_info-chng_time
                        IMPORTING ev_date      = lv_datem
                                  ev_time      = lv_timem ).

      lv_size = ls_info-comp_size.
      lv_mimetype = ls_info-mimetype.
      lv_compid   = ls_info-comp_id.
      lv_status = 'online'.

      READ TABLE lt_comp_data ASSIGNING <ls_comp_data>
           WITH TABLE KEY comp_id = ls_info-comp_id.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      lv_data = <ls_comp_data>-data.
      lv_size = xstrlen( lv_data ).

      lo_entity = go_server->response->add_multipart( ).

      lo_entity->suppress_content_type( ).

      lo_entity->set_header_field( name  = 'Content-Type'
                                   value = lv_mimetype ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'Content-Length'
                                   value = lv_size ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-compId'
                                   value = lv_compid ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-Content-Length'
                                   value = lv_size ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-compDateC'
                                   value = lv_datec ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-compTimeC'
                                   value = lv_timec ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-compDateM'
                                   value = lv_datem ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-compTimeM'
                                   value = lv_timem ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-compStatus'
                                   value = lv_status ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-pVersion'
                                   value = lv_pversion ) ##NO_TEXT.

      " Add new header field to disable auto sniffing by IE
      lo_entity->set_header_field( name  = 'X-Content-Type-Options'
                                   value = 'nosniff' ) ##NO_TEXT.
      TRY.
          lo_entity->set_data( data = lv_data ).
        CATCH cx_vsi.
          MESSAGE e025(cms) WITH 'Virus scan detected '
                                 'an infected file '
                                 'while trying to access the file.'
                                 'Operation aborted'
                                 INTO lv_dummy ##NO_TEXT.
          sys_error_set( ).
          gs_error-status_code = 500.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD process_get.

    CONSTANTS: lc_default_compid_1 TYPE c LENGTH 4 VALUE 'data',
               lc_default_compid_2 TYPE c LENGTH 5 VALUE 'data1'.

    DATA: lo_contrep       TYPE REF TO data,
          lo_docid         TYPE REF TO data,
          lo_compid        TYPE REF TO data,
          lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_struc         TYPE REF TO cl_abap_structdescr,
          ls_component     TYPE REF TO abap_compdescr,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk,
          lo_entity        TYPE REF TO if_http_entity.

    DATA: lv_compid       TYPE string,
          lv_pversion     TYPE string,
          lv_fromoffset   TYPE i,
          lv_tooffset     TYPE i,
          lv_seckey       TYPE string,
          lv_smand        TYPE c LENGTH 1,
          lv_signature    TYPE c LENGTH 1,
          lv_p_bucket     TYPE string,
          lv_p_object     TYPE string,
          lv_ret_code     TYPE i,
          lv_err_text     TYPE string,
          lv_data         TYPE xstring,
          lv_msg          TYPE string,
          lv_datec        TYPE string,
          lv_timec        TYPE string,
          lv_datem        TYPE string,
          lv_timem        TYPE string,
          lv_size         TYPE i,
          lv_mimetype     TYPE string,
          lv_status       TYPE string,
          lv_length       TYPE i,
          lv_size_s       TYPE string,
          lv_length_s     TYPE string,
          lv_sdokprof     TYPE string,
          lv_contdisp     TYPE string,
          lv_contdisp1    TYPE string,
          lv_exp          TYPE tzntstmps,
          lv_time_stamp   TYPE tzntstmps,
          lv_diff         TYPE tzntstmpl,
          lv_max          TYPE string VALUE '86400',
          lv_cval         TYPE string,
          lv_comp_id      TYPE sdok_filnm,
          lv_file_name    TYPE sdok_filnm,
          lv_str2         TYPE string,
          lv_str1         TYPE string,
          lv_doc_prot     TYPE c LENGTH 10 VALUE '-',
          lv_s_doc_prot   TYPE string,
          lv_dummy        TYPE string,
          lv_tm           TYPE string,
          lv_dd           TYPE string,
          lv_time_created TYPE string,
          lv_time_updated TYPE string.

    DATA: ls_object_list   TYPE /goog/cl_storage_v1=>ty_016,
          ls_data          TYPE xstring,
          ls_err_resp      TYPE /goog/err_resp,
          ls_metadata      TYPE scms_acinf,
          ls_output_object TYPE /goog/cl_storage_v1=>ty_013,
          ls_info          TYPE scms_acinf.

    DATA: lt_txt   TYPE STANDARD TABLE OF sdokcntasc,
          lt_bin   TYPE STANDARD TABLE OF sdokcntbin,
          lt_infos TYPE STANDARD TABLE OF scms_acinf,
          lt_comps TYPE STANDARD TABLE OF string.

    FIELD-SYMBOLS: <lv_contrep>       TYPE c,
                   <lv_docid>         TYPE c,
                   <lv_compid>        TYPE c,
                   <ls_comp>          TYPE string,
                   <ls_metadata_comp> TYPE any,
                   <ls_source>        TYPE any,
                   <ls_target>        TYPE any,
                   <ls_source_val>    TYPE any.

    get_parameter_c( EXPORTING iv_name      = 'contRep '
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_docid ).
    get_parameter_c( EXPORTING iv_name    = 'compId'
                     IMPORTING ev_value_s = lv_compid
                               er_value   = lo_compid ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.
    ASSIGN lo_compid->*  TO <lv_compid>.

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_fromoffset = get_parameter_i( iv_name    = 'fromOffset'
                                     iv_default = 0 ).
    lv_tooffset = get_parameter_i( iv_name    = 'toOffset'
                                   iv_default = -1 ).
    lv_seckey = get_parameter( iv_name = 'secKey' ).

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'r' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    load_goog_repo_config( <lv_contrep> ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_p_bucket = gs_repo_config-bucket.

    TRY.
        CREATE OBJECT lo_gcs EXPORTING iv_key_name = gs_repo_config-keyname.

        IF lv_compid IS NOT INITIAL.
          APPEND lv_compid TO lt_comps.
        ELSE.
          APPEND lc_default_compid_1 TO lt_comps.
          APPEND lc_default_compid_2 TO lt_comps.
        ENDIF.

        LOOP AT lt_comps ASSIGNING <ls_comp>.

          lv_p_object = <lv_docid> && '/' && <lv_docid> && '-' && <ls_comp>.

          CLEAR lv_ret_code.
          CLEAR lv_err_text.
          CLEAR ls_output_object.
          lo_gcs->get_objects( EXPORTING iv_p_bucket = lv_p_bucket
                                         iv_p_object = lv_p_object
                               IMPORTING es_output   = ls_output_object
                                         ev_ret_code = lv_ret_code
                                         ev_err_text = lv_err_text ).

          IF lo_gcs->is_success( lv_ret_code ) = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

        lo_gcs->add_common_qparam( iv_name  = 'alt'
                                   iv_value = 'media' ).

        lo_gcs->get_objects( EXPORTING iv_p_bucket = lv_p_bucket
                                       iv_p_object = lv_p_object
                             IMPORTING ev_ret_code = lv_ret_code
                                       ev_err_text = lv_err_text
                                       es_raw      = lv_data ).

        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_msg = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = 400
                   iv_text = lv_msg ).
        RETURN.
    ENDTRY.

    ASSIGN ls_output_object-metadata->* TO <ls_metadata_comp>.

    lo_struc ?= cl_abap_structdescr=>describe_by_data( <ls_metadata_comp> ).

    LOOP AT lo_struc->components REFERENCE INTO ls_component.
      ASSIGN COMPONENT ls_component->name OF STRUCTURE <ls_metadata_comp> TO <ls_source>.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT ls_component->name OF STRUCTURE ls_metadata TO <ls_target>.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      ASSIGN <ls_source>->* TO <ls_source_val>.
      <ls_target> = <ls_source_val>.
    ENDLOOP.

    MOVE-CORRESPONDING ls_metadata TO ls_info.

    lv_time_created = ls_output_object-time_created(19).
    ls_info-crea_time  = convert_str_to_time( lv_time_created ).

    lv_time_updated = ls_output_object-updated(19).
    ls_info-chng_time  = convert_str_to_time( lv_time_updated ).

    ls_info-comp_id    = <ls_comp>.
    ls_info-binary_flg = 'X'.
    APPEND ls_info TO lt_infos.

    lo_entity = go_server->response.

    READ TABLE lt_infos INDEX 1 INTO ls_info.
    IF sy-subrc <> 0.
      gs_error-status_code = 500.
      EXIT.
    ENDIF.

    format_timestamp( EXPORTING iv_timestamp = ls_info-crea_time
                      IMPORTING ev_date      = lv_datec
                                ev_time      = lv_timec ).
    format_timestamp( EXPORTING iv_timestamp = ls_info-chng_time
                      IMPORTING ev_date      = lv_datem
                                ev_time      = lv_timem ).

    lv_size = ls_info-comp_size.
    lv_mimetype = ls_info-mimetype.
    lv_compid   = ls_info-comp_id.
    lv_status = 'online'.

    lv_size = xstrlen( lv_data ).

    IF lv_tooffset >= lv_size AND lv_fromoffset >= lv_size.
      lv_length = 0.
      CLEAR lv_data.
    ELSE.
      IF lv_tooffset < 0 OR lv_tooffset >= lv_size.
        lv_tooffset = lv_size - 1.
      ENDIF.

      IF lv_fromoffset < 0 OR lv_fromoffset >= lv_size.
        lv_fromoffset = lv_size - 1.
      ENDIF.

      IF lv_fromoffset > lv_tooffset.
        CLEAR lv_data.
        lv_length = 0.
      ELSEIF lv_fromoffset > 0 OR lv_tooffset < lv_size.
        lv_length = lv_tooffset - lv_fromoffset + 1.
        IF lv_length <= 0 OR lv_fromoffset < 0.
          lv_length = 0.
          CLEAR lv_data.
        ELSE.
          lv_data = lv_data+lv_fromoffset(lv_length).
        ENDIF.
      ELSE.
        lv_length = lv_size.
      ENDIF.
    ENDIF.

    lv_size_s = lv_size.
    lv_length_s = lv_length.
    " concatenate 'inline; filename="' lv_compid '"' into l_contdisp.
    CALL FUNCTION 'SDOK_PROFILE_READ'
      EXPORTING
        name      = 'CNTDISP'
      IMPORTING
        value     = lv_sdokprof
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF ( sy-subrc <> 0 ) OR ( lv_sdokprof IS INITIAL ).
      " Default Content-Disposition is attachment from BASIS 751 onwords.
      lv_contdisp = 'attachment' ##NO_TEXT.
    ELSE.
      lv_contdisp = lv_sdokprof.
    ENDIF.

    CONCATENATE lv_contdisp'; filename="' lv_compid '"' INTO lv_contdisp1 ##NO_TEXT.

    " Content-Disposition handling
    " the following behavior is realized:
    " auto or default: Send content-disposition only, if compId has an extension.
    " on: Force content-disposition for all compId's
    " off: Turn off content-disposition for all compId's

    IF gv_cd_mode = ' '.
      CALL FUNCTION 'SDOK_PROFILE_READ'
        EXPORTING
          name      = 'CNTDISPMOD'
*         NAME2     =
*         UNAME     = SY-UNAME
*         MANDT     = SY-MANDT
        IMPORTING
          value     = lv_sdokprof
*         VALUE_TAB =
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF ( sy-subrc <> 0 ) OR ( lv_sdokprof IS INITIAL ).
        gv_cd_mode = 'A' ##NO_TEXT.
      ELSE.
        lv_sdokprof = to_upper( lv_sdokprof ).
        CASE lv_sdokprof.
          WHEN 'OFF'.
            gv_cd_mode = 'N' ##NO_TEXT.
          WHEN 'ON'.
            gv_cd_mode = 'Y' ##NO_TEXT.
          WHEN OTHERS.
            gv_cd_mode = 'A' ##NO_TEXT.
        ENDCASE.
      ENDIF.
    ENDIF.

    lo_entity->set_header_field( name  = 'Content-Type'
                                 value = lv_mimetype ) ##NO_TEXT.

    " To enable Cache-Control for /sap/bc/contentserver handler
    FIND REGEX `image/*` IN lv_mimetype.
    IF sy-subrc = 0.
      lv_exp = get_parameter( iv_name = 'expiration' ).
      IF lv_exp IS NOT INITIAL.
        TRY.
            GET TIME STAMP FIELD lv_time_stamp.
            lv_diff = cl_abap_tstmp=>subtract( tstmp1 = lv_exp
                                               tstmp2 = lv_time_stamp ).
          CATCH cx_parameter_invalid_range.
          CATCH cx_parameter_invalid_type.
        ENDTRY.
        IF lv_diff > 0 AND lv_diff < 86400.
          lv_max = ceil( lv_diff ).
        ENDIF.
      ENDIF.

      CONCATENATE 'private, max-age=' lv_max INTO lv_cval ##NO_TEXT.

      lo_entity->set_header_field( name  = 'Cache-Control'
                                   value = lv_cval ) ##NO_TEXT.

    ENDIF.

    " Add new header field to disable auto sniffing by IE
    lo_entity->set_header_field( name  = 'X-Content-Type-Options'
                                 value = 'nosniff' ) ##NO_TEXT.

    CASE gv_cd_mode.
      WHEN 'Y'.

        lv_comp_id = lv_compid.
        PERFORM get_subs_comp_names IN PROGRAM saplscms_api USING    lv_comp_id
                                                            CHANGING    lv_file_name.

        CONCATENATE lv_contdisp'; filename="' lv_compid '"' INTO lv_contdisp1 ##NO_TEXT.

        lo_entity->set_header_field( name  = 'Content-Disposition'
                                     value = lv_contdisp1 ) ##NO_TEXT.

      WHEN 'A' OR ' '.
        IF lv_compid CA '.'.
          lv_str2 = lv_compid.
          WHILE lv_str2 CA '.'.
            SPLIT lv_str2 AT '.' INTO lv_str1 lv_str2.
          ENDWHILE.
          IF NOT lv_str2 CA '/\'.
            lo_entity->set_header_field( name  = 'Content-Disposition'
                                         value = lv_contdisp1 ) ##NO_TEXT.
          ENDIF.
        ENDIF.
    ENDCASE.

    lo_entity->set_header_field( name  = 'X-compId'
                                 value = lv_compid ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-Content-Length'
                                 value = lv_size_s ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-compDateC'
                                 value = lv_datec ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-compTimeC'
                                 value = lv_timec ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-compDateM'
                                 value = lv_datem ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-compTimeM'
                                 value = lv_timem ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-compStatus'
                                 value = lv_status ) ##NO_TEXT.
    lo_entity->set_header_field( name  = 'X-pVersion'
                                 value = lv_pversion ) ##NO_TEXT.

    " Add new header field to disable auto sniffing by IE
    lo_entity->set_header_field( name  = 'X-Content-Type-Options'
                                 value = 'nosniff' ) ##NO_TEXT.

    IF lv_doc_prot <> '-'.
      lv_s_doc_prot = lv_doc_prot.
      lo_entity->set_header_field( name  = 'X-docProt'
                                   value = lv_s_doc_prot ) ##NO_TEXT.
    ENDIF.

    TRY.
        lo_entity->set_data( data = lv_data ).
      CATCH cx_vsi.
        MESSAGE e025(cms) WITH 'Virus scan detected '
                               'an infected file '
                               'while trying to access the file.'
                               'Operation aborted'
                               INTO lv_dummy ##NO_TEXT.
        sys_error_set( ).
        gs_error-status_code = 500.
        RETURN.
    ENDTRY.

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).

  ENDMETHOD.


  METHOD process_info.

    DATA: lo_docid         TYPE REF TO data,
          lo_contrep       TYPE REF TO data,
          lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_struc         TYPE REF TO cl_abap_structdescr,
          lo_component     TYPE REF TO abap_compdescr,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk,
          lo_entity        TYPE REF TO if_http_entity.

    DATA: lv_contrep          TYPE string,
          lv_docid            TYPE string,
          lv_resultas         TYPE string,
          lv_pversion         TYPE string,
          lv_compid           TYPE string,
          lv_seckey           TYPE string,
          lv_smand            TYPE c LENGTH 1,
          lv_signature        TYPE c LENGTH 1,
          lv_p_bucket         TYPE string,
          lv_q_prefix         TYPE string,
          lv_ret_code         TYPE i,
          lv_err_text         TYPE string,
          lv_p_object         TYPE string,
          lv_object_get_error TYPE flag,
          lv_timec            TYPE string,
          lv_datec            TYPE string,
          lv_timem            TYPE string,
          lv_datem            TYPE string,
          lv_msg              TYPE string,
          lv_status           TYPE string,
          lv_inumcomps        TYPE i,
          lv_numcomps         TYPE string,
          lv_size             TYPE string,
          lv_mimetype         TYPE string,
          lv_crea_date        TYPE d,
          lv_crea_time        TYPE t,
          lv_chng_date        TYPE d,
          lv_chng_time        TYPE t,
          lv_time_created     TYPE string,
          lv_time_updated     TYPE string.

    DATA: ls_object_list   TYPE /goog/cl_storage_v1=>ty_016,
          ls_err_resp      TYPE /goog/err_resp,
          ls_item_append   TYPE /goog/cl_storage_v1=>ty_013,
          ls_doc_status    TYPE scms_docst,
          ls_meta          TYPE scms_acinf,
          ls_comp          TYPE scms_stinf,
          ls_output_object TYPE /goog/cl_storage_v1=>ty_013.

    DATA: lt_comps TYPE STANDARD TABLE OF scms_stinf.

    FIELD-SYMBOLS:<lv_contrep>       TYPE c,
                  <lv_docid>         TYPE c,
                  <ls_item>          TYPE /goog/cl_storage_v1=>ty_013,
                  <ls_metadata_comp> TYPE any,
                  <ls_source>        TYPE any,
                  <ls_target>        TYPE any,
                  <ls_source_val>    TYPE any.

    get_parameter_c( EXPORTING iv_name      = 'contRep '
                               iv_mandatory = 'S'
                     IMPORTING ev_value_s   = lv_contrep
                               er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING ev_value_s   = lv_docid
                               er_value     = lo_docid ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.

    lv_resultas = get_parameter( iv_name    = 'resultAs'
                                 iv_default = 'ascii' ).
    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_compid   = get_parameter( iv_name = 'compId' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'r' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    load_goog_repo_config( <lv_contrep> ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA ls_data TYPE xstring.

    lv_p_bucket = gs_repo_config-bucket.

    TRY.
        CREATE OBJECT lo_gcs
          EXPORTING
            iv_key_name = gs_repo_config-keyname.

        lv_q_prefix = <lv_docid>.
        lv_p_bucket = gs_repo_config-bucket.

        IF lv_compid IS INITIAL.

          lo_gcs->list_objects( EXPORTING iv_q_prefix = lv_q_prefix
                                          iv_p_bucket = lv_p_bucket
                                IMPORTING es_output   = ls_object_list
                                          ev_ret_code = lv_ret_code
                                          ev_err_text = lv_err_text
                                          es_err_resp = ls_err_resp ).
          IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
            set_error( iv_code = lv_ret_code
                       iv_text = lv_err_text ).
            RETURN.
          ELSEIF ls_object_list-items IS INITIAL.
            set_error( iv_code = 404
                       iv_text = 'Document not found in the repository' ) ##NO_TEXT.
            RETURN.
          ENDIF.
        ELSE.
          " Doc ID and Comp ID only
          ls_item_append-name = <lv_docid> && '/' && <lv_docid>.
          APPEND ls_item_append TO ls_object_list-items.

          ls_item_append-name = <lv_docid> && '/' && <lv_docid> && '-' && lv_compid.
          APPEND ls_item_append TO ls_object_list-items.
        ENDIF.

        LOOP AT ls_object_list-items ASSIGNING <ls_item>.
          lo_gcs->get_objects( EXPORTING iv_p_bucket = lv_p_bucket
                                         iv_p_object = <ls_item>-name
                               IMPORTING es_output   = ls_output_object
                                         ev_ret_code = lv_ret_code
                                         ev_err_text = lv_err_text ).

          IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
            lv_object_get_error = abap_true.
            set_error( iv_code = lv_ret_code
                       iv_text = lv_err_text ).
            RETURN.
          ENDIF.

          IF <ls_item>-name = <lv_docid> && '/' && <lv_docid>.

            lv_timec = substring( val = ls_output_object-time_created
                                  off = 11
                                  len = 8 ).
            lv_datec = substring( val = ls_output_object-time_created
                                  off = 0
                                  len = 10 ).
            lv_timem = substring( val = ls_output_object-updated
                                  off = 11
                                  len = 8 ).
            lv_datem = substring( val = ls_output_object-updated
                                  off = 0
                                  len = 10 ).
            ls_doc_status = 0.

          ELSE.

            DATA: ls_metadata TYPE scms_acinf.

            ASSIGN ls_output_object-metadata->* TO <ls_metadata_comp>.
            lo_struc ?= cl_abap_structdescr=>describe_by_data( <ls_metadata_comp> ).
            LOOP AT lo_struc->components REFERENCE INTO lo_component.
              ASSIGN COMPONENT lo_component->name OF STRUCTURE <ls_metadata_comp> TO <ls_source>.
              IF sy-subrc IS NOT INITIAL.
                CONTINUE.
              ENDIF.

              ASSIGN COMPONENT lo_component->name OF STRUCTURE ls_metadata TO <ls_target>.
              IF sy-subrc IS NOT INITIAL.
                CONTINUE.
              ENDIF.

              ASSIGN <ls_source>->* TO <ls_source_val>.
              <ls_target> = <ls_source_val>.
            ENDLOOP.

            MOVE-CORRESPONDING ls_metadata TO ls_comp.

            lv_time_created = ls_output_object-time_created(19).
            ls_comp-crea_time = convert_str_to_time( lv_time_created ).

            lv_time_updated = ls_output_object-updated(19).
            ls_comp-chng_time = convert_str_to_time( lv_time_updated ).
            ls_comp-status    = 0.
            APPEND ls_comp TO lt_comps.
            CLEAR ls_comp.

          ENDIF.
        ENDLOOP.

        IF lv_object_get_error IS NOT INITIAL.
          RETURN.
        ENDIF.

      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_msg = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = 400
                   iv_text = lv_msg ).
        RETURN.
    ENDTRY.

    IF lv_compid IS NOT INITIAL.
      lv_compid = to_lower( lv_compid ).
      LOOP AT lt_comps INTO ls_comp.
        ls_comp-comp_id = to_lower( ls_comp-comp_id ).
        IF ls_comp-comp_id <> lv_compid.
          DELETE lt_comps.
        ENDIF.
      ENDLOOP.
      IF lt_comps[] IS INITIAL.
        gs_error-status_code = 404.
        EXIT.
      ENDIF.
    ENDIF.

    lv_status = format_status( iv_status = ls_doc_status ).

    lo_entity = go_server->response.
    IF lv_resultas = 'html'.
      lo_entity->set_header_field( name  = 'Content-Type'
                                   value = 'iv_text/plain' ) ##NO_TEXT.

      CONCATENATE
        'X-DateC:'      gv_sp lv_datec  gv_nl ##NO_TEXT
        'X-TimeC:'      gv_sp lv_timec  gv_nl ##NO_TEXT
        'X-DateM:'      gv_sp lv_datem  gv_nl ##NO_TEXT
        'X-TimeM:'      gv_sp lv_timem  gv_nl ##NO_TEXT
        'X-CompStatus:' gv_sp lv_status gv_nl ##NO_TEXT
        INTO gv_body.
    ELSE.
      DESCRIBE TABLE lt_comps LINES lv_inumcomps.
      lv_numcomps = lv_inumcomps.

      lo_entity->set_header_field( name  = 'Content-Type' ##NO_TEXT
                                   value = 'multipart/form-data' ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-dateC'
                                   value = lv_datec ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-timeC'
                                   value = lv_timec ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-dateM'
                                   value = lv_datem ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-timeM'
                                   value = lv_timem ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-contRep'
                                   value = lv_contrep ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-numberComps'
                                   value = lv_numcomps ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-docId'
                                   value = lv_docid ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-docStatus'
                                   value = lv_status ) ##NO_TEXT.
      lo_entity->set_header_field( name  = 'X-pVersion'
                                   value = lv_pversion ) ##NO_TEXT.
    ENDIF.

    LOOP AT lt_comps INTO ls_comp.

      format_timestamp( EXPORTING iv_timestamp = ls_comp-crea_time
                        IMPORTING ev_date      = lv_datec
                                  ev_time      = lv_timec ).
      format_timestamp( EXPORTING iv_timestamp = ls_comp-chng_time
                        IMPORTING ev_date      = lv_datem
                                  ev_time      = lv_timem ).

      lv_size = ls_comp-comp_size.
      lv_mimetype = ls_comp-mimetype.
      lv_compid   = ls_comp-comp_id.
      lv_status = format_status( iv_status = ls_comp-status ).

      IF lv_resultas = 'html'.
        CONCATENATE
          gv_body gv_nl
          'Content-Type:'     gv_sp lv_mimetype gv_nl ##NO_TEXT
          'Content-Length:'   gv_sp '0'      gv_nl ##NO_TEXT
          'X-CompId:'         gv_sp lv_compid   gv_nl ##NO_TEXT
          'X-Content-Length:' gv_sp lv_size   gv_nl ##NO_TEXT
          'X-compDateC:'      gv_sp lv_datec  gv_nl ##NO_TEXT
          'X-compTimeC:'      gv_sp lv_timec  gv_nl ##NO_TEXT
          'X-compDateM:'      gv_sp lv_datem  gv_nl ##NO_TEXT
          'X-compTimeM:'      gv_sp lv_timem  gv_nl ##NO_TEXT
          'X-compStatus:'     gv_sp lv_status gv_nl ##NO_TEXT
          'X-pVersion:'       gv_sp lv_pversion gv_nl ##NO_TEXT
          INTO gv_body.

      ELSE.
        lo_entity = go_server->response->add_multipart( ).

        lo_entity->suppress_content_type( ).

        IF lv_mimetype IS NOT INITIAL.
          lo_entity->set_header_field( name  = 'Content-Type'
                                       value = lv_mimetype ) ##NO_TEXT.
        ENDIF.
        lo_entity->set_header_field( name  = 'Content-Length'
                                     value = '0' ) ##NO_TEXT.
        lo_entity->set_header_field( name  = 'X-compId'
                                     value = lv_compid ) ##NO_TEXT.
        lo_entity->set_header_field( name  = 'X-Content-Length'
                                     value = lv_size ) ##NO_TEXT.
        lo_entity->set_header_field( name  = 'X-compDateC'
                                     value = lv_datec ) ##NO_TEXT.
        lo_entity->set_header_field( name  = 'X-compTimeC'
                                     value = lv_timec ) ##NO_TEXT.
        lo_entity->set_header_field( name  = 'X-compDateM'
                                     value = lv_datem ) ##NO_TEXT.
        lo_entity->set_header_field( name  = 'X-compTimeM'
                                     value = lv_timem ) ##NO_TEXT.
        lo_entity->set_header_field( name  = 'X-compStatus'
                                     value = lv_status ) ##NO_TEXT.
        lo_entity->set_header_field( name  = 'X-pVersion'
                                     value = lv_pversion ) ##NO_TEXT.

      ENDIF.
    ENDLOOP.

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).
  ENDMETHOD.


  METHOD process_mcreate_post.
    DATA: lo_contrep       TYPE REF TO data,
          lo_docprot       TYPE REF TO data,
          lo_docid         TYPE REF TO data,
          lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk,
          lo_entity        TYPE REF TO if_http_entity.

    DATA: lv_docid       TYPE string,
          lv_pversion    TYPE string ##NEEDED,
          lv_seckey      TYPE string,
          lv_smand       TYPE c LENGTH 1,
          lv_signature   TYPE c LENGTH 1,
          lv_last_docid  TYPE string,
          lv_count       TYPE i,
          lv_subrc       LIKE sy-subrc,
          lv_error_descr TYPE string,
          lv_mimetype    TYPE string,
          lv_compid      TYPE string,
          lv_data        TYPE xstring,
          lv_ret_code    TYPE i,
          lv_err_text    TYPE string,
          lv_code        TYPE string,
          lv_err_flag    TYPE c LENGTH 1 VALUE '',
          lv_msg         TYPE string,
          lv_len         TYPE i,
          lv_p_bucket    TYPE string,
          lv_object_name TYPE string.

    DATA: ls_input_object    TYPE /goog/cl_storage_v1=>ty_013,
          ls_info            TYPE scms_acinf,
          ls_comp_data       TYPE gtyp_s_comp_data,
          ls_output_object   TYPE /goog/cl_storage_v1=>ty_013,
          ls_object_list     TYPE /goog/cl_storage_v1=>ty_083,
          ls_err_resp        TYPE /goog/err_resp,
          ls_managed_folders TYPE /goog/cl_storage_v1=>ty_082.

    DATA: lt_infos     TYPE STANDARD TABLE OF scms_acinf,
          lt_comp_data TYPE STANDARD TABLE OF gtyp_s_comp_data,
          lt_bin       TYPE STANDARD TABLE OF sdokcntbin.

    FIELD-SYMBOLS: <lv_contrep>   TYPE c,
                   <lv_docprot>   TYPE c,
                   <lv_docid>     TYPE c,
                   <ls_comp_data> TYPE gtyp_s_comp_data,
                   <ls_info>      TYPE scms_acinf.

    get_parameter_c( EXPORTING iv_name      = 'contRep'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_contrep ).

    lv_docid = get_parameter( iv_name      = 'docId'
                              iv_mandatory = 'S' ).
    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    get_parameter_c( EXPORTING iv_name      = 'docProt'
                               iv_default   = gv_docprot
                               iv_mandatory = 's'
                     IMPORTING er_value     = lo_docprot ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docprot->* TO <lv_docprot>.

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'c' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    CLEAR lv_last_docid.
    lv_count = go_server->request->num_multiparts( ).

    load_goog_repo_config( <lv_contrep> ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_subrc = 0.

    lv_count = lv_count + 1.
    DO lv_count TIMES.
      IF sy-index < lv_count.
        lo_entity = go_server->request->get_multipart( index = sy-index ).
        lv_mimetype = lo_entity->get_header_field( name = 'Content-Type' ) ##NO_TEXT.
        lv_compid = lo_entity->get_header_field( name = 'X-compId' ) ##NO_TEXT.
        lv_docid = lo_entity->get_header_field( name = 'X-docId' ) ##NO_TEXT.

        IF lv_compid IS INITIAL.   " workarround
          CONTINUE.
        ENDIF.

        lv_data = lo_entity->get_data( ).

      ELSE.
        CLEAR lv_docid.
      ENDIF.

      IF lt_infos IS INITIAL.
        lv_last_docid = lv_docid.
      ENDIF.

      IF lv_docid <> lv_last_docid.

        lv_len = strlen( lv_last_docid ).
        IF lv_len = 0. lv_len = 1. ENDIF.
        CREATE DATA lo_docid TYPE c LENGTH lv_len.
        ASSIGN lo_docid->* TO <lv_docid>.
        <lv_docid> = lv_last_docid.

        lv_p_bucket = gs_repo_config-bucket.

        TRY.
            CREATE OBJECT lo_gcs
              EXPORTING
                iv_key_name = gs_repo_config-keyname.
            lv_object_name = <lv_docid> && '/' && <lv_docid>.

            GET REFERENCE OF lt_infos INTO ls_input_object-metadata.

            lo_gcs->insert_objects( EXPORTING iv_q_ifgenerationmatch = '0'
                                              iv_q_name              = lv_object_name
                                              iv_p_bucket            = lv_p_bucket
                                              is_input               = ls_input_object
                                    IMPORTING es_output              = ls_output_object
                                              ev_ret_code            = lv_ret_code
                                              ev_err_text            = lv_err_text ).
            IF lv_ret_code = 412.
              sy-subrc = 0.
              lv_subrc = 403.
              lv_code  = 403.
            ELSEIF lo_gcs->is_success( lv_ret_code ) = abap_true.
              lv_code = 201.
            ELSE.
              lv_err_flag = 'X'.
              lv_msg = lv_err_text.
            ENDIF.

            IF lv_err_flag IS NOT INITIAL.
              CONTINUE.
            ENDIF.
            LOOP AT lt_comp_data ASSIGNING <ls_comp_data>.
              lv_object_name = <lv_docid> && '/' && <lv_docid> && '-' && <ls_comp_data>-comp_id.

              READ TABLE lt_infos ASSIGNING <ls_info>
                WITH KEY comp_id = <ls_comp_data>-comp_id. "#EC CI_STDSEQ
              IF sy-subrc = 0.
                GET REFERENCE OF <ls_info> INTO ls_input_object-metadata.
              ENDIF.

              lo_gcs->insert_objects( EXPORTING iv_q_ifgenerationmatch = '0'
                                                iv_q_name              = lv_object_name
                                                iv_p_bucket            = lv_p_bucket
                                                is_input               = ls_input_object
                                                is_data                = <ls_comp_data>-data
                                                iv_content_type        = <ls_comp_data>-mime_type
                                      IMPORTING es_output              = ls_output_object
                                                ev_ret_code            = lv_ret_code
                                                ev_err_text            = lv_err_text ).
              IF lv_ret_code = 412.
                sy-subrc = 0.
                lv_subrc = 403.
                lv_code  = 403.
              ELSEIF lo_gcs->is_success( lv_ret_code ) = abap_true.
                lv_code = 201.
              ELSE.
                lv_err_flag = 'X'.
                lv_msg = lv_err_text.
              ENDIF.
            ENDLOOP.

          CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
            lv_msg = lo_abap_sdk_excp->get_text( ).
            lv_code = 400.
            lv_err_flag = 'X'.
        ENDTRY.
        IF lv_err_flag = 'X'.
          lv_code = sy-subrc.
          lv_error_descr = lv_msg.
          gs_error-status_code = 400.
          sys_error_set( ).
        ELSE.
          CLEAR lv_error_descr.
        ENDIF.
        CONCATENATE 'errorDescription="' lv_error_descr '";'
                    INTO lv_error_descr.
        CONCATENATE gv_body
                    'docId="'     lv_last_docid
                    '";retCode="' lv_code '";'
                    lv_error_descr gv_nl
                    INTO gv_body.
        CLEAR: lt_infos[],
               lt_bin[],
               lt_comp_data[],
               lv_err_flag.
        lv_last_docid = lv_docid.
      ENDIF.

      IF lv_compid IS INITIAL.
        CONTINUE.
      ENDIF.

      DESCRIBE TABLE lt_bin LINES ls_info-first_line.
      ls_info-first_line = ls_info-first_line + 1.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer          = lv_data
          append_to_table = 'X'
        IMPORTING
          output_length   = ls_info-comp_size
        TABLES
          binary_tab      = lt_bin.

      ls_info-comp_id    = lv_compid.
      ls_info-mimetype   = lv_mimetype.
      ls_info-binary_flg = 'X'.
      DESCRIBE TABLE lt_bin LINES ls_info-last_line.

      APPEND ls_info TO lt_infos.
      ls_comp_data-comp_id   = lv_compid.
      ls_comp_data-mime_type = lv_mimetype.
      ls_comp_data-data      = lv_data.

      APPEND ls_comp_data TO lt_comp_data.
      CLEAR ls_comp_data.

    ENDDO.

    IF lv_subrc = 403.
      go_server->response->set_status( code   = 250
                                       reason = 'missing documents created' ) ##NO_TEXT.
    ELSE.
      go_server->response->set_status( code   = 201
                                       reason = 'created' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.


  METHOD process_putcert.
    DATA lo_contrep TYPE REF TO data.
    DATA lv_pversion  TYPE string ##NEEDED.
    DATA lv_authid    TYPE string.
    DATA lo_entity    TYPE REF TO if_http_entity.
    DATA lv_data      TYPE xstring.

    FIELD-SYMBOLS <lv_contrep> TYPE c.

    get_parameter_c( EXPORTING iv_name      = 'contRep '
                               iv_mandatory = 's'
                     IMPORTING er_value     = lo_contrep ).

    ASSIGN lo_contrep->* TO <lv_contrep>.

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_authid = get_parameter( iv_name      = 'authId'
                               iv_mandatory = 'X' ).

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lo_entity = go_server->request.

    lv_data = lo_entity->get_data( vscan_scan_always = 'X' ).

    CALL FUNCTION 'SCMS_RACL_PUT_CERT'
      EXPORTING
        contrep = <lv_contrep>
        data    = lv_data
        authid  = lv_authid
      EXCEPTIONS
        OTHERS  = 500.

    IF sy-subrc <> 0.
      gs_error-status_code = sy-subrc.
      sys_error_set( ).
      EXIT.
    ENDIF.

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).
  ENDMETHOD.


  METHOD process_rep_certget.
    DATA lo_contrep   TYPE REF TO data.
    DATA lv_pversion    TYPE string ##NEEDED.
    DATA lv_newcerts    TYPE c LENGTH 1.
    DATA lt_racls       TYPE STANDARD TABLE OF crep_racl.
    DATA ls_crep_http   TYPE crep_http.
    DATA lv_security    TYPE string.
    DATA ls_racl        TYPE crep_racl.
    DATA lv_fingerprint TYPE string.
    DATA lv_subject     TYPE string.
    DATA lv_issuer      TYPE string.
    DATA lv_serialno    TYPE string.
    DATA lv_valid_since TYPE string.
    DATA lv_valid_until TYPE string.
    DATA lv_certificate TYPE xstring.
    DATA lv_cert64      TYPE string.

    FIELD-SYMBOLS <lv_contentrep> TYPE c.

    get_parameter_c( EXPORTING iv_name      = 'contRep '
                               iv_mandatory = 's'
                     IMPORTING er_value     = lo_contrep ).

    ASSIGN lo_contrep->* TO <lv_contentrep>.

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    IF gv_command = 'getCert'.
      lv_newcerts = space.
    ELSE.
      check_authority( iv_read = 1 ).
      IF gs_error IS NOT INITIAL.
        RETURN.
      ENDIF.
      lv_newcerts = get_parameter_b( iv_name      = 'newCerts'
                                     iv_mandatory = 'X' ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_RACL_GET'
      EXPORTING
        contrep      = <lv_contentrep>
      TABLES
        fingerprints = lt_racls.

    go_server->response->set_header_field( name  = 'Content-Type'
                                           value = 'text/plain' ) ##NO_TEXT.

    IF gv_command = 'getCert'.
      PERFORM crep_select IN PROGRAM saplscms_url USING    gv_mandt
                                                           <lv_contentrep>
                                                  CHANGING ls_crep_http.

      IF ls_crep_http-http_nosig = 'X'.
        lv_security = '0'.
      ELSE.
        lv_security = '1'.
      ENDIF.
      body_add_field( iv_name  = 'security'
                      iv_value = lv_security ).
      CONCATENATE gv_body gv_nl INTO gv_body.
    ENDIF.

    LOOP AT lt_racls INTO ls_racl WHERE active <> lv_newcerts. "#EC CI_STDSEQ
      lv_fingerprint = ls_racl-fingerp.
      CALL FUNCTION 'SCMS_RACL_INFO'
        EXPORTING
          fingerprint = lv_fingerprint
        IMPORTING
          subject     = lv_subject
          issuer      = lv_issuer
          serialno    = lv_serialno
          valid_since = lv_valid_since
          valid_until = lv_valid_until
        EXCEPTIONS
          not_found   = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      body_add_field( iv_name  = 'subject'
                      iv_value = lv_subject ).
      body_add_field( iv_name  = 'issuer'
                      iv_value = lv_issuer ).
      body_add_field( iv_name  = 'serialNumber'
                      iv_value = lv_serialno ).
      body_add_field( iv_name  = 'notBefore'
                      iv_value = lv_valid_since ).
      body_add_field( iv_name  = 'notAfter'
                      iv_value = lv_valid_until ).
      body_add_field( iv_name  = 'keyInfo'
                      iv_value = lv_fingerprint ).

      IF gv_command = 'getCert'.
        CALL FUNCTION 'SCMS_CERT_GET'
          EXPORTING
            fingerprint = lv_fingerprint
          IMPORTING
            data        = lv_certificate
          EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.
        IF sy-subrc = 0.
          lv_cert64 = lv_certificate.
          body_add_field( iv_name  = 'certificate'
                          iv_value = lv_cert64 ).
        ENDIF.
      ENDIF.
      CONCATENATE gv_body gv_nl INTO gv_body.

    ENDLOOP.

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).
  ENDMETHOD.


  METHOD process_rep_certset.
    DATA lo_content TYPE REF TO data.
    DATA lv_pversion  TYPE string ##NEEDED.
    DATA lv_cdata     TYPE string.
    DATA lv_active    TYPE c LENGTH 1.
    DATA lv_line      TYPE string.
    DATA lv_key       TYPE string.
    DATA lv_value     TYPE string.
    DATA ls_racl      TYPE crep_racl.
    DATA lt_racls     TYPE STANDARD TABLE OF crep_racl.

    FIELD-SYMBOLS <lv_contentrep> TYPE c.

    DATA: BEGIN OF ls_info,
            subject     TYPE string,
            issuer      TYPE string,
            serialno    TYPE string,
            valid_from  TYPE string,
            valid_until TYPE string,
            fingerprint TYPE string,
          END OF ls_info.

    get_parameter_c( EXPORTING iv_name      = 'contRep '
                               iv_mandatory = 's'
                     IMPORTING er_value     = lo_content ).

    ASSIGN lo_content->* TO <lv_contentrep>.

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    check_authority( iv_write = 1 ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_cdata = go_server->request->get_cdata( ).

    lv_active = 'X'.
    WHILE lv_cdata <> space.
      SPLIT lv_cdata AT gv_nl INTO lv_line lv_cdata.
      IF lv_line IS INITIAL.
        IF lv_active IS INITIAL.
          IF lv_cdata IS INITIAL.
            EXIT.
          ENDIF.
          gs_error-status_code = 400.
        ENDIF.
        lv_active = space.
      ENDIF.
      CLEAR ls_info.
      WHILE lv_line <> space.
        SPLIT lv_line AT '="' INTO lv_key lv_line.
        SPLIT lv_line AT '";' INTO lv_value lv_line.
        CASE lv_key.
          WHEN 'subject'.
            ls_info-subject = lv_value.
          WHEN 'issuer'.
            ls_info-issuer = lv_value.
          WHEN 'serialNumber'.
            ls_info-serialno = lv_value.
          WHEN 'notBefore'.
            ls_info-valid_from = lv_value.
          WHEN 'notAfter'.
            ls_info-valid_until = lv_value.
          WHEN 'keyInfo'.
            ls_info-fingerprint = lv_value.
          WHEN OTHERS.
            gs_error-status_code = 400.
        ENDCASE.
      ENDWHILE.
      IF ls_info-fingerprint IS INITIAL.
*      gs_error-status_code = 400.
      ENDIF.
      ls_racl-crep_id = <lv_contentrep>.
      ls_racl-fingerp = ls_info-fingerprint.
      ls_racl-active  = lv_active.
      APPEND ls_racl TO lt_racls.

    ENDWHILE.
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_RACL_SET'
      EXPORTING
        contrep           = <lv_contentrep>
      TABLES
        fingerprints      = lt_racls
      EXCEPTIONS
        authority_missing = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      IF sy-subrc = 1.
        sys_error_set( ).
        gs_error-status_code      = 401.
        gs_error-set_authenticate = 'X'.
      ELSE.
        sys_error_set( ).
      ENDIF.
    ENDIF.

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).
  ENDMETHOD.


  METHOD process_rep_configget.

    DATA: lo_contrep TYPE REF TO data.

    DATA: lv_pversion TYPE string ##NEEDED,
          lv_security TYPE string,
          lv_docprot  TYPE string.

    DATA: ls_crep_http TYPE crep_http.

    FIELD-SYMBOLS <lv_contentrep> TYPE c.

    get_parameter_c( EXPORTING iv_name      = 'contRep '
                               iv_mandatory = 's'
                     IMPORTING er_value     = lo_contrep ).

    ASSIGN lo_contrep->* TO <lv_contentrep>.

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    check_authority( iv_read = 1 ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    PERFORM crep_select IN PROGRAM saplscms_url USING    gv_mandt
                                                         <lv_contentrep>
                                                CHANGING ls_crep_http.

    IF ls_crep_http-http_nosig = 'X'.
      lv_security = '0'.
    ELSE.
      lv_security = '1'.
    ENDIF.

    lv_docprot = gv_docprot.

    go_server->response->set_header_field( name  = 'Content-Type'
                                           value = 'text/plain' ) ##NO_TEXT.

    body_put_field( iv_name  = 'Security'
                    iv_value = lv_security ) ##NO_TEXT.
    body_put_field( iv_name  = 'DefaultDocProt'
                    iv_value = lv_docprot ).

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).

  ENDMETHOD.


  METHOD process_rep_statget.

    DATA: lo_contrep TYPE REF TO data.

    DATA: lv_pversion  TYPE string ##NEEDED.

    FIELD-SYMBOLS <lv_contentrep> TYPE c.

    get_parameter_c( EXPORTING iv_name  = 'contRep '
                     IMPORTING er_value = lo_contrep ).

    ASSIGN lo_contrep->* TO <lv_contentrep>.

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    check_authority( iv_read = 1 ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    go_server->response->set_header_field( name  = 'Content-Type'
                                           value = 'text/plain' ) ##NO_TEXT.

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).

  ENDMETHOD.


  METHOD process_search.
    TYPES lty_result TYPE c LENGTH 20.

    DATA: lo_contrep TYPE REF TO data,
          lo_docid   TYPE REF TO data,
          lo_compid  TYPE REF TO data,
          lo_pattern TYPE REF TO data.

    DATA: lv_fromoffset    TYPE i,
          lv_tooffset      TYPE i,
          lv_numresults    TYPE i,
          lv_casesens      TYPE string,
          lv_pversion      TYPE string ##NEEDED,
          lv_seckey        TYPE string,
          lv_smand         TYPE c LENGTH 1,
          lv_signature     TYPE c LENGTH 1,
          lv_casesensitive TYPE c LENGTH 1,
          lv_cnt           TYPE i.

    DATA ls_result        TYPE lty_result.

    DATA lt_results       TYPE STANDARD TABLE OF lty_result.

    FIELD-SYMBOLS: <lv_contrep> TYPE c,
                   <lv_docid>   TYPE c,
                   <lv_compid>  TYPE c,
                   <lv_pattern> TYPE c.

    get_parameter_c( EXPORTING iv_name      = 'contRep'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_docid ).
    get_parameter_c( EXPORTING iv_name      = 'compId'
                               iv_mandatory = 'X'
                     IMPORTING er_value     = lo_compid ).
    get_parameter_c( EXPORTING iv_name      = 'pattern'
                               iv_mandatory = 'X'
                               iv_raw_mode  = 'X'
                     IMPORTING er_value     = lo_pattern ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.
    ASSIGN lo_compid->*  TO <lv_compid>.
    ASSIGN lo_pattern->* TO <lv_pattern>.

    lv_fromoffset = get_parameter_i( iv_name    = 'fromOffset'
                                     iv_default = 0 ).
    lv_tooffset = get_parameter_i( iv_name    = 'toOffset'
                                   iv_default = -1 ).
    lv_numresults = get_parameter_i( iv_name    = 'numResults'
                                     iv_default = 1 ).

    lv_casesens = get_parameter( iv_name    = 'caseSensitive'
                                 iv_default = 'n' ).
    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'r' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF lv_casesens = 'n'.
      lv_casesensitive = ' '.
    ELSEIF lv_casesens = 'y'.
      lv_casesensitive = 'X'.
    ELSE.
      gs_error-status_code = 400.
    ENDIF.
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    doc_search( EXPORTING iv_crep_id        = <lv_contrep>
                          iv_doc_id         = <lv_docid>
                          iv_comp_id        = <lv_compid>
                          iv_offset         = lv_fromoffset
                          iv_to_offset      = lv_tooffset
                          iv_case_sensitive = lv_casesensitive
                          iv_num_results    = lv_numresults
                          iv_signature      = lv_signature
                          iv_security       = space
                          iv_raw_pattern    = <lv_pattern>
                IMPORTING et_result         = lt_results ).

    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    DESCRIBE TABLE lt_results LINES lv_cnt.
    ls_result = lv_cnt.
    CONDENSE ls_result NO-GAPS.
    CONCATENATE ls_result ';' INTO gv_body.

    LOOP AT lt_results INTO ls_result.
      CONDENSE ls_result NO-GAPS.
      CONCATENATE gv_body ls_result ';' INTO gv_body.
    ENDLOOP.

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).
  ENDMETHOD.


  METHOD process_serverinfo.
    TYPES:
      BEGIN OF lty_crep,
        crep_id TYPE crep-crep_id,
      END OF lty_crep.

    DATA: lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1,
          lo_entity        TYPE REF TO if_http_entity,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk.

    DATA: lv_status      TYPE string,
          lv_vendor      TYPE string,
          lv_version     TYPE string,
          lv_tst         TYPE timestamp,
          lv_stst        TYPE n LENGTH 14,
          lv_date        TYPE string,
          lv_time        TYPE string,
          lv_contrep     TYPE string,
          lv_contrep1    TYPE saearchivi,
          lv_pversion    TYPE string,
          lv_resultas    TYPE string,
          lv_descript_c  TYPE crepdescr-descript,
          lv_sdescr      TYPE string,
          lv_bucket_name TYPE string,
          lv_ret_code    TYPE i,
          lv_build       TYPE string,
          lv_err_text    TYPE string,
          lv_descript    TYPE string.

    DATA: ls_crep      TYPE lty_crep,
          ls_cont_repo TYPE zgoog_cont_repo.

    DATA: lt_crep_tab  TYPE STANDARD TABLE OF lty_crep.

    lv_status  = 'running'.
    lv_vendor  = 'Google Cloud' ##NO_TEXT.
    lv_version = sy-saprl.

    GET TIME STAMP FIELD lv_tst.
    lv_stst = lv_tst.
    format_timestamp( EXPORTING iv_timestamp = lv_stst
                      IMPORTING ev_date      = lv_date
                                ev_time      = lv_time ).

    lv_contrep = get_parameter( iv_name      = 'contRep '
                                iv_mandatory = ' ' ).
    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_resultas = get_parameter( iv_name    = 'resultAs'
                                 iv_default = 'ascii' ).

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lo_entity = go_server->response.
    IF lv_resultas = 'html'.
      lo_entity->set_header_field( name  = 'Content-Type'
                                   value = 'text/plain' ) ##NO_TEXT.

      CONCATENATE
        'serverStatus:'            gv_sp lv_status   gv_nl
        'serverTime:'              gv_sp lv_time     gv_nl
        'serverDate:'              gv_sp lv_date     gv_nl
        'serverStatusDescription:' gv_sp lv_descript gv_nl
        'pVersion:'                gv_sp lv_pversion gv_nl
        INTO gv_body.
    ELSE.
      lo_entity->set_header_field( name  = 'Content-Type'
                                   value = 'text/plain' ) ##NO_TEXT.

      body_add_field( iv_name  = 'serverStatus'
                      iv_value = lv_status ).
      body_add_field( iv_name  = 'serverTime'
                      iv_value = lv_time ).
      body_add_field( iv_name  = 'serverDate'
                      iv_value = lv_date ).
      body_add_field( iv_name  = 'serverStatusDescription'
                      iv_value = lv_descript ).
      body_add_field( iv_name  = 'pVersion'
                      iv_value = lv_pversion ) ##NO_TEXT.
      CONCATENATE gv_body gv_nl INTO gv_body.
    ENDIF.

    IF lv_contrep IS INITIAL.
      SELECT * FROM crep
        INTO CORRESPONDING FIELDS OF TABLE lt_crep_tab
        WHERE crep_type IN ( '03', '04' )
        AND crep_id IN (
             SELECT archive_id
               FROM zgoog_cont_repo ).                    "#EC CI_SUBRC
    ELSE.
      SELECT * FROM crep
        INTO CORRESPONDING FIELDS OF TABLE lt_crep_tab
        WHERE ( crep_type = '03' OR crep_type = '04' )
          AND crep_id = lv_contrep.                       "#EC CI_SUBRC
    ENDIF.

    LOOP AT lt_crep_tab INTO ls_crep.
      lv_contrep = ls_crep-crep_id.
      SELECT SINGLE descript FROM crepdescr
        INTO lv_descript_c
        WHERE crep_id = lv_contrep AND langu = sy-langu.
      IF sy-subrc = 0.
        lv_descript = lv_descript_c.
      ELSE.
        lv_descript = space.
      ENDIF.

      lv_status = 'running'.
      lv_sdescr = space.

      IF lv_resultas = 'html'.
        lo_entity->set_header_field( name  = 'Content-Type'
                                     value = 'text/plain' ) ##NO_TEXT.

        CONCATENATE
          gv_body                                       gv_nl
          'contRep:'                  gv_sp lv_contrep  gv_nl
          'contRepDescription:'       gv_sp lv_descript gv_nl
          'contRepStatus:'            gv_sp lv_status   gv_nl
          'contRepStatusDescription:' gv_sp lv_sdescr   gv_nl
          INTO gv_body ##NO_TEXT.
      ELSE.
        lo_entity->set_header_field( name  = 'Content-Type'
                                     value = 'text/plain' ) ##NO_TEXT.

        body_add_field( iv_name  = 'contRep'
                        iv_value = lv_contrep ).
        body_add_field( iv_name  = 'contRepDescription'
                        iv_value = lv_descript ).
        body_add_field( iv_name  = 'contRepStatus'
                        iv_value = lv_status ).
        body_add_field( iv_name  = 'contRepStatusDescription'
                        iv_value = lv_sdescr ) ##NO_TEXT.
        CONCATENATE gv_body gv_nl INTO gv_body.
      ENDIF.
    ENDLOOP.

    lv_contrep1 = lv_contrep.
    load_goog_repo_config( lv_contrep1 ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT lo_gcs
          EXPORTING
            iv_key_name = gs_repo_config-keyname.

        lv_bucket_name = gs_repo_config-bucket.

        lo_gcs->get_buckets( EXPORTING iv_p_bucket = lv_bucket_name
                             IMPORTING ev_ret_code = lv_ret_code
                                       ev_err_text = lv_err_text ).
      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_err_text = lo_abap_sdk_excp->get_text( ).
        go_server->response->set_status( code   = 400
                                         reason = lv_err_text ).
        RETURN.

    ENDTRY.

    IF lo_gcs->is_client_error( lv_ret_code ) = abap_true.

      go_server->response->set_status( code   = 400
                                       reason = lv_err_text ).
      RETURN.
    ENDIF.

    go_server->response->set_status( code   = 200
                                     reason = 'OK' ).
  ENDMETHOD.


  METHOD process_update.
    DATA: lo_contrep       TYPE REF TO data,
          lo_docid         TYPE REF TO data,
          lo_compid        TYPE REF TO data,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk,
          lo_entity        TYPE REF TO if_http_entity,
          lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1.

    DATA: lv_compid      TYPE string,
          lv_pversion    TYPE string ##NEEDED,
          lv_seckey      TYPE string,
          lv_smand       TYPE c LENGTH 1,
          lv_signature   TYPE c LENGTH 1,
          lv_data        TYPE xstring,
          lv_mimetype    TYPE string,
          lv_p_bucket    TYPE string,
          lv_object_name TYPE string,
          lv_ret_code    TYPE i,
          lv_err_text    TYPE string,
          lv_msg         TYPE string.

    DATA: ls_object_list     TYPE /goog/cl_storage_v1=>ty_083,
          ls_err_resp        TYPE /goog/err_resp,
          ls_managed_folders TYPE /goog/cl_storage_v1=>ty_082,
          ls_comp_data       TYPE gtyp_s_comp_data,
          ls_input_object    TYPE /goog/cl_storage_v1=>ty_013,
          ls_output_object   TYPE /goog/cl_storage_v1=>ty_013,
          ls_info            TYPE scms_acinf.

    DATA: lt_comp_data TYPE STANDARD TABLE OF gtyp_s_comp_data,
          lt_bin       TYPE STANDARD TABLE OF sdokcntbin,
          lt_infos     TYPE STANDARD TABLE OF scms_acinf.

    FIELD-SYMBOLS: <lv_contrep>   TYPE c,
                   <lv_docid>     TYPE c,
                   <lv_compid>    TYPE c,
                   <ls_comp_data> TYPE gtyp_s_comp_data,
                   <ls_info>      TYPE scms_acinf.

    get_parameter_c( EXPORTING iv_name      = 'contRep'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_docid ).
    get_parameter_c( EXPORTING iv_name      = 'compId'
                               iv_mandatory = 'S'
                     IMPORTING ev_value_s   = lv_compid
                               er_value     = lo_compid ).

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.
    ASSIGN lo_compid->*  TO <lv_compid>.

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'u' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lo_entity = go_server->request.
    lv_data = lo_entity->get_data( ).
    lv_mimetype = lo_entity->get_header_field( name = 'Content-Type' ) ##NO_TEXT.

    load_goog_repo_config( <lv_contrep> ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_p_bucket = gs_repo_config-bucket.

    TRY.
        CREATE OBJECT lo_gcs
          EXPORTING
            iv_key_name = gs_repo_config-keyname.

        lv_object_name = <lv_docid> && '/' && <lv_docid>.

        GET REFERENCE OF lt_infos INTO ls_input_object-metadata.

        lv_object_name = <lv_docid> && '/' && <lv_docid> && '-' && <lv_compid>.

        READ TABLE lt_infos ASSIGNING <ls_info>
          WITH KEY comp_id = <lv_compid>.                "#EC CI_STDSEQ
        IF sy-subrc = 0.
          GET REFERENCE OF <ls_info> INTO ls_input_object-metadata.
        ENDIF.

        lo_gcs->insert_objects( EXPORTING iv_q_name       = lv_object_name
                                          iv_p_bucket     = lv_p_bucket
                                          is_input        = ls_input_object
                                          is_data         = lv_data
                                          iv_content_type = lv_mimetype
                                IMPORTING es_output       = ls_output_object
                                          ev_ret_code     = lv_ret_code
                                          ev_err_text     = lv_err_text ).
        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_msg = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = 400
                   iv_text = lv_msg ).
        RETURN.
    ENDTRY.

    go_server->response->set_status( code   = 201
                                     reason = 'created' ).
  ENDMETHOD.


  METHOD process_update_post.

    DATA: lo_contrep       TYPE REF TO data,
          lo_docid         TYPE REF TO data,
          lo_abap_sdk_excp TYPE REF TO /goog/cx_sdk,
          lo_entity        TYPE REF TO if_http_entity,
          lo_gcs           TYPE REF TO zgoog_cl_storage_cont_repo_v1.

    DATA: lv_pversion    TYPE string ##NEEDED,
          lv_seckey      TYPE string,
          lv_smand       TYPE c LENGTH 1,
          lv_signature   TYPE c LENGTH 1,
          lv_count       TYPE i,
          lv_mimetype    TYPE string,
          lv_compid      TYPE string,
          lv_data        TYPE xstring,
          lv_p_bucket    TYPE string,
          lv_object_name TYPE string,
          lv_ret_code    TYPE i,
          lv_err_text    TYPE string,
          lv_msg         TYPE string.

    DATA: ls_info            TYPE scms_acinf,
          ls_input_object    TYPE /goog/cl_storage_v1=>ty_013,
          ls_output_object   TYPE /goog/cl_storage_v1=>ty_013,
          ls_comp_data       TYPE gtyp_s_comp_data,
          ls_object_list     TYPE /goog/cl_storage_v1=>ty_083,
          ls_err_resp        TYPE /goog/err_resp,
          ls_managed_folders TYPE /goog/cl_storage_v1=>ty_082.

    DATA: lt_bin       TYPE STANDARD TABLE OF sdokcntbin,
          lt_comp_data TYPE STANDARD TABLE OF gtyp_s_comp_data,
          lt_infos     TYPE STANDARD TABLE OF scms_acinf.

    FIELD-SYMBOLS: <lv_contrep>   TYPE c,
                   <lv_docid>     TYPE c,
                   <ls_comp_data> TYPE gtyp_s_comp_data,
                   <ls_info>      TYPE scms_acinf.

    get_parameter_c( EXPORTING iv_name      = 'contRep'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_contrep ).
    get_parameter_c( EXPORTING iv_name      = 'docId'
                               iv_mandatory = 'S'
                     IMPORTING er_value     = lo_docid ).

    lv_pversion = get_parameter( iv_name      = 'pVersion'
                                 iv_mandatory = 'X' ).
    lv_seckey   = get_parameter( iv_name = 'secKey' ).

    ASSIGN lo_contrep->* TO <lv_contrep>.
    ASSIGN lo_docid->*   TO <lv_docid>.

    IF lv_seckey IS NOT INITIAL.
      lv_smand = 'S'.
      get_parameter( iv_name      = 'accessMode'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'authId'
                     iv_mandatory = lv_smand ).
      get_parameter( iv_name      = 'expiration'
                     iv_mandatory = lv_smand ).
    ENDIF.

    check_parameter_consistence( ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_signature = check_signature( iv_docprot = 'u' ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    load_goog_repo_config( <lv_contrep> ).
    IF gs_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_count = go_server->request->num_multiparts( ).
    DO lv_count TIMES.
      lo_entity = go_server->request->get_multipart( index = sy-index ).
      lv_mimetype = lo_entity->get_header_field( name = 'Content-Type' ) ##NO_TEXT.
      lv_compid = lo_entity->get_header_field( name = 'X-compId' ) ##NO_TEXT.

      IF lv_compid IS INITIAL. " workarround
        CONTINUE.
      ENDIF.

      lv_data = lo_entity->get_data( ).

      DESCRIBE TABLE lt_bin LINES ls_info-first_line.
      ls_info-first_line = ls_info-first_line + 1.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer          = lv_data
          append_to_table = 'X'
        IMPORTING
          output_length   = ls_info-comp_size
        TABLES
          binary_tab      = lt_bin.

      ls_info-comp_id    = lv_compid.
      ls_info-mimetype   = lv_mimetype.
      ls_info-binary_flg = 'X'.
      DESCRIBE TABLE lt_bin LINES ls_info-last_line.
      APPEND ls_info TO lt_infos.

      ls_comp_data-comp_id   = lv_compid.
      ls_comp_data-mime_type = lv_mimetype.
      ls_comp_data-data      = lv_data.

      APPEND ls_comp_data TO lt_comp_data.
      CLEAR ls_comp_data.

    ENDDO.

    lv_p_bucket = gs_repo_config-bucket.

    TRY.
        CREATE OBJECT lo_gcs
          EXPORTING
            iv_key_name = gs_repo_config-keyname.

        lv_object_name = <lv_docid> && '/' && <lv_docid>.

        GET REFERENCE OF lt_infos INTO ls_input_object-metadata.

        lo_gcs->insert_objects( EXPORTING iv_q_name   = lv_object_name
                                          iv_p_bucket = lv_p_bucket
                                          is_input    = ls_input_object
                                IMPORTING es_output   = ls_output_object
                                          ev_ret_code = lv_ret_code
                                          ev_err_text = lv_err_text ).
        IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
          set_error( iv_code = lv_ret_code
                     iv_text = lv_err_text ).
          RETURN.
        ENDIF.

        LOOP AT lt_comp_data ASSIGNING <ls_comp_data>.
          lv_object_name = <lv_docid> && '/' && <lv_docid> && '-' && <ls_comp_data>-comp_id.

          READ TABLE lt_infos ASSIGNING <ls_info>
            WITH KEY comp_id = <ls_comp_data>-comp_id.   "#EC CI_STDSEQ
          IF sy-subrc = 0.
            GET REFERENCE OF <ls_info> INTO ls_input_object-metadata.
          ENDIF.

          lo_gcs->insert_objects( EXPORTING iv_q_name       = lv_object_name
                                            iv_p_bucket     = lv_p_bucket
                                            is_input        = ls_input_object
                                            is_data         = <ls_comp_data>-data
                                            iv_content_type = <ls_comp_data>-mime_type
                                  IMPORTING es_output       = ls_output_object
                                            ev_ret_code     = lv_ret_code
                                            ev_err_text     = lv_err_text ).
          IF lo_gcs->is_success( lv_ret_code ) <> abap_true.
            set_error( iv_code = lv_ret_code
                       iv_text = lv_err_text ).
            RETURN.
          ENDIF.

        ENDLOOP.
      CATCH /goog/cx_sdk INTO lo_abap_sdk_excp.
        lv_msg = lo_abap_sdk_excp->get_text( ).
        set_error( iv_code = 400
                   iv_text = lv_msg ).
        RETURN.
    ENDTRY.

    go_server->response->set_status( code   = 200
                                     reason = 'ok' ).
  ENDMETHOD.


  METHOD report_error.
    DATA lv_mandt         TYPE string.
    DATA lv_realm         TYPE string.
    DATA lv_error_message TYPE string.

    IF gs_error-status_code IS INITIAL.
      gs_error-status_code = 500.
    ENDIF.

    IF gs_error-status_text IS INITIAL.
      CASE gs_error-status_code.
        WHEN 200. gs_error-status_text = 'ok' ##NO_TEXT.
        WHEN 201. gs_error-status_text = 'created' ##NO_TEXT.
        WHEN 250. gs_error-status_text = 'missing documents created' ##NO_TEXT.
        WHEN 400. gs_error-status_text = 'bad request' ##NO_TEXT.
        WHEN 401. gs_error-status_text = 'unauthorized' ##NO_TEXT.
        WHEN 403. gs_error-status_text = 'forbidden' ##NO_TEXT.
        WHEN 404. gs_error-status_text = 'not found'  ##NO_TEXT.
        WHEN 406. gs_error-status_text = 'not acceptable' ##NO_TEXT.
        WHEN 409. gs_error-status_text = 'conflict'     ##NO_TEXT.
        WHEN 500. gs_error-status_text = 'internal server error' ##NO_TEXT.
        WHEN OTHERS.
          gs_error-status_text = gs_error-status_code.
          CONCATENATE
            'http error:' gv_sp gs_error-status_text
            INTO gs_error-status_text ##NO_TEXT.
      ENDCASE.
    ENDIF.

    IF gs_error-set_authenticate = 'X'.
      lv_mandt = gv_mandt.

      CONCATENATE 'Basic realm="SAP R/3 [' sy-sysid ']"' INTO lv_realm ##NO_TEXT.

      go_server->response->set_header_field( name  = 'WWW-Authenticate' ##NO_TEXT
                                             value = lv_realm ) ##NO_TEXT.

      go_server->response->set_header_field( name  = 'SAP-Client' ##NO_TEXT
                                             value = lv_mandt ) ##NO_TEXT.
    ENDIF.

    IF gs_error-msgid IS NOT INITIAL.
      MESSAGE ID gs_error-msgid TYPE gs_error-msgty NUMBER gs_error-msgno
              WITH gs_error-msgv1 gs_error-msgv2 gs_error-msgv3 gs_error-msgv4
              INTO lv_error_message.
    ENDIF.

    IF lv_error_message IS INITIAL.
      lv_error_message = gs_error-status_text.
    ENDIF.

    go_server->response->set_status( code   = gs_error-status_code
                                     reason = gs_error-status_text ).

    " Avoid control characters in output
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_error_message WITH '<CRLF>'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_error_message WITH '<LF>'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN lv_error_message WITH '<FF>'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace IN lv_error_message WITH '<BS>'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_error_message WITH '<TAB>'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>vertical_tab IN lv_error_message WITH '<VT>'.

    " Set X-ErrorDescription
    go_server->response->set_header_field( name  = 'X-ErrorDescription'
                                           value = lv_error_message ) ##NO_TEXT.
    " New XSS handling
    lv_error_message = escape( val    = lv_error_message
                               format = cl_abap_format=>e_xss_ml ).

    " Set body
    go_server->response->set_cdata( data = lv_error_message ).

    " Set content type
    go_server->response->set_header_field( name  = 'Content-Type'
                                           value = 'text/html' ) ##NO_TEXT.
  ENDMETHOD.


  METHOD search_binary.
    DATA lv_l TYPE i.
    DATA lv_n TYPE i.
    DATA lv_p TYPE i.

    lv_l = xstrlen( iv_pattern ).
    lv_n = xstrlen( iv_buffer ) - xstrlen( iv_pattern ).
    cv_pos = -1.

    IF iv_pattern IS INITIAL.
      cv_pos = 0.
      EXIT.
    ENDIF.

    lv_p = 0.
    WHILE lv_p <= lv_n.
      IF iv_buffer+lv_p(lv_l) = iv_pattern.
        cv_pos = lv_p.
        EXIT.
      ENDIF.
      lv_p = lv_p + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD search_binary_2.
    DATA: lv_l  TYPE i,
          lv_l2 TYPE i,
          lv_n  TYPE i,
          lv_p  TYPE i,
          lv_pb TYPE i,
          lv_pp TYPE i.

    lv_l = xstrlen( iv_pattern ).
    lv_l2 = xstrlen( iv_pattern ).

    IF lv_l <> lv_l2.
      cv_pos = -1.
    ENDIF.

    lv_n = xstrlen( iv_buffer ) - xstrlen( iv_pattern ).
    cv_pos = -1.

    IF iv_pattern IS INITIAL.
      cv_pos = 0.
      EXIT.
    ENDIF.

    lv_p = 0.
    WHILE lv_p <= lv_n.
      lv_pb = lv_p.
      lv_pp = 0.
      WHILE lv_pp < lv_l.
        IF     iv_buffer+lv_pb(1) <> iv_pattern+lv_pp(1)
           AND iv_buffer+lv_pb(1) <> iv_pattern2+lv_pp(1).
          EXIT.
        ENDIF.
        lv_pp = lv_pp + 1.
        lv_pb = lv_pb + 1.
      ENDWHILE.
      IF lv_pp = lv_l.
        cv_pos = lv_p.
        EXIT.
      ENDIF.
      lv_p = lv_p + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD set_error.

    gs_error-status_code = iv_code.
    gs_error-status_text = iv_text.

  ENDMETHOD.


  METHOD set_locale_for_codepage.
    DATA lv_language TYPE sy-langu.

    lv_language = get_language_for_codepage( iv_codepage ).
    IF lv_language <> sy-langu AND lv_language IS NOT INITIAL.
      SET LOCALE LANGUAGE lv_language.
    ENDIF.
  ENDMETHOD.


  METHOD sys_error_set.
    gs_error-msgno = sy-msgno.
    gs_error-msgty = sy-msgty.
    gs_error-msgid = sy-msgid.
    gs_error-msgv1 = sy-msgv1.
    gs_error-msgv2 = sy-msgv2.
    gs_error-msgv3 = sy-msgv3.
    gs_error-msgv4 = sy-msgv4.
  ENDMETHOD.


  METHOD translate.
    DATA lo_convin TYPE REF TO cl_abap_conv_in_ce.

    TRY.

        " Check if conversion is really needed
        " Perform adjust_externalv_codepage in program scms changing codepage
        lo_convin = cl_abap_conv_in_ce=>create( encoding    = iv_codepage
                                                replacement = '#'
                                                ignore_cerr = abap_true
                                                input       = iv_buffer ).

        lo_convin->read( IMPORTING data = cv_cline ).

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD url_hex_decode.
    DATA lv_pos TYPE i.
    DATA lv_len TYPE i.
    DATA lv_c    TYPE c LENGTH 1.
    DATA lv_rest TYPE i.
    DATA lv_x    TYPE x LENGTH 1.

    IF NOT iv_value CA '%'.
      rv_result = iv_value.
      EXIT.
    ENDIF.

    lv_pos = 0.
    lv_len = strlen( iv_value ).
    CLEAR rv_result.

    WHILE lv_pos < lv_len.
      lv_c = iv_value+lv_pos(1).
      IF lv_c = '%'.
        lv_pos = lv_pos + 1.
        lv_rest = lv_len - lv_pos.
        IF lv_rest < 2.
          gs_error-status_code = '400'.
          EXIT.
        ENDIF.
        lv_x = iv_value+lv_pos(2).
        lv_pos = lv_pos + 2.

        CALL FUNCTION 'SCMS_BIN_TO_TEXT'
          EXPORTING
            bin_line  = lv_x
          IMPORTING
            text_line = lv_c
          EXCEPTIONS
            failed    = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
          gs_error-status_code = 500.
        ENDIF.
      ELSE.
        lv_pos = lv_pos + 1.
      ENDIF.
      IF lv_c IS INITIAL.
        CONCATENATE rv_result gv_sp INTO rv_result.
      ELSE.
        CONCATENATE rv_result lv_c INTO rv_result.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.


  METHOD url_hex_decode_x.

    DATA lo_convout TYPE REF TO cl_abap_conv_out_ce.

    DATA: lv_buffer TYPE string,
          lv_hex    TYPE c LENGTH 2,
          lv_x      TYPE x,
          lv_c      TYPE c.

    CLEAR rv_out.
    lv_buffer = iv_in.

    lo_convout = cl_abap_conv_out_ce=>create( encoding = '1100' ).

    WHILE lv_buffer IS NOT INITIAL.
      IF lv_buffer(1) = '%'.
        SHIFT lv_buffer LEFT BY 1 PLACES.
        lv_hex = lv_buffer.
        IF lv_hex CO '0123456789abcdefABCDEF'.
          lv_x = lv_hex.
          CONCATENATE rv_out lv_x INTO rv_out IN BYTE MODE.
        ENDIF.
        SHIFT lv_buffer LEFT BY 2 PLACES.
      ELSE.
        lv_c = lv_buffer(1).
        lo_convout->write( data = lv_c ).
        lv_x = lo_convout->get_buffer( ).
        lo_convout->reset( ).
        CONCATENATE rv_out lv_x INTO rv_out IN BYTE MODE.
        SHIFT lv_buffer LEFT BY 1 PLACES.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD verify_signature.

    DATA lv_signed TYPE xstring.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input    = iv_seckey
        unescape = 'X'
      IMPORTING
        output   = lv_signed
      EXCEPTIONS
        failed   = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      gs_error-status_code = 400.
      gs_error-status_text = 'seckey format error' ##NO_TEXT.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SCMS_RACL_VERIFY'
      EXPORTING
        uri_for_signature = iv_message
        signature         = lv_signed
        contrep           = iv_contrep
        authid            = iv_authid
      EXCEPTIONS
        OTHERS            = 401.
    IF sy-subrc <> 0.
      gs_error-status_code = sy-subrc.
      sys_error_set( ).
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD convert_str_to_time.

    DATA: lv_date_part TYPE string,
          lv_time_part TYPE string.

    TRY.
        lv_date_part = iv_datetime(10).
        TRANSLATE lv_date_part USING '- '.
        CONDENSE  lv_date_part NO-GAPS.

        lv_time_part = iv_datetime+11(8).
        TRANSLATE lv_time_part USING ': '.
        CONDENSE  lv_time_part NO-GAPS.
        CONCATENATE lv_date_part
                    lv_time_part
                    INTO rv_utc_time.
      CATCH cx_sy_range_out_of_bounds.
        "Do nothing
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
