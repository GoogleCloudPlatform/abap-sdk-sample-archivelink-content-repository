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
class ZGOOG_CL_ARCHIVE_UTILITY definition
  public
  create public .

public section.

  types T_MASS_TR_KEY type CHAR20 .

  class-methods ARCHIVE_OPEN_FOR_READ
    importing
      !IV_OBJECT type OBJCT_TR01
    exporting
      !EV_HANDLE type SYST-TABIX
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T
    changing
      !CT_FILES_READ_SEQUENTIAL type AS_T_RNG_ARCHIV .
  class-methods ARCHIVE_GET_NEXT_OBJECT
    importing
      !IV_HANDLE type SYST-TABIX
    exporting
      !EV_ARCHIVE_NAME type ARKEY
      !EV_OBJ_OFFSET type ADMI_OFFST
      !EV_DATA_IS_BLOCKED type BOOLE_D
      !EV_END_OF_FILE type FLAG
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods ARCHIVE_CLOSE_FILE
    importing
      !IV_HANDLE type SYST-TABIX .
  class-methods ARCHIVE_READ_OBJECT_BY_HANDLE
    importing
      !IV_HANDLE type SYST-TABIX
    exporting
      !EV_SY_SUBRC type SYST_SUBRC
      !ET_RETURN type BAPIRET2_T
    changing
      !CT_OBJ_DATA type AS_T_TABLEBUFFER .
  class-methods WRITE_TABLEDATA_TO_BQ
    importing
      !IV_MASS_TR_KEY type T_MASS_TR_KEY
      !IS_TABLE_DATA type ASTABLEBUFFER
    exporting
      !EV_ERROR_CODE type SYSUBRC
      !ET_RETURN type BAPIRET2_T .
  class-methods WRITE_ARCHIVE_FILEDATA_TO_BQ
    importing
      !IV_OBJECT type OBJCT_TR01
      !IV_MASS_TR_KEY type T_MASS_TR_KEY
    exporting
      !ET_RETURN type BAPIRET2_T
      !EV_ERROR_CODE type SYSUBRC
    changing
      !CT_RANGE_ARKEY type AS_T_RNG_ARCHIV .
  class-methods ADD_BAPIRET2_FROM_SY
    changing
      !CT_BAPIRET2 type BAPIRET2_T .
  class-methods GET_MSGTEXT_FROMBAPIRET2
    importing
      !PI_BAPIRET2 type BAPIRET2
    returning
      value(RV_MESSAGE) type STRING .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZGOOG_CL_ARCHIVE_UTILITY IMPLEMENTATION.


  METHOD add_bapiret2_from_sy.

    DATA: ls_bapiret2 TYPE bapiret2.

    ls_bapiret2-id = sy-msgid.
    ls_bapiret2-number = sy-msgno.
    ls_bapiret2-message_v1 = sy-msgv1.
    ls_bapiret2-message_v2 = sy-msgv2.
    ls_bapiret2-message_v3 = sy-msgv3.
    ls_bapiret2-message_v4 = sy-msgv4.

    APPEND ls_bapiret2 TO ct_bapiret2.

  ENDMETHOD.


  METHOD ARCHIVE_CLOSE_FILE.

    CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
      EXPORTING
        archive_handle = iv_handle.

  ENDMETHOD.


  METHOD archive_get_next_object.

    CLEAR ev_sy_subrc.

    CALL FUNCTION 'ARCHIVE_GET_NEXT_OBJECT'
      EXPORTING
        archive_handle          = iv_handle
      IMPORTING
        object_offset           = ev_obj_offset
        archive_name            = ev_archive_name
        ev_contain_blocked_data = ev_data_is_blocked
      EXCEPTIONS
        end_of_file             = 1
        OTHERS                  = 2.

    IF sy-subrc IS NOT INITIAL.
      IF sy-subrc = 1.
        ev_end_of_file = abap_true.
        RETURN.
      ELSE.
        ev_sy_subrc = sy-subrc.
      ENDIF.
    ENDIF.

    IF ev_sy_subrc > 1.
      add_bapiret2_from_sy(
       CHANGING ct_bapiret2 = et_return ).
    ENDIF.

  ENDMETHOD.


  METHOD archive_open_for_read.

    CALL FUNCTION 'ARCHIVE_OPEN_FOR_READ'
      EXPORTING
        object                       = iv_object
      IMPORTING
        archive_handle               = ev_handle
      TABLES
        archive_files                = ct_files_read_sequential
      EXCEPTIONS
        no_files_available           = 1
        file_already_open            = 2
        file_io_error                = 3
        internal_error               = 4
        object_not_found             = 5
        open_error                   = 6
        not_authorized               = 7
        archiving_standard_violation = 8.

    ev_sy_subrc = sy-subrc.

    IF sy-subrc IS NOT INITIAL.
      add_bapiret2_from_sy(
       CHANGING ct_bapiret2 = et_return ).
    ENDIF.

  ENDMETHOD.


METHOD archive_read_object_by_handle.

  CALL FUNCTION 'ARCHIVE_READ_OBJECT_BY_HANDLE'
    EXPORTING
      iv_handle               = iv_handle
      iv_read_class_data      = 'NO'
    CHANGING
      ct_obj_data             = ct_obj_data
    EXCEPTIONS
      end_of_object           = 0
      internal_error          = 1
      wrong_access_to_archive = 2
      OTHERS                  = 4.

  ev_sy_subrc = sy-subrc.

  IF ev_sy_subrc > 1.
    add_bapiret2_from_sy(
     CHANGING ct_bapiret2 = et_return ).
  ENDIF.

ENDMETHOD.


  METHOD get_msgtext_frombapiret2.

    MESSAGE ID pi_bapiret2-id TYPE 'I' NUMBER pi_bapiret2-number
      WITH pi_bapiret2-message_v1 pi_bapiret2-message_v2 pi_bapiret2-message_v3 pi_bapiret2-message_v4
      INTO rv_message.

  ENDMETHOD.


  METHOD write_archive_filedata_to_bq.

    DATA: lv_handle TYPE syst-tabix.

    DATA: lv_end_of_file     TYPE flag.

    FIELD-SYMBOLS: <ls_obj_data> TYPE astablebuffer.

    DATA: lt_obj_data TYPE as_t_tablebuffer.
    archive_open_for_read(
      EXPORTING
        iv_object                = iv_object
      IMPORTING
        ev_handle                = lv_handle
        ev_sy_subrc              = ev_error_code
        et_return                = et_return
      CHANGING
        ct_files_read_sequential = ct_range_arkey ).

    IF ev_error_code <> 0.
      RETURN.
    ENDIF.

    DO.

      archive_get_next_object(
         EXPORTING
           iv_handle          = lv_handle
         IMPORTING
           ev_sy_subrc        = ev_error_code
           et_return          = et_return
           ev_end_of_file     = lv_end_of_file ).

      IF lv_end_of_file IS NOT INITIAL.
        EXIT.
      ENDIF.

      IF ev_error_code <> 0.
        EXIT.
      ENDIF.

      CLEAR lt_obj_data.
      archive_read_object_by_handle(
         EXPORTING
           iv_handle   = lv_handle
         IMPORTING
           ev_sy_subrc  = ev_error_code
           et_return    = et_return
         CHANGING
           ct_obj_data = lt_obj_data ).

      LOOP AT  lt_obj_data ASSIGNING <ls_obj_data>.

        write_tabledata_to_bq(
          EXPORTING
            iv_mass_tr_key = iv_mass_tr_key
            is_table_data = <ls_obj_data>
          IMPORTING
            ev_error_code  = ev_error_code
            et_return      = et_return ).

        IF ev_error_code <> 0.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDDO.

    archive_close_file(
        iv_handle = lv_handle ).

  ENDMETHOD.


  METHOD write_tabledata_to_bq.

    " Experimental - Not implemented yet

    " If you want to implement this yourself, replace the below code
    " with a call to method INSERT_ALL_TABLEDATA of class /GOOG/CL_BIGQUERY_V2

    DATA: lo_bq_repl TYPE REF TO object.

    FIELD-SYMBOLS: <lt_archive_data> TYPE ANY TABLE.

    ASSIGN is_table_data-tabref->* TO <lt_archive_data>.

    CREATE OBJECT lo_bq_repl TYPE ('ZGOOG_CL_BQ_REPL').

    DATA: lv_table_source TYPE string.
    lv_table_source = is_table_data-tabname.

    CALL METHOD lo_bq_repl->('REPLICATE_DATA')
      EXPORTING
        iv_mass_tr_key = iv_mass_tr_key
        iv_data_source = lv_table_source
        it_content     = <lt_archive_data>
      IMPORTING
        ev_error_code  = ev_error_code
        et_return      = et_return.

  ENDMETHOD.
ENDCLASS.
