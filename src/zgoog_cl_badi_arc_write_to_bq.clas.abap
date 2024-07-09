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
class ZGOOG_CL_BADI_ARC_WRITE_TO_BQ definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ARC_DEL_APPL_TABS .
protected section.
private section.
ENDCLASS.



CLASS ZGOOG_CL_BADI_ARC_WRITE_TO_BQ IMPLEMENTATION.


  method IF_EX_ARC_DEL_APPL_TABS~ON_FILE_DEL_FINISHED.

  endmethod.


METHOD if_ex_arc_del_appl_tabs~on_obj_del_finished.

  "Experimental - Not yet implemented

  DATA: lv_tr_key TYPE zgoog_arch_bq-bq_tr_key.
  DATA: lv_sy_subrc TYPE syst_subrc.
  DATA: lt_return TYPE bapiret2_t.

  FIELD-SYMBOLS: <ls_obj_data> TYPE astablebuffer.
  FIELD-SYMBOLS: <ls_bapiret2> TYPE bapiret2.

  IF iv_testmode IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT SINGLE bq_tr_key
    FROM zgoog_arch_bq
    INTO lv_tr_key
    WHERE archiving_object = iv_archiving_object
      AND active = abap_true.
  IF lv_tr_key IS INITIAL.
    RETURN.
  ENDIF.

  DATA: lt_obj_data TYPE as_t_tablebuffer.

  zgoog_cl_archive_utility=>archive_read_object_by_handle(
   EXPORTING
     iv_handle   = iv_handle
   CHANGING
     ct_obj_data = lt_obj_data ).

  LOOP AT lt_obj_data ASSIGNING <ls_obj_data>.
    zgoog_cl_archive_utility=>write_tabledata_to_bq(
      EXPORTING
        iv_mass_tr_key = lv_tr_key
        is_table_data  = <ls_obj_data>
      IMPORTING
        ev_error_code  = lv_sy_subrc
        et_return      = lt_return ).

    IF lv_sy_subrc > 0.
      LOOP AT lt_return ASSIGNING <ls_bapiret2>
           WHERE type CA 'AEX'.
        EXIT.
      ENDLOOP.
      IF <ls_bapiret2> IS ASSIGNED.
        MESSAGE ID <ls_bapiret2>-id TYPE <ls_bapiret2>-type NUMBER <ls_bapiret2>-number
          WITH <ls_bapiret2>-message_v1 <ls_bapiret2>-message_v2 <ls_bapiret2>-message_v3 <ls_bapiret2>-message_v4.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


  method IF_EX_ARC_DEL_APPL_TABS~ON_SESSION_DEL_FINISHED.
  endmethod.
ENDCLASS.
