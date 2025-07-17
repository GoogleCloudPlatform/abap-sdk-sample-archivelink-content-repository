FUNCTION z_send_archive_link_to_bq.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(LINK) TYPE  TOAV0
*"     VALUE(TAB_NAME) TYPE  STRING
*"     VALUE(BQ_TRKEY) TYPE  /GOOG/TRKEY
*"  EXCEPTIONS
*"      LOAD_ERROR
*"----------------------------------------------------------------------

  DATA: ls_cont_repo TYPE zgoog_cont_repo.

  DATA: lt_link TYPE STANDARD TABLE OF toav0.

  APPEND link TO lt_link.

  DATA: lo_bqtr TYPE REF TO /goog/cl_bqtr_data_load.

  DATA: lx_error TYPE REF TO /goog/cx_sdk.

  TRY.
      CREATE OBJECT lo_bqtr
        EXPORTING
          iv_mass_tr_key = bq_trkey
          iv_data_source = tab_name.

      DATA: lv_error_code TYPE sysubrc.
      DATA: lt_return TYPE bapiret2_t.

      lo_bqtr->replicate_data(
        EXPORTING
          it_content    = lt_link
        IMPORTING
          ev_error_code = lv_error_code
          et_return     = lt_return ).

      DATA: ls_return TYPE bapiret2.

      IF lv_error_code > 0.
        LOOP AT lt_return INTO ls_return WHERE type CA 'EAX'.
          EXIT.
        ENDLOOP.

        IF sy-subrc IS INITIAL.
          MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number WITH
            ls_return-message_v1
            ls_return-message_v2
            ls_return-message_v3
            ls_return-message_v4

            RAISING load_error.

        ELSE.
          MESSAGE 'Unkwown Error Send data to BigQuery' TYPE 'E' RAISING load_error.
        ENDIF.

      ENDIF.

    CATCH /goog/cx_sdk INTO lx_error.

      MESSAGE lx_error->get_text( ) TYPE 'E' RAISING load_error.

  ENDTRY.



ENDFUNCTION.
