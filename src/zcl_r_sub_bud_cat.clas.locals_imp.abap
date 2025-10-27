CLASS lsc_zr_sub_bud_cat DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.
    METHODS save_modified REDEFINITION.

ENDCLASS.


CLASS lsc_zr_sub_bud_cat IMPLEMENTATION.
  METHOD save_modified.
    DATA lt_create           TYPE TABLE OF ztb_sub_bud_cat WITH EMPTY KEY.
    DATA lt_update           TYPE TABLE OF ztb_sub_bud_cat WITH EMPTY KEY.
    DATA lt_delete           TYPE TABLE OF ztb_sub_bud_cat WITH EMPTY KEY.
    DATA lt_aws              TYPE TABLE OF ztb_sub_bud_cat WITH EMPTY KEY.
    DATA events_to_be_raised TYPE TABLE FOR EVENT zr_sub_bud_cat~ApiCalled.

    lt_create = CORRESPONDING #( create-subbudgetcategory MAPPING FROM ENTITY ).
    lt_update = CORRESPONDING #( update-subbudgetcategory MAPPING FROM ENTITY ).
    lt_delete = CORRESPONDING #( delete-subbudgetcategory MAPPING FROM ENTITY ).

    MOVE-CORRESPONDING lt_create TO lt_aws KEEPING TARGET LINES.
    MOVE-CORRESPONDING lt_update TO lt_aws KEEPING TARGET LINES.
    MOVE-CORRESPONDING lt_delete TO lt_aws KEEPING TARGET LINES.
    DELETE lt_aws WHERE uuid_api IS NOT INITIAL.

    IF lt_aws IS NOT INITIAL.
      TRY.
          " TODO: variable is assigned but never used (ABAP cleaner)
          DATA(bgpf_process_name) = zbgpfcl_exe_send_sub_budcat=>run_via_bgpf_tx_uncontrolled(
                                        i_rap_bo_entity_key = lt_aws ).
        CATCH cx_bgmc.
      ENDTRY.
    ENDIF.

    " ---------------------------------------------------------------------

    DATA o_ZTB_SUB_BUD_CAT TYPE ztb_sub_bud_cat.
    DATA n_ZTB_SUB_BUD_CAT TYPE ztb_sub_bud_cat.
    DATA changenumber      TYPE if_chdo_object_tools_rel=>ty_cdchangenr.
    CONSTANTS cdoc_upd_object TYPE if_chdo_object_tools_rel=>ty_cdchngindh VALUE 'U'.
    DATA upd_ZTB_SUB_BUD_CAT TYPE if_chdo_object_tools_rel=>ty_cdchngindh.

    LOOP AT update-subbudgetcategory INTO DATA(ls_subbudgetcategory).

      IF     ls_subbudgetcategory-UuidApi          IS NOT INITIAL
         AND ls_subbudgetcategory-%control-UuidApi  = if_abap_behv=>mk-on.

        CLEAR events_to_be_raised.
        APPEND INITIAL LINE TO events_to_be_raised.
        events_to_be_raised[ 1 ] = CORRESPONDING #( ls_subbudgetcategory ).
        RAISE ENTITY EVENT zr_sub_bud_cat~ApiCalled FROM events_to_be_raised.

      ENDIF.

      READ ENTITIES OF zr_sub_bud_cat
           IN LOCAL MODE
           ENTITY subbudgetcategory
           ALL FIELDS WITH VALUE #( ( %key-Uuid = ls_subbudgetcategory-Uuid ) )
           RESULT DATA(l_data).

      LOOP AT l_data ASSIGNING FIELD-SYMBOL(<f_data>).

        SELECT SINGLE *
          FROM ztb_sub_bud_cat
          WITH
          PRIVILEGED ACCESS
          WHERE uuid = @<f_data>-Uuid
          INTO @DATA(ls_old_data).

        IF ls_subbudgetcategory-%control-ProjectNumber = 01.
          o_ZTB_SUB_BUD_CAT-project_number = ls_old_data-project_number.
          n_ZTB_SUB_BUD_CAT-project_number = <f_data>-ProjectNumber.
        ENDIF.

        IF ls_subbudgetcategory-%control-SubBudcatCategory = 01.
          o_ZTB_SUB_BUD_CAT-sub_budcat_number = ls_old_data-sub_budcat_number.
          n_ZTB_SUB_BUD_CAT-sub_budcat_number = <f_data>-SubBudcatCategory.
        ENDIF.

        IF ls_subbudgetcategory-%control-SubBudcatName = 01.
          o_ZTB_SUB_BUD_CAT-sub_budcat_name = ls_old_data-sub_budcat_name.
          n_ZTB_SUB_BUD_CAT-sub_budcat_name = <f_data>-SubBudcatName.
        ENDIF.

        IF ls_subbudgetcategory-%control-CampusCode = 01.
          o_ZTB_SUB_BUD_CAT-campus_code = ls_old_data-campus_code.
          n_ZTB_SUB_BUD_CAT-campus_code = <f_data>-CampusCode.
        ENDIF.

        IF ls_subbudgetcategory-%control-active = 01.
          o_ZTB_SUB_BUD_CAT-active = ls_old_data-active.
          n_ZTB_SUB_BUD_CAT-active = <f_data>-active.
        ENDIF.

        upd_ZTB_SUB_BUD_CAT = 'U'.
        n_ZTB_SUB_BUD_CAT-uuid = <f_data>-Uuid.
        o_ZTB_SUB_BUD_CAT-uuid = ls_old_data-uuid.
        n_ZTB_SUB_BUD_CAT-uuid_api = <f_data>-UuidApi.
        o_ZTB_SUB_BUD_CAT-uuid_api = ls_old_data-uuid_api.

        CONVERT UTCLONG utclong_current( )
                INTO DATE FINAL(datlo)
                TIME FINAL(timlo)
                TIME ZONE xco_cp_time=>time_zone->user->value.

        TRY.
            zcl_zcdoc_subbudcat_chdo=>write( EXPORTING objectid                = CONV #( <f_data>-Uuid )
                                                       utime                   = timlo
                                                       udate                   = datlo
                                                       username                = xco_cp=>sy->user( )->name
                                                       object_change_indicator = cdoc_upd_object
                                                       o_ZTB_SUB_BUD_CAT       = o_ZTB_SUB_BUD_CAT
                                                       n_ZTB_SUB_BUD_CAT       = n_ZTB_SUB_BUD_CAT
                                                       upd_ZTB_SUB_BUD_CAT     = upd_ZTB_SUB_BUD_CAT
                                             IMPORTING changenumber            = changenumber ).
          CATCH cx_chdo_write_error.
        ENDTRY.

        CLEAR: o_ZTB_SUB_BUD_CAT,
               n_ZTB_SUB_BUD_CAT,
               ls_old_data.
      ENDLOOP.
    ENDLOOP.

    IF update-subbudgetcategory IS NOT INITIAL.
      UPDATE ztb_sub_bud_cat FROM TABLE @update-subbudgetcategory
      INDICATORS SET STRUCTURE %control MAPPING FROM ENTITY.
    ENDIF.

    IF delete-subbudgetcategory IS NOT INITIAL.
      LOOP AT delete-subbudgetcategory INTO DATA(fa_delete).
        DELETE FROM ztb_sub_bud_cat WHERE uuid = @fa_delete-Uuid.
        DELETE FROM ztb_sub_bud_ct_d WHERE uuid = @fa_delete-Uuid.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lhc_zr_sub_bud_cat DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING
      REQUEST requested_authorizations FOR SubBudgetCategory
      RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR SubBudgetCategory RESULT result.

    METHODS setActive FOR DETERMINE ON MODIFY
      IMPORTING keys FOR SubBudgetCategory~setActive.

    METHODS validateData FOR VALIDATE ON SAVE
      IMPORTING keys FOR SubBudgetCategory~validateData.
ENDCLASS.


CLASS lhc_zr_sub_bud_cat IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.
    READ ENTITIES OF zr_sub_bud_cat IN LOCAL MODE
         ENTITY SubBudgetCategory
         ALL FIELDS WITH
         CORRESPONDING #( keys )
         RESULT FINAL(lt_data)
         FAILED failed.

    result = VALUE #( FOR ls_data IN lt_data
                      ( %tky                     = ls_data-%tky
                        %delete                  = if_abap_behv=>fc-o-disabled

                        %field-ProjectNumber     = COND #( WHEN ls_data-Active = abap_true
                                                           THEN if_abap_behv=>fc-f-mandatory
                                                           ELSE if_abap_behv=>fc-f-read_only )
                        %field-SubBudcatCategory = COND #( WHEN ls_data-Active = abap_true
                                                           THEN if_abap_behv=>fc-f-mandatory
                                                           ELSE if_abap_behv=>fc-f-read_only )
                        %field-CampusCode        = COND #( WHEN ls_data-Active = abap_true
                                                           THEN if_abap_behv=>fc-f-mandatory
                                                           ELSE if_abap_behv=>fc-f-read_only )
                        %field-SubBudcatName     = COND #( WHEN ls_data-Active = abap_true
                                                           THEN if_abap_behv=>fc-f-mandatory
                                                           ELSE if_abap_behv=>fc-f-read_only ) ) ).
  ENDMETHOD.

  METHOD setActive.
    READ ENTITIES OF zr_sub_bud_cat IN LOCAL MODE
         ENTITY SubBudgetCategory
         FIELDS ( Active )
         WITH CORRESPONDING #( keys )
         RESULT DATA(lt_data).

    " If Status is already set, do nothing
    DELETE lt_data WHERE Active IS NOT INITIAL.

    IF lt_data IS INITIAL.
      RETURN.
    ENDIF.

    MODIFY ENTITIES OF zr_sub_bud_cat IN LOCAL MODE
           ENTITY SubBudgetCategory
           UPDATE FIELDS ( Active )
           WITH VALUE #( FOR ls_data IN lt_data
                         ( %tky   = ls_data-%tky
                           Active = 'X' ) ).
  ENDMETHOD.

  METHOD validateData.
    DATA lt_data TYPE TABLE FOR READ RESULT zr_sub_bud_cat\\SubBudgetCategory.

    READ ENTITIES OF zr_sub_bud_cat
         IN LOCAL MODE
         ENTITY SubBudgetCategory
         FIELDS ( ProjectNumber SubBudcatCategory )
         WITH CORRESPONDING #( keys )
         RESULT lt_data.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<f_data>).

      APPEND VALUE #( %tky        = <f_data>-%tky
                      %state_area = 'VALIDATE_DATA' ) TO reported-subbudgetcategory.

      SELECT SINGLE COUNT( * ) FROM ztb_sub_bud_cat
        WHERE project_number    = @<f_data>-ProjectNumber
          AND sub_budcat_number = @<f_data>-SubBudcatCategory.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( %tky = <f_data>-%tky ) TO failed-subbudgetcategory.

      APPEND VALUE #( %tky                       = <f_data>-%tky
                      %state_area                = 'VALIDATE_DATA'
                      %msg                       = new_message( id       = 'Z_SUB_BUDCAT_MSG'
                                                                number   = 001
                                                                v1       = <f_data>-ProjectNumber
                                                                v2       = <f_data>-SubBudcatCategory
                                                                severity = if_abap_behv_message=>severity-error )
                      %element-ProjectNumber     = if_abap_behv=>mk-on
                      %element-SubBudcatCategory = if_abap_behv=>mk-on ) TO reported-subbudgetcategory.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
