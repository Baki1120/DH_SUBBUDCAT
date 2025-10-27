@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Sub Budget Category'
define root view entity ZR_SUB_BUD_CAT
  as select from ztb_sub_bud_cat as SubBudgetCategory
  association [0..1] to ZI_USR_SUB_BUDCAT_VH as _BusinessUser on  $projection.CreatedBy = _BusinessUser.BusinessPartner
  association [0..1] to ZSUB_BUDCAT_STA_VH   as _LogStatus    on  $projection.Status  = _LogStatus.Status
                                                              and _LogStatus.language = $session.system_language
{
  key uuid                  as Uuid,
      project_number        as ProjectNumber,
      campus_code           as CampusCode,
      sub_budcat_number     as SubBudcatCategory,
      sub_budcat_name       as SubBudcatName,
      sub_budcat_code       as BudgetCategoryCode,
      active                as Active,
      uuid_api              as UuidApi,
      message               as Message,
      status                as Status,
      case status
        when ' ' then 2
        when 'S' then 3
        when 'U' then 3
        when 'A' then 3
        when 'R' then 1
        when 'F' then 1
        else 0 end          as Criticality,
      case created_by when 'CC0000000005'
        then cast( 'Created by API' as vdm_userdescription )
        else cast( _BusinessUser.FullName as vdm_userdescription preserving type )
        end                 as CreatedByUser,
      @Semantics.user.createdBy: true
      created_by            as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at            as CreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,
      _BusinessUser,
      _LogStatus

}
