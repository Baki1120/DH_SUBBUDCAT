@Metadata.allowExtensions: true
@EndUserText.label: 'Sub Budget Category'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
define root view entity ZC_SUB_BUD_CAT
  provider contract transactional_query
  as projection on ZR_SUB_BUD_CAT
  association [0..*] to ZC_SUB_BUDCAT_LOG as _ChangeDocs on $projection.Uuid = _ChangeDocs.objectid
{
  key Uuid,
      @Search.defaultSearchElement: true
      ProjectNumber,
      CampusCode,
      @Search.defaultSearchElement: true
      SubBudcatCategory,
      SubBudcatName,
      BudgetCategoryCode,
      Active,
      UuidApi,
      @EndUserText.label: 'Status'
      @Semantics.text: true
      @UI.hidden: true
      _LogStatus.Message as StatusText,
      @ObjectModel.text.element: [ 'StatusText' ]
      Status,
      @UI.hidden: true
      Criticality,
      Message,
      CreatedByUser,
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['PersonFullName']
      CreatedBy,
      CreatedAt,
      LocalLastChangedBy,
      LocalLastChangedAt,
      @EndUserText.label: 'User name'
      @Semantics.text: true
      @UI.hidden: true
      _BusinessUser.FullName as PersonFullName,
      _ChangeDocs,
      _BusinessUser

}
