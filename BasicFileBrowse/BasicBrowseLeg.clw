   PROGRAM

   INCLUDE('EQUATES.CLW'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('TPLEQU.CLW'),ONCE
   INCLUDE('ResDef.Clw')
   MAP
     MODULE('BasicBrowseLeg001.clw')
       Main
       BrowseEmpPosCSV
       ReportEmpPosCSV
       UpdateEmpPosCSV
       ProcessEmpPosCSV
     END
     MODULE('BasicBrowseLeg_SF.CLW')
       CheckOpen(FILE File,<BYTE OverrideCreate>,<BYTE OverrideOpenMode>)
       ReportPreview(QUEUE PrintPreviewQueue)
       Preview:JumpToPage(LONG Input:CurrentPage, LONG Input:TotalPages),LONG
       Preview:SelectDisplay(*LONG Input:PagesAcross, *LONG Input:PagesDown)
       StandardWarning(LONG WarningID),LONG,PROC
       StandardWarning(LONG WarningID,STRING WarningText1),LONG,PROC
       StandardWarning(LONG WarningID,STRING WarningText1,STRING WarningText2),LONG,PROC
       SetupStringStops(STRING ProcessLowLimit,STRING ProcessHighLimit,LONG InputStringSize,<LONG ListType>)
       NextStringStop,STRING
       SetupRealStops(REAL InputLowLimit,REAL InputHighLimit)
       NextRealStop,REAL
       INIRestoreWindow(STRING ProcedureName,STRING INIFileName)
       INISaveWindow(STRING ProcedureName,STRING INIFileName)
       RISaveError
     END
     MODULE('BasicBrowseLeg_RU.CLW')
       RIUpdate:EmpPosCSV(BYTE=0),LONG
       RISnap:EmpPosCSV
     END
     MODULE('BasicBrowseLeg_RD.CLW')
       RIDelete:EmpPosCSV,LONG
     END
   END


SaveErrorCode        LONG
SaveError            CSTRING(255)
SaveFileErrorCode    CSTRING(255)
SaveFileError        CSTRING(255)
GlobalRequest        LONG(0),THREAD
GlobalResponse       LONG(0),THREAD
VCRRequest           LONG(0),THREAD
!region File Declaration
EmpPosCSV            FILE,DRIVER('BASIC'),NAME('EmpPos2019.CSV'),PRE(EmpPos),CREATE,BINDABLE,THREAD ! !Previous bug  with /FIRSTROWHEADER=ON
Record                   RECORD,PRE()
PositionCode                STRING(3)                      !                     
Position                    STRING(128)                    !                     
MaxFTESalary                DECIMAL(9,2)                   !                     
MinFTESalary                DECIMAL(9,2)                   !                     
FirstYearReq                STRING(3)                      !                     
BenefitsReq                 STRING(3)                      !                     
FundingSourceReq            STRING(3)                      !                     
BilingualCodeReq            STRING(3)                      !                     
GradeAssignmentReq          STRING(3)                      !                     
SchoolWorkLocationReq       STRING(3)                      !                     
PrimaryWorkLocationReq      STRING(3)                      !                     
Ed360Role                   STRING(40)                     !                     
DataType                    STRING(10)                     !                     
                         END
                     END                       
EmpPosCSV::Used      LONG,THREAD

!endregion

Sort:Name            STRING(ScrollSort:Name)
Sort:Name:Array      STRING(3),DIM(100),OVER(Sort:Name)
Sort:Alpha           STRING(ScrollSort:Alpha)
Sort:Alpha:Array     STRING(2),DIM(100),OVER(Sort:Alpha)

  CODE
  Main
!---------------------------------------------------------------------------
