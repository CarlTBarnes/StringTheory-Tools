

                     MEMBER('BasicBrowseLeg.clw')          ! This is a MEMBER module

!!! <summary>
!!! Generated from procedure template - Frame
!!! Clarion for Windows Wizard Application for BasicFiles.dct
!!! </summary>
Main PROCEDURE

LocalRequest         LONG                         ! 
OriginalRequest      LONG                         ! 
LocalResponse        LONG                         ! 
FilesOpened          LONG                         ! 
WindowOpened         LONG                         ! 
WindowInitialized    LONG                         ! 
ForceRefresh         LONG                         ! 
CurrentTab           STRING(80)                   ! 
AppFrame             APPLICATION('Browse of Basic File Example'),AT(,,750,318),FONT('Microsoft Sans Serif',8,,FONT:regular), |
  RESIZE,CENTER,ICON('WAFRAME.ICO'),MAX,STATUS(-1,80,120,45),SYSTEM,IMM
                       MENUBAR,USE(?MENUBAR1)
                         MENU('&File'),USE(?FileMenu)
                           ITEM('&Print Setup ...'),USE(?PrintSetup),MSG('Setup printer'),STD(STD:PrintSetup)
                           ITEM,USE(?SEPARATOR1),SEPARATOR
                           ITEM('E&xit'),USE(?Exit),MSG('Exit this application'),STD(STD:Close)
                         END
                         MENU('&Edit'),USE(?EditMenu)
                           ITEM('Cu&t'),USE(?Cut),MSG('Cut Selection To Clipboard'),STD(STD:Cut)
                           ITEM('&Copy'),USE(?Copy),MSG('Copy Selection To Clipboard'),STD(STD:Copy)
                           ITEM('&Paste'),USE(?Paste),MSG('Paste From Clipboard'),STD(STD:Paste)
                         END
                         MENU('&Browse'),USE(?BrowseMenu)
                           ITEM('Browse the EmpPosCSV file'),USE(?BrowseEmpPosCSV),MSG('Browse EmpPosCSV')
                         END
                         MENU('&Reports'),USE(?ReportMenu),MSG('Report data')
                           ITEM('Print the EmpPosCSV file'),USE(?ReportEmpPosCSV),MSG('Print in record order')
                         END
                         MENU('&Process'),USE(?ProcessMenu)
                           ITEM('Proccess Template on EmpPosCSV file'),USE(?ProcessEmpPosCSV)
                         END
                         MENU('&Window'),USE(?WindowMenu),STD(STD:WindowList)
                           ITEM('T&ile'),USE(?Tile),MSG('Arrange multiple opened windows'),STD(STD:TileWindow)
                           ITEM('&Cascade'),USE(?Cascade),MSG('Arrange multiple opened windows'),STD(STD:CascadeWindow)
                           ITEM('&Arrange Icons'),USE(?Arrange),MSG('Arrange the icons for minimized windows'),STD(STD:ArrangeIcons)
                         END
                         MENU('&Help'),USE(?HelpMenu)
                           ITEM('&Contents'),USE(?Helpindex),MSG('View the contents of the help file'),STD(STD:HelpIndex)
                           ITEM('&Search for Help On...'),USE(?HelpSearch),MSG('Search for help on a subject'),STD(STD:HelpSearch)
                           ITEM('&How to Use Help'),USE(?HelpOnHelp),MSG('Provides general instructions on using help'), |
  STD(STD:HelpOnHelp)
                         END
                       END
                     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:DoResize
      ForceRefresh = True
      DO RefreshWindow
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(1)
      POST(EVENT:Accepted,?BrowseEmpPosCSV)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE ACCEPTED()
    OF ?BrowseEmpPosCSV
      START(BrowseEmpPosCSV, 050000)
    OF ?ReportEmpPosCSV
      START(ReportEmpPosCSV, 050000)
    OF ?ProcessEmpPosCSV
      START(ProcessEmpPosCSV, 25000)
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  FilesOpened = TRUE
  DO BindFields
  OPEN(AppFrame)
  WindowOpened=True
      AppFrame{PROP:TabBarVisible}  = False
  Do DefineListboxStyle

!---------------------------------------------------------------------------
BindFields ROUTINE
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
  END
  IF WindowOpened
    CLOSE(AppFrame)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF AppFrame{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
!!! <summary>
!!! Generated from procedure template - Browse
!!! Browse the EmpPosCSV file
!!! </summary>
BrowseEmpPosCSV PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
RowOneHide           BYTE                                  ! 

BRW1::View:Browse    VIEW(EmpPosCSV)
                       PROJECT(EmpPos:PositionCode)
                       PROJECT(EmpPos:Position)
                       PROJECT(EmpPos:MaxFTESalary)
                       PROJECT(EmpPos:MinFTESalary)
                       PROJECT(EmpPos:FirstYearReq)
                       PROJECT(EmpPos:BenefitsReq)
                       PROJECT(EmpPos:FundingSourceReq)
                       PROJECT(EmpPos:BilingualCodeReq)
                       PROJECT(EmpPos:GradeAssignmentReq)
                       PROJECT(EmpPos:SchoolWorkLocationReq)
                       PROJECT(EmpPos:PrimaryWorkLocationReq)
                       PROJECT(EmpPos:Ed360Role)
                       PROJECT(EmpPos:DataType)
                     END
Queue:Browse:1       QUEUE,PRE()                           ! Browsing Queue
BRW1::EmpPos:PositionCode LIKE(EmpPos:PositionCode)        ! Queue Display field
BRW1::EmpPos:Position  LIKE(EmpPos:Position)               ! Queue Display field
BRW1::EmpPos:MaxFTESalary LIKE(EmpPos:MaxFTESalary)        ! Queue Display field
BRW1::EmpPos:MinFTESalary LIKE(EmpPos:MinFTESalary)        ! Queue Display field
BRW1::EmpPos:FirstYearReq LIKE(EmpPos:FirstYearReq)        ! Queue Display field
BRW1::EmpPos:BenefitsReq LIKE(EmpPos:BenefitsReq)          ! Queue Display field
BRW1::EmpPos:FundingSourceReq LIKE(EmpPos:FundingSourceReq) ! Queue Display field
BRW1::EmpPos:BilingualCodeReq LIKE(EmpPos:BilingualCodeReq) ! Queue Display field
BRW1::EmpPos:GradeAssignmentReq LIKE(EmpPos:GradeAssignmentReq) ! Queue Display field
BRW1::EmpPos:SchoolWorkLocationReq LIKE(EmpPos:SchoolWorkLocationReq) ! Queue Display field
BRW1::EmpPos:PrimaryWorkLocationReq LIKE(EmpPos:PrimaryWorkLocationReq) ! Queue Display field
BRW1::EmpPos:Ed360Role LIKE(EmpPos:Ed360Role)              ! Queue Display field
BRW1::EmpPos:DataType  LIKE(EmpPos:DataType)               ! Queue Display field
BRW1::Mark             BYTE                                ! Record mark flag
BRW1::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW1::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW1::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW1::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW1::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW1::CurrentEvent   LONG                                  !
BRW1::CurrentChoice  LONG                                  !
BRW1::RecordCount    LONG                                  !
BRW1::SortOrder      BYTE                                  !
BRW1::LocateMode     BYTE                                  !
BRW1::RefreshMode    BYTE                                  !
BRW1::LastSortOrder  BYTE                                  !
BRW1::FillDirection  BYTE                                  !
BRW1::AddQueue       BYTE                                  !
BRW1::Changed        BYTE                                  !
BRW1::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW1::ItemsToFill    LONG                                  ! Controls records retrieved
BRW1::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW1::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW1::NewSelectPosted BYTE                                 ! Queue position of located record
BRW1::PopupText      CSTRING(10000)                        !
BRW1::ActiveInvisible BYTE(1)                              !
BRW1::LoadPending    BYTE                                  !
WinResize            WindowResizeType
QuickWindow          WINDOW('Browse the EmpPosCSV file - Browse of Basic File Example'),AT(,,661,201),FONT('Microsoft ' & |
  'Sans Serif',8,,FONT:regular),RESIZE,GRAY,IMM,MDI,HLP('BrowseEmpPosCSV'),SYSTEM
                       LIST,AT(3,2,656,168),USE(?Browse:1),HVSCROLL,FORMAT('20L(2)|M~Code~@s3@150L(2)|M~Positi' & |
  'on~@s128@44R(2)|M~Max Salary~C(0)@n10.2@44R(2)|M~Min Salary~C(0)@n10.2@30L(2)|M~1st ' & |
  'Year~@s3@32L(2)|M~Benefits~@s3@28L(2)|M~Source~@s3@31L(2)|M~Bilingual~@s3@40L(2)|M~G' & |
  'rade Asn~@s3@40L(2)|M~School Loc~@s3@40L(2)|M~Primary Loc~@s3@100L(2)|M~Ed 360 Role~' & |
  '@s40@40L(2)|M~Data Type~L(0)@s10@'),FROM(Queue:Browse:1),IMM,MSG('Browsing the EmpPosCSV file'), |
  VCR
                       BUTTON('&View'),AT(7,180,49,14),USE(?View:2),LEFT,ICON('WAVIEW.ICO'),FLAT,MSG('View Record'), |
  TIP('View Record')
                       BUTTON('&Insert'),AT(61,180,49,14),USE(?Insert:3),LEFT,ICON('WAINSERT.ICO'),DISABLE,FLAT,MSG('Insert a Record'), |
  TIP('Insert a Record')
                       BUTTON('&Close'),AT(125,180,49,14),USE(?Close),LEFT,ICON('WACLOSE.ICO'),FLAT,MSG('Close Window'), |
  TIP('Close Window')
                       PROMPT('Bug: On the First page hit Page Up key and see the that Previous() goes past th' & |
  'e beginning of the file and repeats the First record repeatedly. Previous is suppose' & |
  ' to work for Basic driver. Only occurs with DRIVER(,/FIRSTROWHEADER=ON) which is now OFF.'), |
  AT(319,174,327,25),USE(?PROMPT1)
                       CHECK('Row One Hide'),AT(210,182),USE(RowOneHide),SKIP,TIP('Simulate FIRSTROWHEADER=ON ' & |
  'and using ValidateRecord filter POINTER()=1')
                     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:CloseDown
      WinResize.Kill()
    OF EVENT:CloseWindow
      WinResize.Kill()
    OF EVENT:DoResize
      ForceRefresh = True
      DO RefreshWindow
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?Browse:1)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Browse:1
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW1::NewSelection
      OF EVENT:ScrollUp
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW1::ProcessScroll
      OF EVENT:PageUp
        DO BRW1::ProcessScroll
      OF EVENT:PageDown
        DO BRW1::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW1::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW1::ScrollDrag
      OF EVENT:AlertKey
        DO BRW1::AlertKey
      END
    OF ?View:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonView
      END
    OF ?Insert:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonInsert
      END
    OF ?Close
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        IF LocalRequest = SelectRecord
           LocalResponse = RequestCancelled
        ELSE
           LocalResponse = RequestCompleted
        END
        POST(EVENT:CloseWindow)
      END
    OF ?RowOneHide
      CASE EVENT()
      OF EVENT:Accepted
        ForceRefresh=1 ; DO RefreshWindow ; SELECT(?Browse:1)
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF EmpPosCSV::Used = 0
    CheckOpen(EmpPosCSV,1)
  END
  EmpPosCSV::Used += 1
  FilesOpened = TRUE
  DO BindFields
  OPEN(QuickWindow)
  WindowOpened=True
  BRW1::AddQueue = True
  BRW1::RecordCount = 0
  WinResize.Init(AppStrategy:Spread,Resize:SetMinSize)
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight
  ?Browse:1{PROP:Alrt,255} = InsertKey

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(EmpPos:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    EmpPosCSV::Used -= 1
    IF EmpPosCSV::Used = 0 THEN CLOSE(EmpPosCSV).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DO BRW1::SelectSort
  ?Browse:1{Prop:VScrollPos} = BRW1::CurrentScroll
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
  DO BRW1::GetRecord
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
!----------------------------------------------------------------------
BRW1::ValidateRecord ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  BRW1::RecordStatus = Record:OutOfRange
  BRW1::RecordStatus = Record:Filtered
  IF RowOneHide AND POINTER(EmpPosCSV)=1 THEN EXIT.
  BRW1::RecordStatus = Record:OK
  EXIT
!----------------------------------------------------------------------
BRW1::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW1::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW1::LastSortOrder = BRW1::SortOrder
  BRW1::Changed = False
  IF BRW1::SortOrder = 0
    BRW1::SortOrder = 1
  END
  IF BRW1::SortOrder <> BRW1::LastSortOrder OR BRW1::Changed OR ForceRefresh OR (BRW1::LoadPending AND ?Browse:1{PROP:VISIBLE})
    DO BRW1::GetRecord
    DO BRW1::Reset
    IF BRW1::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      ELSE
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      END
    ELSE
      IF BRW1::Changed
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      ELSE
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      END
    END
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
    DO BRW1::InitializeBrowse
  ELSE
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW1::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
!----------------------------------------------------------------------
BRW1::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  EmpPos:PositionCode = BRW1::EmpPos:PositionCode
  EmpPos:Position = BRW1::EmpPos:Position
  EmpPos:MaxFTESalary = BRW1::EmpPos:MaxFTESalary
  EmpPos:MinFTESalary = BRW1::EmpPos:MinFTESalary
  EmpPos:FirstYearReq = BRW1::EmpPos:FirstYearReq
  EmpPos:BenefitsReq = BRW1::EmpPos:BenefitsReq
  EmpPos:FundingSourceReq = BRW1::EmpPos:FundingSourceReq
  EmpPos:BilingualCodeReq = BRW1::EmpPos:BilingualCodeReq
  EmpPos:GradeAssignmentReq = BRW1::EmpPos:GradeAssignmentReq
  EmpPos:SchoolWorkLocationReq = BRW1::EmpPos:SchoolWorkLocationReq
  EmpPos:PrimaryWorkLocationReq = BRW1::EmpPos:PrimaryWorkLocationReq
  EmpPos:Ed360Role = BRW1::EmpPos:Ed360Role
  EmpPos:DataType = BRW1::EmpPos:DataType
!----------------------------------------------------------------------
BRW1::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW1::EmpPos:PositionCode = EmpPos:PositionCode
  BRW1::EmpPos:Position = EmpPos:Position
  BRW1::EmpPos:MaxFTESalary = EmpPos:MaxFTESalary
  BRW1::EmpPos:MinFTESalary = EmpPos:MinFTESalary
  BRW1::EmpPos:FirstYearReq = EmpPos:FirstYearReq
  BRW1::EmpPos:BenefitsReq = EmpPos:BenefitsReq
  BRW1::EmpPos:FundingSourceReq = EmpPos:FundingSourceReq
  BRW1::EmpPos:BilingualCodeReq = EmpPos:BilingualCodeReq
  BRW1::EmpPos:GradeAssignmentReq = EmpPos:GradeAssignmentReq
  BRW1::EmpPos:SchoolWorkLocationReq = EmpPos:SchoolWorkLocationReq
  BRW1::EmpPos:PrimaryWorkLocationReq = EmpPos:PrimaryWorkLocationReq
  BRW1::EmpPos:Ed360Role = EmpPos:Ed360Role
  BRW1::EmpPos:DataType = EmpPos:DataType
  BRW1::Position = POSITION(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW1::NewSelectPosted
    BRW1::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:1)
  END
!----------------------------------------------------------------------
BRW1::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW1::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW1::PopupText = ''
    IF BRW1::RecordCount
      IF BRW1::PopupText
        BRW1::PopupText = '&View'&'|-|'&CLIP(BRW1::PopupText)
      ELSE
        BRW1::PopupText = '&View'
      END
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert'
      END
    ELSE
      IF BRW1::PopupText THEN
        IF INSTRING('&View',BRW1::PopupText,1,1) THEN
           BRW1::PopupText = SUB(BRW1::PopupText,1,INSTRING('&View',BRW1::PopupText,1,1)-1)&'~&View'&SUB(BRW1::PopupText,INSTRING('&View',BRW1::PopupText,1,1)+LEN('&View'),LEN(BRW1::PopupText)-INSTRING('&View',BRW1::PopupText,1,1)+LEN('&View'))
        END
      ELSE
        BRW1::PopupText = '~&View'
      END
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert'
      END
    END
    EXECUTE(POPUP(BRW1::PopupText))
      POST(EVENT:Accepted,?Insert:3)
      POST(EVENT:Accepted,?View:2)
    END
  ELSIF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    DO BRW1::FillBuffer
    IF BRW1::RecordCount = ?Browse:1{PROP:Items}
      IF NOT ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = True
      END
    ELSE
      IF ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW1::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW1::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW1::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW1::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW1::RecordCount
    BRW1::CurrentEvent = EVENT()
    CASE BRW1::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW1::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW1::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW1::ScrollEnd
    END
    ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
    DO BRW1::PostNewSelection
  CASE BRW1::SortOrder
  OF 1
    BRW1::CurrentScroll = 50                               ! Move Thumb to center
    IF BRW1::RecordCount = ?Browse:1{PROP:Items}
      IF BRW1::ItemsToFill
        IF BRW1::CurrentEvent = EVENT:ScrollUp
          BRW1::CurrentScroll = 0
        ELSE
          BRW1::CurrentScroll = 100
        END
      END
    ELSE
      BRW1::CurrentScroll = 0
    END
  END
  END
!----------------------------------------------------------------------
BRW1::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW1::FillRecord to retrieve one record in the direction required.
!|
  IF BRW1::CurrentEvent = EVENT:ScrollUp AND BRW1::CurrentChoice > 1
    BRW1::CurrentChoice -= 1
    EXIT
  ELSIF BRW1::CurrentEvent = EVENT:ScrollDown AND BRW1::CurrentChoice < BRW1::RecordCount
    BRW1::CurrentChoice += 1
    EXIT
  END
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = BRW1::CurrentEvent - 2
  DO BRW1::FillRecord
!----------------------------------------------------------------------
BRW1::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW1::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW1::FillRecord doesn't fill a page (BRW1::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  BRW1::FillDirection = BRW1::CurrentEvent - 4
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::ItemsToFill
    IF BRW1::CurrentEvent = EVENT:PageUp
      BRW1::CurrentChoice -= BRW1::ItemsToFill
      IF BRW1::CurrentChoice < 1
        BRW1::CurrentChoice = 1
      END
    ELSE
      BRW1::CurrentChoice += BRW1::ItemsToFill
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW1::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW1::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  DO BRW1::Reset
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::FillDirection = FillForward
  ELSE
    BRW1::FillDirection = FillBackward
  END
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::CurrentChoice = 1
  ELSE
    BRW1::CurrentChoice = BRW1::RecordCount
  END
!----------------------------------------------------------------------
BRW1::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW1::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW1::LocateRecord.
!|
  IF BRW1::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:1{PROPLIST:MouseDownRow}>0
         ?Browse:1{PROP:Selected} = ?Browse:1{PROPLIST:MouseDownRow}
         BRW1::CurrentChoice = CHOICE(?Browse:1)
       END
       DO BRW1::NewSelection
    OF MouseLeft2
      POST(EVENT:Accepted,?View:2) 
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW1::NewSelection
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
      END
    END
  END
  DO BRW1::PostNewSelection
!----------------------------------------------------------------------
BRW1::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW1::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW1::LocateRecord.
!|
  IF ?Browse:1{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:1)
  ELSIF ?Browse:1{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:1)
  ELSE
  END
!----------------------------------------------------------------------
BRW1::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW1::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW1::ItemsToFill records. Normally, this will
!| result in BRW1::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW1::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW1::AddQueue is true, the queue is filled using the BRW1::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW1::AddQueue is false is when the BRW1::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW1::RecordCount
    IF BRW1::FillDirection = FillForward
      GET(Queue:Browse:1,BRW1::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:1,1)                                ! Get the first queue item
    END
    RESET(BRW1::View:Browse,BRW1::Position)                ! Reset for sequential processing
    BRW1::SkipFirst = TRUE
  ELSE
    BRW1::SkipFirst = FALSE
  END
  LOOP WHILE BRW1::ItemsToFill
    IF BRW1::View:Browse{PROP:IPRequestCount} = 0
       BRW1::View:Browse{PROP:IPRequestCount} = BRW1::ItemsToFill
    END
    IF BRW1::FillDirection = FillForward
      NEXT(BRW1::View:Browse)
    ELSE
      PREVIOUS(BRW1::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW1::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'EmpPosCSV')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW1::SkipFirst
       BRW1::SkipFirst = FALSE
       IF POSITION(BRW1::View:Browse) = BRW1::Position
          CYCLE
       END
    END
    DO BRW1::ValidateRecord
    EXECUTE(BRW1::RecordStatus)
      BEGIN
        IF BRW1::FillDirection = FillForward
          GET(Queue:Browse:1,BRW1::RecordCount)            ! Get the first queue item
        ELSE
          GET(Queue:Browse:1,1)                            ! Get the first queue item
        END
        DO BRW1::FillBuffer
        BREAK
      END
      CYCLE
    END
    IF BRW1::AddQueue
      IF BRW1::RecordCount = ?Browse:1{PROP:Items}
        IF BRW1::FillDirection = FillForward
          GET(Queue:Browse:1,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:1,BRW1::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:1)
        BRW1::RecordCount -= 1
      END
      DO BRW1::FillQueue
      IF BRW1::FillDirection = FillForward
        ADD(Queue:Browse:1)
      ELSE
        ADD(Queue:Browse:1,1)
      END
      BRW1::RecordCount += 1
    END
    BRW1::ItemsToFill -= 1
  END
  BRW1::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW1::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW1::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW1::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW1::LocateMode = LocateOnPosition
    BRW1::LocateMode = LocateOnEdit
  END
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = ''
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(EmpPosCSV)
      RESET(EmpPosCSV,BRW1::HighlightedPosition)
      BRW1::HighlightedPosition = ''
    ELSE
      IF POSITION(EmpPosCSV)
        RESET(EmpPosCSV,POSITION(EmpPosCSV))
      ELSE
        SET(EmpPosCSV)
      END
    END
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW1::UsingAdditionalSortOrder = True
    CASE BRW1::SortOrder
    OF 1
       SET(BRW1::View:Browse)
    END
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = FillForward                        ! Fill with next read(s)
  BRW1::AddQueue = False
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  BRW1::AddQueue = True
  IF BRW1::ItemsToFill
    BRW1::RefreshMode = RefreshOnBottom
    DO BRW1::RefreshPage
  ELSE
    BRW1::RefreshMode = RefreshOnPosition
    DO BRW1::RefreshPage
  END
  DO BRW1::PostNewSelection
  BRW1::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW1::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW1::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:1), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW1::RefreshMode = RefreshOnPosition
    BRW1::HighlightedPosition = POSITION(BRW1::View:Browse)
    RESET(BRW1::View:Browse,BRW1::HighlightedPosition)
    BRW1::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:1,RECORDS(Queue:Browse:1))
    END
    BRW1::HighlightedPosition = BRW1::Position
    GET(Queue:Browse:1,1)
    RESET(BRW1::View:Browse,BRW1::Position)
    BRW1::RefreshMode = RefreshOnCurrent
  ELSE
    BRW1::HighlightedPosition = ''
    DO BRW1::Reset
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::RefreshMode = RefreshOnBottom
    BRW1::FillDirection = FillBackward
  ELSE
    BRW1::FillDirection = FillForward
  END
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  IF BRW1::HighlightedPosition
    IF BRW1::ItemsToFill
      IF NOT BRW1::RecordCount
        DO BRW1::Reset
      END
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::FillDirection = FillForward
      ELSE
        BRW1::FillDirection = FillBackward
      END
      DO BRW1::FillRecord
    END
  END
  IF BRW1::RecordCount
    IF BRW1::HighlightedPosition
      LOOP BRW1::CurrentChoice = 1 TO BRW1::RecordCount
        GET(Queue:Browse:1,BRW1::CurrentChoice)
        IF BRW1::Position = BRW1::HighlightedPosition THEN BREAK.
      END
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    ELSE
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::CurrentChoice = RECORDS(Queue:Browse:1)
      ELSE
        BRW1::CurrentChoice = 1
      END
    END
    ?Browse:1{Prop:Selected} = BRW1::CurrentChoice
    DO BRW1::FillBuffer
    ?View:2{PROP:Disable} = 0
  ELSE
    CLEAR(EmpPos:Record)
    BRW1::CurrentChoice = 0
    ?View:2{PROP:Disable} = 1
  END
  SETCURSOR()
  BRW1::RefreshMode = 0
  EXIT
BRW1::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::LoadPending = False
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = ''
    END
    SET(EmpPosCSV)
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    WATCH(BRW1::View:Browse)
    REGET(BRW1::View:Browse,BRW1::Position)
  END
!----------------------------------------------------------------------
BRW1::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
!----------------------------------------------------------------
BRW1::ButtonView ROUTINE
!|
!| This routine calls the BrowseBox`s update procedure (as specified in the
!| BrowseUpdateButtons control template) to view a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW1::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the change is successful (GlobalRequest = RequestCompleted) then the newly changed
!| record is displayed in the BrowseBox.
!|
!| If the change is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = ViewRecord
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow

!----------------------------------------------------------------
BRW1::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW1::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the insert is successful (GlobalRequest = RequestCompleted) then the newly added
!| record is displayed in the BrowseBox, at the top of the listbox.
!|
!| If the insert is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  GET(EmpPosCSV,0)
  CLEAR(EmpPos:Record,0)
  LocalRequest = InsertRecord
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateEmpPosCSV) is called.
!|
!| Upon return from the update, the routine BRW1::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW1::View:Browse)
  LOOP
    GlobalRequest = LocalRequest
    VCRRequest = VCRNone
    UpdateEmpPosCSV
    LocalResponse = GlobalResponse
    CASE VCRRequest
    OF VCRNone
      BREAK
    OF VCRInsert
      IF LocalRequest = ChangeRecord THEN
        LocalRequest = InsertRecord
      END
    OROF VCRForward
      IF LocalRequest = InsertRecord THEN
        GET(EmpPosCSV,0)
        CLEAR(EmpPos:Record,0)
      ELSE
        DO BRW1::PostVCREdit1
        BRW1::CurrentEvent = EVENT:ScrollDown
        DO BRW1::ScrollOne
        DO BRW1::PostVCREdit2
      END
    OF VCRBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollUp
      DO BRW1::ScrollOne
      DO BRW1::PostVCREdit2
    OF VCRPageForward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageDown
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRPageBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageUp
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRFirst
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollTop
      DO BRW1::ScrollEnd
      DO BRW1::PostVCREdit2
    OF VCRLast
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollBottom
      DO BRW1::ScrollEND
      DO BRW1::PostVCREdit2
    END
  END
  DO BRW1::Reset

BRW1::PostVCREdit1 ROUTINE
  DO BRW1::Reset
  BRW1::LocateMode = LocateOnEdit
  DO BRW1::LocateRecord
  DO RefreshWindow

BRW1::PostVCREdit2 ROUTINE
  ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
  DO BRW1::NewSelection
  REGET(BRW1::View:Browse,BRW1::Position)
  CLOSE(BRW1::View:Browse)

!!! <summary>
!!! Generated from procedure template - Report
!!! Adjust Records per cycle to 25 x (Estimated Record Size) for BASIC reading in BYTEs
!!! </summary>
ReportEmpPosCSV PROCEDURE

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
Process:View         VIEW(EmpPosCSV)
                       PROJECT(EmpPos:BenefitsReq)
                       PROJECT(EmpPos:BilingualCodeReq)
                       PROJECT(EmpPos:Ed360Role)
                       PROJECT(EmpPos:FirstYearReq)
                       PROJECT(EmpPos:FundingSourceReq)
                       PROJECT(EmpPos:GradeAssignmentReq)
                       PROJECT(EmpPos:MaxFTESalary)
                       PROJECT(EmpPos:MinFTESalary)
                       PROJECT(EmpPos:Position)
                       PROJECT(EmpPos:PositionCode)
                       PROJECT(EmpPos:PrimaryWorkLocationReq)
                     END
PrintSkipDetails     BOOL,AUTO
SkipPreview          BYTE
PreviewQueueIndex    LONG
LocalOutputFileQueue PrintPreviewFileQueue
PrintPreviewQueue    PrintPreviewFileQueue
ShowOutputProgress   BYTE(1)
Progress:Thermometer BYTE
Report               REPORT('EmpPosCSV Report'),AT(250,917,8000,9583),PRE(RPT),PAPER(PAPER:LETTER),FONT('Microsoft ' & |
  'Sans Serif',8,,FONT:regular),THOUS
                       HEADER,AT(250,250,8000,687),USE(?Header),FONT('Microsoft Sans Serif',8,,FONT:regular)
                         STRING('Report EmpPosCSV file'),AT(0,20,8000,220),USE(?ReportTitle),FONT('Microsoft Sans Serif', |
  8,,FONT:regular),CENTER
                         BOX,AT(0,350,8000,240),USE(?HeaderBox),COLOR(COLOR:Black),LINEWIDTH(1)
                         STRING('Code'),AT(50,390,,170),USE(?HeaderTitle:1),TRN
                         STRING('Position'),AT(490,385,,170),USE(?HeaderTitle:2),TRN
                         STRING('Max Salary'),AT(2375,385,,170),USE(?HeaderTitle:3),TRN
                         STRING('Min Salary'),AT(3062,385,,170),USE(?HeaderTitle:4),TRN
                         STRING('1st Yr'),AT(4604,385,,170),USE(?HeaderTitle:5),TRN
                         STRING('Benefits'),AT(5354,385,,170),USE(?HeaderTitle:6),TRN
                         STRING('Source'),AT(3729,385,,170),USE(?HeaderTitle:7),TRN
                         STRING('Bilingual'),AT(5823,385,,170),USE(?HeaderTitle:8),TRN
                         STRING('Grade'),AT(4979,385,,170),USE(?HeaderTitle:9),TRN
                         STRING('Primary'),AT(4177,385,,170),USE(?HeaderTitle:11),TRN
                         STRING('Ed 360 Role'),AT(6302,385,,170),USE(?HeaderTitle:12),TRN
                       END
Detail                 DETAIL,AT(0,0,8000,281),USE(?Detail)
                         STRING(@s3),AT(50,50,,170),USE(EmpPos:PositionCode)
                         STRING(@s128),AT(365,52,1900,170),USE(EmpPos:Position)
                         STRING(@n10.2),AT(2354,52,573,170),USE(EmpPos:MaxFTESalary),RIGHT
                         STRING(@n10.2),AT(3010,52,573,170),USE(EmpPos:MinFTESalary),RIGHT
                         STRING(@s3),AT(4604,52,,170),USE(EmpPos:FirstYearReq)
                         STRING(@s3),AT(5427,52,,170),USE(EmpPos:BenefitsReq)
                         STRING(@s3),AT(3833,52,,170),USE(EmpPos:FundingSourceReq)
                         STRING(@s3),AT(5771,52,,170),USE(EmpPos:BilingualCodeReq)
                         STRING(@s3),AT(5083,52,,170),USE(EmpPos:GradeAssignmentReq)
                         STRING(@s3),AT(4177,52,,170),USE(EmpPos:PrimaryWorkLocationReq)
                         STRING(@s40),AT(6302,52,1900,170),USE(EmpPos:Ed360Role)
                       END
                       FOOTER,AT(250,10500,8000,250),USE(?Footer)
                         STRING('Date:'),AT(115,52,344,135),USE(?ReportDatePrompt:2),FONT('Arial',8,,FONT:regular), |
  TRN
                         STRING('<<-- Date Stamp -->'),AT(490,52,927,135),USE(?ReportDateStamp:2),FONT('Arial',8,,FONT:regular), |
  TRN
                         STRING('Time:'),AT(1625,52,271,135),USE(?ReportTimePrompt:2),FONT('Arial',8,,FONT:regular), |
  TRN
                         STRING('<<-- Time Stamp -->'),AT(1927,52,927,135),USE(?ReportTimeStamp:2),FONT('Arial',8,, |
  FONT:regular),TRN
                         STRING(@pPage <<#p),AT(6950,52,700,135),USE(?PageCount:2),FONT('Arial',8,,FONT:regular),PAGENO
                       END
                       FORM,AT(250,250,8000,10500),USE(?Form),FONT('Microsoft Sans Serif',8,,FONT:regular)
                         IMAGE,AT(0,0,8000,10500),USE(?FormImage),TILED
                       END
                     END
ProgressWindow       WINDOW('Report EmpPosCSV'),AT(,,142,59),FONT('Microsoft Sans Serif',8,,FONT:regular),DOUBLE, |
  CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(46,42,49,15),USE(?Progress:Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel Report'), |
  TIP('Cancel Report')
                     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  SkipPreview = False
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:DoResize
      ForceRefresh = True
      DO RefreshWindow
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
         DO GetFirstRecord
         IF LocalResponse = RequestCancelled
           POST(EVENT:CloseWindow)
           CYCLE
         END
       OPEN(Report)
       IF NOT ERRORCODE() THEN
         Report$?ReportDateStamp:2{PROP:Text}=FORMAT(TODAY(),@D17)
       END
       IF NOT ERRORCODE() THEN
         Report$?ReportTimeStamp:2{PROP:Text}=FORMAT(CLOCK(),@T7)
       END
       Report{Prop:Preview} = PrintPreviewQueue.Filename
       Do SetStaticControlsAttributes
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?Progress:Thermometer)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF EVENT:Timer
       !Set the MRP to RecordsPerCycle
       IF Process:View{PROP:IPRequestCount}=0
          Process:View{PROP:IPRequestCount}=RecordsPerCycle
       END
       LOOP RecordsPerCycle TIMES
         Do SetDynamicControlsAttributes
         PrintSkipDetails = FALSE
         
         IF ~PrintSkipDetails THEN
           PRINT(RPT:Detail)
         END
         LOOP
           DO GetNextRecord
           DO ValidateRecord
           CASE RecordStatus
             OF Record:OutOfRange
               LocalResponse = RequestCancelled
               BREAK
             OF Record:OK
               BREAK
           END
         END
         IF LocalResponse = RequestCancelled
            LocalResponse = RequestCompleted
            BREAK
         END
         LocalResponse = RequestCancelled
       END
       IF LocalResponse = RequestCompleted
         POST(Event:CloseWindow)
       END
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
         LocalResponse = RequestCancelled
         POST(EVENT:CloseWindow)
      END
    END
  END
  IF SEND(EmpPosCSV,'QUICKSCAN=off').
  IF LocalResponse = RequestCompleted
    ENDPAGE(Report)
    IF NOT SkipPreview THEN
       ReportPreview(PrintPreviewQueue)
    ELSE
       GlobalResponse = RequestCompleted
    END
    IF GlobalResponse = RequestCompleted
          FREE(LocalOutputFileQueue)
          LOOP PreviewQueueIndex=1 TO RECORDS(PrintPreviewQueue)
               GET(PrintPreviewQueue,PreviewQueueIndex)
               IF NOT ERRORCODE() THEN
                  LocalOutputFileQueue.FileName = PrintPreviewQueue.FileName
                  ADD(LocalOutputFileQueue)
               END
          END
          Do ProcessOutputFileQueue
          FREE(LocalOutputFileQueue)
          Report{PROP:FlushPreview} = True
    END
  END
  CLOSE(Report)
  FREE(PrintPreviewQueue)
  DO ProcedureReturn
!---------------------------------------------------------------------------
!-----------------------------------------------------------------------------
ValidateRecord       ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  RecordStatus = Record:OutOfRange
  IF LocalResponse = RequestCancelled THEN EXIT.
  RecordStatus = Record:OK
  EXIT
GetNextRecord ROUTINE
!|
!| This routine is used to retrieve the next record from the VIEW.
!|
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  NEXT(Process:View)
  IF ERRORCODE()
     IF ERRORCODE() <> BadRecErr
       StandardWarning(Warn:RecordFetchError,'EmpPosCSV')
     END
     LocalResponse = RequestCancelled
     EXIT
  ELSE
     LocalResponse = RequestCompleted
  END
  RecordsProcessed += BYTES(EmpPosCSV)
  RecordsThisCycle += BYTES(EmpPosCSV)
  IF PercentProgress < 100
     PercentProgress = (RecordsProcessed / RecordsToProcess)*100
     IF PercentProgress > 100
        PercentProgress = 100
     END
     IF PercentProgress <> Progress:Thermometer THEN
        Progress:Thermometer = PercentProgress
        ?Progress:PctText{Prop:Text} = FORMAT(PercentProgress,@N3) & '% Completed'
        DISPLAY()
     END
  END
GetFirstRecord ROUTINE
!|
!| This routine open the view, set the range and the filter and also
!| is used to retrieve the first record from the VIEW.
!| 
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  SET(EmpPosCSV)
  Process:View{Prop:Filter} = ''
  OPEN(Process:View)
  IF ERRORCODE()
     StandardWarning(Warn:ViewOpenError)
  END
  IF CLIP(Process:View{PROP:Order}) THEN
     SET(Process:View)
  END
  LOOP
     DO GetNextRecord
     DO ValidateRecord
     CASE RecordStatus
        OF Record:Ok
           BREAK
        OF Record:OutOfRange
           LocalResponse = RequestCancelled
           BREAK
     END
  END
ProcessOutputFileQueue          ROUTINE
SetStaticControlsAttributes     ROUTINE
SetDynamicControlsAttributes    ROUTINE
PrepareProcedure ROUTINE
  IF EmpPosCSV::Used = 0
    CheckOpen(EmpPosCSV,1)
  END
  EmpPosCSV::Used += 1
  FilesOpened = TRUE
  DO BindFields
    RecordsToProcess = EmpPosCSV{PROP:FileSize}
    RecordsPerCycle = 1000
    RecordsProcessed = 0
    PercentProgress = 0
    RecordsPerCycle = 25 * 100  !Process fix for BASIC
    !25 records per cycle is the normal for keyed file likes TPS
    !BASIC file is measured in BYTES
    !1000 is not bad but a better number is 25 * Estimated Record size
  OPEN(ProgressWindow)
  WindowOpened=True
  Do DefineListboxStyle
  ProgressWindow{Prop:Text} = 'Generating Report'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  ?Progress:UserString{Prop:Text}=''
  SEND(EmpPosCSV,'QUICKSCAN=on')

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(EmpPos:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    CLOSE(Process:View)
    EmpPosCSV::Used -= 1
    IF EmpPosCSV::Used = 0 THEN CLOSE(EmpPosCSV).
  END
  IF WindowOpened
    CLOSE(ProgressWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF ProgressWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
!!! <summary>
!!! Generated from procedure template - Form
!!! Form EmpPosCSV
!!! </summary>
UpdateEmpPosCSV PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
Update::Reloop  BYTE
Update::Error   BYTE
SAV::EmpPos:Record   LIKE(EmpPos:Record)
QuickWindow          WINDOW('Form EmpPosCSV'),AT(,,358,196),FONT('Microsoft Sans Serif',8,,FONT:regular),GRAY,IMM, |
  MDI,HLP('UpdateEmpPosCSV'),SYSTEM
                       PROMPT('Position Code:'),AT(8,6),USE(?EmpPos:PositionCode:Prompt),TRN
                       ENTRY(@s3),AT(116,6,40,10),USE(EmpPos:PositionCode)
                       PROMPT('Position:'),AT(8,20),USE(?EmpPos:Position:Prompt),TRN
                       ENTRY(@s128),AT(116,20,234,10),USE(EmpPos:Position)
                       PROMPT('Max FTE Salary:'),AT(8,34),USE(?EmpPos:MaxFTESalary:Prompt),TRN
                       ENTRY(@n10.2),AT(116,34,52,10),USE(EmpPos:MaxFTESalary),RIGHT(2)
                       PROMPT('Min FTE Salary:'),AT(8,49),USE(?EmpPos:MinFTESalary:Prompt),TRN
                       ENTRY(@n10.2),AT(116,49,52,10),USE(EmpPos:MinFTESalary),RIGHT(2)
                       PROMPT('First Year Req:'),AT(8,63),USE(?EmpPos:FirstYearReq:Prompt),TRN
                       ENTRY(@s3),AT(116,63,25,10),USE(EmpPos:FirstYearReq)
                       PROMPT('Benefits Req:'),AT(8,76),USE(?EmpPos:BenefitsReq:Prompt),TRN
                       ENTRY(@s3),AT(116,76,25,10),USE(EmpPos:BenefitsReq)
                       PROMPT('Funding Source Req:'),AT(8,90),USE(?EmpPos:FundingSourceReq:Prompt),TRN
                       ENTRY(@s3),AT(116,90,25,10),USE(EmpPos:FundingSourceReq)
                       PROMPT('Bilingual Code Req:'),AT(8,105),USE(?EmpPos:BilingualCodeReq:Prompt),TRN
                       ENTRY(@s3),AT(116,105,25,10),USE(EmpPos:BilingualCodeReq)
                       PROMPT('Grade Assignment Req:'),AT(8,118),USE(?EmpPos:GradeAssignmentReq:Prompt),TRN
                       ENTRY(@s3),AT(116,118,25,10),USE(EmpPos:GradeAssignmentReq)
                       PROMPT('School Work Location Req:'),AT(8,132),USE(?EmpPos:SchoolWorkLocationReq:Prompt),TRN
                       ENTRY(@s3),AT(116,132,25,10),USE(EmpPos:SchoolWorkLocationReq)
                       PROMPT('Primary Work Location Req:'),AT(8,146),USE(?EmpPos:PrimaryWorkLocationReq:Prompt), |
  TRN
                       ENTRY(@s3),AT(116,146,25,10),USE(EmpPos:PrimaryWorkLocationReq)
                       PROMPT('Ed 360 Role:'),AT(8,161),USE(?EmpPos:Ed360Role:Prompt),TRN
                       ENTRY(@s40),AT(116,161,234,10),USE(EmpPos:Ed360Role)
                       PROMPT('Data Type:'),AT(8,175),USE(?EmpPos:DataType:Prompt),TRN
                       ENTRY(@s10),AT(116,175,44,10),USE(EmpPos:DataType)
                       BUTTON('&OK'),AT(211,86,53,20),USE(?OK),LEFT,ICON('WAOK.ICO'),DEFAULT,FLAT,MSG('Accept dat' & |
  'a and close the window'),TIP('Accept data and close the window')
                       BUTTON('&Cancel'),AT(272,86,53,20),USE(?Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel operation'), |
  TIP('Cancel operation')
                     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  CASE LocalRequest
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    IF StandardWarning(Warn:InsertDisabled)
      DO ProcedureReturn
    END
  OF ChangeRecord
    IF StandardWarning(Warn:UpdateDisabled)
      DO ProcedureReturn
    END
  OF DeleteRecord
    IF StandardWarning(Warn:DeleteDisabled)
      DO ProcedureReturn
    END
  END
  QuickWindow{PROP:StatusText,0} = ActionMessage
  DO FORM:ViewRecordMode
  ACCEPT
    CASE EVENT()
    OF EVENT:CloseDown
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
    OF EVENT:CloseWindow
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
    OF EVENT:DoResize
      ForceRefresh = True
      DO RefreshWindow
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?EmpPos:PositionCode:Prompt)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF EVENT() = EVENT:Completed
        CASE LocalRequest
        END
      END
    END
    CASE FIELD()
    OF ?OK
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        IF LocalRequest <> ViewRecord THEN
          IF OriginalRequest = ChangeRecord OR OriginalRequest = InsertRecord
            SELECT()
          ELSE
            POST(EVENT:Completed)
          END
        END
      END
    OF ?Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        LocalResponse = RequestCancelled
        POST(EVENT:CloseWindow)
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF EmpPosCSV::Used = 0
    CheckOpen(EmpPosCSV,1)
  END
  EmpPosCSV::Used += 1
  FilesOpened = TRUE
  DO BindFields
  RISnap:EmpPosCSV
  SAV::EmpPos:Record = EmpPos:Record
  OPEN(QuickWindow)
  WindowOpened=True
  Do DefineListboxStyle

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(EmpPos:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    EmpPosCSV::Used -= 1
    IF EmpPosCSV::Used = 0 THEN CLOSE(EmpPosCSV).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
ClosingWindow ROUTINE
  Update::Reloop = 0
  IF LocalResponse <> RequestCompleted
    RecordChanged = FALSE
    DO CancelAutoIncrement
  END

CancelAutoIncrement ROUTINE
  IF LocalResponse <> RequestCompleted
  END
!----------------------------------------------------------------
FORM:ViewRecordMode  ROUTINE
  IF LocalRequest = ViewRecord THEN
    ?EmpPos:PositionCode{PROP:ReadOnly} = TRUE
    ?EmpPos:Position{PROP:ReadOnly} = TRUE
    ?EmpPos:MaxFTESalary{PROP:ReadOnly} = TRUE
    ?EmpPos:MinFTESalary{PROP:ReadOnly} = TRUE
    ?EmpPos:FirstYearReq{PROP:ReadOnly} = TRUE
    ?EmpPos:BenefitsReq{PROP:ReadOnly} = TRUE
    ?EmpPos:FundingSourceReq{PROP:ReadOnly} = TRUE
    ?EmpPos:BilingualCodeReq{PROP:ReadOnly} = TRUE
    ?EmpPos:GradeAssignmentReq{PROP:ReadOnly} = TRUE
    ?EmpPos:SchoolWorkLocationReq{PROP:ReadOnly} = TRUE
    ?EmpPos:PrimaryWorkLocationReq{PROP:ReadOnly} = TRUE
    ?EmpPos:Ed360Role{PROP:ReadOnly} = TRUE
    ?EmpPos:DataType{PROP:ReadOnly} = TRUE
    DISABLE(?OK)
  END
!----------------------------------------------------------------
!!! <summary>
!!! Generated from procedure template - Process
!!! Process the EmpPosCSV File
!!! </summary>
ProcessEmpPosCSV PROCEDURE

CntRecords LONG
CntTimers  LONG
RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
Process:View         VIEW(EmpPosCSV)
                     END
Progress:Thermometer BYTE
ProgressWindow       WINDOW('Process EmpPosCSV'),AT(,,142,59),FONT('Microsoft Sans Serif',8,,FONT:regular),DOUBLE, |
  CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(46,42,49,15),USE(?Progress:Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel Process'), |
  TIP('Cancel Process')
                     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:DoResize
      ForceRefresh = True
      DO RefreshWindow
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
         DO GetFirstRecord
         IF LocalResponse = RequestCancelled
           POST(EVENT:CloseWindow)
           CYCLE
         END
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?Progress:Thermometer)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF EVENT:Timer
        CntTimers += 1
        RecordsThisCycle = 0
        !Set the MRP to RecordsPerCycle
        IF Process:View{PROP:IPRequestCount}=0
          Process:View{PROP:IPRequestCount}=RecordsPerCycle
        END
        LOOP WHILE RecordsThisCycle < RecordsPerCycle
            CntRecords += 1
          LOOP
             DO GetNextRecord
             DO ValidateRecord
             CASE RecordStatus
               OF Record:OutOfRange
                  LocalResponse = RequestCancelled
                  BREAK
               OF Record:OK
                  BREAK
             END
          END
          IF LocalResponse = RequestCancelled
             LocalResponse = RequestCompleted
             BREAK
          END
          LocalResponse = RequestCancelled
        END
        IF LocalResponse = RequestCompleted
          0{PROP:Timer} = 0
          ?Progress:PctText{Prop:Text} = 'Process Completed'
          DISPLAY(?Progress:PctText)
          POST(Event:CloseWindow)
        END
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
          LocalResponse = RequestCancelled
          0{PROP:Timer} = 0
          POST(EVENT:CloseWindow)
      END
    END
  END
  IF SEND(EmpPosCSV,'QUICKSCAN=off').
  IF LocalResponse = RequestCompleted
    Message('Read EmpPosCSV Process complete' & |
            '|Record Count: ' & CntRecords & |
            '|Timer Cycles: ' & CntTimers & | 
           '||Template Variables' & |            
            '|   RecordsToProcess: ' & RecordsToProcess & |            
            '|   RecordsProcessed: ' & RecordsProcessed & |            
            '|   RecordsPerCycle: ' & RecordsPerCycle & |            
            '', 'ProcessEmpPosCSV')
            
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
!-----------------------------------------------------------------------------
ValidateRecord       ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  RecordStatus = Record:OutOfRange
  IF LocalResponse = RequestCancelled THEN EXIT.
  RecordStatus = Record:OK
  EXIT
GetNextRecord ROUTINE
!|
!| This routine is used to retrieve the next record from the VIEW.
!|
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  NEXT(Process:View)
  IF ERRORCODE()
     IF ERRORCODE() <> BadRecErr
       StandardWarning(Warn:RecordFetchError,'EmpPosCSV')
     END
     LocalResponse = RequestCancelled
     EXIT
  ELSE
     LocalResponse = RequestCompleted
  END
  RecordsProcessed += BYTES(EmpPosCSV)
  RecordsThisCycle += BYTES(EmpPosCSV)
  IF PercentProgress < 100
     PercentProgress = (RecordsProcessed / RecordsToProcess)*100
     IF PercentProgress > 100
        PercentProgress = 100
     END
     IF PercentProgress <> Progress:Thermometer THEN
        Progress:Thermometer = PercentProgress
        ?Progress:PctText{Prop:Text} = FORMAT(PercentProgress,@N3) & '% Completed'
        DISPLAY()
     END
  END
GetFirstRecord ROUTINE
!|
!| This routine open the view, set the range and the filter and also
!| is used to retrieve the first record from the VIEW.
!| 
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  SET(EmpPosCSV)
  Process:View{Prop:Filter} = ''
  OPEN(Process:View)
  IF ERRORCODE()
     StandardWarning(Warn:ViewOpenError)
  END
  IF CLIP(Process:View{PROP:Order}) THEN
     SET(Process:View)
  END
  LOOP
     DO GetNextRecord
     DO ValidateRecord
     CASE RecordStatus
        OF Record:Ok
           BREAK
        OF Record:OutOfRange
           LocalResponse = RequestCancelled
           BREAK
     END
  END
PrepareProcedure ROUTINE
  IF EmpPosCSV::Used = 0
    CheckOpen(EmpPosCSV,1)
  END
  EmpPosCSV::Used += 1
  FilesOpened = TRUE
  DO BindFields
  RecordsToProcess = EmpPosCSV{PROP:FileSize}
  RecordsPerCycle = 1000
  RecordsProcessed = 0
  PercentProgress = 0
    CASE Message('Process template measures BASIC and ASCII using BYTES.' & |
                 '||RecordsPerCycle is best set to 25 * Average Record Length' & |
                 '|so more records are processed per timer event.' & |
                 '||Template default is RecordsPerCycle=' & RecordsPerCycle & |
                 '||File is 7200 bytes for 71 record so about 100/record.' & |
                 '','ProcessEmpPosCSV',,'Default ' & RecordsPerCycle &'|2500=25*100')
    OF 2 ; RecordsPerCycle = 25 * 100    !Better to set to 25 * Average Record 
    END
  OPEN(ProgressWindow)
  WindowOpened=True
  Do DefineListboxStyle
  ProgressWindow{Prop:Text} = 'Processing Records'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  ?Progress:UserString{Prop:Text}=''
  SEND(EmpPosCSV,'QUICKSCAN=on')

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(EmpPos:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    CLOSE(Process:View)
    EmpPosCSV::Used -= 1
    IF EmpPosCSV::Used = 0 THEN CLOSE(EmpPosCSV).
  END
  IF WindowOpened
    CLOSE(ProgressWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF ProgressWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
