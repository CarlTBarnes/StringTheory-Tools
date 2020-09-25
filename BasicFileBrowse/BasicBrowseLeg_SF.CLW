                  MEMBER('BasicBrowseLeg.clw')

                   GROUP,PRE(CS:)
OverrideCharacters   STRING('`!"£$%^&*()''-=_+][#;~@:/.,?\| ')
NumericCharacters    STRING('0123456789')
SortCharacters       STRING(255)
SortCharacterCount   LONG
StringRoot           STRING(255)
StringRootLen        LONG
StepValue            LONG
CurrentStep          LONG
LowValue             LONG
HighValue            LONG
LowString            STRING(4)
HighString           STRING(4)
                   END

                   GROUP,PRE(CR:)
StepValue            REAL
CurrentStep          LONG
LowLimit             REAL
HighLimit            REAL
                   END

SetupRealStops PROCEDURE(REAL InputLowLimit,REAL InputHighLimit)
  CODE
  CR::LowLimit = InputLowLimit
  CR::HighLimit = InputHighLimit
  CR::StepValue = (InputHighLimit - InputLowLimit) / 99
  CR::CurrentStep = 0
  RETURN

NextRealStop  FUNCTION
  CODE
  CR::CurrentStep += 1
  CASE CR::CurrentStep
  OF 1
    RETURN(CR::LowLimit)
  OF 100
    RETURN(CR::HighLimit)
  ELSE
    RETURN(CR::LowLimit + (CR::StepValue * (CR::CurrentStep-1)))
  END

SetupStringStops PROCEDURE(STRING ProcessLowLimit,STRING ProcessHighLimit,LONG InputStringSize,<LONG ListType>)
InputLowLimit      STRING(255)
InputHighLimit     STRING(255)
CurrentStep        LONG
CurrentVal         LONG
LowCharacter       STRING(1)
HighCharacter      STRING(1)
NextCharacter      STRING(1)
  CODE
  IF OMITTED(4)
    Listtype = ScrollSort:AllowAlpha
  END
  CS::SortCharacterCount = 0
  CS::SortCharacters = ''
  LOOP CurrentStep = 1 TO 255
    NextCharacter = CHR(CurrentStep)
    IF INSTRING(NextCharacter,CS::NumericCharacters,1,1)
      IF BAND(Listtype,ScrollSort:AllowNumeric)
        DO AddSortCharacter
      END
    ELSIF INSTRING(NextCharacter,CS::OverrideCharacters,1,1)
      IF BAND(Listtype,ScrollSort:AllowAlt)
        DO AddSortCharacter
      END
    ELSIF ISALPHA(NextCharacter)
      IF BAND(Listtype,ScrollSort:AllowAlpha)
        IF ISUPPER(NextCharacter)
          DO AddSortCharacter
        ELSIF BAND(Listtype,ScrollSort:CaseSensitive)
          DO AddSortCharacter
        END
      END
    END
  END
  CurrentStep = 0
  InputLowLimit = SUB(ProcessLowLimit,1,InputStringSize)
  InputHighLimit = SUB(ProcessHighLimit,1,InputStringSize)
  IF LEN(CLIP(InputHighLimit))<InputStringSize
    NextCharacter = SUB(CS::SortCharacters,CS::SortCharacterCount,1)
    InputHighLimit = CLIP(InputHighLimit) & ALL(NextCharacter,(InputStringSize+1)-LEN(CLIP(InputHighLimit)))
  END
  LOOP CurrentStep = 1 TO InputStringSize
    IF CurrentStep > LEN(InputLowLimit) THEN BREAK.
    LowCharacter = SUB(InputLowLimit,CurrentStep,1)
    HighCharacter = SUB(InputHighLimit,CurrentStep,1)
    IF LowCharacter <> HighCharacter THEN BREAK.
  END
  CS::StringRootLen = CurrentStep - 1
  CS::StringRoot = SUB(InputLowLimit,1,CS::StringRootLen)
  CS::LowString = SUB(InputLowLimit,CurrentStep,4)
  CS::HighString = SUB(InputHighLimit,CurrentStep,4)
  IF BAND(ListType,ScrollSort:AllowAlpha) AND NOT BAND(ListType,ScrollSort:CaseSensitive)
    CS::LowString = UPPER(CS::LowString)
    CS::HighString = UPPER(CS::HighString)
  END
  CurrentStep = 0
  CS::LowValue = 0
  CS::HighValue = 0
  LOOP
    CurrentStep += 1
    IF CurrentStep = 5 THEN BREAK.
    CS::LowValue *= CS::SortCharacterCount + 1
    CS::HighValue *= CS::SortCharacterCount + 1
    IF CurrentStep <= LEN(CLIP(CS::LowString))
      NextCharacter = SUB(CS::LowString,CurrentStep,1)
      CurrentVal = INSTRING(NextCharacter,CS::SortCharacters,1,1)
      CS::LowValue += CurrentVal
    END
    IF CurrentStep <= LEN(CLIP(CS::HighString))
      NextCharacter = SUB(CS::HighString,CurrentStep,1)
      CurrentVal = INSTRING(NextCharacter,CS::SortCharacters,1,1)
      CS::HighValue += CurrentVal
    END
  END
  CS::StepValue = (CS::HighValue - CS::LowValue) / 99
  CS::CurrentStep = 0
  RETURN

AddSortCharacter ROUTINE
  IF CS::SortCharacterCount
    CS::SortCharacters = SUB(CS::SortCharacters,1,CS::SortCharacterCount) & NextCharacter
  ELSE
    CS::SortCharacters = NextCharacter
  END
  CS::SortCharacterCount += 1

NextStringStop  FUNCTION
StepString      STRING(4)
CurrentStop     LONG
CurrentStep     LONG
CurrentVal      LONG
  CODE
  StepString = ''
  CS::CurrentStep += 1
  CASE CS::CurrentStep
  OF 1
    StepString = CS::LowString
  OF 100
    StepString = CS::HighString
  ELSE
    CurrentStop = CS::LowValue + (CS::StepValue * (CS::CurrentStep-1))
    LOOP CurrentStep = 1 TO 4
      CurrentVal = CurrentStop % (CS::SortCharacterCount + 1)
      CurrentStop = (CurrentStop - CurrentVal) / (CS::SortCharacterCount + 1)
      StepString = SUB(CS::SortCharacters,Currentval,1) & StepString
    END
  END
  IF CS::StringRootLen
    RETURN(SUB(CS::StringRoot,1,CS::StringRootLen) & StepString)
  ELSE
    RETURN(StepString)
  END
CheckOpen         PROCEDURE(FILE File,<BYTE OverrideCreate>,<BYTE OverrideOpenMode>)
OpenMode          BYTE
  CODE
  IF OMITTED(3)
    OpenMode = 42h
  ELSE
    OpenMode = OverrideOpenMode
  END
  OPEN(File,OpenMode)
  IF ERRORCODE() = IsLockedErr
    SEND(File,'Recover=10')
    OPEN(File,OpenMode)
  END
  CASE ERRORCODE()                                         ! and check for errors
  OF NoError                                               !Return if no error
  OROF IsOpenErr                                           ! or if already open.
    DO ProcedureReturn
  OF NoFileErr                                             !If file was not found
    IF OMITTED(2)
    ELSIF OverrideCreate = TRUE
      DO CreateFile
    ELSE
      IF StandardWarning(Warn:CreateError,NAME(File)).
    END
  OF InvalidFileErr                                        !Invalid Record Declaration
    IF StandardWarning(Warn:InvalidFile,NAME(File)).
  OF BadKeyErr                                             !Key Files must be rebuilt
    IF StandardWarning(Warn:InvalidKey,NAME(File))
      BUILD(File)                              !Rebuild the key files
    END
    IF ERRORCODE()
      IF StandardWarning(Warn:RebuildError,NAME(File)).
    ELSE
      IF OMITTED(3)
        OPEN(File,42h)                                     !Attempt to open the file
      ELSE
        OPEN(File,OverrideOpenMode)
      END
    END
  END                                                      !End of Case Structure
  IF ERRORCODE()
    IF StandardWarning(Warn:DiskError,NAME(File)) THEN HALT(0,'Disk Error').
  END
  DO ProcedureReturn
!--------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
  RETURN
!--------------------------------------
CreateFile ROUTINE
!|
!| This routine is called to CREATE the file passed to CheckOpen.
!|
  CREATE(File)                                             !Create the file
  IF ERRORCODE()
    IF ERRORCODE() = 90
      IF StandardWarning(Warn:CreateError,NAME(File)).
    ELSE
      IF StandardWarning(Warn:CreateError,NAME(File)).
    END
  END
  IF OMITTED(3)
    OPEN(File,42h)                                         !Attempt to open the file
  ELSE
    OPEN(File,OverrideOpenMode)
  END
  IF ~ERRORCODE()                                          !  And return if it opened
    DO ProcedureReturn
  ELSE
    IF StandardWarning(Warn:CreateOpenError,NAME(File)).
  END

ReportPreview              PROCEDURE (QUEUE PrintPreviewQueue)

CurrentPage                    LONG
PagesAcross                    LONG
PagesDown                      LONG
DisplayedPagesAcross           LONG
DisplayedPagesDown             LONG

ZoomNoZoom                     BYTE
ZoomPageWidth                  BYTE
ZoomPageHeight                 BYTE
Zoom50                         BYTE
Zoom75                         BYTE
Zoom100                        BYTE
Zoom200                        BYTE
Zoom300                        BYTE
ZoomModifier                   REAL

TemporaryImage                 EQUATE(100)
ZoomBox                        EQUATE(102)
ZoomImage                      EQUATE(103)
LowestBorderEquate             EQUATE(104)
LowestImageEquate              EQUATE(204)
HighestControlEquate           EQUATE(404)
MinimumXSeparation             EQUATE(6)
MinimumYSeparation             EQUATE(8)

ImageCount                     LONG
ImageWidth                     LONG
ImageHeight                    LONG
ImageAspectRatio               REAL

ThumbnailsRequired             LONG
CurrentThumbnail               LONG
ThumbnailXPosition             LONG
ThumbnailYPosition             LONG
ThumbnailHeight                LONG
ThumbnailWidth                 LONG
ThumbnailAspectRatio           REAL
ThumbnailRow                   LONG
ThumbnailColumn                LONG

ProcessedPage                  LONG

PageZoom                       REAL

CurrentImageBox                LONG
CurrentBorderBox               LONG

TotalXSeparation               LONG
TotalYSeparation               LONG

ThumbnailsPresent              LONG
ZoomPresent                    BYTE

ReturnValue                    BYTE(FALSE)

Zoomed                         BYTE(FALSE)
ZoomSelection                  BYTE(0)
PopupText                      STRING(80)

Preview:Window WINDOW('Report Preview'),AT(,,,),FONT('MS Shell Dlg',10),COLOR(COLOR:Gray), |
         IMM,ICON(Icon:Print),STATUS(-1,90,70),GRAY,MAX,RESIZE,MAXIMIZE,ALRT(PgUpKey),ALRT(PgDnKey)
       MENUBAR
         MENU('&File'),USE(?FileMenu)
           ITEM('&Print'),USE(?Print),MSG('Print the report')
           ITEM,SEPARATOR
           ITEM('E&xit'),USE(?Exit),MSG('Exit without printing the report')
         END
         MENU('&View'),USE(?ViewMenu)
           ITEM('&Next Page(s)'),USE(?Next),MSG('View the next page or pages of the report')
           ITEM('&Previous Page(s)'),USE(?Previous),DISABLE,MSG('View the next page or pages of the report')
           ITEM,SEPARATOR
           ITEM('&Jump to a page'),USE(?Jump),MSG('Jump to a specific page of the report')
           ITEM,SEPARATOR
           ITEM('&Change Display'),USE(?ChangeDisplay),MSG('Change the number of pages displayed')
         END
         MENU('&Zoom'),USE(?ZoomMenu)
           ITEM('No &Zoom'),USE(ZoomNoZoom),CHECK
           ITEM,SEPARATOR
           ITEM('Page &Width'),USE(ZoomPageWidth),CHECK
           ITEM('Page &Height'),USE(ZoomPageHeight),CHECK
           ITEM,SEPARATOR
           ITEM('&50 Percent'),USE(Zoom50),CHECK
           ITEM('&75 Percent'),USE(Zoom75),CHECK
           ITEM('&100 Percent'),USE(Zoom100),CHECK
           ITEM('&200 Percent'),USE(Zoom200),CHECK
           ITEM('&300 Percent'),USE(Zoom300),CHECK
         END
       END
       TOOLBAR,AT(0,0,341,18)
         BUTTON,AT(3,2,14,14),TIP('Print this report'),USE(?PrintButton),ICON(ICON:Print)
         BUTTON,AT(19,2,14,14),TIP('Exit without printing the report'),USE(?ExitButton),ICON(ICON:NoPrint)
         BUTTON,AT(37,2,14,14),TIP('Zoom in on a page of the report'),USE(?ZoomButton),ICON(ICON:Zoom)
         PROMPT('&Page:'),AT(60,4)
         SPIN(@n4),AT(85,3,33,12),USE(CurrentPage),RANGE(1,10),STEP(1)
         PROMPT('&Across:'),AT(123,4)
         SPIN(@N2),AT(152,3,20,12),USE(PagesAcross),RANGE(1,10),STEP(1),MSG('Select the number of thumbnails in a row'),VSCROLL
         PROMPT('&Down:'),AT(177,4)
         SPIN(@n2),AT(202,3,20,12),USE(PagesDown),RANGE(1,10),STEP(1),MSG('Select the number of thumbnails in a column')
       END
     END

   CODE
   OPEN (Preview:Window)

   TARGET {PROP:MinWidth} = 315
   TARGET {PROP:Pixels}   = TRUE

   ! Retrieve first image to get image aspect ratio, then destroy the image
   GET (PrintPreviewQueue,1)

   CREATE (TemporaryImage,CREATE:Image)
   TemporaryImage{PROP:Text} = PrintPreviewQueue
   ImageWidth = TemporaryImage{PROP:MaxWidth}
   ImageHeight = TemporaryImage{PROP:MaxHeight}
   ImageAspectRatio = ImageHeight / ImageWidth
   DESTROY (TemporaryImage)
   
   ImageCount           = RECORDS(PrintPreviewQueue)
   CurrentPage          = 1
   PagesAcross          = 1
   PagesDown            = 1
   PagesDown            = 1
   DisplayedPagesAcross = 0
   DisplayedPagesDown   = 0
   ThumbnailsPresent    = 0
   ZoomPresent          = FALSE
   ZoomNoZoom           = TRUE
   
   IF ImageCount = 1
     DISABLE (?CurrentPage,?PagesDown)
     DISABLE (?ViewMenu)
   ELSE
     ?CurrentPage {PROP:RangeHigh} = ImageCount
     ?CurrentPage {PROP:Msg} = 'Enter a page number from 1 to ' & ImageCount
   END
   
   DO ChangeDisplay

   ACCEPT
     CASE EVENT()
     OF EVENT:Sized
       POST (EVENT:DoResize)
     OF EVENT:DoResize
       DO ChangeDisplay
     OF EVENT:NewSelection
       CASE FIELD()
       OF ?PagesAcross
       OROF ?PagesDown
         DO ClearZoomValues
         ZoomNoZoom = TRUE
         DO ChangeDisplay
       OF ?CurrentPage
         DO ChangeDisplay
       END
     OF EVENT:Rejected
       CASE REJECTCODE()
       OF Reject:RangeHigh
         CHANGE (FIELD(), FIELD(){PROP:RangeHigh})
       OF Reject:RangeLow
         CHANGE (FIELD(), FIELD(){PROP:RangeLow})
       END
       DO ChangeDisplay
     OF EVENT:AlertKey
       CASE KEYCODE()
       OF PgUpKey
         POST (EVENT:Accepted, ?Previous)
       OF PgDnKey
         POST (EVENT:Accepted, ?Next)
       ELSE
         CASE FIELD()
         OF ZoomImage
           DO ClearZoomValues
           ZoomNoZoom = TRUE
           DO ChangeDisplay
         ELSE
           CurrentPage = (FIELD() - LowestImageEquate) + CurrentPage
           ZoomNoZoom = FALSE
           ZoomPageWidth = TRUE
           DO ChangeDisplay
         END
       END
     OF EVENT:Accepted
       CASE FIELD()
       OF ?Print
       OROF ?PrintButton
         POST (EVENT:CloseWindow)
         ReturnValue = TRUE
       OF ?Exit
       OROF ?ExitButton
         POST (EVENT:CloseWindow)
       OF ?ZoomButton
         !           0         1         2         3         4         5         6
         !           012345678901234567890123456789012345678901234567890123456789012345
         PopupText = '-No Zoom|-|-Page Width|-Page Height|-|-50%|-75%|-100%|-200%|-300%'
         IF ZoomNoZoom
           PopupText[1] = '+'
         ELSIF ZoomPageWidth
           PopupText[12] = '+'
         ELSIF ZoomPageHeight
           PopupText[24] = '+'
         ELSIF Zoom50
           PopupText[39] = '+'
         ELSIF Zoom75
           PopupText[44] = '+'
         ELSIF Zoom100
           PopupText[49] = '+'
         ELSIF Zoom200
           PopupText[55] = '+'
         ELSIF Zoom300
           PopupText[61] = '+'
         END
         ZoomSelection = POPUP (PopupText)
         IF ZoomSelection <> 0
           DO ZoomSelected
         END
       OF ?CurrentPage
         DO ChangeDisplay
       OF ?Next
         DO GetNextPage
         DO ChangeDisplay
       OF ?Previous
         DO GetPrevPage
         DO ChangeDisplay
       OF ?Jump
         CurrentPage = Preview:JumpToPage (CurrentPage, ImageCount)
         DO ChangeDisplay
       OF ?ChangeDisplay
         Preview:SelectDisplay (PagesAcross, PagesDown)
         DO ChangeDisplay
       OF ?ZoomNoZoom TO ?Zoom300
         ZoomSelection = (FIELD() - ?ZoomNoZoom) + 1
         DO ZoomSelected
       END
     END
   END

   IF ReturnValue
     GlobalResponse = RequestCompleted
   ELSE
     GlobalResponse = RequestCancelled
   END
   RETURN

!------------------------------------------------------
ChangeDisplay ROUTINE
!|
!| This routine is called to select the current display mode of the ReportPreview
!| window.
!|
!| If any zooming is being done (ZoomNoZoom = False) then any thumbnails present
!| are destroyed. If the Zoom control is not present, it is created. Finally, the
!| Zoom control is filled with the current page and zoomed.
!|
!| If no zooming is being done, then the Zoom control, if present, is destroyed.
!| Next, if the number of pages to be displayed has changed, then any existing
!| thumbnails are destroyed, and the new thumbnails are created. Finally, the
!| thumbnails are positioned and filled.
!|
  IF ZoomNoZoom
    IF ZoomPresent
      DO DestroyZoomControl
    END
    IF PagesDown <> DisplayedPagesDown OR PagesAcross <> DisplayedPagesAcross
      IF ThumbnailsPresent
        DO DestroyThumbnails
      END
      DO CreateThumbnails
      DisplayedPagesDown = PagesDown
      DisplayedPagesAcross = PagesAcross
    END
    DO PositionThumbnails
    DO FillThumbnails
    TARGET {PROP:StatusText,3} = 'No Zoom'
  ELSE
    IF ThumbnailsPresent
      DO DestroyThumbnails
    END
    IF NOT ZoomPresent
      DO CreateZoomControl
    END
    DO FillZoomControl
    DisplayedPagesAcross = 0
    DisplayedPagesDown   = 0
  END
  DISPLAY (?ZoomNoZoom, ?Zoom300)
  DISPLAY (?CurrentPage, ?PagesDown)
  EXIT

!------------------------------------------------------
CreateZoomControl ROUTINE
!|
!| This routine is used to create the Zoom control and its background.
!|
  CREATE (ZoomBox, CREATE:Box)
  SETPOSITION (ZoomBox, 1, 1)
  ZoomBox {PROP:Color}     = COLOR:Black
  ZoomBox {PROP:Fill}      = COLOR:White
  ZoomBox {PROP:LineWidth} = 1

  CREATE (ZoomImage, CREATE:Image)
  SETPOSITION (ZoomImage, 2, 2)
  ZoomImage {PROP:VScroll}  = FALSE
  ZoomImage {PROP:HScroll}  = FALSE
  ZoomImage {PROP:Alrt,250} = MouseLeft
  ZoomImage {PROP:Cursor}   = CURSOR:Zoom

  UNHIDE (ZoomBox)
  UNHIDE (ZoomImage)
  ZoomPresent = TRUE
  EXIT

!------------------------------------------------------
DestroyZoomControl ROUTINE
!|
!| This routine is used to destroy the zoom control and its background.
!|
  HIDE (ZoomBox)
  HIDE (ZoomImage)
  DESTROY (ZoomBox)
  DESTROY (ZoomImage)
  ZoomPresent = FALSE
  EXIT

!------------------------------------------------------
FillZoomControl ROUTINE
!|
!| This routine is used to fill the Zoom control with an image, and to set the
!| image's zoom ratio to the correct value.
!|
  DATA
WWidth     LONG,AUTO
WHeight    LONG,AUTO
CWidth     LONG,AUTO
CHeight    LONG,AUTO
HS         BYTE(FALSE)
VS         BYTE(FALSE)
  CODE
  DO ConfigureZoomRatio
  GET (PrintPreviewQueue, CurrentPage)
  TARGET {PROP:StatusText,2} = 'Page ' & CurrentPage & ' of ' & ImageCount
  
  WWidth  = TARGET {PROP:ClientWidth} - 4
  WHeight = TARGET {PROP:ClientHeight} - 4
  CWidth  = ImageWidth * ZoomModifier
  CHeight = ImageHeight * ZoomModifier
 
  ZoomImage {PROP:Text} = PrintPreviewQueue
  ZoomImage {PROP:MaxWidth}  = CWidth
  ZoomImage {PROP:MaxHeight} = CHeight
  IF CWidth > WWidth
    CWidth = WWidth
    HS     = TRUE
  END
  IF CHeight > WHeight
    CHeight = WHeight
    VS      = TRUE
  END
  SETPOSITION (ZoomBox,,, CWidth + 2, CHeight + 2)
  SETPOSITION (ZoomImage,,, CWidth, CHeight)

  ZoomImage {PROP:HScroll} = HS
  ZoomImage {PROP:VScroll} = VS

  DISPLAY (ZoomBox, ZoomImage)
  EXIT

!------------------------------------------------------
GetNextPage ROUTINE
!|
!| This routine increments the CurrentPage variable so that the next
!| "page" of images are displayed.
!|
  IF ZoomNoZoom
    IF CurrentPage + ThumbnailsPresent <= ImageCount
      CurrentPage += ThumbnailsPresent
    END
  ELSE
    IF CurrentPage <> ImageCount
      CurrentPage += 1
    END
  END
  EXIT

!------------------------------------------------------
GetPrevPage ROUTINE
!|
!| This routine decrements the CurrentPage variable so that the previous
!| "page" of images are displayed.
!|
  IF ZoomNoZoom
    CurrentPage -= ThumbnailsPresent
    IF CurrentPage < 1
      CurrentPage = 1
    END
  ELSE
    IF CurrentPage <> 1
      CurrentPage -= 1
    END
  END
  EXIT

!------------------------------------------------------
CreateThumbnails ROUTINE
!|
!| This routine is used to create all thumbnail images and their backgrounds
!|
  CurrentBorderBox   = LowestBorderEquate
  CurrentImageBox    = LowestImageEquate
  ThumbnailsRequired = (PagesAcross * PagesDown)

  LOOP CurrentThumbnail = 1 TO ThumbnailsRequired
    CREATE (CurrentBorderBox, CREATE:Box)
    CREATE (CurrentImageBox, CREATE:Image)
    CurrentBorderBox += 1
    CurrentImageBox += 1
  END
  ThumbnailsPresent = ThumbnailsRequired
  EXIT

!------------------------------------------------------
DestroyThumbnails ROUTINE
!|
!| This routine is used to destroy all thumbnail images and their backgrounds
!|
   HIDE (LowestBorderEquate, HighestControlEquate)
   DESTROY (LowestBorderEquate, HighestControlEquate)
   ThumbnailsPresent = 0
   EXIT

!------------------------------------------------------
PositionThumbnails ROUTINE
!|
!| This routine is used to position all of the thumbnails and their backgrounds.
!|
  TotalXSeparation     = 2 + ((PagesAcross - 1) * MinimumXSeparation)
  TotalYSeparation     = 2 + ((PagesDown - 1) * MinimumYSeparation)
  ThumbnailWidth       = (TARGET{PROP:ClientWidth} - TotalXSeparation) / PagesAcross
  ThumbnailHeight      = ((TARGET{PROP:ClientHeight} - 18) - TotalYSeparation) / PagesDown
  ThumbnailAspectRatio = ThumbnailHeight / ThumbnailWidth

  IF ThumbnailAspectRatio < ImageAspectRatio
    ThumbnailWidth = ThumbnailHeight / ImageAspectRatio
  ELSE
    ThumbnailHeight = ThumbnailWidth * ImageAspectRatio
  END
  
  CurrentBorderBox   = LowestBorderEquate
  CurrentImageBox    = LowestImageEquate
  ThumbnailYPosition = 1

  LOOP ThumbnailRow = 1 TO PagesDown
    ThumbnailXPosition = 1
    LOOP ThumbnailColumn = 1 TO PagesAcross
      SETPOSITION (CurrentBorderBox, ThumbnailXPosition, ThumbnailYPosition, ThumbnailWidth, ThumbnailHeight)
      SETPOSITION (CurrentImageBox,  ThumbnailXPosition, ThumbnailYPosition, ThumbnailWidth, ThumbnailHeight)

      CurrentBorderBox {PROP:Color}   = COLOR:Black
      CurrentBorderBox {PROP:Fill}    = COLOR:White
      CurrentImageBox {PROP:Cursor}   = CURSOR:Zoom
      CurrentImageBox {PROP:Alrt,250} = MouseLeft

      ThumbnailXPosition += MinimumXSeparation + ThumbnailWidth
      CurrentBorderBox   += 1
      CurrentImageBox    += 1
    END
    ThumbnailYPosition += MinimumYSeparation + ThumbnailHeight
  END
  EXIT

!------------------------------------------------------
FillThumbnails ROUTINE
!|
!| This routine is used to fill all of the thumbnails.
!|
  CurrentBorderBox = LowestBorderEquate
  CurrentImageBox  = LowestImageEquate

  LOOP CurrentThumbnail = 1 TO ThumbnailsPresent
    ProcessedPage = (CurrentPage + CurrentThumbnail) - 1
    IF ProcessedPage > ImageCount
      IF NOT CurrentImageBox {PROP:Hide}
        HIDE (CurrentBorderBox)
        HIDE (CurrentImageBox)
      END
    ELSE
      GET (PrintPreviewQueue, ProcessedPage)
      CurrentImageBox {PROP:Text} = PrintPreviewQueue

      IF CurrentImageBox {PROP:Hide}
        UNHIDE (CurrentBorderBox)
        UNHIDE (CurrentImageBox)
      END
    END
    CurrentBorderBox += 1
    CurrentImageBox += 1
  END
  IF ImageCount > 1
    IF CurrentPage = 1
      DISABLE (?Previous)
    ELSE
      ENABLE (?Previous)
    END
    IF CurrentPage + ThumbnailsPresent > ImageCount
      DISABLE (?Next)
    ELSE
      ENABLE (?Next)
    END
  END
  IF ThumbnailsPresent > 1
    ProcessedPage = CurrentPage + ThumbnailsPresent - 1
    IF ProcessedPage > ImageCount
      ProcessedPage = ImageCount
    END
    IF CurrentPage = ImageCount
      TARGET {PROP:StatusText,2} = 'Page ' & CurrentPage & ' of ' & ImageCount
    ELSE
      TARGET {PROP:StatusText,2} = 'Pages ' & CurrentPage & '-' & ProcessedPage & ' of ' & ImageCount
    END
  ELSE
    TARGET {PROP:StatusText,2} = 'Page ' & CurrentPage & ' of ' & ImageCount
  END
  EXIT

!------------------------------------------------------
ConfigureZoomRatio        ROUTINE
!|
!| This routine is used to set the Zoom modifier, and to set the status bar
!| text which displays the current zoom mode.
!|
  DATA
ZoomRatio    STRING(20),AUTO
  CODE
  IF ZoomPageWidth
    ZoomModifier = (TARGET {PROP:ClientWidth} - 4) / ImageWidth
    ZoomRatio    = 'Zoom (Page Width)'
  ELSIF ZoomPageHeight
    ZoomModifier = (TARGET {PROP:ClientHeight} - 4) / ImageHeight
    ZoomRatio    = 'Zoom (Page Height)'
  ELSIF Zoom50
    ZoomModifier = 0.5
    ZoomRatio    = 'Zoom (50%)'
  ELSIF Zoom75
    ZoomModifier = 0.75
    ZoomRatio    = 'Zoom (75%)'
  ELSIF Zoom100
    ZoomModifier = 1.0
    ZoomRatio    = 'Zoom (100%)'
  ELSIF Zoom200
    ZoomModifier = 2.0
    ZoomRatio    = 'Zoom (200%)'
  ELSIF Zoom300
    ZoomModifier = 3.0
    ZoomRatio    = 'Zoom (300%)'
  ELSE
    EXIT
  END

  TARGET {PROP:StatusText,3} = ZoomRatio
  EXIT

!---------------------------------------------------------
ClearZoomValues ROUTINE
!|
!| Since the zoom status is kept in eight different flags, this routine
!| is used to clear all of the flags during any zoom mode change.
!|
  ZoomNoZoom     = FALSE
  ZoomPageWidth  = FALSE
  ZoomPageHeight = FALSE
  Zoom50         = FALSE
  Zoom75         = FALSE
  Zoom100        = FALSE
  Zoom200        = FALSE
  Zoom300        = FALSE
  EXIT

!------------------------------------------------------
ZoomSelected ROUTINE
  DO ClearZoomValues
  EXECUTE (ZoomSelection)
    ZoomNoZoom     = TRUE
    ZoomPageWidth  = TRUE
    ZoomPageHeight = TRUE
    Zoom50         = TRUE
    Zoom75         = TRUE
    Zoom100        = TRUE
    Zoom200        = TRUE
    Zoom300        = TRUE
  END
  DO ChangeDisplay
  EXIT

!=========================================================
Preview:JumpToPage FUNCTION(LONG Input:CurrentPage, LONG Input:TotalPages)
JumpPage    LONG
ReturnPage  LONG
JumpWindow WINDOW('Jump to a Report Page'),AT(,,181,26),FONT('MS Shell Dlg',10),CENTER,STATUS,GRAY
       PROMPT('&Page:'),AT(5,8),USE(?JumpPrompt)
       SPIN(@n5),AT(30,7,50,12),MSG('Select a page of the report'),USE(JumpPage),STEP(1)
       BUTTON('OK'),AT(89,7,40,12),MSG('Jump to the selected page'),USE(?OKButton),DEFAULT
       BUTTON('Cancel'),AT(134,7,40,12),MSG('Cancel this selection'),USE(?CancelButton)
     END
  CODE
  JumpPage   = Input:CurrentPage
  ReturnPage = Input:CurrentPage
  OPEN(JumpWindow)
  ?JumpPage{PROP:RangeLow} = 1
  ?JumpPage{PROP:RangeHigh} = Input:TotalPages
  ACCEPT
    CASE ACCEPTED()
    OF ?OKButton
      ReturnPage = JumpPage
      BREAK
    OF ?CancelButton
      BREAK
    END
  END
  CLOSE(JumpWindow)
  RETURN(ReturnPage)

!=========================================================
Preview:SelectDisplay PROCEDURE(*LONG Input:PagesAcross, *LONG Input:PagesDown)
SelectWindow WINDOW('Change the Report Display'),AT(,,141,64),FONT('MS Shell Dlg',10),GRAY
       GROUP('Pages Displayed'),AT(3,2,135,43),USE(?Group1),BOXED
         GROUP('Across'),AT(7,10,62,32),BOXED
           SPIN(@N2),AT(13,22,50,12),USE(Input:PagesAcross),HSCROLL,RANGE(1,10),STEP(1)
         END
         GROUP('Down'),AT(72,10,62,32),BOXED
           SPIN(@N2),AT(79,22,50,12),USE(Input:PagesDown),HVSCROLL,RANGE(1,10),STEP(1)
         END
       END
       BUTTON('OK'),AT(98,47,40,14),KEY(EnterKey),USE(?OK),STD(STD:Close)
     END
  CODE
  OPEN(SelectWindow)
  ACCEPT
  END
  RETURN

INIRestoreWindow   PROCEDURE(STRING ProcedureName,STRING INIFileName)

IsMax     BYTE,AUTO

Info             GROUP
X                   SIGNED
Y                   SIGNED
W                   SIGNED
H                   SIGNED
Maximized           BYTE
Minimized           BYTE
Got                 BYTE
                  END
INI::String       CSTRING(20),AUTO

  CODE
     Info.Got = True
     INI::String = GETINI(ProcedureName,'Maximize',,INIFileName)
     CASE INI::String
     OF 'No'
       Info.Maximized = FALSE
     OF 'Yes'
       Info.Maximized = TRUE
     ELSE
       Info.Got = FALSE
     END
     IF Info.Got
        Info.Minimized = FALSE
        INI::String = GETINI(ProcedureName, 'Minimize', Info.Minimized,INIFileName)

        Info.X = _nopos
        Info.Y = _nopos
        Info.W = _nopos
        Info.H = _nopos

        Info.X = GETINI(ProcedureName, 'XPos', Info.X,INIFileName)
        Info.Y = GETINI(ProcedureName, 'YPos', Info.Y,INIFileName)
        Info.W = GETINI(ProcedureName, 'Width', Info.W,INIFileName)
        Info.H = GETINI(ProcedureName, 'Height', Info.H,INIFileName)

        IsMax = TARGET{PROP:Maximize}

        IF NOT Info.Maximized AND IsMax
          TARGET{PROP:Maximize} = FALSE
        END

        TARGET{PROP:XPos} = Info.X
        TARGET{PROP:YPos} = Info.Y

        IF TARGET{PROP:Resize}
          IF Info.W <> _nopos
            TARGET{PROP:Width} = Info.W
          END
          IF Info.H <> _nopos
            TARGET{PROP:Height} = Info.H
          END
        END

        IF Info.Maximized AND NOT IsMax
          TARGET{PROP:Maximize} = TRUE
        END

        TARGET{PROP:Iconize} = Info.Minimized
     END
     RETURN

INISaveWindow   PROCEDURE(STRING ProcedureName,STRING INIFileName)

Info             GROUP
X                   SIGNED
Y                   SIGNED
W                   SIGNED
H                   SIGNED
Maximized           BYTE
Minimized           BYTE
Got                 BYTE
                  END

  CODE

     Info.Maximized = CHOOSE (TARGET{PROP:Maximize} <> 0)
     Info.Minimized = CHOOSE (TARGET{PROP:Iconize} <> 0)

     Info.X = _nopos
     Info.Y = _nopos
     Info.W = _nopos
     Info.H = _nopos

     IF NOT Info.Minimized AND NOT Info.Maximized
       Info.X = TARGET{PROP:XPos}
       Info.Y = TARGET{PROP:YPos}

       IF TARGET{PROP:Resize}
         Info.W = TARGET{PROP:Width}
         Info.H = TARGET{PROP:Height}
       END
     END

     PUTINI(ProcedureName, 'Maximize', CHOOSE (NOT Info.Maximized, 'No', 'Yes'),INIFileName)
     PUTINI(ProcedureName, 'Minimize', CHOOSE (NOT Info.Minimized, 'No', 'Yes'),INIFileName)

     IF Info.X = _nopos
       PUTINI(ProcedureName, 'XPos',,INIFileName)
     ELSE
       PUTINI(ProcedureName, 'XPos', Info.X,INIFileName)
     END
     IF Info.Y = _nopos
       PUTINI(ProcedureName, 'YPos',,INIFileName)
     ELSE
       PUTINI(ProcedureName, 'YPos', Info.Y,INIFileName)
     END
     IF Info.W = _nopos
       PUTINI(ProcedureName, 'Width',,INIFileName)
     ELSE
       PUTINI(ProcedureName, 'Width', Info.W,INIFileName)
     END
     IF Info.H = _nopos
       PUTINI(ProcedureName, 'Height',,INIFileName)
     ELSE
       PUTINI(ProcedureName, 'Height', Info.H,INIFileName)
     END
     RETURN

StandardWarning      FUNCTION(LONG WarningID)
  CODE
  RETURN(StandardWarning(WarningID,'',''))

StandardWarning      FUNCTION(LONG WarningID,STRING WarningText1)
  CODE
  RETURN(StandardWarning(WarningID,WarningText1,''))

StandardWarning      FUNCTION(LONG WarningID,STRING WarningText1,STRING WarningText2)
ErrorText            STRING(150),AUTO
CurrentErrorCode     LONG,AUTO
ReturnValue          LONG
  CODE
  IF SaveErrorCode
    CurrentErrorCode = SaveErrorCode
    IF SaveErrorCode = 90 OR SaveErrorCode = 100
      ErrorText = CLIP(SaveFileError) & ' (' & SaveFileErrorCode & ')'
    ELSE
      ErrorText = CLIP(SaveError) & ' (' & SaveErrorCode & ')'
    END
  ELSE
    CurrentErrorCode = ERRORCODE()
    IF ERRORCODE() = 90 OR ERRORCODE() = 100
      ErrorText = CLIP(FILEERROR()) & ' (' & CLIP(FILEERRORCODE()) & ')'
    ELSE
      ErrorText = CLIP(ERROR()) & ' (' & ERRORCODE() & ')'
    END
  END
  SaveErrorCode = 0
  SaveError = ''
  SaveFileErrorCode = 0
  SaveFileError = ''
  CASE WarningID
  OF Warn:InvalidFile
    MESSAGE('Error: (' & CLIP(ErrorText) & ') accessing ' |
    & CLIP(WarningText1) & '.  Press OK to end this application.'|
    ,'Invalid File',ICON:Exclamation,Button:OK,BUTTON:OK,0)
    HALT(0,'Invalid File!')
  OF Warn:InvalidKey
    IF MESSAGE(CLIP(WarningText1) & ' key file is invalid.  Do you '|
    &'want to rebuild the key?','Invalid Key',Icon:Question,|
    Button:Yes+Button:No,Button:Yes,0)=Button:No
      HALT(0,'Invalid Key!')
    ELSE
      RETURN(Button:Yes)
    END
  OF Warn:RebuildError
    MESSAGE('Error: (' & CLIP(ErrorText) & ') repairing key for ' |
    & CLIP(WarningText1) & '.  Press OK to end this application.',|
    'Key Rebuild Error',ICON:Exclamation,Button:OK,BUTTON:OK,0)
    HALT(0,'Error Rebuilding Key!')
  OF Warn:CreateError
    MESSAGE('Error: (' & CLIP(ErrorText) & ') creating ' |
    & CLIP(WarningText1) & '.  Press OK to end this application.',|
    'File Creation Error',ICON:Exclamation,Button:OK,BUTTON:OK,0)
    HALT(0,'File Creation Error!')
  OF Warn:CreateOpenError
    MESSAGE('Error: (' & CLIP(ErrorText) & ') opening created ' |
    & 'file:' & CLIP(WarningText1) & '.  Press OK to end this application.',|
    'File Creation Error',ICON:Exclamation,Button:OK,BUTTON:OK,0)
    HALT(0,'File Creation Error!')
  OF Warn:ProcedureToDo
    RETURN(MESSAGE('The Procedure ' & CLIP(WarningText1) & ' has not '|
    &'been defined.','Procedure not defined',ICON:Exclamation,|
    Button:OK,BUTTON:OK,0))
  OF Warn:BadKeyedRec
    RETURN(MESSAGE('Unable to read keyed record.  Error: ' |
    & CLIP(ErrorText) & '.  Insert Aborted',ICON:Exclamation,|
    Button:OK,Button:OK,0))
  OF Warn:OutOfRangeHigh
    RETURN(MESSAGE('The value of ' & CLIP(WarningText1) & ' must'|
    &' be lower than ' & CLIP(WarningText2) & '.','Range Error',|
    ICON:Exclamation,Button:OK,Button:OK,0))
  OF Warn:OutOfRangeLow
    RETURN(MESSAGE('The value of ' & CLIP(WarningText1) & ' must be'|
    &' higher than ' & CLIP(WarningText2) & '.','Range Error',|
    ICON:Exclamation,Button:OK,Button:OK,0))
  OF Warn:OutOfRange
    RETURN(MESSAGE('The value of ' & CLIP(WarningText1) & ' must be '|
    &'between ' & CLIP(WarningText2)|
    & '.','Range Error',ICON:Exclamation,Button:OK,Button:OK,0))
  OF Warn:NotInFile
    RETURN(MESSAGE('The value for ' & CLIP(WarningText1) & ' must be '|
    &'found in the ' & CLIP(WarningText2) & ' file.','Field Contents '|
    &'Error',ICON:Exclamation,Button:OK,Button:OK,0))
  OF Warn:RestrictUpdate
    RETURN(MESSAGE('This record is referenced from the file '|
    & CLIP(WarningText1) & '.  Linking field(s) have been restricted'|
    & ' from change and have been reset to original values.',|
    'Referential Integrity Update Error',ICON:Exclamation,|
    Button:OK,Button:OK,0))
  OF Warn:RestrictDelete
    RETURN(MESSAGE('This record is referenced from the file '|
    & CLIP(WarningText1) & '.  This record cannot be deleted while'|
    & ' these references exist.','Referential Integrity Delete Error'|
    ,ICON:Exclamation,Button:OK,Button:OK,0))
  OF Warn:InsertError
    RETURN(MESSAGE('An error was experienced during the update of'|
    & ' record.  Error: ' & CLIP(ErrorText) & '.'|
    ,'Record Insert Error'|
    ,ICON:Exclamation,Button:OK,Button:OK,0))
  OF Warn:RIUpdateError
    IF CurrentErrorCode AND CurrentErrorCode <> RecordChangedErr
      RETURN(MESSAGE('An error (' & CLIP(ErrorText) & ') was experienced'|
      &' when attempting to update a record from the file.  Probable Cause: ' |
      & CLIP(WarningText1) & '.','Update Operation Error',Icon:Exclamation,|
      Button:OK,Button:OK,0))
    ELSE
      RETURN(MESSAGE('This record was changed by another station.'|
      ,'Update Operation Error',Icon:Exclamation,|
      Button:OK,Button:OK,0))
    END
  OF Warn:RIFormUpdateError
    RETURN(MESSAGE('This record was changed by another station. '|
    &'Those changes will now be displayed. Use the Ditto Button or Ctrl+'''|
    &' to recall your changes.'|
    ,'Record Was Not Updated',Icon:Exclamation,|
    Button:OK,Button:OK,0))
  OF Warn:UpdateError
    RETURN(MESSAGE('An error was experienced changing this record.  '|
    &'Do you want to try to save again?','Record Update Error',|
    Icon:Exclamation,Button:Yes+Button:No+Button:Cancel,Button:Cancel,0))
  OF Warn:RIDeleteError
    RETURN(MESSAGE('An error (' & CLIP(ErrorText) & ') was experienced'|
    &' when attempting to delete a record from the file ' |
    & CLIP(WarningText1) & '.','Delete Operation Error',Icon:Exclamation,|
    Button:OK,Button:OK,0))
  OF Warn:DeleteError
    RETURN(MESSAGE('An error was experienced deleting this record.  '|
    &'Do you want to try to save again?','Record Update Error',|
    Icon:Exclamation,Button:Yes+Button:No+Button:Cancel,Button:Cancel,0))
  OF Warn:InsertDisabled
    RETURN(MESSAGE('This procedure was called to insert a record, '|
    & 'however inserts are not allowed for this procedure.  Press OK '|
    & 'to return to the calling procedure','Invalid Request',|
    Icon:Exclamation,Button:OK,Button:OK,0))
  OF Warn:UpdateDisabled
    RETURN(MESSAGE('This procedure was called to change a record, '|
    & 'however changes are not allowed for this procedure.  Press OK '|
    & 'to return to the calling procedure','Invalid Request',|
    ICON:Exclamation,Button:OK,Button:OK,0))
  OF Warn:DeleteDisabled
    RETURN(MESSAGE('This procedure was called to delete a record, '|
    & 'however deletions are not allowed for this procedure.  Press OK '|
    & 'to return to the calling procedure','Invalid Request',|
    ICON:Exclamation,Button:OK,Button:OK,0))
  OF Warn:NoCreate
    MESSAGE('The File ' & CLIP(WarningText1) & 'was not found, '|
    &'and creation of the file is not allowed.  Press OK to end '|
    &'this application.','File Creation Not Allowed',ICON:Exclamation,|
    Button:OK,BUTTON:OK,0)
    HALT(0,'File Creation Error!')
  OF Warn:ConfirmCancel
    RETURN(MESSAGE('Are you sure you want to cancel?'|
    ,'Update Cancelled',ICON:Question,Button:Yes+Button:No,|
    Button:No,0))
  OF Warn:DuplicateKey
    RETURN(MESSAGE('Adding this record creates a duplicate entry '|
    &'for the key:' & CLIP(WarningText1),'Duplicate Key Error',|
    ICON:Exclamation,Button:OK,Button:OK,0))
  OF Warn:AutoIncError
    RETURN(MESSAGE('Attempts to automatically number this record have '|
    &'failed.  Error: ' & CLIP(ErrorText) & '.',|
    'Auto Increment Error',Icon:Exclamation,Button:Cancel+Button:Retry,|
    Button:Cancel,0))
  OF Warn:FileLoadError
    RETURN(MESSAGE(CLIP(WarningText1) & ' File Load Error.  '|
    &'Error: ' & CLIP(ErrorText) & '.','File Load Error',ICON:Exclamation,|
    Button:OK,Button:OK,0))
  OF Warn:ConfirmCancelLoad
    RETURN(MESSAGE('Are you certain you want to stop loading ' |
    & CLIP(WarningText1) & '?','Cancel Request',|
    ICON:Question,Button:OK+Button:Cancel,Button:Cancel,0))
  OF Warn:FileZeroLength
    RETURN(MESSAGE(CLIP(WarningText1) & ' File Load Error.  '|
    &'The file you''ve requested contains no text.','File Load Error',|
    ICON:Exclamation,Button:OK,Button:OK,0))
  OF Warn:EndOfASCIIQueue
    IF WarningText1 = 'Down'
      RETURN(MESSAGE('The end of the viewed file was encountered.  '|
      & 'Do you want to start again from the beginning?',|
      'End of File Error',ICON:Question,Button:Yes+Button:No,Button:Yes,0))
    ELSE
      RETURN(MESSAGE('The beginning of the viewed file was encountered.  '|
      & 'Do you want to start again from the end of the file?',|
      'Beginning of File Error',ICON:Question,Button:Yes+Button:No,|
      Button:Yes,0))
    END
  OF Warn:DiskError
    RETURN(MESSAGE('File (' & CLIP(WarningText1) & ') could not be '|
    & 'opened.  Error: ' & CLIP(ErrorText) & '.','File Access Error'|
    ,Icon:Exclamation,Button:OK,Button:OK,0))
  OF Warn:ProcessActionError
    IF WarningText1 = 'Put'
      RETURN(MESSAGE('An error was experienced when making changes'|
      & ' to the ' & CLIP(WarningText2) & ' file.  Error: '|
      & CLIP(ErrorText),'Process PUT Error',Icon:Exclamation|
      ,Button:OK,Button:OK,0))
    ELSE
      RETURN(MESSAGE('An error was experienced when deleting a record'|
      & ' from the ' & CLIP(WarningText2) & ' file.  Error: '|
      & CLIP(ErrorText),'Process DELETE Error',Icon:Exclamation|
      ,Button:OK,Button:OK,0))
    END
  OF Warn:StandardDelete
    RETURN(MESSAGE('You''ve selected to delete the highlighted record.  '|
    &'Press OK to confirm deletion of this record.',|
    'Confirm Delete',Icon:Question,Button:OK+Button:Cancel,Button:Cancel,0))
  OF Warn:SaveOnCancel
    RETURN(MESSAGE('Do you want to save the changes to this record?'|
    ,'Update Cancelled',ICON:Question,Button:Yes+Button:No+Button:Cancel,|
    Button:No,0))
  OF Warn:LogoutError
    MESSAGE('Error: (' & CLIP(ErrorText) & ') attempting to frame the ' |
    & LOWER(CLIP(WarningText1)) & ' transaction on ' & CLIP(WarningText2)|
    & '.  ' & CLIP(WarningText1) & ' transaction cancelled.'            |
    ,'Transaction Framing Error',ICON:Exclamation,Button:OK,BUTTON:OK,0)
  OF Warn:RecordFetchError
    MESSAGE('Error: (' & CLIP(ErrorText) & ') attempting to access a ' |
    & 'record from the ' & CLIP(WarningText1) & ' file.  Returning to ' |
    & 'previous window.' |
    ,'Record Retrieval Error',ICON:Exclamation,Button:OK,Button:OK,0)
  OF Warn:ViewOpenError
    MESSAGE('Error: (' & CLIP(ErrorText) & ') attempting to open the ' |
    & 'current VIEW. Filter and Range Limits ignored.' |
    ,'View Open Error',ICON:Exclamation,Button:OK,BUTTON:OK,0)
  OF Warn:NewRecordAdded
    RETURN(MESSAGE('This record has been added to the file. Do you want to add another record?'|
    ,'Record Added',ICON:Question,Button:Yes+Button:No,BUTTON:Yes,0))
  END

RISaveError PROCEDURE
  CODE
  SaveErrorCode = ERRORCODE()
  SaveError = ERROR()
  SaveFileErrorCode = FILEERRORCODE()
  SaveFileError = FILEERROR()
  ROLLBACK

