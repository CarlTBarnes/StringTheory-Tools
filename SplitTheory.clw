! SplitTheory - Try various split parameters without writing code
!
!Defines: StringTheoryLinkMode=>1;StringTheoryDllMode=>0;_ABCLinkMode_=>1;_ABCDllMode_=>0

  PROGRAM  
    INCLUDE 'TplEqu.CLW'
    INCLUDE 'KeyCodes.CLW'
    INCLUDE('StringTheory.inc'),ONCE
    INCLUDE('BigBangTheory.INC'),ONCE
    MAP
SplitTheory PROCEDURE()
ParmBool    PROCEDURE(BOOL pBool, BOOL BlankZero=1),STRING
ParmNum     PROCEDURE(LONG pNum, BOOL BlankZero=1),STRING
ParmStr     PROCEDURE(STRING pStr, BOOL BlankNull=1),STRING

DB          PROCEDURE(STRING DebugMessage)   !Output Debug String
DBClear     PROCEDURE()                      !Clear DebugView Buffer
      MODULE('api')
        OutputDebugString(*CSTRING cMsg),PASCAL,DLL(1),RAW,NAME('OutputDebugStringA')
        GetLastError(),LONG,PASCAL,DLL(1) 
      END
    END
G_Bang BigBangTheory        !Global Classes
G_ST   StringTheory
G_Lne  StringTheory    
    CODE
    SplitTheory()
    RETURN
!===========================================================================
SplitTheory   PROCEDURE
X      LONG
Txt    STRING(40000)
Bang    BigBangTheory 
FileST  StringTheory 
TmpST   StringTheory 
LoadFN  STRING(260)
LineSpl_How    BYTE(1)
LineSpl_Delim  STRING('<<13,10> {30}')
LineSpl_Quote  STRING('"')
LineSpl_Every  USHORT(80) 
LineSpl_Match  STRING(255)
LineSpl_NoCase BYTE(1)

ColsGrp GROUP,PRE()
Col_Split    STRING(', {32}')
Col_Quote1   STRING('" {32}')
Col_Quote2   STRING(32)
Col_QuoteRmv BYTE
Col_Clip     BYTE
Col_Left     BYTE
Col_Sep      STRING(32)
Col_Nested   BYTE
         END
SplitCode2  STRING(255)
Q1          EQUATE('''')
         
Window WINDOW('Split StringTheory '),AT(,,342,227),CENTER,GRAY,IMM,SYSTEM,FONT('Segoe UI',9),RESIZE
        BUTTON('Load Text'),AT(7,4,42),USE(?LoadTextBtn),TIP('SetValue() from TEXT control at bottom')
        BUTTON('Clipoard'),AT(55,4,36),USE(?LoadClipBtn),TIP('SetValue( ClipBoard() )')
        BUTTON('File...'),AT(97,4),USE(?LoadFileBtn)
        BUTTON('ReR&un'),AT(301,21),USE(?ReRunBtn),SKIP,TIP('Run another')
        ENTRY(@s255),AT(136,6,,11),FULL,USE(LoadFN),SKIP,TRN,READONLY
        BUTTON('ValueView(ST)'),AT(7,21),USE(?ValueViewFileST),TIP('View FileST.GetValue')
        STRING('ENTRY strings are Claion Literals allowing << 9 > but double << '' {{'),AT(85,23),USE(?StrLength)
        PANEL,AT(1,39,,2),FULL,USE(?Horz1),BEVEL(0,0,0600H)
        BUTTON('Split Lines'),AT(7,46),USE(?SpiltLinesBtn)
        OPTION('Split Lines Method'),AT(72,42,220,55),USE(LineSpl_How),BOXED
            RADIO('Line End'),AT(78,54),USE(?LineSpl_How:Radio1)
            RADIO('Every'),AT(78,68),USE(?LineSpl_How:Radio2),TIP('SplitEvery for fixed length records')
            RADIO('By Match'),AT(78,82),USE(?LineSpl_How:Radio3)
        END
        ENTRY(@s32),AT(123,54,117,11),USE(LineSpl_Delim),TIP('Line End Delimeter as Clarion String Literal<13,10>Use << ' & |
                '> for low ASCII.')
        PROMPT('Quote:'),AT(245,54),USE(?LineQuote:Pmt)
        ENTRY(@s1),AT(270,54,11,11),USE(LineSpl_Quote),TIP('Line-End inside Quotes are ignored<13,10>common for CSV')
        ENTRY(@n5),AT(123,68,,11),USE(LineSpl_Every),TIP('Line Length for .SplitEvery()')
        STRING('bytes'),AT(163,68),USE(?Bytes)
        ENTRY(@s255),AT(123,82,117,11),USE(LineSpl_Match),TIP('RegEx Match Delimeter as Clarion String Literal')
        CHECK('No Case'),AT(245,82),USE(LineSpl_NoCase),TIP('Case Insensitive Match')
        STRING('Split Lines: 0'),AT(9,68),USE(?SplitLinesCnt)
        BUTTON('View Lines'),AT(7,82),USE(?ViewLinesBtn),TIP('BigBang.LinesViewInList()')
        PANEL,AT(1,103,,2),FULL,USE(?Horz2),BEVEL(0,0,0600H)
        BUTTON('Tab'),AT(103,135,25,11),USE(?ColAsTabBtn),SKIP
        BUTTON('CSV'),AT(103,121,25,11),USE(?ColAsCsvBtn),SKIP
        PROMPT('Column Split:'),AT(7,109),USE(?ColDel:Pmt)
        ENTRY(@s32),AT(53,109,75,11),USE(Col_Split),TIP('Column Delimeter as Clarion String Literal<13,10>Use << > for l' & |
                'ow ASCII.')
        PROMPT('Quote Begin:'),AT(141,109),USE(?ColQt1:Pmt)
        ENTRY(@s32),AT(185,109,75,11),USE(Col_Quote1)
        PROMPT('Quote End:'),AT(147,122),USE(?ColQt2:Pmt)
        ENTRY(@s32),AT(185,122,75,11),USE(Col_Quote2)
        PROMPT('Separator:'),AT(150,135),USE(?ColSep:Pmt)
        ENTRY(@s32),AT(185,135,40,11),USE(Col_Sep),TIP('Quote and End pairs are separated by this character<13,10>Usuall' & |
                'y a single charatcer like a dash')
        CHECK('Remove Quotes'),AT(267,108),USE(Col_QuoteRmv)
        CHECK('Nested Quotes'),AT(267,122),USE(Col_Nested),TIP('Quote and End Quote are Nested')
        CHECK('Clip'),AT(267,135),USE(Col_Clip),TIP('Remove trailing spaces')
        CHECK('Left'),AT(297,135),USE(Col_Left),TIP('Remove leading spaces')
        BUTTON('View Column Split'),AT(7,128),USE(?ViewColumnsBtn)
        ENTRY(@s255),AT(7,151,,11),FULL,USE(SplitCode2),SKIP,TRN,FONT('Consolas'),READONLY,ALRT(MouseLeft2)
        PANEL,AT(1,165,,2),FULL,USE(?Horz3),BEVEL(0,0,0600H)
        PROMPT('&Text - Paste or Type text here and then press the "Load Text" button'),AT(3,168),USE(?Txt:Prompt)
        BUTTON('Tests...'),AT(300,167,,11),USE(?TextTestBtn)
        TEXT,AT(2,180),FULL,USE(txt),HVSCROLL
    END
    CODE
    LoadFN='<<---- Click Load Text, Clipboard or File, then Split Lines, then ...' 
    Txt='Paste or type text here' 
    
    OPEN(WINDOW)
    0{PROP:text}=clip(0{PROP:text}) &' - Library ' & system{PROP:LibVersion,2} &'.'& system{PROP:LibVersion,3}
    !      LoadFN='EmpPos2019.csv' ; FileST.LoadFile(LoadFN) ; Do StrLenRtn
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow 
        OF EVENT:Timer
        END
        CASE ACCEPTED()
        OF ?ReRunBtn          ; RUN(COMMAND('0'))
        OF ?LoadTextBtn       ; FileST.SetValue(CLIP(Txt))   ; DO StrLenRtn
        OF ?LoadClipBtn       ; FileST.SetValue(Clipboard()) ; DO StrLenRtn
        OF ?LoadFileBtn       ; DO LoadFileRtn               ; DO StrLenRtn
        OF ?ValueViewFileST   ; Bang.ValueView(FileST)
        
        OF ?SpiltLinesBtn     ; DO SplitLinesRtn
        OF ?ViewLinesBtn      ; Bang.LinesViewInList(FileST)
        
        OF ?ColAsTabBtn  ; DO TabColsSetupBtn
        OF ?ColAsCsvBtn  ; DO CsvColsSetupBtn
        
        OF ?ViewColumnsBtn    ; DO ViewColumnsRtn        
        OF ?TextTestBtn       ; DO TextTestRtn
        END
        CASE FIELD()
        OF ?SplitCode2
            IF EVENT()=EVENT:AlertKey aND KEYCODE()=MouseLeft2 THEN
               Message(?SplitCode2{PROP:Tip} & |
                       '<13,10,13,10>' & CLIP(SplitCode2), |
                       'Split Code',,,,MSGMODE:CANCOPY+MSGMODE:FIXEDFONT)

            END
        END
    END
    CLOSE(WINDOW)
!========================================================================
StrLenRtn ROUTINE
    ?StrLength{PROP:Text}='String Length: ' & FileST.Length() 
    
CsvColsSetupBtn ROUTINE
    Col_Split=',' ; Col_Quote1='"' ; Col_Quote2='"'
    Col_Sep  =''  ; Col_Nested   =0
    DISPLAY 
TabColsSetupBtn ROUTINE
    Col_Split='<<9>' ; Col_Quote1='' ; Col_Quote2=''
    Col_Sep  =''  ; Col_Nested   =0
    DISPLAY 
    
LoadFileRtn ROUTINE  !---------------------------------------------------
    IF ~FILEDIALOG('Select LoadFile for StringTheory', LoadFN, |
                   'All files|*.*|CSV|*.CSV|Text|*.TXT', FILE:KeepDir+FILE:LongName) THEN 
        EXIT
    END
    IF ~FileST.LoadFile(LoadFN) THEN
        Message('LoadFile Windows Error ' & FileST.winErrorCode,'LoadFile') 
        EXIT
    END
    
SplitLinesRtn ROUTINE  !------------------------------------------------------  
    IF ~FileST.GetValue() THEN
        Message('Blank string, load Text, Clipboard or File') ; EXIT
    END
    CASE LineSpl_How
    OF 1 ; IF ~LineSpl_Delim THEN SELECT(?LineSpl_Delim) ; EXIT.
           FileST.Split(CLIP(UNQUOTE(LineSpl_Delim)),LineSpl_Quote) 

    OF 2 ; IF ~LineSpl_Every THEN SELECT(?LineSpl_Every) ; EXIT.
           FileST.SplitEvery(LineSpl_Every)

    OF 3 ; IF ~LineSpl_Match THEN SELECT(?LineSpl_Match) ; EXIT.
           FileST.SplitByMatch(CLIP(UNQUOTE(LineSpl_Match)), LineSpl_NoCase)
    ELSE ; SELECT(?LineSpl_How) ; EXIT    
    END
    ?SplitLinesCnt{PROP:Text}='Split Lines: ' & FileST.Records()
    EXIT
  
ViewColumnsRtn ROUTINE  !------------------------------------------------------ 
    IF ~Col_Split THEN SELECT(?Col_Split) ; EXIT. 
    IF ~FileST.Records() THEN 
        SELECT(?SpiltLinesBtn)
        Message('You must Split Lines')
        EXIT
    END
    
    SplitCode2='ST.Split( '      & |
         Q1  & CLIP(Col_Split)   & Q1 & |      !   string pSplitStr,         Usuallay <13,10>
         ', '& ParmStr(Col_Quote1)    & |      !   <string pQuotestart>,     "
         ', '& ParmStr(Col_Quote2)    & |      !   <string pQuoteEnd>, 
         ', '& ParmBool(Col_QuoteRmv) & |      !   bool removeQuotes=false,  
         ', '& ParmBool(Col_Clip)     & |      !   bool pClip = false,       Trim trailing spaces
         ', '& ParmBool(Col_Left)     & |      !   bool pLeft=false,         Trim leading spaces
         ', '& ParmStr(Col_Sep)       & |      !   <string pSeparator>,      Sep if QuoteStart has multiple e.g. - for  [-(  )-]
         ', '& ParmBool(Col_Nested)   & ')' & |!   Long pNested=false)       True=Ignore () around "5,6" in (1,2),(3,4),((5,6),7)
         '  <13,10>! Double click to see in Message()'
    DISPLAY

    ?SplitCode2{PROP:Tip}='ST.Split( '      & |
         '<13,10>     '& Q1 & CLIP(Col_Split)   & Q1 & ' <9> ! String pSplitStr,' & |
         '<13,10>   , '& ParmStr(Col_Quote1)    &      ' <9> ! <<String pQuotestart>,' & |
         '<13,10>   , '& ParmStr(Col_Quote2)    &      ' <9> ! <<string pQuoteEnd>,' & |
         '<13,10>   , '& ParmBool(Col_QuoteRmv) &      ' <9> ! Bool removeQuotes=false,' & |
         '<13,10>   , '& ParmBool(Col_Clip)     &      ' <9> ! Bool pClip = false,' & |
         '<13,10>   , '& ParmBool(Col_Left)     &      ' <9> ! Bool pLeft=false,' & |
         '<13,10>   , '& ParmStr(Col_Sep)       &      ' <9> ! <<String pSeparator>,' & |
         '<13,10>   , '& ParmBool(Col_Nested)   &      ' <9> ! Long pNested=false)'
               
    Bang.LinesViewSplit(FileST      , |
         CLIP(UNQUOTE(Col_Split))   , |  !   string pSplitStr,         Usuallay <13,10>
         CLIP(UNQUOTE(Col_Quote1))  , |  !   <string pQuotestart>,     "
         CLIP(UNQUOTE(Col_Quote2))  , |  !   <string pQuoteEnd>, 
         Col_QuoteRmv               , |  !   bool removeQuotes=false,  
         Col_Clip                   , |  !   bool pClip = false,       Trim trailing spaces
         Col_Left                   , |  !   bool pLeft=false,         Trim leading spaces
         CLIP(UNQUOTE(Col_Sep))     , |  !   <string pSeparator>,      Sep if QuoteStart has multiple e.g. - for  [-(  )-]
         Col_Nested                   )  !   Long pNested=false)       True=Ignore () around "5,6" in (1,2),(3,4),((5,6),7)

!===========================================================================
TextTestRtn ROUTINE  
    EXECUTE POPUP('Separator [-( )-]|Directory CSV')
        DO Test_Separator1_Rtn
        DO Test_DirectoryCSV_Rtn
    END
    
!--------------------------    
Test_Separator1_Rtn ROUTINE
    DATA
ST   StringTheory    
    CODE
    st.Start()
    st.AddLine(1,'[12,24,36],Person,(PO Box 111, Plum)') 
    st.AddLine(2,'[22,34,46],   Man,(PO Box 222, Scarlet)') 
    st.AddLine(3,'[32,44,56], Woman,(PO Box 333, Mustard)')
    st.AddLine(4,'[42,55,66],Camera,(PO Box 444, Peacock)')
    st.AddLine(5,'[52,65,76],    TV,(PO Box 555, Green)')    
    st.Join('<13,10>')
    Txt=St.GetValue() 

    LineSpl_How   =1
    LineSpl_Delim ='<<13,10>'
!    LineSpl_Quote ='"'

    Col_Split    =','
    Col_Quote1   ='[-('
    Col_Quote2   =']-)'
!    Col_QuoteRmv BYTE
!    Col_Clip     BYTE
!    Col_Left     BYTE
    Col_Sep      ='-'
    Col_Nested   =0
    DISPLAY()
    POST(EVENT:Accepted,?LoadTextBtn)
    POST(EVENT:Accepted,?SpiltLinesBtn)

!--------------------------
Test_DirectoryCSV_Rtn ROUTINE
    DATA
ST      StringTheory
FilesQ  FILE:Queue    
    CODE
    DIRECTORY(FilesQ,'c:\Windows\*.*',ff_:NORMAL+ff_:DIRECTORY)
    st.SerializeQueue(FilesQ,'<13,10>',',','"') 
    st.Prepend('Name,Short,Date,Time,Size,Attrib<13,10>')
    Txt=St.GetValue()
    LineSpl_How   =1
    LineSpl_Delim ='<<13,10>'
    DO CsvColsSetupBtn
    DISPLAY()
    POST(EVENT:Accepted,?LoadTextBtn)
    POST(EVENT:Accepted,?SpiltLinesBtn)   
       
Extra_1_Rtn ROUTINE  !------------------------------------------------------ 
    DATA
ST   StringTheory
Lne  StringTheory    
    CODE
    
Extra_2_Rtn ROUTINE  !------------------------------------------------------ 
    DATA
ST   StringTheory
Lne  StringTheory 
FilesQ    FILE:Queue   
    CODE       

!========================================================================================
ParmBool PROCEDURE(BOOL pBool, BOOL BlankZero=1) !,STRING
    CODE
    IF ~pBool AND BlankZero THEN RETURN ''. 
    IF ~pBool THEN return 'false'.
    RETURN 'true'
ParmNum PROCEDURE(LONG pNum, BOOL BlankZero=1) !,STRING
    CODE
    IF ~pNum AND BlankZero THEN RETURN ''.
    RETURN pNum    
ParmStr PROCEDURE(STRING pStr, BOOL BlankNull=1) !,STRING
    CODE
    IF ~pStr AND BlankNull THEN RETURN ''.
    RETURN '''' & CLIP(pStr) & ''''
    
!========================================================================================
DB   PROCEDURE(STRING xMessage)
Prfx EQUATE('ScratchST: ')
sz   CSTRING(SIZE(Prfx)+SIZE(xMessage)+1),AUTO
  CODE 
  sz  = Prfx & CLIP(xMessage)
  OutputDebugString( sz )
!------------------
DBClear PROCEDURE()
DbgClear CSTRING('DBGVIEWCLEAR')    !Message to Clear the buffer. Must UPPER and first i.e. without a Prefix
    CODE 
    OutputDebugString(DbgClear)     !Call API directly, cannot have Prefix, must be first