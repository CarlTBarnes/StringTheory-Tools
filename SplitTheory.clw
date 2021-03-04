! SplitTheory - Try various split parameters without writing code
!
!Defines: StringTheoryLinkMode=>1;StringTheoryDllMode=>0;_ABCLinkMode_=>1;_ABCDllMode_=>0
!
!----------------------------------------------------------------------------------------
! 03-Mar-2021   Split Lines add Quote End and Quote Remove. Cosmetic/text improvements
!               Split Lines at bottom add CODE for Split() SplitEvery() SplitByMatch()
! 04-Mar-2021   Load Text button warns if Default text
!               Do2Class change ROUTINE to CLASS named DOO ... I gave you that name and I said goodbye, I knew you'd either git tuff or die

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
LineSpl_Quote1 STRING('" {32}')
LineSpl_Quote2 STRING(32)
LineSpl_QuoteRmv BYTE
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
SplitCode1  STRING(255)
SplitCode2  STRING(255)
Q1          EQUATE('''')

Window WINDOW('Split StringTheory '),AT(,,342,242),CENTER,GRAY,IMM,SYSTEM,FONT('Segoe UI',9),RESIZE
        BUTTON('Load Text'),AT(7,4,42),USE(?LoadTextBtn),TIP('SetValue() from TEXT control at bottom')
        BUTTON('Clipoard'),AT(55,4,36),USE(?LoadClipBtn),TIP('SetValue( ClipBoard() )')
        BUTTON('File...'),AT(97,4),USE(?LoadFileBtn)
        BUTTON('ReR&un'),AT(301,21),USE(?ReRunBtn),SKIP,TIP('Run another')
        ENTRY(@s255),AT(136,6,,11),FULL,USE(LoadFN),SKIP,TRN,READONLY
        BUTTON('ValueView(ST)'),AT(7,21),USE(?ValueViewFileST),TIP('View FileST.GetValue')
        STRING('ENTRY strings are Claion Literals allowing << 9 > but double << '' {{'),AT(85,23),USE(?StrLength)
        PANEL,AT(1,39,,2),FULL,USE(?Horz1),BEVEL(0,0,0600H)
        BUTTON('Split Lines'),AT(7,46),USE(?SpiltLinesBtn)
        OPTION('Split Lines Method'),AT(72,42,266,55),USE(LineSpl_How),BOXED
            RADIO('Line End'),AT(78,54),USE(?LineSpl_How:Radio1)
            RADIO('Every'),AT(78,68),USE(?LineSpl_How:Radio2),TIP('SplitEvery for fixed length records')
            RADIO('Match'),AT(78,82),USE(?LineSpl_How:Radio3),TIP('SplitByMatch( Regulare Expression, NoCase)')
        END
        ENTRY(@s32),AT(123,54,117,11),USE(LineSpl_Delim),TIP('Line End Delimeter as Clarion String Literal<13,10>Use << ' & |
                '> for low ASCII.')
        PROMPT('Quotes:'),AT(245,55),USE(?LineQuote:Pmt)
        ENTRY(@s32),AT(273,54,26,11),USE(LineSpl_Quote1),TIP('Quote Begin<13,10>Line-End inside Quotes are ignored<13>' & |
                '<10>common for CSV')
        ENTRY(@s32),AT(305,54,26,11),USE(LineSpl_Quote2),TIP('Quote End')
        CHECK('Remove Quotes'),AT(272,67),USE(LineSpl_QuoteRmv),TIP('Quotes at the beginning or end of the line will be ' & |
                'removed')
        ENTRY(@n5),AT(123,68,,11),USE(LineSpl_Every),TIP('Line Length for .SplitEvery()')
        STRING('bytes'),AT(159,68),USE(?Bytes)
        ENTRY(@s255),AT(123,82,117,11),USE(LineSpl_Match),TIP('RegEx Match Delimeter as Clarion String Literal')
        CHECK('No Case'),AT(245,82),USE(LineSpl_NoCase),TIP('Case Insensitive Match')
        STRING('Split Lines: 0'),AT(9,68),USE(?SplitLinesCnt)
        BUTTON('View Lines'),AT(7,82),USE(?ViewLinesBtn),TIP('BigBang.LinesViewInList()')
        ENTRY(@s255),AT(7,100,,11),FULL,USE(SplitCode1),SKIP,TRN,FONT('Consolas'),READONLY,ALRT(MouseLeft2)
        PANEL,AT(1,116,,2),FULL,USE(?Horz2),BEVEL(0,0,0600H)
        BUTTON('Tab'),AT(103,148,25,11),USE(?ColAsTabBtn),SKIP
        BUTTON('CSV'),AT(103,134,25,11),USE(?ColAsCsvBtn),SKIP
        PROMPT('Column Split:'),AT(7,122),USE(?ColDel:Pmt)
        ENTRY(@s32),AT(53,122,75,11),USE(Col_Split),TIP('Column Delimeter as Clarion String Literal<13,10>Use << > for l' & |
                'ow ASCII.')
        PROMPT('Quote Begin:'),AT(141,122),USE(?ColQt1:Pmt)
        ENTRY(@s32),AT(185,122,75,11),USE(Col_Quote1)
        PROMPT('Quote End:'),AT(147,135),USE(?ColQt2:Pmt)
        ENTRY(@s32),AT(185,135,75,11),USE(Col_Quote2)
        PROMPT('Separator:'),AT(150,148),USE(?ColSep:Pmt)
        ENTRY(@s32),AT(185,148,40,11),USE(Col_Sep),TIP('Quote and End pairs are separated by this character<13,10>Usuall' & |
                'y a single charatcer like a dash')
        CHECK('Remove Quotes'),AT(267,121),USE(Col_QuoteRmv),TIP('Quotes at the beginning or end of the string will be r' & |
                'emoved')
        CHECK('Nested Quotes'),AT(267,135),USE(Col_Nested),TIP('Quote and End Quote are Nested')
        CHECK('Clip'),AT(267,148),USE(Col_Clip),TIP('Remove trailing spaces')
        CHECK('Left'),AT(297,148),USE(Col_Left),TIP('Remove leading spaces')
        BUTTON('View Column Split'),AT(7,141),USE(?ViewColumnsBtn)
        ENTRY(@s255),AT(7,164,,11),FULL,USE(SplitCode2),SKIP,TRN,FONT('Consolas'),READONLY,ALRT(MouseLeft2)
        PANEL,AT(1,178,,2),FULL,USE(?Horz3),BEVEL(0,0,0600H)
        PROMPT('&Text - Paste or Type text here and then press the "Load Text" button at top left'),AT(3,181),USE(?Txt:Prompt)
        BUTTON('Tests...'),AT(300,180,,11),USE(?TextTestBtn)
        TEXT,AT(2,194),FULL,USE(txt),HVSCROLL
    END
TxtDefault EQUATE('Paste or type text here')

DOO CLASS                         !Created 03/04/21  9:21AM by Do2Class by Carl Barnes
StrLenRtn             PROCEDURE(STRING StrName) !Set ?StrLength Text = Length
CsvColsSetupBtn       PROCEDURE() !Split 2 Defaults for CSV
TabColsSetupBtn       PROCEDURE() !Split 2 Defaults for Tab Delim
LoadFileRtn           PROCEDURE() !StringTheory .LoadFile from FileDialog Pick
SplitLinesRtn         PROCEDURE() !SplitCode1 splits file into lines by 13,10 typically
ViewColumnsRtn        PROCEDURE() !SplitCode2 splits lines into columns by ',' typically
TextTestRtn           PROCEDURE() !Test Button code
Test_Separator1_Rtn   PROCEDURE()
Test_DirectoryCSV_Rtn PROCEDURE()
Extra_1_Rtn           PROCEDURE()
Extra_2_Rtn           PROCEDURE()
    END
    CODE
    LoadFN='<<---- Click Load Text, Clipboard or File, then Split Lines, then ...'
    Txt=TxtDefault

    OPEN(WINDOW)
    0{PROP:text}=clip(0{PROP:text}) &' - Library ' & system{PROP:LibVersion,2} &'.'& system{PROP:LibVersion,3}
    !      LoadFN='EmpPos2019.csv' ; FileST.LoadFile(LoadFN) ; DOO.StrLenRtn()
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow
        OF EVENT:Timer
        END
        CASE ACCEPTED()
        OF ?ReRunBtn          ; RUN(COMMAND('0'))
        OF ?LoadTextBtn       ; IF Txt=TxtDefault THEN
                                   SELECT(?Txt)
                                   MESSAGE('Please enter your test string in the text control at the bottom.','Split',ICON:Asterisk)
                                   CYCLE
                                END
                                FileST.SetValue(CLIP(Txt))   ; DOO.StrLenRtn('Text')
        OF ?LoadClipBtn       ; FileST.SetValue(Clipboard()) ; DOO.StrLenRtn('Clipboard')
        OF ?LoadFileBtn       ; DOO.LoadFileRtn()            ; DOO.StrLenRtn('File')
        OF ?ValueViewFileST   ; Bang.ValueView(FileST)

        OF ?SpiltLinesBtn     ; DOO.SplitLinesRtn()
        OF ?ViewLinesBtn      ; Bang.LinesViewInList(FileST)

        OF ?ColAsTabBtn       ; DOO.TabColsSetupBtn()
        OF ?ColAsCsvBtn       ; DOO.CsvColsSetupBtn()

        OF ?ViewColumnsBtn    ; DOO.ViewColumnsRtn()
        OF ?TextTestBtn       ; DOO.TextTestRtn()
        END
        CASE FIELD()
        OF ?SplitCode1 OROF ?SplitCode2
            IF EVENT()=EVENT:AlertKey AND KEYCODE()=MouseLeft2 THEN
               Message(?{PROP:Tip} & |
                       '<13,10,13,10>' & CONTENTS(?), |
                       'Split Code',,,,MSGMODE:CANCOPY+MSGMODE:FIXEDFONT)

            END
        END
    END
    CLOSE(WINDOW)
!========================================================================
DOO.StrLenRtn PROCEDURE(STRING StrName) !Format ?StrLength
  CODE
    ?StrLength{PROP:Text}=StrName & ' String Length: ' & LEFT(FORMAT(FileST.Length(),@n13))
    DISPLAY
DOO.CsvColsSetupBtn PROCEDURE() !Split 2 Defaults for CSV
  CODE
    Col_Split=',' ; Col_Quote1='"' ; Col_Quote2='"'
    Col_Sep  =''  ; Col_Nested   =0
    DISPLAY
DOO.TabColsSetupBtn PROCEDURE() !Split 2 Defaults for Tab Delim
  CODE
    Col_Split='<<9>' ; Col_Quote1='' ; Col_Quote2=''
    Col_Sep  =''  ; Col_Nested   =0
    DISPLAY
!---------------------------------------------------
DOO.LoadFileRtn PROCEDURE() !StringTheory .LoadFile from FileDialog Pick
  CODE
    IF LoadFN[1]='<<' THEN LoadFN=''. !if ='<--' C10 fails to open FileDialog
    IF ~FILEDIALOG('Select LoadFile for StringTheory', LoadFN, |
                   'All files|*.*|CSV|*.CSV|Text|*.TXT', FILE:KeepDir+FILE:LongName) THEN
        RETURN
    END
    IF ~FileST.LoadFile(LoadFN) THEN
        Message('LoadFile Windows Error ' & FileST.winErrorCode,'LoadFile')
        RETURN
    END
!------------------------------------------------------
DOO.SplitLinesRtn PROCEDURE() !SplitCode1 splits file into lines by 13,10 typically
  CODE
    IF ~FileST.GetValue() THEN
        Message('Blank string, load Text, Clipboard or File') ; RETURN
    END
    CASE LineSpl_How
    OF 1 ; IF ~LineSpl_Delim THEN SELECT(?LineSpl_Delim) ; RETURN.
           FileST.Split(CLIP(UNQUOTE(LineSpl_Delim)),LineSpl_Quote1,LineSpl_Quote2,LineSpl_QuoteRmv)

           SplitCode1='ST.Split('      & |
                Q1  & CLIP(LineSpl_Delim)   & Q1 & |      !   string pSplitStr,         Usuallay <13,10>
                ', '& ParmStr(LineSpl_Quote1)    & |      !   <string pQuotestart>,     "
                ', '& ParmStr(LineSpl_Quote2)    & |      !   <string pQuoteEnd>,
                ', '& ParmBool(LineSpl_QuoteRmv) & |      !   bool removeQuotes=false,
                ')   <13,10>! Double click to see in Message()'
           ?SplitCode1{PROP:Tip}='ST.Split('      & |
                '<13,10>     '& Q1 & CLIP(LineSpl_Delim)   & Q1 & ' <9> ! String pSplitStr' & |
                '<13,10>   , '& ParmStr(LineSpl_Quote1)    &      ' <9> ! <<String pQuotestart>' & |
                '<13,10>   , '& ParmStr(LineSpl_Quote2)    &      ' <9> ! <<string pQuoteEnd>' & |
                '<13,10>   , '& ParmBool(LineSpl_QuoteRmv) &      ' <9> ! Bool<160>removeQuotes=false' & |
                '<13,10>   , '& '   '&                      ' <9> ! Bool<160>pClip=false' & |
                '<13,10>   , '& '   '&                      ' <9> ! Bool<160>pLeft=false' & |
                '<13,10>   , '& '   '&                      ' <9> ! <<String pSeparator>' & |
                '<13,10>   , )'&'   '&                      ' <9> ! Long<160>pNested=false)'

    OF 2 ; IF ~LineSpl_Every THEN SELECT(?LineSpl_Every) ; RETURN.
           FileST.SplitEvery(LineSpl_Every)

           SplitCode1='ST.SplitEvery('& LineSpl_Every &')'
           ?SplitCode1{PROP:Tip}='ST.SplitEvery('& LineSpl_Every & ') <9> ! Long numChars'

    OF 3 ; IF ~LineSpl_Match THEN SELECT(?LineSpl_Match) ; RETURN.
           FileST.SplitByMatch(CLIP(UNQUOTE(LineSpl_Match)), LineSpl_NoCase)

           SplitCode1='ST.SplitByMatch('       & |
                Q1  & CLIP(LineSpl_Match)      & Q1 & |
                ', '& ParmBool(LineSpl_NoCase) & ' )'
           ?SplitCode1{PROP:Tip}='ST.SplitByMatch('      & |
                '<13,10>     '& Q1 & CLIP(LineSpl_Match) & Q1 & ' <9> ! String pRegEx' & |
                '<13,10>   , '& ParmBool(LineSpl_NoCase) & ' )'&' <9> ! Long pNoCase=0 )'

    ELSE ; SELECT(?LineSpl_How) ; RETURN
    END
    ?SplitLinesCnt{PROP:Text}='Split Lines: ' & LEFT(FORMAT(FileST.Records(),@n13))
    DISPLAY
    RETURN
!------------------------------------------------------
DOO.ViewColumnsRtn PROCEDURE() !SplitCode2 splits lines into columns by ',' typically
  CODE
    IF ~Col_Split THEN SELECT(?Col_Split) ; RETURN.
    IF ~FileST.Records() THEN
        SELECT(?SpiltLinesBtn)
        Message('You must Split Lines')
        RETURN
    END

    SplitCode2='ST.Split('      & |
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
         '<13,10>     '& Q1 & CLIP(Col_Split)   & Q1 & ' <9> ! String pSplitStr' & |
         '<13,10>   , '& ParmStr(Col_Quote1)    &      ' <9> ! <<String pQuotestart>' & |
         '<13,10>   , '& ParmStr(Col_Quote2)    &      ' <9> ! <<string pQuoteEnd>' & |
         '<13,10>   , '& ParmBool(Col_QuoteRmv) &      ' <9> ! Bool<160>removeQuotes=false' & |
         '<13,10>   , '& ParmBool(Col_Clip)     &      ' <9> ! Bool<160>pClip=false' & |
         '<13,10>   , '& ParmBool(Col_Left)     &      ' <9> ! Bool<160>pLeft=false' & |
         '<13,10>   , '& ParmStr(Col_Sep)       &      ' <9> ! <<String pSeparator>' & |
         '<13,10>   , '& ParmBool(Col_Nested)   &      ' <9> ! Long<160>pNested=false)'

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
DOO.TextTestRtn PROCEDURE() !Test Button code
  CODE
    EXECUTE POPUP('Separator [-( )-]|Directory CSV')
        DOO.Test_Separator1_Rtn()
        DOO.Test_DirectoryCSV_Rtn()
    END

!--------------------------
DOO.Test_Separator1_Rtn PROCEDURE()
!     DATA
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

!------------------------------------------------------
DOO.Test_DirectoryCSV_Rtn PROCEDURE()
!     DATA
ST      StringTheory
FilesQ  QUEUE(FILE:Queue),PRE(FilQ)
        END
    CODE
    DIRECTORY(FilesQ,'c:\Windows\*.*',ff_:NORMAL+ff_:DIRECTORY)
    st.SerializeQueue(FilesQ,'<13,10>',',','"')
    st.Prepend('Name,Short,Date,Time,Size,Attrib<13,10>')
    Txt=St.GetValue()
    LineSpl_How   =1
    LineSpl_Delim ='<<13,10>'
    DOO.CsvColsSetupBtn()
    DISPLAY()
    POST(EVENT:Accepted,?LoadTextBtn)
    POST(EVENT:Accepted,?SpiltLinesBtn)
!------------------------------------------------------
DOO.Extra_1_Rtn PROCEDURE()
!     DATA
ST   StringTheory
Lne  StringTheory
    CODE
!------------------------------------------------------
DOO.Extra_2_Rtn PROCEDURE()
!     DATA
ST   StringTheory
Lne  StringTheory
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