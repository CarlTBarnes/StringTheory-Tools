                    MEMBER()
!----------------------------------------------------------------------------
! BigBangTheory - View StringTheory Value, or Lines Queue and Optionally Split lines by Delimeter
! (c)2020-2021 by Carl T. Barnes - Released under the MIT License
!----------------------------------------------------------------------------
! 01-June-22    New property "LineViewInConsolas  BOOL" view Lines in Colsolas handy for Code files   
!               LinesViewInList Enter/Mouse2 views line (was on Popup), also Ctrl+C to Copy
! 04-June-22    New "Reflection method to view all ST Properties. Call some ST Functions Sub() Slice(). Bang functions ValueView() LinesView() etc
! 20-June-22    Consolas Font as option on ListView Popup and Column View Checkbox. Value View checkbox for Segoe Font
! 26-June-22    New .ShowAlways BOOL hides/prevents .DoNotShow - For a UI you may not want it turned off.
! 26-June-22    LinesViewInList() buttons above List instead of hiding everything in a Popup()
!----------------------------------------------------------------------------
! TODO
! LinesViewInList() add buttons at the top instead of everything hidden under Popup
!----------------------------------------------------------------------------
    INCLUDE('KEYCODES.CLW')
    INCLUDE('BigBangTheory.INC'),ONCE
_BBT_Needs_Project_Defines EQUATE(StringTheoryLinkMode + StringTheoryDllMode)
! StringTheoryLinkMode=>1;StringTheoryDllMode=>0  or StringTheoryLinkMode=>0;StringTheoryDllMode=>1
  MAP.

_CbWndPreview_  EQUATE(0)    
!    INCLUDE('CbWndPreview.inc'),ONCE   !https://github.com/CarlTBarnes/WindowPreview
!    COMPILE('!*cbw*',_CbWndPreview_)
!WndPrvCls CBWndPreviewClass            !Runtime Window/Control PROP View and Modify Sizes with secret button top-left
!             !*cbw*
!----------------------------------------------------------------------
BigBangTheory.LinesViewInList PROCEDURE(StringTheory LnzST,<STRING CapTxt>)
VlbLines CLASS
FEQ    LONG
RowCnt LONG
ClmCnt USHORT
Init   PROCEDURE(SIGNED xFEQ, LONG xRowCnt, USHORT xClmCnt)
VLBprc PROCEDURE(LONG xRow, USHORT xCol),STRING
      END
Window WINDOW('VLB'),AT(,,450,200),GRAY,SYSTEM,MAX,FONT('Segoe UI',9),RESIZE
        BUTTON('View &Line'),AT(1,2,43,12),USE(?LineView),SKIP,TIP('Display selected Line (Row) Value.<0Dh,0Ah>Double-Cl' & |
                'ick on Line')
        BUTTON('&Copy Line'),AT(48,2,43,12),USE(?CopyLine),SKIP,TIP('Copy Line to Clipboard Ctrl+C')
        BUTTON('&Value View'),AT(95,2,43,12),USE(?ValueView),SKIP,TIP('Display ST Value which was Split into Lines')
        CHECK('No Bang'),AT(319,3),USE(?NoBang),SKIP,TIP('Do Not show BigBang Views')
        CHECK('Consolas'),AT(368,3),USE(?ConsolasFnt),SKIP,TIP('Consolas Font for List')
        LIST,AT(1,17,,183),FULL,USE(?List:LinesQ),FLAT,HVSCROLL,VCR,FORMAT('24R(2)|M~Row~C(0)@n_6@999L(2)~Lines Q~'), |
                ALRT(CtrlC), ALRT(EnterKey)
    END
X LONG,AUTO
P LONG,DIM(4),STATIC
LnzRecords LONG,AUTO
    CODE
  IF SELF.DoNotShow AND ~SELF.ShowAlways THEN RETURN.
  LnzRecords = LnzST.Records()
  IF ~LnzRecords THEN 
     IF Message('No Lines in StringTheory|Split() has not been called','LinesViewInList',|
                ICON:Asterisk,'Close|View Value')=2 THEN SELF.ValueView(LnzST,CapTxt).
     RETURN
  END
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  ?List:LinesQ{PROP:LineHeight}=1+?List:LinesQ{PROP:LineHeight}
  ?List:LinesQ{7A58h}=1  !C11 PROP:PropVScroll
  0{PROP:Text}=CHOOSE(~OMITTED(CapTxt) AND CapTxt,CLIP(CapTxt),'StringTheory Lines') &' - '& LnzRecords & ' Records  -  Right-Click for Options'
  X=LOG10(LnzRecords)+1 ; IF X<4 THEN X=4.
  ?List:LinesQ{PROPLIST:Picture,1}='n_' & X
  ?List:LinesQ{PROPLIST:Width,1}  =2 + 4*X
  IF SELF.ShowAlways THEN HIDE(?NoBang) ELSE ?NoBang{PROP:Use}=SELF.DoNotShow.
  IF SELF.LineViewInConsolas THEN ?List:LinesQ{PROP:FontName}='Consolas'.
  ?ConsolasFnt{PROP:Use}=SELF.LineViewInConsolas
  VlbLines.Init(?List:LinesQ, LnzRecords, 2)
  ACCEPT
    X=CHOICE(?List:LinesQ)
    CASE ACCEPTED()
    OF ?LineView    ; SELF.StringView(LnzST.GetLine(X),'Row ' & X)
    OF ?CopyLine    ; SetClipboard(LnzST.GetLine(X))
    OF ?ValueView   ; SELF.ValueView(LnzST)
    OF ?ConsolasFnt ; ?List:LinesQ{PROP:FontName}=CHOOSE(~SELF.LineViewInConsolas,'Segoe UI','Consolas')                  
    END
    IF FIELD()=?List:LinesQ THEN
       CASE EVENT()
       OF EVENT:AlertKey
          CASE KEYCODE()
          OF EnterKey OROF CtrlC ; POST(EVENT:NewSelection,?)
          END 
       OF EVENT:NewSelection 
          CASE KEYCODE()
          OF MouseRight
             SETKEYCODE(0)
             CASE POPUP('Copy Row to Clipboard <9>Ctrl+C|View Row Text  <9>Enter / Click 2' & |
                     '|-|Copy All Rows Text to Clipboard|View All Rows (View ST Value)')
             OF 1; SetClipboard(LnzST.GetLine(X))
             OF 2; POST(EVENT:Accepted,?LineView)
             OF 3; SetClipboard(LnzST.GetValue())
             OF 4; SELF.ValueView(LnzST)
             END
          OF CtrlC         ; SetClipboard(LnzST.GetLine(X))
          OF MouseLeft2 
          OROF EnterKey    ; POST(EVENT:Accepted,?LineView)  !06/01/22 Double click or Enter views     
          END !CASE KEYCODE()
       END !CASE Event
    END !IF FIELD()=?List:LinesQ
  END
  GETPOSITION(0,P[1],P[2],P[3],P[4])
  RETURN
VlbLines.Init PROCEDURE(SIGNED xFEQ, LONG xRowCnt, USHORT xClmCnt)
  CODE
  SELF.FEQ=xFEQ
  SELF.RowCnt=xRowCnt
  SELF.ClmCnt=xClmCnt
  xFEQ{PROP:VLBval} =ADDRESS(SELF)
  xFEQ{PROP:VLBproc}=ADDRESS(SELF.VLBprc)
  RETURN
VlbLines.VLBprc PROCEDURE(LONG xRow, USHORT xCol)
  CODE
  CASE xRow
  OF -1 ; RETURN SELF.RowCnt !Rows
  OF -2 ; RETURN SELF.ClmCnt !Columns
  OF -3 ; RETURN False
  END
  IF xCol=1 THEN RETURN xRow.
  RETURN LnzST.GetLine(xRow)
!=================================================================================
BigBangTheory.LinesViewSplitCSV   PROCEDURE(StringTheory STLined, BYTE pRemoveQuotes)
    CODE
    SELF.LinesViewSplit(STLined,',','"','"',pRemoveQuotes)
BigBangTheory.LinesViewSplitTAB   PROCEDURE(StringTheory STLined)
    CODE
    SELF.LinesViewSplit(STLined,CHR(9),'','',False)

BigBangTheory.LinesViewSplit PROCEDURE(StringTheory CsvST,string pDelim,<string pQuoteBegin>,<string pQuoteEnd>, BYTE pRemoveQuotes, |
                                          bool pClip, bool pLeft=false, <string pSeparatr>,Long pNested=false)
    MAP
LinSTfromCsvST PROCEDURE(LONG xRow2Load)   !Has lines specs eg. delimeter may not be Comma
ViewColumns    PROCEDURE()
    END
VlbCls CLASS !From Mark Goldberg, but I hacked it to death
FEQ    LONG
RowCnt LONG
ClmCnt USHORT
Chgs   LONG
Init   PROCEDURE(SIGNED xFEQ, LONG xRowCnt, USHORT xClmCnt)!  ,VIRTUAL !<-- cannot because Address(Self) in Init is always VlbCls. Maybe if this was TYPE
VLBprc PROCEDURE(LONG xRow, USHORT xCol),STRING
Contrt PROCEDURE(USHORT ColWd=24)
Expand PROCEDURE()
UnHide PROCEDURE()
      END
X LONG,AUTO
P LONG,DIM(4),STATIC
Fmt ANY
PColumn USHORT
Picture STRING(32)
Window WINDOW('VLB'),AT(,,450,200),GRAY,SYSTEM,MAX,FONT('Segoe UI',9),RESIZE
        BUTTON('&Menu'),AT(2,2,25,12),USE(?MenuBtn),SKIP
        BUTTON('View Lines'),AT(31,2,,12),USE(?LinesBtn),SKIP,TIP('Display raw lines')
        CHECK('Hide Quotes'),AT(87,3),USE(?NoQuotes),SKIP,DISABLE
        CHECK('Left'),AT(142,3),USE(?pLeft),SKIP
        STRING('Picture Column:'),AT(178,3,55),USE(?Pict:Pmt),RIGHT
        COMBO(@s32),AT(238,3,59,10),USE(Picture),DISABLE,VSCROLL,TIP('Change Picture for Column'),DROP(16),FROM('@D1|@D2' & |
                '|@D3|@D17|@N11.2|@S255|@T1|@T3|@T4|@T8')
        CHECK('No Bang'),AT(319,3),USE(?NoBang),SKIP,TIP('Do Not show BigBang Views')
        CHECK('Consolas'),AT(368,3),USE(?ConsolasFnt),SKIP,TIP('Consolas Font for List')
        LIST,AT(1,17),FULL,USE(?List:VLB),FLAT,HVSCROLL,COLUMN,VCR,FORMAT('40L(2)|M~Col1~Q''NAME''')
    END
LinST StringTheory
CsvRecords LONG,AUTO
ColCount LONG,AUTO
CsvST_GotRow LONG
QChanged BOOL
Quote1 PSTRING(16)
Quote2 PSTRING(16)
Separator1 PSTRING(9)
RowInCol1 EQUATE(1) !03/04/21 Put Row# in Column No 1
CellNA    PSTRING(2)
ColumnST  LONG
NoUnHide  PSTRING('~')
    CODE
  IF SELF.DoNotShow  AND ~SELF.ShowAlways THEN RETURN.
  CsvRecords = CsvST.Records()
  IF ~CsvRecords THEN Message('No Lines in Loaded file','LinesViewSplit') ; RETURN .
  IF ~OMITTED(pQuoteBegin) THEN Quote1=pQuoteBegin.
  IF ~OMITTED(pQuoteEnd) THEN Quote2=pQuoteEnd.
  IF ~OMITTED(pSeparatr) AND SIZE(pSeparatr) AND pSeparatr THEN Separator1=pSeparatr.
  LinSTfromCsvST(1)
  ColCount = LinST.Records()  !Assume 1st line has all columns. Pass columns?
  IF RowInCol1 THEN Fmt='20R(2)|M~Row~C(0)@n_6@'.
  LOOP X=1 TO ColCount        !Assume first row has labels
    Fmt=Fmt&'40L(2)|M~' & X & '. <13,10>'& CLIP(SUB(LinST.GetLine(X),1,30)) &'~'
  END
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  IF SELF.LineViewInConsolas THEN ?List:VLB{PROP:FontName}='Consolas'. !06/01/22
  ?ConsolasFnt{PROP:Use}=SELF.LineViewInConsolas
  ?List:VLB{PROP:Format}=Fmt ; CLEAR(Fmt)
  ?List:VLB{PROP:LineHeight}=1+?List:VLB{PROP:LineHeight}
  ?List:VLB{7A58h}=1  !C11 PROP:PropVScroll
  ?Pict:Pmt{PROP:Tip}='Right click on cell to change the Picture'
  ?pLeft{PROP:Use}=pLeft
  IF SELF.ShowAlways THEN HIDE(?NoBang) ELSE ?NoBang{PROP:Use}=SELF.DoNotShow.
  IF Quote1 THEN
     ?NoQuotes{PROP:Use}=pRemoveQuotes ; ENABLE(?NoQuotes)
  END
  0{PROP:Text}='StringTheory Split: '& CsvRecords & ' Records, '& ColCount &' Columns' & |
                ' - SplitStr: ' & QUOTE(pDelim) & '  Quote: ' & QUOTE(Quote1) &'  End: ' & QUOTE(Quote2) &|
                CHOOSE(~Separator1,'','  Separator: ' & QUOTE(Separator1) & CHOOSE(~pNested,'',' (nested)')) & |
               '   Right-Click for Options'
  VlbCls.Init(?List:VLB, CsvRecords, ColCount+RowInCol1)
  ACCEPT
    IF EVENT()=EVENT:NewSelection AND FIELD()=?List:VLB AND KEYCODE()=MouseRight THEN
       SETKEYCODE(0)
       X=?List:VLB{PROPLIST:MouseDownField}
       ColumnST=X-RowInCol1
       CellNA=CHOOSE(ColumnST=0,'~','')
       EXECUTE POPUP(CellNA & 'Copy Cell to Clipboard|' & CellNA & 'View Cell Text' & |
                  '|-|Copy Row to Clipboard|View Row Text|View Row Split in Columns...' & |
                  '|-|Copy All Rows to Clipboard|View All Rows Text' & |
                  '|-|Change @ Picture|Column Alignment{{Left|Center|Right}|-|Hide Column ' & ColumnST)
         SETCLIPBOARD(LinST.GetLine(ColumnST))    !Correct?
         SELF.StringView(LinST.GetLine(ColumnST),'Column  ' & ColumnST & ' in Row ' & CsvST_GotRow)
         SetClipboard(CsvST.GetLine(CsvST_GotRow))   !Copy This Row
         SELF.StringView(CsvST.GetLine(CsvST_GotRow),'View Row ' & CsvST_GotRow) 
         ViewColumns()                               !Vierw Row Split into Cols
         SETCLIPBOARD(CsvST.GetValue())              !Copy All Rows
         SELF.ValueView(CsvST)                       !View All Rows as Text
         BEGIN ; Picture=?List:VLB{PROPLIST:Picture,X} ; ?Pict:Pmt{PROP:Text}='Picture Col ' & ColumnST & ':'
                 ENABLE(?Picture) ; SELECT(?Picture) ; PColumn=X ; END
         ?List:VLB{PROPLIST:Left,X}=1
         ?List:VLB{PROPLIST:Center,X}=1
         ?List:VLB{PROPLIST:Right,X}=1
         BEGIN ; ?List:VLB{PROPLIST:width,X}=0 ; IF X=PColumn THEN DISABLE(?Picture). ; NoUnHide='' ; END  !Hide Column by Width=0       
       END
    END
    CASE ACCEPTED()
    OF ?MenuBtn
        EXECUTE POPUP('Copy All to Clipboard|-|Contract Column Widths|Expand Column Widths' & |
                        '|' & NoUnHide & 'Show Hidden Columns' & |
                        '|-|Copy VLB Format String')
         SETCLIPBOARD(CsvST.GetValue())
         VlbCls.Contrt()
         VlbCls.Expand()
         VlbCls.UnHide()
         SETCLIPBOARD(QUOTE(?List:VLB{PROP:Format}))
        END
    OF ?LinesBtn ; SELF.LinesViewInList(CsvST)
    OF ?NoQuotes OROF ?pLeft ; QChanged=1 ; SELECT(?List:VLB,1)
    OF ?Picture ; ?List:VLB{CHOOSE(~INSTRING(lower(picture[1:2]),'@d@t@n@e'),PROPLIST:Left,PROPLIST:Right) ,PColumn}=1
                  ?List:VLB{PROPLIST:Picture,PColumn}=Picture ; DISPLAY 
    OF ?ConsolasFnt ; ?List:VLB{PROP:FontName}=CHOOSE(~SELF.LineViewInConsolas,'Segoe UI','Consolas')                  
    END
  END
  GETPOSITION(0,P[1],P[2],P[3],P[4])
  RETURN
!------------------------------------------
LinSTfromCsvST PROCEDURE(LONG xRow)    !So all Row Split in one place
  CODE
  IF xRow <> CsvST_GotRow THEN
    CsvST_GotRow = xRow
    LinST.SetValue(CsvST.GetLine(xRow))
    LinST.Split(pDelim,Quote1,Quote2,pRemoveQuotes,pClip,pLeft,Separator1,pNested)
  END
  RETURN
ViewColumns PROCEDURE()
ColST StringTheory
  CODE
  ColST.SetValue(CsvST.GetLine(CsvST_GotRow))
  ColST.Split(pDelim,Quote1,Quote2,pRemoveQuotes)
  SELF.LinesViewInList(ColST,'View Row ' & CsvST_GotRow)
  RETURN
VlbCls.Init PROCEDURE(SIGNED xFEQ, LONG xRowCnt, USHORT xClmCnt)
  CODE
  SELF.FEQ=xFEQ
  SELF.RowCnt=xRowCnt
  SELF.ClmCnt=xClmCnt
  xFEQ{PROP:VLBval} =ADDRESS(SELF)
  xFEQ{PROP:VLBproc}=ADDRESS(SELF.VLBprc)
  RETURN
VlbCls.VLBprc PROCEDURE(LONG xRow, USHORT xCol)
Chg LONG,AUTO
  CODE
  CASE xRow
  OF -1 ; RETURN SELF.RowCnt !Rows
  OF -2 ; RETURN SELF.ClmCnt !Columns
  OF -3 ; Chg=QChanged ; QChanged=0 ; RETURN Chg
  END 
  IF xCol=RowInCol1 THEN 
    RETURN xRow
  ELSIF xRow <> CsvST_GotRow THEN
    LinSTfromCsvST(xRow)
  END
  RETURN LinST.GetLine(xCol-RowInCol1)
VlbCls.Contrt PROCEDURE(USHORT ColWd=24)
  CODE
  LOOP X=1 TO SELF.ClmCnt
       IF SELF.FEQ{PROPLIST:Width,X}>0 THEN SELF.FEQ{PROPLIST:Width,X}=ColWd.
  END
VlbCls.Expand PROCEDURE()
  CODE
  SELF.Contrt(SELF.FEQ{PROP:Width}/SELF.ClmCnt) 
VlbCls.UnHide PROCEDURE()
  CODE
  LOOP X=1 TO SELF.ClmCnt
       IF SELF.FEQ{PROPLIST:Width,X}=0 THEN SELF.FEQ{PROPLIST:Width,X}=40.
  END
  NoUnHide='~'
  RETURN
!-------------------------------
BigBangTheory.StringView PROCEDURE(STRING StrValue, <STRING CapTxt>)
  CODE
  SELF.StringView(StrValue,CapTxt)

BigBangTheory.StringView PROCEDURE(*STRING StrValue, <STRING CapTxt>)
St StringTheory
  CODE
  IF SELF.DoNotShow AND ~SELF.ShowAlways THEN RETURN.
  St.setValue(StrValue)
  SELF.ValueView(St, CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'StringTheory Value'))
!-------------------------------
BigBangTheory.SliceView PROCEDURE(StringTheory pST, Long pStart=1, Long pEnd=0, <STRING CapTxt>) 
SliceSt StringTheory 
Q1 PSTRING(4)  !"?" flags Slice[] out of range in caption, ST will fixup
Q2 PSTRING(4)
SliceCaption PSTRING(48)
  CODE
  IF SELF.DoNotShow AND ~SELF.ShowAlways THEN RETURN.
  IF pStart < 1 OR pStart > pST._DataEnd OR (pStart > pEnd AND pEnd) THEN Q1=' ? '.
  IF pEnd < 1   OR pEnd   > pST._DataEnd OR  pStart > pEnd           THEN Q2=' ? '.
  SliceCaption = 'Slice ['& Q1 & pStart &':'& pEnd & Q2 &'] of 1:'& pST._DataEnd 
!? Below is the kind of "Fix Up" of Start/End that ST does. Maybe check if that will occur and show that fixup [:] in caption?  
!  IF pST._DataEnd > 0 THEN 
!     IF pEnd < 1 OR pEnd > pST._DataEnd THEN pEnd = pST._DataEnd .
!     IF pStart < 1 THEN pStart = 1.
!  END  
  SliceSt.setValue(pST.Slice(pStart,pEnd))
  IF SliceSt._DataEnd=0 THEN  !Was 
     SliceSt.setValue('Invalid? ' & SliceCaption)
  END
  SELF.ValueView(SliceSt,SliceCaption &' - '& CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'StringTheory')) 
  RETURN
!-------------------------------
BigBangTheory.SubView PROCEDURE(StringTheory pST, Long pStart=1, Long pLength=1, <STRING CapTxt>) 
SubSt StringTheory
Q1 PSTRING(4)  !"?" flags Sub() out of range, ST will fixup some
Q2 PSTRING(4)
SubCaption PSTRING(48)
  CODE                              !New 02/04/21
  IF SELF.DoNotShow AND ~SELF.ShowAlways THEN RETURN. 
  IF pStart  < 1           OR pStart > pST._DataEnd THEN Q1=' ? '. 
  IF pLength < 1 OR pLength-1+pStart > pST._DataEnd THEN Q2=' ? '.
  SubCaption = 'Sub('& Q1 & pStart &','& pLength & Q2 &') of 1,'& pST._DataEnd 
  SubSt.setValue(pST.Sub(pStart,pLength))
  IF SubSt._DataEnd=0 THEN 
     SubSt.setValue('Invalid? ' & SubCaption)
  END
  SELF.ValueView(SubSt,SubCaption &' - ' & CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'StringTheory')) 
  RETURN                            
!-------------------------------    
BigBangTheory.ValueView PROCEDURE(StringTheory pST, <STRING CapTxt>)
LenTxt     LONG,AUTO
HexTxt     StringTheory
ShowHex    BYTE
SegoeFnt   BYTE
HScrollTxt BYTE(1),STATIC
VScrollTxt BYTE(1),STATIC
Window WINDOW('S'),AT(,,310,140),GRAY,SYSTEM,MAX,FONT('Segoe UI',9),RESIZE
        TOOLBAR,AT(0,0),USE(?TB1)
            CHECK('Show HEX'),AT(2,0),USE(ShowHex),TIP('See Value in Hex')
            CHECK('HScroll'),AT(74,0),USE(HScrollTxt)
            CHECK('VScroll'),AT(126,0),USE(VScrollTxt)
            CHECK('No Bang'),AT(176,0),USE(?NoBang),TIP('Do Not show BigBang Views')
            CHECK('Segoe'),AT(225,0),USE(SegoeFnt),SKIP,TIP('Check for Segoe<13,10>Uncheck for Consolas')
            BUTTON('Copy'),AT(266,0,30,9),USE(?Copy),TIP('Copy text to Clipboard'),FLAT
        END
        TEXT,AT(0,2),FULL,USE(?Txt),FLAT,HVSCROLL,READONLY,FONT('Consolas',10)
        TEXT,AT(0,2),FULL,USE(?HexTxt),FLAT,HIDE,HVSCROLL,READONLY,FONT('Consolas',10)
    END    
P LONG,DIM(4),STATIC
  CODE
  IF SELF.DoNotShow AND ~SELF.ShowAlways THEN RETURN.
  LenTxt=pSt.length()
  IF ~LenTxt THEN Message('No Text','ValueView') ; RETURN.
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  ?Txt{PROP:HScroll}=HScrollTxt ; ?Txt{PROP:VScroll}=VScrollTxt
  IF LenTxt > 0FFF0h THEN DISABLE(?HScrollTxt,?VScrollTxt). !System Error @ 64k in 11.13505 - Message('Risk GPF?',LenTxt,,'No|Risk')
  ?Txt{PROP:Use}=pSt.valuePtr[1 : LenTxt]
  IF SELF.ShowAlways THEN HIDE(?NoBang) ELSE ?NoBang{PROP:Use}=SELF.DoNotShow.
  0{PROP:Text}=CHOOSE(~OMITTED(CapTxt) AND CapTxt,CLIP(CapTxt),'StringTheory Value') & ' - Length ' & LenTxt
  ACCEPT
    CASE ACCEPTED()
    OF ?HScrollTxt ; ?Txt{PROP:HScroll}=HScrollTxt
    OF ?VScrollTxt ; ?Txt{PROP:VScroll}=VScrollTxt
    OF ?ShowHex
        IF ~HexTxt.length() THEN
           SELF.HexDump(pSt, HexTxt)
           ?HexTxt{PROP:Use}=HexTxt.valuePtr[1 : HexTxt._DataEnd]
        END
        ?Txt{PROP:Hide}=ShowHex ; ?HexTxt{PROP:Hide}=1-ShowHex
    OF ?Copy ; IF ShowHex THEN SETCLIPBOARD(HexTxt.GetValue()) ELSE SETCLIPBOARD(pSt.GetValue()).
    OF ?SegoeFnt ; ?Txt{PROP:FontName}=CHOOSE(~SegoeFnt,'Consolas','Segoe UI')
    END
  END
  GETPOSITION(0,P[1],P[2],P[3],P[4])
  RETURN
!-------------------------------
BigBangTheory.HexDump PROCEDURE(STRING Str2Hex)
  CODE
  RETURN SELF.HexDump(Str2Hex)

BigBangTheory.HexDump PROCEDURE(*STRING Str2Hex)
Str StringTheory
Dmp StringTheory
  CODE
  IF ~SIZE(Str2Hex) THEN RETURN 'Null'.
  Str.SetValue(Str2Hex)
  SELF.HexDump(Str,Dmp)
  RETURN Dmp.GetValue()

BigBangTheory.HexDump PROCEDURE(StringTheory pStr, StringTheory stDump)
P   Long,auto
C   UShort,auto
Lin Group,PRE()
      String('<13,10>')
Off   String('Offset ')
Hex   String(' 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16  ')
Chr   String('0123456789abcdef')
    End
stHex StringTheory
  CODE
  stHex.setValue(pStr)
  stHex.ToHex(st:Upper,true)
  stDump.SetLength((2+int(pStr._DataEnd / 16))*Size(Lin)) ! pre-allocate memory to save dynamic expanding
  stDump.Free()
  stDump.CatAddr(address(Lin)+2,size(Lin)-2)  !Headings w/o 13,10
  loop P = 0 to pStr._DataEnd-1 by 16
     Off=P
     Hex=stHex.Sub(P*3+1, 48) ; Hex[24]='-'
     Chr=pStr.Sub(P+1, 16)
     loop c = 1 to 16
       if val(Chr[c]) < 32 then Chr[c] = '.'.
     end
     stDump.CatAddr(address(Lin),size(Lin))
  end
  RETURN
!-------------------------------
BigBangTheory.WrapView PROCEDURE(StringTheory STorig, <STRING CapTxt>, Bool pWrap=false)
LenTxt      LONG,AUTO
WrapTheTxt  BYTE
WrapWidth   LONG(80)
WrapKeep    BYTE(1)
WrapLeft    BYTE
HScrollTxt  BYTE(1)
VScrollTxt  BYTE(1)
stWrap      StringTheory
Window WINDOW('S'),AT(,,400,250),GRAY,SYSTEM,MAX,FONT('Consolas',10),RESIZE
        TOOLBAR,AT(0,0,350,12),USE(?TB1),FONT('Segoe UI',9)
            CHECK('Wrap Text'),AT(2,1),USE(WrapTheTxt)
            PROMPT('Wrap Column:'),AT(52,2),USE(?WrapPrompt)
            SPIN(@n5),AT(99,2,34,9),USE(WrapWidth),DELAY(50),SKIP,DISABLE,HSCROLL,RIGHT,TIP('Width to word wrap each line<13,10>Pr' & |
                    'ess Enter to wrap at width'),RANGE(5,9999),STEP(5),ALRT(EnterKey)
            CHECK('Keep Breaks'),AT(145,1),USE(WrapKeep),DISABLE,TIP('Keep existing CR/LF in the output')
            CHECK('Left'),AT(202,1),USE(WrapLeft),DISABLE,TIP('Leading white space is removed')
            CHECK('HScroll'),AT(251,1),USE(HScrollTxt)
            CHECK('VScroll'),AT(292,1),USE(VScrollTxt)
            CHECK('No Bang'),AT(350,1),USE(?NoBang),TIP('Do Not show BigBang Views')
        END
        TEXT,AT(0,2),FULL,USE(?Txt),FLAT,HVSCROLL,READONLY
        TEXT,AT(0,2),FULL,USE(?WrapTxt),FLAT,HIDE,HVSCROLL,READONLY
    END
P LONG,DIM(4),STATIC
  CODE
  IF SELF.DoNotShow AND ~SELF.ShowAlways THEN RETURN.
  LenTxt=stOrig.Length()
  IF ~stOrig.clipLength() THEN Message('No Text','WrapView') ; RETURN.
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  ?Txt{PROP:Use}=StOrig.valuePtr[1 : LenTxt]
  IF SELF.ShowAlways THEN HIDE(?NoBang) ELSE ?NoBang{PROP:Use}=SELF.DoNotShow.
  IF LenTxt>0FF00h THEN DISABLE(?HScrollTxt,?VScrollTxt). !System Error @ 64k
  DO CapRtn
  WrapTheTxt=pWrap ; IF pWrap THEN POST(EVENT:Accepted,?WrapTheTxt).
  ACCEPT
    CASE ACCEPTED()
    OF ?HScrollTxt ; ?Txt{PROP:HScroll}=HScrollTxt
    OF ?VScrollTxt ; ?Txt{PROP:VScroll}=VScrollTxt
    OF ?WrapTheTxt ; IF WrapTheTxt THEN ENABLE(?WrapWidth,?WrapLeft) ELSE DISABLE(?WrapWidth,?WrapLeft).
                     ?Txt{PROP:Hide}=WrapTheTxt
                     ?WrapTxt{PROP:Hide}=1-WrapTheTxt
                     IF WrapTheTxt THEN DO WrapRtn ELSE DO CapRtn.
    OF ?WrapWidth OROF ?WrapKeep OROF ?WrapLeft ; DO WrapRtn
    END
    IF FIELD()=?WrapWidth AND INLIST(EVENT(),EVENT:NewSelection,EVENT:AlertKey) THEN
       UPDATE ; DO WrapRtn
    END
  END
  GETPOSITION(0,P[1],P[2],P[3],P[4])
  CLOSE(Window)
  RETURN
CapRtn ROUTINE
  0{PROP:Text}=CHOOSE(~OMITTED(CapTxt) AND CapTxt,CLIP(CapTxt),'StringTheory Wrap') & ' - Length ' & LenTxt & choose(~WrapTheTxt,'',' - Wrapped ' & stWrap.Length())
WrapRtn ROUTINE
  StWrap.SetValue(StOrig)
  StWrap.WrapText(WrapWidth,WrapKeep,WrapLeft)
  ?WrapTxt{PROP:Use}=StWrap.valuePtr[1 : StWrap._DataEnd]
  DO CapRtn
  DISPLAY 
!----------------
BigBangTheory.ReplaceView PROCEDURE(StringTheory St4Rpl, string pOldValue, string pNewValue, |
                        long pCount, long pStart, long pEnd, long pNoCase, bool pRecursive)!,Long,Proc  
P BYTE,AUTO 
RV LONG,AUTO
ST StringTheory 
    CODE !I found Replace a PITA to get NoCase=1
  ST.SetValue(St4Rpl)
  LOOP P=1 TO 2
    SELF.StringView('StringTheory.Replace(' & |  
      '<13,10>string pOldValue =>"' & pOldValue &'"'& |
      '<13,10>string pNewValue =>"' & pNewValue &'"'& |
      '<13,10>long   pCount=0  =>' &  pCount     & |
      '<13,10>long   pStart=1  =>' &  pStart     & |
      '<13,10>long   pEnd=0    =>' &  pEnd       & |
      '<13,10>long   pNoCase=0 =>' &  pNoCase    & |
      '<13,10>bool   pRecursive=false =>' & pRecursive & | 
      '<13,10>={40} <13,10>' & |
      ST.GetValue(),'Replace ' & CHOOSE(P=1,'Before Call','After, Return=' & RV) )
      IF P=1 THEN 
         RV=ST.Replace(pOldValue, pNewValue,pCount,pStart,pEnd,pNoCase,pRecursive)
      END
   END 
   RETURN RV
!----------------------
BigBangTheory.ParmsView PROCEDURE(String P1,<String P2>,<String P3>,<String P4>,<String P5>,<String P6>,<String P7>,<String P8>,<String P9>)!,Long,Proc
P    USHORT,AUTO
PMax USHORT
SP  ANY
    CODE
    LOOP P=1 TO 9 ; IF ~OMITTED(1+P) THEN PMax=P. ; END 
    LOOP P=1 TO PMax
        SP=SP & CHOOSE(P=1,'','<13,10>') & |
                'Parm ' & P &' => ' & |
                CHOOSE(~OMITTED(1+P), | 
                        '"' & QUOTE(CHOOSE(P,P1,P2,P3,P4,P5,P6,P7,P8,P9)) &'"', |
                        '<<omitted>') 
    END
    SELF.StringView(SP,'ParmsView - Count: ' & PMax)
    RETURN PMax 
!----------------------
BigBangTheory.ProtoView PROCEDURE(String Prototype, String P1,<String P2>,<String P3>,<String P4>,<String P5>,<String P6>,<String P7>,<String P8>,<String P9>)
P    USHORT,AUTO
PMax USHORT,AUTO
SP   ANY
stProt StringTheory 
ProCnt USHORT,AUTO
    CODE
    stProt.SetValue(Prototype)
    stProt.Split(',')   !Problem Arrays: *LONG[,] Current
    ProCnt=stProt.Records() 
    PMax=ProCnt
    LOOP P=1 TO 9 ; IF P>PMax AND ~OMITTED(2+P) THEN PMax=P. ; END 
    LOOP P=1 TO PMax
        SP=SP & 'Parm ' & P &' => ' & |
                CHOOSE(~OMITTED(2+P), | 
                        '"' & QUOTE(CHOOSE(P,P1,P2,P3,P4,P5,P6,P7,P8,P9,'?#' & P)) &'"', |
                        '<<omitted>') & |
                '   <9>' & |
                CHOOSE(P<=ProCnt,stProt.GetLine(P),'? Prototype ?') & |
                '<13,10>'
    END
    SP=SP & '<13,10>Prototype: ' & Prototype
    SELF.StringView(SP,'ProtoView - Count: ' & PMax)
    RETURN PMax
!----------------------------------------------------------------------------
BigBangTheory.Reflection          PROCEDURE(StringTheory ST, <STRING CapTxt>, BOOL bIgnoreDoNotShow=False)
ClassDataQ QUEUE,PRE(ClsDatQ)
Type         STRING(1)   !ClsDatQ:Type  D=Data M=Method
Label        STRING(96)  !ClsDatQ:Label
DataValue    STRING(255) !ClsDatQ:DataValue
           END
SubBegin LONG
SubLength LONG
SlcStart LONG
SlcEnd   LONG

Window WINDOW('StringTheory Reflection'),AT(,,395,235),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        BUTTON('Value'),AT(3,2,27,12),USE(?ValueBtn),TIP('View ST .Value <13,10>Calls Bang.ValueView' & |
                '() that calls ST.GetValue()')
        BUTTON('SUB()'),AT(40,2,28,12),USE(?SubBtn),TIP('Fill in Start and Length to the right, then' & |
                ' click SUB <13,10>Calls Bang.SubView() that calls ST.Sub()')
        ENTRY(@n8),AT(71,4,31,9),USE(SubBegin),RIGHT,TIP('Sub String Start Position')
        STRING(','),AT(103,4,4,9),USE(?SubCma),TRN,FONT('Consolas',9)
        ENTRY(@n8),AT(109,4,31,9),USE(SubLength),LEFT,TIP('Sub String Length')
        BUTTON('Slice()'),AT(150,2,30,12),USE(?SliceBtn),TIP('Fill in Begin : End Slice to the right' & |
                ', then click SLICE <13,10>Calls Bang.SloceView() that calls ST.Slice()')
        ENTRY(@n8),AT(183,4,31,9),USE(SlcStart),RIGHT,TIP('Slice String START Position')
        STRING(':'),AT(215,4,4,9),USE(?SlcCln),TRN,FONT('Consolas',9)
        ENTRY(@n_8),AT(221,4,31,9),USE(SlcEnd),LEFT,TIP('Slice String  END Position')
        BUTTON('Lines Q'),AT(262,2,34,12),USE(?LinesBtn),TIP('View ST Lines Q from Split <13,10>Call' & |
                's Bang.LinesViewInList() that calls ST.GetLine()')
        BUTTON('CSV'),AT(298,2,22,12),USE(?CSVBtn),TIP('View Lines with CSV split <13,10>Calls Bang.' & |
                'LinesViewSplitCSV() that calls ST.Split() etc')
        BUTTON('TAB'),AT(322,2,22,12),USE(?TABBtn),TIP('View Lines with TAB split <13,10>Calls Bang.' & |
                'LinesViewSplitTAB()')
        CHECK('No Bang'),AT(353,3),USE(?NoBang),SKIP,FONT(,8),TIP('Do Not show BigBang Views')
        LIST,AT(3,18),FULL,USE(?LIST:ClassDataQ),FONT('Segoe UI',10),FROM(ClassDataQ),FORMAT('18C(0)' & |
                '|FM~DM~C(0)@s1@110L(2)|FM~Label~@s96@?200L(2)|FM~Data~@s255@'),ALRT(CtrlC), |
                 ALRT(CtrlShiftC)
    END

ClassReflect CLASS
Reflect   PROCEDURE(*GROUP pClass,STRING pLevelPrefix)
SetQValue PROCEDURE(STRING pName, STRING pValue)
             END
    CODE
    IF SELF.DoNotShow AND ~SELF.ShowAlways AND ~bIgnoreDoNotShow THEN RETURN.
    OPEN(Window) ; DO PrepareWnd
    ACCEPT
        CASE ACCEPTED()
        OF ?ValueBtn ; HIDE(0) ; SELF.ValueView(ST,CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'')) ; UNHIDE(0)
        OF ?SubBtn   ; HIDE(0) ; SELF.SubView(ST,SubBegin,SubLength,CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'')) ; UNHIDE(0)
        OF ?SliceBtn ; HIDE(0) ; SELF.SliceView(ST,SlcStart,SlcEnd ,CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'')) ; UNHIDE(0)
        OF ?LinesBtn ; HIDE(0) ; SELF.LinesViewInList(ST,CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'')) ; UNHIDE(0)
        OF ?CSVBtn   ; HIDE(0) ; SELF.LinesViewSplitCSV(ST) ; UNHIDE(0)
        OF ?TABBtn   ; HIDE(0) ; SELF.LinesViewSplitTAB(ST) ; UNHIDE(0)

        OF ?LIST:ClassDataQ
            GET(ClassDataQ,CHOICE(?LIST:ClassDataQ))
            CASE KeyCode()
            OF CtrlC      ; SETCLIPBOARD(ClsDatQ:Label)
            OF CtrlShiftC ; SETCLIPBOARD(ClsDatQ:DataValue)
            OF MouseRight
               SETKEYCODE(0)
               CASE POPUP('Copy Label <9>Ctrl+C|Copy Data <9>Ctrl+Shift+C|-|Copy Both')
               OF 1 ; SETCLIPBOARD(ClsDatQ:Label)
               OF 2 ; SETCLIPBOARD(ClsDatQ:DataValue)
               OF 3 ; SETCLIPBOARD(CLIP(ClsDatQ:Label)&' = '&ClsDatQ:DataValue)
               END
            OF MouseLeft2
               CASE ClsDatQ:Label
               OF   'VALUEPTR'
               OROF 'VALUE' ; POST(EVENT:Accepted,?ValueBtn)
               OF 'LINES'   ; POST(EVENT:Accepted,?LinesBtn)
               END
            END
        END
        CASE EVENT()
        OF EVENT:AlertKey
           IF FIELD()=?LIST:ClassDataQ THEN POST(EVENT:Accepted,?LIST:ClassDataQ).
        END
    END
    RETURN

PrepareWnd ROUTINE
    COMPILE('!*cbw*',_CbWndPreview_)
    WndPrvCls.Init(2) !Runtime Window design and PROPs
             !*cbw*
    0{PROP:Text}=CHOOSE(~OMITTED(CapTxt) AND CapTxt, CLIP(CapTxt)&' - ','') & |
                 ' - StringTheory Reflection - Length '& ST.Length() &' - LinesQ '& ST.Records()
    IF ~ST.Records() THEN DISABLE(?CSVBtn,?TABBtn).
    IF SELF.ShowAlways THEN HIDE(?NoBang) ELSE ?NoBang{PROP:Use}=SELF.DoNotShow.
    DISPLAY
    ClassReflect.Reflect(ST,'')
    EXIT
!----------------------------------------------------------------
ClassReflect.Reflect PROCEDURE(*GROUP pClass,STRING pLevelPrefix)
Ndx      LONG,AUTO
AnyData  ANY
WhoName  PSTRING(128)
DimArray LONG   !ManyCnt
DimIdx   LONG
GroupRef &GROUP !Sub Group
GroupPfx PSTRING(128)
AnyStr   STRING(4)
AnyLong  LONG,OVER(AnyStr)
StrRef   ANY
    CODE
    LOOP Ndx = 1 to 999
         AnyData &= WHAT(pClass,Ndx)
         IF AnyData &= NULL THEN BREAK.
         WhoName = pLevelPrefix & WHO(pClass,Ndx)

         CLEAR(ClassDataQ)
         ClsDatQ:Type          =  'D'
         ClsDatQ:Label         =  WhoName
         DimArray = HowMany(pClass,Ndx)
         IF DimArray > 1 THEN
             ClsDatQ:Label = WhoName &' []'
             ClsDatQ:DataValue = 'ARRAY DIM( ' & DimArray &' )'
             IF ISGROUP(pClass,Ndx) THEN
                ClsDatQ:DataValue = 'GROUP....  ' & CLIP(ClsDatQ:DataValue)
                DO Add1DataQ
                CYCLE
             END
             DO Add1DataQ
             LOOP DimIdx=1 TO DimArray
                 AnyData &= WHAT(pClass,Ndx, DimIdx)
                 ClsDatQ:DataValue=AnyData
                 IF ~ClsDatQ:DataValue OR ClsDatQ:DataValue='0' THEN CYCLE.
                 ClsDatQ:Label = WhoName &' [ ' & DimIdx &' ]'
                 DO Add1DataQ
             END
             CYCLE

         ELSIF ISGROUP(pClass,Ndx) THEN
             ClsDatQ:DataValue = 'GROUP.{9}'
             GroupPfx = WhoName &'.'
             ClsDatQ:Label = WhoName & '.{9}'
             DO Add1DataQ
             GroupRef &= GETGROUP(pClass,Ndx)
             SELF.Reflect(GroupRef,GroupPfx)
         ELSE
             ClsDatQ:DataValue = AnyData
             ! DO ST_StringRef_Value_Rtn  Not possible without TUFO ...
             DO Add1DataQ
         END
     END
     RETURN

Add1DataQ ROUTINE   !Hack some &STRING to show value using Who 
    CASE ClsDatQ:Label
    OF 'VALUE'
  OROF 'VALUEPTR'  ; ClsDatQ:DataValue = ST.GetValue() !An &String
    OF 'LASTERROR' ; ClsDatQ:DataValue = ST.LastError  !An &String
    OF 'LINES'     ; ClsDatQ:DataValue = 'Queue with '& ST.Records() &' Records'
    END
    ADD(ClassDataQ)
    CASE ClsDatQ:Label
    OF 'WINERRORCODE'
       IF ST.winErrorCode <> 0 THEN
          ClsDatQ:Type  =  'M'
          ClsDatQ:Label =  'FormatMessage( '& ST.winErrorCode &' )'
          ClsDatQ:DataValue = ST.winErrorCode &' = '& ST.FormatMessage(ST.winErrorCode)
          ADD(ClassDataQ)
       END
    END
    EXIT
!---------------------
ClassReflect.SetQValue PROCEDURE(STRING pName, STRING pValue)
    CODE
    ClsDatQ:Label = UPPER(pName)
    GET(ClassDataQ,ClsDatQ:Label)
    IF ~ERRORCODE() THEN
        ClsDatQ:DataValue=pValue
        PUT(ClassDataQ)
    END
    RETURN