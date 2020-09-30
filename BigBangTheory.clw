                    MEMBER()
!----------------------------------------------------------------------------
! BigBangTheory - View StringTheory Lines Queue and Optionally Split lines by Delimeter
! (c)2020 by Carl T. Barnes - Released under the MIT License
!----------------------------------------------------------------------------
    INCLUDE('KEYCODES.CLW')
    INCLUDE('BigBangTheory.INC'),ONCE
  MAP.
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
        LIST,AT(1,2),FULL,USE(?List:LinesQ),FLAT,HVSCROLL,VCR,FORMAT('24R(2)|M~Row~C(0)@n_6@999L(2)~Lines Q~')
    END
X LONG,AUTO
P LONG,DIM(4),STATIC
LnzRecords LONG,AUTO
    CODE
  IF SELF.DoNotShow THEN RETURN.
  LnzRecords = LnzST.Records()
  IF ~LnzRecords THEN Message('No Lines in Loaded file','LinesViewInList') ; RETURN .
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  ?List:LinesQ{PROP:LineHeight}=1+?List:LinesQ{PROP:LineHeight}
  ?List:LinesQ{7A58h}=1  !C11 PROP:PropVScroll 
  0{PROP:Text}=CHOOSE(~OMITTED(CapTxt) AND CapTxt,CLIP(CapTxt),'StringTheory Lines') &' - '& LnzRecords & ' Records  -  Right-Click for Options'
  X=LOG10(LnzRecords)+1 ; IF X<4 THEN X=4.
  ?List:LinesQ{PROPLIST:Picture,1}='n_' & X
  ?List:LinesQ{PROPLIST:Width,1}  =2 + 4*X
  VlbLines.Init(?List:LinesQ, LnzRecords, 2)
  ACCEPT
    IF EVENT()=EVENT:NewSelection AND FIELD()=?List:LinesQ AND KEYCODE()=MouseRight THEN
       SETKEYCODE(0)
       X=CHOICE(?List:LinesQ)
       EXECUTE POPUP('Copy Row to Clipboard|View Row Text|-|Copy All Rows Text|View All Rows Text')
         SetClipboard(LnzST.GetLine(X))
         SELF.StringView(LnzST.GetLine(X),'Row ' & X) ! MESSAGE(LnzST.GetLine(X),'Row ' & X,,,,MSGMODE:CANCOPY)
         SetClipboard(LnzST.GetValue()) 
         SELF.ValueView(LnzST)
       END
    END
    CASE ACCEPTED()
    END
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

BigBangTheory.LinesViewSplit PROCEDURE(StringTheory CsvST,string pDelim,<string pQuoteBegin>,<string pQuoteEnd>, BYTE pRemoveQuotes)
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
      END
X LONG,AUTO
P LONG,DIM(4),STATIC
Fmt ANY
PColumn USHORT
Picture STRING(32)
Window WINDOW('VLB'),AT(,,450,200),GRAY,SYSTEM,MAX,FONT('Segoe UI',9),RESIZE
        BUTTON('&Menu'),AT(2,2,25,12),USE(?MenuBtn),SKIP
        BUTTON('View Lines'),AT(31,2,,12),USE(?LinesBtn),SKIP,TIP('Display raw lines')
        CHECK('Hide Quotes'),AT(87,3),USE(?NoQuotes),SKIP,HIDE
        STRING('Picture Column:'),AT(173,3,55),USE(?Pict:Pmt),RIGHT
        COMBO(@s32),AT(233,3,59,10),USE(Picture),DISABLE,VSCROLL,TIP('Change Picture for Column'),DROP(16),FROM('@D1|@D2' & |
                '|@D3|@D17|@N11.2|@S255|@T1|@T3|@T4|@T8')
        LIST,AT(1,17),FULL,USE(?List:VLB),FLAT,HVSCROLL,COLUMN,VCR,FORMAT('40L(2)|M~Col1~Q''NAME''')
    END
LinST StringTheory
CsvRecords LONG,AUTO
ColCount LONG,AUTO
CsvST_GotRow LONG
QChanged BOOL
Quote1 PSTRING(9)
Quote2 PSTRING(9)
    CODE
  IF SELF.DoNotShow THEN RETURN.
  CsvRecords = CsvST.Records()
  IF ~CsvRecords THEN Message('No Lines in Loaded file','LinesViewSplit') ; RETURN .
  IF ~OMITTED(pQuoteBegin) THEN Quote1=pQuoteBegin.
  IF ~OMITTED(pQuoteEnd) THEN Quote2=pQuoteEnd.
  LinSTfromCsvST(1)
  ColCount = LinST.Records()  !Assume 1st line has all columns. Pass columns?
  LOOP X=1 TO ColCount        !Assume first row has labels
    Fmt=Fmt&'40L(2)|M~' & X & '. <13,10>'& CLIP(SUB(LinST.GetLine(X),1,30)) &'~'
  END
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  ?List:VLB{PROP:Format}=Fmt ; CLEAR(Fmt)
  ?List:VLB{PROP:LineHeight}=1+?List:VLB{PROP:LineHeight}
  ?List:VLB{7A58h}=1  !C11 PROP:PropVScroll
  ?Pict:Pmt{PROP:Tip}='Right click on cell to change the Picture'
  IF Quote1 THEN 
     ?NoQuotes{PROP:Use}=pRemoveQuotes ; UNHIDE(?NoQuotes)
  END
  0{PROP:Text}='StringTheory Split - '& CsvRecords & ' Records - '& ColCount &' Columns' & |
                ' - SplitStr: ' & QUOTE(pDelim) & '  Quote: ' & QUOTE(Quote1) &'  End: ' & QUOTE(Quote2) &' Remove: ' & pRemoveQuotes & |
               ' -  Right-Click for Options' 
  VlbCls.Init(?List:VLB, CsvRecords, ColCount)
  ACCEPT
    IF EVENT()=EVENT:NewSelection AND FIELD()=?List:VLB AND KEYCODE()=MouseRight THEN
       SETKEYCODE(0)
       X=?List:VLB{PROPLIST:MouseDownField}
       EXECUTE POPUP('Copy Cell to Clipboard|View Cell Text|-|Copy Row to Clipboard|View Row Text|View Row Split...' & |
                  '|-|Copy All Rows to Clipboard|View All Rows Text' & |
                  '|-|Hide Column ' & X &'|Change @ Picture|Column Alignment{{Left|Center|Right}')
         SETCLIPBOARD(LinST.GetLine(X))    !Correct?
         SELF.StringView(LinST.GetLine(X),'Column  ' & X & ' in Row ' & CsvST_GotRow) !MESSAGE(LinST.GetLine(X),'Column  ' & X & ' in Row ' & CsvST_GotRow,,,,MSGMODE:CANCOPY)
         SetClipboard(CsvST.GetLine(CsvST_GotRow))
         SELF.StringView(CsvST.GetLine(CsvST_GotRow),'View Row ' & CsvST_GotRow) ! MESSAGE(CsvST.GetLine(CsvST_GotRow),'View Row ' & CsvST_GotRow,,,,MSGMODE:CANCOPY)
         ViewColumns()
         SETCLIPBOARD(CsvST.GetValue())
         SELF.ValueView(CsvST)
         BEGIN ; ?List:VLB{PROPLIST:width,X}=0 ; IF X=PColumn THEN DISABLE(?Picture). ; END
         BEGIN ; Picture=?List:VLB{PROPLIST:Picture,X} ; ?Pict:Pmt{PROP:Text}='&Picture Col ' & X & ':'
                 ENABLE(?Picture) ; SELECT(?Picture) ; PColumn=X ; END
         ?List:VLB{PROPLIST:Left,X}=1
         ?List:VLB{PROPLIST:Center,X}=1
         ?List:VLB{PROPLIST:Right,X}=1
       END
    END
    CASE ACCEPTED()
    OF ?MenuBtn
        EXECUTE POPUP('Copy All to Clipboard|-|Contract Column Widths|Expand Column Widths' & |
                        '|-|Copy VLB Format String')
         SETCLIPBOARD(CsvST.GetValue())
         VlbCls.Contrt()
         VlbCls.Expand()
         SETCLIPBOARD(?List:VLB{PROP:Format})
        END
    OF ?LinesBtn ; SELF.LinesViewInList(CsvST)
    OF ?NoQuotes ; QChanged=1 ; SELECT(?List:VLB,1)
    OF ?Picture ; ?List:VLB{CHOOSE(~INSTRING(lower(picture[1:2]),'@d@t@n@e'),PROPLIST:Left,PROPLIST:Right) ,PColumn}=1
                  ?List:VLB{PROPLIST:Picture,PColumn}=Picture ; DISPLAY
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
    LinST.Split(pDelim,Quote1,Quote2,pRemoveQuotes)
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
  IF xRow <> CsvST_GotRow THEN
    LinSTfromCsvST(xRow)
  END
  RETURN LinST.GetLine(xCol)
VlbCls.Contrt PROCEDURE(USHORT ColWd)
  CODE
  LOOP X=1 TO SELF.ClmCnt
       IF SELF.FEQ{PROPLIST:Width,X}>0 THEN SELF.FEQ{PROPLIST:Width,X}=ColWd.
  END
VlbCls.Expand PROCEDURE()
  CODE
  SELF.Contrt(SELF.FEQ{PROP:Width}/SELF.ClmCnt)
!-------------------------------
BigBangTheory.ValueView PROCEDURE(StringTheory SeeST, <STRING CapTxt>) 
    CODE
    IF ~SELF.DoNotShow THEN |
       SELF.StringView(SeeST.GetValue(),CHOOSE(~OMITTED(CapTxt),CLIP(CapTxt),'StringTheory Value')) .

BigBangTheory.StringView PROCEDURE(STRING StrValue, <STRING CapTxt>)
LenTxt  LONG,AUTO
HexTxt  ANY
ShowHex    BYTE
HScrollTxt BYTE(1)
VScrollTxt BYTE(1)
Window WINDOW('S'),AT(,,310,140),GRAY,SYSTEM,MAX,FONT('Consolas',10),RESIZE
        TOOLBAR,AT(0,0,325),USE(?TB1)
            CHECK('Show HEX'),AT(2,0),USE(ShowHex),TIP('See Value in Hex')
            CHECK('HScroll'),AT(74,0),USE(HScrollTxt)
            CHECK('VScroll'),AT(126,0),USE(VScrollTxt)
        END
        TEXT,AT(0,2),FULL,USE(?Txt),HVSCROLL,READONLY,FLAT
        TEXT,AT(0,2),FULL,USE(?HexTxt),HIDE,HVSCROLL,READONLY,FLAT
    END
P LONG,DIM(4),STATIC
  CODE
  IF SELF.DoNotShow THEN RETURN.
  LenTxt=SIZE(StrValue)
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]). 
  IF LenTxt > 0FFF0h THEN DISABLE(?HScrollTxt,?VScrollTxt). !System Error @ 64k in 11.13505 - Message('Risk GPF?',LenTxt,,'No|Risk')
  ?Txt{PROP:Use}=StrValue
  0{PROP:Text}=CHOOSE(~OMITTED(CapTxt) AND CapTxt,CLIP(CapTxt),'String Value') & ' - Length ' & LenTxt 
  ACCEPT
    CASE ACCEPTED()
    OF ?HScrollTxt ; ?Txt{PROP:HScroll}=HScrollTxt
    OF ?VScrollTxt ; ?Txt{PROP:VScroll}=VScrollTxt
    OF ?ShowHex 
        IF ~LenTxt THEN CYCLE.
        IF HexTxt &= NULL THEN
           HexTxt = SELF.HexDump(StrValue) 
           ?HexTxt{PROP:Use}=HexTxt
        END
        ?Txt{PROP:Hide}=ShowHex ; ?HexTxt{PROP:Hide}=1-ShowHex
    END
  END
  GETPOSITION(0,P[1],P[2],P[3],P[4]) 
  RETURN
!-------------------------------
BigBangTheory.HexDump PROCEDURE(*STRING Str2Hex)
Str StringTheory
Dmp StringTheory
    CODE
    Str.SetValue(Str2Hex)
    SELF.HexDump(Str,Dmp)
    RETURN Dmp.GetValue()
 
BigBangTheory.HexDump PROCEDURE(StringTheory pStr, StringTheory Dump)
offset  Long,auto
x       Long,auto
Mem_Hex Equate(8+4-4)
Mem_Chr Equate(Mem_Hex+16*3+1)
MemLine String(Mem_Chr+16)    !AAAAAAAA XX x16  16CHRbytes
stHex   StringTheory
Lin_Pos String(9),auto
Lin_Hex String(49),auto
Lin_Chr String(16),auto
  CODE
  stHex.setValue(pStr)
  stHex.ToHex(st:Upper,true)
  MemLine[Mem_Hex : Mem_Hex+16*3]=' 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16'
  MemLine[Mem_Chr : Mem_Chr+15]='0123456789abcdef'
  Dump.setValue(MemLine)
  loop offset=0 to pStr._DataEnd-1 by 16
     Lin_Pos='<13,10>' & offset
     Lin_Hex=stHex.sub(offset*3 + 1, 48) ; Lin_Hex[24]='-'
     Lin_Chr=pStr.sub(offset+1, 16)
     loop x = 1 to 16
       if val(Lin_Chr[x]) < 32 then Lin_Chr[x] = '.'.
     end
     Dump.Append(Lin_Pos & Lin_Hex & Lin_Chr)
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
        END
        TEXT,AT(0,2),FULL,USE(?Txt),FLAT,HVSCROLL,READONLY
        TEXT,AT(0,2),FULL,USE(?WrapTxt),FLAT,HIDE,HVSCROLL,READONLY
    END
P LONG,DIM(4),STATIC
  CODE 
  IF SELF.DoNotShow THEN RETURN. 
  LenTxt=stOrig.Length() 
  IF ~stOrig.clipLength() THEN Message('No Text','WrapView') ; RETURN. 
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  ?Txt{PROP:Use}=StOrig.valuePtr[1 : LenTxt]
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