                    MEMBER()
!----------------------------------------------------------------------------
! BigBangTheory - View StringTheory Lines Queue and Optionally Split lines by Delimeter
! (c)2020 by Carl T. Barnes - Released under the MIT License
!----------------------------------------------------------------------------
    INCLUDE('KEYCODES.CLW')
    INCLUDE('BigBangTheory.INC'),ONCE
_BBT_Needs_Project_Defines EQUATE(StringTheoryLinkMode + StringTheoryDllMode)
! StringTheoryLinkMode=>1;StringTheoryDllMode=>0  or StringTheoryLinkMode=>0;StringTheoryDllMode=>1
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
       EXECUTE POPUP('Copy Row to Clipboard|View Row Text|-|Copy All Rows Text|View All Rows Text' & |
                    CHOOSE(~SELF.DoNotShow,'|-|-','|-|+') & 'Do Not Show BigBang Views')
         SetClipboard(LnzST.GetLine(X))
         SELF.StringView(LnzST.GetLine(X),'Row ' & X) ! MESSAGE(LnzST.GetLine(X),'Row ' & X,,,,MSGMODE:CANCOPY)
         SetClipboard(LnzST.GetValue())
         SELF.ValueView(LnzST)
         SELF.DoNotShow=1-SELF.DoNotShow
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
    CODE
  IF SELF.DoNotShow THEN RETURN.
  CsvRecords = CsvST.Records()
  IF ~CsvRecords THEN Message('No Lines in Loaded file','LinesViewSplit') ; RETURN .
  IF ~OMITTED(pQuoteBegin) THEN Quote1=pQuoteBegin.
  IF ~OMITTED(pQuoteEnd) THEN Quote2=pQuoteEnd.
  IF ~OMITTED(pSeparatr) AND SIZE(pSeparatr) AND pSeparatr THEN Separator1=pSeparatr.
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
  ?pLeft{PROP:Use}=pLeft
  ?NoBang{PROP:Use}=SELF.DoNotShow
  IF Quote1 THEN
     ?NoQuotes{PROP:Use}=pRemoveQuotes ; ENABLE(?NoQuotes)
  END
  0{PROP:Text}='StringTheory Split: '& CsvRecords & ' Records, '& ColCount &' Columns' & |
                ' - SplitStr: ' & QUOTE(pDelim) & '  Quote: ' & QUOTE(Quote1) &'  End: ' & QUOTE(Quote2) &|
                CHOOSE(~Separator1,'','  Separator: ' & QUOTE(Separator1) & CHOOSE(~pNested,'',' (nested)')) & |
               '   Right-Click for Options'
  VlbCls.Init(?List:VLB, CsvRecords, ColCount)
  ACCEPT
    IF EVENT()=EVENT:NewSelection AND FIELD()=?List:VLB AND KEYCODE()=MouseRight THEN
       SETKEYCODE(0)
       X=?List:VLB{PROPLIST:MouseDownField}
       EXECUTE POPUP('Copy Cell to Clipboard|View Cell Text|-|Copy Row to Clipboard|View Row Text|View Row Split...' & |
                  '|-|Copy All Rows to Clipboard|View All Rows Text' & |
                  '|-|Change @ Picture|Column Alignment{{Left|Center|Right}|-|Hide Column ' & X)
         SETCLIPBOARD(LinST.GetLine(X))    !Correct?
         SELF.StringView(LinST.GetLine(X),'Column  ' & X & ' in Row ' & CsvST_GotRow) !MESSAGE(LinST.GetLine(X),'Column  ' & X & ' in Row ' & CsvST_GotRow,,,,MSGMODE:CANCOPY)
         SetClipboard(CsvST.GetLine(CsvST_GotRow))
         SELF.StringView(CsvST.GetLine(CsvST_GotRow),'View Row ' & CsvST_GotRow) ! MESSAGE(CsvST.GetLine(CsvST_GotRow),'View Row ' & CsvST_GotRow,,,,MSGMODE:CANCOPY)
         ViewColumns()
         SETCLIPBOARD(CsvST.GetValue())
         SELF.ValueView(CsvST)
         BEGIN ; Picture=?List:VLB{PROPLIST:Picture,X} ; ?Pict:Pmt{PROP:Text}='Picture Col ' & X & ':'
                 ENABLE(?Picture) ; SELECT(?Picture) ; PColumn=X ; END
         ?List:VLB{PROPLIST:Left,X}=1
         ?List:VLB{PROPLIST:Center,X}=1
         ?List:VLB{PROPLIST:Right,X}=1
         BEGIN ; ?List:VLB{PROPLIST:width,X}=0 ; IF X=PColumn THEN DISABLE(?Picture). ; END         
       END
    END
    CASE ACCEPTED()
    OF ?MenuBtn
        EXECUTE POPUP('Copy All to Clipboard|-|Contract Column Widths|Expand Column Widths' & |
                        '|-|Copy VLB Format String')
         SETCLIPBOARD(CsvST.GetValue())
         VlbCls.Contrt()
         VlbCls.Expand()
         SETCLIPBOARD(QUOTE(?List:VLB{PROP:Format}))
        END
    OF ?LinesBtn ; SELF.LinesViewInList(CsvST)
    OF ?NoQuotes OROF ?pLeft ; QChanged=1 ; SELECT(?List:VLB,1)
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
BigBangTheory.StringView PROCEDURE(STRING StrValue, <STRING CapTxt>)
  CODE
  SELF.StringView(StrValue,CapTxt)

BigBangTheory.StringView PROCEDURE(*STRING StrValue, <STRING CapTxt>)
St StringTheory
  CODE
  IF SELF.DoNotShow THEN RETURN.
  St.setValue(StrValue)
  SELF.ValueView(St, CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'StringTheory Value'))
!-------------------------------
BigBangTheory.SliceView PROCEDURE(StringTheory pST, Long pStart=1, Long pEnd=0, <STRING CapTxt>) 
SliceSt StringTheory 
Q1 PSTRING(4)  !"?" flags Slice[] out of range in caption, ST will fixup
Q2 PSTRING(4)
SliceCaption PSTRING(48)
  CODE
  IF SELF.DoNotShow THEN RETURN.
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
  IF SELF.DoNotShow THEN RETURN. 
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
HScrollTxt BYTE(1),STATIC
VScrollTxt BYTE(1),STATIC
Window WINDOW('S'),AT(,,310,140),GRAY,SYSTEM,MAX,FONT('Consolas',10),RESIZE
        TOOLBAR,AT(0,0,325),USE(?TB1)
            CHECK('Show HEX'),AT(2,0),USE(ShowHex),TIP('See Value in Hex')
            CHECK('HScroll'),AT(74,0),USE(HScrollTxt)
            CHECK('VScroll'),AT(126,0),USE(VScrollTxt)
            CHECK('No Bang'),AT(196,0),USE(?NoBang),TIP('Do Not show BigBang Views')
        END
        TEXT,AT(0,2),FULL,USE(?Txt),HVSCROLL,READONLY,FLAT
        TEXT,AT(0,2),FULL,USE(?HexTxt),HIDE,HVSCROLL,READONLY,FLAT
    END
P LONG,DIM(4),STATIC
  CODE
  IF SELF.DoNotShow THEN RETURN.
  LenTxt=pSt.length()
  IF ~LenTxt THEN Message('No Text','ValueView') ; RETURN.
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  ?Txt{PROP:HScroll}=HScrollTxt ; ?Txt{PROP:VScroll}=VScrollTxt
  IF LenTxt > 0FFF0h THEN DISABLE(?HScrollTxt,?VScrollTxt). !System Error @ 64k in 11.13505 - Message('Risk GPF?',LenTxt,,'No|Risk')
  ?Txt{PROP:Use}=pSt.valuePtr[1 : LenTxt]
  ?NoBang{PROP:Use}=SELF.DoNotShow
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
  IF SELF.DoNotShow THEN RETURN.
  LenTxt=stOrig.Length()
  IF ~stOrig.clipLength() THEN Message('No Text','WrapView') ; RETURN.
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  ?Txt{PROP:Use}=StOrig.valuePtr[1 : LenTxt]
  ?NoBang{PROP:Use}=SELF.DoNotShow
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
                        '"' & CHOOSE(P,P1,P2,P3,P4,P5,P6,P7,P8,P9) &'"', |
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
                        '"' & CHOOSE(P,P1,P2,P3,P4,P5,P6,P7,P8,P9,'?#' & P) &'"', |
                        '<<omitted>') & |
                '   <9>' & |
                CHOOSE(P<=ProCnt,stProt.GetLine(P),'? Prototype ?') & |
                '<13,10>'
    END
    SP=SP & '<13,10>Prototype: ' & Prototype
    SELF.StringView(SP,'ProtoView - Count: ' & PMax)
    RETURN PMax 