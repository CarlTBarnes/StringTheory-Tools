                    MEMBER()
!----------------------------------------------------------------------------
! BigBangString - View Strings in a Window including Hex
! (c)2021 by Carl T. Barnes - Released under the MIT License
!----------------------------------------------------------------------------
! 23-Apr-2021 First Release
!----------------------------------------------------------------------------
    INCLUDE('KEYCODES.CLW')
    INCLUDE('BigBangString.INC'),ONCE
  MAP.
!=================================================================================
BigBangString.StringView PROCEDURE(STRING StrValue, <STRING CapTxt>)
  CODE
  SELF.StringView(StrValue,CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'String Value'))

BigBangString.StringView PROCEDURE(*STRING StrValue, <STRING CapTxt>)  
LenTxt     LONG,AUTO
HexTxt     &String
ShowHex    BYTE
HScrollTxt BYTE(1),STATIC
VScrollTxt BYTE(1),STATIC
Window WINDOW('S'),AT(,,310,170),GRAY,SYSTEM,MAX,FONT('Consolas',10),RESIZE ,center
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
  LenTxt=SIZE(StrValue) 
  IF ~LenTxt THEN Message('No Text','StringView') ; RETURN.
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  ?Txt{PROP:HScroll}=HScrollTxt ; ?Txt{PROP:VScroll}=VScrollTxt
  IF LenTxt > 0FFF0h THEN DISABLE(?HScrollTxt,?VScrollTxt). !System Error @ 64k in 11.13505 - Message('Risk GPF?',LenTxt,,'No|Risk')
  ?Txt{PROP:Use}=StrValue   !pSt.valuePtr[1 : LenTxt]
  ?NoBang{PROP:Use}=SELF.DoNotShow
  0{PROP:Text}=CHOOSE(~OMITTED(CapTxt) AND CapTxt,CLIP(CapTxt),'String Value') & ' - Length ' & LenTxt
  ACCEPT
    CASE ACCEPTED()
    OF ?HScrollTxt ; ?Txt{PROP:HScroll}=HScrollTxt
    OF ?VScrollTxt ; ?Txt{PROP:VScroll}=VScrollTxt
    OF ?ShowHex
        IF HexTxt &= NULL THEN 
           HexTxt &= SELF.HexDump(StrValue) 
           ?HexTxt{PROP:Use}=HexTxt
        END
        ?Txt{PROP:Hide}=ShowHex ; ?HexTxt{PROP:Hide}=1-ShowHex
    END
  END
  GETPOSITION(0,P[1],P[2],P[3],P[4]) 
  CLOSE(Window)
  DISPOSE(HexTxt) 
  RETURN

!-------------------------------
BigBangString.SliceView PROCEDURE(*String StrToViewSlice, Long pStart, Long pEnd, <STRING CapTxt>) 
SliceStr &STRING
BadString STRING(255),AUTO 
Q1 PSTRING(4)  !"?" flags Slice[] out of range in caption, ST will fixup
Q2 PSTRING(4)
SliceCaption PSTRING(48)
_DataEnd LONG,AUTO
  CODE
  IF SELF.DoNotShow THEN RETURN.
  _DataEnd = SIZE(StrToViewSlice)
  IF pStart < 1 OR pStart > _DataEnd OR (pStart > pEnd) THEN Q1=' ? '.
  IF pEnd < 1   OR pEnd   > _DataEnd OR  pStart > pEnd  THEN Q2=' ? '.
  SliceCaption = 'Slice ['& Q1 & pStart &':'& pEnd & Q2 &'] of [1:'& _DataEnd &']'
  IF _DataEnd < 1 OR Q1 OR Q2 THEN
     SliceStr &= BadString
     BadString='Invalid Slice? ' & SliceCaption 
  ELSE
     SliceStr &= StrToViewSlice[pStart : pEnd]   
  END 
  SELF.StringView(SliceStr,SliceCaption &' - '& CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'SliceView')) 
  RETURN

!-------------------------------
BigBangString.SubView PROCEDURE(*String StrToViewSUB, Long pStart, Long pLength, <STRING CapTxt>) 
Q1 PSTRING(4)  !"?" flags Slice[] out of range in caption, ST will fixup
Q2 PSTRING(4)
SubCaption PSTRING(48)
_DataEnd LONG,AUTO
  CODE
  IF SELF.DoNotShow THEN RETURN.
  _DataEnd = SIZE(StrToViewSUB)
  IF pStart < 1  OR pStart > _DataEnd THEN Q1=' ? '.
  IF pLength < 1 OR pLength-1+pStart > _DataEnd  THEN Q2=' ? '.
  SubCaption = 'Sub('& Q1 & pStart &','& pLength & Q2 &') of (1,'& _DataEnd &')'
  SELF.StringView(SUB(StrToViewSUB,pStart,pLength),SubCaption &' - '& CHOOSE(~OMITTED(CapTxt) AND CapTxt,CapTxt,'SubView')) 
  RETURN
!-------------------------------
BigBangString.HexDump  PROCEDURE(*STRING pStr)!,*STRING  
Dump &STRING
DmpX LONG,AUTO     
Lin Group,PRE()
      String('<13,10>')
Off   String('Offset ') 
Hex   String(' 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16  ') 
AHex    String(3),DIM(16),OVER(Hex) 
Chr   String('0123456789abcdef')
    End
SizeLin BYTE,AUTO    
SizeStr LONG,AUTO    
VC BYTE,AUTO
p LONG,AUTO     
c BYTE,AUTO
XDigits STRING('0123456789ABCDEF')
    CODE 
    SizeStr=SIZE(pStr)
    SizeLin=SIZE(Lin) 
    Dump &=NEW(STRING((2+int(SizeStr / 16))*SizeLin ))
    Dump[ 1: SizeLin]=SUB(Lin,3,99) ; DmpX = SizeLin - 1 
    LOOP P = 0 TO SizeStr-1 BY 16
       Off=P  
       Hex='' ; Chr='' 
       LOOP c = 1 TO 16
         IF p+c > SizeStr THEN BREAK.
         VC=VAL(pStr[p+c])
         IF VC >= 32 THEN Chr[c]=CHR(VC) ELSE Chr[c] = '.'.  
         AHex[c,1]=XDigits[BSHIFT(VC, -4) + 1] 
         AHex[c,2]=XDigits[BAND(VC, 0FH) + 1]
       END
       Dump[DmpX : DmpX+SizeLin-1]=Lin
       DmpX += SizeLin 
    END
    RETURN Dump 
!-------------------------------
BigBangString.ParmsView PROCEDURE(String P1,<String P2>,<String P3>,<String P4>,<String P5>,<String P6>,<String P7>,<String P8>,<String P9>)!,Long,Proc
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
BigBangString.ProtoView PROCEDURE(String Prototype, String P1,<String P2>,<String P3>,<String P4>,<String P5>,<String P6>,<String P7>,<String P8>,<String P9>)
P    USHORT,AUTO
PMax USHORT,AUTO
SP   ANY
ProtQ QUEUE,PRE(ProQ)
AParm    STRING(128) !ProQ:AParm
      END 
ProCnt USHORT,AUTO
    CODE
    SP=Prototype   !ST was--> stProt.SetValue(Prototype) ; stProt.Split(',')   !Problem Arrays: *LONG[,] Current
    LOOP 
        P=INSTRING(',',SP) ; IF ~P THEN BREAK.
        ProQ:AParm=LEFT(SUB(SP,1,P-1))       
        ADD(ProtQ)
        SP=LEFT(SUB(SP,P+1,999))       
    END         
    ProQ:AParm=SP ; SP=''
    ADD(ProtQ)
    ProCnt=Records(ProtQ) 
    PMax=ProCnt
    LOOP P=1 TO 9 ; IF P>PMax AND ~OMITTED(2+P) THEN PMax=P. ; END 
    LOOP P=1 TO PMax
        GET(ProtQ,P)
        SP=SP & 'Parm ' & P &' => ' & |
                CHOOSE(~OMITTED(2+P), | 
                        '"' & QUOTE(CHOOSE(P,P1,P2,P3,P4,P5,P6,P7,P8,P9,'?#' & P)) &'"', |
                        '<<omitted>') & |
                '   <9>' & |
                CHOOSE(P<=ProCnt,ProQ:AParm,'? Prototype ?') & |
                '<13,10>'
    END
    SP=SP & '<13,10>Prototype: ' & Prototype
    SELF.StringView(SP,'ProtoView - Count: ' & PMax)
    RETURN PMax 