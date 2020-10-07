! ScratchTheory - Scratch program to write and test StringTheory code
!                 The fastest way to work out code can be a small test program so you can compile and test quickly
!
!Defines: StringTheoryLinkMode=>1;StringTheoryDllMode=>0;_ABCLinkMode_=>1;_ABCDllMode_=>0

  PROGRAM  
    INCLUDE 'TplEqu.CLW'
    INCLUDE 'KeyCodes.CLW'
    INCLUDE('StringTheory.inc'),ONCE
    INCLUDE('BigBangTheory.INC'),ONCE
    MAP
PickProcedure   PROCEDURE()
!Prototype1      PROCEDURE(STRING ProcName, STRING ProcParms, STRING pRV)
Prototype1      PROCEDURE(STRING MethodQ_Record, STRING MethodQ_Pointer)
Upper1          PROCEDURE(*STRING InOutStr)
ShellExecuteOpen PROCEDURE(STRING File2Do)
DB          PROCEDURE(STRING DebugMessage)   !Output Debug String
DBClear     PROCEDURE()                      !Clear DebugView Buffer
      MODULE('api')
        OutputDebugString(*CSTRING cMsg),PASCAL,DLL(1),RAW,NAME('OutputDebugStringA')
        DebugBreak(),PASCAL,DLL(1) 
        GetLastError(),LONG,PASCAL,DLL(1)
        ShellExecute(UNSIGNED,LONG,LONG,LONG,LONG,SIGNED),UNSIGNED,PASCAL,DLL(1),PROC,NAME('ShellExecuteA') 
      END
    END
   
ConfigINI   EQUATE('.\WriteTheory.INI') 
MethodQ  QUEUE,PRE(MethQ)
LineNo          LONG            !MethQ:LineNo
Name            STRING(40)      !MethQ:Name 
RV              STRING(40)      !MethQ:RV 
Parms           STRING(1000)    !MethQ:Parms
ParmsTip        STRING(1000)    !MethQ:ParmsTip 
           END 
CBLocateCls CLASS,TYPE
Init          PROCEDURE(QUEUE QRef, *STRING QStrRef, LONG ListFEQ, LONG FindTextFEQ, LONG BtnNextFEQ, LONG BtnPrevFEQ, BYTE Hack=0)
Kill          PROCEDURE()
DisableIfNone PROCEDURE()
Locate        PROCEDURE(SHORT NextPrev=1, BOOL IsButton=0)  !1=Next -1=Prev
TakeAccepted  PROCEDURE(),BYTE,PROC,PROTECTED
TakeAlertKey  PROCEDURE(),BYTE,PROC,PROTECTED
IsInit  BYTE
QRef    &QUEUE 
QString &STRING 
ListFEQ LONG
TextFEQ LONG
NextBtn LONG
PrevBtn LONG
            END           
    CODE
    PickProcedure()
    RETURN
!===========================================================================
PickProcedure   PROCEDURE
StIncFile   STRING(260)
X      LONG
Txt    STRING(4000)
FindName STRING(32)           
FindInParm BYTE
Window WINDOW('Write Theory - The Comma Killer'),AT(,,400,200),CENTER,GRAY,IMM,SYSTEM,ICON(ICON:Thumbnail),FONT('Segoe UI',10), |
            RESIZE
        BUTTON('Load'),AT(4,2,30,13),USE(?LoadIncBtn)
        BUTTON('...'),AT(37,2,15,13),USE(?PickIncBtn)
        ENTRY(@s255),AT(60,3,331,12),USE(StIncFile),SKIP
        ENTRY(@s32),AT(3,18,94,10),USE(FindName),SKIP,FONT('Consolas',9,,FONT:regular)
        BUTTON('&Find'),AT(102,17,23,11),USE(?FindNameNext),SKIP
        BUTTON('Pre&v'),AT(127,17,23,11),USE(?FindNamePrev),SKIP
        CHECK('Find in &Prototype'),AT(157,17,,11),USE(FindInParm),SKIP,TIP('Locate in Prototype, uncheck to for Name')
        LIST,AT(3,31),FULL,USE(?List:MethodQ),VSCROLL,FROM(MethodQ),FORMAT('24R(2)|M~Line#~C(0)@n5@70L(2)|M~Procedure~@s' & |
                '40@?30L(2)|M~Return~C(0)@s40@20L(2)|MP~Prototype~@s255@')
    END
LocateCls  CBLocateCls
    CODE
    SYSTEM{PROP:PropVScroll}=1 ; SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY
    StIncFile='StringTheory.Inc' 
    StIncFile=GETINI('Setup','st.inc','StringTheory.Inc',ConfigINI)     
    OPEN(WINDOW)
    LocateCls.Init(MethodQ,MethodQ.Name,?List:MethodQ,?FindName,?FindNameNext,?FindNamePrev)
    IF EXISTS(StIncFile) THEN DO LoadIncRtn.
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow 
        END
        CASE ACCEPTED()
        OF ?LoadIncBtn  ; DO LoadIncRtn
                          IF EXISTS(StIncFile) THEN
                             PUTINI('Setup','st.inc',StIncFile,ConfigINI)     
                          END
        OF ?PickIncBtn  ; DO PickIncRtn
        OF ?FindInParm  ; IF FindInParm THEN 
                             LocateCls.QString &= MethodQ.Parms
                          ELSE
                             LocateCls.QString &= MethodQ.Name
                          END
        END
        CASE FIELD() 
        OF ?List:MethodQ
           GET(MethodQ,CHOICE(?List:MethodQ))
           CASE EVENT()
           !OF EVENT:AlertKey ; POST(EVENT:NewSelection,?)
           OF EVENT:NewSelection  
              CASE KEYCODE()
              OF MouseLeft2 OROF EnterKey 
                 START(Prototype1,,MethodQ, POINTER(MethodQ))
              END
           END
        END
    END
    CLOSE(WINDOW)
!========================================================================
LoadIncRtn ROUTINE  !------------------------------------------------------
    DATA
Bang BigBangTheory
ST   StringTheory
lfST StringTheory
pmz  StringTheory 
ALine   STRING(1024)
ULine   STRING(1024)
St_InClass    BOOL
St_Label   EQUATE('STRINGTHEORY')
Spc1 LONG
Paren2 LONG
ProcedureLit    EQUATE('PROCEDURE') !Plus Space or Comma (
FunctionLit     EQUATE('FUNCTION') 
    CODE
    FREE(MethodQ)
    lfST.LoadFile(StIncFile)
    lfST.split('<13,10>')   ! ; Bang.LinesViewInList(st)
    LOOP X=1 TO lfST.Records() 
         ALine=lfST.GetLine(X) 
         IF ~ALine THEN CYCLE.  !Blank        
         ULine=UPPER(ALine)
!TODO grab Equates into Queue to use         
!TODO change to not be string theory specific
         !Look for: StringTheory        Class(),
         IF ~St_InClass THEN 
             IF ~ULine[1] THEN CYCLE. 
             IF ~MATCH(ULine,'^' & St_Label &' +'& 'CLASS[ ,(]',Match:Regular) THEN CYCLE.
             St_InClass=True 
              ! Message('Line ' & X & '|St_InClass=1   St_Label=' & St_Label & '||' &  ALine )
             cycle
         END

         IF ~ALine[1] THEN  !Not a Label if Char 1 blank 
             IF SUB(LEFT(ULine),1,4) = 'END ' THEN 
                St_InClass=False
             END
             CYCLE
         END 
         IF ULine[1] < 'A' OR ULine[1] > 'Z' THEN CYCLE.
         
         !MethodName        Procedure (long pAddr, long pLen),virtual 

         IF ~MATCH(ULine,'^[^ ]+ +'& '{{PROCEDURE|FUNCTION}[ ,(]',Match:Regular) THEN CYCLE.
 
         Spc1=INSTRING(' ',ALine)    !Find 1st space so can 
         ALine=SUB(ALine,1,Spc1) & LEFT(SUB(ALine,Spc1+1,9999))  !CatAddr Procedure
         ULine=UPPER(ALine)
         
         CLEAR(MethodQ)
         MethQ:Name=SUB(ALine,1,Spc1)
         MethQ:Name[1]=UPPER(MethQ:Name[1])
         MethQ:LineNo = X

         !Cutoff front "Label Procedure"
         IF SUB(Uline,Spc1+1,SIZE(ProcedureLit)) = ProcedureLit THEN 
            ALine=LEFT( SUB(Aline,Spc1+1+SIZE(ProcedureLit),999) )  
         ELSIF SUB(Uline,Spc1+1,SIZE(FunctionLit)) = FunctionLit THEN 
            ALine=LEFT( SUB(Aline,Spc1+1+SIZE(FunctionLit),999) )  
         ELSE   !Failed
            STOP('Did not find Proc or Fct<13,10>'&  SUB(ALine,Spc1+1,999 ) )
            !MethQ:Parms = ALine            
         END

         Paren2=INSTRING(')',ALine)    !(),ReturnValue  
!TODO can have  Procedure,ReturnValue  I think
         IF Paren2 THEN 
            MethQ:RV=LEFT(SUB(ALine,Paren2+1,999))
            IF MethQ:RV[1]=',' THEN MethQ:RV=LEFT(SUB(MethQ:RV,2,99)). 
            st.SetValue(MethQ:RV,1)
            st.Replace('virtual', '',,,, st:nocase,)
            st.Replace(',', ' ')
            st.squeeze(ST:NOPUNCTUATION)

            MethQ:RV=LEFT(st.GetValue()) 
            Upper1(MethQ:RV)
            ALine=SUB(ALine,1,Paren2-1)
         END
         IF ALine[1]='(' OR ALine[1]=',' THEN
            ALine=LEFT(SUB(ALine,2,999))
         END 
         Upper1(ALine)
         MethQ:Parms = ALine
         !MethQ:ParmsTip = ALine 
         st.SetValue(lfST.GetLine(X)) 
         st.squeeze(ST:NOPUNCTUATION)
         MethQ:ParmsTip=st.GetValue()
         ADD(MethodQ) 
         
!         pmz.SetValue(MethQ:Parms)
!         pmz.Split(',')
!         IF pmz.Records() > M# THEN M#=pmz.Records() .
!         IF pmz.Records() > 9 THEN Message('Over 9 ' & st.GetLine(X) ).
        
    END 
    SORT(MethodQ,MethQ:Name, MethQ:LineNo)
!    MethQ:Name='Replace'
!    GET(MethodQ,MethQ:Name) 
!    IF ~ERRORCODE() THEN SELECT(?List:MethodQ,POINTER(MethodQ)).
    DISPLAY
    !Message('Max Parm Count M#=' &  M# ) 
    
PickIncRtn ROUTINE  !------------------------------------------------------ 
    IF ~FILEDIALOG('Select StringTheory.Inc', StIncFile, |
                   'Include files (*.inc)|*.inc|CLarion Source|*.clw;*.inc|All Files|*.*', |
                   FILE:KeepDir+FILE:LongName) THEN 
        EXIT
    END
    DISPLAY
    DO LoadIncRtn
    EXIT
Test2Rtn ROUTINE  !------------------------------------------------------
    DATA
Bang BigBangTheory
ST   StringTheory
Lne  StringTheory    
    CODE
    
!========================================================================================
Upper1 PROCEDURE(*STRING Str) 
    CODE        
    IF Str[1]>='A' THEN       !string  
       Str[1]=UPPER(Str[1]) 
    ELSIF Str[2]>='A' THEN    !<sring     
       Str[2]=UPPER(Str[2]) 
    ELSE 
       Str[3]=UPPER(Str[3])   !<*string
    END
    RETURN
!======================================================================================== 
ShellExecuteOpen PROCEDURE(STRING File2Do) 
ShellCmnd           CSTRING(1000),AUTO
ShRetErr            LONG,AUTO
lpOperation     cstring('open')    !only one operation support so far
  CODE                                                     ! Begin processed code
    ShellCmnd = CLIP(File2Do)
    SETCURSOR(CURSOR:Wait)
    ShRetErr = ShellExecute(0, lpOperation, ADDRESS(ShellCmnd),0,0,5)  !5=SW_SHOW
    SETCURSOR
    RETURN
!========================================================================================
DB   PROCEDURE(STRING xMessage)
Prfx EQUATE('ScratchST: ')
sz   CSTRING(SIZE(Prfx)+SIZE(xMessage)+1),AUTO
  CODE 
  sz  = Prfx & CLIP(xMessage)
  OutputDebugString( sz )
!------------------
DBClear PROCEDURE()
DbgClear CSTRING('DBGVIEWCLEAR')
    CODE 
    OutputDebugString(DbgClear)
!========================================================================================
Prototype1 PROCEDURE(STRING pMethodQ_Record, STRING pMethodQ_Pointer)
MethodGrp GROUP(MethodQ),PRE(MethGrp),AUTO
          END
MethodQPtr  LONG 
ProcName    PSTRING(64)
MaxCnt          EQUATE(9)
ParmGrp GROUP,PRE() 
Parm        STRING(40),DIM(9)   !8 is maximum 
        END
Proto   STRING(40),DIM(9)   !8 is maximum
SaveQ   QUEUE,PRE(SaveQ),STATIC 
MethQPtr   LONG                  !SaveQ:MethQPtr =MethodQPtr
Parms      STRING(SIZE(ParmGrp)) !SaveQ:Parms    =ParmGrp
        END 
PCount  USHORT 
P  USHORT 
X  LONG 
Y  LONG 
CallTxt STRING(2000)

Window WINDOW('Protype:'),AT(,,329,195),GRAY,SYSTEM,ICON(ICON:JumpPage),FONT('Segoe UI',9)
        PROMPT('Parameter to Pass:'),AT(17,5),USE(?Heading1)
        PROMPT(' Prototype:'),AT(191,5),USE(?Heading2)
        BUTTON('Close'),AT(254,3,33,12),USE(?CloseBtn),SKIP,STD(STD:Close)
        BUTTON('Help'),AT(292,3,29,12),USE(?wwwBtn),SKIP,TIP('Open Capesoft.com')
        ENTRY(@s40),AT(19,18,164,11),USE(Parm[1]),FONT('Consolas')
        ENTRY(@s40),AT(19,32,164,11),USE(Parm[2])
        ENTRY(@s40),AT(19,46,164,11),USE(Parm[3])
        ENTRY(@s40),AT(19,60,164,11),USE(Parm[4])
        ENTRY(@s40),AT(19,74,164,11),USE(Parm[5])
        ENTRY(@s40),AT(19,88,164,11),USE(Parm[6])
        ENTRY(@s40),AT(19,102,164,11),USE(Parm[7])
        ENTRY(@s40),AT(19,116,164,11),USE(Parm[8])
        ENTRY(@s40),AT(19,130,164,11),USE(Parm[9])
        ENTRY(@s40),AT(192,18,128,11),USE(Proto[1]),SKIP,TRN,FONT('Consolas')
        ENTRY(@s40),AT(192,32,128,11),USE(Proto[2]),SKIP,TRN
        ENTRY(@s40),AT(192,46,128,11),USE(Proto[3]),SKIP,TRN
        ENTRY(@s40),AT(192,60,128,11),USE(Proto[4]),SKIP,TRN
        ENTRY(@s40),AT(192,74,128,11),USE(Proto[5]),SKIP,TRN
        ENTRY(@s40),AT(192,88,128,11),USE(Proto[6]),SKIP,TRN
        ENTRY(@s40),AT(192,102,128,11),USE(Proto[7]),SKIP,TRN
        ENTRY(@s40),AT(192,116,128,11),USE(Proto[8]),SKIP,TRN
        ENTRY(@s40),AT(192,130,128,11),USE(Proto[9]),SKIP,TRN
        PROMPT('1.'),AT(6,18),USE(?PROMPTNo1)
        PROMPT('2.'),AT(6,32),USE(?PROMPTNo2)
        PROMPT('3.'),AT(6,46),USE(?PROMPTNo3)
        PROMPT('4.'),AT(6,60),USE(?PROMPTNo4)
        PROMPT('5.'),AT(6,74),USE(?PROMPTNo5)
        PROMPT('6.'),AT(6,88),USE(?PROMPTNo6)
        PROMPT('7.'),AT(6,102),USE(?PROMPTNo7)
        PROMPT('8.'),AT(6,116),USE(?PROMPTNo8)
        PROMPT('9.'),AT(6,130),USE(?PROMPTNo9)
        BUTTON,AT(1,152,15,15),USE(?CopyBtn),SKIP,ICON(ICON:Copy),TIP('Copy Code'),FLAT
        TEXT,AT(19,148,301,39),USE(CallTxt),SKIP,VSCROLL,FONT('Consolas')
    END
Bang BigBangTheory
ST   StringTheory
    CODE
    MethodGrp = pMethodQ_Record 
    ProcName  = CLIP(MethGrp:Name) 
    MethodQPtr= pMethodQ_Pointer 
    SaveQ:MethQPtr =MethodQPtr
    GET(SaveQ,SaveQ:MethQPtr)
    IF ~ERRORCODE() THEN ParmGrp=SaveQ:Parms.

    OPEN(Window)
    0{PROP:Text}='Prototype: ' & ProcName & ' - Returns: ' & MethGrp:RV 
    ?Heading1{PROP:Text}='Parameters for ' & ProcName &'()'
    DO WinOpenRtn
    ACCEPT
        IF EVENT()=EVENT:Accepted THEN DO CallTxtRtn.
        CASE ACCEPTED()
        OF ?CopyBtn ; SETCLIPBOARD(CallTxt)
        OF ?wwwBtn  ; ShellExecuteOpen('https://www.capesoft.com/docs/StringTheory3/StringTheory.htm#st' & CLIP(ProcName))
        END
    END
    IF ParmGrp THEN 
       CLEAR(SaveQ)
       SaveQ:MethQPtr =MethodQPtr
       GET(SaveQ,SaveQ:MethQPtr)
       SaveQ:Parms=ParmGrp  
       IF ERRORCODE() THEN ADD(SaveQ,SaveQ:MethQPtr) ELSE PUT(SaveQ).
    END
    RETURN

WinOpenRtn ROUTINE
    st.SetValue(MethGrp:Parms)  ! ; bang.ValueView(st)
    st.Split(',',,,1,1)         ! ; bang.LinesViewInList(st)
    PCount=st.Records() 
    IF ~MethGrp:Parms THEN PCount=0.
    IF PCount > MaxCnt THEN   
       Message(MaxCnt & ' Parameters Max, this has ' & PCount & ' parameters||' & MethGrp:Parms,'Prototype1')
       PCount=MaxCnt
    END
    LOOP P=1 TO PCount
        Proto[P]=LEFT(st.GetLine(P)) 
        Upper1(Proto[P])
        Proto[P]=P & '. '& Proto[P]
        (?Parm_1  +P-1){PROP:Tip}=Proto[P]
        (?Parm_1  +P-1){PROP:FontName}='Consolas'
        (?Proto_1 +P-1){PROP:FontName}='Consolas'
    END
    LOOP P=PCount+1 TO MaxCnt
         HIDE(?Parm_1 +P-1)
         HIDE(?Proto_1 +P-1)
         HIDE(?PROMPTNo1 +P-1) 
         IF P=PCount+1 THEN 
            ?CallTxt{PROP:YPos} = (?Parm_1 +P-2){PROP:YPos} + 18
            ?CopyBtn{PROP:YPos} = ?CallTxt{PROP:YPos} + 10
            0{PROP:Height} = ?CallTxt{PROP:YPos} + ?CallTxt{PROP:Height} + 8
         END
    END
!    IF PCount=0 THEN DO CallTxtRtn.
    DO CallTxtRtn
    
CallTxtRtn ROUTINE
    CallTxt='.' & ProcName &'('
    LOOP P=1 TO PCount
        CallTxt=CLIP(CallTxt) & CHOOSE(P=1,'',', ')& CLIP(Parm[P]) 
    END
    CallTxt=CLIP(CallTxt) & ')<13,10>!('& CLIP(MethGrp:Parms) &') ' & MethGrp:RV
    DISPLAY   
!=========
!==========================================================

CBLocateCls.Init  PROCEDURE(QUEUE QRef, *STRING QStrRef, LONG ListFEQ, LONG TextFEQ, LONG NextBtn, LONG PrevBtn, BYTE Hack=0)
  CODE
  SELF.IsInit=1 
  SELF.QString  &= QStrRef
  SELF.QRef   &= QRef    ; SELF.NextBtn = NextBtn
  SELF.ListFEQ = ListFEQ ; SELF.PrevBtn = PrevBtn
  SELF.TextFEQ = TextFEQ ; IF Hack THEN RETURN.
  TextFEQ{PROP:Key}=CtrlF ; TextFEQ{PROP:Alrt,255}=EnterKey
  TextFEQ{PROP:Tip}=CHOOSE(~TextFEQ{PROP:Tip},'Text to Locate, (Ctrl F) to Select.',TextFEQ{PROP:Tip}) & |
                      '<13,10>Prefix with % for Regular Expression'
  REGISTER(EVENT:AlertKey,ADDRESS(SELF.TakeAlertKey),ADDRESS(SELF),,TextFEQ)
  REGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,TextFEQ)
  REGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,NextBtn)
  IF ~NextBtn{PROP:Tip} THEN NextBtn{PROP:Tip}='Find Next'.
  IF PrevBtn THEN
     IF ~PrevBtn{PROP:Tip} THEN PrevBtn{PROP:Tip}='Find Previous'.
     REGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,PrevBtn)
  END   
  RETURN
CBLocateCls.Kill  PROCEDURE()
  CODE
  IF ~SELF.IsInit THEN RETURN.
  IF SELF.TextFEQ THEN 
     UnREGISTER(EVENT:AlertKey,ADDRESS(SELF.TakeAlertKey),ADDRESS(SELF),,SELF.TextFEQ)
     UnREGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,SELF.TextFEQ)
  END 
  IF SELF.NextBtn THEN UnREGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,SELF.NextBtn).
  IF SELF.PrevBtn THEN UnREGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,SELF.PrevBtn).
  SELF.IsInit=0 ; SELF.TextFEQ=0 ; SELF.NextBtn=0 ; SELF.PrevBtn=0
  RETURN
CBLocateCls.TakeAlertKey PROCEDURE()
  CODE
  IF FIELD()=SELF.TextFEQ AND KEYCODE()=EnterKey THEN
     UPDATE(SELF.TextFEQ) ; SELF.Locate(1)
  END
  RETURN 0
CBLocateCls.TakeAccepted PROCEDURE()
  CODE
  UPDATE !SKIP on ENTRY no Update for press Alt+F
  CASE FIELD()
  OF 0     !Not OF SELF.TextFEQ ; SELF.Locate()  else AltF does 2x
  OF SELF.NextBtn ;  SELF.Locate(1,1)
  OF SELF.PrevBtn ;  SELF.Locate(-1,1)
  END
  RETURN 0
CBLocateCls.Locate PROCEDURE(SHORT NextPrev=1, BOOL IsButton=0)
Txt   PSTRING(65),AUTO
Ndx   LONG,AUTO
RegEx BYTE
    CODE
  Txt=CLIP(lower(LEFT(CONTENTS(SELF.TextFEQ)))) 
  IF ~Txt OR ~SELF.IsInit THEN
      IF IsButton THEN SELECT(SELF.TextFEQ). ; RETURN
  END    
  IF Txt[1]='%' AND LEN(Txt)>1 THEN RegEx=1 ; Txt=Txt[2 : LEN(Txt)] .
  Ndx = CHOICE(SELF.ListFEQ) ; IF ~NextPrev THEN NextPrev=1.
  IF -1=NextPrev AND Ndx<2 THEN Ndx=RECORDS(SELF.QRef)+1.
  LOOP
     Ndx += NextPrev
     GET(SELF.QRef, Ndx) ; IF ERRORCODE() THEN BREAK.
     IF (~RegEx AND INSTRING(Txt,lower(SELF.QString),1,1)) |
     OR (RegEx  AND MATCH(lower(SELF.QString),Txt,MATCH:Regular)) THEN
        SELECT(SELF.ListFEQ, Ndx)
        BREAK
     END
  END
  RETURN 
CBLocateCls.DisableIfNone PROCEDURE()
D STRING(1)
  CODE
  IF RECORDS(SELF.QRef)<2 THEN D='1'.
  SELF.TextFEQ{PROP:Disable}=D ; SELF.NextBtn{PROP:Disable}=D
  IF SELF.PrevBtn THEN SELF.PrevBtn{PROP:Disable}=D.
  DISPLAY  
  RETURN
!########################      