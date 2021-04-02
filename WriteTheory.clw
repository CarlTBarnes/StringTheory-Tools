! WriteTheory - Write StringTheory calls and get the parameters right
!    Many methods have a lot of parameters, here are the most with 6,7 and 8
!    6=Between 6=FindBetween 6=FindBetweenPosition 6=FindWord 6=Instring 6=SerializeGroup 6=Sort 
!    7=Count 7=InLine 7=Replace 7=SerializeQueue     8=Split
!
!Defines: StringTheoryLinkMode=>1;StringTheoryDllMode=>0;_ABCLinkMode_=>1;_ABCDllMode_=>0
!
!  TODO
!10/26/20   Support Odd Job     
!10/26/20   Store Class in Queue so can do multiple classes
!10/27/20   Sort by Name or Line# Original Order
!           Line continuation
!10/26/20   Limit of 9 needs to be 10 for OddJob 
!03/04/21   Position Prototype Window over INC window or to last moved
!03/21/21   Correct ShellExecute parms
!04/02/21   Add Help button to List INC Window

  PROGRAM  
    INCLUDE 'TplEqu.CLW'
    INCLUDE 'KeyCodes.CLW'
    INCLUDE('StringTheory.inc'),ONCE
    INCLUDE('BigBangTheory.INC'),ONCE
    MAP
PickProcedure   PROCEDURE()
Prototype1      PROCEDURE(STRING MethodQ_Record, STRING MethodQ_ID)
Upper1          PROCEDURE(*STRING InOutStr)
ShellExecuteOpen PROCEDURE(STRING File2Do)
DB          PROCEDURE(STRING DebugMessage)   !Output Debug String
DBClear     PROCEDURE()                      !Clear DebugView Buffer
      MODULE('api')
        OutputDebugString(*CSTRING cMsg),PASCAL,DLL(1),RAW,NAME('OutputDebugStringA')
        DebugBreak(),PASCAL,DLL(1) 
        GetLastError(),LONG,PASCAL,DLL(1)
        ShellExecute(LONG,<*CSTRING>,*CSTRING,LONG,LONG,LONG),LONG,RAW,PASCAL,DLL(1),PROC,NAME('ShellExecuteA') 
      END
    END
   
ConfigINI   EQUATE('.\WriteTheory.INI') 
MethodQ  QUEUE,PRE(MethQ)
LineNo          LONG            !MethQ:LineNo
ClassSpec       STRING(64)      !MethQ:ClassSpec => Name (BaseClass)
ClassTip        STRING(64)      !MethQ:ClassTip
Name            STRING(40)      !MethQ:Name 
RV              STRING(40)      !MethQ:RV 
Parms           STRING(1000)    !MethQ:Parms
ParmsTip        STRING(1000)    !MethQ:ParmsTip
ClassIndex      BYTE            !MethQ:ClassIndex
ClassName       STRING(40)      !MethQ:ClassName    UPPER  
ID              LONG            !MethQ:ID           Unique ID
           END
G:MethodID LONG
G:Pz LONG,DIM(4)

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
ConsolasFont BYTE
Window WINDOW('Write Theory - The Comma Killer'),AT(,,400,200),CENTER,GRAY,IMM,SYSTEM,MAX, |
            ICON(ICON:Thumbnail),FONT('Segoe UI',10),RESIZE
        BUTTON('&Load'),AT(4,2,30,13),USE(?LoadIncBtn)
        BUTTON('...'),AT(37,2,15,13),USE(?PickIncBtn)
        ENTRY(@s255),AT(60,3,331,12),USE(StIncFile),SKIP
        ENTRY(@s32),AT(3,18,109,10),USE(FindName),SKIP,FONT('Consolas',9,,FONT:regular)
        BUTTON('&Find'),AT(117,17,23,11),USE(?FindNameNext),SKIP
        BUTTON('Pre&v'),AT(142,17,23,11),USE(?FindNamePrev),SKIP
        CHECK('Find in &Prototype'),AT(172,17,,11),USE(FindInParm),SKIP,TIP('Locate in Prototype, un' & |
                'check to for Name')
        BUTTON('&Sort'),AT(262,17,29,11),USE(?SortBtn),SKIP
        BUTTON('&Help'),AT(300,17,29,11),USE(?HelpMainBtn),SKIP,TIP('Open Capesoft Help')
        CHECK('Consolas'),AT(342,17,,11),USE(ConsolasFont),SKIP,FONT('Consolas')
        LIST,AT(3,31),FULL,USE(?List:MethodQ),VSCROLL,FROM(MethodQ),FORMAT('24R(2)|M~Line#~C(0)@n5@4' & |
                '1L(2)|MP~Class~@s64@?70L(2)|M~Procedure~@s40@?30L(2)|M~Return~C(0)@s40@20L(2)|MP~Pr' & |
                'ototype~@s255@')
    END
LocateCls  CBLocateCls
    CODE
    SYSTEM{PROP:PropVScroll}=1 ; SYSTEM{7A7Dh}=MSGMODE:CANCOPY    !C11: 7A7Dh=PROP:MsgModeDefault 
    StIncFile='StringTheory.Inc'  
    StIncFile=GETINI('Setup','st.inc',StIncFile,ConfigINI) 
    OPEN(WINDOW)
    LocateCls.Init(MethodQ,MethodQ.Name,?List:MethodQ,?FindName,?FindNameNext,?FindNamePrev)
    IF EXISTS(StIncFile) THEN DO LoadIncRtn.
    ACCEPT
        CASE EVENT()
        OF   EVENT:OpenWindow 
        OROF EVENT:Moved ; GETPOSITION(0,G:Pz[1],G:Pz[2],G:Pz[3],G:Pz[4]) 
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
        OF ?ConsolasFont ; ?List:MethodQ{PROP:FontName}=CHOOSE(~ConsolasFont,'Segoe UI','Consolas')
        OF ?SortBtn
                    GET(MethodQ,CHOICE(?List:MethodQ))
                    CASE POPUP('Procedure Name|Line Number|Class Name')
                    OF 1 ; SORT(MethodQ,MethQ:ClassIndex, MethQ:Name, MethQ:LineNo)
                    OF 2 ; SORT(MethodQ,MethQ:LineNo)
                    OF 3 ; SORT(MethodQ,MethQ:ClassName, MethQ:Name, MethQ:LineNo)
                    ELSE ; CYCLE
                    END
                    GET(MethodQ,MethQ:ID)
                    SELECT(?List:MethodQ,POINTER(MethodQ))
        OF ?HelpMainBtn    ; DO HelpMainRtn
        END
        CASE FIELD() 
        OF ?List:MethodQ
           GET(MethodQ,CHOICE(?List:MethodQ))
           CASE EVENT()
           !OF EVENT:AlertKey ; POST(EVENT:NewSelection,?)
           OF EVENT:NewSelection  
              CASE KEYCODE()
              OF MouseLeft2 OROF EnterKey 
                 START(Prototype1,,MethodQ, MethQ:Id)
              END
           END
        END
    END
    CLOSE(WINDOW)
!========================================================================
LoadIncRtn ROUTINE  !------------------------------------------------------
    DATA
Bang BigBangTheory
ST   StringTheory   !Little bits of code
lfST StringTheory 
ALine   STRING(1024)
ULine   STRING(1024)
Code1   STRING(1024)
Spc1   LONG
Ins1   LONG
Paren2 LONG 
ClassSpec       STRING(64)
ClassName       STRING(64)
ClassIndex      BYTE
ProcedureLit    EQUATE('PROCEDURE') !Plus Space or Comma (
FunctionLit     EQUATE('FUNCTION')  
CntParms    EQUATE(0)  !8 is max, several with 7 and 6 parms
PmzCntList  ANY 
PmzST       StringTheory
    CODE
    FREE(MethodQ)
    lfST.LoadFile(StIncFile)
    lfST.split('<13,10>')   ! ; Bang.LinesViewInList(st) 
!TODO Line Continuation - Loop in Reverse and if line ends with "|" add to previous    
    LOOP X=1 TO lfST.Records() 
         ALine=lfST.GetLine(X) 
         CASE ALine[1]         !Only lines with Label in column 1
         OF 'A' TO 'Z'         !so no need [A-Z_] in Regex
         OF 'a' TO 'z'
         OF '_'
         ELSE
            CYCLE
         END       
         ULine=UPPER(ALine)
!TODO grab Equates into Queue to use?         

         !-- Label CLASS(Base) --------------------------------------
         IF MATCH(ULine&',','^[^ ]+ +{{CLASS|INTERFACE}[ ,(]',Match:Regular) THEN   
            Spc1=INSTRING(' ',ALine)    !Find "LABEL <space> CLASS
            ClassName=SUB(ALine,1,Spc1) ; ClassName[1]=UPPER(ClassName[1])              
            ClassSpec=ClassName
            ClassIndex += 1
            Code1=LEFT(SUB(ALine,Spc1+1,9999))  !CLASS(base),more
            Ins1=INSTRING(',',Code1)    !Find "," in CLASS(),Module(
            IF Ins1 THEN Code1=SUB(Code1,1,Ins1). !Cutoff ,Module(
            Ins1=INSTRING('(',Code1)    !Find "(" in CLASS(base)
            IF Ins1 THEN 
               Code1=LEFT(SUB(Code1,Ins1+1,999)) !Now have: BaseClass)
               Ins1=INSTRING(')',Code1)    !Find ")" in BaseClass)
               IF Ins1 THEN 
                  Code1=LEFT(SUB(Code1,1,Ins1-1)) !Now have BaseClass
                  IF Code1 THEN
                     Code1[1]=UPPER(Code1[1])
                     ClassSpec=CLIP(ClassName)&' ('& CLIP(Code1) &')' 
                  END
               END

            END
            !Message('Class: ' & ClassName & '||Line: ' & CLIP(ALine) ) 
            CYCLE         
         END
         IF ~ClassIndex THEN CYCLE.
         
         !MethodName        Procedure (long pAddr, long pLen),virtual 

         IF ~MATCH(ULine&',','^[^ ]+ +'& '{{PROCEDURE|FUNCTION}[ ,(]',Match:Regular) THEN CYCLE.
 
         Spc1=INSTRING(' ',ALine)    !Find 1st space so can 
         ALine=SUB(ALine,1,Spc1) & LEFT(SUB(ALine,Spc1+1,9999))  !CatAddr Procedure
         ULine=UPPER(ALine)
         
         CLEAR(MethodQ)
         G:MethodID += 1 
         MethQ:ID         = G:MethodID        
         MethQ:ClassSpec  = ClassSpec
         MethQ:ClassTip   = ClassSpec
         MethQ:ClassName  = UPPER(ClassName)
         MethQ:ClassIndex = ClassIndex
         MethQ:Name=SUB(ALine,1,Spc1)
         MethQ:Name[1]=UPPER(MethQ:Name[1])
            CASE UPPER(MethQ:Name[1:10])
            OF 'CONSTRUCT' OROF 'DESTRUCT' ; CYCLE  !No need for help with
            END
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
            Ins1=INSTRING('!',MethQ:RV)
            IF Ins1 THEN MethQ:RV=SUB(MethQ:RV,1,Ins1-1).  !no !Comments
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
   
         !I am curious what methods have a lot of Parms
         IF CntParms THEN 
             pmzST.SetValue(MethQ:Parms)
             pmzST.Split(',')
             IF pmzST.Records() >= CntParms THEN 
                PmzCntList=PmzCntList &'<13,10>#' & pmzST.Records() &' = '& lfST.GetLine(X) 
                IF pmzST.Records() > 9 THEN Message('Over 9 ' & lfST.GetLine(X) ).
             END
         END 
        
    END 
    SORT(MethodQ,MethQ:ClassIndex, MethQ:Name, MethQ:LineNo) 
    SELECT(?List:MethodQ,1)
    DISPLAY
    IF CntParms THEN
       SETCLIPBOARD(PmzCntList) ; Message('Parms Count on Clip')
    END 
    
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

HelpMainRtn ROUTINE  !------------------------------------------------------
    DATA
Url STRING(255)
BookMark PSTRING(32)
ST_Home  EQUATE('https://www.capesoft.com/accessories/StringTheorysp.htm')    
ST_Docs  EQUATE('https://www.capesoft.com/docs/StringTheory3/StringTheory.htm')    
    CODE 
    url=ST_Home   
!    name="FormatEtc"  in <td colspan="2" class="highlighted"><a href="#formating">
!    name="HexEtc"     in <td colspan="2" class="highlighted"><a href="#hex">Base  -crlf-  Conversion, ...
!    name="UnicodeEtc" in <td colspan="2" class="highlighted"><a href="#Unicode">Unicode  -crlf-  Encoding, ...
!    name="EncodeEtc"  in <td colspan="2" class="highlighted">Other Encodings</td>
!    name="ZipEtc"     in <td colspan="2" class="highlighted">Compression and Decompression</td>    
    EXECUTE POPUP('~[31763(4796)]StringTheory Help on CapeSoft.com|-' & |
                '|Documentation Main Page' & |
                '|All Methods Reference' & |
                '|Method Topics' & |
                  '{{Split, Manipulate and Join string Help Index' & |
                   '|Formatting and Deformatting' & |
                   '|Base Conversion, Hexadecimal Encoding and Byte Ordering' & |
                   '|Unicode Encoding, Conversion and Handling' & |
                   '|Other Encodings' & |
                   '|Binary Data handling and storage' & |
                   '|Compression and Decompression' & |
                 '|-|Parsing CSV Files ' & |
                   '|File Name Manipulation ' & |
               '}|String Theory Main Page' & |
                '|Big Bang Theory (ST Tools)' & |
                '')
        BEGIN ; END  !Title        
        BookMark='#' !Docs Main                
        BookMark='#StringTheoryMethods'         
        BookMark='#SplitEtc'        !Split Manipulate
        BookMark='#FormatEtc'       !Formating        Bruce has to add theses
        BookMark='#HexEtc'          !Base Conversion   
        BookMark='#UnicodeEtc'      !Unicode Encoding
        BookMark='#EncodeEtc'       !Other Encodings
        BookMark='#Unicode'         !Binary Data
        BookMark='#ZipEtc'          !Compression
        BookMark='#ParsingCSVFile'                
        BookMark='#FileNameManipulation'                
        BookMark=''  !ST Home
        url='https://github.com/CarlTBarnes/StringTheory-Tools#BigBangTheory-Value-and-Split-Lines-Viewer'
    ELSE 
        EXIT
    END
    IF BookMark THEN
        Url=ST_Docs & BookMark
    END 
    IF BAND(KEYSTATE(),0200h) THEN   !Ctrl+
       SetClipboard(Url)
    ELSE 
       ShellExecuteOpen(Url)
    END    
    
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
ShellCmnd CSTRING(1000),AUTO
ShRetErr  LONG,AUTO
OpenOp    CSTRING('open')    !only one operation support so far
  CODE
    ShellCmnd = CLIP(File2Do)
    SETCURSOR(CURSOR:Wait)
    ShRetErr = ShellExecute(0,OpenOp,ShellCmnd,0,0,5)  !5=SW_SHOW
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
Prototype1 PROCEDURE(STRING pMethodQ_Record, STRING pMethodQ_Id)
MethodGrp GROUP(MethodQ),PRE(MethGrp),AUTO
          END
MethodQId   LONG 
ProcName    PSTRING(64)
MaxCnt              EQUATE(10)
ParmGrp GROUP,PRE() 
Parm        STRING(64),DIM(10)   !8 is maximum seen
        END
Proto   STRING(40)    ,DIM(10)   !8 is maximum
SaveQ   QUEUE,PRE(SaveQ),STATIC 
MethQId    LONG                  !SaveQ:MethQId  =MethodQId
Parms      STRING(SIZE(ParmGrp)) !SaveQ:Parms    =ParmGrp
        END 
PCount  USHORT 
P  USHORT 
X  LONG 
Y  LONG 
CallProc ANY
CallTxt STRING(2000) 
OnePerLine  BYTE 
MethodInfo  STRING(128) 

Window WINDOW('Protype:'),AT(,,329,209),GRAY,IMM,SYSTEM,ICON(ICON:JumpPage),FONT('Segoe UI',9),RESIZE
        ENTRY(@s128),AT(19,5,164,11),USE(MethodInfo),SKIP,TRN,READONLY
        CHECK(',&&|'),AT(191,5),USE(OnePerLine),SKIP,TIP('One Parm per Line')
        BUTTON('&Clear'),AT(270,3,25,12),USE(?ClearBtn),SKIP
        BUTTON('Help'),AT(300,3,23,12),USE(?HelpBtn),SKIP,TIP('Open Capesoft.com<13,10>Ctrl+Click to Copy URL')
        ENTRY(@s64),AT(19,18,164,11),USE(Parm[1]),FONT('Consolas')
        ENTRY(@s64),AT(19,32,164,11),USE(Parm[2])
        ENTRY(@s64),AT(19,46,164,11),USE(Parm[3])
        ENTRY(@s64),AT(19,60,164,11),USE(Parm[4])
        ENTRY(@s64),AT(19,74,164,11),USE(Parm[5])
        ENTRY(@s64),AT(19,88,164,11),USE(Parm[6])
        ENTRY(@s64),AT(19,102,164,11),USE(Parm[7])
        ENTRY(@s64),AT(19,116,164,11),USE(Parm[8])
        ENTRY(@s64),AT(19,130,164,11),USE(Parm[9])
        ENTRY(@s64),AT(19,144,164,11),USE(Parm[10])
        ENTRY(@s64),AT(192,18,,11),FULL,USE(Proto[1]),SKIP,TRN,FONT('Consolas')
        ENTRY(@s40),AT(192,32,,11),FULL,USE(Proto[2]),SKIP,TRN
        ENTRY(@s40),AT(192,46,,11),FULL,USE(Proto[3]),SKIP,TRN
        ENTRY(@s40),AT(192,60,,11),FULL,USE(Proto[4]),SKIP,TRN
        ENTRY(@s40),AT(192,74,,11),FULL,USE(Proto[5]),SKIP,TRN
        ENTRY(@s40),AT(192,88,,11),FULL,USE(Proto[6]),SKIP,TRN
        ENTRY(@s40),AT(192,102,,11),FULL,USE(Proto[7]),SKIP,TRN
        ENTRY(@s40),AT(192,116,,11),FULL,USE(Proto[8]),SKIP,TRN
        ENTRY(@s40),AT(192,130,,11),FULL,USE(Proto[9]),SKIP,TRN
        ENTRY(@s40),AT(192,144,,11),FULL,USE(Proto[10]),SKIP,TRN
        PROMPT('1.'),AT(6,18),USE(?PROMPTNo1)
        PROMPT('2.'),AT(6,32),USE(?PROMPTNo2)
        PROMPT('3.'),AT(6,46),USE(?PROMPTNo3)
        PROMPT('4.'),AT(6,60),USE(?PROMPTNo4)
        PROMPT('5.'),AT(6,74),USE(?PROMPTNo5)
        PROMPT('6.'),AT(6,88),USE(?PROMPTNo6)
        PROMPT('7.'),AT(6,102),USE(?PROMPTNo7)
        PROMPT('8.'),AT(6,116),USE(?PROMPTNo8)
        PROMPT('9.'),AT(6,130),USE(?PROMPTNo9)
        PROMPT('A.'),AT(6,144),USE(?PROMPTNoA)
        BUTTON,AT(2,162,14,14),USE(?CopyBtn),SKIP,ICON(ICON:Copy),TIP('Copy Call'),FLAT
        BUTTON,AT(2,185,14,14),USE(?CopyAllBtn),SKIP,ICON(ICON:Copy),TIP('Copy All'),FLAT
        TEXT,AT(19,162,301,39),USE(CallTxt),SKIP,VSCROLL,FONT('Consolas')
    END
Bang BigBangTheory
ST   StringTheory
Pz LONG,DIM(3),STATIC
    CODE
    MethodGrp = pMethodQ_Record 
    ProcName  = CLIP(MethGrp:Name) 
    MethodQId= pMethodQ_Id
    SaveQ:MethQId =MethodQId
    GET(SaveQ,SaveQ:MethQId)
    IF ~ERRORCODE() THEN ParmGrp=SaveQ:Parms.
    IF ~Pz[3] THEN 
        Pz[1]=G:Pz[1]+20 ; Pz[2]=G:Pz[2]+40 ;  Pz[3]=1
    END 
    OPEN(Window)
    IF Pz[3] THEN SETPOSITION(0,Pz[1],Pz[2]).
    0{PROP:Text}=ProcName & ' - Returns: ' & CLIP(MethGrp:RV) & |
                 ' - Class: ' & MethGrp:ClassSpec 
    MethodInfo='.' & ProcName &'() in ' & MethGrp:ClassSpec
    ?MethodInfo{PROP:Tip}=MethodInfo
    DO WinOpenRtn
    ACCEPT
        CASE EVENT()
        OF EVENT:Moved ; GETPOSITION(0,Pz[1],Pz[2],Pz[3]) ; Pz[1] += 10 ; Pz[2] += 20
        END 
        CASE ACCEPTED()
        OF ?CopyBtn    ; SETCLIPBOARD(CallProc)
        OF ?CopyAllBtn ; SETCLIPBOARD(CallTxt)
        OF ?HelpBtn    ; DO HelpRtn
        OF ?Parm_1  |       
         TO ?Parm_10   ; DO CallTxtRtn
        OF ?ClearBtn   ; CLEAR(Parm[]) ; DO CallTxtRtn
        OF ?OnePerLine ; ?CallTxt{PROP:HScroll}=OnePerLine 
                         DO CallTxtRtn
        END
    END
    GETPOSITION(0,Pz[1],Pz[2],Pz[3])
    IF ParmGrp THEN 
       CLEAR(SaveQ)
       SaveQ:MethQId =MethodQId
       GET(SaveQ,SaveQ:MethQId)
       SaveQ:Parms=ParmGrp  
       IF ERRORCODE() THEN ADD(SaveQ,SaveQ:MethQId) ELSE PUT(SaveQ).
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
            Y=?CallTxt{PROP:YPos}
            ?CallTxt{PROP:YPos} = (?Parm_1 +P-2){PROP:YPos} + 18 
            Y=Y - ?CallTxt{PROP:YPos}
            ?CopyBtn{PROP:YPos} = ?CopyBtn{PROP:YPos} - Y !+ 10
            ?CopyAllBtn{PROP:YPos} = ?CopyAllBtn{PROP:YPos} - Y
            0{PROP:Height} = ?CallTxt{PROP:YPos} + ?CallTxt{PROP:Height} + 8
         END
    END
    0{PROP:MinWidth}=0{PROP:Width}
   ! 0{PROP:MaxWidth}=0{PROP:Width} 
    0{PROP:MinHeight}=0{PROP:Height}
    ?CallTxt{PROP:NoWidth}=1
    ?CallTxt{PROP:NoHeight}=1
    ?CallTxt{PROP:Full}=1
    DO CallTxtRtn
    
CallTxtRtn ROUTINE
    IF OnePerLine THEN 
       DO CallOnePerRtn
       EXIT
    END
    CallProc='.' & ProcName &'('
    LOOP P=1 TO PCount
        CallProc=CallProc & CHOOSE(P=1,'',', ')& CLIP(Parm[P]) 
    END
    CallProc=CallProc & ')' 
    CallTxt=CallProc & '<13,10>!('& CLIP(MethGrp:Parms) &') ' & MethGrp:RV
    DISPLAY   
CallOnePerRtn ROUTINE
    DATA 
PWidth LONG(8)
Indent PSTRING(64)
    CODE
    LOOP P=1 TO PCount 
        X=LEN(CLIP(Parm[P]))
        IF X > PWidth THEN PWidth=X.
    END    
    CallProc='.' & ProcName &'('
    Indent=ALL(' ',LEN(CallProc)) 
    LOOP P=1 TO PCount 
        CallProc=CallProc & |
                 CHOOSE(P=1,'',Indent) & |
                 SUB(Parm[P],1,PWidth) & |
                 CHOOSE(P<PCount,' , &|',' )   ') & |
                 ' ! ' & CLIP(Proto[P]) & '<13,10>'            
    END
    CallTxt=CallProc & '!('& CLIP(MethGrp:Parms) &') ' & MethGrp:RV
    DISPLAY   

HelpRtn ROUTINE
    DATA
Url STRING(255)
CS_ST  EQUATE('https://www.capesoft.com/docs/StringTheory3/StringTheory.htm')    
CS_OJb EQUATE('https://www.capesoft.com/docs/OddJob/')
CS_OJm EQUATE('https://www.capesoft.com/docs/OddJob/OddJob.htm')
    CODE   
    Url='https://www.capesoft.com/docs/stringtheory/StringTheory.htm' 
    CASE MethodGrp.ClassName
    !-- StringTheory --------
        OF 'STRINGTHEORY'   ; Url=CS_ST &'#st'& ProcName
        OF 'STRINGPICTURE'  ; Url=CS_ST &'#StringPictureMethods'
        OF 'UNIXDATE'       ; Url=CS_ST &'#UnixDateMethods'
        OF 'STRINGFORMAT'   ; Url=CS_ST &'#StringFormatMethods'
        OF 'STRINGDEFORMAT' ; Url=CS_ST &'#StringDeformatMethods'
    !-- OddJob --------
        OF 'APIBASE'        ; Url=CS_OJb &'JobClasses.htm#APIBase'
        OF 'JOBOBJECT'      ; Url=CS_OJb &'JobClasses.htm#'& ProcName
        OF 'PIPECORE'       ; Url=CS_OJm 
        OF 'PIPESERVER'     ; Url=CS_OJm 
        OF 'PIPECLIENT'     ; Url=CS_OJm 
        OF 'CONSOLECLASS'   ; Url=CS_OJm 

    END
    IF BAND(KEYSTATE(),0200h) THEN   !Ctrl+
       SetClipboard(Url)
    ELSE 
       ShellExecuteOpen(Url)
    END

!==========================================================
!Region CBLocateCls
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
!endRegion CBLocateCls     