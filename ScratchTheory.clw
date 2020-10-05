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
TestCode    PROCEDURE()
SaveAsNewScratch PROCEDURE()
DB          PROCEDURE(STRING DebugMessage)   !Output Debug String
DBClear     PROCEDURE()                      !Clear DebugView Buffer
      MODULE('api')
        OutputDebugString(*CSTRING cMsg),PASCAL,DLL(1),RAW,NAME('OutputDebugStringA')
        DebugBreak(),PASCAL,DLL(1) 
        GetLastError(),LONG,PASCAL,DLL(1) 
      END
    END

G_Bang BigBangTheory        !Globals
G_ST   StringTheory
G_Lne  StringTheory    

    CODE
    TestCode()
    RETURN
!===========================================================================
TestCode   PROCEDURE
X      LONG
Txt    STRING(4000)
Window WINDOW('Scratch StringTheory '),AT(,,400,200),CENTER,GRAY,IMM,SYSTEM,FONT('Segoe UI',9),RESIZE
        BUTTON('Test 1'),AT(9,10),USE(?Test1)
        BUTTON('Test 2'),AT(9,30),USE(?Test2)
        BUTTON('Test 3'),AT(9,50),USE(?Test3)
        BUTTON('Test 4 - Split test Separator && Between && SerializeQueue'),AT(9,70),USE(?Test4)
        BUTTON('Test 5 - Load File, View Split'),AT(9,90),USE(?Test5)
        BUTTON('Save As <13,10>New Scratch'),AT(350,5,45,22),USE(?SaveAsBtn),SKIP,TIP('Copy ScratchTheory Clw and CwProj to make new Project')
        TEXT,AT(1,150),FULL,USE(txt),HVSCROLL
    END
    CODE
    OPEN(WINDOW)
    0{PROP:text}=clip(0{PROP:text}) &' - Library ' & system{PROP:LibVersion,2} &'.'& system{PROP:LibVersion,3}
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow 
        OF EVENT:Timer
        END
        CASE ACCEPTED()
        OF ?Test1       ; DO Test1Rtn
        OF ?Test2       ; DO Test2Rtn
        OF ?Test3       ; DO Test3Rtn
        OF ?Test4       ; DO Test4Rtn
        OF ?Test5       ; DO Test5Rtn 
        OF ?SaveAsBtn   ; SaveAsNewScratch()
        END
        CASE FIELD()
        END
    END
    CLOSE(WINDOW)
!========================================================================
Test1Rtn ROUTINE  !------------------------------------------------------
    DATA
Bang BigBangTheory
ST   StringTheory
Lne  StringTheory    
    CODE
    
Test2Rtn ROUTINE  !------------------------------------------------------
    DATA
Bang BigBangTheory
ST   StringTheory
Lne  StringTheory    
    CODE
    
Test3Rtn ROUTINE  !------------------------------------------------------ 
    DATA
Bang BigBangTheory
ST   StringTheory
Lne  StringTheory    
    CODE
    
Test4Rtn ROUTINE  !------------------------------------------------------ 
    DATA
Bang BigBangTheory
ST   StringTheory
Lne  StringTheory 
FilesQ    FILE:Queue   
    CODE       
    st.SetValue('[12,24,36],bruce,(po box 511, plumstead)') 
        bang.ValueView(ST, 'Value Split  "[-(","]-)",false,,,- ' ) 
    st.split(',','[-(',']-)',false,,,'-') ! result is ! [12,24,36] ! bruce ! (po box 511, plumstead)
        bang.LinesViewInList(ST,'Split [-( ]-) Separator "-" ')

    st.SetValue('(123)(456)(789)')
        bang.ValueView(ST,'Value test SplitBetween "(" ")"') 
    st.splitBetween('(',')') 
        bang.LinesViewInList(ST,'SplitBetween "( )" ')

   ! DIRECTORY(FilesQ,'c:\windows\system32\*.*',ff_:NORMAL)
    DIRECTORY(FilesQ,'*.*',ff_:NORMAL)
    st.SerializeQueue(FilesQ,'<13,10>',',','"') 
    st.Prepend('Name,Short,Date,Time,Size,Attrib<13,10>')
       bang.ValueView(ST,'Directory SerializeQueue Value')
    st.Split('<13,10>')       
       bang.LinesViewInList(ST,'Directory SerializeQueue Lines')
       bang.LinesViewSplitCSV(ST)
       EXIT    

Test5Rtn ROUTINE  !---- Split code you can modify -------------------------------------------------- 
    DATA
Bang BigBangTheory
ST   StringTheory
Lne  StringTheory
LoadFN   STRING(260)
Delim   PSTRING(9)    
Qt1     PSTRING(9)    
Qt2     PSTRING(9)    
    CODE
    IF ~FILEDIALOG('Select LoadFile for StringTheory', LoadFN, |
                   'All files|*.*|CSV|*.CSV|Text|*.TXT', FILE:KeepDir+FILE:LongName) THEN 
        EXIT
    END
    IF ~ST.LoadFile(LoadFN) THEN
        Message('LoadFile Windows Error ' & ST.winErrorCode,'LoadFile') 
        EXIT
    END
    Bang.ValueView(ST,'LoadFile ' & LoadFN)
    ST.Split('<13,10>')
       Bang.LinesViewInList(ST,'LoadFile ' & LoadFN)
    CASE Message('View Split into Columns?','Bang LinesViewSplit() ?',, |
                    'No View|CSV|TAB|Pipe')
    OF 1 ; EXIT  !BEGIN ; END  !NO
    OF 2 ; Bang.LinesViewSplitCSV(ST)       ; Delim=','     ; Qt1='"'
    OF 3 ; Bang.LinesViewSplitTAB(ST)       ; Delim=CHR(9)
    OF 4 ; Bang.LinesViewSplit(ST,'|','"')  ; Delim='|'     ; Qt1='"'
    END
    LOOP X=1 TO ST.Records()       
        Lne.SetValue(ST.GetLine(X))
        IF X=5 THEN   !See line 5 in LIST to be sure code is right
           Lne.Split(Delim,Qt1,Qt2)
           Bang.LinesViewInList(Lne,'LNE Split ' & QUOTE(Delim) &' '& Qt1 )
        END
    END
    
!========================================================================================
SaveAsNewScratch PROCEDURE()
Bang BigBangTheory
ST      StringTheory
stPrj   StringTheory
CopyNameOnly STRING('ScratchTheory')    
CopyPrjName  STRING('ScratchTheory.CwProj')    
CopyClwName  STRING('ScratchTheory.clw')
NewPrjDiskFN STRING(260),STATIC  !c:\dev\proj\NewName.cwproj  
NewPrjPath  STRING(255)          !c:\dev\proj
NewPrjFN    STRING(64)           !            NewName.cwproj
NewPrjName  STRING(64)           !            NewName
SazCls  CLASS
Copy2New    PROCEDURE(STRING FromFN,STRING ToFN)
        END
    CODE
    IF ~EXISTS(CopyClwName) THEN   
        MESSAGE('CLW file does not exist: ' & CopyClwName,'SaveAsNewScratchRtn')
        RETURN
    END 
    IF ~stPrj.LoadFile(CopyPrjName) THEN
        Message('Failed LoadFile "' & CopyPrjName &'" Windows Error ' & stPrj.winErrorCode,'SaveAsNewScratchRtn')
        RETURN
    END  ! ; Bang.ValueView(Prj)
    CASE MESSAGE('To create a new Scratch project a "Save" FileDialog will open.' & |
                 '||Select the Folder and type the name of you new .CwProj file.', |
                 'Save As New Scratch',ICON:Clarion,'Continue|Cancel')
    OF 2 ; RETURN
    END
    IF ~NewPrjDiskFN THEN NewPrjDiskFN='ScratchST_' & FORMAT(TODAY(),@d11) &'.CwProj'.
    IF ~FILEDIALOG('New Scratch CwProj File', NewPrjDiskFN, |
                    'CwProj|*.cwproj|All|*.*', |
                    FILE:Save + FILE:LongName+FILE:AddExtension+FILE:KeepDir)
        RETURN
    END 
    
    ST.SetValue(NewPrjDiskFN)
    IF ~ST.Instring('.cwproj',1,1,,1) THEN
        Message('Must be .CwProj') ; RETURN
    END 
    NewPrjPath  =ST.PathOnly()
    NewPrjFN    =ST.FileNameOnly(,1)
    NewPrjName  =ST.FileNameOnly(,0)
    IF lower(NewPrjFN)=lower(CopyPrjName) THEN
       IF lower(longpath(NewPrjPath))=lower(longpath()) THEN
          MESSAGE('You cannot blow away |file ' & CopyPrjName &'|in ' & LongPath())
          RETURN
       END
    END  
    stPrj.Replace(CLIP(CopyClwName),CLIP(NewPrjName)&'.clw', ,,,1)   
        !Bang.ReplaceView(stPrj,'>' & CLIP(CopyNameOnly) &'<<','>' & CLIP(NewPrjName) &'<<', ,,,1) 
    stPrj.Replace('>' & CLIP(CopyNameOnly) &'<<','>' & CLIP(NewPrjName) &'<<', ,,,1) 
    stPrj.SaveFile(NewPrjDiskFN)
    SazCls.Copy2New(CopyClwName,CLIP(NewPrjName) &'.clw')
    SazCls.Copy2New('BigBangTheory.clw','BigBangTheory.clw')
    SazCls.Copy2New('BigBangTheory.inc','BigBangTheory.inc')
    RETURN
SazCls.Copy2New    PROCEDURE(STRING FromFN,STRING ToFN) 
    CODE
    IF ~EXISTS(CLIP(NewPrjPath) &'\'& ToFN) AND EXISTS(FromFN) THEN 
        Copy(FromFN,CLIP(NewPrjPath) &'\'& ToFN) 
    END     
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