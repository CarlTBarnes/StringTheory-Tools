  PROGRAM
  INCLUDE('StringTheory.inc'),ONCE
  INCLUDE('BigBangTheory.INC'),ONCE
  INCLUDE('BigBangSystemString.INC'),ONCE
  INCLUDE('SystemString.INC'),ONCE
 
  MAP
Test_StringTheory PROCEDURE()  
Test_SystemString PROCEDURE()
HaltBangTest      PROCEDURE()  
  END
  CODE
  START(HaltBangTest)
  Test_StringTheory()  
 ! Test_SystemString()   cannot do Quotes but does work
!-------------------
Test_StringTheory PROCEDURE()  
Bang BigBangTheory  !Carl Viewer
ST   StringTheory
lne  StringTheory
X    LONG 
    CODE 
!    Bang.DoNotShow=1      !When True None of the Bang windows show  
!    DO SplitMultiQuotesTestRtn ; halt()  
    DO CsvTestRtn
    DO TabTestRtn
    DO PipeSplitRtn
    DO WrapTestRtn
    DO SerializeQRtn
    DO HexTestRtn
    DO SplitMultiQuotesTestRtn
    HALT()
    RETURN

!-------------------------------------
CsvTestRtn ROUTINE    
  IF ~ST.LoadFile('EmpPos2019.csv') THEN Message('LoadFile EmpPos2019.CSV Failed ' & ST.winErrorCode ).
  Bang.ValueView(ST,'ValueView ST GetValue of EmpPos2019.CSV')
  ST.Split('<13,10>')
  Bang.LinesViewInList(ST,'Split 13,10 EmpPos2019.CSV')    !See Raw Lines split by 13,10 in LIST
  Bang.LinesViewSplitCSV(ST)                  !<-- this line does below CSV split
! Bang.LinesViewSplit(ST ,',' ,'"','"',True)  !<-- same as above CSV function
  LOOP X=5 TO 5
    lne.SetValue(ST.GetLine(X))
    lne.Split(',','"','"', True)  ! Your Exact Split Code e.g. CSV
    Bang.LinesViewInList(lne)     ! See the results of your exact split code
  END
! EXIT
  IF ~ST.LoadFile('LangCds.Pipe') THEN Message('LoadFile LangCds.Pipe Failed').
  ST.Split('<13,10>') 
  Bang.LinesViewInList(ST) !See the Raw Lines split 13,10
  Bang.LinesViewSplit(ST ,'|')  !Split those lines using Pipes

!-------------------------------------
TabTestRtn ROUTINE
  !   Bang.ValueView(ST,'Empty ST test')
  IF ~ST.LoadFile('LangCds.TAB') THEN Message('LoadFile LangCds.TAB Failed').
  Bang.ValueView(ST,'ValueView LangCds.TAB')  
!  EXIT
  ST.Split('<13,10>') 
  Bang.LinesViewInList(ST)
!  Bang.LinesViewSplitTAB(ST) 
  Bang.LinesViewSplit(ST,CHR(9)) 
   
  ST.Base64Encode() 
  Bang.ValueView(ST,'ValueView Base64 Encode LangCds.TAB') 
  
!-------------------------------------
PipeSplitRtn ROUTINE

  IF ~ST.LoadFile('LangCds.Pipe') THEN Message('LoadFile LangCds.Pipe Failed').
  ST.Split('<13,10>', True)
  Bang.LinesViewInList(ST) 
  Bang.LinesViewSplit(ST ,'|','"')  

!-------------------------------------
WrapTestRtn ROUTINE
  IF ~ST.LoadFile('AliceANSI.txt') THEN Message('LoadFile AliceANSI.txt Failed').   
  Bang.ValueView(ST,'ST Value Full Alice.txt') 
!  Bang.StringView(SUB(ST.getValue(),1,0FF00h),'64kb of Alice.txt') 
  Bang.WrapView(ST,'Wrap Alice.txt') !, true) 

 ST.SetValue('Test-SplitIntoWords - ' & |
    'While impervious to everything, they had no resistance to the microbes ' & | 
    'in our atmosphere to which we have long since become immune. After all that men ' & | 
    'could do had failed, the Trumpians were destroyed and humanity was saved by the ' & | 
    'littlest things, which God, in His wisdom, had put upon this Earth.')
          
 ST.SplitIntoWords()
 Bang.LinesViewInList(ST)
!----------------------------
SerializeQRtn ROUTINE
    DATA
FilesQ    FILE:Queue   
    CODE
    DIRECTORY(FilesQ,'c:\windows\system32\*.*',ff_:NORMAL) ; SORT(FilesQ,-FilesQ.Size)
    !DIRECTORY(FilesQ,'*.*',ff_:NORMAL)
    st.SerializeQueue(FilesQ,'<13,10>',',','"') 
    st.Prepend('Name,Short,Date,Time,Size,Attrib<13,10>')
       bang.ValueView(ST,'Directory() SerializeQueue Value')
    st.Split('<13,10>')       
       bang.LinesViewInList(ST,'Directory() SerializeQueue Lines')
       bang.LinesViewSplitCSV(ST)
       EXIT  
!-------------------------------------
HexTestRtn ROUTINE   !
    DATA 
s256    STRING(256)   
    CODE
    LOOP X=1 to 255 ; s256[x]=CHR(x) ; END 
    Bang.StringView(s256 & s256 & s256 & s256 & '1024 Byte Tester','Check "HEX" Test 1024 bytes')
    Bang.StringView(s256,'Call *STRING (256) sig')
    IF ST.LoadFile('LangCds.TAB') THEN 
       Bang.ValueView(ST,'Check "HEX" LangCds.TAB')  
    END

!------------------------------------- 
SplitMultiQuotesTestRtn ROUTINE
    st.Start()
    st.AddLine(1,'[12,24,36],Person,(PO Box 111, Plum)') 
    st.AddLine(2,'[22,34,46],   Man,(PO Box 222, Scarlet') 
    st.AddLine(3,'[32,44,56], Woman,(PO Box 333, Mustard')
    st.AddLine(4,'[42,55,66],Camera,(PO Box 444, Peacock')
    st.AddLine(5,'[52,65,76],    TV,(PO Box 555, Green')  
    
    st.Join('<13,10>')
        bang.LinesViewInList(st,'AddLines with Quotes [()]')
        bang.LinesViewSplit(st,',','[-(',']-)',false,,,'-')
        exit
! StringTheory.Split Procedure(
!   string pSplitStr,         <13,10> or <9> if 1 line of Tab Delim
!   <string pQuotestart>,     "
!   <string pQuoteEnd>, 
!   bool removeQuotes=false,  
!   bool pClip = false,       Trim trailing spaces
!   bool pLeft=false,         Trim leading spaces
!   <string pSeparator>,      Sep if QuoteStart has multiple e.g. - for  [-(  )-]
!   Long pNested=false)       True=Ignore () around "5,6" in (1,2),(3,4),((5,6),7)

!========================================================================== 
! systemStringClass cannot do Quotes in Split
Test_SystemString PROCEDURE()  
Bang BigBangSystemString  
!ST   StringTheory 
SSC  SystemStringClass 
    CODE     
  !GOTO PipeLabel: 
  !GOTO TabTestLabel: 
  IF SSC.FromFile('EmpPos2019.csv') THEN Message('FromFile EmpPos2019.CSV Failed '). 
  SSC.Split('<13,10>', True)
  Bang.LinesViewInList(SSC)    !See Raw Lines
  !AFAIK SystemStringClass does NOT support the CSV with Quotes 
  Bang.LinesViewSplitCSV(SSC)  !does-->  Bang.LinesViewSplit(ST ,',' ,'"','"')
! return 
PipeLabel:
  IF SSC.FromFile('LangCds.Pipe') THEN Message('FromFile LangCds.Pipe Failed').
  SSC.Split('<13,10>', True)
  Bang.LinesViewInList(SSC) 
  Bang.LinesViewSplit(SSC ,'|')

! return
TabTestLabel:
  IF SSC.FromFile('LangCds.TAB') THEN Message('FromFile LangCds.TAB Failed').
  SSC.Split('<13,10>')
  Bang.LinesViewSplitTAB(SSC) 

!===========================================================================
HaltBangTest      PROCEDURE()
W   WINDOW('Bang Test'),AT(1,1,80,40),GRAY,SYSTEM,FONT('Segoe UI',9)
        BUTTON('Halt Bang Test'),AT(9,4,60,33),USE(?HaltBtn),ICON(ICON:Cross)
    END
    CODE
    OPEN(W)
    ACCEPT
        IF ACCEPTED() THEN BREAK.
    END
    HALT()