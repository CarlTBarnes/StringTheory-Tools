  PROGRAM
  INCLUDE('StringTheory.inc'),ONCE
  INCLUDE('BigBangTheory.INC'),ONCE
  INCLUDE('BigBangSystemString.INC'),ONCE
  INCLUDE('SystemString.INC'),ONCE
  MAP
Test_StringTheory PROCEDURE()  
Test_SystemString PROCEDURE()  
  END
  CODE
  Test_StringTheory()  
 ! Test_SystemString()   cannot do Quotes but does work
!-------------------
Test_StringTheory PROCEDURE()  
Bang BigBangTheory  !Carl Viewer
ST   StringTheory 
    CODE
  !GOTO TabTestLabel:
  IF ~ST.LoadFile('EmpPos2019.csv') THEN  |
        Message('LoadFile EmpPos2019.CSV Failed ' & ST.winErrorCode ).
  ST.Split('<13,10>') !,'"','"') !<- GPF in 2014 ST
  Bang.LinesViewInList(ST)    !See Raw Lines
  Bang.LinesViewSplitCSV(ST)  !does-->  Bang.LinesViewSplit(ST ,',' ,'"','"')
! return
  IF ~ST.LoadFile('LangCds.Pipe') THEN |
        Message('LoadFile LangCds.Pipe Failed').
  ST.Split('<13,10>') !,'"','"') !<- GPF in 2014 ST  
  Bang.LinesViewInList(ST) !See the Raw Lines
  Bang.LinesViewSplit(ST ,'|' ,'"','"')

! return
TabTestLabel:
  IF ~ST.LoadFile('LangCds.TAB') THEN |
        Message('LoadFile LangCds.TAB Failed').
  ST.Split('<13,10>')
  Bang.LinesViewSplitTAB(ST) 
  
!-------------------
Test_SystemString PROCEDURE()  
Bang BigBangSystemString  !Carl Viewer
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
  