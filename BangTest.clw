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
        !  Bang.DoNotShow=1      !When True None of the Bang windows show  
!    DO SerializeQRtn
    DO CsvTestRtn
    DO TabTestRtn
    DO PipeSplitRtn
    DO WrapTestRtn
    DO SerializeQRtn
    DO HexTestRtn
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

 ST.SetValue('Test-SplitIntoWords - So she was considering in her own mind (as well as she could, for the hot day ' & |
          'made her feel very sleepy and stupid), whether the pleasure of making a ' & |
          'daisy-chain would be worth the trouble of getting up and picking the daisies, ' & |
          'when suddenly a White Rabbit with pink eyes ran close by')
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
    IF ST.LoadFile('LangCds.TAB') THEN 
       Bang.ValueView(ST,'Check "HEX" LangCds.TAB')  
    END

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