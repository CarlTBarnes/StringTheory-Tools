                     MEMBER('BasicBrowseLeg.clw')


!--------------------------------------------------
RIDelete:EmpPosCSV   FUNCTION
Current:Position     STRING(512)
  CODE
  Current:Position = POSITION(EmpPosCSV)
  REGET(EmpPosCSV,Current:Position)
  DELETE(EmpPosCSV)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:RIDeleteError,'EmpPosCSV')
    DO RICloseFiles
    RETURN(1)
  ELSE
    DO RICloseFiles
    RETURN(0)
  END
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  EXIT
