# StringTheory-LoadFile-Split-Viewer
 StringTheory LoadFile and Split Viewer aka BigBangTheory

StringTheory makes it easy to load a CSV file and split it into columns. 
This class helps by allowing you to view the file right from the StringTheory object to confirm the data is what you exect, and to see how the splits are working.

You write code like below (from https://www.capesoft.com/docs/StringTheory3/StringTheory.htm#ParsingCSVFile).
```Clarion
str   StringTheory
lne   StringTheory
x     Long
  code
  str.LoadFile('Somefile.CSV')
  str.Split('<13,10>','"')
  loop x = 1 to str.Records()
    Lne.SetValue(Str.GetLine(x))
    Lne.Split(',','"','"',true)
    field1 = Lne.GetLine(1)
    field2 = Lne.GetLine(2)
  End
```

Insert BigBang to see the results.

```Clarion
str   StringTheory
lne   StringTheory
x     Long
Bang BigBangTheory    !<-- Viewer
  code
  str.LoadFile('Somefile.CSV')
  str.Split('<13,10>','"')
  IF DebugLoad THEN               !<-- Limit to just developers
     Bang.LinesViewInList(str)    !<-- Add to see Lines split by 13,10
     Bang.LinesViewSplitCSV(Str)  !<-- Add to see CSV columns
     Bang.LinesViewSplit(Str,',','"','"')  !Alternate specify Split() parms
  END
  loop x = 1 to str.Records()
    Lne.SetValue(Str.GetLine(x))
    Lne.Split(',','"','"',true)
    field1 = Lne.GetLine(1)
    field2 = Lne.GetLine(2)
  End
```

Screen capture shows the views available. 

![BigBangCapture](screenshotbang1.png)

I would expect you might use this class in your APP to allow a quick way to view files before import. You probably will want to remove some of the features that would confuse end users, like the Picture thing and Menu.

There is also a SystemStringClass version. That class has no ability to deal with CSV type quoted values.
