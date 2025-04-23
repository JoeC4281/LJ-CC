' Utility to set the "console application" flag in a Windows executable program
' header. Invoke with a filespec on the command line or use interactively.
' Laurence Jackson, Aug 2011. Compile with PB/Win 9 or PB/Win 10.
'------------------------------------------------------------------------------

#dim all
#compiler pbwin 10
#include "Win32API.inc"


function pbmain _______________________________________________________________
                                                         () as long
'------------------------------------------------------------------------------
'
'------------------------------------------------------------------------------

local NT_Headers as IMAGE_NT_HEADERS
local DOS_Header as IMAGE_DOS_HEADER
local lgFile as long
local stFile as string
local lgCmndLine as long
local lgResult as long

stFile = command$
if stFile= "" then
  lgResult = msgbox("This utility sets the 'console application' flag in a Windows" _
           + $CRLF + "executable program header. Is this what you want to do?" _
           , %MB_YESNO, "gui2con")
  if lgResult = %IDNO then exit function
  display openfile %HWND_DESKTOP, , , "", "", _
  chr$("Executable programs", 0, "*.exe", 0), "", "", %OFN_FILEMUSTEXIST to stFile
  if stFile = "" then exit function 'Cancel
else
  lgCmndLine = %TRUE
end if

lgFile = freefile
open stFile for binary as lgFile
if len(lgFile) = 0 then
  close lgFile
  kill stFile
  msgbox stFile + " was not found", , "gui2con"
  exit function
end if

get lgFile, , DOS_Header
seek lgFile, DOS_Header.e_lfanew + 1
get lgFile, , NT_Headers

if NT_Headers.OptionalHeader.Magic <> %IMAGE_NT_OPTIONAL_HDR32_MAGIC then
  msgbox stFile + " is not a 32-bit Windows executable", , "gui2con"
else
  if NT_Headers.OptionalHeader.SubSystem = %IMAGE_SUBSYSTEM_WINDOWS_GUI then
    NT_Headers.OptionalHeader.SubSystem = %IMAGE_SUBSYSTEM_WINDOWS_CUI
    seek lgFile, DOS_Header.e_lfanew + 1
    put lgFile, , NT_Headers
    if isfalse(lgCmndLine) then
      msgbox stFile + " is now a console application", , "gui2con"
    end if
  elseif NT_Headers.OptionalHeader.SubSystem = %IMAGE_SUBSYSTEM_WINDOWS_CUI then
    msgbox stFile + " is already a console application and was not modified", , "gui2con"
  else
    msgbox stFile + " is an unknown type of application and was not modified", , "gui2con"
  end if
end if
close lgFile

end function
