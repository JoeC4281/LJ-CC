' LJ's Complete Console (LJ-CC) for PB/Win by Laurence Jackson (Public Domain)
'    A Comprehensive Set of Commands and Functions Supporting Console I/O
'------------------------------------------------------------------------------
'
' The reference documentation for this code is LJ-CC.chm (HTML help).
'
' Tested with PB/Win 8, 9 & 10 and with Win32API.inc from PB/Win 8 (PB),
' 9 (PB & JR) and 10 (PB & JR).
'
' All necessary declarations are included here to:
' (a) Avoid compatibility issues between header file versions.
' (b) To avoid conficts with any header files used by the main program.
' (c) To provide unicode console support under PB/Win 10 without requiring the
'     main program to use unicode APIs (the global %UNICODE flag is not set).
'
' First issue: August 22, 2011
'
'------------------------------------------------------------------------------

' Use wide (unicode) character I/O in PB/Win 10+ and PB/CC 6+.

#IF %DEF(%PB_WIN32) AND %PB_REVISION >= &H1000
  %WIDECHAR = 1
#ELSEIF %DEF(%PB_CC32) AND %PB_REVISION >= &H600
  %WIDECHAR = 1
#ENDIF

#IF %DEF(%WIDECHAR)
  MACRO CreateFileAlias(P1) = "CreateFileW"
  MACRO GetWindowTextAlias(P1) = "GetWindowTextW"
  MACRO ReadConsoleOutputAlias(P1) = "ReadConsoleOutputW"
  MACRO WriteConsoleOutputAlias(P1) = "WriteConsoleOutputW"
  MACRO GetConsoleTitleAlias(P1) = "GetConsoleTitleW"
  MACRO WriteConsoleAlias(P1) = "WriteConsoleW"
  MACRO ReadConsoleInputAlias(P1) = "ReadConsoleInputW"
  MACRO PeekConsoleInputAlias(P1) = "PeekConsoleInputW"
  MACRO SetConsoleTitleAlias(P1) = "SetConsoleTitleW"
  MACRO ReadConsoleOutputCharacterAlias(P1) = "ReadConsoleOutputCharacterW"
  MACRO ScrollConsoleScreenBufferAlias(P1) = "ScrollConsoleScreenBufferW"
  MACRO WriteConsoleInputAlias(P1) = "WriteConsoleInputW"
  MACRO dstring = WSTRING
  MACRO zstring = WSTRINGZ
#ELSE
  MACRO CreateFileAlias(P1) = "CreateFileA"
  MACRO GetWindowTextAlias(P1) = "GetWindowTextA"
  MACRO ReadConsoleOutputAlias(P1) = "ReadConsoleOutputA"
  MACRO WriteConsoleOutputAlias(P1) = "WriteConsoleOutputA"
  MACRO GetConsoleTitleAlias(P1) = "GetConsoleTitleA"
  MACRO WriteConsoleAlias(P1) = "WriteConsoleA"
  MACRO ReadConsoleInputAlias(P1) = "ReadConsoleInputA"
  MACRO PeekConsoleInputAlias(P1) = "PeekConsoleInputA"
  MACRO SetConsoleTitleAlias(P1) = "SetConsoleTitleA"
  MACRO ReadConsoleOutputCharacterAlias(P1) = "ReadConsoleOutputCharacterA"
  MACRO ScrollConsoleScreenBufferAlias(P1) = "ScrollConsoleScreenBufferA"
  MACRO WriteConsoleInputAlias(P1) = "WriteConsoleInputA"
  MACRO dstring = STRING
  MACRO zstring = ASCIIZ
#ENDIF

' The dummy macro parameters above are required for compatibility
' of code between different versions of PB.

%NULL = 0
%FALSE = 0
%TRUE = 1
%STD_INPUT_HANDLE_X  = &HFFFFFFF6
%STD_OUTPUT_HANDLE_X = &HFFFFFFF5
%STD_ERROR_HANDLE_X  = &HFFFFFFF4
%CONSOLE_TEXTMODE_BUFFER = 1
%GENERIC_READ_X = &H80000000
%GENERIC_WRITE = &H40000000
%FILE_SHARE_READ = 1
%FILE_SHARE_WRITE = 2
%OPEN_EXISTING = 3
%KEY_EVENT = 1
%MOUSE_EVENT = 2
%SWP_NOSIZE = 1
%SWP_NOZORDER = 4
%ENABLE_MOUSE_INPUT = 16
%MOUSE_MOVED = 1
%DOUBLE_CLICK = 2

TYPE CON_COORD
  x AS INTEGER
  y AS INTEGER
END TYPE

UNION COORD_UNION
  wd AS CON_COORD
  dw AS DWORD
END UNION

TYPE SECURITY_ATTRIBUTES_X
  nLength AS DWORD
  lpSecurityDescriptor AS DWORD
  bInheritHandle AS LONG
END TYPE

TYPE CON_CURSOR_INFO
  dwSize AS DWORD
  bVisible AS LONG
END TYPE

TYPE CON_CHAR_INFO
  Char AS WORD
  Attributes AS WORD
END TYPE

TYPE CON_RECT
  nLeft AS INTEGER
  nTop AS INTEGER
  nRight AS INTEGER
  nBottom AS INTEGER
END TYPE

TYPE WIN_RECT
  nLeft AS LONG
  nTop AS LONG
  nRight AS LONG
  nBottom AS LONG
END TYPE

TYPE CON_SCREEN_BUFFER_INFO
  dwSize AS CON_COORD
  dwCursorPosition AS CON_COORD
  wAttributes AS WORD
  srWindow AS CON_RECT
  dwMaximumWindowSize AS CON_COORD
END TYPE

TYPE CON_KEY_EVENT_RECORD
  bKeyDown AS LONG
  wRepeatCount AS WORD
  wVirtualKeyCode AS WORD
  wVirtualScanCode AS WORD
  uChar AS WORD
  dwControlKeyState AS DWORD
END TYPE

TYPE CON_MOUSE_EVENT_RECORD
  dwMousePosition AS CON_COORD
  dwButtonState AS DWORD
  dwControlKeyState AS DWORD
  dwEventFlags AS DWORD
END TYPE

TYPE CON_WINDOW_BUFFER_SIZE_RECORD
  dwSize AS CON_COORD
END TYPE

TYPE CON_MENU_EVENT_RECORD
  dwCommandId AS DWORD
END TYPE

TYPE CON_FOCUS_EVENT_RECORD
  bSetFocus AS LONG
END TYPE

UNION CON_EVENT_UNION
  KeyEvent AS CON_KEY_EVENT_RECORD
  MouseEvent AS CON_MOUSE_EVENT_RECORD
  WindowBufferSizeEvent AS CON_WINDOW_BUFFER_SIZE_RECORD
  MenuEvent AS CON_MENU_EVENT_RECORD
  FocusEvent AS CON_FOCUS_EVENT_RECORD
END UNION

TYPE CON_INPUT_RECORD DWORD
  EventType AS WORD
  Event AS CON_EVENT_UNION
END TYPE

TYPE OVERLAPPED_UNION_STRUCT DWORD
  Offset AS DWORD
  OffsetHigh AS DWORD
END TYPE

UNION OVERLAPPED_UNION DWORD
  OVERLAPPED_UNION_STRUCT
  Pointer AS DWORD
END UNION

TYPE OVERLAPPED_X DWORD
  Internal AS DWORD
  InternalHigh AS DWORD
  OVERLAPPED_UNION
  hEvent AS DWORD
END TYPE

DECLARE FUNCTION SetForegroundWindow LIB "USER32.DLL" _
   ALIAS "SetForegroundWindow" _
 ( BYVAL hWnd AS DWORD _
 ) AS LONG

DECLARE FUNCTION EnumWindows LIB "USER32.DLL" _
   ALIAS "EnumWindows" _
 ( BYVAL lpEnumFunc AS DWORD _
 , BYVAL lParam AS LONG _
 ) AS LONG

DECLARE FUNCTION GetWindowTextX LIB "USER32.DLL" _
   ALIAS GetWindowTextAlias(1) _
 ( BYVAL hWnd AS DWORD _
 , BYREF lpString AS zstring _
 , BYVAL nMaxCount AS LONG _
 ) AS LONG

DECLARE FUNCTION GetWindowRect LIB "USER32.DLL" _
   ALIAS "GetWindowRect" _
 ( BYVAL hWnd AS DWORD _
 , BYREF lpRect AS WIN_RECT _
 ) AS LONG

DECLARE FUNCTION SetWindowPos LIB "USER32.DLL" _
   ALIAS "SetWindowPos" _
 ( BYVAL hWnd AS DWORD _
 , BYVAL hWndInsertAfter AS DWORD _
 , BYVAL X AS LONG _
 , BYVAL Y AS LONG _
 , BYVAL cx AS LONG _
 , BYVAL cy AS LONG _
 , BYVAL uFlags AS DWORD _
 ) AS LONG

DECLARE FUNCTION WriteFile LIB "KERNEL32.DLL" _
   ALIAS "WriteFile" _
 ( BYVAL hFile AS DWORD _
 , BYREF lpBuffer AS ANY _
 , BYVAL nNumberOfBytesToWrite AS DWORD _
 , BYREF lpNumberOfBytesWritten AS DWORD _
 , BYREF lpOverlapped AS OVERLAPPED_X _
 ) AS LONG

DECLARE FUNCTION ReadFile LIB "KERNEL32.DLL" _
   ALIAS "ReadFile" _
 ( BYVAL hFile AS DWORD _
 , BYREF lpBuffer AS ANY _
 , BYVAL nNumberOfBytesToRead AS DWORD _
 , BYREF lpNumberOfBytesRead AS DWORD _
 , BYREF lpOverlapped AS OVERLAPPED_X _
 ) AS LONG

DECLARE FUNCTION GetStdHandle LIB "KERNEL32.DLL" _
   ALIAS "GetStdHandle" _
 ( BYVAL nStdHandle AS DWORD _
 ) AS DWORD

DECLARE FUNCTION CreateFileX LIB "KERNEL32.DLL" _
   ALIAS CreateFileAlias(1) _
 ( BYREF lpFileName AS zstring _
 , BYVAL dwDesiredAccess AS DWORD _
 , BYVAL dwShareMode AS DWORD _
 , BYREF lpSecurityAttributes AS SECURITY_ATTRIBUTES_X _
 , BYVAL dwCreationDisposition AS DWORD _
 , BYVAL dwFlagsAndAttributes AS DWORD _
 , BYVAL hTemplateFile AS DWORD _
 ) AS DWORD

DECLARE FUNCTION CloseHandle LIB "KERNEL32.DLL" _
   ALIAS "CloseHandle" _
 ( BYVAL hObject AS DWORD _
 ) AS LONG

DECLARE FUNCTION SetConsoleScreenBufferSizeX LIB "KERNEL32.DLL" _
   ALIAS "SetConsoleScreenBufferSize" _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYVAL dwSize AS CON_COORD _
 ) AS LONG

DECLARE FUNCTION SetConsoleCursorPositionX LIB "KERNEL32.DLL" _
   ALIAS "SetConsoleCursorPosition" _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYVAL dwCursorPosition AS CON_COORD _
 ) AS LONG

DECLARE FUNCTION ReadConsoleOutputX LIB "KERNEL32.DLL" _
   ALIAS ReadConsoleOutputAlias(1) _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYREF lpBuffer AS CON_CHAR_INFO _
 , BYVAL dwBufferSize AS CON_COORD _
 , BYVAL dwBufferCoord AS CON_COORD _
 , BYREF lpReadRegion AS CON_RECT _
 ) AS LONG

DECLARE FUNCTION WriteConsoleOutputX LIB "KERNEL32.DLL" _
   ALIAS WriteConsoleOutputAlias(1) _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYREF lpBuffer AS CON_CHAR_INFO _
 , BYVAL dwBufferSize AS CON_COORD _
 , BYVAL dwBufferCoord AS CON_COORD _
 , BYREF lpWriteRegion AS CON_RECT _
 ) AS LONG

DECLARE FUNCTION GetLargestConsoleWindowSize LIB "KERNEL32.DLL" _
   ALIAS "GetLargestConsoleWindowSize" _
 ( BYVAL hConsoleOutput AS DWORD _
 ) AS DWORD

DECLARE FUNCTION GetConsoleTitleX LIB "KERNEL32.DLL" _
   ALIAS GetConsoleTitleAlias(1) _
 ( BYREF lpConsoleTitle AS zstring _
 , BYVAL nSize AS DWORD _
 ) AS DWORD

DECLARE FUNCTION CreateConsoleScreenBuffer LIB "KERNEL32.DLL" _
   ALIAS "CreateConsoleScreenBuffer" _
 ( BYVAL dwDesiredAccess AS DWORD _
 , BYVAL dwShareMode AS DWORD _
 , BYREF lpSecurityAttributes AS SECURITY_ATTRIBUTES_X _
 , BYVAL dwFlags AS DWORD _
 , BYVAL lpScreenBufferData AS DWORD _
 ) AS DWORD

DECLARE FUNCTION GetConsoleScreenBufferInfo LIB "KERNEL32.DLL" _
   ALIAS "GetConsoleScreenBufferInfo" _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYREF lpConsoleScreenBufferInfo AS CON_SCREEN_BUFFER_INFO _
 ) AS LONG

DECLARE FUNCTION SetConsoleActiveScreenBuffer LIB "KERNEL32.DLL" _
   ALIAS "SetConsoleActiveScreenBuffer" _
 ( BYVAL hConsoleOutput AS DWORD _
 ) AS LONG

DECLARE FUNCTION AllocConsole LIB "KERNEL32.DLL" _
   ALIAS "AllocConsole" _
() AS LONG

DECLARE FUNCTION FreeConsole LIB "KERNEL32.DLL" _
   ALIAS "FreeConsole" _
() AS LONG

DECLARE FUNCTION WriteConsoleX LIB "KERNEL32.DLL" _
   ALIAS WriteConsoleAlias(1) _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYVAL lpBuffer AS DWORD _
 , BYVAL nNumberOfCharsToWrite AS DWORD _
 , BYREF lpNumberOfCharsWritten AS DWORD _
 , BYVAL lpReserved AS DWORD _
 ) AS LONG

DECLARE FUNCTION GetNumberOfConsoleInputEvents LIB "KERNEL32.DLL" _
   ALIAS "GetNumberOfConsoleInputEvents" _
 ( BYVAL hConsoleInput AS DWORD _
 , BYREF lpNumberOfEvents AS DWORD _
 ) AS LONG

DECLARE FUNCTION ReadConsoleInputX LIB "KERNEL32.DLL" _
   ALIAS ReadConsoleInputAlias(1) _
 ( BYVAL hConsoleInput AS DWORD _
 , BYREF lpBuffer AS CON_INPUT_RECORD _
 , BYVAL nLength AS DWORD _
 , BYREF lpNumberOfEventsRead AS DWORD _
 ) AS LONG

DECLARE FUNCTION PeekConsoleInputX LIB "KERNEL32.DLL" _
   ALIAS PeekConsoleInputAlias(1) _
 ( BYVAL hConsoleInput AS DWORD _
 , BYREF lpBuffer AS CON_INPUT_RECORD _
 , BYVAL nLength AS DWORD _
 , BYREF lpNumberOfEventsRead AS DWORD _
 ) AS LONG

DECLARE FUNCTION FlushConsoleInputBuffer LIB "KERNEL32.DLL" _
   ALIAS "FlushConsoleInputBuffer" _
 ( BYVAL hConsoleInput AS DWORD _
 ) AS LONG

DECLARE FUNCTION SetConsoleWindowInfo LIB "KERNEL32.DLL" _
   ALIAS "SetConsoleWindowInfo" _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYVAL bAbsolute AS LONG _
 , BYREF lpConsoleWindow AS CON_RECT _
 ) AS LONG

DECLARE FUNCTION SetConsoleTitleX LIB "KERNEL32.DLL" _
   ALIAS SetConsoleTitleAlias(1) _
 ( BYREF lpConsoleTitle AS zstring _
 ) AS LONG

DECLARE FUNCTION GetConsoleCursorInfo LIB "KERNEL32.DLL" _
   ALIAS "GetConsoleCursorInfo" _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYREF lpConsoleCursorInfo AS CON_CURSOR_INFO _
 ) AS LONG

DECLARE FUNCTION SetConsoleCursorInfo LIB "KERNEL32.DLL" _
   ALIAS "SetConsoleCursorInfo" _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYREF lpConsoleCursorInfo AS CON_CURSOR_INFO _
 ) AS LONG

DECLARE FUNCTION ReadConsoleOutputAttributeX LIB "KERNEL32.DLL" _
   ALIAS "ReadConsoleOutputAttribute" _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYREF lpAttribute AS ANY _
 , BYVAL nLength AS DWORD _
 , BYVAL dwReadCoord AS CON_COORD _
 , BYREF lpNumberOfAttrsRead AS DWORD _
 ) AS LONG

DECLARE FUNCTION SetConsoleTextAttributeX LIB "KERNEL32.DLL" _
   ALIAS "SetConsoleTextAttribute" _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYVAL wAttributes AS WORD _
 ) AS LONG

DECLARE FUNCTION FillConsoleOutputAttributeX LIB "KERNEL32.DLL" _
   ALIAS "FillConsoleOutputAttribute" _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYVAL wAttribute AS WORD _
 , BYVAL nLength AS DWORD _
 , BYVAL dwWriteCoord AS CON_COORD _
 , BYREF lpNumberOfAttrsWritten AS DWORD _
 ) AS LONG

DECLARE FUNCTION ReadConsoleOutputCharacterX LIB "KERNEL32.DLL" _
   ALIAS ReadConsoleOutputCharacterAlias(1) _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYREF lpCharacter AS zstring _
 , BYVAL nLength AS DWORD _
 , BYVAL dwReadCoord AS CON_COORD _
 , BYREF lpNumberOfCharsRead AS DWORD _
 ) AS LONG

DECLARE FUNCTION ScrollConsoleScreenBufferX LIB "KERNEL32.DLL" _
   ALIAS ScrollConsoleScreenBufferAlias(1) _
 ( BYVAL hConsoleOutput AS DWORD _
 , BYREF lpScrollRectangle AS CON_RECT _
 , BYREF lpClipRectangle AS CON_RECT _
 , BYVAL dwDestinationOrigin AS CON_COORD _
 , BYREF lpFill AS CON_CHAR_INFO _
 ) AS LONG

DECLARE FUNCTION SetConsoleMode LIB "KERNEL32.DLL" _
   ALIAS "SetConsoleMode" _
 ( BYVAL hConsoleHandle AS DWORD  _
 , BYVAL dwMode AS DWORD _
 ) AS LONG

DECLARE FUNCTION WriteConsoleInputX LIB "KERNEL32.DLL" _
   ALIAS WriteConsoleInputAlias(1) _
 ( BYVAL hConsoleInput AS DWORD _
 , BYREF lpBuffer AS CON_INPUT_RECORD _
 , BYVAL nLength AS DWORD _
 , BYREF lpNumberOfEventsWritten AS DWORD _
 ) AS LONG

DECLARE FUNCTION GetNumberOfConsoleMouseButtons LIB "KERNEL32.DLL" _
   ALIAS "GetNumberOfConsoleMouseButtons" _
 ( BYREF lpNumberOfMouseButtons AS DWORD _
 ) AS LONG


FUNCTION ConStdOut ____________________________________________________________
                                  (OPTIONAL BYREF lgConsole AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Returns TRUE if standard output is going to the console.
'
' Internally, this function is also called to condition the flag.
'
'------------------------------------------------------------------------------

STATIC lgConsole_static AS LONG

IF VARPTR(lgConsole) > 0 THEN
  lgConsole_static = lgConsole
END IF
FUNCTION = lgConsole_static

END FUNCTION


FUNCTION ConStdIn _____________________________________________________________
                                  (OPTIONAL BYREF lgConsole AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Returns TRUE if standard input is coming from the local keyboard.
'
' Internally, this function is also called to condition the flag.
'
'------------------------------------------------------------------------------

STATIC lgConsole_static AS LONG

IF VARPTR(lgConsole) > 0 THEN
  lgConsole_static = lgConsole
END IF
FUNCTION = lgConsole_static

END FUNCTION


FUNCTION ConAvailable _________________________________________________________
                                    (OPTIONAL BYVAL lgReset AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Returns TRUE (1) if it is OK to create or use the console. Returns FALSE
' if functionality should be limited to standard input and output only.
'
' Returns FALSE if this code is part of an application that does not have
' the "console executable" flag set and the application was invoked from
' another console. If another console is created, any existing redirection
' of stdin or stdout will be lost, assigned to the new console.
'
' The optional parameter, if specified as non-zero, forces a recheck of the
' console status. This is used after the creation or destruction of a console
' is forced.
'
' The returned value from GetStdHandle(%STD_OUTPUT_HANDLE) is different for
' WinXP and Win7 when this is in a non-console app with no existing console:
' Win7 returns 0, WinXP returns 00010001h.
'
'------------------------------------------------------------------------------

STATIC ConCheck_static AS LONG

LOCAL lgLen AS LONG
LOCAL hPage AS LONG

IF lgReset > 0 THEN
  ConCheck_static = 0
END IF

IF ConCheck_static = 0 THEN
  IF (GetStdHandle(%STD_OUTPUT_HANDLE_X) < 1) _
  OR (GetStdHandle(%STD_OUTPUT_HANDLE_X) > &H0FFFF) THEN
    'GUI application with no existing console.
    'OK to create a new console.
    'No redirection is possible.
    'When the new console is created, all the
    'standard handles will be directed to it.
    ConCheck_static = 1
    ConStdOut %TRUE
    ConStdIn %TRUE
  ELSE
    'Standard output is already assigned.
    'Either a console was created by Windows at startup,
    'or this application was invoked from an existing console.
    hPage = CreateFileX("CONOUT$", %GENERIC_WRITE OR %GENERIC_READ_X, _
    %FILE_SHARE_WRITE, BYVAL %NULL, %OPEN_EXISTING, %NULL, %NULL)
    IF hPage > 0 THEN
      'Console application with an automatic or existing console.
      'AllocConsole will fail, but OK to use console.
      CloseHandle hPage
      ConCheck_static = 1
    ELSE
      'GUI application invoked from an existing console.
      'Limit functionality to stdin and stdout only.
      ConCheck_static = -1
    END IF
    IF GetStdHandle(%STD_OUTPUT_HANDLE_X) < 16 THEN
      ConStdOut %TRUE
    END IF
    IF GetStdHandle(%STD_INPUT_HANDLE_X) < 16 THEN
      ConStdIn %TRUE
    END IF
  END IF
END IF

IF ConCheck_static = -1 THEN
  FUNCTION = 0
ELSE
  FUNCTION = 1
END IF

END FUNCTION


FUNCTION ConWndHndl ___________________________________________________________
                                       (OPTIONAL BYREF hWnd AS DWORD          _
                                                          ) AS DWORD
'------------------------------------------------------------------------------
'
' Gets the Windows API handle of the console window.
'
' The GetConsoleWindow API is available from Windows 2000, but for
' compatibility with earlier versions, the EnumWindows method is used
' on the first call and the handle is stored in a static for return on
' subsequent calls.
'
'------------------------------------------------------------------------------

STATIC hConWnd_static AS DWORD

LOCAL szTitle AS ASCIIZ*64
LOCAL lgParam AS LONG

IF VARPTR(hWnd) > 0 THEN
  hConWnd_static = hWnd
ELSE
  IF hConWnd_static = 0 THEN
    EnumWindows CODEPTR(EnumWindowsCB), lgParam
  END IF
END IF

FUNCTION = hConWnd_static

END FUNCTION


FUNCTION EnumWindowsCB ________________________________________________________
                                                (BYVAL hWnd AS DWORD,         _
                                              BYVAL lgParam AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Internal callback function for EnumWindows. Search for the console window
' to get its handle. When it found, this function calls back to ConWndHndl
' to record it.
'
'------------------------------------------------------------------------------

LOCAL Index_LG AS LONG
LOCAL szCaption AS zstring*64
LOCAL szTitle AS zstring*64
LOCAL lgSize AS LONG

lgSize = SIZEOF(szTitle)
IF lgSize > 64 THEN lgSize = lgSize \ 2

GetConsoleTitleX szTitle, lgSize
IF szTitle = "" THEN EXIT FUNCTION
GetWindowTextX hWnd, szCaption, lgSize
IF szCaption = szTitle THEN
  ConWndHndl hWnd
  EXIT FUNCTION
END IF
FUNCTION = %TRUE

END FUNCTION


FUNCTION ConKbdHndl ___________________________________________________________
                                    (OPTIONAL BYREF hConInp AS DWORD          _
                                                          ) AS DWORD
'------------------------------------------------------------------------------
'
' Gets the Windows API handle of the keyboard attached to the console.
'
' Internally, this function is also called to record the handle number.
'
'------------------------------------------------------------------------------

STATIC hConInp_static AS DWORD

IF VARPTR(hConInp) > 0 THEN
  hConInp_static = hConInp
END IF
FUNCTION = hConInp_static

END FUNCTION


FUNCTION ConPageVisible _______________________________________________________
                              (OPTIONAL BYREF lgVisiblePage AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Sets the visible console buffer page number.
' Gets the visible console buffer page number.
'
'------------------------------------------------------------------------------

STATIC lgVisiblePage_static AS LONG

IF VARPTR(lgVisiblePage) > 0 THEN
  lgVisiblePage_static = lgVisiblePage
END IF
IF lgVisiblePage_static < 1 THEN
  lgVisiblePage_static = 1
ELSEIF lgVisiblePage_static > 8 THEN
  lgVisiblePage_static = 1
END IF
FUNCTION = lgVisiblePage_static

END FUNCTION


FUNCTION ConPageActive ________________________________________________________
                               (OPTIONAL BYREF lgActivePage AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Sets the active console buffer page number.
' Gets the active console buffer page number.
'
'------------------------------------------------------------------------------

STATIC lgActivePage_static AS LONG

IF VARPTR(lgActivePage) > 0 THEN
  lgActivePage_static = lgActivePage
END IF
IF lgActivePage_static < 1 THEN
  lgActivePage_static = 1
ELSEIF lgActivePage_static > 8 THEN
  lgActivePage_static = 1
END IF
FUNCTION = lgActivePage_static

END FUNCTION


FUNCTION ConPageHndl __________________________________________________________
                                           (BYVAL lgPageNum AS LONG,          _
                                     OPTIONAL BYREF hConOut AS DWORD          _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Gets the Windows API handle of the specified console output buffer page.
'
' Internally, this function is also called to record the handle numbers.
'
'------------------------------------------------------------------------------

STATIC hConOut_static() AS DWORD

IF UBOUND(hConOut_static) < 8 THEN
  REDIM hConOut_static(0 TO 8)
END IF

IF VARPTR(hConOut) > 0 THEN
  IF (lgPageNum > 0) AND (lgPageNum < 9) THEN
    hConOut_static(lgPageNum) = hConOut
  ELSE
    hConOut_static(1) = hConOut
  END IF
END IF

IF (lgPageNum > 0) AND (lgPageNum < 9) THEN
  FUNCTION = hConOut_static(lgPageNum)
ELSE
  FUNCTION = hConOut_static(ConPageActive())
END IF

END FUNCTION


FUNCTION ConPageVerify ________________________________________________________
                                           (BYVAL lgPageNum AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Internal function: Verifies whether the specified screen page buffer exists
' and creates it if it does not.
'
' Does nothing if PageNum& is less than 2 or greater than 8, or if an output
' page buffer numbered 2 through 8 has already been created.
'
' There should be no need for the user program to call this function directly
' since additional buffer pages are created implicitly with first use in other
' functions.
'
' A new buffer is made the same size as page 1. In PB, all buffer pages are
' the same size.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

IF (lgPageNum >= 2) AND (lgPageNum <= 8) THEN
  IF ConPageHndl(lgPageNum) = 0 THEN
    ConPageHndl lgPageNum, CreateConsoleScreenBuffer _
    (%GENERIC_WRITE OR %GENERIC_READ_X, %FILE_SHARE_WRITE, BYVAL %NULL, _
     %CONSOLE_TEXTMODE_BUFFER, %NULL)
    GetConsoleScreenBufferInfo ConPageHndl(1), BufferInfo
    SetConsoleScreenBufferSizeX ConPageHndl(lgPageNum), BufferInfo.dwSize
  END IF
  FUNCTION = %TRUE
END IF

END FUNCTION


FUNCTION ConSetPage ___________________________________________________________
                                        (BYVAL lgActivePage AS LONG,          _
                               OPTIONAL BYVAL lgVisiblePage AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Sets the active and visible console buffer page numbers.
'
'------------------------------------------------------------------------------

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew

IF (lgVisiblePage > 0) AND (lgVisiblePage <= 8) THEN
  ConPageVerify lgVisiblePage
  SetConsoleActiveScreenBuffer ConPageHndl(lgVisiblePage)
  ConPageVisible lgVisiblePage
END IF

IF (lgActivePage > 0) AND (lgActivePage <= 8) THEN
  ConPageVerify lgActivePage
  ConPageActive lgActivePage
END IF

END FUNCTION


FUNCTION ConPageCopy __________________________________________________________
                                        (BYVAL lgSourcePage AS LONG,          _
                                           BYVAL lgDestPage AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Copies all text and color attributes from one buffer page to another.
'
'------------------------------------------------------------------------------

LOCAL Origin AS CON_COORD
LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL CharInfoBuf() AS CON_CHAR_INFO
LOCAL PageRegion AS CON_RECT
LOCAL lgBufSize AS LONG

IF (lgSourcePage < 1) OR (lgSourcePage > 8) THEN EXIT FUNCTION
IF (lgDestPage < 1) OR (lgDestPage > 8) THEN EXIT FUNCTION
IF lgSourcePage = lgDestPage THEN EXIT FUNCTION

ConPageVerify lgSourcePage
IF ConPageHndl(lgSourcePage) = 0 THEN EXIT FUNCTION
ConPageVerify lgDestPage
IF ConPageHndl(lgDestPage) = 0 THEN EXIT FUNCTION

GetConsoleScreenBufferInfo ConPageHndl(lgSourcePage), BufferInfo
lgBufSize = BufferInfo.dwSize.x * BufferInfo.dwSize.y
REDIM CharInfoBuf(lgBufSize)

PageRegion.nRight = BufferInfo.dwSize.x - 1
PageRegion.nBottom = BufferInfo.dwSize.y - 1

ReadConsoleOutputX ConPageHndl(lgSourcePage), CharInfoBuf(0), _
                   BufferInfo.dwSize, Origin, PageRegion

WriteConsoleOutputX ConPageHndl(lgDestPage), CharInfoBuf(0), _
                    BufferInfo.dwSize, Origin, PageRegion

FUNCTION = 1

END FUNCTION


FUNCTION ConNew _______________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Creates the console window if it has not yet been created.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL wdAttributes AS WORD
LOCAL hDlg AS DWORD

IF ConKbdHndl() = 0 THEN
  AllocConsole
  ConPageHndl 1, CreateFileX("CONOUT$", %GENERIC_WRITE OR %GENERIC_READ_X, _
  %FILE_SHARE_WRITE, BYVAL %NULL, %OPEN_EXISTING, %NULL, %NULL)
  ConPageVisible 1
  ConPageActive 1
  ConKbdHndl CreateFileX("CONIN$", %GENERIC_READ_X OR %GENERIC_WRITE, _
  %FILE_SHARE_READ, BYVAL %NULL, %OPEN_EXISTING, %NULL, %NULL)
  SetConsoleMode ConKbdHndl(), %NULL
  'The following is to avoid a disconcerting "waiting" mouse cursor for
  'five seconds after the console is created. There must be a better way,
  'but experimentation with API functions failed to find it. Introducing
  'a DDT dialog adds 8KB (PB/Win8), 11KB (PB/Win9) or 34KB (PB/Win10) to
  'the program size.
  DIALOG NEW 0, "", 0, 0, 0, 0 TO hDlg
  DIALOG SHOW MODELESS hDlg
  DIALOG END hDlg
  ConAvailable 1 'Re-check console availability
  FUNCTION = 1
END IF

END FUNCTION


FUNCTION ConEnd _______________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Terminate the console window.
'
'------------------------------------------------------------------------------

LOCAL lgIdx AS LONG

FOR lgIdx = 1 TO 8
  IF ConPageHndl(lgIdx) THEN
    CloseHandle ConPageHndl(lgIdx)
    ConPageHndl lgIdx, 0
  END IF
NEXT lgIdx
CloseHandle ConKbdHndl
ConKbdHndl 0
FreeConsole()

END FUNCTION


FUNCTION ConSetWindowLoc ______________________________________________________
                                           (BYVAL lgPixelsX AS LONG,          _
                                            BYVAL lgPixelsY AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Moves the console window to a new location on the desktop.
'
'------------------------------------------------------------------------------

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew
SetWindowPos ConWndHndl(), 0, lgPixelsX, lgPixelsY, 0, 0, _
             %SWP_NOSIZE OR %SWP_NOZORDER

END FUNCTION


FUNCTION ConGetWindowLoc ______________________________________________________
                                           (BYREF lgPixelsX AS LONG,          _
                                            BYREF lgPixelsY AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Gets the location of the console window on the desktop.
'
'------------------------------------------------------------------------------

LOCAL tRect AS WIN_RECT

GetWindowRect ConWndHndl(), tRect
lgPixelsX = tRect.nLeft
lgPixelsY = tRect.nTop

END FUNCTION


FUNCTION ConWindowLeftLoc _____________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the x-coordinate of the location of the console window on the desktop.
'
'------------------------------------------------------------------------------

LOCAL tRect AS WIN_RECT

GetWindowRect ConWndHndl(), tRect
FUNCTION = tRect.nLeft

END FUNCTION


FUNCTION ConWindowTopLoc ______________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the y-coordinate of the location of the console window on the desktop.
'
'------------------------------------------------------------------------------

LOCAL tRect AS WIN_RECT

GetWindowRect ConWndHndl(), tRect
FUNCTION = tRect.nTop

END FUNCTION


FUNCTION ConGetWindowSize _____________________________________________________
                                           (BYREF lgPixelsX AS LONG,          _
                                            BYREF lgPixelsY AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Gets the size of the console window, in pixels.
'
'------------------------------------------------------------------------------

LOCAL tRect AS WIN_RECT

GetWindowRect ConWndHndl(), tRect
lgPixelsX = 1 + tRect.nRight - tRect.nLeft
lgPixelsY = 1 + tRect.nBottom - tRect.nTop

END FUNCTION


FUNCTION ConWindowWidth _______________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the width of the console window, in pixels.
'
'------------------------------------------------------------------------------

LOCAL tRect AS WIN_RECT

GetWindowRect ConWndHndl(), tRect
FUNCTION = 1 + tRect.nRight - tRect.nLeft

END FUNCTION


FUNCTION ConWindowHeight ______________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the height of the console window, in pixels.
'
'------------------------------------------------------------------------------

LOCAL tRect AS WIN_RECT

GetWindowRect ConWndHndl(), tRect
FUNCTION = 1 + tRect.nBottom - tRect.nTop

END FUNCTION


FUNCTION ConCaption ___________________________________________________________
                                  (OPTIONAL BYREF stCaption AS dstring        _
                                                          ) AS dstring
'------------------------------------------------------------------------------
'
' Sets the text to be displayed on the title bar of the console window.
' Gets the text which is displayed on the title bar of the console window.
'
'------------------------------------------------------------------------------

LOCAL szTitle AS zstring*64
LOCAL lgSize AS LONG

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew

lgSize = SIZEOF(szTitle)
IF lgSize > 64 THEN lgSize = lgSize \ 2

IF VARPTR(stCaption) > 0 THEN
  szTitle = stCaption
  SetConsoleTitleX szTitle
END IF
GetConsoleTitleX szTitle, lgSize
FUNCTION = szTitle

END FUNCTION


FUNCTION ConClear _____________________________________________________________
                                                            () AS LONG
'------------------------------------------------------------------------------
'
' Clears the active console page to the current background color.
'
'------------------------------------------------------------------------------

LOCAL Origin AS CON_COORD
LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL CharInfoBuf() AS CON_CHAR_INFO
LOCAL PageRegion AS CON_RECT
LOCAL lgBufSize AS LONG
LOCAL lgIdx AS LONG

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN
  FUNCTION = ConNew
  EXIT FUNCTION
END IF

GetConsoleScreenBufferInfo ConPageHndl(ConPageActive()), BufferInfo
lgBufSize = BufferInfo.dwSize.x * BufferInfo.dwSize.y
REDIM CharInfoBuf(lgBufSize)

PageRegion.nRight = BufferInfo.dwSize.x - 1
PageRegion.nBottom = BufferInfo.dwSize.y - 1

FOR lgIdx = 0 TO lgBufSize
  CharInfoBuf(lgIdx).Char = 32
  CharInfoBuf(lgIdx).Attributes = BufferInfo.wAttributes
NEXT lgIdx

WriteConsoleOutputX ConPageHndl(ConPageActive()), CharInfoBuf(0), _
                    BufferInfo.dwSize, Origin, PageRegion

BufferInfo.dwCursorPosition.x = 0
BufferInfo.dwCursorPosition.y = 0
SetConsoleCursorPositionX ConPageHndl(ConPageActive()), BufferInfo.dwCursorPosition

BufferInfo.srWindow.nRight = BufferInfo.srWindow.nRight - BufferInfo.srWindow.nLeft
BufferInfo.srWindow.nBottom = BufferInfo.srWindow.nBottom - BufferInfo.srWindow.nTop
BufferInfo.srWindow.nLeft = 0
BufferInfo.srWindow.nTop = 0
SetConsoleWindowInfo ConPageHndl(ConPageActive()), %TRUE, BufferInfo.srWindow

FUNCTION = 1

END FUNCTION


FUNCTION ConSetScreenSize _____________________________________________________
                                              (BYVAL lgRows AS LONG,          _
                                               BYVAL lgCols AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Changes the size of the visible console window, and its screen buffers.
'
'------------------------------------------------------------------------------

LOCAL tRect AS WIN_RECT
LOCAL MaxSize AS COORD_UNION
LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL lgOldScreenW AS LONG
LOCAL lgOldScreenH AS LONG
LOCAL lgMaxScreenW AS LONG
LOCAL lgMaxScreenH AS LONG
LOCAL lgOldBufferW AS LONG
LOCAL lgOldBufferH AS LONG
LOCAL lgIdx AS LONG
LOCAL lgOldW, lgOldH AS LONG
LOCAL lgActivePage AS LONG

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew

' For PB/CC compatibility...

lgActivePage = ConPageActive
FOR lgIdx = 1 TO 8
  IF ConPageHndl(lgIdx) THEN
    ConSetPage lgIdx
    ConClear
  END IF
NEXT lgIdx
ConSetPage lgActivePage

GetConsoleScreenBufferInfo ConPageHndl(1), BufferInfo
lgOldScreenW = 1 + BufferInfo.srWindow.nRight - BufferInfo.srWindow.nLeft
lgOldScreenH = 1 + BufferInfo.srWindow.nBottom - BufferInfo.srWindow.nTop

lgOldBufferW = BufferInfo.dwSize.x
lgOldBufferH = BufferInfo.dwSize.y

IF lgCols = lgOldScreenW THEN
  IF lgRows = lgOldScreenH THEN
    IF lgCols = lgOldBufferW THEN
      IF lgRows = lgOldBufferH THEN
        EXIT FUNCTION
      END IF
    END IF
  END IF
END IF

MaxSize.dw = GetLargestConsoleWindowSize(ConPageHndl(1))
lgMaxScreenW = MaxSize.wd.x
lgMaxScreenH = MaxSize.wd.y
IF lgCols > lgMaxScreenW THEN EXIT FUNCTION
IF lgRows > lgMaxScreenH THEN EXIT FUNCTION
IF lgCols < 8 THEN EXIT FUNCTION
IF lgRows < 3 THEN EXIT FUNCTION

GetWindowRect ConWndHndl(), tRect
lgOldW = 1 + tRect.nRight - tRect.nLeft
lgOldH = 1 + tRect.nBottom - tRect.nTop

IF lgCols < lgOldScreenW THEN 'Window is being reduced in width:
  'First reduce the window width:
  BufferInfo.srWindow.nRight = BufferInfo.srWindow.nLeft + lgCols - 1
  SetConsoleWindowInfo ConPageHndl(1), %TRUE, BufferInfo.srWindow
  'Then reduce the width of all allocated buffers
  BufferInfo.dwSize.x = lgCols
  FOR lgIdx = 1 TO 8
    IF ConPageHndl(lgIdx) THEN
      SetConsoleScreenBufferSizeX ConPageHndl(lgIdx), BufferInfo.dwSize
    END IF
  NEXT lgIdx
ELSEIF lgCols > lgOldScreenW THEN 'Window is being increased in width:
  'First, if necessary, change the width of all allocated buffers:
  IF lgCols <> BufferInfo.dwSize.x THEN
    BufferInfo.dwSize.x = lgCols
    FOR lgIdx = 1 TO 8
      IF ConPageHndl(lgIdx) THEN
        SetConsoleScreenBufferSizeX ConPageHndl(lgIdx), BufferInfo.dwSize
      END IF
    NEXT lgIdx
  END IF
  'Then increase the window width
  BufferInfo.srWindow.nRight = BufferInfo.srWindow.nLeft + lgCols - 1
  SetConsoleWindowInfo ConPageHndl(1), %TRUE, BufferInfo.srWindow
ELSEIF lgCols <> BufferInfo.dwSize.x THEN
  BufferInfo.dwSize.x = lgCols
  FOR lgIdx = 1 TO 8
    IF ConPageHndl(lgIdx) THEN
      SetConsoleScreenBufferSizeX ConPageHndl(lgIdx), BufferInfo.dwSize
    END IF
  NEXT lgIdx
END IF

IF lgRows < lgOldScreenH THEN 'Window is being reduced in height:
  'First reduce the window height:
  BufferInfo.srWindow.nBottom = BufferInfo.srWindow.nTop + lgRows - 1
  SetConsoleWindowInfo ConPageHndl(1), %TRUE, BufferInfo.srWindow
  'Then reduce the height of all allocated buffers
  BufferInfo.dwSize.y = lgRows
  FOR lgIdx = 1 TO 8
    IF ConPageHndl(lgIdx) THEN
      SetConsoleScreenBufferSizeX ConPageHndl(lgIdx), BufferInfo.dwSize
    END IF
  NEXT lgIdx
ELSEIF lgRows > lgOldScreenH THEN 'Window is being increased in height:
  'First, if necessary, change the height of all allocated buffers:
  IF lgRows <> BufferInfo.dwSize.y THEN
    BufferInfo.dwSize.y = lgRows
    FOR lgIdx = 1 TO 8
      IF ConPageHndl(lgIdx) THEN
        SetConsoleScreenBufferSizeX ConPageHndl(lgIdx), BufferInfo.dwSize
      END IF
    NEXT lgIdx
  END IF
  'Then increase the window height
  BufferInfo.srWindow.nBottom = BufferInfo.srWindow.nTop + lgRows - 1
  SetConsoleWindowInfo ConPageHndl(1), %TRUE, BufferInfo.srWindow
ELSEIF lgRows <> BufferInfo.dwSize.y THEN
  BufferInfo.dwSize.y = lgRows
  FOR lgIdx = 1 TO 8
    IF ConPageHndl(lgIdx) THEN
      SetConsoleScreenBufferSizeX ConPageHndl(lgIdx), BufferInfo.dwSize
    END IF
  NEXT lgIdx
END IF

' If GetWindowRect (via ConGetWindowSize) is called immediately after
' SetConsoleWindowInfo (via ConSetScreenSize, this function) the old
' window size can still be reported. The following code ensures that
' the screen size change has rippled through to the window size report
' before this function returns.

IF (lgRows <> lgOldScreenH) OR (lgCols <> lgOldScreenW) THEN
  DO
    GetWindowRect ConWndHndl(), tRect
    IF lgRows = lgOldScreenH THEN      'Width change only:
      IF lgOldW <> (1 + tRect.nRight - tRect.nLeft) THEN EXIT DO
    ELSEIF lgCols = lgOldScreenW THEN 'Height change only:
      IF lgOldH <> (1 + tRect.nBottom - tRect.nTop) THEN EXIT DO
    ELSE                         'Width and height change:
      IF lgOldW <> (1 + tRect.nRight - tRect.nLeft) THEN
        IF lgOldH <> (1 + tRect.nBottom - tRect.nTop) THEN
          EXIT DO
        END IF
      END IF
    END IF
    SLEEP 1
  LOOP
END IF

FUNCTION = 1

END FUNCTION


FUNCTION ConGetScreenSize _____________________________________________________
                                              (BYREF lgRows AS LONG,          _
                                               BYREF lgCols AS LONG           _
                                                          ) AS STRING
'------------------------------------------------------------------------------
'
' Gets the size of the visible part of the console window.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew
GetConsoleScreenBufferInfo ConPageHndl(1), BufferInfo
lgCols = 1 + BufferInfo.srWindow.nRight - BufferInfo.srWindow.nLeft
lgRows = 1 + BufferInfo.srWindow.nBottom - BufferInfo.srWindow.nTop

END FUNCTION


FUNCTION ConGetScreenRows _____________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the height of the visible part of the console window, in text rows.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew
GetConsoleScreenBufferInfo ConPageHndl(1), BufferInfo
FUNCTION = 1 + BufferInfo.srWindow.nBottom - BufferInfo.srWindow.nTop

END FUNCTION


FUNCTION ConGetScreenCols _____________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the width of the visible part of the console window, in text columns.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew
GetConsoleScreenBufferInfo ConPageHndl(1), BufferInfo
FUNCTION = 1 + BufferInfo.srWindow.nRight - BufferInfo.srWindow.nLeft

END FUNCTION


FUNCTION ConGetMaxScreenSize __________________________________________________
                                              (BYREF lgRows AS LONG,          _
                                               BYREF lgCols AS LONG           _
                                                          ) AS STRING
'------------------------------------------------------------------------------
'
' Gets the maximum size that could be set for the console window.
'
'------------------------------------------------------------------------------

LOCAL MaxSize AS COORD_UNION

MaxSize.dw = GetLargestConsoleWindowSize(ConPageHndl(1))
lgCols = MaxSize.wd.x
lgRows = MaxSize.wd.y

END FUNCTION


FUNCTION ConSetPageSize _______________________________________________________
                                              (BYVAL lgRows AS LONG,          _
                                               BYVAL lgCols AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Sets the size of the console output page buffers.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL lgIdx AS LONG

GetConsoleScreenBufferInfo ConPageHndl(1), BufferInfo
IF lgCols < (1 + BufferInfo.srWindow.nRight - BufferInfo.srWindow.nLeft) THEN
  EXIT FUNCTION
END IF
IF lgRows < (1 + BufferInfo.srWindow.nBottom - BufferInfo.srWindow.nTop) THEN
  EXIT FUNCTION
END IF

IF lgCols = BufferInfo.dwSize.x THEN
  IF lgRows = BufferInfo.dwSize.y THEN
    EXIT FUNCTION
  END IF
END IF

BufferInfo.dwSize.x = lgCols
BufferInfo.dwSize.y = lgRows
FOR lgIdx = 1 TO 8
  IF ConPageHndl(lgIdx) THEN
    SetConsoleScreenBufferSizeX ConPageHndl(lgIdx), BufferInfo.dwSize
  END IF
NEXT lgIdx

END FUNCTION


FUNCTION ConGetPageSize _______________________________________________________
                                              (BYREF lgRows AS LONG,          _
                                               BYREF lgCols AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Gets the size of the console output page buffers.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

GetConsoleScreenBufferInfo ConPageHndl(1), BufferInfo
lgCols = BufferInfo.dwSize.x
lgRows = BufferInfo.dwSize.y

END FUNCTION


FUNCTION ConPageRows _______________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the height of the console page buffers, in text rows.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

GetConsoleScreenBufferInfo ConPageHndl(1), BufferInfo
FUNCTION = BufferInfo.dwSize.y

END FUNCTION


FUNCTION ConPageCols _______________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the width of the console page buffers, in text columns.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

GetConsoleScreenBufferInfo ConPageHndl(1), BufferInfo
FUNCTION = BufferInfo.dwSize.x

END FUNCTION


FUNCTION ConCursor ____________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Sets the cursor (caret) height.
'
'------------------------------------------------------------------------------

LOCAL ConsoleCursorInfo AS CON_CURSOR_INFO

GetConsoleCursorInfo ConPageHndl(1), ConsoleCursorInfo
IF ConsoleCursorInfo.bVisible THEN
  FUNCTION = ConsoleCursorInfo.dwSize
END IF

END FUNCTION


FUNCTION ConCursorOff _________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Sets the cursor (caret) visibility state to invisible.
'
'------------------------------------------------------------------------------

LOCAL ConsoleCursorInfo AS CON_CURSOR_INFO

GetConsoleCursorInfo ConPageHndl(1), ConsoleCursorInfo
ConsoleCursorInfo.bVisible = %FALSE
SetConsoleCursorInfo ConPageHndl(1), ConsoleCursorInfo

END FUNCTION


FUNCTION ConCursorOn __________________________________________________________
                                (OPTIONAL BYVAL dwCaretSize AS DWORD          _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Makes the cursor (caret) visible and, optionally, defines the cursor height.
'
'------------------------------------------------------------------------------

LOCAL ConsoleCursorInfo AS CON_CURSOR_INFO

GetConsoleCursorInfo ConPageHndl(1), ConsoleCursorInfo
ConsoleCursorInfo.bVisible = %TRUE
IF (dwCaretSize > 0) AND (dwCaretSize <= 100) THEN
  ConsoleCursorInfo.dwSize = dwCaretSize
END IF
SetConsoleCursorInfo ConPageHndl(1), ConsoleCursorInfo

END FUNCTION


FUNCTION ConSetPos ____________________________________________________________
                                               (BYVAL lgRow AS LONG,          _
                                                BYVAL lgCol AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Sets the cursor (caret) position for the active console page.
'
'------------------------------------------------------------------------------

LOCAL Position AS CON_COORD

Position.x = lgCol - 1
Position.y = lgRow - 1
SetConsoleCursorPositionX ConPageHndl(ConPageActive()), Position

END FUNCTION


FUNCTION ConGetPos ____________________________________________________________
                                               (BYREF lgRow AS LONG,          _
                                                BYREF lgCol AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Gets the cursor (caret) position for the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

GetConsoleScreenBufferInfo ConPageHndl(ConPageActive()), BufferInfo
lgRow = BufferInfo.dwCursorPosition.y + 1
lgCol = BufferInfo.dwCursorPosition.x + 1

END FUNCTION


FUNCTION ConPosRow ____________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the vertical cursor (caret) position for the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

GetConsoleScreenBufferInfo ConPageHndl(ConPageActive()), BufferInfo
FUNCTION = BufferInfo.dwCursorPosition.y + 1

END FUNCTION


FUNCTION ConPosCol ____________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the horizontal cursor (caret) position for the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

GetConsoleScreenBufferInfo ConPageHndl(ConPageActive()), BufferInfo
FUNCTION = BufferInfo.dwCursorPosition.x + 1

END FUNCTION


FUNCTION ConTab _______________________________________________________________
                                   (OPTIONAL BYVAL lgColumn AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Moves the print position to a new column.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL lgCol AS LONG
LOCAL lgZone AS LONG

GetConsoleScreenBufferInfo ConPageHndl(ConPageActive()), BufferInfo
IF lgColumn > 0 THEN
  lgCol = lgColumn - 1
  IF lgCol <= BufferInfo.dwCursorPosition.x THEN
    INCR BufferInfo.dwCursorPosition.y
  END IF
ELSE
  lgCol = BufferInfo.dwCursorPosition.x
  lgZone = lgCol \ 14
  INCR lgZone
  lgCol = lgZone * 14
END IF
DO WHILE lgCol > BufferInfo.dwSize.x
  lgCol = lgCol - BufferInfo.dwSize.x
  INCR BufferInfo.dwCursorPosition.y
LOOP
BufferInfo.dwCursorPosition.x = lgCol
SetConsoleCursorPositionX ConPageHndl(ConPageActive()), BufferInfo.dwCursorPosition

END FUNCTION


FUNCTION ConColor _____________________________________________________________
                                           (BYVAL lgFgColor AS LONG,          _
                                            BYVAL lgBgColor AS LONG,          _
                                     OPTIONAL BYVAL lgCount AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Sets the text foreground and background colors for the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL wdAttributes AS WORD
LOCAL dwLength AS DWORD
LOCAL btFg, btBg AS BYTE

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew

GetConsoleScreenBufferInfo ConPageHndl(ConPageActive()), BufferInfo
IF (lgFgColor < 0) OR (lgFgColor > 15) THEN
  btFg = BufferInfo.wAttributes
  btFg = btFg AND &H0F
ELSE
  btFg = lgFgColor
END IF
IF (lgBgColor < 0) OR (lgBgColor > 15) THEN
  btBg = BufferInfo.wAttributes
  btBg = (btBg AND &HF0) \ 16
ELSE
  btBg = lgBgColor
END IF
wdAttributes = (btBg * 16) + btFg
SetConsoleTextAttributeX ConPageHndl(ConPageActive()), wdAttributes
IF lgCount < 1 THEN EXIT FUNCTION
FillConsoleOutputAttributeX ConPageHndl(ConPageActive()), wdAttributes, _
  lgCount, BufferInfo.dwCursorPosition, dwLength

END FUNCTION


FUNCTION ConSetAttr ___________________________________________________________
                                            (BYVAL lgColors AS LONG,          _
                                     OPTIONAL BYVAL lgCount AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Sets the color attribute byte for the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL wdAttributes AS WORD
LOCAL dwLength AS DWORD

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew

GetConsoleScreenBufferInfo ConPageHndl(ConPageActive()), BufferInfo
wdAttributes = lgColors AND &H0FF
SetConsoleTextAttributeX ConPageHndl(ConPageActive()), wdAttributes
IF lgCount < 1 THEN EXIT FUNCTION
FillConsoleOutputAttributeX ConPageHndl(ConPageActive()), wdAttributes, _
  lgCount, BufferInfo.dwCursorPosition, dwLength

END FUNCTION


FUNCTION ConCellAttr __________________________________________________________
                                               (BYVAL lgRow AS LONG,          _
                                                BYVAL lgCol AS LONG           _
                                                          ) AS BYTE
'------------------------------------------------------------------------------
'
' Gets a color attribute byte for the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL wdAttributes AS WORD
LOCAL Position AS CON_COORD
LOCAL dwLength AS DWORD

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew

IF (lgRow < 1) OR (lgCol < 1) THEN
  GetConsoleScreenBufferInfo ConPageHndl(ConPageActive()), BufferInfo
  FUNCTION = BufferInfo.wAttributes
  EXIT FUNCTION
END IF

Position.x = lgCol - 1
Position.y = lgRow - 1
dwLength = 1
ReadConsoleOutputAttributeX ConPageHndl(ConPageActive()), wdAttributes, _
  dwLength, Position, dwLength
FUNCTION = wdAttributes

END FUNCTION


FUNCTION ConCellChar __________________________________________________________
                                               (BYVAL lgRow AS LONG,          _
                                                BYVAL lgCol AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Gets the numeric code of a character on the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL Position AS CON_COORD
LOCAL lgLength AS LONG
LOCAL szChar AS zstring*8

IF (lgCol < 1) OR (lgRow < 1) THEN EXIT FUNCTION
GetConsoleScreenBufferInfo ConPageHndl(ConPageHndl(ConPageActive())), BufferInfo
IF lgCol > BufferInfo.dwSize.x THEN EXIT FUNCTION
IF lgRow > BufferInfo.dwSize.y THEN EXIT FUNCTION

Position.y = lgRow - 1
Position.x = lgCol - 1

ReadConsoleOutputCharacterX ConPageHndl(ConPageActive()), _
  szChar, 1, Position, lgLength
FUNCTION = CVWRD(szChar)

END FUNCTION


FUNCTION ConSetPageView _______________________________________________________
                                               (BYVAL lgRow AS LONG,          _
                                                BYVAL lgCol AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Sets the upper left position of the visible page in the console window.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL lgPageW AS LONG
LOCAL lgPageH AS LONG
LOCAL lgWindW AS LONG
LOCAL lgWindH AS LONG

IF (lgCol < 1) OR (lgRow < 1) THEN EXIT FUNCTION
GetConsoleScreenBufferInfo ConPageHndl(ConPageHndl(ConPageVisible())), BufferInfo
lgPageW = BufferInfo.dwSize.x
lgPageH = BufferInfo.dwSize.y
lgWindW = 1 + BufferInfo.srWindow.nRight - BufferInfo.srWindow.nLeft
lgWindH = 1 + BufferInfo.srWindow.nBottom - BufferInfo.srWindow.nTop
IF (lgPageW = lgWindW) AND (lgPageH = lgWindH) THEN EXIT FUNCTION

DECR lgCol
DECR lgRow
BufferInfo.srWindow.nLeft = lgCol
BufferInfo.srWindow.nTop = lgRow
BufferInfo.srWindow.nRight = lgCol + lgWindW - 1
BufferInfo.srWindow.nBottom = lgRow + lgWindH - 1
IF BufferInfo.srWindow.nRight > (lgPageW - 1) THEN
  BufferInfo.srWindow.nRight = lgPageW - 1
  BufferInfo.srWindow.nLeft = lgPageW - lgWindW
END IF
IF BufferInfo.srWindow.nBottom > (lgPageH - 1) THEN
  BufferInfo.srWindow.nBottom = lgPageH - 1
  BufferInfo.srWindow.nTop = lgPageH - lgWindH
END IF
SetConsoleWindowInfo ConPageHndl(ConPageHndl(ConPageVisible())), _
  %TRUE, BufferInfo.srWindow

END FUNCTION


FUNCTION ConGetPageView _______________________________________________________
                                               (BYREF lgRow AS LONG,          _
                                                BYREF lgCol AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Gets the upper left position of the visible page in the console window.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew
GetConsoleScreenBufferInfo ConPageHndl(ConPageHndl(ConPageVisible())), BufferInfo
lgCol = 1 + BufferInfo.srWindow.nLeft
lgRow = 1 + BufferInfo.srWindow.nTop
FUNCTION = 1

END FUNCTION


FUNCTION ConPageViewRow _______________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the top visible page row number in the console window.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew
GetConsoleScreenBufferInfo ConPageHndl(ConPageHndl(ConPageVisible())), BufferInfo
FUNCTION = 1 + BufferInfo.srWindow.nTop

END FUNCTION


FUNCTION ConPageViewCol _______________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the left-side visible page column number in the console window.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew
GetConsoleScreenBufferInfo ConPageHndl(ConPageHndl(ConPageVisible())), BufferInfo
FUNCTION = 1 + BufferInfo.srWindow.nLeft

END FUNCTION


FUNCTION ConScrollUp __________________________________________________________
                                    (OPTIONAL BYVAL lgCells AS LONG,          _
                                      OPTIONAL BYVAL lgRow1 AS LONG,          _
                                               BYVAL lgCol1 AS LONG,          _
                                      OPTIONAL BYVAL lgRow2 AS LONG,          _
                                               BYVAL lgCol2 AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Moves all or part of the character cells up on the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL SourceRect AS CON_RECT
LOCAL DestOrigin AS CON_COORD
LOCAL FillChar AS CON_CHAR_INFO
LOCAL lgShift AS LONG

GetConsoleScreenBufferInfo ConPageHndl(ConPageHndl(ConPageActive())), BufferInfo
FillChar.Attributes = BufferInfo.wAttributes
FillChar.Char = 32

SourceRect.nTop = 0
SourceRect.nLeft = 0
SourceRect.nBottom = BufferInfo.dwSize.y - 1
SourceRect.nRight = BufferInfo.dwSize.x - 1
lgShift = 1

IF lgCells > 0 THEN lgShift = lgCells
IF lgRow1 > 0 THEN SourceRect.nTop = lgRow1 - 1
IF lgCol1 > 0 THEN SourceRect.nLeft = lgCol1 - 1
IF lgRow2 > 0 THEN SourceRect.nBottom = lgRow2 - 1
IF lgCol2 > 0 THEN SourceRect.nRight = lgCol2 - 1

DestOrigin.y = SourceRect.nTop
DestOrigin.x = SourceRect.nLeft

SourceRect.nTop = SourceRect.nTop + lgShift
'SourceRect.nBottom = SourceRect.nBottom + lgShift

ScrollConsoleScreenBufferX ConPageHndl(ConPageHndl(ConPageActive())), _
  SourceRect, BYVAL %NULL, DestOrigin, FillChar

END FUNCTION


FUNCTION ConScrollDown ________________________________________________________
                                    (OPTIONAL BYVAL lgCells AS LONG,          _
                                      OPTIONAL BYVAL lgRow1 AS LONG,          _
                                               BYVAL lgCol1 AS LONG,          _
                                      OPTIONAL BYVAL lgRow2 AS LONG,          _
                                               BYVAL lgCol2 AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Moves all or part of the character cells down on the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL SourceRect AS CON_RECT
LOCAL DestOrigin AS CON_COORD
LOCAL FillChar AS CON_CHAR_INFO
LOCAL lgShift AS LONG

GetConsoleScreenBufferInfo ConPageHndl(ConPageHndl(ConPageActive())), BufferInfo
FillChar.Attributes = BufferInfo.wAttributes
FillChar.Char = 32

SourceRect.nTop = 0
SourceRect.nLeft = 0
SourceRect.nBottom = BufferInfo.dwSize.y - 1
SourceRect.nRight = BufferInfo.dwSize.x - 1
lgShift = 1

IF lgCells > 0 THEN lgShift = lgCells
IF lgRow1 > 0 THEN SourceRect.nTop = lgRow1 - 1
IF lgCol1 > 0 THEN SourceRect.nLeft = lgCol1 - 1
IF lgRow2 > 0 THEN SourceRect.nBottom = lgRow2 - 1
IF lgCol2 > 0 THEN SourceRect.nRight = lgCol2 - 1

DestOrigin.y = SourceRect.nTop
DestOrigin.x = SourceRect.nLeft

DestOrigin.y = SourceRect.nTop + lgShift

ScrollConsoleScreenBufferX ConPageHndl(ConPageHndl(ConPageActive())), _
  SourceRect, BYVAL %NULL, DestOrigin, FillChar

END FUNCTION


FUNCTION ConScrollLeft ________________________________________________________
                                    (OPTIONAL BYVAL lgCells AS LONG,          _
                                      OPTIONAL BYVAL lgRow1 AS LONG,          _
                                               BYVAL lgCol1 AS LONG,          _
                                      OPTIONAL BYVAL lgRow2 AS LONG,          _
                                               BYVAL lgCol2 AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Moves all or part of the character cells left on the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL SourceRect AS CON_RECT
LOCAL DestOrigin AS CON_COORD
LOCAL FillChar AS CON_CHAR_INFO
LOCAL lgShift AS LONG

GetConsoleScreenBufferInfo ConPageHndl(ConPageHndl(ConPageActive())), BufferInfo
FillChar.Attributes = BufferInfo.wAttributes
FillChar.Char = 32

SourceRect.nTop = 0
SourceRect.nLeft = 0
SourceRect.nBottom = BufferInfo.dwSize.y - 1
SourceRect.nRight = BufferInfo.dwSize.x - 1
lgShift = 1

IF lgCells > 0 THEN lgShift = lgCells
IF lgRow1 > 0 THEN SourceRect.nTop = lgRow1 - 1
IF lgCol1 > 0 THEN SourceRect.nLeft = lgCol1 - 1
IF lgRow2 > 0 THEN SourceRect.nBottom = lgRow2 - 1
IF lgCol2 > 0 THEN SourceRect.nRight = lgCol2 - 1

DestOrigin.y = SourceRect.nTop
DestOrigin.x = SourceRect.nLeft

SourceRect.nLeft = SourceRect.nLeft + lgShift

ScrollConsoleScreenBufferX ConPageHndl(ConPageHndl(ConPageActive())), _
  SourceRect, BYVAL %NULL, DestOrigin, FillChar

END FUNCTION


FUNCTION ConScrollRight _______________________________________________________
                                    (OPTIONAL BYVAL lgCells AS LONG,          _
                                      OPTIONAL BYVAL lgRow1 AS LONG,          _
                                               BYVAL lgCol1 AS LONG,          _
                                      OPTIONAL BYVAL lgRow2 AS LONG,          _
                                               BYVAL lgCol2 AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Moves all or part of the character cells right on the active console page.
'
'------------------------------------------------------------------------------

LOCAL BufferInfo AS CON_SCREEN_BUFFER_INFO
LOCAL SourceRect AS CON_RECT
LOCAL DestOrigin AS CON_COORD
LOCAL FillChar AS CON_CHAR_INFO
LOCAL lgShift AS LONG

GetConsoleScreenBufferInfo ConPageHndl(ConPageHndl(ConPageActive())), BufferInfo
FillChar.Attributes = BufferInfo.wAttributes
FillChar.Char = 32

SourceRect.nTop = 0
SourceRect.nLeft = 0
SourceRect.nBottom = BufferInfo.dwSize.y - 1
SourceRect.nRight = BufferInfo.dwSize.x - 1
lgShift = 1

IF lgCells > 0 THEN lgShift = lgCells
IF lgRow1 > 0 THEN SourceRect.nTop = lgRow1 - 1
IF lgCol1 > 0 THEN SourceRect.nLeft = lgCol1 - 1
IF lgRow2 > 0 THEN SourceRect.nBottom = lgRow2 - 1
IF lgCol2 > 0 THEN SourceRect.nRight = lgCol2 - 1

DestOrigin.y = SourceRect.nTop
DestOrigin.x = SourceRect.nLeft

DestOrigin.x = SourceRect.nLeft + lgShift
SourceRect.nRight = SourceRect.nRight - lgShift

ScrollConsoleScreenBufferX ConPageHndl(ConPageHndl(ConPageActive())), _
  SourceRect, BYVAL %NULL, DestOrigin, FillChar

END FUNCTION


FUNCTION ConMouseOn ___________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Enable mouse event reports from ConInKey$ and ConWaitKey$.
'
'------------------------------------------------------------------------------

SetConsoleMode ConKbdHndl(), %ENABLE_MOUSE_INPUT

END FUNCTION


FUNCTION ConMouseOff ___________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Disable mouse event reports from ConInKey$ and ConWaitKey$.
'
'------------------------------------------------------------------------------

SetConsoleMode ConKbdHndl(), %NULL

END FUNCTION


FUNCTION ConMouseButtonMask ___________________________________________________
                                     (OPTIONAL BYREF lgMask AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Internal function: Gets/Sets the bit-mask for mouse buttons to be reported.
'
'------------------------------------------------------------------------------

STATIC lgMask_static AS LONG

IF VARPTR(lgMask) > 0 THEN
  lgMask_static = lgMask
END IF
FUNCTION = lgMask_static

END FUNCTION


FUNCTION ConMouseEventMask ____________________________________________________
                                     (OPTIONAL BYREF lgMask AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Internal function: Gets/Sets the bit-mask for mouse events to be reported.
'
'------------------------------------------------------------------------------

STATIC lgMask_static AS LONG

IF VARPTR(lgMask) > 0 THEN
  lgMask_static = lgMask
END IF
FUNCTION = lgMask_static

END FUNCTION


FUNCTION ConMouse _____________________________________________________________
                                         (BYVAL lgEventCode AS LONG,          _
                                OPTIONAL BYVAL lgButtonCode AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Specifies the type of events which will trigger mouse reports.
'
'------------------------------------------------------------------------------

ConMouseEventMask (lgEventCode AND 15)
IF (lgButtonCode AND 31) = 0 THEN
  ConMouseButtonMask 1
ELSE
  ConMouseButtonMask (lgButtonCode AND 31)
END IF

END FUNCTION


FUNCTION ConMouseButtons ______________________________________________________
                                                     () AS LONG
'------------------------------------------------------------------------------
'
' Gets the number of buttons on the mouse used by the console.
'
'------------------------------------------------------------------------------

LOCAL dwButtons AS DWORD

GetNumberOfConsoleMouseButtons dwButtons
FUNCTION = dwButtons

END FUNCTION


FUNCTION ConMouseRow __________________________________________________________
                                      (OPTIONAL BYREF inRow AS INTEGER        _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Gets the row position of the most recently reported mouse event.
'
' Internally, this function is also called to store the row value.
'
'------------------------------------------------------------------------------

STATIC lgRow_static AS LONG

IF VARPTR(inRow) > 0 THEN
  lgRow_static = inRow
END IF
FUNCTION = lgRow_static + 1

END FUNCTION


FUNCTION ConMouseCol __________________________________________________________
                                      (OPTIONAL BYREF inCol AS INTEGER        _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Gets the column position of the most recently reported mouse event.
'
' Internally, this function is also called to store the column value.
'
'------------------------------------------------------------------------------

STATIC lgCol_static AS LONG

IF VARPTR(inCol) > 0 THEN
  lgCol_static = inCol
END IF
FUNCTION = lgCol_static + 1

END FUNCTION


FUNCTION ConSetFocus __________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Brings the console to the foreground and directs the keyboard focus to it.
'
'------------------------------------------------------------------------------

SetForegroundWindow(ConWndHndl())

END FUNCTION


FUNCTION ConFuncKey ___________________________________________________________
                                            (BYREF lgKeyNum AS LONG,          _
                                   OPTIONAL BYREF stKeyText AS STRING         _
                                                          ) AS STRING
'------------------------------------------------------------------------------
'
' Assigns a string to a function key.
' Gets the string assigned to a function key.
'
'------------------------------------------------------------------------------

STATIC stKeyText_static() AS STRING

IF UBOUND(stKeyText_static) < 12 THEN
  REDIM stKeyText_static(12)
END IF

IF lgKeyNum < 1 THEN EXIT FUNCTION
IF lgKeyNum > 12 THEN
  IF lgKeyNum = 30 THEN
    lgKeyNum = 11
  ELSEIF lgKeyNum = 31 THEN
    lgKeyNum = 12
  ELSE
    EXIT FUNCTION
  END IF
END IF

IF VARPTR(stKeyText) > 0 THEN
  stKeyText_static(lgKeyNum) = stKeyText
END IF
FUNCTION = stKeyText_static(lgKeyNum)

END FUNCTION


FUNCTION StdErrHndl ___________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the Windows API handle of the standard error device.
'
'------------------------------------------------------------------------------

FUNCTION = GetStdHandle(%STD_ERROR_HANDLE_X)

END FUNCTION


FUNCTION StdInHndl ____________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Gets the Windows API handle of the standard input device.
'
'------------------------------------------------------------------------------

FUNCTION = GetStdHandle(%STD_INPUT_HANDLE_X)

END FUNCTION


FUNCTION StdOutHndl ___________________________________________________________
                                                         () AS DWORD
'------------------------------------------------------------------------------
'
' Gets the Windows API handle of the standard output device.
'
'------------------------------------------------------------------------------

FUNCTION = GetStdHandle(%STD_OUTPUT_HANDLE_X)

END FUNCTION


FUNCTION StdErrPrint __________________________________________________________
                                              (BYREF stText AS STRING         _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Outputs a string expression to the standard error device.
'
'------------------------------------------------------------------------------

LOCAL lgLen AS LONG

WriteFile GetStdHandle(%STD_ERROR_HANDLE_X), BYVAL STRPTR(stText), _
          LEN(stText), lgLen, BYVAL %NULL

END FUNCTION


FUNCTION StdOutPrint __________________________________________________________
                                              (BYREF stText AS STRING         _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Outputs a string expression to the standard output device.
'
'------------------------------------------------------------------------------

LOCAL lgLen AS LONG

WriteFile GetStdHandle(%STD_OUTPUT_HANDLE_X), BYVAL STRPTR(stText), _
          LEN(stText), lgLen, BYVAL %NULL

END FUNCTION


FUNCTION StdInEOF _____________________________________________________________
                                      (OPTIONAL BYREF lgEOF AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Returnd true if end-of-file has been passed on standard input.
'
' Internally, this function is also called to condition the flag.
'
'------------------------------------------------------------------------------

STATIC lgEOF_static AS LONG

IF VARPTR(lgEOF) > 0 THEN
  lgEOF_static = lgEOF
END IF
FUNCTION = lgEOF_static

END FUNCTION


FUNCTION ConPrint _____________________________________________________________
                                              (BYREF stText AS dstring        _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Writes text data to the active console page buffer.
'
'------------------------------------------------------------------------------

LOCAL lgLen AS LONG
LOCAL lgPtr AS LONG
LOCAL lgStart AS LONG
LOCAL stChar AS dstring
LOCAL stSubString AS dstring
LOCAL lgRows AS LONG
LOCAL lgCols AS LONG
LOCAL lgRow AS LONG
LOCAL lgCol AS LONG

LOCAL btColors AS BYTE
LOCAL Origin AS CON_COORD
LOCAL CharInfo AS CON_CHAR_INFO
LOCAL dwSize AS CON_COORD
LOCAL PageRegion AS CON_RECT

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew
IF stText = "" THEN EXIT FUNCTION

ConGetScreenSize lgRows, lgCols

' Chop up the passed string if it contains embedded control codes

lgStart = 1
stChar = MID$(stText, lgStart, 1)
FOR lgPtr = 2 TO LEN(stText)
  IF stChar < CHR$(32) THEN
    IF MID$(stText, lgPtr, 1) >= CHR$(32) THEN
      GOSUB WriteOutput
      lgStart = lgPtr
      stChar = MID$(stText, lgStart, 1)
    END IF
  ELSE
    IF MID$(stText, lgPtr, 1) < CHR$(32) THEN
      GOSUB WriteOutput
      lgStart = lgPtr
      stChar = MID$(stText, lgStart, 1)
    END IF
  END IF
NEXT lgPtr
GOSUB WriteOutput

EXIT FUNCTION

WriteOutput:
stSubString = MID$(stText, lgStart, lgPtr - lgStart)
IF stChar >= CHR$(32) THEN
  stChar = ""
  ConGetPos lgRow, lgCol
  IF lgRow = lgRows THEN
    IF lgCol + LEN(stSubString) = lgCols + 1 THEN
      stChar = RIGHT$(stSubString, 1)
      stSubString = LEFT$(stSubString, -1)
    END IF
  END IF
ELSE
  stChar = ""
END IF
WriteConsoleX ConPageHndl(ConPageActive()), BYVAL STRPTR(stSubString), _
              LEN(stSubString), lgLen, BYVAL %NULL
IF LEN(stChar) THEN
  btColors = ConCellAttr(0, 0)
  CharInfo.Char = ASC(stChar)
  CharInfo.Attributes = btColors
  PageRegion.nTop = lgRows - 1
  PageRegion.nLeft = lgCols - 1
  PageRegion.nBottom = lgRows - 1
  PageRegion.nRight = lgCols - 1
  dwSize.x = 1
  dwSize.y = 1
  WriteConsoleOutputX ConPageHndl(ConPageActive()), CharInfo, _
                      dwSize, Origin, PageRegion
END IF
RETURN

END FUNCTION


FUNCTION StdInputLine _________________________________________________________
                                                         () AS STRING
'------------------------------------------------------------------------------
'
' Reads a text line, terminated by a CR code, from the standard input device.
'
'------------------------------------------------------------------------------

LOCAL lgOK AS LONG
LOCAL stChar AS STRING
LOCAL stText AS STRING
LOCAL lgLen AS LONG
LOCAL lgKbd AS LONG
LOCAL lgColPos AS LONG
LOCAL lgRowPos AS LONG
LOCAL lgMaxCol AS LONG

lgKbd = ConStdIn
stChar = " "

IF StdInEOF THEN
  IF lgKbd THEN
    ConPrint "^Z"
  ELSE
    EXIT FUNCTION
  END IF
END IF

IF lgKbd THEN
  ConGetScreenSize lgRowPos, lgMaxCol
END IF

DO
  lgOK = ReadFile(GetStdHandle(%STD_INPUT_HANDLE_X), BYVAL STRPTR(stChar), _
         1, lgLen, BYVAL %NULL)
  IF lgOK THEN
    IF lgLen = 1 THEN
      SELECT CASE stChar
      CASE $LF
       'Ignore
      CASE $CR
        EXIT DO
      CASE $BS
        IF LEN(stText) THEN
          stText = LEFT$(stText, -1)
          IF lgKbd THEN
            ConGetPos lgRowPos, lgColPos
            IF lgColPos = 1 THEN
              lgRowPos = lgRowPos - 1
              lgColPos = lgMaxCol + 1
            END IF
            ConSetPos lgRowPos, lgColPos - 1
            ConPrint " "
            ConSetPos lgRowPos, lgColPos - 1
          END IF
        END IF
      CASE CHR$(26)
        StdInEOF %TRUE
        IF lgKbd THEN
          ConPrint "^Z"
        ELSE
          EXIT DO
        END IF
      CASE ELSE
        IF ISFALSE(StdInEOF()) THEN
          stText = stText + stChar
          IF lgKbd THEN ConPrint UCODE$(stChar)
        END IF
      END SELECT
    ELSE
      StdInEOF %TRUE
      IF lgKbd THEN
        ConPrint "^Z"
      ELSE
        EXIT DO
      END IF
    END IF
  ELSE
    StdInEOF %TRUE
    IF lgKbd THEN
      ConPrint "^Z"
    ELSE
      EXIT DO
    END IF
  END IF
LOOP

FUNCTION = stText

END FUNCTION


FUNCTION ConInShift ___________________________________________________________
                                  (OPTIONAL BYREF dwBitmask AS DWORD          _
                                                          ) AS DWORD
'------------------------------------------------------------------------------
'
' Returns the state of the keyboard shift and lock keys.
'
' Internally, this function also called to record the shift flags.
'
'------------------------------------------------------------------------------

STATIC dwBitmask_static AS DWORD

IF VARPTR(dwBitmask) > 0 THEN
  dwBitmask_static = dwBitmask
END IF
FUNCTION = dwBitmask_static

END FUNCTION


FUNCTION ConPrevButtonState ___________________________________________________
                                  (OPTIONAL BYREF dwButtons AS DWORD          _
                                                          ) AS DWORD
'------------------------------------------------------------------------------
'
' Internal function to store/get the previous mouse button state.
'
'------------------------------------------------------------------------------

STATIC dwButtons_static AS DWORD

IF VARPTR(dwButtons) > 0 THEN
  dwButtons_static = dwButtons
END IF
FUNCTION = dwButtons_static

END FUNCTION


FUNCTION ConCheckInput ________________________________________________________
                                             (InputRecord AS CON_INPUT_RECORD _
                                                        ) AS dstring
'------------------------------------------------------------------------------
'
' Internal function used by ConInKey, ConInStat and ConMouseStat.
'
'------------------------------------------------------------------------------

LOCAL dwButton AS DWORD
LOCAL dwPress AS DWORD
LOCAL stText AS dstring

IF InputRecord.EventType = %KEY_EVENT THEN
  IF InputRecord.Event.KeyEvent.bKeyDown THEN
    IF InputRecord.Event.KeyEvent.uChar THEN
      stText = CHR$(InputRecord.Event.KeyEvent.uChar)
    ELSE
      SELECT CASE InputRecord.Event.KeyEvent.wVirtualScanCode
      CASE 29 'Ctrl
      CASE 42 'Left Shift
      CASE 54 'Right Shift
      CASE 56 'Alt
      CASE 58 'Caps Lock
      CASE 69 'Num Lock
      CASE 70 'Scroll Lock
      CASE ELSE
        stText = CHR$(0) + CHR$(InputRecord.Event.KeyEvent.wVirtualScanCode)
      END SELECT
    END IF
  END IF
ELSEIF InputRecord.EventType = %MOUSE_EVENT THEN
  SELECT CASE InputRecord.Event.MouseEvent.dwEventFlags
  CASE %DOUBLE_CLICK
    IF (ConMouseEventMask() AND 2) THEN
      dwButton = (ConPrevButtonState() XOR InputRecord.Event.MouseEvent.dwButtonState)
      IF (dwButton AND ConMouseButtonMask()) THEN
        stText = CHR$(2) + CHR$(dwButton)
      END IF
    END IF
  CASE %MOUSE_MOVED
    'Windows sends a spurious move event on a button
    'up, so a check is made for a real move.
    IF (ConMouseEventMask() AND 1) THEN
      IF (ConMouseCol() - 1) <> InputRecord.Event.MouseEvent.dwMousePosition.x THEN
        stText = CHR$(1) + CHR$(0)
      ELSEIF (ConMouseRow() - 1) <> InputRecord.Event.MouseEvent.dwMousePosition.y THEN
        stText = CHR$(1) + CHR$(0)
      END IF
    END IF
  CASE 0 'Press or release:
    dwButton = (ConPrevButtonState() XOR InputRecord.Event.MouseEvent.dwButtonState)
    IF (dwButton AND ConMouseButtonMask()) THEN
      dwPress = (dwButton AND InputRecord.Event.MouseEvent.dwButtonState)
      IF dwPress THEN
        IF (ConMouseEventMask() AND 4) THEN
          stText = CHR$(4) + CHR$(dwButton)
        END IF
      ELSE
        IF (ConMouseEventMask() AND 8) THEN
          stText = CHR$(8) + CHR$(dwButton)
        END IF
      END IF
    END IF
  END SELECT
  IF LEN(stText) THEN
    stText = CHR$(255) + CHR$(255) + stText
  END IF
END IF
FUNCTION = stText

END FUNCTION


FUNCTION ConInKey _____________________________________________________________
                                                         () AS dstring
'------------------------------------------------------------------------------
'
' Returns a keyboard or mouse event from the input buffer, if available.
'
'------------------------------------------------------------------------------

LOCAL InputRecord AS CON_INPUT_RECORD
LOCAL lgEvents AS LONG
LOCAL dwLength AS DWORD
LOCAL lgPtr AS LONG
LOCAL stText AS dstring
LOCAL stString AS dstring

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew
DO
  GetNumberOfConsoleInputEvents ConKbdHndl(), lgEvents
  IF lgEvents = 0 THEN EXIT DO
  ReadConsoleInputX ConKbdHndl(), InputRecord, 1, dwLength
  stText = ConCheckInput(InputRecord)
  IF LEN(stText) = 4 THEN 'Mouse event:
    ConMouseCol InputRecord.Event.MouseEvent.dwMousePosition.x
    ConMouseRow InputRecord.Event.MouseEvent.dwMousePosition.y
    ConPrevButtonState InputRecord.Event.MouseEvent.dwButtonState
    ConInShift InputRecord.Event.MouseEvent.dwControlKeyState
  ELSEIF LEN(stText) = 2 THEN 'Navigation or function keypress:
    lgPtr = ASC(MID$(stText, 2)) - 58
    IF lgPtr = 29 THEN lgPtr = 11
    IF lgPtr = 30 THEN lgPtr = 12
    stString = ConFuncKey(lgPtr)
    IF LEN(stString) THEN
      FOR lgPtr = 1 TO LEN(stString)
        InputRecord.Event.KeyEvent.uChar = ASC(MID$(stString, lgPtr))
        WriteConsoleInputX ConKbdHndl(), InputRecord, 1, dwLength
      NEXT lgPtr
      stText = ""
    ELSE
      ConInShift InputRecord.Event.KeyEvent.dwControlKeyState
    END IF
  ELSEIF LEN(stText) THEN 'Character or control keypress:
    ConInShift InputRecord.Event.KeyEvent.dwControlKeyState
  END IF
LOOP WHILE stText = ""
FUNCTION = stText

END FUNCTION


FUNCTION ConInStat ____________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
' Determines whether an input event is ready to be retrieved from the console.
'
'------------------------------------------------------------------------------

LOCAL InputRecord() AS CON_INPUT_RECORD
LOCAL lgEvents AS LONG
LOCAL dwLength AS DWORD
LOCAL lgPtr AS LONG
LOCAL stText AS dstring

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew

GetNumberOfConsoleInputEvents ConKbdHndl(), lgEvents
IF lgEvents = 0 THEN EXIT FUNCTION
REDIM InputRecord(lgEvents)
PeekConsoleInputX ConKbdHndl(), InputRecord(1), lgEvents, dwLength
FOR lgPtr = 1 TO dwLength
  stText = ConCheckInput(InputRecord(lgPtr))
  IF LEN(stText) THEN
    FUNCTION = %TRUE
    EXIT FOR
  END IF
NEXT lgPtr

END FUNCTION


FUNCTION ConMouseStat _________________________________________________________
                                                     () AS LONG
'------------------------------------------------------------------------------
'
' Determines whether a mouse event is ready to be retrieved from the console.
'
'------------------------------------------------------------------------------

LOCAL InputRecord() AS CON_INPUT_RECORD
LOCAL lgEvents AS LONG
LOCAL dwLength AS DWORD
LOCAL lgPtr AS LONG
LOCAL stText AS dstring

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew

GetNumberOfConsoleInputEvents ConKbdHndl(), lgEvents
IF lgEvents = 0 THEN EXIT FUNCTION
REDIM InputRecord(lgEvents)
PeekConsoleInputX ConKbdHndl(), InputRecord(1), lgEvents, dwLength
FOR lgPtr = 1 TO dwLength
  stText = ConCheckInput(InputRecord(lgPtr))
  IF LEN(stText) = 4 THEN
    FUNCTION = %TRUE
    EXIT FOR
  END IF
NEXT lgPtr

END FUNCTION


FUNCTION ConWaitStat __________________________________________________________
                                  (OPTIONAL BYVAL lgTimeout AS LONG           _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Waits until an input event is ready to be retrieved from the console.
'
'------------------------------------------------------------------------------

LOCAL dpExitTime AS DOUBLE

IF lgTimeout > 0 THEN
  dpExitTime = TIMER + (lgTimeOut / 1000)
  IF dpExitTime > 86400 THEN dpExitTime = dpExitTime - 86400
ELSE
  dpExitTime = 87000
END IF

DO UNTIL ConInStat
  IF ((dpExitTime - TIMER) < 0) AND ((dpExitTime - TIMER) > -1) THEN
    EXIT DO
  END IF
  SLEEP 1
LOOP
IF ConInStat THEN FUNCTION = 1

END FUNCTION


FUNCTION ConWaitKey ___________________________________________________________
                                  (OPTIONAL BYREF stKeyMask AS dstring,       _
                                   OPTIONAL BYVAL lgTimeout AS LONG           _
                                                          ) AS dstring
'------------------------------------------------------------------------------
'
' Returns a keyboard or mouse event from the input buffer, waiting for input.
'
'------------------------------------------------------------------------------

LOCAL dpExitTime AS DOUBLE
LOCAL stMaskString AS STRING
LOCAL stText AS dstring

IF lgTimeout > 0 THEN
  dpExitTime = TIMER + (lgTimeOut / 1000)
  IF dpExitTime > 86400 THEN dpExitTime = dpExitTime - 86400
ELSE
  dpExitTime = 87000
END IF

IF VARPTR(stKeyMask) > 0 THEN
  stMaskString = stKeyMask
END IF

DO
  DO UNTIL ConInStat
    IF ((dpExitTime - TIMER) < 0) AND ((dpExitTime - TIMER) > -1) THEN
      EXIT, EXIT
    END IF
    SLEEP 1
  LOOP
  IF ConInStat THEN
    stText = ConInKey
    IF LEN(stText) THEN
      IF LEN(stMaskString) THEN
        IF INSTR(stMaskString, stText) = 0 THEN
          stText = ""
        END IF
      END IF
    END IF
  END IF
LOOP UNTIL LEN(stText)
FUNCTION = stText

END FUNCTION


FUNCTION ConFlush _____________________________________________________________
                                                     () AS LONG
'------------------------------------------------------------------------------
'
' Removes all pending keyboard and mouse events from the console input buffer.
'
'------------------------------------------------------------------------------

IF ISFALSE(ConAvailable()) THEN EXIT FUNCTION
IF ConKbdHndl() = 0 THEN ConNew
FlushConsoleInputBuffer ConKbdHndl()

END FUNCTION


FUNCTION ConInputLine _________________________________________________________
                                            (BYREF stPrompt AS dstring,       _
                                               BYREF stText AS dstring        _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Reads a line of text, terminated by <Enter>, from the keyboard.
'
'------------------------------------------------------------------------------

LOCAL lgColPos AS LONG
LOCAL lgRowPos AS LONG
LOCAL lgMaxCol AS LONG
LOCAL stChar AS dstring

stText = ""
ConPrint stPrompt
ConGetScreenSize lgRowPos, lgMaxCol

DO
  stChar = ConWaitKey
  IF LEN(stChar) = 1 THEN
    IF stChar < $SPC THEN
      IF stChar = $CR THEN EXIT LOOP
      IF stChar = CHR$(8) THEN
        IF LEN(stText) THEN
          stText = LEFT$(stText, -1)
          ConGetPos lgRowPos, lgColPos
          IF lgColPos = 1 THEN
            lgRowPos = lgRowPos - 1
            lgColPos = lgMaxCol + 1
          END IF
          ConSetPos lgRowPos, lgColPos - 1
          ConPrint " "
          ConSetPos lgRowPos, lgColPos - 1
        END IF
      END IF
    ELSE
      stText = stText + stChar
      ConPrint stChar
    END IF
  END IF
LOOP

END FUNCTION
