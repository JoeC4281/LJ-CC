' Drop-down menu demonstration for PB/CC or LJ-CC   Laurence Jackson, Aug 2011
' For PB/CC 4, PB/CC 5, PB/CC 6, PB/Win 8, PB/Win 9 or PB/Win 10.
'------------------------------------------------------------------------------
' Compiles to 60KB with PB/Win 8 + LJ-CC, 30KB with PB/CC 4.

'#compiler pbwin 8
#COMPILE EXE
#DIM ALL

#IF NOT %DEF(%PB_CC32)
  #INCLUDE "LJ-CC.bas"
#ENDIF

%SCREEN_TEXT_COLOR = 15 'White
%SCREEN_BGND_COLOR = 1  'Blue

%STATUS_TEXT_COLOR = 7  'Light gray
%STATUS_BGND_COLOR = 3  'Cyan

%MENU_TEXT_COLOR   = 0  'Black
%MENU_BAR_COLOR    = 7  'Light gray
%MENU_BOX_COLOR    = 15 'White

%MOUSE_MOVE    = 1
%DOUBLE_CLICK  = 2
%BUTTON_DOWN   = 4
%BUTTON_UP     = 8

%LEFT_BUTTON   = 1
%RIGHT_BUTTON  = 2
%CENTER_BUTTON = 4

%ALT_KEY       = 3
%CTRL_KEY      = 12
%SHIFT_KEY     = 16

%CURSOR_UP    = 72
%CURSOR_LEFT  = 75
%CURSOR_RIGHT = 77
%CURSOR_DOWN  = 80
%MENU_KEY     = 93

' Define menu
'
' The menu is defined by a string array which is dimensioned up to 0 to 4095.
' Most elements of this apparently large array will actually contain null
' strings in a typical application. The nibbles of the subscript are assigned
' as follows:
'
' &H0000
'   ||||
'   |||'--- Sub-menu item (not implemented)
'   ||'---- Main menu item
'   |'----- Menu title
'   '------ not used
'
' Thus, up to 16 menus can be implemented (although 10 is probably the practical
' limit for 80-column screens), each of up to 15 items (item number zero being
' reserved for the menu title). The system also allows for any menu item to have
' a sub-menu of up to 15 sub-items, although this is not implemented yet.
'
' Strings are assigned to the array using the equates defined below. These equates
' also serve to identify the numeric commands returned from the GetInput function.

%FILE               = &H0000
%FILE_NEW           = %FILE + &H010
%FILE_OPEN          = %FILE + &H020
%FILE_CLOSE         = %FILE + &H030
%FILE_SAVE          = %FILE + &H040
%FILE_SAVE_AS       = %FILE + &H050
%FILE_REVERT        = %FILE + &H060
%FILE_PRINT         = %FILE + &H070
%FILE_PRINT_PREVIEW = %FILE + &H080
%FILE_PRINT_SETUP   = %FILE + &H090
%FILE_PREFERENCES   = %FILE + &H0A0
%FILE_EXIT          = %FILE + &H0B0

%EDIT               = &H0100
%EDIT_UNDO          = %EDIT + &H010
%EDIT_REDO          = %EDIT + &H020
%EDIT_CUT           = %EDIT + &H030
%EDIT_COPY          = %EDIT + &H040
%EDIT_PASTE         = %EDIT + &H050
%EDIT_FIND          = %EDIT + &H060
%EDIT_REPLACE       = %EDIT + &H070
%EDIT_SELECT_ALL    = %EDIT + &H080
%EDIT_DELETE        = %EDIT + &H090

%SCROLL             = &H0200
%SCROLL_UP          = %SCROLL + &H010
%SCROLL_DOWN        = %SCROLL + &H020
%SCROLL_LEFT        = %SCROLL + &H030
%SCROLL_RIGHT       = %SCROLL + &H040

%VIEW               = &H0300
%VIEW_ZOOM          = %VIEW + &H010
%VIEW_TOOLBARS      = %VIEW + &H020
%VIEW_RULERS        = %VIEW + &H030
%VIEW_STATUS_BAR    = %VIEW + &H040

%FORMAT             = &H0400
%FORMAT_CHARACTER   = %FORMAT + &H010
%FORMAT_PARAGRAPH   = %FORMAT + &H020
%FORMAT_WRAP        = %FORMAT + &H030

%WINDOW             = &H0500
%WINDOW_CASCADE     = %WINDOW + &H010
%WINDOW_TILE        = %WINDOW + &H020
%WINDOW_NEXT        = %WINDOW + &H030
%WINDOW_MAXIMIZE    = %WINDOW + &H040
%WINDOW_MINIMIZE    = %WINDOW + &H050

%HELP               = &H0600
%HELP_USING_HELP    = %HELP + &H010
%HELP_INDEX         = %HELP + &H020
%HELP_QUICK_START   = %HELP + &H030
%HELP_ABOUT         = %HELP + &H040


FUNCTION InitMenu _____________________________________________________________
                                                  (stMenu() AS STRING         _
                                                          ) AS LONG
'------------------------------------------------------------------------------
'
' Dimensions and fills the menu array.
'
'------------------------------------------------------------------------------

stMenu(%FILE)               = "File"
stMenu(%FILE_NEW)           = "New"
stMenu(%FILE_OPEN)          = "Open"
stMenu(%FILE_CLOSE)         = "Close"
stMenu(%FILE_SAVE)          = "Save"
stMenu(%FILE_SAVE_AS)       = "Save As..."
stMenu(%FILE_REVERT)        = "Revert"
stMenu(%FILE_PRINT)         = "Print"
stMenu(%FILE_PRINT_PREVIEW) = "Print Preview"
stMenu(%FILE_PRINT_SETUP)   = "Print Setup"
stMenu(%FILE_PREFERENCES)   = "Preferences"
stMenu(%FILE_EXIT)          = "Exit"

stMenu(%EDIT)               = "Edit"
stMenu(%EDIT_UNDO)          = "Undo"
stMenu(%EDIT_REDO)          = "Redo"
stMenu(%EDIT_CUT)           = "Cut"
stMenu(%EDIT_COPY)          = "Copy"
stMenu(%EDIT_PASTE)         = "Paste"
stMenu(%EDIT_FIND)          = "Find"
stMenu(%EDIT_REPLACE)       = "Replace"
stMenu(%EDIT_SELECT_ALL)    = "Select All"
stMenu(%EDIT_DELETE)        = "Delete"

stMenu(%SCROLL)             = "Scroll"
stMenu(%SCROLL_UP)          = "Up"
stMenu(%SCROLL_DOWN)        = "Down"
stMenu(%SCROLL_LEFT)        = "Left"
stMenu(%SCROLL_RIGHT)       = "Right"

stMenu(%VIEW)               = "View"
stMenu(%VIEW_ZOOM)          = "Zoom..."
stMenu(%VIEW_TOOLBARS)      = "Toolbars..."
stMenu(%VIEW_RULERS)        = "Rulers..."
stMenu(%VIEW_STATUS_BAR)    = "Status Bar"

stMenu(%FORMAT)             = "Format"
stMenu(%FORMAT_CHARACTER)   = "Character..."
stMenu(%FORMAT_PARAGRAPH)   = "Paragraph..."
stMenu(%FORMAT_WRAP)        = "Wrap"

stMenu(%WINDOW)             = "Window"
stMenu(%WINDOW_CASCADE)     = "Cascade"
stMenu(%WINDOW_TILE)        = "Tile"
stMenu(%WINDOW_NEXT)        = "Next"
stMenu(%WINDOW_MAXIMIZE)    = "Maximize"
stMenu(%WINDOW_MINIMIZE)    = "Minimize"

stMenu(%HELP)               = "Help"
stMenu(%HELP_USING_HELP)    = "Using Help"
stMenu(%HELP_INDEX)         = "Index"
stMenu(%HELP_QUICK_START)   = "Quick Start"
stMenu(%HELP_ABOUT)         = "About"

END FUNCTION


FUNCTION GetInput _____________________________________________________________
                                                  (stMenu() AS STRING         _
                                                          ) AS STRING
'------------------------------------------------------------------------------
'
' WAITKEY$ substitute which manages a drop-down menu and returns menu commands
' in addition to standard key presses and mouse clicks.
'
' The menu information is passed and the menu headings are re-created on
' every call, so menus can be modified "on the fly" by the caller.
'
' If the returned string is three bytes long, it contains a numeric menu
' command which can be retrieved as CVWRD(StringVar).
'
' Exercises for the student:
'   Add sub-menu support
'   Add divider lines
'   Add disabled items
'
'------------------------------------------------------------------------------

LOCAL lgMenus AS LONG
LOCAL lgPtr AS LONG
LOCAL stText AS STRING
LOCAL lgEvent AS LONG
LOCAL lgMenu AS LONG
LOCAL stItem AS STRING
LOCAL lgRowIn AS LONG
LOCAL lgColIn AS LONG
LOCAL btFgIn AS BYTE
LOCAL btBgIn AS BYTE
LOCAL lgItem AS LONG
LOCAL lgItems AS LONG
LOCAL lgLongestItem AS LONG
LOCAL lgBase AS LONG
LOCAL lgMenuTop AS LONG
LOCAL lgMenuLeft AS LONG
LOCAL lgMenuBottom AS LONG
LOCAL lgMenuRight AS LONG
LOCAL lgKey AS LONG
LOCAL lgRow AS LONG
LOCAL lgCol AS LONG
LOCAL lgShiftKeys AS LONG
LOCAL lgMenuCol() AS LONG
REDIM lgMenuCol(0 TO 16)

lgMenus = 0
lgMenuCol(0) = 3
stText = stMenu(0)

#IF %DEF(%PB_CC32)
  lgRowIn = cursory
  lgColIn = cursorx
  btFgIn = screenattr(0, 0)
  btBgIn = (btFgIn AND &HF0) \ 16
  btFgIn = btFgIn AND &H0F
  locate 1, 1
  COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR
  PRINT "   ";
  locate 1, lgMenuCol(0) + 1
  DO WHILE LEN(stText)
    PRINT stText + "  ";
    INCR lgMenus
    lgMenuCol(lgMenus) = cursorx - 1
    IF lgMenuCol(lgMenus) > 70 THEN EXIT DO
    lgPtr = lgPtr + 256
    stText = stMenu(lgPtr)
    IF stMenu(lgPtr + 16) = "" THEN
      stText = ""
    END IF
  LOOP
  PRINT " ";
  mouse %LEFT_BUTTON, MOVE, DOWN
  mouse ON
  COLOR btFgIn, btBgIn
  locate lgRowIn, lgColIn
  stText = WAITKEY$
#ELSE
  lgRowIn = ConPosRow()
  lgColIn = ConPosCol()
  btFgIn = ConCellAttr(0, 0)
  btBgIn = (btFgIn AND &HF0) \ 16
  btFgIn = btFgIn AND &H0F
  ConSetPos 1, 1
  ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR
  ConPrint "   "
  ConSetPos 1, lgMenuCol(0) + 1
  DO WHILE LEN(stText)
    ConPrint stText + "  "
    INCR lgMenus
    lgMenuCol(lgMenus) = ConPosCol() - 1
    IF lgMenuCol(lgMenus) > 70 THEN EXIT DO
    lgPtr = lgPtr + 256
    stText = stMenu(lgPtr)
  LOOP
  ConPrint " "
  ConMouse %MOUSE_MOVE + %BUTTON_DOWN, %LEFT_BUTTON
  ConMouseOn
  ConColor btFgIn, btBgIn
  ConSetPos lgRowIn, lgColIn
  stText = ConWaitKey
#ENDIF

DO
  #IF %DEF(%PB_CC32)
    lgShiftKeys = inshift
  #ELSE
    lgShiftKeys = ConInShift()
  #ENDIF
  IF LEN(stText) = 4 THEN
    lgEvent = ASC(MID$(stText, 3))
    #IF %DEF(%PB_CC32)
      lgRow = mousey
      lgCol = mousex
    #ELSE
      lgRow = ConMouseRow()
      lgCol = ConMouseCol()
    #ENDIF
    IF lgEvent = %MOUSE_MOVE THEN
      IF lgRow = 1 THEN
        IF (lgCol > lgMenuCol(0)) AND (lgCol <= lgMenuCol(lgMenus)) THEN
          FOR lgMenu = lgMenus TO 2 STEP -1
            IF lgCol > lgMenuCol(lgMenu - 1) THEN
              EXIT FOR
            END IF
          NEXT lgMenu
        ELSE
          #IF %DEF(%PB_CC32)
            stText = WAITKEY$
          #ELSE
            stText = ConWaitKey
          #ENDIF
          lgMenu = -1
        END IF
      ELSE
        #IF %DEF(%PB_CC32)
          stText = WAITKEY$
        #ELSE
          stText = ConWaitKey
        #ENDIF
        lgMenu = -1
      END IF
    ELSEIF lgEvent = %BUTTON_DOWN THEN
      lgMenu = 0
      IF lgRow = 1 THEN
        IF (lgCol > lgMenuCol(0)) AND (lgCol <= lgMenuCol(lgMenus)) THEN
          FOR lgMenu = lgMenus TO 2 STEP -1
            IF lgCol > lgMenuCol(lgMenu - 1) THEN
              EXIT FOR
            END IF
          NEXT lgMenu
          GOSUB DropMenu
          IF LEN(stText) THEN
            EXIT DO
          ELSE
            lgMenu = -1
          END IF
        END IF
      END IF
    END IF
  ELSEIF LEN(stText) = 2 THEN
    lgKey = ASC(MID$(stText, 2))
    IF lgKey = %MENU_KEY THEN
      IF lgMenu = 0 THEN lgMenu = 1
      GOSUB DropMenu
      IF LEN(stText) THEN
        EXIT DO
      ELSE
        lgMenu = -1
      END IF
    ELSEIF (lgShiftKeys AND %ALT_KEY) THEN
      IF lgKey = %CURSOR_LEFT THEN
        IF lgMenu = 0 THEN lgMenu = 1
        IF lgMenu > 1 THEN lgMenu = lgMenu - 1
      ELSEIF lgKey = %CURSOR_RIGHT THEN
        IF lgMenu < (lgMenus) THEN lgMenu = lgMenu + 1
      ELSE
        lgMenu = 0
      END IF
    ELSE
      lgMenu = 0
    END IF
  ELSE
    IF stText = $CR THEN
      IF (lgShiftKeys AND 31) = 0 THEN
        IF lgMenu THEN
          GOSUB DropMenu
          IF LEN(stText) THEN
            EXIT DO
          ELSE
            lgMenu = -1
          END IF
        ELSE
          lgMenu = 0
        END IF
      ELSE
        lgMenu = 0
      END IF
    ELSE
      lgMenu = 0
    END IF
  END IF
  IF lgMenu > 0 THEN
    lgPtr = (lgMenu - 1) * 256
    #IF %DEF(%PB_CC32)
      locate 1, lgMenuCol(lgMenu - 1)
      COLOR %MENU_TEXT_COLOR, %MENU_BOX_COLOR, LEN(stMenu(lgPtr)) + 2
      COLOR btFgIn, btBgIn
      locate lgRowIn, lgColIn
      stText = WAITKEY$
      locate 1, lgMenuCol(lgMenu - 1) - 1
      COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgPtr)) + 4
      COLOR btFgIn, btBgIn
      locate lgRowIn, lgColIn
    #ELSE
      ConSetPos 1, lgMenuCol(lgMenu - 1)
      ConColor %MENU_TEXT_COLOR, %MENU_BOX_COLOR, LEN(stMenu(lgPtr)) + 2
      ConColor btFgIn, btBgIn
      ConSetPos lgRowIn, lgColIn
      stText = ConWaitKey
      ConSetPos 1, lgMenuCol(lgMenu - 1) - 1
      ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgPtr)) + 4
      ConColor btFgIn, btBgIn
      ConSetPos lgRowIn, lgColIn
    #ENDIF
  END IF
LOOP UNTIL ISFALSE(lgMenu)

FUNCTION = stText

EXIT FUNCTION

DropMenu: '--------------------------------------------------------------------

IF (lgMenu < 1) OR (lgMenu > lgMenus) THEN
  lgMenu = 0
  stText = ""
  RETURN
END IF

lgBase = (lgMenu - 1) * 256
lgItems = 0
lgLongestItem = 0
FOR lgPtr = lgBase + 16 TO lgBase + 255 STEP 16
  IF LEN(stMenu(lgPtr)) THEN
    INCR lgItems
    IF LEN(stMenu(lgPtr)) > lgLongestItem THEN
      lgLongestItem = LEN(stMenu(lgPtr))
    END IF
  ELSE
    EXIT FOR
  END IF
NEXT lgPtr

lgMenuTop = 2
lgMenuLeft = lgMenuCol(lgMenu - 1) - 1
IF (lgMenuLeft + lgLongestItem + 5) > 79 THEN
  lgMenuLeft = 79 - lgLongestItem - 4
END IF
lgMenuBottom = lgMenuTop + lgItems + 1
lgMenuRight = lgMenuLeft + lgLongestItem + 4

#IF %DEF(%PB_CC32)
  locate 1, lgMenuCol(lgMenu - 1)
  COLOR %MENU_TEXT_COLOR, %MENU_BOX_COLOR, LEN(stMenu(lgBase)) + 2
  cursor OFF
  pcopy 1, 2
  locate lgMenuTop, lgMenuLeft
  PRINT CHR$(218) + STRING$(lgLongestItem + 3, 196) + CHR$(191);
  FOR lgItem = 1 TO lgItems
    lgPtr = lgBase + lgItem * 16
    IF LEN(stMenu(lgPtr)) = 0 THEN EXIT FOR
    locate lgMenuTop + lgItem, lgMenuLeft
    PRINT CHR$(179) + SPACE$(lgLongestItem + 3) + CHR$(186);
    locate lgMenuTop + lgItem, lgMenuLeft + 2
    PRINT stMenu(lgPtr) + ""
  NEXT lgItem
  locate lgMenuBottom, lgMenuLeft
  PRINT CHR$(192) + STRING$(lgLongestItem + 3, 205) + CHR$(188);
  lgItem = 1
  locate lgMenuTop + lgItem, lgMenuLeft + 1
  COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR, lgLongestItem + 3
#ELSE
  ConSetPos 1, lgMenuCol(lgMenu - 1)
  ConColor %MENU_TEXT_COLOR, %MENU_BOX_COLOR, LEN(stMenu(lgBase)) + 2
  ConCursorOff
  ConPageCopy 1, 2
  ConSetPos lgMenuTop, lgMenuLeft
  ConPrint CHR$(218) + STRING$(lgLongestItem + 3, 196) + CHR$(191)
  FOR lgItem = 1 TO lgItems
    lgPtr = lgBase + lgItem * 16
    IF LEN(stMenu(lgPtr)) = 0 THEN EXIT FOR
    ConSetPos lgMenuTop + lgItem, lgMenuLeft
    ConPrint CHR$(179) + SPACE$(lgLongestItem + 3) + CHR$(186)
    ConSetPos lgMenuTop + lgItem, lgMenuLeft + 2
    ConPrint stMenu(lgPtr) + ""
  NEXT lgItem
  ConSetPos lgMenuBottom, lgMenuLeft
  ConPrint CHR$(192) + STRING$(lgLongestItem + 3, 205) + CHR$(188)
  lgItem = 1
  ConSetPos lgMenuTop + lgItem, lgMenuLeft + 1
  ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR, lgLongestItem + 3
#ENDIF

DO
  #IF %DEF(%PB_CC32)
    stText = WAITKEY$
  #ELSE
    stText = ConWaitKey
  #ENDIF
  IF LEN(stText) = 4 THEN
    #IF %DEF(%PB_CC32)
      lgRow = mousey
      lgCol = mousex
    #ELSE
      lgRow = ConMouseRow()
      lgCol = ConMouseCol()
    #ENDIF
    IF ASC(MID$(stText, 3)) = %MOUSE_MOVE THEN
      IF lgRow = 1 THEN
        IF (lgCol > lgMenuCol(0)) AND (lgCol <= lgMenuCol(lgMenus)) THEN
          FOR lgPtr = lgMenus TO 2 STEP -1
            IF lgCol > lgMenuCol(lgPtr - 1) THEN
              EXIT FOR
            END IF
          NEXT lgPtr
          IF lgPtr <> lgMenu THEN
            #IF %DEF(%PB_CC32)
              pcopy 2, 1
              locate 1, lgMenuCol(lgMenu - 1) - 1
              COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgBase)) + 4
            #ELSE
              ConPageCopy 2, 1
              ConSetPos 1, lgMenuCol(lgMenu - 1) - 1
              ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgBase)) + 4
            #ENDIF
            lgMenu = lgPtr
            GOTO DropMenu
          END IF
        END IF
      ELSEIF (lgRow > lgMenuTop) AND lgRow < lgMenuBottom THEN
        IF (lgCol > lgMenuLeft) AND (lgCol < lgMenuRight) THEN
          lgPtr = lgRow - lgMenuTop
          IF lgPtr <> lgItem THEN
            #IF %DEF(%PB_CC32)
              locate lgMenuTop + lgItem, lgMenuLeft + 1
              COLOR %MENU_TEXT_COLOR, %MENU_BOX_COLOR, lgLongestItem + 3
              locate lgMenuTop + lgPtr, lgMenuLeft + 1
              COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR, lgLongestItem + 3
            #ELSE
              ConSetPos lgMenuTop + lgItem, lgMenuLeft + 1
              ConColor %MENU_TEXT_COLOR, %MENU_BOX_COLOR, lgLongestItem + 3
              ConSetPos lgMenuTop + lgPtr, lgMenuLeft + 1
              ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR, lgLongestItem + 3
            #ENDIF
            lgItem = lgPtr
          END IF
        END IF
      END IF
    ELSEIF ASC(MID$(stText, 3)) = %BUTTON_DOWN THEN
      IF (lgRow >= lgMenuTop) AND lgRow <= lgMenuBottom THEN
        IF (lgCol >= lgMenuLeft) AND (lgCol <= lgMenuRight) THEN
          IF (lgRow = lgMenuTop + lgItem) THEN
            IF (lgCol > lgMenuLeft) AND (lgCol < lgMenuRight) THEN
              #IF %DEF(%PB_CC32)
                locate 1, lgMenuCol(lgMenu - 1) - 1
                COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgBase)) + 4
              #ELSE
                ConSetPos 1, lgMenuCol(lgMenu - 1) - 1
                ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgBase)) + 4
              #ENDIF
              stText = CHR$(lgItem * 16) + CHR$(lgMenu - 1) + CHR$(0)
              EXIT DO
            END IF
          END IF
        ELSE
          stText = ""
          EXIT DO
        END IF
      ELSE
        stText = ""
        EXIT DO
      END IF
    END IF
  ELSEIF LEN(stText) = 2 THEN
    lgKey = ASC(MID$(stText, 2))
    SELECT CASE lgKey
    CASE %CURSOR_LEFT
      IF lgMenu > 1 THEN
        #IF %DEF(%PB_CC32)
          pcopy 2, 1
          locate 1, lgMenuCol(lgMenu - 1) - 1
          COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgBase)) + 4
        #ELSE
          ConPageCopy 2, 1
          ConSetPos 1, lgMenuCol(lgMenu - 1) - 1
          ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgBase)) + 4
        #ENDIF
        DECR lgMenu
        GOTO DropMenu
      END IF
    CASE %CURSOR_RIGHT
      IF lgMenu < lgMenus THEN
        #IF %DEF(%PB_CC32)
          pcopy 2, 1
          locate 1, lgMenuCol(lgMenu - 1) - 1
          COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgBase)) + 4
        #ELSE
          ConPageCopy 2, 1
          ConSetPos 1, lgMenuCol(lgMenu - 1) - 1
          ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgBase)) + 4
        #ENDIF
        INCR lgMenu
        GOTO DropMenu
      END IF
    CASE %CURSOR_UP
      IF lgItem > 1 THEN
        #IF %DEF(%PB_CC32)
          locate lgMenuTop + lgItem, lgMenuLeft + 1
          COLOR %MENU_TEXT_COLOR, %MENU_BOX_COLOR, lgLongestItem + 3
          DECR lgItem
          locate lgMenuTop + lgItem, lgMenuLeft + 1
          COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR, lgLongestItem + 3
        #ELSE
          ConSetPos lgMenuTop + lgItem, lgMenuLeft + 1
          ConColor %MENU_TEXT_COLOR, %MENU_BOX_COLOR, lgLongestItem + 3
          DECR lgItem
          ConSetPos lgMenuTop + lgItem, lgMenuLeft + 1
          ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR, lgLongestItem + 3
        #ENDIF
      END IF
    CASE %CURSOR_DOWN
      IF lgItem < lgItems THEN
        #IF %DEF(%PB_CC32)
          locate lgMenuTop + lgItem, lgMenuLeft + 1
          COLOR %MENU_TEXT_COLOR, %MENU_BOX_COLOR, lgLongestItem + 3
          INCR lgItem
          locate lgMenuTop + lgItem, lgMenuLeft + 1
          COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR, lgLongestItem + 3
        #ELSE
          ConSetPos lgMenuTop + lgItem, lgMenuLeft + 1
          ConColor %MENU_TEXT_COLOR, %MENU_BOX_COLOR, lgLongestItem + 3
          INCR lgItem
          ConSetPos lgMenuTop + lgItem, lgMenuLeft + 1
          ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR, lgLongestItem + 3
        #ENDIF
      END IF
    END SELECT
  ELSE
    IF (lgShiftKeys AND 31) = 0 THEN
      IF stText = $CR THEN
        #IF %DEF(%PB_CC32)
          locate 1, lgMenuCol(lgMenu - 1) - 1
          COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgBase)) + 4
        #ELSE
          ConSetPos 1, lgMenuCol(lgMenu - 1) - 1
          ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR, LEN(stMenu(lgBase)) + 4
        #ENDIF
        stText = CHR$(lgItem * 16) + CHR$(lgMenu - 1) + CHR$(0)
        EXIT DO
      ELSEIF stText = $ESC THEN
        stText = ""
        EXIT DO
      END IF
    END IF
  END IF
LOOP

lgBase = (lgMenu - 1) * 256
#IF %DEF(%PB_CC32)
  pcopy 2, 1
  locate 1, lgMenuCol(lgMenu - 1) - 1
  COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR
  PRINT "  " + stMenu(lgBase) + "  "
  COLOR btFgIn, btBgIn
  locate lgRowIn, lgColIn
  cursor ON
#ELSE
  ConPageCopy 2, 1
  ConSetPos 1, lgMenuCol(lgMenu - 1) - 1
  ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR
  ConPrint "  " + stMenu(lgBase) + "  "
  ConColor btFgIn, btBgIn
  ConSetPos lgRowIn, lgColIn
  ConCursorOn
#ENDIF
lgMenu = 0

RETURN

END FUNCTION


FUNCTION PBMAIN _______________________________________________________________
                                                         () AS LONG
'------------------------------------------------------------------------------
'
'------------------------------------------------------------------------------

LOCAL stUserInput AS STRING
LOCAL stMenu() AS STRING
LOCAL lgMenuItem AS LONG
LOCAL lgPtr AS LONG
LOCAL stCaption AS STRING
LOCAL lgX, lgY AS LONG
LOCAL lgConsoleW, lgConsoleH AS LONG
LOCAL lgDesktopW, lgDesktopH AS LONG
LOCAL lgLocX, lgLocY AS LONG
LOCAL lgStyle AS LONG
LOCAL lgFlags AS LONG
LOCAL stText AS STRING
LOCAL lgShiftKeys AS LONG

#IF %DEF(%PB_WIN32) AND %PB_REVISION >= &H1000
  UCODEPAGE OEM
#ELSEIF %DEF(%PB_CC32) AND %PB_REVISION >= &H600
  UCODEPAGE OEM
#ENDIF

#IF %DEF(%PB_CC32)
  console SET screen 25, 80
  console GET SIZE TO lgConsoleW, lgConsoleH
#ELSE
  ConSetScreenSize 25, 80
  ConGetWindowSize lgConsoleW, lgConsoleH
#ENDIF

DESKTOP GET CLIENT TO lgDesktopW, lgDesktopH
DESKTOP GET LOC TO lgX, lgY
lgLocX = ((lgDesktopW - lgConsoleW) \ 2) + lgX
lgLocY = ((lgDesktopH - lgConsoleH) \ 2) + lgY

#IF %DEF(%PB_CC32)
  console SET LOC lgLocX, lgLocY
  console NAME "PB/CC Drop-down menu demonstration"
  COLOR %SCREEN_TEXT_COLOR, %SCREEN_BGND_COLOR
  cls
  locate 1, 1
  COLOR %MENU_TEXT_COLOR, %MENU_BAR_COLOR
  PRINT SPACE$(80);
  COLOR %STATUS_TEXT_COLOR, %STATUS_BGND_COLOR
  locate 25,1
  PRINT READ$(1);
  locate 4, 1
  COLOR %SCREEN_TEXT_COLOR, %SCREEN_BGND_COLOR
  FOR lgPtr = 2 TO DATACOUNT - 1
    PRINT READ$(lgPtr)
  NEXT lgPtr
  PRINT READ$(DATACOUNT);
#ELSE
  ConSetWindowLoc lgLocX, lgLocY
  ConCaption "LJ-CC Drop-down menu demonstration"
  ConColor %SCREEN_TEXT_COLOR, %SCREEN_BGND_COLOR
  ConClear
  ConSetPos 1, 1
  ConColor %MENU_TEXT_COLOR, %MENU_BAR_COLOR
  ConPrint SPACE$(80)
  ConSetPos 25,1
  ConColor %STATUS_TEXT_COLOR, %STATUS_BGND_COLOR
  ConPrint READ$(1)
  ConSetPos 4, 1
  ConColor %SCREEN_TEXT_COLOR, %SCREEN_BGND_COLOR
  FOR lgPtr = 2 TO DATACOUNT - 1
    ConPrint READ$(lgPtr) + $CRLF
  NEXT lgPtr
  ConPrint READ$(DATACOUNT)
#ENDIF

REDIM stMenu(0 TO &H0800)
InitMenu stMenu()

DO
  stUserInput = GetInput(stMenu())
  stText = ""
  #IF %DEF(%PB_CC32)
    lgShiftKeys = inshift
  #ELSE
    lgShiftKeys = ConInShift()
  #ENDIF
  IF (lgShiftKeys AND %ALT_KEY) THEN stText = stText + "Alt+"
  IF (lgShiftKeys AND %CTRL_KEY) THEN stText = stText + "Ctrl+"
  IF (lgShiftKeys AND %SHIFT_KEY) THEN stText = stText + "Shift+"
  IF LEN(stUserInput) = 3 THEN
    lgMenuItem = CVWRD(stUserInput)
    SELECT CASE lgMenuItem
    CASE %FILE_NEW
      stText = "File -> New"
    CASE %FILE_OPEN
      stText = "File -> Open"
    CASE %FILE_CLOSE
      stText = "File -> Close"
    CASE %FILE_SAVE
      stText = "File -> Save"
    CASE %FILE_SAVE_AS
      stText = "File -> Save As"
    CASE %FILE_REVERT
      stText = "File -> Revert"
    CASE %FILE_PRINT
      stText = "File -> Print"
    CASE %FILE_PRINT_PREVIEW
      stText = "File -> Print Preview"
    CASE %FILE_PRINT_SETUP
      stText = "File -> Setup"
    CASE %FILE_PREFERENCES
      stText = "File -> Preferences"
    CASE %FILE_EXIT
      EXIT DO
      stText = "File -> Exit"
    CASE %EDIT_UNDO
      stText = "Edit -> Undo"
    CASE %EDIT_REDO
      stText = "Edit -> Redo"
    CASE %EDIT_CUT
      stText = "Edit -> Cut"
    CASE %EDIT_COPY
      stText = "Edit -> Copy"
    CASE %EDIT_PASTE
      stText = "Edit -> Paste"
    CASE %EDIT_FIND
      stText = "Edit -> Find"
    CASE %EDIT_REPLACE
      stText = "Edit -> Replace"
    CASE %EDIT_SELECT_ALL
      stText = "Edit -> Select All"
    CASE %EDIT_DELETE
      stText = "Edit -> Delete"
    CASE %SCROLL_UP
      stText = "Scroll -> Up"
      #IF %DEF(%PB_CC32)
        scroll down 1, 2, 1, 24, 80
      #ELSE
        ConScrollUp 1, 2, 1, 24, 80
      #ENDIF
    CASE %SCROLL_DOWN
      stText = "Scroll -> Down"
      #IF %DEF(%PB_CC32)
        scroll up     1, 2, 1, 24, 80
      #ELSE
        ConScrollDown 1, 2, 1, 24, 80
      #ENDIF
    CASE %SCROLL_LEFT
      stText = "Scroll -> Left"
      #IF %DEF(%PB_CC32)
        scroll LEFT   1, 2, 1, 24, 80
      #ELSE
        ConScrollLeft 1, 2, 1, 24, 80
      #ENDIF
    CASE %SCROLL_RIGHT
      stText = "Scroll -> Right"
      #IF %DEF(%PB_CC32)
        scroll RIGHT   1, 2, 1, 24, 80
      #ELSE
        ConScrollRight 1, 2, 1, 24, 80
      #ENDIF
    CASE %VIEW_ZOOM
      stText = "View -> Zoom"
    CASE %VIEW_TOOLBARS
      stText = "View -> Toolbars"
    CASE %VIEW_RULERS
      stText = "View -> Rulers"
    CASE %VIEW_STATUS_BAR
      stText = "View -> Status Bar"
    CASE %FORMAT_CHARACTER
      stText = "Format -> Character"
    CASE %FORMAT_PARAGRAPH
      stText = "Format -> Paragraph"
    CASE %FORMAT_WRAP
      stText = "Format -> Wrap"
    CASE %WINDOW_CASCADE
      stText = "Window -> Cascade"
    CASE %WINDOW_TILE
      stText = "Window -> Tile"
    CASE %WINDOW_NEXT
      stText = "Window -> Next"
    CASE %WINDOW_MAXIMIZE
      stText = "Window -> Maximize"
    CASE %WINDOW_MINIMIZE
      stText = "Window -> Minimize"
    CASE %HELP_USING_HELP
      stText = "Help -> Using Help"
    CASE %HELP_INDEX
      stText = "Help -> Index"
    CASE %HELP_QUICK_START
      stText = "Help -> Quick Start"
    CASE %HELP_ABOUT
      stText = "Help -> About"
    CASE ELSE
      stText = ""
      FOR lgPtr = 1 TO LEN(stUserInput)
        stText = stText + "<" + FORMAT$(ASC(MID$(stUserInput, lgPtr))) + ">"
      NEXT lgPtr
    END SELECT
  ELSEIF LEN(stUserInput) = 4 THEN
    #IF %DEF(%PB_CC32)
      stText = "Click at " + FORMAT$(mousey) + "," + FORMAT$(mousex)
    #ELSE
      stText = "Click at " + FORMAT$(ConMouseRow()) + "," + FORMAT$(ConMouseCol())
    #ENDIF
  ELSEIF LEN(stUserInput) = 2 THEN
    FOR lgPtr = 1 TO LEN(stUserInput)
      stText = stText + "<" + FORMAT$(ASC(MID$(stUserInput, lgPtr))) + ">"
    NEXT lgPtr
  ELSEIF LEN(stUserInput) THEN
    IF ASC(stUserInput) < 32 THEN
      IF stUserInput = $CR THEN
        IF (lgShiftKeys AND %CTRL_KEY) THEN
          stText = stText + CHR$(ASC(stUserInput) + ASC("@"))
        ELSE
          stText = stText + "Enter"
        END IF
      ELSEIF stUserInput = $TAB THEN
        IF (lgShiftKeys AND %CTRL_KEY) THEN
          stText = stText + CHR$(ASC(stUserInput) + ASC("@"))
        ELSE
          stText = stText + "Tab"
        END IF
      ELSEIF stUserInput = $ESC THEN
        IF (lgShiftKeys AND %CTRL_KEY) THEN
          stText = stText + CHR$(ASC(stUserInput) + ASC("@"))
        ELSE
          stText = stText + "Esc"
        END IF
      ELSEIF stUserInput = $BS THEN
        IF (lgShiftKeys AND %CTRL_KEY) THEN
          stText = stText + CHR$(ASC(stUserInput) + ASC("@"))
        ELSE
          stText = stText + "Backspace"
        END IF
      ELSEIF stUserInput = $LF THEN
        stText = stText + CHR$(ASC(stUserInput) + ASC("@")) + " or Ctrl+Enter"
      ELSE
        stText = stText + CHR$(ASC(stUserInput) + ASC("@"))
      END IF
    ELSEIF stUserInput = CHR$(127) THEN
      stText = stText + "Backspace"
    ELSE
      IF stUserInput = " " THEN
        stText = stText + "Space"
      ELSE
        stText = stText + stUserInput
      END IF
    END IF
  ELSE
    stText = ""
  END IF
  IF LEN(stText) THEN
    #IF %DEF(%PB_CC32)
      lgY = cursory
      lgX = cursorx
      locate 25,1
      COLOR %STATUS_TEXT_COLOR, %STATUS_BGND_COLOR
      PRINT SPACE$(80);
      locate 25, 40 - LEN(stText)\2
      PRINT stText;
      COLOR %SCREEN_TEXT_COLOR, %SCREEN_BGND_COLOR
      locate lgY, lgX
    #ELSE
      lgY = ConPosRow
      lgX = ConPosCol
      ConSetPos 25,1
      ConColor %STATUS_TEXT_COLOR, %STATUS_BGND_COLOR
      ConPrint SPACE$(80)
      ConSetPos 25, 40 - LEN(stText)\2
      ConPrint stText + ""
      ConColor %SCREEN_TEXT_COLOR, %SCREEN_BGND_COLOR
      ConSetPos lgY, lgX
    #ENDIF
  END IF
LOOP

DATA "  Use mouse or Alt with cursor keys to access menus, then click or press Enter  "
DATA "  A Windows 'console' is a window which uses only fixed pitch text for output,"
DATA "  just like the old DOS text mode screen (in fact, a console is often referred"
DATA "  to as a 'DOS box'). As such, it provides the quickest and easiest route for"
DATA "  the conversion of programs written for DOS text mode to Windows. A console"
DATA "  primarily uses the keyboard for input, but the mouse may also be enabled for"
DATA "  additional user input support, with the mouse location expressed in text"
DATA "  screen rows and columns, as used in some DOS programs. PowerBASIC also"
DATA "  provides support for the conversion of DOS programs written for graphics"
DATA "  modes, in the form of the separate GRAPHIC WINDOW mechanism, available in"
DATA "  both PB/Win and PB/CC. But Windows consoles have uses and functionality"
DATA "  beyond the emulation of DOS text mode. Graphical User Interface (GUI)"
DATA "  applications require a whole new, and more complex, programming paradigm"
DATA "  which may not be worth the trouble for quick-and-dirty utilities. PB/Win's"
DATA "  DDT mechanism makes GUI programming easier, but it may still be too time-"
DATA "  consuming for some simple programs. But even within a GUI application, a"
DATA "  console may be useful for the output of debugging or logging information,"
DATA "  maybe even retain an older text-mode interface while developing a new GUI"
DATA "  interface. For the first time, LJ-CC allows the easy combination of PB/Win's"
DATA "  DDT with the full console functionality of PB/CC."

END FUNCTION
