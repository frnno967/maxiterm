'Simple terminal program v 0.1 by Rich Martin (datawiz) 11:13pm 02 Oct 2020
'Also code from Flashback.bas 1.0.0 by Rich Martin (datawiz)
'Also code from vegipete for the GetFile routine
'Also code from davervw for Xmodem 07 Mar 2021
'Version 1.9.5 John Crutti Jr 4-12-2021

OPTION EXPLICIT
ON ERROR ABORT
OPTION CRLF CRLF
OPTION CONSOLE SCREEN
'===================
' required setup for function of getfile routines
CONST DIRCOUNT = 50   ' max number of sub-directories
CONST FILCOUNT = 255  ' max number of files
CONST NAMELENGTH = 64
DIM dir_dirs$(DIRCOUNT) length NAMELENGTH  ' store list of directories
DIM dir_fils$(FILCOUNT) length NAMELENGTH  ' store list of files
DIM dir_hist$(DIRCOUNT) length 8 ' store directory number visited along path
DIM d_cwidth, d_x, d_y, d_lines
DIM d_colours(3) = (&hA0A040,&h101010,&hFFFFFF,&h303030) 'array of 4 colour values
' end of setup for funtion
'===================
' declare, define, dimension other stuff here
dim NameOfFile$(1)  ' place to put chosen filename string, goes in element 0
'===================

dim comportstr$ = "COM1" ' COM port as a string for COM Port subroutine.
dim comchoice$ = "1" ' used in COM selection subroutine.
dim comspeedchoice$ = "9" ' used in COM Speed selection subroutine.
dim comspeed$ = "115200" ' COM Speed as a string for COM Speed subroutine.
dim comporttype$ = "TTL Serial" 'default to TTL, other option is RS-232 Serial
dim CHAR_OUT$ 'characters we're typing at the console to be sent to modem
dim altflag% 'is ALT key pressed?
dim winflag% 'is WINDOWS key pressed?
dim shiftflag% = 0 'is a SHIFT key pressed?
dim ctrlflag% = 0 'is a CONTROL key pressed?
dim keyvalue% = 0 'acsii value of key pressed in getchar() routine
dim keyflag% = 0 'has a key been pressed in getchar() routine
dim keylast% = 0 'value of last ascii key typed in getchar() routine
dim lastmodifier% = 0 'value of the last modifier keys used in getchar() routine
dim CHARS_IN$ 'characters being received from the modem
dim echosetting$ = "Echo Off" 'is local echo enabled. Other option is Echo On
dim linefeedstate$ = "LF Off" 'setting of line feeds to be sent to modem after every CR
dim width%
dim height%
dim onlineflag% = 1 'set to 1 if currently printing serial data to the screen
dim soundflag% = 0 'if sound will be generated with every keypress
dim fwidth% = mm.info(fontwidth)
dim fheight% = mm.info(fontheight)
dim xpos% = 0
dim ypos% = 0
dim receivefile$ 'filename of file we're downloading
dim text_color = 1 'used in setting font color choice
dim text_colorstr$ 'string for saving text color choice
dim feature$ 'output of getchar function to select program features
dim modemresetstring$ = "ATZ" 'command to reset the modem
dim modeminitstring$ = "ATE1V1X0Q0" 'command used to configure the modem
dim modeminfostring$ = "ATI" 'command to ask modem to show information
setpin 32,DOUT 'setup RTS/CTS pins
setpin 33,DIN 'setup RTS/CTS pins
PIN(32) = 0 'setup RTS/CTS pins
dim TERM_COLOR1 = 255 'init value for white
dim TERM_COLOR2 = 255 'init value for white
dim TERM_COLOR3 = 255 'init value for white
dim debug% 'for debug mode
dim phonebookentry$(10) as string ' array holding the user's phone book
dim phonebookusername$(10) as string 'array holding user's login name
dim phonebookpassword$(10) as string 'array holding user's password
dim phoneentry% = 0 'int of array value for the phone book
dim dialchoice$ 'string of selections in autodial phone book screen
dim second$ 'for blinking cursor routine
dim numtime%, cycles%, underscore% 'for blinking cursor
dim blinkingcursor$ = "Blinking Off" 'for turning blinking cursor on or off
dim x%, y%, xoffset%, yoffset%
dim transferdone% = 0 ' for ending transfers
dim bold% = 0
dim fg_colour1 = 172
dim fg_colour2 = 172
dim fg_colour3 = 172
dim bg_colour1 = 0
dim bg_colour2 = 0
dim bg_colour3 = 0
dim save_x% = 0
dim save_y% = 0
dim escape_flag% = 0

dim params%(9)
dim param_at% = 0


gui cursor load "cursor.spr" 'cursor sprite for blinking cursor function
gui cursor on 2,x%,y%
gui cursor hide


_xmodem_dim

'main function
cls
load font "CP437(8x12).FNT"
font #8
introscreen 'show the title screen when launched
pause 2500
cls
loadphonebook 'load phone book, if exists
welcomebanner 'banner at top of terminal screen showing help and exit commands
  if mm.info(FILESIZE "settings.cfg") = -1 then
    setcomport 'set the COM port you want to use 1 or 2
    setcomspeed 'choose the speed of the COM port
    setcomtype
    pickcolor 'pick the color you want out of White, Amber, and Green
  else
    loadconfig
  end if
setupcolor 'configure the color
startcomport 'open the COM port for communications
terminalonline 'just tells you that you're online and ready to communicate
modemreset 'send modem reset "ATZ" string
modeminit 'send modem setup string
modeminfo 'ask modem to print its info for the user
terminal


sub terminal
do 'user input routine
  if x% <> MM.INFO(HPOS) or y% <> MM.INFO(VPOS) then 'only update vars when POS changes
  x% = MM.INFO(HPOS)
  y% = MM.INFO(VPOS)
    if x% >= 800 then
    x% = 799 'for some reason x% exceeds 800 sometimes, causing crash in GUI CURSOR
    end if
  gui cursor x%,y% 'update the cursor position variables.
  end if
CHAR_OUT$ = INKEY$ 'typed input from terminal
if CHAR_OUT$ <> "" then
  feature$ = getchar$()'typing processed by getchar routine to watch for modifier keys
    if altflag% = 1 then 'check for ALT being asserted
      select case feature$ 'turn all characters to lowercase
      case chr$(137) 'Page UP button
        download
      case chr$(136) 'Page Down button
        CHAR_OUT$ = "" : upload 'sending a CRLF first so buffer will be clear
      case "a" 'show the autodial phone book screen
        gui cursor hide : phonebook 
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
      case "b" 'set the COM port parameters again
        gui cursor hide 'hide blinking cursor if on
        onlineflag% = 0 'take us "offline" so we don't see incoming data during this.
        cls
        setcomport : setcomspeed : terminalonline : startcomport
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
        onlineflag% = 1 'back online
      case "c" 'clear the existing screen contents
        text 400,300, "  CLEARING SCREEN  ", "CM",1,1, RGB(BLACK), RGB(WHITE)
        pause 750 : welcomebanner
      case "d" ' List files on SD card or turn on DEBUG mode when holding WIN key too.
        if winflag% = 1 then
          if debug% = 0 then
            colour rgb(black), rgb(red)
            print "*** Debug Mode On ***"
            setupcolor : debug% = 1
          else
            colour rgb(black), rgb(red)
            print "*** Debug Mode Off ***"
            setupcolor : debug% = 0
          end if
        else
          gui cursor hide : listfiles 'list the files
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
        end if
      case "e" 'turn on local echo in case modem isn't set to echo
        if echosetting$ = "Echo On" then
          gui cursor hide
          welcomebanner : text 400,300, "*** Local Echo Off ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor : print "" : pause 1000
          echosetting$ = "Echo Off" : welcomebanner
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
        else
          gui cursor hide
          text 400,300, "*** Local Echo On ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor : print "" : pause 1000
          echosetting$ = "Echo On" : welcomebanner
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
        end if
      case "f" 'change the font color again
        gui cursor hide : pickcolor : setupcolor
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
      case "h" 'user help screen
          gui cursor hide : cls : dialogHelp
      case "i" 're-initialize the modem
        gui cursor hide : colour rgb(black), rgb(red)
        print "*** SENDING MODEM INITIALIZATION ***" : setupcolor : modeminit
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
      case "l" ' change the line feed TX setting
        gui cursor hide : cls : changelinefeeds : welcomebanner
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
      case "p" 'show the current com port settings
        gui cursor hide : comsettings
      case "q" 'exit the terminal
        termexit
      case "r" 'run the initial modem setup routine again in case it gets wonky
        print "*** RESETTING MODEM ***" : modemreset : pause 250
        modeminit : pause 250 : modeminfo
      case "s" 'enable annoying beep sound for every key press
        if soundflag% = 0 then
          gui cursor hide
          welcomebanner : text 400,300, "*** Sound On ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor : print "" : pause 1000
          welcomebanner : soundflag% = 1
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
        else
          gui cursor hide
          welcomebanner : text 400,300, "*** Sound Off ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor : print "" : pause 1000
          welcomebanner : soundflag% = 0
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
        end if
      case "t" 'change the com port type, TTL or RS-232 levels
        gui cursor hide : setcomtype : startcomport
          if blinkingcursor$ = "Blinking On" then
          gui cursor show
          end if
      case "u" ' underline blinking cursor
        if blinkingcursor$ = "Blinking On" then
          welcomebanner
          text 400,300, "*** Blinking Cursor Off ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor : print "" : pause 1000
          blinkingcursor$ = "Blinking Off" : welcomebanner
        else
        text 400,300, "*** Blinking Cursor On ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor : print "" : pause 1000
          blinkingcursor$ = "Blinking On" : welcomebanner
        end if
      case "v" 'show the credits screen
        if winflag% = 1 then 'credits screen takes ALT and WIN keys to show
          credits
        end if
      case "x" 'hangup the modem/close the connection
        echosetting$ = "Echo Off" : hangup
    end select
  else
      select case asc(CHAR_OUT$)
        case 128
          'up
          print #5, CHR$(27) + "[A";
        case 129
          'down
          print #5, CHR$(27) + "[B";
        case 130
          'left
          print #5, CHR$(27) + "[D";
        case 131
          'right
          print #5, CHR$(27) + "[C";
        case 136
          'page up
          print #5, CHR$(27) + "[V";
        case 137
          'page down
          print #5, CHR$(27) + "[U";
        case 134
          'home
          print #5, CHR$(27) + "[H";
        case 135
          'end
          print #5, CHR$(27) + "[K";
        case else
          print #5, CHAR_OUT$;
          if linefeedstate$ = "LF On" and CHAR_OUT$ = chr$(13) then
            print #5, ""
          end if
       end select
  end if
  if echosetting$ = "Echo On" and CHAR_OUT$ <> "" then
      print CHAR_OUT$;
      if CHAR_OUT$ = chr$(13) then 'workaround for mmbasic bug
        print ""
      end if
  end if
end if
end if
  if blinkingcursor$ = "Blinking On" then
    second$ = right$(time$, 1)
    numtime% = val(second$)
    if numtime% <> cycles% and underscore% = 1 then
      gui cursor show
      let cycles% = numtime%
      underscore% = 0
    end if
    if numtime% <> cycles% and underscore% = 0 then
      gui cursor hide
      let cycles% = numtime%
      underscore% = 1
    end if
  else
    gui cursor hide
  end if
  get_serial_input()
loop
end sub


sub loadconfig 'loading program configuration settings
open "settings.cfg" for input as #7
line input #7, comporttype$
line input #7, linefeedstate$
line input #7, modeminitstring$
line input #7, echosetting$
line input #7, comportstr$
line input #7, text_colorstr$
line input #7, comspeed$
line input #7, blinkingcursor$
close #7
  select case text_colorstr$
    case "White"
    text_color = 1
    case "Amber"
    text_color = 2
    case "Green"
    text_color = 3
  end select
end sub


sub saveconfig 'save program configuration settings
open "settings.cfg" for output as #7
print #7, comporttype$
print #7, linefeedstate$
print #7, modeminitstring$
print #7, echosetting$
print #7, comportstr$
print #7, text_colorstr$
print #7, comspeed$
print #7, blinkingcursor$
close #7
end sub


sub loadphonebook 'not in any standard phone book format
  if mm.info(FILESIZE "bbslist.cfg") <> -1 then
    open "bbslist.cfg" for input as #6
line input #6, phonebookentry$(1):line input #6, phonebookusername$(1):line input #6, phonebookpassword$(1)
line input #6, phonebookentry$(2):line input #6, phonebookusername$(2):line input #6, phonebookpassword$(2)
line input #6, phonebookentry$(3):line input #6, phonebookusername$(3):line input #6, phonebookpassword$(3)
line input #6, phonebookentry$(4):line input #6, phonebookusername$(4):line input #6, phonebookpassword$(4)
line input #6, phonebookentry$(5):line input #6, phonebookusername$(5):line input #6, phonebookpassword$(5)
line input #6, phonebookentry$(6):line input #6, phonebookusername$(6):line input #6, phonebookpassword$(6)
line input #6, phonebookentry$(7):line input #6, phonebookusername$(7):line input #6, phonebookpassword$(7)
line input #6, phonebookentry$(8):line input #6, phonebookusername$(8):line input #6, phonebookpassword$(8)
line input #6, phonebookentry$(9):line input #6, phonebookusername$(9):line input #6, phonebookpassword$(9)
line input #6, phonebookentry$(10):line input #6, phonebookusername$(10):line input #6, phonebookpassword$(10)
close #6
end if
end sub


sub savephonebook ''not in any standard phone book format
open "bbslist.cfg" for output as #6
print #6, phonebookentry$(1):print #6, phonebookusername$(1):print #6, phonebookpassword$(1)
print #6, phonebookentry$(2):print #6, phonebookusername$(2):print #6, phonebookpassword$(2)
print #6, phonebookentry$(3):print #6, phonebookusername$(3):print #6, phonebookpassword$(3)
print #6, phonebookentry$(4):print #6, phonebookusername$(4):print #6, phonebookpassword$(4)
print #6, phonebookentry$(5):print #6, phonebookusername$(5):print #6, phonebookpassword$(5)
print #6, phonebookentry$(6):print #6, phonebookusername$(6):print #6, phonebookpassword$(6)
print #6, phonebookentry$(7):print #6, phonebookusername$(7):print #6, phonebookpassword$(7)
print #6, phonebookentry$(8):print #6, phonebookusername$(8):print #6, phonebookpassword$(8)
print #6, phonebookentry$(9):print #6, phonebookusername$(9):print #6, phonebookpassword$(9)
print #6, phonebookentry$(10):print #6, phonebookusername$(10):print #6, phonebookpassword$(10)
close #6
end sub


function getchar$() as string 'capture special keys pressed during typing
getchar$ = ""
keyflag% = keydown(7)
if keydown(7) > 0 and keyflag% <> lastmodifier% then
    select case keyflag%
      case 1, 16, 17 'ALT Keys
        altflag% = 1 : winflag% = 0 : ctrlflag% = 0 : shiftflag% = 0
      case 4, 64, 68 'WIN keys
        altflag% = 0 : winflag% = 1 : ctrlflag% = 0 : shiftflag% = 0
      case 2, 32, 34 'Control Keys
        altflag% = 0 : winflag% = 0 : ctrlflag% = 1 : shiftflag% = 0
      case 8, 128, 136 'Shift keys
        altflag% = 0 : winflag% = 0 : ctrlflag% = 0 : shiftflag% = 1
      case 5, 20, 21, 65, 80, 81, 85 'ALT+WIN Keys
        altflag% = 1 : winflag% = 1 : ctrlflag% = 0 : shiftflag% = 0
      case else
    end select
end if
if keydown(7) = 0 then
  altflag% = 0 : winflag% = 0 : ctrlflag% = 0 : shiftflag% = 0
end if
lastmodifier% = keyflag% : keyvalue% = keydown(1)
  if keydown(0) > 0 and keylast% <> keyvalue% then
    getchar$ = chr$(keyvalue%) : getchar$ = lcase$(getchar$)
  end if
keylast% = keyvalue% : pause 5
end function


sub introscreen 'opening intro
  const ox = 35 : const oy = 15 : cls
  box ox*fwidth%, oy*fheight%, 28*fwidth%, 14*fheight%, 1,,rgb(black)
  print @((ox+2)*fwidth%,(oy+1)*fheight%) "        MAXITERM";
  print @((ox+2)*fwidth%,(oy+2)*fheight%) "        --------";
  print @((ox+2)*fwidth%,(oy+3)*fheight%) "";
  print @((ox+2)*fwidth%,(oy+4)*fheight%) "        for  the";
  print @((ox+2)*fwidth%,(oy+5)*fheight%) "    Color Maximite 2";
  print @((ox+2)*fwidth%,(oy+6)*fheight%) "";
  print @((ox+2)*fwidth%,(oy+7)*fheight%) "       Version 1.9.5";
  print @((ox+2)*fwidth%,(oy+8)*fheight%) "           by";
  print @((ox+2)*fwidth%,(oy+9)*fheight%) "       Jay Crutti";
  print @((ox+2)*fwidth%,(oy+10)*fheight%)"          2021";
  print @((ox+2)*fwidth%,(oy+11)*fheight%)"";
  print @((ox+2)*fwidth%,(oy+12)*fheight%)"    www.jaycrutti.com";
end sub


sub welcomebanner 'show the opening text at the top of the screen
cls : print "Terminal running. ALT-Q to Exit. ALT-H for Help."
end sub


sub pickcolor 'pick your desired font color
local textchoice$
local done%
print ""
print ""
do
input "Which color do you want 1.White [DEFAULT], 2.Amber, or 3.Green "; textchoice$
select case textchoice$
  case "","1" ' hitting enter or 1
    text_color = 1 : text_colorstr$ = "White" : setupcolor
    print "White Selected."
    done% = 1
  case "2"
    text_color = 2 : text_colorstr$ = "Amber" : setupcolor
    print "Amber Selected."
    done% = 1
  case "3"
    text_color = 3 : text_colorstr$ = "Green" : setupcolor
    print "Green Selected."
    done% = 1
  case else
    print "Invalid Selection, please try again."
    done% = 0
end select
loop until done% = 1
end sub


sub changelinefeeds 'choose if you want a LF after your CR's
local lfchoice$
local done% = 1
do
print ""
print "Send Line Feeds after Carriage Return?"
print "1.No [DEFAULT]"
print "2.Yes"
input "Make Selection;"; lfchoice$
select case lfchoice$
  case "", "1" ' hitting enter or 1
    linefeedstate$ = "LF Off" : print "Line Feeds will not be sent."
    done% = 1
  case "2"
    linefeedstate$ = "LF On" :  print "Line Feeds will be sent."
    done% = 1
  case else
    print "Invalid Selection, please try again."
    done% = 0
end select
pause 1200
loop until done% = 1
end sub


sub changeecho 'set local echo. NOT ECHO THRU MODEM
local echochoice$
local done% = 1
do
print @(0,420)""
print "Turn on Local Echo?"
print "1.No [DEFAULT]"
print "2.Yes"
input "Make Selection;"; echochoice$
select case echochoice$
  case "", "1" ' hitting enter or 1
    echosetting$ = "Echo Off" : print "Local Echo is Off."
    done% = 1
  case "2"
    echosetting$ = "Echo On"  : print "Local Echo is On."
    done% = 1
  case else
    print "Invalid Selection, please try again."
    done% = 0
end select
pause 1200
loop until done% = 1
end sub


sub setcomport 'COM port selection
local done% = 1
do
print ""
input "Choose COM Port, COM 1 [DEFAULT], 2, or 3 "; comchoice$
select case comchoice$
  case "", "1" 'hitting enter or 1
    comportstr$ = "COM1" :  print "COM1 Selected."
    done% = 1
  case "2"
    comportstr$ = "COM2" : print "COM2 Selected."
    done% = 1
  case "3"
    comportstr$ = "COM3" : print "COM3 (via USB Type B port) Selected."
    if mm.errno <> 0 then
      Print "Error: ";mm.errmsg$,
    end if
    done% = 1
  case else
    print "Invalid COM port, please try again."
    pause 1200 ' wait for them to read the response
    done% = 0 ' start over
end select
loop until done% = 1
startcomport
end sub


sub setcomtype 'choosing RS-232 inverts the logic levels only, not the voltages.
local comtype$
local done% = 1
do
onlineflag% = 0 'disable so incoming data doesn't disturb our decision
print ""
print ""
print "Select COM Port Type"
print "1) TTL Serial [DEFAULT]"
print "2) RS-232 Serial"
input "Make Selection: ", comtype$
select case comtype$
  case "", "1" 'hitting enter or 1
    comporttype$ = "TTL Serial" 'this string is important for the settings.cfg file!
    print "TTL Serial Selected."
    done% = 1
  case "2"
    comporttype$ = "RS-232 Serial"
    print "RS-232 Serial Selected."
    done% = 1
  case else
    print "Invalid selection, please try again."
    pause 1200
    done% = 0
end select
loop until done% = 1
startcomport
onlineflag% = 1 'annnd we're back
end sub


sub setcomspeed
onlineflag% = 0 'disable so incoming data doesn't disturb our decision
local done% = 1
do
print ""
print ""
print "Select COM Port Speed"
print "1) 1200 BPS" 'CMM2 doesn't support 300 baud.
print "2) 2400 BPS"
print "3) 4800 BPS"
print "4) 9600 BPS"
print "5) 19200 BPS"
print "6) 38400 BPS"
print "7) 57600 BPS"
print "8) 115200 BPS [DEFAULT]"
input "Make Selection: ", comspeedchoice$
  select case comspeedchoice$
    case "", "8" ' the default choice is fastest
      print "115200 Selected." : comspeed$ = "115200"
      done% = 1
    case "1"
      print "1200 Selected." : comspeed$ = "1200"
      done% = 1
    case "2"
      print "2400 Selected." : comspeed$ = "2400"
      done% = 1
    case "3"
      print "4800 Selected." : comspeed$ = "4800"
      done% = 1
    case "4"
      print "9600 Selected." : comspeed$ = "9600"
      done% = 1
    case "5"
      print "19200 Selected." : comspeed$ = "19200"
      done% = 1
    case "6"
      print "38400 Selected." : comspeed$ = "38400"
      done% = 1
    case "7"
      print "57600 Selected." : comspeed$ = "57600"
      done% = 1
    case else
      print "Invalid selection. Please try again."
      pause 1200
      done% = 0
  end select
loop until done% = 1
onlineflag% = 1 'enable so we're back online
startcomport
end sub


sub setupcolor ' translate our font choice into RGB values
select case TEXT_COLOR
  case 1
TERM_COLOR1 = fg_colour1
TERM_COLOR2 = fg_colour2
TERM_COLOR3 = fg_colour3
  case 2
TERM_COLOR1 = 255
TERM_COLOR2 = 176
TERM_COLOR3 = 0
  case 3
TERM_COLOR1 = 51
TERM_COLOR2 = 255
TERM_COLOR3 = 0
end select
colour rgb(TERM_COLOR1, TERM_COLOR2, TERM_COLOR3), rgb(bg_colour1,bg_colour2,bg_colour3)
end sub


sub startcomport 'start the physical COM port
ON ERROR SKIP 'needed because if the port is already open for next command, we'll crash
close #5
ON ERROR ABORT 'back to normal
  if comporttype$ = "RS-232 Serial" then
    open comportstr$+":"+comspeed$+","+"8192"+",INV" as #5
  else
    open comportstr$+":"+comspeed$+","+"8192" as #5
  end if
end sub


sub modemreset 'usually ATZ
print #5; modemresetstring$
pause 250 ' wait for modem to process
end sub


sub modeminit 'if no string is saved by the user then use the one from the dim above
print #5; modeminitstring$
pause 250 'wait for modem to process
end sub


sub modeminfo 'Ask modem for info with ATI
print #5; modeminfostring$
end sub


sub get_serial_input 'collect the data from the serial port
  local i%
  local cursor_x%
  local cursor_y%

  if xmodem_up$<>"" or xmodem_down$<>"" then
    CHARS_IN$ = input$(1,#5) ' only one char at a time
    if CHARS_IN$ <> "" then
      _xmodem_handler CHARS_IN$
    end if
  else
    if onlineflag% = 1 then
'      print CHARS_IN$;

      CHARS_IN$ = input$(1,#5)
      if CHARS_IN$ <> "" then
        if escape_flag% = 0 then
          if CHARS_IN$ = CHR$(27) then
            escape_flag% = 1
          else
            print CHARS_IN$;
            if soundflag% = 1 AND CHARS_IN$ = chr$(13) then
              ON ERROR IGNORE
              PLAY mp3 "sound.mp3"
              On ERROR ABORT
            end if
          end if
        elseif escape_flag% = 1 then
          if CHARS_IN$ = "[" then
            escape_flag% = 2
            for i% = 0 to 9
              params%(i%) = 0
            next i%
            param_at% = 0
          else
            escape_flag% = 0
          end if
        elseif escape_flag% = 2 then
          if CHARS_IN$ = ";" then
            param_at% = param_at% + 1
          elseif asc(CHARS_IN$) >= 48 and asc(CHARS_IN$) <= 57 then
            if param_at% = 0 then
              param_at% = 1
            endif
            params%(param_at% - 1) = params%(param_at% - 1) * 10 + (ASC(CHARS_IN$) - ASC("0"))
          else
            escape_flag% = 3
          end if
        end if

        if escape_flag% = 3 then
          select case CHARS_IN$
            case "m"
              for i% = 1 to param_at%
                select case params%(i% - 1)
                  case 0
                    bold% = 0
                    fg_colour1 = 170
                    fg_colour2 = 170
                    fg_colour3 = 170
                    bg_colour1 = 0
                    bg_colour2 = 0
                    bg_colour3 = 0
                  case 1
                    bold% = 1
                    if fg_colour1 = 0 then
                      fg_colour1 = 85
                    else if fg_colour1 = 170 then
                      fg_colour1 = 255
                    end if
                    if fg_colour2 = 0 then
                      fg_colour2 = 85
                    else if fg_colour2 = 170 then
                      fg_colour2 = 255
                    end if
                    if fg_colour3 = 0 then
                      fg_colour3 = 85
                    else if fg_colour3 = 170 then
                      fg_colour3 = 255
                    end if
                  case 30
                    if bold% = 1 then
                      fg_colour1 = 85
                      fg_colour2 = 85
                      fg_colour3 = 85
                    else
                      fg_colour1 = 0
                      fg_colour2 = 0
                      fg_colour3 = 0
                    end if
                  case 31
                    if bold% = 1 then
                      fg_colour1 = 255
                      fg_colour2 = 85
                      fg_colour3 = 85
                    else
                      fg_colour1 = 170
                      fg_colour2 = 0
                      fg_colour3 = 0
                    end if
                  case 32
                    if bold% = 1 then
                      fg_colour1 = 85
                      fg_colour2 = 255
                      fg_colour3 = 85
                    else
                      fg_colour1 = 0
                      fg_colour2 = 170
                      fg_colour3 = 0
                    end if
                  case 33
                    if bold% = 1 then
                      fg_colour1 = 255
                      fg_colour2 = 255
                      fg_colour3 = 85
                    else
                      fg_colour1 = 170
                      fg_colour2 = 85
                      fg_colour3 = 0
                    end if
                  case 34
                    if bold% = 1 then
                      fg_colour1 = 85
                      fg_colour2 = 85
                      fg_colour3 = 255
                    else
                      fg_colour1 = 0
                      fg_colour2 = 0
                      fg_colour3 = 170
                    end if
                  case 35
                    if bold% = 1 then
                      fg_colour1 = 255
                      fg_colour2 = 85
                      fg_colour3 = 255
                    else
                      fg_colour1 = 170
                      fg_colour2 = 0
                      fg_colour3 = 170
                    end if
                  case 36
                    if bold% = 1 then
                      fg_colour1 = 85
                      fg_colour2 = 255
                      fg_colour3 = 255
                    else
                      fg_colour1 = 0
                      fg_colour2 = 170
                      fg_colour3 = 170
                    end if
                  case 37
                    if bold% = 1 then
                      fg_colour1 = 255
                      fg_colour2 = 255
                      fg_colour3 = 255
                    else
                      fg_colour1 = 170
                      fg_colour2 = 170
                      fg_colour3 = 170
                    end if
                  case 40
                    bg_colour1 = 0
                    bg_colour2 = 0
                    bg_colour3 = 0
                  case 41
                    bg_colour1 = 170
                    bg_colour2 = 0
                    bg_colour3 = 0
                  case 42
                    bg_colour1 = 0
                    bg_colour2 = 170
                    bg_colour3 = 0
                  case 43
                    bg_colour1 = 170
                    bg_colour2 = 85
                    bg_colour3 = 0
                  case 44
                    bg_colour1 = 0
                    bg_colour2 = 0
                    bg_colour3 = 170
                  case 45
                    bg_colour1 = 170
                    bg_colour2 = 0
                    bg_colour3 = 170
                  case 46
                    bg_colour1 = 0
                    bg_colour2 = 170
                    bg_colour3 = 170
                  case 47
                    bg_colour1 = 170
                    bg_colour2 = 170
                    bg_colour3 = 170
                end select
              next i%
              colour RGB(fg_colour1, fg_colour2, fg_colour3), RGB(bg_colour1, bg_colour2, bg_colour3)
            case "H","f"
              if params%(1) > 0 then
                params%(1) = params%(1) - 1
              end if

              if params%(0) > 0 then
                params%(0) = params%(0) - 1
              end if
              
              cursor_x% = params%(1)
              cursor_y% = params%(0)

              if cursor_x% < 0 then 
                cursor_x% = 0
              elseif cursor_x% > MM.HRES / fwidth% - 1 then
                cursor_x% = MM.HRES / fwidth% - 1
              end if

              if cursor_y% < 0 then
                cursor_y% = 0
              elseif cursor_y% > MM.VRES / fheight% - 1 then
                cursor_y% = MM.VRES / fheight% - 1
              end if

              print @(fwidth% * cursor_x%, fheight% * cursor_y%) "";
            case "A"
              if param_at% > 0 then
                cursor_y% = MM.INFO(VPOS) - fheight% * params%(0)
              else
                cursor_y% = MM.INFO(VPOS) - fheight%
              end if
              if cursor_y% >= 0 then
                print @(MM.INFO(HPOS), cursor_y%) "";
              else
                print @(MM.INFO(HPOS), 0) "";
              end if
            case "B"
              if param_at% > 0 then
                cursor_y% = MM.INFO(VPOS) + fheight% * params%(0)
              else
                cursor_y% = MM.INFO(VPOS) + fheight%
              end if
              if cursor_y% <= MM.VRES - fheight% then
                print @(MM.INFO(HPOS), cursor_y%) "";
              else
                print @(MM.INFO(HPOS), MM.VRES - fheight%) "";
              end if
            case "C"
              if param_at% > 0 then
                cursor_x% = MM.INFO(HPOS) + fwidth% * params%(0)
              else
                cursor_x% = MM.INFO(HPOS) + fwidth%
              end if
              if cursor_x% <= MM.HRES - fwidth% then
                print @(cursor_x%, MM.INFO(VPOS)) "";
              else
                print @(MM.HRES - fwidth%, MM.INFO(VPOS)) "";
              end if
            case "D"
              if param_at% > 0 then
                cursor_x% = MM.INFO(HPOS) - fwidth% * params%(0)
              else
                cursor_x% = MM.INFO(HPOS) - fwidth%
              end if
              if cursor_x% >= 0 then
                print @(cursor_x%, MM.INFO(VPOS)) "";
              else
                print @(0, MM.INFO(VPOS)) "";
              end if
            case "s"
              save_x% = MM.INFO(HPOS)
              save_y% = MM.INFO(VPOS)
            case "u"
              print @(save_x%, save_y%) "";
            case "J"
              if param_at% = 0 then
                box 0, MM.INFO(VPOS), MM.HRES, MM.VRES,0, , rgb(bg_colour1, bg_colour2, bg_colour3)
                print @(0, 0) "";
              else
                if params%(0) = 1 then
                  box 0, 0, MM.HRES, MM.INFO(VPOS) - fheight%,0,, rgb(bg_colour1, bg_colour2, bg_colour3)
                else if params%(0) = 2 then
                  box 0, 0, MM.HRES, MM.VRES,0, , rgb(bg_colour1, bg_colour2, bg_colour3)
                end if
              end if
            case "K"
              if params%(0) = 0 then
                box MM.INFO(HPOS), MM.INFO(VPOS), MM.HRES - MM.INFO(HPOS), fheight%,0, , rgb(bg_colour1, bg_colour2, bg_colour3)
              else
                if params%(0) = 1 then
                  box 0, MM.INFO(VPOS), MM.INFO(HPOS), fheight%,0, , rgb(bg_colour1, bg_colour2, bg_colour3)
                else if params%(0) = 2 then
                  box 0, MM.INFO(VPOS), MM.HRES, fheight%,0, , rgb(bg_colour1, bg_colour2, bg_colour3)
                end if
              end if
            case "n"
              if params%(0) = 6 then 'report cursor position
                print #5, CHR$(27) + "[" + STR$(MM.INFO(VPOS) / fheight% + 1) + ";" + STR$(MM.INFO(HPOS) / fwidth% + 1) + "R"
              end if
          end select
          escape_flag% = 0
        end if
      end if
    end if
  end if
end sub


sub terminalonline 'just letting the user know we're ready to boogie
    print chr$(13); chr$(10); "*** TERMINAL ONLINE ***"
    setupcolor
    print ""
end sub


sub download
cls
onlineflag% = 0
print "Xmodem Download"
input "Enter Filename: "; receivefile$
    if receivefile$ = "" then
      print chr$(13); chr$(10); "*** Download Cancelled ***"
      pause 1500
      welcomebanner
      'terminal
      exit sub
  else
    print "Please wait, downloading "; receivefile$
    print ""
    _xmodem_recv receivefile$
    onlineflag% = 1
  end if
end sub


sub upload
cls
print "Xmodem Upload"
FileDialog(NameOfFile$())   ' no options so allow any file to be selected
  if NameOfFile$(0) = "" then
    welcomebanner
    print chr$(13); chr$(10); "*** Upload Cancelled ***"
    pause 1500
    'terminal
    exit sub
  else
    cls
    print "Please wait, uploading "; NameOfFile$(0)
    _xmodem_send NameOfFile$(0)
  end if
end sub


sub listfiles
cls
FileDialog(NameOfFile$())   ' no options so allow any file to be selected
welcomebanner
end sub


sub hangup
  print chr$(13); chr$(10); "*** DISCONNECTING ***"
    onlineflag% = 0 'disable incoming data display while hanging up
      print #5; chr$(13); chr$(10)
      pause 1200 'take our time. too fast and modem will ignore
      print #5; chr$(43);chr$(43);chr$(43);
      print "+++"
      pause 1500 'take our time. too fast and modem will ignore
      print "" : pause 100
      print #5; " ATH0"
      print "*** DISCONNECTED ***"
    onlineflag% = 1 'back online
end sub


sub termexit
  close #5
  print chr$(13); chr$(10); "*** EXITING TERMINAL ***"
  gui cursor off
pause 750
end
end sub


sub changeinitstring
local newinitstring$
  print @(0,420) ""
  print "Current modem initialization string: ";modeminitstring$
  input "Enter new modem initialization string: ", newinitstring$
    if newinitstring$ <> "" then
      print "Changing modem initialization string to "; newinitstring$
      modeminitstring$ = newinitstring$
      pause 1500
    else
      print "Not updated."
      pause 1500
    end if
end sub


sub comsettings
local comwindow$
local done% = 1
const ox = 20
const oy = 6
do
  cls
  box ox*fwidth%, oy*fheight%, 60*fwidth%, 19*fheight%, 1,,rgb(black)
  print @((ox+2)*fwidth%,(oy+1)*fheight%) "COM PORT AND OTHER SETTINGS";
  print @((ox+2)*fwidth%,(oy+2)*fheight%) "---------------------------";
  print @((ox+2)*fwidth%,(oy+3)*fheight%) "A.COM PORT                :",comportstr$
  print @((ox+2)*fwidth%,(oy+4)*fheight%) "B.BAUD RATE               :",comspeed$
  print @((ox+2)*fwidth%,(oy+5)*fheight%) "C.COM PORT TYPE           :",comporttype$
  print @((ox+2)*fwidth%,(oy+6)*fheight%) "D.DATA BITS               : 8";
  print @((ox+2)*fwidth%,(oy+7)*fheight%) "E.PARITY                  : NONE";
  print @((ox+2)*fwidth%,(oy+8)*fheight%) "F.FLOW CONTROL            : (NOT IMPLEMENTED)";
  print @((ox+2)*fwidth%,(oy+9)*fheight%) "G.STOP BITS               : 1";
  print @((ox+2)*fwidth%,(oy+10)*fheight%)"H.SEND LINE FEED AFTER CR :",linefeedstate$
  print @((ox+2)*fwidth%,(oy+11)*fheight%)"I.INIT STRING             :",modeminitstring$
  print @((ox+2)*fwidth%,(oy+12)*fheight%)"J.LOCAL ECHO              :",echosetting$
  print @((ox+2)*fwidth%,(oy+13)*fheight%)"K.FONT COLOR              :",text_colorstr$
  print @((ox+2)*fwidth%,(oy+14)*fheight%)"L.BLINKING CURSOR         :",blinkingcursor$
  print @((ox+2)*fwidth%,(oy+15)*fheight%)"";
  print @((ox+2)*fwidth%,(oy+16)*fheight%)"To change settings, enter letter or hit enter to exit.";
  print @((ox+2)*fwidth%,(oy+17)*fheight%)"Enter S) to save. Make Selection"; : input comwindow$,
select case comwindow$
  case "" ' they hit enter
    print @(0,420) "Returning to terminal."
    pause 1200 : welcomebanner : done% = 1
  case "a", "A"
    print chr$(10),chr$(13)
    setcomport : pause 1200 : done% = 0
  case "b", "B"
    setcomspeed : pause 1200 : done% = 0
  case "c", "C"
    setcomtype : pause 1200 : done% = 0
  case "d", "D"
    print @(0,420) "Option not implemented yet."
    pause 1500 : done% = 0
  case "e", "E"
    print @(0,420) "Option not implemented yet."
    pause 1500 : done% = 0
  case "f", "F"
    print @(0,420) "Option not implemented yet."
    pause 1500 : done% = 0
  case "g", "G"
    print @(0,420) "Option not implemented yet."
    pause 1500 : done% = 0
  case "h", "H"
    print chr$(10),chr$(13) : changelinefeeds : pause 1200 : done% = 0
  case "i", "I"
    changeinitstring : pause 1200 : done% = 0
  case "j", "J"
    changeecho : pause 1200 : done% = 0
  case "k", "K"
    pickcolor : pause 1200 : done% = 0
  case "l", "L"
        if blinkingcursor$ = "Blinking On" then
          cls
          text 400,300, "*** Blinking Cursor Off ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor : print "" : pause 1000
          blinkingcursor$ = "Blinking Off" : cls
        else
          cls
          text 400,300, "*** Blinking Cursor On ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor : print "" : pause 1000
          blinkingcursor$ = "Blinking On" : cls
        end if
    pause 1200 : done% = 0
  case "s", "S"
    print @(0,420) "Saving Configuration to settings.cfg"
    saveconfig : pause 1500 : done% = 0
  case else
    print @(0,420) "Invalid option. Try again." : pause 1500 : done% = 0
end select
loop until done% = 1
end sub


sub dialoghelp
  const ox = 30
  const oy = 15
  cls
  box ox*fwidth%, oy*fheight%, 40*fwidth%, 24*fheight%, 1,,rgb(black)
  print @((ox+2)*fwidth%,(oy+1)*fheight%) "ALT-A Autodial Phone Book";
  print @((ox+2)*fwidth%,(oy+2)*fheight%) "ALT-B Quick Change COM Port Settings";
  print @((ox+2)*fwidth%,(oy+3)*fheight%) "ALT-C Clear Screen";
  print @((ox+2)*fwidth%,(oy+4)*fheight%) "ALT-D List Local Directory";
  print @((ox+2)*fwidth%,(oy+5)*fheight%) "ALT-E Local Echo on/off";
  print @((ox+2)*fwidth%,(oy+6)*fheight%) "ALT-F Change the Font Color";
  print @((ox+2)*fwidth%,(oy+7)*fheight%) "ALT-H Help Menu";
  print @((ox+2)*fwidth%,(oy+8)*fheight%) "ALT-I Send Modem Initialization";
  print @((ox+2)*fwidth%,(oy+9)*fheight%) "ALT-L Line Feed TX Setting";
  print @((ox+2)*fwidth%,(oy+10)*fheight%)"ALT-P Program and COM Settings";
  print @((ox+2)*fwidth%,(oy+11)*fheight%)"ALT-Q Quit and Exit Terminal";
  print @((ox+2)*fwidth%,(oy+12)*fheight%)"ALT-R Reset the Modem";
  print @((ox+2)*fwidth%,(oy+13)*fheight%)"ALT-S Key Sound on/off";
  print @((ox+2)*fwidth%,(oy+14)*fheight%)"ALT-T Change Com Port Type";
  print @((ox+2)*fwidth%,(oy+15)*fheight%)"ALT-U Toggle Blinking Cursor";
  print @((ox+2)*fwidth%,(oy+16)*fheight%)"ALT-X Disconnect Session";
  print @((ox+2)*fwidth%,(oy+17)*fheight%)"";
  print @((ox+2)*fwidth%,(oy+18)*fheight%)"ALT+Page UP = Upload File";
  print @((ox+2)*fwidth%,(oy+19)*fheight%)"ALT+Page DOWN = Download File";
  print @((ox+2)*fwidth%,(oy+20)*fheight%)"";
  print @((ox+2)*fwidth%,(oy+21)*fheight%)"ALT+WIN-D Toggle debug on/off";
  print @((ox+2)*fwidth%,(oy+22)*fheight%)"ALT+WIN-V Show Version info";
  do while inkey$ = "" : loop
welcomebanner
'terminal
end sub


sub credits
  const ox = 30
  const oy = 15
  cls
  box ox*fwidth%, oy*fheight%, 40*fwidth%, 15*fheight%, 1,,rgb(black)
  print @((ox+2)*fwidth%,(oy+1)*fheight%) "Maxiterm for the Color Maximite 2";
  print @((ox+2)*fwidth%,(oy+2)*fheight%) "---------------------------------";
  print @((ox+2)*fwidth%,(oy+3)*fheight%) "Version 1.9.5";
  print @((ox+2)*fwidth%,(oy+4)*fheight%) "John 'Jay' Crutti Jr. and friends. ";
  print @((ox+2)*fwidth%,(oy+5)*fheight%) "Copyright 2021, MIT LICENSE";
  print @((ox+2)*fwidth%,(oy+6)*fheight%) "";
  print @((ox+2)*fwidth%,(oy+7)*fheight%) "Special thanks to Rich Martin,";
  print @((ox+2)*fwidth%,(oy+8)*fheight%) "Robert Severson, Piotr Siwy, vegipete";
  print @((ox+2)*fwidth%,(oy+9)*fheight%) "Dave Van Wagner, TassyJim, the";
  print @((ox+2)*fwidth%,(oy+10)*fheight%)"Back Shed users, and the 1980's for";
  print @((ox+2)*fwidth%,(oy+11)*fheight%)"code, support, and inspiration.";
  print @((ox+2)*fwidth%,(oy+12)*fheight%)"";
  print @((ox+2)*fwidth%,(oy+13)*fheight%)"Support email: recstudio@gmail.com";
  do while inkey$ = "" : loop
welcomebanner
'terminal
end sub


sub phonebook
local newphoneentry$
local newphoneusername$
local newphonepassword$
local done% = 1
const ox = 3
const oy = 3

do
  cls
  box ox*fwidth%, oy*fheight%, 94*fwidth%, 20*fheight%, 1,,rgb(black)
  print @((ox+2)*fwidth%,(oy+1)*fheight%) "AUTODIAL PHONE BOOK";
  print @((ox+2)*fwidth%,(oy+2)*fheight%) "-------------------";
  print @((ox+2)*fwidth%,(oy+3)*fheight%) "HOSTNAME / PHONE NUMBER:                       USERNAME:         PASSWORD:"
  print @((ox+2)*fwidth%,(oy+4)*fheight%) ""
  print @((ox+2)*fwidth%,(oy+5)*fheight%) "1.", phonebookentry$(1)
  print @((ox+47)*fwidth%,(oy+5)*fheight%)"", phonebookusername$(1)
  print @((ox+66)*fwidth%,(oy+5)*fheight%)"", phonebookpassword$(1)
  print @((ox+2)*fwidth%,(oy+6)*fheight%) "2.", phonebookentry$(2)
  print @((ox+47)*fwidth%,(oy+6)*fheight%)"", phonebookusername$(2)
  print @((ox+66)*fwidth%,(oy+6)*fheight%)"", phonebookpassword$(2)
  print @((ox+2)*fwidth%,(oy+7)*fheight%) "3.", phonebookentry$(3)
  print @((ox+47)*fwidth%,(oy+7)*fheight%)"", phonebookusername$(3)
  print @((ox+66)*fwidth%,(oy+7)*fheight%)"", phonebookpassword$(3)
  print @((ox+2)*fwidth%,(oy+8)*fheight%) "4.", phonebookentry$(4)
  print @((ox+47)*fwidth%,(oy+8)*fheight%)"", phonebookusername$(4)
  print @((ox+66)*fwidth%,(oy+8)*fheight%)"", phonebookpassword$(4)
  print @((ox+2)*fwidth%,(oy+9)*fheight%) "5.", phonebookentry$(5)
  print @((ox+47)*fwidth%,(oy+9)*fheight%)"", phonebookusername$(5)
  print @((ox+66)*fwidth%,(oy+9)*fheight%)"", phonebookpassword$(5)
  print @((ox+2)*fwidth%,(oy+10)*fheight%)"6.", phonebookentry$(6)
  print @((ox+47)*fwidth%,(oy+10)*fheight%)"", phonebookusername$(6)
  print @((ox+66)*fwidth%,(oy+10)*fheight%)"", phonebookpassword$(6)
  print @((ox+2)*fwidth%,(oy+11)*fheight%)"7.", phonebookentry$(7)
  print @((ox+47)*fwidth%,(oy+11)*fheight%)"", phonebookusername$(7)
  print @((ox+66)*fwidth%,(oy+11)*fheight%)"", phonebookpassword$(7)
  print @((ox+2)*fwidth%,(oy+12)*fheight%)"8.", phonebookentry$(8)
  print @((ox+47)*fwidth%,(oy+12)*fheight%)"", phonebookusername$(8)
  print @((ox+66)*fwidth%,(oy+12)*fheight%)"", phonebookpassword$(8)
  print @((ox+2)*fwidth%,(oy+13)*fheight%)"9.", phonebookentry$(9)
  print @((ox+47)*fwidth%,(oy+13)*fheight%)"", phonebookusername$(9)
  print @((ox+66)*fwidth%,(oy+13)*fheight%)"", phonebookpassword$(9)
  print @((ox+2)*fwidth%,(oy+14)*fheight%)"10.", phonebookentry$(10)
  print @((ox+47)*fwidth%,(oy+14)*fheight%)"", phonebookusername$(10)
  print @((ox+66)*fwidth%,(oy+14)*fheight%)"", phonebookpassword$(10)
  print @((ox+2)*fwidth%,(oy+15)*fheight%)"";
  print @((ox+2)*fwidth%,(oy+16)*fheight%)"D) or # to Dial. E) to Edit Host/Phone, L) to Edit Login/PW";
  print @((ox+2)*fwidth%,(oy+17)*fheight%)"C) to Clear an entry, S) to Save the Phonebook, or Enter to Exit.";
  print @((ox+2)*fwidth%,(oy+18)*fheight%)"Make Selection:"; : input dialchoice$,
  select case dialchoice$
      case "" 'they hit enter
        print @(0,420)"Returning to terminal." 'print text below the box
        pause 1200
        welcomebanner
        done% = 1
      case "c" 'clear an entry
        print @(0,420) ""
        input "Enter entry to clear: ", phoneentry%
          if phoneentry% < 1 then ' they hit enter or negative input
                print "Clearing aborted."
                pause 1500
                done% = 0
          end if
          if phoneentry% > 10 then
                print "Clearing aborted."
                pause 1500
                done% = 0
          end if
            print "Clearing entry"; phoneentry%
            phonebookentry$(phoneentry%) = ""
            phonebookusername$(phoneentry%) = ""
            phonebookpassword$(phoneentry%) = ""
            pause 1500
            done% = 0
      case "e" 'edit an entry
        print @(0,420) ""
        input "Enter entry to edit: ", phoneentry%
          if phoneentry% < 1 then
                print "Not updated."
                pause 1500
                done% = 0
          end if
          if phoneentry% > 10 then
                print "Not updated."
                pause 1500
                done% = 0
          end if
            print "Current hostname / phone number: ";phonebookentry$(phoneentry%)
            input "Enter new hostname / phone number: ", newphoneentry$
              if newphoneentry$ <> "" then
                print "Changing Entry";phoneentry%; " to "; newphoneentry$
                phonebookentry$(phoneentry%) = newphoneentry$
                pause 1500
                done% = 0
              end if
      case "d"
        print @(0,420) chr$(10),chr$(13)
        input "Entry # to dial: ", phoneentry%
        print "Dialing entry";phoneentry%;", " phonebookentry$(phoneentry%)
        print #5; "atdt"; phonebookentry$(phoneentry%)"", chr$(13)
      case "l" 'edit the login for an entry
        print @(0,420) ""
        input "Enter entry to edit: ", phoneentry%
          if phoneentry% < 1 then
                print "Not updated."
                pause 1500
                done% = 0
          end if
          if phoneentry% > 10 then
                print "Invalid entry."
                pause 1500
                done% = 0
          end if
            print "Current Username: ";phonebookusername$(phoneentry%)
            print "Current Password: ";phonebookpassword$(phoneentry%)
            input "Enter new Username: ", newphoneusername$
              if newphoneusername$ <> "" then
                print "Changing Username ";phonebookusername$(phoneentry%); " to "; newphoneusername$
                phonebookusername$(phoneentry%) = newphoneusername$
                pause 1500
              else
                print "Not updated."
                pause 1500
              end if
            print chr$(10), chr$(13)
            input "Enter new Password: ", newphonepassword$
              if newphonepassword$ <> "" then
                print "Changing Password ";phonebookpassword$(phoneentry%); " to "; newphonepassword$
                phonebookpassword$(phoneentry%) = newphonepassword$
                pause 1500
                done% = 0
              else
                print "Not updated."
                pause 1500
                done% = 0
              end if
          end if
      case "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"
        phoneentry% = val(dialchoice$)
        print @(0,420) "Dialing entry ";dialchoice$;", " phonebookentry$(phoneentry%)
        print #5; "atdt"; phonebookentry$(phoneentry%)"", chr$(13)
        done% = 1
      case "s" 'save updated phone book to config file
        print @(0,420) "Phonebook Saved."
        savephonebook
        pause 1500
        done% = 0
      case else 'invalid junk
        print @(0,420)"Invalid selection."
        pause 1500
        done% = 0
      end select
  loop until done% = 1
end sub


sub blinkcursor 'currently in main loop
end sub


sub disableblink 'not used
gui cursor off
end sub


'*****************************************************************
' Sub FileDialog(result$() [,spec$][,x_position][,y_position][,height][,width])
'
' This routine displays a centered dialog box on the screen, allows
' the user to choose a file and returns the full path of the chosen
' file. The underlying screen is restored when the dialog closes.
' UP and DOWN arrows to select, ENTER to choose selection
' ESC to cancel, LEFT arrow to go up directory
'
'   version 1.0    Original release vegipete, Oct 2020
'   version 1.1    all navigation by arrow keys only, can return directory names too
'   version 1.2    remembers selection when moving back up directories, can specify
'                  file criteria, forcing selection type
'   version 1.2b   fixed variable declarations so OPTION EXPLICIT works
'
' Input:  result$(): array to hold results, dim(1) for single result, more for multi
'         spec$: optional - wildcard matching, plus following
'           standard functions:
'             "*"        show all files, allow any file to be selected - default
'             "*.BAS"    show only request file types, standard MMBasic wildcard matching
'             "<DIR>"    show all files, allow only selection of directories
'           multi-select functions (only if result$ is an array, otherwise defaults to standard):
'             "<M>*"     show and multi-select all file types
'             "<M>*.BAS" show and multi-select specified file types
'             "<MDIR>"   show all, multi-select only directories
'             "<MALL>"   show all, multi-select files and directories
'           save file function:
'             "<SAVE>["starting value"]" show all, returns path of displayed directory + string
'                        from text box, which is initialized with the optional string.
'         x_position:  optional, default = centered dialog
'         y_position:  optional, default = centered dialog
'         height:      optional, height in characters of directory/file list, default 15
'         width:       optional, width in characters of directory/file list, default 32
'
' Output: result$(0) contains full path of [first] item chosen, or "" if nothing
'         result$(1-n) contain full path of remaining items chosen, or "" if nothing
'         Note: the directory part of the path will be capitalized. This is just
'         how the CWD$ function works. Fortunately, MMBasic is case insensitive.
'
' The following global variables should be declared before use:
' CONST DIRCOUNT = 50   ' max number of sub-directories
' CONST FILCOUNT = 255  ' max number of files
' CONST NAMELENGTH = 64
' DIM dir_dirs$(DIRCOUNT) length NAMELENGTH  ' store list of directories
' DIM dir_fils$(FILCOUNT) length NAMELENGTH  ' store list of files
' DIM dir_hist$(DIRCOUNT) length 8 ' store directory number visited along path
' DIM d_cwidth, d_x, d_y, d_lines
' DIM d_colours(3) = (&hA0A040,&h101010,&hFFFFFF,&h303030) 'array of 4 colour values
'    'd_colours(0)  frame               (&hA0A040 sort-of gold     )
'    'd_colours(1)  body                (&h101010 really dark grey )
'    'd_colours(2)  text                (&hFFFFFF white            )
'    'd_colours(3)  shadow              (&h303030 dark sort-of gold)
'
' Routines Used:  (included below)
'   sub ReadDir(d_spec$,d_top_item,d_sel_item,d_top_last) ' reads current directory into the above arrays
'   sub ListDir(first, nlines, hilite)  ' shows a portion of the current directory
'
Sub FileDialog(result$(), p_spec$, p_x, p_y, p_height, p_width)
  local d_spec$, d_startdir$, d_height, d_mode, d_width
  local d_framec, d_backc, d_textc, d_shadowc, d_k, d_changed
  local d_top_item, d_sel_item, d_top_last, d_chosen

  ' dialog box dimensions
  ' test parameters, fill in defaults if needed
  d_spec$ = p_spec$ : if d_spec$ = "" then d_spec$ = "*"
  d_lines = p_height : if d_lines = 0 then d_lines = 15     ' height in characters
  d_height = 50 + (d_lines - 1) * MM.INFO(FONTHEIGHT)       ' height in pixels
  d_mode = 0      ' getfile mode
  if ucase$(left$(d_spec$,6)) = "<SAVE>" then
    d_height = d_height + MM.INFO(FONTHEIGHT) + 3           ' make room for filename text line
    d_mode = 1    ' savefile mode
  endif
  d_cwidth = p_width : if d_cwidth = 0 then d_cwidth = 32   ' width in characters
  d_width = 44 + d_cwidth * MM.INFO(FONTWIDTH)              ' width in pixels
  d_x = p_x : if d_x = 0 then d_x = (MM.HRES - d_width)/2   ' location of top left corner
  d_y = p_y : if d_y = 0 then d_y = (MM.VRES - d_height)/2  '    of dialog box

  d_startdir$ = cwd$      ' save starting directory
  for d_k = 1 to DIRCOUNT ' set all elements to 1 - 1st item selected
    dir_hist$(d_k) = "1,1"
  next d_k
  dir_hist$(0) = "1"      ' initially at top directory level
  if d_startdir$ <> "A:/" then  ' determine starting directory depth
    d_startdir$ = d_startdir$ + "/"
    for d_k = 1 to len(d_startdir$)
      if mid$(d_startdir$,d_k,1) = "/" then
        dir_hist$(0) = str$(val(dir_hist$(0)) + 1) ' another level deeper
      endif
    next d_k
  endif

  ' save underlying screen image in buffer #64
  blit read 64, d_x, d_y, d_width, d_height
  ' draw dialog box
  rbox d_x + 7, d_y +  7, d_width -  8, d_height -  8, 10, d_colours(3), d_colours(3) ' shadow
  rbox d_x    , d_y     , d_width -  8, d_height -  8, 10, d_colours(0), d_colours(0) ' frame
  rbox d_x + 5, d_y + 22, d_width - 18, d_height - 34,  5, d_colours(1), d_colours(1) ' text area
  if ucase$(d_spec$) = "<DIR>" then
    text d_x+10,d_y+6,"Select Directory...", "LT", 1, 1, 0, -1
  else
    text d_x+10,d_y+6,"Select File...", "LT", 1, 1, 0, -1
  endif
  text d_x+d_width-12,d_y+1,"1", "RT", 11, 1, 0, -1  ' Arrow/Ent/Esc/space

  '--------------------
  ReadDir(d_spec$,d_top_item,d_sel_item,d_top_last)
  ListDir(d_top_item, d_lines, d_sel_item)  ' populate the dialog box

  do
    d_k = asc(inkey$)
    d_changed = 0
    select case d_k
      case  27  ' ESC
        result$(0) = ""  ' Cancel so return blank
        exit do
      case 128  ' UP arrow
        if d_sel_item = 1 then  ' is the top item selected?
          if d_top_item > 1 then  ' at top of list?
            d_top_item = d_top_item - 1  ' no so shift list up one
            d_changed = 1
          endif
        else
          d_sel_item = d_sel_item - 1  ' shift selection up one
          d_changed = 1
        endif
      case 129  ' DOWN arrow
        if d_sel_item = d_lines then  ' is the bottom item selected?
          if d_top_item < d_top_last then  ' at bottom of list?
            d_top_item = d_top_item + 1  ' no so shift list down one
            d_changed = 1
          endif
        else if d_sel_item < val(dir_dirs$(0)) + val(dir_fils$(0)) then
          ' don't shift down past last item
          d_sel_item = d_sel_item + 1  ' shift selection down one
          d_changed = 1
        endif
      case 130  ' LEFT Arrow - directory up if not root
        if cwd$ <> "A:/" then ' in a sub-directory?
          chdir ".."     'directory up chosen
          ReadDir(d_spec$,d_top_item,d_sel_item,d_top_last)
          dir_hist$(0) = str$(val(dir_hist$(0)) - 1)
          d_top_item = val(field$(dir_hist$(val(dir_hist$(0))),1,","))
          d_sel_item = val(field$(dir_hist$(val(dir_hist$(0))),2,","))
          d_changed = 1
        endif
      case 131  ' RIGHT Arrow - directory down if directory selected
        d_chosen = d_top_item + d_sel_item - 1
        if d_chosen <= val(dir_dirs$(0)) then ' item number in directory range?

          dir_hist$(val(dir_hist$(0))) = str$(d_top_item) + "," + str$(d_sel_item)
          'dir_hist$(dir_hist$(0)) = d_chosen    ' save selection number if we come back up
          dir_hist$(0) = str$(val(dir_hist$(0)) + 1)

          if right$(cwd$,1) = "/" then
            chdir cwd$ + dir_dirs$(d_chosen)  ' tunnel down a directory from root
          else
            chdir cwd$ + "/" + dir_dirs$(d_chosen)  ' tunnel down a directory
          endif
          ReadDir(d_spec$,d_top_item,d_sel_item,d_top_last)
          d_changed = 1
        endif

      case  13  ' ENTER - something has been selected
        d_chosen = d_top_item + d_sel_item - 1
        if d_chosen <= val(dir_dirs$(0)) then ' item number in directory range?
          if ucase$(d_spec$) = "<DIR>" then   ' was directory selection chosen?
            if right$(cwd$,1) = "/" then
              result$(0) = cwd$ + dir_dirs$(d_chosen) + "/"  ' directory at root level
            else
              result$(0) = cwd$ + "/" + dir_dirs$(d_chosen) + "/"   ' directory deeper
            endif     ' Note: cwd$ returns all uppercase
            exit do
          endif
        else    ' Yahoo! A filename has been chosen
          if ucase$(d_spec$) <> "<DIR>" then   ' was other than directory selection chosen?
            d_chosen = d_chosen - val(dir_dirs$(0))
            if dir_fils$(d_chosen) <> "" then  ' in case directory has no (specified) file
              if right$(cwd$,1) = "/" then
                result$(0) = cwd$ + dir_fils$(d_chosen)  ' filename at root level
              else
                result$(0) = cwd$ + "/" + dir_fils$(d_chosen)  ' filename deeper
              endif     ' Note: cwd$ returns all uppercase
              exit do
            endif
          endif
        endif
    end select
    if d_changed then   ' something changed so redisplay directory list
      ListDir(d_top_item, d_lines, d_sel_item)
    endif
  loop
  '--------------------

  ' restore original screen image  (box not needed with  f/w v5.05.06+)
  box d_x, d_y, d_width, d_height, 1, 0, 0 ' must clear to black first
  blit write 64, d_x, d_y ', 0   ' now restore all non-black pixels
  blit close 64

  ' restore starting directory
  chdir d_startdir$

  do : loop until inkey$ = ""   ' clear the keyboard buffer
end sub

'*****************************************************************
' Read directories and specified files in the current directory
sub ReadDir(spec$,d_top_item,d_sel_item,d_top_last)
  local item_cnt, i

  for i = 1 to DIRCOUNT
    dir_dirs$(i) = ""   ' clear the array
  next i
  for i = 1 to FILCOUNT
    dir_fils$(i) = ""   ' clear the array
  next i

  ' read directories first
  dir_dirs$(0) = ""  ' 0 items to begin
  item_cnt = 1
  dir_dirs$(item_cnt) = left$(Dir$("*", DIR),NAMELENGTH) ' WARNING - possible truncation
  Do While dir_dirs$(item_cnt) <> "" and item_cnt < DIRCOUNT - 1
    If dir_dirs$(item_cnt) <> "." Then item_cnt = item_cnt + 1 ' ignore "."
    dir_dirs$(item_cnt) = Dir$()
  Loop
  if dir_dirs$(item_cnt) = "" then item_cnt = item_cnt - 1

  ' Sort directories
  Sort dir_dirs$()    ' note:  "" < "A"
  ' shift non-blank entries to front of array
  for i = 1 to item_cnt
    dir_dirs$(i) = dir_dirs$(DIRCOUNT-item_cnt+i)
  next i
  dir_dirs$(0) = str$(item_cnt)   ' store number of items

  ' now read files
  dir_fils$(0) = ""  ' 0 items to begin
  item_cnt = 1
  if ucase$(spec$) = "<DIR>" then
    dir_fils$(item_cnt) = left$(Dir$("*", FILE),NAMELENGTH) ' WARNING - possible truncation
  else
    dir_fils$(item_cnt) = left$(Dir$(spec$, FILE),NAMELENGTH) ' WARNING - possible truncation
  endif
  Do While dir_fils$(item_cnt) <> "" and item_cnt < FILCOUNT - 1
    If dir_fils$(item_cnt) <> "." Then item_cnt = item_cnt + 1 ' ignore "."
    dir_fils$(item_cnt) = Dir$()
  Loop
  if dir_fils$(item_cnt) = "" then item_cnt = item_cnt - 1

  ' Sort files and shift non-blank entries to front of array
  Sort dir_fils$()
  for i = 1 to item_cnt
    dir_fils$(i) = dir_fils$(FILCOUNT-item_cnt+i)
  next i
  dir_fils$(0) = str$(item_cnt)   ' store number of items

  d_top_item = 1
  d_sel_item = 1
  d_top_last = val(dir_dirs$(0)) + val(dir_fils$(0)) - d_lines + 1

end sub

'*****************************************************************
' Display (part of) directory
' Show 'nlines' number of items, starting with item 'first',
' hilite given item
sub ListDir(first, nlines, hilite)
  local i, item, d_txt$

  for i = 0 to nlines - 1
    item = first + i
    if item > val(dir_dirs$(0)) then
      d_txt$ = dir_fils$(item - val(dir_dirs$(0)))
    else
      d_txt$ = "<DIR> " + dir_dirs$(item)
    endif
    if len(d_txt$) > d_cwidth then d_txt$ = left$(d_txt$,d_cwidth-1) + chr$(148)
    d_txt$ = left$(d_txt$ + space$(d_cwidth),d_cwidth)

    if i = hilite - 1 then
      text d_x+17, d_y+24+i*MM.INFO(FONTHEIGHT), d_txt$,"LT",1,1,d_colours(1),d_colours(0)
    else
      text d_x+17, d_y+24+i*MM.INFO(FONTHEIGHT), d_txt$,"LT",1,1,d_colours(2),d_colours(1)
    endif
  next i

end sub

'*****************************************************************
' a small gliph showing arrow keys, tab/ent/space
DefineFont #11
  04301538
  00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
  00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
  00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
  00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
  00000000 00000000 00000000 00000000 00000000 00000000 FF3F0000 FFC1FF1F
  B0FF43F0 FCFBE7FF FEB7FF5F 5FFCF1E7 2FACB718 F746FEE0 FBEF96B1 B7375FFE
  FEFBEFB6 B6B7D75F 42FEFBEF 6EB7B038 FF3FEEFB FFCCFF1F 000000E6 020E0800
  00000000 00E6FF0C 0E000000 0000EEFB FB0F0000 000000FE FEFB0F00 00000000
  00FEFB0F 0F000000 0000FEE0 F1070000 000000FC FCFB0700 00000000 00F0FF01
  00000000 00000000 00000000 1FFF3F00 F0FFC1FF FFB0FF43 5FFCFBE7 E7FEB7FF
  185FFCF1 E02FACB7 B1F746FE FEFBEF96 B6B7375F 5FFEFBEF EFB6B7D7 3842FEFB
  FB6EB7B0 1FFF3FEE E6FFCCFF 00000000 3F020E08 CCFFFFFF FF7FE6FF FBEEFFFF
  8C237EEE FEFBEF67 DB6BED7D 7EFEFBEF EFC36B6D AD7FFEFB E0EFDF4B AC637CFE
  FCF1E763 FFFFEF7F 3FFCFBE7 C1FFFFEF 0000F0FF 00000000 00000000 00000000
  FF1FFF3F 43F0FFC1 E7FFB0FF FF5FFCFB F1E7FEB7 B7185FFC FEE02FAC 96B1F746
  5FFEFBEF EFB6B737 D75FFEFB FBEFB6B7 B03842FE EEFB6EB7 FF1FFF3F 00E6FFCC
  08000000 FF3F020E FFCCFFFF FFFF7FE6 EEFBEEFF 678C237E 7DFEFBEF EFDB6BED
  6D7EFEFB FBEFC36B 4BAD7FFE FEE0EFDF 63AC637C 7FFCF1E7 E7FFFFEF EF3FFCFB
  FFC1FFFF 000000F0 00000000
End DefineFont
'*****************************************************************

'//////////////////////////////////////////////////////////////////////////////
' XMODEM receive for Color MaxiMite 2
' (C) 2021 David R. Van Wagner, John A. Crutti Jr
' MIT LICENSE
'//////////////////////////////////////////////////////////////////////////////

sub _xmodem_dim
  dim nak$:nak$=chr$(21) '^U
  dim ack$:ack$=chr$(6)  '^F
  dim soh$:soh$=chr$(1)  '^A
  dim can$:can$=chr$(24) '^X
  dim eot$:eot$=chr$(4)  '^D
  dim eof$:eof$=chr$(26) '^Z
  dim cr$:cr$=chr$(13)   '^M
  dim lf$:lf$=chr$(10)   '^J
  dim bs$:bs$=chr$(8)    '^H
  dim bel$:bel$=chr$(7)  '^G
  dim stx$:stx$=chr$(2)  '^B and STX is used to indicate a 1K packet
  'dim crc$:crc$='C'

  dim xmodem_sum%
  dim xmodem_block%:xmodem_block%=1
  dim xmodem_state%:xmodem_state%=0
  dim xmodem_buffer$:xmodem_buffer$=""
  dim xmodem_errors%:xmodem_errors%=0
  dim xmodem_last_recv
  dim xmodem_up$
  dim xmodem_down$
'  dim xmodem_option_crnul%:xmodem_option_crnul%=1
  dim xmodem_option_crnul%:xmodem_option_crnul%=0
  dim xmodem_lastrx$:xmodem_lastrx$=""
end sub '_xmodem_dim_const

'XMODEM is simple algorithm
'blocks are [SOH x01][BLK#][~BLK#][128-BYTES][SUM]
'sender expects [NAK x15] to start
'sender expects [ACK x06] on successful block receive, otherwise NAK will cause retransmit
'[CAN x18] or [CAN][CAN] is an extension to cancel transmission from receiver
'[EOT] signals no more to send
'all data is 8-bit binary, including simple sum.  ~BLK# is inverse BLK#, xor 255 (equiv. 255-BLK#)
'block number starts at 1, increments by 1, wraps from 255 to 0
'last block is padded to end with [EOF x1A] to make up 128 bytes
'10 second timeout to get SOH, max 10 errors, about 90 seconds timeout at start
  'block receive timeout is 7 seconds

sub _xmodem_terminal
  option crlf lf

  if len(xmodem_down$)>0 then
    print "^X to cancel transmission"
    print #5,nak$; ' start download immediately
  end if

  xmodem_last_recv = timer
  local key$
  do ' terminal - send pressed keys, send [nak] on timeout
    key$ = inkey$
    if len(key$) > 0 then
      print #5;key$;
      _xmodem_timer_handler key$
    end if
    _xmodem_timer_handler ""
  loop while xmodem_up$<>"" OR xmodem_down$<>""

  option crlf crlf
end sub '_xmodem_terminal

sub _xmodem_send xmodem_filename$
  xmodem_up$ = xmodem_filename$
  _xmodem_terminal
end sub

sub _xmodem_recv xmodem_filename$
  xmodem_down$ = xmodem_filename$
  _xmodem_terminal
end sub

' handle timeouts for xmodem
' key$ is whether a key was pressed
sub _xmodem_timer_handler key$
  if len(key$)>0 then xmodem_last_recv = timer
  if (xmodem_state% = 0 and (timer - xmodem_last_recv) > 10000) or (xmodem_state% > 0 and (timer - xmodem_last_recv) > 7000) then
    print #5,nak$;
    xmodem_last_recv=timer
    if xmodem_block% > 1 then
      if xmodem_state%=0 or xmodem_state%=4 then ' didn't receive block
        'print "TIMEOUT"
        xmodem_errors%=xmodem_errors%+1
      else
        if xmodem_state%=5 then
          xmodem_state%=0 ' cancel send
          close #2
          xmodem_block%=0
        else
          xmodem_state%=4 ' failed block
        end if
      end if
      if xmodem_errors% >= 10 and xmodem_block%<>0 then
        'print "FAILED"
        close #1
        xmodem_block%=1
        xmodem_down$=""

      end if
    end if
  end if
end sub

' serial read handler
sub _serial_read
  local serial$ = input$(1, #5)
  _xmodem_handler serial$
end sub '_serial_read

' serial$ is one character read from serial port
' global xmodem_down$ is download filename, or empty string to not download
' global xmodem_up$ is upload filename, or empty string to not upload
' should only call if uploading or downloading
sub _xmodem_handler serial$
  if len(serial$)=0 then exit 'sub
  xmodem_last_recv = timer
  if xmodem_option_crnul%=1 and xmodem_lastrx$=chr$(13) and serial$=chr$(0) then
    xmodem_lastrx$=serial$
    return 'eat extra nul after cr from Zimodem
  else
    xmodem_lastrx$=serial$
  end if

  'XMODEM state machine
  'state 0 - ready for block
  'state 1 - soh received, waiting for blk#
  'state 2 - blk# received, waiting for inverse blk#
  'state 3 - receiving block, waiting for complete block including checksum
  'state 4 - receiving error block, waiting for timeout
  'state 5 - sending, waiting for ACK

  select case xmodem_state%
    case 0: ' ready for block
      if serial$=soh$ and len(xmodem_down$)<>0 then
         if xmodem_block%=1 then
            open xmodem_down$ for output as #1
         end if
         xmodem_state%=1
         if debug% then print "<SOH>";
      else if xmodem_block%=1 and serial$=nak$ and len(xmodem_up$)<>0 then
         open xmodem_up$ for input as #2
         xmodem_buffer$=input$(128,#2)
         _xmodem_send_buffer
         xmodem_state%=5
      else
         if xmodem_block%>1 and serial$=eot$ then
            if debug% then print "<EOT> Success!"
            _xmodem_status_clear
            print "[ XMODEM Receiving Success ]"
            close #1
            print #5,ack$; ' Signal Received End of Transmission
            xmodem_block%=1
            xmodem_down$=""
            ' note does not change state yet, waits for ACK
            ' Jay: need to test scenario send EOT recv NAK, send EOT again recv ACK to really be done.
         else
            if serial$=cr$ or serial$=lf$ or serial$=bs$ or serial$=bel$ then
              if serial$ <> bel$ then
                if debug% then
                  print "<x";hex$(asc(serial$),2);">";
                  if serial$=cr$ then serial$=cr$+lf$
                end if
                print serial$;
              end if
            else
              if asc(serial$)<32 or asc(serial$)>126 then
                print "<x";hex$(asc(serial$),2);">";
              else
                print serial$;
              end if
            end if
         end if
      end if
    case 1: 'soh received, waiting for blk#
      if serial$<>chr$(xmodem_block% and 255) then
         print "<??";asc(serial$);"??>";
         xmodem_state%=4
      else
         if debug% then print "<BLK";xmodem_block% and 255;">";
         xmodem_state%=2
      end if
    case 2: ' blk# received, waiting for inverse blk#
      if serial$<>chr$((xmodem_block% and 255) xor 255) then
         print "<??";asc(serial$);"??>";
         xmodem_state%=4
      else
         if debug% then print "<~BLK";asc(serial$);">";
         xmodem_state%=3:xmodem_buffer$="":xmodem_sum%=0
      end if
    case 3: ' receiving block, waiting for complete block including checksum
      if len(xmodem_buffer$) < 128 then
         xmodem_buffer$=xmodem_buffer$+serial$
         xmodem_sum%=(xmodem_sum%+asc(serial$)) and 255
         'print "<";len(xmodem_buffer$);":";asc(serial$)">";
         if len(xmodem_buffer$) = 128 then
            if debug% then print "<128 BYTES>";
         end if
      else
         if xmodem_sum% = asc(serial$) then
            if debug% then print "<SUM";xmodem_sum%;">"
            xmodem_block% = xmodem_block% + 1
            print #1,xmodem_buffer$;
            print #5,ack$; ' signal received block
            xmodem_state%=0
         else
            print "<SUM";asc(serial$);"!=";xmodem_sum%;">"
            print #5,nak$; ' Signal Problem in Communication
            xmodem_state%=1
         end if
      end if
    case 4: ' receiving error block, waiting for timeout
      if debug% then print "TIMEOUT"
      xmodem_errors%=xmodem_errors%+1
      xmodem_buffer$=""
      if xmodem_errors% = 10 then
         print "FAILED"
         xmodem_state%=0
         close #1
         xmodem_block%=1
         xmodem_down$=""
      end if
    case 5: 'sending, waiting for ACK
      if serial$=ack$ then
         if debug% then print "<ACK>"
         if len(xmodem_buffer$) > 0 then
           xmodem_buffer$ = input$(128, #2) ' get next block
           if len(xmodem_buffer$) = 0 then
             if debug% then print "Wrapping up";
             print #5,eot$;
           else
             xmodem_block%=xmodem_block%+1
             _xmodem_send_buffer
           end if
         else
           if debug% then print "Success!!!"
           _xmodem_status_clear
           print "[ XMODEM Sending Success ]"
           xmodem_state%=0
           xmodem_block%=1
           close #2
           xmodem_up$=""
         endif
      else
         if serial$=nak$ then
           if debug% then print "<NAK>"
           _xmodem_send_buffer
         else if serial$=can$ then
           if debug% then print "<CAN>"
           xmodem_state%=0
           xmodem_block%=1
           close #2
           xmodem_down$=""
         end if
      end if
  end select
  if xmodem_state%<>0 or xmodem_block%<>1 then _xmodem_status_progress
end sub

sub _xmodem_send_buffer
  local i
  if debug% then print "<BLOCK";xmodem_block% and 255;">";
  if len(xmodem_buffer$)<128 then
    do
      xmodem_buffer$=xmodem_buffer$+eof$
    loop until len(xmodem_buffer$)=128
  end if
  print #5, soh$;
  print #5, chr$(xmodem_block% and 255);
  print #5, chr$((xmodem_block% and 255) xor 255);
  print #5, xmodem_buffer$;
  xmodem_sum% = 0
  for i=1 to 128:xmodem_sum% = xmodem_sum% + asc(mid$(xmodem_buffer$, i, 1)):next
  print #5, chr$(xmodem_sum% and 255);
end sub '_xmodem_send_buffer

sub _xmodem_status_progress
  ' build status line
  local status$
  status$=" XMODEM"
  if xmodem_state% = 5 then
    status$=status$+" Sending"
  else
    status$=status$+" Receiving"
  end if
  status$=status$+" #"+STR$(xmodem_block%)+" "+STR$(xmodem_errors%)+" errs "

  _xmodem_status_text status$
end sub '_xmodem_status_progress

sub _xmodem_status_text status$
  local save_x, save_y, cols, add, i

  ' save cursor position
  save_x = mm.info(hpos)
  save_y = mm.info(vpos)

  ' append spaces to fill to end of line
  cols = mm.hres/mm.info(fontwidth)
  add = cols-1-len(status$)
  for i = 1 to add : status$=status$+" " : next i

  ' display status line
  print @(0,mm.vres-mm.info(fontheight),2) status$;

  ' restore cursor position
  print @(save_x,save_y,1) " ";
  print bs$;
end sub '_xmodem_status_text

sub _xmodem_status_clear
  box 0, mm.vres-mm.info(fontheight), mm.hres, mm.info(fontheight), 0, , rgb(black)
end sub '_xmodem_status_clear

