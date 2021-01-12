'Simple terminal program v 0.1 by Rich Martin (datawiz) 11:13pm 02 Oct 2020
'Also code from Flashback.bas 1.0.0 by Rich Martin (datawiz)
'Also code from vegipete for the GetFile routine
'Version 1.9 John Crutti Jr 1-09-2021

OPTION EXPLICIT
ON ERROR IGNORE
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

dim comportnum% = 1 ' COM port number as an integer for COM Port subroutine.
dim comportstr$ = "COM1" ' COM port as a string for COM Port subroutine.
dim comchoice$ = "1" ' used in COM selection subroutine.
dim comspeedchoice$ = "9" ' used in COM Speed selection subroutine.
dim comspeed$ = "115200" ' COM Speed as a string for COM Speed subroutine.
dim rs232% = 0 'start with TTL type serial port
dim comporttype$ = "TTL Serial"
dim CHAR_OUT$ 'characters we're typing at the console to be sent to modem
dim altflag% 'is ALT key pressed?
dim winflag% 'is WINDOWS key pressed?
dim CHARS_IN$ 'characters being received from the modem
dim echo% = 0 'is local echo enabled?
dim linefeeds% = 0 'setting of line feeds to be sent to modem after every CR
dim linefeedstate$ = "LF Off"
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
dim C$ 'used as part of the ALT case statement section
dim kp%, kl%, kf% 'not sure what these do
dim modemresetstring$ = "ATZ" 'command to reset the modem
dim modeminitstring$ = "ATE1V1X0Q0s40=512s0=1s41=1f0" 'command used to configure the modem
dim modeminfostring$ = "ATI" 'command to ask modem to show information
setpin 32,DOUT 'setup RTS/CTS pins
setpin 33,DIN 'setup RTS/CTS pins
PIN(32) = 0 'setup RTS/CTS pins
dim TERM_COLOR1 = 255 'init value for white
dim TERM_COLOR2 = 255 'init value for white
dim TERM_COLOR3 = 255 'init value for white
dim debug% 'for future debug mode
dim phonebookentry$(10) as string ' array holding the user's phone book
dim phonebookusername$(10) as string
dim phonebookpassword$(10) as string
dim phoneentry% = 0 'int of array value for the phone book
dim dialchoice$ 'string of selections in autodial phone book screen



'main function
cls
introscreen 'show the title screen when launched
pause 2500
cls
loadphonebook 'load phone book, if exists
loadconfig
welcomebanner 'banner at top of terminal screen showing help and exit commands
setcomport 'set the COM port you want to use 1 or 2
setcomspeed 'choose the speed of the COM port
pickcolor 'pick the color you want out of White, Amber, and Green
setupcolor 'configure the color
startcomport 'open the COM port for communications
terminalonline 'just tells you that you're online and ready to communicate
modemreset 'send modem reset "ATZ" string
pause 500 ' wait for modem to process
modeminit 'send modem setup string
pause 500 'wait for modem to process
modeminfo 'ask modem to print its info for the user
do 'user input routine
CHAR_OUT$ = INKEY$
if CHAR_OUT$ <> "" then
  if CHAR_OUT$ = chr$(137) then download
    end if
  if CHAR_OUT$ = chr$(136) then upload
    end if
 C$ = getchar$() 'not sure what this does
  if altflag% = 1 then 'check for ALT being asserted
  select case lcase$(C$) 'turn all characters to lowercase
      case "a" 'show the autodial phone book screen
        phonebook
      case "b" 'set the COM port parameters again
        onlineflag% = 0 'take us "offline" so we don't see incoming data during this.
        cls
        close #5 'close the COM port if it's already open.
        setcomport
        setcomspeed
        terminalonline
        startcomport
      case "c" 'clear the existing screen contents
        text 400,300, "  CLEARING SCREEN  ", "CM",1,1, RGB(BLACK), RGB(WHITE)
        pause 750  
        welcomebanner      
      case "d" 'leftovver from flashback terminal. Not sure what to do here yet.
        if winflag% = 1 then
          if debug% = 0 then 
            colour rgb(black), rgb(red)
            print "*** Debug Mode On *** (NOT IMPLEMENTED)"
          setupcolor
          debug% = 1
          else
            colour rgb(black), rgb(red)
            print "*** Debug Mode Off *** (NOT IMPLEMENTED)"
          setupcolor
          debug% = 0
          end if
        else        
          listfiles       
        end if
      case "f" 'change the font color again
        pickcolor
        setupcolor
      case "i" 're-initialize the modem
        colour rgb(black), rgb(red)
        print "*** SENDING MODEM INITIALIZATION ***"
        setupcolor
        modeminit
      case "l" ' change the line feed TX setting
        cls : changelinefeeds : welcomebanner
      case "x" 'hangup the modem/close the connection
        echo% = 0
        hangup
      case "q" 'exit the terminal
        termexit
      case "s" 'enable annoying beep sound for every key press
        if soundflag% = 0 then
          welcomebanner
          text 400,300, "*** Sound On ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor
          print ""
          pause 1000
          welcomebanner
          soundflag% = 1
        else
          welcomebanner
          text 400,300, "*** Sound Off ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor
          print ""
          pause 1000
          welcomebanner
          soundflag% = 0
        end if
      case "h" 'user help screen
          cls
          dialogHelp
      case "e" 'turn on local echo in case modem isn't set to echo    
        if echo% = 1 then
          welcomebanner
          text 400,300, "*** Local Echo Off ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor
          print ""
          pause 1000
          echo% = 0 
          welcomebanner
        else
        text 400,300, "*** Local Echo On ***", "CM",1,1, RGB(BLACK), RGB(WHITE)
          setupcolor
          print ""
          pause 1000
          echo% = 1
          welcomebanner
        end if  
      case "p" 'show the current com port settings
        comsettings  
      case "v" 'show the credits screen
        if winflag% = 1 then 'credits screen takes ALT and WIN keys to show
          credits
        end if
      case "t" 'change the com port type, TTL or RS-232 levels 
        setcomtype
        startcomport
      case "r" 'run the initial modem setup routine again in case it gets wonky
        colour rgb(black), rgb(red)
        print "*** RESETTING MODEM ***"
        setupcolor
        modemreset
        pause 250
        modeminit
        pause 250
        modeminfo  
    end select
  else  
blinkcursor
print #5, CHAR_OUT$;
      if linefeeds% = 1 and CHAR_OUT$ = chr$(13) then    
        print #5, ""
      end if
end if
  if echo% = 1 and CHAR_OUT$ <> "" then    
      print CHAR_OUT$;
      if CHAR_OUT$ = chr$(13) then 'workaround for mmbasic bug??
        print ""
      end if
  end if
loop
end

sub loadconfig
open "settings.cfg" for input as #7
line input #7, comporttype$
line input #7, linefeedstate$
line input #7, modeminitstring$
close #7
  if comporttype$ = "RS-232 Serial" then
    rs232% = 1
  else
    rs232% = 0
  end if
  if linefeedstate$ = "LF On" then
    linefeeds% = 1
  else
    linefeeds% = 0
  end if
end sub

sub saveconfig
open "settings.cfg" for output as #7
print #7, comporttype$
print #7, linefeedstate$
print #7, modeminitstring$
close #7
end sub



sub loadphonebook
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
end sub

sub savephonebook
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


function getchar$() as string 'magic from Rich Martin
  altflag% = 0
  winflag% = 0
  getchar$ = ""
  if keydown(7) > 0 then
    kf% = keydown(7)
    if (kf% AND &b00000001) = &b00000001 then altflag% = 1 
    if (kf% AND &b00010000) = &b00010000 then altflag% = 1
    if (kf% AND &b00000100) = &b00000100 then winflag% = 1
    if (kf% AND &b00100000) = &b01000000 then winflag% = 1
  end if
  kp% = keydown(0)
  if kp% > 0 then
    kp% = keydown(1)
    if kl% <> kp% then
      getchar$ = chr$(kp%)    
      timer = 0
    else
      'if timer > kreprate then
       ' getchar$ = chr$(kp%)
        timer = 0
      end if
    end if
    kl% = kp%
    pause 10
  end if
end function


sub introscreen
  const ox = 35
  const oy = 15
  cls
  box ox*fwidth%, oy*fheight%, 28*fwidth%, 14*fheight%, 1,,rgb(black)
  print @((ox+2)*fwidth%,(oy+1)*fheight%) "        MAXITERM";
  print @((ox+2)*fwidth%,(oy+2)*fheight%) "        --------";
  print @((ox+2)*fwidth%,(oy+3)*fheight%) "";
  print @((ox+2)*fwidth%,(oy+4)*fheight%) "        for  the";
  print @((ox+2)*fwidth%,(oy+5)*fheight%) "    Color Maximite 2";
  print @((ox+2)*fwidth%,(oy+6)*fheight%) "";
  print @((ox+2)*fwidth%,(oy+7)*fheight%) "       Version 1.9";
  print @((ox+2)*fwidth%,(oy+8)*fheight%) "           by";
  print @((ox+2)*fwidth%,(oy+9)*fheight%) "       Jay Crutti";
  print @((ox+2)*fwidth%,(oy+10)*fheight%)"          2021";
  print @((ox+2)*fwidth%,(oy+11)*fheight%)"";
  print @((ox+2)*fwidth%,(oy+12)*fheight%)"    www.jaycrutti.com";
end sub

sub welcomebanner
cls
print "Terminal running. ALT-Q to Exit. ALT-H for Help."
end sub



sub pickcolor
local textchoice$
print ""
input "Which color do you want 1.White [DEFAULT], 2.Amber, or 3.Green "; textchoice$
select case textchoice$
  case "" ' hitting enter
    text_color = 1 : setupcolor
    print "White Selected."
  case "1"
    text_color = 1 : setupcolor
    print "White Selected."
  case "2" 
    text_color = 2 : setupcolor
    print "Amber Selected."
  case "3"
    text_color = 3 : setupcolor
    print "Green Selected."
  case else
    print "Invalid Selection, please try again."
    pickcolor
end select
end sub




sub changelinefeeds
local lfchoice$
print ""
print "Send Line Feeds after Carriage Return?"
print "1.No [DEFAULT]"
print "2.Yes"
input "Make Selection;"; lfchoice$
select case lfchoice$
  case "" ' hitting enter
    linefeeds% = 0 
    linefeedstate$ = "LF Off"
    print "Line Feeds will not be sent."
  case "1"
    linefeeds% = 0
    linefeedstate$ = "LF Off"
    print "Line Feeds will not be sent."
  case "2"
    linefeeds% = 1
    linefeedstate$ = "LF On"
    print "Line Feeds will be sent."
  case else
    print "Invalid Selection, please try again."
    pause 1200
    changelinefeeds
end select
pause 1200
end sub




sub setcomport
print ""
input "Choose COM Port, COM 1 [DEFAULT], 2, or 3 "; comchoice$
select case comchoice$
  case "" 'hitting enter
      comportstr$ = "COM1"
      comportnum% = 1
      print "COM1 Selected."
  case "1"
    comportstr$ = "COM1" : comportnum% = 1
    print "COM1 Selected."
  case "2" 
    comportstr$ = "COM2" : comportnum% = 2
    print "COM2 Selected."
  case "3"
      comportstr$ = "COM3" : comportnum% = 3
      print "COM3 (via USB Type B port) Selected."
        if mm.errno <> 0 then 
        Print "Error: ";mm.errmsg$,
      end if
  case else 'invalid input
      print "Invalid COM port, please try again."
      pause 1200 ' wait for them to read the response
      setcomport ' start over
end select
startcomport
end sub




sub setcomtype
local comtype$
cls
print ""
print "Select COM Port Type"
print "1) TTL Serial [DEFAULT]"
print "2) RS-232 Serial"
input "Make Selection: ", comtype$
select case comtype$
  case "" 'hitting enter
    rs232% = 0 ' 0 means TTL
    comporttype$ = "TTL Serial" 'this string is important for the settings.cfg file!
    print "TTL Serial Selected."
  case "1" 
    rs232% = 0
    comporttype$ = "TTL Serial"
    print "TTL Serial Selected." 
  case "2"
    rs232% = 1 ' 1 is INVerted RS232 levels
    comporttype$ = "RS-232 Serial"
    print "RS-232 Serial Selected." 
  case else
    print "Invalid selection, please try again."
    pause 1200
    setcomtype
end select
startcomport
end sub




sub setcomspeed
onlineflag% = 0 'disable so incoming data doesn't disturb our decision
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
    case ""
      print "115200 Selected." : comspeed$ = "115200"
    case "1"
      print "1200 Selected." : comspeed$ = "1200"
    case "2"
      print "2400 Selected." : comspeed$ = "2400"
    case "3"
      print "4800 Selected." : comspeed$ = "4800"
    case "4"
      print "9600 Selected." : comspeed$ = "9600"
    case "5"
      print "19200 Selected." : comspeed$ = "19200"
    case "6"
      print "38400 Selected." : comspeed$ = "38400"
    case "7"
      print "57600 Selected." : comspeed$ = "57600"
    case "8"
      print "115200 Selected." : comspeed$ = "115200"
    case else
      print "Invalid selection. Please try again."
      pause 1200  
      setcomspeed
  end select
onlineflag% = 1 'enable so we're back online
end sub



sub setupcolor
select case TEXT_COLOR
  case 1 
TERM_COLOR1 = 255
TERM_COLOR2 = 255
TERM_COLOR3 = 255
  case 2
TERM_COLOR1 = 255
TERM_COLOR2 = 176
TERM_COLOR3 = 0
  case 3
TERM_COLOR1 = 51
TERM_COLOR2 = 255
TERM_COLOR3 = 0
end select
colour rgb(TERM_COLOR1,TERM_COLOR2,TERM_COLOR3), rgb(black)
end sub



sub startcomport
  close #5
    if rs232% = 1 then
      open comportstr$+":"+comspeed$+","+"256"+",get_serial_input"+",INV" as #5
    else
      open comportstr$+":"+comspeed$+","+"256"+",get_serial_input" as #5
    end if
end sub



sub modemreset
print #5; modemresetstring$
end sub


sub modeminit
print #5; modeminitstring$
end sub


sub modeminfo
print #5; modeminfostring$
end sub


sub get_serial_input
CHARS_IN$ = input$(LOC(#5),#5)
if onlineflag% = 1 then
  print CHARS_IN$;
    if soundflag% = 1 AND CHARS_IN$ = chr$(13) then PLAY mp3 "sound.mp3"
    end if  
end if
end sub



sub terminalonline
    colour rgb(black), rgb(red)
    print chr$(13); chr$(10); "*** TERMINAL ONLINE ***"
    setupcolor
end sub



sub download
cls
  print "Xmodem Download (REQUIRES 5.05.06 or higher firmware)"
  input "Enter Filename: "; receivefile$
   if receivefile$ = "" then
    colour rgb(black), rgb(red)
    print chr$(13); chr$(10); "*** Download Cancelled ***"
    setupcolor
    pause 1500
    welcomebanner
    exit sub
else
  onlineflag% = 0
  print "Please wait, downloading "; receivefile$
      XMODEM R receivefile$, comportnum% 
  if mm.errno <> 0 then 
    Print "Download Error: ";mm.errmsg$,
    onlineflag% = 1
  end if
  if mm.errno = 0 then 
    onlineflag% = 0
    print chr$(13); chr$(10);"Download Complete.",
    print ""
    onlineflag% = 1
  end if
end sub



sub upload
cls
print "Xmodem Upload (REQUIRES 5.05.06 or higher firmware)"
FileDialog(NameOfFile$())   ' no options so allow any file to be selected
  if NameOfFile$(0) = "" then
    welcomebanner
    colour rgb(black), rgb(red)
    print chr$(13); chr$(10); "*** Upload Cancelled ***"
    setupcolor
    pause 1500
    exit sub
  else
    cls    
    print "Please wait, uploading "; NameOfFile$(0)
      XMODEM S NameOfFile$(0), comportnum%
  end if      
  if mm.errno <> 0 then Print "Upload Error: ";mm.errmsg$
      end if
    print "Exiting Upload."
  end if
end sub


sub listfiles
cls
FileDialog(NameOfFile$())   ' no options so allow any file to be selected
welcomebanner
end sub


sub hangup
  colour rgb(black), rgb(red)
  print chr$(13); chr$(10); "*** DISCONNECTING ***"
  colour rgb(white), rgb(black)  
    onlineflag% = 0 'disable incoming data display while hanging up
      print #5; chr$(13); chr$(10)
      pause 1200 'take our time. too fast and modem will ignore
      print #5; chr$(43);chr$(43);chr$(43); 
      pause 1500 'take our time. too fast and modem will ignore
      print ""
      print #5;"ATH0"
  colour rgb(black), rgb(red)
    print "*** DISCONNECTED ***"
      setupcolor        
onlineflag% = 1
end sub

sub termexit
  colour rgb(red), rgb(black)
  close #5
  colour rgb(black), rgb(red)
  print chr$(13); chr$(10); "*** EXITING TERMINAL ***"
  setupcolor
pause 750
exit
end sub


sub changeinitstring
local newinitstring$
        print @(0,420) ""        
            print "Current modem initialization string: ";modeminitstring$
            input "Enter new modem initialization string: ", newinitstring$
              if newinitstring$ <> "" then 
                print "Changing modem initializatin string to "; newinitstring$ 
                modeminitstring$ = newinitstring$
                pause 1500
              else          
                print "Not updated." 
                pause 1500
              end if
end sub



sub comsettings
local comwindow$
  const ox = 20
  const oy = 15
  cls
  box ox*fwidth%, oy*fheight%, 60*fwidth%, 16*fheight%, 1,,rgb(black)
  print @((ox+2)*fwidth%,(oy+1)*fheight%) "CURRENT COM PORT SETTINGS";
  print @((ox+2)*fwidth%,(oy+2)*fheight%) "-------------------------";
  print @((ox+2)*fwidth%,(oy+3)*fheight%) "A.COM PORT                :",comportstr$
  print @((ox+2)*fwidth%,(oy+4)*fheight%) "B.BAUD RATE               :",comspeed$
  print @((ox+2)*fwidth%,(oy+5)*fheight%) "C.COM PORT TYPE           :",comporttype$ 
  print @((ox+2)*fwidth%,(oy+6)*fheight%) "D.DATA BITS               : 8";
  print @((ox+2)*fwidth%,(oy+7)*fheight%) "E.PARITY                  : NONE";
  print @((ox+2)*fwidth%,(oy+8)*fheight%) "F.FLOW CONTROL            : (NOT IMPLEMENTED)";
  print @((ox+2)*fwidth%,(oy+9)*fheight%) "G.STOP BITS               : 1";
  print @((ox+2)*fwidth%,(oy+10)*fheight%)"H.SEND LINE FEED AFTER CR :",linefeedstate$
  print @((ox+2)*fwidth%,(oy+11)*fheight%)"I.INIT STRING             :",modeminitstring$
  print @((ox+2)*fwidth%,(oy+12)*fheight%)"";
  print @((ox+2)*fwidth%,(oy+13)*fheight%)"To change settings, enter letter or hit enter to exit.";
  print @((ox+2)*fwidth%,(oy+14)*fheight%)"Enter S) to save. Make Selection"; : input comwindow$,
select case comwindow$
  case "" ' they hit enter
    print @(0,420) "Returning to terminal."
    pause 1200 : welcomebanner
  case "a", "A"
    print chr$(10),chr$(13)
    setcomport : pause 1200 : comsettings
  case "b", "B"
    setcomspeed : pause 1200 : comsettings
  case "c", "C"
    setcomtype : pause 1200 : comsettings
  case "d", "D"
    print @(0,420) "Option not implemented yet."
    pause 1500 : comsettings
  case "e", "E"
    print @(0,420) "Option not implemented yet."
    pause 1500 : comsettings
  case "f", "F"
    print @(0,420) "Option not implemented yet."
    pause 1500 : comsettings
  case "g", "G"
    print @(0,420) "Option not implemented yet."
    pause 1500 : comsettings
  case "h", "H"
    print chr$(10),chr$(13) : changelinefeeds : pause 1200 : comsettings
  case "i", "I"
    changeinitstring : pause 1200 : comsettings
  case "s", "S"
    print @(0,420) "Saving Configuration to settings.cfg"
    saveconfig : pause 1500 : comsettings
  case else
    print @(0,420) "Invalid option. Try again." : pause 1500 : comsettings
end select
end sub


sub dialoghelp
  const ox = 30
  const oy = 15
  cls
  box ox*fwidth%, oy*fheight%, 38*fwidth%, 23*fheight%, 1,,rgb(black)
  print @((ox+2)*fwidth%,(oy+1)*fheight%) "ALT-A Autodial Phone Book";
  print @((ox+2)*fwidth%,(oy+2)*fheight%) "ALT-B Change COM Port Settings";
  print @((ox+2)*fwidth%,(oy+3)*fheight%) "ALT-C Clear Screen";
  print @((ox+2)*fwidth%,(oy+4)*fheight%) "ALT-D List Local Directory";
  print @((ox+2)*fwidth%,(oy+5)*fheight%) "ALT-E Local Echo on/off";
  print @((ox+2)*fwidth%,(oy+6)*fheight%) "ALT-F Change the Font Color";
  print @((ox+2)*fwidth%,(oy+7)*fheight%) "ALT-H Help Menu";
  print @((ox+2)*fwidth%,(oy+8)*fheight%) "ALT-I Send Modem Initialization";
  print @((ox+2)*fwidth%,(oy+9)*fheight%) "ALT-L Line Feed TX Setting";
  print @((ox+2)*fwidth%,(oy+10)*fheight%)"ALT-P Show COM Port Settings";
  print @((ox+2)*fwidth%,(oy+11)*fheight%)"ALT-Q Quit and Exit Terminal";
  print @((ox+2)*fwidth%,(oy+12)*fheight%)"ALT-R Reset the Modem";
  print @((ox+2)*fwidth%,(oy+13)*fheight%)"ALT-S Key Sound on/off";
  print @((ox+2)*fwidth%,(oy+14)*fheight%)"ALT-T Change Com Port Type";
  print @((ox+2)*fwidth%,(oy+15)*fheight%)"ALT-X Disconnect Session";
  print @((ox+2)*fwidth%,(oy+16)*fheight%)"";
  print @((ox+2)*fwidth%,(oy+17)*fheight%)"Page UP = Upload File";
  print @((ox+2)*fwidth%,(oy+18)*fheight%)"Page DOWN = Download File";
  print @((ox+2)*fwidth%,(oy+19)*fheight%)"";
  print @((ox+2)*fwidth%,(oy+20)*fheight%)"ALT+WIN-D Toggle debug on/off";
  print @((ox+2)*fwidth%,(oy+21)*fheight%)"ALT+WIN-V Show Version info";
  do while inkey$ = "" : loop
welcomebanner
end sub

sub credits
  const ox = 30
  const oy = 15
  cls
  box ox*fwidth%, oy*fheight%, 40*fwidth%, 15*fheight%, 1,,rgb(black)
  print @((ox+2)*fwidth%,(oy+1)*fheight%) "Maxiterm for the Color Maximite 2";
  print @((ox+2)*fwidth%,(oy+2)*fheight%) "---------------------------------";
  print @((ox+2)*fwidth%,(oy+3)*fheight%) "Version 1.9";
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
end sub


sub phonebook
local newphoneentry$
local newphoneusername$
local newphonepassword$

  const ox = 3
  const oy = 3
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
     case "c" 'clear an entry
        print @(0,420) ""        
        input "Enter entry to clear: ", phoneentry%
          if phoneentry% = 1 to 10 then
            print "Clearing entry"; phoneentry%
            phonebookentry$(phoneentry%) = ""
            phonebookusername$(phoneentry%) = ""
            phonebookpassword$(phoneentry%) = ""
            pause 1500
            phonebook
          end if  
          if phoneentry% = 0 then ' they hit enter 
                print "Clearing aborted." 
                pause 1500
                phonebook
          end if
     case "e" 'edit an entry
        print @(0,420) ""        
        input "Enter entry to edit: ", phoneentry%
          if phoneentry% = 1 to 10 then
            print "Current hostname / phone number: ";phonebookentry$(phoneentry%)
            input "Enter new hostname / phone number: ", newphoneentry$
              if newphoneentry$ <> "" then 
                print "Changing Entry";phoneentry%; " to "; newphoneentry$ 
                phonebookentry$(phoneentry%) = newphoneentry$
                pause 1500
                phonebook
              else          
                print "Not updated." 
                pause 1500
                phonebook
              end if
          end if
      case "d"
        print @(0,420) chr$(10),chr$(13)        
        input "Entry # to dial: ", phoneentry%
        print "Dialing entry";phoneentry%;", " phonebookentry$(phoneentry%)
        print #5; "atdt"; phonebookentry$(phoneentry%)"", chr$(13)
      case "l" 'edit the login for an entry
        print @(0,420) ""        
        input "Enter entry to edit: ", phoneentry%
          if phoneentry% = 1 to 10 then
            print "Current Username: ";phonebookusername$(phoneentry%)
            print "Current Password: ";phonebookpassword$(phoneentry%)
            input "Enter new Username: ", newphoneusername$
              if newphoneusername$ <> "" then 
                print "Changing Username";phonebookusername$(phoneentry%); " to "; newphoneusername$ 
                phonebookusername$(phoneentry%) = newphoneusername$
                pause 1500
              else          
                print "Not updated." 
                pause 1500
                phonebook
              end if
            print chr$(10), chr$(13)
            input "Enter new Password: ", newphonepassword$
              if newphonpassword$ <> "" then 
                print "Changing Password";phonebookpassword$(phoneentry%); " to "; newphonepassword$ 
                phonebookpassword$(phoneentry%) = newphonepassword$
                pause 1500
                phonebook
              else          
                print "Not updated." 
                pause 1500
                phonebook
              end if
          end if
      case "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"
        phoneentry% = val(dialchoice$)
        print @(0,420) "Dialing entry ";dialchoice$;", " phonebookentry$(phoneentry%)
        print #5; "atdt"; phonebookentry$(phoneentry%)"", chr$(13)
      case "s" 'save updated phone book to config file
        print @(0,420) "Phonebook Saved."
        savephonebook
        pause 1500
        phonebook
      case else 'invalid junk
        print @(0,420)"Invalid selection."
        pause 1500
        phonebook  
      end select  
end sub


sub blinkcursor 'future feature
  local x%, y%
  x% = xpos%
  y% = ypos%
'  select case cursortype%
'  case 0
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) " ";
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) "_";
'  case 1
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) "/";
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) "-";
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) "\";
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) "|";
'  case 2
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) ":";
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) "-";
'  case 3
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) "<";
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) "^";
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) ">";
'    print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) "v";
'  end select
'  print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) scr_text$(x%,y%);
'  print @((x%+xoffset%)*fwidth%,(y%+yoffset%)*fheight%) CHARS_OUT$(x%,y%);
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
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
