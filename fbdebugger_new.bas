'=================================================================
'===== DEBUGGER FOR FREEBASIC === (C) 2006-2021 Laurent GRAS =====
'=================================================================
#include once "dbg_defines.bi"

''source files
dim Shared as String  source(SRCMAX)        ''source names
dim shared as any ptr sourceptr(SRCMAX)     ''pointer doc scintilla
dim Shared As UByte   sourcebuf(SRCSIZEMAX) ''buffer for loading source file
dim Shared as Integer sourcenb =-1          ''number of src, 0 based
dim Shared As integer sourceix              ''source index when loading data
dim shared as any ptr currentdoc            ''current doc pointer  todo set local in source_load ?

dim Shared As integer srcdisplayed			''index displayed source

''lines
dim Shared As Integer linenb,rlineprev ''numbers of lines, index of previous executed line (rline)
dim Shared As Integer linenbprev ''used for dll
dim Shared As Integer lastline
dim Shared as tline rline(LINEMAX) ''1 based

''current tab:line
dim Shared As integer srccur   ''index source line to be executed
dim Shared As Integer linecur  ''line to be executed (inside source)
dim Shared As Integer rlinecur ''line to be executed

''procedures
dim Shared as tproc proc(PROCMAX) ''list of procs in code
dim shared As integer procnb

dim Shared As integer procsv,procad,procin,procsk,proccurad,procregsp,procfn,procbot,proctop,procsort
dim Shared As tprocr procr(PROCRMAX) ''list of running proc
dim shared As Integer procrnb

''arrays
dim Shared  As tarr arr(ARRMAX)
dim shared as integer arrnb

''variables
dim Shared vrbloc      As Integer ''pointer of loc variables or components (init VGBLMAX+1)
Dim Shared vrbgbl      As Integer ''pointer of globals or components
Dim Shared vrbgblprev  As Integer ''for dll, previous value of vrbgbl, initial 1
Dim Shared vrbptr      As Integer Ptr ''equal @vrbloc or @vrbgbl
Dim Shared vrb(VARMAX) As tvrb ''1 based

''running variables
Dim Shared vrr(VRRMAX) As tvrr
Dim Shared vrrnb As integer

''tracking arrays
Dim Shared As ttrckarr trckarr(TRCKARRMAX)

''udt/structures
Dim Shared udt(TYPEMAX) As tudt,udtidx As Integer
Dim Shared cudt(CTYPEMAX) As tcudt,cudtnb As Integer,cudtnbsav As Integer
'in case of module or DLL the udt number is initialized each time
Dim Shared As Integer udtcpt,udtmax 'current, max cpt

'dwarf management todo remove
'Dim Shared As Long udtbeg,cudtbeg,locbeg,vrbbeg,prcbeg

''excluded lines
Dim Shared As integer excldnb
Dim Shared As texcld excldlines(EXCLDMAX)


#ifdef __fb_win32__
	''Threads
	Dim Shared thread(THREADMAX) As tthread
	Dim Shared threadnb As Integer =-1
	Dim Shared threadcur As Integer
	Dim Shared threadprv As Integer     'previous thread used when mutexunlock released thread or after thread create
	Dim Shared threadsel As Integer     'thread selected by user, used to send a message if not equal current
	Dim Shared threadaut As Integer     'number of threads for change when  auto executing
	Dim Shared threadcontext As HANDLE
	Dim Shared threadhs As HANDLE       'handle thread to resume
	Dim Shared dbgprocid As Integer     'pinfo.dwProcessId : debugged process id
	Dim Shared dbgthreadID As Integer   'pinfo.dwThreadId : debugged thread id
	Dim Shared dbghand As HANDLE  		'debugged proc handle
	Dim Shared dbghthread As HANDLE     'debuggee thread handle
	Dim Shared dbghfile  As HANDLE   	'debugged file handle
	Dim Shared pinfo As PROCESS_INFORMATION

	''DLL
	Dim Shared As tdll dlldata(DLLMAX) ''base 1
	Dim Shared As Integer dllnb

	''attach running exe
	Dim Shared hattach As HANDLE    'handle to signal attchement done	
	
#endif

''miscellanous data
dim shared as boolean prun=false    ''debuggee running
dim shared as integer runtype=RTOFF ''running type
Dim Shared As integer breakcpu=&hCC ''asm code for breakpoint
Dim Shared As Integer dsptyp=0      ''type of display : 0 normal/ 1 source/ 2 var/ 3 memory

''put in a ctx with type ??
dim shared As boolean procnodll
dim shared As boolean flagmain
Dim Shared As boolean flagkill =FALSE 'flag if killing process to avoid freeze in thread_del
Dim Shared As Integer flagrestart=-1  'flag to indicate restart in fact number of bas files to avoid to reload those files
Dim Shared As Integer flagwtch  =0     'flag =0 clean watched / 1 no cleaning in case of restart 
Dim Shared As Byte flaglog =0         ' flag for dbg_prt 0 --> no output / 1--> only screen / 2-->only file / 3 --> both
Dim Shared As Byte flagtrace          ' flag for trace mode : 1 proc / +2 line
Dim Shared As Byte flagverbose        ' flag for verbose mode
dim shared as boolean flagupdate = true ''if true proc/var, dump and watched displayed
Dim Shared as boolean flagattach      ' flag for attach

''handles
dim shared as HWND hmain,hscint,hsettings
dim shared as hwnd hdumpbx ''window for handling dump

''for autostepping
dim shared as integer autostep=50
' input_box
Dim Shared inputval As ZString *25
Dim Shared inputtyp As Byte
dim shared as hwnd hinputbx ''window for handling inputval

''watched
dim Shared wtch(WTCHMAX) As twtch  ''zero based
Dim Shared wtchcpt As Integer 'counter of watched value, used for the menu 
Dim Shared hwtchbx As HWND    'handle
Dim Shared wtchidx As Integer 'index for delete 
Dim Shared wtchexe(9,WTCHMAX) As String 'watched var (no memory for next execution)
Dim Shared wtchnew As Integer 'to keep index after creating new watched

''breakpoint on line
dim Shared as breakol brkol(BRKMAX)
dim shared as integer brknb
Dim Shared as String brkexe(9,BRKMAX) 'to save breakpoints by session

''breakpoint on variable/memory (when there is a change)
Dim Shared brkv As tbrkv 
Dim Shared brkv2 As tbrkv 'copie for use inside brkv_box
Dim Shared brkvhnd As HWND   'handle

''Backtracking
Dim Shared As Integer bcktrkpr

''dump memory
Dim Shared dumplines As Integer =20 'nb lines(default 20)
Dim Shared dumpadr As Integer    'address for dump
Dim Shared dumpdec As Integer =0 'value dump dec=0 or hexa=50
Dim Shared dumpnbcol As Integer
Dim Shared dumptyp As Integer =1

''font
Dim Shared As Integer fontsize=KSIZE8
Dim Shared As Integer fontcolor
Dim Shared As String  fontname
fontname="Courier new"

''for retrieving data from ini file
Dim Shared As Integer restorefontsize
Dim Shared As Integer restorefontcolor
Dim Shared As String  restorefontname
Dim Shared As Integer restorex
Dim Shared As Integer restorey
Dim Shared As Integer restorew
Dim Shared As Integer restoreh

Dim Shared htviewcur As HWND  'TV1 ou TV2 ou TV3
Dim Shared htviewvar As HWND 'running proc/var
Dim Shared htviewprc As HWND 'all proc
Dim Shared htviewthd As HWND 'all threads
Dim Shared htviewwch As HWND 'watched variables
Dim Shared hlviewdump as hwnd 'dump

''codes when debuggee stopped and corresponding texts
Dim Shared stopcode As Integer
Dim Shared stoplibel(20) As String*17 =>{"","cursor","tempo break","break","Break var","Break mem"_
,"Halt by user","Access violation","New thread","Exception"}


''variable find
Dim Shared As tvarfind varfind

'''show/expand
Dim Shared As Integer shwexpnb 'current number of show/expand box
Dim Shared As tshwexp shwexp(1 To SHWEXPMAX) 'data for show/expand
Dim Shared As tvrp vrp(SHWEXPMAX,VRPMAX)

dim shared as HMENU HMenusource
dim shared as HMENU HMenusource2
dim shared as HMENU HMenusource3
dim shared as HMENU HMenusource4
dim shared as HMENU HMenusource5
dim shared as HMENU HMenuvar
dim shared as HMENU HMenuvar2
dim shared as HMENU HMenuvar3
dim shared as HMENU HMenuvar4
dim shared as HMENU HMenuvar5
dim shared as HMENU HMenuprc
dim shared as HMENU HMenuwch
dim shared as HMENU HMenuthd
dim shared as HMENU HMenutools

Dim Shared fasttimer As double

''slash for file WDS<>LNX
dim shared as zstring *2 slash
#Ifdef __fb_win32__
	slash="\"
#else
	slash="/"
#endif

udt(0).nm="Unknown"

#Ifdef __FB_64BIT__
   udt(1).nm="long":udt(1).lg=Len(Long)
#Else
   udt(1).nm="Integer":udt(1).lg=Len(Integer)
#EndIf
udt(2).nm="Byte":udt(2).lg=Len(Byte)
udt(3).nm="Ubyte":udt(3).lg=Len(UByte)
udt(4).nm="Zstring":udt(4).lg=Len(Integer)
udt(5).nm="Short":udt(5).lg=Len(Short)
udt(6).nm="Ushort":udt(6).lg=Len(UShort)
udt(7).nm="Void":udt(7).lg=Len(Integer)

''todo remove
'udt(7).index=7'dwarf

#Ifdef __FB_64BIT__
   udt(8).nm="Ulong":udt(8).lg=Len(ULong)
#Else
   udt(8).nm="Uinteger":udt(8).lg=Len(UInteger)
#EndIf

#Ifdef __FB_64BIT__
   udt(9).nm="Integer":udt(9).lg=Len(Integer)
#Else
   udt(9).nm="Longint":udt(9).lg=Len(LongInt)
#EndIf

#Ifdef __FB_64BIT__
   udt(10).nm="Uinteger":udt(10).lg=Len(UInteger)
#Else
   udt(10).nm="Ulongint":udt(10).lg=Len(ULongInt)
#EndIf

udt(11).nm="Single":udt(11).lg=Len(Single)
udt(12).nm="Double":udt(12).lg=Len(Double)
udt(13).nm="String":udt(13).lg=Len(String)
udt(14).nm="Fstring":udt(14).lg=Len(Integer)
udt(15).nm="fb_Object":udt(15).lg=Len(UInteger)
udt(16).nm="Boolean":udt(16).lg=Len(boolean)


''last debugged exes / command line parameters
Dim Shared savexe(9) As String 'last 10 exe, 0=more recent
Dim Shared cmdexe(9) As String 'last 10 exe
dim shared as string exename
Dim Shared exedate As Double 'serial date

''============================
#include "dbg_gui.bas"
#include "dbg_tools.bas"
#include "dbg_extract.bas"
#include "dbg_buttons.bas"
#include "dbg_menu.bas"
#Ifdef __fb_win32__
	#include "dbg_windows.bas"
#else	
	
#EndIf


gui_init
reinit

SetStatusBarField(1,0,100,"No debuggee")
''for testing todo remove
exename="D:\telechargements\win9\tmp\test_include_main"
exe_sav(exename)
srccur=0
linecur=10
rlinecur=14
''
settitle()
SetStatusBarField(1,0,100,"Loading data")
if elf_extract(exename) then
	list_all
	SetStatusBarField(1,0,100,"Waiting")
	print "------------------ after extraction -----------------------"
	print "Number of files=";sourcenb
	
	'hidewindow(scint ,1)
	for isrc as integer =0 to sourcenb
		source(isrc)="D:\telechargements\win9\tmp\"+source_name(source(isrc))
		print "index=";isrc;" file=";source(isrc)
	
		AddPanelGadgetItem(GSRCTAB,isrc,source_name(source(isrc)))
		''later sort the files to get them in alphabetic order
		AddComboBoxItem(GFILELIST,source_name(source(isrc)),-1)
	next
	'SetGadgetFont(GSRCTAB,CINT(LoadFont("Courier New",10)))
	source_load(0,filedatetime(exename))
	
	''for testing to be removed
		
		'line_color(5,2)
		'line_color(10,2)
		'line_color(4,2)
		for imark as Integer = 0 To 5
			send_sci(SCI_MarkerAdd, imark, imark)       'line, marker#
		next
		linecur_change(rlinecur)
	hidewindow(hscint ,0)
	SetStatusBarField(1,0,100,"Waiting")
end if


''========================================
''========================================
''========================================
''========================================


do
	Var event=WaitEvent()
	If Event=EventClose then
		if EventHwnd=hmain then
			if messbox("Quit Fbdebugger","Are you sure ? (pgm still running)",MB_YESNO)=6 then
				''todo need to release doc SCI_RELEASEDOCUMENT(<unused>, pointer doc)
				release_doc
				end
			endif
		else
			HideWindow(EventHwnd,1)
			if EventHwnd=hsettings then
				''todo put in a dedicated proc
				cmdexe(0)=GetGadgetText(GCMDLPARAM)
				autostep=valint(GetGadgetText(GAUTODELAY))
				if autostep<50 then
					autostep=50
					SetGadgetText(GAUTODELAY,str(autostep))
					messbox("Delay for autostepping","Too small, reset to "+str(autostep))
				elseIf autostep>10000 then
					autostep=10000
					SetGadgetText(GAUTODELAY,str(autostep))
					messbox("Delay for autostepping","Too big, reset to "+str(autostep))
				end if
			elseif EventHwnd=hinputbx then ''resets inputval and closes box
				inputval=""
				hidewindow(hinputbx,1)
			end if
		end if
	end if
	If event=EventMenu then
		menu_action(EventNumber)
	ElseIf event=eventrbdown then
		if GlobalMouseX<500 then
			DisplayPopupMenu(HMenusource, GlobalMouseX,GlobalMouseY)
		else
			if PanelGadgetGetCursel(GRIGHTTABS)=0 then
				DisplayPopupMenu(HMenuvar, GlobalMouseX,GlobalMouseY)
			elseif PanelGadgetGetCursel(GRIGHTTABS)=1 then
				DisplayPopupMenu(HMenuprc, GlobalMouseX,GlobalMouseY)
			endif
		endif
	elseif event=eventgadget then
		if eventnumber()=GFILESEL then
        	if GetItemComboBox(GFILELIST)<>-1 then
        		if GetItemComboBox(GFILELIST)<>PanelGadgetGetCursel(GSRCTAB) then
	        		MessBox("Jumping to file ="+str(GetItemComboBox(GFILELIST)),source(GetItemComboBox(GFILELIST)))
	        		PanelGadgetSetCursel(GSRCTAB,GetItemComboBox(GFILELIST))
        			source_change(GetItemComboBox(GFILELIST))
        		else
        			MessBox("File already displayed",source(GetItemComboBox(GFILELIST)))
        		endif
        	else
        		messbox("Select a file", "before clicking on the button")
        	endif
		elseif eventnumber()=GCURRENTLINE then
				messbox("Click on Current line","Jumping to the file/line")
				linecur_display()
		elseif eventnumber()=GSRCTAB then
			Messbox("The selected panel for file:",source(PanelGadgetGetCursel(GSRCTAB)))
			source_change(PanelGadgetGetCursel(GSRCTAB))
		elseif eventnumber()=GFILELIST then
			''nothing to execute with file combo
		else
			button_action(eventnumber())
		endif
	endif
loop
