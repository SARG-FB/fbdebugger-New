''=================================================================
'===== DEBUGGER FOR FREEBASIC === (C) 2006-2022 Laurent GRAS =====
'=================================================================

dim shared as string defarray(99999) ''remove me when all runs finely

#include once "dbg_defines.bi"

''mutex for blocking second thread before continue
dim shared as any ptr blocker
blocker=mutexcreate

''state of cc on each line can be KCC_ALL / KCC_NONE
''could be partially true but should be the main state
dim shared as KCC_STATE ccstate

dim shared as integer debugevent
dim shared as integer debugdata ''index of bp or address of BP (case BP on mem)
dim shared as STRING  libelexception
dim shared as integer ssadr ''address of line for restoring breakcpu when singlestepping
dim shared as INTEGER firsttime
dim shared as INTEGER srcstart

''codes when debuggee stopped and corresponding texts
Dim Shared stopcode As Integer
Dim Shared stoplibel(20) As String*17 =>{"","BP On line","BP perm/tempo","BP cond","BP var","BP mem"_
,"BP count","Halt by user","Access violation","thread created","Exception","multi threads"}

''source files
dim Shared as String  source(SRCMAX)        ''source names with path
dim Shared as String  srcname(SRCMAX)       ''source names without path
dim Shared as tlist   srclist(SRCMAX)       ''to sort
dim shared as integer srclistfirst          ''first sorted element
dim shared as integer srccombocur           ''current combo choice
dim shared as any ptr sourceptr(SRCMAX)     ''pointer doc scintilla
dim shared as any ptr oldscintilla          ''last pointer for scintilla
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
dim shared As integer procnew ''used for finding address of first fbc line
dim shared As Integer procmain
dim shared as tlist   proclist(PROCMAX)
dim shared as integer proclistfirst ''first sorted element

dim Shared As integer procsv,procsk,proccurad,procfn,procsort
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

''excluded lines
Dim Shared As integer excldnb
Dim Shared As texcld excldlines(EXCLDMAX)

''log
dim shared as hwnd hlogbx
dim shared as string vlog
dim SHARED as integer logtyp

dim shared as hwnd heditorbx
dim shared as integer afterkilled ''what doing after debuggee killed

#ifdef __fb_win32__
	''Threads
	Dim Shared thread(THREADMAX) As tthread  ''zero based
	Dim Shared threadnb As Integer
	Dim Shared threadcur As Integer
	Dim Shared threadprv As Integer     'previous thread used when mutexunlock released thread or after thread create
	Dim Shared threadsel As Integer     'thread selected by user, used to send a message if not equal current
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

	'print "MutexLock00"
	MutexLock blocker
#else ''linux
	dim shared as long dbgpid '' debugger pid
	dim shared as long pid '' in code defined dgbhand
	dim shared as long thread2 ''second thread
	dim shared as long threadhs '' in code defined dgbhand
	dim shared as long threadcur '' index
	dim shared as long threadsel '' index
	dim shared as long threadnewid '' new thread tid
	dim shared as integer threadnewidcount '' count for new thread (needed for accessing first basic line)

	dim shared as long statussaved
	dim shared as long threadsaved

	dim shared as pt_regs regs
	Dim Shared As tthread thread(THREADMAX) ''zero based
	Dim Shared As Integer threadnb
	dim shared as GtkWidget ptr wsci
	''DLL = .so
	Dim Shared As tdll dlldata(DLLMAX) ''base 1
	Dim Shared As Integer dllnb
	Dim Shared As Integer msgcmd
	Dim Shared As Integer msgad
	Dim Shared As Integer msgad2
	Dim Shared As Integer msgdata
	Dim Shared As Integer msgdata2
	dim shared as ANY ptr condid
	condid=condcreate()
	dim shared as boolean bool1,bool2 ''predicate for each thread of the debugger


	dim shared errorlibel(...) as string*35=>{"","Operation not permitted","No such file or directory","No such process","Interrupted system call" _
	,"I/O error","No such device or address","Argument list too long","Exec format error","Bad file number","No child processes","Try again" _
	,"Out of memory","Permission denied","Bad address","Block device required","Device or resource busy","File exists","Cross-device link" _
	,"No such device","Not a directory","Is a directory","Invalid argument","File table overflow","Too many open files","Not a typewriter" _
	,"Text file busy","File too large","No space left on device","Illegal seek","Read-only file system","Too many links","Broken pipe" _
	,"Math argument out of domain of func","Math result not representable"}

#endif

''miscellanous data
dim shared as boolean prun=false    ''debuggee running
dim shared as integer runtype=RTOFF ''running type
Dim Shared As integer breakcpu=&hCC ''asm code for breakpoint
Dim Shared As integer breakadr ''address of last ABP kept in case of exception (RTCRASH)
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
Dim Shared As Byte flagascii          ' flag for dump displays only code ascii <128 by default just >32
dim shared as boolean flagupdate = true ''if true proc/var, dump and watched displayed
Dim Shared as boolean flagattach      ' flag for attach

''handles
dim shared as HWND hmain,hscint

''settings
dim shared as HWND hsettings
dim shared as integer setbuttons =-1 ''all buttons are set

''for autostepping
dim shared as integer autostep=50
dim shared as integer flaghalt

''watched
dim Shared wtch(WTCHMAX) As twtch  ''zero based
Dim Shared wtchcpt As Integer 'counter of watched value, used for the menu
Dim Shared hwtchbx As HWND    'handle
Dim Shared wtchidx As Integer 'index for delete
Dim Shared wtchexe(9,WTCHMAX) As String 'watched var (no memory for next execution)
Dim Shared wtchnew As Integer 'to keep index after creating new watched

''breakpoint on line
dim Shared as tbrkol brkol(BRKMAX)
dim shared as integer brknb
dim Shared as String brkexe(9,BRKMAX) 'to save breakpoints by session
dim shared as hwnd hbrkbx ''window for managing breakpoints
dim shared as integer brkidx1 ''index for BP mem or cond
dim shared as integer brkidx2
dim shared as integer brkadr1 ''address for BP mem or cond
dim shared as integer brkadr2
dim shared as integer brkdatatype
dim shared as integer brkttb
dim shared as valeurs brkdata2
dim shared as integer brktyp  ''type of BP mem/const or mem/mem
dim shared as hwnd hbpcondbx ''dialog box for managing var/const cond BP
dim shared as tbrclist listitem(VRRMAX)
dim shared as integer listcpt ''used when filling array for cond BP
dim shared as INTEGER brkline(BRKMAX) ''used when deleting the BP one by one

''breakpoint on variable/memory (when there is a change)
Dim Shared As tbrkv brkv
Dim Shared As HWND hbrkvbx ''handle

''call chain
Dim Shared As HWND hcchainbx
Dim Shared As HWND hlviewcchain
dim shared as integer procrsav(PROCRMAX) ''index of procr
dim shared as integer cchainthid

''edit box
Dim Shared As HWND heditbx
dim shared as tedit edit ''data when editing var or mem

''dump memory
dim shared as hwnd hdumpbx ''window for handling dump
Dim Shared dumplines As Integer =20 'nb lines(default 20)
Dim Shared dumpadr   As Integer    'address for dump
Dim Shared dumpbase  As Integer =0 'value dump dec=0 or hexa=50
dim shared dumpadrbase   as integer =1 'address display in dec (1) or hex (0)
Dim Shared dumpnbcol As Integer
Dim Shared dumptyp   As Integer =2

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

Dim Shared htviewvar As HWND 'running proc/var
Dim Shared htviewprc As HWND 'all proc
Dim Shared htviewthd As HWND 'all threads
Dim Shared htviewwch As HWND 'watched variables
Dim Shared hlviewdmp as hwnd 'dump

''variable find
Dim Shared As tvarfind varfind

''show/expand
Dim Shared As tshwexp shwexp
Dim Shared As hwnd hshwexpbx
Dim Shared As hwnd htviewshw
Dim Shared As tvrp vrp(VRPMAX)



dim shared as HMENU HMenusource
dim shared as HMENU HMenusource1
dim shared as HMENU HMenusource2
dim shared as HMENU HMenusource3
dim shared as HMENU HMenusource4
dim shared as HMENU HMenusource5
dim shared as HMENU HMenuvar
dim shared as HMENU HMenuvar1
dim shared as HMENU HMenuvar2
dim shared as HMENU HMenuvar3
dim shared as HMENU HMenuvar4
dim shared as HMENU HMenuvar5
dim shared as HMENU HMenuvar6
dim shared as HMENU HMenuprc
dim shared as HMENU HMenuwch
dim shared as HMENU HMenuthd
dim shared as HMENU HMenutools

Dim Shared As double fasttimer

'' index selection
#define KCOLMAX 30
#define KLINEMAX 50

dim shared as hwnd hindexbx
Dim Shared As tindexdata indexdata

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
dim shared as string compilerversion ''compiler version retrieved stabs code = 255
dim shared as string cmdlimmediat
''============================
includebinary("buttons/step.bmp",butSTEP)
includebinary("buttons/stepover.bmp",butSTEPOVER)
includebinary("buttons/auto.bmp",butAUTO)
includebinary("buttons/stop.bmp",butSTOP)
includebinary("buttons/runto.bmp",butCURSOR)
includebinary("buttons/runtoend.bmp",butRUNEND)
includebinary("buttons/runtoexit.bmp",butRUNEXIT)
includebinary("buttons/kill.bmp",butKILL)
includebinary("buttons/free.bmp",butFREE)
includebinary("buttons/exemod.bmp",butEXEMOD)
includebinary("buttons/runtocrash.bmp",butRUNCRASH)

includebinary("buttons/BP_perm.bmp",butBRKP)
includebinary("buttons/BP_cond.bmp",butBRKC)
includebinary("buttons/BP_temp.bmp",butBRKT)
includebinary("buttons/BP_count.bmp",butBRKN)
includebinary("buttons/BP_disab.bmp",butBRKD)

includebinary("buttons/restart.bmp",butRERUN)
includebinary("buttons/multiexe.bmp",butLASTEXE)
includebinary("buttons/attachexe.bmp",butATTCH)
includebinary("buttons/files.bmp",butFILE)
includebinary("buttons/tools.bmp",butTOOL)

includebinary("buttons/update.bmp",butUPDATE)
includebinary("buttons/noupdate.bmp",butNOUPDATE)

#include "dbg_gui.bas"
#include "dbg_tools.bas"
#include "dbg_brk.bas"
#include "dbg_extract.bas"
#include "dbg_proc.bas"
#include "dbg_actions.bas"
#Ifdef __fb_win32__
	#include "dbg_windows.bas"
#else
	#include "dbg_linux.bas"
	dbgpid=getpid
	'print "first pid=";dbgpid
#EndIf
gui_init()
ini_read()
reinit()
statusbar_text(KSTBSTS,"Ready")

''fbdebugger launched by script or another application (ex editor) with debuggee and possibly params
if command(0)<>"" then
	external_launch()
EndIf
'====================
''main loop
'====================
#ifdef __fb_win32__
Dim As Long iEventRightClick = Eventrbdown
#else
Dim As Long iEventRightClick = Eventrbup
#endif
do
	Var event=WaitEvent()
	''closing a window
	If Event=EventClose then
		if EventHwnd=hmain then  ''main window
			closes_debugger()
		else
			HideWindow(EventHwnd,KHIDE)
			if EventHwnd=hsettings then ''closing settings box
				settings_update()
			elseif EventHwnd=hshwexpbx then ''releases shwewp
				shwexp.free=true
			end if
		end if
		continue do
	end if
	'size changed
	if event=EventSize then
		if EventHwnd=hmain then
			size_changed()
		EndIf
		continue do
	EndIf

	If event=EventMenu then
		menu_action(EventNumber)

	'' contextual menu
	Elseif Event=iEventRightClick Then
		context_menu()
	elseIf event=EventLBdown Then
		If EventNumberListView=GIDXTABLE Then
			index_cell()
		elseIf EventNumberTreeView=GTVIEWBRC Then
				brc_check()
		EndIf
	elseif event=eventgadget then
		button_action(eventnumber())
	endif
loop
