''defines etc for fbdebugger_new
''dbg_define.bi 

''table of enums

''menu : 1000+




#macro mydefine(d,v)
	#define ##d v
	if defarray(v)<>"" then 
		print "error value ";v;" for ";##d; " already used by ";defarray(v)
	else	
		defarray(v)=##d
	end if
#endmacro


#Define fbdebuggerversion "V 3.00 BETA 32-64bit"

'#define fulldbg_prt 'uncomment to get more information
#Define dbg_prt2 Rem 'used temporary for debugging fbdebugger, change rem by dbg_prt

 'take l char form a string and complete with spaces if needed
#Define fmt(t,l) Left(t,l)+Space(l-Len(t))+"  "
#Define fmt2(t,l) Left(t,l)+Space(l-Len(t))


#include once "Window9.bi"
#include once "scintilla.bi"
#include once "SciLexer.bi"
#Include Once "file.bi"

'define data 64bit/32bit
#Ifdef __FB_64BIT__
   #Define regip rip
   #Define regbp rbp
   #Define regsp rsp
   #if __FB_VERSION__ >= "1.08"
   		#define ver3264 "(1.08-64bit) "
   #else
   		#define ver3264 "(1.07-64bit) "
   #endif
#Else
   #Define regip eip
   #Define regbp ebp
   #Define regsp esp
   #if __FB_VERSION__ >= "1.08"
   		#define ver3264 "(1.08-32bit) "
   #else
   		#define ver3264 "(1.07-32bit) "
   #endif
#endif

''to handle new added field in array descriptor structure
#if __FB_VERSION__ >= "1.08"
	#define KNEWARRAYFIELD ''to skip flag field
#endif

#ifdef __fb_win32__
	#Include Once "windows.bi"
	#Include Once "win\commctrl.bi"
	#Include Once "win\commdlg.bi"
	#Include Once "win\wingdi.bi"
	#Include Once "win\richedit.bi"
	#Include Once "win\tlhelp32.bi"
	#Include Once "win\shellapi.bi"
	#Include Once "win\psapi.bi" 


	'' if they are not already defined
	#Ifndef EXCEPTION_DEBUG_EVENT
		#Define EXCEPTION_DEBUG_EVENT  1
		#define CREATE_THREAD_DEBUG_EVENT  2
		#define CREATE_PROCESS_DEBUG_EVENT  3
		#define EXIT_THREAD_DEBUG_EVENT  4
		#define EXIT_PROCESS_DEBUG_EVENT  5
		#define LOAD_DLL_DEBUG_EVENT  6
		#define UNLOAD_DLL_DEBUG_EVENT  7
		#define OUTPUT_DEBUG_STRING_EVENT  8
		#define RIP_EVENT  9
		'' DUPLICATE #define DBG_CONTINUE  &h00010002
		#define DBG_TERMINATE_THREAD           &h40010003
		#define DBG_TERMINATE_PROCESS          &h40010004
		#define DBG_CONTROL_C                  &h40010005
		#define DBG_CONTROL_BREAK              &h40010008
	#EndIf
	'' DBG_EXCEPTION_NOT_HANDLED = &H80010001
	#Define EXCEPTION_GUARD_PAGE_VIOLATION      &H80000001
	#Define EXCEPTION_NO_MEMORY                 &HC0000017
	#Define EXCEPTION_FLOAT_DENORMAL_OPERAND    &HC000008D
	#Define EXCEPTION_FLOAT_DIVIDE_BY_ZERO      &HC000008E
	#Define EXCEPTION_FLOAT_INEXACT_RESULT      &HC000008F
	#Define EXCEPTION_FLOAT_INVALID_OPERATION   &HC0000090
	#Define EXCEPTION_FLOAT_OVERFLOW            &HC0000091
	#Define EXCEPTION_FLOAT_STACK_CHECK         &HC0000092
	#Define EXCEPTION_FLOAT_UNDERFLOW           &HC0000093
	#Define EXCEPTION_INTEGER_DIVIDE_BY_ZERO    &HC0000094
	#Define EXCEPTION_INTEGER_OVERFLOW          &HC0000095
	#Define EXCEPTION_PRIVILEGED_INSTRUCTION    &HC0000096
	#Define EXCEPTION_CONTROL_C_EXIT            &HC000013A


		''DLL
	Const DLLMAX=300
	Type tdll
		As HANDLE   hdl 'handle to close
		As UInteger bse 'base address
		As integer  tv  'item treeview to delete
		As Integer gblb 'index/number in global var table
		As Integer gbln 
		As Integer  lnb 'index/number in line
		As Integer  lnn 
		As String   fnm 'full name
	End Type

	'' Output information
	#define dbg_prt(txt) output_wds(txt)
	declare sub output_wds(as string)
''end of define for windows

#else
	'' Output information
	define dbg_prt(txt) output_lnx(txt)
	'declare output_lnx(as string)  todo create output_lnx
#endif

#Define TYPESTD 17 ''upper limit for standard type, now 17 for va_list 2020/02/05

'' DATA STAB
Type udtstab
	stabs As long    ''offset for string
	code As UShort   ''stabs type
	nline As UShort  ''line number
	ad As Integer   ''address 64bit for gas64
End Type
#Define STAB_SZ_MAX 60000  ''max stabs string

Enum
NODLL
DLL
End Enum

''source code files
#Define SRCSIZEMAX 5000000 ''max source size

Const   SRCMAX=1000		   ''max source file

enum
	GSRCTAB=500       ''panel
	GFILELIST    ''file combo
	GFILESEL	   ''button for selecting a file

	GSCINTILLA

	''current line
	GCURRENTLINE
	GCURLINETTIP

	''right panels
	GRIGHTTABS
	GTVIEWVAR
	GTVIEWPRC
	GTVIEWTHD
	GTVIEWWCH
	GDUMPMEM
end enum

'' don't change the numbers as direct index for right tabs
#define TABIDXVAR 0
#define TABIDXPRC 1
#define TABIDXTHD 2
#define TABIDXWCH 3
#define TABIDXDMP 4


''different styles windows/linux
#Ifdef __FB_WIN32__
	#define KTRRESTYLE TVS_HASLINES or TVS_HASBUTTONS or TVS_LINESATROOT
#Else
	#define KTRRESTYLE 0
	#define hicon HBITMAP
#endif





''log and others uses
#define GEDITOR 330

'button main screen
enum
	IDBUTSTEP=600
	IDBUTSTEPP
	IDBUTSTEPM
	IDBUTAUTO
	IDBUTRUN
	IDBUTSTOP
	IDBUTFREE
	IDBUTTOOL
	IDBUTFILE
	IDBUTRERUN
	TTRERUN ''tooltip for button rerun
	IDBUTATTCH
	IDBUTKILL
	IDBUTLASTEXE
	IDBUTFASTRUN
	IDBUTEXEMOD
	IDBUTSTEPB
	IDBUTSTEPT
	IDBUTCURSOR
	IDBUTUPDATE

	IDBUTENLRSRC
	IDBUTENLRVAR
	IDBUTENLRMEM
end enum

'' source menu
enum
	MNSTEP=1000
	MNCURSOR
	MNSTEPP
	MNSTEPM
	MNSTEPT
	MNSTEPB
	MNRUN
	MNFASTRUN
	MNSTOP
	MNKILL
	MNAUTO
	MNTHRDAUT'automatic execution  alternating threads
	MNEXEMOD	
	MNSETBRK
	MNSETBRKC
	MNRSTBRKC
	MNCHGBRKC
	MNSETBRT
	MNBRKENB
	MNMNGBRK
	MNSHWVAR
	MNSETWVAR
	MNFNDTXT
	MNGOTO
	MNLINEADR
	MNASMLINE
	MNASMPRCL
	MNASMREGS
	MNACCLINE
	MNFCSSRC	
end enum

''proc/var menu
enum
	MNVARDMP=1050
	MNVAREDT
	MNVARBRK
	MNSELIDX
	MNSHSTRG
	MNSHWEXP
	MNSETWTCH
	MNSETWTTR
	MNCHGZSTR
	MNCALLINE
	MNLSTVARA 'list  all proc/variables
	MNLSTVARS 'list  onlyselectedand below
	MNPBCKTRK 'backtrakingfromproc/var
	MNPCHNING 'chaining  from proc/var
	MNSHCHAR  'show  character  in a  string at a  selectedposition'03/11/2014
	MNCLBVARA 'copy  to clipboard  all procs/vars '28/11/2014
	MNCLBVARS 'copy  to clipboard  selectedvar
	MNPTDUMP  'dump  deferenced data
	MNFNDVAR  'find  proc or var in proc/var
end enum

''tracking array menu
enum
	MNTRCKIDX0=1100 ''variable used as index
	MNTRCKIDX1
	MNTRCKIDX2
	MNTRCKIDX3
	MNTRCKIDX4
	MNTRCKARR ''associate var indexes to an array
	MNTRCKRST ''reset all
end enum

'' proc menu
enum
	MNRSTPRC=1150  ''reset procfollow
	MNSETPRC  ''setprocfollow
	MNSORTPRC ''toggle sortby module name/  procname
	MNASMPRC  ''listing of asm code of a  proc
end enum

'' thread menu
enum
	MNTHRDCHG=1200 ''select thread
	MNLOCPRC  ''locate proc
	MNTHRDKLL ''kill  thread
	MNEXCLINE ''show  next executed line
	MNCREATHR ''show  line creating thread
	MNTHRDLST ''list  threads
	MNSHWPROC ''show  procin proc/var
	MNSHPRSRC ''show  procin source
	MNTBCKTRK ''backtracking
	MNTCHNING ''chaining
	MNTHRDEXP ''expand one thread
	MNTHRDCOL ''collapse  all threads
	MNPRCRADR ''addr  about  running procstart,end  stack
end enum

'' tools menu
enum
	MNABOUT=1250	
	MNCMPINF	
	MNDBGHELP
	MNSHWLOG	
	MNDELLOG	
	MNLISTENUM
	MNLISTPROCESS
	MNLISTDLL
	MNWINMSG
	MNSHWBDH
	MNFRTIMER
	MNJITDBG	
end enum

'' watched menu
enum
	MNWCHVAR=1300
	MNWCHDMP
	MNWCHDEL
	MNWCHSTG
	MNWCHSHW
	MNWCHEDT
	MNWCHTTGL
	MNWCHTTGA
	MNWCHDALL
end enum

'' previous debugged exe
enum
	MNEXEFILE0=1350
	MNEXEFILE1
	MNEXEFILE2
	MNEXEFILE3
	MNEXEFILE4
	MNEXEFILE5
	MNEXEFILE6
	MNEXEFILE7
	MNEXEFILE8
	MNEXEFILE9
end enum

''for find text
enum
	MNFNDTXUP=1400
	MNFNDTXDW
end enum

''for scintilla
#define KRED    &hFF
#define KBLUE   &hFF0000
#define KGREEN  &hB700
#define KYELLOW &hFFFF
#define KORANGE &h04A0FB
#define KPURPLE &hEB80EB
#define KGREY   &h808080
#define KWHITE  &hFFFFFF

#define RETYES 6
#define RETNO  7

#define KSHOW 0
#define KHIDE 1

''for breakpoint management
enum 
	GBRKDEL01=800
	GBRKDEL02
	GBRKDEL03
	GBRKDEL04
	GBRKDEL05
	GBRKDEL06
	GBRKDEL07
	GBRKDEL08
	GBRKDEL09
	GBRKDEL10
	GBRKDSB01
	GBRKDSB02
	GBRKDSB03
	GBRKDSB04
	GBRKDSB05
	GBRKDSB06
	GBRKDSB07
	GBRKDSB08
	GBRKDSB09
	GBRKDSB10
	GBRKLINE01
	GBRKLINE02
	GBRKLINE03
	GBRKLINE04
	GBRKLINE05
	GBRKLINE06
	GBRKLINE07
	GBRKLINE08
	GBRKLINE09
	GBRKLINE10
	GBRKCLOSE
	GBRKDELALL
	GBRKDISABLE
	GBRKENABLE
End Enum

''index selection
enum 
	GIDXVAR=850
	GIDXMIN1
	GIDXMIN5=GIDXMIN1+4
	GIDXMAX1
	GIDXMAX5=GIDXMAX1+4
	GIDXUP1
	GIDXUP5=GIDXUP1+4
	GIDXTABLE
	GIDXAPPLY
	GIDXDEC
	GIDXINC
	GIDXAUTO
	GIDXUPD
	GIDXROWP
	GIDXROWL
	GIDXPAGEL
	GIDXPAGEP
	GIDXBLKL
	GIDXBLKP
	GIDXCOLL
	GIDXCOLP
	GIDXWIDTH
end enum

''for settings
enum
	LOGGROUP=900
	GNOLOG
	GSCREENLOG
	GFILELOG
	GBOTHLOG
	GTRACEPROC
	GTRACELINE
	GVERBOSE
	GTEXTDELAY
	GAUTODELAY
	GTEXTCMDLP
	GCMDLPARAM

	FONTGROUP
	GTEXTFTYPE
	GTEXTFSIZE
	GTEXTFCOLOR
end enum

''for inputval box
enum
GINPUTVAL=920
INPUTVALOK
INPUTVALCANCEL
end enum

''for dump memory
enum
	GDUMPAPPLY=930
	GDUMPADR

	GDUMPTSIZE
	GDUMPSIZE

	GDUMPMOVEGRP
	GDUMPCL
	GDUMPCP
	GDUMPLL
	GDUMPLP
	GDUMPPL
	GDUMPPP

	GDUMUSEGRP
	GDUMPNEW
	GDUMPWCH
	GDUMPBRK
	GDUMPSHW

	GDUMPPTRGRP
	GDUMPPTRNO
	GDUMPPTR1
	GDUMPPTR2

	GDUMPBASEGRP
	GDUMPDEC
	GDUMPHEX

	GDUMPSGNGRP
	GDUMPSGN
	GDUMPUSGN
end enum

''shw/exp
enum
	GSHWWCH=960
	GSHWDMP
	GSHWEDT
	GSHWSTR
	GSHWNEW
	GSHWRPL
	GSHWCUR
	GSHWMIN
	GSHWMAX
	GSHWSET
	GSHWINC
	GSHWRED
	GSHWCLOSE
	GSHWUPD
	GTVIEWSHW
end enum

''break on var/mem
enum
	GBRKVAR=980
	GBRKVVALUE
	GBRKVOK
	GBRKVDEL
	GBRKCOND
end enum

''procedure tracking
enum
	GTRACKPRV=990
	GTRACKCUR
	GTRACKNXT
	GTRACKPPRV
	GTRACKPCUR
	GTRACKPNXT
end enum
#Ifdef __fb_win32__
	#define send_sci(b,c,d) sendmessage(hscint,b,c,cast(integer,d))
#else
	#define send_sci(b,c,d) scintilla_send_message(cast(scintillaobject ptr,scint),b,c,cast(integer,d))
	extern "C"

	type scintillaObject as _scintillaObject
	type scintillaObjectClass as _scintillaClass

	type _scintillaObject
		cont as GtkContainer
		pscin as any ptr
	end type

	type _scintillaClass
		parent_class as GtkContainerClass
		command as sub(byval sci as scintillaObject ptr, byval cmd as long, byval window as GtkWidget ptr)
		notify as sub(byval sci as scintillaObject ptr, byval id as long, byval scn as SCNotification ptr)
	end type

	declare function scintilla_object_get_type() as GType
	declare function scintilla_object_new() as GtkWidget ptr
	declare function scintilla_object_send_message(byval sci as scintillaObject ptr, byval iMessage as ulong, byval wParam as guintptr, byval lParam as gintptr) as gintptr
	declare function scnotification_get_type() as GType
	type scintillaClass as _scintillaClass
	declare function scintilla_get_type() as GType
	declare function scintilla_new() as GtkWidget ptr
	declare sub scintilla_set_id(byval sci as scintillaObject ptr, byval id as uptr_t)
	declare function scintilla_send_message(byval sci as scintillaObject ptr, byval iMessage as ulong, byval wParam as uptr_t, byval lParam as sptr_t) as sptr_t
	declare sub scintilla_release_resources()

	#define scintILLA(obj) G_TYPE_CHECK_INSTANCE_CAST(obj, scintilla_get_type(), scintillaObject)
	#define scintILLA_CLASS(klass) G_TYPE_CHECK_CLASS_CAST(klass, scintilla_get_type(), scintillaClass)
	#define IS_scintILLA(obj) G_TYPE_CHECK_INSTANCE_TYPE(obj, scintilla_get_type())
	#define scintILLA_TYPE_OBJECT scintilla_object_get_type()
	#define scintILLA_OBJECT(obj) G_TYPE_CHECK_INSTANCE_CAST((obj), scintILLA_TYPE_OBJECT, scintillaObject)
	#define scintILLA_IS_OBJECT(obj) G_TYPE_CHECK_INSTANCE_TYPE((obj), scintILLA_TYPE_OBJECT)
	#define scintILLA_OBJECT_CLASS(klass) G_TYPE_CHECK_CLASS_CAST((klass), scintILLA_TYPE_OBJECT, scintillaObjectClass)
	#define scintILLA_IS_OBJECT_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE((klass), scintILLA_TYPE_OBJECT)
	#define scintILLA_OBJECT_GET_CLASS(obj) G_TYPE_INSTANCE_GET_CLASS((obj), scintILLA_TYPE_OBJECT, scintillaObjectClass)
	#define scintILLA_TYPE_NOTIFICATION scnotification_get_type()
	#define scintILLA_NOTIFY "sci-notify"

	end extern

#endif

#define KSTYLBREAK      1
#define KSTYLBREAKTEMPO 2
#define KSTYLBREAKCOUNT 3
#define KSTYLBREAKDISAB 4

#define KSTYLENONE 0
#define KSTYLECUR  2




Union valeurs
vinteger As Integer
vuinteger As UInteger
vsingle As Single
vdouble As Double
vlongint As LongInt
vulongint As ULongInt
vbyte As Byte
vubyte As UByte
vshort As Short
vushort As UShort
'vstring as string
'vzstring as zstring
'vwstring as wstring
End union

''font size
Const KSIZE8=8
Const KSIZE10=10
Const KSIZE12=12

'' for proc_find / thread
Const KFIRST=1
Const KLAST=2

Enum ''type udt/redim/dim
	TYUDT
	TYRDM
	TYDIM
End enum

Enum ''type of running
	RTRUN
	RTSTEP
	RTAUTO
	RTOFF
	RTFRUN
	RTFREE
	RTEND
End enum

Enum ''code stop
	CSSTEP=0
	CSCURSOR
	CSBRKTEMPO
	CSBRK
	CSBRKV
	CSBRKM
	CSHALTBU
    CSACCVIOL
    CSNEWTHRD
    CSEXCEP
End Enum

''for dissassembly
#define KLINE 1 ''from source code
#define KPROC 2 ''from source code
#define KSPROC 3 ''from proc/var

Union pointeurs
	pxxx As Any Ptr
	pinteger As Integer Ptr
	puinteger As UInteger Ptr
	psingle As Single Ptr
	pdouble As Double Ptr
	plongint As LongInt Ptr
	pulongint As ULongInt Ptr
	pbyte As Byte Ptr
	pubyte As UByte Ptr
	pshort As Short Ptr
	pushort As UShort Ptr
	pstring As String Ptr
	pzstring As ZString Ptr
	pwstring As WString Ptr
End Union

'================ Lines ==============================================
Const LINEMAX=100000
Type tline
	ad As uinteger ''offset relative to proc address
	nu As integer  ''number in file
	sv As byte     ''saved value replaced by &hCC
	px As UShort   ''proc index
	sx As UShort   ''source index need it now for lines from include and not inside proc
end Type
'===================== Procedures (sub, function, operator) ============================
Const PROCMAX=20000 'in sources
Enum
 KMODULE=0 'used with IDSORTPRC
 KPROCNM
End Enum

Type tproc
	nm As String   'name
	db As UInteger 'lower address
	fn As UInteger 'upper line address
	ed As UInteger 'upper proc end
	sr As UShort   'source index
	nu As Long     'line number to quick access
	'todo remove lastline As Long 'last line of proc (use when dwarf data)
	vr As UInteger 'lower index variable upper (next proc) -1
	rv As Integer  'return value type
	pt As Long     'counter pointer for return value (** -> 2)
	rvadr As Integer 'offset for return value adr (for now only dwarf)
    tv As integer 'in tview2
    st As Byte     'state followed = not checked
End Type

Const PROCRMAX=50000 'Running proc
Type tprocr
	sk   As UInteger  'stack
	idx  As UInteger  'index for proc
	tv   As integer 'index for treeview todo changed for linux
	'lst as uinteger 'future array in LIST
	cl   As Integer   'calling line
	thid As Integer   'idx thread
	vr   As Integer   'lower index running variable upper (next proc) -1
End type
''======================== Arrays =========================================
Const ARRMAX=1500
Type tnlu
	lb As UInteger
	ub As UInteger
End Type
Type tarr 'five dimensions max
	dm As UInteger
	nlu(5) As tnlu
End type

''====================== Variables gloables/common/locales/parameters ============================
const VARMAX=20000 'CAUTION 3000 elements taken for globals
Const VGBLMAX=3000 'max globals
Const KBLOCKIDX=100 'max displayed lines inside index selection
Type tvrb
	nm As String    'name
	typ As Integer  'type
	adr As Integer  'address or offset
	mem As UByte    'scope
	arr As tarr Ptr 'pointer to array def
	pt As long      'pointer
End type

''========================== Running variables ============================
Const VRRMAX=200000
Type tvrr
	ad    As UInteger 'address
	tv    As integer  'tview handle
	vr    As Integer  'variable if >0 or component if <0
	ini   As UInteger 'dyn array address (structure) or initial address in array
	gofs  As UInteger 'global offset to optimise access
	ix(4) As Integer  '5 index max in case of array
	arrid As integer  'index in array tracking for automatic tracking ''2016/06/02
End type

''========================= Tracking an array, displaying value using variables as indexes ================
'' ex array1(i,j) when i or j change the corresponding value of array1 is displayed
Const TRCKARRMAX=4
Type ttrckarr
	typ    As UByte     ''type or lenght ???
	memadr As UInteger  ''adress in memory
	iv     As UInteger  ''vrr index used when deleting proc
	idx    As Integer   ''array variable index in vrr
	''bname as string
End Type

''====================== UDT structures and fields ==============================
Const TYPEMAX=80000,CTYPEMAX=100000
'CAUTION : TYPEMAX is the type for bitfield so the real limit is typemax-1
Type tudt
	nm As String  'name of udt
	lb As Integer 'lower limit for components
	ub As Integer 'upper
	lg As Integer 'lenght
	en As Integer 'flag if enum 1 or 0

	''todo remove
	'index As Integer 'dwarf
	'what As Integer 'dwarf udt/pointer/array
	'typ As Integer 'dwarf
	'dimnb As Long 'dwarf
	'bounds(5) As UInteger 'dwarf
End Type
Type tcudt
	nm As String    'name of components or text for enum
	Union
	typ As Integer  'type
	Val As Integer  'value for enum
	End Union
	ofs As UInteger 'offset
	ofb As UInteger 'rest offset bits
    lg As UInteger  'lenght
	arr As tarr Ptr 'arr ptr
	pt As long
End Type

''========================= Excluded lines for procs added in dll (DllMain and tmp$x) ================
Const EXCLDMAX=10
Type texcld
	db As UInteger
	fn As UInteger
End type

''========================= Watched variables or memory ==================================
Const WTCHMAX=19 ''zero based
Const WTCHALL=9999999
Type twtch
    hnd As HWND     'handle
    tvl As integer  'tview handle
    adr As UInteger 'memory address
    typ As Integer  'type for var_sh2
    pnt As Integer  'nb pointer
    ivr As Integer  'index vrr
    psk As Integer  'stk procr or -1 (empty)/-2 (memory)/-3 (non-existent local var)/-4 (session)
    lbl As String   'name & type,etc
    arr As UInteger 'ini for dyn arr
    tad As Integer  'additionnal type
    old As String   'keep previous value for tracing
    idx As Integer  'index proc only for local var
    dlt As Integer  'delta on stack only for local var
    vnb As Integer  'number of level
    vnm(10) As String   'name of var or component
    vty(10) As String   'type
    Var     As Integer  'array=1 / no array=0
End type

''========================= Breakpoint on line ===================================
Const BRKMAX=10 'breakpoint index zero for "run to cursor"
Type breakol
	isrc    As UShort   'source index 
	nline   As UInteger 'num line for display
	index   As Integer  'index for rline
	ad      As UInteger 'address
	typ     As Byte	  'type normal=1 /temporary=0, 3 or 4 =disabled
	counter As UInteger 'counter to control the number of times the line should be executed before stopping 02/09/2015
	cntrsav As UInteger 'to reset if needed the initial value of the counter '03/09/2015
End type

''========================= Breakpoint on variable ===================================
Type tbrkv
	typ As Integer   'type of variable
	adr As UInteger  'address
	arr As UInteger  'adr if dyn array
	ivr As Integer   'variable index
	psk As Integer   'stack proc
	Val As valeurs   'value
	vst As String    'value as string
	tst As Byte=1    'type of comparison (1 to 6)
	ttb As Byte      'type of comparison (16 to 0)
	txt As String	  'name and value just for brkv_box
End type

''======================== Threads ====================================
Const THREADMAX=50
Type tthread
 hd  As HANDLE    'handle
 id  As UInteger  'ident
 pe  As Integer   'flag if true indicates proc end 
 sv  As Integer   'sav line
 od  As Integer   'previous line
 nk  As UInteger  'for naked proc, stack and used as flag
 st  As Integer   'to keep starting line
 tv  As integer 'to keep handle of thread item todo 3 lines 
 plt As integer 'to keep handle of last proc of thread in proc/var tview 
 ptv As integer 'to keep handle of last proc of thread in thread tview 
 exc As Integer   'to indicate execution in case of auto 1=yes, 0=no
 
End Type

''variable find
Type tvarfind
	ty As Integer
	pt As Integer
	nm As String    'var name or description when not a variable
	pr As Integer   'index of running var parent (if no parent same as ivr)
	ad As UInteger
	iv As Integer   'index of running var
	tv As HWND      'handle treeview
    tl As integer 'handle line
End Type

''show/expand
Const SHWEXPMAX=10 'max shwexp boxes
Const VRPMAX=5000  'max elements in each treeview
Type tshwexp
	bx As HWND     'handle pointed value box   todo check if gadget ?
	tv As HWND     'corresponding tree view
	nb As Integer  'number of elements tvrp
	cu As HWND     'handle of the current index label
	mn As HWND     'handle of the mini index label
	mx as HWND     'handle of the max indexlabel
	curidx As Integer  'current index only for array
	minidx As Integer  'min index
	maxidx As Integer  'max index
	
	procr  as integer 'index of running proc  (-1 if memory) used to delete the shw/exp when proc is exiting (local var)
	arradr as integer 'address of pointer in descriptor array (-1 if not a dynamic array)
	mem    as integer 'if static don't delete the shw/exp when the proc is closed
	parent as integer 'index of higher parent
End Type
Type tvrp
	nm As String    'name
	ty As Integer   'type
	pt As Integer   'is pointer
	ad As UInteger  'address
	tl As integer   'line in treeview
	iv As Integer   'index of variables 
End Type


''Backtracking
Type tbcktrk
	As Integer bcw
	As Integer frw
End Type

''index box
Const INDEXBOXMAX=9

''============================= Declares ==============================================
Declare Function win9AddNewGadget(ByVal gadget As Integer, ByVal hWin As HWND) As integer
Declare Function win9GetCurrent() As HWND
declare function source_name(fullname as string)as string
declare function dll_name(FileHandle As HANDLE,t As Integer =1 )As String
declare function var_find2(tv As HWND) As Integer
declare sub proc_del(j As Integer,t As Integer=1)
declare sub dsp_change(index As Integer)
declare sub size_change()