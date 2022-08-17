''defines etc for fbdebugger_new
''dbg_define.bi

''table of enums

''menu : 1000+

'define data 64bit/32bit
#Ifdef __FB_64BIT__
	#Define regip rip
	#Define regbp rbp
	#Define regsp rsp
	#define ver3264 "(64bit) "
#Else
	#Define regip eip
	#Define regbp ebp
	#Define regsp esp
	#define ver3264 "(32bit) "
#endif

#Define fbdebuggerversion "V 3.00 BETA e "+ver3264

'#define fulldbg_prt 'uncomment to get more information
#Define dbg_prt2 rem 'dbg_prt 'used temporary for debugging fbdebugger, change rem by dbg_prt

 'take l char form a string and complete with spaces if needed
#Define fmt(t,l) Left(t,l)+Space(l-Len(t))+"  "
#Define fmt2(t,l) Left(t,l)+Space(l-Len(t))
#Define fmt3(t,l) Space(l-Len(t))+Left(t,l)

#define disable_webgadget ''to avoid issue with libwebkitgtk1.0.so old version not always installed
#include once "window9.bi"
#include once "scintilla.bi"
#include once "SciLexer.bi"
#Include Once "file.bi"

#ifdef __FB_WIN32 ''sometime need of double & otherwise underlined next character
	#define KAMPERSAND "&&"
#else
	#define KAMPERSAND "&"
#EndIf



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

	#define HCOMBO 500

	''on Windows the function can return zero even an item is selected so replaced by the API
	#undef getitemtreeview
	#define getitemtreeview(treeid) SendMessage(gadgetid(treeid),TVM_GETNEXTITEM,TVGN_CARET,0)


''end of define for windows

''==========================================================
#else ''======================== LINUX =====================
''==========================================================

	enum
		KPT_EXIT    ''exiting loop in br_stop
		KPT_CONT=1
		KPT_CONTALL ''continue all stopped threads
		KPT_GETREGS
		KPT_SETREGS
		KPT_CC      ''for restoring breakpoint instruction
		KPT_CCALL    ''for restoring/removing all breakpoints on fbc instruction
		KPT_WRITEMEM
		KPT_READMEM
		KPT_RESTORE ''for restoring the saved byte
		KPT_RESTALL ''for restoring all the saved bytes (halting)
		KPT_KILL    ''quit thread2
		KPT_SSTEP   ''executing single_step
		KPT_XIP     ''for updating xip
		KPT_SIGNAL  ''pending signal
	End Enum

	enum
		KDONOTHING=-1
		KRESTART
		KRESTART9=KRESTART+9
		KENDALL
		KLOADEXE
		KCRASHED
	End Enum

	#ifdef __fb_64bit__
		#define FIRSTBYTE &hFFFFFFFFFFFFFF00
	#else
		#define FIRSTBYTE &hFFFFFF00
	#EndIf

	enum PTRACE_REQUEST
		PTRACE_TRACEME             =0
		PTRACE_PEEKTEXT            '1
		PTRACE_PEEKDATA            '2
		PTRACE_PEEKUSR             '3
		PTRACE_POKETEXT            '4
		PTRACE_POKEDATA            '5
		PTRACE_POKEUSR             '6
		PTRACE_CONT                '7
		PTRACE_KILL                '8
		PTRACE_SINGLESTEP          '9
		PTRACE_GETREGS         =   12
		PTRACE_SETREGS            '13
		PTRACE_GETFPREGS          '14 'Get all floating point registers used by a processes.
		PTRACE_SETFPREGS          '15 'Set all floating point registers used by a processes.
		PTRACE_ATTACH             '16
		PTRACE_DETACH             '17
		PTRACE_GETFPXREGS         '18 'Get all extended floating point registers used by a processes.
		PTRACE_SETFPXREGS         '19 'Set all extended floating point registers used by a processes.
		PTRACE_SYSCALL    =        24 'Continue and stop at the next (return from) syscall
		PTRACE_SETOPTIONS  =   &h4200
		PTRACE_GETEVENTMSG = &h4201 'Get last ptrace message
		PTRACE_GETSIGINFO = &h4202 'Get siginfo for process
		PTRACE_SETSIGINFO = &h4203 'Set new siginfo for process
		PTRACE_GETREGSET = &h4204 'Get register content
		PTRACE_SETREGSET = &h4205 'Set register content
		PTRACE_SEIZE = &h4206 'Like PTRACE_ATTACH, but do not force tracee to trap and do not affect signal or group stop state
		PTRACE_INTERRUPT = &h4207 'Trap seized tracee
		PTRACE_LISTEN = &h4208 'Wait for next group event
		PTRACE_PEEKSIGINFO = &h4209 'Retrieve siginfo_t structures without removing signals from a queue
		PTRACE_GETSIGMASK = &h420a 'Get the mask of blocked signals
		PTRACE_SETSIGMASK = &h420b 'Change the mask of blocked signals
		PTRACE_SECCOMP_GET_FILTER = &h420c 'Get seccomp BPF filters
		PTRACE_SECCOMP_GET_METADATA = &h420d 'Get seccomp BPF filter metadata
	end enum

	'' Output information
	#define dbg_prt(txt) output_lnx(txt)
	#define dbghand pid

	#define HCOMBO 30
	#define LPCVOID integer ptr
	#define LPVOID integer ptr
	declare sub output_lnx(as string)
	declare function ReadProcessMemory(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr =0) as integer
	declare function ReadProcessMemory_th2(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr=0) as integer
	declare function readmemlongint(child As long,addrdbge As integer)as longint
	declare function writeprocessmemory(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr =0) as integer
	declare sub thread_rsm()
	declare Function OpenFileRequesterExe(sTitle As String , sCurDir As String, sPattern As String = "EXE files", iFlag As Long = 0, sTemplateFileName As String = "",  hParentWin As HWND = 0) As String
	declare function elf_extract(as string)as INTEGER
	declare function pthread_kill alias "pthread_kill"(as long,as long) as long
	Declare function linux_kill alias "kill"(as long,as long) as long
	declare sub sigusr_send()
	declare function signal_pending() as integer
	declare sub exec_order(order as integer)
	Extern "C"
		Declare Function wait_ Alias "wait" (wiStatus As long Ptr) As pid_t
		'Declare Function gettid Alias "gettid" () As pid_t
		Declare Function ptrace(request As ptrace_request, pid As  pid_t, addr As Any Ptr, uData As Any Ptr) As integer
		'declare function pthread_mutex_trylock(mutex as ANY	ptr) as long
	End Extern

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

	type pt_regs 'or user_regs_struct
	#ifndef __FB_64BIT__
		as long ebx
		as long ecx
		as long edx
		as long esi
		as long edi
		as long xbp
		as long eax
		as long ds', __dsu
		as long es', __esu
		as long fs', __fsu
		as long gs', __gsu
		as long orig_eax
		as ulong xip
		as long  cs', __csu
		as long eflags
		as long xsp
		as long ss', __ssu
	#else
	   As Uinteger r15
	   As Uinteger r14
	   As Uinteger r13
	   As Uinteger r12
	   As Uinteger xbp
	   As Uinteger rbx
	   As Uinteger r11
	   As Uinteger r10
	   As Uinteger r9
	   As Uinteger r8
	   As Uinteger rax
	   As Uinteger rcx
	   As Uinteger rdx
	   As Uinteger rsi
	   As Uinteger rdi
	   As Uinteger orig_rax
	   As Uinteger xip
	   As Uinteger cs
	   As Uinteger eflags
	   As Uinteger xsp
	   As Uinteger ss
	   As Uinteger fs_base
	   As Uinteger gs_base
	   As Uinteger ds
	   As Uinteger es
	   As Uinteger fs
	   As Uinteger gs
	end type
	#endif
	#define	EPERM		 1
	#define	ENOENT		 2
	#define	ESRCH		 3
	#define	EINTR		 4
	#define	EIO		 	 5
	#define	ENXIO		 6
	#define	E2BIG		 7
	#define	ENOEXEC		 8
	#define	EBADF		 9
	#define	ECHILD		10
	#define	EAGAIN		11
	#define	ENOMEM		12
	#define	EACCES		13
	#define	EFAULT		14
	#define	ENOTBLK		15
	#define	EBUSY		16
	#define	EEXIST		17
	#define	EXDEV		18
	#define	ENODEV		19
	#define	ENOTDIR		20
	#define	EISDIR		21
	#define	EINVAL		22
	#define	ENFILE		23
	#define	EMFILE		24
	#define	ENOTTY		25
	#define	ETXTBSY		26
	#define	EFBIG		27
	#define	ENOSPC		28
	#define	ESPIPE		29
	#define	EROFS		30
	#define	EMLINK		31
	#define	EPIPE		32
	#define	EDOM		33
	#define	ERANGE		34
#endif
''====================== end for linux =========================
type tlist
	as integer parent
	as integer child
	as zstring ptr nm
	'as INTEGER idx
end type

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
	GSRCCURRENT=500 ''current source
	GFILELIST       ''file combo

	GSCINTILLA

	''current line
	GCURRENTLINE
	GCURLINETTIP
	''button to show variable
	GBUTSHOWVAR
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

''log
#define KLOGOFF  0  ''no log
#define KLOGON   1  ''log displayed only on demand
#define KLOGCONT 2  ''log always updated and continously displayed
#define log_write(text) vlog+=text+chr(10)
#define GLOG 330

            'Setselecttexteditorgadget(1,-1,-1) ' cursor in the end position
            'Pasteeditor(1,"paste")

''EDITOR for displaying miscellanous thing
#define GEDITOR 331

#define GSTATUSBAR 332
#define KSTBSTS 0
#define KSTBTHD 1
#define KSTBUID 2
#define KSTBSRC 3
#define KSTBPRC 4
#define KSTBBPM 5
#define KSTBFRT 6

'button main screen
enum
	IDBUTSTEP=600
	IDBUTSTEPOVER
	IDBUTAUTO
	IDBUTSTOP
	IDBUTCURSOR
	IDBUTRUNEND
	IDBUTRUNEXIT
	IDBUTKILL
	IDBUTEXECMOD
	IDBUTCRASH
	IDBUTFREE

	IDBUTTOOL
	IDBUTFILE
	IDBUTRERUN
	TTRERUN ''tooltip for button rerun
	IDBUTATTCH
	IDBUTLASTEXE
	IDBUTUPDATE

	IDBUTBRKP
	IDBUTBRKC
	IDBUTBRKT
	IDBUTBRKN
	IDBUTBRKD
	IDBUTBRKB
end enum

'' source menu
enum
	MNTHRDAUT =1000 'automatic execution  alternating threads
	MNSETBRKP
	MNSETBRKC
	MNSETBRKT
	MNSETBRKN
	MNRSTBRKN
	MNCHGBRKN
	MNSETBRKD
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
end enum

''proc/var menu
enum
	MNVARDMP=1050 '' memory variable dump
	MNVAREDT
	MNBRKVC
	MNBRKV1
	MNBRKV2
	MNBRKVS
	MNBRCV1
	MNBRCV2
	MNSELIDX
	MNSHSTRG
	MNSHWEXP
	MNSETWTCH
	MNSETWTTR
	MNCHGZSTR
	MNCALLINE
	MNSHCHAR  'show  character  in a  string at a  selected position
	MNCLBVARA 'copy  to clipboard  all procs/vars
	MNCLBVARS 'copy  to clipboard  selectedvar
	MNLSTVARA 'copy  to log  all procs/vars
	MNLSTVARS 'copy  to log selectedvar
	MNPTDUMP  'dump  deferenced data
	MNFNDVAR  'find  proc or var in proc/var
	MNVARCOLI 'collapse item
	MNVAREXPI 'expand item
	MNVARCOLA 'collapse all
	MNVAREXPA 'expand all
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
	MNENBPRC=1150  ''enable/disable
	MNSORTPRC ''toggle sortby module name/  procname
	MNASMPRC  ''listing of asm code of a  proc
end enum

'' thread menu
enum
	MNTHRDCHG=1200 ''select thread
	MNLOCPRC  ''locate proc
	MNTHRDKLL ''kill  thread
	MNTHRDBLK ''block thread
	MNCREATHR ''show  line creating thread
	MNTHRDLST ''list  threads
	MNSHWPROC ''show  proc in proc/var
	MNSHPRSRC ''show  proc in source
	MNTCHNING ''call chain
	MNTHRDEXP ''expand one thread
	MNTHRDCOL ''collapse  all threads
	MNPRCRADR ''addr  about  running procstart,end  stack
end enum

'' tools menu
enum
	MNABOUT=1250
	MNSETTINGS
	MNCMPINF
	MNDBGHELP
	MNSHOWLOG
	MNRESETLOG
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
#define KBLUE   &hEBA200
#define KGREEN  &hB700
#define KYELLOW &hFFFF
#define KORANGE &h04A0FB
#define KPURPLE &hEB80EB
#define KGREY   &hC3C3C3
#define KWHITE  &hFFFFFF

#define KSHOW 0
#define KHIDE 1

''for breakpoint management
enum
	GBRKDEL01=800
	GBRKDEL10=GBRKDEL01+9
	GBRKDSB01
	GBRKDSB10=GBRKDSB01+9
	GBRKRST01
	GBRKRST10=GBRKRST01+9
	GBRKCHG01
	GBRKCHG10=GBRKCHG01+9
	GBRKLINE01
	GBRKLINE10=GBRKLINE01+9
	GBRKIMG01
	GBRKIMG10=GBRKIMG01+9
	GBRKCLOSE
	GBRKDELALL
	GBRKDISABLE
	GBRKENABLE

	GBRCVAR1
	GBRCVALUE
	GBRCOK
	GBRCDEL
	GBRCCOND
	GTVIEWBRC ''869
End Enum

''index selection
enum
	GIDXVAR=870
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

type tindexdata
		indexvar as INTEGER
		sizeline as INTEGER
		size     as INTEGER
		nbdim 	 as INTEGER
		curidx(4)  as INTEGER
		vlbound(4) as integer
		vubound(4) as integer
		adr as  INTEGER
		typ as  INTEGEr ''type
		typ2 as INTEGEr
		delta2  as INTEGEr
		autoupd as boolean ''auto update the table
		typvar  as boolean ''var or cudt
End Type

''for settings
enum
	LOGGROUP=910
	GLOGOFF
	GLOGON
	GLOGCONT
	GTRACEPROC
	GTRACELINE
	GASCII
	GVERBOSE
	GTEXTDELAY
	GAUTODELAY
	GTEXTCMDLP
	GCMDLPARAM
	GCMDLKEEP

	FONTGROUP
	GTEXTFTYPE
	GTEXTFSIZE
	GTEXTFCOLOR

	BUTGROUP
	SETBUTSTEP
	SETBUTSTEPOVER
	SETBUTAUTO
	SETBUTSTOP
	SETBUTCURSOR
	SETBUTRUNEND
	SETBUTRUNEXIT
	SETBUTKILL
	SETBUTEXECMOD
	SETBUTCRASH
	SETBUTFREE

end enum

''for dump memory
enum
	GDUMPAPPLY=940
	GDUMPADR
	GDUMPEDIT

	GDUMPTSIZE
	GDUMPSIZE
	GDUMPCLIP
	GDUMPBASEADR
	GDUMPDECHEX
	GDUMPSIGNED

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

	GDUMPTYPE
	GDUMPCTRL
end enum

''shw/exp
enum
	GSHWWCH=970
	GSHWDMP
	GSHWEDT
	GSHWSTR
	GSHWNEW
	GSHWCUR
	GSHWMIN
	GSHWMAX
	GSHWSET
	GSHWINC
	GSHWDEC
	GSHWUPD
	GTVIEWSHW
end enum

''break on var/mem or cond
'#define KBRCMEMCONST 1 ''cond  var/const
#define KBRCMEMMEM   1 ''cond  var/var
#define KBRKMEMCONST 2 ''mem   var-mem/const
#define KBRKMEMMEM   3 ''mem   var-mem/mem

enum
	GBRKVAR1=990
	GBRKVAR2
	GBRKVALUE
	GBRKVOK
	GBRKVDEL
	GBRKVCOND
end enum

''miscellaneous
enum
''procedure tracking
	GCCHAIN=1000
'' timer
	GTIMER001
end enum

'' debug events
enum
 KDBGNOTHING = 0
 KDBGRKPOINT
 KDBGCREATEPROCESS
 KDBGCREATETHREAD
 KDBGEXITPROCESS
 KDBGEXITTHREAD
 KDBGDLL
 KDBGDLLUNLOAD
 KDBGEXCEPT
 'KDBGSTRING
end enum

''editing value variable/memory
#define KEDITVAR  0
#define KEDITDMP  1
#define KEDITARR  2
#define KEDITWCH  3
#define KEDITPTD  4
#define KEDITSHW  5
#define KEDITTOP  6

enum
	GEDTVAR=1100
	GEDTVALUE
	GEDTOK
	GEDTCANCEL
	GEDTPTD
	GEDTPTDEDT
	GEDTPTDVAL
end enum

type tedit
	adr as INTEGER
	typ as INTEGER
	pt  as INTEGER
	ptdadr	as integer
	ptdval as STRING
	src as INTEGER
End Type

#Ifdef __fb_win32__
	#define send_sci(b,c,d) sendmessage(hscint,b,c,cast(integer,d))
#else
	#define send_sci(b,c,d) scintilla_send_message(cast(scintillaobject ptr,hscint),b,c,cast(integer,d))
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
#define KSTYLECUR  150




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

enum ''thread status
	KTHD_RUN '0
	KTHD_STOP
	KTHD_BLKD
	KTHD_INIT
	KTHD_OUT ''out of scope  ?? used
End Enum

 ''for ccstate
enum KCC_STATE
	KCC_NONE
	KCC_ALL
End Enum

Enum ''type of running
	RTRUN
	RTSTEP
	RTAUTO
	RTOFF
	RTFREE
	RTEND
	RTCRASH
End enum

enum ''stop code
	CSSTEP=0
	CSLINE
	CSBRKPT
	CSCOND
	CSVAR
	CSMEM
	CSCOUNT
	CSUSER
	CSACCVIOL
	CSNEWTHRD
	CSEXCEP
	CSTHREADS
End Enum

''for dissassembly
#define KLINE 1 ''from source code
#define KPROC 2 ''from source code
#define KSPROC 3 ''from proc/var

Union pointeurs
	pxxx As Any Ptr
	#Ifdef __FB_64BIT__
	   pinteger As Long Ptr
	   puinteger As ULong Ptr
	#Else
	   pinteger As Integer Ptr
	   puinteger As UInteger Ptr
	#EndIf
	'pinteger As Integer Ptr
	'puinteger As UInteger Ptr
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
	sv As ubyte     ''saved value replaced by breakcpu
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
	nm As String   ''name
	db As Integer ''beginning address
	first as integer ''first address corresponding to a fbc line
	fn As Integer ''last address of fbc line
	ed As Integer ''last address +1 (begin of next proc)
	sr As UShort   'source index
	nu As Long     'line number to quick access
	vr As UInteger 'lower index variable upper (next proc) -1
	rv As Integer  'return value type
	pt As Long     'counter pointer for return value (** -> 2)
	rvadr As Integer 'offset for return value adr (for now only dwarf)
    tv As integer 'in tview2
    enab As boolean 'state enable/disable
End Type

Const PROCRMAX=50000 'Running proc
Type tprocr
	sk   As Integer  'stack
	ret  as integer  'return address
	idx  As Integer  'index for proc
	tv   As integer  'index for treeview
	'lst as uinteger 'future array in LIST
	cl   As Integer  'calling line
	thid As Integer  'idx thread
	vr   As Integer  'lower index running variable upper (next proc) -1
End type
''======================== Arrays =========================================
Const ARRMAX=1500
#define KMAXDIM 5
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
end Type
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
Type tbrkol
	isrc    As UShort  'source index
	nline   As Integer 'num line for display
	index   As Integer 'index for rline
	ad      As Integer 'address
	typ     As Byte	   'type
	ivar1   as integer
	union
		adrvar1 as INTEGER
		counter As UInteger 'counter to control the number of times the line should be executed before stopping
	end union
	ivar2   as integer
	union
		adrvar2 as INTEGER
		cntrsav As UInteger 'to reset if needed the initial value of the counter
	end union
	datatype as byte
	Val As valeurs   ''constant value for BP cond
	ttb as byte
End type

''========================= Breakpoint on variable ===================================
Type tbrkv
	typ As Integer   'type of variable
	adr1 As Integer  'address
	adr2 As Integer  'address
	arr As Integer  'adr if dyn array
	ivr1 As Integer   'variable index
	ivr2 As Integer   'variable index
	psk As Integer   'stack proc
	Val As valeurs   'value
	vst As String    'value as string
	ttb As Byte      'type of comparison (32 shr tst)
	txt As String	 'name and value for menu
End type

type tbrclist ''list of item handle for cond BP
	items as INTEGER
	itemc as INTEGER
End Type
''======================== Threads ====================================
Const THREADMAX=50
Type tthread
#ifdef __fb_win32__
	hd  As HANDLE    'handle
	id  As UInteger  'ident
 #else
	hd as INTEGER  'not use
	id as long
#endif
 pe  As Integer   'flag if true indicates proc end
 sv  As Integer   'sav line
 od  As Integer   'previous line
 nk  As UInteger  'for naked proc, stack and used as flag
 st  As Integer   'to keep starting line
 tv  As integer 'to keep handle of thread item
 plt As integer 'to keep handle of last proc of thread in proc/var tview
 ptv As integer 'to keep handle of last proc of thread in thread tview
 exc As Integer   'to indicate execution in case of auto 1=yes, 0=no
 sts as integer ''status running /stopped /init /out of scop debugger (library)
 stack as integer ''stack of last proc
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
''Const SHWEXPMAX=10 'max shwexp boxes
Const VRPMAX=5000  'max elements in each treeview
Type tshwexp
	'bx As HWND     'handle pointed value box
	'tv As HWND     'corresponding tree view
	nb As Integer  'number of elements tvrp
	'cu As HWND     'handle of the current index label
	'mn As HWND     'handle of the mini index label
	'mx as HWND     'handle of the max indexlabel
	curidx As Integer  'current index only for array
	minidx As Integer  'min index
	maxidx As Integer  'max index

	procr  as integer 'index of running proc  (-1 if memory) used to delete the shw/exp when proc is exiting (local var)
	arradr as integer 'address of pointer in descriptor array (-1 if not a dynamic array)
	mem    as integer 'if static don't delete the shw/exp when the proc is closed
	parent as integer 'index of higher parent
	free   as boolean 'shwexp in use or not
End Type
Type tvrp
	nm As String    'name
	ty As Integer   'type
	pt As Integer   'is pointer
	ad As UInteger  'address
	tl As integer   'line in treeview
	iv As Integer   'index of variables
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
declare sub hard_closing(errormsg as string)
declare function wait_debug() As Integer
declare sub dump_sh()
declare sub var_sh()
declare sub index_update()
declare function var_find() as INTEGER
declare function var_sh2(t As Integer,pany As UInteger,p As UByte=0,sOffset As String="") As String
declare sub shwexp_init()
declare sub edit_fill(txt as string,adr as integer,typ as integer, pt as integer, src as integer)
declare function debug_extract(exebase As UInteger,nfile As String,dllflag As Long=NODLL) as integer
declare sub button_action(button as integer)
declare sub ini_write()
declare sub singlestep_on(tid as integer,rln as integer,running as integer =1)
declare sub brk_del(n as integer)
declare sub brkv_set(a As Integer)
declare sub brk_apply()
declare sub brk_sav()
declare sub process_list()
declare sub gest_brk(ad As Integer,byval rln as integer =-1)
declare sub list_all()
declare sub restart_exe(byval idx as integer)
declare sub dll_load()
declare sub winmsg()
declare sub start_pgm(p As Any Ptr)
declare sub resume_exec()
declare function proc_find(thid As Integer,t As Byte) As Integer
declare function proc_name(ad As UInteger) As String
declare sub proc_sh()
declare function proc_verif(p as integer) As Boolean
declare function proc_retval(prcnb As Integer) As String
declare sub proc_watch(procridx As Integer)
declare sub brk_manage(title as string)
declare sub var_tip()
