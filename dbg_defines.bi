''defines etc for fbdebugger_new
''dbg_define.bi
''

#Define fbdebuggerversion "V 2.98.2 32-64bit"

'#define fulldbg_prt 'uncomment to get more information
#Define dbg_prt2 Rem 'used temporary for debugging fbdebugger, change rem by dbg_prt 

#include once "Window9.bi"
#include once "Scintilla.bi"
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

''end of define for windows	
#endif

#Define TYPESTD 17 ''upper limit for standard type, now 17 for va_list 2020/02/05

''source code files
#Define SRCSIZEMAX 5000000 ''max source size
#Define STAB_SZ_MAX 60000  ''max stabs string
Const   SRCMAX=1000		   ''max source file
#define GSRCTAB 650        ''panel
#define GFILELIST 3000     ''file combo file
#define GFILESEL 3001	   ''button for selcting a file

#define GSCINTILLA 5000

''current line
#define GCURRENTLINE 600
#define GCURLINETTIP 601

''different styles windows/linux
#Ifdef __FB_WIN32__
	#define KTRRESTYLE TVS_HASLINES or TVS_HASBUTTONS or TVS_LINESATROOT
#Else
	#define KTRRESTYLE 0
	#define hicon HBITMAP
#endif

''right panels
#define GRIGHTTABS 200
#define GTVIEWVAR 300
#define GTVIEWPRC 301
#define GDUMPMEM 320



'buttons main screen
#define IDBUTSTEP   101
#define IDBUTSTEPP  102
#define IDBUTSTEPM  103
#define IDBUTAUTO   104
#define IDBUTRUN    105
#define IDBUTSTOP   106
#define IDBUTMINI   107
#define IDBUTFREE   108
#define IDBUTTOOL   109
#define IDBUTFILE   110
#define IDBUTRRUNE  111
#define IDBUTATTCH  112
#define IDBUTKILL   113
#define IDNOTES     114
#define IDLSTEXE    115
#define IDFASTRUN   116
#define IDEXEMOD    117
#define IDBUTSTEPB  118
#define IDBUTSTEPT  119
#define IDCONTHR 120 '''503 'used also with button
#define IDUPDATE  128

''NOT ANY MORE USED
'#define IDWATCH1  120
'#define IDWATCH2  121
'#define IDWATCH3  122
'#define IDWATCH4  123
'#define IDBRKVAR  124
'#define IDCURLIG  125
'#define IDBMKCMB  126
'#define IDDUMP    127


#define ENLRSRC   130
#define ENLRVAR   131
#define ENLRMEM   132



''for scintilla
#define KRED &hFF
#define KBLUE &hFF0000
#define KGREEN &hB700
#define KYELLOW &hFFFF
#define KORANGE &h04A0FB
#Ifdef __fb_win32__
	#define send_sci(b,c,d) sendmessage(scint,b,c,cast(integer,d))
#else
	#define send_sci(b,c,d) scintilla_send_message(cast(scintillaobject ptr,scint),b,c,cast(integer,d))
	extern "C"

	type ScintillaObject as _ScintillaObject
	type ScintillaObjectClass as _ScintillaClass

	type _ScintillaObject
		cont as GtkContainer
		pscin as any ptr
	end type

	type _ScintillaClass
		parent_class as GtkContainerClass
		command as sub(byval sci as ScintillaObject ptr, byval cmd as long, byval window as GtkWidget ptr)
		notify as sub(byval sci as ScintillaObject ptr, byval id as long, byval scn as SCNotification ptr)
	end type

	declare function scintilla_object_get_type() as GType
	declare function scintilla_object_new() as GtkWidget ptr
	declare function scintilla_object_send_message(byval sci as ScintillaObject ptr, byval iMessage as ulong, byval wParam as guintptr, byval lParam as gintptr) as gintptr
	declare function scnotification_get_type() as GType
	type ScintillaClass as _ScintillaClass
	declare function scintilla_get_type() as GType
	declare function scintilla_new() as GtkWidget ptr
	declare sub scintilla_set_id(byval sci as ScintillaObject ptr, byval id as uptr_t)
	declare function scintilla_send_message(byval sci as ScintillaObject ptr, byval iMessage as ulong, byval wParam as uptr_t, byval lParam as sptr_t) as sptr_t
	declare sub scintilla_release_resources()

	#define SCINTILLA(obj) G_TYPE_CHECK_INSTANCE_CAST(obj, scintilla_get_type(), ScintillaObject)
	#define SCINTILLA_CLASS(klass) G_TYPE_CHECK_CLASS_CAST(klass, scintilla_get_type(), ScintillaClass)
	#define IS_SCINTILLA(obj) G_TYPE_CHECK_INSTANCE_TYPE(obj, scintilla_get_type())
	#define SCINTILLA_TYPE_OBJECT scintilla_object_get_type()
	#define SCINTILLA_OBJECT(obj) G_TYPE_CHECK_INSTANCE_CAST((obj), SCINTILLA_TYPE_OBJECT, ScintillaObject)
	#define SCINTILLA_IS_OBJECT(obj) G_TYPE_CHECK_INSTANCE_TYPE((obj), SCINTILLA_TYPE_OBJECT)
	#define SCINTILLA_OBJECT_CLASS(klass) G_TYPE_CHECK_CLASS_CAST((klass), SCINTILLA_TYPE_OBJECT, ScintillaObjectClass)
	#define SCINTILLA_IS_OBJECT_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE((klass), SCINTILLA_TYPE_OBJECT)
	#define SCINTILLA_OBJECT_GET_CLASS(obj) G_TYPE_INSTANCE_GET_CLASS((obj), SCINTILLA_TYPE_OBJECT, ScintillaObjectClass)
	#define SCINTILLA_TYPE_NOTIFICATION scnotification_get_type()
	#define SCINTILLA_NOTIFY "sci-notify"

end extern

#endif
#define KSTYLBREAK
#define KSTYLBREAKTEMPO
#define KSTYLBREAKCOUNT
#define KSTYLBREAKDISABLED

#define KSTYLENONE 0
#define KSTYLECUR  2
'================ Lines ==============================================
Const LINEMAX=100000
Type tline
	ad As uinteger ''offset relative to proc address
	nu As integer  ''number in file
	sv As byte     ''saved value replaced by &hCC
	px As UShort   ''proc index
	sx As UShort   ''source index 2018/08/02 need it now for lines from include and not inside proc
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
	ed As UInteger 'upper proc end 18/08/2015
	sr As UShort   'source index
	nu As Long     'line number to quick access
	lastline As Long 'last line of proc (use when dwarf data) ''2016/03/24
	vr As UInteger 'lower index variable upper (next proc) -1
	rv As Integer  'return value type
	pt As Long     'counter pointer for return value (** -> 2)
	rvadr As Integer 'offset for return value adr (for now only dwarf) 19/08/2015
    'tv As HTREEITEM 'in tview2 todo changed for linux
    st As Byte     'state followed = not checked
End Type

Const PROCRMAX=50000 'Running proc
Type tprocr
	sk   As UInteger  'stack
	idx  As UInteger  'index for proc
	'tv   As HTREEITEM 'index for treeview todo changed for linux
	'lst as uinteger 'future array in LIST
	cl   As Integer   'calling line
	thid As Integer   'idx thread
	vr   As Integer   'lower index running variable upper (next proc) -1
End type



''Declares
Declare Function win9AddNewGadget(ByVal gadget As Integer, ByVal hWin As HWND) As integer
Declare Function win9GetCurrent() As HWND
