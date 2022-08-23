''Linux system for fbdebugger_new
''dbg_linux.bi
''==================TO BE MOVED IN DBG_DEFINES============================
'======================== for LINUX =========================
#INCLUDE "crt.bi"
#INCLUDE "crt/linux/unistd.bi"
#INCLUDE "crt/linux/fcntl.bi"

#define SIGINT 2
#define SIGILL 4
#define SIGTRAP 5
#define SIGABRT 6
#define SIGKILL 9
#define SIGSEGV 11
#define SIGUSR1 10
#define SIGUSR2 12
#define SIGTERM 15
#define SIGCHLD 17
#define SIGCONT 18
#define SIGSTOP 19
'SIGHUP 1
'SIGINT	     2	Term	Interruption depuis le clavier.
'SIGQUIT	 3	Core	Demande « Quitter » depuis le clavier.
'SIGILL	     4	Core	Instruction illégale.
'SIGABRT	 6	Core	Signal d'arrêt depuis abort(3).
'SIGFPE	     8	Core	Erreur mathématique virgule flottante.
'SIGKILL	 9	Term	Signal « KILL ».
'SIGSEGV	11	Core	Référence mémoire invalide.
'SIGPIPE	13	Term	Écriture dans un tube sans lecteur.
'SIGALRM	14	Term	Temporisation alarm(2) écoulée.
'SIGTERM	15	Term	Signal de fin.
'Signal	Code	Reason
'SIGILL	ILL_ILLOPC	Illegal opcode
 	'ILL_ILLOPN	Illegal operand (not currently used)
 	'ILL_ILLADR	Illegal addressing mode (not currently used)
 	'ILL_ILLTRP	Illegal trap (not currently used)
 	'ILL_PRVOPC	Privileged opcode; instruction requires privileged CPU mode
 	'ILL_PRVREG	Privileged register (not currently used)
 	'ILL_COPROC	Coprocessor instruction error
 	'ILL_BADSTK	Internal stack error
	'
'SIGSEGV	SEGV_MAPERR	Address isn't mapped to an object
 	'SEGV_ACCERR	The mapping doesn't allow the attempted access
	'
'SIGTRAP	TRAP_BRKPT	Process breakpoint trap
 	'TRAP_TRACE	Process trace trap
	'
'SIGCHLD
	'CLD_EXITED	   1 Child has exited
 	'CLD_KILLED	   2 Child has terminated abnormally and did not create a core file
 	'CLD_DUMPED	   3 Child has terminated abnormally and created a core file
 	'CLD_TRAPPED   4 Traced child has trapped
 	'CLD_STOPPED   5 Child has stopped
 	'CLD_CONTINUED 6 Stopped child has continued
	'
'Signal	Member	Value
'SIGILL, SIGFPE	void *si_addr	The address of the faulting instruction
'SIGSEGV, SIGBUS	void *si_addr	The address of the faulting memory reference
'SIGCHLD	pid_t si_pid	The child process ID
 	'int si_status	The exit value or signal
 	'uid_t si_uid	The real user ID of the process that sent the signal

 #define __WNOHANG		&h00000001
'#define WUNTRACED	0x00000002
'#define WSTOPPED	WUNTRACED
'#define WEXITED		0x00000004
'#define WCONTINUED	0x00000008
'#define WNOWAIT		0x01000000	'/* Don't reap, just poll status.  */

'#define __WNOTHREAD	0x20000000	'/* Don't wait on children of other threads in this group */
#define __WALL		&h40000000	'/* Wait on all children, regardless of type */
'#define __WCLONE	0x80000000	'/* Wait only on non-SIGCHLD children */

#define SIG_BLOCK 0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2

''todo checking
''TKILL 64 bit = 200 32bit = 238
''TGKILL         234         270
''GETTID		 186         224

#ifdef __fb_64bit__
	#define SYS_TKILL   238 '?? 200
	#define SYS_TGKILL  234'270
	#define SYS_GETTID  186'224
	#define SYS_SIGPEND 127
#else
	#define SYS_TKILL  200
	#define SYS_TGKILL 234
	#define SYS_GETTID 186
#endif

#define SA_SIGINFO	4
#define   PTRACE_O_TRACESYSGOOD	  &h00000001
#define   PTRACE_O_TRACEFORK	  &h00000002
#define   PTRACE_O_TRACEVFORK     &h00000004
#define   PTRACE_O_TRACECLONE	  &h00000008
#define   PTRACE_O_TRACEEXEC	  &h00000010
#define   PTRACE_O_TRACEVFORKDONE &h00000020
#define   PTRACE_O_TRACEEXIT	  &h00000040
#define   PTRACE_O_TRACESECCOMP   &h00000080
#define   PTRACE_O_EXITKILL	      &h00100000
#define   PTRACE_O_MASK		      &h001000ff

#define   PTRACE_EVENT_CLONE      &h3
type tsiginfo
	as long si_signo, si_code, si_errno
	as any ptr si_addr
	'as short si_addr_lsb
	as long dummy(50)
End Type

'Type tsiginfo
  'si_signo As long
  'si_errno As long
  'si_code As long
  'Union
  'as byte pad(112)
   '/' kill() '/
   'si_pid As long		/' sender s pid '/
   'si_uid As long		/' sender s uid '/
   '/' SIGCHLD '/
   '_pid As long		/' which child '/
   '_uid As long		/' sender s uid '/
   '_status As long		/' exit code '/
   '_utime As Integer
   '_stime As Integer
'
'
   '/' SIGILL, SIGFPE, SIGSEGV, SIGBUS SIGTRAP'/
   'Type
   '_addr As Integer /' faulting insn/memory ref. '/
   'End Type
       ''_pad(29) As long '32bit -->29 / 64bit --> 28
  'End Union
  '
'End Type
'===============
'type siginfo
'
	'as long si_signo, si_code, si_errno
	'union 'si_fields
		'as byte pd(112) ''116 if 32bit
		'union 'first
			'type _piduid
				'as long si_pid,si_uid
			'end type
			'type '_timer
				'as long si_timerid,si_overrun
			'end type
		'end union
		'
		'union 'second
			'union sigval
				'as long sival_int
				'as any ptr sival_ptr
			'end union
			'type
				'as long si_status
				'as longint si_utime,si_stime
			'end type
		'end union
		'
		'type _sigfault
			'as any ptr si_addr
			'as short si_addr_lsb
			'union first
				'type addr_bnd
					'as any ptr si_lower
					'as any ptr si_upper
				'end type
				'as long si_pkey
			'end union
		'end type
		'
		'type sigpoll
			'as clong si_band
			'as long si_fd
		'end type
		'
		'type sigsys
			'as any ptr si_call_addr
			'as long si_syscall
			'as long si_arch
		'end type
	'end union
	'dummy(20) as integer
'end type
''=====

'union sigval {
	'int sival_int;
	'void *sival_ptr;
'};
'typedef struct {
'
	'union {
		'char __pad[128 - 2*sizeof(int) - sizeof(long)]; 128 - 8 -8
			'
		'struct {
			'void *si_addr;
			'short si_addr_lsb;
			'union {
				'struct {
					'void *si_lower;
					'void *si_upper;
				'} __addr_bnd;
				'unsigned si_pkey;
			'} __first;
		'} __sigfault;
		'struct {
			'long si_band;
			'int si_fd;
		'} __sigpoll;
		'struct {
			'void *si_call_addr;
			'int si_syscall;
			'unsigned si_arch;
		'} __sigsys;
	'} __si_fields;
'} siginfo_t;


'===============
type sigset
	as long dum(32)
End Type

Type tsigaction
   Union
   sa_handler As Sub (As long)
   sa_sigaction As Sub (As long,As tsiginfo Ptr,As Any Ptr)
   End Union
   sa_mask As sigset'128 bytes long
   sa_flags As long
   sa_restorer As Sub()
End Type
'print sizeof(sigaction)
'declare function getpid alias "getpid"() as Integer
'declare sub execve alias "execve"(as zstring ptr,as zstring ptr,as zstring ptr)
#undef Wait
'#undef Kill
'declare function fork alias "fork"() as Integer
declare function wait alias "wait"(as any ptr) as long
declare function waitpid alias "waitpid"(as long,as any ptr,as long) as long
'declare function pause alias "pause"() as Integer

declare function tkill alias "tkill"(as integer,as integer) as Integer
'declare function syscall cdecl  alias "syscall"(as integer,...) as Integer
declare function signal alias "signal"(as integer,as Integer ptr) as Integer

'declare function perror alias "perror"(as integer)as zstring Ptr
declare sub putbreaks
'declare function sigpending alias "sigpending" (as any ptr) as Integer
declare function sigprocmask alias "sigprocmask"(as integer,as any ptr,as any ptr) as Integer
Declare function sigaction Alias "sigaction"(As long, As tsigaction Ptr, As tsigaction Ptr) as integer
declare function sigemptyset   alias "sigemptyset"(As sigset ptr) as long
declare function sigpending  alias "sigpending"(As sigset ptr) as long
declare sub putremove_breaks(byval pid as long,byval typ as integer=1)

private sub sig_handler(signum as long,siginfo as tsiginfo ptr,dummy as any ptr)
	dim as INTEGER dta,restall
	print "stop running debuggee"
	linux_kill(thread(threadcur).id,SIGSTOP)
	sleep 500 'waiting to be sure debuggee is stopped
	print "restoring saved byte on every line=",thread(threadcur).id
	putremove_breaks(thread(threadcur).id)
End Sub

private sub sig_init()
	dim as tsigaction act
	sigemptyset(@act.sa_mask)
	act.sa_flags=SA_SIGINFO
	act.sa_sigaction=@sig_handler()
	print "sigaction USR";sigaction(SIGUSR1,@act,null)
	print "errno=";errno
End Sub

Const ORIG_RAX =15
Const RAX   =10
Const RBX   =5
Const RCX   =11
Const RDX   =12
Const RSI   =13
Const RDI   =14
Const SYS_write =1
/'
siginfo_t {
    int      si_signo;    /* NumÃ©ro de signal*/
    int      si_errno;    /* NumÃ©ro d'erreur */
    int      si_code;     /* Code du signal  */
    int      si_trapno;   /* NumÃ©ro de trappe qui a causÃ©
       le signal gÃ©nÃ©rÃ© par le
       matÃ©riel (pas utilisÃ© sur la
       plupart des architectures) */
    pid_t    si_pid;      /* PID de l'Ã©metteur          */
    uid_t    si_uid;      /* UID rÃ©el de l'Ã©metteur     */
    int      si_status;   /* Valeur de sortie ou signal */
    clock_t  si_utime;    /* Temps utilisateur Ã©coulÃ©   */
    clock_t  si_stime;    /* Temps systÃ¨me Ã©coulÃ©       */
    sigval_t si_value;    /* Valeur de signal*/
    int      si_int;      /* Signal POSIX.1b */
    void    *si_ptr;      /* Signal POSIX.1b */
    int      si_overrun;  /* DÃ©compte de dÃ©passement des
       horloges (POSIX.1b)        */
    int      si_timerid;  /* ID d'horloge (POSIX.1b)    */
    void    *si_addr;     /* Emplacement mÃ©moire ayant
       causÃ© l'erreur  */
    long     si_band;     /* Band event (Ã©tait int dans
       glibc 2.3.2 et antÃ©rieures */
    int      si_fd;       /* Descripteur de fichier     */
    short    si_addr_lsb; /* Bit le moins significatif de l'adresse
       (depuis Linux 2.6.32)   */
=============================
 wait() et waitpid()
       L'appel  systÃ¨me  wait()  suspend l'exÃ©cution du processus appelant jusqu'Ã  ce que l'un de
       ses enfants se termine. L'appel wait(&status) est Ã©quivalent Ã  :

           waitpid(-1, &status, 0);

       L'appel systÃ¨me waitpid() suspend l'exÃ©cution du processus appelant jusqu'Ã  ce qu'un  fils
       spÃ©cifiÃ©  par  l'argument  pid  change d'Ã©tat. Par dÃ©faut, waitpid() n'attend que les fils
       terminÃ©s, mais ce comportement peut Ãªtre modifiÃ©  par  l'argument  options,  de  la  faÃ§on
       dÃ©crite ci-dessous.

       La valeur de pid peut Ãªtre l'une des suivantes :

       < -1   Attendre la fin de n'importe quel processus fils appartenant au groupe de processus
              d'ID -pid.

       -1     Attendre n'importe lequel des processus fils.

       0      Attendre la fin de n'importe quel processus fils du mÃªme groupe que l'appelant.

       > 0    Attendre la fin du processus numÃ©ro pid.

       La valeur de l'argument option options est un OU binaire entre les constantes suivantes :

       WNOHANG     Ne pas bloquer si aucun fils ne s'est terminÃ©.

       WUNTRACED   Recevoir l'information concernant Ã©galement les fils bloquÃ©s (mais non  suivis
                   par  ptrace(2))  si  on  ne  l'a  pas encore reÃ§ue. L'Ã©tat des fils suivis est
                   fourni mÃªme sans cette option.

       WCONTINUED (Depuis Linux 2.6.10)
                   Renvoyer Ã©galement si un processus fils stoppÃ© a Ã©tÃ©  relancÃ©  par  le  signal
                   SIGCONT.

       (Pour les options spÃ©cifiques Ã  Linux, voir plus bas.)

       Si  status n'est pas NULL, wait() et waitpid() stockent l'Ã©tat du fils dans la variable de
       type int pointÃ©e. Cet entier peut Ãªtre Ã©valuÃ© avec  les  macros  suivantes  (qui  prennent
       l'entier  lui-mÃªme  comme argument, et pas un pointeur vers celui-ci, comme le font wait()
       et waitpid() !) :

       WIFEXITED(status)
              Vrai si le fils s'est terminÃ© normalement, c'est-Ã -dire par un appel Ã   exit(3)  ou
              _exit(2), ou par un return depuis main().

       WEXITSTATUS(status)
              Donne  le  code  de  retour, consistant en les 8 bits de poids faibles du paramÃ¨tre
              status fourni Ã  exit(3) ou _exit(2) ou dans le return de la routine  main().  Cette
              macro ne peut Ãªtre Ã©valuÃ©e que si WIFEXITED est non nul.

       WIFSIGNALED(status)
              Vrai si le fils s'est terminÃ© Ã  cause d'un signal non interceptÃ©.

       WTERMSIG(status)
              Donne  le  numÃ©ro  du  signal  qui a causÃ© la fin du fils. Cette macro ne peut Ãªtre
              Ã©valuÃ©e que si WIFSIGNALED est non nul.

       WCOREDUMP(status)
              Vrai si le processus fils a produit une image mÃ©moire (Â« core dump Â»). Cette  macro
              ne doit Ãªtre Ã©valuÃ©e que si WIFSIGNALED a renvoyÃ© une valeur non nulle.

       WIFSTOPPED(status)
              Vrai si le fils est actuellement arrÃªtÃ©. Cela n'est possible que si l'on a effectuÃ©
              l'appel avec l'option WUNTRACED ou si le fils est suivi (voir ptrace(2)).

       WSTOPSIG(status)
              Donne  le  numÃ©ro  du  signal qui a causÃ© l'arrÃªt du fils. Cette macro ne peut Ãªtre
              Ã©valuÃ©e que si WIFSTOPPED est non nul.

       WIFCONTINUED(status)
              (Depuis Linux 2.6.10) Vrai si le processus fils a Ã©tÃ© relancÃ© par SIGCONT.
=============================
	 * The conditions WIFEXITED, WIFSIGNALED, WIFSTOPPED
	 * are mutually exclusive:
	 * WIFEXITED:  (status & 0x7f) == 0, WEXITSTATUS: top 8 bits
	 * and now WCOREDUMP:  (status & 0x80) != 0
	 * WIFSTOPPED: (status & 0xff) == 0x7f, WSTOPSIG: top 8 bits
	 * WIFSIGNALED: all other cases, (status & 0x7f) is signal.
 '/
'#define WEXITSTATUS(status) (((status)  & 0xff00) >> 8) Return exit status.
#define WEXITSTATUS(status) (((status)  and &hff00) shr 8)

'#define WTERMSIG(status) ((status) & 0x7f) Return signal number that caused process to terminate.
#define WTERMSIG(status) ((status) and &h7f)

'#define WIFEXITED(status) (WTERMSIG(status) == 0) True if child exited normally.
#define WIFEXITED(status) (WTERMSIG(status) = 0)

'#define WIFSTOPPED(status) (((status) & 0xff) == 0x7f) True if child is currently stopped.
#define WIFSTOPPED(status) (((status) and &hff) = &h7f)

'#define WIFSIGNALED(status) (!WIFSTOPPED(status) && !WIFEXITED(status)) True if child exited due to uncaught signal.
#define WIFSIGNALED(status) (not WIFSTOPPED(status) andalso not WIFEXITED(status))

'#define WSTOPSIG(status) WEXITSTATUS(status) Return signal number that caused process to stop.
#define WSTOPSIG(status) WEXITSTATUS(status)

'#define WCOREDUMP(status) ((status) & 0x80) syscall
#define WCOREDUMP(status) ((status) and &h80)




''===============END OF TO BE MOVED ===============================
'' for memory or use :
 ''  ptrace(PTRACE_POKEUSER, pid, sizeof(long)*REGISTER_IP, bp->addr);
 '' long v = ptrace(PTRACE_PEEKUSER, pid, sizeof(long)*REGISTER_IP);


dim shared as pid_t debugpid,order,threadchild,retsegv,addr


Dim Shared as string sigcode(20)
sigcode(SIGINT)="SIGINT"
sigcode(SIGTRAP)="SIGTRAP"
sigcode(SIGABRT)="SIGABRT"
sigcode(SIGKILL)="SIGKILL"
sigcode(SIGSEGV)="SIGSEGV"
sigcode(SIGUSR1)="SIGUSR1"
sigcode(SIGUSR2)="SIGUSR2"
sigcode(SIGTERM)="SIGTERM"
sigcode(SIGCHLD)="SIGCHLD"
sigcode(SIGCONT)="SIGCONT"
sigcode(SIGSTOP)="SIGSTOP"

private sub sigusr_send()
	print "sigusr1 to be sent=";SYS_tgkill,dbgpid,thread2,SIGUSR1
	print "sigusr1 sent with return code=";syscall(sys_tgkill,dbgpid,thread2,SIGUSR1)
end sub

'===============================
''reads proc/maps-mem
'===============================
private sub read_proc(filesyst as string)
	Dim filein As Integer,lineread As String
	Filein = FreeFile
	Open filesyst For Input As #Filein
	print "reading =";filesyst
	Do While Not Eof(Filein)
		Line Input #filein,lineread
		print lineread
	loop
	print "end of reading"
	close #filein
end sub
'==================================
'' 3 procs for selecting only exes
'==================================
Function FileFilterFunc Cdecl (filter_info As Const GtkFileFilterInfo Ptr, p As gpointer) As gboolean
	If g_content_type_can_be_executable(filter_info->mime_type) Andalso Instr(Lcase(*filter_info->mime_type) , "text") = 0 Then
		Return True
	Else
		Return False
	Endif
End Function
'=============
Sub SetExeFilterRequester(chooser As GtkFileChooser Ptr  , sName As String)
    Dim As GtkFileFilter Ptr filter
    filter = gtk_file_filter_new ()
    gtk_file_filter_add_custom(filter,GTK_FILE_FILTER_MIME_TYPE, @FileFilterFunc, 0,0)
    gtk_file_filter_set_name(filter , Strptr(sName))
    gtk_file_chooser_add_filter (chooser, filter)
End Sub
'=============
Function OpenFileRequesterExe(sTitle As String , sCurDir As String, sPattern As String = "EXE files", iFlag As Long = 0, sTemplateFileName As String = "",  hParentWin As HWND = 0) As String
	Dim As GtkWidget Ptr dialog
	Dim As GtkFileChooser Ptr chooser
	Dim As Long iAction = GTK_FILE_CHOOSER_ACTION_OPEN
	Dim As gint res
	If iFlag And OFN_DIRECTORY Then
		iAction = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER
	Endif
	dialog = gtk_file_chooser_dialog_new(sTitle, Cast(Any Ptr, hParentWin) , iAction, "Cancel",_
									   GTK_RESPONSE_CANCEL, "OK", GTK_RESPONSE_ACCEPT, NULL)
	chooser = GTK_FILE_CHOOSER(dialog)
	If sCurDir <> "" Then
		gtk_file_chooser_set_current_folder(chooser , sCurDir)
	Endif
	If iFlag And OFN_ALLOWMULTISELECT Then
		gtk_file_chooser_set_select_multiple(chooser , 1)
	Endif
	SetExeFilterRequester(chooser , sPattern)
	res = gtk_dialog_run(GTK_DIALOG(dialog))
	If res = GTK_RESPONSE_ACCEPT Then
		Dim As Zstring Ptr psRet = gtk_file_chooser_get_filename (chooser)
		Dim As String sRet = *psRet
		g_free (psRet)
		gtk_widget_destroy(dialog)
		Return sRet
	Endif
	gtk_widget_destroy(dialog)
End Function
'========================
'' open/close a console
'========================
'flaglog=0 --> no output / 1--> only screen / 2-->only file / 3 --> both
private sub output_lnx(txt As String)
	'messbox("Feature missing","open/close console : ouput_lnx")
	print txt
end sub
'=============================================================
private function findadr() as integer
''-------------
	dim as integer xip
	ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
	''address where really stopped, current was pointed to next instruction
	xip=regs.xip-1
	For jline As Long=1 To linenb
		'print"searching ",rline(jline).ad
		if rline(jline).ad=xip then
			addr=rline(jline).ad
			print "line found idx,number,addr = ";jline,rline(jline).nu,hex(addr)
			return jline
		endif
	next
	print "line not found"
	return -1
end function
'======================================
''show all the registers
'======================================
private sub show_regs()
	messbox("Show registers","Not yet implemented for linux")
End Sub
'=================================================================
'' Executes order from first thread in second thread using ptrace
'=================================================================
private sub exec_order(order as integer)
	mutexlock blocker
	msgcmd=order
	bool2=true
	condsignal(condid)
	while bool1<>true
		condwait(condid,blocker)
	wend
	bool1=false
	mutexunlock blocker
end sub
'=========================================
private function ReadProcessMemory(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr=0) as integer
	msgad=cast(integer,addrdbge)
	msgad2=cast(integer,addrdbgr)
	msgdata=lg
	exec_order(KPT_READMEM)
	return 1
End Function
'=========================================
private function WriteProcessMemory(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr=0) as integer
	msgad=cast(integer,addrdbge)
	msgad2=cast(integer,addrdbgr)
	msgdata=lg
	exec_order(KPT_WRITEMEM)
	return 1
End Function
'===============================================================
private function writeprocessmemory_th2(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr=0)as integer
	Dim As integer integersize=SizeOf(Integer),i,j=lg\integersize
	Dim As Integer buf,errn
	if addrdbge=0 or addrdbgr=0 then
		print "write addrdbge=";hex(addrdbge),"addrdbgr=";hex(addrdbgr)
		return 1
	EndIf
	If lg<=integersize Then
		buf=ptrace(PTRACE_PEEKDATA,child, addrdbge,NULL) 'read current value
		'print "buf0=";hex(buf)
		memcpy(@buf,addrdbgr,lg) 'copy just bytes needed
		errn=ptrace(PTRACE_POKEDATA,child, addrdbge,cast(any ptr,buf)) 'write all
		if errn<>0 then print "in write peek errno=";errno,hex(addrdbge)
		return 1
	EndIf

	While i<j
		buf=ptrace(PTRACE_PEEKDATA,child, addrdbge,NULL) 'read current value
		memcpy(@buf,addrdbgr,integersize)
		errn=ptrace(PTRACE_POKEDATA,child, addrdbge,cast(any ptr,buf))
		i+=1
		addrdbge+=integersize
		addrdbgr+=integersize
	Wend

	j=lg Mod integersize
	If j Then
		buf=ptrace(PTRACE_PEEKDATA,child, addrdbge,NULL) 'read current value
		memcpy(@buf,addrdbgr,j) 'copy just bytes needed
		errn=ptrace(PTRACE_POKEDATA,child, addrdbge,cast(any ptr,buf)) 'write all
	EndIf
	return 1
End function
private function readmemlongint(child As long,addrdbge As integer)as longint
	#ifdef __FB_64BIT__
		return ptrace(PTRACE_PEEKDATA,child, cast(any ptr,addrdbge),NULL)
	#else
		dim as longint lgint
		dim as integer value
		value=ptrace(PTRACE_PEEKDATA,child, cast(any ptr,addrdbge),NULL)
		lgint=value
		addrdbge+=sizeof(integer)
		value=ptrace(PTRACE_PEEKDATA,child, cast(any ptr,addrdbge),NULL)
		lgint+=value shl 32
		return lgint
	#endif
End Function
'==========================================
private function ReadProcessMemory_th2(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr=0) as integer
	Dim As integer integersize=SizeOf(Integer),i,j=lg\integersize
	Dim As integer buf(j)
	if addrdbge=0 or addrdbgr=0 then
		'print "read addrdbge=";hex(addrdbge),"addrdbgr=";hex(addrdbgr)
		return 1
	EndIf
	If lg<=integersize Then
		buf(0)=ptrace(PTRACE_PEEKDATA,child, addrdbge,NULL)
		memcpy(addrdbgr,@buf(0),lg)
		'*rd=lg
		return 1 ''return 0 if error
	EndIf
	While i<j
		buf(i)=ptrace(PTRACE_PEEKDATA,child, addrdbge,NULL)
		i+=1
		addrdbge+=integersize 'next pointed value (add 4 or 8 bytes)
	Wend
	j=lg Mod integersize
	If j Then
		buf(i)=ptrace(PTRACE_PEEKDATA,child, addrdbge,NULL)
	EndIf

	memcpy(addrdbgr,@buf(0),lg)
	'*rd=lg
	return 1 ''return 0 if error
End function
'========================================================
''puts the breakpoints on all lines saving previous value
'========================================================
private sub putbreaks()
''-------------
	dim dta as integer 'ALWAYS INTEGER 4 or 8 bytes
	print"in put breaks",linenb,"pid";debugpid
	errno=0
	For j As Long=1 To linenb
		With rline(j)
			'Print "j=";j;" nu=";.nu;" ad=";.ad;"/";hex(.ad);" px-nu=";proc(.px).nu
			If proc(.px).nu=-1 Then
				Print " never reached added by compiler=";hex(rline(j).ad),rline(j).nu
			Else
				dta = ptrace(PTRACE_PEEKTEXT,debugpid,cast(any ptr,.ad),null)
				if errno<>0 then print "error=";errno
				.sv=dta and &hFF
				dta = (dta and FIRSTBYTE ) or &hCC
				ptrace(PTRACE_POKETEXT,debugpid,cast(any ptr,.ad),cast(any ptr,dta))
			EndIf
		End With
	Next
	ccstate=KCC_ALL
end Sub
'=========================================================
''puts(default=1)/removes(0) the breakpoints for all lines
'=========================================================
private sub putremove_breaks(byval pid as long,byval typ as integer=1)
''-------------
	dim dta as integer 'ALWAYS INTEGER 4 or 8 bytes
	print"in put/remove breaks",linenb,"pid=";pid
	if typ then
		''puts breakpoints
		For j As Long=1 To linenb
			With rline(j)
			If proc(.px).nu=-1 Then
				'Print " never reached added by compiler"
			Else
				if proc(.px).enab = true then
					dta = ptrace(PTRACE_PEEKTEXT,pid,cast(any ptr,.ad),null)
					dta = (dta and FIRSTBYTE ) or &hCC
					if ptrace(PTRACE_POKETEXT,pid,cast(any ptr,.ad),cast(any ptr,dta))=-1 then
						print "impossible to poketext, errno=";errorlibel(errno)
						exit for
					End If
				end if
			End If
			End With
		Next
		ccstate=KCC_ALL
	else
		''restores intructions
		For j As Long=1 To linenb
			With rline(j)
			If proc(.px).nu=-1 Then
				'Print " never reached added by compiler"
			Else
				dta = ptrace(PTRACE_PEEKTEXT,pid,cast(any ptr,.ad),null)
				dta = (dta and FIRSTBYTE ) or rline(j).sv
				ptrace(PTRACE_POKETEXT,pid,cast(any ptr,.ad),cast(any ptr,dta))
			end if
			End With
		Next
		ccstate=KCC_NONE
	end if
end Sub
'===========
private sub thread_new(pid as long)
	If threadnb<THREADMAX Then
		threadnb+=1
		thread(threadnb).id=pid
		thread(threadnb).pe=FALSE
		thread(threadnb).sv=-1 'used for thread not debugged
		thread(threadnb).plt=0 'used for first proc of thread then keep the last proc
		thread(threadnb).st=thread(threadcur).od 'used to keep line origin
		thread(threadnb).tv=0
		thread(threadnb).sts=KTHD_INIT
		print "new thread nb=";threadnb,"pid=";pid
	Else
		hard_closing("Number of threads ("+Str(THREADMAX+1)+") exceeded , change the THREADMAX value."+Chr(10)+Chr(10)+"CLOSING FBDEBUGGER, SORRY" )
	EndIf
End Sub
'=====================================================
Private sub thread_rsm_one()
	print "in thread_rsm restore=";threadcur,thread(threadcur).sv,rLine(thread(threadcur).sv).nu,hex(rLine(thread(threadcur).sv).sv)
	if rLine(thread(threadcur).sv).sv=-1 then exit sub

	msgad=rLine(thread(threadcur).sv).ad
	msgdata=rLine(thread(threadcur).sv).sv and &hFF
	exec_order(KPT_RESTORE)
End Sub
'=====================================================
Private sub thread_rsm()
	print "in thread_rsm executing KPT_CONTALL"
	exec_order(KPT_CONTALL)
End Sub
'=====================================================
Private sub singlestep_rsm(rln as INTEGER)
	msgad=rLine(rln).ad
	msgdata=rLine(rln).sv
	print "in singlestep_rsm=";rln,hex(msgad),rLine(rln).nu,msgdata
	exec_order(KPT_SSTEP)
End Sub
'=====================================
private sub resume_exec()
	For jbrk As Integer = 1 To brknb ''restore if needed the UBP
		If brkol(jbrk).typ<50 Then
			if rlinecur=brkol(jbrk).index then ''if current line is a BP (permanent/cond/counter)
				singlestep_rsm(rlinecur)
				exit sub
			EndIf
		end if
	Next
	thread_rsm()
end sub
'====================================================================
'' Executes all commands from main thread
'====================================================================
private sub brp_stop(tidx as integer,bptype as integer,ddata as integer,dbgevent as INTEGER = KDBGRKPOINT)
	dim as integer dta
	threadcur=tidx''only useful case when exiting thread replaced by first thread
	print "brp_stop=";tidx,thread(tidx).id,bptype,hex(ddata)
	stopcode=bptype
	debugdata=ddata
	debugevent=dbgevent
	'===========
	while 1
		mutexlock blocker
		while bool2<>true
			condwait(condid,blocker)
		wend
		'print "in brp_stop after condwait";msgcmd,hex(msgad),hex(msgad2),hex(msgdata)
		bool2=false
		select case as const msgcmd

			Case KPT_CONT
				print "cont"
				'bool1=true
				'condsignal(condid)
				'mutexunlock blocker
				ptrace(PTRACE_CONT,thread(threadcur).id,0,0)
				'exit while

			Case KPT_CONTALL
				for ith as integer =0 to threadnb
					print "KPT_CONTALL idx,sts=";ith,thread(ith).sts
					if thread(ith).sts=KTHD_STOP and rLine(thread(threadcur).sv).sv<>-1 then
						dta = ptrace(PTRACE_PEEKTEXT,thread(ith).id,cast(any ptr,rLine(thread(ith).sv).ad),null)
						dta = (dta and FIRSTBYTE ) or (rLine(thread(ith).sv).sv and &hFF)
						print "KPT_CONTALL=";ptrace(PTRACE_POKETEXT,thread(ith).id,cast(any ptr,rLine(thread(ith).sv).ad),cast(any ptr,dta));" ";hex(rLine(thread(ith).sv).ad);" ";,hex(dta)
						ptrace(PTRACE_CONT,thread(ith).id,0,0)
						thread(ith).sts=KTHD_RUN
					end if
				next
				bool1=true
				condsignal(condid)
				mutexunlock blocker

				exit while

			Case KPT_XIP ''update EIP or RIP
				ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
				print "update xip=";hex(regs.xip),hex(msgad)
				regs.xip=msgad
				ptrace(PTRACE_SETREGS, thread(threadcur).id, NULL, @regs)

			Case KPT_CCALL
				putremove_breaks(thread(threadcur).id,msgdata)

			Case KPT_CC
				dta = ptrace(PTRACE_PEEKTEXT,thread(threadcur).id,cast(any ptr,msgad),null)
				dta = (dta and FIRSTBYTE ) or &hCC
				ptrace(PTRACE_POKETEXT,thread(threadcur).id,cast(any ptr,msgad),cast(any ptr,dta))

			case KPT_SSTEP
				dta = ptrace(PTRACE_PEEKTEXT,thread(threadcur).id,cast(any ptr,msgad),null)
				dta = (dta and FIRSTBYTE ) or msgdata
				ptrace(PTRACE_POKETEXT,thread(threadcur).id,cast(any ptr,msgad),cast(any ptr,dta))
				ssadr=msgad
				print "single step defined by KPT_SSTEP threadcur=";threadcur,hex(ssadr)
				bool1=true
				condsignal(condid)
				mutexunlock blocker
				ptrace(PTRACE_SINGLESTEP, thread(threadcur).id, NULL, NULL)
				exit while

			Case KPT_GETREGS
				if ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs) then
					print "error in getregs=";errno
				EndIf

			Case KPT_SETREGS
				if ptrace(PTRACE_SETREGS, thread(threadcur).id, NULL, @regs)=-1 then
					print "error in setregs=";errno
				EndIf

			Case KPT_READMEM
				ReadProcessMemory_th2(thread(threadcur).id,cast(any ptr,msgad),cast(any ptr,msgad2),msgdata)

			Case KPT_WRITEMEM
				WriteProcessMemory_th2(thread(threadcur).id,cast(any ptr,msgad),cast(any ptr,msgad2),msgdata)

			case KPT_RESTORE
				dta = ptrace(PTRACE_PEEKTEXT,thread(threadcur).id,cast(any ptr,msgad),null)
				'print "data read=";hex(dta),hex((dta and FIRSTBYTE )),hex(msgdata)
				dta = (dta and FIRSTBYTE ) or msgdata
				print "KPT_RESTORE=";ptrace(PTRACE_POKETEXT,thread(threadcur).id,cast(any ptr,msgad),cast(any ptr,dta));" ";hex(msgad);" ";,hex(dta)

				ptrace( PTRACE_CONT, thread(threadcur).id, NULL, NULL )

				bool1=true
				condsignal(condid)
				mutexunlock blocker

				exit while

			case KPT_KILL
				print "sending sigkill"
				linux_kill(debugpid,9) ''send SIGKILL
				bool1=true
				condsignal(condid)
				mutexunlock blocker
				prun=false
				exit while

			case KPT_SIGNAL
				''get pending signal no stop but signal is removed so need to be saved
				threadsaved=waitpid(-1,@statussaved,__WNOHANG)
				print "KPT_SIGNAL thread=";threadsaved,"status=";hex(statussaved)

				bool1=true
				condsignal(condid)
				mutexunlock blocker

			case else ''can be used for exiting the loop
				print "msgcmd not handled exiting loop=";msgcmd
				bool1=true
				condsignal(condid)
				mutexunlock blocker
				exit while
		End Select
		bool1=true
		condsignal(condid)
		mutexunlock blocker
	Wend
	'===========
	print "in br_stop exit while"


end sub
'=============================================
'' Executed when the first SIGTRAP is received
'=============================================
private sub first_sigtrap()
	threadcur=0
	thread(threadcur).id=debugpid
	print "thread(threadcur).id=";thread(threadcur).id
	print "get regs in first=";ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
	print "xip=";hex(regs.xip)
	'debugdata=Cast(Integer,.lpBaseOfImage)
	debugevent=KDBGCREATEPROCESS
	mutexlock blocker
	while bool2<>true
		condwait(condid,blocker)
	wend
	bool2=false
	mutexunlock blocker
	'read_proc("/proc/"+ltrim(str(pid))+"/maps")
	'read_proc("/proc/"+ltrim(str(pid))+"/status")
	putbreaks()
	firsttime=1
	prun=true
	runtype=RTSTEP
	ptrace(PTRACE_SETOPTIONS, thread(threadcur).id, 0, Cast(Any Ptr,(PTRACE_O_TRACECLONE)))
	ptrace(PTRACE_CONT,thread(threadcur).id,0,0)
End Sub
'================================================
'' starts debuggee for linux
'================================================
private sub start_pgm(p As Any Ptr)
	dim as long threadSig
	static as long newpid
	thread2=syscall(SYS_GETTID)
	if thread2=-1 then print "errno=";errno
	print "current pid=";thread2
	dim as pid_t pid=fork()

	print "pid=";pid,"size=";sizeof(pid_t)
	dim as long status
	dim as INTEGER	dta,bptyp
	Select Case pid
		case -1 'error child not created
			Print "error child not created"'"errno = ";errno 'use perror("fork") ?
			exit sub

		Case 0 'child created now the debuggee is going to be started
			debugpid=getpid
			Print "Debuggee pid = ",debugpid,syscall(SYS_GETTID)
			if ptrace(PTRACE_TRACEME, 0, 0,0) then
				Print "error traceme"'"errno = ";errno 'use perror("ptrace")
	    		exit sub
			EndIf
			print "name", exename
			if execv(strptr(exename),NULL) then
				print "error on starting debuggee=";errno ' argv, envp)=-1
				exit sub
			EndIf

		Case Else
			sig_init()
			debugpid=pid
			threadnb=0
			thread(threadcur).id=pid
			thread(threadcur).sv=-1
			print "debugpid in scope debugger =";pid
			Print "Debugger pid (+thread) = ";getpid,syscall(SYS_GETTID)
			wait(0)

			first_sigtrap()

			print "entering in loop"
			while 1
				''waiting loop, exit when debuggee terminated
				if statussaved then
					status=statussaved
					threadsig=threadsaved
					statussaved=0
					threadsaved=0
					print "Using saved data thread=";threadsig,"status=";hex(status)
				else
					threadSig=waitpid(-1,@status,__WALL)
				EndIf
				print "======================================== pid=";threadSig;" ======== status=";hex(status)
				if threadsig=-1 then
					print "thread =-1 so errno=";errno
				EndIf
				''handle status
				if WIFEXITED(status) then
					if threadSig=debugpid then
						print "normal exit with status=";WEXITSTATUS(status)
						prun=false
						afterkilled=KDONOTHING
						debugdata=WEXITSTATUS(status)
						debugevent=KDBGEXITPROCESS
						exit while
					else
						print "thread=";threadSig;" exit with status=";WEXITSTATUS(status)
						brp_stop(0,0,threadSig,KDBGEXITTHREAD) ''for waiting command
					end if

				elseif WIFSIGNALED(status) then
					prun=false
					print "Killed by=";WTERMSIG(status)
					debugdata=WTERMSIG(status)
					debugevent=KDBGEXITPROCESS
					exit while

				elseif WIFSTOPPED(status) then
					print "signal stop=";WSTOPSIG(status)

					if threadsig<>thread(threadcur).id then ''other thread than the current ?
						print "Thread signal <> current thread=";threadcur," sig=";threadSig
						if WSTOPSIG(status)<>SIGSTOP then ''maybe SIGSTOP for new thread so DON'T change threadcur
							for ith as integer = 0 to threadnb
								if threadsig=thread(ith).id then
									threadcur=ith
									print "thread changed index, pid=",threadcur,threadSig
									exit for
								EndIf
							Next
						end if
					EndIf

					select case WSTOPSIG(status)

						case SIGTRAP

							if (((status shr 16) and &hffff) = PTRACE_EVENT_CLONE) then  ''checks new thread ?
								dim as integer eventmsg
								ptrace(PTRACE_GETEVENTMSG,thread(threadcur).id,null,@eventmsg)
								ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
								newpid=eventmsg
								''to handle cases where 3057F arrives after 137F
								for ithd as integer =0 to threadnb
									if thread(ithd).id=eventmsg then
										newpid=0
										print "NEW thread already created current xip=";eventmsg
										exit for
									EndIf
								next
								if newpid then
									print "NEW thread created current xip=";newpid
									thread_new(newpid)
								end if
								ptrace(PTRACE_CONT,thread(threadcur).id,0,0)
								continue while
							end if

							print "-------------  EXCEPTION_BREAKPOINT   ------------------------------"
							if ssadr<>0 then ''a single step has been executed
								print "handling single step=";hex(ssadr),threadcur,thread(threadcur).id
								dta = ptrace(PTRACE_PEEKTEXT,thread(threadcur).id,cast(any ptr,ssadr),null)
								'print "data read=";hex(dta),hex((dta and FIRSTBYTE )),hex(msgdata)
								dta = (dta and FIRSTBYTE ) or &hCC
								ptrace(PTRACE_POKETEXT,thread(threadcur).id,cast(any ptr,ssadr),cast(any ptr,dta))
								ssadr=0
								ptrace(PTRACE_CONT,thread(threadcur).id,0,0)
							else
								dim as integer xip, bpidx=-1
								ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
								''address where really stopped, current was pointed to next instruction
								'if findadr() <> -1 then
									xip=regs.xip-1
								'end if
								print "xip=";hex(xip),"runtype=";runtype,"RTRUN=";RTRUN,"RTSTEP=";RTSTEP
								if runtype=RTCRASH then
									''don't stop as running until a crash
									breakadr=xip
									for irln as integer =1 to linenb
										if rline(irln).ad=xip then ''need to find where we are
											singlestep_on(thread(threadcur).id,irln)
											'ptrace(PTRACE_SINGLESTEP, thread(threadcur).id, NULL, NULL)
											'exit for
											continue while
										EndIf
									Next
								elseif runtype=RTRUN then
									if brkv.adr1<>0 then
										if brk_test(brkv.adr1,brkv.adr2,brkv.typ,brkv.val,brkv.ttb) then
											if brkv.ivr1=0 then
												brp_stop(threadcur,CSMEM,xip)
											else
												brp_stop(threadcur,CSVAR,xip)
											end if
											continue while
										else
											for irln as integer =1 to linenb
												if rline(irln).ad=xip then ''need to find where we are
													singlestep_on(thread(threadcur).id,irln)
													'ptrace(PTRACE_SINGLESTEP, thread(threadcur).id, NULL, NULL)
													'exit for
													continue while
												EndIf
											Next
										end if

									end if
									''retrieves BP corresponding at address (loop) -->bpidx
									For ibrk as integer =0 To brknb
										If brkol(ibrk).typ>50 Then Continue For ''BP disabled
										if brkol(ibrk).ad=xip then
											bpidx=ibrk
											print "BP found idx=";bpidx,"typ=";brkol(ibrk).typ
											exit for
										EndIf
									Next
									if bpidx<>-1 then
										if bpidx=0 then ''BP on LINE (line, cursor, over,eop,xop)
											print "BP on line"
											brp_stop(threadcur,CSLINE,bpidx)
											continue while
										end if
										bptyp=brkol(bpidx).typ
										if bptyp=2 or bptyp=3 then  ''BP conditional
											if brk_test(brkol(bpidx).adrvar1,brkol(bpidx).adrvar2,brkol(bpidx).datatype,brkol(bpidx).val,brkol(bpidx).ttb) then
												brp_stop(threadcur,CSCOND,bpidx)
											else
												singlestep_on(thread(threadcur).id,brkol(bpidx).index)
												'ptrace(PTRACE_SINGLESTEP, thread(threadcur).id, NULL, NULL)
											end if
											continue while
										elseif bptyp=4 then ''BP counter
											print "counter=";bpidx,brkol(bpidx).counter
											If brkol(bpidx).counter>0 Then
												brkol(bpidx).counter-=1'decrement counter
												print "decrement counter=";brkol(bpidx).counter
												singlestep_on(thread(threadcur).id,brkol(bpidx).index)
												'ptrace(PTRACE_SINGLESTEP, thread(threadcur).id, NULL, NULL)
											else
												brp_stop(threadcur,CSCOUNT,bpidx)
											end if
											continue while
										else
											''simple BP (perm/tempo)
											brp_stop(threadcur,CSBRKPT,bpidx)
											continue while
										end if
										if stopcode=CSUSER then
											brp_stop(threadcur,stopcode,xip)
											continue while
										end if
									else
										''case halt by user so no breakpoint and stopcode should be CSUSER
										''continue waiting to a normal breakpoint
										'ptrace(PTRACE_CONT,thread(threadcur).id,0,0)
										'for irln as integer =1 to linenb
											'if rline(irln).ad=xip then ''need to find where we are
												'print "Breakpoint not a User BP, line found, executing single step on ";hex(xip)
												'singlestep_on(thread(threadcur).id,irln)
												'runtype=RTSTEP
												'
												'continue while
											'EndIf
										'Next
										if stopcode<>CSUSER then
											print "STOPPED without UBP CSNEWTHRD=";thread(threadcur).id,hex(xip)
											brp_stop(threadcur,CSNEWTHRD,xip)
										else
											brp_stop(threadcur,CSUSER,xip)
										end if
									end if

								else ''RTSTEP/RTAUTO
									if stopcode=CSUSER then ''CSUSER : user halts the debuggee
										brp_stop(threadcur,stopcode,xip)

									else
										print "CSSTEP"
										brp_stop(threadcur,CSSTEP,xip)
									end if
									continue while
								end if
							end if

						case SIGSEGV
							ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
							print "sigsegv=";hex(regs.xip)
							dim as tsiginfo siginfo
							ptrace(PTRACE_GETSIGINFO, thread(threadcur).id, NULL, @siginfo)
							print "siginfo=";siginfo.si_signo, siginfo.si_code, siginfo.si_errno,hex(siginfo.si_addr)

						case SIGILL
							ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
							print "sigill=";hex(regs.xip)
							dim as tsiginfo siginfo
							ptrace(PTRACE_GETSIGINFO, thread(threadcur).id, NULL, @siginfo)
							print "siginfo=";siginfo.si_signo, siginfo.si_code, siginfo.si_errno,hex(siginfo.si_addr)

						case SIGCHLD
							ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
							print "sigchld=";hex(regs.xip)
							dim as tsiginfo siginfo
							ptrace(PTRACE_GETSIGINFO, thread(threadcur).id, NULL, @siginfo)
							print "siginfo=";siginfo.si_signo, siginfo.si_code, siginfo.si_errno,hex(siginfo.si_addr),siginfo.dummy(0),siginfo.dummy(1)
							ptrace(PTRACE_CONT,thread(threadcur).id,0,0)
						case else

							'print "other Signals=";WSTOPSIG(status)
							if WSTOPSIG(status)=SIGSTOP then
								print "SIGSTOP"
							elseIf WSTOPSIG(status)=SIGCONT then
								print "SIGCONT"
							End If

							if thread(threadcur).id <> threadSig then ''new thread ready to start
								print "threadcur, id, sig=";threadcur,thread(threadcur).id , threadSig
								print "ptrace get bis regs=";thread(threadnb).id,ptrace(PTRACE_GETREGS, thread(threadnb).id, NULL, @regs)
								print "xip=";hex(regs.xip)
								if newpid=0 then ''special case signal 3057F missing so only 137F for new thread
									print "executing thread_new as signal 3057F missing for =";threadSig
									thread_new(threadSig)
								else
									newpid=0
								EndIf
								threadnewid=threadsig
								threadnewidcount=2
								putremove_breaks(thread(threadnb).id)
								ptrace(PTRACE_CONT,thread(threadnb).id,0,0)
							end if
					end select
				else
					print "Status not handled=";status
				end if
			wend
	End Select
end sub
'==================================
'' lists all the processes
'==================================
private sub process_list()
  	messbox("For Linux","Process list not yet implemented")
End Sub
'==================================
private function dll_name(FileHandle As HANDLE,t As Integer =1 )As String ' t=1 --> full name, t=2 --> short name
	return "name of dll..."
end function
'====================================================================
'' prepares singlestepping for restoring BP
'====================================================================
private sub singlestep_on(tid as integer,rln as integer,running as integer =1)
	dim as integer dta
	For i As Integer =0 To threadnb
		If tid=thread(i).id Then
			threadcur=i
			if running then ''when not running initial code is already restored and no need to decrease EIP
				ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
				''restore initial code
				dta = ptrace(PTRACE_PEEKTEXT,thread(threadcur).id,cast(any ptr,rline(rln).ad),null)
				'print "data read=";hex(dta),hex((dta and FIRSTBYTE )),hex(msgdata)
				dta = (dta and FIRSTBYTE ) or rLine(rln).sv
				ptrace(PTRACE_POKETEXT,thread(threadcur).id,cast(any ptr,rline(rln).ad),cast(any ptr,dta))
				''change EIP go back 1 byte
				regs.xip-=1
				ptrace(PTRACE_SETREGS, thread(threadcur).id, NULL, @regs)
				print "single step restoring old value on=";hex(rline(rln).ad),"new xip=";hex(regs.xip)
			end if
			''rline for restoring BP
			ssadr=rline(rln).ad
			print "single step defined=";hex(ssadr)
			ptrace(PTRACE_SINGLESTEP, thread(threadcur).id, NULL, NULL)
			exit sub
		End If
	Next
End Sub
'========================================================
''  handles breakpoints
'========================================================
private sub gest_brk(ad As Integer,byval rln as integer =-1)
	Dim As Integer i,debut=1,fin=linenb+1,adr,iold
   'egality added in case attach (example access violation) without -g option, ad=procfn=0....
	If ad>=procfn Then
		thread_resume()
		Exit Sub
	EndIf


	'' when there are dynamic arrays it's necessary to change a value in their descriptor
	'' so access to the memory and thread must be waiting orders.....
	if firsttime=1 then
		print "try to change for dynamic arrays"
		firsttime=2
		init_debuggee(srcstart)
	EndIf

	dbg_prt2("")
	dbg_prt2("AD gest brk="+hex(ad)+" th="+Str(threadcur))+" runtype="+str(runtype)
	'show_context

	proccurad=ad
	print "rln=";rln,thread(threadcur).sv+1,hex(rline(thread(threadcur).sv+1).ad)
	if rln=-1 then ''search the line using address
		i=thread(threadcur).sv+1
		If rline(i).ad<>ad Then 'hope next source line is next executed line (optimization)
			While 1
				iold=i
				i=(debut+fin)\2 'first consider that the addresses are sorted increasing order
				If i=iold Then 'loop
					For j As Integer =1 To linenb
						If rline(j).ad=ad Then i=j:Exit While
					Next
				End If
				If ad>rLine(i).ad Then
					debut=i
				ElseIf ad<rLine(i).ad Then
					fin=i
				Else
					Exit While
				End If
			Wend
		EndIf
		rln=i
	end if
	rlinecur=rln
	print "rlinecur=";rlinecur

	thread(threadcur).od=thread(threadcur).sv
	thread(threadcur).sv=rln

	print "in gest brk sv=";thread(threadcur).sv

	procsv=rline(rln).px
	'dbg_prt2("proc ="+Str(procsv)+" "+proc(procsv).nm+" "+hex(proc(procsv).db)+" "+source(proc(procsv).sr)+" "+hex(proccurad))
	'dbg_prt2("line="+Str(rline(i).nu))

	'get registers
	exec_order(KPT_GETREGS)
	print "gest_brk xip=";hex(regs.xip),hex(regs.xsp),time

	print "test proccurad=proc(procsv).db",hex(proccurad),hex(proc(procsv).db)
	If proccurad=proc(procsv).db Then 'is first proc instruction

		If rline(rln).sv=85 Then'check if the first instruction is push ebp opcode=85 / push rbp opcode=&h55=85dec
			'in this case there is a prologue
			 'at the beginning of proc EBP not updated so use ESP
			procsk=regs.xsp-SizeOf(Integer)
		Else
			If procrnb<>0 Then  'no main and no prologue so naked proc, procrnb not yet updated
				procsk=regs.xsp
			   thread(threadcur).nk=procsk
			Else
				messbox("Main procedure problem","No standard prologue --> random behaviour")			
				procsk=regs.xsp-SizeOf(Integer)
			EndIf
		End If
	else
		'only for naked, check if return by comparing top of stack because no epilog
		If thread(threadcur).nk Then
			if regs.xsp>thread(threadcur).nk Then
				thread(threadcur).pe=TRUE
				thread(threadcur).nk=0
			EndIf
		End If
	End If
	regs.xip=ad
	print "in gest_brk setregs xip=",hex(regs.xip)
	exec_order(KPT_SETREGS)

	'dbg_prt2("PE"+Str(thread(threadcur).pe)+" "+Str(proccurad)+" "+Str(proc(procsv).fn))
	If thread(threadcur).pe Then 'if previous instruction was the last of proc
		If proccurad<>proc(procsv).db Then 'reload procsk with rbp/ebp test added for case constructor on shared
			procsk=regs.xbp
		EndIf
		proc_end():thread(threadcur).pe=FALSE
	EndIf

	If proccurad=proc(procsv).db Then 'is first proc instruction
		if threadnewid<>0 then
			print "new thread beginning of proc"
			threadnewidcount-=1
		end if
		thread_rsm_one() ''should reach first basic line before stopping
		Exit Sub
	end if
	thread(threadcur).sts=KTHD_STOP ''doing that here because can exit just above and still running

''=========== end of code to move ========================================================
	If runtype=RTRUN Then
   		fasttimer=Timer-fasttimer
		'''''''==== useful ?? ===============
		'''''print "restoring all CC after RTRUN"
	  	'''''For i As Integer = 1 To linenb 'restore CC
   			'''''WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@breakcpu,1,0)
	  	'''''Next
		'''''''==== end of code ===============
		'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(rln).ad),@rLine(rln).sv,1,0) 'restore old value for execution
		'if rln<>rLine(brkol(0).index then ''case BP cond/etc and run to cursor/EOP/XOP
			'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(brkol(0).index).ad),@rLine(brkol(0).index).sv,1,0) 'restore old value for execution
		'end if
   	''?????	brk_test(proccurad) ' cancel breakpoint on line, if command halt not really used

		if brkol(0).typ<>10 then ''for skip over always in same proc, if different thread ???
			print "in gest_brk runnew"
			proc_runnew   'creating running proc tree
		end if
   		var_sh			'updating information about variables

   		''2022/02/15 runtype=RTSTEP   now done when there is not anymore signal

   		dsp_change(rln)
		if stopcode=CSLINE then
			brk_del(0)
		elseif stopcode=CSBRKPT then
			for ibrk as INTEGER	= 1 to brknb
				if brkol(ibrk).index=rln then
					if brkol(ibrk).typ=5 then
						brk_del(ibrk) ''remove tempo BP
						exit for
					EndIf
				EndIf
			Next
		End If

   Else 'RTSTEP or RTAUTO
		If flagattach Then
			proc_runnew
			flagattach=FALSE
		else
			If proccurad=proc(procsv).first Then 'is first fbc instruction ?
				if procsk<>thread(threadcur).stack then ''check if not in the current proc
					print "going to proc_new",hex(proccurad),hex(proc(procsv).first)
					proc_new()
				end if
			ElseIf proccurad=proc(procsv).fn Then
				thread(threadcur).pe=TRUE        'is last instruction ?
			End If
		End If
		'NOTA If rline(i).nu=-1 Then
			'fb_message("No line for this proc","Code added by compiler (constructor,...)")
		'Else
		dsp_change(rln)
		'EndIf


		''restore CC previous line
		If thread(threadcur).od<>-1 Then
			msgad=rLine(thread(threadcur).od).ad
			print "restoring 01 CC01 ad=";hex(msgad)
			exec_order(KPT_CC)
		End If

		If runtype=RTAUTO Then
			Sleep(autostep)
			'thread_resume()
		End If
   End If

   if threadnewid then
		if threadcur<>thread_index(threadnewid) then
			print "in gest brk exiting loop br_stop cur/new?";threadcur,threadnewid
			if threadnewidcount=1 then
				print "count=1 so exit br_stop"
				threadnewid=0
				threadnewidcount=0
				runtype=RTSTEP
				thread(threadcur).sts=KTHD_STOP
				exec_order(KPT_EXIT)
			else
				'' check if a signal is pending (maybe  a SIGTRAP for the new thread)
				print "count=";threadnewidcount,"check signal pending"
				exec_order(KPT_SIGNAL)
				if statussaved then ''exit br_stop for allowing waitpid
					print "signal pending exit br_stop"
					exec_order(KPT_EXIT)
				else
					print "no signal pending so new thread out of scope"
					threadnewid=0 ''no signal pending so new thread out of scope
					threadnewidcount=0
					thread(threadnb).sts=KTHD_OUT
					If runtype=RTAUTO Then
						print "auto resume all 00"
						thread_resume()
					End If
				end if
			end if
		else
			print "new thread threadnewidcount=";threadnewidcount
			exec_order(KPT_SIGNAL)
			if statussaved then ''exit br_stop for allowing waitpid
				print "signal pending exit br_stop"
				exec_order(KPT_EXIT)
			else
				If runtype=RTAUTO Then
					print "auto resume all 01"
					thread_resume()
				End If
			end if
			threadnewid=0
			threadnewidcount=0
			thread(threadnb).sts=KTHD_STOP
		EndIf
	else
		''when there are many signals from different threads

		exec_order(KPT_SIGNAL)
		if statussaved then ''exit br_stop for allowing waitpid
			print "OTHER signal pending exit br_stop=";hex(statussaved)
			exec_order(KPT_EXIT)
		else
			If runtype=RTAUTO Then
				print "auto resume all 02"
				thread_resume()
			elseif runtype=RTSTEP and flaghalt then
				flaghalt=false
				print "step resume all"
				thread_resume()
			elseif runtype=RTRUN Then
				print "rtrun changed in rtstep (no more signal)"
				runtype=RTSTEP
			End If
		end if
   End If
End Sub
