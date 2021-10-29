''Linux system for fbdebugger_new
''dbg_linux.bi
''==================TO BE MOVED IN DBG_DEFINES============================
'======================== for LINUX =========================
#INCLUDE "crt.bi"
#INCLUDE "crt/linux/unistd.bi"
#INCLUDE "crt/linux/fcntl.bi"

#define SIGINT 2
#define SIGTRAP 5
#define SIGABRT 6
#define SIGKILL 9
#define SIGSEGV 11
#define SIGUSR1 10
#define SIGUSR2 12
#define SIGTERM 15
#define SIGCHILD 17
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


#define SIG_BLOCK 0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2


#ifdef __fb_64bit__
	#define SYS_TKILL 238
	#define SYS_TGKILL 234'270
	#define SYS_GETTID 186'224
#else
	#define SYS_TKILL 200
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


Type tsiginfo
  si_signo As long
  si_errno As long
  si_code As long
  Union
  as byte pad(112)
   /' kill() '/
   si_pid As long		/' sender s pid '/
   si_uid As long		/' sender s uid '/
   /' SIGCHLD '/
   _pid As long		/' which child '/
   _uid As long		/' sender s uid '/
   _status As long		/' exit code '/
   _utime As Integer
   _stime As Integer


   /' SIGILL, SIGFPE, SIGSEGV, SIGBUS SIGTRAP'/
   Type
   _addr As Integer /' faulting insn/memory ref. '/
   End Type 
       '_pad(29) As long '32bit -->29 / 64bit --> 28
  End Union
  
End Type

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
declare function wait alias "wait"(as any ptr) as Integer
'declare function pause alias "pause"() as Integer

declare function tkill alias "tkill"(as integer,as integer) as Integer
'declare function syscall cdecl  alias "syscall"(as integer,...) as Integer
declare function signal alias "signal"(as integer,as Integer ptr) as Integer

'declare function perror alias "perror"(as integer)as zstring Ptr
declare sub putbreaks
declare function sigpending alias "sigpending" (as any ptr) as Integer
'declare function __errno_location alias "__errno_location"()  as integer Ptr
declare function sigprocmask alias "sigprocmask"(as integer,as any ptr,as any ptr) as Integer
'#define errno *__errno_location()
Declare function sigaction Alias "sigaction"(As long, As tsigaction Ptr, As tsigaction Ptr) as integer
declare function sigemptyset   alias "sigemptyset"(As sigset ptr) as long

private sub sig_handler(signum as long,siginfo as tsiginfo ptr,dummy as any ptr)
	dim as INTEGER dta,restall
	print "stop running debuggee"
	linux_kill(thread(threadcur).id,SIGSTOP)
	sleep 500 'waiting 1 second to be sure debuggee is stopped
	print "restoring saved byte on every line",thread(threadcur).id
	'runtype=RTSTEP
	For ilin As Integer = 1 To linenb 'restore every breakpoint
		dta = ptrace(PTRACE_PEEKTEXT,thread(threadcur).id,cast(any ptr,rline(ilin).ad),null)
		dta = (dta and FIRSTBYTE ) or &hCC
		restall=ptrace(PTRACE_POKETEXT,thread(threadcur).id,cast(any ptr,rline(ilin).ad),cast(any ptr,dta))
		if restall then print "errno=";errno
	Next
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
sigcode(SIGCHILD)="SIGCHILD"
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
Sub SetExeFilterRequester(chooser As GtkFileChooser Ptr  , sName As String)
    Dim As GtkFileFilter Ptr filter
    filter = gtk_file_filter_new ()
    gtk_file_filter_add_custom(filter,GTK_FILE_FILTER_MIME_TYPE, @FileFilterFunc, 0,0)
    gtk_file_filter_set_name(filter , Strptr(sName))
    gtk_file_chooser_add_filter (chooser, filter)
End Sub
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
private sub findadr() ''used ????
''-------------
	dim as integer xip
	ptrace(PTRACE_GETREGS, debugpid, NULL, @regs)
	''address where really stopped, current was pointed to next instruction
	xip=regs.xip-1
	For jline As Long=1 To linenb
		'print"searching ",rline(jline).ad
		if rline(jline).ad=xip then
			addr=rline(jline).ad
			print "line found idx,number,addr = ";jline,rline(jline).nu,hex(addr)
			exit sub
		endif
	next
	print "line not found"
end sub
'=========================================
private function ReadProcessMemory(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr=0) as integer
	msgad=cast(integer,addrdbge)
	msgad2=cast(integer,addrdbgr)
	msgdata=lg
	'print "in readmem before blocker=";hex(msgad),hex(msgad2),lg
	mutexlock blocker
	msgcmd=KPT_READMEM
	bool2=true
	condsignal(condid)
	while bool1<>true
		condwait(condid,blocker)
	wend
	bool1=false
	mutexunlock blocker
	return 1
End Function
'=========================================
private function WriteProcessMemory(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr=0) as integer
	msgad=cast(integer,addrdbge)
	msgad2=cast(integer,addrdbgr)
	msgdata=lg
	'print "in readmem before blocker=";hex(msgad),hex(msgad2),lg
	mutexlock blocker
	msgcmd=KPT_WRITEMEM
	bool2=true
	condsignal(condid)
	while bool1<>true
		condwait(condid,blocker)
	wend
	bool1=false
	mutexunlock blocker
	return 1
End Function
'===============================================================
private function writeprocessmemory_th2(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr=0)as integer
	Dim As integer integersize=SizeOf(Integer),i,j=lg\integersize
	Dim As Integer buf,errn
	If lg<=integersize Then
		buf=ptrace(PTRACE_PEEKDATA,child, addrdbge,NULL) 'read current value
		memcpy(@buf,addrdbgr,lg) 'copy just bytes needed
		errn=ptrace(PTRACE_POKEDATA,child, addrdbge,cast(any ptr,buf)) 'write all
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
''puts the breakpionts on all lines saving previous value
'========================================================
private sub putbreaks()
''-------------
	dim dta as integer 'ALWAYS INTEGER 4 or 8 bytes
	dim as integer child=debugpid
	print"in put breaks",linenb,"child=";child
	For j As Long=1 To linenb
		With rline(j)
			'Print "nu=";.nu;" ad=";.ad;"/";hex(.ad),
			If proc(.px).nu=-1 Then 
				Print " never reached added by compiler" 
			Else
				'print
				errno=0
				dta = ptrace(PTRACE_PEEKTEXT,child,cast(any ptr,.ad),null)
				if errno<>0 then print "error=";errno
				.sv=dta and &hFF
				dta = (dta and FIRSTBYTE ) or &hCC
				ptrace(PTRACE_POKETEXT,child,cast(any ptr,.ad),cast(any ptr,dta))
			EndIf
		End With
	Next
end Sub
'==========================================
''puts(default=1)/removes(0) the breakpoints on all lines
'==========================================
private sub putremove_breaks(byval typ as integer=1)
''-------------
	dim dta as integer 'ALWAYS INTEGER 4 or 8 bytes
	dim as integer child=debugpid
	print"in put/remove breaks",linenb,"child=";child
	if typ then 
		For j As Long=1 To linenb
			With rline(j)
			If proc(.px).nu=-1 Then 
				Print " never reached added by compiler" 
			Else
				dta = ptrace(PTRACE_PEEKTEXT,child,cast(any ptr,.ad),null)
				dta = (dta and FIRSTBYTE ) or &hCC
				ptrace(PTRACE_POKETEXT,child,cast(any ptr,.ad),cast(any ptr,dta))
			EndIf
			End With
		Next
	else
		For j As Long=1 To linenb
			With rline(j)
				dta = ptrace(PTRACE_PEEKTEXT,child,cast(any ptr,.ad),null)
				dta = (dta and FIRSTBYTE ) or rline(j).sv
				ptrace(PTRACE_POKETEXT,child,cast(any ptr,.ad),cast(any ptr,dta))
			End With
		Next
	end if
end Sub
'=====================================================
Private sub thread_rsm()
	msgad=rLine(thread(threadcur).sv).ad
	msgdata=rLine(thread(threadcur).sv).sv and &hFF
	print "in thread_rsm restore=";thread(threadcur).sv,rLine(thread(threadcur).sv).nu,hex(rLine(thread(threadcur).sv).sv)
	mutexlock blocker
	msgcmd=KPT_RESTORE
	bool2=true
	condsignal(condid)
	while bool1<>true
		condwait(condid,blocker)
	wend
	bool1=false
	mutexunlock blocker
End Sub
'=====================================================
Private sub singlestep_rsm(rln as INTEGER)
	msgad=rLine(rln).ad
	msgdata=rLine(rln).sv
	print "in singlestep_rsm=";rln,hex(msgad),rLine(rln).nu
	mutexlock blocker
	msgcmd=KPT_SSTEP
	bool2=true
	condsignal(condid)
	while bool1<>true
		condwait(condid,blocker)
	wend
	bool1=false
	mutexunlock blocker
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
private sub brp_stop(tid as integer,bptype as integer,ddata as integer)
	dim as integer dta
	print "brp_stop=";tid,bptype,ddata
	For i As Integer =0 To threadnb
		If tid=thread(i).id Then
			threadcur=i
			stopcode=bptype
			debugdata=ddata
			debugevent=KDBGRKPOINT
			'===========
			while 1
				mutexlock blocker
				while bool2<>true
					condwait(condid,blocker)
				wend
				bool2=false
				select case as const msgcmd
					Case KPT_CONT
						print "cont"
						bool1=true
						condsignal(condid)
						mutexunlock blocker
						ptrace(PTRACE_CONT,thread(threadcur).id,0,0)									
						exit while
						
					Case KPT_XIP ''update EIP or RIP
						ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
						regs.xip=msgad
						ptrace(PTRACE_SETREGS, thread(threadcur).id, NULL, @regs)						
						
					Case KPT_CC
						dta = ptrace(PTRACE_PEEKTEXT,thread(threadcur).id,cast(any ptr,msgad),null)		
						dta = (dta and FIRSTBYTE ) or &hCC
						ptrace(PTRACE_POKETEXT,thread(threadcur).id,cast(any ptr,msgad),cast(any ptr,dta))
					
					case KPT_SSTEP
						dta = ptrace(PTRACE_PEEKTEXT,thread(threadcur).id,cast(any ptr,msgad),null)	
						dta = (dta and FIRSTBYTE ) or msgdata
						ptrace(PTRACE_POKETEXT,thread(threadcur).id,cast(any ptr,msgad),cast(any ptr,dta))					
						ssadr=msgad
						print "single step defined by KPT_SSTEP=";hex(ssadr)
						bool1=true
						condsignal(condid)
						mutexunlock blocker
						ptrace(PTRACE_SINGLESTEP, thread(threadcur).id, NULL, NULL)	
						exit while
						
					Case KPT_GETREGS
						ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
						
					Case KPT_SETREGS
						ptrace(PTRACE_SETREGS, thread(threadcur).id, NULL, @regs)
						
					Case KPT_READMEM
						ReadProcessMemory_th2(thread(threadcur).id,cast(any ptr,msgad),cast(any ptr,msgad2),msgdata)
						
					Case KPT_WRITEMEM
						WriteProcessMemory_th2(thread(threadcur).id,cast(any ptr,msgad),cast(any ptr,msgad2),msgdata)
						
					case KPT_RESTORE
						dta = ptrace(PTRACE_PEEKTEXT,thread(threadcur).id,cast(any ptr,msgad),null)	
						'print "data read=";hex(dta),hex((dta and FIRSTBYTE )),hex(msgdata)
						dta = (dta and FIRSTBYTE ) or msgdata
						print "KPT_RESTORE=";ptrace(PTRACE_POKETEXT,thread(threadcur).id,cast(any ptr,msgad),cast(any ptr,dta));" ";hex(msgad);" ";,hex(dta)
						bool1=true
						condsignal(condid)
						mutexunlock blocker
						ptrace( PTRACE_CONT, thread(threadcur).id, NULL, NULL )
						exit while
									
					case KPT_EXIT
						print "sending sigkill"
						linux_kill(debugpid,9) ''send SIGKILL
						bool1=true
						condsignal(condid)
						mutexunlock blocker
						prun=false
						exit while
					case else
						print "msgcmd not handled"
				End Select		
				bool1=true
				condsignal(condid)
				mutexunlock blocker
			Wend
			'===========
			exit sub
		End If
	Next
end sub
'================================================
'' starts debuggee for linux
'================================================
private sub start_pgm(p As Any Ptr)
	thread2=syscall(SYS_gettid)
	if thread2=-1 then print "errno=";errno
	print "current pid=";thread2
	dim as pid_t pid=fork()

	print "pid=";pid,"size=";sizeof(pid_t)
	dim as long status
	dim as INTEGER	firsttime,dta,bptyp
	Select Case pid
		case -1 'error child not created
			Print "error child not created"'"errno = ";errno 'use perror("fork") ?
			exit sub
		Case 0 'child created now the debuggee is going to be started
			debugpid=getpid
			Print "Debuggee pid = ",debugpid,syscall(SYS_gettid)
			if ptrace(PTRACE_TRACEME, 0, 0,0) then 
				Print "error traceme"'"errno = ";errno 'use perror("ptrace")
	    		exit sub
			EndIf
			print "name", exename
			if execve(strptr(exename),NULL,NULL) then
				print "error on execve" ' argv, envp)=-1 
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
			ptrace(PTRACE_SETOPTIONS, pid, 0, Cast(Any Ptr,PTRACE_O_TRACESYSGOOD)) ''todo add for clone/fork/etc for trapping thread creation
			prun=true:runtype=RTSTEP

			'print ptrace(PTRACE_GETREGS, debugpid, NULL, @regs)
			'perror(@"test")
			print "entering in loop"
			while 1
				''waiting loop, exit when debuggee terminated
				'====================
				wait(@status)
				''handle status
				if WIFEXITED(status) then
					print "normal exit with status=";WEXITSTATUS(status)
					prun=false
					afterkilled=KDONOTHING
					debugdata=WEXITSTATUS(status)
					debugevent=KDBGEXITPROC
					exit while
				elseif WIFSIGNALED(status) then
					prun=false
					print "Killed by=";WTERMSIG(status)
					debugdata=WTERMSIG(status)
					debugevent=KDBGEXITPROC
					exit while
				elseif WIFSTOPPED(status) then
					print "signal stop=";WSTOPSIG(status)
					select case WSTOPSIG(status)
						case SIGTRAP
							if firsttime=0 then
								print "first time so putbreaks and ptrace_cont",debugpid,pid
								threadcur=0
								thread(threadcur).id=debugpid
								print "thread(threadcur).id=";thread(threadcur).id
								print "get regs in first=";ptrace(PTRACE_GETREGS, thread(threadcur).id, NULL, @regs)
								print "xip=";hex(regs.xip)
								'debugdata=Cast(Integer,.lpBaseOfImage)
								debugevent=KDBGCREATEPROC
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
								ptrace(PTRACE_CONT,pid,0,0)
							'Else
							'==========================================================================
					'=========================
					' BREAKPOINT
					'=========================
							else
								print "------------------------------------------------------------------------------------------"
								print "EXCEPTION_BREAKPOINT"
								
								if ssadr<>0 then ''a single step has been executed
									print "handling single step"
									dta = ptrace(PTRACE_PEEKTEXT,thread(threadcur).id,cast(any ptr,ssadr),null)	
									'print "data read=";hex(dta),hex((dta and FIRSTBYTE )),hex(msgdata)
									dta = (dta and FIRSTBYTE ) or &hCC
									ptrace(PTRACE_POKETEXT,thread(threadcur).id,cast(any ptr,ssadr),cast(any ptr,dta))
									ssadr=0
									ptrace(PTRACE_CONT,thread(threadcur).id,0,0)
								else
									dim as integer xip, bpidx=-1
									ptrace(PTRACE_GETREGS, debugpid, NULL, @regs)
									''address where really stopped, current was pointed to next instruction

									xip=regs.xip-1
									print "xip=";hex(xip),runtype,"RTRUN,RTSTEP=";RTRUN,RTSTEP
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
													brp_stop(thread(threadcur).id,CSMEM,xip)
												else
													brp_stop(thread(threadcur).id,CSVAR,xip)
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
												brp_stop(thread(threadcur).id,CSLINE,bpidx)
												continue while
											end if
											bptyp=brkol(bpidx).typ
											if bptyp=2 or bptyp=3 then  ''BP conditional
												if brk_test(brkol(bpidx).adrvar1,brkol(bpidx).adrvar2,brkol(bpidx).datatype,brkol(bpidx).val,brkol(bpidx).ttb) then
													brp_stop(thread(threadcur).id,CSCOND,bpidx)
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
													brp_stop(thread(threadcur).id,CSCOUNT,bpidx)
												end if
												continue while
											else
												''simple BP (perm/tempo)
												brp_stop(thread(threadcur).id,CSBRKPT,bpidx)
												continue while
											end if
											if stopcode=CSUSER then
												brp_stop(thread(threadcur).id,stopcode,xip)
												continue while
											end if
										else
											''case halt by user so no breakpoint and stopcode should be CSUSER
											''continue waiting to a normal breakpoint
											ptrace(PTRACE_CONT,thread(threadcur).id,0,0)
											runtype=RTSTEP
											continue while
										end if

									else ''RTSTEP/RTAUTO
										if stopcode=CSUSER then ''CSUSER : user halts the debuggee
											brp_stop(thread(threadcur).id,stopcode,xip)
										else
											brp_stop(thread(threadcur).id,CSSTEP,xip)
										end if
										continue while
									end if
								end if
							end if
						case SIGSEGV
						print "sigsegv"
						case else
							print "other Signals=";WSTOPSIG(status)
							if WSTOPSIG(status)=SIGSTOP then
									'print "release stopped debuggee=";linux_kill(thread(threadcur).id,SIGCONT)
									'PRINT "CONT2=";ptrace(PTRACE_CONT,pid,0,0)
									messbox("SIGSTOP","should not happen")
							elseIf WSTOPSIG(status)=SIGCONT then
								messbox("SIGCONT","should not happen")
							End If
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

	dbg_prt2("")
	dbg_prt2("AD gest brk="+hex(ad)+" th="+Str(threadcur))
	'show_context

	proccurad=ad

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
'' ========================= move in step/stepauto ???
	''restore CC previous line
	If thread(threadcur).sv<>-1 Then
		msgad=rLine(thread(threadcur).sv).ad
		print "restoring CC01 ad=";hex(msgad)
		mutexlock blocker
		msgcmd=KPT_CC
		bool2=true
		condsignal(condid)
		while bool1<>true
			condwait(condid,blocker)
		wend
		bool1=false
		mutexunlock blocker
	EndIf
   ''thread changed by threadcreate or by mutexunlock
	If threadcur<>threadprv Then
		If thread(threadprv).sv<>i Then 'don't do it if same line otherwise all is blocked.....not sure it's usefull
			msgad=rline(thread(threadprv).sv).ad
			print "restoring CC01 ad=";hex(msgad)
			mutexlock blocker
			msgcmd=KPT_CC
			bool2=true
			condsignal(condid)
			while bool1<>true
				condwait(condid,blocker)
			wend
			bool1=false
			mutexunlock blocker
		EndIf
		stopcode=CSNEWTHRD  'status HALT NEW THREAD
		runtype=RTSTEP
		thread_text(threadprv) 'not next executed
		thread_text(threadcur) 'next executed
		threadprv=threadcur
	EndIf

	thread(threadcur).od=thread(threadcur).sv:thread(threadcur).sv=rln
	procsv=rline(rln).px
	'dbg_prt2("proc ="+Str(procsv)+" "+proc(procsv).nm+" "+hex(proc(procsv).db)+" "+source(proc(procsv).sr)+" "+hex(proccurad))
	'dbg_prt2("line="+Str(rline(i).nu))

	'get registers
	mutexlock blocker
	msgcmd=KPT_GETREGS
	bool2=true
	condsignal(condid)
	'print "gest_brk after condsignal"
	'print "gest_brk after mutexlock"
	while bool1<>true
		condwait(condid,blocker)
	wend
	bool1=false
	'print "gest_brk after condwait"
	mutexunlock blocker
	print "gest_brk xip=";hex(regs.xip),time

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
				procsk=regs.xsp-20 ''todo check value 20 should not be correct if 64bit
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
	EndIf
	regs.xip=ad
	mutexlock blocker
	msgcmd=KPT_SETREGS
	bool2=true
	condsignal(condid)
	while bool1<>true
		condwait(condid,blocker)
	wend
	bool1=false
	mutexunlock blocker
	'dbg_prt2("PE"+Str(thread(threadcur).pe)+" "+Str(proccurad)+" "+Str(proc(procsv).fn))
	If thread(threadcur).pe Then 'if previous instruction was the last of proc
		If proccurad<>proc(procsv).db Then 'reload procsk with rbp/ebp test added for case constructor on shared
			procsk=regs.xbp
		EndIf
		proc_end():thread(threadcur).pe=FALSE
	EndIf
	
	If proccurad=proc(procsv).db Then 'is first proc instruction
		thread_resume():Exit Sub
	end if
	

''=========== end of code to move ========================================================
	If runtype=RTRUN Then
   		fasttimer=Timer-fasttimer
		''==== useful ?? ===============
	  	For i As Integer = 1 To linenb 'restore CC
   			WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@breakcpu,1,0)
	  	Next
		''==== end of code ===============
		WriteProcessMemory(dbghand,Cast(LPVOID,rLine(rln).ad),@rLine(rln).sv,1,0) 'restore old value for execution
		'if rln<>rLine(brkol(0).index then ''case BP cond/etc and run to cursor/EOP/XOP
			'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(brkol(0).index).ad),@rLine(brkol(0).index).sv,1,0) 'restore old value for execution
		'end if
   	''?????	brk_test(proccurad) ' cancel breakpoint on line, if command halt not really used

		if brkol(0).typ<>10 then ''for skip over always in same proc, if different thread ???
			proc_runnew   'creating running proc tree
		end if
   		var_sh			'updating information about variables
   		runtype=RTSTEP
   		dsp_change(rln)
		if stopcode=CSLINE then
			brk_del(0)
		elseif stopcode=CSBRKPT then
			for ibrk as INTEGER	= 1 to brknb
				if brkol(ibrk).index=rln then
					if brkol(ibrk).typ=6 then
						brk_del(ibrk) ''remove tempo BP
						exit for
					EndIf
				EndIf
			Next
		EndIf
   Else 'RTSTEP or RTAUTO
		If flagattach Then 
			proc_runnew
			flagattach=FALSE
		else
			If proccurad=proc(procsv).first Then 'is first fbc instruction ?
				print "going to proc_new"
				proc_new()
				'thread_resume():Exit Sub
			ElseIf proccurad=proc(procsv).fn Then
				thread(threadcur).pe=TRUE        'is last instruction ?
			EndIf
		EndIf
		'NOTA If rline(i).nu=-1 Then
			'fb_message("No line for this proc","Code added by compiler (constructor,...)")
		'Else
		dsp_change(rln)
		'EndIf
		If runtype=RTAUTO Then
			Sleep(autostep)
			If threadaut>1 Then 'at least 2 threads
				Dim As Integer c=threadcur
				Do
					c+=1:If c>threadnb Then c=0
				Loop Until thread(c).exc
				thread_change(c)
			EndIf
			thread_resume()
		EndIf
		If threadsel<>threadcur AndAlso messbox("New Thread","Previous thread "+Str(thread(threadsel).id)+" changed by "+Str(thread(threadcur).id) _
				+Chr(10)+Chr(13)+" Keep new one ?",MB_YESNO)=IDNO Then
				thread_change(threadsel)
		Else
			threadsel=threadcur
		EndIf
   End If
End Sub

