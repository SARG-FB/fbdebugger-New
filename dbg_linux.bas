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

#define SIG_BLOCK 0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2

#define SYS_TKILL 238
#define SYS_TGKILL 270
#define SYS_GETTID 224

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
  si_signo As Integer
  si_errno As Integer
  si_code As Integer
  Union
   _pad(29) As Integer '32bit -->29 / 64bit --> 28
   
   /' kill() '/
   Type 
   si_pid As Integer		/' sender s pid '/
   si_uid As Integer		/' sender s uid '/
   End Type
   /' SIGCHLD '/
   Type
   _pid As Integer		/' which child '/
   _uid As Integer		/' sender s uid '/
   _status As Integer		/' exit code '/
   _utime As Integer
   _stime As Integer
   End Type

   /' SIGILL, SIGFPE, SIGSEGV, SIGBUS SIGTRAP'/
   Type
   _addr As Integer /' faulting insn/memory ref. '/
   End Type 
    
  End Union
End Type
Type tsigaction
   Union
   sa_handler As Sub (As Integer)
   sa_sigaction As Sub (As Integer,As tsiginfo Ptr,As Any Ptr)
   End Union
   Union
   sa_mask As UInteger '128 bytes long
   padding(31) As long
   End Union
   sa_flags As uInteger
   sa_restorer As Sub()
End Type

'declare function getpid alias "getpid"() as Integer
'declare sub execve alias "execve"(as zstring ptr,as zstring ptr,as zstring ptr)
#undef Wait
'#undef Kill
'declare function fork alias "fork"() as Integer
declare function wait alias "wait"(as any ptr) as Integer
'declare function pause alias "pause"() as Integer
Declare function linux_kill alias "kill"(as integer,as integer) as Integer
declare function tkill alias "tkill"(as integer,as integer) as Integer
'declare function syscall cdecl  alias "syscall"(as integer,...) as Integer
declare function signal alias "signal"(as integer,as Integer ptr) as Integer
declare function pthread_kill alias "pthread_kill"(as integer,as integer) as Integer
'declare function perror alias "perror"(as integer)as zstring Ptr
declare sub putbreaks
declare function sigpending alias "sigpending" (as any ptr) as Integer
'declare function __errno_location alias "__errno_location"()  as integer Ptr
declare function sigprocmask alias "sigprocmask"(as integer,as any ptr,as any ptr) as Integer
'#define errno *__errno_location()
Declare function sigaction Alias "sigaction"(As Integer, As tsigaction Ptr, As tsigaction Ptr) as integer
declare function sigemptyset   alias "sigemptyset"(As Integer ptr) as Integer


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
'========================
'' open/close a console
'========================
'flaglog=0 --> no output / 1--> only screen / 2-->only file / 3 --> both
private sub output_lnx(txt As String)
	messbox("Feature missing","open/close console : ouput_lnx")
	print txt
end sub

'=====================================================
Private sub thread_rsm() ''TODO write this proc for Linux and remove in fbdebg_win
	''TODO
	''WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@rLine(thread(threadcur).sv).sv,1,0) 'restore old value for execution
	''resumethread(threadhs)
	'IupMessage("Linux version","Thread resume to be written")
	ptrace( PTRACE_CONT, debugpid, NULL, NULL )

	End Sub
'================================================
'' starts debugge for windows
'================================================
private function start_pgm() as long
	dim as pid_t pid=fork()
	dim as long status
	Select Case pid
		case -1 'error child not created
			Print "error child not created"'"errno = ";errno 'use perror("fork") ?
			return 0
		Case 0 'child created now the debuggee is going to be started
			debugpid=getpid
			Print "Debuggee pid = ",debugpid,syscall(SYS_GETTID)
			if ptrace(PTRACE_TRACEME, 0, 0,0) then 
				Print "error traceme"'"errno = ";errno 'use perror("ptrace")
	    		end
			EndIf
			print "name", exename
			''exename="/home/laurent/test" ''TODO remove this line used for testing
			sleep 500
			if execve(strptr(exename),NULL,NULL) then print "error on execve" ' argv, envp)=-1 
		Case Else
			debugpid=pid
			print "debugpid in scope debugger =";pid
			Print "Debugger pid (+thread) = ";getpid,syscall(SYS_GETTID)
			ptrace(PTRACE_SETOPTIONS, pid, 0, Cast(Any Ptr,PTRACE_O_TRACESYSGOOD)) ''todo add for clone/fork/etc for trapping thread creation
			prun=true:runtype=RTSTEP
			debugevent=KDBGCREATEPROC
			print "sortie du launch"
			'return 0
			''waiting loop, exit when debuggee terminated
			'====================
			wait(@status)
			''handle status
			while 1
				if WIFEXITED(status) then
					print "normal exit with status=";WEXITSTATUS(status)
					exit while
				elseif WIFSIGNALED(status) then
					print "Killed by=";WTERMSIG(status)
					exit while
				elseif WIFSTOPPED(status) then
					print "signal stop=";WSTOPSIG(status)
					select case WSTOPSIG(status)
						case SIGTRAP
							if ssadr then
								''todo RESTORE CC
								ssadr=0
							end if
							ptrace(PTRACE_CONT,pid,0,0)
						case SIGSEGV
						
						case else
							print "Signal not handled=";WSTOPSIG(status)
					end select
					
					
				else
					print "Status not handled=";status
				end if
			wend 
	End Select
end function

'===============
'GESTION DES EREURS A RAJOUTER todo si return values of ptrace =-1 alors tester errno si <>0 et rd (adresse pour nb bytes lus)
'===============================================================
'ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@adr,4,0)
private function ReadProcessMemory(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As any ptr=0) as integer
	Dim As integer integersize=SizeOf(Integer),i,j=lg\integersize
	Dim As integer buf(j)
	If lg<=integersize Then
		buf(0)=ptrace(PTRACE_PEEKDATA,child, addrdbge,NULL)
		memcpy(addrdbgr,@buf(0),lg)
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
	return 1 ''return 0 if error
End function
'===============================================================
'WriteProcessMemory(dbghand,Cast(LPVOID,rline(linenb).ad),@rLine(linenb).sv,1,0)
private Sub writeprocessmemory(child As long,addrdbge As any ptr,addrdbgr As any ptr,lg As Long,rd As Long=0)
	Dim As integer integersize=SizeOf(Integer),i,j=lg\integersize
	Dim As Integer buf,errn
	
	If lg<=integersize Then
		buf=ptrace(PTRACE_PEEKDATA,child, addrdbge,NULL) 'read current value
		memcpy(@buf,addrdbgr,lg) 'copy just bytes needed
		errn=ptrace(PTRACE_POKEDATA,child, addrdbge,NULL)'write all
		Exit sub
	EndIf
	
	While i<j
		memcpy(@buf,addrdbgr,integersize)
		errn=ptrace(PTRACE_POKEDATA,child, addrdbge,NULL)
		i+=1
		addrdbge+=integersize
		addrdbgr+=integersize
	Wend
	
	j=lg Mod integersize
	If j Then
		buf=ptrace(PTRACE_PEEKDATA,child, addrdbge,NULL) 'read current value
		memcpy(@buf,addrdbgr,j) 'copy just bytes needed
		errn=ptrace(PTRACE_POKEDATA,child, addrdbge,NULL)'write all
	EndIf
End Sub

'=============================================================
private sub findadr() ''used ????
''-------------
	dim as integer xip
	print "regs recup111 ";ptrace(PTRACE_GETREGS, debugpid, NULL, @regs)
	''address where really stopped, current was pointed to next instruction
	#ifdef __fb_64bit__
		xip=regs.xip-1
	#Else
		xip=regs.eip-1
	#endif
	print "xip=";xip
	For jline As Long=1 To linenb
		print"searching ",rline(jline).ad
		if rline(jline).ad=xip then
			addr=rline(jline).ad
			print "line found idx,number,addr = ";jline,rline(jline).nu,hex(addr)
			exit for
		endif
	next   
end sub
''===================================================================
private sub putbreaks()
''-------------
	dim dta as integer 'ALWAYS INTEGER 4 or 8 bytes
	dim as integer child=debugpid
	print"in put breaks",linenb
	For j As Long=1 To linenb
		With rline(j)
		Print "nu=";.nu;" ad=";.ad;"/";hex(.ad),
		If proc(.px).nu=-1 Then 
			Print " never reached added by compiler" 
		Else
			print
			errno=0
			dta = ptrace(PTRACE_PEEKTEXT,child,cast(any ptr,.ad),null)
			if errno<>0 then print "error"
			.sv=dta and &hFF
			print "Insert CC old value=";hex(dta), "sav=",.sv
			dta = (dta and &hFFFFFFFFFFFFFF00 ) or &hCC
			print "write, if value =-1  --> error ";ptrace(PTRACE_POKETEXT,child,cast(any ptr,.ad),cast(any ptr,dta));" ";hex(.ad);" ";,hex(dta)
			' dta = ptrace(PTRACE_PEEKTEXT,child,.ad,null)
			'print "MAJ=";dta
		EndIf

		End With
	Next
	print "first ptrace sending continue to debuggee ",ptrace( PTRACE_CONT, child, NULL, NULL )
end Sub
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
messbox("singlestep linux","To be coded")
    'For i As Integer =0 To threadnb
		'If tid=thread(i).id Then
			'threadcontext=thread(i).hd
'
			''get context
			'vcontext.contextflags=CONTEXT_CONTROL
			'GetThreadContext(threadcontext,@vcontext)
'
			'if running then ''when not running initial code is already restored and no need to decrease EIP
				'''restore initial code
				'WriteProcessMemory(dbghand,Cast(LPVOID,rline(rln).ad),@rLine(rln).sv,1,0)
				'''change EIP go back 1 byte
				'vcontext.regip-=1
			'end if
'
			'''set Trace flag on
			'vcontext.eflags=bitset(vcontext.eflags,8)
			'''update context
			'setThreadContext(threadcontext,@vcontext)
			'''rline for restoring BP
			'ssadr=rline(rln).ad
			'exit sub
		'End If
	'Next
End Sub