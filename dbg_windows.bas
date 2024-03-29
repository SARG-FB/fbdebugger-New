''Windows system for fbdebugger_new
''dbg_windows.bas

'====================================================
'' starts debuggee for windows
'====================================================
private sub start_pgm(p As Any Ptr)
		Dim  As Integer pclass,st
		Dim  As String workdir,cmdl
		Dim sinfo As STARTUPINFO
		'directory
		st=0
		While InStr(st+1,exename,"\")
		   st=InStr(st+1,exename,"\")
		Wend
		workdir=Left(exename,st)

		if cmdlimmediat<>"" then
			cmdl=""""+exename+""" "+cmdlimmediat
			cmdlimmediat=""
		else
			cmdl=""""+exename+""" "+cmdexe(0)
		end if

		#Ifdef fulldbg_prt
			dbg_prt (Date+" "+Time+"Start Debug with "+cmdl)
		#EndIf
		sinfo.cb = Len(sinfo)
		'Set the flags
		sinfo.dwFlags = STARTF_USESHOWWINDOW
		'Set the window's startup position
		sinfo.wShowWindow = SW_NORMAL
		'Set the priority class
		pclass = NORMAL_PRIORITY_CLASS Or CREATE_NEW_CONSOLE Or DEBUG_PROCESS Or DEBUG_ONLY_THIS_PROCESS
		'Start the program
		If CreateProcess(exename,StrPtr(cmdl),ByVal NULL,ByVal NULL, FALSE, pclass, _
		NULL, WorkDir, @sinfo, @pinfo) Then
			'Wait
			WaitForSingleObject pinfo.hProcess, 10
			dbgprocId=pinfo.dwProcessId
			dbgthreadID=pinfo.dwThreadId
			dbghand=pinfo.hProcess
			dbghthread=pinfo.hThread
			#Ifdef fulldbg_prt
				dbg_prt ("Create process")
				dbg_prt ("pinfo.hThread "+Str(pinfo.hThread))
				dbg_prt ("pinfo.dwThreadId "+Str(pinfo.dwThreadId))
				dbg_prt ("hand "+Str(dbghand)+" Pid "+Str(dbgprocid))
			#EndIf
			prun=TRUE
			runtype=RTSTEP
			wait_debug()
		Else
			messbox("PROBLEM","no debugged pgm -->"+exename+Chr(10)+"error :"+Str(GetLastError()),MB_SYSTEMMODAL)
	   End If
End Sub
'===================================================
'' Loads dll elements
'===================================================
private sub dll_load()
	Dim loaddll As LOAD_DLL_DEBUG_INFO=*cast(LOAD_DLL_DEBUG_INFO ptr,debugdata) '' copy of data from thread 2
	Dim As String dllfn
	Dim As Integer d,delta,srcstart

	dllfn=dll_name(loaddll.hFile)
	'check yet loaded
	For i As Integer= 1 To dllnb
		If dllfn=dlldata(i).fnm Then d=i:Exit For
	Next

	If d=0 Then 'not found
		If dllnb>=DLLMAX Then 'limit reached
			hard_closing("New dll, Number of dll ("+Str(DLLMAX)+") exceeded , change the DLLMAX value."+Chr(10)+Chr(10)+"CLOSING FBDEBUGGER, SORRY")
		EndIf
		dllnb+=1
		dlldata(dllnb).hdl=loaddll.hfile
		dlldata(dllnb).bse=Cast(UInteger,loaddll.lpBaseOfDll)
		srcstart=sourcenb+1
		if debug_extract(Cast(UInteger,loaddll.lpBaseOfDll),dllfn,DLL)=-1 then
			dllnb-=1
			statusbar_text(KSTBSTS,"Running")
			exit sub
		end if
		If (linenb-linenbprev)=0 Then 'not debugged so not taking in account
			dllnb-=1
		Else
			ResetAllComboBox(GFILELIST) ''as combobox completely refilled in init_debuggee
			init_debuggee(srcstart)

			dlldata(dllnb).fnm=dllfn
			dlldata(dllnb).gbln=vrbgbl-vrbgblprev
			dlldata(dllnb).gblb=vrbgblprev+1
			dlldata(dllnb).lnb=linenbprev+1
			dlldata(dllnb).lnn=linenb
		End If
	Else
		dlldata(d).hdl=loaddll.hfile
		delta=Cast(Integer,loaddll.lpBaseOfDll-dlldata(d).bse)
		If delta<>0 Then ''different address so need to change some thing
			''lines
			For i As Integer=dlldata(dllnb).lnb To dlldata(dllnb).lnb+dlldata(dllnb).lnb-1
				rline(i).ad+=delta
			Next
			''globals
			For i As Integer=dlldata(dllnb).gblb To dlldata(dllnb).gblb+dlldata(dllnb).gbln-1
				vrb(i).adr+=delta
			Next
		End If
		''normally done during debug_extract
		For i As Integer=dlldata(dllnb).lnb To dlldata(dllnb).lnb+dlldata(dllnb).lnb-1
			ReadProcessMemory(dbghand,Cast(LPCVOID,rline(i).ad),@rLine(i).sv,1,0) 'sav 1 byte before writing breakcpu
			WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@breakcpu,1,0)
		Next
		globals_load(d)
		brk_apply()
	EndIf
	statusbar_text(KSTBSTS,"Running")
end sub
'=====================================================================
''finds the dll name
'=====================================================================
private function dll_name(FileHandle As HANDLE,t As Integer =1 )As String ' t=1 --> full name, t=2 --> short name
   Dim As ZString*251 fileName
   Dim As ZString*512 zstr,dn,tzstr=" :"
   Dim As HANDLE hfileMap
   Dim As Long fileSizeHi,fileSizeLo,p
	Dim As Any Ptr pmem
	Dim As String tstring
   fileSizeLo = GetFileSize(FileHandle, @fileSizeHi)
   If fileSizeLo = 0 And fileSizeHi=0 Then Return "Empty file." ' cannot map an 0 byte file
   hfileMap = CreateFileMapping(FileHandle,0,PAGE_READONLY, 0, 1, NULL)
   If hfileMap Then
      pMem = MapViewOfFile(hfileMap,FILE_MAP_READ, 0, 0, 1)
      If pMem Then
      	GetMappedFileName(GetCurrentProcess(),pMem, @fileName, 250)
         UnmapViewOfFile(pMem)
         CloseHandle(hfileMap)
         If Len(fileName) > 0 Then
				getlogicaldrivestrings(511,zstr)'get all the device letters c:\ d:\ etc separate by null
				While zstr[p]
					tzstr[0]=zstr[p]'replace space by letter
					querydosdevice(tzstr,dn,511)'get corresponding device name
					If InStr(fileName,dn) Then
						tstring=fileName
						str_replace(tstring,dn,tzstr)
						If t=1 Then
							Return tstring 'full name
						Else
							Return source_name(tstring) ''extract only name without path
						EndIf
					EndIf
					p+=4'next letter skip ":\<null>"
				Wend
         Else
            Return "Empty filename."
         EndIf
      EndIf
   End If
   Return "Empty filemap handle."
End Function
'===============================
'' used with dbg_prt
'================================
private function CtrlHandler(fdwCtrlType As Integer) As Integer
    'PostMessage(windmain,WM_CLOSE,0,0)
	Return TRUE
End Function
'========================
'' open/close a console
'========================
'flaglog=0 --> no output / 1--> only screen / 2-->only file / 3 --> both
private sub output_wds(t As String)
	Static As HANDLE scrnnumber
	Static As Integer filenumber
	Dim cpt As long,libel As String
	Dim As COORD maxcoord
	Dim As CONSOLE_SCREEN_BUFFER_INFO csbi
	Dim As SMALL_RECT disparea=Type(0,0,0,0)
	Dim As Short maxcoordysav
	If t=" $$$$___CLOSE ALL___$$$$ " Then
		If scrnnumber<>0 And (flaglog And 1)=0 Then FreeConsole():scrnnumber=0
		If filenumber And (flaglog And 2)=0 Then Close filenumber:filenumber=0
		Exit Sub
	EndIf

	If scrnnumber=0 And (flaglog And 1) Then
		libel=Chr(13)+Chr(10)+Date+" "+Time+Chr(13)+Chr(10)
		AllocConsole()
		scrnnumber=GetStdHandle(STD_OUTPUT_HANDLE)
		setconsoletitle(StrPtr("FBdebugger trace/log"))
		maxcoord=GetLargestConsoleWindowSize(scrnnumber) 'max values displayed on screen
		maxcoordysav=maxcoord.y
		GetConsoleScreenBufferInfo(scrnnumber,@csbi) 'max values using buffer/screen/font

		'change buffer size, only width
		maxcoord.x*=.5 'reducing by 50% of max possible
		maxcoord.y=csbi.dwsize.y 'no change
		SetConsoleScreenBufferSize(scrnnumber,maxcoord)

		'change display,
		disparea.left=0
		disparea.top=0
		disparea.right=maxcoord.x-1
		disparea.bottom=maxcoordysav*.8 '80% of height max possible
		SetConsoleWindowInfo(scrnnumber,TRUE,@disparea)

		'align window on the left/top corner of screen
		SetWindowPos(GetConsoleWindow, 0, 10,10, 0, 0, SWP_NOSIZE Or SWP_NOZORDER)

		SetConsoleCtrlHandler(Cast(PHANDLER_ROUTINE,@CtrlHandler),TRUE)
		WriteConsole(scrnnumber, StrPtr(libel),Len(libel),@cpt,0)
	EndIf

	If filenumber=0 And (flaglog And 2) Then
		filenumber=FreeFile:Open ExePath+"\dbg_log_file.txt"  For Append As filenumber
		print #filenumber,Date,Time
	EndIf

	If (flaglog And 1) Then libel=t+Chr(13)+Chr(10):WriteConsole(scrnnumber, StrPtr(libel),Len(libel),@cpt,0)
	If (flaglog And 2) Then print # filenumber,t

End Sub
'=======================================================================
private function excep_lib(e As Integer) As String 'not managed exception
	Select Case e
		Case EXCEPTION_GUARD_PAGE_VIOLATION
			Return "EXCEPTION_GUARD_PAGE_VIOLATION"  '&H80000001
		Case EXCEPTION_DATATYPE_MISALIGNMENT
			Return "EXCEPTION_DATATYPE_MISALIGNMENT" '&H80000002
		Case EXCEPTION_SINGLE_STEP
			Return "EXCEPTION_SINGLE_STEP" '&H80000004
		Case EXCEPTION_ACCESS_VIOLATION
			Return "EXCEPTION_ACCESS_VIOLATION" '&HC0000005
		Case EXCEPTION_IN_PAGE_ERROR
			Return "EXCEPTION_IN_PAGE_ERROR" '&HC0000006
		Case EXCEPTION_INVALID_HANDLE
			Return "EXCEPTION_INVALID_HANDLE" '&HC0000008
		Case EXCEPTION_NO_MEMORY
			Return "EXCEPTION_NO_MEMORY" '&HC0000017
		Case EXCEPTION_ILLEGAL_INSTRUCTION
			Return "EXCEPTION_ILLEGAL_INSTRUCTION" '&HC000001D
		Case EXCEPTION_NONCONTINUABLE_EXCEPTION
			Return "EXCEPTION_NONCONTINUABLE_EXCEPTION" '&HC0000025
		Case EXCEPTION_INVALID_DISPOSITION
			Return "EXCEPTION_INVALID_DISPOSITION" '&HC0000026
		Case EXCEPTION_ARRAY_BOUNDS_EXCEEDED
			Return "EXCEPTION_ARRAY_BOUNDS_EXCEEDED" '&HC000008C
		Case EXCEPTION_FLOAT_DENORMAL_OPERAND
			Return "EXCEPTION_FLOAT_DENORMAL_OPERAND" '&HC000008D
		Case EXCEPTION_FLOAT_DIVIDE_BY_ZERO
			Return "EXCEPTION_FLOAT_DIVIDE_BY_ZERO" '&HC000008E
		Case EXCEPTION_FLOAT_INEXACT_RESULT
			Return "EXCEPTION_FLOAT_INEXACT_RESULT" '&HC000008F
		Case EXCEPTION_FLOAT_INVALID_OPERATION
			Return "EXCEPTION_FLOAT_INVALID_OPERATION" '&HC0000090
		Case EXCEPTION_FLOAT_OVERFLOW
			Return "EXCEPTION_FLOAT_OVERFLOW" '&HC0000091
		Case EXCEPTION_FLOAT_STACK_CHECK
			Return "EXCEPTION_FLOAT_STACK_CHECK" '&HC0000092
		Case EXCEPTION_FLOAT_UNDERFLOW
			Return "EXCEPTION_FLOAT_UNDERFLOW" '&HC0000093
		Case EXCEPTION_INTEGER_DIVIDE_BY_ZERO
			Return "EXCEPTION_INTEGER_DIVIDE_BY_ZERO" '&HC0000094
		Case EXCEPTION_INTEGER_OVERFLOW
			Return "EXCEPTION_INTEGER_OVERFLOW" '&HC0000095
		Case EXCEPTION_PRIVILEGED_INSTRUCTION
			Return "EXCEPTION_PRIVILEGED_INSTRUCTION" '&HC0000096
		Case EXCEPTION_STACK_OVERFLOW
			Return "EXCEPTION_STACK_OVERFLOW" '&HC00000FD
		Case EXCEPTION_CONTROL_C_EXIT
			Return "EXCEPTION_CONTROL_C_EXIT" '&HC000013A
		Case DBG_CONTROL_C
			Return "DBG_CONTROL_C" '&h40010005
		Case DBG_TERMINATE_PROCESS
			Return "DBG_TERMINATE_PROCESS" '&h40010004
		Case DBG_TERMINATE_THREAD
			Return "DBG_TERMINATE_THREAD"  '&h40010003
		Case DBG_CONTROL_BREAK
			Return "DBG_CONTROL_BREAK"  '&h40010008
		Case Else
			Return "Unknown Exception code (D/H)= "+Str(e)+" / "+Hex(e) '07/10/2014
	End Select
End Function
'======================================================
private sub debugstring_read(debugev As debug_event)
	Dim As WString *400 wstrg
	Dim As ZString *400 sstrg
	Dim leng As Integer
	If debugev.u.debugstring.nDebugStringLength<400 Then
		leng=debugev.u.debugstring.nDebugStringLength
	Else
		leng=400
	endif

	If debugev.u.debugstring.fUnicode Then
		ReadProcessMemory(dbghand,Cast(LPCVOID,debugev.u.debugstring.lpDebugStringData),_
		@wstrg,leng,0)
		'messagebox(0,wstrg,WStr("debug wstring"),MB_OK or MB_SYSTEMMODAL)
		'dbg_prt2 "debugstring=";wstrg
	else
		ReadProcessMemory(dbghand,Cast(LPCVOID,debugev.u.debugstring.lpDebugStringData),_
		@sstrg,leng,0)
		'messagebox(0,sstrg,@"debug string",MB_OK or MB_SYSTEMMODAL)
		'dbg_prt2 "debugstring=";sstrg
	endif

End Sub
'====================================================================
'' prepares singlestepping for restoring BP
'====================================================================
private sub singlestep_on(tid as integer,rln as integer,running as integer =1)
	dim as integer dummy ''used to align vcontext on 16bit
	Dim vcontext As CONTEXT
    For i As Integer =0 To threadnb
		If tid=thread(i).id Then
			threadcontext=thread(i).hd

			'get context
			vcontext.contextflags=CONTEXT_CONTROL
			GetThreadContext(threadcontext,@vcontext)

			if running then ''when not running initial code is already restored and no need to decrease EIP
				''restore initial code
				WriteProcessMemory(dbghand,Cast(LPVOID,rline(rln).ad),@rLine(rln).sv,1,0)
				''change EIP go back 1 byte
				vcontext.regip-=1
			end if

			''set Trace flag on
			vcontext.eflags=bitset(vcontext.eflags,8)
			''update context
			setThreadContext(threadcontext,@vcontext)
			''rline for restoring BP
			ssadr=rline(rln).ad
			exit sub
		End If
	Next
End Sub
'=====================================
private sub resume_exec()
	For jbrk As Integer = 1 To brknb ''restore if needed the UBP
		If brkol(jbrk).typ<50 Then
			if rlinecur=brkol(jbrk).index then ''if current line is a BP (permanent/cond/counter)
				singlestep_on(thread(threadcur).id,rlinecur,0)
				exit for
			EndIf
		end if
	Next
	thread_resume()
end sub
'====================================================================
private sub thread_search(tid as integer,bptype as integer,ddata as integer)
	For i As Integer =0 To threadnb
		If tid=thread(i).id Then
			threadcontext=thread(i).hd
			threadhs=threadcontext
			suspendthread(threadcontext)
			threadcur=i
			stopcode=bptype
			debugdata=ddata
			debugevent=KDBGRKPOINT
			if stopcode<>CSSTEP and thread(i).rtype=RTAUTO then
				dbg_prt2 "RTSTEP forced"
				'thread(i).rtype=RTSTEP ''in case user's BP and RTAUTO
				runtype=RTSTEP
			end if
			mutexlock blocker
			mutexunlock blocker
			exit sub
		End If
	Next
end sub
'=====================================
'' Set breakpoint (CC) for every line
'=====================================
private sub set_cc()
	if ccstate=KCC_NONE then ''only done if needed
		For iline As Integer = 1 To linenb
			if proc(rline(iline).px).enab = true then
				WriteProcessMemory(dbghand,Cast(LPVOID,rline(iline).ad),@breakcpu,1,0)
			end if
		Next
		ccstate=KCC_ALL
	endif
End Sub
'========================================================
''  handles breakpoints
'========================================================
private sub gest_brk(ad As Integer,byval rln as integer =-1)

	dim as integer dummy
	Dim vcontext As CONTEXT
	vcontext.contextflags=CONTEXT_CONTROL or CONTEXT_INTEGER
	Dim As Integer i,debut=1,fin=linenb+1,adr,iold
   'egality added in case attach (example access violation) without -g option, ad=procfn=0....
	If ad>=procfn Then
		dbg_prt2 __FUNCTION__,__LINE__,thread(threadcur).sts,thread(threadcur).rtype
		thread_resume()
		Exit Sub
	EndIf

	dbg_prt2("")
	dbg_prt2 "AD gest brk="+hex(ad)+" th="+Str(threadcur)," thid="+Str(thread(threadcur).id)
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
	dbg_prt2 "rlinecur=";rlinecur,rline(rln).nu,thread(threadcur).sv

	thread(threadcur).od=thread(threadcur).sv:thread(threadcur).sv=rln
	procsv=rline(rln).px
	'dbg_prt2("proc ="+Str(procsv)+" "+proc(procsv).nm+" "+hex(proc(procsv).db)+" "+source(proc(procsv).sr)+" "+hex(proccurad))
	'dbg_prt2("line="+Str(rline(i).nu))

	''get registers
	GetThreadContext(threadcontext,@vcontext)

	'dbg_prt2 "test proccurad=proc(procsv).db",proccurad,proc(procsv).db
	If proccurad=proc(procsv).db Then 'is first proc instruction
		thread(threadcur).cl=thread(threadcur).od ''done here to avoid a wrong assignment
		If rline(rln).sv=85 Then'check if the first instruction is push ebp opcode=85 / push rbp opcode=&h55=85dec
			'in this case there is a prologue
			 'at the beginning of proc EBP not updated so use ESP
			procsk=vcontext.regsp-SizeOf(Integer) 'ESP-4 for respecting prologue : push EBP then mov ebp,esp / 64bit push rbp then mov rbp,rsp
		Else
			If procrnb<>0 Then  'no main and no prologue so naked proc, procrnb not yet updated
				procsk=vcontext.regsp
			    thread(threadcur).nk=procsk
			Else
				messbox("Main procedure problem","No standard prologue --> random behaviour")
				procsk=vcontext.regsp-SizeOf(Integer)
			EndIf
		End If
	else
		'only for naked, check if return by comparing top of stack because no epilog
		If thread(threadcur).nk Then
			If vcontext.regsp>thread(threadcur).nk Then
				thread(threadcur).pe=TRUE
				thread(threadcur).nk=0
			EndIf
		End If
	EndIf
		vcontext.regip=ad
		setThreadContext(threadcontext,@vcontext)
		dbg_prt2 "reg IP=";hex(ad),vcontext.regbp
	'dbg_prt2("PE"+Str(thread(threadcur).pe)+" "+Str(proccurad)+" "+Str(proc(procsv).fn))
	If thread(threadcur).pe Then 'if previous instruction was the last of proc
		If proccurad<>proc(procsv).db Then 'reload procsk with rbp/ebp test added for case constructor on shared
			procsk=vcontext.regbp
		EndIf
		proc_end():thread(threadcur).pe=FALSE
	EndIf

	If proccurad=proc(procsv).db Then 'is first proc instruction
		'''from Linux useful ?
		'''if threadnewid<>0 then
			'''dbg_prt2 "new thread beginning of proc"
			'''threadnewidcount-=1
		'''EndIf
		
		''restore CC previous line
		If thread(threadcur).od<>-1 Then
			dbg_prt2 "restore CC 00 ad=";hex(rLine(thread(threadcur).od).ad)
			WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).od).ad),@breakcpu,1,0)
		End If
		dbg_prt2 "first instruc so exit"
		thread(threadcur).sts=KTHD_STOP ''threads not followed stay in init status
		prolog=1 ''only used when multithread to avoid release next thread too quickly
		thread_resume(threadcur)
		Exit Sub
	end if
	if thread(threadcur).sts=KTHD_INIT then ''case create thread when RTRUNning
		dbg_prt2 "rtype=";threadcur,thread(threadcur).rtype
		thread(threadcur).rtype=RTRUN
	EndIf
	dbg_prt2 "threadcur, rtype=";threadcur,thread(threadcur).rtype,vcontext.regbp
	thread(threadcur).sts=KTHD_STOP
	thread_status()

	If thread(threadcur).rtype=RTRUN Then

		if brkol(0).typ<>10 then ''for skip over always in same proc, if different thread ???
			proc_runnew   ''fill running proc tree
		end if

			'var_sh			'updating information about variables
		runtype=RTSTEP ''another threads could be still running but allows display (buttons, etc)

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
		EndIf

   Else 'RTSTEP or RTAUTO
		If flagattach Then
			proc_runnew
			flagattach=FALSE
		else
			dbg_prt2 "in mode step runtype=";runtype,threadlistidx
			If proccurad=proc(procsv).first Then 'is first fbc instruction ?
				''check if not in the current proc, if used set 0 to .stack() when creation of thread????
				'if procsk<>thread(threadcur).stack then
				prolog=0
				dbg_prt2 "check stack=";vcontext.regbp,procsk,thread(threadcur).stack
				if vcontext.regbp<>thread(threadcur).stack then
					proc_new()
				EndIf
				'thread_resume():Exit Sub
			ElseIf proccurad=proc(procsv).fn Then
				thread(threadcur).pe=TRUE        'is last instruction ?
			EndIf

		EndIf
		'NOTA If rline(i).nu=-1 Then
			'fb_message("No line for this proc","Code added by compiler (constructor,...)")

		If runtype=RTSTEP Then
			thread(threadcur).rtype=RTSTEP ''case RTAUTO but halted
		end if
dbg_prt2 "rLine(thread(threadcur).sv).nu=";rLine(thread(threadcur).sv).nu
		dsp_change(rln)

		''restore CC previous line
		If thread(threadcur).od<>-1 Then
			dbg_prt2 "restore CC 01 ad=";hex(rLine(thread(threadcur).od).ad)
			WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).od).ad),@breakcpu,1,0)
		End If

		If runtype=RTAUTO Then
			Sleep(autostep)
			dbg_prt2 "auto thread=";threadnb,threadlistidx
			if threadnb=0 then
				dbg_prt2 "auto 1 thread"
				thread_resume(threadcur)
			else
				if threadlistidx=-1 then
					dbg_prt2 "auto x threads"
					thread_set()
				End if
			end if
		end if
   End If
End Sub
'========================================================
private function wait_debug() As Integer
Dim DebugEv As DEBUG_EVENT    ' debugging event information
Dim dwContinueStatus As Long =DBG_CONTINUE ' exception continuation
Dim As Integer firstchance,flagsecond,bptyp,cpt,adr
Dim As String Accviolstr(1)={"TRYING TO READ","TRYING TO WRITE"}
' Wait for a debugging event to occur. The second parameter indicates
' that the function does not return until a debugging event occurs.
While 1
	If WaitForDebugEvent(@DebugEv, infinite)=0 Then 'INFINITE ou null ou x
		ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		Exit Function
	End If
	' Process the debugging event code.
	'dbg_prt("exception code "+Str(DebugEv.dwDebugEventCode))
	Select Case (DebugEv.dwDebugEventCode)
		'=========================
		Case EXCEPTION_DEBUG_EVENT
		'=========================
			'dbg_prt("exception code "+Hex(DebugEv.u.Exception.ExceptionRecord.ExceptionCode))'+DebugEv.u.Exception.dwfirstchance+" adr : "+DebugEv.u.Exception.ExceptionRecord.ExceptionAddress)

			firstchance=DebugEv.u.Exception.dwfirstchance
			adr=cast(integer,DebugEv.u.Exception.ExceptionRecord.ExceptionAddress)
			'dbg_prt2 "DEBUG EVENT EXCEPTION adr=";adr,DebugEv.u.Exception.ExceptionRecord.ExceptionCode
			'dbg_prt("firstchance="+Str(firstchance))'25/01/2015
			If firstchance=0 Then 'second try
				If flagsecond=0 Then
					flagsecond=1
					firstchance=1
					For i As Integer =0 To threadnb
						If DebugEv.dwThreadId=thread(i).id Then
							threadcontext=thread(i).hd:threadhs=threadcontext
							threadcur=i
  						Exit For
						End If
					Next
				   For i As Integer =1 To linenb 'debugbreak or access violation could be in the middle of line
						If rline(i).ad<=adr And rline(i+1).ad>adr Then
							thread(threadcur).od=thread(threadcur).sv:thread(threadcur).sv=i
							Exit For
						EndIf
				   Next
				EndIf
			Else
	        	flagsecond=0
			End If
			'dbg_prt("CODE "+Str(DebugEv.u.Exception.ExceptionRecord.ExceptionCode))
			If firstchance Then 'if =0 second try so no compute code
				'dbg_prt("before select case with breakpoint")
				Select Case (DebugEv.u.Exception.ExceptionRecord.ExceptionCode)
					'=========================
					'Case EXCEPTION_BREAKPOINT
					'=========================
						'For i As Integer =0 To threadnb 'if msg from thread then flag off
							'If DebugEv.dwThreadId=thread(i).id Then
								'threadcontext=thread(i).hd:threadhs=threadcontext
								'suspendthread(threadcontext)
			               		'threadcur=i
								'Exit For
							'End If
						'Next
						'''''''''''''''''''''''''''''new way gest_brk(adr)
						'debugdata=adr
						'debugevent=KDBGRKPOINT
						'mutexlock blocker ''waiting the Go from main thread
						'mutexunlock blocker
						'ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
					'==========================
					case EXCEPTION_SINGLE_STEP
					'==========================
						''even if singlestepping the exception could also be on a line with a UBP ????
						''restore previous code
						WriteProcessMemory(dbghand,Cast(LPVOID,ssadr),@breakcpu,1,0)
						ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)

					'=========================
					Case EXCEPTION_BREAKPOINT
					'=========================
						while 1
							'dbg_prt2 "------------------------------------------------------------------------------------------"
							'dbg_prt2 "EXCEPTION_BREAKPOINT",adr
							dim as integer bpidx=-1
							if runtype=RTCRASH then
								''don't stop as running until a crash
								breakadr=adr
								for irln as integer =1 to linenb
									if rline(irln).ad=adr then
										singlestep_on(DebugEv.dwThreadId,irln)
										exit for
									EndIf
								Next
								exit while
							end if

							if runtype=RTRUN or runtype=RTAUTO then
								if brkv.adr1<>0 then
									if brk_test(brkv.adr1,brkv.adr2,brkv.typ,brkv.val,brkv.ttb) then
										if brkv.ivr1=0 then
											thread_search(DebugEv.dwThreadId,CSMEM,adr)
										else
											thread_search(DebugEv.dwThreadId,CSVAR,adr)
										end if
										exit while
									else
										for irln as integer =1 to linenb
											if rline(irln).ad=adr then
												singlestep_on(DebugEv.dwThreadId,irln)
												exit for
											EndIf
										Next
									end if
									'exit while
								end if
								''retrieves BP corresponding at address (loop) -->bpidx
								For ibrk as integer =0 To brknb
									If brkol(ibrk).typ>50 Then Continue For 'disabled
									if brkol(ibrk).ad=adr then
										bpidx=ibrk
										runtype=RTRUN ''forcing by default RTRUN usefull only if RTAUTO
										exit for
									EndIf
								Next
								if bpidx<>-1 then
									if bpidx=0 then ''BP on LINE (line, cursor, over,eop,xop)
										thread_search(DebugEv.dwThreadId,CSLINE,bpidx)
										exit while
									end if
									bptyp=brkol(bpidx).typ
									if bptyp=2 or bptyp=3 then  ''BP conditional
										if brk_test(brkol(bpidx).adrvar1,brkol(bpidx).adrvar2,brkol(bpidx).datatype,brkol(bpidx).val,brkol(bpidx).ttb) then
											thread_search(DebugEv.dwThreadId,CSCOND,bpidx)
										else
											singlestep_on(DebugEv.dwThreadId,brkol(bpidx).index)
										end if
										exit while
									elseif bptyp=4 then ''BP counter
										If brkol(bpidx).counter>0 Then
											brkol(bpidx).counter-=1'decrement counter
											singlestep_on(DebugEv.dwThreadId,brkol(bpidx).index)
										else
											thread_search(DebugEv.dwThreadId,CSCOUNT,bpidx)
										end if
										exit while
									else
										''simple BP (perm/tempo)
										thread_search(DebugEv.dwThreadId,CSBRKPT,bpidx)
										exit while
									end if
									if stopcode=CSUSER then
										thread_search(DebugEv.dwThreadId,stopcode,adr)
										exit while
									end if
								else
									thread_search(DebugEv.dwThreadId,CSSTEP,adr)
									exit while
								end if

							else ''RTSTEP
								if stopcode=CSUSER then ''CSUSER
									thread_search(DebugEv.dwThreadId,stopcode,adr)
								else
									thread_search(DebugEv.dwThreadId,CSSTEP,adr)
								end if
								exit while
							end if
						wend

						ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
					'=========================
					case Else ' EXCEPTION
					'=========================
						dbg_prt("Exception in thread="+str(DebugEv.dwThreadId))
						With DebugEv.u.Exception.ExceptionRecord
							For i As Integer =0 To threadnb 'if msg from thread then flag off
	        					If DebugEv.dwThreadId=thread(i).id Then
	        						If  thread(i).sv=-1 Then 'the exception is not in an handled thread, it's going to be ignored, hoping to no other effect ;-) 25/01/2015
										'fb_message("EXCEPTION","This arises in a not handled thread so ignoring it",MB_SYSTEMMODAL Or MB_ICONSTOP)
	        							For j As Integer=0 To threadnb
		        							If  thread(j).sv=-1 then
		        								'continuedebugevent(DebugEv.dwProcessId,thread(j).id, dwContinueStatus)
		        								'dbg_prt2(Str(GetLastError))
		        								continuedebugevent(DebugEv.dwProcessId,thread(j).id, DBG_EXCEPTION_NOT_HANDLED)
		        							EndIf
	        							next
	        							'ContinueDebugEvent(DebugEv.dwProcessId,thread(i).id, dwContinueStatus)
	        							'Exit function
	        							continue while ''2021
	        						End If
	        						threadcontext=thread(i).hd:threadhs=threadcontext
	                        		threadcur=i
	        						Exit For
	        					End If
							Next

							libelexception=excep_lib(DebugEv.u.Exception.ExceptionRecord.ExceptionCode)+Chr(13)+Chr(10) 'need chr(10) to dbg_prt otherwise bad dbg_prt2
							If DebugEv.u.Exception.ExceptionRecord.ExceptionCode=EXCEPTION_ACCESS_VIOLATION Then
								'dbg_prt2 "info=";.ExceptionInformation(0),.ExceptionInformation(1),Hex(.ExceptionInformation(1))
								libelexception+=Accviolstr(.ExceptionInformation(0))+" AT ADR (dec/hex) : "+Str(.ExceptionInformation(1))+" / "+Hex(.ExceptionInformation(1))+Chr(13)+Chr(10)
							EndIf

							If flagverbose Then
								libelexception+=Chr(13)+Chr(10)+"Thread ID "+Str(DebugEv.dwThreadId)+" adr : "+Str(adr)+" / "+Hex(adr)+Chr(13)+Chr(10)
							EndIf

							#Ifdef fulldbg_prt
								dbg_prt (libelexception)
								'show_context
							#EndIf

							If runtype=RTRUN OrElse runtype=RTFREE Then
								runtype=RTRUN
								For i As Integer =1 To linenb
									'WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@breakcpu,1,0)'restore CC
									If rline(i).ad<=adr AndAlso rline(i+1).ad>adr Then
										thread(threadcur).od=thread(threadcur).sv:thread(threadcur).sv=i
										Exit For
									EndIf
								Next
							Else
								runtype=RTSTEP
							End If

							If DebugEv.dwDebugEventCode Then
								stopcode=CSACCVIOL
							Else
								stopcode=CSEXCEP 'excep_lib(DebugEv.u.Exception.ExceptionRecord.ExceptionCode)
							EndIf

							debugdata=adr
							debugevent=KDBGEXCEPT
							mutexlock blocker ''waiting the Go from main thread
							mutexunlock blocker

							if debugdata=IDYES then
								suspendthread(threadcontext)
								ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
							Else
								ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, DBG_EXCEPTION_NOT_HANDLED)
							End If
						End With
				End Select
			Else'second chance
				dbg_prt("second chance")
	         	ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, DBG_EXCEPTION_NOT_HANDLED)
			End If
		'=========================
		Case CREATE_THREAD_DEBUG_EVENT
		'=========================
	        With DebugEv.u.Createthread
	         	#Ifdef fulldbg_prt
		         	dbg_prt(""):dbg_prt ("Create thread : DebugEv.dwProcessId "+Str(DebugEv.dwProcessid))
		         	dbg_prt ("DebugEv.dwThreadId "+Str(DebugEv.dwThreadId))
		         	dbg_prt ("hthread "+Str(.hthread)+" start address "+Str(.lpStartAddress))
	         	#EndIf
					'dbg_prt2 "DebugEv.dwThreadId "+Str(DebugEv.dwThreadId)
					'dbg_prt2 "hthread "+Str(.hthread)+" start address "+Str(.lpStartAddress)
					If threadnb<THREADMAX Then
					      threadnb+=1 :thread(threadnb).hd=.hthread:thread(threadnb).id=DebugEv.dwThreadId
					      dbg_prt2 "DebugEv.dwThreadId ";threadnb,Str(DebugEv.dwThreadId)
					      threadcontext=.hthread
					      thread(threadnb).pe=FALSE
					      thread(threadnb).sv=-1 'used for thread not debugged
					      thread(threadnb).plt=0 'used for first proc of thread then keep the last proc
					      thread(threadnb).st=thread(threadcur).od 'used to keep line origin
					      thread(threadnb).tv=0
					      thread(threadnb).exc=0 'no exec auto
					      thread(threadnb).sts=KTHD_INIT
					      thread(threadnb).rtype=runtype
					Else
				      	hard_closing("Number of threads ("+Str(THREADMAX+1)+") exceeded , change the THREADMAX value."+Chr(10)+Chr(10)+"CLOSING FBDEBUGGER, SORRY" )
					EndIf
	        End With
	        ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		'=========================
		Case CREATE_PROCESS_DEBUG_EVENT
		'=========================
			With DebugEv.u.CreateProcessInfo
				dbghfile=.hfile' to close the handle and liberate the file .exe
				threadnb=0:thread(0).hd=.hthread:thread(0).id=DebugEv.dwThreadId
				threadcontext=.hthread
				thread(0).pe=FALSE
				thread(0).sv=-1  'used for thread not debugged
				thread(0).plt=0 'used for first proc of thread then keep the last proc
				thread(0).tv=0  'handle of thread
				thread(0).exc=0 'no exec auto
				thread(0).rtype=RTSTEP
				thread(0).sts=KTHD_STOP
				#Ifdef fulldbg_prt
		  			dbg_prt ("create process debug")
					dbg_prt ("DebugEv.dwProcessId "+Str(DebugEv.dwProcessid))
					dbg_prt ("DebugEv.dwThreadId "+Str(DebugEv.dwThreadId))
		    		dbg_prt ("hFile "+Str(.hfile))
		    		dbg_prt ("hProcess "+Str(.hprocess))
					dbg_prt ("hThread "+Str(.hthread))
					dbg_prt ("lpBaseOfImage "+Str(.lpBaseOfImage))
					dbg_prt ("dwDebugInfoFileOffset "+Str(.dwDebugInfoFileOffset))
					dbg_prt ("nDebugInfoSize "+Str(.nDebugInfoSize))
					dbg_prt ("lpThreadLocalBase "+Str(.lpThreadLocalBase))
					dbg_prt ("lpStartAddress "+Str(.lpStartAddress))
					dbg_prt ("lpImageName "+Str(.lpImageName))
					dbg_prt ("fUnicode "+Str(.fUnicode))
					'show_context
				#EndIf
				debugdata=Cast(Integer,.lpBaseOfImage)
				debugevent=KDBGCREATEPROCESS
				mutexlock blocker ''waiting the Go from main thread
				mutexunlock blocker
				''''''''''''''''' new way debug_extract(Cast(UInteger,.lpBaseOfImage),exename)
			End With
			ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		'=========================
		Case EXIT_THREAD_DEBUG_EVENT
		'=========================
			#Ifdef fulldbg_prt
					dbg_prt ("exit thread "+Str(DebugEv.dwProcessId)+" " +Str(DebugEv.dwThreadId)+" "+Str(debugev.u.exitthread.dwexitcode))
			#EndIf
			If flagkill=FALSE Then ''otherwise there is deadlock between a loop waiting prun=0 and the timerproc never executed so the mutex never unlock
				debugdata=DebugEv.dwThreadId
				debugevent=KDBGEXITTHREAD
				mutexlock blocker ''waiting the Go from main thread
				mutexunlock blocker
			end if
			ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		'=========================
		Case EXIT_PROCESS_DEBUG_EVENT
		'=========================
			'#Ifdef fulldbg_prt
				dbg_prt2 ("exit process="+Str(debugev.u.exitprocess.dwexitcode))
			'#EndIf
			prun=FALSE

			closehandle(dbghand)
			closehandle(dbghfile)
			closehandle(dbghthread)
			For i As Integer=1 To dllnb
				closehandle dlldata(i).hdl ''closes all the dll handles
			Next
			debugdata=debugev.u.exitprocess.dwexitcode
			debugevent=KDBGEXITPROCESS
			mutexlock blocker
			mutexunlock blocker
			ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
			Exit While ''goes out the loop so closes the thread 2
		'=========================
		Case LOAD_DLL_DEBUG_EVENT
		'=========================
     		Dim loaddll As LOAD_DLL_DEBUG_INFO=DebugEv.u.loaddll

      		#ifdef fulldbg_prt
	      	   	dbg_prt(""):dbg_prt("Load dll event Pid/Tid "+Str(DebugEv.dwProcessId)+" "+Str(DebugEv.dwThreadId))
	      		dbg_prt ("hFile="+Str(loaddll.hFile)+" lpBaseOfDll="+Str(loaddll.lpBaseOfDll)+" "+dll_name(loaddll.hFile))
			#EndIf

			debugdata=Cast(Integer,@loaddll)
			debugevent=KDBGDLL
			mutexlock blocker ''waiting the Go from main thread
			mutexunlock blocker
			ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		'=========================
		Case UNLOAD_DLL_DEBUG_EVENT
		'=========================
			Dim unloaddll As UNLOAD_DLL_DEBUG_INFO =DebugEv.u.unloaddll
			#Ifdef fulldbg_prt
				dbg_prt(""):dbg_prt("UnLoad dll event "+Str(DebugEv.dwProcessId)+" "+Str(DebugEv.dwThreadId))
				dbg_prt ("lpBaseOfDll "+Str(unloaddll.lpBaseOfDll))
			#EndIf
			For idll As Integer=1 To dllnb
				If dlldata(idll).bse=unloaddll.lpBaseOfDll Then
					closehandle dlldata(idll).hdl
					dlldata(idll).hdl=0
					debugdata=idll
					debugevent=KDBGDLLUNLOAD ''only for handled dll
					mutexlock blocker ''waiting the Go from main thread
					mutexunlock blocker
					Exit For
				EndIf
			Next
         	ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		'=========================
		Case OUTPUT_DEBUG_STRING_EVENT
		'=========================
			#Ifdef fulldbg_prt
    			dbg_prt( "OUTPUT DEBUG")
    		#EndIf
			debugstring_read(debugev) ''nothing done in main thread
    		ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		'=========================
		Case RIP_EVENT
		'=========================
			#Ifdef fulldbg_prt
				dbg_prt( "RIP EVENT")
			#EndIf
			ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
	End Select
Wend
Return 0 'not really used
End Function
'=========================================================
'' translates code message to the text (only windows)
'=========================================================
private sub winmsg()
	Dim Buffer As String*210
	var inputval=input_bx("Window message number","Enter the Windows code",,5)
	If valint(inputval)<>0 Then
		'Format the message string
		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, ByVal 0,ValInt(inputval) , LANG_NEUTRAL, Buffer, 200, ByVal 0)
		messbox("Windows message","Code : "+inputval+Chr(10)+"Message : "+buffer)
	End If
End Sub
'=======================================
'' lists processes ''todo win32 only
'=======================================
private sub process_list()
	Dim prcinfo As PROCESSENTRY32,snap As HANDLE
	dim as string text
	snap=CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0)'Take snapshot of running processes
	If snap <> INVALID_HANDLE_VALUE Then
		prcinfo.dwSize=SizeOf(PROCESSENTRY32)
		text="file Process name     ID  Nthread parent id"+Chr(13)+Chr(10)
		If Process32First (snap,@prcinfo) Then
			Do
				text+=fmt(prcinfo.szExeFile,20)+fmt(Str(prcinfo.th32ProcessID),5)+fmt(Str(prcinfo.cntThreads),3)+fmt(Str(prcinfo.th32ParentProcessID),5)+Chr(13)+Chr(10)
			Loop While  Process32Next (snap,@prcinfo)
		Else
			messbox("Process list error","Failed to create process list!")
		End If
		CloseHandle (snap)
	End If
	SetWindowText(heditorbx,"Process list")
	SetGadgetText(GEDITOR,text)
	hidewindow(heditorbx,KSHOW)
end sub
'======================================
''show all the registers
'======================================
private sub show_regs()
	dim as integer dummy ''used to align vcontext on 16bit
	Dim vcontext As CONTEXT
	Dim As String reg_values(),text
	vcontext.contextflags=CONTEXT_CONTROL or CONTEXT_INTEGER ' 64 bit rbp is gotten with CONTEXT_INTEGER , instead in x86 CONTEXT_CONTROL
	For ith As Integer=0 To threadnb
		if thread(ith).tv=0 then
			continue for
		end if

		GetThreadContext(thread(ith).hd,@vcontext)
		If thread(ith).hd=threadcontext Then
		   text+="Current Thread Id="+Str(thread(ith).id)+" / "+Hex(thread(ith).id)+chr(13)
		   text+="Current Thread Hd="+Str(thread(ith).hd)+" / "+Hex(thread(ith).hd)

		Else
		   text+="Other   Thread Id="+Str(thread(ith).id)+" / "+Hex(thread(ith).id)+chr(13)
		   text+="Other   Thread Hd="+Str(thread(ith).hd)+" / "+Hex(thread(ith).hd)
		EndIf

		#Ifdef __FB_64BIT__
			redim reg_values(15)
			reg_values(0)="Rax="+fmt(Str(vcontext.rax),20)+"/ "+Hex(vcontext.rax)
			reg_values(1)="Rcx="+fmt(Str(vcontext.rcx),20)+"/ "+Hex(vcontext.rcx)
			reg_values(2)="Rdx="+fmt(Str(vcontext.rdx),20)+"/ "+Hex(vcontext.rdx)
			reg_values(3)="Rbx="+fmt(Str(vcontext.rbx),20)+"/ "+Hex(vcontext.rbx)
			reg_values(4)="Rsp="+fmt(Str(vcontext.rsp),20)+"/ "+Hex(vcontext.rsp)
			reg_values(5)="Rbp="+fmt(Str(vcontext.rbp),20)+"/ "+Hex(vcontext.rbp)
			reg_values(6)="Rsi="+fmt(Str(vcontext.rsi),20)+"/ "+Hex(vcontext.rsi)
			reg_values(7)="R8 ="+fmt(Str(vcontext.r8),20) +"/ "+Hex(vcontext.r8)
			reg_values(8)="R9 ="+fmt(Str(vcontext.r9),20) +"/ "+Hex(vcontext.r9)
			reg_values(9)="R10="+fmt(Str(vcontext.r10),20)+"/ "+Hex(vcontext.r10)
			reg_values(10)="R11="+fmt(Str(vcontext.r11),20)+"/ "+Hex(vcontext.r11)
			reg_values(11)="R12="+fmt(Str(vcontext.r12),20)+"/ "+Hex(vcontext.r12)
			reg_values(12)="R13="+fmt(Str(vcontext.r13),20)+"/ "+Hex(vcontext.r13)
			reg_values(13)="R14="+fmt(Str(vcontext.r14),20)+"/ "+Hex(vcontext.r14)
			reg_values(14)="R15="+fmt(Str(vcontext.r15),20)+"/ "+Hex(vcontext.r15)
			reg_values(15)="Rip="+fmt(Str(vcontext.rip),20)+"/ "+Hex(vcontext.rip)

			For i As Long =0 To 15
		#Else
			redim reg_values(8)
			reg_values(0)="Edi="+fmt(Str(vcontext.edi),11)+"/ "+Hex(vcontext.edi)
			reg_values(1)="Esi="+fmt(Str(vcontext.esi),11)+"/ "+Hex(vcontext.esi)
			reg_values(2)="Ebx="+fmt(Str(vcontext.ebx),11)+"/ "+Hex(vcontext.ebx)
			reg_values(3)="Edx="+fmt(Str(vcontext.edx),11)+"/ "+Hex(vcontext.edx)
			reg_values(4)="Ecx="+fmt(Str(vcontext.ecx),11)+"/ "+Hex(vcontext.ecx)
			reg_values(5)="Eax="+fmt(Str(vcontext.eax),11)+"/ "+Hex(vcontext.eax)
			reg_values(6)="Ebp="+fmt(Str(vcontext.ebp),11)+"/ "+Hex(vcontext.ebp)
			reg_values(7)="Eip="+fmt(Str(vcontext.eip),11)+"/ "+Hex(vcontext.eip)
			reg_values(8)="Esp="+fmt(Str(vcontext.esp),11)+"/ "+Hex(vcontext.esp)

			For i As Long =0 To 8
		#EndIf
				text+=chr(13)+reg_values(i)
			Next
			text+=chr(13)+chr(13)
	Next
	readonlyeditor(GEDITOR,0)
	setgadgettext(GEDITOR,text)
	readonlyeditor(GEDITOR,1)
	hidewindow(heditorbx,KSHOW)
End Sub
'====================================================
'' for attaching application already running
'====================================================
private sub attach_debuggee(p As Any Ptr)
	Dim As ZString *150 zsrtg=Space(149)
	dbghand=openprocess(PROCESS_ALL_ACCESS,FALSE,dbgprocid)
	If dbghand=0 Then messbox("Attach error open","error : "+ Str(GetLastError)):Exit Sub
	If debugactiveprocess(dbgprocid)=0 Then messbox("Attachment error","error : "+ Str(GetLastError)):Exit Sub
	prun=TRUE
	#Ifdef fulldbg_prt
		dbg_prt ("hand "+Str(dbghand)+" Pid "+Str(dbgprocid))
	#EndIf
	runtype=RTSTEP
	flagattach=TRUE
	'but_enable()
	'menu_enable()
	getmodulefilenameex(dbghand,0,@zsrtg,Len(zsrtg)) 'executable name
	exename=zsrtg
	'no needed --> exedate=FileDateTime (exename) 'exec date for test with sources date
	exe_sav(exename,"")
	wait_debug
End Sub

