''Windows system for fbdebugger_new
''dbg_windows.bas

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
		Print #filenumber,Date,Time
	EndIf

	If (flaglog And 1) Then libel=t+Chr(13)+Chr(10):WriteConsole(scrnnumber, StrPtr(libel),Len(libel),@cpt,0)
	If (flaglog And 2) Then Print # filenumber,t

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
		messagebox(0,wstrg,WStr("debug wstring"),MB_OK or MB_SYSTEMMODAL)
	else
		ReadProcessMemory(dbghand,Cast(LPCVOID,debugev.u.debugstring.lpDebugStringData),_
		@sstrg,leng,0)
		messagebox(0,sstrg,@"debug string",MB_OK or MB_SYSTEMMODAL)
	endif

End Sub
'========================================================
private function wait_debug() As Integer
Dim DebugEv As DEBUG_EVENT    ' debugging event information
Dim dwContinueStatus As Long =DBG_CONTINUE ' exception continuation
Dim recup As String *300,libel As String
Dim erreur As Long,cpt As Integer
Dim As Integer firstchance,flagsecond
Dim As UInteger adr
Dim As String Accviolstr(1)={"TRYING TO READ","TRYING TO WRITE"}
' Wait for a debugging event to occur. The second parameter indicates
' that the function does not return until a debugging event occurs.
If hattach Then setevent(hattach):hattach=0
While 1
	If WaitForDebugEvent(@DebugEv, infinite)=0 Then 'INFINITE ou null ou x
		erreur=GetLastError
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
			adr=Cast(UInteger,DebugEv.u.Exception.ExceptionRecord.ExceptionAddress)
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
					Case EXCEPTION_BREAKPOINT
					'=========================
						For i As Integer =0 To threadnb 'if msg from thread then flag off
							If DebugEv.dwThreadId=thread(i).id Then
								threadcontext=thread(i).hd:threadhs=threadcontext
								suspendthread(threadcontext)
			               		threadcur=i
								Exit For
							End If
						Next
						''''''''''''''''''''''''''''new way gest_brk(adr)
						print "BREAKPOINT gest_brk adr=";adr
						debugevent=KDBGBPOINT
						debugdata=adr
						mutexlock blocker ''waiting the Go from main thread
						mutexunlock blocker
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

							libel=excep_lib(DebugEv.u.Exception.ExceptionRecord.ExceptionCode)+Chr(13)+Chr(10) 'need chr(10) to dbg_prt otherwise bad print
							If DebugEv.u.Exception.ExceptionRecord.ExceptionCode=EXCEPTION_ACCESS_VIOLATION Then
								libel+=Accviolstr(.ExceptionInformation(0))+" AT ADR (dec/hex) : "+Str(.ExceptionInformation(1))+" / "+Hex(.ExceptionInformation(1))+Chr(13)+Chr(10)
							EndIf

							If flagverbose Then
								libel+=Chr(13)+Chr(10)+"Thread ID "+Str(DebugEv.dwThreadId)+" adr : "+Str(adr)+" / "+Hex(adr)+Chr(13)+Chr(10)
							EndIf

							#Ifdef fulldbg_prt
								dbg_prt (libel)
								'show_context
							#EndIf

							If runtype=RTFRUN OrElse runtype=RTFREE Then
								runtype=RTFRUN
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
    						gest_brk(rline(thread(threadcur).sv).ad)

							source_change(rline(thread(threadcur).sv).sx) 'display source
							line_display(rline(thread(threadcur).sv).nu-1)
							recup=line_text(rline(thread(threadcur).sv).nu-1)
							'case error inside proc initialisation (e.g. stack over flow)
							If adr>rline(thread(threadcur).sv).ad And _
								adr<rline(thread(threadcur).sv+1).ad And _
								rline(thread(threadcur).sv+1).nu=rline(thread(threadcur).sv).nu Then
								libel+="ERROR AT BEGINNING OF PROC NOT REALLY ON THIS LINE"+Chr(13)+ _
								"CHECK DIM (e.g. width array to big), Preferably don't continue"+Chr(13)+Chr(13)
							Else
								libel+="Possible error on this line but not SURE"+Chr(13)+Chr(13)
							End If

							libel+="File  : "+source(rline(thread(threadcur).sv).sx)+Chr(13)+ _
							"Proc  : "+proc(rline(thread(threadcur).sv).px).nm+Chr(13)+ _
							"Line  : "+Str(rline(thread(threadcur).sv).nu)+" (selected and put in red)"+Chr(13)+ _
							recup+Chr(13)+Chr(13)+"Try To continue ? (if yes change values and/or use [M]odify execution)"
							If messbox("EXCEPTION",libel,MB_YESNO or MB_SYSTEMMODAL) = RETYES Then
								suspendthread(threadcontext)
								ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
							Else
								ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, DBG_EXCEPTION_NOT_HANDLED)
							End If
						End With
					'case Else
						'fb_message("EXCEPTION_DEBUG_EVENT ","Code :"+excep_lib(DebugEv.u.Exception.ExceptionRecord.ExceptionCode),MB_SYSTEMMODAL Or MB_ICONSTOP)
						'#Ifdef fulldbg_prt
						'	dbg_prt("EXCEPTION_DEBUG_EVENT : "+excep_lib(DebugEv.u.Exception.ExceptionRecord.ExceptionCode))
						'#EndIf
					   'ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, DBG_EXCEPTION_NOT_HANDLED)
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
					If threadnb<THREADMAX Then
					      threadnb+=1 :thread(threadnb).hd=.hthread:thread(threadnb).id=DebugEv.dwThreadId
					      threadcontext=.hthread
					      thread(threadnb).pe=FALSE
					      thread(threadnb).sv=-1 'used for thread not debugged
					      thread(threadnb).plt=0 'used for first proc of thread then keep the last proc
					      thread(threadnb).st=thread(threadcur).od 'used to keep line origin
					      thread(threadnb).tv=0
					      thread(threadnb).exc=0 'no exec auto
					Else
				      	hard_closing("Number of threads ("+Str(THREADMAX+1)+") exceeded , change the THREADMAX value."+Chr(10)+Chr(10)+"CLOSING FBDEBUGGER, SORRY" )
					EndIf
	        End With
	        'mutexlock blocker ''waiting the Go from main thread
			'mutexunlock blocker
			print "---------------------------------------------------- end of create thread"
	        ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		'=========================
		Case CREATE_PROCESS_DEBUG_EVENT
		'=========================
			With DebugEv.u.CreateProcessInfo
				dbghfile=.hfile' to close the handle and liberate the file .exe
				threadnb=0:thread(0).hd=.hthread:thread(0).id=DebugEv.dwThreadId
				threadcontext=.hthread
				thread(0).pe=FALSE
				thread(0).sv=0  'used for thread not debugged
				thread(0).plt=0 'used for first proc of thread then keep the last proc
				thread(0).tv=0  'handle of thread
				thread(0).exc=0 'no exec auto
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
				print "create process","create process 00"
				debugdata=Cast(Integer,.lpBaseOfImage)
				print "create process","create process 01"
				debugevent=KDBGBCREATEPR

				print "---------------------------------------------------- create process","create process 02"
				mutexlock blocker ''waiting the Go from main thread
				mutexunlock blocker
				''''''''''''''''' new way debug_extract(Cast(UInteger,.lpBaseOfImage),exename)

			End With
			print "---------------------------------------------------- end of create process"
			ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		'=========================
		Case EXIT_THREAD_DEBUG_EVENT
		'=========================
			#Ifdef fulldbg_prt
					dbg_prt ("exit thread "+Str(DebugEv.dwProcessId)+" " +Str(DebugEv.dwThreadId)+" "+Str(debugev.u.exitthread.dwexitcode))
			#EndIf
			If flagkill=FALSE Then thread_del(DebugEv.dwThreadId)
			ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		'=========================
		Case EXIT_PROCESS_DEBUG_EVENT
		'=========================
			messbox("","END OF DEBUGGED PROCESS",MB_SYSTEMMODAL)
			#Ifdef fulldbg_prt
				dbg_prt ("exit process"+Str(debugev.u.exitprocess.dwexitcode))
			#EndIf
			closehandle(dbghand)
			closehandle(dbghfile)
			closehandle(dbghthread)
			For i As Integer=1 To dllnb
				closehandle dlldata(i).hdl 'close all the dll handles
			Next
			watch_sav()
			brk_sav()
			ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
			prun=FALSE
			runtype=RTEND
			but_enable()
			menu_enable()
			Exit While
		'=========================
		Case LOAD_DLL_DEBUG_EVENT
		'=========================
     		Dim loaddll As LOAD_DLL_DEBUG_INFO=DebugEv.u.loaddll
     		Dim As String dllfn
     		Dim As Integer d,delta
      		#ifdef fulldbg_prt
	      	   	dbg_prt(""):dbg_prt("Load dll event Pid/Tid "+Str(DebugEv.dwProcessId)+" "+Str(DebugEv.dwThreadId))
	      		dbg_prt ("hFile="+Str(loaddll.hFile)+" lpBaseOfDll="+Str(loaddll.lpBaseOfDll)+" "+dll_name(loaddll.hFile))
			#EndIf


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
	      		debug_extract(Cast(UInteger,loaddll.lpBaseOfDll),dllfn,DLL)
	      		If (linenb-linenbprev)=0 Then 'not debugged so not taking in account
	      			dllnb-=1
	      		Else
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
					ReadProcessMemory(dbghand,Cast(LPCVOID,rline(i).ad),@rLine(i).sv,1,0) 'sav 1 byte before writing &CC
					WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@breakcpu,1,0)
				Next
				globals_load(d)
				brk_apply
			EndIf

			ContinueDebugEvent(DebugEv.dwProcessId,DebugEv.dwThreadId, dwContinueStatus)
		'=========================
		Case UNLOAD_DLL_DEBUG_EVENT
		'=========================
			Dim unloaddll As UNLOAD_DLL_DEBUG_INFO =DebugEv.u.unloaddll
			#Ifdef fulldbg_prt
				dbg_prt(""):dbg_prt("UnLoad dll event "+Str(DebugEv.dwProcessId)+" "+Str(DebugEv.dwThreadId))
				dbg_prt ("lpBaseOfDll "+Str(unloaddll.lpBaseOfDll))
			#EndIf
			cpt=0
			For i As Integer=1 To dllnb
				If dlldata(i).bse=unloaddll.lpBaseOfDll Then
					closehandle dlldata(i).hdl
					dlldata(i).hdl=0
					For m As Integer =2 To procrnb
						If procr(m).tv=dlldata(i).tv Then proc_del(m,2):Exit For 'delete procr().tv
					Next

					'For m As Integer =1 To brknb 'delete brkpoint but before trying to save it in brkexe
					Dim As Integer m=1
					While m<=brknb
						If brkol(m).index>=dlldata(i).lnb AndAlso brkol(m).index<=dlldata(i).lnn Then 'inside rline of dll
							'create in brkexe for use in next dll loading
							For n As Integer = BRKMAX To 1 Step-1 'search by the last slot, later if there are BRKMAX brkpt this one will be lost
								If brkexe(0,n)="" Then 'find an empty slot if not data is lost
									brkexe(0,n)=source_name(source(brkol(m).isrc))+","+Str(brkol(m).nline)+","+Str(brkol(m).typ)
								EndIf
								Exit For
							Next
							brk_del(m)
						EndIf
						m+=1
					Wend
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
    		debugstring_read(debugev)
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


