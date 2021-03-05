''tools for fbdebugger_new
''dbg_tools.bas

'===============================================
'' check if the line is executable return true
'===============================================
function line_check(pline as integer)as boolean
	For iline as integer =1 To linenb
		If rline(iline).nu=pline AndAlso rline(iline).sx=srcdisplayed Then
			return true
		end if
	next
	return false
end function
'-----------------------------------------------
'' Reinitialisation GUI todo move in dbg_gui
'-----------------------------------------------
sub reinit_gui

end sub
'-----------------------------------------------
'' Reinitialisation
'-----------------------------------------------
sub reinit
	vrbgbl=0:vrbloc=VGBLMAX:vrbgblprev=0
	prun=FALSE
	runtype=RTOFF
	flagmain=true
	sourcenb=-1
	vrrnb=0:procnb=0:procrnb=0:linenb=0:cudtnb=0:arrnb=0:procr(1).vr=1
	'procin=0:procfn=0:procbot=0:proctop=FALSE
	proc(1).vr=VGBLMAX+1 'for the first stored proc
	udtcpt=0:udtmax=0
	'================================================================
				'	'======== init =========================================
				'private sub re_ini()
				'
				'If TimerID1<>0 Then KillTimer(windmain,TimerID1) ''27/12/26

				'brkv.adr=0 'no break on var
				'brknb=0 'no break on line
				'brkol(0).ad=0   'no break on cursor
				'
				'setwindowtext(hcurline,"")
				'setwindowtext(brkvhnd,"Break on var")
				'menu_chg(menuvar,idvarbrk,"Break on var value")
				'setwindowtext(windmain,"DEBUG "+ver3264)
				'
				'SendMessage(listview1,LVM_DELETEALLITEMS,0,0) 'dump
				'SendMessage(tviewvar,TVM_DELETEITEM,0,Cast(LPARAM,TVI_ROOT)) 'procs/vars
				'SendMessage(tviewprc,TVM_DELETEITEM,0,Cast(LPARAM,TVI_ROOT)) 'procs
				'SendMessage(tviewthd,TVM_DELETEITEM,0,Cast(LPARAM,TVI_ROOT)) 'threads
				'
				'ShowWindow(tviewcur,SW_HIDE):tviewcur=tviewvar:ShowWindow(tviewcur,SW_SHOW)
				'SendMessage(htab2,TCM_SETCURSEL,0,0)
				'If dsptyp Then dsp_hide(dsptyp)
				'dsp_sizecalc
				'threadnb=-1
				'If flagrestart=-1 Then 'add test for restart without loading again all the files
				'	setwindowtext(richeditcur,"Your source")
				'	sendmessage(htab1,TCM_DELETEALLITEMS ,0,0) 'zone tab
				'	For i As Integer=0 To MAXSRC:setWindowText(richedit(i),""):ShowWindow(richedit(i),SW_HIDE):Next
				'Else
				'	sel_line(curlig-1,0,1,richedit(curtab),FALSE) 'default color
				'EndIf
				'curlig=0
				':dllnb=0
				'excldnb=0
				'dumpadr=0:copybeg=-99:copyend=-99:copycol=-99 '23/11/2014
				''flaglog=0:dbg_prt(" $$$$___CLOSE ALL___$$$$ "):flagtrace=0
				':flagattach=FALSE:flagkill=FALSE
				'
				'For i As Integer = 0 To 4 :SendMessage(dbgstatus,SB_SETTEXT,i,Cast(LPARAM,@"")) : Next '08/04/2014 3-->4
				'
				'
				'bx_closing
				'array_tracking_remove
				'
				''reset bookmarks
				'sendmessage(bmkh,CB_RESETCONTENT,0,0)
				'bmkcpt=0:For i As Integer =1 To BMKMAX:bmk(i).ntab=-1:Next
				'EnableMenuItem(menuedit,IDNXTBMK,MF_GRAYED)
				'EnableMenuItem(menuedit,IDPRVBMK,MF_GRAYED)
				'EnableMenuItem(menutools,IDHIDLOG,MF_GRAYED)
				'compinfo="" 'information about compilation
				'threadprv=0
				'threadsel=0
				'
				'For i As Long =TYPESTD+1 To TYPEMAX 'reinit index to avoid message udt nok when executing an other debuggee, only gcc 16/08/2015 20/08/2015 boolean
				'   udt(i).typ=0
				'Next
				'End sub
	'================================================================

end sub
'=========================================================================
'' something wrong happens so close fbdebugger after displaying a message
'==========================================================================
sub hard_closing(errormsg as string)
	messbox("Need to close fbdebugger",_
			  "Sorry an unrecoverable problem occurs :"+chr(13)+errormsg+chr(13)+chr(13)+"Report to dev please")
	''freegadgets()  todo free all gadget number by number
	close_window(hmain)
	end
end sub
'================================================================
'' check if exe bitness if not wrong 32bit<>64bit windows only
'================================================================

function check_bitness(fullname as string) as integer
	#Ifdef __fb_win32__
		dim as long bintype
			getbinarytype(strptr(fullname),@bintype) ''a control to prevent 32bit<>64bit 2020/02/05
		#Ifdef __FB_64BIT__
			if bintype=SCS_32BIT_BINARY then
			   messbox("FBdebugger 64bit","can not be used for debugging 32bit exe...")
				return 0
			end if
		#else
			if bintype=SCS_64BIT_BINARY then
				messbox("FBdebugger 32bit","can not be used for debugging 64bit exe...")
				return 0
			end if
		#endif
	#else
		dim as ubyte ubyt
		open fullname for binary as #1
		get #1,5,ubyt ''offset=4 32bit or 64bit
		close #1
		#Ifdef __FB_64BIT__
			if ubyt<>2 then
				messbox("FBdebugger 64bit","can not be used for debugging 32bit exe...")
				close #1
				return 0
			end if
		#else
			if ubyt<>1 then
				messbox("FBdebugger 32bit","can not be used for debugging 64bit exe...")
				close #1
				return 0
			end if
		#endif
	#endif
	return -1
end function
'===========================================
'' restore instruction and resume thread
'===========================================
Private sub thread_resume()
	#ifdef __fb_win32__
		writeprocessmemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@rLine(thread(threadcur).sv).sv,1,0) 'restore old value for execution
		resumethread(threadhs)
	#else
		''todo LINUX, maybe moved in dbg_linux.bas
	#endif
End sub
'====================================================================================================================
'' if debuggee running ask for killing return true after killing or if nothing running, false debuggee still running
'====================================================================================================================
private function kill_process(text As String) As Integer
    dim As Long retcode,lasterr
    if prun then
	    If messbox("Kill current running Program ?",text+Chr(10)+Chr(10) _
	    	       +"USE CARREFULLY SYSTEM CAN BECOME UNSTABLE, LOSS OF DATA, MEMORY LEAK"+Chr(10) _
	               +"Try to close your program first",MB_YESNO) = RETYES then
	    	flagkill=true
		    #ifdef __fb_win32__
		    	retcode=terminateprocess(dbghand,999)
		      	lasterr=getlasterror
		    #else
		    	''todo linux function from W9
			#endif
		    #ifdef fulldbg_prt
		      	dbg_prt ("return code terminate process ="+Str(retcode)+" lasterror="+Str(lasterr))
		   	#endif
			thread_resume()
			While prun:Sleep 500:Wend
			Return TRUE
	    Else
	        Return FALSE
	    endif
	else
		Return true
    end if
end function

'====================================================
'' starts debugge for windows
'====================================================
#ifdef __fb_win32__
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
		cmdl=""""+exename+""" "+cmdexe(0)
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
		  'wait_debug todo add and uncomment
	   Else
	   	   ''todo use std meesage windows with ,MB_ICONERROR Or MB_SYSTEMMODAL ???
		  messbox("PROBLEM","no debugged pgm -->"+exename+Chr(10)+"error :"+Str(GetLastError()))
	   End If
#EndIf
End Sub
'======================================================================
''extracts the file name from full name
'======================================================================
private function source_name(fullname as string)as string
	dim as integer cpos=instrrev(fullname,any "/\")
	return mid(fullname,cpos+1)
end function
'================================================================
'' Add new exe / cmdline to the list  and swap watched/brk
'================================================================
private sub exe_sav(exename As String,cmdline As String="")
	 Dim As Integer c
	 Dim As Double tempdate=FileDateTime(exename)
	If flagwtch=0 OrElse exedate<>tempdate Then
		'todo add watch_del()
	EndIf
    exedate=tempdate
    For i As Integer =0 To 8
    	If savexe(0)<>exename Then
    		Swap savexe(0),savexe(c)
    		Swap cmdexe(0),cmdexe(c)

    		For j As Integer=0 To WTCHMAX
				Swap wtchexe(0,j),wtchexe(c,j)
    		Next

    		For j As Integer=0 To BRKMAX
				Swap brkexe(0,j),brkexe(c,j)
    		Next

    		c+=1
    	Else
    		Exit For
    	End If
    Next
    savexe(0)=exename
    If cmdline<>"" Then cmdexe(0)=cmdline
    SetToolTipText(IDBUTRRUNE,TTRRUNE,exename)
    settitle()
End sub
'======================================================================
''loads the source code files, by slice : n contains the first to be loaded until sourcenb
''n=0 for the first loading
''=====================================================================
private sub source_load(n As integer,exedate as double)
	dim As integer flgt,fnum
	dim as any ptr ptrdoc
	if flagrestart Then
	   	for isrc As Integer=n To sourcenb ' main index =0
		   	print "loading ="+source(isrc)
		   	if FileExists(source(isrc))=0 Then
		   		messbox("Loading Source error","File : "+source(isrc)+" not found")
		   		continue For
		   	end if
		   	clear(sourcebuf(0),0,SRCSIZEMAX)
	    	fnum = FreeFile
	    	open source(isrc) For Binary As #fnum
	    	flgt=Lof(fnum)
	    	if flgt>SRCSIZEMAX Then
	    		messbox("Loading Source error","File : "+source(isrc)+" too large ("+Str(flgt)+">"+Str(SRCSIZEMAX)+") not loaded")
	    	else
		    	Get #fnum,,sourcebuf() 'get source
	    	end If
	    	Close #fnum

	    	''unicode
	    	'If buf(0)=&hEF AndAlso buf(1)=&hBB AndAlso buf(2)=&hBF Then 'UTF8
	    	'	'fb_message("","UTF8 "+hex(buf(0))+" "+hex(buf(1))+" "+hex(buf(2)))
	    	'	srcunicode(i)=1
	    	'	'setWindowTextW(richedit(i),CPtr(UShort ptr,@buf(3)))
	    	'ElseIf (buf(0)=&hFE AndAlso buf(1)=&hFF) OrElse (buf(0)=&hFF AndAlso buf(1)=&hFE) Then 'UTF16 FE or
	    	'	srcunicode(i)=1
	    	'	'setWindowTextW(richedit(i),CPtr(UShort ptr,@buf(2)))
	    	'ElseIf (buf(0)=&h00 AndAlso buf(1)=&h00 andalso buf(2)=&hFE AndAlso buf(3)=&hFF) _
	    	'OrElse (buf(0)=&hFF AndAlso buf(1)=&hFE AndAlso buf(2)=&h00 AndAlso buf(3)=&h00) Then 'UTF32
	    	'	srcunicode(i)=1
	    	'	'setWindowTextW(richedit(i),CPtr(UShort ptr,@buf(4)))
	    	'Else 'ascii
	    	'	srcunicode(i)=0
	    	'	'setWindowText(richedit(i),@buf(0))
	    	'EndIf
	    	if (sourcebuf(0)=&hFF AndAlso sourcebuf(1)=&hFE) then
	    		''todo
	    		''setWindowTextW(richedit(i),@sourcebuf(2))
			else

				if isrc=0 then
					''first file
					currentdoc=cast(any ptr,Send_sci(SCI_GETDOCPOINTER,0,0))
					sourceptr(0)=currentdoc
					Send_sci(SCI_SetText, 0, @sourcebuf(0))
					Send_sci(SCI_SETREADONLY,1,0)
				else
					ptrdoc=cast(any ptr,Send_sci(SCI_GETDOCPOINTER,0,0))
					Send_sci(SCI_ADDREFDOCUMENT,0,ptrdoc)
					''new document
					Send_sci(SCI_SETDOCPOINTER,0,0)
					sourceptr(isrc)=cast(any ptr,Send_sci(SCI_GETDOCPOINTER,0,0))
					Send_sci(SCI_SetText, 0, @sourcebuf(0))
					Send_sci(SCI_SETREADONLY,1,0)
				end if

	    	endif
			If FileDateTime (source(isrc))>exedate Then
				messbox("Loading source file","WARNING Date of "+source(isrc)+Chr(13)+" is > date of exe")
			EndIf
	   	next
	   	''selects the current doc after loading (if files (dll) have been added it can another than main file)
	   	if sourcenb<>0 then
		   	Send_sci(SCI_ADDREFDOCUMENT,0,sourceptr(sourcenb))
			Send_sci(SCI_SETDOCPOINTER,0,currentdoc)
		end if
''todo
		'EnableMenuItem(menutools,IDHIDLOG,MF_GRAYED) 'log file tab canceled so option menu grayed
	else 'restart with same exe, only the main files are not loaded, dll sources are removed

''todo change later
		'For i As Integer=sourcenb+1 To flagrestart
		'	setWindowText(richedit(i),""):ShowWindow(richedit(i),SW_HIDE)'hide all the exceding windows (>sourcenb)
		'	sendmessage(htab1,TCM_DELETEITEM ,i,0) 'delete tabs
		'Next
		flagrestart = true
	endif
End sub
''==============================================================================
'' retrieves the first line for main procedure as not provided by the debug data
''==============================================================================
private sub main_line
	For iproc As Integer =0 To procnb 'As .nu not consistent for main
		Dim As integer temp=proc(iproc).db
		If proc(iproc).nm="main" Then
			For iline As Integer =1 To linenb
				If rline(iline).ad>temp Then 'found first line of main
					proc(iproc).nu=rline(iline).nu
					rlineprev=iline
					print "first line of main="+str(iline)
					Exit For,For
				EndIf
			Next
		End If
	Next
end sub
'======================================================================
''releases the scintilla docs except the one attached to the window
'======================================================================
private sub release_doc
	var ptrdoc=cast(any ptr,Send_sci(SCI_GETDOCPOINTER,0,0))
	for isrc as integer =0 to sourcenb
		if sourceptr(isrc)<>ptrdoc then
			send_sci(SCI_RELEASEDOCUMENT,0,sourceptr(isrc))
		end if
	next
end sub
'=======================================================================
'' write some options on file for next launch of fbdebugger
'=======================================================================
private sub ini_write()
	Dim As Integer fileout
	If Dir(ExePath+slash+"fbdebugger.ini")<>"" Then
		If Dir(ExePath+slash+"fbdebuggersav.ini")<>"" Then Kill ExePath+slash+"fbdebuggersav.ini"
		Name (ExePath+slash+"fbdebugger.ini",ExePath+slash+"fbdebuggersav.ini")
    EndIf
    fileout=FreeFile
    Open ExePath+slash+"fbdebugger.ini" For Output As fileout

	For i As Integer = 0 To 9
		If savexe(i)<>"" Then
			Print #fileout,"[EXE]="+savexe(i)
			If cmdexe(i)<>"" Then Print #fileout,"[CMD]="+cmdexe(i)
			For j As Integer =0 To WTCHMAX
				If wtchexe(i,j)<>"" Then
					Print #fileout,"[WTC]="+wtchexe(i,j)
				Else
					Exit For
				EndIf
			Next

			For j As Integer =1 To BRKMAX
				If brkexe(i,j)<>"" Then
					Print #fileout,"[BRK]="+brkexe(i,j)
				EndIf
			Next
		End If
	Next
	Print #fileout,"[FTN]="+fontname
	Print #fileout,"[FTS]="+Str(fontsize)
	Print #fileout,"[FTC]="+Str(fontcolor)
	Print #fileout,"[LOG]="+Str(flaglog) 'type of log
	Print #fileout,"[TRC]="+Str(flagtrace) 'type of trace

	Print #fileout,"[WDX]="+Str(WindowX(hmain))
	Print #fileout,"[WDY]="+Str(WindowY(hmain))
	Print #fileout,"[WDW]="+str(WindowWidth(hmain))
	Print #fileout,"[WDH]="+str(WindowHeight(hmain))

	'Print #fileout,"[CHK]="+Str(clrkeyword) 'color highlighted keywords
	'Print #fileout,"[CCL]="+Str(clrcurline) 'color current line
	'Print #fileout,"[CTB]="+Str(clrtmpbrk) 'color tempo breakpoint
	'Print #fileout,"[CPB]="+Str(clrperbrk) 'color perm breakpoint
	'print #fileout,"[DPO]="+Str(dspofs)
	'If jitprev<>"" Then Print #fileout,"[JIT]="+jitprev
	'Print #fileout,"[PST]="+Str(procsort) 'type of procs sort

	close fileout
End sub
'======================================================================
'' read some options from previous launch of fbdebugger saved on file
'=======================================================================
private sub ini_read()

    Dim filein As Integer,lineread As String, c As Integer=-1,w As Integer,b As Integer
   Dim As Long lft,top,rgt,bot,p,q
    If Dir(ExePath+slash+"fbdebugger.ini")="" Then
       'fb_message("Init Error","fbdebugger.ini doesn't exist"+chr(10)+"compilation impossible")
       Exit Sub
    End If
    Filein = FreeFile
    Open ExePath+slash+"fbdebugger.ini" For Input As #Filein
    Do While Not Eof(Filein)
        Line Input #filein,lineread
		if Left(lineread,6)="[EXE]=" Then
        		lineread=RTrim(Mid(lineread,7))
        		If Dir(lineread)<>"" And InStr(LCase(lineread),".exe") Then
        			c+=1
        			savexe(c)=lineread:cmdexe(c)=""
        			w=-1:b=0
        		EndIf
        ElseIf Left(lineread,6)="[CMD]=" Then
        		cmdexe(c)=RTrim(Mid(lineread,7))
        ElseIf Left(lineread,6)="[WTC]=" Then
        		w+=1
        		wtchexe(c,w)=RTrim(Mid(lineread,7))
        ElseIf Left(lineread,6)="[BRK]=" Then
        		b+=1
        		brkexe(c,b)=RTrim(Mid(lineread,7))
		ElseIf Left(lineread,6)="[FTN]=" Then
    			restorefontname=RTrim(Mid(lineread,7))
        ElseIf Left(lineread,6)="[FTS]=" Then
        		restorefontsize=ValInt(RTrim(Mid(lineread,7)))
		ElseIf Left(lineread,6)="[FTC]=" Then 'color
            	restorefontcolor=ValInt(RTrim(Mid(lineread,7)))
			''todo set color for source font
		ElseIf Left(lineread,6)="[LOG]=" Then	'type of log
        		flaglog=ValInt(RTrim(Mid(lineread,7)))
        ElseIf Left(lineread,6)="[TRC]=" Then	'flagtrace
        		flagtrace=ValInt(RTrim(Mid(lineread,7)))
        'ElseIf Left(lineread,6)="[PST]=" Then	'type of proc sort
        '		procsort=ValInt(RTrim(Mid(lineread,7)))
		ElseIf Left(lineread,6)="[WDX]=" Then ''for restoring position/size of main window
				restorex=valint(RTrim(Mid(lineread,7)))
				if restorex<0 or restorex>1080 then ''security to not get a window out of visibility
					restorex=0
				end if
		ElseIf Left(lineread,6)="[WDY]=" Then ''for restoring position/size of main window
				restorey=valint(RTrim(Mid(lineread,7)))
				if restorey<0 or restorey>800 then ''security
					restorey=0
				end if
		ElseIf Left(lineread,6)="[WDW]=" Then ''for restoring position/size of main window
				restorew=valint(RTrim(Mid(lineread,7)))
		ElseIf Left(lineread,6)="[WDH]=" Then ''for restoring position/size of main window
				restoreh=valint(RTrim(Mid(lineread,7)))

            '' and modify values to avoid issue with display if erroneus values (negative)
            'If lft<GetSystemMetrics(SM_XVIRTUALSCREEN) Or lft>(GetSystemMetrics(SM_XVIRTUALSCREEN)+GetSystemMetrics(SM_CXVIRTUALSCREEN)) Then lft=GetSystemMetrics(SM_XVIRTUALSCREEN)
            'If (rgt-lft)<700 Then rgt=700+lft
            '
            'If top<GetSystemMetrics(SM_YVIRTUALSCREEN) Or top>(GetSystemMetrics(SM_YVIRTUALSCREEN)+GetSystemMetrics(SM_CYVIRTUALSCREEN)) Then top=GetSystemMetrics(SM_yVIRTUALSCREEN)
            'If (bot-top)<500 Then bot=500+top

           'SetWindowPos(windmain,HWND_NOTOPMOST,lft,top,rgt-lft,bot-top,SWP_NOACTIVATE Or SWP_FRAMECHANGED)
           'dsptyp=0
           'dsp_size
           'SetWindowPos(windmain,HWND_NOTOPMOST,lft,top,rgt-lft,bot-top,SWP_NOACTIVATE Or SWP_FRAMECHANGED)


		'elseif Left(lineread,6)="[CRK]=" Then	'color highlighted keywords
        '		clrkeyword=ValInt(RTrim(Mid(lineread,7)))
        'ElseIf Left(lineread,6)="[CCL]=" Then	'color current line
        '		clrcurline=ValInt(RTrim(Mid(lineread,7)))
        'ElseIf Left(lineread,6)="[CTB]=" Then	'color tempo breakpoint
        '		clrtmpbrk=ValInt(RTrim(Mid(lineread,7)))
		'ElseIf Left(lineread,6)="[CPB]=" Then	'color perm breakpoint
        '		clrperbrk=ValInt(RTrim(Mid(lineread,7)))
        'ElseIf Left(lineread,6)="[JIT]=" Then
        '	jitprev=RTrim(Mid(lineread,7))
		'ElseIf Left(lineread,6)="[DPO]=" Then
    	'	dspofs=ValInt(RTrim(Mid(lineread,7)))

        End If
    Loop
    Close #Filein
    exename=savexe(0)
    'todo fb_UpdateTooltip(fb_hToolTip,butrrune,"Restart "+exename,"",0)
End sub
'===================================================
'' Drag and drop
'===================================================
private sub drag_n_drop
	messbox("feature to be coded","drag_n_drop")
end sub
'-------------------------------------------
''launch by command line or file explorer
'-------------------------------------------
private sub external_launch()
	if command(0)="" then exit sub ''not launched by command line
	dim as string debuggee=command(1)
	if debuggee="" then exit sub ''not debuggee

	if instr(debuggee,slash)=0 then debuggee=exepath+slash+debuggee ''debugge without path so exepath added

	if check_bitness(debuggee)=0 then exit sub ''bitness of debuggee and fbdebugger not corresponding

	if kill_process("Trying to launch but debuggee still running")=FALSE then exit sub

	dim as string cmdline=mid(command(-1),len(debuggee)+2)

	reinit ''reinit all except GUI parts

	exename=debuggee
    exe_sav(exename,cmdline)

    'If ThreadCreate(@start_pgm)=0 Then todo add start_pgm and uncomment
    	'messbox("ERROR unable to start the thread managing the debuggee","Debuggee not running")
    'endif

end sub
'--------------------------------------------------------
'' Debuggee restarted, last debugged (using IDBUTRRUNE) or one of the 9/10 others
'--------------------------------------------------------
private sub restart(byval idx as integer=0)
	idx-=MNSTARTEXE
	messbox("Check exe number","restart"+str(idx)+" "+savexe(idx))
	if idx=0 then
		Dim As Double dtempo=FileDateTime(exename)
		If exedate<>0 AndAlso exedate=dtempo Then
			flagrestart=sourcenb
			''todo maybe cmdline changed so need to be saved
		EndIf
		If wtchcpt Then flagwtch=1
	else
		exename=savexe(idx)
	end if

	''todo make a sub and call also in sub external_launch, in select_file
	if check_bitness(exename)=0 then exit sub ''bitness of debuggee and fbdebugger not corresponding
	if kill_process("Trying to launch but debuggee still running")=FALSE then exit sub
	reinit ''reinit all except GUI parts

	'If ThreadCreate(@start_pgm)=0 Then todo add start_pgm and uncomment
	'messbox("ERROR unable to start the thread managing the debuggee","Debuggee not running")
    'endif
end sub
'--------------------------------------------------------
'' Debuggee provided by jitdebugger
'--------------------------------------------------------
private sub jitdbg()
	messbox("feature to be coded","attach")
	'p=instr(Command,"-p")
	'	If p<>0 And InStr(Command,"-e")<>0 And InStr(Command,"-g")<>0 Then 'started by
	'	dbgprocid=ValInt(Mid(Command,P+3))
	'	p=InStr(p+3,Command,"-e")
	'	p=ValInt(Mid(Command,P+3))
	'	hattach=Cast(HANDLE,p)
	'	ThreadCreate(@dbg_attach)
	'	Exit Sub
	'EndIf
end sub


