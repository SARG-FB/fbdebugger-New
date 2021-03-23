''button handle for fbdebugger_new
''dbg_buttons.bas

sub select_file()
	#Ifdef __fb_win32__
		var ddd= OpenFileRequester("Select exe file","C:\","Exe files (*.exe)"_
		+Chr(0)+"*.exe"+Chr(0))
	#else
		var ddd= OpenFileRequester("Select exe file","","Exe files)"_
		+Chr(0)+"*.*"+Chr(0))
	#endif
	If ddd="" then
		messbox("No file selected","")
		exename=""
		exit sub
	else
		exename=ddd
		messbox("File selected",exename)
	end if

	if check_bitness(exename)=0 then exit sub ''bitness of debuggee and fbdebugger not corresponding

	if kill_process("Trying to launch but debuggee still running")=FALSE then exit sub

	reinit ''reinit all except GUI parts

    exe_sav(exename,"")

    'If ThreadCreate(@start_pgm)=0 Then
    	'messbox("ERROR unable to start the thread managing the debuggee","Debuggee not running")
    'endif

end sub
'==============================================================
'' handles actions for each button
'==============================================================
sub button_action(button as integer)
	select case button
		case IDBUTSTEP 'STEP
			stopcode=0
			bcktrk_close
			thread_resume()
		case IDBUTSTEPP 'STEP+ over
			procin=procsk
			runtype=RTRUN
			but_enable()
			bcktrk_close
			thread_resume()
		Case IDBUTSTEPM 'STEP- out
			If (threadcur<>0 andalso proc_find(thread(threadcur).id,KLAST)<>proc_find(thread(threadcur).id,KFIRST)) _
			OrElse (threadcur=0 AndAlso proc(procr(proc_find(thread(0).id,KLAST)).idx).nm<>"main") Then 'impossible to go out first proc of thread, constructor for shared
				procad=procsv
				runtype=RTRUN
				but_enable()
			End If
			bcktrk_close
			thread_resume()
        Case IDBUTAUTO 'simple thread auto
			runtype=RTAUTO
			but_enable()
			bcktrk_close
			thread_resume()
		case IDBUTRUN
			runtype=RTRUN
			but_enable()
			bcktrk_close
			fasttimer=Timer
			thread_resume()
		case IDBUTSTOP
			If runtype=RTFREE Or runtype=RTFRUN Then
				runtype=RTFRUN 'to treat free as fast
				For i As Integer = 1 To linenb 'restore every breakpoint
					WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@breakcpu,1,0)
				Next
			Else
				runtype=RTSTEP:procad=0:procin=0:proctop=FALSE:procbot=0
			EndIf
			Stopcode=CSHALTBU
		case IDBUTFREE
		   If messbox("FREE","Release debugged prgm",MB_YESNO)=RETYES Then
				For i As Integer = 1 To linenb 'restore old instructions
					WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@rLine(i).sv,1,0)
				Next
				runtype=RTFREE
				but_enable()
				thread_resume()
			End If
			
		case IDBUTTOOL
			If Dir(ExePath+"\dbg_log_file.txt")="" Then 'the file can be deleted by user outside
				SetStateMenu(HMenutools,MNSHWLOG,1)
				SetStateMenu(HMenutools,MNDELLOG,1)
			Else
				SetStateMenu(HMenutools,MNSHWLOG,0)
				SetStateMenu(HMenutools,MNDELLOG,0)
			End If
            DisplayPopupMenu(HMenutools, GlobalMouseX,GlobalMouseY)
			
		case IDBUTFILE
			select_file()
			
		case IDBUTRERUN ''restart current exe
			restart()
			
		case IDBUTATTCH
			messbox("feature not implemented","button = IDBUTATTACH")
		case IDBUTKILL
			kill_process("Terminate immediatly no saved data, other option Release")
		case IDBUTLASTEXE
			var HMenuexe=CreatePopMenu()
			For iitem As integer =0 To 9
				If savexe(iitem)<>"" Then
					MenuItem(MNEXEFILE0+iitem,HMenuexe,savexe(iitem))
					If iitem=0 Then MenuBar(HMenuexe)
				EndIf
			Next
			DisplayPopupMenu(HMenuexe, GlobalMouseX,GlobalMouseY)
			Delete_Menu(HMenuexe)
		case IDBUTFASTRUN
			bcktrk_close
			but_enable()
			fastrun()
			send_sci(SCI_MarkerAdd, line_cursor-1, 4)
		case IDBUTEXEMOD
			messbox("feature not implemented","button = IDBUTEXEMOD")
        Case IDBUTSTEPB 'STEP at bottom of proc
			If rline(thread(threadcur).sv).ad<>proc(procsv).fn Then 'if current line is end of proc simple step
				procbot=procsv
				runtype=RTRUN
				but_enable()
			EndIf
			bcktrk_close
			thread_resume()
        Case IDBUTSTEPT 'STEP at top of proc
			If rline(thread(threadcur).sv).ad<>proc(procsv).fn Then 'if current line is end of proc simple step
				proctop=TRUE
				runtype=RTRUN
				but_enable()
			EndIf
			bcktrk_close
			thread_resume()
		case IDBUTCURSR
			messbox("Running to cursor","Source="+source(PanelGadgetGetCursel(GSRCTAB))+" line="+str(line_cursor))
			send_sci(SCI_MarkerAdd, line_cursor-1, 4)
		case IDBUTUPDATE
			if flagupdate=true then
				flagupdate=false
				load_button(IDBUTUPDATE,@"update.bmp",660,,@"Update On /Update off : variables, dump",,0)
				SetImageGadget(IDBUTUPDATE,Load_image("."+slash+"buttons"+slash+"noupdate.bmp"))
			else
				flagupdate=true
				SetImageGadget(IDBUTUPDATE,Load_image("."+slash+"buttons"+slash+"update.bmp"))
				var_sh()
				dump_sh()
			end if
		case IDBUTENLRSRC
			messbox("feature not implemented","button = IDBUTENLRSRC" )
		case IDBUTENLRVAR
			messbox("feature not implemented","button = IDBUTENLRVAR" )
		case IDBUTENLRMEM
			messbox("feature not implemented","button = IDBUTENLRMEM" )

		case GSCINTILLA

		case GNOLOG
			''todo update log
		case GSCREENLOG
			''todo update log
		case GFILELOG
			''todo update log
		case GBOTHLOG
			''todo update log
		'case GVERBOSE
		case GAUTODELAY

		case GCMDLPARAM
		
		case INPUTVALOK 
			input_check()
		case INPUTVALCANCEL
			inputval=""
		hidewindow(hinputbx,1)
		case else
			messbox("feature not implemented","sorry gadget="+str(button))
	end select
end sub
