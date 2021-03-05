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
	
	if check_bitness(debuggee)=0 then exit sub ''bitness of debuggee and fbdebugger not corresponding
	
	if kill_process("Trying to launch but debuggee still running")=FALSE then exit sub	
	
	reinit ''reinit all except GUI parts
	
    exe_sav(exename,"")

    If ThreadCreate(@start_pgm)=0 Then
    	messbox("ERROR unable to start the thread managing the debuggee","Debuggee not running")
    endif	
	
end sub




sub button_action(button as integer)
	select case button
		case IDBUTSTEP
			rlinecur+=1
			linecur_change(rlinecur)
		'case IDBUTSTEPP
		'case IDBUTSTEPM
		'case IDBUTAUTO
		'case IDBUTRUN
		'case IDBUTSTOP
		'case IDBUTMINI
		'case IDBUTFREE
		case IDBUTTOOL
			HideWindow(hsettings,0)
		case IDBUTFILE
			select_file
		'case IDBUTRRUNE
		'case IDBUTATTCH
		'case IDBUTKILL
		'case IDNOTES
		'case IDLSTEXE
		case IDFASTRUN
			send_sci(SCI_MarkerAdd, line_cursor-1, 4)
		'case IDEXEMOD
		'case IDBUTSTEPB
		'case IDBUTSTEPT
		case IDCONTHR ''503 'used also with button
			messbox("Running to cursor","Source="+source(PanelGadgetGetCursel(GSRCTAB))+" line="+str(line_cursor))
			send_sci(SCI_MarkerAdd, line_cursor-1, 4)
		'case IDUPDATE
		
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
		
		case else
			messbox("feature not implemented","sorry gadget="+str(button))
	end select
end sub
