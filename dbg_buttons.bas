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

sub button_action(button as integer)
	select case button
		case IDBUTSTEP
			rlinecur+=1
			linecur_change(rlinecur)
			messbox("feature not implemented","button = IDBUTSTEP")
		case IDBUTSTEPP
			messbox("feature not implemented","button = IDBUTSTEPP")
		case IDBUTSTEPM
			messbox("feature not implemented","button = IDBUTSTEPM")
		case IDBUTAUTO
			messbox("feature not implemented","button = IDBUTAUTO")
		case IDBUTRUN
			messbox("feature not implemented","button = IDBUTRUN")
		case IDBUTSTOP
			messbox("feature not implemented","button = IDBUTSTOP")
		case IDBUTFREE
			messbox("feature not implemented","button = IDBUTFREE")
		case IDBUTTOOL
			HideWindow(hsettings,0)
			messbox("feature not implemented","button = IDBUTTOOL")
		case IDBUTFILE
			select_file
		case IDBUTRERUN
			restart()
		case IDBUTATTCH
			messbox("feature not implemented","button = IDBUTATTACH")
		case IDBUTKILL
			messbox("feature not implemented","button = IDBUTKILL")
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
			messbox("feature not implemented","button = IDBUTFASTRUN")
			send_sci(SCI_MarkerAdd, line_cursor-1, 4)
		case IDBUTEXEMOD
			messbox("feature not implemented","button = IDBUTEXEMOD")
		case IDBUTSTEPB
			messbox("feature not implemented","button = IDBUTSTEPB")
		case IDBUTSTEPT
			messbox("feature not implemented","button = IDBUTSTEPT")
		case IDBUTCURSR
			messbox("Running to cursor","Source="+source(PanelGadgetGetCursel(GSRCTAB))+" line="+str(line_cursor))
			send_sci(SCI_MarkerAdd, line_cursor-1, 4)
		case IDBUTUPDATE
			messbox("feature not implemented","button = IDBUTUPDATE")
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
