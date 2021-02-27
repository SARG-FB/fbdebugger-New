''button handle for fbdebugger_new
''dbg_buttons.bas

sub select_file
	#Ifdef __fb_win32__
		var ddd= OpenFileRequester("Select exe file","C:\","Exe files (*.exe)"_
		+Chr(0)+"*.exe"+Chr(0))
	#else
		var ddd= OpenFileRequester("Select exe file","C:\","Exe files)"_
		+Chr(0)+"*.*"+Chr(0))
	#endif
	If ddd="" then
		messbox("No file selected",exename)
		exename=""
	else
		exename=ddd
		messbox("File selected",exename)
	end if
end sub







sub button_action(button as integer)
	select case button
		'case IDBUTSTEP
		'case IDBUTSTEPP
		'case IDBUTSTEPM
		'case IDBUTAUTO
		'case IDBUTRUN
		'case IDBUTSTOP
		'case IDBUTMINI
		'case IDBUTFREE
		'case IDBUTTOOL
		case IDBUTFILE
			select_file
		'case IDBUTRRUNE
		'case IDBUTATTCH
		'case IDBUTKILL
		'case IDNOTES
		'case IDLSTEXE
		'case IDFASTRUN
		'case IDEXEMOD
		'case IDBUTSTEPB
		'case IDBUTSTEPT
		'case IDCONTHR ''503 'used also with button
		'case IDUPDATE
		case else
			messbox("feature not implemented","sorry")
	end select
end sub
