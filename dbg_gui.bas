''gui for fbdebuuger_new
''dbg_gui.bas

'=====================
''Loading of buttons
'=====================
sub load_button(id as integer,button_name as zstring ptr,xcoord as integer,tooltiptext as zstring ptr=0,idtooltip as integer=-1,disab as long=1)
	Var HIMAGE=Load_image(*button_name)
	ButtonImageGadget(id,xcoord,0,30,26,HIMAGE,  BS_BITMAP)
	if tooltiptext then
		if idtooltip<>-1 then
			GadgetToolTip(id,*tooltiptext,idtooltip)
		else
			GadgetToolTip(id,*tooltiptext)
		endif
	end if
	disablegadget(id,disab)
end sub
'===================================================
'' changes the color/style of line in displayed src
'===================================================
sub line_color(byval pline as integer,byval style as ulong)
	var begpos=Send_sci(SCI_POSITIONFROMLINE,pline-1,0)
	var endpos=Send_sci(SCI_GETLINEENDPOSITION,pline-1,0)
	'begin styling at pos=c
	Send_sci(SCI_StartStyling, begpos, 0)
	 'style next chars with style #x
	Send_sci(SCI_SetStyling, endpos-begpos,style)
end sub
'==========================================================
'' displays current line after restoring previous one
'==========================================================
sub curline_display(linenew as integer)
	if srccur<>srcdisplayed then
		Send_sci(SCI_ADDREFDOCUMENT,0,sourceptr(srcdisplayed))
		Send_sci(SCI_SETDOCPOINTER,0,sourceptr(srccur))
		srcdisplayed=srccur
	end if
	line_color(linecur,KSTYLENONE)
	if rline(linenew).sx<>srcdisplayed then
		srccur=rline(linenew).sx
		Send_sci(SCI_ADDREFDOCUMENT,0,sourceptr(srcdisplayed))
		Send_sci(SCI_SETDOCPOINTER,0,sourceptr(srccur))
		srcdisplayed=srccur
	end if
	linecur=rline(linenew).nu
	line_color(linecur,KSTYLECUR)

	'' display in current line gadget
	var lgt=send_sci(SCI_LINELENGTH,linecur-1,0)
	var txt=space(lgt) + Chr(0)
	send_sci(SCI_GETLINE,linecur-1,strptr(txt))
	setgadgettext(GCURRENTLINE,txt)
end sub
'======================================
'' notification from scintilla gadget
'======================================
#ifdef __FB_WIN32__
	function getMessages(hwnd as hwnd , msg as UINteger , wparam as wparam , lparam as lparam) as Integer
		select case msg
			Case WM_NOTIFY
				dim as SCNotification ptr pSn = cast(SCNotification ptr , lparam) 'SCNotification ->https://www.scintilla.org/ScintillaDoc.html#Notifications
				if pSn->nmhdr.code = SCN_CHARADDED then
					? pSn->ch ' press keys and look in the console/terminal
				EndIf
				'? pSn->nmhdr.idFrom ' number gadget
				'? pSn->nmhdr.hwndFrom ' hwnd sciHWND
		end select
		return 0
	End Function
#else
	Sub getMessages cdecl(w as hwnd, p as gint, notification as SCNotification ptr, userData as gpointer )	
		dim as SCNotification ptr pSn = cast(SCNotification ptr , notification)
		if pSn->nmhdr.code = SCN_CHARADDED then
			? pSn->ch ' press keys and look in the console/terminal
		EndIf
		'? pSn->nmhdr.idFrom ' number gadget
		'? pSn->nmhdr.hwndFrom ' hwnd sciHWND
	End Sub
#endif
'============================
''create scintilla windows
'============================
sub create_sci(gadget as long, x as Long, y as Long , w as Long , h as Long  , Exstyle as integer = 0)
	dim as HWND hsci
	#ifdef __fb_win32__
		if dylibload("SciLexer.dll")=0 then ''todo if not loaded -->error and exit
		'if dylibload ( "D:\laurent_divers\fb dev\En-cours\FBDEBUG NEW\asm64_via_llvm\test_a_garder/Scintilla" )=0 then

			messbox("SciLexer.dll problem","dll not found"+chr(13)+"Quitting fbdebugger")
			end
		end if
		hsci = CreateWindowEx(Exstyle,"Scintilla","", WS_CHILD Or WS_VISIBLE Or WS_CLIPCHILDREN,x,y,w,h,Cast(HWND,win9GetCurrent()), Cast(HMENU,CInt(gadget)), 0, 0)
		win9AddNewGadget(gadget,hsci)
		setwindowcallback(cint(@getMessages) , 0) ' set callback for main window (mainHWND)	
	#else
		#inclib "scintilla"
		dim as GtkWidget ptr editor
		dim as ScintillaObject ptr sci
		Dim As HWND  vBox , mainBox
		dim as ListT ptr pListTemp
		editor = scintilla_new()
		sci = SCINTILLA(editor)
		pListTemp = cast(ListT ptr,pGlobalTypeWindow9->ListWinAndContainers->findNodeFunc(cint(pGlobalTypeWindow9->CurentHwnd)))		
		mainBox = cast(hwnd , pListTemp->anyTwoData)
		vbox = gtk_fixed_new()
		gtk_container_add (GTK_CONTAINER(mainBox), vbox)
		gtk_fixed_put(GTK_FIXED(vbox), editor , x , y)
		scintilla_set_id(sci, gadget)
		gtk_widget_set_size_request(editor, w, h)
		g_signal_connect(G_OBJECT(sci), "sci-notify", G_CALLBACK (@getMessages), 0)
		gtk_widget_show_all(pGlobalTypeWindow9->CurentHwnd)
		gtk_widget_grab_focus(GTK_WIDGET(editor))
		hsci=cast(hwnd, sci)
	#endif
	scint=hsci ''need to be done as used in send_sci
	
	send_sci(SCI_SETMARGINTYPEN,0,SC_MARGIN_NUMBER )
	send_sci(SCI_SETMARGINWIDTHN,0,40)
	send_sci(SCI_SETMARGINTYPEN,1,SC_MARGIN_SYMBOL )
	send_sci(SCI_SETMARGINWIDTHN,1,12)
	send_sci(SCI_SETFOLDMARGINCOLOUR,0,BLACK_BRUSH )
	
	'Set default FG/BG
	send_sci(SCI_SetLexer, SCLEX_Null, 0)
	send_sci(SCI_StyleSetFore, STYLE_DEFAULT, &h404040)''grey
	send_sci(SCI_StyleSetBack, STYLE_DEFAULT, &hFFFFFF) ''white background
	send_sci(SCI_StyleClearAll, 0, 0)     ''set all styles to style_default
	
	''markers
	send_sci(SCI_MarkerDefine, 0,SC_MARK_CIRCLE)
	send_sci(SCI_MarkerDefine, 1,SC_MARK_FULLRECT)
	send_sci(SCI_MarkerDefine, 2,SC_MARK_ARROW)
	send_sci(SCI_MarkerDefine, 3,SC_MARK_SMALLRECT)
	send_sci(SCI_MarkerDefine, 4,SC_MARK_SHORTARROW)
	send_sci(SCI_MarkerDefine, 5,SC_MARK_CHARACTER+65)
	''color markers
	send_sci(SCI_MARKERSETFORE,0,KBLUE)
	send_sci(SCI_MARKERSETBACK,0,KBLUE)
	send_sci(SCI_MARKERSETFORE,1,KRED)
	send_sci(SCI_MARKERSETBACK,1,KRED)
	send_sci(SCI_MARKERSETFORE,2,KRED)
	send_sci(SCI_MARKERSETBACK,2,KRED)
	send_sci(SCI_MARKERSETFORE,3,KORANGE)
	send_sci(SCI_MARKERSETBACK,3,KORANGE)
	
	send_sci(SCI_StyleSetFore, 2, KRED)    ''style #2 FG set to red
	send_sci(SCI_StyleSetBack, 2, KYELLOW) ''style #2 BB set to green

	for imark as Integer = 0 To 5
	    send_sci(SCI_SetMarginMaskN, 1,-1)  ''all symbols allowed
	next
	'SendMessage(sciHWND, SCI_SETCODEPAGE, SC_CP_UTF8 ,0)
	send_sci(SCI_SETLEXER, SCLEX_FREEBASIC, 0 )
	send_sci(SCI_SETKEYWORDS,0, @"sub function operator constructor destructor")
	send_sci(SCI_STYLESETFORE, SCE_B_CONSTANT, 0)
	send_sci(SCI_STYLESETFORE, SCE_B_KEYWORD, &hff00ff)
	
End sub
private sub gui_init

	''main windows
	mainwindow=OpenWindow("New FBDEBUGGER with window9 :-)",10,10,1100,500)
	
	''scintilla gadget
	create_sci(GSCINTILLA,0,65,400,WindowClientHeight(mainwindow)-90,)

	''source panel
	'Var font=LoadFont("Arial",40)

	PanelGadget(GSRCTAB,2,42,400,20)
    SetGadgetFont(GSRCTAB,CINT(LoadFont("Courier New",11)))	
		
	''file combo/buuton ''idee mettre dans le menu affichage de la liste (du combo)
	ComboBoxGadget(GFILELIST,790,0,200,80)
	ButtonGadget(GFILESEL,992,2,30,20,"Go")
	
	''status bar
	StatusBarGadget(1,"")
	SetStatusBarField(1,0,100,"Status")
	SetStatusBarField(1,1,200,"Thread number")
	setstatusbarfield(1,2,-1,"Current file")
	
	''current line
	textGadget(GCURRENTLINE,2,28,400,20,"Next exec line : ",SS_NOTIFY )
	GadgetToolTip(GCURRENTLINE,"next executed line"+chr(13)+"Click on me to reach the line",GCURLINETTIP)


	''buttons
	load_button(IDBUTSTEP,@".\buttons\step.bmp",8,@"[S]tep/line by line",)
	load_button(IDCONTHR,@".\buttons\runto.bmp",40,@"Run to [C]ursor",)
	load_button(IDBUTSTEPP,@".\buttons\step_over.bmp",72,@"Step [O]ver sub/func",)
	load_button(IDBUTSTEPT,@".\buttons\step_start.bmp",104,@"[T]op next called sub/func",)
	load_button(IDBUTSTEPB,@".\buttons\step_end.bmp",136,@"[B}ottom current sub/func",)
	load_button(IDBUTSTEPM,@".\buttons\step_out.bmp",168,@"[E]xit current sub/func",)
	load_button(IDBUTAUTO,@".\buttons\auto.bmp",200,@"Step [A]utomatically, stopped by [H]alt",)
	load_button(IDBUTRUN,@".\buttons\run.bmp",232,@"[R]un, stopped by [H]alt",)
	load_button(IDBUTSTOP,@".\buttons\stop.bmp",264,@"[H]alt running pgm",)
	load_button(IDFASTRUN,@".\buttons\fastrun.bmp",328,@"[F]AST Run to cursor",)
	load_button(IDEXEMOD,@".\buttons\exemod.bmp",360,@"[M]odify execution, continue with line under cursor",)
	load_button(IDBUTFREE,@".\buttons\free.bmp",392,@"Release debuged prgm",)
	load_button(IDBUTKILL,@".\buttons\kill.bmp",424,@"CAUTION [K]ill process",)
	load_button(IDBUTRRUNE,@".\buttons\restart.bmp",466,@"Restart debugging (exe)",,0)
	load_button(IDLSTEXE,@".\buttons\multiexe.bmp",498,@"Last 10 exe(s)",,0)
	load_button(IDBUTATTCH,@".\buttons\attachexe.bmp",530,@"Attach running program",,0)
	load_button(IDBUTFILE,@".\buttons\files.bmp",562,@"Select EXE/BAS",,0)
	load_button(IDNOTES,@".\buttons\notes.bmp",600,@"Open or close notes",,0)
	''missing line for the icon of the second notes
	load_button(IDBUTTOOL,@".\buttons\tools.bmp",632,"Some usefull....Tools",,0)
	load_button(IDUPDATE,@".\buttons\update.bmp",663,@"Update On /Update off : variables, dump",,0)
	load_button(ENLRSRC,@".\buttons\source.bmp",690,@"Enlarge/reduce source",)
	load_button(ENLRVAR,@".\buttons\varproc.bmp",720,@"Enlarge/reduce proc/var",)
	load_button(ENLRMEM,@".\buttons\memory.bmp",750,@ "Enlarge/reduce dump memory",)
	
	''bmb(25)=Loadbitmap(fb_hinstance,Cast(LPSTR,MAKEINTRESOURCE(1025))) 'if toogle noupdate
	''no sure to implement this one	 
	''load_button(IDBUTMINI,@".\buttons\minicmd.bmp",296,@ "Mini window",)
	
	''icon on title bar
	''-----> ONLY WINDOWS
	'Var icon=LoadIcon(null,@"D:\telechargements\win9\tmp\fbdebugger.ico")
	'print icon,getlasterror()
	'    'SendMessage(hwnd,WM_SETICON,ICON_BIG,Cast(Lparam,icon))
	'    sendmessage(hwnd,WM_SETICON,ICON_SMALL,Cast(Lparam,LoadIcon(GetModuleHandle(0),@".\fbdebugger.ico")))
	'Var icon=LoadIcon(GetModuleHandle(0),MAKEINTRESOURCE(100))
	'  SendMessage(hwnd,WM_SETICON,ICON_BIG,Cast(Lparam,icon))
	'  SendMessage(hwnd,WM_SETICON,ICON_SMALL,Cast(Lparam,icon))
	'D:\telechargements\win9\tmp\
	var icon=loadimage(0,@"fbdebugger.ico",IMAGE_ICON,0,0,LR_LOADFROMFILE or LR_DEFAULTSIZE)
	sendmessage(mainwindow,WM_SETICON,ICON_BIG,Cast(Lparam,icon))
	
	''right panels
	PanelGadget(GRIGHTTABS,500,30,499,300)
	SetGadgetFont(GRIGHTTABS,CINT(LoadFont("Courier New",11)))
	''treeview proc/var
	var htabvar=AddPanelGadgetItem(GRIGHTTABS,0,"Proc/var",,1)
	'var hbmp = load_Icon("1.ico")
	'var hbmp1 = load_Icon("2.ico")	
	treeviewgadget(GTVIEWVAR,0,0,499,299,KTRRESTYLE)
	''filling treeview for example
	var Pos_=AddTreeViewItem(GTVIEWVAR,"Myvar udt ",cast (hicon, 0),cast (hicon, 0),0,0)
	AddTreeViewItem(GTVIEWVAR,"first field",cast (hicon, 0),0,1,Pos_)
	Pos_=AddTreeViewItem(GTVIEWVAR,"my second var",cast (hicon, 0),0,0)
	AddTreeViewItem(GTVIEWVAR,"first field",cast (hicon, 0),0,0,Pos_)
	
	HideWindow(htabvar,0)
	''treeview procs
	var htabprc=AddPanelGadgetItem(GRIGHTTABS,1,"Procs",,1)
	treeviewgadget(GTVIEWPRC,0,0,499,299,KTRRESTYLE)
	AddTreeViewItem(GTVIEWPRC,"first proc",cast (hicon, 0),0,0)
	AddTreeViewItem(GTVIEWPRC,"second proc",cast (hicon, 0),0,0)
	AddTreeViewItem(GTVIEWPRC,"third proc",cast (hicon, 0),0,0)
	''treeview threads
	var htabthrd=AddPanelGadgetItem(GRIGHTTABS,2,"Threads")
	''treeview watched
	var htabwatch=AddPanelGadgetItem(GRIGHTTABS,3,"Watched")
	
	''dump memory
	var htabmem=AddPanelGadgetItem(GRIGHTTABS,4,"Memory",,1)
	ListViewGadget(GDUMPMEM,0,0,499,299,LVS_EX_GRIDLINES)
	AddListViewColumn(GDUMPMEM, "Address",0,0,100)
	for icol as integer =1 to 4
		AddListViewColumn(GDUMPMEM, "+0"+str((icol-1)*4),icol,icol,40)
	next
	AddListViewColumn(GDUMPMEM, "Ascii value",5,5,100)
	
end sub

