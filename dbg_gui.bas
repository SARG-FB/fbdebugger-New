''gui for fbdebuuger_new
''dbg_gui.bas

'================================================================================
'' finds child of an item
'================================================================================
private function FindChildTreeviewItem(iGadget as long, Item As Integer ) as integer
	#ifdef __fb_win32__
		return sendmessage(Gadgetid(iGadget),TVM_GETNEXTITEM,TVGN_CHILD,Cast(LPARAM,item))
	#else
		messbox("Missing feature for linux","FindChildtreeviewItem")
		return 0
	#endif
end function
'================================================================================
'' finds parent of an item
'================================================================================
private function FindParentTreeviewItem(iGadget as long, Item As Integer ) as integer
	#ifdef __fb_win32__
		return sendmessage(Gadgetid(iGadget),TVM_GETNEXTITEM,TVGN_PARENT,Cast(LPARAM,item))
	#else
		messbox("Missing feature for linux","FindParentTreeviewItem")
		return 0
	#endif
end function
'=======================================================================================
'' displays an item in a treeview (can be removed when the feature will be added in W9
'=======================================================================================
private Sub SetSelectTreeViewItem(iGadget As Long  , Item As Integer )
	#ifdef __fb_win32__
		setfocus(Gadgetid(iGadget))
		sendmessage(Gadgetid(iGadget) , TVM_SELECTITEM ,  TVGN_CARET , Cast(lparam, Item))
	#else
		Dim As Any Ptr treeview = Gadgetid(iGadget)
		Dim As Any Ptr treestore = Cast(Any Ptr , gtk_tree_view_get_model(treeview))
		Dim As GtkTreeSelection Ptr selection = gtk_tree_view_get_selection (treeview)
		Dim As GtkTreeIter iter , iterfirst
		Dim As GtkTreePath Ptr path
		If gtk_tree_model_get_iter_first(treestore , @iterfirst) Then
			iter.stamp =  iterfirst.stamp
			iter.user_data = Cast(Any Ptr,Item)
			path = gtk_tree_model_get_path(treestore, @iter)
			gtk_tree_view_scroll_to_cell (treeview, path, NULL, True, 0.5, 0.0)
			gtk_tree_view_expand_to_path(treeview,path)
			If gtk_tree_view_row_expanded(treeview , path) Then
				gtk_tree_view_collapse_row(treeview , path )
			Endif
			gtk_tree_selection_select_path(selection, path)		
			gtk_tree_path_free(path)		
		Endif
	#endif
End Sub
'=====================
''Loading of buttons
'=====================
private sub load_button(id as integer,button_name as zstring ptr,xcoord as integer,tooltiptext as zstring ptr=0,idtooltip as integer=-1,disab as long=1)
	Var HIMAGE=Load_image("."+slash+"buttons"+slash+*button_name)
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
'============================================
''changes the displayed source
'============================================
private sub source_change(numb as integer)
	static as integer numbold=-1
	dim as any ptr ptrdoc
	if numb=numbold then exit sub
	numbold=numb
	ptrdoc=cast(any ptr,Send_sci(SCI_GETDOCPOINTER,0,0))
	Send_sci(SCI_ADDREFDOCUMENT,0,ptrdoc)
	Send_sci(SCI_SETDOCPOINTER,0,sourceptr(numb))
end sub
'===================================================
'' return line where is the cursor
'===================================================
private function line_cursor() as integer
	return Send_sci(SCI_LINEFROMPOSITION,Send_sci(SCI_GETCURRENTPOS,0,0),0)+1
end function
'===================================================
'' changes the color/style of line in displayed src
'===================================================
private sub line_color(byval pline as integer,byval style as ulong)
	var begpos=Send_sci(SCI_POSITIONFROMLINE,pline-1,0)
	var endpos=Send_sci(SCI_GETLINEENDPOSITION,pline-1,0)
	'begin styling at pos
	Send_sci(SCI_StartStyling, begpos, 0)
	'style next chars with style #x
	Send_sci(SCI_SetStyling, endpos-begpos,style)
end sub
'==========================================================
'' displays line
'==========================================================
private sub line_display(pline as integer)
	send_sci(SCI_SETFIRSTVISIBLELINE, pline,0)
	if pline-send_sci(SCI_GETFIRSTVISIBLELINE,0,0)+5>send_sci(SCI_LINESONSCREEN,0,0) then
		send_sci(SCI_LINESCROLL,0,+5)
	else
		send_sci(SCI_LINESCROLL,0,-5)
	end if
	'print send_sci(SCI_GETFIRSTVISIBLELINE,0,0)
end sub
'==========================================================
'' displays line current
'==========================================================
private sub linecur_display()
	source_change(srccur)
	line_display(linecur)
end sub
'=========================================================
'' selects the text of line
'=========================================================
function line_text(pline as integer)as string
	var lgt=send_sci(SCI_LINELENGTH,pline,0)
	var txt=space(lgt) + Chr(0)
	send_sci(SCI_GETLINE,pline,strptr(txt))
	return txt
end function
'==========================================================
'' changes current line after restoring previous one
'==========================================================
private sub linecur_change(linenew as integer)
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
	linecur_display()
	'' display in current line gadget
	setgadgettext(GCURRENTLINE,"Current line : "+line_text(linecur-1))
end sub
'===================================================
'' set/unset breakpoint markers
'===================================================
sub brk_marker(brkidx as integer)
	dim as integer src,pline=brkol(brkidx).nline,typ
	
	if brkol(brkidx).typ>2 then
		typ=4 ''disabled --> marker
	else
		if brkol(brkidx).cntrsav then
			typ=3 ''if counter<>0 --> marker 3
		else
			typ=brkol(brkidx).typ  ''permanent or tempo --> marker 1 or 2
		EndIf
	EndIf
	
	source_change(brkol(brkidx).isrc)
	
	if typ then
		send_sci(SCI_MARKERADD, pline-1, typ)
	else
		send_sci(SCI_MARKERDELETE, pline-1, -1)
	end if
end sub
'======================================
'' notification from scintilla gadget
'======================================
#ifdef __FB_WIN32__
	private function getMessages(hwnd as hwnd , msg as UINteger , wparam as wparam , lparam as lparam) as Integer
		select case msg
			Case WM_NOTIFY
				dim as SCNotification ptr pSn = cast(SCNotification ptr , lparam) 'SCNotification ->https://www.scintilla.org/scintillaDoc.html#Notifications
				if pSn->nmhdr.code = SCN_CHARADDED then
					? pSn->ch ' press keys and look in the console/terminal
				EndIf
				'? pSn->nmhdr.idFrom ' number gadget
				'? pSn->nmhdr.hwndFrom ' hwnd sciHWND
		end select
		return 0
	End Function
#else
	private sub getMessages cdecl(w as hwnd, p as gint, notification as SCNotification ptr, userData as gpointer )	
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
private sub create_sci(gadget as long, x as Long, y as Long , w as Long , h as Long  , Exstyle as integer = 0)
	dim as HWND hsci
	#ifdef __fb_win32__
		if dylibload("SciLexer.dll")=0 then ''todo if not loaded -->error and exit
		'if dylibload ( "D:\laurent_divers\fb dev\En-cours\FBDEBUG NEW\asm64_via_llvm\test_a_garder/scintilla" )=0 then

			messbox("SciLexer.dll problem","dll not found"+chr(13)+"Quitting fbdebugger")
			end
		end if
		hsci = CreateWindowEx(Exstyle,"scintilla","", WS_CHILD Or WS_VISIBLE Or WS_CLIPCHILDREN,x,y,w,h,Cast(HWND,win9GetCurrent()), Cast(HMENU,CInt(gadget)), 0, 0)
		win9AddNewGadget(gadget,hsci)
		setwindowcallback(cint(@getMessages) , 0) ' set callback for main window (mainHWND)	
	#else
		#inclib "scintilla"
		dim as GtkWidget ptr editor
		dim as scintillaObject ptr sci
		Dim As HWND  vBox , mainBox
		dim as ListT ptr pListTemp
		editor = scintilla_new()
		sci = scintILLA(editor)
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
	hscint=hsci ''need to be done as used in send_sci
	
	send_sci(SCI_SETMARGINTYPEN,0,SC_MARGIN_NUMBER )
	send_sci(SCI_SETMARGINWIDTHN,0,40)
	send_sci(SCI_SETMARGINTYPEN,1,SC_MARGIN_SYMBOL )
	send_sci(SCI_SETMARGINWIDTHN,1,12)
	send_sci(SCI_SETFOLDMARGINCOLOUR,0,&h202020 )
	
	'Set default FG/BG
	send_sci(SCI_SetLexer, SCLEX_Null, 0)
	send_sci(SCI_StyleSetFore, STYLE_DEFAULT, &h404040)''grey
	send_sci(SCI_StyleSetBack, STYLE_DEFAULT, &hFFFFFF) ''white background
	send_sci(SCI_StyleClearAll, 0, 0)     ''set all styles to style_default
	
	''markers
	''SC_MARK_CIRCLE SC_MARK_FULLRECT SC_MARK_ARROW SC_MARK_SMALLRECT SC_MARK_SHORTARROW
	send_sci(SCI_MarkerDefine, 0,SC_MARK_CIRCLE)
	send_sci(SCI_MarkerDefine, 1,SC_MARK_FULLRECT)
	send_sci(SCI_MarkerDefine, 2,SC_MARK_FULLRECT)
	send_sci(SCI_MarkerDefine, 3,SC_MARK_FULLRECT)
	send_sci(SCI_MarkerDefine, 4,SC_MARK_FULLRECT)
	send_sci(SCI_MarkerDefine, 5,SC_MARK_SHORTARROW)
	''color markers
	send_sci(SCI_MARKERSETFORE,0,KRED)
	send_sci(SCI_MARKERSETBACK,0,KWHITE)
	send_sci(SCI_MARKERSETFORE,1,KRED)
	send_sci(SCI_MARKERSETBACK,1,KRED)
	send_sci(SCI_MARKERSETFORE,2,KORANGE)
	send_sci(SCI_MARKERSETBACK,2,KORANGE)
	send_sci(SCI_MARKERSETFORE,3,KPURPLE)
	send_sci(SCI_MARKERSETBACK,3,KPURPLE)
	send_sci(SCI_MARKERSETFORE,4,KGREY)
	send_sci(SCI_MARKERSETBACK,4,KGREY)
	send_sci(SCI_MARKERSETFORE,5,KGREEN)
	send_sci(SCI_MARKERSETBACK,5,KGREEN)
	
	send_sci(SCI_StyleSetFore, 2, KRED)    ''style #2 FG set to red
	send_sci(SCI_StyleSetBack, 2, KYELLOW) ''style #2 BB set to green

	for imark as Integer = 0 To 5
	    send_sci(SCI_SetMarginMaskN, 1,-1)  ''all symbols allowed
	next
	'SendMessage(sciHWND, SCI_SETCODEPAGE, SC_CP_UTF8 ,0)
	'send_sci(SCI_SETLEXER, SCLEX_FREEBASIC, 0 )
	'send_sci(SCI_SETKEYWORDS,0, @"sub function operator constructor destructor")
	'send_sci(SCI_STYLESETFORE, SCE_B_CONSTANT, 0)
	'send_sci(SCI_STYLESETFORE, SCE_B_KEYWORD, &hff00ff)
	
End sub
'===========================================================
''set the title of main window
'===========================================================
private sub settitle()
	dim as string title="Fbdebugger "+ver3264+exename
	setwindowtext(hmain,strptr(title))
end sub
'=============================================
'' settings window
'=============================================
private sub create_settingsbx()
	hsettings=OpenWindow("Settings",10,10,500,500)
	centerWindow(hsettings)
	groupgadget(LOGGROUP,10,10,450,85,"Log  fbdebugger path"+slash+"dbg_log.txt")
	optiongadget(GNOLOG,12,32,80,18,"No log")
	SetGadgetState(GNOLOG,1)''set on overriden by read_ini
	optiongadget(GSCREENLOG,102,32,80,18,"Screen")
	optiongadget(GFILELOG,192,32,80,18,"File")
	optiongadget(GBOTHLOG,282,32,80,18,"Both")
	CheckBoxGadget(GTRACEPROC,12,70,220,15,"Trace on for proc")
	CheckBoxGadget(GTRACELINE,232,70,220,15,"Trace on for line")
	CheckBoxGadget(GVERBOSE,12,100,220,15,"Verbose Mode On for proc/var")
	textgadget(GTEXTDELAY,12,125,200,15,"50< delay auto (ms) <10000",0)
	stringgadget(GAUTODELAY,210,125,50,15,str(autostep))
	textgadget(GTEXTCMDLP,12,155,200,15,"Command line parameters",0)
	stringgadget(GCMDLPARAM,210,155,200,15,cmdexe(0))
	
	groupgadget(FONTGROUP,10,240,450,80,"Font for source code")
	textgadget(GTEXTFTYPE,12,260,200,15,"type",0)
	textgadget(GTEXTFSIZE,12,280,200,15,"size",0)
	textgadget(GTEXTFCOLOR,12,300,200,15,"color",0)
	
end sub
'=============================================
'' inputval window
'=============================================
private sub create_inputbx()
	hinputbx=OpenWindow("",10,10,80,150)
	centerWindow(hinputbx)
	StringGadget(GINPUTVAL,10,10,100,15,"")
	ButtonGadget (INPUTVALOK, 10, 40, 60, 15,  "Ok")
	ButtonGadget (INPUTVALCANCEL, 10, 40, 60, 15,  "Cancel")
	hidewindow(hinputbx,1)
end sub	
'=========================================================================
'' enables or disables buttons according the status and updates status
'=========================================================================
private sub but_enable()
 	select Case runtype
    	Case RTSTEP 'wait
			DisableGadget(IDBUTSTEP,0)
			DisableGadget(IDBUTSTEPP,0)
			DisableGadget(IDBUTSTEPT,0)
			DisableGadget(IDBUTSTEPB,0)
			DisableGadget(IDBUTSTEPM,0)
			DisableGadget(IDBUTAUTO,0)
			DisableGadget(IDBUTRUN,0)
			DisableGadget(IDBUTFASTRUN,0)
			DisableGadget(IDBUTSTOP,0)
			DisableGadget(IDBUTCURSR,0)
			DisableGadget(IDBUTFREE,0)
			DisableGadget(IDBUTKILL,0)
			DisableGadget(IDBUTEXEMOD,0)
			
			SetStatusBarField(1,0,100,"Waiting "+stoplibel(stopcode))
			SetStatusBarField(1,1,200,"Thread "+Str(thread(threadcur).id))
			SetStatusBarField(1,2,300,"Thread "+Str(thread(threadcur).id))
			SetStatusBarField(1,3,400,source_name(source(proc(procsv).sr)))
			SetStatusBarField(1,4,500,proc(procsv).nm)
			SetStatusBarField(1,5,-1,left(Str(fasttimer),10))
			'todo frground()
   		case RTRUN,RTFREE,RTFRUN 'step over / out / free / run / fast run
			DisableGadget(IDBUTSTEP,1)
			DisableGadget(IDBUTSTEPP,1)
			DisableGadget(IDBUTSTEPT,1)
			DisableGadget(IDBUTSTEPB,1)
			DisableGadget(IDBUTSTEPM,1)
			DisableGadget(IDBUTAUTO,1)
			DisableGadget(IDBUTRUN,1)
			DisableGadget(IDBUTFASTRUN,1)
			DisableGadget(IDBUTCURSR,1)
			DisableGadget(IDBUTFREE,1)
			''DisableGadget(IDBUTkill,1) ''to let the possibility to kill the debuggee when running
			DisableGadget(IDBUTEXEMOD,1)
			Select Case runtype
				Case RTRUN
					SetStatusBarField(1,0,100,"Running")
				Case RTFRUN
					SetStatusBarField(1,0,100,"FAST Running")
				Case else
					SetStatusBarField(1,0,100,"Released")
			End Select
    	Case RTAUTO 'auto
			DisableGadget(IDBUTSTEP,1)
			DisableGadget(IDBUTSTEPP,1)
			DisableGadget(IDBUTSTEPT,1)
			DisableGadget(IDBUTSTEPB,1)
			DisableGadget(IDBUTSTEPM,1)
			DisableGadget(IDBUTAUTO,1)
			DisableGadget(IDBUTRUN,1)
			DisableGadget(IDBUTFASTRUN,1)
			DisableGadget(IDBUTCURSR,1)
			DisableGadget(IDBUTFREE,1)
			DisableGadget(IDBUTKILL,1)
			DisableGadget(IDBUTEXEMOD,1)
      		SetStatusBarField(1,0,100,"Auto")
   		case Else 'prun=1 --> terminated or no pgm
			DisableGadget(IDBUTSTEP,1)
			DisableGadget(IDBUTSTEPP,1)
			DisableGadget(IDBUTSTEPT,1)
			DisableGadget(IDBUTSTEPB,1)
			DisableGadget(IDBUTSTEPM,1)
			DisableGadget(IDBUTAUTO,1)
			DisableGadget(IDBUTRUN,1)
			DisableGadget(IDBUTFASTRUN,1)
			DisableGadget(IDBUTSTOP,1)
			DisableGadget(IDBUTCURSR,1)
			DisableGadget(IDBUTFREE,1)
			DisableGadget(IDBUTKILL,1)
			DisableGadget(IDBUTEXEMOD,1)
    	  	If runtype=RTEND Then SetStatusBarField(1,0,100,"Terminated")
   	End Select
End Sub

'=============================================================
'' enables or disables menu options according the status
'=============================================================
private sub menu_enable()
	dim flag As Integer
	If prun Then flag=0 Else flag=1
	SetStateMenu(HMenusource,MNSETBRK,  flag)
	SetStateMenu(HMenusource,MNSETBRT,  flag)
	SetStateMenu(HMenusource,MNSETBRKC, flag)
	SetStateMenu(HMenusource,MNRSTBRKC, flag)
	SetStateMenu(HMenusource,MNCHGBRKC, flag)
	SetStateMenu(HMenusource,MNBRKENB,  flag)
	SetStateMenu(HMenusource,IDBUTCURSR,flag)
	SetStateMenu(HMenusource,IDBUTSTEP, flag)
	SetStateMenu(HMenusource,IDBUTSTEPP,flag)
	SetStateMenu(HMenusource,IDBUTSTEPM,flag)
	SetStateMenu(HMenusource,IDBUTSTEPB,flag)
	SetStateMenu(HMenusource,IDBUTSTEPT,flag)
	SetStateMenu(HMenusource,IDBUTRUN,  flag)
	SetStateMenu(HMenusource,IDBUTEXEMOD,  flag)
	SetStateMenu(HMenusource,IDBUTFASTRUN, flag)
	SetStateMenu(HMenusource,IDBUTKILL, flag)
	SetStateMenu(HMenusource,IDBUTSTOP, flag)
	SetStateMenu(HMenusource,IDBUTAUTO, flag)
	SetStateMenu(HMenusource,MNTHRDAUT, flag)
	SetStateMenu(HMenusource,MNSHWVAR,  flag)
	SetStateMenu(HMenusource,MNSETWVAR, flag)
	SetStateMenu(HMenusource,MNLINEADR, flag)
	SetStateMenu(HMenusource2,MNLINEADR,flag)
	SetStateMenu(HMenusource2,MNASMLINE,flag)
	SetStateMenu(HMenusource2,MNASMPRCL,flag)
	SetStateMenu(HMenusource2,MNASMREGS,flag)
	
	SetStateMenu(HMenuvar2,MNSETWTCH,   flag)
	SetStateMenu(HMenuvar2,MNSETWTTR,   flag)
	SetStateMenu(HMenuvar,MNSELIDX,     flag)
	SetStateMenu(HMenuvar,MNTRCKARR,    flag)
	SetStateMenu(HMenuvar,MNTRCKIDX0,   flag)
	SetStateMenu(HMenuvar,MNTRCKIDX1,   flag)
	SetStateMenu(HMenuvar,MNTRCKIDX2,   flag)
	SetStateMenu(HMenuvar,MNTRCKIDX3,   flag)
	SetStateMenu(HMenuvar,MNTRCKIDX4,   flag)
	SetStateMenu(HMenuvar,MNTRCKRST,    flag)
	
	SetStateMenu(HMenuvar,MNVARDMP,  flag)
	SetStateMenu(HMenuvar,MNVAREDT,  flag)
	SetStateMenu(HMenuvar,MNSHWEXP,  flag)
	SetStateMenu(HMenuvar,MNVARBRK,  flag)
	SetStateMenu(HMenuvar,MNSHSTRG,  flag)
	SetStateMenu(HMenuvar,MNSHCHAR,  flag)
	SetStateMenu(HMenuvar,MNCHGZSTR, flag)
	SetStateMenu(HMenuvar,MNLSTVARA, flag)
	SetStateMenu(HMenuvar,MNLSTVARS, flag)
	SetStateMenu(HMenuvar,MNCLBVARA, flag)
	SetStateMenu(HMenuvar,MNCLBVARS, flag)
	SetStateMenu(HMenuvar,MNPTDUMP , flag)
	SetStateMenu(HMenuvar,MNFNDVAR , flag)
	
	SetStateMenu(HMenuthd,MNTHRDCHG,flag)
	SetStateMenu(HMenuthd,MNTHRDKLL,flag)
	SetStateMenu(HMenuthd,MNEXCLINE,flag)
	SetStateMenu(HMenuthd,MNCREATHR,flag)
	SetStateMenu(HMenuthd,MNTHRDEXP,flag)
	SetStateMenu(HMenuthd,MNTHRDCOL,flag)
	SetStateMenu(HMenuthd,MNLOCPRC, flag)
	SetStateMenu(HMenuthd,MNSHWPROC,flag) 
	SetStateMenu(HMenuthd,MNTBCKTRK,flag)
	SetStateMenu(HMenuthd,MNTCHNING,flag)
	SetStateMenu(HMenuthd,MNSHPRSRC,flag)
	SetStateMenu(HMenuthd,MNPRCRADR,flag)
	SetStateMenu(HMenuthd,MNTHRDLST,flag)
	
	SetStateMenu(HMenutools,MNLSTDLL, flag)
	
	If wtchcpt<>0 AndAlso prun=true Then flag=0 Else flag=1
	SetStateMenu(HMenuwch,MNWCHVAR,flag)
	SetStateMenu(HMenuwch,MNWCHDMP,flag)
	SetStateMenu(HMenuwch,MNWCHSHW,flag)
	SetStateMenu(HMenuwch,MNWCHSTG,flag)
	SetStateMenu(HMenuwch,MNWCHDEL,flag)
	SetStateMenu(HMenuwch,MNWCHDALL,flag)
	SetStateMenu(HMenuwch,MNWCHEDT,flag)
	SetStateMenu(HMenuwch,MNWCHTTGL,flag)
	SetStateMenu(HMenuwch,MNWCHTTGA,flag)
	
	If procnb Then flag=0
	SetStateMenu(HMenuprc,MNRSTPRC,flag)
	SetStateMenu(HMenuprc,MNASMPRC,flag)
	SetStateMenu(HMenuprc,MNSETPRC,flag)
	SetStateMenu(HMenuprc,MNLOCPRC,flag)
	SetStateMenu(HMenuprc,MNSORTPRC,flag)
	
	SetStateMenu(HMenuvar,MNPBCKTRK,flag)
	SetStateMenu(HMenuvar,MNPCHNING,flag)
	SetStateMenu(HMenuvar,MNLOCPRC,flag)
	SetStateMenu(HMenuvar,MNCALLINE,flag)
	SetStateMenu(HMenuvar,MNASMPRC,flag)
	
	If brknb then flag=0
	SetStateMenu(HMenusource,MNMNGBRK,flag)

End sub
'=================================================
''contextual menus
'================================================
private sub menu_set()

''menu source
	HMenusource=CreatePopMenu()
	MenuItem(IDBUTCURSR,HMenusource,"Run to Cursor / C")
	MenuItem(IDBUTSTEP,HMenusource,"Next step / S")
	MenuItem(IDBUTSTEPP,HMenusource,"Step over procs / O")
	MenuItem(IDBUTSTEPM,HMenusource,"Step out current proc / E")
	MenuItem(IDBUTSTEPT,HMenusource,"Step top called proc / T")
	MenuItem(IDBUTSTEPB,HMenusource,"Step bottom current proc / B")
	MenuItem(IDBUTRUN,HMenusource,"Run / R")
	MenuItem(IDBUTFASTRUN,HMenusource,"Fast Run / F")
	MenuItem(IDBUTSTOP,HMenusource,"Halt running debuggee / H")
	MenuItem(IDBUTKILL,HMenusource,"Kill debuggee / K")
	MenuItem(IDBUTAUTO,HMenusource,"Step auto / A")
	MenuItem(MNTHRDAUT,HMenusource,"Step auto multi threads / D")
	MenuItem(IDBUTEXEMOD,HMenusource,"Modify execution / M")
	MenuBar(HMenusource)
	MenuItem(MNSETBRK,HMenusource,"Set/Clear Breakpoint / F3")
	MenuItem(MNSETBRKC,HMenusource,"Set/clear Breakpoint with counter Ctrl+F3")
	MenuItem(MNRSTBRKC,HMenusource,"ReSet initial value counter of a Breakpoint")
	MenuItem(MNCHGBRKC,HMenusource,"Change value counter of a Breakpoint")
	MenuItem(MNSETBRT,HMenusource,"Set/Clear tempo Breakpoint / Shift+F3")
	MenuItem(MNBRKENB,HMenusource,"Enable/disable Breakpoint")
	MenuItem(MNMNGBRK,HMenusource, "Manage Breakpoints")
	MenuBar(HMenusource)
	MenuItem(MNSHWVAR,HMenusource,"Show var"+Chr(9)+"Ctrl+Left click")
	MenuItem(MNSETWVAR,HMenusource,"Set watched var"+Chr(9)+"Alt+Left click")
	MenuBar(HMenusource)
	MenuItem(MNFNDTXT,HMenusource,"Find text / Ctrl+F")
	MenuItem(MNGOTO,HMenusource,"Goto Line")
	var HMenusource2=OpenSubMenu(HMenusource,"ASM data")
	MenuItem(MNLINEADR,HMenusource2,"Line Address")
	MenuItem(MNASMLINE,HMenusource2, "Asm code of line")
	MenuItem(MNASMPRCL,HMenusource2,"Asm code of proc (from line)")
	MenuItem(MNASMREGS,HMenusource2,"Show registers for current thread)")
	MenuItem(MNACCLINE,HMenusource,"Mark no executable lines")
	MenuItem(MNFCSSRC,HMenusource,"Focus lines / L")

''menu proc/var
	HMenuvar=CreatePopMenu()
	HMenuvar2=OpenSubMenu(HMenuvar,"Set watched")
	MenuItem(MNSETWTCH,HMenuvar2,"Set watched")
	MenuItem(MNSETWTTR,HMenuvar2,"Set watched+trace")
	MenuItem(MNVARBRK,HMenuvar, "Break on var value")
	MenuBar(HMenuvar)
	MenuItem(MNSELIDX,HMenuvar, "Select index")
	HMenuvar5=OpenSubMenu(HMenuvar,"Array tracking")
	MenuItem(MNTRCKARR,HMenuvar5, "Assign vars to an array")  
	MenuItem(MNTRCKIDX0,HMenuvar5, "Set Variable for index 1") 
	MenuItem(MNTRCKIDX1,HMenuvar5, "Set Variable for index 2")
	MenuItem(MNTRCKIDX2,HMenuvar5, "Set Variable for index 3")
	MenuItem(MNTRCKIDX3,HMenuvar5, "Set Variable for index 4")
	MenuItem(MNTRCKIDX4,HMenuvar5, "Set Variable for index 5") 
	MenuItem(MNTRCKRST,HMenuvar5,  "Reset all choices") 
	MenuItem(MNVARDMP,HMenuvar, "Variable Dump")
	MenuItem(MNPTDUMP,HMenuvar, "Pointed data Dump")
	MenuItem(MNVAREDT,HMenuvar, "Edit var value")
	MenuItem(MNSHWEXP,HMenuvar, "Show/expand variable")
	MenuItem(MNSHSTRG,HMenuvar, "Show z/w/string")
	MenuItem(MNSHCHAR,HMenuvar, "Show char at position")
	MenuItem(MNCHGZSTR,HMenuvar,"Change (u)byte<>zstring type")
	MenuBar(HMenuvar)
	MenuItem(MNLOCPRC,HMenuvar, "Locate proc (source)")
	MenuItem(MNCALLINE,HMenuvar,"Locate calling line")
	MenuItem(MNPBCKTRK,HMenuvar,"Proc call Backtracking")
	MenuItem(MNPCHNING,HMenuvar,"Proc call Chaining")
	MenuItem(MNASMPRC,HMenuvar, "Asm code of proc")
	MenuItem(MNFNDVAR,HMenuvar, "Find any text")
	HMenuvar3=OpenSubMenu(HMenuvar,"List to log")
	MenuItem(MNLSTVARA,HMenuvar3, "List all proc/var")  
	MenuItem(MNLSTVARS,HMenuvar3, "List selected proc/var")
	HMenuvar4=OpenSubMenu(HMenuvar,"Copy to clipboard")
	MenuItem(MNCLBVARA,HMenuvar4, "Copy all proc/var")  
	MenuItem(MNCLBVARS,HMenuvar4, "Copy selected proc/var")

''menu watched
	HMenuwch=CreatePopMenu()
	MenuItem(MNWCHVAR,HMenuwch, "Show in var window")
	MenuItem(MNWCHEDT,HMenuwch, "Edit value")
	MenuItem(MNWCHDMP,HMenuwch, "Memory Dump")
	MenuItem(MNWCHSHW,HMenuwch, "Show/expand variable")
	MenuItem(MNWCHSTG,HMenuwch, "Show z/w/string")
	MenuBar(HMenuwch)
	MenuItem(MNWCHTTGL,HMenuwch,"Toggle Tracing")
	MenuItem(MNWCHTTGA,HMenuwch,"Cancel all Tracing")
	MenuBar(HMenuwch)
	MenuItem(MNWCHDEL,HMenuwch,"Delete")
	MenuItem(MNWCHDALL,HMenuwch,"Delete all")

''menu proc
	HMenuprc=CreatePopMenu()
	MenuItem(MNLOCPRC,HMenuprc, "Locate proc (source)")
	MenuItem(MNASMPRC,HMenuprc, "Asm code of proc")
	MenuItem(MNSORTPRC,HMenuprc,"Toggle sort by module or by proc")
	MenuBar(HMenuprc)
	MenuItem(MNRSTPRC,HMenuprc, "All procs followed")
	MenuItem(MNSETPRC,HMenuprc, "No proc followed")

''menu thread
	HMenuthd=CreatePopMenu()
	MenuItem(MNTHRDCHG,HMenuthd, "Select next thread to be executed")
	MenuItem(MNEXCLINE,HMenuthd, "Show next executed line (source)")
	MenuItem(MNCREATHR,HMenuthd, "Show line creating thread (source)")
	MenuItem(MNLOCPRC,HMenuthd,  "Show first proc of thread (source)")
	MenuItem(MNSHWPROC,HMenuthd, "Show proc (proc/var)")
	MenuItem(MNSHPRSRC,HMenuthd, "Show proc (source)")
	MenuItem(MNTBCKTRK,HMenuthd, "Proc call Backtracking")
	MenuItem(MNTCHNING,HMenuthd, "Proc call Chaining")
	MenuItem(MNPRCRADR,HMenuthd, "Proc Addresses")
	MenuItem(MNTHRDKLL,HMenuthd, "Kill thread")
	MenuItem(MNTHRDEXP,HMenuthd, "Expand one thread")
	MenuItem(MNTHRDCOL,HMenuthd, "Collapse all threads")
	MenuItem(MNTHRDLST,HMenuthd, "List all threads")

''menu tools
	HMenutools=CreatePopMenu()
	MenuItem(MNABOUT,HMenutools,   "About")
	MenuItem(MNCMPINF,HMenutools,  "Compile info")
	MenuItem(MNDBGHELP,HMenutools, "Help / F1")
	MenuItem(MNCLIPBRD,HMenutools, "Copy notes to clipboard")
	MenuItem(MNSHWLOG,HMenutools,  "Show log file")
	MenuItem(MNHIDLOG,HMenutools,  "Hide log file")
	MenuItem(MNDELLOG,HMenutools,  "Delete log file")
	MenuItem(MNSHENUM,HMenutools,  "List enum")
	MenuItem(MNINFOS,HMenutools,  "Process list")
	MenuItem(MNLSTDLL,HMenutools, "Dlls list")
	MenuItem(MNLSTSHC,HMenutools, "Shortcut keys list")
	MenuItem(MNWINMSG,HMenutools, "Translate Win Message")
	MenuItem(MNSHWBDH,HMenutools, "Bin/Dec/Hex")
	MenuItem(MNFRTIMER,HMenutools,"Show fast run timer")
	MenuItem(MNJITDBG,HMenutools, "Set JIT Debugger")
End Sub

'===========================================
'' Initialise all the GUI windows/gadgets
'===========================================
private sub gui_init

	''main windows
	hmain=OpenWindow("New FBDEBUGGER with window9 :-)",10,10,1100,500)
	
	''scintilla gadget
	create_sci(GSCINTILLA,0,65,400,WindowClientHeight(hmain)-90,)

	''source panel
	'Var font=LoadFont("Arial",40)

	PanelGadget(GSRCTAB,2,42,400,20)
    SetGadgetFont(GSRCTAB,CINT(LoadFont("Courier New",11)))	
		
	''file combo/buuton ''idee mettre dans le menu affichage de la liste (du combo)
	ComboBoxGadget(GFILELIST,790,0,200,80)
	ButtonGadget(GFILESEL,992,2,30,20,"Go")
	
	''status bar
	StatusBarGadget(1,"")
	SetStatusBarField(1,0,100,"No program")
	SetStatusBarField(1,1,200,"Thread number")
	SetStatusBarField(1,2,300,"UID number Linux")
	SetStatusBarField(1,3,400,"Current source")
	SetStatusBarField(1,4,500,"Current proc")
	setstatusbarfield(1,5,-1,"Fast time ?")
	
	''current line
	textGadget(GCURRENTLINE,2,28,400,20,"Next exec line : ",SS_NOTIFY )
	GadgetToolTip(GCURRENTLINE,"next executed line"+chr(13)+"Click on me to reach the line",GCURLINETTIP)

	''buttons
	load_button(IDBUTSTEP,@"step.bmp",8,@"[S]tep/line by line",,0)
	load_button(IDBUTCURSR,@"runto.bmp",40,@"Run to [C]ursor",,0)
	load_button(IDBUTSTEPP,@"step_over.bmp",72,@"Step [O]ver sub/func",)
	load_button(IDBUTSTEPT,@"step_start.bmp",104,@"[T]op next called sub/func",)
	load_button(IDBUTSTEPB,@"step_end.bmp",136,@"[B}ottom current sub/func",)
	load_button(IDBUTSTEPM,@"step_out.bmp",168,@"[E]xit current sub/func",)
	load_button(IDBUTAUTO,@"auto.bmp",200,@"Step [A]utomatically, stopped by [H]alt",)
	load_button(IDBUTRUN,@"run.bmp",232,@"[R]un, stopped by [H]alt",)
	load_button(IDBUTSTOP,@"stop.bmp",264,@"[H]alt running pgm",)
	load_button(IDBUTFASTRUN,@"fastrun.bmp",328,@"[F]AST Run to cursor",)
	load_button(IDBUTEXEMOD,@"exemod.bmp",360,@"[M]odify execution, continue with line under cursor",)
	load_button(IDBUTFREE,@"free.bmp",392,@"Release debuged prgm",)
	load_button(IDBUTKILL,@"kill.bmp",424,@"CAUTION [K]ill process",)
	load_button(IDBUTRERUN,@"restart.bmp",466,@"Restart debugging (exe)",TTRERUN,0)
	load_button(IDBUTLASTEXE,@"multiexe.bmp",498,@"Last 10 exe(s)",,0)
	load_button(IDBUTATTCH,@"attachexe.bmp",530,@"Attach running program",,0)
	load_button(IDBUTFILE,@"files.bmp",562,@"Select EXE/BAS",,0)
	load_button(IDBUTTOOL,@"tools.bmp",628,"Some usefull....Tools",,0)
	load_button(IDBUTUPDATE,@"update.bmp",660,@"Update On /Update off : variables, dump",,0)
	load_button(IDBUTENLRSRC,@"source.bmp",692,@"Enlarge/reduce source",)
	load_button(IDBUTENLRVAR,@"varproc.bmp",724,@"Enlarge/reduce proc/var",)
	load_button(IDBUTENLRMEM,@"memory.bmp",756,@ "Enlarge/reduce dump memory",)
	
	''bmb(25)=Loadbitmap(fb_hinstance,Cast(LPSTR,MAKEINTRESOURCE(1025))) 'if toogle noupdate
	''no sure to implement this one	 
	''load_button(IDBUTMINI,@"minicmd.bmp",296,@ "Mini window",)
	
	''icon on title bar
	''-----> ONLY WINDOWS
	'Var icon=LoadIcon(null,@"D:"+slash+"telechargements"+slash+"win9"+slash+"tmp"+slash+"fbdebugger.ico")
	'print icon,getlasterror()
	'    'SendMessage(hwnd,WM_SETICON,ICON_BIG,Cast(Lparam,icon))
	'    sendmessage(hwnd,WM_SETICON,ICON_SMALL,Cast(Lparam,LoadIcon(GetModuleHandle(0),@"."+slash+"fbdebugger.ico")))
	'Var icon=LoadIcon(GetModuleHandle(0),MAKEINTRESOURCE(100))
	'  SendMessage(hwnd,WM_SETICON,ICON_BIG,Cast(Lparam,icon))
	'  SendMessage(hwnd,WM_SETICON,ICON_SMALL,Cast(Lparam,icon))
	'D:"+slash+"telechargements"+slash+"win9"+slash+"tmp"+slash+"
	
	#ifdef __fb_win32__
		var icon=loadimage(0,@"fbdebugger.ico",IMAGE_ICON,0,0,LR_LOADFROMFILE or LR_DEFAULTSIZE)
		sendmessage(hmain,WM_SETICON,ICON_BIG,Cast(Lparam,icon))
	#endif
	''right panels
	PanelGadget(GRIGHTTABS,500,30,499,300)
	SetGadgetFont(GRIGHTTABS,CINT(LoadFont("Courier New",11)))
	''treeview proc/var
	var htabvar=AddPanelGadgetItem(GRIGHTTABS,0,"Proc/var",,1)
	'var hbmp = load_Icon("1.ico")
	'var hbmp1 = load_Icon("2.ico")	
	htviewvar=treeviewgadget(GTVIEWVAR,0,0,499,299,KTRRESTYLE)
	''filling treeview for example
	var Pos_=AddTreeViewItem(GTVIEWVAR,"Myvar udt ",cast (hicon, 0),cast (hicon, 0),0,0)
	AddTreeViewItem(GTVIEWVAR,"first field",cast (hicon, 0),0,1,Pos_)
	Pos_=AddTreeViewItem(GTVIEWVAR,"my second var",cast (hicon, 0),0,0)
	AddTreeViewItem(GTVIEWVAR,"first field",cast (hicon, 0),0,0,Pos_)
	
	HideWindow(htabvar,0)
	''treeview procs
	var htabprc=AddPanelGadgetItem(GRIGHTTABS,1,"Procs",,1)
	htviewprc=treeviewgadget(GTVIEWPRC,0,0,499,299,KTRRESTYLE)
	AddTreeViewItem(GTVIEWPRC,"first proc",cast (hicon, 0),0,0)
	AddTreeViewItem(GTVIEWPRC,"second proc",cast (hicon, 0),0,0)
	AddTreeViewItem(GTVIEWPRC,"third proc",cast (hicon, 0),0,0)
	''treeview threads
	var htabthrd=AddPanelGadgetItem(GRIGHTTABS,2,"Threads")
	''treeview watched
	var htabwatch=AddPanelGadgetItem(GRIGHTTABS,3,"Watched")
	htviewwch=treeviewgadget(GTVIEWWCH,0,0,499,299,KTRRESTYLE)
	''dump memory
	var htabmem=AddPanelGadgetItem(GRIGHTTABS,4,"Memory",,1)
	hlviewdump=ListViewGadget(GDUMPMEM,0,0,499,299,LVS_EX_GRIDLINES)
	AddListViewColumn(GDUMPMEM, "Address",0,0,100)
	'for icol as integer =1 to 4
		'AddListViewColumn(GDUMPMEM, "+0"+str((icol-1)*4),icol,icol,40)
	'next
	AddListViewColumn(GDUMPMEM, "Ascii value",5,5,100)
	
	create_settingsbx()
	create_inputbx()
	menu_set()
end sub

