''gui for fbdebuuger_new
''dbg_gui.bas

/' TIPS ===============================================
''todo Sous Windows, il s'agit de InvalidateRect, sous Linux gtk_widget_queue_draw_area ou gdk_window_invalidate_rect ou gdk_window_invalidate_region
'hwnd=OpenWindow("1",30,30,200,200 , WS_OVERLAPPEDWINDOW) ' style without ws_visible
'UpdateInfoXServer ' only for Linux
'CenterWindow(hwnd)
'hidewindow(hwnd , 0)

Var hwnd = OpenWindow ("", 10,10,100,100)
gdk_window_set_functions (gtk_widget_get_window (hwnd), GDK_FUNC_CLOSE or GDK_FUNC_MOVE)
'======================================================= '/

'==================================================================================
'' checks the current value in a spingadget (replace the control normally done)
'==================================================================================
private sub updown_check(gadget as integer,valuemin as integer,valuemax as INTEGER)
	var valuecurrent=getgadgetstate(gadget)
	if valuecurrent>valuemax then
		setgadgetstate(gadget,valuemax)
	elseif valuecurrent<valuemin then
		setgadgetstate(gadget,valuemin)
	EndIf
End Sub
'=======================================================
'' prepares the window for filling data in index_fill
'=======================================================
private sub index_sel()
	dim as integer typ,typ2,size,sizeline,adr,nbdim,temp,curidx(KMAXDIM),vlbound(KMAXDIM),vubound(KMAXDIM),delta2,indexvar
	dim as STRING strg,txt
	typ2=0
	indexvar=var_find() 'search index variable under cursor
	if indexvar=0 then exit sub

	while 2 ''infinite loop
		if indexvar>0 then ''var type
			indexdata.typvar=true ''is a var type
			If vrb(vrr(indexvar).vr).pt Then 'pointer
			   size=SizeOf(Integer)
			   typ=1 'integer
			Else
			   size=udt(vrb(vrr(indexvar).vr).typ).lg
			   typ=vrb(vrr(indexvar).vr).typ
			EndIf
			If Cast(Integer,vrb(vrr(indexvar).vr).arr)=-1 Then 'dynamic array
				adr=vrr(indexvar).ini+SizeOf(Integer)
				readprocessmemory(dbghand,Cast(LPCVOID,adr),@adr,SizeOf(Integer),0)
				if adr Then
					adr=vrr(indexvar).ini+4*SizeOf(Integer) 'nb dim
					readProcessMemory(dbghand,Cast(LPCVOID,adr),@nbdim,SizeOf(Integer),0)
					#ifdef KNEWARRAYFIELD
						adr+=SizeOf(Integer) ''skip flag field
					#endif
					For k As Integer =0 To nbdim-1
						adr+=SizeOf(Integer)*2
						ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@vlbound(k),SizeOf(Integer),0)
						adr+=SizeOf(Integer)
						readProcessMemory(dbghand,Cast(LPCVOID,adr),@vubound(k),SizeOf(Integer),0)
					Next
					Exit While
				else
					messbox("Index selection","Dynamic array not yet defined. Try later")
					exit sub
				End If
			ElseIf vrb(vrr(indexvar).vr).arr Then
				nbdim=vrb(vrr(indexvar).vr).arr->dm
				For k As Integer =0 To nbdim-1
					vlbound(k)=vrb(vrr(indexvar).vr).arr->nlu(k).lb
					vubound(k)=vrb(vrr(indexvar).vr).arr->nlu(k).ub
				Next
				Exit While
			Else
				messbox("Index selection","Not an array, Select an other variable")
				exit sub
			End If

		'====================================
		else ''cudt type
			indexdata.typvar=false ''is not a var type
			indexvar=Abs(indexvar)
			with cudt(Abs(vrr(indexvar).vr))
				If .pt Then 'pointer
					size=SizeOf(Integer)
					typ=1 'integer
				Else
					size=udt(.typ).lg
					typ=.typ
				EndIf
				If .arr Then
					If Cast(Integer,.arr)=-1 Then
						temp=getparentitemtreeview(GTVIEWVAR,vrr(indexvar).tv) ''finding parent
						For k As Integer=1 To vrrnb
							If vrr(k).tv=temp Then
								temp=k
								Exit For
							end if
						Next
						adr=vrr(temp).ad+vrr(indexvar).ini+SizeOf(Integer)
						ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@adr,4,0) ''ptr instead data, added for dyn array in udt
						If adr Then
							adr=vrr(indexvar).ini+4*SizeOf(Integer)*4 'nb dim
							ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@nbdim,4,0)
							#ifdef KNEWARRAYFIELD
								adr+=SizeOf(Integer) ''skip flag field
							#endif
							For k As Integer =0 To nbdim-1
								adr+=SizeOf(Integer)*2
								ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@vlbound(k),4,0)
								adr+=SizeOf(Integer)
								readProcessMemory(dbghand,Cast(LPCVOID,adr),@vubound(k),4,0)
							Next

							Exit While
						Else
							messbox("Index selection","Dynamic array not yet defined. Try later")
							exit sub
						End If
					Else
						nbdim=.arr->dm
						For k As Integer =0 To nbdim-1
							vlbound(k)=.arr->nlu(k).lb
							vubound(k)=.arr->nlu(k).ub
						Next
					EndIf
					Exit while
				Else
					If typ2=0 Then
						typ2=.typ
						delta2=vrr(indexvar).ad
						strg=" -->"+GetTextTreeView(GTVIEWVAR,vrr(indexvar).tv)
					EndIf
					temp=getparentitemtreeview(GTVIEWVAR,vrr(indexvar).tv) ''finding parent
					For k As Long =1 To vrrnb
						If vrr(k).tv=temp Then
							If vrr(k).vr<0 Then
								indexvar=-k
							Else
								indexvar=k
							EndIf
							Exit For
						EndIf
					Next
				End If
			end with
		EndIf
	Wend

	''shows the gadgets
	For k As Integer =0 To nbdim-1
		SetGadgetText(GIDXMIN1+k,Str(vlbound(k)))
		SetGadgetText(GIDXMAX1+k,Str(vubound(k)))
		SetGadgetState(GIDXUP1+k,vrr(indexvar).ix(k))
		hidegadget(GIDXMIN1+k,KSHOW)
		hidegadget(GIDXMAX1+k,KSHOW)
		hidegadget(GIDXUP1+k,KSHOW)
	Next
	''hide the gadgets for the dimensions not used
	For k As Integer =nbdim to KMAXDIM-1
		hidegadget(GIDXMIN1+k,KHIDE)
		hidegadget(GIDXMAX1+k,KHIDE)
		hidegadget(GIDXUP1+k,KHIDE)
	next

	txt=GetTextTreeView(GTVIEWVAR,vrr(indexvar).tv)
	txt+=strg
	SetGadgetText(GIDXVAR,txt)

	'case where selection done not on an array but a parent is an array
	If typ2<>0 Then
		typ=typ2
		adr=delta2
		delta2-=vrr(indexvar).ad
	Else
		adr=vrr(indexvar).ad
	EndIf

	''keep data for next actions
	indexdata.indexvar=indexvar
	indexdata.nbdim=nbdim
	for idx as integer =0 to 4
		indexdata.curidx(idx)=vrr(indexvar).ix(idx)
		indexdata.vlbound(idx)=vlbound(idx)
		indexdata.vubound(idx)=vubound(idx)
	next
	indexdata.adr=adr
	indexdata.typ=typ
	indexdata.typ2=typ2
	indexdata.delta2=delta2


	If typ>0 AndAlso typ<TYPESTD andalso nbdim<=2 Then
		dim lvCol   As LVCOLUMN
		hidegadget(GIDXTABLE,KSHOW)
		hidegadget(GIDXAUTO,KSHOW)
		hidegadget(GIDXUPD,KSHOW)
		hidegadget(GIDXROWP,KSHOW)
		hidegadget(GIDXROWL,KSHOW)
		hidegadget(GIDXPAGEP,KSHOW)
		hidegadget(GIDXPAGEL,KSHOW)
		hidegadget(GIDXWIDTH,KSHOW)

		indexdata.size=size
		''displays the array if one or 2 dimensions
		If nbdim=2 Then
			indexdata.sizeline=size*(vubound(1)-vlbound(1)+1) 'nb elements last dim
			index_update()

			hidegadget(GIDXCOLP,KSHOW) ''moving by one column or by block (several columns)
			hidegadget(GIDXCOLL,KSHOW)
			hidegadget(GIDXBLKP,KSHOW)
			hidegadget(GIDXBLKL,KSHOW)
			hidegadget(GIDXWIDTH,KSHOW)

		Elseif nbdim=1 then
			''only one dim
			indexdata.sizeline=size
			index_update()
			hidegadget(GIDXCOLL,KHIDE)
			hidegadget(GIDXCOLP,KHIDE)
			hidegadget(GIDXBLKP,KHIDE)
			hidegadget(GIDXBLKL,KHIDE)
			hidegadget(GIDXWIDTH,KHIDE)
		End If
	else
		hidegadget(GIDXTABLE,KHIDE)
		hidegadget(GIDXAUTO,KHIDE)
		hidegadget(GIDXUPD,KHIDE)
		hidegadget(GIDXROWP,KHIDE)
		hidegadget(GIDXROWL,KHIDE)
		hidegadget(GIDXPAGEP,KHIDE)
		hidegadget(GIDXPAGEL,KHIDE)
		hidegadget(GIDXWIDTH,KHIDE)
		hidegadget(GIDXCOLL,KHIDE)
		hidegadget(GIDXCOLP,KHIDE)
		hidegadget(GIDXBLKP,KHIDE)
		hidegadget(GIDXBLKL,KHIDE)
	EndIf

	hidewindow(hindexbx,KSHOW)

end sub

'==============================================================
'' changes the text of a field in statusbar and tooltip
'==============================================================
private sub statusbar_text(fieldn as long, text as string)
	dim as integer fieldpos
	select case fieldn
		Case KSTBSTS
			fieldpos=200
		case KSTBTHD
			fieldpos=300
		Case KSTBUID
			fieldpos=400
		Case KSTBSRC
			fieldpos=600
		Case KSTBPRC
			fieldpos=800
		case KSTBFRT
			fieldpos=-1
	End Select
	SetStatusBarField(GSTATUSBAR,fieldn,fieldpos,text)
	ToolTipStatusBar(GSTATUSBAR,fieldn,text)
End Sub
'=======================================================
'=======================================================
private sub dump_set()
    Dim tmp As String
	dim as integer lg,delta,combo
	For icol as integer=1 to dumpnbcol
		DeleteListViewColumn(GDUMPMEM,1) ''delete each time column 1 keep address/ascii
	Next
	if dumptyp>=100 then ''change number of bytes
		combo=dumptyp-100
		select case combo
			Case 0
				dumpnbcol=16 :lg=40
			Case 1
				dumpnbcol=8 :lg=60
			Case 2
				dumpnbcol=4 :lg=120
			Case 3
				dumpnbcol=2 :lg=160
		End Select
	else
		Select Case dumptyp
			Case 2,3,16  'byte/ubyte/boolean    dec/hex
				dumpnbcol=16 :lg=40:combo=0
			Case 5,6  'short/ushort
				dumpnbcol=8 :lg=60:combo=1
			Case 1,8,7  'integer/uinteger
				dumpnbcol=4 :lg=90:combo=2
			Case 9,10  'longinteger/ulonginteger
				dumpnbcol=2 :lg=160:combo=3
			Case 11 'single
				dumpnbcol=4 :lg=120:combo=2
				dumpbase=0
				SetGadgettext(GDUMPDECHEX,">Hex")
			Case 12 'double
				dumpnbcol=2 :lg=180:combo=3
				dumpbase=0
				SetGadgettext(GDUMPDECHEX,">Hex")
		End Select
	EndIf
	delta=16/dumpnbcol
	For icol as integer =1 To dumpnbcol 'nb columns except address and ascii
		tmp="+"+Right("0"+Str(delta*(icol-1)),2)
		AddListViewColumn(GDUMPMEM,tmp,icol,icol,lg)
	Next
	SetItemListBox(GDUMPSIZE,combo)
	SetGadgetText(GDUMPADR,str(dumpadr))
	setgadgettext(GDUMPTYPE,"Current type="+udt(dumptyp).nm)
End Sub
'================================================================================
'' Changes size gadgets when main window is resized
'===================================================
private sub size_changed()
	'messbox("resizing",str(SizeX)+" "+str(SizeY))
	if sizey>250 then
		#ifdef __fb_win32__
			ResizeWindow(hscint,0,80,,WindowClientHeight(hmain)-100)
		#else
			messbox("Function not coded under linux","so size remains inchanged")
		#endif
	end if
end sub
'================================
''Loading of buttons from files
'================================
private sub load_button(id as integer,button_name as zstring ptr,xcoord as integer,ycoord as integer=0,tooltiptext as zstring ptr=0,idtooltip as integer=-1,disab as long=1)
	Var himage=Load_image(exepath+slash+"buttons"+slash+*button_name)
	ButtonImageGadget(id,xcoord,ycoord,30,26,himage,  BS_BITMAP)
	if tooltiptext then
		if idtooltip<>-1 then
			GadgetToolTip(id,*tooltiptext,idtooltip)
		else
			GadgetToolTip(id,*tooltiptext)
		endif
	end if
	disablegadget(id,disab)
end sub
'=================================
''Loading of buttons from memory
'=================================
private sub load_button2(id as INTEGER,xcoord as INTEGEr,ycoord as INTEGER=0,himage as any ptr,tooltiptext as zstring ptr=0,idtooltip as integer=-1,disab as long=1)
	ButtonImageGadget(id,xcoord,ycoord,30,26,Catch_Image(himage),  BS_BITMAP)
	if tooltiptext then
		if idtooltip<>-1 then
			GadgetToolTip(id,*tooltiptext,idtooltip)
		else
			GadgetToolTip(id,*tooltiptext)
		endif
	end if
	disablegadget(id,disab)
End Sub
'============================================
''changes the displayed source
'============================================
private sub source_change(numb as integer)
	static as integer numbold=-1
	dim as any ptr ptrdoc
	if numb=-1 then
		numbold=-1 ''reinit
		exit sub
	EndIf
	if numb=numbold then exit sub
	numbold=numb
	ptrdoc=cast(any ptr,Send_sci(SCI_GETDOCPOINTER,0,0))
	Send_sci(SCI_ADDREFDOCUMENT,0,ptrdoc)
	Send_sci(SCI_SETDOCPOINTER,0,sourceptr(numb))
	srcdisplayed=numb
	PanelGadgetSetCursel(GSRCTAB,numb)
	SetItemComboBox(GFILELIST,srcdisplayed)
end sub
'=======================================================================================
'' return line where is the cursor  scintilla first line=0 --> rline=1 so 1 is added
'=======================================================================================
private function line_cursor() as integer
	return Send_sci(SCI_LINEFROMPOSITION,Send_sci(SCI_GETCURRENTPOS,0,0),0)+1
end function
'===================================================
'' changes the color/style of line in displayed src
'===================================================
private sub line_color(byval pline as integer,byval style as ulong)
	var begpos=Send_sci(SCI_POSITIONFROMLINE,pline-1,0)
	var endpos=Send_sci(SCI_GETLINEENDPOSITION,pline-1,0)

	''old way
	'begin styling at pos
	'send_sci(SCI_StartStyling, begpos, 0)
	'style next chars with style #x
	'Send_sci(SCI_SetStyling, endpos-begpos,style)

	''new way
	if style=0 then
		send_sci(SCI_INDICATORCLEARRANGE,begpos,endpos-begpos)
	else
		send_sci(SCI_INDICATORFILLRANGE,begpos,endpos-begpos)
	EndIf


end sub
'==========================================================
'' displays line
'==========================================================
private sub line_display(pline as integer,highlight as integer=0)
	send_sci(SCI_SETFIRSTVISIBLELINE, pline-1,0)
	if pline-send_sci(SCI_GETFIRSTVISIBLELINE,0,0)+5>send_sci(SCI_LINESONSCREEN,0,0) then
		send_sci(SCI_LINESCROLL,0,+5)
	else
		send_sci(SCI_LINESCROLL,0,-5)
	end if
	if highlight=1 then
		send_sci( SCI_SETSEL, send_sci( SCI_POSITIONFROMLINE, pLine-1 , 0 ), send_sci( SCI_GETLINEENDPOSITION, pLine-1 , 0 ) )
	EndIf
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
private function line_text(pline as integer)as string
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
	linecur_display()
	line_color(linecur,KSTYLECUR)

	'' display in current line gadget removing all left spaces/tabs
	setgadgettext(GCURRENTLINE,"Current line : "+left(trim(line_text(linecur-1),any " "+chr(9)),50))
end sub
'===================================================
'' set/unset breakpoint markers
'===================================================
sub brk_marker(brkidx as integer)
	dim as integer src,lline=brkol(brkidx).nline-1,typ

	if brkol(brkidx).typ>50 then
		typ=5 ''disabled --> grey marker
	else
		if brkidx=0 then
			if brkol(brkidx).typ<>0 then
				typ=7 ''red circle
				'messbox("red circle on line=",str(lline+1))
			end if
		else
			typ=brkol(brkidx).typ  ''permanent cond tempo --> marker 1 or 2 or 3
		end if
	End If

	src=srcdisplayed
	source_change(brkol(brkidx).isrc)
	if typ then
		send_sci(SCI_MARKERDELETE, lline, -1)
		send_sci(SCI_MARKERADD, lline, typ)
	else
		send_sci(SCI_MARKERDELETE, lline, -1)
	end if

	source_change(src)
end sub
'========================================================
'' displays the breakpoint data
'========================================================
private sub brk_manage()
	dim as string text
	dim as integer srcprev=srcdisplayed,typ

	For ibrk as integer =1 To brknb
		source_change(brkol(ibrk).isrc)
		text=line_text(brkol(ibrk).nline-1)

		text=" "+source_name(source(brkol(ibrk).isrc))+" ["+Str(brkol(ibrk).nline)+"] cntr="+Str(brkol(ibrk).counter)+" >> "+Left(Trim(text,Any Chr(9)+" "),65)
		SetGadgetText(GBRKLINE01+ibrk-1,text)
		hidegadget(GBRKLINE01+ibrk-1,KSHOW)

		hidegadget(GBRKDEL01+ibrk-1,KSHOW)

		typ=brkol(ibrk).typ
		If typ>50 Then
			text="ENB"
			typ-=50
		Else
			text="DSB"
		EndIf
		SetGadgetText(GBRKDSB01+ibrk-1,text)
		hidegadget(GBRKDSB01+ibrk-1,KSHOW)
		select case typ
			Case 1
				SetImageGadget(GBRKIMG01+ibrk-1,catch_image(butBRKP))
			Case 2,3
				SetImageGadget(GBRKIMG01+ibrk-1,catch_image(butBRKC))
			Case 4
				SetImageGadget(GBRKIMG01+ibrk-1,catch_image(butBRKN))
			Case 6
				SetImageGadget(GBRKIMG01+ibrk-1,catch_image(butBRKT))
		End Select
		hidegadget(GBRKIMG01+ibrk-1,KSHOW)
	next
	''hides the last lines
	For ibrk as integer =brknb+1 to 10
		hidegadget(GBRKIMG01+ibrk-1,KHIDE)
		hidegadget(GBRKLINE01+ibrk-1,KHIDE)
		hidegadget(GBRKDSB01+ibrk-1,KHIDE)
		hidegadget(GBRKDEL01+ibrk-1,KHIDE)
	next
	hidewindow(hbrkbx,KSHOW)
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
'==============================================================================
'' creates/fixes size/centers a window
'==============================================================================
private function create_window(title as string,x as integer,y as integer,w as integer,h as integer) as hwnd
	var hwnd=OpenWindow(title,x,y,w,h, WS_OVERLAPPEDWINDOW)
	WindowBounds(hwnd,w,h,w,h)
	centerWindow(hwnd)
	return hwnd
End function
'==============================================================================
'' creates the window for editing variable/memory
'==============================================================================
private sub create_editbx()
	heditbx=create_window("Edit variable",10,10,700,145)

	textgadget(GEDTVAR,15,10,445,30,"Fb_myvar <Byval param / **Zstring>=37415896")
	stringgadget(GEDTVALUE,450,10,185,30,"3741589637415896")
	buttongadget(GEDTOK,420,70,75,30,"Apply")
	buttongadget(GEDTCANCEL,500,70,75,30,"Cancel")

	''if pointer
	textgadget(GEDTPTD,15,35,85,30,"458785")
	textgadget(GEDTPTDVAL,105,35,200,30,"String pointed")
	buttongadget(GEDTPTDEDT,150,70,90,30,"Edit pointed")
end sub
'==============================================================================
'' creates the window for showing z/w/string
'========================================================
'' creates the window for managing the array indexes
'========================================================
private sub create_indexbx()
	dim as integer ypos
	hindexbx=create_window("Array management",10,10,800,560)

	textgadget(GIDXVAR,18,5,501,30,"Variable name + data")

	For idx as integer =0 To 4
		textgadget(GIDXMIN1+idx,18,40+32*idx,93,30,"1")
		textgadget(GIDXMAX1+idx,117,40+32*idx,93,30,"15")
		spingadget(GIDXUP1+idx,219,40+32*idx,102,25,100,-2147483648 ,21474836487)
	next

	buttongadget(GIDXAPPLY,327,40,66,30,"Apply")
	buttongadget(GIDXDEC,400,40,80,30,"Apply +1")
	buttongadget(GIDXINC,485,40,80,30,"Apply -1")

	CheckBoxGadget(GIDXAUTO,651,130,100,30,"Auto update")
	buttongadget(GIDXUPD,651,210,70,30,"Update")
	buttongadget(GIDXROWP,651,240,70,30,"Row +")
	buttongadget(GIDXROWL,651,270,70,30,"Row -")
	buttongadget(GIDXPAGEP,651,300,70,30,"Page +")
	buttongadget(GIDXPAGEL,651,330,70,30,"Page -")
	buttongadget(GIDXCOLP,651,360,70,30,"Column +")
	buttongadget(GIDXCOLL,651,390,70,30,"Column -")
	buttongadget(GIDXBLKP,651,420,70,30,"Block + >")
	buttongadget(GIDXBLKL,651,450,70,30,"< Block -")
	spingadget(GIDXWIDTH,648,480,80,30,80,-2147483648 ,21474836487)

	#Ifdef __FB_WIN32__
		Var Style=LVS_EX_GRIDLINES or LVS_EX_FULLROWSELECT
	#Else
		Var Style=LVS_EX_GRIDLINES
	#EndIf
	listviewgadget(GIDXTABLE,18,210,624,290,style)
end sub
'========================================================
'' creates the window for managing the breakpoint data
'========================================================
private sub create_brkbx()
	dim as integer ypos
	hbrkbx=create_window("Breakpoint management",10,10,850,420)

	For ibrk as integer =0 To 9
		ypos=5+32*ibrk
		buttongadget(GBRKDEL01+ibrk,30,ypos,40,30,"DEL")
		imagegadget(GBRKIMG01+ibrk,4,ypos+5,23,19,0)
		'hidegadget(GBRKDEL01+ibrk,1)
		buttongadget(GBRKDSB01+ibrk,75,ypos,40,30,"DSB")
		'hidegadget(GBRKDSB01+ibrk,1)
		textgadget(GBRKLINE01+ibrk,120,ypos+5,740,30,"Test lenght of line could be greater",SS_NOTIFY or SS_LEFT)
		'hidegadget(GBRKLINE01+ibrk,1)
	next

	buttongadget(GBRKCLOSE,10,ypos+40,80,30,"Close")
	buttongadget(GBRKDELALL,105,ypos+40,80,30,"Delete all")
	buttongadget(GBRKDISABLE,200,ypos+40,80,30,"Disable all")
	buttongadget(GBRKENABLE,285,ypos+40,80,30,"Enable all")

end sub
'==============================================================================
'' creates the window for managing the breakpoint on variable/memory change
'==============================================================================
private sub create_brkvbx()
	hbrkvbx=create_window("Breakpoint on value",10,10,600,150)
	centerWindow(hbrkvbx)

	textgadget(GBRKVAR1,6,6,390,30,"Stop if b<byte>=-88")
	textgadget(GBRKVAR2,6,35,390,30,"var2")

	stringgadget(GBRKVVALUE,6,35,120,30,"")
	buttongadget(GBRKVOK,190,75,55,30,"Apply")
	buttongadget(GBRKVDEL,250,75,55,30,"Delete")
	comboboxgadget(GBRKVCOND,402,3,54,150)
end sub
'==============================================================================
'' creates the window for managing the breakpoint on variable/memory change
'==============================================================================
private sub create_bpcondbx()
	hbpcondbx=create_window("Breakpoint cond var/const",10,10,700,650)
	centerWindow(hbpcondbx)

	treeviewgadget(GTVIEWBRC,0,0,500,600,KTRRESTYLE)
	textgadget(GBRCVAR1,505,5,390,30,"Variable")
	stringgadget(GBRCVALUE,505,40,120,30,"789")
	comboboxgadget(GBRCCOND,505,80,54,150)
	AddComboBoxItem(GBRKVCOND,"=",-1)
	AddComboBoxItem(GBRKVCOND,"<>",-1)
	AddComboBoxItem(GBRKVCOND,">",-1)
	AddComboBoxItem(GBRKVCOND,"<",-1)
	AddComboBoxItem(GBRKVCOND,">=",-1)
	AddComboBoxItem(GBRKVCOND,"<=",-1)
	SetItemComboBox(GBRKVCOND,0)
	buttongadget(GBRCOK,505,300,55,30,"Apply")
	buttongadget(GBRCDEL,570,300,55,30,"Cancel")
end sub
'==============================================================================
'' creates the window for Procedure call chain
'==============================================================================
private sub create_cchainbx()
	hcchainbx=create_window("Procedure call chain",10,10,900,650)

	#Ifdef __FB_WIN32__
		Var Style=LVS_EX_GRIDLINES or LVS_EX_FULLROWSELECT
	#Else
		Var Style=LVS_EX_GRIDLINES
	#EndIf
	hlviewcchain=ListViewGadget(GCCHAIN,0,0,850,600,style)
	AddListViewColumn(GCCHAIN, "Procedure           Thread=12345",0,0,200)
	AddListViewColumn(GCCHAIN, "Calling Line",1,1,200)
	AddListViewColumn(GCCHAIN, "L. Nbr.",2,2,50)
	AddListViewColumn(GCCHAIN, "File",3,3,200)
end sub
'========================================================
'' creates the window for show/expand  (shw/exp)
'========================================================
private sub create_shwexpbx()
	hshwexpbx=create_window("Shw/exp : variable",10,10,800,550)

	buttongadget(GSHWWCH,510,5,95,30,"Watched")
	buttongadget(GSHWDMP,510,40,95,30,"Dump")
	buttongadget(GSHWEDT,510,75,95,30,"Edit")
	buttongadget(GSHWSTR,510,110,95,30,"Show string")
	buttongadget(GSHWNEW,510,145,95,30,"New shw/exp")
	textgadget(GSHWCUR,510,215,200,30,"Index cur : 2")
	textgadget(GSHWMIN,510,250,200,30,"Index min : 1")
	textgadget(GSHWMAX,510,285,200,30,"Index max : 500")
	buttongadget(GSHWSET,510,325,95,30,"Set index")
	buttongadget(GSHWDEC,610,325,25,30,"-1")
	buttongadget(GSHWINC,640,325,25,30,"+1")
	buttongadget(GSHWUPD,510,360,95,30,"Update")
	htviewshw=treeviewgadget(GTVIEWSHW,0,0,500,500,KTRRESTYLE)

end sub
'=============================================================
'' creates the window for handling parameters of dump memory
'=============================================================
private sub create_dumpbx()
	hdumpbx=create_window("Handling dump parameters",10,10,370,450)

	'load_button(IDBUTENLRMEM,@"memory.bmp",300,5,@"Reduce the window",,0)

	ButtonGadget(GDUMPAPPLY,12,5,110,30,"Apply address : ")
	stringgadget(GDUMPADR,130,5,95,30,"12345678901")

	ButtonGadget(GDUMPEDIT,230,5,120,30,"Edit top/left cell")
	ButtonGadget(GDUMPCLIP,210,40,140,30,"Copy all to clipboard")

	textgadget(GDUMPTSIZE,12,40,105,30,"Size of column : ",0)
	ListBoxGadget(GDUMPSIZE,130,40,75,70)
	AddListBoxItem(GDUMPSIZE,"1 byte")
	AddListBoxItem(GDUMPSIZE,"2 bytes")
	AddListBoxItem(GDUMPSIZE,"4 bytes")
	AddListBoxItem(GDUMPSIZE,"8 bytes")

	ButtonGadget(GDUMPBASEADR,12,120,100,30,"Dec/Hex adr")
	ButtonGadget(GDUMPDECHEX,130,120,100,30,">Hex data")
	ButtonGadget(GDUMPSIGNED,248,120,100,30,"U/Signed")

	groupgadget(GDUMPMOVEGRP,10,190,205,60,"Move by Cell / Line / Page")
	ButtonGadget(GDUMPCL,12, 214, 30, 30,  "C-")
	ButtonGadget(GDUMPCP,44, 214, 30, 30,  "C+")
	ButtonGadget(GDUMPLL,80, 214, 30, 30,  "L-")
	ButtonGadget(GDUMPLP,112, 214, 30, 30,  "L+")
	ButtonGadget(GDUMPPL,148, 214, 30, 30,  "P-")
	ButtonGadget(GDUMPPP,180, 214, 30, 30,  "P+")

	groupgadget(GDUMUSEGRP,10,260,338,115,"Use cell value for")
	ButtonGadget(GDUMPNEW,12,282,80,30,"NEW ADR")
	ButtonGadget(GDUMPWCH,95,282,80,30,  "WATCHED")
	ButtonGadget(GDUMPBRK,178,282,80,30,  "BREAK ON")
	ButtonGadget(GDUMPSHW,260,282,80,30,  "SHW/EXP")

	groupgadget(GDUMPPTRGRP,15,313,250,55,"Use value as pointer")
	optiongadget(GDUMPPTRNO,20,335,80,30,"No ptr")
	optiongadget(GDUMPPTR1,100,335,50,30,"x 1")
	optiongadget(GDUMPPTR2,180,335,50,30,"x 2")
	SetGadgetState(GDUMPPTRNO,1)

	textgadget(GDUMPTYPE,12,380,200,30,"Type",0)
end sub

'============================
''create scintilla windows
'============================
private sub create_scibx(gadget as long, x as Long, y as Long , w as Long , h as Long  , Exstyle as integer = 0)
	dim as HWND hsci
	#ifdef __fb_win32__
		if dylibload("SciLexer.dll")=0 then
			hard_closing("SciLexer.dll not found")
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
	send_sci(SCI_MarkerDefine, 5,SC_MARK_FULLRECT)
	send_sci(SCI_MarkerDefine, 6,SC_MARK_FULLRECT)
	send_sci(SCI_MarkerDefine, 7,SC_MARK_CIRCLE)
	''color markers
	send_sci(SCI_MARKERSETFORE,0,KRED)
	send_sci(SCI_MARKERSETBACK,0,KWHITE)
	send_sci(SCI_MARKERSETFORE,1,KRED)
	send_sci(SCI_MARKERSETBACK,1,KRED)
	send_sci(SCI_MARKERSETFORE,2,KPURPLE)
	send_sci(SCI_MARKERSETBACK,2,KPURPLE)
	send_sci(SCI_MARKERSETFORE,3,KPURPLE)
	send_sci(SCI_MARKERSETBACK,3,KPURPLE)
	send_sci(SCI_MARKERSETFORE,4,KBLUE)
	send_sci(SCI_MARKERSETBACK,4,KBLUE)
	send_sci(SCI_MARKERSETFORE,5,KGREY)
	send_sci(SCI_MARKERSETBACK,5,KGREY)
	send_sci(SCI_MARKERSETFORE,6,KORANGE)
	send_sci(SCI_MARKERSETBACK,6,KORANGE)
	send_sci(SCI_MARKERSETFORE,7,KRED)
	send_sci(SCI_MARKERSETBACK,7,KRED)

	send_sci(SCI_StyleSetFore, KSTYLECUR, KRED)    ''style #50 FG set to red
	send_sci(SCI_StyleSetBack, KSTYLECUR, KYELLOW) ''style #50 BB set to green

	for imark as Integer = 0 To 5
	    send_sci(SCI_SetMarginMaskN, 1,-1)  ''all symbols allowed
	next
	'SendMessage(sciHWND, SCI_SETCODEPAGE, SC_CP_UTF8 ,0)
	'send_sci(SCI_SETLEXER, SCLEX_VB, 0 )
	'send_sci(SCI_SETKEYWORDS,0, @"sub function operator constructor destructor dim")
	'send_sci(SCI_STYLESETFORE, SCE_B_CONSTANT, &hFFFFFFFF)
	'send_sci(SCI_STYLESETFORE, SCE_B_NUMBER, &hFFFFFFFF)
	'send_sci(SCI_STYLESETFORE, SCE_B_KEYWORD, &hff00ff)
'===========
    send_sci(SCI_SETLEXER, SCLEX_FREEBASIC, 0 )
    'send_sci(SCI_STYLESETFONT, STYLE_DEFAULT , cast(lparam,@"Courier New"))
    'send_sci( SCI_STYLESETSIZE,STYLE_DEFAULT,11)
    send_sci(SCI_STYLECLEARALL, 0, 0)
    'send_sci( SCI_SETCODEPAGE, SC_CP_UTF8 ,0)
    send_sci(SCI_SETKEYWORDS,0, Cast(LPARAM,@"sub function operator constructor destructor property"))
    'send_sci(SCI_STYLESETFORE, SCE_B_COMMENT, &hff)
    send_sci(SCI_STYLESETFORE, SCE_B_KEYWORD, &h000000ff)
    'send_sci(SCI_STYLESETFORE, SCE_B_NUMBER, &h0)
    'send_sci(SCI_StyleSetBack, 2,&hFFFFFF)

    send_sci(SCI_SETCARETLINEVISIBLE, TRUE, 0)
    send_sci(SCI_SETCARETLINEBACK, &hf0f0f0 , 0)

    ''indicator style and color
    send_sci(SCI_INDICSETSTYLE,0,INDIC_FULLBOX)
    send_sci(SCI_INDICSETFORE,0,KYELLOW)
    send_sci(SCI_INDICSETUNDER,0,TRUE)
    send_sci(SCI_INDICSETALPHA,0,255)
    send_sci(SCI_SETINDICATORVALUE,0,0)
'===========
	'send_sci(SCI_USEPOPUP,SC_POPUP_NEVER,0)
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
	hsettings=create_window("Settings",10,10,500,500)

	groupgadget(LOGGROUP,10,10,450,95," Log  (shown on demand or continuously visible)")
	optiongadget(GLOGOFF,12,32,80,30,"Off")
	SetGadgetState(GLOGOFF,1) ''set on overriden by read_ini
	optiongadget(GLOGON,102,32,80,30,"On")
	optiongadget(GLOGCONT,192,32,90,30,"Continuous")
	CheckBoxGadget(GTRACEPROC,12,70,220,30,"Trace on for proc")
	CheckBoxGadget(GTRACELINE,232,70,220,30,"Trace on for line")
	CheckBoxGadget(GVERBOSE,12,120,220,30,"Verbose Mode On for proc/var")
	textgadget(GTEXTDELAY,12,155,200,30,"50< delay auto (ms) <10000",0)
	stringgadget(GAUTODELAY,210,155,50,30,str(autostep))
	textgadget(GTEXTCMDLP,12,195,110,30,"Command line",0)
	stringgadget(GCMDLPARAM,130,195,360,30,cmdexe(0))

	groupgadget(FONTGROUP,10,240,450,125,"Font for source code")
	textgadget(GTEXTFTYPE,12,260,200,30,"type",0)
	textgadget(GTEXTFSIZE,12,295,200,30,"size",0)
	textgadget(GTEXTFCOLOR,12,330,200,30,"color",0)

	groupgadget(BUTGROUP,10,380,450,60,"Click on a button for setting/removing")
	load_button2(SETBUTSTEP,12,400,butSTEP,@"[S]tep/line by line",,0)
	load_button2(SETBUTSTEPOVER,44,400,butSTEPOVER,@"Step [O]ver line",,0)
	load_button2(SETBUTAUTO,76,400,butAUTO,@"Step [A]utomatically, stopped by [H]alt",,0)
	load_button2(SETBUTSTOP,108,400,butSTOP,@"[H]alt running pgm",,0)
	load_button2(SETBUTCURSOR,140,400,butCURSOR,@"[R]un to cursor, stopped by [H]alt",,0)
	load_button2(SETBUTRUNEND,172,400,butRUNEND,@"[E]nd current proc, stopped by [H]alt",,0)
	load_button2(SETBUTRUNEXIT,204,400,butRUNEXIT,@"Run to e[X]it of prog, stopped by [H]alt",,0)
	load_button2(SETBUTKILL,236,400,butKILL,@"CAUTION [K]ill process",,0)
	load_button2(SETBUTEXECMOD,268,400,butEXEMOD,@"[M]odify execution, continue with line under cursor",,0)
	load_button2(SETBUTCRASH,300,400,butRUNCRASH,@"Run to crash in [L]ibrary, stopped by [H]alt",,0)
	load_button2(SETBUTFREE,332,400,butFREE,@"[F]ree debugged prgm",,0)

end sub
'=========================================================================
'' enables or disables buttons according the status and updates status
'=========================================================================
private sub but_enable()
 	select Case runtype
    	Case RTSTEP 'wait
			DisableGadget(IDBUTSTEP,0)
			DisableGadget(IDBUTSTEPOVER,0)
			DisableGadget(IDBUTRUNEND,0)
			DisableGadget(IDBUTAUTO,0)
			DisableGadget(IDBUTRUNEXIT,0)
			DisableGadget(IDBUTSTOP,0)
			DisableGadget(IDBUTCURSOR,0)
			DisableGadget(IDBUTCRASH,0)
			DisableGadget(IDBUTFREE,0)
			DisableGadget(IDBUTKILL,0)
			DisableGadget(IDBUTEXECMOD,0)

			statusbar_text(KSTBSTS,"Waiting "+stoplibel(stopcode))
			statusbar_text(KSTBTHD,"Thread "+Str(thread(threadcur).id))
			statusbar_text(KSTBUID,"Thread "+Str(thread(threadcur).id))
			statusbar_text(KSTBSRC,source_name(source(proc(procsv).sr)))
			statusbar_text(KSTBPRC,proc(procsv).nm)
			statusbar_text(KSTBFRT,left(Str(fasttimer),10))

   		case RTRUN,RTFREE 'step over / out / free / run / fast run
			DisableGadget(IDBUTSTEP,1)
			DisableGadget(IDBUTSTEPOVER,1)
			DisableGadget(IDBUTRUNEND,1)
			DisableGadget(IDBUTAUTO,1)
			DisableGadget(IDBUTRUNEXIT,1)
			DisableGadget(IDBUTCURSOR,1)
			DisableGadget(IDBUTCRASH,1)
			DisableGadget(IDBUTFREE,1)
			''DisableGadget(IDBUTkill,1) ''to let the possibility to kill the debuggee when running
			DisableGadget(IDBUTEXECMOD,1)
			Select Case runtype
				Case RTRUN
					statusbar_text(KSTBSTS,"Running")
				Case RTCRASH
					statusbar_text(KSTBSTS,"Run to crash")
				Case else
					statusbar_text(KSTBSTS,"Released")
			End Select

    	Case RTAUTO 'auto
			DisableGadget(IDBUTSTEP,1)
			DisableGadget(IDBUTSTEPOVER,1)
			DisableGadget(IDBUTRUNEND,1)
			DisableGadget(IDBUTAUTO,1)
			DisableGadget(IDBUTRUNEXIT,1)
			DisableGadget(IDBUTCURSOR,1)
			DisableGadget(IDBUTCRASH,1)
			DisableGadget(IDBUTFREE,1)
			DisableGadget(IDBUTKILL,1)
			DisableGadget(IDBUTEXECMOD,1)
      		statusbar_text(KSTBSTS,"Auto")
   		case Else 'prun=1 --> terminated or no pgm
			DisableGadget(IDBUTSTEP,1)
			DisableGadget(IDBUTRUNEND,1)
			DisableGadget(IDBUTAUTO,1)
			DisableGadget(IDBUTRUNEXIT,1)
			DisableGadget(IDBUTSTOP,1)
			DisableGadget(IDBUTCURSOR,1)
			DisableGadget(IDBUTCRASH,1)
			DisableGadget(IDBUTFREE,1)
			DisableGadget(IDBUTKILL,1)
			DisableGadget(IDBUTEXECMOD,1)
    	  	If runtype=RTEND Then statusbar_text(KSTBSTS,"Terminated")
   	End Select
End Sub
'=============================================================
'' enables or disables shortcuts according the status
'=============================================================
private sub shortcut_enable()
	if prun then
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_S,IDBUTSTEP)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_R,IDBUTCURSOR)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_O,IDBUTSTEPOVER)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_E,IDBUTRUNEND)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_A,IDBUTAUTO)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_X,IDBUTRUNEXIT)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_H,IDBUTSTOP)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_M,IDBUTEXECMOD)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_K,IDBUTKILL)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_L,IDBUTCRASH)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_F,IDBUTFREE)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_P,MNSETBRKP)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_C,MNSETBRKC)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_N,MNSETBRKN)
		AddKeyboardShortcut(hmain,FSHIFT  ,VK_N,MNRSTBRKN)
		AddKeyboardShortcut(hmain,FCONTROL,VK_N,MNCHGBRKN)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_T,MNSETBRKT)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_D,MNSETBRKD)
		AddKeyboardShortcut(hmain,FVIRTKEY,VK_B,MNMNGBRK)
	else
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTSTEP)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTCURSOR)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTSTEPOVER)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTRUNEND)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTAUTO)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTRUNEXIT)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTSTOP)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTEXECMOD)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTKILL)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTCRASH)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,IDBUTFREE)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,MNSETBRKP)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,MNSETBRKC)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,MNSETBRKN)
		AddKeyboardShortcut(hmain,FSHIFT  ,0,MNRSTBRKN)
		AddKeyboardShortcut(hmain,FCONTROL,0,MNCHGBRKN)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,MNSETBRKT)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,MNSETBRKD)
		AddKeyboardShortcut(hmain,FVIRTKEY,0,MNMNGBRK)
	end if
end sub
'=============================================================
'' enables or disables menu options according the status
'=============================================================
private sub menu_enable()
	dim flag As Integer
	If prun Then flag=0 Else flag=1
	SetStateMenu(HMenusource,MNSETBRKP, flag)
	SetStateMenu(HMenusource,MNSETBRKC, flag)
	SetStateMenu(HMenusource,MNSETBRKT, flag)
	SetStateMenu(HMenusource,MNSETBRKN, flag)
	SetStateMenu(HMenusource,MNRSTBRKN, flag)
	SetStateMenu(HMenusource,MNCHGBRKN, flag)
	SetStateMenu(HMenusource,MNSETBRKD,  flag)
	SetStateMenu(HMenusource,MNTHRDAUT, flag)
	SetStateMenu(HMenusource,MNSHWVAR,  flag)
	SetStateMenu(HMenusource,MNSETWVAR, flag)
	SetStateMenu(HMenusource,MNLINEADR, flag)
	SetStateMenu(HMenusource2,MNLINEADR,flag)
	SetStateMenu(HMenusource2,MNASMLINE,flag)
	SetStateMenu(HMenusource2,MNASMPRCL,flag)
	SetStateMenu(HMenusource2,MNASMREGS,flag)
	SetStateMenu(HMenusource,MNACCLINE,flag)

	SetStateMenu(HMenuvar1,MNSETWTCH,   flag)
	SetStateMenu(HMenuvar1,MNSETWTTR,   flag)
	SetStateMenu(HMenuvar,MNSELIDX,     flag)
	SetStateMenu(HMenuvar5,MNTRCKARR,    flag)
	SetStateMenu(HMenuvar5,MNTRCKIDX0,   flag)
	SetStateMenu(HMenuvar5,MNTRCKIDX1,   flag)
	SetStateMenu(HMenuvar5,MNTRCKIDX2,   flag)
	SetStateMenu(HMenuvar5,MNTRCKIDX3,   flag)
	SetStateMenu(HMenuvar5,MNTRCKIDX4,   flag)
	SetStateMenu(HMenuvar5,MNTRCKRST,    flag)

	SetStateMenu(HMenuvar,MNVARDMP,  flag)
	SetStateMenu(HMenuvar,MNVAREDT,  flag)
	SetStateMenu(HMenuvar,MNSHWEXP,  flag)
	SetStateMenu(HMenuvar2,MNBRKVC,   flag)
	SetStateMenu(HMenuvar2,MNBRKV1,   flag)
	SetStateMenu(HMenuvar2,MNBRKV2,   flag)
	SetStateMenu(HMenuvar6,MNBRCVC,   flag)
	SetStateMenu(HMenuvar6,MNBRCV1 ,  flag)
	SetStateMenu(HMenuvar6,MNBRCV2 ,  flag)
	SetStateMenu(HMenuvar,MNSHSTRG,  flag)
	SetStateMenu(HMenuvar,MNSHCHAR,  flag)
	SetStateMenu(HMenuvar,MNCHGZSTR, flag)
	SetStateMenu(HMenuvar3,MNLSTVARA, flag)
	SetStateMenu(HMenuvar3,MNLSTVARS, flag)
	SetStateMenu(HMenuvar3,MNCLBVARA, flag)
	SetStateMenu(HMenuvar3,MNCLBVARS, flag)
	SetStateMenu(HMenuvar,MNPTDUMP,  flag)
	SetStateMenu(HMenuvar,MNFNDVAR,  flag)
	SetStateMenu(HMenuvar4,MNVARCOLI, flag)
	SetStateMenu(HMenuvar4,MNVAREXPI, flag)
	SetStateMenu(HMenuvar4,MNVARCOLA, flag)
	SetStateMenu(HMenuvar4,MNVAREXPA, flag)

	SetStateMenu(HMenuthd,MNTHRDCHG,flag)
	SetStateMenu(HMenuthd,MNTHRDKLL,flag)
	SetStateMenu(HMenuthd,MNEXCLINE,flag)
	SetStateMenu(HMenuthd,MNCREATHR,flag)
	SetStateMenu(HMenuthd,MNTHRDEXP,flag)
	SetStateMenu(HMenuthd,MNTHRDCOL,flag)
	SetStateMenu(HMenuthd,MNLOCPRC, flag)
	SetStateMenu(HMenuthd,MNSHWPROC,flag)
	SetStateMenu(HMenuthd,MNTCHNING,flag)
	SetStateMenu(HMenuthd,MNSHPRSRC,flag)
	SetStateMenu(HMenuthd,MNPRCRADR,flag)
	SetStateMenu(HMenuthd,MNTHRDLST,flag)

	SetStateMenu(HMenutools,MNLISTDLL, flag)

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

	If procnb Then flag=0 Else flag=1
	SetStateMenu(HMenuprc,MNENBPRC,flag)
	SetStateMenu(HMenuprc,MNASMPRC,flag)
	SetStateMenu(HMenuprc,MNLOCPRC,flag)
	SetStateMenu(HMenuprc,MNSORTPRC,flag)

	SetStateMenu(HMenuvar,MNLOCPRC,flag)
	SetStateMenu(HMenuvar,MNCALLINE,flag)

	If brknb then flag=0 else flag=1
	SetStateMenu(HMenusource,MNMNGBRK,flag)

End sub
'=================================================
''contextual menus
'================================================
private sub menu_set()

''menu source
	HMenusource=CreatePopMenu()
	MenuItem(MNSETBRKP,HMenusource,"Set/Clear [P]ermanent Breakpoint")
	CreateIconItemMenu(HMenusource,MNSETBRKP,catch_image(butBRKP))
	MenuItem(MNSETBRKC,HMenusource,"Set/Clear [C]onditionnal Breakpoint")
	CreateIconItemMenu(HMenusource,MNSETBRKC,catch_image(butBRKC))
	MenuItem(MNSETBRKT,HMenusource,"Set/Clear [T]emporary Breakpoint")
	CreateIconItemMenu(HMenusource,MNSETBRKT,catch_image(butBRKT))
	MenuItem(MNSETBRKN,HMenusource,"Set/clear Breakpoint with cou[N]ter")
	CreateIconItemMenu(HMenusource,MNSETBRKN,catch_image(butBRKN))
	MenuItem(MNSETBRKD ,HMenusource,"[D]isable/enable Breakpoint")
	CreateIconItemMenu(HMenusource,MNSETBRKD,catch_image(butBRKD))
	HMenusource1=OpenSubMenu(HMenusource,"Manage breakpoints")
	MenuItem(MNRSTBRKN,HMenusource1,"ReSet initial counter value")
	MenuItem(MNCHGBRKN,HMenusource1,"Change counter value")
	MenuItem(MNMNGBRK ,HMenusource1,"Manage [B]reakpoints")
	MenuBar(HMenusource)
	MenuItem(MNACCLINE,HMenusource,"Mark not executable lines")
	MenuItem(MNTHRDAUT,HMenusource,"Step auto multi threads")
	MenuBar(HMenusource)
	MenuItem(MNSHWVAR,HMenusource,"Show var"+Chr(9)+"Ctrl+Left click")
	MenuItem(MNSETWVAR,HMenusource,"Set watched var"+Chr(9)+"Alt+Left click")
	MenuBar(HMenusource)
	MenuItem(MNFNDTXT,HMenusource,"Find text / Ctrl+F")
	MenuItem(MNGOTO,HMenusource,"Goto Line")
	HMenusource2=OpenSubMenu(HMenusource,"ASM data")
	MenuItem(MNLINEADR,HMenusource2,"Line Address")
	MenuItem(MNASMLINE,HMenusource2, "Asm code of line")
	MenuItem(MNASMPRCL,HMenusource2,"Asm code of proc (from line)")
	MenuItem(MNASMREGS,HMenusource2,"Show registers for current thread)")


''menu proc/var
	HMenuvar=CreatePopMenu()
	HMenuvar1=OpenSubMenu(HMenuvar,"Set watched")
	MenuItem(MNSETWTCH,HMenuvar1,"Set watched")
	MenuItem(MNSETWTTR,HMenuvar1,"Set watched+trace")
	HMenuvar2=OpenSubMenu(HMenuvar,"Break on var value")
	MenuItem(MNBRKVC,HMenuvar2, "Var / const")
	MenuItem(MNBRKV1,HMenuvar2, "Var1  (+ var2)")
	MenuItem(MNBRKV2,HMenuvar2, "Var2")
	MenuItem(MNBRKVS,HMenuvar2, "Show BP if any")
	HMenuvar6=OpenSubMenu(HMenuvar,"Breakpoint conditional")
	MenuItem(MNBRCVC,HMenuvar6, "Var / const")
	MenuItem(MNBRCV1,HMenuvar6, "Var1  (+ var2 then select line)")
	MenuItem(MNBRCV2,HMenuvar6, "Var2")
	MenuBar(HMenuvar)
	MenuItem(MNSELIDX,HMenuvar, "Array management (Index selection)")
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
	MenuItem(MNFNDVAR,HMenuvar, "Find any text")
	HMenuvar3=OpenSubMenu(HMenuvar,"List to log / Copy to clipboard")
	MenuItem(MNLSTVARA,HMenuvar3, "List all items")
	MenuItem(MNLSTVARS,HMenuvar3, "List selected item+childs")
	MenuItem(MNCLBVARA,HMenuvar3, "Copy all items")
	MenuItem(MNCLBVARS,HMenuvar3, "Copy selected item+childs")
	HMenuvar4=OpenSubMenu(HMenuvar,"Collapse / Expand")
	MenuItem(MNVARCOLI,HMenuvar4,  "Collapse Item+childs")
	MenuItem(MNVAREXPI,HMenuvar4,  "Expand item+childs")
	MenuItem(MNVARCOLA,HMenuvar4,  "Collapse All")
	MenuItem(MNVAREXPA,HMenuvar4,  "Expand All")

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
	MenuItem(MNENBPRC,HMenuprc, "Enable/disable")


''menu thread
	HMenuthd=CreatePopMenu()
	MenuItem(MNTHRDCHG,HMenuthd, "Select next thread to be executed")
	MenuItem(MNEXCLINE,HMenuthd, "Show next executed line (source)")
	MenuItem(MNCREATHR,HMenuthd, "Show line creating thread (source)")
	MenuItem(MNLOCPRC,HMenuthd,  "Show first proc of thread (source)")
	MenuItem(MNSHWPROC,HMenuthd, "Show proc (proc/var)")
	MenuItem(MNSHPRSRC,HMenuthd, "Show proc (source)")
	MenuItem(MNTCHNING,HMenuthd, "Proc call Chain")
	MenuItem(MNPRCRADR,HMenuthd, "Proc Addresses")
	MenuItem(MNTHRDKLL,HMenuthd, "Kill thread")
	MenuItem(MNTHRDEXP,HMenuthd, "Expand one thread")
	MenuItem(MNTHRDCOL,HMenuthd, "Collapse all threads")
	MenuItem(MNTHRDLST,HMenuthd, "List all threads")

''menu tools
	HMenutools=CreatePopMenu()
	MenuItem(MNABOUT,HMenutools,   "About")
	MenuItem(MNSETTINGS,HMenutools,   "Settings")
	MenuItem(MNCMPINF,HMenutools,  "Compile info")
	MenuItem(MNDBGHELP,HMenutools, "Help / F1")
	MenuItem(MNSHOWLOG,HMenutools,  "Show log file")
	MenuItem(MNRESETLOG,HMenutools, "Delete log file")
	MenuItem(MNLISTENUM,HMenutools,  "List enum")
	MenuItem(MNLISTPROCESS,HMenutools,  "Process list")
	MenuItem(MNLISTDLL,HMenutools, "Dlls list")
	MenuItem(MNWINMSG,HMenutools, "Translate Win Message")
	MenuItem(MNSHWBDH,HMenutools, "Bin/Dec/Hex")
	MenuItem(MNFRTIMER,HMenutools,"Show fast run timer")
	MenuItem(MNJITDBG,HMenutools, "Set JIT Debugger")
End Sub

'===========================================
'' Initialise all the GUI windows/gadgets
'===========================================
private sub gui_init()

	''main windows
	hmain=OpenWindow("",10,10,1150,600)
	settitle()
	''scintilla gadget
	create_scibx(GSCINTILLA,0,80,450,WindowClientHeight(hmain)-100,)

	''source panel
	'Var font=LoadFont("Arial",40)

	PanelGadget(GSRCTAB,2,52,450,20)
    SetGadgetFont(GSRCTAB,CINT(LoadFont("Courier New",11)))

	''file combo/buuton ''idee mettre dans le menu affichage de la liste (du combo)
	ComboBoxGadget(GFILELIST,790,0,200,80)
	ButtonGadget(GFILESEL,992,2,30,20,"Go")

	''status bar
	StatusBarGadget(GSTATUSBAR,"",SBT_TOOLTIPS)
	statusbar_text(KSTBSTS,"No program")
	statusbar_text(KSTBTHD,"Thread number")
	statusbar_text(KSTBUID,"UID number Linux")
	statusbar_text(KSTBSRC,"Current source")
	statusbar_text(KSTBPRC,"Current proc")
	statusbar_text(KSTBFRT,"Fast time ?")

	''current line
	textGadget(GCURRENTLINE,2,28,450,30,"Next exec line : ",SS_NOTIFY )
	'textGadget(GCURRENTLINE,2,28,450,30,"1234567890123456789012345678901234567890123456789012345678901234567890",SS_NOTIFY )
	GadgetToolTip(GCURRENTLINE,"next executed line"+chr(13)+"Click on me to reach the line",GCURLINETTIP)

	''buttons
	load_button2(IDBUTSTEP,8,,butSTEP,@"[S]tep/line by line")
	load_button2(IDBUTSTEPOVER,40,,butSTEPOVER,@"Step [O]ver line")
	load_button2(IDBUTAUTO,72,,butAUTO,@"Step [A]utomatically, stopped by [H]alt")
	load_button2(IDBUTSTOP,110,,butSTOP,@"[H]alt running pgm")
	load_button2(IDBUTCURSOR,148,,butCURSOR,@"[R]un to cursor, stopped by [H]alt")
	load_button2(IDBUTRUNEND,180,,butRUNEND,@"[E]nd current proc, stopped by [H]alt")
	load_button2(IDBUTRUNEXIT,212,,butRUNEXIT,@"Run to e[X]it of prog, stopped by [H]alt")
	load_button2(IDBUTKILL,250,,butKILL,@"CAUTION [K]ill process")
	load_button2(IDBUTEXECMOD,282,,butEXEMOD,@"[M]odify execution, continue with line under cursor")
	load_button2(IDBUTCRASH,314,,butRUNCRASH,@"Run to crash in [L]ibrary, stopped by [H]alt")
	load_button2(IDBUTFREE,352,,butFREE,@"[F]ree debugged prgm")

	load_button2(IDBUTRERUN,466,,butRERUN,@"Restart debugging (exe)",TTRERUN,1)
	load_button2(IDBUTLASTEXE,498,,butLASTEXE,@"Last 10 exe(s)",,0)
	load_button2(IDBUTATTCH,530,,butATTCH,@"Attach running program",,0)
	load_button2(IDBUTFILE,562,,butFILE,@"Select EXE",,0)
	load_button2(IDBUTTOOL,628,,butTOOL,@"Some usefull....Tools",,0)
	load_button2(IDBUTUPDATE,660,,butUPDATE,@"Update On /Update off : variables, dump",,0)
	load_button2(IDBUTENLRSRC,692,,butENLRSRC,@"Enlarge/reduce source")
	load_button2(IDBUTENLRVAR,724,,butENLRVAR,@"Enlarge/reduce proc/var")
	load_button2(IDBUTENLRMEM,756,,butENLRMEM,@ "Enlarge/reduce dump memory")

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
	PanelGadget(GRIGHTTABS,500,30,599,410)
	SetGadgetFont(GRIGHTTABS,CINT(LoadFont("Courier New",11)))

	''treeview proc/var
	var htabvar=AddPanelGadgetItem(GRIGHTTABS,TABIDXVAR,"Proc/var",,1)
	htviewvar=treeviewgadget(GTVIEWVAR,0,0,599,365,KTRRESTYLE)

	''treeview procs
	var htabprc=AddPanelGadgetItem(GRIGHTTABS,TABIDXPRC,"Procs",,1)
	htviewprc=treeviewgadget(GTVIEWPRC,0,0,599,365,KTRRESTYLE)

	''treeview threads
	var htabthd=AddPanelGadgetItem(GRIGHTTABS,TABIDXTHD,"Threads",,1)
	htviewthd=treeviewgadget(GTVIEWTHD,0,0,599,365,KTRRESTYLE)

	''treeview watched
	var htabwch=AddPanelGadgetItem(GRIGHTTABS,TABIDXWCH,"Watched",,1)
	htviewwch=treeviewgadget(GTVIEWWCH,0,0,599,365,KTRRESTYLE)

	''dump memory
	var htabmem=AddPanelGadgetItem(GRIGHTTABS,TABIDXDMP,"Memory",,1)
	#Ifdef __FB_WIN32__
		Var Style=LVS_EX_GRIDLINES or LVS_EX_FULLROWSELECT
	#Else
		Var Style=LVS_EX_GRIDLINES
	#EndIf
	hlviewdmp=ListViewGadget(GDUMPMEM,0,0,599,380,style)
	AddListViewColumn(GDUMPMEM, "Address",0,0,100)
	AddListViewColumn(GDUMPMEM, "Ascii value",5,5,100)

	''for log display
	hlogbx=create_window("Log file",10,10,450,550)
	EditorGadget(GLOG,10,10,400,500,"Your log file if any")
	ReadOnlyEditor(GLOG,1)



	create_shwexpbx()
	create_settingsbx()
	create_dumpbx()
	create_brkbx()
	create_bpcondbx()
	create_indexbx()
	create_cchainbx()
	create_brkvbx
	create_editbx()
	menu_set()

end sub
'===============================================
'' Freea all the gadgets
'===============================================
private sub freegadgets()
	messbox("Feature to be coded","freegadgets()")
end sub
