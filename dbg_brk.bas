'===================================================
'' set/unset breakpoint markers
'===================================================
sub brk_marker(brkidx as integer)
	dim as integer src,lline=brkol(brkidx).nline-1,typ

	if brkol(brkidx).typ>50 then
		typ=6 ''disabled --> grey marker
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
'========================================
''
'========================================
private function brk_comp(tst as INTEGER) as  zstring ptr
	Select Case tst
		Case 32
		return @"="
		Case 16
		return @"<>"
		Case 8
		return @">"
		Case 4
		return @"<"
		Case 2
		return @">="
		Case 0
		return @"<="
	End Select
end function

'========================================
'' updates break on var/mem
'========================================
private sub brkv_update()

	var txt=getgadgettext(GBRKVALUE)
	dim as integer vflag=1
	dim as double vald

	brkv.ttb=32 Shr GetItemComboBox(GBRKVCOND)

	vald=Val(txt)
	Select Case brkv.typ
	   Case 2
		  If vald<-128 Or vald>127 Then setwindowtext(hbrkvbx,"min -128,max 127"):vflag=0
	   Case 3
		  If vald<0 Or vald>255 Then setwindowtext(hbrkvbx,"min 0,max 255"):vflag=0
	   Case 5
		  If vald<-32768 Or vald>32767 Then setwindowtext(hbrkvbx,"min -32768,max 32767"):vflag=0
	   Case 6
		  If vald<0 Or vald>65535 Then setwindowtext(hbrkvbx,"min 0,max 65535"):vflag=0
	   Case 1
		  If vald<-2147483648 Or vald>2147483648 Then setwindowtext(hbrkvbx,"min -2147483648,max +2147483647"):vflag=0
	   Case 7,8
		  If vald<0 Or vald>4294967395 Then setwindowtext(hbrkvbx,"min 0,max 4294967395"):vflag=0
	   Case 9
		  If vald<-9223372036854775808 Or vald>9223372036854775807 Then setwindowtext(hbrkvbx,"min -9223372036854775808,max 9223372036854775807"):vflag=0
	   Case 10
		  If vald<0 Or vald>18446744073709551615 Then setwindowtext(hbrkvbx,"min 0,max 18446744073709551615"):vflag=0
	End Select
	if vflag=1 then
		Select Case brkv.typ
			Case 2
				brkv.val.vbyte=ValInt(txt)
				brkv.vst=Str(brkv.val.vbyte)
			Case 3
				brkv.val.vubyte=ValUInt(txt)
				brkv.vst=Str(brkv.val.vubyte)
			Case 5
				brkv.val.vshort=ValInt(txt)
				brkv.vst=Str(brkv.val.vshort)
			Case 6
				brkv.val.vushort=ValUInt(txt)
				brkv.vst=Str(brkv.val.vushort)
			Case 1
				brkv.val.vinteger=ValInt(txt)
				brkv.vst=Str(brkv.val.vinteger)
			Case 7,8
				brkv.val.vuinteger=ValUInt(txt)
				brkv.vst=Str(brkv.val.vuinteger)
			Case 9
				 brkv.val.vlongint=ValLng(txt)
				 brkv.vst=Str(brkv.val.vlongint)
			Case 10
				 brkv.val.vulongint=ValULng(txt)
				 brkv.vst=Str(brkv.val.vulongint)
			Case Else
				brkv.vst=Left(txt,26)'str(brkv.val.vuinteger)
		End Select

		Select Case brkv.ttb
			Case 1
			brkv.txt+="="
			Case 2
			brkv.txt+="<>"
			Case 3
			brkv.txt+=">"
			Case 4
			brkv.txt+="<"
			Case 5
			brkv.txt+=">="
			Case 6
			brkv.txt+="<="
		End Select
		Modify_Menu(MNBRKVC,HMenuvar,brkv.txt+brkv.vst)
		hidewindow(hbrkvbx ,KHIDE)
	end if
end sub
'========================================================================
''
'========================================================================
private sub brkv_set(a As Integer) ''break on variable change
	Dim As String txt
	If a=0 Then 'cancel break
		brkv.adr1=0
		brkv.adr2=0
		brkv.ivr1=0
		brkv.ivr2=0
		Modify_Menu(MNBRKVS,HMenuvar2,"Show BP if any")
		statusbar_text(KSTBBPM,"")
		SetStateMenu(HMenuvar2,MNBRKVS,1)
		hidewindow(hbrkvbx ,KHIDE)
		Exit Sub

	ElseIf a=1 Then ''mem/const
		var_fill(brkidx1)
		brkv.typ=brkdatatype
		brkv.adr1=varfind.ad
		brkv.vst=""
		brkv.ttb=32 shr GetItemComboBox(GBRKVCOND)
		brkv.ivr1=brkidx1
		txt=varfind.nm+" "+*brk_comp(brkv.ttb)+" "
		if brkdatatype=11 then
			brkv.val.vsingle=val(getgadgettext(GBRKVALUE))
			txt+=str(brkv.val.vsingle)
		elseIf brkdatatype=12 then
			brkv.val.vdouble=val(getgadgettext(GBRKVALUE))
			txt+=str(brkv.val.vdouble)
		else
			brkv.val.vlongint=vallng(getgadgettext(GBRKVALUE))
			txt+=str(brkv.val.vlongint)
		EndIf

	ElseIf a=2 Then ''mem/mem
		var_fill(brkidx1)
		brkv.typ=brkdatatype
		brkv.adr1=varfind.ad
		txt=varfind.nm
		var_fill(brkidx2)
		brkv.adr2=varfind.ad
		brkv.ttb=32 shr GetItemComboBox(GBRKVCOND)
		brkv.ivr1=brkidx1
		brkv.ivr2=brkidx2
		txt+=*brk_comp(brkv.ttb)+" "+varfind.nm
	end if

	modify_menu(MNBRKVS,HMenuvar2,txt)
	SetStateMenu(HMenuvar2,MNBRKVS,0)
	statusbar_text(KSTBBPM,"BPM-> "+txt)
		'' if dyn array store real adr
		'If Cast(Integer,vrb(varfind.pr).arr)=-1 Then
			'ReadProcessMemory(dbghand,Cast(LPCVOID,vrr(varfind.iv).ini),@brkv.arr,sizeof(integer),0)
		'Else
			'brkv.arr=0
		'End If

		'If vrb(varfind.pr).mem=3 Then
			'brkv.psk=-2 'static
		'Else
			'For j As UInteger = 1  To procrnb 'find proc to delete watching
				'If varfind.iv>=procr(j).vr And varfind.iv<procr(j+1).vr Then
					'brkv.psk=procr(j).sk
					'Exit For
				'EndIf
			'Next
		'End If
		'ztxt=GetTextTreeView(GTVIEWVAR,vrr(varfind.iv).tv)
	'Else 'update
		'ztxt=GetTextTreeView(GTVIEWVAR,vrr(varfind.iv).tv)
	'End If
	'brkv.txt=Left(ztxt,InStr(ztxt,"<"))+var_sh2(brkv.typ,brkv.adr,p)



	'If brkv.vst="" Then
	  'brkv.vst=Mid(brkv.txt,InStr(brkv.txt,"=")+1,25)
	'End If
	'brkv.txt+=" Stop if it becomes "
	'ShowListComboBox(GBRKVCOND,1)
	'SetGadgetText(GBRKVAR1,brkv.txt)
	'setgadgettext(GBRKVALUE,brkv.vst)
	'hidewindow(hbrkvbx,KSHOW)
End Sub
'================================
Private sub brk_apply()
'brkexe = <name>,<#line>,<typ>
Dim f As boolean =FALSE
For i As Integer =1 To BRKMAX
	Dim As String brks,fn
	Dim As Integer p,p2,ln,ty
	Dim As UInteger cntr
	If brknb=BRKMAX Then Exit For 'no more breakpoint possible

	If brkexe(0,i)<>"" Then 'not empty
		brks=brkexe(0,i)
		p=InStr(brks,",") 'parsing
		fn=Left(brks,p-1) 'file name
		p2=p+1
		p=InStr(p2,brks,",")
		ln=ValInt(Mid(brks,p2,p-p2)) 'number line
		p2=p+1
		p=InStr(p2,brks,",")
		cntr=ValUInt(Mid(brks,p2,p-p2)) 'counter
		ty=ValInt(Right(brks,1)) 'type
		For j As Integer =0 To sourcenb
			If source_name(source(j))=fn Then 'name matching
				For k As Integer= 1 To linenb
					If rline(k).nu=ln AndAlso rline(k).sx=j Then 'searching index in rline
						brknb+=1
						brkol(brknb).isrc =j
						brkol(brknb).nline=ln
						brkol(brknb).index=k
						brkol(brknb).ad   =rline(k).ad
						brkol(brknb).typ  =ty
						brkol(brknb).cntrsav=cntr
						brkol(brknb).counter=cntr
						brk_marker(brknb)
						f=TRUE 'flag for managing breakpoint
						Exit For
					EndIf
				Next
				brkexe(0,i)="" 'used one time
				Exit For
			EndIf
		Next
	EndIf

Next
If f Then
	brk_manage("Restart debuggee, managing breakpoints")
	SetStateMenu(HMenusource,MNMNGBRK,0)
	DisableGadget(IDBUTBRKB,0)
EndIf
End Sub
'============================================
Private sub brk_sav()
	For i As Integer =1 To BRKMAX
		If i<=brknb Then
			if brkol(i).typ=1 or brkol(i).typ=5 or brkol(i).typ=4 or brkol(i).typ=51 or brkol(i).typ=55 or brkol(i).typ=54 then ''only permanent/tempo/counter
				brkexe(0,i)=source_name(source(brkol(i).isrc))+","+Str(brkol(i).nline)+","+Str(brkol(i).cntrsav)+","+Str(brkol(i).typ)
			else
				brkol(i).typ=0
				brk_marker(i)
			EndIf
		EndIf
	Next
End Sub
'================================================
 '' deletes one breakpoint and compresses data
'================================================
 private sub brk_del(n as integer)
	brkol(n).typ=0
	brkol(n).cntrsav=0
	brk_marker(n)
	if n=0 then ''run
		exit sub
	EndIf
	brknb-=1
	For i As Integer =n To brknb
		brkol(i)=brkol(i+1)
	Next
	If brknb=0 Then
		SetStateMenu(HMenusource,MNMNGBRK,1)
		DisableGadget(IDBUTBRKB,1)
		hidewindow(hbrkbx,KHIDE) ''even if not show
	EndIf
End Sub
'================================================================================
'' tests the values for breakpoint on var/mem or conditionnal
'================================================================================
private function brk_test(adr1 as INTEGER,adr2 as integer=0,datatype as integer,data2 as valeurs,comptype as byte) As integer

	'Dim As Integer adr,temp2,temp3
	'Dim As String strg1,strg2,strg3
	'dim ptrs As pointeurs
	'ptrs.pxxx=@recup(0)
	Dim As Integer flag=0
	'dim as integer recup(20)
	dim as valeurs recup1,recup2
'print adr1,adr2,datatype,data2.vlongint,data2.vdouble,comptype
		'If brkv.arr Then 'watching dyn array element ?
			'adr=vrr(brkv.ivr).ini
			'ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@adr,4,0)
			'If adr<>brkv.arr Then brkv.adr+=brkv.arr-adr:brkv.arr=adr 'compute delta then add it if needed
			'temp2=vrr(brkv.ivr).ini+2*SizeOf(Integer) 'adr global size
			'ReadProcessMemory(dbghand,Cast(LPCVOID,temp2),@temp3,SizeOf(Integer),0)
			'If brkv.adr>adr+temp3 Then 'out of limit ?
				'brkv_set(0) 'erase
				'Return FALSE
			'End If
		'End If
	''recup2 recup1
	''21 --> <> or > or >=
	''26 --> <> or < or <=
	''35 --> = or >= or <=
	''16 --> <>

	#Ifdef __fb_win32__
		ReadProcessMemory(dbghand,Cast(LPCVOID,adr1),@recup1,8,0)
	#else
		recup1.vlongint=readmemlongint(thread(threadcur).id, adr1)
	#endif
	if adr2 then
		#Ifdef __fb_win32__
			ReadProcessMemory(dbghand,Cast(LPCVOID,adr2),@recup2,8,0)
		#else
			recup2.vlongint=readmemlongint(thread(threadcur).id, adr2)
		#endif
	else
		recup2=data2
	EndIf
'print "recup1,recup2=";recup1.vlongint,recup2.vlongint ''todo remove me
	Select Case datatype
		Case 2 'byte
			if recup2.vbyte>recup1.vbyte then
				If 21 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vbyte<recup1.vbyte Then
				If 26 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vbyte=recup1.vbyte Then
				If 35 And comptype Then
					flag=1
				EndIf
			End If

		Case 3 'ubyte
			if recup2.vubyte>recup1.vubyte then
				If 21 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vubyte<recup1.vubyte Then
				If 26 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vubyte=recup1.vubyte Then
				If 35 And comptype Then
					flag=1
				EndIf
			End If

		Case 5 'short
			if recup2.vshort>recup1.vshort then
				If 21 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vshort<recup1.vshort Then
				If 26 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vshort=recup1.vshort Then
				If 35 And comptype Then
					flag=1
				EndIf
			End If

		Case 6 'ushort
			if recup2.vushort>recup1.vushort then
				If 21 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vushort<recup1.vushort Then
				If 26 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vushort=recup1.vushort Then
				If 35 And comptype Then
					flag=1
				EndIf
			End If

		Case 1 'integer32/long
			if recup2.vinteger>recup1.vinteger then
				If 21 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vinteger<recup1.vinteger Then
				If 26 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vinteger=recup1.vinteger Then
				If 35 And comptype Then
					flag=1
				EndIf
			End If

		Case 8 'uinteger32/ulong
			if recup2.vuinteger>recup1.vuinteger then
				If 21 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vuinteger<recup1.vuinteger Then
				If 26 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vuinteger=recup1.vuinteger Then
				If 35 And comptype Then
					flag=1
				EndIf
			End If

		Case 9 'integer64/longint
			if recup2.vlongint>recup1.vlongint then
				If 21 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vlongint<recup1.vlongint Then
				If 26 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vlongint=recup1.vlongint Then
				If 35 And comptype Then
					flag=1
				EndIf
			End If
		Case 10 'uinteger64/ulonginit
			if recup2.vulongint>recup1.vulongint then
				If 21 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vulongint<recup1.vulongint Then
				If 26 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vulongint=recup1.vulongint Then
				If 35 And comptype Then
					flag=1
				EndIf
			End If
		Case 11 'single
			if recup2.vsingle>recup1.vsingle then
				If 21 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vsingle<recup1.vsingle Then
				If 26 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vsingle=recup1.vsingle Then
				If 35 And comptype Then
					flag=1
				EndIf
			End If

		Case 12 'double
			if recup2.vdouble>recup1.vdouble then
				If 21 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vdouble<recup1.vdouble Then
				If 26 And comptype Then
					flag=1
				EndIf
			ElseIf recup2.vdouble=recup1.vdouble Then
				If 35 And comptype Then
					flag=1
				EndIf
			End If
		''strings not yet handled
		'Case 4,13,14
			'If brkv.typ=13 Then  ' normal string
				'ReadProcessMemory(dbghand,Cast(LPCVOID,brkv.adr),@adr,SizeOf(Integer),0) 'address ptr 25/07/2015 64bit
			'Else
				'adr=brkv.adr
			'End If
			'Clear recup(0),0,26 'max 25 char
			'ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@recup(0),25,0) 'value
			'strg1=*ptrs.pzstring
			'If brkv.ttb=16 Then
				'If brkv.vst<>strg1 Then
					'flag=1
				'EndIf
			'Else
				'If brkv.vst=strg1
				'Then flag=1
			'EndIf

	End Select
	return flag ''0 test false / 1 test true
end function
'=======================================================================
'' removes all ABP / disables all UBP if necessary
'=======================================================================
private sub brk_unset(ubpon as integer=false)
	if brkv.adr1 then ''restore by default ABP on all line
		For j As Integer = 1 To linenb 'restore all instructions
		  WriteProcessMemory(dbghand,Cast(LPVOID,rline(j).ad),@breakcpu,1,0)
		Next
	else
	'print "in brk_unset restoring all instructions"

		#ifdef __fb_WIN32__
			For j As Integer = 1 To linenb 'restore all instructions
			'If proc(rline(j).px).nu=-1 Then print "brk_unset =-1"
			'If proc(rline(j).px).nm="main" or proc(rline(j).px).nm="MAIN" then
				'print "set only main=";hex(rline(j).ad)
				WriteProcessMemory(dbghand,Cast(LPVOID,rline(j).ad),@rLine(j).sv,1,0)
				'sleep 500
			'EndIf

			Next
		#else
			if ccstate=KCC_ALL then
				msgdata=0 ''restore sv everywhere
				exec_order(KPT_CCALL)
			end if
		#EndIf




		For jbrk As Integer = 1 To brknb ''restore if needed the UBP
			If brkol(jbrk).typ<50 Then
				if ubpon=true then
					WriteProcessMemory(dbghand,Cast(LPVOID,brkol(jbrk).ad),@breakcpu,1,0) ''only BP enabled
				else
					brkol(jbrk).typ+=50 ''disable all UBP
					brk_marker(jbrk)
				end if
			end if
		Next
		if brkol(0).typ<>0 then
			'print "put CC on line ad=";hex(brkol(0).ad)
			WriteProcessMemory(dbghand,Cast(LPVOID,brkol(0).ad),@breakcpu,1,0)
		EndIf
	end if
End Sub
'============================================================================
''fills array listitem()
'============================================================================
private sub brc_fill(parent as integer,hitem as INTEGER)
	dim as integer child
	while hitem<>0
		listcpt+=1
		listitem(listcpt).items=hitem
		listitem(listcpt).itemc=AddTreeViewItem(GTVIEWBRC,GetTextTreeView(GTVIEWVAR,hitem),cast (hicon, 0),0,TVI_LAST,parent)
		child=GetChildItemTreeView(GTVIEWVAR,hitem)
		if child<>0 then
			brc_fill(listitem(listcpt).itemc,child)
		EndIf
	    hitem=GetNextItemTreeView(GTVIEWVAR,hitem)
	wend
End Sub
'============================================================================
''prepares for listing all the existing variables/fields for cond BP
'============================================================================
private sub brc_fillinit()
	dim as integer hitem
	DeleteTreeViewItemAll(GTVIEWBRC)
	hitem=getfirstitemtreeview(GTVIEWVAR)
	listcpt=-1
	brc_fill(0,hitem)
End Sub
'====================================================
'' handles cond BP
'====================================================
private sub brc_choice()
	dim as integer cln=line_cursor() 'get line
	dim as integer rln=line_exec(cln,"Break point Not possible")
	dim as integer ibrk,typ
	For ibrk=1 To brknb 'search if still put on this line
		If brkol(ibrk).nline=cln And brkol(ibrk).isrc=srcdisplayed Then Exit For
	Next
	if ibrk<=brknb Then ''existing
		typ=brkol(ibrk).typ
		brk_del(ibrk)
		if typ=2 or typ=3 then
			exit sub
		EndIf
	end if
	brc_fillinit()
	varfind.ad=0
	setgadgettext(GBRCVAR1,"?")
	setgadgettext(GBRCVALUE,"0")
	SetItemComboBox(GBRKVCOND,0)
	hidewindow(hbpcondbx,KSHOW)
end sub
'====================================================
'' checks if variable selected for cond BP is ok
'====================================================
private sub brc_check()
	dim as INTEGER hitem=getitemtreeview(GTVIEWBRC),items
	for ilist as integer = 0 to listcpt
		if hitem=listitem(ilist).itemc then
			items=listitem(ilist).items
			for ivrr as INTEGER = 1 to vrrnb
				if vrr(ivrr).tv=items then
					if vrr(ivrr).vr=0 Then exit sub
					brkidx1=ivrr
					brkidx2=0
					var_fill(brkidx1)
					#Ifdef __FB_64BIT__
					If varfind.pt Then varfind.ty=9 ''pointer integer64 (longint)
					#Else
					If varfind.pt Then varfind.ty=1 ''pointer integer32 (long)
					#EndIf
					If varfind.ty>12 then
						messbox("Break on var selection error","Only [unsigned] Byte, Short, integer, longint, single, double")
						brkidx1=0
						brkidx2=0
						exit sub
					End If
					SetGadgetText(GBRCVAR1,varfind.nm)
				EndIf
			Next
			exit for
		EndIf
	Next
End Sub
'=======================================================================
'' t 1=permanent breakpoint / 2(var/const)-3(var-var)=conditionnal (on a line + condition) / 4=breakpoint with counter /
''   5=tempo / 6=disable-enable / 7=change value counter / 8 reset to initial value / 9=cursor line / over =10 / 11=end of proc / 12=end of prog
'=======================================================================
Private sub brk_set(t As Integer)
	Dim As Integer cln,rln,ibrk
	dim as string inputval

	cln=line_cursor() 'get line

select case t

	case 9 ''to cursor
		rln=line_exec(cln,"Run to cursor not possible, select an executable line")
		if rln=-1 then exit sub
		for ibrk as INTEGER	=1 to brknb
			if rline(rln).ad=brkol(ibrk).ad then
				messbox("Run to cursor","Impossible as a breakpoint is already set on this line")
				exit sub
			EndIf
		Next
		If linecur=cln And srcdisplayed=srccur Then
			If messbox("Run to cursor","Same line, continue ?",MB_YESNO)=IDNO Then Exit Sub
		End If
		if brkol(0).typ<>0 then ''cancel previous run to xxx (eg run to cursor stopped by a BPP then run to end of prog)
			brk_del(0)
		EndIf
		brkol(0).ad=rline(rln).ad
		brkol(0).typ=9
		brkol(0).index=rln
		brkol(0).isrc=srcdisplayed
		runtype=RTRUN
		but_enable()
		brkol(0).nline=cln
		brk_marker(0)
		brk_unset(true) ''remove ABP + keep UBP or disable them ?
		resume_exec()

	case 10 ''Skip current line / step over
		rln=rlinecur
		For j As Integer =1 To procnb
			If rline(rln).ad=proc(j).fn Then
				messbox("Skip line not possible","Last line of proc")
				Exit Sub
			end if
		Next
		if brkol(0).typ<>0 then
			brk_del(0)
		End If
		rln+=1
		brkol(0).ad=rline(rln).ad ''address of next line
		brkol(0).index=rln
		brkol(0).isrc=srcdisplayed
		brkol(0).typ=10
		runtype=RTRUN
		but_enable()
		brkol(0).nline=rline(rln).nu
		'print "skipping line=";rline(rln).nu,rline(rln).ad
		brk_marker(0)
		brk_unset(true) ''remove ABP + keep UBP or disable them ?
		resume_exec()

	case 11 '' run until end of proc  = EOP
		''todo add test if proc is disabled then messbox("End of proc","procedure disabled":exit sub
		if brkol(0).typ<>0 then
			brk_del(0)
		End If
		rln=rlinecur
		brkol(0).ad=proc(rline(rln).px).fn ''last executable line of proc
		For rln=1 To linenb
			If rline(rln).ad=brkol(0).ad Then Exit For ''find correponding line
		Next
		brkol(0).index=rln
		brkol(0).isrc=srcdisplayed
		brkol(0).typ=11
		runtype=RTRUN
		but_enable()
		brkol(0).nline=rline(rln).nu
		brk_marker(0)
		brk_unset(true) ''remove ABP + keep UBP or disable them ?
		resume_exec()

	case 12 '' run until exit of prog  = XOP
		if brkol(0).typ<>0 then
			brk_del(0)
		End If
		brkol(0).ad=proc(procmain).ed-1 ''BP on ret instruction but doesn't stop ????
		brkol(0).typ=12
		runtype=RTRUN
		but_enable()
		brk_unset(true) ''remove ABP + keep UBP
		resume_exec() ''prepare single step then resume

	case else
		rln=line_exec(cln,"Break point Not possible")
		if rln=-1 then exit sub

		For ibrk=1 To brknb 'search if still put on this line
			If brkol(ibrk).nline=cln And brkol(ibrk).isrc=srcdisplayed Then Exit For
		Next
		If ibrk>brknb Then 'not put
			If brknb=BRKMAX Then messbox("Max of brk reached ("+Str(BRKMAX)+")","Delete one and retry"):Exit Sub
			if t=6 then exit sub
			brknb+=1
			brkol(brknb).nline=cln
			brkol(brknb).typ=t
			brkol(brknb).index=rln
			brkol(brknb).isrc=srcdisplayed
			brkol(brknb).ad=rline(rln).ad
			brkol(brknb).cntrsav=0
			brkol(brknb).counter=0

			select case t
				Case 2
					brkol(brknb).adrvar1=brkadr1
					brkol(brknb).ivar1=brkidx1
					if brkdatatype=11 then
						brkol(brknb).val.vsingle=brkdata2.vsingle
					else
						'' integer (whole) or double number
						brkol(brknb).val.vlongint=brkdata2.vlongint
					end if
					brkol(brknb).ttb=brkttb
					brkol(brknb).datatype=brkdatatype
					brktyp=0
					modify_menu(MNSETBRKC,HMenusource,"Set/Clear [Ctrl + C]onditionnal Breakpoint")
					brkidx1=0
				case 3
					brkol(brknb).adrvar1=brkadr1
					brkol(brknb).ivar1=brkidx1
					brkol(brknb).adrvar2=brkadr2
					brkol(brknb).ivar2=brkidx2
					brkol(brknb).ttb=brkttb
					brkol(brknb).datatype=brkdatatype
					brktyp=0
					modify_menu(MNSETBRKC,HMenusource,"Set/Clear [Ctrl + C]onditionnal Breakpoint")
					brkidx1=0
				case 4 'define value counter
					inputval=input_bx("breakpoint with a counter","Set value counter for a breakpoint","0",7)
					brkol(brknb).counter=ValUInt(inputval)
					if brkol(brknb).counter=0 then
						messbox("Counter BP","Value = zero --> not created")
						brknb-=1
						exit sub
					EndIf
					brkol(brknb).cntrsav=brkol(brknb).counter
			End select
		Else 'still put
			If t=7 Then 'change value counter
				if  brkol(ibrk).cntrsav Then
					inputval=input_bx("Change value counter, remaining = "+Str(brkol(ibrk).counter)," initial = "+Str(brkol(ibrk).cntrsav),,7)
					brkol(ibrk).counter=ValUInt(inputval)
					if brkol(brknb).counter=0 then
						brkol(ibrk).counter=brkol(ibrk).cntrsav
						messbox("Change counter","Value = zero, enter another value or delete BP")
						exit sub
					EndIf
					brkol(ibrk).cntrsav=brkol(ibrk).counter
				else
					messbox("Change counter","No counter for this breakpoint")
				end if
			ElseIf t=8 Then 'reset to initial value
				If brkol(ibrk).cntrsav Then
					brkol(ibrk).counter=brkol(ibrk).cntrsav
				Else
					messbox("Reset counter","No counter for this breakpoint")
				EndIf
			ElseIf t=6 Then 'toggle enabled/disabled
				If brkol(ibrk).typ>50 Then
					brkol(ibrk).typ-=50
				Else
					brkol(ibrk).typ+=50
				EndIf
			ElseIf t=brkol(ibrk).typ OrElse (t<>1 and t<>6) then 'brkol(i).typ>1 Then 'cancel breakpoint
				brk_del(ibrk)
				Exit Sub
			Else 'change type of breakpoint to only permanent/temporary
				brkol(ibrk).typ=t
				brkol(ibrk).cntrsav=0
			End If
		End If

		brk_marker(ibrk)

	   If brknb=1 Then
			SetStateMenu(HMenusource,MNMNGBRK,0)
			DisableGadget(IDBUTBRKB,0)
	   EndIf
	'End If
	end select
End Sub
'========================================================
'' displays the breakpoint data
'========================================================
private sub brk_manage(title as string)
	dim as string text
	dim as integer srcprev=srcdisplayed,typ,cpt

	For ibrk as integer =1 To brknb
		source_change(brkol(ibrk).isrc)
		'text=line_text(brkol(ibrk).nline-1)

		text=" "+source_name(source(brkol(ibrk).isrc))+" ["+Str(brkol(ibrk).nline)+"]"
		if brkol(ibrk).typ=4 then
			text+=" cntr="+Str(brkol(ibrk).counter)+"/"+Str(brkol(ibrk).cntrsav)
			hidegadget(GBRKRST01+ibrk-1,KSHOW)
			hidegadget(GBRKCHG01+ibrk-1,KSHOW)
		else
			hidegadget(GBRKRST01+ibrk-1,KHIDE)
			hidegadget(GBRKCHG01+ibrk-1,KHIDE)
		end if
		text+=" >> "+Left(Trim(line_text(brkol(ibrk).nline-1),Any Chr(9)+" "),65)
		cpt+=1
		brkline(cpt)=brkol(ibrk).index
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
			Case 5
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
		hidegadget(GBRKRST01+ibrk-1,KHIDE)
		hidegadget(GBRKCHG01+ibrk-1,KHIDE)
	next
	SetWindowText(hbrkbx,strptr(title))
	hidewindow(hbrkbx,KSHOW)
end sub