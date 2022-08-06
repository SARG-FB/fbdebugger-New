''tools for fbdebugger_new
''dbg_tools.bas

'========================================
'' Insert in alphanumeric order (CAUTION 1 based)
'========================================
sub list_insert(list() as tlist,zstrg as zstring ptr, prnb as integer,byref listbegin as INTEGER)
		dim as integer listidx
		if prnb=1 then
			list(1).parent=-1
			list(1).child=-1
			list(1).nm=zstrg
			listbegin=1
		else
			listidx=listbegin
			while 1
				if *zstrg>*list(listidx).nm then
					if list(listidx).child=-1 then ''last element of the list
						''insert after last element
						list(listidx).child=prnb
						list(prnb).parent=listidx
						list(prnb).child=-1
						list(prnb).nm=zstrg
						exit while
					else
						listidx=list(listidx).child
					EndIf
				else
					if list(listidx).parent=-1 then ''first element of the list
						''insert before first element
						list(prnb).parent=-1
						list(prnb).child=listidx
						list(listidx).parent=prnb
						list(prnb).nm=zstrg
						listbegin=prnb
						exit while
					else ''insert in the middle
						list(prnb).parent=list(listidx).parent
						list(prnb).child=listidx
						list(list(listidx).parent).child=prnb
						list(listidx).parent=prnb
						list(prnb).nm=zstrg
						exit while
					EndIf
				EndIf
			wend
		EndIf
End Sub
'================================================================
''lists procedures and variables then puts in clipboard or log
'================================================================
private sub var_list(tlc as integer)
	dim as integer iparent,inext,temp,hitem,level
	dim as string text,textcb

	if tlc=1 Or tlc=3 Then
		hitem=GetItemTreeView(GTVIEWVAR)
		iparent=GetParentItemTreeView(GTVIEWVAR,hitem)
		inext=GetNextItemTreeView(GTVIEWVAR,hitem)
	else
		hitem=getfirstitemtreeview(GTVIEWVAR)
		iparent=0
		inext=0
	end if

	While hitem<>0
		text=GetTextTreeView(GTVIEWVAR,hitem)
		textcb+=Space(level*4)+left(text,50)+Chr(13)+Chr(10)
		temp=GetChildItemTreeView(GTVIEWVAR,hitem)
		level+=1
	    While temp=0
			temp=GetNextItemTreeView(GTVIEWVAR,hitem)
			If temp<>0 Then
				If inext=temp Then Exit While,While
				level-=1:Exit While
			EndIf
			hitem=GetParentItemTreeView(GTVIEWVAR,hitem)
			level-=1
			If hitem=iparent Then Exit While,While
		Wend
		hitem=temp
	Wend

	If tlc=2 OrElse tlc=3 Then
		setclipboardtext(textcb)
	else
		vlog+=textcb
		if logtyp=KLOGCONT then
			pasteeditor(GLOG,textcb)
		end if
	end if
End Sub
'===================================
'' show the value of a z/w/string
'===================================
private sub string_sh(tv as integer)
	static as byte buf(52004)
	dim as integer f,stringadr,inc
	dim as WString *52004 wstrg
	#Ifdef __FB_WIN32__
		dim As Short bufw
		#define KCHARSIZE 2
	#Else
		dim As long bufw
		#define KCHARSIZE 4
	#EndIf

	If var_find2(gadgetID(tv))=-1 Then Exit Sub 'search index variable under cursor

	If varfind.ty<>4 And varfind.ty<>13 And varfind.ty<>14 And varfind.ty <>6 Then 'or ty<>15
	   messbox("Show string error","Select only a string variable")
	   Exit Sub
	End If

	stringadr=varfind.ad
	If varfind.pt Then
	   ReadProcessMemory(dbghand,Cast(LPCVOID,stringadr),@stringadr,SizeOf(Integer),0) ''string ptr
	   If varfind.pt=2 Then ReadProcessMemory(dbghand,Cast(LPCVOID,stringadr),@stringadr,SizeOf(Integer),0) ''if two levels
	EndIf

	If varfind.ty <>6 Then
		''string zstring
		If varfind.ty=13 Then 'string
		   ReadProcessMemory(dbghand,Cast(LPCVOID,stringadr),@stringadr,SizeOf(Integer),0) ''string address
		End If

		inc=52000
		f=stringadr
		While inc<>0
			If ReadProcessMemory(dbghand,Cast(LPCVOID,f+inc),@buf(0),4,0) Then
				f+=inc
				Exit While
			Else
				inc\=2
			End If
		Wend
		ReadProcessMemory(dbghand,Cast(LPCVOID,stringadr),@buf(0),f-stringadr,0)
		buf(f-stringadr+1)=0 ''end of string if length >52000
		setgadgettext(GEDITOR,*cast(zstring ptr, @buf(0)))
	Else
		''wstring
      	inc=0:wstrg=""
      	ReadProcessMemory(dbghand,Cast(LPCVOID,stringadr),@bufw,KCHARSIZE,0)
		While bufw
			wstrg[inc]=bufw
			inc+=1
			If inc=52000 Then Exit While 'limit if wstring >52000
			ReadProcessMemory(dbghand,Cast(LPCVOID,stringadr+inc*2),@bufw,KCHARSIZE,0)
		Wend
		WStrg[inc]=0:WStrg[inc+1]=0:WStrg[inc+2]=0:WStrg[inc+3]=0 'end of wstring
		setgadgettext(GEDITOR,wstrg)
	EndIf
	setwindowtext(heditorbx,varfind.nm)
	hidewindow(heditorbx,KSHOW)
End Sub
'====================================
'' click on a cell in an array
'====================================
private sub index_cell()
	var iline=GetItemListView()
	var icol=GetSubItemListView()
	''checking out of bounds
	if icol<1 then exit sub
	if icol+indexdata.curidx(1)-1>indexdata.vubound(1) then exit sub
	if iline<0 then exit sub
	if iline+indexdata.curidx(0)>indexdata.vubound(0) then exit sub

	if FlagKeyListView=2 then ''CTRL
		vrr(indexdata.indexvar).ix(0)=iline+indexdata.curidx(0)
		vrr(indexdata.indexvar).ix(1)=icol+indexdata.curidx(1)-1
		var_sh()
		hidewindow(hindexbx,KHIDE)
	else
		''editing value
		var adr=indexdata.adr+iline*indexdata.sizeline+indexdata.size*(icol-1)
		edit_fill("Array element"+str(iline)+"/"+str(icol)+"="+GetTextItemListView(GIDXTABLE,iline,icol),adr,indexdata.typ,0,KEDITARR)
	EndIf
end sub
'==============================================
'' prepares a fully update of listview
'==============================================
private sub index_fullupdate()
	dim as integer delta,total,adr
	For k As Integer =0 To indexdata.nbdim-1
		vrr(indexdata.indexvar).ix(k)=getgadgetstate(GIDXUP1+k)
	Next

	delta=0:total=1
	For k As Integer = indexdata.nbdim-1 To 0 Step -1
		delta+=(vrr(indexdata.indexvar).ix(k)-indexdata.vlbound(k))*total
		total*=(indexdata.vubound(k)-indexdata.vlbound(k)+1)
	Next
	If (Cast(Integer,vrb(vrr(indexdata.indexvar).vr).arr)=-1 Andalso indexdata.typvar=true) OrElse (Cast(Integer,cudt(Abs(vrr(indexdata.indexvar).vr)).arr)=-1 AndAlso indexdata.typvar=false) Then 'dynamic array
		adr=vrr(indexdata.indexvar).ini+SizeOf(Integer)
		ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@adr,SizeOf(Integer),0) ''ptr instead data
		indexdata.adr+=delta*indexdata.size
	Else
		indexdata.adr=vrr(indexdata.indexvar).ini+delta*indexdata.size
	EndIf
	If indexdata.typ2<>0 Then indexdata.adr+=indexdata.delta2 'add the delta

	If indexdata.nbdim<=2 Then
		index_update()
	end if
end sub
'=============================================================================
'' applies changes in index selection and closes dialog box
'=============================================================================
private sub index_apply()
	For k As Integer =0 To 4 '' 5 maxi, done for all
		vrr(indexdata.indexvar).ix(k)=GetGadgetState(GIDXUP1+k)
	Next
	var_sh
	hidewindow(hindexbx,KHIDE)
end sub
'=============================================================================
'' updates the select index box
'=============================================================================
'index_update(listview,vrr(indexvar).ix(0),vubound(0),vrr(indexvar).ix(1),vubound(1),adr,typ,sizeline)
'Private sub index_update(listview As integer,idx As Long,limit As Long,idx2 As Long,limit2 As Long,adr As Integer,typ As Long,size As Long)
Private sub index_update()
	dim as INTEGER listview=GIDXTABLE,idx=indexdata.curidx(0),limit=indexdata.vubound(0)
	dim as integer idx2=indexdata.curidx(1),limit2=indexdata.vubound(1)
	dim as integer adr=indexdata.adr,typ=indexdata.typ,sizeline=indexdata.sizeline,size=indexdata.size
	Dim As String  txt
	Dim As Integer adrsav=adr,column,iline
	DeleteListViewItemsAll(listview)
	for icol as integer= KCOLMAX to 0 step -1
		DeleteListViewColumn(listview,icol)
	Next
	''column header (first line)
	AddListViewColumn(listview,"Index(es)",0,0,60)
	If indexdata.nbdim=1 Then ''one dim
		AddListViewColumn(listview,"value",1,1,495)
		idx2=1:limit2=1
	else
		For colindex As Long =idx2 To IIf(limit2-idx2>KCOLMAX-1,idx2+KCOLMAX-1,limit2) ''KCOLMAX columns max
			txt=Str(colindex)
			column=colindex-idx2+1
			AddListViewColumn(listview,txt,column,column,60)
		Next
	EndIf
		''todo if number column inchanged use  : SetTextColumnListView(GIDXTABLE,i,txt)

	''data
	for lineindex as integer = idx to iif(limit-idx>KLINEMAX-1,idx+KLINEMAX-1,limit) ''KLINEMAX lines max
		iline=lineindex-idx
		AddListViewItem(listview,str(lineindex),0,iline,0) ''displays first index

		adrsav=adr
		For colindex As Long =idx2 To IIf(limit2-idx2>KCOLMAX-1,idx2+KCOLMAX-1,limit2) ''30 columns max
			txt=var_sh2(typ,adr,0,"")
			txt=Mid(txt,InStr(txt,"=")+1) ''only data after "="
			column=colindex-idx2+1
			AddListViewItem(listview,txt,0,iline,column)
			adr+=size
		Next
		adr=adrsav+sizeline
	next

End Sub
'=====================================
'' fills shw / expand
'=====================================
private sub shwexp_fill(adr As Integer,typ As Integer,tv as integer,pt As Integer,iv As integer)
	Dim As String text
	Dim As Integer ad,temp
	Dim As integer tvchild
	Dim As Integer vrpnb,ivcnt=iv

	If pt>221 Or (pt>201 And pt<220) Or (pt And pt<200) Then
		If shwexp.nb=VRPMAX Then
			messbox("Creating data for show/expand","Too much lines")
			Exit Sub
		end if
		shwexp.nb+=1
		ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@ad,4,0)
		text="<"+var_sh2(typ,ad,pt,"")
		tvchild=AddTreeViewItem(GTVIEWSHW,text,cast (hicon, 0),0,TVI_LAST,tv)
		vrpnb=shwexp.nb
		vrp(vrpnb).ty=typ
		vrp(vrpnb).pt=pt
		vrp(vrpnb).ad=ad
		vrp(vrpnb).tl=tvchild
		ivcnt+=1
		vrp(vrpnb).iv=ivcnt
		shwexp_fill(ad,typ,tvchild,pt-1,ivcnt)
		vrpnb=shwexp.nb 'counter updated in the executed proc
	Else
		If typ>TYPESTD And udt(typ).en=0 Then
		   For i As Integer = udt(typ).lb To udt(typ).ub
			If shwexp.nb=VRPMAX Then
				messbox("Creating data for show/expand","Too much lines")
				Exit Sub
			end if
		   	shwexp.nb+=1
			  With cudt(i)
				text=.nm+" "
				If .arr Then
				   If Cast(Integer,.arr)=-1 Then
				      text+=" [Dynamic array can't display value]"
				      ad=0
				   Else
				    	text+=" [ "
					   For j As Integer =0 To .arr->dm-1
				 	   text+=Str(.arr->nlu(j).lb)+"-"+Str(.arr->nlu(j).ub)+" "
					   Next
					   text+="] "
					   ad=adr+.ofs
				   End If
				Else
				   ad=adr+.ofs
				End If
				If .typ=TYPEMAX Then 'bitfield
					text+="<BITF"+var_sh2(2,ad,.pt,Str(.ofs)+" / ")
					temp=ValInt(Right(text,1)) 'byte value
					temp=temp Shr .ofb        'shifts to get the concerned bit on the right
					temp=temp And ((2*.lg)-1) 'clear other bits
					Mid(text,Len(text)) =Str(temp) 'exchange byte value by bit value
					Mid(text,InStr(text,"<BITF")+5)="IELD"  'exchange 'byte' by IELD
				Else
					text+="<"+var_sh2(.typ,ad,.pt,Str(.ofs)+" / ")
				End If
				tvchild=AddTreeViewItem(GTVIEWSHW,text,cast (hicon, 0),0,TVI_LAST,tv)
				vrpnb=shwexp.nb
				vrp(vrpnb).nm=.nm
				vrp(vrpnb).ty=.typ
				vrp(vrpnb).pt=.pt
				vrp(vrpnb).ad=ad
				vrp(vrpnb).tl=tvchild
				ivcnt+=1
				vrp(vrpnb).iv=iv
				If .pt>221 Or (.pt>201 And .pt<220) Or (.pt And .pt<200) Or .pt=0 Then
					temp=.pt
			   	If .pt>0 Then ReadProcessMemory(dbghand,Cast(LPCVOID,ad),@ad,4,0):temp-=1
				   If .pt>200 Or (.pt And .typ<=TYPESTD)  Or (.pt And .typ>TYPESTD And udt(.typ).en) Or (.pt=0 And .typ>TYPESTD And udt(.typ).en=0 And .typ<>TYPEMAX) Then
			   		shwexp_fill(ad,.typ,tvchild,temp,ivcnt)
			   		vrpnb=shwexp.nb 'counter updated in the executed proc
			   	EndIf
			   End If
			  End With
		   Next
		Else
		   If shwexp.nb=VRPMAX Then
				messbox("Creating data for show/expand","To much lines")
				Exit Sub
			end if
			shwexp.nb+=1
			text="<"+var_sh2(typ,adr,0,"")
			vrpnb=shwexp.nb
			vrp(vrpnb).ty=typ
			vrp(vrpnb).pt=0
			vrp(vrpnb).ad=adr
			vrp(vrpnb).tl=AddTreeViewItem(GTVIEWSHW,text,cast (hicon, 0),0,TVI_LAST,tv)
			ivcnt+=1
			vrp(vrpnb).iv=iv
		End If
	End If
End Sub
'=================================================
'' checks if a dynamic array has changed
''return -1 address changed / -2 only dimensions
'=================================================
function shwexp_checkarr() as integer
	dim as integer adr,dm,idxcur,idxmin,idxmax
	if shwexp.arradr then
		adr=vrr(shwexp.parent).ini+sizeof(integer) ''vrr(parent).ini MUST be not null meaning th array is initialized
		ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@adr,SizeOf(Integer),0)
		if shwexp.arradr<>adr then
			''save information
			varfind.nm=vrp(1).nm
			varfind.ty=vrp(1).ty
			varfind.pt=vrp(1).pt
			varfind.ad=vrr(vrp(1).iv).ad
			varfind.pr=shwexp.parent
			messbox("Redim has been executed","Updating all by a new window")
			shwexp_init()
			return -1
		else
			''no change of address but may be a smaller redim in this case the data is not move
			with shwexp
				idxcur=.curidx
				idxmin=.minidx
				idxmax=.maxidx
				adr=vrr(.parent).ini+4*SizeOf(Integer) 'nb dim
				readProcessMemory(dbghand,Cast(LPCVOID,adr),@dm,SizeOf(Integer),0)
				#ifdef KNEWARRAYFIELD
					adr+=SizeOf(Integer) ''skip flag field
				#endif
				'retrieve bounds for last dimension
				adr+=SizeOf(Integer)*(dm*3-1)
				ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@.minidx,SizeOf(Integer),0)
				adr+=SizeOf(Integer)
				readProcessMemory(dbghand,Cast(LPCVOID,adr),@.maxidx,SizeOf(Integer),0)
				.curidx=iif(.curidx<.minidx,.minidx,iif(.curidx>.maxidx,.maxidx,.curidx))''current index inbetween the bounds
				if .curidx<>idxcur or .minidx<>idxmin or .maxidx<>idxmax then
					vrp(1).ad+=(.curidx-idxcur)*udt(vrp(1).ty).lg ''new address calculated with the delta between the indexes
					setgadgettext(GSHWCUR,"Index cur : "+Str(.curidx))
					setgadgettext(GSHWMIN,"Index min : "+Str(.minidx))
					setgadgettext(GSHWMAX,"Index max : "+Str(.maxidx))
					messbox("Redim has been executed","Updating min/max indexes and possibly current")
					return -2
				endif
			end with
		end if
	end if
end function
''===========================================
'' initialises a new show / expand
''===========================================
private sub shwexp_update(nm As String="",ad As Integer=0,ty As Integer=0,pt As integer=0,iv As integer=0)
	Dim As Integer vrpnb,temp,htv,adr
	dim as string text

	DeleteTreeViewItemAll(GTVIEWSHW)

	vrpnb=1
	shwexp.nb=vrpnb
	if ad=0 then ''not a new one only address changed
		nm=vrp(vrpnb).nm
		vrp(vrpnb).nm=nm
		ty=vrp(vrpnb).ty
		pt=vrp(vrpnb).pt
		ad=vrp(vrpnb).ad
		iv=vrp(vrpnb).iv
	else
		vrp(vrpnb).nm=nm
		vrp(vrpnb).ty=ty
		vrp(vrpnb).pt=pt
		vrp(vrpnb).ad=ad
		vrp(vrpnb).iv=iv
	end if


	text="--> "+nm+" <"+var_sh2(ty,ad,pt,"")
	htv=AddTreeViewItem(GTVIEWSHW,text,cast (hicon, 0),0,TVI_FIRST,0)
	vrp(vrpnb).tl=htv

	adr=ad
	If pt>221 Or (pt>201 And pt<220) Or (pt And pt<200) Or (ty>TYPESTD And udt(ty).en=0) Then
		If pt Then temp=pt-1:ReadProcessMemory(dbghand,Cast(LPCVOID,ad),@adr,4,0)
		shwexp_fill(adr,ty,htv,temp,iv)
		ExpandTreeViewItem(GTVIEWSHW,htv,1) ''expand included child items
	EndIf
end sub
'==================================================
'' new show / expand variable or memory
'==================================================
private sub shwexp_new(gtview as integer)

	dim as integer hitem,temp,typ,pt,rvadr,adr,addr

	If shwexp.free=false Then ''shw/exp in use
		if messbox("Show/Expand already in use","Do you want to change it ?",MB_YESNO)=IDNO then
			exit sub
		end if
	end if

	If gtview=GTVIEWVAR Then 'if not called from tviewvar not usefull to search return value
		'get current hitem in tree
		hitem=GetItemTreeView(GTVIEWVAR)
		'search procr index
		For i As Integer =1 To procrnb
			If procr(i).tv=hitem Then
				temp=procr(i).idx
				addr=procr(i).sk
				rvadr=proc(procr(i).idx).rvadr
			 Exit For
			EndIf
		Next
	EndIf
	If temp<>0 Then
		typ=proc(temp).rv
		pt=proc(temp).pt
		dbg_prt2("show ret="+proc(temp).nm+" "+Str(typ)+" "+Str(pt))
		if typ=7 AndAlso pt=0 then
			messbox("Return value","Select a function not a sub !!")
			Exit Sub
		end if
		If pt Then 'pointer : type doesn't matter
			addr-=4 ''todo check 8 ???
		Else
			If typ=13 Then 'string
				addr-=12
			Else 'other type without pointer
				If typ>TYPESTD Then ''udt
					'typ=7
					addr+=8
					pt=1
				Else
					If udt(proc(temp).rv).lg>4 Then
						addr-=8
					Else
						addr-=4
					EndIf
				EndIf
			EndIf
		EndIf
		'fill data to simulate var_find
		varfind.nm="Proc="+proc(temp).nm+" Return value address="+Str(addr)
		varfind.ty=proc(temp).rv
		varfind.ad=addr
		varfind.pt=pt
	ElseIf gtview<>0 Then ''not coming from dump
		If var_find2(GadgetID(gtview))=-1 Then 'found the variable ? no if -1
			Exit Sub
		end if
	EndIf

	shwexp_init()
	shwexp.free=false
	setwindowtext(hshwexpbx,"Show/expand : "+varfind.nm)
End Sub
'==============================================
'' fill the show / expand box
'==============================================
private sub shwexp_init()
	dim as integer iv,temp,dm,adr
	shwexp.arradr=0 ''by default not a dynamic array

	iv=varfind.iv
	''handling array  var or field / fixed or dynamic
	if iv>0 then
		'type var
		If Cast(Integer,vrb(vrr(iv).vr).arr)=-1 Then
			'dynamic array
			adr=vrr(iv).ini+SizeOf(Integer)
			readprocessmemory(dbghand,Cast(LPCVOID,adr),@adr,SizeOf(Integer),0)
			shwexp.arradr=adr
			if adr Then
				adr=vrr(iv).ini+4*SizeOf(Integer) 'nb dim
				readProcessMemory(dbghand,Cast(LPCVOID,adr),@dm,SizeOf(Integer),0)
				#ifdef KNEWARRAYFIELD
					adr+=SizeOf(Integer) ''skip flag field
				#endif
				'retrieve bounds for last dimension
				adr+=SizeOf(Integer)*(dm*3-1)
				ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@shwexp.minidx,SizeOf(Integer),0)
				adr+=SizeOf(Integer)
				readProcessMemory(dbghand,Cast(LPCVOID,adr),@shwexp.maxidx,SizeOf(Integer),0)
			end If
		ElseIf vrb(vrr(iv).vr).arr then
			'fixed lenght array
			dm=vrb(vrr(iv).vr).arr->dm
			shwexp.minidx=vrb(vrr(iv).vr).arr->nlu(dm-1).lb
			shwexp.maxidx=vrb(vrr(iv).vr).arr->nlu(dm-1).ub
		End If

	elseif iv<>0 then
		'type cudt
		iv=Abs(iv)
		with cudt(Abs(vrr(iv).vr))
			'dynamic array
			if Cast(Integer,.arr)=-1 Then
				temp=getparentitemtreeview(GTVIEWVAR,vrr(iv).tv)
				For k As Integer=1 To vrrnb
					If vrr(k).tv=temp Then temp=k:Exit For
				Next
				adr=vrr(temp).ad+vrr(iv).ini+SizeOf(Integer)
				ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@adr,4,0) '' ptr instead data, added for dyn array in udt
				If adr Then
					adr=vrr(iv).ini+4*SizeOf(integer) 'nb dim
					ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@dm,4,0)
					#ifdef KNEWARRAYFIELD
						adr+=SizeOf(Integer) ''skip flag field
					#endif
					adr+=SizeOf(Integer)*(dm*3-1)
					ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@shwexp.minidx,SizeOf(Integer),0)
					adr+=SizeOf(Integer)
					readProcessMemory(dbghand,Cast(LPCVOID,adr),@shwexp.maxidx,SizeOf(Integer),0)
				End If
			Elseif .arr then
				'fixed lenght array
				dm=.arr->dm
				shwexp.minidx=.arr->nlu(dm-1).lb
				shwexp.maxidx=.arr->nlu(dm-1).ub
			endif
		end With
	endif

	if iv<>0 then

		if dm then
			shwexp.curidx=vrr(iv).ix(dm-1)
			setgadgettext(GSHWCUR,"Index cur : "+Str(shwexp.curidx))
			setgadgettext(GSHWMIN,"Index min : "+str(shwexp.minidx))
			setgadgettext(GSHWMAX,"Index max : "+str(shwexp.maxidx))
			hidegadget(GSHWCUR,KSHOW)
			hidegadget(GSHWMIN,KSHOW)
			hidegadget(GSHWMAX,KSHOW)
			hidegadget(GSHWDEC,KSHOW)
			hidegadget(GSHWINC,KSHOW)
			hidegadget(GSHWSET,KSHOW)
		else
			hidegadget(GSHWCUR,KHIDE)
			hidegadget(GSHWMIN,KHIDE)
			hidegadget(GSHWMAX,KHIDE)
			hidegadget(GSHWDEC,KHIDE)
			hidegadget(GSHWINC,KHIDE)
			hidegadget(GSHWSET,KHIDE)
		end if

		for iprocr As UInteger = 1  To procrnb 'find running proc
			If iv>=procr(iprocr).vr And iv<procr(iprocr+1).vr then
				shwexp.procr=iprocr
				exit for
			end if
		next
		shwexp.mem=vrb(varfind.pr).mem ''if static will not be closed when procr is closed
		shwexp.parent=varfind.pr

		'dbg_prt("vrb="+vrb(varfind.pr).nm+" "+str(vrb(varfind.pr).mem)+" "+str(shwexp.arradr))
	else
		shwexp.procr=0 ''as proc=0 coming from dump and never closed
		shwexp.arradr=0
	endif

	shwexp_update(varfind.nm,varfind.ad,varfind.ty,varfind.pt,varfind.iv)
	hidewindow(hshwexpbx,KSHOW)
end sub

'=====================================
'' checks and updates value for edt
'=====================================
private sub edit_update()

  dim As Integer vflag,p2,typtemp
  Dim As valeurs edt
  dim As Double vald
  dim as string txt


	txt=GetGadgetText(GEDTVALUE)
	vald=Val(txt)
	vflag=1

	If edit.pt Then
	   typtemp=7
	Else
	   typtemp=edit.typ
	EndIf

	Select Case As Const typtemp
		Case 2
			If vald<-128 Or vald>127 Then
				setwindowtext(heditbx,"min -128,max 127"):vflag=0
			else
				edt.vbyte=ValInt(txt) :p2=1
			end if
		Case 3
			If vald<0 Or vald>255 Then
				setwindowtext(heditbx,"min 0,max 255"):vflag=0
			else
				edt.vubyte=ValUInt(txt) :p2=1
			end if
		Case 5
			If vald<-32768 Or vald>32767 Then
				setwindowtext(heditbx,"min -32768,max 32767"):vflag=0
			else
				edt.vshort=ValInt(txt) :p2=2
			end if
		Case 6
			If vald<0 Or vald>65535 Then
				setwindowtext(heditbx,"min 0,max 65535"):vflag=0
			else
				edt.vushort=ValUInt(txt) :p2=2
			end if
		Case 1
			If vald<-2147483648 Or vald>2147483648 then
				setwindowtext(heditbx,"min -2147483648,max +2147483647"):vflag=0
			else
				edt.vinteger=ValInt(txt) :p2=4
			end if
		Case 7
			#Ifdef __FB_64BIT__
				If Vald<0 Or vald>18446744073709551615 Then
					setwindowtext(heditbx,"min 0,max 18446744073709551615"):vflag=0
				else
					edt.vulongint=ValULng(txt) :p2=8
				end if
			#Else
				If vald<0 Or vald>4294967395 Then
					setwindowtext(heditbx,"min 0,max 4294967395"):vflag=0
				else
					edt.vuinteger=ValUInt(txt) :p2=4
				end if
			#EndIf
		Case 8
			If vald<0 Or vald>4294967395 Then
				setwindowtext(heditbx,"min 0,max 4294967395"):vflag=0
			else
				edt.vuinteger=ValUInt(txt) :p2=4
			end if
		Case 9
			If Vald<-9223372036854775808  Or vald>9223372036854775807 Then
				setwindowtext(heditbx,"min -9223372036854775808,max 9223372036854775807"):vflag=0
			else
				edt.vlongint=ValLng(txt) :p2=8
			end if
		Case 10
			If Vald<0 Or vald>18446744073709551615 Then
				setwindowtext(heditbx,"min 0,max 18446744073709551615"):vflag=0
			else
				edt.vulongint=ValULng(txt) :p2=8
			end if
		Case 16
			If vald>1 or vald<-1 Then
				SetWindowText(heditbx,"Authorized value only 0 or 1"):vflag=0
			else
				edt.vbyte=ValInt(txt) :p2=1
			end if
		Case 11
			edt.vsingle=Val(txt) :p2=4
		Case 12
			edt.vdouble=Val(txt) :p2=8
		Case Else
			#Ifdef __FB_64BIT__
				edt.vulongint=ValULng(txt) :p2=8
			#Else
				edt.vuinteger=ValUInt(txt) :p2=4
			#EndIf
	End Select

	if vflag then
		hidewindow(heditbx,KHIDE)
		writeprocessmemory(dbghand,Cast(LPVOID,edit.adr),@edt,p2,0)
		var_sh()

		if edit.src=KEDITARR then
			var iline=GetItemListView()
			var icol=GetSubItemListView()
			SetTextItemListView(GIDXTABLE,iline,icol,getgadgettext(GEDTVALUE))
		endif

		if edit.src=KEDITDMP then
			var iline=GetItemListView()
			var icol=GetSubItemListView()
			SetTextItemListView(GDUMPMEM,iline,icol,getgadgettext(GEDTVALUE))
		else
			dump_sh()
		endif

		if edit.src=KEDITTOP then
			var iline=GetItemListView()
			var icol=GetSubItemListView()
			SetTextItemListView(GDUMPMEM,0,1,getgadgettext(GEDTVALUE))
		else
			dump_sh()
		endif

		''todo update watched ?
	end if
end sub
'==============================================
'' updates settings when settings box is closed
'==============================================
private sub settings_update()
	cmdexe(0)=GetGadgetText(GCMDLPARAM)
	autostep=valint(GetGadgetText(GAUTODELAY))
	if autostep<50 then
		autostep=50
		SetGadgetText(GAUTODELAY,str(autostep))
		messbox("Delay for autostepping","Too small, reset to "+str(autostep))
	elseIf autostep>10000 then
		autostep=10000
		SetGadgetText(GAUTODELAY,str(autostep))
		messbox("Delay for autostepping","Too big, reset to "+str(autostep))
	end if
end sub
'===================
'' lists enums
'===================
private sub enum_list()
	dim as string text
	For i As Integer = 1 To udtmax
		If udt(i).en=i Then ' to avoid duplicate
			text=udt(i).nm+chr(13)
			For j As Integer = udt(i).lb To udt(i).ub
				text+=cudt(j).nm+"="+str(cudt(j).val)+chr(13)
			Next
		End If
	Next
End Sub
'=================================================
'' lists dll
'=================================================
private sub dll_list()
	dim as string text
	For i As Integer=1 To dllnb
		text+=dlldata(i).fnm
		If dlldata(i).hdl=0 Then text+="  Currently not used"
		text+=Chr(13)+Chr(10)
	Next
	SetGadgetText(GEDITOR,text)
	hidewindow(heditorbx,KSHOW)
end sub

'================================================================
'' finds thread index based on cursor or threadid
'================================================================
private function thread_select(id As Integer =0) As Integer
	Dim text As string, pr As Integer, thid As Integer
	Dim As Integer hitem,temp

	If id =0 Then  'take on cursor
	'get current hitem in tree
		temp=GetItemTreeView(GTVIEWTHD)
		Do 'search thread item
			hitem=temp
			temp=getParentItemTreeview(GTVIEWTHD,hitem)
		Loop While temp

		text=GetTextTreeView(GTVIEWTHD,hitem)
		thid=ValInt(Mid(text,13,6))
	Else
		thid=id
	End If
	For p As Integer =0 To threadnb
		If thid=thread(p).id Then Return p 'find index number
	Next
End Function
'============================================================
'' shows the next executed line=1 or threadcreate line=2
'============================================================
private sub thread_execline(s As Integer,thid As Integer=0)
 Dim As Integer thidx,thline
   thidx=thread_select(thid)
	If s=1 Then
		thline=thread(thidx).sv
	Else
		If thidx=0 Then
			messbox("Threadcreate line","Main so no such line !!")
			Exit Sub
		EndIf
		If thread(thidx).st=0 Then
			messbox("Threadcreate line","Impossible to locate in case of fast run !!")
			Exit Sub
		EndIf
		thline=thread(thidx).st
	End If
	source_change(rline(thline).sx)  ''display source
	line_display(rline(thline).nu) ''Select Line
End Sub
'===========================================
'' kills a thread
'===========================================
private sub thread_kill()
 Dim t As Integer
   t=thread_select()
	If t=0 Then
		messbox("Killing thread","This is the first thread --> process"+Chr(10)+"Use kill process button")
		Exit Sub
	EndIf
	If messbox("Killing thread : "+Str(thread(t).id),"Are you sure ?"+Chr(10)+"It could cause Memory leak, etc.",MB_YESNO)=IDYES Then
		#ifdef __fb_win32__
			If terminatethread(thread(t).hd,999)=0 Then
				messbox("Thread killing","Something goes wrong, error: "+Str(GetLastError))
			EndIf
		#else
			messbox("Feature not coded","thread_kill for linux, maybe make a sub")
		#endif
	EndIf
End Sub
'==============================================================================
'' displays the call chain for the current thread or for a selected one
'==============================================================================
private sub call_chain(thid as integer)
	Static As Integer procrsav(PROCRMAX),iline,first,prevpr,vproc
	dim as string txt
	dim as integer srcprev=srcdisplayed ''save now for restoring at the end of the table filling
	DeleteListViewItemsAll(GCCHAIN)
	txt="Calling line               [ThID="+ Str(thid)+"]"
	SetTextColumnListView(GCCHAIN,0,txt)
	for iprocr as integer = procrnb to 1 step -1
		If procr(iprocr).thid=thid Then
			procrsav(iline)=iprocr ''to make easier the location
			if first=0 then
				first=1
				AddListViewItem(GCCHAIN,proc(procr(iprocr).idx).nm,0,0,0)

				thread_execline(1,thid)
				txt=space(31)
				Send_sci(SCI_GETCURLINE,30,strptr(txt))
				AddListViewItem(GCCHAIN,txt,0,0,1)

				txt=Right("______"+Str(linecur),6)
				AddListViewItem(GCCHAIN,txt,0,0,2)
				AddListViewItem(GCCHAIN,source(srccur),0,0,3)
				prevpr=iprocr
			else
				iline+=1
				vproc=procr(iprocr).idx
				AddListViewItem(GCCHAIN,proc(vproc).nm,0,iline,0)

				source_change(proc(vproc).sr)
				txt=line_text(rline(procr(iprocr).cl).nu-1) ''calling line
				AddListViewItem(GCCHAIN,txt,0,iline,1)

				txt=Right("______"+Str(rline(procr(prevpr).cl).nu),6)
				AddListViewItem(GCCHAIN,txt,0,iline,2)

				txt=source_name(source(proc(vproc).sr))
				AddListViewItem(GCCHAIN,txt,0,iline,3)
				prevpr=iprocr
			end if
		end if
	next
	cchainthid=thid
	source_change(srcprev)
	hidewindow(hcchainbx,KSHOW)
end sub
'===================================================================================
'' check if the line is executable return -1 if false otherwise the rline index
'===================================================================================
private function line_exec(pline as integer,msg as string)as integer
	For iline as integer =1 To linenb
		If rline(iline).nu=pline AndAlso rline(iline).sx=srcdisplayed Then
			For jproc As Integer =1 To procnb
				If rline(iline).ad=proc(jproc).db Then
					messbox(msg,"Line not executable")
					return -1
				EndIf
			Next
			return iline
		end if
	next
	messbox(msg,"Line not executable")
	return -1
end function
'=======================================================
'' shows address in memory of line where is the cursor
'========================================================
private sub line_adr()
Dim As Integer l, rl

l=line_cursor()
rl=line_exec(l,"No address line")

if rl=-1 Then
	Exit Sub
EndIf

For j As Integer =0 To procnb
	If rline(rl).ad=proc(j).db Then
		messbox("Line memory address","Not executable so no address")
		Exit Sub
	EndIf
Next
messbox("Line memory address","Adr = "+Str(rline(rl).ad)+" / &h "+Hex(rline(rl).ad))
End Sub
'=======================================================
'' displays the input box and check value
'=======================================================
private function input_bx(title as string,text1 as string,text2 as string="",inputtyp as INTEGER) as string
	dim as boolean vflag=true
	dim as double vald
	dim as string inputval
	do
		inputval=inputbox(title,text1,text2)
		if inputtyp=99 then
			vald=Val(inputval)
			Select Case inputtyp
				Case 2
					If vald<-128 Or vald>127 Then text1="min -128,max 127":vflag=false
				Case 3
					If vald<0 Or vald>255 Then text1="min 0,max 255":vflag=false
				Case 5
					If vald<-32768 Or vald>32767 Then text1="min -32768,max 32767":vflag=false
				Case 6
					If vald<0 Or vald>65535 Then text1="min 0,max 65535":vflag=false
				Case 1
					If vald<-2147483648 Or vald>2147483648 Then text1="min -2147483648,max +2147483647":vflag=false
				Case 7,8
					If vald<0 Or vald>4294967395 Then text1="min 0,max 4294967395":vflag=false
			End Select
		end if
	loop until vflag=true
	return inputval
end function
'===============================
'' shows value in dec/hex/bin
'===============================
private sub dechexbin()
	var inputval=input_bx("Display value in dec/hex/bin","Input value HEX("+KAMPERSAND+"h) or DEC",,99)
	messbox("Value in dec, hex and bin","Dec= "+Str(Val(inputval))+Chr(10)+"Hex="+Hex(Val(inputval))+Chr(10)+"Bin="+Bin(Val(inputval)))
End Sub
'===============================
''Goto selected line number
'===============================
private sub line_goto()
	Dim linegoto As Integer
	var inputval=input_bx("Goto line","Enter a value between 1 to "+str(send_sci(SCI_GETLINECOUNT,0,0)),,99)
	Linegoto=ValInt(inputval)
	If linenb>=0 Then
	   line_display(linegoto,1)
	End If
End Sub
'============================================
'' splits a string in parts of 2 characters
'============================================
private function split_hex(strg as string)as string
	dim as string temp=left(strg,2)
	if len(strg)=2 then return temp
	for i as integer=1 to len(strg)\2-1
		temp+=" "+mid(strg,i*2+1,2)
	Next
	return temp
End Function
'==========================================
'' extracts all dump and put in clipboard
'==========================================
private sub dump_extract()
	Dim As String dumpall
	dim as integer lgt

	Select Case dumptyp+dumpbase
		Case 2,3
			lgt=4
		Case 5,6 'short/dec/sng
			lgt=6
		Case 1,7,8 'integer/dec/usng
			lgt=11
		Case 9,10 'longinteger/dec/usng
			lgt=21
		Case 11 'single
			lgt=13
		Case 12 'double
			lgt=22
	End Select

	For jline as integer =0 To dumplines-1
		dumpall+=fmt3(GetTextItemListView(GDUMPMEM,jline,0),20)+"  "
		For icol as integer =1 To dumpnbcol
			if dumpbase=0 then
				dumpall+=fmt3(GetTextItemListView(GDUMPMEM,jline,icol),lgt)+" "
			else
				dumpall+=GetTextItemListView(GDUMPMEM,jline,icol)+" "
			end if
		Next
		dumpall+="  "+GetTextItemListView(GDUMPMEM,jline,dumpnbcol+1)+chr(10) ''ascii
	Next
	SetClipBoardText(dumpall)
End Sub
'======================================
'' changes the sign for some datatypes
'======================================
private sub dump_signed()
	Select Case dumptyp
		Case 2
			dumptyp=3
		Case 3
			dumptyp=2
		Case 5
			dumptyp=6
		Case 6
			dumptyp=5
		Case 1
			dumptyp=8
		Case 8
			dumptyp=1
		Case 9
			dumptyp=10
		Case 10
			dumptyp=9
		Case else
			messbox("Changing sign","Not allowed for this datatype")
			exit sub
	end select
	setgadgettext(GDUMPTYPE,"Current type="+udt(dumptyp).nm)
	dump_sh()
End Sub
'===============================================================
'' changes the used base (dec/hex) for the displayed addresses
'===============================================================
private sub dump_baseadr()
	dim as integer ad=dumpadr
	dumpadrbase=1-dumpadrbase
	For jline as integer =0 To dumplines-1
		if dumpadrbase=1 then
			SetTextItemListView(GDUMPMEM,jline,0,str(ad))
		else
			SetTextItemListView(GDUMPMEM,jline,0,hex(ad))
		end if
		ad+=16
	Next
end sub
'======================================
'' changes the first address or edit cell
'======================================
private sub dump_cell()
	if dumpnbcol=0 then exit sub
	var iline=GetItemListView()
	var icol=GetSubItemListView()
	var flag=FlagKeyListView
	if iline>=0 and icol>0 then
		if flag=2 then ''CTRL
			dumpadr+=16*iline+(16\dumpnbcol)*(icol-1)
			dump_sh()
			SetGadgetText(GDUMPADR,str(dumpadr))
		else
			''editing value
			var adr=dumpadr+16*iline+(16\dumpnbcol)*(icol-1)
			edit_fill("Memory "+str(adr)+"="+GetTextItemListView(GDUMPMEM,iline,icol),adr,dumptyp,0,KEDITDMP)
			exit sub
		end if
	end if
	hidewindow(hdumpbx,KSHOW)
End Sub
'======================================
'' changes the first address
'======================================
private sub dump_change()
	if GetItemListView()>0 and GetSubItemListView()>0 then
		dumpadr+=16*GetItemListView()+(16\dumpnbcol)*(GetSubItemListView()-1)
		dump_sh()
		SetGadgetText(GDUMPADR,str(dumpadr))
	end if
	hidewindow(hdumpbx,KSHOW)
end sub
'======================================
'' edits the top/left cell
'======================================
private sub dump_edit()
	edit_fill("Memory "+str(dumpadr)+"="+str(GetTextItemListView(GDUMPMEM,0,1)),dumpadr,dumptyp,0,KEDITTOP)
end sub
'======================================
'' displays updated dumpmem
'======================================
private sub dump_sh()
	Dim As String tmp
	Dim buf(16) As UByte,r As Integer,ad As Integer
	Dim ascii As String
	Dim ptrs As pointeurs
	if dumpnbcol=0 then exit sub
	DeleteListViewItemsAll(GDUMPMEM) ''delete all items
	ad=dumpadr
	For jline as integer =0 To dumplines-1
		if dumpadrbase=1 then
			AddListViewItem(GDUMPMEM,str(ad),0,jline,0) ''adress
		else
			AddListViewItem(GDUMPMEM,hex(ad),0,jline,0) ''adress
		end if
		ReadProcessMemory(dbghand,Cast(LPCVOID,ad),@buf(0),16,@r)
		#ifdef __fb_win32__
			ad+=r
		#else
			ad+=16
		#endif
		ptrs.pxxx=@buf(0)
		For icol as integer =1 To dumpnbcol
		  Select Case dumptyp+dumpbase
			 Case 2,16,66 'byte/dec/sng - boolean hex or dec
				tmp=Str(*ptrs.pbyte)
				ptrs.pbyte+=1
			 Case 3 'byte/dec/usng
				tmp=Str(*ptrs.pubyte)
				ptrs.pubyte+=1
			 Case 5 'short/dec/sng
				tmp=Str(*ptrs.pshort)
				ptrs.pshort+=1
			 Case 6 'short/dec/usng
				tmp=Str(*ptrs.pushort)
				ptrs.pushort+=1
			 Case 1 'integer/dec/sng
				tmp=Str(*ptrs.pinteger)
				ptrs.pinteger+=1
			 Case 7,8 'integer/dec/usng
				tmp=Str(*ptrs.puinteger)
				ptrs.puinteger+=1
			 Case 9 'longinteger/dec/sng
				tmp=Str(*ptrs.plongint)
				ptrs.plongint+=1
			 Case 10 'longinteger/dec/usng
				tmp=Str(*ptrs.pulongint)
				ptrs.pulongint+=1
			 Case 11 'single
				tmp=Str(*ptrs.psingle)
				ptrs.psingle+=1
			 Case 12 'double
				tmp=Str(*ptrs.pdouble)
				ptrs.pdouble+=1
			 Case 52,53 'byte/hex
				tmp=split_hex(Right("0"+Hex(*ptrs.pbyte),2))
				ptrs.pbyte+=1
			 Case 55,56 'short/hex
				tmp=split_hex(Right("000"+Hex(*ptrs.pshort),4))
				ptrs.pshort+=1
			 Case 51,58,61 'integer/hex
				tmp=split_hex(Right("0000000"+Hex(*ptrs.pinteger),8))
				ptrs.pinteger+=1
			Case 59,60,62 'longinteger/hex
				tmp=split_hex(Right("000000000000000"+Hex(*ptrs.plongint),16))
				ptrs.pulongint+=1
		  End Select
		  AddListViewItem(GDUMPMEM,tmp,0,jline,icol)
		Next
		ascii=""
		#ifdef __FB_LINUX__
			For ibuf as integer=1 To 16
				ascii+=Chr(buf(ibuf-1))
			Next

			if g_utf8_validate(strptr(ascii),-1,0)<>true then
				for ibuf as integer=0 to 15
					if ascii[ibuf]<32 or ascii[ibuf]>126 then
						ascii[ibuf]=asc(".")
					EndIf
				Next
			EndIf
		#else
			For ibuf as integer=1 To 16
				If buf(ibuf-1)<32 orelse (flagascii=1 and buf(ibuf-1)>126) Then
					ascii+="."
				Else
					ascii+=Chr(buf(ibuf-1))
				End If
			Next
		#endif
		AddListViewItem(GDUMPMEM,ascii,0,jline,dumpnbcol+1)
	Next
End Sub
'==========================================
'' finds the calling line for proc
'==========================================
private function line_call(adr As Integer) As Integer
	For irline As Integer=1 To linenb
		If adr<=rLine(irline).ad Then
			if adr=rLine(irline).ad then
				Return irline-1
			else
				return irline
			end if
		EndIf
	Next
	Return linenb
end Function
'=====================================================================
'in string STRG all the occurences of SRCH are replaced by REPL
'=====================================================================
private sub str_replace(strg As String,srch As String, repl As String)
	Dim As Integer p,lgr=Len(repl),lgs=Len(srch)
	p=InStr(strg,srch)
	While p
		strg=Left(strg,p-1)+repl+Mid(strg,p+lgs)
		p=InStr(p+lgr,strg,srch)
	Wend
End Sub
'=========================================================================
'' something wrong happens so close fbdebugger after displaying a message
'==========================================================================
private sub hard_closing(errormsg as string)
	messbox("Need to close fbdebugger",_
			  "Sorry an unrecoverable problem occurs :"+chr(13)+errormsg+chr(13)+chr(13)+"Report to dev please")
	freegadgets()
	close_window(hmain)
	end
end sub
'==========================================
'' dumps variable memory
'==========================================
private sub var_dump(tv As HWND,ptd As Long =0)

	If var_find2(tv)=-1 Then Exit Sub 'search index variable under cursor

	dumpadr=varfind.ad

	If ptd Then 'dumping pointed data
		If varfind.pt=0 Then messbox("Dumping pointed data","The selected variable is not a pointer"):Exit Sub
		ReadProcessMemory(dbghand,Cast(LPCVOID,dumpadr),@dumpadr,SizeOf(Integer),0)
	EndIf

	If udt(varfind.ty).en Then
	   dumptyp=1 'if enum then as integer
	Else
	   dumptyp=varfind.ty
	End If
	If varfind.pt Then
	   dumptyp=8
	Else
	   Select Case dumptyp
		Case 13 'string
			 dumptyp=2 'default for string
			 ReadProcessMemory(dbghand,Cast(LPCVOID,dumpadr),@dumpadr,SizeOf(Integer),0) ''string address
		Case 4,14 'f or zstring
			dumptyp=2
		Case Is>TYPESTD
			 dumptyp=8 'default for pudt and any
	   End Select
	End If

	dump_set()
	dump_sh()
	PanelGadgetSetCursel(GRIGHTTABS,TABIDXDMP)
End Sub
'==========================================================
private function var_parent(child As integer) As Integer 'find var master parent
	Dim As integer temp,temp2,hitemp
	temp=child
	Do
		hitemp=temp2
		temp2=temp
		temp=GetParentItemTreeview(GTVIEWVAR,temp)
	Loop While temp
For i As Integer =1 To vrrnb
	If vrr(i).tv=hitemp Then Return i
Next
End Function

'==========================================================
'' returns the variable index under the cursor in tviewvar
'==========================================================
private function var_find() As Integer 'return NULL if error
	Dim hitem As Integer
	'get current hitem in tree
	hitem=GetItemTreeView(GTVIEWVAR)
	For i As Integer = 1 To vrrnb 'search index variable
		If vrr(i).tv=hitem Then
			If vrr(i).ad=0 Then messbox("Variable selection error","Dynamic array not yet sized !!"):Return 0
			If vrr(i).vr<0 Then
				Return -i
			Else
				Return i
			EndIf
		End If
	Next
	messbox("Variable selection error","          Select only a variable")
	Return 0
End Function
'===========================================================================
private sub var_fill(i As Integer)
	If vrr(i).vr<0 Then
		varfind.ty=cudt(-vrr(i).vr).Typ
		varfind.pt=cudt(-vrr(i).vr).pt
		varfind.nm=cudt(-vrr(i).vr).nm
		varfind.pr=vrr(var_parent(vrr(i).tv)).vr'index of the vrb
	Else
		varfind.ty=vrb(vrr(i).vr).Typ
		varfind.pt=vrb(vrr(i).vr).pt
		varfind.nm=vrb(vrr(i).vr).nm
		varfind.pr=vrr(i).vr 'no parent so himself, index of the vrb
	End If
	varfind.ad=vrr(i).ad
	varfind.iv=i
	varfind.tv=htviewvar '
	varfind.tl=vrr(i).tv 'handle line
End Sub
'======================================================
private function Val_string(strg As String)As String
	Dim strg2 As String,vl As Integer
	For i As Integer=1 To Len(strg) Step 2
		vl=ValInt("&h"+Mid(strg,i,2))
		If vl>31 Then
			strg2+=Chr(vl)
		Else
			strg2+="."
		EndIf
	Next
	Return strg2
End Function
'====================================================
private function val_byte(strg As String)As String
	Dim strg2 As String,vl As Integer
	For i As Integer=1 To Len(strg) Step 2
		vl=ValInt("&h"+Mid(strg,i,2))
		strg2+=Str(vl)+","
	Next
	Return Left(strg2,Len(strg2)-1)
End Function
'======================================================
private function val_word(strg As String)As String
	Dim strg2 As String,vl As Integer
	For i As Integer=1 To Len(strg) Step 4
		vl=ValInt("&h"+Mid(strg,i,4))
		strg2+=Str(vl)+","
	Next
	Return Left(strg2,Len(strg2)-1)
End Function
'=========================================================================
private function var_add(strg As String,t As Integer,d As Integer)As String
	Dim As Integer ValueInt
	Dim As LongInt valuelng

	Select Case As Const t
		Case 2,3 'byte,ubyte
			Valueint=ValInt(strg)
			Select Case  As Const d
				Case 1'hex
					Return Hex(valueint,2)
				Case 2'binary
					Return Bin(valueint,8)
				Case 3'ascii
					Return Val_string(Hex(valueint,2))
			End Select
		Case 5,6 'Short,ushort
			valueint=ValInt(strg)
			Select Case  As Const d
				Case 1'hex
					Return Hex(valueint,4)
				Case 2'binary
					Return Bin(valueint,16)
				Case 3'ascii
					Return Val_string(Hex(valueint,4))
				Case 4'byte
					Return Val_byte(Hex(valueint,4))
			End Select
		Case 1,7,8 'integer,void,uinteger
			valueint=ValInt(strg)
			Select Case  As Const d
				Case 1'hex
					Return Hex(valueint)
				Case 2'binary
					Return Bin(valueint)
				Case 3'ascii
					Return Val_string(Hex(valueint))
				Case 4'byte
					Return Val_byte(Hex(valueint))
				Case 5'word
					Return Val_word(Hex(valueint))
			End Select
		Case 9,10 'longint,ulongint
			valuelng=ValInt(strg)
			Select Case  As Const d
				Case 1'hex
					Return Hex(valuelng)
				Case 2'binary
					Return Bin(valuelng)
				Case 3'ascii
					Return Val_string(Hex(valuelng))
				Case 4'byte
					Return Val_byte(Hex(valuelng))
				Case 5'word
					Return Val_word(Hex(valuelng))
			End Select
	End Select
End Function
'======================================================
private sub var_iniudt(Vrbe As UInteger,adr As UInteger,tv As integer,voffset As UInteger,mem As UByte)'store udt components ''scope added
	Dim ad As UInteger,text As String,vadr As UInteger
	For i As Integer =udt(Vrbe).lb To udt(vrbe).ub
		vadr=adr
		With cudt(i)
			'dbg_prt2("var ini="+.nm+" "+Str(.ofs)+" "+Str(voffset)+" "+Str(adr))
		 vrrnb+=1:vrr(vrrnb).vr=-i
		 ad=.ofs+voffset 'offset of current element + offset all levels above
		 vrr(vrrnb).gofs=ad 'however keep (global) offset

			If adr=0 Then 'dyn array not defined
			 vrr(vrrnb).ad=0 'element in dyn array not defined  so also for the field
			Else
			vrr(vrrnb).ad=adr+ad 'real address
			vrr(vrrnb).ini=adr+ad 'used when changing index
			End If
			If .arr Then
				If Cast(Integer,.arr)<>-1 Then
					For k As Integer =0 To 4 'set index by puting ubound
						vrr(vrrnb).ix(k)=.arr->nlu(k).lb
					Next
				Else
					ad=0 'next sub field (if any) offsets are defined from this level
	 				vadr=0
					vrr(vrrnb).ini=0 'when starting again without leaving fbdebugger '19/05/2014
					If vrr(vrrnb).ad<>0 Then
						vrr(vrrnb).ini=vrr(vrrnb).ad
		 				'dbg_prt2("reset for dyn arr cudt="+Str(vrr(vrrnb).ini+4))
		 				If mem<>4 Then
   	  				   WriteProcessMemory(dbghand,Cast(LPVOID,vrr(vrrnb).ini+SizeOf(Integer)),@ad,SizeOf(Integer),0) 'reset area ptr 25/07/2015 64bit
   	  				EndIf
					EndIf
	 				vrr(vrrnb).ad=0
				EndIf
			End If
			'dbg_prt2("variniudt final ="+.nm+" "+Str(vrr(vrrnb).ad)+" "+Str(vrr(vrrnb).gofs)+" "+Str(.ofs)+" "+Str(vrr(vrrnb).ini)+" "+Str(1))
		vrr(vrrnb).tv=AddTreeViewItem(GTVIEWVAR,"Not yet filled",cast (hicon, 0),0,TVI_LAST,tv)
		If .pt=0 AndAlso .typ>TYPESTD AndAlso .typ<>TYPEMAX  AndAlso udt(.typ).en=0 Then 'show components for bitfield
			var_iniudt(.typ,vadr,vrr(vrrnb).tv,ad,mem) 'scope added
		End If
		End With
	Next
End Sub
'==============================================
'fill treeview and real address
private sub var_ini(j As UInteger ,bg As Integer ,ed As Integer) 'store information For master var
	Dim adr As UInteger
		For i As Integer = bg To ed
			With vrb(i)
			If .mem<>2 AndAlso .mem<>3 AndAlso .mem<>6 Then
					adr=.adr+procr(j).sk 'real adr
			Else
					adr=.adr
			EndIf
				If vrrnb=VRRMAX Then hard_closing("Too many running variables/fields, limit="+str(VRRMAX))''never go back here
				vrrnb+=1:vrr(vrrnb).vr=i
				vrr(vrrnb).ad=adr
				If .arr Then
					vrr(vrrnb).ini=adr 'keep adr for [0] or structure for dyn
					'dynamic array not yet known so initialise address with null
					If Cast(Integer,.arr)=-1 Then
						vrr(vrrnb).ad=0
						If .mem <>4 Then adr=0:WriteProcessMemory(dbghand,Cast(LPVOID,vrr(vrrnb).ini+SizeOf(Integer)),@adr,SizeOf(Integer),0)
					Else
							For k As Integer =0 To 4 'clear index puting lbound
								vrr(vrrnb).ix(k)=.arr->nlu(k).lb
							Next
					End If
				EndIf
				If .mem =4 Then 'modif for byref only real address
					vrr(vrrnb).ini=adr
					ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@adr,SizeOf(Integer),0) '64bit
					If Cast(Integer,.arr)=-1 Then vrr(vrrnb).ini=adr Else vrr(vrrnb).ad=adr
				End If
				vrr(vrrnb).tv=AddTreeViewItem(GTVIEWVAR,"Not filled",cast (hicon, 0),0,TVI_LAST,procr(j).tv)
				If .pt=0 And .typ>TYPESTD AndAlso .typ<>TYPEMAX AndAlso udt(.typ).en=0 Then 'show components for bitfield 20/08/2015
					var_iniudt(.typ,adr,vrr(vrrnb).tv,0,.mem) 'scope
				End If
			End With
		Next
End Sub
'====================================================
private function var_search(pproc As Integer,text() As String,vnb As Integer,varr As Integer,vpnt As Integer=0) As Integer
	Dim As Integer begv=procr(pproc).vr,endv=procr(pproc+1).vr,tvar=1,flagvar
'dbg_prt2("searching="+text(1)+"__"+text(2)+"***"+Str(vnb))'18/01/2015
	flagvar=TRUE 'either only a var either var then its components
	While begv<endv And tvar<=vnb 'inside the local vars and all the elements (see parsing)
		If flagvar Then
			If vrr(begv).vr>0 Then 'var ok
				If vrb(vrr(begv).vr).nm=text(tvar) Then 'name ok
					'testing array or not
						flagvar=0 'only one time
						If tvar=vnb Then
  	                  		If (varr=1 AndAlso vrb(vrr(begv).vr).arr<>0 ) OrElse (varr=0 And vrb(vrr(begv).vr).arr=0) Then
								Return begv 'main level
  	                  		EndIf

						EndIf
						tvar+=1 'next element, a component
				EndIf
			EndIf
		Else
			'component level
			If vrr(begv).vr<0 Then
				If cudt(Abs(vrr(begv).vr)).nm=text(tvar) Then
				  If tvar=vnb Then
						If (varr=1 AndAlso cudt(Abs(vrr(begv).vr)).arr<>0 ) OrElse (varr=0 And cudt(Abs(vrr(begv).vr)).arr=0) Then
							Return begv'happy found !!!
						EndIf
				  EndIf
				  tvar+=1 'next element
				End If
			Else
				Exit While 'not found inside the UDT
			EndIf
		End If
		begv+=1 'next running  var or component
	Wend
	Return -1
End Function
'===============================================================
private function watch_find() As Integer
	Dim hitem As Integer
	'get current hitem in tree
	hitem=GetItemTreeView(GTVIEWWCH)
	For k As Integer =0 To WTCHMAX
		If wtch(k).tvl=hitem Then Return k 'found
	Next
End Function
'==================================================================================
private function var_find2(tv As HWND) As Integer 'return -1 if error
	Dim hitem As integer,idx As Integer
	If tv=htviewvar Then
		'get current hitem in tree
		hitem=GetItemTreeView(GTVIEWVAR)
		For i As Integer = 1 To vrrnb 'search index variable
			If vrr(i).tv=hitem Then
				If vrr(i).ad=0 Then messbox("Variable selection error","Dynamic array not yet sized !!"):Return -1
				var_fill(i)
				Return i
			End If
		Next
		messbox("Variable selection error2","         Select only a variable")
		Return -1
	ElseIf tv=htviewwch Then
		idx=watch_find()
		If wtch(idx).psk=-3 OrElse wtch(idx).psk=-4 Then Return -1 'case non-existent local
		If wtch(idx).adr=0 Then Return -1 'dyn array
		varfind.nm=Left(wtch(idx).lbl,Len(wtch(idx).lbl)-1)
	   varfind.ty=wtch(idx).typ
	   varfind.pt=wtch(idx).pnt
	   varfind.ad=wtch(idx).adr
	   varfind.tv=htviewwch 'handle treeview
	   varfind.tl=wtch(idx).tvl 'handle line
	   varfind.iv=wtch(idx).ivr
	Else'shw/expand tree
		'get current hitem in tree
		hitem=GetItemTreeView(ID_In_Number(tv))
		For i As Integer = 1 To shwexp.nb 'search index variable
			If vrp(i).tl=hitem Then
				varfind.nm=vrp(i).nm
				If varfind.nm="" Then varfind.nm="<Memory>"
				varfind.ty=vrp(i).Ty
				varfind.pt=vrp(i).pt
				varfind.ad=vrp(i).ad
				varfind.tv=tv 'handle treeview
				varfind.tl=hitem 'handle line
				varfind.iv=-1
				Return i
			End If
		Next
	End If
End Function

'=======================================
'' lists threads
'=======================================
private sub thread_list()
	Dim As Integer thid,p
	dim as string text
	For i As Integer =0 To threadnb
		thid=thread(i).id
		text+="ID="+fmt2(Str(thid),4)+"/"+fmt2(Hex(thid),4)+" HD="+fmt2(Str(thread(i).hd),4)+"/"+fmt2(Hex(thread(i).hd),3)+" : "
		If thread(i).sv<>-1 Then 'thread debugged
			p=proc_find(thid,KLAST)
			text+=proc(procr(p).idx).nm
			If threadhs=thread(i).hd Then text+="(next execution)"
		Else
			text+="(not debugged, hidden)"
		End If
		text+=Chr(13)
	Next
	SetGadgetText(GEDITOR,text)
	hidewindow(heditorbx,KSHOW)
end sub
'===========================================
'' restore instruction and resume thread
'===========================================
Private sub thread_resume()
	#ifdef __fb_win32__
		''todo move to dbg_windows.bas
		'print "resume begin";str(rLine(thread(threadcur).sv).ad),"value="+str(rLine(thread(threadcur).sv).sv)
		writeprocessmemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@rLine(thread(threadcur).sv).sv,1,0) 'restore old value for execution
		resumethread(threadhs)
	#else
		''LINUX, maybe moved in dbg_linux.bas
		thread_rsm()
	#endif
End sub

'=====================================================
'' finds the thread in GTVIEWTHD
'=====================================================
private function thread_find() as integer
	Dim As Integer hitem,temp
	''get current hitem in tree
	temp=GetItemTreeView(GTVIEWTHD)
	Do ''search index thread
		hitem=temp
		temp=getParentItemTreeview(GTVIEWTHD,hitem)
	Loop While temp
	return hitem
End function
'=====================================================
'=====================================================
private sub thread_procloc(t As Integer)
	Dim As Integer hitem,temp,cpt,thid,pr
	Dim As String text
	''get current hitem in tree
	temp=GetItemTreeView(GTVIEWTHD)
	Do ''search index thread
		hitem=temp
		temp=getParentItemTreeview(GTVIEWTHD,hitem)
		cpt+=1
	Loop While temp
	If cpt>1 Then  ''thread item and first proc have same index in procr
		cpt-=1
	Else
		cpt=1
	EndIf

	GetTextTreeView(GTVIEWTHD,hitem)
	thid=ValInt(Mid(text,10,6))

	For pr =1 To procrnb ''finding proc index
		If procr(pr).thid=thid Then
			cpt-=1
			If cpt=0 Then Exit For
		EndIf
	Next

	If t=1 Then ''proc in proc/var
		PanelGadgetSetCursel(GRIGHTTABS,TABIDXVAR)
		SetSelectTreeViewItem(GTVIEWVAR,procr(pr).tv)
	ElseIf t=2 Then'proc in source
		source_change(proc(procr(pr).idx).sr) ''display source
		line_display(proc(procr(pr).idx).nu-1) ''Select Line
	Else 'info about running proc
		messbox("Proc : "+proc(procr(pr).idx).nm,"Start address ="+Str(proc(procr(pr).idx).db)+"/&h"+Hex(proc(procr(pr).idx).db)+Chr(10)_
												   +"End   address ="+Str(proc(procr(pr).idx).fn)+"/&h"+Hex(proc(procr(pr).idx).fn)+Chr(10)_
												   +"Stack address ="+Str(procr(pr).sk)+"/&h"+Hex(procr(pr).sk))
	End If
End Sub
'===========================================
'' updates text of thread(s)
'===========================================
private sub thread_text(th As Integer=-1)
	Dim libel As String
	Dim As Integer thid,p,lo,hi
	If th=-1 Then
		lo=0:hi=threadnb
	Else
		lo=th:hi=th
	EndIf

	For ith As Integer=lo To hi
		if thread(ith).tv=0 then
			continue for ''no need to update
		end if
		thid=thread(ith).id
		p=proc_find(thid,KLAST)
		select case thread(ith).sts
			case KTHD_RUN
				libel="R> "
			case KTHD_STOP
				libel="S> "
			case KTHD_BLKD
				libel="B> "
			case KTHD_OUT
				libel="O> "
			Case else
				libel="?> "
		End select
		libel+="threadID="+fmt2(Str(thid),6)+" : "+proc(procr(p).idx).nm

		#ifdef __FB_WIN32__
			If flagverbose Then
				libel+=" HD: "+Str(thread(ith).hd)
			EndIf
			'If threadhs=thread(ith).hd Then
				'libel+=" (next execution)"
			'EndIf
		#else
			'If threadcur=ith Then
				'libel+=" (next execution)"
			'EndIf
		#endif
		SetTextItemTreeView(GTVIEWTHD,thread(ith).tv,libel)
	Next
End Sub
'=======================================================================================
private sub thread_change(th As Integer =-1)
	Dim As Integer t,s

	if runtype<>RTSTEP then
		messbox("Changing thread","Not possible as current thread is running"+chr(10)+"If the thread is waiting (sleep, input, etc) try to quit this state")
		exit sub
	EndIf

	If th=-1 Then
		t=thread_select()
	Else
		t=th
	EndIf

	s=threadcur
	'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@breakcpu,1,0) 'restore CC previous line current thread
	threadcur=t
	'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@rLine(thread(threadcur).sv).sv,1,0) 'restore old value for execution selected thread
	threadhs=thread(threadcur).hd
	procsv=rline(thread(threadcur).sv).px
	threadsel=threadcur
	thread_text(t)
	thread_text(s)
	dsp_change(thread(threadcur).sv)
End Sub
'================================================
'' blocks/unblocks the selected thread
'================================================
private sub thread_block()
	var th=thread_select()
	if thread(th).sts=KTHD_BLKD then
		thread(th).sts=KTHD_STOP
	elseif thread(th).sts=KTHD_STOP then
		thread(th).sts=KTHD_BLKD
	else
		messbox("Trying to block thread","Only if stopped")
	EndIf
	thread_text(th)
End Sub
'====================================
'' returns index using thread().id
'====================================
private function thread_index(tid as long) as INTEGER
	For ith As Integer =0 To threadnb
		If tid=thread(ith).id Then
			return ith
		EndIf
	next
end function
'===================================================
private function enum_find(t As Integer,v As Integer) As String
	'find the text associated with an enum value
	For i As Integer =udt(t).lb To udt(t).ub
		If cudt(i).val=v Then  Return cudt(i).nm
	Next
	Return "Unknown Enum value"
End Function

'======================================================
private function var_sh2(t As Integer,pany As UInteger,p As UByte=0,sOffset As String="") As String
	Dim adr As UInteger,varlib As String
	Union lpointers
	#Ifdef __FB_64BIT__
	   pinteger As Long Ptr
	   puinteger As ULong Ptr
	#Else
	   pinteger As Integer Ptr
	   puinteger As UInteger Ptr
	#EndIf
	pbyte As Byte Ptr
	pubyte As UByte Ptr
	pzstring As ZString Ptr
	pshort As Short Ptr
	pushort As UShort Ptr
	pvoid as integer Ptr
	pLongint As LongInt Ptr
	puLongint As ULongInt Ptr
	psingle As Single Ptr
	pdouble As Double Ptr
	pstring As String Ptr
	pfstring As ZString Ptr
	pany As Any Ptr
	End Union
	Dim Ptrs As lpointers,recup(71) As Byte
	ptrs.pany=@recup(0)
	If p Then
	 	If p>220 Then
			varlib=String(p-220,"*")+" Function>"
		ElseIf p>200 Then
			varlib=String(p-200,"*")+" Sub>"
	 	Else
	 		varlib=String(p,"*")+" "+udt(t).nm+">"
		End If

		If flagverbose Then varlib+="[sz"+Str(SizeOf(Integer))+" / "+sOffset+Str(pany)+"]"
		If pany Then
			ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),sizeof(Integer),0)
	 		If p>200 Then
	 			varlib+="="+proc_name(*ptrs.puinteger) 'proc name
	 		Else
				varlib+="="+Str(*ptrs.puinteger) 'just the value
	 		EndIf
		Else
			varlib+=" No valid value"
		End If
	Else
		varlib=udt(t).nm+">"
		If flagverbose Then varlib+="[sz "+Str(udt(t).lg)+" / "+sOffset+hex(pany)+"]"
		If pany Then
		 If t>0 And t<=TYPESTD Then
		 	varlib+="="
				Select Case t
				   Case 1 'integer32/long
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),4,0)
						varlib+=Str(*ptrs.pinteger)
					Case 2 'byte
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),1,0)
						varlib+=Str(*ptrs.pbyte)
					Case 3 'ubyte
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),1,0)
						varlib+=Str(*ptrs.pubyte)
					Case 4,13,14 'stringSSSS
						If t=13 Then  ' normal string
		   				ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@adr,SizeOf(Integer),0) 'address
						Else
							adr=pany
						End If
		   			Clear recup(0),0,71 'max 70 char
		   			ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@recup(0),70,0) 'value
					   varlib+=*ptrs.pzstring
					Case 5 'short
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),2,0)
						varlib+=Str(*ptrs.pshort)
					Case 6 'ushort
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),2,0)
						varlib+=Str(*ptrs.pushort)
				   Case 7 'void
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),SizeOf(Integer),0)
						varlib+=Str(*ptrs.pvoid)
				   Case 8 'uinteger/ulong
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),4,0)
						varlib+=Str(*ptrs.puinteger)
				   Case 9 'longint/integer64
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),8,0)
						varlib+=Str(*ptrs.plongint)
				   Case 10 'ulongint/uinteger64
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),8,0)
						varlib+=Str(*ptrs.pulongint)
					Case 11 'single
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),4,0)
						varlib+=Str(*ptrs.psingle)
					Case 12 'double
						ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),8,0)
						varlib+=Str(*ptrs.pdouble)
				   Case 16 'boolean
							ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),1,0)
						''varlib+=Cast(boolean,*ptrs.pbyte)
						varlib+=IIf(*ptrs.pbyte,"True","False")
					'Case Else
						'Return "Unmanaged Type>"
				End Select
		 Else
		 	If udt(t).en Then
		 		If pany Then ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),sizeof(integer),0)
		 		varlib+="="+Str(*ptrs.pinteger)+" >> "+enum_find(t,*ptrs.pinteger) 'value/enum text
		 	EndIf
		 End If
	  Else
		 varlib+=" No valid value"
	  End If
	End If
	Return varlib
End Function
'==============================================
'' prepares dump for string/fstring/zstring
'==============================================
private sub dump_string(adr as integer,typ as integer)
	dumpadr=adr
	dumptyp=2
	if typ=13 then ''string
		ReadProcessMemory(dbghand,Cast(LPCVOID,dumpadr),@dumpadr,SizeOf(Integer),0)'string address 27/07/2015
	end if
	dump_set()
	dump_sh()
	PanelGadgetSetCursel(GRIGHTTABS,TABIDXDMP)
end sub
'=====================================================================
'' manages editing of var/mem etc see KEDITVAR/CELL/DMP/SHW/PTD/WCH
'=====================================================================
private sub edit_fill(txt as string,adr as integer,typ as integer, pt as integer,src as integer=0)
	dim as integer p2,aptr

	If (typ=4 Or typ=13 Or typ=14 Or typ=15) And pt=0 Then
	   messbox("Edit variable error","Select only a numeric variable"+Chr(13)+"For string use change with dump")
	   dump_string(adr,typ)
	   hidewindow(heditbx,KHIDE)
	   exit sub
	End If

	If typ>TYPESTD And pt=0 And udt(typ).en=0 Then
	   messbox("Edit variable error","Select only a numeric variable")
	   hidewindow(heditbx,KHIDE)
	   Exit sub
	End If

	edit.src=src ''used later in edit_update
	setgadgettext(GEDTVAR,txt)
	txt=Mid(txt,InStr(txt,"=")+1,25)
	If typ=16 Then 'boolean
		txt=IIf(txt="False","0","1")
	EndIf
	setgadgettext(GEDTVALUE,txt)

	If pt Then 'pointer
		If pt>220 Then
			p2=pt-220
		ElseIf pt>200 Then
			p2=pt-200
		Else
			p2=pt
		End If
		aptr=adr
		For j As Integer = 1 To p2 'only the last pointer is displayed
			ReadProcessMemory(dbghand,Cast(LPCVOID,aptr),@aptr,SizeOf(Integer),0)
			setgadgettext(GEDTPTD,Str(aptr))
		next
		If aptr Then ''if null address do nothing
			If pt>200 Then
				proc_name(adr)''procedure name
			Else
				txt=var_sh2(typ,aptr)
				txt=Mid(txt,InStr(txt,"=")+1,90)
				If typ=16 Then 'boolean
					txt=iif(txt="False","0","1")
				EndIf
				setgadgettext(GEDTPTDVAL,txt) ''pointed value
			End If
			If (typ<>4 And typ<>13 And typ<>14 And typ<>15 And pt<200) Or  udt(typ).en Then
				hidegadget(GEDTPTDEDT,KSHOW)
				edit.ptdadr=aptr
				edit.ptdval=txt
			End If
		Else
			setgadgettext(GEDTPTDVAL,"Null address, Nothing to display")
		End If
		hidegadget(GEDTPTD,KSHOW)
		hidegadget(GEDTPTDVAL,KSHOW)
	else
		hidegadget(GEDTPTD,KHIDE)
		hidegadget(GEDTPTDEDT,KHIDE)
		hidegadget(GEDTPTDVAL,KHIDE)
	End If
	hidewindow(heditbx,KSHOW)
	edit.adr=adr
	edit.typ=typ
	edit.pt =pt
end sub
'==============================================================================
''to propagate address dynamaic array or after changing index or erase
'==============================================================================
Private sub update_address(midx As Long)
	Dim As Long child
	Dim As Integer arradr=vrr(midx).ad

	child=GetChilditemTreeview(GTVIEWVAR,vrr(midx).tv) ''first child (at least one as it's an udt)
	For i As Long = midx+1 To vrrnb
		If vrr(i).tv=child Then ''only done with direct child not with the childs of a child
			If arradr=0 Then ''case array erased
				vrr(i).ini=0
				vrr(i).ad=0
			Else
				''add the offset from its close parent
				vrr(i).ad=arradr+cudt(Abs(vrr(i).vr)).ofs
				'dbg_prt("updating address="+cudt(Abs(vrr(i).vr)).nm+" "+str(vrr(i).ad)+" "+str(cudt(Abs(vrr(i).vr)).ofs))
				If Cast(Integer,cudt(Abs(vrr(i).vr)).arr) Then
					vrr(i).ini=vrr(i).ad
					''vrr(i).ad=0
				EndIf
			EndIf
		child=GetChildItemTreeview(GTVIEWVAR,child)
		If child=0 Then Exit For ''no more child
		EndIf
	next
End Sub
'================================================================================
private function var_sh1(i As Integer) As String
	Dim adr As Integer,text As String,soffset As String
	Dim As Integer temp1,temp2,temp3,vlbound(4),vubound(4)
	Dim As tarr Ptr arradr
	Dim As Long vflong,udtlg,nbdim,typ

	If vrr(i).vr<0 Then ''field
		text=cudt(Abs(vrr(i).vr)).nm+" "
		arradr=cudt(Abs(vrr(i).vr)).arr
		udtlg=udt(cudt(Abs(vrr(i).vr)).typ).lg
		if Cast(Integer,cudt(Abs(vrr(i).vr)).arr)>0 Then nbdim=cudt(Abs(vrr(i).vr)).arr->dm-1 ''only used in case of fixed-lenght array
		typ=cudt(Abs(vrr(i).vr)).typ
	Else
		text=vrb(vrr(i).vr).nm+" "
		arradr=vrb(vrr(i).vr).arr
		udtlg=udt(vrb(vrr(i).vr).typ).lg
		If Cast(Integer,vrb(vrr(i).vr).arr)>0 Then nbdim=vrb(vrr(i).vr).arr->dm-1 ''only used in case of fixed-lenght array
		typ=vrb(vrr(i).vr).typ
	endif

	if arradr Then ''fixed lenght or dynamic array
		if Cast(Integer,arradr)=-1 Then ''dynamic
			If vrr(i).ini Then 'initialized so it's possible to get data, otherwise adr=0 only usefull for cudt array, var always true
				adr=vrr(i).ini+SizeOf(Integer)  'ptr not data
				ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@adr,SizeOf(Integer),0)
			EndIf
			'print "vrr ini=";vrr(i).ini,adr,arradr
			If adr Then 'sized ?
				text+="[ Dyn "
				temp2=vrr(i).ini+4*SizeOf(Integer) ''adr nb dim
				ReadProcessMemory(dbghand,Cast(LPCVOID,temp2),@temp3,SizeOf(Integer),0)
				#ifdef KNEWARRAYFIELD
					temp2+=SizeOf(Integer) ''skip flag field
				#endif
			   	temp3-=1
				For k As Integer =0 To temp3
		  			If vrr(i).arrid then ''array tracked ?
		   			''Read the value need to be done here as the value could not be retrieved too late after the display of the array
		   				If trckarr(k).memadr<>0 Then
							Dim As String libel=var_sh2(trckarr(k).typ,trckarr(k).memadr,0,"")
							vflong=ValInt(Mid(libel,InStr(libel,"=")+1))
				   			vrr(i).ix(k)=vflong
		   				endif
		  			End If

					temp2+=2*SizeOf(Integer) ' 'lbound
					ReadProcessMemory(dbghand,Cast(LPCVOID,temp2),@temp1,SizeOf(Integer),0)
					If vrr(i).ad=0 Then 'init index lbound
						print "i k temp3 temp2 temp1=";i,k,temp3,temp2,temp1
						vrr(i).ix(k)=temp1
					Else
						If vrr(i).ix(k)<temp1 Then vrr(i).ix(k)=temp1 'index can't be <lbound
					End If
					text+=Str(temp1)+"-"
					vlbound(k)=temp1

					temp2+=SizeOf(Integer)' 'ubound
					ReadProcessMemory(dbghand,Cast(LPCVOID,temp2),@temp1,SizeOf(Integer),0)
					If vrr(i).ix(k)>temp1 Then vrr(i).ix(k)=temp1 'index can't be >ubound
					text+=Str(temp1)+":"+Str(vrr(i).ix(k))+" "
					vubound(k)=temp1

				Next
				text+="]"
				''calculate the new adress for the array value depending on the new indexes, LIMIT 5 DIMS
				temp1=0:temp2=1
				For k As Integer = temp3 To 0 Step -1
					temp1+=(vrr(i).ix(k)-vlbound(k))*temp2
					temp2*=(vubound(k)-vlbound(k)+1)
				Next

				vrr(i).ad=adr+temp1*udtlg
				If typ>TYPESTD Then update_address(i) 'update new address of udt components

			Else
				text+="[ Dyn array not defined]"
				If vrr(i).ad<>0 Then vrr(i).ad=0:update_address(i) 'weird case erase array() so defined then not defined..... not sure for cudt
			End If
	 	Else '' fixed lenght array
		 text+="[ "
			For k As Integer =0 To nbdim
				vubound(k)=arradr->nlu(k).ub
				vlbound(k)=arradr->nlu(k).lb
			  	If vrr(i).arrid then ''array tracked ?
	''Read the value need to be done here as the value could not be retrieved too late after the display of the array
					If trckarr(k).memadr<>0 Then
						Dim As String libel=var_sh2(trckarr(k).typ,trckarr(k).memadr,0,"")
						vflong=ValInt(Mid(libel,InStr(libel,"=")+1))

						''Check If the value Is inside the bounds, If Not the Case set LBound/UBound
					   	If vflong>vubound(k) Then
					   		vrr(i).ix(k)=vubound(k)
					   	Else
						  	If vflong<vlbound(k) Then
					   			vrr(i).ix(k)=vlbound(k)
						  	Else
						  		vrr(i).ix(k)=vflong
					   		EndIf
					   	EndIf
					End If
			  	End If
				text+=Str(vlbound(k))+"-"+Str(vubound(k))+":"+Str(vrr(i).ix(k))+" "
			Next
		   text+="]"

		''calculate the new adress for the array value depending on the new indexes, LIMIT 5 DIMS
			temp1=0:temp2=1
			For k As Integer = nbdim To 0 Step -1
				temp1+=(vrr(i).ix(k)-vlbound(k))*temp2
				temp2*=(vubound(k)-vlbound(k)+1)
			Next
		   vrr(i).ad=vrr(i).ini+temp1*udtlg
		 If typ>TYPESTD Then update_address(i) ''udt case
	 	End If
   End If


   If vrr(i).vr<0 Then ''field
	  With cudt(Abs(vrr(i).vr))
			If .typ=TYPEMAX Then 'bitfield
				text+="<BITF"+var_sh2(2,vrr(i).ad,.pt,Str(.ofs)+" / ")
				temp1=ValInt(Right(text,1)) 'byte value
				temp1=temp1 Shr .ofb        'shifts to get the concerned bit on the right
				temp1=temp1 And ((2*.lg)-1) 'clear others bits
				Mid(text,Len(text)) =Str(temp1) 'exchange byte value by bit value
				Mid(text,InStr(text,"<BITF")+5)="IELD"  'exchange 'byte' by IELD
			Else
				soffset=Str(.ofs)+" / "
				If Cast(Integer,.arr)=-1 Then soffset+=Str(vrr(i).ini)+" >> "  '19/05/2014
				text+="<"+var_sh2(.typ,vrr(i).ad,.pt,soffset)
				If .typ>TYPESTD then update_address(i) ''udt case
			End If
			Return text
	  End With
   Else
	  With vrb(vrr(i).vr) ''variable
		 adr=vrr(i).ad

			Select Case .mem
				Case 1
					'text+="<Local / " 'remove "Local / " as it's not a very usefull information
					text+="<"
					soffset=Str(.adr)+" / " 'offset
				Case 2
					text+="<Shared / "
				Case 3
					text+="<Static / "
				Case 4 'use *adr
					text+="<Byref param / "
					 ' ini keep the stack adr but not for param array() in this case always dyn so structure
					If Cast(Integer,.arr)<>-1 Then soffset=Str(.adr)+" / "+Str(vrr(i).gofs)+" / " 'to be checked
				Case 5
					text+="<Byval param / "
					soffset=Str(.adr)+" / "
				Case 6
					text+="<Common / "
			End Select
			If Cast(Integer,.arr)=-1 Then soffset+=Str(vrr(i).ini+SizeOf(Integer))+" >> "  '25/07/2015
		 text+=var_sh2(.typ,adr,.pt,soffset)
		 Return text
	  End With
   End If
End Function
'==========================================
private sub watch_check(wname()As String)
   Dim As Integer dlt,bg,ed,pidx,vidx,tidx,index,p,q,vnb,varb,ispnt,tad
  	Dim As String pname,vname,vtype

While wname(index)<>""
	pidx=-1:vidx=-1:p=0:vnb=1

   p=InStr(wname(index),"/")
	pname=Mid(wname(index),1,P-1)
	If InStr(pname,".dll") Then 'shared in dll
		pidx=0
	Else
	'check proc existing ?
		For i As Integer=1 To procnb
			If proc(i).nm=pname Then pidx=i:Exit For
		Next
	EndIf
	'var name : vname,vtype/ and so on then pointer number
	q=p+1
	p=InStr(q,wname(index),",")
	vname=Mid(wname(index),q,p-q)

	q=p+1
	p=InStr(q,wname(index),"/")
	vtype=Mid(wname(index),q,p-q)

	If pidx=-1 Then
	  messbox("Watched variables","Proc <"+pname+"> for <"+vname+"> removed, canceled")
	  index+=1
	  Continue While 'proc has been removed
	EndIf
	'check var existing ?
	bg=proc(pidx).vr:ed=proc(pidx+1).vr-1
   If pname="main" Then
		For i As Integer = 1 To vrbgbl
		   If vrb(i).nm=vname AndAlso udt(vrb(i).typ).nm=vtype AndAlso vrb(i).arr=0 Then
				vidx=i
				tidx=vrb(i).typ
				ispnt=vrb(i).pt
				Exit For
		   End If
		Next
   Else
   	If pidx=0 Then 'DLL [WTC]=dll.dll/B,Integer/0/0
			For i As Integer= 1 To TYPESTD
				If udt(i).nm=vtype Then tidx=i:Exit For
			Next
			wtch(index).typ=tidx
			wtch(index).psk=-4
			wtch(index).vnb=1 'only basic type or pointer
			wtch(index).idx=pidx
			wtch(index).pnt=ValInt(Right(wname(index),1))
			wtch(index).tad=0 'unknown address
			wtch(index).vnm(vnb)=vname
			wtch(index).var=0 'not an array
			wtch(index).vty(vnb)=vtype

			wtch(index).tvl=AddTreeViewItem(GTVIEWWCH,"",cast (hicon, 0),0,TVI_LAST,0)

			wtch(index).lbl=pname+"/"+vname+" <"+String(wtch(index).pnt,"*")+" "+udt(tidx).nm+">=Dll not loaded"
			wtchcpt+=1
   		index+=1
   		Continue While
   	EndIf
   EndIf
   If vidx=-1 Then
   	'local
	  For i As Integer = bg To ed
		 If vrb(i).nm=vname AndAlso udt(vrb(i).typ).nm=vtype AndAlso vrb(i).arr=0 Then
		 	vidx=i
			tidx=vrb(i).typ
			ispnt=vrb(i).pt
			Exit For
		 End If
	  Next
   EndIf
   If vidx=-1 Then
	  'var has been removed
	  messbox("Applying watched variables","<"+vname+"> removed, canceled")
	  index+=1
	  Continue While
   End If
	'store value for var_search
	wtch(index).vnm(vnb)=vname
	wtch(index).var=0
	wtch(index).vty(vnb)=vtype
	varb=vidx
	'check component
	q=p+1
	p=InStr(q,wname(index),",")
	While p
	  vidx=-1
	  vname=Mid(wname(index),q,p-q)
		q=p+1
		p=InStr(q,wname(index),"/")
		vtype=Mid(wname(index),q,p-q)
		For i As Integer =udt(tidx).lb To udt(tidx).ub
		 With cudt(i)
		 If .nm=vname AndAlso udt(.typ).nm=vtype AndAlso .arr=0 Then
			vidx=i:tidx=.typ
			ispnt=cudt(i).pt
			Exit For
		 End If
		 End With
		Next
		If vidx=-1 Then
		  'udt has been removed
		  messbox("Applying watched variables","udt <"+vname+"> removed, canceled")
		  index+=1
		  Continue While,While
		End If
		vnb+=1
		wtch(index).vnm(vnb)=vname
		wtch(index).vty(vnb)=vtype
		If tidx<=TYPESTD Then Exit While '20/08/215
		q=p+1
		p=InStr(q,wname(index),",")
	Wend
	tad=ValInt(Mid(wname(index),q,1)) 'tad
	q+=2
	If ispnt<>ValInt(Mid(wname(index),q,1)) Then 'pnt
		'pointer doesn't match
	   messbox("Applying watched variables",Left(wname(index),Len(wname(index))-2)+" not a pointer or pointer, canceled")
	   index+=1
	   Continue While
	EndIf

	wtch(index).tvl=AddTreeViewItem(GTVIEWWCH,"",cast (hicon, 0),0,TVI_LAST,0)
	wtch(index).lbl=proc(pidx).nm+"/"+vname+" <"+String(ispnt,"*")+" "+udt(tidx).nm+">=LOCAL NON-EXISTENT"
	wtch(index).typ=tidx
	wtch(index).psk=-4
	wtch(index).vnb=vnb
	wtch(index).idx=pidx
	wtch(index).pnt=ispnt
	wtch(index).tad=tad
	wtchcpt+=1
   index+=1
Wend
If wtchcpt Then menu_enable()
End Sub
'===================================================
private sub watch_del(i As Integer=WTCHALL)
	Dim As Integer bg,ed

	If i=WTCHALL Then
		bg=0:ed=WTCHMAX
	Else
		bg=i:ed=i
	EndIf
	For j As Integer=bg To ed
	   If wtch(j).psk=-1 Then Continue For
	   wtch(j).psk=-1
	   wtch(j).old=""
	   wtch(j).tad=0
	   wtchcpt-=1
	   If wtchcpt=0 Then menu_enable
	   DeleteTreeViewItem(GTVIEWWCH,wtch(j).tvl)
	Next
End Sub
'======================================
private sub watch_array()
	'compute watch for dyn array
	Dim As UInteger adr,temp2,temp3
	For k As Integer = 0 To WTCHMAX
		 If wtch(k).psk=-1 OrElse wtch(k).psk=-3 OrElse wtch(k).psk=-4 Then Continue For
		If wtch(k).arr Then 'watching dyn array element ?
			adr=vrr(wtch(k).ivr).ini
			ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@adr,SizeOf(Integer),0) '25/07/2015
			If adr<>wtch(k).arr Then wtch(k).adr+=wtch(k).arr-adr:wtch(k).arr=adr 'compute delta then add it if needed
			temp2=vrr(wtch(k).ivr).ini+2*SizeOf(Integer) 'adr global size '25/07/2015
			ReadProcessMemory(dbghand,Cast(LPCVOID,temp2),@temp3,SizeOf(Integer),0) '25/072015
			If wtch(k).adr>adr+temp3 Then 'out of limit ?
				watch_del(k) ' then erase
			End If
		End If
	Next
End Sub
'===================================================
private sub watch_sel(i As Integer) 'i index
If wtch(i).psk=-1 OrElse wtch(i).psk=-3 OrElse wtch(i).psk=-4 Then Exit Sub
If wtch(i).ivr=0 Then ''watched memory so adapt dumpmemory to the type
	dumpadr=wtch(i).adr
	dumptyp=wtch(i).typ
	dump_set()
	dump_sh
	If hdumpbx=0 Then
		''todo displays hdumpbx fb_Dialog(@dump_box,"Manage dump",windmain,283,25,120,150)
	End If
Else
	If vrr(wtch(i).ivr).ad=wtch(i).adr Then
		''todo select an item in treeview prc/var for displaying it
		''SendMessage(tviewvar,TVM_SELECTITEM,TVGN_CARET,Cast(LPARAM,vrr(wtch(i).ivr).tv))
	Else
		messbox("Select watched variable","Not possible : changed index (different address)")
	End If
End If
End Sub
'===========================================
private sub watch_sav() ''save watched
'example main/TUTU,TTEST/B,TTITI/C,Integer/pnt
'= PROC main,tutu type ttest,B type ttiti, C type integer, nb pointer
   Dim As Integer begb,Endb,stepb
   Dim As String  text

	For i As Integer =0 To WTCHMAX
		If wtch(i).psk=-1 OrElse wtch(i).psk=-2 OrElse wtch(i).Var<>0 Then 'not used or memory not saved or array
			text=""
		Else  'dll, more than one level and not a basic type on not a pointer ?
			If wtch(i).idx=0 AndAlso wtch(i).vnb>1 AndAlso wtch(i).typ>TYPESTD AndAlso wtch(i).pnt=0 Then
				text=""
			Else
				If wtch(i).idx=0 Then 'shared in dll
					text=Left( wtch(i).lbl , InStr( wtch(i).lbl ,"/") ) 'dll name
				   'watched used or saved previously but not used this time (psk= -3 or 4)
				Else
					text=proc(wtch(i).idx).nm+"/" 'proc name
				End If
				'if -4 order of storage is different than -3 so inverse stepping
				If wtch(i).psk=-4 Then begb=1:Endb=wtch(i).vnb:Stepb=1 Else begb=wtch(i).vnb:Endb=1:Stepb=-1
				For j As Integer =begb To Endb Step Stepb '10 levels max
					text+=wtch(i).vnm(j)+","+wtch(i).vty(j)+"/" 'name type
				Next
				text+=Str(wtch(i).tad)+"/"+Str(wtch(i).pnt)
			EndIf
		EndIf
		If text="/0/0" Then text=""
		if right(text,3)="0/0" Then text="" ''todo remove me after
		wtchexe(0,i)=text
	Next
End Sub
'==================================================================================
private sub watch_sh(aff As Integer=WTCHALL) 'default all watched
Dim As Integer vbeg,vend
Dim As String libel,value
If aff=WTCHALL Then vbeg=0:vend=WTCHMAX Else vbeg=aff:vend=aff
For i As Integer= vbeg To vend
   If wtch(i).psk<>-1 Then
   	libel=wtch(i).lbl
	  If wtch(i).psk=-3 Then
		 value=libel
		 libel+=udt(wtch(i).typ).nm
		 If wtch(i).idx Then
		 	libel+=">=LOCAL NON-EXISTENT"
		 Else
		 	libel+=">=Dll not loaded"
		 EndIf
	  ElseIf wtch(i).psk=-4 Then
	  	value=libel
	  Else
			value=var_sh2(wtch(i).typ,wtch(i).adr,wtch(i).pnt)
			libel+=value '2 spaces for trace T
	  End If
		'trace
		If Len(wtch(i).old)<>0 Then
				If wtch(i).old<>value Then dbg_prt("Trace :"+libel):wtch(i).old=value
				Mid(libel,1, 1) = "T"
		End If
		'additionnal data
		If wtch(i).tad Then libel+=" "+var_add(Mid(value,InStr(value,"=")+1),wtch(i).typ,wtch(i).tad)'additionnal info
		'watched tab
		SetTextItemTreeView(GTVIEWWCH,wtch(i).tvl,libel)
   End If
Next
End Sub
'=======================================================================
private sub watch_add(f As Integer,r As Integer =-1) 'if r<>-1 session watched, return index
	Dim As Integer t
	Dim As String temps,temps2
	If r=-1 Then
		'Find first free slot
		For i As Integer =0 To WTCHMAX
			If wtch(i).psk=-1 Then t=i:Exit For 'found
		Next
		wtchcpt+=1
		If wtchcpt=1 Then menu_enable 'enable the context menu for the watched window
	Else
		t=r
	End If

	wtch(t).typ=varfind.ty
	wtch(t).pnt=varfind.pt
	wtch(t).adr=varfind.ad
	wtch(t).arr=0
	wtch(t).tad=f

If varfind.iv=-1 Then 'memory from dump_box or shw/expand
	wtch(t).lbl=varfind.nm
	wtch(t).psk=-2
	wtch(t).ivr=0
Else 'variable
	wtch(t).ivr=varfind.iv
	' if dyn array store real adr
	If Cast(Integer,vrb(varfind.pr).arr)=-1 Then
	   ReadProcessMemory(dbghand,Cast(LPCVOID,vrr(varfind.iv).ini),@wtch(t).arr,4,0)
	End If

	If varfind.iv<procr(1).vr then'shared 04/02/2014
		wtch(t).psk=0
		For j As long =1 To procnb
			If proc(j).nm="main" Then
		   	wtch(t).idx=j 'data for reactivating watch
		   	Exit For
			EndIf
		Next
   	wtch(t).dlt=wtch(t).adr
		temps2="main"
	Else
		For j As UInteger = 1  To procrnb 'find proc to delete watching 'index 0 instead 1 03/02/2014
	   	If varfind.iv>=procr(j).vr And varfind.iv<procr(j+1).vr Then
	   		wtch(t).psk=procr(j).sk
	   		wtch(t).idx=procr(j).idx 'data for reactivating watch
	   		wtch(t).dlt=varfind.iv-procr(j).vr '06/02/2014 wtch(t).dlt=wtch(t).adr-wtch(t).psk

			 If procr(j).idx=0 Then 'dll
				 For k As Integer =1 To dllnb
				 	If dlldata(k).bse=procr(j).sk Then
				 		temps2=dll_name(dlldata(k).hdl,2)
				 		Exit For
				 	EndIf
				 Next
			 Else
			  	temps2=proc(procr(j).idx).nm
			 End If
			 Exit For
	   	EndIf
		Next
	EndIf
	'temps=var_sh1(varfind.iv) replaced by lines below 08/05/2014
	temps=GetTextTreeView(GTVIEWTHD,vrr(varfind.iv).tv)
	If temps="Not filled" Then
		temps=Mid(wtch(t).lbl,InStr(wtch(t).lbl,"/")+1)
	EndIf
	If InStr(temps,"/") Then 'not cudt
		temps=Left(temps,InStr(temps,"/"))
	Else
		temps=Left(temps,InStr(temps,"<"))
	EndIf
	wtch(t).lbl="  "+temps2+"/"+temps+" "

	If vrb(varfind.pr).mem=3 Then
		wtch(t).psk=0'procr(1).sk 'static
	End If
	'fill wtch vnm, vty, var - component/var bottom to top
	Dim As integer temp,temp2,hitemp
	Dim As Integer iparent,c=0
	iparent=wtch(t).ivr
 	Do
	 	If vrr(iparent).vr>0 Then
			c+=1
			wtch(t).vnm(c)=vrb(vrr(iparent).vr).nm
			wtch(t).vty(c)=udt(vrb(vrr(iparent).vr).typ).nm
			If vrb(vrr(iparent).vr).arr Then
				wtch(t).var=1
			Else
				wtch(t).var=0
			EndIf
			Exit Do
	 	Else
			c+=1
			wtch(t).vnm(c)=cudt(Abs(vrr(iparent).vr)).nm
			wtch(t).vty(c)=udt(cudt(Abs(vrr(iparent).vr)).typ).nm
			If cudt(Abs(vrr(iparent).vr)).arr Then
				wtch(t).var=1:Exit Do
			Else
				wtch(t).var=0
			EndIf
	 	End If
		temp=GetParentItemTreeView(GTVIEWVAR,vrr(iparent).tv)
	 	For i As Integer =1 To vrrnb
			If vrr(i).tv=temp Then iparent=i
	 	Next
 	Loop While 1
 	wtch(t).vnb=c

EndIf
If r=-1 Then wtch(t).tvl=AddTreeViewItem(GTVIEWWCH,"Not filled",cast (hicon, 0),0,TVI_FIRST,0) 'create an empty line in treeview
watch_sh(t)
wtchnew=t
End Sub
'==========================================
private sub watch_set()
	If wtchcpt>WTCHMAX Then ' free slot not found
		PanelGadgetSetCursel(GRIGHTTABS,TABIDXWCH)
		messbox("Add watched variable","No free slot, delete one")
		Exit Sub
	EndIf
	'Already set ?
	For i As Integer =0 To WTCHMAX
		If wtch(i).psk<>-1 AndAlso wtch(i).adr=varfind.ad AndAlso wtch(i).typ=varfind.ty AndAlso _
			wtch(i).pnt=varfind.pt Then'found
			'If fb_message("Set watched variable/memory","Already existing"+Chr(13)+"Continue ?", _
			'MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON1) = IDNO Then exit sub
			wtchidx=i'for delete
			'todo windowhide(hwtch_bx,0) fb_MDialog(@watch_box,"Adding watched : "+Left(wtch(i).lbl,Len(wtch(i).lbl)-1)+" already existing",windmain,50,25,180,90)
			Exit Sub
		EndIf
	Next
	watch_add(0)'first create no additional type
End Sub
'===========================================================
private sub watch_trace(t As Integer=WTCHALL)
	If t=WTCHALL Then 'reset all
		For i As Integer =0 To WTCHMAX
			If wtch(i).old<>"" Then
				wtch(i).old=""
				watch_sh(i)
			EndIf
		Next
	Else
		If wtch(t).old<>"" Then 'reset one
			wtch(t).old=""
		Else 'set tracing
			If wtch(t).typ>15 AndAlso wtch(t).pnt=0 Then
				messbox("Tracing Watched var/mem","Only with pointer or standard type")
				Exit Sub
			Else
				If flaglog=0 Then
					If MESSBOX("Tracing var/mem","No log output defined"+Chr(13)+"Open settings ?",MB_YESNO)=IDYES Then
						hidewindow(hsettings,KSHOW) ''can be changed here
					EndIf
					If flaglog=0 Then
						messbox("Tracing var/mem","No log output defined"+Chr(13)+"So doing nothing")
						Exit Sub
					EndIf
				EndIf
				If wtch(t).psk=-2 Then
					wtch(t).old=var_sh2(wtch(t).typ,wtch(t).adr,wtch(t).pnt)
				ElseIf wtch(t).psk=-3 Or wtch(t).psk=-4 Then
					wtch(t).old=wtch(t).lbl
				Else
					wtch(t).old=var_sh2(wtch(t).typ,wtch(t).adr,wtch(t).pnt)
				EndIf
			EndIf
		EndIf
		watch_sh(t)
	EndIf
End Sub
'=====================================================================
private sub watch_addtr()
	wtchnew=-1
	If var_find2(htviewvar)<>-1 Then
		watch_set()
		If wtchnew<>-1 Then watch_trace(wtchnew)
	EndIf
End Sub
'========================================================
private sub var_sh() 'show master var
	For i As Integer =1 To vrrnb
		SetTextItemTreeView(GTVIEWVAR,vrr(i).tv,var_sh1(i))
	Next
   watch_array()
   watch_sh
End Sub
'======================================================
'' handles display at end of run or when running auto
'======================================================
private sub dsp_change(index As Integer)
	dim as integer tot
	linecur_change(index)
	If flagtrace And 2 Then dbg_prt(LTrim(line_text(linecur-1),Any " "+Chr(9)))
	If runtype=RTAUTO Then
		watch_array() 'update adr watched dyn array
		watch_sh()    'update watched but not all the variables
	ElseIf runtype=RTSTEP Then
		if flagupdate=true then
			var_sh()
			dump_sh()
		end if
		watch_array() ''even flagupdate is off watched are updated
		watch_sh()

		for ithd as integer =0 to threadnb
			if  thread(ithd).sts=KTHD_STOP then
				tot+=1
			EndIf
		Next
		if tot>1 then
			stopcode=CSTHREADS
		EndIf

		but_enable()
		If PanelGadgetgetCursel(GRIGHTTABS) = TABIDXPRC Then
			proc_sh()
		elseIf PanelGadgetgetCursel(GRIGHTTABS) = TABIDXTHD Then
			thread_text()
		EndIf

		if indexdata.autoupd then
			index_fullupdate()
		EndIf
	End If
End Sub
'====================================================================
''  load shared and common variables, default=no dll number (d=0)
'====================================================================
private sub globals_load(d As Integer=0)
	Dim temp As integer
	Dim As Integer vb,ve 'begin/end index global vars
	Dim As Integer vridx
	If vrbgblprev<>vrbgbl Then 'need to do ?
		If vrbgblprev=0 Then
			procr(procrnb).tv=AddTreeViewItem(GTVIEWVAR,"Globals (shared/common) in : main ",cast (hicon, 0),0,0,0) 'only first time
			'print "var ini in globals_load 00"
			var_ini(procrnb,1,vrbgbl) ''add vrbgblprev instead 1
			'print "var ini in globals_load 01"
			'dbg_prt2("procrnb="+Str(procrnb))
			procr(procrnb+1).vr=vrrnb+1 ''to avoid removal of global vars when the first executed proc is not the main one
		Else
			procrnb+=1
			temp=getprevitemtreeview(GTVIEWVAR,procr(1).tv)
			If temp=0 Then 'no item before main
				temp=TVI_FIRST
			EndIf
			If d=0 Then ''called from extract stabs
				d=dllnb
				vb=vrbgblprev+1
				ve=vrbgbl
			Else ''from load_dll
				vb=dlldata(d).gblb
				ve=dlldata(d).gblb+dlldata(d).gbln-1
			End If
			procr(procrnb).tv=AddTreeViewItem(GTVIEWVAR,"Globals in : "+dll_name(dlldata(d).hdl,2),cast (hicon, 0),0,temp,0)
			procr(procrnb).sk=dlldata(d).bse
			dlldata(d).tv=procr(procrnb).tv
			var_ini(procrnb,vb,ve)
			procr(procrnb+1).vr=vrrnb+1 'put lower limit for next procr
			procr(procrnb).idx=0

			If wtchcpt Then
				For i As Integer= 0 To WTCHMAX
   				If wtch(i).psk=-3 Then 'restart
   					If wtch(i).idx=0 Then 'shared dll
		   				wtch(i).adr=vrr(procr(procrnb).vr+wtch(i).dlt).ad'06/02/2014  wtch(i).dlt+procr(procrnb).sk
		   				wtch(i).psk=procr(procrnb).sk
   					EndIf
   				ElseIf wtch(i).psk=-4 Then 'session watch
   					If wtch(i).idx=0 Then 'shared dll
							vridx=var_search(procrnb,wtch(i).vnm(),wtch(i).vnb,wtch(i).var,wtch(i).pnt)
							If vridx=-1 Then messbox("Proc watch","Running var not found"):Continue For
							var_fill(vridx)
		 					watch_add(wtch(i).tad,i)
   					EndIf
   				EndIf
				Next
			EndIf

		EndIf
	EndIf
End Sub

'=====================================================
private sub thread_del(thid As UInteger)
	Dim As Integer k=1,threadsup,threadold=threadcur
	'print "in thread_del thid=";thid

	For i As Integer =1 To threadnb
		If thid<>thread(i).id Then
			If i<>k Then
				thread(k)=thread(i)
				If i=threadcur Then
					threadcur=k 'optimization
				EndIf
			EndIf
			k+=1
		Else
			threadsup=i
			thread(threadsup).plt=0
			thread(threadsup).pe=0
			If thread(i).sv<>-1 Then
				'delete thread item and child
				'proc delete
				For j As Integer = procrnb To 2 Step -1 'always keep procr(1)=main
					If procr(j).thid=thid Then
						proc_del(j)
					EndIf
				Next
				DeleteTreeViewItem(GTVIEWTHD,thread(i).tv)
				thread(i).tv=0
			EndIf
	    End If
	Next

	threadnb-=1
	If threadsup<>threadold Then Exit Sub 'if deleted thread was the current, replace by first thread

	threadcur=0 'first thread
	threadsel=0
	threadhs=thread(0).hd
	'runtype=RTSTEP
	dsp_change(thread(0).sv)

End Sub
'======================================================================
''releases the scintilla docs except the one attached to the window
'======================================================================
private sub release_doc(typ as INTEGER=1)
	if typ=1 then
		var ptrdoc=cast(any ptr,Send_sci(SCI_GETDOCPOINTER,0,0))
		for isrc as integer =0 to sourcenb
			if sourceptr(isrc)<>ptrdoc then
				send_sci(SCI_RELEASEDOCUMENT,0,sourceptr(isrc))
			end if
		next
	else
		oldscintilla=cast(any ptr,Send_sci(SCI_GETDOCPOINTER,0,0))
		for isrc as integer =0 to sourcenb
			if sourceptr(isrc)<>0 and sourceptr(isrc)<>oldscintilla then
				send_sci(SCI_RELEASEDOCUMENT,0,sourceptr(isrc))
			end if
		next
		if oldscintilla then
			Send_sci(SCI_SetText, 0, @"No source found or at least not all")
			currentdoc=oldscintilla
		EndIf
	end if
end sub
'==============================
'' Reinitialisation
'==============================
private sub reinit()
	vrbgbl=0:vrbloc=VGBLMAX:vrbgblprev=0
	prun=FALSE
	runtype=RTOFF
	stopcode=0
	firsttime=0
	flagmain=true
	compilerversion=""
	if flagrestart=-1 then
		if currentdoc then ''removes all previous docs
			release_doc()
			oldscintilla=cast(any ptr,Send_sci(SCI_GETDOCPOINTER,0,0)) ''keep for release later
			for isrc As Integer=0 To sourcenb
				DeleteItemPanelGadget(GSRCTAB,isrc)
			next
			ResetAllComboBox(GFILELIST)
		end if
	EndIf
	sourcenb=-1:dllnb=0
	vrrnb=0:procnb=0:procrnb=0:linenb=0:cudtnb=0:arrnb=0:procr(1).vr=1
	procfn=0
	procnew=-1
	proc(1).vr=VGBLMAX+1 'for the first stored proc
	udtcpt=0:udtmax=0
	procsort=KPROCNM
	flagtrace=0
	flagattach=FALSE
	flagkill=FALSE
	shwexp.free=true
	threadsel=0
	for ith as integer =0 to threadnb
		thread(ith).plt=0
		thread(ith).pe=false
	Next
	threadnb=-1
	dumpadr=0
	brkv_set(0) ''no break on var
	brk_del(0) ''no break on cursor/etc
	brknb=0
	'for ibrk as integer = 1 to brknb
		'if brkol(ibrk).typ<>1 and brkol(ibrk).typ<>4 and brkol(ibrk).typ<>6 and brkol(ibrk).typ<>51 and brkol(ibrk).typ<>54 and brkol(ibrk).typ<>56 then ''keep only perm/temp/counter BP
			'brk_del(ibrk)
		'EndIf
	'Next
	DeleteTreeViewItemAll(GTVIEWVAR)
	PanelGadgetSetCursel(GRIGHTTABS,TABIDXVAR)
	DeleteTreeViewItemAll(GTVIEWTHD)
	'todo DeleteTreeViewItem(GTVIEWWCH,0) 'watched   needed ????
	hidewindow(hshwexpbx,KHIDE)
	for icol as INTEGER= 1 to dumpnbcol
		DeleteListViewColumn(GDUMPMEM,1)
	next
	dumpnbcol=0
	DeleteListViewItemsAll(GDUMPMEM)
	''todo 'array_tracking_remove
	source_change(-1) ''reinit to avoid a potential problem
	menu_enable()
end sub
'================================================================
'' check if exe bitness if not wrong 32bit<>64bit windows only
'================================================================
private function check_bitness(fullname as string) as integer
	#Ifdef __fb_win32__
		dim as long bintype
			getbinarytype(strptr(fullname),@bintype) ''a control to prevent 32bit<>64bit
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
				'close #1
				return 0
			end if
		#else
			if ubyt<>1 then
				messbox("FBdebugger 32bit","can not be used for debugging 64bit exe...")
				'close #1
				return 0
			end if
		#endif
	#endif
	return -1
end function
'===============================================================================
private sub close_all()
	'print "destroy mutex and condvar"
	mutexdestroy blocker
	#ifndef __fb_win32__
		condsignal(condid)
		conddestroy(condid)
	#endif
	'print "release doc"
	release_doc ''releases scintilla docs
	''todo free all the objects menus, etc
	If sourcenb<>-1 Then
		watch_sav()
		brk_sav()
	EndIf
	ini_write()
	end
End Sub
'================================
''
'================================
private sub process_terminated()
	KillTimer(hmain,GTIMER001)
	watch_sav()
	brk_sav()
	runtype=RTEND
	but_enable()
	menu_enable()
	shortcut_enable()

	#ifdef __fb_win32__
		messbox("","END OF DEBUGGED PROCESS",MB_SYSTEMMODAL)
		mutexunlock blocker
		mutexlock blocker
	#else
		messbox("","END OF DEBUGGED PROCESS "+"afterkilled code="+str(afterkilled))
		select case as const afterkilled
			case KDONOTHING
				exit sub
			case KENDALL
				close_all()
			case KRESTART to KRESTART9
				restart_exe(afterkilled-KRESTART)
			case else
				messbox("","Afterkilled not handled")
		End select
		afterkilled=KDONOTHING
	#endif
End Sub
'====================================================================================================================
'' if debuggee running ask for killing return true after killing or if nothing running, false debuggee still running
'====================================================================================================================
private function kill_process(text As String) As Integer
	dim As Long retcode,lasterr
	if prun then ''debuggee waiting or running
		If messbox("Kill current running Program ?",text+Chr(10)+Chr(10) _
			       +"USE CARREFULLY SYSTEM CAN BECOME UNSTABLE, LOSS OF DATA, MEMORY LEAK"+Chr(10) _
				   +"Try to close your program first",MB_YESNO) = IDYES then
			flagkill=true
			#ifdef __fb_win32__
				retcode=terminateprocess(dbghand,999)
			  	lasterr=getlasterror
			  	thread_resume()
			  	#ifdef fulldbg_prt
					dbg_prt ("return code terminate process ="+Str(retcode)+" lasterror="+Str(lasterr))
				#endif
				While prun:Sleep 500:Wend
			#else
				'print "Killing process=";pid
				if runtype=RTSTEP then ''waiting
					mutexlock blocker
					msgcmd=KPT_KILL
					bool2=true
					condsignal(condid)
					while bool1<>true
						condwait(condid,blocker)
					wend
					bool1=false
					mutexunlock blocker
				else ''running
					linux_kill(thread(0).id,9)
				end if
				'print"end of cmd exit"
			#endif
			Return TRUE
		Else
			Return FALSE
		endif
	else
		Return true
	end if
end function
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
		watch_del()
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
	'messbox("new save(0)","now="+savexe(0))
	If cmdline<>"" Then cmdexe(0)=cmdline
	DisableGadget(IDBUTRERUN,0)
	SetToolTipText(IDBUTRERUN,TTRERUN,exename)
	settitle()
	ini_write() ''done also when fbdebugger closing but also here in case of crash of fbdebugger
End sub
'======================================================================
''loads the source code files, by slice : n contains the first to be loaded until sourcenb
''n=0 for the first loading
''=====================================================================
private sub sources_load(n As integer,exedate as double)
	dim As integer flgt,fnum,msgnofile
	dim as any ptr ptrdoc
	'print "loading sources nb,n=";sourcenb,n,"flagrestart=";flagrestart
	if flagrestart=-1 Then
		statusbar_text(KSTBSTS,"Loading sources")

	   	for isrc As Integer=n To sourcenb ' main index =0
			'print "loading =";isrc,source(isrc)
		   	if FileExists(source(isrc))=0 Then
				if msgnofile=0 then
					messbox("Loading Source error","File : "+source(isrc)+" not found"+chr(10)+chr(10)+"This message appears only one time even if many files are missing")
					msgnofile=1
				end if
				print "Loading Source error","File : ";source(isrc);" not found"
				sourcebuf(0)=asc("N"):sourcebuf(1)=asc("o"):sourcebuf(2)=asc(" "):sourcebuf(3)=asc("F"):sourcebuf(4)=asc("i"):sourcebuf(5)=asc("l"):sourcebuf(6)=asc("e"):sourcebuf(7)=0
		   	else
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
			end if
			AddPanelGadgetItem(GSRCTAB,isrc,source_name(source(isrc)))

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
				messbox("Maybe something to do","the source code contains a BOM code so --> unicode")
			else

				if isrc=0 and oldscintilla=0 then
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
			if oldscintilla then
				send_sci(SCI_RELEASEDOCUMENT,0,oldscintilla)
			EndIf
		end if
		SetItemComboBox(GFILELIST,1)
		''todo
		'EnableMenuItem(menutools,IDHIDLOG,MF_GRAYED) 'log file tab canceled so option menu grayed
	else 'restart with same exe, only the main files are not loaded, dll sources are removed

		''todo change later
		'For i As Integer=sourcenb+1 To flagrestart
		'	setWindowText(richedit(i),""):ShowWindow(richedit(i),SW_HIDE)'hide all the exceding windows (>sourcenb)
		'	sendmessage(htab1,TCM_DELETEITEM ,i,0) 'delete tabs
		'Next
		flagrestart = -1
	endif
End sub
''==============================================================================
'' retrieves the first line for main procedure as not provided by the debug data
''==============================================================================
private sub main_line()
	For iproc As Integer =0 To procnb 'As .nu not consistent for main
		Dim As integer temp=proc(iproc).db
		If proc(iproc).nm="main" Then
			For iline As Integer =1 To linenb
				If rline(iline).ad>temp Then 'found first line of main
					proc(iproc).nu=rline(iline).nu
					rlineprev=iline
					'print "first line of main="+str(iline)
					Exit For,For
				EndIf
			Next
		End If
	Next
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
	Print #fileout,"[BUT]="+str(setbuttons)
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
				If Dir(lineread)<>"" then 'And InStr(LCase(lineread),".exe") Then
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

		ElseIf Left(lineread,6)="[BUT]=" Then ''buttons set/unset
				setbuttons=ValInt(RTrim(Mid(lineread,7)))

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
	if exename<>"" then
		DisableGadget(IDBUTRERUN,0)
		SetToolTipText(IDBUTRERUN,TTRERUN,"Restart "+exename)
	end if

	if setbuttons<>-1 then
		for ibut as integer=IDBUTSTEP to IDBUTFREE
			if bit(setbuttons,ibut-IDBUTSTEP)=0 then ''button is unset
				hidegadget(ibut, KHIDE)
			End if
		next
	end if
End sub

'===============================================================================
private sub closes_debugger()
	if prun then
		#ifndef __fb_win32__
			afterkilled=KENDALL
		#endif
		if kill_process("Closing debugger")=true then
			close_all
		EndIf
	else
		if messbox("Quit Fbdebugger","Are you sure ?",MB_YESNO)=IDYES then
			close_all()
		End If
	end if
end sub
''==============================================================================
'' freeing all before quitting the debugger
''==============================================================================
private sub closes_debugger2()
	dim as zstring *50 text =""

	If prun Then
		text=>"CAUTION PROGRAM STILL RUNNING."+Chr(10)+Chr(10)
	EndIf
	if messbox("Quit Fbdebugger",text+"Are you sure ?",MB_YESNO)=IDYES then
		If prun Then
			'print "closing so sending sigkill"
			'linux_kill(thread(0).id,9)
			'sleep 500
		end if
		'print "destroy mutex"
		mutexunlock blocker
		mutexdestroy blocker
		#ifndef __fb_win32__
			condsignal(condid)
			conddestroy(condid)
		#endif
		sleep 500
		KillTimer(hmain,GTIMER001)
		release_doc ''releases scintilla docs
		''todo free all the objects menus, etc
		If sourcenb<>-1 Then ''case exiting without stopping debuggee before
			watch_sav()
			brk_sav()
		EndIf
		ini_write()
		end
	endif
end sub
'===================================================
'' Drag and drop
'===================================================
private sub drag_n_drop()
	messbox("feature to be coded","drag_n_drop")
end sub
'=======================================================================
'' puts the intruction breakcpu at the beginning of every executable line
'=======================================================================
private sub put_breakcpu(beginline as integer=1)
	For iline As Integer=beginline to linenb
		ReadProcessMemory(dbghand,Cast(LPCVOID,rline(iline).ad),@rLine(iline).sv,1,0) 'sav 1 byte before writing breakcpu
		WriteProcessMemory(dbghand,Cast(LPVOID,rline(iline).ad),@breakcpu,1,0)
	Next
End Sub
'===================================================
'' initializes for the current debuggee
'===================================================
private sub init_debuggee(srcstart as integer)
	''end of extraction ''todo add that for linux when the exe is running
	'print "in init_debuggee"
	dim as INTEGER listidx
	globals_load()
	If procrnb=0 Then
	   If flagwtch=0 AndAlso wtchexe(0,0)<>"" Then watch_check(wtchexe())
	   flagwtch=0
	EndIf
	'list_all() ''list all the debug data

	listidx=srclistfirst
	while listidx<>-1
		AddComboBoxItem(GFILELIST,srcname(listidx-1),-1)
		'print srcname(listidx-1)
		listidx=srclist(listidx).child
	Wend


	#Ifdef __fb_win32__
		put_breakcpu()
	#endif
	''srcstart contains the index for starting the loading of source codes
	sources_load(srcstart,filedatetime(exename))
	'activate buttons/menu after real start
	but_enable()
	menu_enable()
	shortcut_enable()
	'apply previous breakpoints
	brk_apply()
end sub
'===================================================
'' Unloads dll elements
'===================================================
private sub dll_unload(idll as integer)
	Dim As Integer ibr=1
	For ipr As Integer =2 To procrnb
		If procr(ipr).tv=dlldata(idll).tv Then
			proc_del(ipr,2) ' 'delete procr().tv
			Exit For
		EndIf
	Next

	''delete brkpoint but before trying to save it in brkexe
	While ibr<=brknb
		If brkol(ibr).index>=dlldata(idll).lnb AndAlso brkol(ibr).index<=dlldata(idll).lnn Then 'inside rline of dll
			'create in brkexe for use in next dll loading
			For n As Integer = BRKMAX To 1 Step-1 'search by the last slot, later if there are BRKMAX brkpt this one will be lost
				If brkexe(0,n)="" Then 'find an empty slot if not data is lost
					brkexe(0,n)=source_name(source(brkol(ibr).isrc))+","+Str(brkol(ibr).nline)+","+Str(brkol(ibr).typ)
				EndIf
				Exit For
			Next
			brk_del(ibr)
		EndIf
		ibr+=1
	Wend
End Sub
'===========================================
''handles other exceptions than breakpoint
'===========================================
private sub exception_handle(adr as INTEGER)
	dim as string linetext
	'gest_brk(rline(thread(threadcur).sv).ad)
	gest_brk(adr)
	source_change(rline(thread(threadcur).sv).sx) 'display source
	line_display(rline(thread(threadcur).sv).nu-1)
	linetext=line_text(rline(thread(threadcur).sv).nu-1)
	'case error inside proc initialisation (e.g. stack over flow)
	If adr>rline(thread(threadcur).sv).ad And _
		adr<rline(thread(threadcur).sv+1).ad And _
		rline(thread(threadcur).sv+1).nu=rline(thread(threadcur).sv).nu Then
		libelexception+="ERROR AT BEGINNING OF PROC NOT REALLY ON THIS LINE"+Chr(13)+ _
		"CHECK DIM (e.g. width array to big), Preferably don't continue"+Chr(13)+Chr(13)
	Else
		libelexception+="Possible error on this line but not SURE"+Chr(13)+Chr(13)
	End If

	libelexception+="File  : "+source(rline(thread(threadcur).sv).sx)+Chr(13)+ _
	"Proc  : "+proc(rline(thread(threadcur).sv).px).nm+Chr(13)+ _
	"Line  : "+Str(rline(thread(threadcur).sv).nu)+" (selected and put in red)"+Chr(13)+ _
	linetext+Chr(13)+Chr(13)+"Try To continue ? (if yes change values and/or use [M]odify execution)"
	debugdata=messbox("EXCEPTION",libelexception,MB_YESNO)  ''used in thread2
end sub
'===================================================
'' handles the debug events  (triggered by timer)
'===================================================
private function debug_event() as INTEGER
	dim as integer dbgevent=debugevent
	static as INTEGER thprev
	debugevent=KDBGNOTHING
	if dbgevent = KDBGNOTHING then return true
	'print "************ debug_event ";time;" ";dbgevent;" ";hex(debugdata);" stopcode=";stoplibel(stopcode)
	select case as const dbgevent
		Case KDBGRKPOINT

			'print "KDBGRKPOINT=";stopcode,"csline=";CSLINE,hex(debugdata)
			if stopcode=CSSTEP orelse stopcode=CSMEM orelse stopcode=CSVAR orelse stopcode=CSUSER orelse stopcode=CSNEWTHRD then
				gest_brk(debugdata)
			else
				gest_brk(brkol(debugdata).ad,brkol(debugdata).index) ''address and line index
			end if

		Case KDBGCREATEPROCESS
			srcstart=sourcenb+1
			#Ifdef __fb_win32__
				if debug_extract(debugdata,exename)=0 then ''otherwise there is a problem (no debug data or when reading debuggee memory)
					init_debuggee(srcstart)
				EndIf
			#else
				elf_extract(exename)

				mutexlock blocker
				bool2=true
				condsignal(condid)
				mutexunlock blocker
			#endif

		Case KDBGCREATETHREAD
			#Ifdef __fb_linux__
			''Linux creation fo a new thread
			var ret=messbox("New Thread","Thread created = "+Str(thread(threadnb).id)+" / current = "+Str(thread(threadcur).id) _
				+Chr(10)+Chr(13)+" Continue with new one or with current ?" ,MB_YESNO)
			if ret=IDYES Then
				msgdata=1
				'thread_change(threadnb)
			Elseif ret=IDNO then
				msgdata=0
				threadsel=threadcur
			else
				msgdata=2
				threadsel=threadcur
			EndIf
			mutexlock blocker
			bool2=true
			condsignal(condid)
			mutexunlock blocker
			#endif

		Case KDBGEXITPROCESS
			process_terminated()

		Case KDBGEXITTHREAD
			thread_del(debugdata)
			#Ifdef __fb_linux__
			msgcmd=0 ''no action
			mutexlock blocker
			bool2=true
			condsignal(condid)
			mutexunlock blocker
			#endif

		Case KDBGDLL
		#Ifdef __fb_win32__
			dll_load()
		#else
			messbox("Linux dll","nedd to be coded")
		#endif
		Case KDBGDLLUNLOAD
			dll_unload(debugdata)

		Case KDBGEXCEPT
			exception_handle(debugdata)

		''Case KDBGSTRING not used

		Case else
			messbox("Handling debug event","Debug event unkown, not handled ="+str(debugevent))
	End Select
	#Ifdef __fb_win32__
		'print "MutexunLock DBGEVENT"
		mutexunlock blocker ''release second thread
		'print "MutexLock DBGEVENT"
		mutexlock   blocker ''lock for next event
	#endif
	return true
end function
'===================================================
''launch by command line
'===================================================
private sub external_launch()

	dim as string debuggee=command(1)
	dim as string cmdline=mid(command(),len(debuggee)+2)

	if debuggee="" then exit sub ''no debuggee

	if instr(debuggee,slash)=0 then debuggee=exepath+slash+debuggee ''debugge without path so exepath added
	if dir(debuggee)="" then
		messbox("File as parameter","File ="+debuggee+" doesn't exist")
		exit sub
	EndIf

	if check_bitness(debuggee)=0 then exit sub ''bitness of debuggee and fbdebugger not corresponding

	exename=debuggee
	exe_sav(exename,cmdline)
	SetTimer(hmain,GTIMER001,100,Cast(Any Ptr,@debug_event))
	#Ifdef __fb_win32__
		If ThreadCreate(@start_pgm)=0 Then
			KillTimer(hmain,GTIMER001)
			messbox("Debuggee not running","ERROR unable to start the thread managing the debuggee")

		endif
	#else
		If ThreadCreate(@start_pgm)=0 Then
			KillTimer(hmain,GTIMER001)
			messbox("Debuggee not running","ERROR unable to start the thread managing the debuggee")
		endif
	#endif
end sub
'===================================================================================
''real restart after killing debuggee if needed
'===================================================================================
private sub restart_exe(byval idx as integer)
	''todo maybe cmdline changed so need to be saved
	if idx=0 then
		Dim As Double dtempo=FileDateTime(exename)
		If exedate=0 then
			exedate=dtempo
		elseif exedate=dtempo Then
			flagrestart=sourcenb ''exe not changed so no need to reload sources
		else
			flagrestart=-1 ''need to reload sources
		EndIf
		If wtchcpt Then flagwtch=1
	else
		exename=savexe(idx)
		exe_sav(exename,cmdexe(idx))
	end if

	''todo make a sub and call also in sub external_launch, in select_file
	if check_bitness(exename)=0 then exit sub ''bitness of debuggee and fbdebugger not corresponding

	reinit ''reinit all except GUI parts
	settitle()

	SetTimer(hmain,GTIMER001,100,Cast(Any Ptr,@debug_event))

	#Ifdef __fb_win32__
		If ThreadCreate(@start_pgm)=0 Then
			KillTimer(hmain,GTIMER001)
			messbox("ERROR unable to start the thread managing the debuggee","Debuggee not running")
		endif
	#else
		If ThreadCreate(@start_pgm)=0 Then
			KillTimer(hmain,GTIMER001)
			messbox("Debuggee not running","ERROR unable to start the thread managing the debuggee")
		endif
	#endif
end sub
'==================================================================================
'' Debuggee restarted, last debugged (using IDBUTRERUN) or one of the 9/10 others
'==================================================================================
private sub restart(byval idx as integer=0)
	if prun then
		#Ifndef __fb_win32__
			afterkilled=KRESTART+idx
		#EndIf
		if kill_process("Trying to launch but debuggee still running")=FALSE then
			print "in restart false ?????"
			exit sub
		end if
	'else
		'restart_exe(idx)
	EndIf
	restart_exe(idx)
end	sub
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
'=================================================================================
'' changes address of execution (forward or backward) only in the same procedure
'==================================================================================
private sub exec_mod() 'execution from cursor
	#Ifdef __fb_win32__
		dim as integer dummy ''used to align vcontext on 16 bytes
		Dim vcontext As CONTEXT
		vcontext.contextflags=CONTEXT_CONTROL
	#endif
	Dim cln As Integer,rln As Integer',b As Integer

	cln=line_cursor() 'get line
	rln=line_exec(cln,"Changing next executed line not possible, select an executable line")
	'print "cln rln=";cln,rln,rline(rln).nu
	if rln=-1 then exit sub

	'If rline(rln+1).nu=cln+1 And rline(rln+1).sx=srcdisplayed Then rln+=1 ''weird case : first line main proc

	if rln=rlinecur then exit sub ''same line

	'If linecur=l+1 And srcdisplayed=srccur Then
		'If messbox("Execution on cursor","Same line, continue ?",MB_YESNO)=IDNO Then
			'Exit Sub
		'EndIf
	'End If

	'check inside same proc if not msg
	If rLine(rln).ad>proc(procsv).fn Or rLine(rln).ad<=proc(procsv).db Then
		messbox("Changing next executed line","Not possible : only inside the current procedure")
		Exit Sub
	End If
	If rLine(rln).ad=proc(procsv).fn Then
		thread(threadcur).pe=TRUE        'is last instruction
	EndIf
	'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@breakcpu,1,0) 'restore CC previous line
	'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(i).ad),@rLine(i).sv,1,0) 'restore old value for execution
	thread(threadcur).od=thread(threadcur).sv:thread(threadcur).sv=rln
	'get and update registers
	#Ifdef __fb_win32__
		GetThreadContext(threadhs,@vcontext)
		vcontext.regip=rline(rln).ad
		SetThreadContext(threadhs,@vcontext)
	#else
	'print "changing exec=";hex(rline(rln).ad),rline(rln).nu
		mutexlock blocker
		msgad=rline(rln).ad
		msgcmd=KPT_XIP
		bool2=true
		condsignal(condid)
			while bool1<>true
		condwait(condid,blocker)
		wend
		bool1=false
		mutexunlock blocker
	#endif
	'dsp_change(rln)
	linecur_change(rln)
	rlinecur=rln
End Sub
