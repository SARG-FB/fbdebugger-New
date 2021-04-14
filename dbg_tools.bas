''tools for fbdebugger_new
''dbg_tools.bas


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
		dump_sh()
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
'=======================================================
'' lists selected var with the children to log
'=======================================================
private sub procvar_list() 
	Dim text As String, pr As Integer
	Dim As Integer hitem,temp,level,iparent,inext
	Dim As hwnd hedit
	
	'get current hitem in tree
	hitem=GetItemTreeView(GTVIEWVAR)
	
	While hitem<>0
		GetTextTreeView(GTVIEWVAR,hitem)
		'#Ifdef fulldbg_prt
			dbg_prt(Space(level*4)+text)
		'#EndIf
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
End Sub
'=================================================
'' shows string zstring
'=================================================
private sub string_show(stringadr as integer)
	dim as integer f,inc
	dim as byte buf(32004)
	dim as string text
	inc=32000
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
	buf(f-stringadr+1)=0 'end of string if length >32000
	text=*cast(zstring ptr,@buf(0))
	SetGadgetText(GEDITOR,text)
end sub
'=================================================
'' shows wstring
'=================================================
private sub wstring_show(stringadr as integer)
	dim wstrg As WString *32001,bufw As UShort
	dim as integer inc
	messbox("Something missing","Need to open a window  for wstring")
	inc=0:wstrg=""
	ReadProcessMemory(dbghand,Cast(LPCVOID,stringadr),@bufw,2,0)
	While bufw
		wstrg[inc]=bufw
		inc+=1
		If inc=32000 Then Exit While 'limit if wstring >32000
		ReadProcessMemory(dbghand,Cast(LPCVOID,stringadr+inc*2),@bufw,2,0)
	Wend
	WStrg[inc]=0 'end of wstring
	SetGadgetText(GEDITOR,wstrg)
end sub
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
		thid=ValInt(Mid(text,10,6))
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
    line_display(rline(thline).nu-1) ''Select Line
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
	If messbox("Killing thread : "+Str(thread(t).id),"Are you sure ?"+Chr(10)+"It could cause Memory leak, etc.",MB_YESNO)=RETYES Then
		#ifdef __fb_win32__
			If terminatethread(thread(t).hd,999)=0 Then
				messbox("Thread killing","Something goes wrong, error: "+Str(GetLastError))
			EndIf
		#else
			messbox("Feature not coded","thread_kill for linux, maybe make a sub")
		#endif
	EndIf
End Sub
'=======================================
''
'=======================================
private sub bcktrk_launch(pr As Integer,typ As Integer)''typ=1 -->simple backtracking / typ=2 -->full backtracking or chaining

	messbox("Feature not yet coded","bcktrk_launch")

	'If bcktrkbx<>0 Then Exit Sub
	'bcktrkpr=pr
	'If typ=1 Then
		'fb_Dialog(@bcktrk1_box,"Proc Backtracking",windmain,300,100,190,40)
	'Else
		'fb_Dialog(@bcktrk2_box,"Proc call chaining",windmain,300,500,300,900)
	'EndIf
End Sub
'===============================================================
'' 1 juste calling / 2 backtracking / 3 chaining
'===============================================================
private sub proc_loccall(typ As Integer=1)
	Dim As Integer hitem,temp,gadget
	'get current hitem in tree
	gadget=PanelGadgetGetCursel(GRIGHTTABS)
	temp=GetItemTreeView(gadget)

	If temp=procr(1).tv AndAlso typ=1 Then
			messbox("Locate calling line","      Main so no call !!")
		Exit Sub 	
	EndIf

	Do 'search index proc
		hitem=temp
		temp=GetParentItemTreeView(gadget,hitem)
	Loop While temp

	For i As Integer =1 To procrnb
		If procr(i).tv=hitem Then
			If typ=1 Then
				If procr(i).cl=-1 Then 
					'fb_message("Locate calling line","First proc of thread so no call !!"):Exit Sub
					thread_execline(2):Exit Sub
				EndIf
				temp=procr(i).cl 'calling line
				source_change(rline(temp).sx) ''display source
				line_display(rline(temp).nu-1)'Select Line
			ElseIf typ=2 Then
				bcktrk_launch(i,1)
			ElseIf typ=3 Then
				bcktrk_launch(i,2)
			EndIf
			Exit Sub
  		EndIf
	Next
	If typ=2 Then
		messbox("Locate calling line","Impossible to find with Global shared")
	elseIf typ=2 Then
		bcktrk_launch(1,1)
	ElseIf typ=3 Then
		bcktrk_launch(1,2)
	EndIf
End Sub
'===================================================================================
'' check if the line is executable return -1 if false otherwise the rline index
'===================================================================================
private function line_exec(pline as integer)as integer
	For iline as integer =1 To linenb
		If rline(iline).nu=pline AndAlso rline(iline).sx=srcdisplayed Then
			return iline
		end if
	next
	return -1
end function
'=======================================================
'' shows address in memory of line where is the cursor
'========================================================
private sub line_adr() 
Dim As Integer l, rl

l=line_cursor()
rl=line_exec(l)

if rl=-1 Then
	messbox("Line memory address","Not executable so no address")
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
'' displays the inputval box
'=======================================================
private sub input_bx(msg as string)
	hidewindow(hinputbx,0)
	SetWindowText(hinputbx,msg)
	SetGadgetText(GINPUTVAL,inputval)
end sub
'=========================================================
'' translates code message to the text (only windows)
'=========================================================
private sub winmsg()' winmessage
	Dim Buffer As String*210
	inputval=""
	inputtyp=5
	input_bx("Window message number")
	
	If inputval<>"" Then
		'Format the message string
		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, ByVal 0,ValInt(inputval) , LANG_NEUTRAL, Buffer, 200, ByVal 0)
		messbox("Windows message","Code : "+inputval+Chr(10)+"Message : "+buffer) 
	End If
End Sub
'===============================
'' shows value in dec/hex/bin
'===============================
private sub dechexbin()
	inputval=""
	inputtyp=99
	input_bx("Input value HEX(&h) or DEC")
	If inputval<>"" Then
		messbox("Value in dec, hex and bin","Dec= "+Str(Val(inputval))+Chr(10)+"Hex="+Hex(Val(inputval))+Chr(10)+"Bin="+Bin(Val(inputval)))
	End If
End Sub
'===============================
''Goto selected line number
'===============================
private sub line_goto() 
	Dim Linenb As Integer
	inputval=""
	inputtyp=99
	linenb=line_cursor() ''get line zero based
	input_bx("Current line "+Str(linenb)+", Goto line ?")
	Linenb=ValInt(inputval)-1
	If linenb>=0 Then
	   line_display(linenb)
	End If
End Sub
'=======================================
'' lists processes ''todo win32 only
'=======================================
private sub process_list()
	Dim prcinfo As PROCESSENTRY32,snap As HANDLE
	dim as string text
	snap=CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0)'Take snapshot of running processes
	If snap <> INVALID_HANDLE_VALUE Then
		prcinfo.dwSize=SizeOf(PROCESSENTRY32)
		text="file Process name     ID  Nthread parent id"+Chr(13)+Chr(10)  
		If Process32First (snap,@prcinfo) Then
			Do
				text+=fmt(prcinfo.szExeFile,20)+fmt(Str(prcinfo.th32ProcessID),5)+fmt(Str(prcinfo.cntThreads),3)+fmt(Str(prcinfo.th32ParentProcessID),5)+Chr(13)+Chr(10)    
			Loop While  Process32Next (snap,@prcinfo)
		Else
			messbox("Process list error","Failed to create process list!")
		End If
		CloseHandle (snap)
	End If
	SetGadgetText(GEDITOR,text)
end sub
'=================================================
'' shows log file
'=================================================
private sub log_show()
		Dim f As Integer,buffer As string, l As Integer,flaguse As Integer
	If (flaglog And 2) Then 'close if needed
		flaglog And=1
		dbg_prt(" $$$$___CLOSE ALL___$$$$ ") ''todo  sub close
		flaguse=1
	EndIf

	If Dir(ExePath+"\dbg_log_file.txt")="" Then
		messbox("Display dbg_log_file.txt","File doesn't exist")
		Exit Sub
	EndIf

	f = FreeFile 
	Open ExePath+"\dbg_log_file.txt" For Binary As #f 
	l=Lof(f)
	buffer = space(l)
	Get #f,,buffer ''get file
	Close #f

	buffer=left(buffer,l)
	SetGadgetText(GEDITOR,buffer)

	If flaguse Then flaglog Or=2
End Sub
'=================================================
'' deletes log file if it exists
'=================================================
private sub log_del()
	Dim savflog As Integer=flaglog       'save see below
	flaglog=flaglog And 1                'change the value but keeps screen output
	dbg_prt(" $$$$___CLOSE ALL___$$$$ ") 'close the file if needed todo do a sub for closing by spliting the code
	Kill (ExePath+"\dbg_log_file.txt")   'delete it
	flaglog=savflog 'restore the value to keep the use of file log
End Sub
'=============================================
private sub bcktrk_close()
	If htrckbx Then 
		hidewindow(htrckbx,KHIDE)
	EndIf
End Sub
'=============================================
'' updates all the select index boxes
'=============================================
private sub index_update()
	'messbox("To be coded improvement","updates all the index boxes")
	'For iindex As Long =0 To INDEXBOXMAX
		'If hindexbx(iindex)<>0 Then
			'If autoupd(iindex)=TRUE Then 
				'' index_update(iindex)
			'EndIf
		'EndIf	
	'Next
	''for now only one index selection box
	If hindexbx<>0 Then
		If autoupd=TRUE Then 
			'todo index_update(iindex)
		EndIf
	EndIf
end sub
'======================================
'' changes the sign for some datatypes
'======================================
private sub dump_sign()
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
	dump_sh()
End Sub
'======================================
'' 
'======================================
private sub dump_sh()
	Dim As String tmp
	Dim buf(16) As UByte,r As Integer,ad As UInteger
	Dim ascii As String
	Dim ptrs As pointeurs
	Dim As Long errorformat
	
	DeleteListViewItemsAll(GDUMPMEM) ''delete all items
	ad=dumpadr
	For jline as integer =0 To dumplines-1
		AddListViewItem(GDUMPMEM,str(ad),0,jline,0) ''adress
		ReadProcessMemory(dbghand,Cast(LPCVOID,ad),@buf(0),16,@r)
		ad+=r
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
				tmp=Right("0"+Hex(*ptrs.pbyte),2)
				ptrs.pbyte+=1
			 Case 55,56 'short/hex
				tmp=Right("000"+Hex(*ptrs.pshort),4)
				ptrs.pshort+=1
			 Case 51,58 'integer/hex
				tmp=Right("0000000"+Hex(*ptrs.pinteger),8)
				ptrs.pinteger+=1
			Case 59,60 'longinteger/hex
				tmp=Right("000000000000000"+Hex(*ptrs.plongint),16)
				ptrs.pulongint+=1
			Case Else
				tmp="Error"
				errorformat=1
				Exit for
		  End Select
		  AddListViewItem(GDUMPMEM,tmp,0,jline,icol)
		Next
		ascii=""
		For ibuf as integer=1 To 16
		  If buf(ibuf-1)>31 Then
			  ascii+=Chr(buf(ibuf-1))
		  Else
			  ascii+="."
		  End If
		Next
		AddListViewItem(GDUMPMEM,ascii,0,jline,dumpnbcol+1)
	Next 
	If errorformat Then messbox("Error format="+str(dumptyp+dumpbase),"Impossible to display single or double in hex"+Chr(13)+"Retry with another format")
End Sub
'==========================================
'' finds the calling line for proc
'==========================================
private function line_call(regip As UInteger) As Integer 
	For i As Integer=1 To linenb
		If regip<=rLine(i).ad Then
			Return i-1
		EndIf
	Next
	Return linenb	
End Function
'===================================================
'' checks if inputval is in the datatype range
'===================================================
private sub input_check()
	dim as boolean vflag=true
	dim as double vald
	vald=Val(inputval)
	Select Case inputtyp
		Case 2
		If vald<-128 Or vald>127 Then setwindowtext(hinputbx,"min -128,max 127"):vflag=false
		Case 3
		If vald<0 Or vald>255 Then setwindowtext(hinputbx,"min 0,max 255"):vflag=false
		Case 5
		If vald<-32768 Or vald>32767 Then setwindowtext(hinputbx,"min -32768,max 32767"):vflag=false
		Case 6
		If vald<0 Or vald>65535 Then setwindowtext(hinputbx,"min 0,max 65535"):vflag=false
		Case 1
		If vald<-2147483648 Or vald>2147483648 Then setwindowtext(hinputbx,"min -2147483648,max +2147483647"):vflag=false
	Case 7,8
		If vald<0 Or vald>4294967395 Then setwindowtext(hinputbx,"min 0,max 4294967395"):vflag=false
	End Select
	If vflag Then hidewindow(hinputbx,KHIDE) ''hide the window if value is good
end sub
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
private sub var_dump(tv As HWND,ptd As Long =0) 'dump variable '28/11/2014

	If var_find2(tv)=-1 Then Exit Sub 'search index variable under cursor

	dumpadr=varfind.ad

	If ptd Then 'dumping pointed data
		If varfind.pt=0 Then messbox("Dumping pointed data","The selected variable is not a pointer"):Exit Sub
		ReadProcessMemory(dbghand,Cast(LPCVOID,dumpadr),@dumpadr,4,0)
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
			 ReadProcessMemory(dbghand,Cast(LPCVOID,dumpadr),@dumpadr,4,0)'string address
		Case 4,14 'f or zstring
			dumptyp=2
		Case Is>TYPESTD
			 dumptyp=8 'default for pudt and any
	   End Select
	End If
	PanelGadgetSetCursel(GRIGHTTABS,TABIDXDMP)
	dump_set()
	dump_sh()

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
	            		If .mem <>4 Then adr=0:WriteProcessMemory(dbghand,Cast(LPVOID,vrr(vrrnb).ini+SizeOf(Integer)),@adr,SizeOf(Integer),0) '05/05/2014 25/07/2015 64bit
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
       For idx =1 To SHWEXPMAX
    		If shwexp(idx).tv=tv Then Exit For 'found index matching tview
       Next 
		'get current hitem in tree
		hitem=GetItemTreeView(ID_In_Number(tv))
        For i As Integer = 1 To shwexp(idx).nb 'search index variable
            If vrp(idx,i).tl=hitem Then
				varfind.nm=vrp(idx,i).nm
				If varfind.nm="" Then varfind.nm="<Memory>"
                varfind.ty=vrp(idx,i).Ty
                varfind.pt=vrp(idx,i).pt
                varfind.ad=vrp(idx,i).ad
                varfind.tv=tv 'handle treeview
                varfind.tl=hitem 'handle line
                varfind.iv=-1
                Return i
            End If
        Next
    End If
End Function
'=====================================================================
private function proc_find(thid As Integer,t As Byte) As Integer 'find first/last proc for thread
	If t=KFIRST Then
		For i As Integer =1 To procrnb
			If procr(i).thid = thid Then Return i
		Next
	Else
		For i As Integer = procrnb To 1 Step -1
			If procr(i).thid = thid Then Return i
		Next
	End If
End Function
'===================================
private function proc_retval(prcnb As Integer) As String
	Dim p As Integer = proc(prcnb).pt
	If p Then
     	If p>220 Then
			Return String(p-220,"*")+" Function"
		ElseIf p>200 Then 
			Return String(p-200,"*")+" Sub"
		Else
			Return String(p,"*")+" "+udt(proc(prcnb).rv).nm
     	End If
	End If
	Return udt(proc(prcnb).rv).nm
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
end sub
'===========================================
'' restore instruction and resume thread
'===========================================
Private sub thread_resume()
	#ifdef __fb_win32__
		''moved to dbg_windows.bas
		messbox("addr restoing="+str(rLine(thread(threadcur).sv).ad),"value="+str(rLine(thread(threadcur).sv).sv))
		writeprocessmemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@rLine(thread(threadcur).sv).sv,1,0) 'restore old value for execution
		resumethread(threadhs)
	#else
		''LINUX, maybe moved in dbg_linux.bas
		messbox("For Linux","thread_resume() needed to be added")
	#endif
End sub
'====================================================================
'' locates a proc in sources from selected line in current treeview
'====================================================================
private sub proc_loc()
	Dim As Integer hitem,temp,t,gadget
	'get current hitem in tree
	gadget=PanelGadgetGetCursel(GRIGHTTABS)
	temp=GetItemTreeView(gadget)
	If gadget=GTVIEWVAR Then
		Do 'search index proc
			hitem=temp
			temp=GetParentItemTreeView(GTVIEWVAR,hitem)
		Loop While temp
		temp=0
		For i As Integer =1 To procrnb
			If procr(i).tv=hitem Then
				temp=procr(i).idx
	         Exit For               
			EndIf
		Next
		If temp=0 Then
			messbox("Locate proc","Global variables no proc associated !!")
			Exit Sub
		EndIf
	ElseIf gadget=GTVIEWPRC Then
		hitem=temp
		For i As Integer =1 To procnb
			If proc(i).tv=hitem Then
				temp=i
	         Exit For               
			EndIf
		Next
	ElseIf gadget=GTVIEWTHD Then
		t=thread_select()
		If t=0 Then ''main, select first line
			temp=procr(1).idx
		Else
			temp=procr(proc_find(thread(t).id,KFIRST)).idx
		EndIf
	EndIf
	If proc(temp).nu=-1 Then 
		messbox("Locate proc","Not possible perhaps add by compiler (ie default constructor, let, etc)")
		Exit sub
	EndIf
	
	source_change(proc(temp).sr) ''display source
	line_display(proc(temp).nu-1)  ''Select Line
End Sub
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
	ElseIf t=3 Then'backtracking
		bcktrk_launch(pr,1)
	ElseIf t=4 Then'chaining
		bcktrk_launch(pr,2)	
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
	For i As Integer=lo To hi
		thid=thread(i).id
		p=proc_find(thid,KLAST)
		libel="threadID="+fmt2(Str(thid),6)+" : "+proc(procr(p).idx).nm
		If flagverbose Then
			libel+=" HD: "+Str(thread(i).hd)
		EndIf
		If threadhs=thread(i).hd Then
			libel+=" (next execution)"
		EndIf
		RenameItemTreeView(GTVIEWTHD,thread(i).tv,libel)
	Next
End Sub
'=======================================================================================
private sub thread_change(th As Integer =-1)
	Dim As Integer t,s
	If th=-1 Then
		t=thread_select()
	Else
		t=th
	EndIf
	s=threadcur
	'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@breakcpu,1,0) 'restore CC previous line current thread
	threadcur=t:threadprv=t 
	'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@rLine(thread(threadcur).sv).sv,1,0) 'restore old value for execution selected thread
	threadhs=thread(threadcur).hd
	procsv=rline(thread(threadcur).sv).px
	thread_text(t)
	thread_text(s)
	threadsel=threadcur
	dsp_change(thread(threadcur).sv)
End Sub

'===========================================================
private sub proc_sh()
	Dim libel As String

	'For iproc As Integer =1 To procnb
		'DeleteTreeViewItem(GTVIEWPRC,proc(iproc).tv)
	'next
	DeleteTreeViewItem(GTVIEWPRC,getparentitemtreeview(GTVIEWPRC,proc(1).tv)) ''delete all by root
	
	If procsort=KMODULE Then 'sorted by module
		messbox("feature not coded","sort by module for procs so forcing by name")
		procsort=KPROCNM
	end if
	
	For j As Integer =1 To procnb
		With proc(j)
			if .st=1 then
				libel="F> " '' for indicating if the proc is followed
			else
				libel=""
			EndIf
			If procsort=KMODULE Then 'sorted by module
				'libel+=name_extract(source(.sr))+">> "+.nm+":"+proc_retval(j)
			Else 'sorted by proc name
				libel+=.nm+":"+proc_retval(j)+"   in : "+source_name(source(.sr))
			EndIf
			If flagverbose Then libel+=" ["+Str(.db)+"]"
			.tv=AddTreeViewItem(GTVIEWPRC,libel,cast (hicon, 0),0,TVI_LAST,0)
		End With
	Next
End Sub
'=====================================================
private function proc_name(ad As UInteger) As String 'find name proc using address
	For i As Integer =1 To procnb
		If proc(i).db=ad Then Return Str(ad)+" >> "+proc(i).nm
	Next
	Return Str(ad)
End Function
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
	#If __FB64BIT__
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
      If flagverbose Then varlib+="[sz "+Str(udt(t).lg)+" / "+sOffset+Str(pany)+"]"
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
         		If pany Then ReadProcessMemory(dbghand,Cast(LPCVOID,pany),@recup(0),4,0)
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
'============================================================
'' manages editing of var (src=0) or mem (src=1)
'============================================================
private sub edit_fill(txt as string,adr as integer,typ as integer, pt as integer)
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
         If .typ>TYPESTD Then update_address(i) ''udt case
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
		RenameItemTreeView(GTVIEWWCH,wtch(i).tvl,libel)
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
					If MESSBOX("Tracing var/mem","No log output defined"+Chr(13)+"Open settings ?",MB_YESNO)=RETYES Then 
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
private sub watch_addtr
	wtchnew=-1
	If var_find2(htviewvar)<>-1 Then
		watch_set()
		If wtchnew<>-1 Then watch_trace(wtchnew)
	EndIf
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
		p=InStr(brks,",")'parsing
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
	'todo windowhide(hbrk_bx,0) fb_MDialog(@brk_manage,"Restart debuggee, managing breakpoints",windmain,500,8,330,170)
	SetStateMenu(HMenusource,MNMNGBRK,0)
EndIf
End Sub 
'============================================
Private sub brk_sav
	For i As Integer =1 To BRKMAX
		If i<=brknb Then
			brkexe(0,i)=source_name(source(brkol(i).isrc))+","+Str(brkol(i).nline)+","+Str(brkol(i).cntrsav)+","+Str(brkol(i).typ) '03/09/2015
		EndIf
	Next
End Sub
'================================================
 '' deletes one breakpoint and compresses data
'================================================
private sub brk_del(n As Integer)
	brkol(n).typ=0
	brkol(n).cntrsav=0
	brk_marker(n)
	if n=0 then ''run or fast run
		exit sub
	EndIf
	brknb-=1
	For i As Integer =n To brknb
		brkol(i)=brkol(i+1)
	Next
   If brknb=0 Then SetStateMenu(HMenusource,MNMNGBRK,1)
End Sub
'==============================================
Private function brk_test(ad As UInteger) As Byte 'check on breakpoint ?
 For i As Integer=0 To brknb
 	If brkol(i).typ>2 Then Continue For 'disabled 
 	If ad=brkol(i).ad Then 'reached line = breakpoint
 		If brkol(i).counter>0 Then brkol(i).counter-=1:Return FALSE 'decrement counter 02/09/2015
 		stopcode=CSBRK 
 		If i=0 Then
 			brkol(0).ad=0 'delete continue to cursor
 			stopcode=CSCURSOR
 		Else
 			If brkol(i).typ=2 Then brk_del(i):stopcode=CSBRKTEMPO 'tempo breakpoint
 		End If
 		Return TRUE
 	End If
 Next
 Return FALSE
End Function
'=======================================================================
'' t 1=permanent breakpoint / 2=tempo breakpoint / 3=breakpoint with counter by changed in type 1 / 
''   4=disable / 7=change value counter / 8 reset to initial value / 9=same line
'=======================================================================
Private sub brk_set(t As Integer)
	Dim l As Integer,i As Integer,ln As Integer

	l=line_cursor() 'get line

	For i=1 To linenb
		If rline(i).nu=l And rline(i).sx=srcdisplayed Then Exit For 'check nline 
	Next
	If i>linenb Then messbox("Break point Not possible","Inaccessible line (not executable)") :Exit Sub
	For j As Integer =1 To procnb
		If rline(i).ad=proc(j).db Then messbox("Break point Not possible","Inaccessible line (not executable)") :Exit Sub
	Next
	ln=i
	If t=9 Then 'run to cursor
		'l N°line/by 0
		If linecur=l And srcdisplayed=srccur Then
			If messbox("Run to cursor","Same line, continue ?",MB_YESNO)=RETNO Then Exit Sub
		End If
		brkol(0).ad=rline(ln).ad
		brkol(0).typ=2 ''tempo so cleared when reached
		runtype=RTRUN
		but_enable()
		brkol(brknb).nline=l
		brk_marker(0)
		thread_resume()
	Else
		For i=1 To brknb 'search if still put on this line
			If brkol(i).nline=l And brkol(i).isrc=srcdisplayed Then Exit For
		Next
		If i>brknb Then 'not put
			If brknb=BRKMAX Then messbox("Max of brk reached ("+Str(BRKMAX)+")","Delete one and retry"):Exit Sub
			brknb+=1
			brkol(brknb).nline=l
			brkol(brknb).typ=t
			brkol(brknb).index=ln
			brkol(brknb).isrc=srcdisplayed
			brkol(brknb).ad=rline(ln).ad
			brkol(brknb).cntrsav=0
			brkol(brknb).counter=0
			If t=3 Then 'change value counter
				inputval="0"
				inputtyp=7 'ulong 
				input_bx("Set value counter for a breakpoint")
				brkol(brknb).counter=ValUInt(inputval)
				brkol(brknb).cntrsav=brkol(i).counter
				brkol(brknb).typ=1 'forced permanent
			EndIf
	    Else 'still put
			If t=7 Then 'change value counter
				inputval=Str(brkol(i).cntrsav)
				inputtyp=7 'ulong 
				input_bx("Change value counter, remaining= "+Str(brkol(i).counter)+" initial below")
				If inputval="" Then inputval=Str(brkol(i).cntrsav) 'cancel button selected so no value
				brkol(i).counter=ValUInt(inputval)
				brkol(i).cntrsav=brkol(i).counter
			ElseIf t=8 Then 'reset to initial value
				If brkol(i).cntrsav Then
					brkol(i).counter=brkol(i).cntrsav
				Else
					messbox("Reset counter","No counter for this breakpoint")
				EndIf
			ElseIf t=4 Then 'toggle enabled/disabled
				If brkol(i).typ>2 Then
					brkol(i).typ-=2
				Else
					brkol(i).typ+=2
				EndIf
		ElseIf t=brkol(i).typ OrElse brkol(i).typ>2 Then 'cancel breakpoint
			brk_del(i)
			Exit Sub
		Else 'change type of breakpoint
			brkol(i).typ=t
			End If
		End If
		
		brk_marker(i)
		
	   If brknb=1 Then SetStateMenu(HMenusource,MNMNGBRK,0)
	End If
End Sub
'=============================================================
private sub proc_watch(procridx As Integer) 'called with running proc index
	Dim As Integer prcidx=procr(procridx).idx,vridx
	If wtchcpt=0 Then Exit Sub
	For i As Integer= 0 To WTCHMAX
	   If wtch(i).psk=-3 Then 'local var
			If wtch(i).idx=prcidx Then 
			   wtch(i).adr=vrr(procr(procridx).vr+wtch(i).dlt).ad
			   wtch(i).psk=procr(procridx).sk
			EndIf
	   ElseIf wtch(i).psk=-4 Then 'session watch
		If wtch(i).idx=prcidx Then
				vridx=var_search(procridx,wtch(i).vnm(),wtch(i).vnb,wtch(i).var,wtch(i).pnt)
				If vridx=-1 Then
					messbox("Proc watch","Running var not found")
					Continue For
				EndIf
				var_fill(vridx)
			  watch_add(wtch(i).tad,i)
		EndIf
	   EndIf
	Next
End Sub
'=======================================
'' new procedure sub ou function
'=======================================
private sub proc_new()
	Dim libel As String
	Dim tv As integer
	If procrnb=PROCRMAX Then hard_closing("Max number of sub/func reached limit="+str(PROCRMAX))
	procrnb+=1'new proc ADD A POSSIBILITY TO INCREASE THIS ARRAY
	procr(procrnb).sk=procsk
	procr(procrnb).thid=thread(threadcur).id
	procr(procrnb).idx=procsv
	
	'test if first proc of thread
	If thread(threadcur).plt=0 Then
		procr(procrnb).cl=-1  ''no real calling line
		libel="ThID="+Str(procr(procrnb).thid)+" "
		thread(threadcur).tv=AddTreeViewItem(GTVIEWTHD,"Not filled",cast (hicon, 0),0,TVI_LAST,0)
		thread(threadcur).ptv=thread(threadcur).tv ''last proc 
		thread_text() ''put text not only current but all to reset previous thread text
		tv=TVI_LAST ''insert in last position
	Else
		procr(procrnb).cl=thread(threadcur).od
		tv=thread(threadcur).plt 'insert after the last item of thread
	EndIf
		
	'add manage LIST
	If flagtrace Then dbg_prt ("NEW proc "+Str(procsv)+" "+proc(procsv).nm)
	libel+=proc(procsv).nm+":"+proc_retval(procsv)
	If flagverbose Then libel+=" ["+Str(proc(procsv).db)+"]"

	procr(procrnb).tv=AddTreeViewItem(GTVIEWVAR,libel,cast (hicon, 0),0,tv,0)
	thread(threadcur).plt=procr(procrnb).tv 'keep handle last item
	
	'add new proc to thread treeview
	thread(threadcur).ptv=AddTreeViewItem(GTVIEWTHD,"Proc : "+proc(procsv).nm,cast (hicon, 0),0,TVI_FIRST,thread(threadcur).ptv)
	thread_text(threadcur)
	var_ini(procrnb,proc(procr(procrnb).idx).vr,proc(procr(procrnb).idx+1).vr-1)
	procr(procrnb+1).vr=vrrnb+1
	If proc(procsv).nm="main" Then 
		procr(procrnb).vr=1 'constructors for shared are executed before main so reset index for first variable of main 04/02/2014
	EndIf
	proc_watch(procrnb) 'reactivate watched var
End Sub
'=============================
'' end of proc
'=============================
Private sub proc_end()
Dim As Long limit=-1
Var thid=thread(threadcur).id
'find the limit for deleting proc (see below different cases)
For j As Long =procrnb To 1 Step -1
	If procr(j).thid =thid  Then
	   If limit=-1 Then limit=j
	   If procr(j).idx=procsv Then
	   	If j<>limit Then limit=j+1
	   	Exit For
	   EndIf
	EndIf  
Next
''delete all elements (treeview, varr, ) until the limit
For j As Long =procrnb To limit Step -1
   If procr(j).thid = thid Then
		proc_del(j)
	End If
Next

If flagtrace Then dbg_prt ("RETURN to proc "+proc(procsv).nm)

'' The removing of procs is done after the end is eached AND when a next line is executed. It could be a simple line or a call to a new proc.
'' in this last case it's a bit complicated.

'' handle case multiple returns in a recursive proc, breakcpu not restored between each execution....
'see example below, the first test data would not be deleted
'function test(a As integer) As integer
'	If a=2 Then
'		Return test(1)
'	EndIf
'End function
'test(2)

''4 cases :
'' index    1         2            3                       also 3       limit
''normal   main --> mysub                                               >> 2
''recursif main --> test -->      test                                     2 
''         main --> mysub --> constructor then immediatly destructor       3
''         main --> destructor then immediatly       same destructor       3

End Sub
'========================================================
private sub var_sh() 'show master var
    For i As Integer =1 To vrrnb
    	RenameItemTreeView(GTVIEWVAR,vrr(i).tv,var_sh1(i))
    Next
   watch_array()
   watch_sh
End Sub
'======================================================
'' handles display at end of run or when running auto
'======================================================
private sub dsp_change(index As Integer)
	linecur_change(index)
	If flagtrace And 2 Then dbg_prt(LTrim(line_text(linecur-1),Any " "+Chr(9)))
	If runtype=RTAUTO Then
		watch_array() 'update adr watched dyn array
		watch_sh()    'update watched but not all the variables
	ElseIf runtype=RTSTEP Then
		if flagupdate=true then
			var_sh()
			dump_sh()
		else
			watch_array() ''even flagupdate is off watched are updated
			watch_sh()
		endif
		but_enable()
		If PanelGadgetgetCursel(GRIGHTTABS) = TABIDXPRC Then
			proc_sh()
		elseIf PanelGadgetgetCursel(GRIGHTTABS) = TABIDXTHD Then
			thread_text()
		EndIf
		index_update()
	End If
End Sub
'========================================================================
private sub brkv_set(a As Integer) ''break on variable change
	Dim As Integer t,p
	Dim Title As String, j As UInteger,ztxt As string,tvi As integer
	If a=0 Then 'cancel break
		brkv.adr=0
		SetGadgetText(GBRKVAR,"Break on var")
		Modify_Menu(MNVARBRK,HMenuvar,"Break on var")
		Exit Sub
	ElseIf a=1 Then 'new
		If var_find2(htviewvar)=-1 Then Exit Sub 'search index variable under cursor
		'search master variable

		t=varfind.Ty
		p=varfind.pt

		#Ifdef __FB_64BIT__ 
		If p Then t=9 ''pointer integer64 (ulongint)
		#Else
		If p Then t=1 ''pointer integer32 (long)
		#EndIf

		If t>10 AndAlso p=0 AndAlso t<>4 AndAlso t<>13 AndAlso t<>14 Then
		messbox("Break on var selection error","Only [unsigned] Byte, Short, integer, longint or z/f/string")
		Exit Sub
	End If


		brkv2.typ=t           'change in brkv_box if pointed value
		brkv2.adr=varfind.ad   'idem
		brkv2.vst=""          'idem
		brkv2.tst=1           'type of test
		brkv2.ivr=varfind.iv
		' if dyn array store real adr
		If Cast(Integer,vrb(varfind.pr).arr)=-1 Then
			ReadProcessMemory(dbghand,Cast(LPCVOID,vrr(varfind.iv).ini),@brkv2.arr,4,0)
		Else
			brkv2.arr=0
		End If

		If vrb(varfind.pr).mem=3 Then
			brkv2.psk=-2 'static
		Else
			For j As UInteger = 1  To procrnb 'find proc to delete watching
				If varfind.iv>=procr(j).vr And varfind.iv<procr(j+1).vr Then brkv2.psk=procr(j).sk:Exit For
			Next
		End If
		ztxt=GetTextTreeView(GTVIEWVAR,vrr(varfind.iv).tv)
	Else 'update
		brkv2=brkv
		ztxt=GetTextTreeView(GTVIEWVAR,vrr(varfind.iv).tv)
	End If
	brkv2.txt=Left(ztxt,InStr(ztxt,"<"))+var_sh2(brkv2.typ,brkv2.adr,p)

	'todo fb_MDialog(@brkv_box,"Test for break on value",windmain,283,25,350,50)

End Sub
'===================== break on var ===============================================================
private function brkv_test() As Byte
	Dim recup(20) As Integer,ptrs As pointeurs,flag As Integer=0
	Dim As Integer adr,temp2,temp3
	Dim As String strg1,strg2,strg3
		ptrs.pxxx=@recup(0)
	   
		If brkv.arr Then 'watching dyn array element ?
			adr=vrr(brkv.ivr).ini
			ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@adr,4,0)
			If adr<>brkv.arr Then brkv.adr+=brkv.arr-adr:brkv.arr=adr 'compute delta then add it if needed
			temp2=vrr(brkv.ivr).ini+2*SizeOf(Integer) 'adr global size 25/07/2015 64bit
			ReadProcessMemory(dbghand,Cast(LPCVOID,temp2),@temp3,SizeOf(Integer),0)
			If brkv.adr>adr+temp3 Then 'out of limit ?
				brkv_set(0) 'erase
				Return FALSE 
			End If
		End If    
		''26 --> <> or > or >=
		''21 --> <> or < or <=
		''35 --> = or >= or <=
		''16 --> <>
		Select Case brkv.typ
		Case 2 'byte
			ReadProcessMemory(dbghand,Cast(LPCVOID,brkv.adr),@recup(0),1,0)
			If brkv.val.vbyte>*ptrs.pbyte Then
				If 26 And brkv.ttb Then flag=1
			ElseIf brkv.val.vbyte<*ptrs.pbyte Then
				If 21 And brkv.ttb Then flag=1
			ElseIf brkv.val.vbyte=*ptrs.pbyte Then
				If 35 And brkv.ttb Then flag=1
			End If
		Case 3 'ubyte
			ReadProcessMemory(dbghand,Cast(LPCVOID,brkv.adr),@recup(0),1,0)
			If brkv.val.vubyte<*ptrs.pubyte Then
				If 26 And brkv.ttb Then flag=1
			ElseIf brkv.val.vubyte>*ptrs.pubyte Then
				If 21 And brkv.ttb Then flag=1
			ElseIf brkv.val.vubyte=*ptrs.pubyte Then 
				If 35 And brkv.ttb Then flag=1
			End If
		Case 5 'short
			ReadProcessMemory(dbghand,Cast(LPCVOID,brkv.adr),@recup(0),2,0)
			If brkv.val.vshort<*ptrs.pshort Then
				If 26 And brkv.ttb Then flag=1
			ElseIf brkv.val.vshort>*ptrs.pshort Then
				If 21 And brkv.ttb Then flag=1
			ElseIf brkv.val.vshort=*ptrs.pshort Then
				If 35 And brkv.ttb Then flag=1
			End If
		Case 6 'ushort
			ReadProcessMemory(dbghand,Cast(LPCVOID,brkv.adr),@recup(0),2,0)
			If brkv.val.vushort<*ptrs.pushort Then
				If 26 And brkv.ttb Then flag=1
			ElseIf brkv.val.vushort>*ptrs.pushort Then
				If 21 And brkv.ttb Then flag=1
			ElseIf brkv.val.vushort=*ptrs.pushort Then
				If 35 And brkv.ttb Then flag=1
			End If
		Case 1 'integer32/long
			ReadProcessMemory(dbghand,Cast(LPCVOID,brkv.adr),@recup(0),4,0)
			If brkv.val.vinteger<*ptrs.pinteger Then
				If 26 And brkv.ttb Then flag=1
			ElseIf brkv.val.vinteger>*ptrs.pinteger Then
				If 21 And brkv.ttb Then flag=1
			ElseIf brkv.val.vinteger=*ptrs.pinteger Then
				If 35 And brkv.ttb Then flag=1
			End If
		Case 8 'uinteger32/ulong       pointer
			ReadProcessMemory(dbghand,Cast(LPCVOID,brkv.adr),@recup(0),4,0)
			If brkv.val.vuinteger<*ptrs.puinteger Then
				If 26 And brkv.ttb Then flag=1
			ElseIf brkv.val.vuinteger>*ptrs.puinteger Then
				If 21 And brkv.ttb Then flag=1
			ElseIf brkv.val.vuinteger=*ptrs.puinteger Then
				If 35 And brkv.ttb Then flag=1
			End If
		Case 9 'integer64/longint
			ReadProcessMemory(dbghand,Cast(LPCVOID,brkv.adr),@recup(0),8,0)
			If brkv.val.vlongint<*ptrs.plongint Then
				If 26 And brkv.ttb Then flag=1
			ElseIf brkv.val.vlongint>*ptrs.plongint Then
				If 21 And brkv.ttb Then flag=1
			ElseIf brkv.val.vlongint=*ptrs.plongint Then
				If 35 And brkv.ttb Then flag=1
			End If
		Case 10 'uinteger64/ulonginit       pointer
			ReadProcessMemory(dbghand,Cast(LPCVOID,brkv.adr),@recup(0),8,0)
			If brkv.val.vulongint<*ptrs.pulongint Then
				If 26 And brkv.ttb Then flag=1
			ElseIf brkv.val.vulongint>*ptrs.pulongint Then
				If 21 And brkv.ttb Then flag=1
			ElseIf brkv.val.vulongint=*ptrs.pulongint Then
				If 35 And brkv.ttb Then flag=1
			End If
		Case 4,13,14
			If brkv.typ=13 Then  ' normal string
				ReadProcessMemory(dbghand,Cast(LPCVOID,brkv.adr),@adr,SizeOf(Integer),0) 'address ptr 25/07/2015 64bit
			Else 
				adr=brkv.adr
			End If
			Clear recup(0),0,26 'max 25 char
			ReadProcessMemory(dbghand,Cast(LPCVOID,adr),@recup(0),25,0) 'value
			strg1=*ptrs.pzstring 
			If brkv.ttb=16 Then
				If brkv.vst<>strg1 Then flag=1
			Else
				If brkv.vst=strg1 Then flag=1
			EndIf     
		End Select
	If flag Then
		If brkv.ivr=0 Then stopcode=CSBRKM Else stopcode=CSBRKV  ''memory or variable
		strg1=getGadgetText(GBRKVAR) ''current string for break on var
		strg2=var_sh2(brkv.typ,brkv.adr,0) ''last parameter in var_sh2 can be zero because type has been changed and even its a pointer no need *
		
		strg3=Left(strg1,InStr(strg1,">"))          ''beginning of string
		strg3+=Mid(strg2,InStr(strg2,">")+1,25)       ''max 25 characters in case of string, with others datatype no problem of lenght
		strg3+=Mid(strg1,InStr(strg1," Stop if"),99) ''end of string

		If messbox("Break on var","Current value"+Mid(strg2,InStr(strg2,">")+1,25)+Chr(13)+"NB : Not yet updated in Proc/Var panel"+Chr(13)+Chr(13)+"Remove break condition ?"_
			,MB_YESNO)=RETYES Then
			brkv_set(0)
		else
			SetGadgetText(GBRKVAR,strg3)
			Modify_Menu(MNVARBRK,HMenuvar,strg3)
		end if
		Return TRUE
	End If
	Return FALSE
End Function
'=======================================================
'' after stopping fastrun  retrieves all procedures
'=======================================================
private sub proc_newfast()
   Dim vcontext As CONTEXT
   Dim libel As String
   Dim As UInteger regbp,regip,regbpnb,regbpp(PROCRMAX)
   Dim As ULong j,k,pridx(PROCRMAX),calin(PROCRMAX)
   Dim tv As integer
   vcontext.contextflags=CONTEXT_CONTROL or CONTEXT_INTEGER
   'loading with rbp/ebp and proc index
   For i As Integer =0 To threadnb
   	regbpnb=0
		GetThreadContext(thread(i).hd,@vcontext)
		regbp=vcontext.regbp:regip=vcontext.regip 'current proc
		While 1
			For j =1 To procnb
			   If regip>=proc(j).db And regip<=proc(j).fn Then
			   	regbpnb+=1:regbpp(regbpnb)=regbp:pridx(regbpnb)=j
			   	Exit For
			   EndIf
			Next
			If j>procnb Then Exit While
			ReadProcessMemory(dbghand,Cast(LPCVOID,regbp+SizeOf(integer)),@regip,SizeOf(Integer),0) 'return EIP/RIP    12/07/2015 4 replaced by sizeof(integer)
			ReadProcessMemory(dbghand,Cast(LPCVOID,regbp)                ,@regbp,SizeOf(integer),0) 'previous RBP/EBP

			calin(regbpnb)=line_call(regip)
		Wend
		'delete only if needed
		j=regbpnb:k=0
		While k<procrnb
			k+=1
			If procr(k).thid <> thread(i).id Then Continue While
			If procr(k).idx=pridx(j) Then
				j-=1 'running proc still existing so kept
			Else
				proc_del(k) 'delete procr
				k-=1 'to take in account that a procr has been deleted
			EndIf
		Wend
		'create new procrs
		For k As Integer =j To 1 Step -1
			If procrnb=PROCRMAX Then 
				hard_closing("Max number of sub/func reached")
				Exit Sub
			EndIf
			If proc(pridx(k)).st=2 Then Continue For 'proc state don't follow
			procrnb+=1 
			procr(procrnb).sk=regbpp(k)
			procr(procrnb).thid=thread(i).id
			procr(procrnb).idx=pridx(k)
			
			'test if first proc of thread
			If thread(i).plt=0 Then
				thread(i).tv=AddTreeViewItem(GTVIEWTHD,"",cast (hicon, 0),0,0,0)
				thread(i).ptv=thread(i).tv 'last proc 
				thread_text(i)'put text
				thread(i).st=0 'with fast no starting line could be gotten
				procr(procrnb).cl=-1  ' no real calling line 
				libel="ThID="+Str(procr(procrnb).thid)+" "
				tv=TVI_LAST 'insert in last position
			Else
				tv=thread(i).plt 'insert after the last item of thread
				procr(procrnb).cl=calin(k)
				libel=""
			EndIf
			'add manage LIST
			If flagtrace Then dbg_prt ("NEW proc "+proc(pridx(k)).nm)
			libel+=proc(pridx(k)).nm+":"+proc_retval(pridx(k))
			If flagverbose Then libel+=" ["+Str(proc(pridx(k)).db)+"]"
			
			    vrr(vrrnb).tv=AddTreeViewItem(GTVIEWVAR,"Not yet filled",cast (hicon, 0),0,TVI_LAST,tv)
			procr(procrnb).tv=AddTreeViewItem(GTVIEWVAR,libel,cast (hicon, 0),0,tv,0)
			
			thread(i).plt=procr(procrnb).tv 'keep handle last item
			thread(i).ptv=AddTreeViewItem(GTVIEWTHD,proc(pridx(k)).nm,cast (hicon, 0),0,TVI_FIRST,thread(i).ptv)
			
			var_ini(procrnb,proc(procr(procrnb).idx).vr,proc(procr(procrnb).idx+1).vr-1)
			procr(procrnb+1).vr=vrrnb+1
			If proc(procsv).nm="main" Then procr(procrnb).vr=1 'constructors for shared they are executed before main so reset index for first variable of main 04/02/2014
			proc_watch(procrnb) 'reactivate watched var
		Next
   Next
End Sub
'============================================================================
'' fast run to cursor or breakpoint !!! Be carefull
'============================================================================
private sub fastrun()
	Dim l As Integer,i As Integer

	l=line_cursor() 'get line
	For i=1 To linenb
		If rline(i).nu=l And rline(i).sx=srcdisplayed Then Exit For 'check nline 
	Next
	If i>linenb Then messbox("Fast run Not possible","Inaccessible line (not executable)") :Exit Sub
	For j As Integer =1 To procnb 'first line of proc
		If rline(i).ad=proc(j).db Then messbox("Fast run Not possible","Inaccessible line (not executable)") :Exit Sub
	Next
	If linecur=l+1 And srcdisplayed=srccur Then messbox("Fast run Not possible","Same line"): Exit Sub
	For j As Integer = 1 To linenb 'restore all instructions
	  WriteProcessMemory(dbghand,Cast(LPVOID,rline(j).ad),@rLine(j).sv,1,0)
	Next
	'create run to cursor
	brkol(0).ad=rline(i).ad
	brkol(0).typ=2 'to clear when reached
	For j As Integer = 0 To brknb 'breakpoint
		If brkol(j).typ<3 Then WriteProcessMemory(dbghand,Cast(LPVOID,brkol(j).ad),@breakcpu,1,0) 'only enabled
	Next
	runtype=RTFRUN
	but_enable()
	fasttimer=Timer
	thread_resume()
End Sub
'======================================================
private sub gest_brk(ad As UInteger)
   Dim As UInteger i,debut=1,fin=linenb+1,adr,iold

   Dim vcontext As CONTEXT
   'egality added in case attach (example access violation) without -g option, ad=procfn=0....
	If ad>=procfn Then 
		thread_resume()
		Exit Sub
	EndIf
   
	dbg_prt2("")
	dbg_prt2("AD gest brk="+hex(ad)+" th="+Str(threadcur))
	'show_context
   
	i=thread(threadcur).sv+1
	proccurad=ad
   
	If rline(i).ad<>ad Then 'hope next source line is next executed line (optimization)
		While 1
			iold=i
			i=(debut+fin)\2 'first consider that the addresses are sorted increasing order
			If i=iold Then 'loop
				For j As Integer =1 To linenb
					If rline(j).ad=ad Then i=j:Exit While
				Next
			End If	
			If ad>rLine(i).ad Then
				debut=i
			ElseIf ad<rLine(i).ad Then
				fin=i
			Else
				Exit While
			End If
		Wend
	EndIf
   
	''restore CC previous line
	If thread(threadcur).sv<>-1 Then WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@breakcpu,1,0)
   ''thread changed by threadcreate or by mutexunlock
	If threadcur<>threadprv Then
		If thread(threadprv).sv<>i Then 'don't do it if same line otherwise all is blocked.....not sure it's usefull
			WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadprv).sv).ad),@breakcpu,1,0) 'restore CC 
		EndIf
		stopcode=CSNEWTHRD  'status HALT NEW THREAD
		runtype=RTSTEP
		thread_text(threadprv) 'not next executed
		thread_text(threadcur) 'next executed
		threadprv=threadcur
	EndIf
   
	thread(threadcur).od=thread(threadcur).sv:thread(threadcur).sv=i
	procsv=rline(i).px
	'dbg_prt2("proc ="+Str(procsv)+" "+proc(procsv).nm+" "+hex(proc(procsv).db)+" "+source(proc(procsv).sr)+" "+hex(proccurad))
	'dbg_prt2("line="+Str(rline(i).nu))

	'get and update registers
	vcontext.contextflags=CONTEXT_CONTROL
	GetThreadContext(threadcontext,@vcontext)

	If proccurad=proc(procsv).db Then 'is first proc instruction
   
		If rline(i).sv=85 Then'check if the first instruction is push ebp opcode=85 / push rbp opcode=&h55=85dec
			'in this case there is a prologue
			 'at the beginning of proc EBP not updated so use ESP
			procsk=vcontext.regsp-SizeOf(Integer) 'ESP-4 for respecting prologue : push EBP then mov ebp,esp / 64bit push rbp then mov rbp,rsp
		Else
			If procrnb<>0 Then  'no main and no prologue so naked proc, procrnb not yet updated
			   procsk=vcontext.regsp
			   thread(threadcur).nk=procsk
			Else
				procsk=vcontext.regsp-20 'if gcc>3 for main prologue is different
			EndIf
		End If
	else
		'only for naked, check if return by comparing top of stack because no epilog
		If thread(threadcur).nk Then
			If vcontext.regsp>thread(threadcur).nk Then
				thread(threadcur).pe=TRUE
				thread(threadcur).nk=0
			EndIf
		End If
	EndIf
	vcontext.regip=ad
   
	setThreadContext(threadcontext,@vcontext)
	'dbg_prt2("PE"+Str(thread(threadcur).pe)+" "+Str(proccurad)+" "+Str(proc(procsv).fn))
	If thread(threadcur).pe Then 'if previous instruction was the last of proc
		If proccurad<>proc(procsv).db Then
		procsk=vcontext.regbp 'reload procsk with rbp/ebp 04/02/2014 test added for case constructor on shared
		EndIf
		proc_end():thread(threadcur).pe=FALSE
	EndIf

	If proccurad=proc(procsv).db Then 'is first instruction ?
		If proctop Then
			runtype=RTSTEP
			procad=0:procin=0:proctop=FALSE:procbot=0' step call execute one step to reach first line
		EndIf
		proc_new
		thread_resume():Exit Sub
	ElseIf proccurad=proc(procsv).fn Then
		thread(threadcur).pe=TRUE        'is last instruction ?
	EndIf
	If runtype=RTRUN Then
		' test breakpoint on line
		If brk_test(proccurad) Then
			fasttimer=Timer-fasttimer
			runtype=RTSTEP
			procad=0:procin=0:proctop=FALSE:procbot=0
			dsp_change(i)
			brk_del(0)
			Exit Sub
		EndIf
		'test beakpoint on var
		If brkv.adr<>0 Then
			If brkv_test() Then
				runtype=RTSTEP
				procad=0:procin=0:proctop=FALSE:procbot=0
				dsp_change(i)
				Exit Sub
			EndIf
		End If
		If procad Then 	'test out
			If proc(procad).fn=proccurad Then procad=0:procin=0:proctop=FALSE:procbot=0:runtype=RTSTEP 'still running ONE step before stopping
		ElseIf procin Then 'test over
			If procsk>=procin Then procad=0:procin=0:proctop=FALSE:procbot=0:runtype=RTSTEP:dsp_change(i):Exit Sub
		ElseIf procbot Then 'test end of proc
			 If proc(procbot).fn=proccurad Then procad=0:procin=0:proctop=FALSE:procbot=0:runtype=RTSTEP:dsp_change(i):Exit Sub 'stop on end of proc STEPRETURN
		End If
		thread_resume()
	ElseIf runtype=RTFRUN Then
   		fasttimer=Timer-fasttimer
      	For i As Integer = 1 To linenb 'restore CC
   			WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@breakcpu,1,0)
      	Next
      	'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(i).ad),@rLine(i).sv,1,0) 'restore old value for execution
   		brk_test(proccurad) ' cancel breakpoint on line, if command halt not really used
   		proc_newfast   'creating running proc tree 
   		var_sh			'updating information about variables
   		runtype=RTSTEP
   		dsp_change(i)
		brk_del(0)
   Else 'RTSTEP or RTAUTO
		If flagattach Then proc_newfast:flagattach=FALSE
		'NOTA If rline(i).nu=-1 Then
			'fb_message("No line for this proc","Code added by compiler (constructor,...)")
		'Else	
		dsp_change(i)
		'EndIf
		If runtype=RTAUTO Then 
			Sleep(autostep)
			If threadaut>1 Then 'at least 2 threads
				Dim As Integer c=threadcur
				Do
					c+=1:If c>threadnb Then c=0
				Loop Until thread(c).exc
				thread_change(c)
			EndIf
			thread_resume
		EndIf
		If threadsel<>threadcur AndAlso messbox("New Thread","Previous thread "+Str(thread(threadsel).id)+" changed by "+Str(thread(threadcur).id) _
				+Chr(10)+Chr(13)+" Keep new one ?",MB_YESNO)=RETNO Then
				thread_change(threadsel)
		Else
			threadsel=threadcur
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
			var_ini(procrnb,1,vrbgbl) ''add vrbgblprev instead 1
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
'======================================================================================================
private sub proc_del(j As Integer,t As Integer=1)
Dim  As Integer tempo,th
Dim parent As integer
Dim As String text 
' delete procr in treeview
DeleteTreeViewItem(GTVIEWVAR,procr(j).tv)
'delete watch
For i As Integer =0 To WTCHMAX 
	'keep the watched var for reusing !!!
   If wtch(i).psk=procr(j).sk Then
    	wtch(i).psk=-3
   End If
Next
'delete breakvar
If brkv.psk=procr(j).sk Then brkv_set(0)

'' remove array tracking : either suppressed array or one of the variable used as index
If procr(j+1).vr>trckarr(0).idx AndAlso trckarr(0).idx>=procr(j).vr Then
	''todo add array_tracking_remove
Else
	For i As Long =0 To 4
		If trckarr(i).memadr Then
			If procr(j+1).vr>trckarr(i).iv AndAlso trckarr(i).iv>=procr(j).vr Then
				''todo add array_tracking_remove
				Exit For
			End If
		End if
	Next
End If
''close shw/exp except if static
If shwexpnb then
    For i As Integer=1 To SHWEXPMAX
        if shwexp(i).bx Then 
        	if shwexp(i).procr=j then
        		if shwexp(i).mem<>3 then ''only local and not a static
        			''seems not working destroywindow(shwexp(i).bx)
        			sendmessage(shwexp(i).bx,WM_CLOSE,0,0)
        			shwexp(i).bx=0
        			shwexpnb-=1
        		end if
        	end if
        end if
    Next
end if

' compress running variables
tempo=procr(j+1).vr-procr(j).vr
vrrnb-=tempo
For i As Integer = procr(j).vr To vrrnb
	vrr(i)=vrr(i+tempo)
Next


If t=1 Then 'not dll
	th=thread_select(procr(j).thid) 'find thread index
	parent=GetParentItemTreeView(GTVIEWTHD,thread(th).ptv)
	DeleteTreeViewItem(GTVIEWTHD,thread(th).ptv) ''delete item
	thread(th).ptv=parent 'parent becomes the last
	thread_text(th) 'update thread text
EndIf

' compress procr and update vrr index
procrnb-=1
For i As Integer =j To procrnb
	procr(i)=procr(i+1)
	procr(i).vr-=tempo
Next

End Sub
'=====================================================
private sub thread_del(thid As UInteger)
Dim As Integer k=1,threadsup,threadold=threadcur
For i As Integer =1 To threadnb
   If thid<>thread(i).id Then
    	If i<>k Then thread(k)=thread(i):If i=threadcur Then threadcur=k 'optimization
    	k+=1
   Else
   	threadsup=i
   	If thread(i).sv<>-1 Then
   		'delete thread item and child
			DeleteTreeViewItem(GTVIEWTHD,thread(i).tv)
   		'proc delete
			For j As Integer = procrnb To 2 Step -1 'always keep procr(1)=main
				If procr(j).thid=thid Then
					proc_del(j)
				EndIf
			Next
   	EndIf
   End If 
Next
threadnb-=1
If threadsup<>threadold Then Exit Sub 'if deleted thread was the current, replace by first thread

If runtype=RTAUTO AndAlso threadaut>1 Then 'searching next thread
	threadaut-=1
	k=threadcur
	Do
		k+=1:If k>threadnb Then k=0
	Loop Until thread(k).exc
	thread_change(k)
	thread_resume()
Else
	threadcur=0 'first thread
	threadprv=0 'no change
	threadsel=0
	threadhs=thread(0).hd
	thread_text(0)
	runtype=RTSTEP
	dsp_change(thread(0).sv)
EndIf
End Sub
'===============================================
'' returns true if proc running
'===============================================
private function proc_verif(p As UShort) As Byte 
	For i As UShort =1 To procrnb
		If procr(i).idx = p Then Return TRUE
	Next
	Return FALSE
End function
'====================================================================================================================
'' deletes all the elements of a treeview
'' firstly find the most prev item of a higher level item then delete all the siblings and finally the first one
'====================================================================================================================
private sub deleteallitemstreeview(byval gadget as long,byval item as INTEGER)
	dim as INTEGER itemprev,itemnext

	'messbox("delete first item="+str(item),gettexttreeview(gadget,item))

	'itemprev=GetPrevItemTreeView(gadget,item)
	
	do
		itemprev=item
		item=GetPrevItemTreeView(gadget,item)
	loop until item=0
	'' now itemprev contains the first item of the treeview
	
	itemnext=getnextitemtreeview(gadget,itemprev)
	while itemnext<>0	
		'messbox("delete="+str(item),gettexttreeview(gadget,itemnext))
		DeleteTreeViewItem(gadget,itemnext)
		itemnext=getnextitemtreeview(gadget,itemprev)
	wend
	DeleteTreeViewItem(gadget,itemprev)
end sub
'==============================
'' Reinitialisation
'==============================
private sub reinit()
	vrbgbl=0:vrbloc=VGBLMAX:vrbgblprev=0
	prun=FALSE
	runtype=RTOFF
	flagmain=true
	sourcenb=-1:dllnb=0
	vrrnb=0:procnb=0:procrnb=0:linenb=0:cudtnb=0:arrnb=0:procr(1).vr=1
	'procin=0:procfn=0:procbot=0:proctop=FALSE
	proc(1).vr=VGBLMAX+1 'for the first stored proc
	udtcpt=0:udtmax=0
	deleteallitemstreeview(GTVIEWVAR,thread(0).plt)
	deleteallitemstreeview(GTVIEWTHD,thread(0).tv)
	'todo DeleteTreeViewItem(GTVIEWWCH,0)
	'SendMessage(htviewwch,TVM_DELETEITEM,0,Cast(LPARAM,TVI_ROOT)) 'watched   needed ????
	
	procsort=KPROCNM
	'================================================================
				'	'======== init =========================================
				'private sub re_ini()
				'
				'If TimerID1<>0 Then KillTimer(windmain,TimerID1)

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
				'
				PanelGadgetSetCursel(GRIGHTTABS,TABIDXVAR)
				'If dsptyp Then dsp_hide(dsptyp)
				'dsp_sizecalc
				'threadnb=-1
				'If flagrestart=-1 Then 'add test for restart without loading again all the files
				'	setwindowtext(richeditcur,"Your source")
				'	sendmessage(htab1,TCM_DELETEALLITEMS ,0,0) 'zone tab
				'	For i As Integer=0 To MAXSRC:setWindowText(richedit(i),""):ShowWindow(richedit(i),SW_HIDE):Next
				'Else
				'	sel_line(linecur-1,0,1,richedit(srccur),FALSE) 'default color
				'EndIf
				'linecur=0
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
'================================================================
'' check if exe bitness if not wrong 32bit<>64bit windows only
'================================================================
private function check_bitness(fullname as string) as integer
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
		    	messbox("Feature missing for Linux","Kill_process")
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
		  wait_debug()
	   Else
		  messbox("PROBLEM","no debugged pgm -->"+exename+Chr(10)+"error :"+Str(GetLastError()),MB_SYSTEMMODAL)
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
    messbox("new save(0)","now="+savexe(0))
    If cmdline<>"" Then cmdexe(0)=cmdline
    SetToolTipText(IDBUTRERUN,TTRERUN,exename)
    settitle()
End sub
'======================================================================
''loads the source code files, by slice : n contains the first to be loaded until sourcenb
''n=0 for the first loading
''=====================================================================
private sub sources_load(n As integer,exedate as double)
	dim As integer flgt,fnum
	dim as any ptr ptrdoc
	if flagrestart Then
		statusbar_text(KSTBSTS,"Loading sources")
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

			AddPanelGadgetItem(GSRCTAB,isrc,source_name(source(isrc)))
			''todo later sort the files to get them in alphabetic order
			AddComboBoxItem(GFILELIST,source_name(source(isrc)),-1)



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
		SetItemComboBox(GFILELIST,1)
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
    if exename<>"" then
		SetToolTipText(IDBUTRERUN,TTRERUN,"Restart "+exename)
	end if
End sub
''==============================================================================
'' freeing all before quitting the debugger 
''==============================================================================
private sub closes_debugger()
	dim as zstring *50 text =""

    If prun Then
    	text=>"CAUTION PROGRAM STILL RUNNING."+Chr(10)+Chr(10)
    EndIf
	if messbox("Quit Fbdebugger",text+"Are you sure ?",MB_YESNO)=RETYES then
		release_doc ''releases scintilla docs
		''todo free all the objects menus, etc
		If sourcenb<>-1 Then ''case exiting without stopping debuggee before
			watch_sav()
			brk_sav 
		EndIf
		ini_write()
		end
	endif
end sub
'===================================================
'' Drag and drop
'===================================================
private sub drag_n_drop
	messbox("feature to be coded","drag_n_drop")
end sub
'===================================================
''launch by command line or file explorer
'===================================================
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

	#Ifdef __fb_win32__
		If ThreadCreate(@start_pgm)=0 Then
			messbox("ERROR unable to start the thread managing the debuggee","Debuggee not running")
		endif
	#else
		messbox("feature to be coded linux","after selecting file")
	#endif

end sub
'==================================================================================
'' Debuggee restarted, last debugged (using IDBUTRERUN) or one of the 9/10 others
'==================================================================================
private sub restart(byval idx as integer=0)
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
	settitle()
	#Ifdef __fb_win32__
		If ThreadCreate(@start_pgm)=0 Then
			messbox("ERROR unable to start the thread managing the debuggee","Debuggee not running")
		endif
	#else
		messbox("feature to be coded linux","after selecting file")
	#endif    
    
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

'=================================================================================
'' changes address of execution (forward or backward) only in the same procedure
'==================================================================================
private sub exe_mod() 'execution from cursor
	Dim l As Integer,i As Integer',b As Integer
	Dim vcontext As CONTEXT

	l=line_cursor
	
	For i=1 To linenb  'check nline 
		If rline(i).nu=l And rline(i).sx=srcdisplayed Then 
			If rline(i+1).nu=l+1 And rline(i+1).sx=srcdisplayed Then i+=1 'weird case : first line main proc 
			Exit For 
		End If 
	Next 

	If i>linenb Then
		messbox("Execution on cursor","Inaccessible line (not executable)")
		Exit Sub 
	EndIf

	If linecur=l+1 And srcdisplayed=srccur Then 
		If messbox("Execution on cursor","Same line, continue ?",MB_YESNO)=RETNO Then
			Exit Sub 
		EndIf
	End If 

	'check inside same proc if not msg 
	If rLine(i).ad>proc(procsv).fn Or rLine(i).ad<=proc(procsv).db Then 
		messbox("Execution on cursor","Only inside current proc !!!")
		Exit Sub 
	End If 
	If rLine(i).ad=proc(procsv).fn Then
		thread(threadcur).pe=TRUE        'is last instruction
	EndIf
	'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@breakcpu,1,0) 'restore CC previous line
	'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(i).ad),@rLine(i).sv,1,0) 'restore old value for execution
	thread(threadcur).od=thread(threadcur).sv:thread(threadcur).sv=i
	'get and update registers
	vcontext.contextflags=CONTEXT_CONTROL
	GetThreadContext(threadhs,@vcontext)
	vcontext.regip=rline(i).ad
	SetThreadContext(threadhs,@vcontext)

	dsp_change(i)
End Sub
