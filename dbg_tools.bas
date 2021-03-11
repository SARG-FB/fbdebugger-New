''tools for fbdebugger_new
''dbg_tools.bas
'=====================================================================
'in string STRG all the occurences of SRCH are replaced by REPL
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
sub hard_closing(errormsg as string)
	messbox("Need to close fbdebugger",_
			  "Sorry an unrecoverable problem occurs :"+chr(13)+errormsg+chr(13)+chr(13)+"Report to dev please")
	''freegadgets()  todo free all gadget number by number
	close_window(hmain)
	end
end sub
'==========================================================
private function var_parent(child As integer) As Integer 'find var master parent
	Dim As integer temp,temp2,hitemp
	temp=child
	Do
		hitemp=temp2
		temp2=temp
		''temp=Cast(HTREEITEM,SendMessage(tviewvar,TVM_GETNEXTITEM,TVGN_PARENT,Cast(LPARAM,temp)))
		''todo need to code the function as TVGN_PARENT doesn't exist
	Loop While temp
For i As Integer =1 To vrrnb
    If vrr(i).tv=hitemp Then Return i
Next
End Function
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
private function Val_byte(strg As String)As String
	Dim strg2 As String,vl As Integer
	For i As Integer=1 To Len(strg) Step 2
		vl=ValInt("&h"+Mid(strg,i,2))
		strg2+=Str(vl)+","
	Next
	Return Left(strg2,Len(strg2)-1)
End Function
'======================================================
private function Val_word(strg As String)As String
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
'===========================================
'' restore instruction and resume thread
'===========================================
Private sub thread_resume()
	#ifdef __fb_win32__
		writeprocessmemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@rLine(thread(threadcur).sv).sv,1,0) 'restore old value for execution
		resumethread(threadhs)
	#else
		''todo LINUX, maybe moved in dbg_linux.bas
		messbox("For Linux","thread_resume() needed to be added")
	#endif
End sub
'================================================================
private function thread_select(id As Integer =0) As Integer 'find thread index based on cursor or threadid
	Dim text As string, pr As Integer, thid As Integer
	Dim As Integer hitem,temp

	If id =0 Then  'take on cursor
	'get current hitem in tree
		temp=GetItemTreeView(GTVIEWTHD)
		Do 'search thread item
			hitem=temp
			''todo		temp=SendMessage(tviewcur,TVM_GETNEXTITEM,TVGN_PARENT,hitem)
			messbox("TODO","temp=SendMessage(tviewcur,TVM_GETNEXTITEM,TVGN_PARENT,hitem")
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
'===========================================================================
private sub thread_text(th As Integer=-1)'update text of thread(s)
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
		libel="threadID="+fmt2(Str(thid),4)+" : "+proc(procr(p).idx).nm
		If flagverbose Then libel+=" HD: "+Str(thread(i).hd)
		If threadhs=thread(i).hd Then libel+=" (next execution)"
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
private function var_sh2(t As Integer,pany As UInteger,p As UByte,sOffset As String="") As String
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
'===========================================
private sub watch_sav()'save watched
'example main/TUTU,TTEST/B,TTITI/C,Integer/pnt 
'= PROC main,tutu type ttest,B type ttiti, C type integer, nb pointer
   Dim As Integer begb,Endb,stepb
   Dim As String  text

	For i As Integer =0 To WTCHMAX
		If wtch(i).psk=-1 OrElse wtch(i).psk=-2 OrElse wtch(i).Var<>0 Then 'not used or memory not saved or array
			text=""
		Else  'dll, more than one level and not a basic type on not a pointer ?
			If wtch(i).idx=0 AndAlso wtch(i).vnb>1 AndAlso wtch(i).typ>TYPESTD AndAlso wtch(i).pnt=0 Then '20/08/2015
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
		wtchexe(0,i)=text
	Next
End Sub
'==================================================================================
private sub watch_sh(aff As Integer) 'default all watched
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
'=====================================================================
private sub watch_addtr
	wtchnew=-1
	If var_find2(htviewvar)<>-1 Then
		watch_set()
		If wtchnew<>-1 Then watch_trace(wtchnew)
	EndIf
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
		'todo temp=Cast(HTREEITEM,SendMessage(tviewvar,TVM_GETNEXTITEM,TVGN_PARENT,Cast(LPARAM,vrr(iparent).tv)))
     
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
'================================ 
Private sub brk_apply()
'brkexe = <name>,<#line>,<typ>
Dim f As Integer =FALSE
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
						brk_color(brknb)
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
	'todo fb_MDialog(@brk_manage,"Restart debuggee, managing breakpoints",windmain,500,8,330,170)
	SetStateMenu(HMenusource,MNMNGBRK,0)
EndIf
End Sub 
'============================================
Private sub brk_sav
	For i As Integer =1 To BRKMAX
		If i<=brknb Then
			brkexe(0,i)=name_extract(source(brkol(i).isrc))+","+Str(brkol(i).nline)+","+Str(brkol(i).cntrsav)+","+Str(brkol(i).typ) '03/09/2015
		EndIf
	Next
End Sub
'=============================================
private sub brk_del(n As Integer) 'delete one breakpoint
	brkol(n).typ=0
	brk_color(n)
	brknb-=1
	For i As Integer =n To brknb
		brkol(i)=brkol(i+1)
	Next
   If brknb=0 Then SetStateMenu(HMenusource,MNMNGBRK,MF_GRAYED)
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
't 1=permanent breakpoint / 2=tempo breakpoint / 3=breakpoint with counter by changed in type 1 / 4=disable / 7=change value counter / 8 reset to initial value / 9=same line
'=======================================================================
Private sub brk_set(t As Integer)
Dim l As Integer,i As Integer,range As charrange,b As Integer,ln As Integer
range.cpmin=-1 :range.cpmax=0

l=line_cursor() 'get line

For i=1 To linenb
	If rline(i).nu=l+1 And rline(i).sx=shwtab Then Exit For 'check nline 
Next
If i>linenb Then messbox("Break point Not possible","Inaccessible line (not executable)") :Exit Sub
For j As Integer =1 To procnb
	If rline(i).ad=proc(j).db Then messbox("Break point Not possible","Inaccessible line (not executable)") :Exit Sub
Next
ln=i
If t=9 Then 'run to cursor
    'l N°line/by 0
    If curlig=l+1 And shwtab=curtab Then
        If messbox("Run to cursor","Same line, continue ?",MB_YESNO)=RETNO Then Exit Sub
    End If
    brkol(0).ad=rline(ln).ad
    brkol(0).typ=2 'to clear when reached
    runtype=RTRUN
    but_enable()
    thread_rsm()
Else
   For i=1 To brknb 'search if still put on this line
   	If brkol(i).nline=l+1 And brkol(i).isrc=shwtab Then Exit For
   Next
	If i>brknb Then 'not put
		If brknb=BRKMAX Then messbox("Max of brk reached ("+Str(BRKMAX)+")","Delete one and retry"):Exit Sub
		brknb+=1
   	brkol(brknb).nline=l+1
   	brkol(brknb).typ=t
   	brkol(brknb).index=ln
   	brkol(brknb).isrc=shwtab
   	brkol(brknb).ad=rline(ln).ad
   	brkol(brknb).cntrsav=0
   	brkol(brknb).counter=0
   	If t=3 Then 'change value counter
			inputval="0"
			inputtyp=7 'ulong 
			''todo fb_MDialog(@input_box,"Set value counter for a breakpoint",windmain,283,25,120,30)
			brkol(i).counter=ValUInt(inputval)
			brkol(i).cntrsav=brkol(i).counter
			brkol(brknb).typ=1 'forced permanent
   	EndIf
   Else 'still put
		If t=7 Then 'change value counter
			inputval=Str(brkol(i).cntrsav)
			inputtyp=7 'ulong 
			fb_MDialog(@input_box,"Change value counter, remaining= "+Str(brkol(i).counter)+" initial below",windmain,283,25,140,30)
			If inputval="" Then inputval=Str(brkol(i).cntrsav) 'cancel button selected so no value
			brkol(i).counter=ValUInt(inputval)
			brkol(i).cntrsav=brkol(i).counter
		ElseIf t=8 Then 'reset to initial value
			If brkol(i).cntrsav Then
				brkol(i).counter=brkol(i).cntrsav
			Else
				fb_message("Reset counter","No counter for this breakpoint")
			EndIf
		ElseIf t=4 Then 'toggle enabled/disabled 03/09/2015
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
	brk_color(i)
   If brknb=1 Then EnableMenuItem(menuedit,IDMNGBRK,MF_ENABLED)
End If
End Sub
'====================== new sub ou func ===============
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
		procr(procrnb).cl=-1  ' no real calling line
		libel="ThID="+Str(procr(procrnb).thid)+" "
		tv=TVI_LAST 'insert in last position
		AddTreeViewItem(GTVIEWTHD,"Not filled",cast (hicon, 0),0,TVI_LAST,0)
		thread(threadcur).ptv=thread(threadcur).tv 'last proc 
		thread_text() 'put text not only current but all to reset previous thread text 
	Else
		procr(procrnb).cl=thread(threadcur).od
		tv=thread(threadcur).plt 'insert after the last item of thread
	EndIf
		
	'add manage LIST
	If flagtrace Then dbg_prt ("NEW proc "+Str(procsv)+" "+proc(procsv).nm)
	libel+=proc(procsv).nm+":"+proc_retval(procsv)
	If flagverbose Then libel+=" ["+Str(proc(procsv).db)+"]"

	procr(procrnb).tv=AddTreeViewItem(GTVIEWVAR,libel,cast (hicon, 0),0,TVI_LAST,tv)
	thread(threadcur).plt=procr(procrnb).tv 'keep handle last item
	
	'add new proc to thread treeview
	'thread(threadcur).ptv=Tree_AddItem(,"Proc : "+proc(procsv).nm,TVI_FIRST,tviewthd)
	AddTreeViewItem(GTVIEWTHD,"Proc : "+proc(procsv).nm,cast (hicon, 0),0,TVI_FIRST,thread(threadcur).ptv)
	thread_text(threadcur)
	''RedrawWindow tviewthd, 0, 0, 1	todo USELESS ???
	var_ini(procrnb,proc(procr(procrnb).idx).vr,proc(procr(procrnb).idx+1).vr-1)
	procr(procrnb+1).vr=vrrnb+1
	If proc(procsv).nm="main" Then 
		procr(procrnb).vr=1 'constructors for shared are executed before main so reset index for first variable of main 04/02/2014
	EndIf
	proc_watch(procrnb) 'reactivate watched var
	'RedrawWindow tviewvar, 0, 0, 1  	todo USELESS ???
End Sub
'============================= proc ending ============
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
        Tree_upditem(vrr(i).tv,var_sh1(i),tviewvar)
    Next
   watch_array()
   watch_sh
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
		strg1=Space(151)''2017/12/13
		GetWindowText(brkvhnd,StrPtr(strg1),150)''current string for break on var
		strg2=var_sh2(brkv.typ,brkv.adr,0) ''last parameter in var_sh2 can be zero because type has been changed and even its a pointer no need *
		
		strg3=Left(strg1,InStr(strg1,">"))          ''beginning of string
		strg3+=Mid(strg2,InStr(strg2,">")+1,25)       ''max 25 characters in case of string, with others datatype no problem of lenght
		strg3+=Mid(strg1,InStr(strg1," Stop if"),99) ''end of string

		If fb_message("Break on var","Current value"+Mid(strg2,InStr(strg2,">")+1,25)+Chr(13)+"NB : Not yet updated in Proc/Var panel"+Chr(13)+Chr(13)+"Remove break condition ?"_
		   ,MB_YESNO Or MB_ICONQUESTION Or MB_SETFOREGROUND Or MB_SYSTEMMODAL)=IDYES Then
		   brkv_set(0)
		else
         SetWindowText(brkvhnd,strg3)
		   menu_chg(menuvar,idvarbrk,strg3)
		end if
      Return TRUE
End If
Return FALSE
End Function
'====================== new sub ou func ===============
private sub proc_newfast()
   Dim vcontext As CONTEXT
   Dim libel As String
   Dim As UInteger regbp,regip,regbpnb,regbpp(PROCRMAX)
   Dim As ULong j,k,pridx(PROCRMAX),calin(PROCRMAX)
   Dim tv As HTREEITEM
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
			If procrnb=PROCRMAX Then fb_message ("CLOSING DEBUGGER","Max number of sub/func reached"):DestroyWindow (windmain):Exit Sub
			If proc(pridx(k)).st=2 Then Continue For 'proc state don't follow
			procrnb+=1 
			procr(procrnb).sk=regbpp(k)
			procr(procrnb).thid=thread(i).id
			procr(procrnb).idx=pridx(k)
			
			'test if first proc of thread
			If thread(i).plt=0 Then
				thread(i).tv= Tree_AddItem(NULL,"", 0, tviewthd)'create item 
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
			
			procr(procrnb).tv=Tree_AddItem(0,libel,tv,tviewvar)
			thread(i).plt=procr(procrnb).tv 'keep handle last item
			
			thread(i).ptv=Tree_AddItem(thread(i).ptv,proc(pridx(k)).nm,TVI_FIRST,tviewthd)
			var_ini(procrnb,proc(procr(procrnb).idx).vr,proc(procr(procrnb).idx+1).vr-1)
			procr(procrnb+1).vr=vrrnb+1
			If proc(procsv).nm="main" Then procr(procrnb).vr=1 'constructors for shared they are executed before main so reset index for first variable of main 04/02/2014
			proc_watch(procrnb) 'reactivate watched var
		Next
		RedrawWindow tviewthd, 0, 0, 1
		RedrawWindow tviewvar, 0, 0, 1
   Next
End Sub
'===============
private sub fastrun() 'running until cursor or breakpoint !!! Be carefull
Dim l As Integer,i As Integer,range As charrange,b As Integer
range.cpmin=-1 :range.cpmax=0

sendmessage(richeditcur,EM_exsetsel,0,Cast(LPARAM,@range)) 'deselect          utilite ????
l=sendmessage(richeditcur,EM_EXLINEFROMCHAR,0,-1) 'get line
For i=1 To linenb
	If rline(i).nu=l+1 And rline(i).sx=shwtab Then Exit For 'check nline 
Next
If i>linenb Then fb_message("Fast run Not possible","Inaccessible line (not executable)") :Exit Sub
For j As Integer =1 To procnb 'first line of proc
	If rline(i).ad=proc(j).db Then fb_message("Fast run Not possible","Inaccessible line (not executable)") :Exit Sub
Next
If curlig=l+1 And shwtab=curtab Then fb_message("Fast run Not possible","Same line"): Exit Sub
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
thread_rsm()
End Sub
'======================================================
private sub gest_brk(ad As UInteger)
   Dim As UInteger i,debut=1,fin=linenb+1,adr,iold

   Dim vcontext As CONTEXT
   'egality added in case attach (example access violation) without -g option, ad=procfn=0....
   If ad>=procfn Then thread_resume():Exit Sub 'out of normal area, the first exception breakpoint is dummy or in case of use of debugbreak
   
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
   
   'restore CC previous line
   If thread(threadcur).sv<>-1 Then WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@breakcpu,1,0)
   'thread changed by threadcreate or by mutexunlock
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
   Else
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
      If proccurad<>proc(procsv).db Then procsk=vcontext.regbp 'reload procsk with rbp/ebp 04/02/2014 test added for case constructor on shared
   	proc_end():thread(threadcur).pe=FALSE
   EndIf

   If proccurad=proc(procsv).db Then 'is first instruction ?
   	If proctop Then runtype=RTSTEP:procad=0:procin=0:proctop=FALSE:procbot=0' step call execute one step to reach first line
   	proc_new
   	thread_resume():Exit Sub
   ElseIf proccurad=proc(procsv).fn Then
   	thread(threadcur).pe=TRUE        'is last instruction ?
   EndIf
   If runtype=RTRUN Then
   	' test breakpoint on line
   	If brk_test(proccurad) Then fasttimer=Timer-fasttimer:runtype=RTSTEP:procad=0:procin=0:proctop=FALSE:procbot=0:dsp_change(i):Exit Sub
   	'test beakpoint on var
   	If brkv.adr<>0 Then
           If brkv_test() Then runtype=RTSTEP:procad=0:procin=0:proctop=FALSE:procbot=0:dsp_change(i):Exit Sub
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
   		runtype=RTSTEP:dsp_change(i)
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
''  load shared and common variables, input default=no dll number
'====================================================================
private sub globals_load(d As Integer=0)
Dim temp As integer
Dim As Integer vb,ve 'begin/end index global vars
Dim As Integer vridx 
	If vrbgblprev<>vrbgbl Then 'need to do ?
		If vrbgblprev=0 Then
			procr(procrnb).tv= Tree_AddItem(NULL,"Globals (shared/common) in : main ", 0, tviewvar) 'only first time
			var_ini(procrnb,1,vrbgbl)'add vrbgblprev instead 1
			'dbg_prt2("procrnb="+Str(procrnb))
			procr(procrnb+1).vr=vrrnb+1 'to avoid removal of global vars when the first executed proc is not the main one
		Else
			procrnb+=1
			''todo temp=Cast(HTREEITEM,SendMessage(tviewvar,TVM_GETNEXTITEM,TVGN_PREVIOUS,Cast(LPARAM,procr(1).tv)))
			If temp=0 Then 'no item before main
				temp=TVI_FIRST
			EndIf
			If d=0 Then 'called from extract stabs
				d=dllnb
				vb=vrbgblprev+1
				ve=vrbgbl
			Else
				vb=dlldata(d).gblb
				ve=dlldata(d).gblb+dlldata(d).gbln-1
			End If
			procr(procrnb).tv= AddTreeViewItem(GTVIEWVAR,"Globals in : "+dll_name(dlldata(d).hdl,2),cast (hicon, 0),0,temp,0)
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
		'todo remove RedrawWindow tviewvar, 0, 0, 1
	EndIf
End Sub
'========================================================================
private sub brkv_set(a As Integer) ''break on variable change
	Dim As Integer t,p
	Dim Title As String, j As UInteger,ztxt As string,tvi As integer
	If a=0 Then 'cancel break
		brkv.adr=0
		setWindowText(brkvhnd,"Break on var")
		'menu_chg(,idvarbrk,"Break on var")
		' Modify_Menu(1001,MenName,"Changed")
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
'======================================================================================================
private sub proc_del(j As Integer,t As Integer=1)
Dim  As Integer tempo,th
Dim parent As HTREEITEM 
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

'' remove array tracking : either suppressed array or one of the variable used as index 2016/08/10
If procr(j+1).vr>trckarr(0).idx AndAlso trckarr(0).idx>=procr(j).vr Then 
	array_tracking_remove
Else
	For i As Long =0 To 4
		If trckarr(i).memadr Then
			If procr(j+1).vr>trckarr(i).iv AndAlso trckarr(i).iv>=procr(j).vr Then 
		   array_tracking_remove
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
	parent=Cast(HTREEITEM,sendmessage(tviewthd,TVM_GETNEXTITEM,TVGN_PARENT,Cast(LPARAM,thread(th).ptv))) 'find parent of last proc item
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
private function proc_verif(p As UShort) As Byte 'return true if proc running
	For i As UShort =1 To procrnb
		If procr(i).idx = p Then Return TRUE
	Next
	Return FALSE
End Function
'===============================================
'' check if the line is executable return true
'===============================================
function line_check(pline as integer)as boolean
	For iline as integer =1 To linenb
		If rline(iline).nu=pline AndAlso rline(iline).sx=srcdisplayed Then
			return true
		end if
	next
	return false
end function
'-----------------------------------------------
'' Reinitialisation GUI todo move in dbg_gui
'-----------------------------------------------
sub reinit_gui

end sub
'-----------------------------------------------
'' Reinitialisation
'-----------------------------------------------
sub reinit
	vrbgbl=0:vrbloc=VGBLMAX:vrbgblprev=0
	prun=FALSE
	runtype=RTOFF
	flagmain=true
	sourcenb=-1
	vrrnb=0:procnb=0:procrnb=0:linenb=0:cudtnb=0:arrnb=0:procr(1).vr=1
	'procin=0:procfn=0:procbot=0:proctop=FALSE
	proc(1).vr=VGBLMAX+1 'for the first stored proc
	udtcpt=0:udtmax=0
	'================================================================
				'	'======== init =========================================
				'private sub re_ini()
				'
				'If TimerID1<>0 Then KillTimer(windmain,TimerID1) ''27/12/26

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
				'SendMessage(tviewvar,TVM_DELETEITEM,0,Cast(LPARAM,TVI_ROOT)) 'procs/vars
				'SendMessage(tviewprc,TVM_DELETEITEM,0,Cast(LPARAM,TVI_ROOT)) 'procs
				'SendMessage(tviewthd,TVM_DELETEITEM,0,Cast(LPARAM,TVI_ROOT)) 'threads
				'
				'ShowWindow(tviewcur,SW_HIDE):tviewcur=tviewvar:ShowWindow(tviewcur,SW_SHOW)
				'SendMessage(htab2,TCM_SETCURSEL,0,0)
				'If dsptyp Then dsp_hide(dsptyp)
				'dsp_sizecalc
				'threadnb=-1
				'If flagrestart=-1 Then 'add test for restart without loading again all the files
				'	setwindowtext(richeditcur,"Your source")
				'	sendmessage(htab1,TCM_DELETEALLITEMS ,0,0) 'zone tab
				'	For i As Integer=0 To MAXSRC:setWindowText(richedit(i),""):ShowWindow(richedit(i),SW_HIDE):Next
				'Else
				'	sel_line(curlig-1,0,1,richedit(curtab),FALSE) 'default color
				'EndIf
				'curlig=0
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

function check_bitness(fullname as string) as integer
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
		  'wait_debug todo add and uncomment
	   Else
	   	   ''todo use std meesage windows with ,MB_ICONERROR Or MB_SYSTEMMODAL ???
		  messbox("PROBLEM","no debugged pgm -->"+exename+Chr(10)+"error :"+Str(GetLastError()))
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
		'todo add watch_del()
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
    If cmdline<>"" Then cmdexe(0)=cmdline
    SetToolTipText(IDBUTRRUNE,TTRRUNE,exename)
    settitle()
End sub
'======================================================================
''loads the source code files, by slice : n contains the first to be loaded until sourcenb
''n=0 for the first loading
''=====================================================================
private sub source_load(n As integer,exedate as double)
	dim As integer flgt,fnum
	dim as any ptr ptrdoc
	if flagrestart Then
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
    'todo fb_UpdateTooltip(fb_hToolTip,butrrune,"Restart "+exename,"",0)
End sub
'===================================================
'' Drag and drop
'===================================================
private sub drag_n_drop
	messbox("feature to be coded","drag_n_drop")
end sub
'-------------------------------------------
''launch by command line or file explorer
'-------------------------------------------
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

    'If ThreadCreate(@start_pgm)=0 Then todo add start_pgm and uncomment
    	'messbox("ERROR unable to start the thread managing the debuggee","Debuggee not running")
    'endif

end sub
'--------------------------------------------------------
'' Debuggee restarted, last debugged (using IDBUTRRUNE) or one of the 9/10 others
'--------------------------------------------------------
private sub restart(byval idx as integer=0)
	idx-=MNEXEFILE0
	messbox("Check exe number","restart"+str(idx)+" "+savexe(idx))
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

	'If ThreadCreate(@start_pgm)=0 Then todo add start_pgm and uncomment
	'messbox("ERROR unable to start the thread managing the debuggee","Debuggee not running")
    'endif
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


