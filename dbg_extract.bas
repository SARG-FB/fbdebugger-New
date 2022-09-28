''debug data extraction
''dbg_extract.bas

'#ifdef  __fb_linux__
	#define of_entry &h18
	#ifdef __FB_64BIT__
		#define of_section &h28
		#define of_section_size &h3A
		#define of_section_num &h3C
		#define of_section_str &h3E
		#define sect_size &h40
		#define of_offset_infile &h18
		#define of_size_infile &h20
		union ustab
			full as longint
			type
				offst as long
				cod as short
				desc as short
			end type
		end union
	#else
		#define of_section &h20 ''section header table
		#define of_section_size &h2E ''size of a section header table entry
		#define of_section_num &h30 ''number of entries in SHT
		#define of_section_str &h32 ''
		#define sect_size &h28  '' size of section header
		#define of_offset_infile &h10 ''offset of the section in file image
		#define of_size_infile &h14 ''size in bytes
		type ustab
			offst as long
			cod as short
			desc as short
		end type
	#endif
'#endif
'--------------------------------------
'' check if local var already stored
'--------------------------------------
private function local_exist() As long
	Dim ad As UInteger=vrb(vrbloc).adr
	For i As Integer = proc(procnb).vr To proc(procnb+1).vr-2
		If vrb(i).adr=ad AndAlso vrb(i).nm=vrb(vrbloc).nm Then
			vrbloc-=1
			proc(procnb+1).vr-=1
			Return true 'return true if variable local already stored
		EndIf
	Next
	Return FALSE
End function
'------------------------------------
'' check if common already stored
'------------------------------------
private function Common_exist(ad As UInteger) As Integer
	For i As Integer = 1 To vrbgbl
		If vrb(i).adr=ad Then Return TRUE 'return true if common already stored
	Next
	Return FALSE
End function
'---------------------------------
'' check if enum already stored
'---------------------------------
private sub enum_check(idx As Integer)
	For i As Integer =1 To udtmax-1
		If udt(i).en Then 'enum
			If udt(idx).nm=udt(i).nm Then 'same name
				If udt(idx).ub-udt(idx).lb=udt(i).ub-udt(i).lb Then 'same number of elements
					If cudt(udt(idx).ub).nm=cudt(udt(i).ub).nm Then 'same name for last element
						If cudt(udt(idx).lb).nm=cudt(udt(i).lb).nm Then 'same name for first element
							'enum are considered same
							udt(idx).lb=udt(i).lb:udt(idx).ub=udt(i).ub
							udt(idx).en=i
							cudtnb=cudtnbsav
							Exit Sub
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	Next
End sub
'------------------------
'' parse variable names
'------------------------
private function parse_name(strg As String) As String
	 '"__ZN9TESTNAMES2XXE:S1
	Dim As Integer p,d
	Dim As String nm,strg2,nm2
	p=InStr(strg,"_ZN")
	strg2=Mid(strg,p+3,999)
	p=Val(strg2)
	If p>9 Then d=3 Else d=2
	nm=Mid(strg2,d,p)
	strg2=Mid(strg2,d+p)
	p=Val(strg2)
	If p>9 Then d=3 Else d=2
	nm2=Mid(strg2,d,p)
	Return nm+"."+nm2
End function
'----------------
'' parse arrays
'----------------
private function parse_array(gv As String,d As Integer,f As Byte) As Integer
	Dim As Integer p=d,q,c

	If arrnb>ARRMAX Then hard_closing("Max array reached limit="+str(ARRMAX))
	arrnb+=1

	'While gv[p-1]=Asc("a")
	While InStr(p,gv,"ar")
		'GCC todo remove
		'p+=4

		If InStr(gv,"=r(")Then
			p=InStr(p,gv,";;")+2 'skip range =r(n,n);n;n;;
		Else
			p=InStr(p,gv,";")+1 'skip ar1;
		End If

		q=InStr(p,gv,";")
		'END GCC
		arr(arrnb).nlu(c).lb=Val(Mid(gv,p,q-p)) 'lbound

		p=q+1
		q=InStr(p,gv,";")
		arr(arrnb).nlu(c).ub=Val(Mid(gv,p,q-p))'ubound
		'''arr(arrnb).nlu(c).nb=arr(arrnb).nlu(c).ub-arr(arrnb).nlu(c).lb+1 'dim
		p=q+1
		c+=1
	Wend
		arr(arrnb).dm=c 'nb dim
	If f=TYDIM Then
		vrb(*vrbptr).arr=@arr(arrnb)
	Else
		cudt(cudtnb).arr=@arr(arrnb)
	End If
	Return p
End function
'--------------------------
'' parse returned value
'--------------------------
private sub parse_retval(prcnb As Integer,gv2 As String)
	'example :f7 --> private sub /  :F18=*19=f7" --> public sub ptr / :f18=*19=*1 --> private integer ptr ptr
	Dim p As Integer,c As Integer,e As Integer
	For p=0 To Len(gv2)-1
		If gv2[p]=Asc("*") Then c+=1
		If gv2[p]=Asc("=") Then e=p+1
	next
	If c Then 'pointer
		If InStr(gv2,"=f") OrElse InStr(gv2,"=F") Then
			If InStr(gv2,"=f7") OrElse InStr(gv2,"=F7") Then
				p=200+c 'sub
			Else
				p=220+c 'function
			EndIf
		Else
			If gv2[e]=Asc("*")Then e+=1
         p=c
		End If
	Else
		p=0
	End If
	c=Val(Mid(gv2,e+1))
	'workaround with gas boolean are not correctly defined type value 15 instead 16 so change the value as pchar (15) is not used
	'done also with simple and array var
	'dbg_prt2("cut up=2"+vrb(*vrbptr).nm+" value c="+Str(c))
	If c=15 Then c=16
	'========================================================

	If c>TYPESTD Then c+=udtcpt
	proc(prcnb).pt=p
	proc(prcnb).rv=c
End sub
'------------------
'' parse scopes
'------------------
private function parse_scope(gv As Byte, ad As UInteger,dlldelta As Integer=0) As Integer
	Select Case gv
		Case Asc("S"),Asc("G")     'shared/common
			If gv=Asc("G") Then If Common_exist(ad) Then Return 0 'to indicate that no needed to continue
			If vrbgbl=VGBLMAX then hard_closing("Init Globals reached limit "+Str(VGBLMAX))
			vrbgbl+=1
			vrb(vrbgbl).adr=ad
			vrbptr=@vrbgbl
			Select Case gv
				Case Asc("S")		'shared
					vrb(vrbgbl).mem=2
					vrb(vrbgbl).adr+=dlldelta 'in case of relocation dll, all shared addresses are relocated
				Case Asc("G")     'common
					vrb(vrbgbl).mem=6
			End Select
			Return 2
		Case Else
			If vrbloc=VARMAX Then hard_closing("Init locals reached limit="+Str(VARMAX-3000))
			vrbloc+=1
			vrb(vrbloc).adr=ad
			vrbptr=@vrbloc
			proc(procnb+1).vr=vrbloc+1 'just to have the next beginning
			Select Case gv
				Case Asc("V")     'static
					vrb(vrbloc).mem=3
					Return 2
				Case Asc("v")     'byref parameter
					vrb(vrbloc).mem=4
					Return 2
				Case Asc("p")     'byval parameter
					vrb(vrbloc).mem=5
					Return 2
				Case Else         'local
					vrb(vrbloc).mem=1
					Return 1
			End Select
	End select
End function
'--------------------------------------------
'' parse
'--------------------------------------------
private sub parse_var2(gv As String,f As Byte)
Dim p As Integer=1,c As Integer,e As Integer,gv2 As String,pp As Integer
If InStr(gv,"=")=0 Then
	c=Val(Mid(gv,p,9))
	'workaround with gas boolean are not correctly defined type value 15 instead 16 so change the value as pchar (15) is not used
	'done also with array just below and param
	'dbg_prt2("cut up=2"+vrb(*vrbptr).nm+" value c="+Str(c))
	If c=15 Then c=16
	'==================================

	If c>TYPESTD Then c+=udtcpt 'udt type so adding the decal
	pp=0
	If f=TYUDT then
		cudt(cudtnb).typ=c
		cudt(cudtnb).pt=pp
		cudt(cudtnb).arr=0 'by default not an array
	Else
		vrb(*vrbptr).typ=c
		vrb(*vrbptr).pt=pp
		vrb(*vrbptr).arr=0 'by default not an array
	End if
Else
	If InStr(gv,"=ar1") Then p=parse_array(gv,InStr(gv,"=ar1")+1,f)
	gv2=Mid(gv,p)
	For p=0 To Len(gv2)-1
		If gv2[p]=Asc("*") Then c+=1
		If gv2[p]=Asc("=") Then e=p+1
	next
	If c Then 'pointer
		If InStr(gv2,"=f") Then 'proc
			If InStr(gv2,"=f7") Then
				pp=200+c 'sub
			Else
				pp=220+c 'function
			EndIf
		Else
			pp=c
			If gv2[e]=Asc("*")Then e+=1
		End If
	Else
		pp=0
	End If
	c=Val(Mid(gv2,e+1))

	'workaround with gas boolean are not correctly defined type value 15 instead 16 so change the value as pchar (15) is not used
	'done also with simple var and param
	'dbg_prt2("cut up=2"+vrb(*vrbptr).nm+" value c="+Str(c))
	If c=15 Then c=16
	'========================================================

		If c>TYPESTD Then c+=udtcpt 'udt type so adding the decal 20/08/2015
		If f=TYUDT Then
			cudt(cudtnb).pt=pp
			cudt(cudtnb).typ=c
		Else
			vrb(*vrbptr).pt=pp
			vrb(*vrbptr).typ=c
		End If
	EndIf
End Sub

'------------
'' parse udt
'------------
private sub parse_udt(readl As String)
	Dim As Integer p,q,lgbits
	Dim As String tnm
	p=InStr(readl,":")

	tnm=Left(readl,p-1)
	If InStr(readl,":Tt") Then
	   p+=3 'skip :Tt
	else
	   p+=2 'skip :T  GCC
	endif
	'dbg_prt2 "parse_udt=";readl
	q=InStr(readl,"=")

	udtidx=Val(Mid(readl,p,q-p))

	udtidx+=udtcpt:If udtidx>udtmax Then udtmax=udtidx
	If udtmax > TYPEMAX-1 Then hard_closing("Storing UDT limit reached "+Str(TYPEMAX))
	udt(udtidx).nm=tnm
	If left(tnm,4)="TMP$" Then Exit Sub 'gcc redim
	p=q+2
	q=p-1
	While readl[q]<64
		q+=1
	Wend
	q+=1
	udt(udtidx).lg=Val(Mid(readl,p,q-p))
	p=q
	udt(udtidx).lb=cudtnb+1
	while readl[p-1]<>Asc(";")
		'dbg_prt("STORING CUDT "+readl)
		If cudtnb = CTYPEMAX Then hard_closing("Storing CUDT limit reached "+Str(CTYPEMAX))
		cudtnb+=1

		q=InStr(p,readl,":")
		cudt(cudtnb).nm=Mid(readl,p,q-p) 'variable name
		p=q+1
		q=InStr(p,readl,",")

		parse_var2(Mid(readl,p,q-p),TYUDT) 'variable type

		''new way for redim
		If Left(udt(cudt(cudtnb).typ).nm,7)="FBARRAY" Then

		'.stabs "__FBARRAY1:Tt25=s32DATA:26=*1,0,32;PTR:27=*7,32,32;SIZE:1,64,32;ELEMENT_LEN:1,96,32;DIMENSIONS:1,128,32;DIMTB:28=ar1;0;0;29,160,96;;",128,0,0,0
		'.stabs "TTEST2:Tt23=s40VVV:24=ar1;0;1;2,0,16;XXX:1,32,32;ZZZ:25,64,256;;",128,0,0,0
		'.stabs "__FBARRAY1:Tt21=s32DATA:22=*23,0,32;PTR:30=*7,32,32;SIZE:1,64,32;ELEMENT_LEN:1,96,32;DIMENSIONS:1,128,32;DIMTB:31=ar1;0;0;29,160,96;;",128,0,0,0
		'.stabs "TTEST:Tt20=s56AAA:3,0,8;BBB:21,32,256;CCC:32=ar1;1;2;10,320,128;;",128,0,0,0
		'.stabs "__FBARRAY8:Tt18=s116DATA:19=*20,0,32;PTR:33=*7,32,32;SIZE:1,64,32;ELEMENT_LEN:1,96,32;DIMENSIONS:1,128,32;DIMTB:34=ar1;0;0;29,160,768;;",128,0,0,0
		'.stabs "VTEST:18",128,0,0,-176
				cudt(cudtnb).pt=cudt(udt(cudt(cudtnb).typ).lb).pt-1 'pointer always al least 1 so reduce by one
				cudt(cudtnb).typ=cudt(udt(cudt(cudtnb).typ).lb).typ 'real type
				cudt(cudtnb).arr=Cast(tarr Ptr,-1) 'defined as dyn arr

				'dbg_prt2("dyn array="+cudt(cudtnb).nm+" "+Str(cudt(cudtnb).typ)+" "+Str(cudt(cudtnb).pt)+" "+cudt(udt(cudt(cudtnb).typ).lb).nm)
			EndIf
			'end new redim

			p=q+1
			q=InStr(p,readl,",")
			cudt(cudtnb).ofs=Val(Mid(readl,p,q-p))  'bits offset / beginning
			p=q+1
			q=InStr(p,readl,";")
			lgbits=Val(Mid(readl,p,q-p))	'length in bits

			if cudt(cudtnb).typ<>4 And cudt(cudtnb).pt=0 And cudt(cudtnb).arr=0 Then 'not zstring, pointer,array !!!
				If lgbits<>udt(cudt(cudtnb).typ).lg*8 Then 'bitfield
				  cudt(cudtnb).typ=TYPEMAX 'special type for bitfield
				  cudt(cudtnb).ofb=cudt(cudtnb).ofs-(cudt(cudtnb).ofs\8) * 8 ' bits mod byte
				  cudt(cudtnb).lg=lgbits  'length in bits
				end If
			end If
		''''''''''''''''''EndIf 'end change 17/04/2014
		p=q+1
		cudt(cudtnb).ofs=cudt(cudtnb).ofs\8 'offset bytes
	Wend
	udt(udtidx).ub=cudtnb
End sub

'----------------
'' parse enum
'----------------
private sub parse_enum(readl As String)
'.stabs "TENUM:T26=eESSAI:5,TEST08:8,TEST09:9,TEST10:10,FIN:99,;",128,0,0,0
	Dim As Integer p,q
	Dim As String tnm
	p=InStr(readl,":")
	tnm=Left(readl,p-1)
	p+=2 'skip :T
	q=InStr(readl,"=")
	udtidx=Val(Mid(readl,p,q-p))
	udtidx+=udtcpt:If udtidx>udtmax Then udtmax=udtidx
	If udtmax > TYPEMAX Then hard_closing("Storing ENUM="+tnm+" limit reached "+Str(TYPEMAX))
	udt(udtidx).nm=tnm 'enum name

	udt(udtidx).en=udtidx 'flag enum, in case of already treated use same previous cudt
	udt(udtidx).lg=Len(Integer) 'same size as integer
	'each cudt contains the value (typ) and the associated text (nm)
	udt(udtidx).lb=cudtnb+1
	p=q+2
	cudtnbsav=cudtnb 'save value for restoring see enum_check
	If InStr(readl,";")=0 Then
		cudtnb+=1
		cudt(cudtnb).nm="DUMMY"
		cudt(cudtnb).val=0
		messbox("Storing ENUM="+tnm,"Data not correctly formated "):Exit sub
	Else
		While readl[p-1]<>Asc(";")
		q=InStr(p,readl,":") 'text
		If cudtnb>=CTYPEMAX Then hard_closing("Storing ENUM="+tnm+" limit reached "+Str(CTYPEMAX))
		cudtnb+=1
		cudt(cudtnb).nm=Mid(readl,p,q-p)

		p=q+1
		q=InStr(p,readl,",") 'value
		cudt(cudtnb).val=Val(Mid(readl,p,q-p))
		p=q+1

		Wend
	EndIf
	udt(udtidx).ub=cudtnb
	enum_check(udtidx) 'eliminate duplicates
End sub
'-------------------
'' parse variables
'-------------------
private sub parse_var(gv As String,ad As UInteger, dlldelta As Integer=0)
	Dim p As Integer
	Static defaulttype As Integer
	Dim As String vname

	If instr(gv,"va_list:t") Then 'last default type
		  defaulttype=0
	ElseIf InStr(gv,"integer:t") Then
		  defaulttype=1
	EndIf

	If defaulttype Then Exit sub

	'=====================================================
	vname=Left(gv,InStr(gv,":")+1)

	p=InStr(vname,"$")
	If p=0 Then 'no $ in the string
		If InStr(vname,":t")<>0 Then
			If UCase(Left(vname,InStr(vname,":")))<>Left(vname,InStr(vname,":")) Then
				Exit Sub 'don't keep  <lower case name>:t, keep <upper case name>:t  => enum
			EndIf
		ElseIf InStr(vname,"_ZTSN")<>0  orelse InStr(vname,"_ZTVN")<>0 then
			Exit Sub 'don't keep _ZTSN or _ZTVN (extra data for class) or with double underscore  __Z
		EndIf
		'dbg_prt2 "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ VNAME=";vname
		If Left(vname,3)="LT_" Then 'LT_xxxx
			Exit Sub
		elseif Left(vname,2)="_{" Then '_{fbdata}_<label name>
			Exit Sub
		elseif Left(vname,3)=".Lt" Then '.Ltxxxx used with data
			Exit Sub
		elseIf Left(vname,3)="Lt_" Then 'Lt_xxxx used with extern and array
			Exit Sub
		EndIf
		'dbg_prt2 "parse_var=";gv
	Else '$ in the string
		If InStr(p+1,vname,"$") <>0 AndAlso InStr(vname,"$fb_Object")=0 then
			Exit Sub 'don't keep TMP$xx$xx:
		EndIf
		'''''''''''''If InStr(vname,":T")<>0 OrElse InStr(vname,":t")<>0 Then
		'$9CABRIOLET:T(0,51)=s16$BASE:(0,48),0,128;;
		If InStr(vname,":t")<>0 Then 'InStr(vname,":T")<>0 OrElse InStr(vname,":t")<>0 Then
			If Left(vname,5)<>"$fb_O" andalso Left(vname,4)<>"TMP$" Then  'redim
				Exit Sub 'don't keep
			End if
		EndIf
		If InStr(vname,"$fb_RTTI") OrElse InStr(vname,"fb$result$") Then
			Exit Sub 'don't keep
		EndIf
		If Left(vname,3)="vr$" OrElse Left(vname,4)="tmp$" then
			Exit Sub 'don't keep  vr$xx: or tmp$xx$xx:
		EndIf
		'eliminate $ and eventually the number at the end of name ex udt$1 --> udt
		If Left(vname,4)<>"TMP$" Then ' use with redim
			If p<>1 Then gv=Left(gv,p-1)+Mid(gv,InStr(gv,":"))
		EndIf
	EndIf
	'======================================================
	If InStr(gv,";;") Then 'defined type or redim var
		If InStr(gv,":T") Then 'GCC change ":Tt" in just ":T"
					'UDT
			parse_udt(gv)
		Else
      'REDIM
			If parse_scope(gv[InStr(gv,":")],ad,dlldelta)=0 Then Exit Sub 'Scope / increase number and put adr
			'if common exists return 0 so exit sub
			vrb(*vrbptr).nm=Left(gv,InStr(gv,":")-1) 'var or parameter

		'.stabs "VTEST:22=s32DATA:25=*23=24=*1,0,32;PTR:26=*23=24=*1,32,32;SIZE:1,64,32;ELEMENT_LEN:1,96,32;DIMENSIONS:1,128,32;dim1_ELEMENTS:1,160,32;dim1_LBOUND:1,192,32;
	   'dim1_UBOUND:1,224,32;;
	   'DATA:27=*dim1_20=*21,0,32;PTR:28=*dim1_20=*21,32,32;SIZE:1,64,32;ELEMENT_LEN:1,96,32;DIMENSIONS:1,128,32;dim1_ELEMENTS:1,160,32;
	   'dim1_LBOUND:1,192,32;dim1_UBOUND:1,224,32;;21",128,0,0,-168

			p=InStr(gv,";;")+2 ' case dyn var including dyn array field...... 21/04/2014 to be removed when 0.91 is released
			While InStr(p,gv,";;")<>0 '29/04/2014
				p=InStr(p,gv,";;")+2
			Wend

			parse_var2(Mid(gv,p),TYRDM) 'datatype
			vrb(*vrbptr).arr=Cast(tarr Ptr,-1) 'redim array
		EndIf
	ElseIf InStr(gv,"=e") Then
		'ENUM
		parse_enum(gv)
	Else
		'DIM
		If InStr(gv,"FDBG_COMPIL_INFO") Then Exit Sub
		If gv[0]=Asc(":") Then Exit Sub 'no name, added by compiler don't take it
		p=parse_scope(gv[InStr(gv,":")],ad,dlldelta)'Scope / increase number and put adr
		If p=0 Then Exit Sub 'see redim
		If InStr(gv,"_ZN") AndAlso InStr(gv,":") Then
			vrb(*vrbptr).nm=parse_name(gv) 'namespace
		Else
			vrb(*vrbptr).nm=Left(gv,InStr(gv,":")-1) 'var or parameter
	      'to avoid two lines in proc/var tree, case dim shared array and use of erase or u/lbound
			If vrb(*vrbptr).mem=2 AndAlso vrb(*vrbptr).nm=vrb(*vrbptr-1).nm Then 'check also if shared
            *vrbptr-=1 'decrement pointed value, vrbgbl in this case
            Exit sub
			EndIf
			If vrb(*vrbptr).mem=1 Then ''local var ''2016/08/12
				If local_exist Then Exit sub'' check if same adr and name exist (case several for loops repeated with the same iterator)
			EndIf
		End If
		parse_var2(Mid(gv,InStr(gv,":")+p),TYDIM)
		'11/05/2014 'new way for redim
		If Left(udt(vrb(*vrbptr).typ).nm,7)="FBARRAY" Then 'new way for redim array

			'.stabs "__FBARRAY2:Tt23=s44DATA:24=*10,0,32;PTR:25=*7,32,32;SIZE:1,64,32;ELEMENT_LEN:1,96,32;DIMENSIONS:1,128,32;DIMTB:26=ar1;0;1;22,160,192;;",128,0,0,0
			'.stabs "MYARRAY2:S23",38,0,0,_MYARRAY2
			vrb(*vrbptr).pt=cudt(udt(vrb(*vrbptr).typ).lb).pt-1 'pointer always al least 1 so reduce by one
			vrb(*vrbptr).typ=cudt(udt(vrb(*vrbptr).typ).lb).typ 'real type
			vrb(*vrbptr).arr=Cast(tarr Ptr,-1) 'defined as dyn arr

			'dbg_prt2("dyn array="+vrb(*vrbptr).nm+" "+Str(vrb(*vrbptr).typ)+" "+Str(vrb(*vrbptr).pt)+" "+cudt(udt(vrb(*vrbptr).typ).lb).nm)
		EndIf
		'end new redim
	EndIf
End sub
'-----------------------
'' parse type operator
'-----------------------
private function parse_typeope(vchar As long) As String
	'RPiR8vector2D or R8vector2DS0_ or R8FBSTRINGR8VECTOR2D
	Dim As Long typ
	If vchar=Asc("P") Then
		Return "*" 'pointer
	Else
		'l=long/m=unsigned long/n=__int128/o=unsigned __int128/e=long double, __float80
		Select Case As Const vchar
			Case Asc("i")
				typ=1
			Case Asc("l")
			#Ifdef __FB_64BIT__
				typ=9
			#else
				typ=1
			#endif
			Case Asc("a")
				typ=2
			Case Asc("h")
				typ=3
			Case Asc("c") 'Zstring
				typ=4
			Case Asc("s")
				typ=5
			Case Asc("t")
				typ=6
			Case Asc("v")
				typ=7
		    Case Asc("j")',Asc("m")
				typ=8
			Case Asc("x")
				typ=9
			Case Asc("y")
				typ=10
			Case Asc("f")
				typ=11
			Case Asc("d")
				typ=12
		    case Asc("b")
				typ=16
			Case Asc("w") ''wString
				return "wstring"
			'Case Asc("")'Fstring
			'	typ=14
			Case Else
				typ=0
		End Select
		Return udt(typ).nm
	EndIf
End function

'-----------------
'' parse operator
'-----------------
private function parse_op (op As String) As string
Select Case  op
	case "aS"
    Function = "Let"
	Case "pl"
    Function = "+"
	Case "pL"
    Function = "+="
Case "mi"
    Function = "-"
Case "mI"
    Function = "-="
Case "ml"
    Function = "*"
Case "mL"
    Function = "*="
Case "dv"
    Function = "/"
Case "dV"
    Function = "/="
Case "Dv"
    Function = "\"
Case "DV"
    Function = "\="
Case "rm"
    Function = "mod"
Case "rM"
    Function = "mod="
Case "an"
    Function = "and"
Case "aN"
    Function = "and="
Case "or"
    Function = "or"
Case "oR"
    Function = "or="
Case "aa"
    Function = "andalso"
Case "aA"
    Function = "andalso="
Case "oe"
    Function = "orelse"
Case "oE"
    Function = "orelse="
Case "eo"
    Function = "xor"
Case "eO"
    Function = "xor="
Case "ev"
    Function = "eqv"
Case "eV"
    Function = "eqv="
Case "im"
    Function = "imp"
Case "iM"
    Function = "imp="
Case "ls"
    Function = "shl"
Case "lS"
    Function = "shl="
Case "rs"
    Function = "shr"
	Case "rS"
    Function = "shr="
Case "po"
    Function = "^"
Case "pO"
    Function = "^="
Case "ct"
    Function = "&"
Case "cT"
    Function = "&="
	Case "eq"
    Function = "eq"
Case "gt"
    Function = "gt"
Case "lt"
    Function = "lt"
Case "ne"
    Function = "ne"
Case "ge"
    Function = "ge"
Case "le"
    Function = "le"
Case "nt"
    Function = "not"
	Case "ng"
    Function = "neg"
	Case"ps"
    Function = "ps"
	Case "ab"
    Function = "ab"
	Case "fx"
    Function = "fix"
Case "fc"
    Function = "frac"
Case "sg"
    Function = "sgn"
Case "fl"
    Function = "floor"
Case "nw"
    Function = "new"
Case "na"
    Function = "new []?"
Case "dl"
    Function = "del"
Case "da"
    Function = "del[]?"
Case "de"
    Function = "operator *"
Case "pt"
    Function = "->"
Case "ad"
    Function = "@"
Case "fR"
    Function = "for"
Case "sT"
    Function = "step"
Case "nX"
    Function = "next"
	case "cv"
   	Function = "Cast"
	Case "C1"
		Function = "(Constructor)"
	Case "D1"
		Function = "(Destructor)"
Case Else
    Function = "Unknow"
End Select
End function
'--------------------
'' parse procedure
'--------------------
private function parse_proc(fullname As String) As String
	Dim As Long p=3,lg,namecpt,ps
	Dim As String strg,strg2,names(20),mainname,strg3
	lg=InStr(fullname,"@")
    If lg=0 Then lg=InStr(fullname,":")
    strg=Left(fullname,lg-1)

	If left(strg,2)<>"_Z" and left(strg,3)<>"__Z" Then Return strg

	If strg[2]=Asc("Z") Then p+=1 'add 1 case _ _ Z
	If strg[p-1]=Asc("N") Then 'nested waiting "E"
		mainname=""
		p+=1
		While Strg[p-1]<>Asc("E")
			lg=ValInt(Mid(strg,p,2)) 'evaluate possible lenght of name eg 7NAMESPC
			If lg Then 'name of namespace or udt
				If lg>9 Then p+=1 '>9 --> 2 characters
				strg3=Mid(strg,p+1,lg) 'extract name and keep it for later
				ps=InStr(strg3,"__get__")
				If ps Then
				   strg3=Left(strg3,ps-1)+" (Get property)"
				Else
				   ps=InStr(strg3,"__set__")
				   If ps then
						strg3=Left(strg3,ps-1)+" (Set property)"
				   EndIf
				EndIf
				if mainname="" Then
					mainname=strg3
					strg2+=strg3
				else
					mainname+="."+strg3
					strg2+="."+strg3
				EndIf
				namecpt+=1
				names(namecpt)=mainname
				p+=1+lg'next name
			Else 'operator
				strg2+=" "+parse_op(Mid(strg,p,2))+" " 'extract name of operator
				p+=2
				mainname=""
				While Strg[p-1]<>Asc("E") 'more data eg FBSTRING,
					lg=ValInt(Mid(strg,p,2))
					If lg Then
						If lg>9 Then p+=1
						strg3=Mid(strg,p+1,lg) 'extract name and keep it for later
						If strg3="FBSTRING" Then strg3="string"
            		If mainname="" Then
            			mainname=strg3
            			strg2+=strg3
            		Else
            			mainname+="."+strg3
            			strg2+="."+strg3
            		endif
            		namecpt+=1
            		names(namecpt)=mainname
						p+=1+lg
					Else
						strg2+=parse_typeope(Asc(Mid(strg,p,1)))
						p+=1
					EndIf
				Wend

			EndIf
		Wend
	Else
	   lg=ValInt(Mid(strg,p,2)) 'overloaded proc eg. for sub testme overload (as string) --> __ZN6TESTMER8FBSTRING@4
	   If lg Then
			If lg>9 Then p+=1
			strg2=Mid(strg,p+1,lg) 'extract name
			p+=1+lg'next
	   Else
		   strg2=parse_op(Mid(strg,p,2))+" "
		   p+=2
	   End If
	EndIf

	If strg[p-1]=Asc("E") Then p+=1 'skip "E"

	'parameters
	mainname=""
	strg2+="("
	While p<=Len(strg)
		lg=ValInt(Mid(strg,p,2))
		If lg Then
			If lg>9 Then p+=1
			strg3=Mid(strg,p+1,lg) 'extract name and keep it for later
			If strg3="FBSTRING" Then strg3="String"
            if mainname="" Then
       			mainname=strg3
       			strg2+=strg3
       		Else
       			mainname+="."+strg3
       			strg2+="."+strg3
       		EndIf
       		namecpt+=1
            names(namecpt)=strg3'mainname
			p+=1+lg
		elseIf strg[p-1]=Asc("R") Then
			If Right(strg2,1)<>"(" AndAlso Right(strg2,1)<>"," Then
				strg2+=","
			EndIf
			strg2+="@"
			p+=1
		elseIf strg[p-1]=Asc("N") Then
			If Right(strg2,1)<>"(" AndAlso Right(strg2,1)<>"," Then strg2+=","
			mainname=""
			p+=1
		elseIf strg[p-1]=Asc("K") Then
			If Right(strg2,1)<>"(" AndAlso Right(strg2,1)<>"," Then
				strg2+=",const."
			Else
				strg2+="const."
			EndIf
			p+=1
		elseIf strg[p-1]=Asc("E") then
			p+=1
		ElseIf strg[p-1]=Asc("S") Then 'S_ = 1 S0_=2 S1_=3 -->	'repeating a previous type
    		If Right(strg2,1)<>"(" AndAlso Right(strg2,1)<>"," AndAlso Right(strg2,2)<>"* " Then
    			strg2+=",":mainname=""
    		EndIf
			p+=1
			If strg[p-1]=asc("_") Then
				strg3=names(1)
				if strg[p-3]=Asc("P") then
					namecpt+=1
					names(namecpt)="* "+strg3
				EndIf
				p+=1
			Else
				strg3=names(strg[p-1]-46)
				if strg[p-3]=Asc("P") then
					namecpt+=1
					names(namecpt)=strg3
				EndIf
				p+=2
			endif
	        if mainname="" Then
       			strg2+=strg3
	        else
       			strg2+="."+strg3
	        endif
		elseIf strg[p-1]=Asc("P") then ''pointer followed by datatype
			p+=1
			If Right(strg2,1)="(" Then
				strg2+="* "
			else
				strg2+=",* "
			end if
	    elseif strg[p-1]=Asc("u") then  ''extended type ex : u7INTEGER
			p+=1
	        lg=ValInt(Mid(strg,p,2))
			If lg>9 Then p+=1 ''lenght>9
			strg3=Mid(strg,p+1,lg) 'extract name and keep it for later
			strg3=ucase(left(strg3,1))+lcase(mid(strg3,2)) ''first char upper/ others lower case
			if mainname="" Then
				mainname=strg3
				strg2+=strg3
			Else
				mainname+="."+strg3
				strg2+="."+strg3
			EndIf
			namecpt+=1
            names(namecpt)=strg3
			p+=1+lg
		else
			strg3=parse_typeope(Asc(Mid(strg,p,1)))
			if strg[p-2]=Asc("P") then
				namecpt+=1
				names(namecpt)="* "+strg3
				strg2+=strg3
			else
				If Right(strg2,1)="(" Then
					strg2+=strg3
				Else
					strg2+=","+strg3
				EndIf
			end if
			p+=1
		EndIf
	Wend

	strg2+=")"
	If Right(strg2,6)="(Void)" Then
		strg2=Left(strg2,Len(strg2)-6)
	endif
	Return strg2
End Function
'' --------------------------------
'' check if source already stored
'' --------------------------------
private function check_source(sourcenm As String) As integer
   For i As Integer=0 To sourcenb
      If source(i)=sourcenm Then Return i 'found
	Next
	Return -1 'not found
End function
'' -------------------------------
'' Handling main source code=100
'' -------------------------------
private sub dbg_file(strg as string,value as integer)
	static as string path
	dim as string fullname
	if strg="" then ''end of main ?
		udtcpt=udtmax-TYPESTD
		'dbg_prt2 "---------------- end of current debug data -------------------"
	else
		if right(strg,4)<>".bas" and right(strg,3)<>".bi" then
			path=strg
		else
			#ifdef __fb_win32__
				fullname=lcase(path+strg)
			#else
				fullname=path+strg ''file names are sensitive on linux
			#endif
			path=""

			'dbg_prt2 "full source name=";fullname
			if check_source(fullname)=-1 then ''useful check_source ???
				sourcenb+=1
				source(sourcenb)=fullname
				srcname(sourcenb)=source_name(source(sourcenb))
				list_insert(srclist(),strptr(srcname(sourcenb)),sourcenb+1,srclistfirst) ''zero based so add 1
				sourceix=sourcenb
				'dbg_prt2 "new source=";fullname
			else
				'dbg_prt2 "keep current source with code 100 check that =";strg
			end if
		end if
	end if
end sub
'' --------------------------------------------------------
'' Handling include or return in previous source code=132
'' --------------------------------------------------------
private sub dbg_include(strg as string)
	dim as integer temp
	if instr(strg,any "/\")=0 then ''just the name no path
		strg=left(source(0),instrrev(source(0),any "/\"))+strg ''adding path
	end if
	#ifdef __fb_win32__
		strg=lcase(strg) ''only WDS file names are sensitive on LNX
	#endif

	temp=check_source(strg)
	if temp=-1 Then
		sourcenb+=1
		source(sourcenb)=strg
		sourceix=sourcenb
		srcname(sourcenb)=source_name(source(sourcenb))
		list_insert(srclist(),strptr(srcname(sourcenb)),sourcenb+1,srclistfirst) ''zero based so add 1
		'dbg_prt2 "new source=";sourcenb,strg
	else
	  	sourceix=temp
		'dbg_prt2 "keep current source=";sourceix,strg
	end if
end sub
'' -----------------------
'' Handling line code=68
'' -----------------------
private sub dbg_line(linenum as integer,ofset as integer)
	if linenum then
		#Ifndef __FB_64BIT__
		''to skip stabd
		if linenum<rline(linenb).nu then
			if procnb=rLine(linenb).px then
				exit sub
			end if
		end if
		#endif
		if ofset+proc(procnb).db<>rline(linenb).ad Then ''checking to avoid asm with just comment line
			linenb+=1
		endif
		rline(linenb).ad=ofset+proc(procnb).db
		rLine(linenb).nu=linenum
		rLine(linenb).px=procnb

		if procnew <> procnb then ''find the address for first fbc line of the proc
			if ofset<>0 then
				procnew=procnb
				proc(procnb).first=rline(linenb).ad
			EndIf
		EndIf
		''to be checked maybe fixed so useless
		rline(linenb).sx=sourceix ''for line in include and not in a proc
		'dbg_prt2 "linenum=";linenum,hex(rline(linenb).ad)
	else
		'dbg_prt2 "line number=0"
	end if
end sub
'' ---------------------------------------------
'' Handling procedure sub/function/etc code=36
'' ---------------------------------------------
private sub dbg_proc(strg as string,linenum as integer,adr as integer)
	dim as string procname
	if linenum then
		procnodll=false
		procname=parse_proc(strg)

		'procname=left(strg,instr(strg,":")-1)
		if procname<>"" and (flagmain=true or procname<>"main") then
			'If InStr(procname,".LT")=0 then  ''to be checked if useful
		 	If flagmain=TRUE And procname="main" Then
				procmain=procnb+1
		 		flagmain=false
		 		'flagstabd=TRUE'first main ok but not the others
				'dbg_prt2 "main found=";procnb+1
		 	endif
			procnodll=true

			procnb+=1
			'dbg_prt2 "in proc=";procname,procnb,sourceix
			proc(procnb).sr=sourceix
			proc(procnb).nm=procname
			list_insert(proclist(),strptr(proc(procnb).nm),procnb,proclistfirst)

			proc(procnb).db=adr'+exebase-baseimg 'only when <> exebase and baseimg (DLL)
			''to be added
			parse_retval(procnb,Mid(strg,InStr(strg,":")+2,99)) 'return value .rv + pointer .pt
			proc(procnb).enab=true
			proc(procnb).nu=linenum
			lastline=0
			proc(procnb+1).vr=proc(procnb).vr 'in case there is not param nor local var
			proc(procnb).rvadr=0 'for now only used in gcc case

			'dbg_prt2 "proc =";proc(procnb).nm;" in source=";source(proc(procnb).sr)
		end if
	else
		proc(procnb).ed=proc(procnb).db+adr
		'dbg_prt2 "end of proc=";proc(procnb).ed,hex(proc(procnb).ed)
		proc(procnb).sr=sourceix
		if proc(procnb).fn>procfn Then procfn=proc(procnb).fn+1 ' just to be sure to be above see gest_brk

		''todo be checked ??????
		'for proc added by fbc (constructor, operator, ...) ''adding >2 to avoid case only one line ...
		'dbg_prt2 "Checking procedure added by compiler =";proc(procnb).nm,proc(procnb).nu,rline(linenb).nu,hex(proc(procnb).db),hex(proc(procnb).fn)
		'If proc(procnb).nu=rline(linenb).nu AndAlso linenb>2 then
		If rline(linenb).nu=1 then

			proc(procnb).nu=-1
			linenb-=1
			dbg_prt2 "Procedure added by compiler (constructor, etc) =";proc(procnb).nm
           	'For i As Integer =1 To linenb
           		'dbg_prt2("Proc db/fn inside for stab="+Hex(proc(procnb).db)+" "+Hex(proc(procnb).fn))
           		'dbg_prt2("Line Adr="+Hex(rline(i).ad)+" "+Str(rline(i).ad))
           		'If rline(i).ad>=proc(procnb).db AndAlso rline(i).ad<=proc(procnb).fn Then
           			'#Ifdef __fb_win32__
						'dbg_prt2 "Cancel breakpoint adr="+Hex(rline(i).ad)+" "+Str(rline(i).ad))
						'WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@rLine(i).sv,1,0)
           			'#else
						'dbg_prt2 "Procedure added by compiler (constructor, etc) line number should be -1=";proc(procnb).nm,rline(i).nu
           			'#endif
           			''nota rline(linenb).nu=-1
           		'EndIf
           	'next
		end if

        ''removing {modlevel empty just prolog and epilog
        if proc(procnb).nm="{MODLEVEL}" And proc(procnb).fn-proc(procnb).db <8 Then
             ''removing lines
            for iline As Long = linenb To 1 Step -1
                If rline(iline).px=procnb Then
                   'writeProcessMemory(dbghand,Cast(LPVOID,rline(iline).ad),@rLine(iline).sv,1,0) 'restore to avoid stop
                   linenb-=1
                Else
                   Exit For
                EndIf
            next
            'dbg_prt2 "remove modlevel in"+source(proc(procnb).sr)
            procnb-=1 ''removing proc
        end if

	end if
end sub
'' ------------------------------------
'' Handling procedure epilog code=224
'' ------------------------------------
private sub dbg_epilog(ofset as integer)
	proc(procnb).fn=proc(procnb).db+ofset
	if proc(procnb).fn<>rline(linenb).ad then
		if proc(procnb).nm<>"main" and proc(procnb).nm<>"{MODLEVEL}" then
		'' this test is useless as for sub it is ok  .fn = .ad  --> mov rsp, rbp
		'' for function the last line ('end function' is not given by 224)
		'' so forcing it except for main
		'dbg_prt2 "EPILOG=";hex(rline(linenb).ad),hex(proc(procnb).fn),"##";proc(procnb).nm;"##"
			rline(linenb).ad=proc(procnb).fn ''KEEP this line even if test is removed
		'else
			'proc(procnb).fn=rline(linenb).ad
		end if
	end if
	if procnb=procmain then
		for iline as integer =1 to linenb
			if rLine(iline).px=procmain then
				rLine(iline).nu=99999999 ''to avoid unexecutable line if first fbc line is 1 in main
				proc(procmain).nu=rline(iline+1).nu
				exit for
			EndIf
		Next
		''in case there are procedures after the last line of main
		for iline as integer = linenb to 1 step -1
			if rLine(iline).px=procmain then
				for iproc as integer = 1 to procnb
					if proc(iproc).nu=rLine(iline).nu then
						if proc(iproc).sr=proc(procmain).sr then
							linenb-=1
							exit for,for
						EndIf
					EndIf
				Next
				exit for
			EndIf
		Next
	end if
end sub
'' -------------------------------
'' Extracting debug data for elf
'' -------------------------------
private sub load_dat(byval ofset as integer,byval size as integer,byval ofstr as integer)
	dim as ustab stab
	dim as integer value
	dim as string strg
	ofstr+=1 ''1 based
	#Ifdef __FB_64BIT__
		dim as longint buf(1) ''16 bytes
	#else
		dim as long buf(2) ''12 bytes
	#endif
	dim as integer ofsmax,ofstemp
	#Ifdef __FB_64BIT__
	for ibuf as integer =1 to size/16
	#else
	for ibuf as integer =1 to size/12
	#endif
		get #1,ofset+1,buf()
		#Ifdef __FB_64BIT__
		stab.full=buf(0)
		#else
		stab.offst=buf(0)
		stab.cod=buf(1)  and &hffff
		stab.desc=buf(1) shr 16
		#endif
		'dbg_prt2 "C="+str(stab.cod)+" D="+str(stab.desc)+" O="+str(stab.offst);" ";
		strg=space(2000)
		get #1,ofstr+stab.offst,strg
		strg=""+*strptr(strg)
		ofstemp=ofstr+stab.offst+len(strg)+1
		if ofstemp>ofsmax then
			ofsmax=ofstemp
		EndIf
		'dbg_prt2 strg,ofstr,stab.offst,ofsmax
		#Ifdef __FB_64BIT__
			value=buf(1)
		#else
			value=buf(2)
		#endif
		'dbg_prt2 "D="+str(value)+" H="+hex(value)

		select case as const stab.cod
			case 100 '' file name
				dbg_file(strg,value)
			case 255 ''not as standard stab freebasic version and maybe other information
				'dbg_prt2 "compiled with=";strg
			Case 32,38,40,128,160 'init common/ var / uninit var / local / parameter
               	parse_var(strg,value)',exebase-baseimg) ''todo
			case 132 '' file name
				'dbg_prt2 "dbg include=";strg
				dbg_include(strg)
			case 36 ''procedure
				dbg_proc(strg,stab.desc,value)
			case 68 ''line
				dbg_line(stab.desc,value)
			case 224 ''address epilog
				dbg_epilog(value)
			case 42 ''main entry point
				'not used
			case 0
				#Ifdef __FB_64BIT__
				if ofsmax<>ofstr+7 then
					ofstr=ofsmax
				end if
				#endif
			case else
				dbg_prt2 "Unknow stab cod=";stab.cod
		end select
		#Ifdef __FB_64BIT__
		ofset+=16
		#else
		ofset+=12
		#endif
	next
	'dbg_prt2
end sub
'' ------------------------------------------------------------------------------------
'' Retrieving sections .dbgdat (offset and size) and .dbgdat (offset) in the elf file
'' return 0 if an error (wrong bitness) otherwise -1
'' ------------------------------------------------------------------------------------
private function elf_extract(filename as string) as integer
	dim lgf As Integer,ulgt as integer,ulg as ulong,usht as ushort,ofset as ulongint,ubyt as ubyte
	dim as integer start_section,str_section,sect_num,walk_section,dbg_dat_of,dbg_dat_size,dbg_str_of
	dim as string sect_name=space(40)

	open filename for binary as #1
	lgf=lof(1)
	'dbg_prt2 "lenght=";lgf

	ofset=of_entry
	get #1,ofset+1,ulgt
	'dbg_prt2 "entry=";hex(ulgt)

	ofset=of_section
	get #1,ofset+1,ulgt
	'dbg_prt2 "start section header=";hex(ulgt)
	start_section=ulgt

	ofset=of_section_size
	get #1,ofset+1,usht
	'dbg_prt2 "section size=";usht


	ofset=of_section_num
	get #1,ofset+1,usht
	'dbg_prt2 "section number=";usht
	sect_num=usht

	ofset=of_section_str
	get #1,ofset+1,usht
	'dbg_prt2 "section string=";usht
	ofset=start_section+(usht)*sect_size+of_offset_infile
	get #1,ofset+1,ulgt
	'dbg_prt2 "start offset string=";hex(ulgt)
	str_section=ulgt

	''sections

	walk_section=start_section
	for isec as integer = 1 to sect_num
		'dbg_prt2 "section=";isec;" ";

		ofset=walk_section
		get #1,ofset+1,ulg ''offset in str table
		'dbg_prt2 "offset string=";ulg;" ";
		ofset=str_section+ulg
		sect_name=space(40)
		get #1,ofset+1,sect_name
		sect_name=""+*strptr(sect_name)
		'dbg_prt2 "name=";sect_name

		ofset=walk_section+of_offset_infile
		get #1,ofset+1,ulgt
		'dbg_prt2 "offset in file= ";hex(ulgt);" ";
		#Ifdef __FB_64BIT__
		If sect_name=".dbgdat" Then
		#else
		If sect_name=".stab" Then
		#endif
			'dbg_prt2 "name=";sect_name
			dbg_dat_of=ulgt
		#Ifdef __FB_64BIT__
		ElseIf sect_name=".dbgstr" Then
		#else
		ElseIf sect_name=".stabstr" Then
		#endif
			'dbg_prt2 "name=";sect_name
			dbg_str_of=ulgt
			exit for ''not anymore section to retrieve
		end if

		ofset=walk_section+of_size_infile
		get #1,ofset+1,ulgt
		'dbg_prt2 "size= ";hex(ulgt);" ";ulgt
		#Ifdef __FB_64BIT__
		If sect_name=".dbgdat" Then
		#else
		If sect_name=".stab" Then
		#endif
			dbg_dat_size=ulgt
		endif

		walk_section+=sect_size
	next
	if dbg_dat_of=0 then
		messbox("Loading error","Debug data not found, compile with -gen gas/gas64 and -g")
		close #1
		return 0
	else
		load_dat(dbg_dat_of,dbg_dat_size,dbg_str_of)
	endif
	close #1
	return -1
end function
'============================================================================
'' PE_extract inside memory from loaded sections (when debuggee is running)
'' return -1 if any problem
'============================================================================
private function debug_extract(exebase As UInteger,nfile As String,dllflag As Long=NODLL) as integer
	'
	'lastline As UShort=0,firstline As Integer=0
	'integer -->,proc1,proc2
	'Dim sourceix As Integer,sourceixs As Integer
	'Dim As Byte procfg,flag=0,procnodll=TRUE,flagstabd=TRUE 'flags  (flagstabd to skip stabd 68,0,1)
	'Dim procnmt As String

	dim As Integer pe,flagdll
	dim as integer secnb
	dim as string *8 secnm
	Dim As Integer basestab=0,basestabs=0,baseimg,sizemax,sizestabs
	Dim As udtstab recupstab
	Dim recup As ZString *STAB_SZ_MAX

	flagdll=dllflag
	vrbgblprev=vrbgbl
	linenbprev=linenb ''used for dll

	statusbar_text(KSTBSTS,"Loading debug data")

	ReadProcessMemory(dbghand,Cast(LPCVOID,exebase+&h3C),@pe,4,0)
	pe+=exebase+6 'adr nb section
	ReadProcessMemory(dbghand,Cast(LPCVOID,pe),@secnb,2,0)
	#Ifdef __FB_64BIT__
		pe+=42
	#else
		pe+=46 'adr compiled baseimage
	#endif
	ReadProcessMemory(dbghand,Cast(LPCVOID,pe),@baseimg,sizeof(integer),0)
	#Ifdef __FB_64BIT__
		pe+=&hD8
	#else
		pe+=&hC4 'adr sections
	#EndIf

	For i As UShort =1 To secnb
		Dim As UInteger basedata,sizedata
		secnm=String(8,0) 'Init var
		ReadProcessMemory(dbghand,Cast(LPCVOID,pe),@secnm,8,0) 'read 8 bytes max name size
	#Ifdef __FB_64BIT__
		If secnm=".dbgdat" Then
	#else
		If secnm=".stab" Then
	#endif
			ReadProcessMemory(dbghand,Cast(LPCVOID,pe+12),@basestab,4,0)
	#Ifdef __FB_64BIT__
		ElseIf secnm=".dbgstr" Then
	#else
		ElseIf secnm=".stabstr" Then
	#endif
			ReadProcessMemory(dbghand,Cast(LPCVOID,pe+12),@basestabs,4,0)
			ReadProcessMemory(dbghand,Cast(LPCVOID,pe+8),@sizestabs,4,0)
		EndIf

		pe+=40
	Next
	If basestab=0 OrElse basestabs=0 Then
		If flagdll=NODLL Then
			if flagattach=false then
				messbox("NO information for Debugging","compile with -gen gas/gas64 and -g"+chr(10)+"killing the debuggee")
				#ifdef __fb_win32__
					KillTimer(hmain,GTIMER001)
					terminateprocess(dbghand,-1)
				#else
					''todo linux function from W9
					messbox("Feature missing for Linux","Kill_process")
				#endif
			else
				hard_closing("Attaching running program"+chr(10)+"No information for Debugging")
			end if
		EndIf
		return -1
	Else
		basestab+=exebase+sizeof(udtstab) ''12 for 32bit / 16 for 64bit could be greater if udtstab is changed
		basestabs+=exebase

		While 1
			If ReadProcessMemory(dbghand,Cast(LPCVOID,basestab),@recupstab,sizeof(udtstab),0)=0 Then
				#Ifdef fulldbg_prt
					#ifdef __fb_win32__
						dbg_prt ("error reading memory "+Str(GetLastError))
					#else
						dbg_prt ("error reading memory")
					#EndIf
				#EndIf
				messbox("Loading stabs","ERROR When reading memory"):return -1
			End If

			#Ifdef fulldbg_prt
			dbg_prt ("stabs="+Str(recupstab.stabs)+" "+Str(recupstab.code)+" "+Str(recupstab.nline)+" "+Str(recupstab.ad))
			#EndIf

			If recupstab.code=0 Then Exit While
			If sizestabs-recupstab.stabs>STAB_SZ_MAX Then
			'fb_message("Loading stabs","ERROR not enough space to load stabs string (" + Str(STAB_SZ_MAX) + "), change STAB_SZ_MAX"):Exit Sub
				sizemax=STAB_SZ_MAX
			Else
				sizemax=sizestabs-recupstab.stabs
			EndIf

			If ReadProcessMemory(dbghand,Cast(LPCVOID,recupstab.stabs+basestabs),@recup,sizemax,0)=0 Then
				#Ifdef fulldbg_prt
					#ifdef __fb_win32__
						dbg_prt ("error reading memory "+Str(GetLastError))
					#else
						dbg_prt ("error reading memory")
					#EndIf
				#EndIf
				messbox("Loading stabs","ERROR When reading memory"+Chr(10)+"Exit loading"):return -1
			End If

			#Ifdef fulldbg_prt
				dbg_prt (recup)
			#EndIf
	'=========================================
			select case as const recupstab.code
				case 100 '' file name
					dbg_file(recup,recupstab.ad)
				case 255 ''not as standard stab freebasic version and maybe other information
					compilerversion=recup
				Case 32,38,40,128,160 'init common/ var / uninit var / local / parameter
					parse_var(recup,recupstab.ad)',exebase-baseimg) ''todo
				case 132 '' file name
					dbg_include(recup)
				case 36 ''procedure
					dbg_proc(recup,recupstab.nline,recupstab.ad)
				case 68 ''line
					dbg_line(recupstab.nline,recupstab.ad)
				case 224 ''address epilog
					dbg_epilog(recupstab.ad)
				case 42 ''main entry point
					'not used
				case else
					'dbg_prt2 "Unknow stab cod=";recupstab.code
			end select
	'=========================================
			basestab+=sizeof(udtstab)
		Wend
	EndIf
end function
'===================================================
'' list all extracted data
'===================================================
private sub list_all()
	dim scopelabel(1 to ...) as const zstring ptr={@"local",@"global",@"static",@"byref param",@"byval param",@"common"}
	dbg_prt2 "sources ------------------------------------------------------- ";"total=";sourcenb+1
	for isrc as integer =0 to sourcenb
		dbg_prt2 "isrc=";isrc;" ";source(isrc)
	next
	dbg_prt2 "procedures ------------------------------------------------------- ";procnb
	for iprc as integer =1 to procnb
		dbg_prt2 "iprc=";iprc;" ";source(proc(iprc).sr);" ";proc(iprc).nm;" ";proc(iprc).nu;" ";udt(proc(iprc).rv).nm
		dbg_prt2 "lower/upper/end ad=";hex(proc(iprc).db);" ";hex(proc(iprc).fn);" ";hex(proc(iprc).ed)
	next
	dbg_prt2 "Lines ---------------------------------------------------------- ";linenb
	for iline as integer = 1 to linenb
		dbg_prt2 "iline=";iline;" proc=";proc(rline(iline).px).nm;" ";rline(iline).nu;" ";hex(rline(iline).ad)
	next
	dbg_prt2
	dbg_prt2 "types ----------------------------------------------------------- ";udtmax
	for iudt as integer=1 to udtmax
		if udt(iudt).nm<>"" then
			dbg_prt2 "iudt=";iudt;" ";udt(iudt).nm;" ";udt(iudt).lg
			if udt(iudt).ub<>0 then
				for icudt as integer =udt(iudt).lb to udt(iudt).ub
					dbg_prt2 "icudt=";cudt(icudt).nm
				next
			end if
		end if
	next
	dbg_prt2 "global variables ---------------------------------------------------------- ";vrbgbl
	for ivrb as integer=1 to vrbgbl
		dbg_prt2 "ivrb=";ivrb;" ";vrb(ivrb).nm;" ";udt(vrb(ivrb).typ).nm;" ";vrb(ivrb).adr;" ";*scopelabel(vrb(ivrb).mem)
	next
	dbg_prt2 "local variables ----------------------------------------------------------- ";vrbloc-VGBLMAX
	for ivrb as integer=VGBLMAX+1 to vrbloc
		dbg_prt2 "ivrb=";ivrb;" ";vrb(ivrb).nm;" ";udt(vrb(ivrb).typ).nm;" ";vrb(ivrb).adr;" ";*scopelabel(vrb(ivrb).mem)
	next
	dbg_prt2 "end of list all"
end sub


