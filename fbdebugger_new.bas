'=================================================================
'===== DEBUGGER FOR FREEBASIC === (C) 2006-2021 Laurent GRAS =====
'=================================================================
#include once "dbg_defines.bi"

''source files
dim Shared as String  source(SRCMAX)        ''source names
dim shared as any ptr sourceptr(SRCMAX)     ''pointer doc scintilla
dim Shared As UByte   sourcebuf(SRCSIZEMAX) ''buffer for loading source file
dim Shared as Integer sourcenb =-1          ''number of src, 0 based
dim Shared As integer sourceix              ''source index when loading data
dim shared as any ptr currentdoc            ''current doc pointer


''lines
dim Shared As Integer linenb,rlineprev ''numbers of lines, index of previous executed line (rline)
dim Shared As Integer linenbprev ''used for dll
dim Shared As Integer lastline
dim Shared as tline rline(LINEMAX)

''procedures
dim Shared as tproc proc(PROCMAX) ''list of procs in code
dim shared As integer procnb

dim Shared As integer procsv,procad,procin,procsk,proccurad,procregsp,procfn,procbot,proctop,procsort
dim Shared As tprocr procr(PROCRMAX) ''list of running proc
dim shared As Integer procrnb


''put in a ctx with type ??
dim shared As boolean procnodll
dim shared As boolean flagmain

''handle
dim shared as HWND mainwindow,scint

''flags
''todo type and tflag and flag.restart, etc
dim shared as boolean flagrestart = true


#include "dbg_extract.bas"
#include "dbg_gui.bas"
#include "dbg_tools.bas"


'if prbm
'Declare Function AddTreeViewItem OverLoad(ByVal gadget As long, ByRef string_ As String, ByVal Idimage_0 As HBITMAP, ByVal Idimage_Selected As HBITMAP, ByVal pos_ As integer, ByVal parent As Integer=0) As Integer
'Declare Function AddTreeViewItem(ByVal gadget As Long, ByRef string_ As String,ByVal Idimage_0 As HICON,ByVal Idimage_Selected As HICON,ByVal pos_ As integer,ByVal parent As Integer=0) As integer



gui_init

SetStatusBarField(1,0,100,"Loading data")
dim as string exename="D:\telechargements\win9\tmp\test_include_main"
dim as string title="Fbdebugger "+exename
setwindowtext(mainwindow,strptr(title))
elf_extract(exename)
print "------------------ after extraction -----------------------"
print "Number of files=";sourcenb

'hidewindow(scint ,1)
for isrc as integer =0 to sourcenb
	source(isrc)="D:\telechargements\win9\tmp\"+source_name(source(isrc))
	print "index=";isrc;" file=";source(isrc)

	AddPanelGadgetItem(GSRCTAB,isrc,source_name(source(isrc)))
	''later sort the files to get them in alphabetic order
	AddComboBoxItem(GFILELIST,source_name(source(isrc)),-1)
next
'SetGadgetFont(GSRCTAB,CINT(LoadFont("Courier New",10)))
source_load(0,filedatetime(exename))

''for testing to be removed
	
	line_color(5,2)
	line_color(10,2)
	for imark as Integer = 0 To 5
		send_sci(SCI_MarkerAdd, imark, imark)       'line, marker#
	next
hidewindow(scint ,0)
SetStatusBarField(1,0,100,"Waiting")
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

''========================================
''========================================
''========================================
''========================================


''=================================================
''contextual menus
#define MNSETBRK 1001
#define MNDISAS  1002
#define MNLOCSRC 1101
#define MNDISPROC 1102
#define MNEDITVAR 1201
#define MNDUMPVAR 1202

var HMenusource=CreatePopMenu()
MenuItem(1001,HMenusource,"Set breakpoint")
MenuItem(1002,HMenusource,"Disass line")

var HMenuvar=CreatePopMenu()
MenuItem(1201,HMenuvar,"Edit var")
MenuItem(1202,HMenuvar,"Dump var")


var HMenuprc=CreatePopMenu()
MenuItem(1101,HMenuprc,"Locate in source")
MenuItem(1102,HMenuprc,"Disable proc")



do
	Var event=WaitEvent()
	If Event=EventClose then
		if messbox("Quitting Fbdebugger","Are you sure ? (pgm still running)",MB_YESNO)=6 then
			''todo need to release doc SCI_RELEASEDOCUMENT(<unused>, pointer doc)
			release_doc
			end
		endif
	end if
	If event=EventMenu then
		Select case EventNumber
			case MNSETBRK
				MessBox("","Set breakpoint")
			case MNDISAS
				MessBox("","Disass line")
			case MNLOCSRC
				MessBox("","Locate in source")
			case MNDISPROC
				MessBox("","Disable proc")
		End select
	ElseIf event=eventrbdown then
		if GlobalMouseX<500 then
			DisplayPopupMenu(HMenusource, GlobalMouseX,GlobalMouseY)
		else
			if PanelGadgetGetCursel(GRIGHTTABS)=0 then
				DisplayPopupMenu(HMenuvar, GlobalMouseX,GlobalMouseY)
			elseif PanelGadgetGetCursel(GRIGHTTABS)=1 then
				DisplayPopupMenu(HMenuprc, GlobalMouseX,GlobalMouseY)
			endif
		endif
	elseif event=eventgadget then
		if eventnumber()=GFILESEL Then
        	if GetItemComboBox(GFILELIST)<>-1 then
        		if GetItemComboBox(GFILELIST)<>PanelGadgetGetCursel(GSRCTAB) then
	        		MessBox("Jumping to file ="+str(GetItemComboBox(GFILELIST)),source(GetItemComboBox(GFILELIST)))
	        		PanelGadgetSetCursel(GSRCTAB,GetItemComboBox(GFILELIST))
        			source_change(GetItemComboBox(GFILELIST))
        		else
        			MessBox("File already displayed",source(GetItemComboBox(GFILELIST)))
        		endif
        	else
        		messbox("Select a file", "before clicking on the button")
        	endif
		elseif eventnumber()=GCURRENTLINE then
			if PanelGadgetGetCursel(GSRCTAB)<>0 then
				messbox("Click on Current line","Jumping to the file/line")
				source_change(0)''todo doc where is the next executed line
				send_sci(SCI_SETFIRSTVISIBLELINE, 2,0)
				send_sci(SCI_LINESCROLL,0,-5)
				line_color(5,0)
			end if
		elseif eventnumber()=GSRCTAB then
			Messbox("The selected panel for file:",source(PanelGadgetGetCursel(GSRCTAB)))
			source_change(PanelGadgetGetCursel(GSRCTAB))
		endif
	endif
loop
'Infos à garder
'win9GetCurrent - renvoie la fenêtre actuelle de la liste liée, pour y placer des gadgets. Identique à UseGadgetList, mais vice versa.
'win9AddNewGadget - Ajoute un gadget et sa fenêtre associée à la liste liée

