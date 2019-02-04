Program TicTacToe;
Uses Crt, Windows, Console, Board;
Const WinWidth : Integer = 150;
Type
EKey = Procedure (event: KEY_EVENT_RECORD);
EMouse = Procedure (event: MOUSE_EVENT_RECORD);
EWinBufferSize = Procedure (event: WINDOW_BUFFER_SIZE_RECORD);
EFocus = Procedure (event: FOCUS_EVENT_RECORD);
EMenu = Procedure (event: MENU_EVENT_RECORD);

Var
irInBuf: Array Of INPUT_RECORD;
cNumRead: DWord;
i: Integer;
onKey: EKey;
onMouse: EMouse;
onWinBufferSize: EWinBufferSize;
onFocus: EFocus;
onMenu: EMenu;
anyFile: Text;
str: String;

Procedure Noop(event: KEY_EVENT_RECORD); Begin End;
Procedure Noop(event: MOUSE_EVENT_RECORD); Begin End;
Procedure Noop(event: WINDOW_BUFFER_SIZE_RECORD); Begin End;
Procedure Noop(event: FOCUS_EVENT_RECORD); Begin End;
Procedure Noop(event: MENU_EVENT_RECORD); Begin End;

Procedure WriteCenter(str : String; ln : Integer);
Begin
	GoToXY((WinWidth - Length(str)) div 2, ln);
	Write(str);
End;

Procedure StopProgram();
Begin
	RestoreConsole();
	Halt();
End;

Begin
	TextBackground(Black);
	TextColor(White);
	GoToXY(1, 1);
	ClrScr();
	CursorOff();
	InitConsole();
	SetConsoleSize(WinWidth, 45);
	SetLength(irInBuf, 128);
	onKey := @Noop;
	onMouse := @Noop;
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
	Assign(anyFile, 'splash.txt');
	Reset(anyFile);
	i := 8;
	While Not EOF(anyFile) Do
	Begin
		Inc(i);
		ReadLn(anyFile, str);
		WriteCenter(str, i);
	End;
	Close(anyFile);
	While True Do
	Begin
		PollConsoleInput(irInBuf, 128, cNumRead);
		For i := 0 To 127 Do
		Case irInBuf[i].EventType Of
			KEY_EVENT: onKey(irInBuf[i].Event.KeyEvent);
			{ MOUSE_EVENT(0x0002) is declared both as a function and a constant }
			2: onMouse(irInBuf[i].Event.MouseEvent);
			WINDOW_BUFFER_SIZE_EVENT: onWinBufferSize(irInBuf[i].Event.WindowBufferSizeEvent);
			FOCUS_EVENT: onFocus(irInBuf[i].Event.FocusEvent);
			MENU_EVENT: onMenu(irInBuf[i].Event.MenuEvent);
		End;
	End;
End.