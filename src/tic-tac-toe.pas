Program TicTacToe;
Uses SysUtils, Windows, MyCrt, Board, Menu;
Type
EKey = Procedure (Const event: KEY_EVENT_RECORD);
EMouse = Procedure (Const event: MOUSE_EVENT_RECORD);
EWinBufferSize = Procedure (Const event: WINDOW_BUFFER_SIZE_RECORD);
EFocus = Procedure (Const event: FOCUS_EVENT_RECORD);
EMenu = Procedure (Const event: MENU_EVENT_RECORD);

Var
irInBuf: Array[0..127] Of INPUT_RECORD;
cNumRead: DWord;
i: Integer;
onKey: EKey;
onMouse: EMouse;
onWinBufferSize: EWinBufferSize;
onFocus: EFocus;
onMenu: EMenu;
menuOptions: Array Of String;
configOptions: Array Of String;
Width, Height, RequiredRow: Integer;
gameBoard: TBoard;
boxSelected: Coord;
currentPlayer: Integer;
win, draw, lose: Integer;

Procedure Noop(Const event: KEY_EVENT_RECORD); Begin End;
Procedure Noop(Const event: MOUSE_EVENT_RECORD); Begin End;
Procedure Noop(Const event: WINDOW_BUFFER_SIZE_RECORD); Begin End;
Procedure Noop(Const event: FOCUS_EVENT_RECORD); Begin End;
Procedure Noop(Const event: MENU_EVENT_RECORD); Begin
{$IFNDEF FALLBACK}
	SetConsoleSize(WinWidth, 45);
	TextBackground(Black);
	TextColor(White);
	ClrScr();
	onKey := @Noop;
	onMouse := @Noop;
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
	Write('All settiings will be applied at runtime. Please don''t open the menu, closing the program');
	ReadLn();
	Halt(1);
{$ENDIF}
End;

Procedure WriteCenter(Const str : String; Const ln : Integer);
Begin
	GoToXY((WinWidth - Length(str)) Div 2, ln);
	Write(str);
End;

Procedure WriteFileCenter(Const filename: String);
Var
textFile: Text;
i: Integer;
str: String;
Begin
	If Not FileExists(filename) Then
	Begin
		ClrScr();
		Write('The game file ' + filename + ' is missing, exiting');
		ReadLn();
		Halt(1);
	End;
	Assign(textFile, filename);
	Reset(textFile);
	i := -1;
	While Not EOF(textFile) Do
	Begin
		Inc(i);
		ReadLn(textFile, str);
		WriteCenter(str, i);
	End;
	Close(textFile);
End;

Procedure LoadConfig(Const filename: String);
Var
c: Char;
cfgFile: Text;
Begin
	If Not FileExists('config\' + filename) Then
	Begin
		SetConsoleColor(Black * 16 + White);
		ClrScr();
		Write('Config file ' + filename + ' does not exists, exiting');
		ReadLn();
		Halt(1);
	End;
	Assign(cfgFile, 'config\' + filename);
	Reset(cfgFile);
	Read(cfgFile, c);
	Width := Ord(c);
	Read(cfgFile, c);
	Height := Ord(c);
	Read(cfgFile, c);
	RequiredRow := Ord(c);
	Close(cfgFile);
	Assign(cfgFile, '.tic-tac-toe');
	Rewrite(cfgFile);
	WriteLn(cfgFile, Chr(win Div 256) + Chr(win Mod 256) + Chr(draw Div 256) + Chr(draw Mod 256) + Chr(lose Div 256) + Chr(lose Mod 256));
	Write(cfgFile, filename);
	Close(cfgFile);
End;

Procedure CreateConfig(Const filename: String; Const Width: Integer; Const Height: Integer; Const RequiredRow: Integer);
Var
cfgFile: Text;
Begin
	Assign(cfgFile, 'config\' + filename);
	Rewrite(cfgFile);
	Write(cfgFile, Chr(Width) + Chr(Height) + Chr(RequiredRow));
	Close(cfgFile);
End;

Procedure ResetGame();
Var
textFile : Text;
Begin
	Width := 3;
	Height := 3;
	RequiredRow := 3;
	win := 0;
	draw := 0;
	lose := 0;
	Assign(textFile, '.tic-tac-toe');
	Rewrite(textFile);
	WriteLn(textFile, StrDup(Chr(0), 6));
	Write(textFile, 'tic-tac-toe');
	Close(textFile);
	If Not DirectoryExists('config') Then
		CreateDir('config');
	CreateConfig('tic-tac-toe', 3, 3, 3);
	CreateConfig('gomoku', 19, 19, 5);
End;

Procedure InitProgram();
Var
c1, c2: Char;
filename: String;
textFile : Text;
Begin
	SetLength(menuOptions, 4);
	menuOptions[0] := 'Play';
	menuOptions[1] := 'Options';
	menuOptions[2] := 'Statistic';
	menuOptions[3] := 'Exit';
	SetLength(configOptions, 4);
	configOptions[0] := 'Load config';
	configOptions[1] := 'Create config';
	configOptions[2] := 'Remove game data and reset all config';
	configOptions[3] := 'Return to main menu';
	If Not FileExists('.tic-tac-toe') Then
		ResetGame()
	Else
	Begin
		Assign(textFile, '.tic-tac-toe');
		Reset(textFile);
		Read(textFile, c1, c2);
		win := Ord(c1) * 256 + Ord(c2);
		Read(textFile, c1, c2);
		draw := Ord(c1) * 256 + Ord(c2);
		ReadLn(textFile, c1, c2);
		lose := Ord(c1) * 256 + Ord(c2);
		ReadLn(textFile, filename);
		Close(textFile);
		LoadConfig(filename);
	End;
End;

Procedure EnterMainMenu(); Forward;
Procedure EnterGame(); Forward;
Procedure EnterGameEnd(Const player: Integer); Forward;
Procedure EnterConfig(); Forward;
Procedure EnterStat(); Forward;

Procedure MenuSelection(Const result: Integer);
Begin
	Case result Of
		0: EnterGame();
		1: EnterConfig();
		2: EnterStat();
		3: Halt(0);
	End;
End;

Procedure EnterMainMenu();
Begin
	SetConsoleSize(WinWidth, 45);
	TextBackground(Black);
	TextColor(White);
	ClrScr();
	WriteFileCenter('scenes\menu_title.txt');
	EnterMenu(onKey, onMouse, @MenuSelection, menuOptions);
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
End;

Procedure DrawBoxSelected(Const hover: Boolean);
Begin
	DrawBoxBackground(boxSelected.X, boxSelected.Y, gameBoard[boxSelected.X][boxSelected.Y], hover);
End;

Procedure GameChangeSelection(Const X: Integer; Const Y: Integer);
Begin
	If (boxSelected.X <> X) Or (boxSelected.Y <> Y) Then
	Begin
		DrawBoxSelected(False);
		boxSelected.X := X;
		boxSelected.Y := Y;
		DrawBoxSelected(True);
	End;
End;

Function CheckEnd(Const gameBoard: TBoard; Const boxSelected: Coord; Const currentPlayer: Integer): Boolean;
Var
i, j, num: Integer;
tmpSelected: Coord;
cnts: Array[1..4] Of Integer;
result: Boolean;
Begin
	result := False;
	gameBoard[boxSelected.X][boxSelected.Y] := currentPlayer;
	For i := 1 To 4 Do
		cnts[i] := -1;
	{ Method: Count in 8 directions from boxSelected }
	For i := -1 To 1 Do
		For j := -1 To 1 Do
		Begin
			If (i <> 0) Or (j <> 0) Then
			Begin
				tmpSelected := boxSelected;
				num := Abs(i * 3 + j);
				While gameBoard[tmpSelected.X][tmpSelected.Y] = currentPlayer Do
				Begin
					tmpSelected.X := tmpSelected.X + i;
					tmpSelected.Y := tmpSelected.Y + j;
					Inc(cnts[num]);
					If Not (tmpSelected.X In [0..Width - 1]) Or Not (tmpSelected.Y In [0..Height - 1]) Then
						Break;
				End;
			End;
		End;
	For i := 1 To 4 Do
		If cnts[i] >= RequiredRow Then
		Begin
			If currentPlayer = -1 Then EnterGameEnd(currentPlayer)
			Else EnterGameEnd(currentPlayer);
			result := True;
		End;
	If Not result Then
	Begin
		result := True;
		For i := 0 To Width - 1 Do
			For j := 0 To Height - 1 Do
				If gameBoard[i][j] = 0 Then
					result := False;
		If result Then
		Begin
			EnterGameEnd(currentPlayer);
		End;
	End;
	CheckEnd := result;
End;

Procedure GameSelection();
Begin
	If gameBoard[boxSelected.X][boxSelected.Y] = 0 Then
	Begin
		DrawBoxContent(boxSelected.X, boxSelected.Y, currentPlayer);
		If Not CheckEnd(gameBoard, boxSelected, currentPlayer) Then
		Begin
			{ Redraw the selection to red }
			i := boxSelected.X;
			If boxSelected.X = 0 Then
				boxSelected.X := 1
			Else
				boxSelected.X := 0;
			GameChangeSelection(i, boxSelected.Y);
			currentPlayer := -currentPlayer;
		End;
	End;
End;

Procedure GameEvent(Const event: KEY_EVENT_RECORD);
Begin
	If event.bKeyDown Then
	Case event.wVirtualKeyCode Of
		VK_UP: GameChangeSelection(boxSelected.X, (boxSelected.Y - 1 + Height) Mod Height);
		VK_DOWN: GameChangeSelection(boxSelected.X, (boxSelected.Y + 1) Mod Height);
		VK_LEFT: GameChangeSelection((boxSelected.X - 1 + Width) Mod Width, boxSelected.Y);
		VK_RIGHT: GameChangeSelection((boxSelected.X + 1) Mod Width, boxSelected.Y);
	End
	Else
	Case event.wVirtualKeyCode Of
		VK_RETURN: GameSelection();
	End;
End;

Procedure GameEvent(Const event: MOUSE_EVENT_RECORD);
Var
i, j, hoverX, hoverY: Integer;
Begin
	{ Mouse released }
	If event.dwButtonState = 0 Then
	Begin
		hoverX := -1;
		hoverY := -1;
		For i := 0 To Width - 1 Do
			For j := 0 To Height - 1 Do
				If MouseInBox(event.dwMousePosition.X, event.dwMousePosition.Y, i, j) Then
				Begin
					hoverX := i;
					hoverY := j;
				End;
		If (hoverX <> -1) And (hoverY <> -1) Then
		Case event.dwEventFlags Of
			0: GameSelection();
			MOUSE_MOVED:
			If (boxSelected.X <> hoverX) Or (boxSelected.Y <> hoverY) Then
				GameChangeSelection(hoverX, hoverY);
		End;
	End;
End;

Procedure EnterGame();
Begin
	onKey := @GameEvent;
	onMouse := @GameEvent;
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
	gameBoard := InitBoard(Width, Height);
	boxSelected.X := 1;
	boxSelected.Y := 0;
	currentPlayer := 1;
	SetConsoleSize(Width * BoxWidth, Height * BoxHeight);
	ClrScr();
	DrawBoard(gameBoard);
	GameChangeSelection(0, 0);
End;

Procedure GameEndEvent(Const event: KEY_EVENT_RECORD);
Begin
	EnterMainMenu();
End;

Procedure GameEndEvent(Const event: MOUSE_EVENT_RECORD);
Begin
	If (event.dwButtonState = 0) And (event.dwEventFlags = 0) Then
		EnterMainMenu();
End;

Procedure EnterGameEnd(Const player: Integer);
Var
i: Integer;
result: Char;
configFile: Text;
s: String;
Begin
	If player = 0 Then
	Begin
		result := ' ';
		Inc(draw);
	End
	Else If player = -1 Then
	Begin
		result := 'X';
		Inc(lose);
	End
	Else
	Begin
		result := 'O';
		Inc(win);
	End;
	Assign(configFile, '.tic-tac-toe');
	Reset(configFile);
	ReadLn(configFile);
	ReadLn(configFile, s);
	Close(configFile);
	Rewrite(configFile);
	WriteLn(configFile, Chr(win Div 256) + Chr(win Mod 256) + Chr(draw Div 256) + Chr(draw Mod 256) + Chr(lose Div 256) + Chr(lose Mod 256));
	Write(configFile, s);
	Close(configFile);
	onKey := @GameEndEvent;
	onMouse := @GameEndEvent;
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
	TextBackground(Black);
	TextColor(White);
	SetConsoleSize(Width * BoxWidth + 30, Height * BoxHeight);
	For i := 0 To Height * BoxHeight - 1 Do
		WriteDupAttr(Width * BoxWidth, i, 30);
	GoToXY(Width * BoxWidth + 1, 0);
	If result = ' ' Then
	Begin
		Write('The game ends in a draw.');
		Str(win, s);
		GoToXY(Width * BoxWidth + 1, 1);
		Write('O wins: ' + s);
		Str(draw, s);
		GoToXY(Width * BoxWidth + 1, 2);
		Write('Draw: ' + s);
		Str(lose, s);
		GoToXY(Width * BoxWidth + 1, 3);
		Write('X wins: ' + s);
	End
	Else
	Begin
		Write('Congratulations!');
		GoToXY(Width * BoxWidth + 1, 1);
		Write('Player ' + result + ' wins!');
		Str(win, s);
		GoToXY(Width * BoxWidth + 1, 2);
		Write('O wins: ', s);
		Str(draw, s);
		GoToXY(Width * BoxWidth + 1, 3);
		Write('Draw: ', s);
		Str(lose, s);
		GoToXY(Width * BoxWidth + 1, 4);
		Write('X wins: ', s);
	End;
End;

Procedure ConfigSelection(Const result: Integer);
Var
filename: String;
Width: Integer;
Height: Integer;
RequiredRow: Integer;
c: Char;
Begin
	TextBackground(Black);
	TextColor(White);
	ClrScr();
	onKey := @Noop;
	onMouse := @Noop;
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
	Case result Of
		0: Begin
			CursorOn();
			GoToXY(0, 2);
			Write('Build-in config: tic-tac-toe, gomoku');
			GoToXY(0, 0);
			Write('Enter the name of config: ');
			Read(filename);
			If filename = '' Then
				Read(filename);
			If Not FileExists('config\' + filename) Then
			Begin
				GoToXY(0, 1);
				Write('The config file does not exist.');
				CursorOff();
				Sleep(2000);
				EnterMainMenu();
			End
			Else
			Begin
				LoadConfig(filename);
				ClrScr();
				Write('Loaded the config file successfully.');
				CursorOff();
				Sleep(2000);
				EnterMainMenu();
			End;
		End;
		1: Begin
			CursorOn();
			Write('Enter the name of config: ');
			Read(filename);
			If filename = '' Then
				Read(filename);
			GoToXY(0, 1);
			Write('Enter the width of the board: ');
			Read(Width);
			GoToXY(0, 2);
			Write('Enter the height of the board: ');
			Read(Height);
			GoToXY(0, 3);
			Write('Enter the required ply in a row to win: ');
			Read(RequiredRow);
			GoToXY(0, 4);
			WriteLn('Filename: ''' + filename + ''', Width: ', Width, ', Height: ', Height, ', Required ply in a row: ', RequiredRow);
			Write('Type ''Y'' to confirm write or ''N'' to cancel: ');
			Repeat
				Read(c);
			Until (UpCase(c) = 'Y') Or (UpCase(c) = 'N');
			GoToXY(0, 6);
			CursorOff();
			If UpCase(c) <> 'Y' Then
			Begin
				Write('Cancelled');
				Sleep(2000);
				EnterMainMenu();
			End
			Else
			Begin
				CreateConfig(filename, Width, Height, RequiredRow);
				LoadConfig(filename);
				Write('Created and loaded config successfully.');
				Sleep(2000);
				EnterMainMenu();
			End;
		End;
		2: Begin
			ResetGame();
			Write('Done!');
			Sleep(1000);
			EnterMainMenu();
		End;
		3: EnterMainMenu();
	End;
End;

Procedure EnterConfig();
Begin
	TextBackground(Black);
	TextColor(White);
	ClrScr();
	WriteFileCenter('scenes\menu_title.txt');
	EnterMenu(onKey, onMouse, @ConfigSelection, configOptions);
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
End;

Procedure EnterStat();
Var
statFile: Text;
s: String;
Begin
	onKey := @GameEndEvent;
	onMouse := @GameEndEvent;
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
	TextBackground(Black);
	TextColor(White);
	SetConsoleSize(50, 20);
	ClrScr();
	Assign(statFile, 'scenes\stat.txt');
	Reset(statFile);
	While Not EOF(statFile) Do
	Begin
		ReadLn(statFile, s);
		WriteLn(s);
	End;
	Close(statFile);
	GoToXY(13, 15);
	Write(win);
	GoToXY(35, 15);
	Write(lose);
	GoToXY(18, 1);
	Write('Total Game: ', win + draw + lose);
End;

Begin
	InitConsole();
	SetConsoleSize(WinWidth, 45);
	ClrScr();
	WriteFileCenter('scenes\splash.txt');
	{ InitProgram reads file and it takes time }
	InitProgram();
	Sleep(3000);
	EnterMainMenu();
	{ Infinite Event Loop }
	While True Do
	Begin
		PollConsoleInput(irInBuf, 128, cNumRead);
		For i := 0 To cNumRead - 1 Do
		Case irInBuf[i].EventType Of
			KEY_EVENT: onKey(irInBuf[i].Event.KeyEvent);
			2: onMouse(irInBuf[i].Event.MouseEvent);
			WINDOW_BUFFER_SIZE_EVENT: onWinBufferSize(irInBuf[i].Event.WindowBufferSizeEvent);
			FOCUS_EVENT: onFocus(irInBuf[i].Event.FocusEvent);
			MENU_EVENT: onMenu(irInBuf[i].Event.MenuEvent);
		End;
	End;
End.