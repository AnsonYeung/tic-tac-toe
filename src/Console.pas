Unit Console;
Interface
Uses Windows, Math;

Procedure InitConsole();
Procedure SetConsoleSize(Const Width: Integer; Const Height: Integer);
Procedure PollConsoleInput(Var irInBuf: Array Of INPUT_RECORD; Const bufSize: DWord; Var cNumRead: DWord);
Procedure RestoreConsole();

Implementation
Var
hStdin: Handle;
hStdout: Handle;
fdwSaveOldMode: DWord;

Procedure InitConsole();
Var
fdwMode : DWord;
Begin
	hStdin := GetStdHandle(STD_INPUT_HANDLE);
	hStdout := GetStdHandle(STD_OUTPUT_HANDLE);
	GetConsoleMode(hStdin, @fdwSaveOldMode);
	fdwMode := ENABLE_WINDOW_INPUT or ENABLE_MOUSE_INPUT;
	SetConsoleMode(hStdin, fdwMode);
End;

Procedure SetConsoleSize(Const Width: Integer; Const Height: Integer);
Var
BufferSize: Coord;
ConsoleSize: SMALL_RECT;
CurrentInfo: CONSOLE_SCREEN_BUFFER_INFO;
Begin
	{ Set a buffer size bigger than the console size }
	GetConsoleScreenBufferInfo(hStdout, @CurrentInfo);
	BufferSize.X := Max(CurrentInfo.dwSize.X, Width);
	BufferSize.Y := Max(CurrentInfo.dwSize.Y, Height);
	SetConsoleScreenBufferSize(hStdout, BufferSize);
	{ Then safely set the console size }
	ConsoleSize.Top := 0;
	ConsoleSize.Left := 0;
	ConsoleSize.Right := Width - 1;
	ConsoleSize.Bottom := Height - 1;
	SetConsoleWindowInfo(hStdout, True, ConsoleSize);
	{ Set the buffer size to be equal to the console size }
	BufferSize.X := Width;
	BufferSize.Y := Height;
	SetConsoleScreenBufferSize(hStdout, BufferSize);
End;

Procedure PollConsoleInput(Var irInBuf: Array Of INPUT_RECORD; Const bufSize: DWord; Var cNumRead: DWord);
Begin
	ReadConsoleInput(hStdin, irInBuf, bufSize, @cNumRead);
End;

Procedure RestoreConsole();
Begin
	SetConsoleMode(hStdin, fdwSaveOldMode);
End;

End.