unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  SynEdit, SynEditTypes, LCLType, Process, LCLIntf, StrUtils,
  Clipbrd, X, XLib, XUtil; // Added clipboard and X11 libraries

type
  { TForm1 }
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SynEdit1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SynEdit1MouseLeave(Sender: TObject);
    procedure SynEdit1Click(Sender: TObject);
    procedure SynEdit1Paint(Sender: TObject);
  private
    FCommandLine: Integer;
    FCommandStart: Integer;
    FCommandEnd: Integer;
    function GetCommandUnderMouse(X, Y: Integer): string;
    procedure ExecuteUnixCommand(const Cmd: string);
    procedure ClearCommandPosition;
    function GetX11Selection: string; // New function to get X11 selection
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  HomeDir, FilePath: string;
  Lines: TStringList;
begin
  // Hide the gutter (line numbers)
  SynEdit1.Gutter.Visible := False;

  // Load commands from ~/heliko.txt
  HomeDir := GetEnvironmentVariable('HOME');
  FilePath := IncludeTrailingPathDelimiter(HomeDir) + 'heliko.txt';
  if FileExists(FilePath) then
  begin
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(FilePath);
      SynEdit1.Text := Lines.Text;
    finally
      Lines.Free;
    end;
  end
  else
    SynEdit1.Text := 'Example Unix commands: ;ls -la; or ;uname -a; or ;ls -al ^;';

    ClearCommandPosition;  // Set up our custom paint handler
  //SynEdit1.OnPaint := @SynEdit1Paint;
  SynEdit1.Font.Size := 24;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  HomeDir, FilePath: string;
  Lines: TStringList;
begin
  // Save commands to ~/heliko.txt
  HomeDir := GetEnvironmentVariable('HOME');
  FilePath := IncludeTrailingPathDelimiter(HomeDir) + 'heliko.txt';
  Lines := TStringList.Create;
  try
    Lines.Text := SynEdit1.Text;
    Lines.SaveToFile(FilePath);
  finally
    Lines.Free;
  end;
end;

procedure TForm1.ClearCommandPosition;
begin
  FCommandLine := -1;
  FCommandStart := -1;
  FCommandEnd := -1;
end;

function TForm1.GetCommandUnderMouse(X, Y: Integer): string;
var
  Pt: TPoint;
  LineText: string;
  LineIndex: Integer;
  CharPos: Integer;
begin
  Result := '';

  // Get character position from mouse coordinates
  Pt := SynEdit1.PixelsToRowColumn(Point(X, Y));
  LineIndex := Pt.Y - 1;
  CharPos := Pt.X;

  // Validate position
  if (LineIndex < 0) or (LineIndex >= SynEdit1.Lines.Count) then
  begin
    ClearCommandPosition;
    Exit;
  end;

  LineText := SynEdit1.Lines[LineIndex];

  // Validate character position
  if (CharPos < 1) or (CharPos > Length(LineText)) then
  begin
    ClearCommandPosition;
    Exit;
  end;

  // Find command boundaries
  FCommandStart := RPosEx(';', LineText, CharPos);
  if FCommandStart <= 0 then
  begin
    ClearCommandPosition;
    Exit;
  end;

  FCommandEnd := PosEx(';', LineText, FCommandStart + 1);
  if FCommandEnd <= FCommandStart then
  begin
    ClearCommandPosition;
    Exit;
  end;

  // Check if cursor is between semicolons
  if (CharPos >= FCommandStart) and (CharPos <= FCommandEnd) then
  begin
    FCommandLine := LineIndex;
    Result := Trim(Copy(LineText, FCommandStart + 1, FCommandEnd - FCommandStart - 1));
  end
  else
    ClearCommandPosition;
end;

procedure TForm1.SynEdit1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Cmd: string;
  OldLine: Integer;
begin
  OldLine := FCommandLine;
  Cmd := GetCommandUnderMouse(X, Y);

  if Cmd <> '' then
    SynEdit1.Cursor := crHandPoint
  else
    SynEdit1.Cursor := crDefault;

  // Force repaint of the editor if command position changed
  if (OldLine <> FCommandLine) or ((OldLine >= 0) and (FCommandLine >= 0)) then
    SynEdit1.Invalidate;
end;

procedure TForm1.SynEdit1MouseLeave(Sender: TObject);
begin
  SynEdit1.Cursor := crDefault;
  if FCommandLine >= 0 then
  begin
    ClearCommandPosition;
    SynEdit1.Invalidate;
  end;
end;

function TForm1.GetX11Selection: string;
var
  Display: PDisplay;
  Window: TWindow;
  Event: TXEvent;
  ClipboardAtom, PrimaryAtom, TargetAtom, PropAtom: TAtom;
  ActualType: TAtom;
  ActualFormat: LongInt;
  NItems, BytesAfter: LongInt;
  Value: PChar;
  Status: Integer;
  StartTime: QWord;
  Timeout: Integer;
begin
  Result := '';
  Display := XOpenDisplay(nil);
  if Display = nil then Exit;

  try
    Window := XCreateSimpleWindow(Display, RootWindow(Display, DefaultScreen(Display)), 0, 0, 1, 1, 0, 0, 0);

    // Define Atoms
    ClipboardAtom := XInternAtom(Display, 'CLIPBOARD', False);
    PrimaryAtom := XInternAtom(Display, 'PRIMARY', False);
    TargetAtom := XInternAtom(Display, 'UTF8_STRING', False);
    if TargetAtom = 0 then
      TargetAtom := XInternAtom(Display, 'STRING', False); // Fallback
    PropAtom := XInternAtom(Display, 'PENGUIN', False);

    // 1. Check CLIPBOARD
    XConvertSelection(Display, ClipboardAtom, TargetAtom, PropAtom, Window, CurrentTime);
    XFlush(Display);

    // Wait for SelectionNotify event with timeout
    Timeout := 1000; // 1 second
    StartTime := GetTickCount64;
    while (GetTickCount64 - StartTime < Timeout) do
    begin
      if XPending(Display) > 0 then
      begin
        XNextEvent(Display, @Event);
        if (Event._type = SelectionNotify) and (Event.xselection.selection = ClipboardAtom) then
          Break;
      end
      else
        Sleep(50); // Avoid high CPU usage
    end;

    // Retrieve data after event
    Status := XGetWindowProperty(Display, Window, PropAtom, 0, 1024, False,
                                AnyPropertyType, @ActualType, @ActualFormat,
                                @NItems, @BytesAfter, @Value);

    // Debug: Uncomment to inspect values
    // Writeln('CLIPBOARD Status: ', Status, ' | NItems: ', NItems, ' | Type: ', XGetAtomName(Display, ActualType));

    // 2. Fallback to PRIMARY only if CLIPBOARD retrieval failed
    if (Status <> Success) or (Value = nil) or (NItems = 0) then
    begin
      // Explicitly reset variables for PRIMARY check
      Value := nil;
      NItems := 0;

      XConvertSelection(Display, PrimaryAtom, TargetAtom, PropAtom, Window, CurrentTime);
      XFlush(Display);
      StartTime := GetTickCount64;
      while (GetTickCount64 - StartTime < Timeout) do
      begin
        if XPending(Display) > 0 then
        begin
          XNextEvent(Display, @Event);
          if (Event._type = SelectionNotify) and (Event.xselection.selection = PrimaryAtom) then
            Break;
        end
        else
          Sleep(50);
      end;

      Status := XGetWindowProperty(Display, Window, PropAtom, 0, 1024, False,
                                  AnyPropertyType, @ActualType, @ActualFormat,
                                  @NItems, @BytesAfter, @Value);

      // Debug: Uncomment to inspect values
      // Writeln('PRIMARY Status: ', Status, ' | NItems: ', NItems, ' | Type: ', XGetAtomName(Display, ActualType));
    end;

    // Process result
//    if (Status = Success) and (Value <> nil) and (NItems > 0) then
  {  if (Value <> nil)  then
    begin
      //SetString(Result, Value, NItems);
      Result := Value;
      XFree(Value);
    end;
   }
       // Process result
    if (Status = Success) and (Value <> nil) then
    begin
      if NItems > 0 then
        SetString(Result, Value, NItems) // Use NItems if valid
      else
        Result := StrPas(Value); // Fallback: treat as null-terminated string
      XFree(Value);
    end;
    XDestroyWindow(Display, Window);
  finally
    XCloseDisplay(Display);
  end;
end;

procedure TForm1.SynEdit1Click(Sender: TObject);
var
  MousePos: TPoint;
  Cmd, Selection: string;
begin
  MousePos := SynEdit1.ScreenToClient(Mouse.CursorPos);
  Cmd := GetCommandUnderMouse(MousePos.X, MousePos.Y);

  if Cmd <> '' then
  begin
    if Pos('^', Cmd) > 0 then
    begin
      Selection := Trim(GetX11Selection);
      if Selection = '' then
      begin
        ShowMessage('No text selected or copied! Highlight text (PRIMARY) or copy to clipboard (CLIPBOARD) first.');
        Exit;
      end;
      // Enclose selection in single quotes to handle spaces
      Cmd := StringReplace(Cmd, '^', '''' + Selection + '''', [rfReplaceAll]);
    end;

    ExecuteUnixCommand(Cmd);
  end;
end;

procedure TForm1.SynEdit1Paint(Sender: TObject);
var
  LineRect: TRect;
  StartX, EndX: Integer;
  LineY: Integer;
  GutterWidth: Integer;
begin
  // Only draw if we have a valid command position
  if (FCommandLine < 0) or (FCommandStart <= 0) or (FCommandEnd <= FCommandStart) then
    Exit;

  // Is the line visible?
  if (FCommandLine < SynEdit1.TopLine-1) or
     (FCommandLine >= SynEdit1.TopLine + SynEdit1.LinesInWindow) then
    Exit;

  // Calculate gutter width
  GutterWidth := 0;
  if SynEdit1.Gutter.Visible then
    GutterWidth := SynEdit1.Gutter.Width;

  // Calculate line position
  LineY := (FCommandLine - SynEdit1.TopLine + 1) * SynEdit1.LineHeight;

  // Calculate start and end X positions
  StartX := GutterWidth + (FCommandStart - SynEdit1.LeftChar) * SynEdit1.CharWidth;
  EndX := GutterWidth + (FCommandEnd - SynEdit1.LeftChar) * SynEdit1.CharWidth;

  // Stay within visible area
  if StartX < GutterWidth then StartX := GutterWidth;
  if EndX > SynEdit1.ClientWidth then EndX := SynEdit1.ClientWidth;

  // Draw underline if valid position
  if StartX < EndX then
  begin
    SynEdit1.Canvas.Pen.Color := clBlue;
    SynEdit1.Canvas.Pen.Width := 1;
    SynEdit1.Canvas.MoveTo(StartX, LineY + SynEdit1.LineHeight - 2);
    SynEdit1.Canvas.LineTo(EndX, LineY + SynEdit1.LineHeight - 2);
  end;
end;

procedure TForm1.ExecuteUnixCommand(const Cmd: string);
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'xterm';
    Proc.Parameters.Add('-hold');
    Proc.Parameters.Add('-e');
    Proc.Parameters.Add('/bin/sh');
    Proc.Parameters.Add('-c');
    Proc.Parameters.Add(Cmd);
    Proc.Options := [poNoConsole, poDetached];
    Proc.Execute;
  finally
    Proc.Free;
  end;
end;

end.
