unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  SynEdit, SynEditTypes, LCLType, Process, LCLIntf, StrUtils,
  IniFiles;

type
  { TForm1 }
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SynEdit1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SynEdit1MouseLeave(Sender: TObject);
    procedure SynEdit1Click(Sender: TObject);
    procedure SynEdit1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SynEdit1Paint(Sender: TObject);
    // this is to prevent disappearing of synedit's own selection
    procedure SynEdit1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SynEdit1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FCommandLine: Integer;
    FCommandStart: Integer;
    FCommandEnd: Integer;
    FIsClickOnCommand: Boolean;
    function GetCommandUnderMouse(X, Y: Integer): string;
    procedure ExecuteUnixCommand(const Cmd: string);
    procedure ClearCommandPosition;
  public
  end;

var
  Form1: TForm1;

implementation
    uses Math, //for max
      xSelection;
{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  HomeDir, FilePath: string;
  Lines: TStringList;
  // to determine optimal font size
  ScreenWidth, ScreenHeight: Integer;
  BaseFontSize: Integer;
  ScalingFactor: Double;

  Ini: TIniFile;
  ConfigPath: string;
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

  //SynEdit1.Options2 := SynEdit1.Options2 + [eoPersistentBlock];
  Form1.Caption:='heliko';

  // SynEdit1.Font.Size := 24;
   // Get primary screen dimensions
  ScreenWidth := Screen.Width;
  ScreenHeight := Screen.Height;

  // Calculate base font size relative to screen height (adjust divisor as needed)
  BaseFontSize := Round(ScreenHeight / 60); // Example: 24px for 1440p screen

  // Adjust for DPI scaling
  ScalingFactor := Screen.PixelsPerInch / 96; // 96 = standard DPI
  BaseFontSize := Round(BaseFontSize * ScalingFactor);

  // Clamp values to reasonable bounds
  BaseFontSize := Max(12, Min(BaseFontSize, 36));

  SynEdit1.Font.Size := BaseFontSize;
    OnMouseWheel := @FormMouseWheel;
    SynEdit1.OnMouseWheel := @FormMouseWheel;


  ConfigPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.heliko.conf';
  Ini := TIniFile.Create(ConfigPath);
  try
    Width := Ini.ReadInteger('Window', 'Width', Round(ScreenWidth/2.3));
    Height := Ini.ReadInteger('Window', 'Height', Round(ScreenHeight-ScreenHeight/7));
  finally
    Ini.Free;
  end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  NewSize: Integer;
begin
  if (Sender = SynEdit1) and (ssCtrl in Shift) then
  begin
    NewSize := SynEdit1.Font.Size;

    // Adjust size based on scroll direction
    if WheelDelta > 0 then
      NewSize := NewSize + 2  // Scroll up = larger font
    else
      NewSize := Max(8, NewSize - 2); // Scroll down = smaller font (min 8px)

    SynEdit1.Font.Size := NewSize;
    Handled := True; // Block default scroll behavior
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  HomeDir, FilePath: string;
  Lines: TStringList;

  Ini: TIniFile;
  ConfigPath: string;
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

  ConfigPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.heliko.conf';
  Ini := TIniFile.Create(ConfigPath);
  try
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);
  finally
    Ini.Free;
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


procedure TForm1.SynEdit1Click(Sender: TObject);
var
  MousePos: TPoint;
  Cmd, Selection: string;
begin
  // Only process left-clicks
 // if not (ssLeft in Shift) then Exit;
 {
  MousePos := SynEdit1.ScreenToClient(Mouse.CursorPos);
  Cmd := GetCommandUnderMouse(MousePos.X, MousePos.Y);

  if Cmd <> '' then
  begin
    if Pos('^', Cmd) > 0 then
    begin
      // Check SynEdit's selection first
      if SynEdit1.SelAvail then
        Selection := Trim(SynEdit1.SelText)
      else
        Selection := Trim(GetX11Selection);

      if Selection = '' then
      begin
        ShowMessage('No text selected!');
        Exit;
      end;

      Cmd := StringReplace(Cmd, '^', '''' + Selection + '''', [rfReplaceAll]);
    end;

    ExecuteUnixCommand(Cmd);
  end;
  }
end;

procedure TForm1.SynEdit1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

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

procedure TForm1.SynEdit1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Cmd: string;
begin
  if Button = mbRight then  // Handle right-click only
  begin
    Cmd := GetCommandUnderMouse(X, Y);
    FIsClickOnCommand := (Cmd <> '');
    if FIsClickOnCommand then
      Abort; // Suppress default right-click behavior (e.g., context menu)
  end;
end;

procedure TForm1.SynEdit1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MousePos: TPoint;
  Cmd, Selection: string;
begin
  if (Button = mbRight) and FIsClickOnCommand then
  begin
    FIsClickOnCommand := False;
    MousePos := SynEdit1.ScreenToClient(Mouse.CursorPos);
    Cmd := GetCommandUnderMouse(MousePos.X, MousePos.Y);

    if Cmd <> '' then
    begin
      if Pos('^', Cmd) > 0 then
      begin
        // Use SynEdit's selection if available, else X11
        if SynEdit1.SelAvail then
          Selection := Trim(SynEdit1.SelText)
        else
          Selection := Trim(GetX11Selection);

        if Selection = '' then
        begin
          ShowMessage('No text selected!');
          Exit;
        end;

        Cmd := StringReplace(Cmd, '^', '''' + Selection + '''', [rfReplaceAll]);
      end;

      ExecuteUnixCommand(Cmd);
    end;
    Abort; // Prevent SynEdit from processing the right-click
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
