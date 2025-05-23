unit baseeditform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  SynEdit, SynEditTypes, LCLType, Process, LCLIntf, StrUtils,
  IniFiles, Clipbrd, xSelection;

type
  TBaseEditForm = class(TForm)
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SynEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SynEditMouseLeave(Sender: TObject);
    procedure SynEditPaint(Sender: TObject; ACanvas: TCanvas);
    procedure SynEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynEditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    FCommandLine: Integer;
    FCommandStart: Integer;
    FCommandEnd: Integer;
    FIsClickOnCommand: Boolean;
    function GetCommandUnderMouse(X, Y: Integer): string;
    procedure ExecuteCommand(const Cmd: string); virtual;
    procedure ClearCommandPosition;
    procedure RunCommandInTerminal(const Cmd: string);
    procedure RunCommandInWindow(const Cmd: string);
  private
    FSynEdit: TSynEdit;
    FLastSelectionUpdate: QWord;
    FSelectionDebounceDelay: Integer;
    FLastSelText: string;
    FSharedSelection: string;
    class procedure CheckSelectionTimerEvent(Sender: TObject);
    procedure HandleSynEditCopy(Sender: TObject; var AText: string;
  var AMode: TSynSelectionMode; ALogStartPos: TPoint;
  var AnAction: TSynCopyPasteAction);
    procedure HandleSynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
  public
    class var UseXTerm: Boolean;
    class var SharedSelection: string;
    property SynEdit: TSynEdit read FSynEdit;
  end;

implementation

uses
  Math, //for max
  DateUtils,
  ExtCtrls; //for ttimer

class procedure TBaseEditForm.CheckSelectionTimerEvent(Sender: TObject);
var
  Timer: TTimer;
  OutputForm: TBaseEditForm;
  CurrentSelText: string;
begin
  Timer := TTimer(Sender);
  OutputForm := TBaseEditForm(Timer.Tag);

  if (OutputForm <> nil) and (OutputForm.SynEdit <> nil) and OutputForm.SynEdit.SelAvail then
  begin
    CurrentSelText := OutputForm.SynEdit.SelText;

    // Always update on any selection (don't skip if same as last)
    OutputForm.FLastSelText := CurrentSelText;

    // Store in class-wide variable for sharing between instances
    TBaseEditForm.SharedSelection := CurrentSelText;

    // Update both clipboard mechanisms
    try
      Clipboard.AsText := CurrentSelText;
    except
      // Handle clipboard error silently
    end;

    SetX11Selection(CurrentSelText);

    // For debugging
    //OutputForm.Caption := 'Selected: ' + Copy(CurrentSelText, 1, 20) + '...';
  end;
end;

procedure TBaseEditForm.HandleSynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (scSelection in Changes) and SynEdit.SelAvail then
  begin
    // Always update selections immediately
    TBaseEditForm.SharedSelection := SynEdit.SelText;
    SetX11Selection(SynEdit.SelText);
    FLastSelectionUpdate := GetTickCount64;

    try
      Clipboard.AsText := SynEdit.SelText;
    except
      // Handle clipboard error silently
    end;
  end;
end;

procedure TBaseEditForm.HandleSynEditCopy(Sender: TObject; var AText: string;
  var AMode: TSynSelectionMode; ALogStartPos: TPoint;
  var AnAction: TSynCopyPasteAction);
begin
  if SynEdit.SelAvail then
  begin
    Clipboard.AsText := SynEdit.SelText;
    SetX11Selection(SynEdit.SelText);
  end;
end;

procedure TBaseEditForm.FormCreate(Sender: TObject);
var
  BaseFontSize: Integer;
  ScalingFactor: Double;
  ScreenHeight: Integer;
begin




  // Font sizing
  ScreenHeight := Screen.Height;
  BaseFontSize := Round(ScreenHeight / 60);
  ScalingFactor := Screen.PixelsPerInch / 96;
  FSynEdit.Font.Size := Max(12, Min(Round(BaseFontSize * ScalingFactor), 36));


end;

constructor TBaseEditForm.CreateNew(AOwner: TComponent; Num: Integer);
var
  BaseFontSize: Integer;
  ScalingFactor: Double;
  ScreenWidth, ScreenHeight: Integer;
begin
    inherited CreateNew(AOwner, Num);
  // Manually initialize components instead of relying on FormCreate
  FSynEdit := TSynEdit.Create(Self);
  FSynEdit.Parent := Self;
  FSynEdit.Align := alClient;
  FSynEdit.Gutter.Visible := False;
  FSelectionDebounceDelay := 200;
  FLastSelText := '';

  Width := 800;
  Height := 600;

  FormCreate(Self);

  FSynEdit.OnMouseMove := @SynEditMouseMove;
  FSynEdit.OnMouseLeave := @SynEditMouseLeave;
  FSynEdit.OnPaint := @SynEditPaint;
  FSynEdit.OnMouseDown := @SynEditMouseDown;
  FSynEdit.OnMouseUp := @SynEditMouseUp;

  OnMouseWheel := @FormMouseWheel;
  FSynEdit.OnMouseWheel := @FormMouseWheel;


  FSynEdit.OnCutCopy := @HandleSynEditCopy;  // Works for both Ctrl+C and right-click copy
  FSynEdit.OnStatusChange := @HandleSynEditStatusChange;  // Track selection changes

  InitializeX11Selection;
end;

procedure TBaseEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TBaseEditForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  NewSize: Integer;
begin
  if (Sender = FSynEdit) and (ssCtrl in Shift) then
  begin
    NewSize := SynEdit.Font.Size;

    // Adjust size based on scroll direction
    if WheelDelta > 0 then
      NewSize := NewSize + 2  // Scroll up = larger font
    else
      NewSize := Max(8, NewSize - 2); // Scroll down = smaller font (min 8px)

    SynEdit.Font.Size := NewSize;
    Handled := True; // Block default scroll behavior
  end;
end;

procedure TBaseEditForm.ClearCommandPosition;
begin
  FCommandLine := -1;
  FCommandStart := -1;
  FCommandEnd := -1;
end;

procedure TBaseEditForm.ExecuteCommand(const Cmd: string);
begin
  if UseXTerm then
    RunCommandInTerminal(Cmd)
  else
    RunCommandInWindow(Cmd);
    //RunCommandInWindow(Cmd + ' < /dev/null');
end;

procedure TBaseEditForm.RunCommandInTerminal(const Cmd: string);
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

procedure TBaseEditForm.RunCommandInWindow(const Cmd: string);
var
  OutputForm: TBaseEditForm;
  Proc: TProcess;
  BytesRead: LongInt;
  Buffer: array[0..4095] of Char;
  s: string;
  HasOutput: Boolean;
  ExitCode: Integer;
  StartTime: TDateTime;
  TimeoutSecs: Integer;
  ElapsedSecs: Integer;
  SelectionTimer: TTimer;
begin
  OutputForm := TBaseEditForm.CreateNew(Application);
  try
    OutputForm.Caption := 'Output: ' + Cmd;
    OutputForm.Show;
    OutputForm.SynEdit.Text := '> ' + Cmd + LineEnding + LineEnding;

    OutputForm.SynEdit.OnStatusChange := @OutputForm.HandleSynEditStatusChange;
    OutputForm.SynEdit.OnCutCopy := @OutputForm.HandleSynEditCopy;

    // Create timer for selection monitoring with faster interval
    SelectionTimer := TTimer.Create(OutputForm);
    SelectionTimer.Interval := 50; // Check every 50ms instead of 100ms
    SelectionTimer.Tag := PtrInt(OutputForm); // Store OutputForm reference in Tag
    SelectionTimer.OnTimer := @CheckSelectionTimerEvent;
    SelectionTimer.Enabled := True;

    // Make the output window selection immediately active for HandleSynEditStatusChange
    OutputForm.FSelectionDebounceDelay := 0; // No delay for selection updates

    // Rest of the procedure remains the same...
    Proc := TProcess.Create(nil);
    try
      Proc.Executable := '/bin/sh';
      Proc.Parameters.Add('-c');
      Proc.Parameters.Add(Cmd);

      // Redirect stdin but don't try to close it directly
      Proc.Options := [poUsePipes, poStderrToOutPut];
      Proc.Execute;

      HasOutput := False;
      StartTime := Now;
      TimeoutSecs := 10; // 10 second timeout

      // Non-blocking output read loop with timeout
      while True do
      begin
        // Check if process is still running
        if not Proc.Running then Break;

        // Check for timeout
        ElapsedSecs := SecondsBetween(Now, StartTime);
        if ElapsedSecs > TimeoutSecs then
        begin
          OutputForm.SynEdit.Text := OutputForm.SynEdit.Text +
            LineEnding + '⚠️ Command timed out after ' + IntToStr(TimeoutSecs) + ' seconds';
          Proc.Terminate(9); // Force kill with SIGKILL
          Break;
        end;

        // Check if there's data to read (non-blocking)
        BytesRead := Proc.Output.NumBytesAvailable;
        if BytesRead > 0 then
        begin
          FillChar(Buffer, SizeOf(Buffer), 0);
          BytesRead := Proc.Output.Read(Buffer, SizeOf(Buffer));
          SetString(s, Buffer, BytesRead);
          OutputForm.SynEdit.Text := OutputForm.SynEdit.Text + s;
          Application.ProcessMessages;
          HasOutput := True;
        end
        else
          Sleep(10); // Prevent CPU overload
      end;

      // Read remaining output after process exits
      repeat
        BytesRead := Proc.Output.NumBytesAvailable;
        if BytesRead > 0 then
        begin
          FillChar(Buffer, SizeOf(Buffer), 0);
          BytesRead := Proc.Output.Read(Buffer, SizeOf(Buffer));
          SetString(s, Buffer, BytesRead);
          OutputForm.SynEdit.Text := OutputForm.SynEdit.Text + s;
          Application.ProcessMessages;
          HasOutput := True;
        end;
      until (BytesRead <= 0) or (not Proc.Running);

      // Get exit status and provide feedback
      Proc.WaitOnExit;
      ExitCode := Proc.ExitCode;

      if not HasOutput then
      begin
        if ExitCode = 0 then
          OutputForm.SynEdit.Text := OutputForm.SynEdit.Text + '✅ Command executed successfully'
        else
          OutputForm.SynEdit.Text := OutputForm.SynEdit.Text + '❌ Command failed (exit code: ' + IntToStr(ExitCode) + ')';
      end;

    finally
      Proc.Free;
    end;
  except
    on E: Exception do
      OutputForm.SynEdit.Text := OutputForm.SynEdit.Text + LineEnding + '🚨 Error: ' + E.Message;
  end;
end;

function TBaseEditForm.GetCommandUnderMouse(X, Y: Integer): string;
var
  Pt: TPoint;
  LineText: string;
  LineIndex: Integer;
  CharPos: Integer;
begin
  Result := '';

  // Get character position from mouse coordinates
  Pt := SynEdit.PixelsToRowColumn(Point(X, Y));
  LineIndex := Pt.Y - 1;
  CharPos := Pt.X;

  // Validate position
  if (LineIndex < 0) or (LineIndex >= SynEdit.Lines.Count) then
  begin
    ClearCommandPosition;
    Exit;
  end;

  LineText := SynEdit.Lines[LineIndex];

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

procedure TBaseEditForm.SynEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Cmd: string;
  OldLine: Integer;
begin
  OldLine := FCommandLine;
  Cmd := GetCommandUnderMouse(X, Y);

  if Cmd <> '' then
    SynEdit.Cursor := crHandPoint
  else
    SynEdit.Cursor := crDefault;

  // Force repaint of the editor if command position changed
  if (OldLine <> FCommandLine) or ((OldLine >= 0) and (FCommandLine >= 0)) then
    SynEdit.Invalidate;
end;

procedure TBaseEditForm.SynEditMouseLeave(Sender: TObject);
begin
  SynEdit.Cursor := crDefault;
  if FCommandLine >= 0 then
  begin
    ClearCommandPosition;
    SynEdit.Invalidate;
  end;
end;

procedure TBaseEditForm.SynEditPaint(Sender: TObject; ACanvas: TCanvas);
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
  if (FCommandLine < SynEdit.TopLine-1) or
     (FCommandLine >= SynEdit.TopLine + SynEdit.LinesInWindow) then
    Exit;

  // Calculate gutter width
  GutterWidth := 0;
  if SynEdit.Gutter.Visible then
    GutterWidth := SynEdit.Gutter.Width;

  // Calculate line position
  LineY := (FCommandLine - SynEdit.TopLine + 1) * SynEdit.LineHeight;

  // Calculate start and end X positions
  StartX := GutterWidth + (FCommandStart - SynEdit.LeftChar) * SynEdit.CharWidth;
  EndX := GutterWidth + (FCommandEnd - SynEdit.LeftChar) * SynEdit.CharWidth;

  // Stay within visible area
  if StartX < GutterWidth then StartX := GutterWidth;
  if EndX > SynEdit.ClientWidth then EndX := SynEdit.ClientWidth;

  // Draw underline if valid position
  if StartX < EndX then
  begin
  ACanvas.Pen.Color := clBlue;
  ACanvas.Pen.Width := 1;
  ACanvas.MoveTo(StartX, LineY + SynEdit.LineHeight - 2);
  ACanvas.LineTo(EndX, LineY + SynEdit.LineHeight - 2);
  end;
end;

procedure TBaseEditForm.SynEditMouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TBaseEditForm.SynEditMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MousePos: TPoint;
  Cmd, Selection: string;
begin
  if (Button = mbRight) and FIsClickOnCommand then
  begin
    FIsClickOnCommand := False;
    MousePos := SynEdit.ScreenToClient(Mouse.CursorPos);
    Cmd := GetCommandUnderMouse(MousePos.X, MousePos.Y);

    if Cmd <> '' then
    begin
      if Pos('^', Cmd) > 0 then
      begin
        // Use SynEdit's selection if available, else X11
       { if SynEdit.SelAvail then
          Selection := Trim(SynEdit.SelText)
        else
          Selection := Trim(GetX11Selection);
        }


         // Try multiple sources for selection in this order:
        // 1. Current SynEdit selection
        // 2. Our shared class variable (populated by timer)
        // 3. System clipboard
        // 4. X11 selection

        if SynEdit.SelAvail then
          Selection := Trim(SynEdit.SelText)
        else if TBaseEditForm.SharedSelection <> '' then
          Selection := Trim(TBaseEditForm.SharedSelection)
        else
        begin
          // Try clipboard
          try
            Selection := Trim(Clipboard.AsText);
          except
            Selection := '';
          end;

          // If clipboard was empty, try X11 selection
          if Selection = '' then
            Selection := Trim(GetX11Selection);
        end;

        if Selection = '' then
        begin
          ShowMessage('No text selected!');
          Exit;
        end;


       // Cmd := StringReplace(Cmd, '^', '''' + Selection + '''', [rfReplaceAll]);
       Selection := StringReplace(Selection, '''', '''"''"''', [rfReplaceAll]);
       Cmd := StringReplace(Cmd, '^', '''' + Selection + '''', [rfReplaceAll]);
      end;

      ExecuteCommand(Cmd);
    end;
    Abort; // Prevent SynEdit from processing the right-click
  end;
end;

initialization
  TBaseEditForm.SharedSelection := '';

end.
