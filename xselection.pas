unit xSelection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Clipbrd, X, XLib, XUtil, Types;

function GetX11Selection: string;
procedure SetX11Selection(const Text: string);
procedure InitializeX11Selection;

implementation
var
  PersistentDisplay: PDisplay = nil;
  PersistentWindow: TWindow = 0;

procedure InitializePersistentX11;
begin
  if PersistentDisplay = nil then
  begin
    PersistentDisplay := XOpenDisplay(nil);
    PersistentWindow := XCreateSimpleWindow(PersistentDisplay,
      RootWindow(PersistentDisplay, DefaultScreen(PersistentDisplay)),
      0, 0, 1, 1, 0, 0, 0);
  end;
end;


function GetX11Selection: string;
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
    Timeout := 2000; // 1 second
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
   { if (Status = Success) and (Value <> nil) then
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
  end;}
     if (Status = Success) and (Value <> nil) then
    begin
      if NItems > 0 then
        SetString(Result, Value, NItems)
      else
        Result := StrPas(Value);
      XFree(Value);
      Result := Trim(Result);
    end;

    // Fallback to system clipboard if X11 selection is empty
    if Result = '' then
    begin
      try
        Result := Clipboard.AsText;
      except
        on E: Exception do
          Result := ''; // Gracefully handle clipboard errors
      end;
    end;

    XDestroyWindow(Display, Window);

   finally
     XCloseDisplay(Display);
   end;
end;

procedure InitializeX11Selection;
var
  Display: PDisplay;
  Window: TWindow;
  PrimaryAtom: TAtom;
begin
  // Just initialize the X11 connection and set up a dummy selection
  Display := XOpenDisplay(nil);
  if Display = nil then Exit;

  try
    Window := XCreateSimpleWindow(Display, RootWindow(Display, DefaultScreen(Display)), 0, 0, 1, 1, 0, 0, 0);
    PrimaryAtom := XInternAtom(Display, 'PRIMARY', False);

    // Set empty selection just to register our window
    XSetSelectionOwner(Display, PrimaryAtom, Window, CurrentTime);
    XFlush(Display);

    // Short delay to ensure processing
    Sleep(50);

    XDestroyWindow(Display, Window);
  finally
    XCloseDisplay(Display);
  end;
end;

procedure SetX11Selection(const Text: string);
var
  ClipboardAtom, PrimaryAtom, TargetAtom: TAtom;
  Data: PChar;
  DataSize: Integer;
begin
  InitializePersistentX11;
  if PersistentDisplay = nil then Exit;
  if Text = '' then Exit;

  // Define Atoms
  ClipboardAtom := XInternAtom(PersistentDisplay, 'CLIPBOARD', False);
  PrimaryAtom := XInternAtom(PersistentDisplay, 'PRIMARY', False);
  TargetAtom := XInternAtom(PersistentDisplay, 'UTF8_STRING', False);
  if TargetAtom = 0 then
    TargetAtom := XInternAtom(PersistentDisplay, 'STRING', False);

  Clipboard.AsText := Text;
  // Prepare data
  DataSize := Length(Text);
  GetMem(Data, DataSize + 1);
  try
    StrPCopy(Data, Text);

    // Set selections
    XSetSelectionOwner(PersistentDisplay, PrimaryAtom, PersistentWindow, CurrentTime);
    XSetSelectionOwner(PersistentDisplay, ClipboardAtom, PersistentWindow, CurrentTime);

    // Notify clipboard managers (use PrimaryAtom here)
    XConvertSelection(PersistentDisplay, ClipboardAtom, TargetAtom, PrimaryAtom, PersistentWindow, CurrentTime);
    // Force XFlush
    XFlush(PersistentDisplay);

  finally
    FreeMem(Data);
  end;
end;

end.

