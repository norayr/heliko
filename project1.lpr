program heliko;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Forms, Interfaces,
  baseeditform in 'baseeditform.pas',
  unit1 in 'unit1.pas',
  xselection in 'xselection.pas';


{$R *.res}

begin
  RequireDerivedFormResource := False;
  //RequireDerivedFormResource:=True;
  Application.Title:='heliko';
  Application.Scaled:=True;
  Application.MainFormOnTaskbar:=True;

   if ParamCount > 0 then
  begin
    if ParamStr(1) = '--xterm' then
      TBaseEditForm.UseXTerm := True;
  end;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.





