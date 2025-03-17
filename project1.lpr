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

procedure ShowHelp;
begin
  WriteLn('heliko - Command execution tool');
  WriteLn('');
  WriteLn('Usage: heliko [OPTIONS] [FILE]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  --help    Show this help message and exit');
  WriteLn('  --xterm   Execute commands in xterm instead of internal window');
  WriteLn('');
  WriteLn('Arguments:');
  WriteLn('  FILE      Optional path to load commands from (default: ~/heliko.txt)');
  Halt(0);
end;

begin
  RequireDerivedFormResource := False;
  //RequireDerivedFormResource:=True;
  Application.Title:='heliko';
  Application.Scaled:=True;
  Application.MainFormOnTaskbar:=True;



  Application.Initialize;
  Application.CreateForm(TForm1, Form1);


  Application.Run;
end.





