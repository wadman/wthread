program run;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cmem,
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, main
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TfrmRun, frmRun);
    Application.Run;
end.

