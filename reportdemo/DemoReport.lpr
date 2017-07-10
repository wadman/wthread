program DemoReport;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, memdslaz, main
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TfrmMain, frmMain);
    Application.Run;
end.

