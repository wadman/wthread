{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Threading;

interface

uses
    wcthread, wlog, wthread, wcthreadreg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('wcthreadreg', @wcthreadreg.Register);
end;

initialization
  RegisterPackage('Threading', @Register);
end.
