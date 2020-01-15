{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit likeandedit_rt;

{$warn 5023 off : no warning about unused units}
interface

uses
  likeaneditctrl, likeaneditreg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('likeaneditreg', @likeaneditreg.Register);
end;

initialization
  RegisterPackage('likeandedit_rt', @Register);
end.
