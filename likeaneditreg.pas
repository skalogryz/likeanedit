unit likeaneditreg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, likeaneditctrl;

procedure Register;

implementation

procedure Register;
begin
  RegisterClasses([TCustomLikeAnEdit,TLikeAnEdit]);
  RegisterComponents('Standard',[TLikeAnEdit]);
end;

end.

