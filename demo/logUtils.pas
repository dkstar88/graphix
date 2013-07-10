unit LogUtils;

interface

procedure Log(AFilename: String; ALine: String); overload;
procedure Log(ALine: String); overload;

implementation

uses SysUtils;

procedure Log(ALine: String); overload;
begin
  Log(ChangeFileExt(ParamStr(0), '.log'), Aline);
end;

procedure Log(AFilename: String; ALine: String);
var
  f: TextFile;
begin
{$IFNDEF DEBUG}
  if not FileExists(AFilename) then Exit;
{$ENDIF}
  AssignFile(F, AFilename);
{$I-}
  Append(F);
  if IOResult <> 0 then Rewrite(F);
  WriteLn(F, ALine);
{$I+}
  CloseFIle(F);
end;

end.

