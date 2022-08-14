program ComboAutoComplete;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Edit.Suggest2 in '..\code\FMX.Edit.Suggest2.pas',
  UFormAutoComplete in '..\code\UFormAutoComplete.pas' {FormAutoComplete};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormAutoComplete, FormAutoComplete);
  Application.Run;

end.
