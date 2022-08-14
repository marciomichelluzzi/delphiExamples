program WorkFlow;

uses
  System.StartUpCopy,
  FMX.Forms,
  UEtapaWorkFlow in '..\code\UEtapaWorkFlow.pas',
  UFrameEtapaWorkFlow in '..\code\UFrameEtapaWorkFlow.pas' {FrameEtapaWorkFlow: TFrame},
  UFrameWorkFlow in '..\code\UFrameWorkFlow.pas' {FrameWorkFlow: TFrame},
  UFormWorkFlow in '..\code\UFormWorkFlow.pas' {FormWorkFlow};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormWorkFlow, FormWorkFlow);
  Application.Run;
end.
