unit UFormFloatingButton;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,
  FMX.Ani,
  UFrameFloatingButton,
  FMX.Edit, FMX.TabControl;

type
  TFormFloatingButton = class(TForm)
    FrameFloatingButton: TFrameFloatingButton;
    TabControlTabs: TTabControl;
    TabItemDashboard: TTabItem;
    TabItemSettings: TTabItem;
    RectangleDashboardBackground: TRectangle;
    RectangleSettingsBackground: TRectangle;
    RectangleDashboardItem: TRectangle;
    RectangleSettingItem: TRectangle;
    EditSettings: TEdit;
    EditDashboard: TEdit;
    procedure FrameFloatingButtonImageButtonOneClick(Sender: TObject);
    procedure FrameFloatingButtonImageButtonTwoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormFloatingButton: TFormFloatingButton;

implementation

{$R *.fmx}

procedure TFormFloatingButton.FrameFloatingButtonImageButtonOneClick(Sender: TObject);
begin
  TabControlTabs.ActiveTab := TabItemDashboard;
  FrameFloatingButton.ImageButtonHideClick(Sender);
end;

procedure TFormFloatingButton.FrameFloatingButtonImageButtonTwoClick(Sender: TObject);
begin
  TabControlTabs.ActiveTab := TabItemSettings;
  FrameFloatingButton.ImageButtonHideClick(Sender);
end;

end.
