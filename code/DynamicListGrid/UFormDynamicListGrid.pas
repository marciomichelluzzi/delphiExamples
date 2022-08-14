unit UFormDynamicListGrid;

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
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Effects,
  FMX.Objects;

type
  TTipoLayout = (tlPortait, tlLandScape);

  TFormDynamicListGrid = class(TForm)
    RectangleToolBar: TRectangle;
    ShadowEffect1: TShadowEffect;
    btnVisualizacao: TButton;
    VertScrollBoxDynamic: TVertScrollBox;
    GridLayoutDynamic: TGridLayout;
    Rectangle2: TRectangle;
    ShadowEffect2: TShadowEffect;
    Rectangle3: TRectangle;
    ShadowEffect3: TShadowEffect;
    Rectangle4: TRectangle;
    ShadowEffect4: TShadowEffect;
    Rectangle5: TRectangle;
    ShadowEffect5: TShadowEffect;
    Rectangle6: TRectangle;
    ShadowEffect6: TShadowEffect;
    Rectangle7: TRectangle;
    ShadowEffect7: TShadowEffect;
    Rectangle8: TRectangle;
    ShadowEffect8: TShadowEffect;
    Rectangle9: TRectangle;
    ShadowEffect9: TShadowEffect;
    Rectangle10: TRectangle;
    ShadowEffect10: TShadowEffect;
    Rectangle11: TRectangle;
    ShadowEffect11: TShadowEffect;
    procedure btnVisualizacaoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    TipoLayout: TTipoLayout;
    procedure TamanhoItens;
    procedure LayoutPortait(PGridLayout: TGridLayout);
    procedure LayoutLandScape(PGridLayout: TGridLayout);
    procedure MudaLayout;
  public
    { Public declarations }
  end;

var
  FormDynamicListGrid: TFormDynamicListGrid;

implementation

{$R *.fmx}

procedure TFormDynamicListGrid.FormCreate(Sender: TObject);
begin
  TamanhoItens;
  MudaLayout;
end;

procedure TFormDynamicListGrid.TamanhoItens;
var
  iMargin, iItemHeight: Single;
  iControlsCount: Integer;
begin
  iControlsCount := GridLayoutDynamic.ControlsCount;
  if (Odd(iControlsCount)) then
    iControlsCount := iControlsCount + 1;
  iMargin := 10 * iControlsCount;
  iItemHeight := GridLayoutDynamic.ItemHeight - 10;
  GridLayoutDynamic.Height := (iItemHeight * (iControlsCount / 2) - iMargin);
  GridLayoutDynamic.ItemWidth := ClientWidth / 2;
  GridLayoutDynamic.ItemHeight := ClientHeight / 2 - 62;
end;

procedure TFormDynamicListGrid.LayoutPortait(PGridLayout: TGridLayout);
var
  LMargin, LControlsCount: Integer;
begin
  LControlsCount := PGridLayout.ControlsCount;
  if (PGridLayout <> NIL) then
  begin
    if (Odd(LControlsCount)) then
      LControlsCount := LControlsCount + 1;
    LMargin := 10 * LControlsCount;
    PGridLayout.AnimateFloat('ItemWidth', ClientWidth / 2, 0.4,
      TAnimationType.InOut, TInterpolationType.Cubic);
    PGridLayout.AnimateFloat('ItemHeight', ClientHeight / 2 - 62, 0.4,
      TAnimationType.InOut, TInterpolationType.Cubic);
    PGridLayout.Width := ClientWidth;
    TipoLayout := tlPortait;
    PGridLayout.Height := ((PGridLayout.ItemHeight * LControlsCount) / 2);
  end;
end;

procedure TFormDynamicListGrid.btnVisualizacaoClick(Sender: TObject);
begin
  MudaLayout;
end;

procedure TFormDynamicListGrid.LayoutLandScape(PGridLayout: TGridLayout);
begin
  if (PGridLayout <> NIL) then
  begin
    PGridLayout.AnimateFloat('ItemWidth', ClientWidth, 0.1,
      TAnimationType.InOut, TInterpolationType.Cubic);
    PGridLayout.AnimateFloat('ItemHeight', ClientHeight / 2 - 62, 0.1,
      TAnimationType.InOut, TInterpolationType.Cubic);
    PGridLayout.Height := (PGridLayout.ItemHeight * PGridLayout.ControlsCount);
    TipoLayout := tlLandScape;
  end;
end;

procedure TFormDynamicListGrid.MudaLayout;
begin
  if (not(TipoLayout = tlPortait)) then
  begin
    LayoutPortait(GridLayoutDynamic);
  end
  else
  begin
    LayoutLandScape(GridLayoutDynamic);
  end;
end;

end.
