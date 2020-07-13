                    MAP

                        MODULE('') !Make sure to include WIN32EXT library in your project.
                            FastQRCodeA(*CSTRING,*CSTRING),LONG,PASCAL,RAW,NAME('FastQRCode')
                        END

                        module('TimbrePac.lib')
                            AbreCajon(bstring),pascal,raw,dll(1),name('AbreCajon')
                            Enviar   (bstring,bstring,bstring,bstring,bstring,bstring,bstring,bstring,bstring,bstring),bstring,pascal,raw,dll(1),name('Enviar')
                        end !module('TimbrePac.lib')

                    END

!--------------------------------------------
! Definimos el Queue para Los conceptos de la factura
!--------------------------------------------
Error:Certificado   EQUATE(1)
Error:CertString    EQUATE(2)
Error:CrearXml      EQUATE(3)
Error:SellarXml     EQUATE(4)
Error:WebService    EQUATE(-1)
Error:NoResponseW   EQUATE(5)
Error:LoadXml       EQUATE(6)


!Variables Globales
ConceptosQueue      QUEUE,TYPE
descripcion             STRING(100)
cantidad                decimal(10,2)
unidad                  STRING(20)
valorUnitario           decimal(12,2)
importe                 decimal(12,2)
cveProdServ             string(10)
cveUnidad               string(4)
UUID                    STRING(50)
Moneda                  STRING(3)
TipoCambio              decimal(12,2)
SaldoAnt                decimal(12,2)
SaldoNvo                decimal(12,2)
IvaConcepto             BYTE
PorIva                  LONG
Iva                     DECIMAL(12,2)
                    END
ConceptoNomina      QUEUE,TYPE
TipoPercepcion          STRING(3)
Clave                   STRING(4)
Concepto                STRING(40)
ImporteGravado          DECIMAL(12,2)
ImporteExcento          DECIMAL(12,2)
					END


Version             EQUATE('3.3')
AStr                STRING(50000)
AStr2               STRING(50000)
!Atrib               CSTRING(50000)
!Elem                STRING(256)
sello               STRING(50000)
oleFeq              SIGNED
newFeq              SIGNED
IniPath             STRING(250)




TimbrarClass        class,module('TimbrarClass.clw'),link('TimbrarClass',1),Thread,Type
!Propierties 
Oobjeto                 LONG,PRIVATE
PathXml                 STRING(250)
PathXmlSellado          STRING(250)
PathXmlTimbrado         STRING(250)
PathCertificado         STRING(250)
PathXmlCancelado        STRING(250)
PathKey                 STRING(250)
ClaveKey                STRING(100)
QConcepto               &ConceptosQueue
QPercepciones           &ConceptoNomina
QDeducciones            &ConceptoNomina
BaseGroup               GROUP
lugExpedicion               STRING(100)
Moneda                      STRING(1)
metodoPago                  STRING(50)
CtadePago                   STRING(4)
tipoComprobante             STRING(1)
tipoCambio                  DECIMAL(12,2)
PueoPPD                     string(3)
subTotal                    DECIMAL(12,2)
Descuento                   DECIMAL(12,2)
totalFactura                decimal(12,2)
Serie                       STRING(6)
Folio                       LONG
Fecha                       STRING(30)
formaPago                   STRING('Pago en una sola exhibicion')
Version                     STRING('3.3')
                        END

Emisor                  Group
Rfc                         string(15)
Nombre                      string(100)
Regimen                     STRING(100)
Calle                       string(100)
NumExt                      string(20)
NumInt                      string(20)
Colonia                     string(40)
CodPos                      string(10)
Ciudad                      string(30)
Municipio                   string(30)
Estado                      string(30)
Pais                        string(20)
CveRegimen                  string(3)
Curp                        STRING(20)
                        END
Receptor                Group
Rfc                         string(15)
Nombre                      string(100)
Calle                       string(100)
NumExt                      string(20)
NumInt                      string(20)
Colonia                     string(40)
CodPos                      string(10)
Ciudad                      string(30)
Municipio                   string(30)
Estado                      string(30)
Pais                        string(20)
usoCfdi                     string(3)

                        END
Impuestos               GROUP
RetencionIva                decimal(12,2)
RetencionISr                DECIMAL(12,2)
TrasladoIva                 decimal(12,2)
PorIva                      LONG
                        end
Errorno                 BYTE
G_NoCertificado           STRING(40)
G_SelloSat              STRING(1000)
G_NoCertSat             STRING(100)
G_FecCertif             STRING(50)
G_UUID                  STRING(50)
G_SelloDig              STRING(1000)
G_Cadena                STRING(1000)
G_RFCPAC                STRING(20)
LastError               STRING(1000)

NotCred                      GROUP
TipoRelacion                string(2)
UUIDRelacionado             STRING(50)
								end

!---Para el timbrado de retenciones generams un grupo 
tRetenciones            GROUP
FolioInt                    string(20)
FechaExp                    DATE
HoraExp                     long
CveRetenc                   string(2)
DescRetenc                  string(100)
NomDenRazSocE               string(100)
RFCEmisor                   string(15)
nacionalidad                STRING(10)
NomDenRazSocR               string(100)
NumRegIdTrib                string(20)
MesIni                      LONG
MesFin                      LONG
Ejerc                       LONG
MontoOperacion              decimal(12,2)
Base                        decimal(12,2)
Impuesto                    string(2)
MontoRetencion              decimal(12,2)
MontoExcento                decimal(12,2)
MontoGravado                decimal(12,2)
TipodePago                  STRING(60)
                        end

REP                     GROUP
Total                       decimal(12,2)
Fecha                       DATE
FormaPago                   string(2)
MonedaP                     STRING(3)
TipoCambiop                 decimal(12,2)
rfcBanOrd                   string(20)
ctaOrdena                   string(20)
rfcBanRec                   string(20)
ctaRecept                   string(20)
ImporteFactoraje            decimal(12,2)
						end
MensajeCancela          STRING(100)
EsNomina                BYTE

Nomina                  GROUP
TipoNomina                  STRING(1)
FechaPago                   DATE
FechaInicial                DATE
FechaFinal                  DATE
DiasPago                    LONG
TotalPercepcion             DECIMAL(12,2)
TotalGravado                DECIMAL(12,2)
TotalExcento                DECIMAL(12,2)
TotalDeduccion              DECIMAL(12,2)
Ispt                        DECIMAL(12,2)
CredSal                     DECIMAL(12,2)
TotalOtrosPagos             DECIMAL(12,2)
RegistroPatronal            STRING(20)
ClaveEntFedera              STRING(3)
TipoJornada                 STRING('01')
Sdi                         DECIMAL(12,2)
RiesgoPuesto                STRING('5')
Antiguedad                  STRING(20)
FechaIngreso                DATE
Nss                         STRING(20)
TipoRegimen                 STRING('2')
TipoContrato                string('01')
Periodicidad                STRING('02')
Curp                        STRING(20)
Cuenta                      STRING(12)
NumEmp                      LONG
Puesto                      STRING(30)
Banco                       STRING(3)


						end


!Timbre                  GROUP
!nocert

!--------------------------------------------------------------------------------------
!Metodos
!-------------------------------------------------------------------------------------
Construct               PROCEDURE()
Destruct                PROCEDURE()
Init                    PROCEDURE(STRING pPath)
!getQueue                PROCEDURE()
creaXml                 PROCEDURE(),BYTE
creaXmlNomina           PROCEDURE(),BYTE
SetCertificado          PROCEDURE(STRING pNocer)
QuitaEspacios           PROCEDURE(STRING pValor),STRING
gSAT_GetCertNumber      PROCEDURE(*STRING PAR:Buffer,STRING PAR:XML),LONG
gSAT_LastError          PROCEDURE(*STRING PAR:Msg),LONG
errorMsg                PROCEDURE()
gSAT_GetCertAsString    PROCEDURE(*STRING PAR:Buffer,STRING PAR:XML),LONG
Convierte_Utf8          PROCEDURE(STRING pTexto),STRING
SellarXml               PROCEDURE(),BYTE
gSAT_SignXml            PROCEDURE(STRING PAR:Buffer,STRING PAR:XML,STRING PAR:Key,STRING PAR:Password,STRING PAR:Cert),LONG 
gSAT_ValidateXml        PROCEDURE(STRING PAR:XML),LONG 
gSAT_VerifySignature    PROCEDURE(STRING PAR:XML),LONG 
gSAT_GetXmlAttribute    PROCEDURE(*STRING PAR:Buffer,STRING PAR:XML,STRING PAR:Atrib,STRING PAR:Elem),LONG
!TimbrarXml              PROCEDURE(STRING pUsuario,STRING pPassword,STRING pTipoDoc),BYTE
TimbrarXml2              PROCEDURE(STRING pUsuario,STRING pPassword,STRING pTipoDoc),BYTE
TimbrarXmlR             PROCEDURE(STRING pUsuario,STRING pPassword,STRING pTipoDoc,STRING pTipoRet),BYTE 
!CancelarXml             PROCEDURE(STRING pUUID,STRING pUsuario,STRING pPassword,STRING pRFC,STRING pRfcCli,STRING pTotal),BYTE
CancelarXml2             PROCEDURE(STRING pUUID,STRING pUsuario,STRING pPassword,STRING pRFC,STRING pRfcCli,STRING pTotal),BYTE
LoadXml                 PROCEDURE(),STRING
SaveXml                 PROCEDURE(STRING pRespuesta),BYTE
SaveXmlCancelado        PROCEDURE(STRING pRespuesta),BYTE
SetFecha                PROCEDURE(DATE pFecha,TIME pHora),STRING
ValidaCampos            PROCEDURE(),BYTE
Remplazar               PROCEDURE(STRING pTekst,STRING pToken,STRING pReplace),STRING
LoadXmlQueue            PROCEDURE(STRING pRespuesta),BYTE
GetAtributoXml          PROCEDURE(STRING pElemento, STRING pAtributo),STRING
CreaQrCode              PROCEDURE(STRING pContenido,STRING pPath)
NumeroaLetra            PROCEDURE(STRING pValor,STRING pMoneda),STRING
BuscaAtributos          PROCEDURE()
ArmaQrCode              PROCEDURE(STRING pPath)
creaXmlRetencion        PROCEDURE(),BYTE
creaXmlPago       PROCEDURE(),BYTE


                    END




