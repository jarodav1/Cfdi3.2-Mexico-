         MAP
!                        MODULE('Windows API')
!                            _lcreat(*CSTRING,SIGNED),SIGNED,PASCAL,RAW
!                            _hwrite(SIGNED,*CSTRING,LONG),LONG,PASCAL,RAW
!                            _lclose(SIGNED),SIGNED,PASCAL
!                        END

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
                    END
Version             EQUATE('3.2')
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
BaseGroup               GROUP
lugExpedicion               STRING(100)
Moneda                      STRING(1)
metodoPago                  STRING(50)
CtadePago                   STRING(4)
tipoComprobante             STRING(1)
subTotal                    DECIMAL(12,2)
Descuento                   DECIMAL(12,2)
totalFactura                decimal(12,2)
Serie                       STRING(6)
Folio                       LONG
Fecha                       STRING(30)
formaPago                   STRING('Pago en una sola exhibicion')
Version                     STRING('3.2')
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
                        END
Impuestos               GROUP
RetencionIva                decimal(12,2)
RetencionISr                DECIMAL(12,2)
TrasladoIva                 decimal(12,2)
PorIva                      LONG
                        end
Errorno                 BYTE
NoCertificado           STRING(40)
LastError               STRING(120)


!--------------------------------------------------------------------------------------
!Metodos
!-------------------------------------------------------------------------------------
Construct               PROCEDURE()
Destruct                PROCEDURE()
Init                    PROCEDURE(STRING pPath)
!getQueue                PROCEDURE()
creaXml                 PROCEDURE(),BYTE
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
TimbrarXml              PROCEDURE(STRING pUsuario,STRING pPassword),BYTE
CancelarXml             PROCEDURE(STRING pUUID,STRING pUsuario,STRING pPassword,STRING pRFC),BYTE
LoadXml                 PROCEDURE(),STRING
SaveXml                 PROCEDURE(STRING pRespuesta),BYTE
SaveXmlCancelado        PROCEDURE(STRING pRespuesta),BYTE
SetFecha                PROCEDURE(DATE pFecha,TIME pHora),STRING
ValidaCampos            PROCEDURE(),BYTE
Remplazar               PROCEDURE(STRING pTekst,STRING pToken,STRING pReplace),STRING
LoadXmlQueue            PROCEDURE(STRING pRespuesta),BYTE
GetAtributoXml          PROCEDURE(STRING pElemento, STRING pAtributo),STRING


                    END




