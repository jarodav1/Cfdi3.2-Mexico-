                    Member()
!------------------------------------------------------------------------------------------
!Autor        : Victor David Montanez Chavez
!Fecha        : 12 de Diciembre de 2016
!Descripcion  : La siguiente clase fue concebida debido a la problematica con la que 
!me encontraba cada vez que tenia que realizar un sistema que necesitara timbrar con un pac
!la clase implementa las funciones de firma sat para validacion y sello de los xml 
!las clases de Iq- XMl para la creacion del archivo Xml
!la clase libcurl para consumir el web service de Pax Facturacion 
!y algunas api's para leer y crear archivos planos 
!pueden usarla, extenderla libremente siempre y cuando respente el derecho de autor
!-------------------------------------------------------------------------------------------

                    Map
                        INCLUDE('iQXML.INC','Modules')
                        INCLUDE('FIRMASAT.INC','Prototypes')
                    End						
    INCLUDE('TimbrarClass.inc'),once
    INCLUDE('keycodes.clw'),ONCE
    INCLUDE('iQXML.INC','Equates'),ONCE
    INCLUDE('FIRMASAT.INC','Equates'),ONCE
    INCLUDE('libcurl.INC'),ONCE
    INCLUDE('SystemString.inc'),ONCE

    

    
!----------------------------------------------------------
!
!----------------------------------------------------------
TimbrarClass.Construct      PROCEDURE
pVersion                        string(3)
    CODE
        Self.QConcepto   &= NEW ConceptosQueue
        !SELF.GbaseGroup  &= NEW BaseGroup
        pVersion =Version        
!----------------------------------------------------------
!
!----------------------------------------------------------
TimbrarClass.Destruct       PROCEDURE
    CODE
        FREE(SELF.QConcepto)
        DISPOSE(SELF.QConcepto)
!---------------------------------------------------------
!    
!---------------------------------------------------------
TimbrarClass.Init   PROCEDURE(STRING pPath)
    CODE
        SELF.PathXml = pPath
        !MESSAGE(BaseGroup.CtadePago)
        
!---------------------------------------------------------
!    
!---------------------------------------------------------
TimbrarClass.creaXml        PROCEDURE()!,BYTE
Totalret                        decimal(12,2)
lResponse                       BYTE
    code
        IF ~SELF.ValidaCampos() THEN RETURN TRUE END
        lResponse = XML:CreateXMLFILE(SELF.PathXml) !crea archivo xml
        IF  lResponse <> 0 
            SELF.Errorno = Error:CrearXml
            SELF.errorMsg()
            RETURN Level:Notify
        END
        XML:CreateParent('cfdi:Comprobante')
        XML:CreateAttribute('xsi:schemaLocation','http://www.sat.gob.mx/cfd/3 http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv32.xsd')
        XML:CreateAttribute('LugarExpedicion',CLIP(SELF.BaseGroup.lugExpedicion))
        IF SELF.BaseGroup.Moneda = 'D'
            XML:CreateAttribute('Moneda','USD')
        ELSE
            XML:CreateAttribute('Moneda','MXN')
        END
        XML:CreateAttribute('metodoDePago',CHOOSE(CLIP(SELF.BaseGroup.metodoPago)<>'',CLIP(SELF.BaseGroup.metodoPago),'NA'))
        IF CLIP(SELF.BaseGroup.CtadePago)<>''
            XML:CreateAttribute('NumCtaPago',CLIP(SELF.BaseGroup.CtadePago))
        END
        IF SELF.BaseGroup.tipoComprobante='I' OR CLIP(SELF.BaseGroup.tipoComprobante)=''
            XML:CreateAttribute('tipoDeComprobante','ingreso')
        ELSE
            XML:CreateAttribute('tipoDeComprobante','egreso')
        END
        XML:CreateAttribute('total',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.totalFactura,@N_12.2)))
        XML:CreateAttribute('subTotal',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.subTotal,@N_12.2)))
        IF CLIP(SELF.BaseGroup.Serie)<>'' THEN  XML:CreateAttribute('serie',SELF.BaseGroup.Serie) END
        XML:CreateAttribute('folio',SELF.BaseGroup.Folio)
        XML:CreateAttribute('fecha',CLIP(SELF.BaseGroup.Fecha))
        XML:CreateAttribute('formaDePago',SELF.BaseGroup.formaPago)
        XML:CreateAttribute('descuento',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.Descuento,@N_12.2)))
        XML:CreateAttribute('version',Version)
        XML:CreateAttribute('xmlns:xsi','http://www.w3.org/2001/XMLSchema-instance')
        XML:CreateAttribute('xmlns:cfdi','http://www.sat.gob.mx/cfd/3')
        !=======================================================================
        !obtenemos el no de certificado
        r# = SELF.gSAT_GetCertNumber(AStr,clip(SELF.PathCertificado))!pasamos el certificado guardo en datos del sistema
        IF r# <= 0
            r# = SELF.gSAT_LastError(AStr)
            self.Errorno=Error:Certificado
            SELF.errorMsg()
            RETURN Level:Notify
        ELSE
            XML:CreateAttribute('noCertificado',AStr) !NO DE CERTIFICADO
            SELF.NoCertificado = CLIP(AStr)
            !Certificado en estring 
            r# = SELF.gSAT_GetCertAsString(AStr,clip(SELF.PathCertificado))
            IF r# <= 0
                r# = SELF.gSAT_LastError(AStr)
                SELF.Errorno= Error:CertString
                SELF.errorMsg()
                RETURN Level:Notify
            END
        END
        XML:CreateAttribute('certificado','')
        XML:CreateAttribute('sello','')
        XML:AddParent()
        !---------------------------- EMISOR --------------------------------
        XML:CreateAttribute('nombre',SELF.Convierte_Utf8(SELF.Emisor.Nombre))
        XML:CreateAttribute('rfc',SELF.Convierte_Utf8(SELF.Emisor.Rfc))
        XML:CreateParent('cfdi:Emisor');XML:AddParent()
        XML:CreateParent('cfdi:DomicilioFiscal')!;XML:AddParent()
        XML:CreateAttribute('calle',SELF.Convierte_Utf8(SELF.Emisor.Calle))
        XML:CreateAttribute('codigoPostal',CLIP(SELF.Emisor.CodPos))
        XML:CreateAttribute('colonia',SELF.Convierte_Utf8(SELF.Emisor.Colonia))
        XML:CreateAttribute('estado',SELF.Convierte_Utf8(SELF.Emisor.Estado))
        XML:CreateAttribute('localidad',SELF.Convierte_Utf8(SELF.Emisor.Ciudad))
        XML:CreateAttribute('municipio',SELF.Convierte_Utf8(SELF.Emisor.Municipio))
        XML:CreateAttribute('noExterior',SELF.Emisor.NumExt)
        if CLIP(SELF.Emisor.NumInt)<>'' THEN XML:CreateAttribute('noInterior',SELF.Emisor.NumInt) END
        XML:CreateAttribute('pais',SELF.Convierte_Utf8(CLIP(SELF.Emisor.Pais)))
        XML:AddParent(TRUE)
            !expedido end
          !=========Version 3.2 ========================================
        XML:CreateParent('cfdi:RegimenFiscal')
        XML:CreateAttribute('Regimen',SELF.Convierte_Utf8(SELF.Emisor.Regimen))
        XML:AddParent(true)
          !=============================================================
        xml:CloseParent('cfdi:Emisor')
        !-----------------------------Fin Emisor------------------------------
        !---------------------------- RECEPTOR -------------------------------
        XML:CreateAttribute('nombre',SELF.Convierte_Utf8(SELF.Receptor.Nombre))
        XML:CreateAttribute('rfc',SELF.Convierte_Utf8(SELF.Receptor.Rfc))
        XML:CreateParent('cfdi:Receptor');XML:AddParent()
        XML:CreateParent('cfdi:Domicilio')
        IF CLIP(SELF.Receptor.Calle)<>''   THEN  XML:CreateAttribute('calle',SELF.Convierte_Utf8(SELF.Receptor.Calle))     END
        IF CLIP(SELF.Receptor.CodPos)<>''  THEN  XML:CreateAttribute('codigoPostal',CLIP(SELF.Receptor.CodPos))       END
        IF CLIP(SELF.Receptor.Colonia)<>'' THEN  XML:CreateAttribute('colonia',SELF.Convierte_Utf8(SELF.Receptor.Colonia)) END
        XML:CreateAttribute('estado',SELF.Convierte_Utf8(SELF.Receptor.Estado))
        XML:CreateAttribute('localidad',SELF.Convierte_Utf8(SELF.Receptor.Ciudad))
        XML:CreateAttribute('municipio',SELF.Convierte_Utf8(SELF.Receptor.Municipio))
        IF CLIP(SELF.Receptor.NumExt)<>'' THEN   XML:CreateAttribute('noExterior',SELF.Convierte_Utf8(SELF.Receptor.NumExt)) END
        IF CLIP(SELF.Receptor.NumInt)<>'' THEN   XML:CreateAttribute('noInterior',SELF.Convierte_Utf8(SELF.Receptor.NumInt)) END
        XML:CreateAttribute('pais',SELF.Convierte_Utf8(SELF.Receptor.Pais))
        XML:AddParent(TRUE)
        xml:CloseParent('cfdi:Receptor')
        !--------------------Fin Receptor
        XML:CreateParent('cfdi:Conceptos');XML:AddParent()
        LOOP I#=1 TO RECORDS(SELF.QConcepto)
            GET(SELF.QConcepto,I#)
            IF ~ERRORCODE()
                XML:CreateParent('cfdi:Concepto')
                XML:CreateAttribute('cantidad',SELF.QConcepto.cantidad)
                XML:CreateAttribute('descripcion',SELF.Convierte_Utf8(clip(SELF.QConcepto.descripcion)))
                XML:CreateAttribute('unidad',CLIP(SELF.QConcepto.unidad))
                XML:CreateAttribute('importe',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.importe,@N_12.2)))
                XML:CreateAttribute('valorUnitario',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.valorUnitario,@N_12.2)))
                XML:AddParent(TRUE)
            END
        END
        xml:CloseParent('cfdi:Conceptos')
        !----------------------------------- IMPUESTOS --------------------------
        Totalret = SELF.Impuestos.RetencionIva+SELF.Impuestos.RetencionISr 
        XML:CreateParent('cfdi:Impuestos')
        IF Totalret>0 THEN  XML:CreateAttribute('totalImpuestosRetenidos',SELF.QuitaEspacios(FORMAT((Totalret),@N_12.2))) END
        XML:CreateAttribute('totalImpuestosTrasladados',SELF.QuitaEspacios(FORMAT(SELF.Impuestos.TrasladoIva,@N_12.2)))
        XML:AddParent()
        IF Totalret>0 THEN XML:CreateParent('cfdi:Retenciones');XML:AddParent() END
        IF SELF.Impuestos.RetencionIva>0
            XML:CreateParent('cfdi:Retencion')
            XML:CreateAttribute('impuesto','IVA')
            XML:CreateAttribute('importe',SELF.QuitaEspacios(FORMAT(SELF.Impuestos.RetencionIva,@N_12.2)))
            XML:AddParent(TRUE)
        END
        IF SELF.Impuestos.RetencionISr>0
            XML:CreateParent('cfdi:Retencion')
            XML:CreateAttribute('impuesto','ISR')
            XML:CreateAttribute('importe',SELF.QuitaEspacios(FORMAT(SELF.Impuestos.RetencionISr,@N_12.2)))
            XML:AddParent(TRUE)
        END
        IF Totalret  THEN XML:CloseParent('cfdi:Retenciones') END
        !traslados
        XML:CreateParent('cfdi:Traslados');XML:AddParent()
        XML:CreateParent('cfdi:Traslado')
        XML:CreateAttribute('impuesto','IVA')
        XML:CreateAttribute('tasa',SELF.QuitaEspacios(format(SELF.Impuestos.PorIva,@N_12.2)))
        XML:CreateAttribute('importe',SELF.QuitaEspacios(FORMAT(SELF.Impuestos.TrasladoIva,@N_12.2)))
        XML:AddParent(TRUE)
        !ieps   
        !XML:CreateParent('cfdi:Traslado')
        !XML:CreateAttribute('impuesto','IEPS')
        !XML:CreateAttribute('tasa',QuitaEspacios(CHOOSE(SQ:C1>0,format(SQ:C1,@N_12.2),format(0,@N_12.2))))
        !XML:CreateAttribute('importe',QuitaEspacios(FORMAT(SQ:C2,@N_12.2)))
        !XML:AddParent(TRUE)
        XML:CloseParent('cfdi:Traslados')
        XML:CloseParent('cfdi:Impuestos')
        
        XML:CloseParent('cfdi:Comprobante')
        XML:CloseXMLFile()
        
        RETURN lResponse
        
!---------------------------------------------------------
!    No se va a usar 
!---------------------------------------------------------
TimbrarClass.SetCertificado PROCEDURE(STRING pNocer)
    CODE
        XML:CreateAttribute('noCertificado',pNocer) 


!---------------------------------------------------------
!    Quita los espacios en un valor tipo decimal
!---------------------------------------------------------
TimbrarClass.QuitaEspacios  PROCEDURE(STRING pValor)!,STRING
Loc:Cantidad                    STRING(12)
    code
        LOOP I#=1 TO LEN(CLIP(pValor))
            IF SUB(pValor,I#,1)=''
                Loc:Cantidad=SUB(pValor,I#+1,LEN(CLIP(pValor)))
            END
        END
        Return(clip(Loc:Cantidad))
        
!---------------------------------------------------------
!    Obtiene el no de certificado
!---------------------------------------------------------
TimbrarClass.gSAT_GetCertNumber     PROCEDURE(*STRING PAR:Buffer,STRING PAR:XML)!,LONG   
NChars                                  LONG
NumStr                                  CSTRING(42)
AXML                                    CSTRING(2050)
    CODE
        PAR:Buffer = ''
        AXML  = CLIP(PAR:XML)
        NChars = SAT_GetCertNumber(NumStr, SIZE(NumStr) - 1, AXML, 0)
        IF NChars > 0
            PAR:Buffer = NumStr
        END
        RETURN(NChars)

!---------------------------------------------------------
!    Obtiene el  certificado
!---------------------------------------------------------
TimbrarClass.gSAT_GetCertAsString   PROCEDURE(*STRING PAR:Buffer,STRING PAR:XML)!,LONG
NChars                                  LONG
Buf                                     CSTRING(20000)
AXML                                    CSTRING(2050)
    CODE
        PAR:Buffer = ''
        Buf   = ''
        AXML  = CLIP(PAR:XML)
        NChars = SAT_GetCertAsString(Buf, 0, AXML, 0)
        IF NChars => 0
            Buf = ALL(' ',NChars + 1)
            NChars = SAT_GetCertAsString(Buf, NChars, AXML, 0)
            PAR:Buffer = Buf
        END
        RETURN(NChars)
        

                                            
!---------------------------------------------------------
!    Obtiene el ultimo Error 
!---------------------------------------------------------
TimbrarClass.gSAT_LastError PROCEDURE(*STRING PAR:Msg)!,LONG
Mensaje                         CSTRING(2049)
NChars                          LONG        
    CODE
        NChars = SAT_LastError(Mensaje,SIZE(Mensaje) - 1)
        IF NChars > 0
            PAR:Msg = Mensaje
        END
        RETURN(NChars)

        
!---------------------------------------------------------
!    Sella el Certificado 
!---------------------------------------------------------
TimbrarClass.gSAT_SignXml   PROCEDURE(STRING PAR:Buffer,STRING PAR:XML,STRING PAR:Key,STRING PAR:Password,STRING PAR:Cert)!,LONG         
NChars                          LONG
Buf                             CSTRING(20000)
AXML                            CSTRING(2050)
ALlave                          CSTRING(2050)
APass                           CSTRING(2050)
ACert                           CSTRING(2050)
    CODE
        Buf    = CLIP(PAR:Buffer)
        AXML   = CLIP(PAR:XML)
        ALlave = CLIP(PAR:Key)
        APass  = CLIP(PAR:Password)
        ACert  = CLIP(PAR:Cert)
        NChars = SAT_SignXml(Buf, AXML, ALlave, APass, ACert,SAT_HASH_SHA1)
        RETURN(NChars)

!---------------------------------------------------------
!    Valida XML 
!---------------------------------------------------------
TimbrarClass.gSAT_ValidateXml       PROCEDURE(STRING PAR:XML)!,LONG         
NChars                                  LONG
AXML                                    CSTRING(2050)
    CODE
        AXML  = CLIP(PAR:XML)
        NChars = SAT_ValidateXml(AXML, 0)
        RETURN(NChars)

!---------------------------------------------------------
!    Valida Firmado 
!---------------------------------------------------------
TimbrarClass.gSAT_VerifySignature   PROCEDURE(STRING PAR:XML)!,LONG         
NChars                                  LONG
AXML                                    CSTRING(2050)
Buf                                     CSTRING(20000)
    CODE
        AXML = CLIP(PAR:XML)
        Buf  = ''
        NChars = SAT_VerifySignature(AXML, Buf, 0)
        RETURN(NChars)

        
!---------------------------------------------------------
!    Funcion de FirmaSat para obtener un atributo de xml
!---------------------------------------------------------        
TimbrarClass.gSAT_GetXmlAttribute    PROCEDURE(*STRING PAR:Buffer,STRING PAR:XML,STRING PAR:Atrib,STRING PAR:Elem)!,LONG        
NChars                                      LONG
Buf                                         cSTRING(20000)
AXML                                        CSTRING(1025)
Atrib                                       CSTRING(1025)
Elem                                        CSTRING(1025)
    CODE
        PAR:Buffer = ''
        Buf = ''
        AXML  = CLIP(PAR:XML)
        Atrib = CLIP(PAR:Atrib)
        Elem  = CLIP(PAR:Elem)
        NChars = SAT_GetXmlAttribute(Buf, 0, AXML, Atrib, Elem)
        IF NChars => 0
            Buf = ALL(' ',NChars + 1)
            NChars = SAT_GetXmlAttribute(Buf, NChars, AXML, Atrib, Elem)
            PAR:Buffer = Buf
        END
        RETURN(NChars)

        
!---------------------------------------------------------
!    Muestra el Ultimo Error
!---------------------------------------------------------
TimbrarClass.errorMsg       PROCEDURE()        
    CODE
        CASE SELF.Errorno
        OF Error:Certificado
            MESSAGE('Error al obtener el no de Certificado '&CLIP(SELF.LastError))
        OF Error:CertString
            MESSAGE('Error al obtener el string del Certificado '&CLIP(SELF.LastError))
        OF Error:CrearXml
            MESSAGE('Error al Crear el Xml, Verifique el Path '&CLIP(SELF.LastError))
        OF Error:SellarXml
            MESSAGE('Error al Sellar el Xml, Intente de nuevo por favor '&CLIP(SELF.LastError))
        OF Error:WebService OROF Error:NoResponseW
            MESSAGE('Error Web Service '&clip(SELF.LastError))
        END
        
!---------------------------------------------------------
!    Convierte a UTF-8
!---------------------------------------------------------
TimbrarClass.Convierte_Utf8 PROCEDURE(STRING pTexto)!,STRING  
UTF8_Text                       CSTRING(4000)
    CODE
        UTF8_Text=''
        loop x# = 1 To Len(pTexto)
            wCHR# = VAL(SUB(pTexto,x#,1))
            If wCHR# < 128 Then
                UTF8_Text = UTF8_Text & chr(wCHR#)
            ElsIf ((wCHR# > 127) And (wCHR# < 2048)) Then
                UTF8_Text = UTF8_Text & chr( bor(int(wCHR# / 64),192))
                UTF8_Text = UTF8_Text & chr(Bor(band(wCHR#,63),128))
            Else
                UTF8_Text = UTF8_Text & chr(BOR(int(wCHR#/44),234))
                UTF8_Text = UTF8_Text & Chr(bor(band(int(wCHR#/64),63),128))
                UTF8_Text = UTF8_Text & Chr(bor(band(wCHR#,63),128))
            End
        end
        return UTF8_Text
!---------------------------------------------------------
!    Convierte a UTF-8
!---------------------------------------------------------
TimbrarClass.SellarXml      PROCEDURE()!,BYTE
lResult                         LONG
lOk                             BYTE
    CODE
        lOk = TRUE     
        SELF.LastError = ''
        IF EXISTS(SELF.PathXml) AND EXISTS(SELF.PathCertificado) AND EXISTS(SELF.PathKey)
            lResult = SELF.gSAT_SignXml(SELF.PathXmlSellado,SELF.PathXml,SELF.PathKey,CLIP(SELF.ClaveKey),SELF.PathCertificado)
            IF lResult = 0
            ! --- Validación --- !
                lResult = SELF.gSAT_ValidateXml(SELF.PathXmlSellado)
                IF lResult <> 0
                    lResult = SELF.gSAT_LastError(AStr)
                    SELF.Errorno = Error:SellarXml
                    SELF.LastError = ' Al  Validar el Xml errorno '&lResult
                    SELF.errorMsg()
                    lOk = FALSE
                ELSE
              ! --- Verificar firma --- !
                    lResult = SELF.gSAT_VerifySignature(SELF.PathXmlSellado)
                    IF lResult <> 0
                        lResult = SELF.gSAT_LastError(AStr)
                        SELF.Errorno = Error:SellarXml
                        SELF.LastError = ' Al  Verificar la Firma errorno '&lResult
                        SELF.errorMsg()
                        lOk = FALSE
                    END
                END
            ELSE
                MESSAGE('ubo errro ='&lResult)
                lResult = SELF.gSAT_LastError(AStr)
                SELF.LastError = clip(AStr)
                SELF.LastError = ' Al  Sellar el Xml errorno '&lResult&' '&AStr
                SELF.Errorno = Error:SellarXml
                SELF.errorMsg()
                lOk = FALSE
            END
        END   
        
        RETURN lOk
        
!---------------------------------------------------------
!    Timbrar el Xml 
!---------------------------------------------------------
TimbrarClass.TimbrarXml     PROCEDURE(STRING pUsuario,STRING pPassword)!,BYTE     
curl                            TCurlMailClass
res                             CURLcode,Auto
respBuffer                      CSTRING(32768) 
Respuesta1                      STRING(32768)  
Respuesta                       STRING(32768)  
envia                           STRING(32768)
Recs                            Long
lOk                             BYTE

    CODE
        lOk = FALSE
        curl.Init()
        envia ='<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:pax="https://www.paxfacturacion.com.mx:453">'&|
            '<soapenv:Header/>'&|
            '<soapenv:Body>'&|
            '<pax:fnEnviarXML>'&|
            '<pax:psComprobante><![CDATA['&CLIP(SELF.LoadXml())&']]></pax:psComprobante>'&|
            '<pax:psTipoDocumento>factura</pax:psTipoDocumento>'&|
            '<pax:pnId_Estructura>0</pax:pnId_Estructura>'&|
            '<pax:sNombre>'&clip(pUsuario)&'</pax:sNombre>'&|
            '<pax:sContraseña>'&clip(pPassword)&'</pax:sContraseña>'&|
            '<pax:sVersion>3.2</pax:sVersion>'&|
            '</pax:fnEnviarXML>'&|
            '</soapenv:Body>'&|
            '</soapenv:Envelope>'
        envia = SELF.Convierte_Utf8(envia)
        
        curl.AddHttpHeader('Content-Type: text/xml;charset=UTF-8')
        curl.AddHttpHeader('SOAPAction: "https://www.paxfacturacion.com.mx:453/fnEnviarXML"')
        curl.AddHttpHeader('Content-Length:'&len(clip(envia)))
        curl.AddHttpHeader('Host: www.paxfacturacion.com.mx:453')
        curl.SetHttpHeaders()  
        curl.SetSSLVerifyHost(false)  
        curl.SetSSLVerifyPeer(false)  
        res = curl.SendRequestStr('https://www.paxfacturacion.com.mx:453/webservices/wcfRecepcionasmx.asmx', clip(envia), respuesta1)
        Respuesta=respuesta1
        IF res = CURLE_OK
            IF LEN(CLIP(Respuesta))>100
                X#= SELF.SaveXml(Respuesta)
                lOk = TRUE
            END
        ELSIF res = -1 
            SELF.Errorno = res
            SELF.LastError ='El servicio regreso error'
            SELF.errorMsg()
        ELSE
            SELF.Errorno= error:NoResponseW
            SELF.LastError = 'No hay respuesta del web service '
            SELF.errorMsg()
        END
        curl.Cleanup()
        
        RETURN lok

!---------------------------------------------------------
!    Cancala el Xml 
!---------------------------------------------------------
TimbrarClass.CancelarXml             PROCEDURE(STRING pUUID,STRING pUsuario,STRING pPassword,STRING pRFC)!,BYTE
curl                            TCurlMailClass
res                             CURLcode,Auto
respBuffer                      CSTRING(32768) 
Respuesta1                      STRING(32768)  
Respuesta                       STRING(32768)  
envia                           STRING(32768)
Recs                            Long
lOk                             BYTE

    CODE
        !MESSAGE('Usuario='&pUsuario&' pass='&clip(pPassword)&' rfc='&clip(pRFC))
        lOk = FALSE
        curl.Init()
        envia ='<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:pax="https://www.paxfacturacion.com.mx:453">'&|
            '<soapenv:Header/>'&|
            '<soapenv:Body>'&|
            '<pax:fnCancelarXML>'&|
            ' <pax:sListaUUID>'&|
            '<pax:string>'&clip(pUUID)&'</pax:string>'&|
            '</pax:sListaUUID>'&|
            '<pax:psRFC>'&clip(pRFC)&'</pax:psRFC>'&|
            '<pax:pnId_Estructura>0</pax:pnId_Estructura>'&|
            '<pax:sNombre>'&clip(pUsuario)&'</pax:sNombre>'&|
            '<pax:sContraseña>'&clip(pPassword)&'</pax:sContraseña>'&|
            '</pax:fnCancelarXML>'&|
            '</soapenv:Body>'&|
            '</soapenv:Envelope>'
        envia = SELF.Convierte_Utf8(envia)
        curl.AddHttpHeader('Content-Type: text/xml;charset=UTF-8')
        curl.AddHttpHeader('SOAPAction: "https://www.paxfacturacion.com.mx:453/fnCancelarXML"')
        curl.AddHttpHeader('Content-Length:'&len(clip(envia)))
        curl.AddHttpHeader('Host: www.paxfacturacion.com.mx:453')
        curl.SetHttpHeaders()  
        curl.SetSSLVerifyHost(false)  
        curl.SetSSLVerifyPeer(false)  
        res = curl.SendRequestStr('https://www.paxfacturacion.com.mx:453/webservices/wcfCancelaasmx.asmx', clip(envia), respuesta1)
        Respuesta=respuesta1
        IF res = CURLE_OK
            IF LEN(CLIP(Respuesta))>50
                IF SELF.SaveXmlCancelado(Respuesta)
                    lOk = TRUE
                END
            END
        ELSIF res = -1 
            SELF.Errorno = res
            SELF.LastError ='El servicio regreso error'
            SELF.errorMsg()
        ELSE
            SELF.Errorno= error:NoResponseW
            SELF.LastError = 'No hay respuesta del web service '
            SELF.errorMsg()
        END
        curl.Cleanup()
        
        RETURN lok

        
        
!---------------------------------------------------------
!   Carga a un queue el Valor Obtenido de la cancelacion
!---------------------------------------------------------        
TimbrarClass.LoadXmlQueue   PROCEDURE(STRING pRespuesta)!,BYTE
Recs                            LONG
Respuesta                       CSTRING(32670)
QRetVal                                 QUEUE,pre(Qr)
UUID                                        CSTRING(30)
UUIDEstatus                                 CSTRING(4)
UUIDdescripcion                             CSTRING(50)
UUIDfecha                                   CSTRING(20)
                                        END
lResult                          BYTE


    CODE
        lResult = FALSE
        Respuesta = pRespuesta
        If NOT XML:LoadFromString(Respuesta)
            XML:GotoTop
            If NOT xml:FindNextNode('Folios')
                Recs = XML:LoadQueue(QRetVal,True,True,False,,False)
                XML:Free()
            End
            get(QRetVal,1)
            IF ~ERRORCODE() AND QRetVal.UUIDEstatus='201' 
                lResult = TRUE
            ELSE
                MESSAGE(QRetVal.UUIDdescripcion)
            END
        ELSE
            MESSAGE('ERROR AL CARGAR EL XML DE CANCELACION')
        END
        RETURN lResult
        
            
!---------------------------------------------------------
!    Carga el archivo xml Generado
!---------------------------------------------------------        
TimbrarClass.LoadXml        PROCEDURE()!,STRING    
Xml                             STRING(32768)
oStr                            SystemStringClass
    CODE
        IF ~oStr.FromFile(SELF.PathXmlSellado)
            Xml = oStr.Str()
        ELSE
            Xml = ''
            SELF.Errorno = Error:LoadXml
            SELF.errorMsg()
        END
        
        RETURN Xml
        
!---------------------------------------------------------
!    Guarda el archivo xml Timbrado
!---------------------------------------------------------        
TimbrarClass.SaveXml        PROCEDURE(STRING pRespuesta)!,BYTE      
lOk                             BYTE
oStr                            SystemStringClass
Resultado                       STRING(32768)
Quitar                          STRING(20000)
    CODE
        lOk = FALSE
        !Formateamos el String y quitamos lo que nos regresa el SOAP y convertimos a UTF-8 los <>
        Quitar = '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><soap:Body><fnEnviarXMLResponse xmlns="https://www.paxfacturacion.com.mx:453"><fnEnviarXMLResult>'        
        Resultado = SELF.Remplazar(pRespuesta,Quitar,'')
        Quitar = '</fnEnviarXMLResult></fnEnviarXMLResponse></soap:Body></soap:Envelope>'
        Resultado =  SELF.Remplazar(clip(Resultado),clip(Quitar),'')
        Quitar = '&gt;'
        Resultado = SELF.Remplazar(clip(Resultado),clip(Quitar),'>')
        Quitar = '&lt;'
        Resultado = SELF.Remplazar(clip(Resultado),clip(Quitar),'<')
        
        oStr.Str(Resultado)
        IF ~oStr.ToFile(SELF.PathXmlTimbrado)
            lOk = TRUE
        END
        RETURN lOk
!---------------------------------------------------------
!    Guarda el archivo xml Cancelado que regresa el pac
!---------------------------------------------------------        
TimbrarClass.SaveXmlCancelado       PROCEDURE(STRING pRespuesta)!,BYTE
lOk                                     BYTE
Resultado                       STRING(32768)
Quitar                          STRING(20000)
oStr                            SystemStringClass
    CODE
        lOk = FALSE
        Quitar = '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><soap:Body><fnCancelarXMLResponse xmlns="https://www.paxfacturacion.com.mx:453"><fnCancelarXMLResult>'
        Resultado = SELF.Remplazar(clip(pRespuesta),clip(Quitar),'')
        Quitar = '</fnCancelarXMLResult></fnCancelarXMLResponse></soap:Body></soap:Envelope>'
        Resultado = SELF.Remplazar(clip(Resultado),clip(Quitar),'')
        Quitar = '&gt;'
        Resultado = SELF.Remplazar(clip(Resultado),clip(Quitar),'>')
        Quitar = '&lt;'
        Resultado = SELF.Remplazar(clip(Resultado),clip(Quitar),'<')
        IF SELF.LoadXmlQueue(clip(Resultado))
            oStr.Str(clip(Resultado))
            IF ~oStr.ToFile(SELF.PathXmlCancelado)
                lOk = TRUE
            END
        END
        RETURN lOk
        
!---------------------------------------------------------
!    Formatea la fecha como la requiere el XMl 
!---------------------------------------------------------        
TimbrarClass.SetFecha                PROCEDURE(DATE pFecha,TIME pHora)!,STRING
FechaStr                                    STRING(13)        
HoraStr                                     STRING(10)
    CODE
        FechaStr = FORMAT(pFecha,@D10-)
        HoraStr  = FORMAT(pHora,@T04)
        RETURN CLIP(FechaStr)&'T'&CLIP(HoraStr)

        
!---------------------------------------------------------
!    Remplaza un substring en un string
!---------------------------------------------------------        
TimbrarClass.Remplazar       PROCEDURE(STRING pTekst,STRING pToken,STRING pReplace)!,STRING       
RetVal                          STRING(65520)
Pos                             LONG
Tl                              SHORT   !TokenLenght
    CODE
        !(STRING pTekst, STRING pToken, STRING pReplace), String
        RetVal = pTekst
        Tl     = LEN(CLIP(pToken))
        IF RetVal = '' OR Tl = 0
            RETURN RetVal
        END
      !
        Pos = INSTRING(pToken, RetVal, 1, 1)
        LOOP WHILE Pos
            RetVal = RetVal[1 : Pos-1] & CLIP(pReplace) & RetVal[Pos+Tl : 65520]
            Pos = INSTRING(pToken, RetVal, 1, 1)
        END
      !
        RETURN RetVal


        
!---------------------------------------------------------
!    Retorna un atributo de un Xml
!---------------------------------------------------------        
TimbrarClass.GetAtributoXml PROCEDURE(STRING pElemento, STRING pAtributo)!,STRING        
result                          LONG
    CODE
        result = SELF.gSAT_GetXmlAttribute(AStr,SELF.PathXmlTimbrado,pAtributo,pElemento)
        IF result<=0
            MESSAGE('Error al buscar el atributo ['&Clip(pAtributo)&'] en el XMl atrr '&clip(AStr))
            AStr = ''
        END
        RETURN CLIP(AStr)
        
        
!---------------------------------------------------------
!    Formatea la fecha como la requiere el XMl 
!---------------------------------------------------------        
TimbrarClass.ValidaCampos   PROCEDURE()!,BYTE
lReult                          BYTE
    CODE
        lReult = TRUE
        IF CLIP(SELF.PathXml)=''
            MESSAGE('Debe indicar Path del xml a generar')
            lReult = FALSE
        END
        IF CLIP(SELF.PathCertificado)=''
            MESSAGE('Debe indicar Path del Certificado')
            lReult = FALSE
        END
        IF CLIP(SELF.PathKey)=''
            MESSAGE('Debe indicar Path del archivo key')
            lReult = FALSE
        END
        IF CLIP(SELF.PathXmlSellado)=''
            MESSAGE('Debe indicar Path del xml sellado')
            lReult = FALSE
        END
        IF CLIP(SELF.PathXmlTimbrado)=''
            MESSAGE('Debe indicar Path del xml timbrado')
            lReult = FALSE
        END
        IF CLIP(SELF.BaseGroup.lugExpedicion) = ''
            MESSAGE('Debe indicar el lugar de expedicion')
            lReult = FALSE
        END
        IF CLIP(SELF.BaseGroup.metodoPago) = ''
            MESSAGE('Debe indicar el Metodo de Pago')
            lReult = FALSE
        END
        IF CLIP(SELF.BaseGroup.Fecha) = ''
            MESSAGE('Debe indicar la fecha del comrpobante')
            lReult = FALSE
        END
        IF CLIP(SELF.BaseGroup.Folio) = 0
            MESSAGE('Debe indicar el folio del comprobante')
            lReult = FALSE
        END
        IF CLIP(SELF.BaseGroup.Moneda) = ''
            MESSAGE('Debe indicar moneda del comprobante')
            lReult = FALSE
        END
        IF CLIP(SELF.BaseGroup.tipoComprobante) = ''
            MESSAGE('Debe indicar tipo de comprobante')
            lReult = FALSE
        END
        IF CLIP(SELF.BaseGroup.subTotal) = ''
            MESSAGE('Debe indicar subtotal del comprobante')
            lReult = FALSE
        END
        IF CLIP(SELF.BaseGroup.totalFactura) = ''
            MESSAGE('Debe indicar total de comprobante')
            lReult = FALSE
        END
        !Valida Emisor
        IF CLIP(SELF.Emisor.Nombre) = ''
            MESSAGE('Debe indicar el emisor')
            lReult = FALSE
        END
        IF CLIP(SELF.Emisor.Rfc) = ''
            MESSAGE('Debe indicar el rfc del emisor')
            lReult = FALSE
        END
        IF CLIP(SELF.Emisor.Calle) = ''
            MESSAGE('Debe indicar calle del emisor')
            lReult = FALSE
        END
        IF CLIP(SELF.Emisor.NumExt) = ''
            MESSAGE('Debe indicar numero exterior del emisor')
            lReult = FALSE
        END
        IF CLIP(SELF.Emisor.Colonia) = ''
            MESSAGE('Debe indicar colonia del emisor')
            lReult = FALSE
        END
        IF CLIP(SELF.Emisor.CodPos) = ''
            MESSAGE('Debe indicar codigo postal del emisor')
            lReult = FALSE
        END
        IF CLIP(SELF.Emisor.Ciudad) = ''
            MESSAGE('Debe indicar ciudad del emisor')
            lReult = FALSE
        END
        IF CLIP(SELF.Emisor.Municipio) = ''
            MESSAGE('Debe indicar municipio del emisor')
            lReult = FALSE
        END
        IF CLIP(SELF.Emisor.Estado) = ''
            MESSAGE('Debe indicar estado del emisor')
            lReult = FALSE
        END
        IF CLIP(SELF.Emisor.Pais) = ''
            MESSAGE('Debe indicar pais del emisor')
            lReult = FALSE
        END
        IF CLIP(SELF.Emisor.Regimen) = ''
            MESSAGE('Debe indicar regimen del emisor')
            lReult = FALSE
        END
        !validamos Receptos
        IF CLIP(SELF.Receptor.Nombre) = ''
            MESSAGE('Debe indicar el nombre del receptor')
            lReult = FALSE
        END
        IF CLIP(SELF.Receptor.Rfc) = ''
            MESSAGE('Debe indicar RFC del Receptor')
            lReult = FALSE
        END
        IF CLIP(SELF.Receptor.Ciudad) = ''
            MESSAGE('Debe indicar la ciudad del Receptor')
            lReult = FALSE
        END
        IF CLIP(SELF.Receptor.Municipio) = ''
            MESSAGE('Debe indicar el municipio del Receptor')
            lReult = FALSE
        END
        IF CLIP(SELF.Receptor.Estado) = ''
            MESSAGE('Debe indicar estado del Receptor')
            lReult = FALSE
        END
        IF CLIP(SELF.Receptor.Pais) = ''
            MESSAGE('Debe indicar el pais del Receptor')
            lReult = FALSE
        END
        RETURN lReult
        