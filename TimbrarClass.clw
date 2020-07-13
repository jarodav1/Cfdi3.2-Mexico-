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
		Self.QConcepto     &= NEW ConceptosQueue
		self.QPercepciones &=new ConceptoNomina
		self.QDeducciones  &=new ConceptoNomina
		
		pVersion =Version        
!----------------------------------------------------------
!
!----------------------------------------------------------
TimbrarClass.Destruct       PROCEDURE
	CODE
		FREE(SELF.QConcepto)
		DISPOSE(SELF.QConcepto)
		FREE(SELF.QPercepciones)
		DISPOSE(SELF.QPercepciones)
		FREE(SELF.QDeducciones)
		DISPOSE(SELF.QDeducciones)
!---------------------------------------------------------
!    
!---------------------------------------------------------
TimbrarClass.Init   PROCEDURE(STRING pPath)
	CODE
		SELF.PathXml = pPath
      ! if SELF.QConcepto<>
        !MESSAGE(BaseGroup.CtadePago)
		

!---------------------------------------------------------
!    
!---------------------------------------------------------
TimbrarClass.creaXml        PROCEDURE()!,BYTE
Totalret                        decimal(12,2)
l:subtot                        decimal(12,2)
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
		XML:CreateAttribute('xsi:schemaLocation','http://www.sat.gob.mx/cfd/3 http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv33.xsd')
		XML:CreateAttribute('LugarExpedicion',CLIP(SELF.Emisor.CodPos))
		IF SELF.BaseGroup.Moneda = 'D'
			XML:CreateAttribute('Moneda','USD')
			XML:CreateAttribute('TipoCambio',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.tipoCambio,@N_12.2)))
		ELSE
			XML:CreateAttribute('Moneda','MXN')
		END
		XML:CreateAttribute('MetodoPago',SELF.BaseGroup.PueoPPD)
		XML:CreateAttribute('FormaPago',CHOOSE(CLIP(SELF.BaseGroup.metodoPago)<>'',CLIP(SELF.BaseGroup.metodoPago),'99'))
		IF SELF.BaseGroup.tipoComprobante='I' OR CLIP(SELF.BaseGroup.tipoComprobante)=''
			XML:CreateAttribute('TipoDeComprobante','I')
		ELSE
			XML:CreateAttribute('TipoDeComprobante','E')
		END
		l:subtot=0
		LOOP I#=1 TO RECORDS(SELF.QConcepto)
			GET(SELF.QConcepto,I#)
            IF ~ERRORCODE()
                if SELF.QConcepto.IvaConcepto
                    l:subtot+=SELF.QConcepto.Iva
                ELSE    
                    IF SELF.Impuestos.PorIva>0
                        l:subtot+=SELF.QConcepto.importe*(SELF.Impuestos.PorIva/100)
                      !  XML:CreateAttribute('Importe',SELF.QuitaEspacios( FORMAT(SELF.QConcepto.importe*(SELF.Impuestos.PorIva/100),@N_12.2)))
                    END    
                END    
			END
		END   
       ! XML:CreateAttribute('Total',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.totalFactura,@N_12.2)))
		XML:CreateAttribute('Total',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.subTotal+l:subtot-self.Impuestos.RetencionIva,@N_12.2)))
		XML:CreateAttribute('SubTotal',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.subTotal,@N_12.2)))
      !  XML:CreateAttribute('SubTotal',SELF.QuitaEspacios(FORMAT(l:subtot,@N_12.2)))
		IF CLIP(SELF.BaseGroup.Serie)<>'' THEN  XML:CreateAttribute('Serie',SELF.BaseGroup.Serie) END
		XML:CreateAttribute('Folio',SELF.BaseGroup.Folio)
		XML:CreateAttribute('Fecha',CLIP(SELF.BaseGroup.Fecha))
        !XML:CreateAttribute('Descuento',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.Descuento,@N_12.2)))
		XML:CreateAttribute('Version',Version)
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
			XML:CreateAttribute('NoCertificado',AStr) !NO DE CERTIFICADO
			SELF.G_NoCertificado = CLIP(AStr)
            !Certificado en estring 
			r# = SELF.gSAT_GetCertAsString(AStr,clip(SELF.PathCertificado))
			IF r# <= 0
				r# = SELF.gSAT_LastError(AStr)
				SELF.Errorno= Error:CertString
				SELF.errorMsg()
				RETURN Level:Notify
			END
		END
		XML:CreateAttribute('Certificado','')
		XML:CreateAttribute('Sello','')
		XML:AddParent()
		IF SELF.BaseGroup.tipoComprobante='E'
			XML:CreateAttribute('TipoRelacion',CLIP(self.NotCred.TipoRelacion))
			XML:CreateParent('cfdi:CfdiRelacionados');XML:AddParent()
			XML:CreateParent('cfdi:CfdiRelacionado')
			XML:CreateAttribute('UUID',CLIP(SELF.NotCred.UUIDRelacionado))
			XML:AddParent(TRUE)
			xml:CloseParent('cfdi:CfdiRelacionados')
		END

		
        !---------------------------- EMISOR --------------------------------
		XML:CreateParent('cfdi:Emisor')
		XML:CreateAttribute('Nombre',SELF.Convierte_Utf8(SELF.Emisor.Nombre))
		XML:CreateAttribute('Rfc',SELF.Convierte_Utf8(SELF.Emisor.Rfc))
		XML:CreateAttribute('RegimenFiscal',SELF.Convierte_Utf8(SELF.Emisor.CveRegimen))
		XML:AddParent(TRUE)
       !expedido end
        !-----------------------------Fin Emisor------------------------------
        
        !---------------------------- RECEPTOR -------------------------------
		XML:CreateParent('cfdi:Receptor')
		XML:CreateAttribute('Nombre',SELF.Convierte_Utf8(SELF.Receptor.Nombre))
		XML:CreateAttribute('Rfc',SELF.Convierte_Utf8(SELF.Receptor.Rfc))
		XML:CreateAttribute('UsoCFDI',SELF.Convierte_Utf8(SELF.Receptor.usoCfdi))
		XML:AddParent(TRUE)
 
        !--------------------Fin Receptor
        
		XML:CreateParent('cfdi:Conceptos');XML:AddParent()
		LOOP I#=1 TO RECORDS(SELF.QConcepto)
			GET(SELF.QConcepto,I#)
			IF ~ERRORCODE()
				XML:CreateAttribute('Cantidad',SELF.QConcepto.cantidad)
				XML:CreateAttribute('Descripcion',SELF.Convierte_Utf8(clip(SELF.QConcepto.descripcion)))
				IF CLIP(SELF.QConcepto.unidad)<>''
					XML:CreateAttribute('Unidad',CLIP(SELF.QConcepto.unidad))
				END	
				XML:CreateAttribute('Importe',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.importe,@N_12.2)))
				XML:CreateAttribute('ValorUnitario',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.valorUnitario,@N_12.2)))
				XML:CreateAttribute('ClaveUnidad',CLIP(SELF.QConcepto.cveUnidad))
				XML:CreateAttribute('ClaveProdServ',CLIP(SELF.QConcepto.cveProdServ))
				XML:CreateParent('cfdi:Concepto');XML:AddParent()
                !MESSAGE('impuesto iva='&SELF.Impuestos.PorIva)
                IF SELF.QConcepto.IvaConcepto
                        XML:CreateParent('cfdi:Impuestos');XML:AddParent()
                        XML:CreateParent('cfdi:Traslados');XML:AddParent()
                        XML:CreateParent('cfdi:Traslado')
                        XML:CreateAttribute('Base',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.importe,@N_12.2)))
                        XML:CreateAttribute('Impuesto',CLIP('002'))
                        XML:CreateAttribute('TipoFactor',CLIP('Tasa'))
                        XML:CreateAttribute('TasaOCuota',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.PorIva/100,@N_12.6)))
                        XML:CreateAttribute('Importe',SELF.QuitaEspacios( FORMAT(SELF.QConcepto.Iva,@N_12.2)))
                        XML:AddParent(TRUE)
                        xml:CloseParent('cfdi:Traslados')  
                        !DO Retenciones
                        !RETENCIONES
                        IF SELF.Impuestos.RetencionIva>0
                            XML:CreateParent('cfdi:Retenciones');XML:AddParent()
                            XML:CreateParent('cfdi:Retencion')
                            XML:CreateAttribute('Base',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.importe,@N_12.2)))
                            XML:CreateAttribute('Impuesto',CLIP('002'))
                            XML:CreateAttribute('TipoFactor',CLIP('Tasa'))
                            XML:CreateAttribute('TasaOCuota','0.160000')
                            XML:CreateAttribute('Importe',SELF.QuitaEspacios( FORMAT(SELF.QConcepto.importe*(SELF.Impuestos.PorIva/100),@N_12.2)))
                            XML:AddParent(TRUE)
                            xml:CloseParent('cfdi:Retenciones')
                        END
                        xml:CloseParent('cfdi:Impuestos') 
                ELSE    
                    IF SELF.Impuestos.PorIva>0
                        XML:CreateParent('cfdi:Impuestos');XML:AddParent()
                        XML:CreateParent('cfdi:Traslados');XML:AddParent()
                        XML:CreateParent('cfdi:Traslado')
                        XML:CreateAttribute('Base',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.importe,@N_12.2)))
                        XML:CreateAttribute('Impuesto',CLIP('002'))
                        XML:CreateAttribute('TipoFactor',CLIP('Tasa'))
                        XML:CreateAttribute('TasaOCuota',SELF.QuitaEspacios(FORMAT(SELF.Impuestos.PorIva/100,@N_12.6)))
                        XML:CreateAttribute('Importe',SELF.QuitaEspacios( FORMAT(SELF.QConcepto.importe*(SELF.Impuestos.PorIva/100),@N_12.2)))
                        XML:AddParent(TRUE)
                        xml:CloseParent('cfdi:Traslados')  
                        !DO Retenciones
                        !RETENCIONES
                        IF SELF.Impuestos.RetencionIva>0
                            XML:CreateParent('cfdi:Retenciones');XML:AddParent()
                            XML:CreateParent('cfdi:Retencion')
                            XML:CreateAttribute('Base',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.importe,@N_12.2)))
                            XML:CreateAttribute('Impuesto',CLIP('002'))
                            XML:CreateAttribute('TipoFactor',CLIP('Tasa'))
                            XML:CreateAttribute('TasaOCuota','0.160000')
                            XML:CreateAttribute('Importe',SELF.QuitaEspacios( FORMAT(SELF.QConcepto.importe*(SELF.Impuestos.PorIva/100),@N_12.2)))
                            XML:AddParent(TRUE)
                            xml:CloseParent('cfdi:Retenciones')
                        END
                    END    
                    !FIN RETENCIONES'
					xml:CloseParent('cfdi:Impuestos') 
				END    
				xml:CloseParent('cfdi:Concepto')
			END
		END
		xml:CloseParent('cfdi:Conceptos')
        !----------------------------------- IMPUESTOS --------------------------
		Totalret = SELF.Impuestos.RetencionIva+SELF.Impuestos.RetencionISr 
		XML:CreateParent('cfdi:Impuestos')
		IF Totalret>0 THEN  XML:CreateAttribute('TotalImpuestosRetenidos',SELF.QuitaEspacios(FORMAT((Totalret),@N_12.2))) END
        !XML:CreateAttribute('TotalImpuestosTrasladados',SELF.QuitaEspacios(FORMAT(SELF.Impuestos.TrasladoIva,@N_12.2)))
		XML:CreateAttribute('TotalImpuestosTrasladados',SELF.QuitaEspacios(FORMAT(l:subtot,@N_12.2)))
        
		XML:AddParent()
		IF Totalret>0 THEN XML:CreateParent('cfdi:Retenciones');XML:AddParent() END
		IF SELF.Impuestos.RetencionIva>0
			XML:CreateParent('cfdi:Retencion')
			XML:CreateAttribute('Impuesto','002')
			XML:CreateAttribute('Importe',SELF.QuitaEspacios(FORMAT(SELF.Impuestos.RetencionIva,@N_12.2)))
			XML:AddParent(TRUE)
		END
		IF SELF.Impuestos.RetencionISr>0
			XML:CreateParent('cfdi:Retencion')
			XML:CreateAttribute('Impuesto','001')
			XML:CreateAttribute('Importe',SELF.QuitaEspacios(FORMAT(SELF.Impuestos.RetencionISr,@N_12.2)))
			XML:AddParent(TRUE)
		END
		IF Totalret  THEN XML:CloseParent('cfdi:Retenciones') END
        !traslados
		XML:CreateParent('cfdi:Traslados');XML:AddParent()
		XML:CreateParent('cfdi:Traslado')
		XML:CreateAttribute('Impuesto','002')
		XML:CreateAttribute('TipoFactor','Tasa')
		XML:CreateAttribute('TasaOCuota',SELF.QuitaEspacios(CHOOSE(SELF.Impuestos.PorIva>0,format(SELF.Impuestos.PorIva/100,@N_12.6),format(0,@N_12.6)))) 
		XML:CreateAttribute('Importe',SELF.QuitaEspacios(FORMAT(l:subtot,@N_12.2)))
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

		
TimbrarClass.creaXmlNomina  PROCEDURE()!,BYTE
Totalret                        decimal(12,2)
l:subtot                        decimal(12,2)
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
		XML:CreateAttribute('xsi:schemaLocation','http://www.sat.gob.mx/cfd/3 http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv33.xsd http://www.sat.gob.mx/nomina12 http://www.sat.gob.mx/sitio_internet/cfd/nomina/nomina12.xsd')
		XML:CreateAttribute('LugarExpedicion',CLIP(SELF.Emisor.CodPos))
		XML:CreateAttribute('Moneda','MXN')
		XML:CreateAttribute('TipoCambio','1')
		XML:CreateAttribute('MetodoPago','PUE')
		XML:CreateAttribute('FormaPago','99')
		XML:CreateAttribute('TipoDeComprobante','N')
		XML:CreateAttribute('Total',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.totalFactura,@N_12.2)))
		XML:CreateAttribute('SubTotal',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.subTotal,@N_12.2)))
		IF CLIP(SELF.BaseGroup.Serie)<>'' THEN  XML:CreateAttribute('Serie',SELF.BaseGroup.Serie) END
		XML:CreateAttribute('Folio',SELF.BaseGroup.Folio)
		XML:CreateAttribute('Fecha',CLIP(SELF.BaseGroup.Fecha))
		if SELF.BaseGroup.Descuento>0 THEN XML:CreateAttribute('Descuento',SELF.QuitaEspacios(FORMAT(SELF.BaseGroup.Descuento,@N_12.2))) END
		XML:CreateAttribute('Version',Version)
		XML:CreateAttribute('xmlns:cfdi','http://www.sat.gob.mx/cfd/3')
		XML:CreateAttribute('xmlns:tfd','http://www.sat.gob.mx/TimbreFiscalDigital')
		XML:CreateAttribute('xmlns:xsi','http://www.w3.org/2001/XMLSchema-instance')
		XML:CreateAttribute('xmlns:nomina12','http://www.sat.gob.mx/nomina12')
        !=======================================================================
        !obtenemos el no de certificado
		r# = SELF.gSAT_GetCertNumber(AStr,clip(SELF.PathCertificado))!pasamos el certificado guardo en datos del sistema
		IF r# <= 0
			r# = SELF.gSAT_LastError(AStr)
			self.Errorno=Error:Certificado
			SELF.errorMsg()
			RETURN Level:Notify
		ELSE
			XML:CreateAttribute('NoCertificado',AStr) !NO DE CERTIFICADO
			SELF.G_NoCertificado = CLIP(AStr)
            !Certificado en estring 
			r# = SELF.gSAT_GetCertAsString(AStr,clip(SELF.PathCertificado))
			IF r# <= 0
				r# = SELF.gSAT_LastError(AStr)
				SELF.Errorno= Error:CertString
				SELF.errorMsg()
				RETURN Level:Notify
			END
		END
		XML:CreateAttribute('Certificado','')
		XML:CreateAttribute('Sello','')
		XML:AddParent()
        !---------------------------- EMISOR --------------------------------
		XML:CreateParent('cfdi:Emisor')
		XML:CreateAttribute('Nombre',SELF.Convierte_Utf8(SELF.Emisor.Nombre))
		XML:CreateAttribute('Rfc',SELF.Convierte_Utf8(SELF.Emisor.Rfc))
		!IF clip(SELF.Emisor.Curp)<>''
		!	XML:CreateAttribute('Curp',SELF.Convierte_Utf8(SELF.Emisor.Curp))
		!END		
		XML:CreateAttribute('RegimenFiscal',SELF.Convierte_Utf8(SELF.Emisor.CveRegimen))
		XML:AddParent(TRUE)
        !---------------------------- RECEPTOR -------------------------------
		XML:CreateParent('cfdi:Receptor')
		XML:CreateAttribute('Nombre',SELF.Convierte_Utf8(SELF.Receptor.Nombre))
		XML:CreateAttribute('Rfc',SELF.Convierte_Utf8(SELF.Receptor.Rfc))
		XML:CreateAttribute('UsoCFDI',SELF.Convierte_Utf8('P01'))
		XML:AddParent(TRUE)
        
		GET(SELF.QConcepto,1)
		XML:CreateParent('cfdi:Conceptos');XML:AddParent()
        XML:CreateAttribute('Cantidad','1')
        if self.Nomina.TotalDeduccion
            XML:CreateAttribute('Descuento',SELF.QuitaEspacios(FORMAT(self.Nomina.TotalDeduccion,@N_12.2)))
        END    
		XML:CreateAttribute('Descripcion',SELF.Convierte_Utf8(clip(SELF.QConcepto.descripcion)))
		XML:CreateAttribute('Importe',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.importe,@N_12.2)))
		XML:CreateAttribute('ValorUnitario',SELF.QuitaEspacios(FORMAT(SELF.QConcepto.valorUnitario,@N_12.2)))
		XML:CreateAttribute('ClaveUnidad',CLIP(SELF.QConcepto.cveUnidad))
		XML:CreateAttribute('ClaveProdServ',CLIP(SELF.QConcepto.cveProdServ))
		XML:CreateParent('cfdi:Concepto');XML:AddParent()
		XML:CloseParent('cfdi:Concepto')
		xml:CloseParent('cfdi:Conceptos')
		
		!===============complemento=======================
		XML:CreateParent('cfdi:Complemento');XML:AddParent()
		XML:CreateAttribute('Version','1.2')
		XML:CreateAttribute('TipoNomina',SELF.Nomina.TipoNomina)
		XML:CreateAttribute('FechaPago',(FORMAT(SELF.Nomina.FechaPago,@D10-)))
		XML:CreateAttribute('FechaInicialPago',(FORMAT(SELF.Nomina.FechaInicial,@D10-)))
        XML:CreateAttribute('FechaFinalPago',(FORMAT(SELF.Nomina.FechaFinal,@D10-)))
        IF SELF.Nomina.DiasPago>=1
            XML:CreateAttribute('NumDiasPagados',SELF.Nomina.DiasPago)
        ELSE
            XML:CreateAttribute('NumDiasPagados','0.001')
        END
		XML:CreateAttribute('TotalPercepciones',SELF.QuitaEspacios(FORMAT(SELF.Nomina.TotalPercepcion,@N_12.2)))
		if SELF.Nomina.TotalDeduccion then XML:CreateAttribute('TotalDeducciones',SELF.QuitaEspacios(FORMAT(SELF.Nomina.TotalDeduccion,@N_12.2))) END
		XML:CreateAttribute('TotalOtrosPagos',SELF.QuitaEspacios(FORMAT(SELF.Nomina.TotalOtrosPagos,@N_12.2)))
        XML:CreateParent('nomina12:Nomina');XML:AddParent()
        if clip(SELF.Nomina.RegistroPatronal)<>''
            XML:CreateAttribute('RegistroPatronal',SELF.Convierte_Utf8(SELF.Nomina.RegistroPatronal))
        END    
        if  clip(SELF.Emisor.Curp)<>''
            XML:CreateAttribute('Curp',SELF.Convierte_Utf8(SELF.Emisor.Curp))
        END
		XML:CreateParent('nomina12:Emisor');XML:AddParent(TRUE)

		XML:CreateAttribute('ClaveEntFed',SELF.Convierte_Utf8(SELF.Nomina.ClaveEntFedera))
        XML:CreateAttribute('TipoJornada',SELF.Convierte_Utf8(format(SELF.Nomina.TipoJornada,@n02)))
        IF  SELF.Nomina.Sdi>0
            XML:CreateAttribute('SalarioDiarioIntegrado',SELF.QuitaEspacios(format(SELF.Nomina.Sdi,@n_12.2)))
        END
        if clip(SELF.Nomina.RiesgoPuesto)<>''
            XML:CreateAttribute('RiesgoPuesto',SELF.Convierte_Utf8(SELF.Nomina.RiesgoPuesto))
        END    
        if clip(SELF.Nomina.Antiguedad)<>''
            XML:CreateAttribute(SELF.Convierte_Utf8('Antigüedad'),SELF.Convierte_Utf8(SELF.Nomina.Antiguedad))    
        END    
        if SELF.Nomina.FechaIngreso
            XML:CreateAttribute('FechaInicioRelLaboral',FORMAT(SELF.Nomina.FechaIngreso,@D10-))
        END    
        if clip(SELF.Nomina.Nss)<>''
            XML:CreateAttribute('NumSeguridadSocial',SELF.Convierte_Utf8(SELF.Nomina.Nss))
        END
		XML:CreateAttribute('TipoRegimen',SELF.Convierte_Utf8(format(SELF.Nomina.TipoRegimen,@n02)))
		XML:CreateAttribute('TipoContrato',SELF.Convierte_Utf8(format(SELF.Nomina.TipoContrato,@n02)))
		XML:CreateAttribute('PeriodicidadPago',SELF.Convierte_Utf8(SELF.Nomina.Periodicidad))
		XML:CreateAttribute('Curp',SELF.Convierte_Utf8(SELF.Nomina.Curp))
		IF CLIP(SELF.Nomina.Cuenta)<>''
			XML:CreateAttribute('CuentaBancaria',SELF.Nomina.Cuenta)
		END
		XML:CreateAttribute('NumEmpleado',SELF.Convierte_Utf8(SELF.Nomina.NumEmp))
		XML:CreateAttribute('Puesto',SELF.Convierte_Utf8(SELF.Nomina.Puesto))
		XML:CreateAttribute('Banco',SELF.Convierte_Utf8(format(SELF.Nomina.Banco,@n03)))
		XML:CreateParent('nomina12:Receptor');XML:AddParent(TRUE)
		!percepciones
		IF SELF.Nomina.TotalPercepcion
			XML:CreateParent('nomina12:Percepciones') 
			XML:CreateAttribute('TotalGravado',SELF.QuitaEspacios(FORMAT(SELF.Nomina.TotalGravado,@N_12.2)))
			XML:CreateAttribute('TotalExento',SELF.QuitaEspacios(FORMAT(SELF.Nomina.TotalExcento,@N_12.2)))
			XML:CreateAttribute('TotalSueldos',SELF.QuitaEspacios(FORMAT(SELF.Nomina.TotalGravado+SELF.Nomina.TotalExcento,@N_12.2)))
			XML:AddParent() 
			LOOP I#=1 TO RECORDS(SELF.QPercepciones)
				GET(SELF.QPercepciones,I#)
				IF ERRORCODE() THEN CYCLE END
				XML:CreateParent('nomina12:Percepcion')
				XML:CreateAttribute('TipoPercepcion',SELF.QPercepciones.TipoPercepcion)
				XML:CreateAttribute('Clave',SELF.QPercepciones.Clave)
				XML:CreateAttribute('Concepto',CLIP(SELF.QPercepciones.Concepto))
				XML:CreateAttribute('ImporteGravado',SELF.QuitaEspacios(FORMAT(SELF.QPercepciones.ImporteGravado,@N_12.2)))
				XML:CreateAttribute('ImporteExento',SELF.QuitaEspacios(FORMAT(SELF.QPercepciones.ImporteExcento,@N_12.2)))
				XML:AddParent(TRUE)
			END
			XML:CloseParent('nomina12:Percepciones')
		END	
		!Deducciones
		IF SELF.Nomina.TotalDeduccion>0
			XML:CreateParent('nomina12:Deducciones')
			XML:CreateAttribute('TotalOtrasDeducciones',self.QuitaEspacios(FORMAT(self.Nomina.TotalDeduccion-self.Nomina.Ispt,@N_12.2)))
			IF self.Nomina.Ispt>0
				XML:CreateAttribute('TotalImpuestosRetenidos',SELF.QuitaEspacios(FORMAT(self.Nomina.Ispt,@N_12.2)))
			END
			XML:AddParent()
			LOOP I#=1 TO RECORDS(SELF.QDeducciones)
				GET(SELF.QDeducciones,I#)
				IF ERRORCODE() THEN CYCLE END
				XML:CreateParent('nomina12:Deduccion')
				XML:CreateAttribute('TipoDeduccion',SELF.QDeducciones.TipoPercepcion)
				XML:CreateAttribute('Clave',SELF.QDeducciones.Clave)
				XML:CreateAttribute('Concepto',CLIP(SELF.QDeducciones.Concepto))
				XML:CreateAttribute('Importe',SELF.QuitaEspacios(FORMAT(SELF.QDeducciones.ImporteGravado,@N_12.2)))
				XML:AddParent(TRUE)
			END
			XML:CloseParent('nomina12:Deducciones')
		END
		!IF SELF.Nomina.CredSal
			XML:CreateParent('nomina12:OtrosPagos');XML:AddParent()
			XML:CreateAttribute('TipoOtroPago','002')
			XML:CreateAttribute('Clave','002')
			XML:CreateAttribute('Concepto',CLIP('SUBSIDIO PARA EL EMPLEO'))
			XML:CreateAttribute('Importe',SELF.QuitaEspacios(FORMAT(SELF.Nomina.CredSal,@N_12.2)))
			XML:CreateParent('nomina12:OtroPago');XML:AddParent()
			XML:CreateAttribute('SubsidioCausado',SELF.QuitaEspacios(FORMAT(SELF.Nomina.CredSal,@N_12.2)))
			XML:CreateParent('nomina12:SubsidioAlEmpleo');XML:AddParent(TRUE)
			xml:CloseParent('nomina12:OtroPago')
			xml:CloseParent('nomina12:OtrosPagos')
		!END
		
		xml:CloseParent('nomina12:Nomina')
		xml:CloseParent('cfdi:Complemento')
		XML:CloseParent('cfdi:Comprobante')
		XML:CloseXMLFile()
        
		RETURN lResponse		
        
TimbrarClass.creaXmlPago    PROCEDURE()
lResponse                       BYTE
Fecha                           string(30)
Loc:Moneda                      string(3)

	code
		lResponse = XML:CreateXMLFILE(SELF.PathXml) !crea archivo xml
		IF  lResponse <> 0 
			SELF.Errorno = Error:CrearXml
			SELF.errorMsg()
			RETURN Level:Notify
		END
        !crea encabezado
		XML:CreateParent('cfdi:Comprobante')
		XML:CreateAttribute('xsi:schemaLocation','http://www.sat.gob.mx/cfd/3 http://www.sat.gob.mx/sitio_internet/cfd/3/cfdv33.xsd http://www.sat.gob.mx/Pagos http://www.sat.gob.mx/sitio_internet/cfd/Pagos/Pagos10.xsd')
		XML:CreateAttribute('LugarExpedicion',CLIP(SELF.Emisor.CodPos))
		XML:CreateAttribute('Moneda','XXX')
		XML:CreateAttribute('TipoDeComprobante','P')
		XML:CreateAttribute('Total','0')
		XML:CreateAttribute('SubTotal','0')
		XML:CreateAttribute('Folio',SELF.BaseGroup.Folio)
		XML:CreateAttribute('Fecha',CLIP(SELF.BaseGroup.Fecha))
		XML:CreateAttribute('Version','3.3')
		XML:CreateAttribute('xmlns:xsi','http://www.w3.org/2001/XMLSchema-instance')
		XML:CreateAttribute('xmlns:cfdi','http://www.sat.gob.mx/cfd/3')
		XML:CreateAttribute('xmlns:pago10','http://www.sat.gob.mx/Pagos')
        !=======================================================================
        
        !obtenemos el no de certificado
		r# = SELF.gSAT_GetCertNumber(AStr,clip(SELF.PathCertificado))!pasamos el certificado guardo en datos del sistema
		IF r# > 0
          !pasa
		else
			r# = SELF.gSAT_LastError(AStr)
			self.Errorno=Error:Certificado
			SELF.errorMsg()
			RETURN Level:Notify
		END
		XML:CreateAttribute('NoCertificado',AStr) !NO DE CERTIFICADO
		SELF.G_NoCertificado =CLIP(AStr)
        !obtenemos la cadena original.
		r# = SELF.gSAT_GetCertAsString(AStr,clip(SELF.PathCertificado))
		IF r# > 0
          !nada
		ELSE
			r# = self.gSAT_LastError(AStr)
			self.Errorno=Error:Certificado
			SELF.errorMsg()
			RETURN Level:Notify
		END
		XML:CreateAttribute('Certificado','')
		XML:CreateAttribute('Sello','')
		XML:AddParent()

         !Emisor
		XML:CreateParent('cfdi:Emisor')
		XML:CreateAttribute('Nombre',SELF.Convierte_Utf8(SELF.Emisor.Nombre))
		XML:CreateAttribute('Rfc',SELF.Convierte_Utf8(SELF.Emisor.Rfc))
		XML:CreateAttribute('RegimenFiscal',SELF.Convierte_Utf8(SELF.Emisor.CveRegimen))
		XML:AddParent(TRUE)
          !=============================================================

          !---------------------------- RECEPTOR -------------------------------
		XML:CreateParent('cfdi:Receptor')
		XML:CreateAttribute('Nombre',SELF.Convierte_Utf8(SELF.Receptor.Nombre))
		XML:CreateAttribute('Rfc',SELF.Convierte_Utf8(SELF.Receptor.Rfc))
		XML:CreateAttribute('UsoCFDI',SELF.Convierte_Utf8('P01'))
		XML:AddParent(TRUE)
            
        
          !------------------------------- CONCEPTOS -------------------------------
		XML:CreateParent('cfdi:Conceptos');XML:AddParent()
		XML:CreateParent('cfdi:Concepto')
		XML:CreateAttribute('Cantidad',1)
		XML:CreateAttribute('Descripcion',SELF.Convierte_Utf8(clip('Pago')))
		XML:CreateAttribute('Importe','0')
		XML:CreateAttribute('ValorUnitario','0')
		XML:CreateAttribute('ClaveUnidad','ACT')
		XML:CreateAttribute('ClaveProdServ','84111506')
		XML:AddParent(TRUE)
            
		xml:CloseParent('cfdi:Conceptos')
        
		XML:CreateParent('cfdi:Complemento');XML:AddParent()
		XML:CreateAttribute('Version','1.0')
		XML:CreateParent('pago10:Pagos');XML:AddParent()
		XML:CreateAttribute('FechaPago',clip(format(SELF.rep.Fecha,@d10-))&'T12:00:00')
		XML:CreateAttribute('FormaDePagoP',clip(SELF.rep.FormaPago))
		XML:CreateAttribute('MonedaP',clip(clip(self.rep.MonedaP)))
		if clip(self.rep.MonedaP)<>'MXN'
			XML:CreateAttribute('TipoCambioP',self.QuitaEspacios(FORMAT(SELF.rep.TipoCambiop,@N_12.2)))
		END
		XML:CreateAttribute('Monto',self.QuitaEspacios(FORMAT(SELF.rep.Total,@N_14.2)))
		IF CLIP(SELF.rep.rfcBanOrd)<>'' AND CLIP(SELF.rep.ctaOrdena)<>'' AND cliP(SELF.rep.rfcBanRec)<>'' AND clip(SELF.rep.ctaRecept)<>''
			XML:CreateAttribute('RfcEmisorCtaOrd',clip(SELF.rep.rfcBanOrd))
			XML:CreateAttribute('CtaOrdenante',clip(SELF.rep.ctaOrdena))
			XML:CreateAttribute('RfcEmisorCtaBen',cliP(SELF.rep.rfcBanRec))
			XML:CreateAttribute('CtaBeneficiario',clip(SELF.rep.ctaRecept))
		END
		XML:CreateParent('pago10:Pago');XML:AddParent()
        !pago relacionado
        
		loop i#=1 to RECORDS(self.QConcepto)
			get(self.QConcepto,i#)
			if ERRORCODE() then cycle END
			XML:CreateParent('pago10:DoctoRelacionado')
			XML:CreateAttribute('IdDocumento',CLIP(self.QConcepto.UUID))
			XML:CreateAttribute('MonedaDR',CLIP(self.QConcepto.Moneda))
			if clip(self.QConcepto.Moneda)<>CLIP(SELF.REP.MonedaP)
				XML:CreateAttribute('TipoCambioDR',SELF.QuitaEspacios(FORMAT(self.QConcepto.TipoCambio,@N_12.4))) 
			END                
			XML:CreateAttribute('NumParcialidad','1')
            XML:CreateAttribute('MetodoDePagoDR','PPD')
            IF SELF.REP.ImporteFactoraje=0
                XML:CreateAttribute('ImpSaldoAnt',SELF.QuitaEspacios(FORMAT(self.QConcepto.SaldoAnt,@N_12.2)))
    			XML:CreateAttribute('ImpPagado',self.QuitaEspacios(FORMAT(self.QConcepto.importe,@N_12.2)))
                XML:CreateAttribute('ImpSaldoInsoluto',self.QuitaEspacios(FORMAT(self.QConcepto.SaldoNvo,@N_12.2)))
            END
			XML:AddParent(TRUE)
		END
		xml:CloseParent('pago10:Pago')
        xml:CloseParent('pago10:Pagos')
        IF SELF.REP.ImporteFactoraje
            XML:CreateAttribute('Version','1.0')
            XML:CreateParent('pago10:Pagos');XML:AddParent()
            XML:CreateAttribute('FechaPago',clip(format(SELF.rep.Fecha,@d10-))&'T12:00:00')
		    XML:CreateAttribute('FormaDePagoP',clip('17'))
		    XML:CreateAttribute('MonedaP',clip(clip(self.rep.MonedaP)))
            XML:CreateAttribute('Monto',self.QuitaEspacios(FORMAT(SELF.REP.ImporteFactoraje,@N_12.2)))
            XML:CreateParent('pago10:Pago');XML:AddParent()
            xml:CloseParent('pago10:Pago')
            xml:CloseParent('pago10:Pagos')
        END
		xml:CloseParent('cfdi:Complemento')!cerramos el complemento
        
		xml:CloseParent('cfdi:Comprobante')
		XML:CloseXMLFile()

		RETURN lResponse
                                            
TimbrarClass.creaXmlRetencion       PROCEDURE()
lResponse                               BYTE
Fecha                                   string(30)
	code
        !IF ~SELF.ValidaCampos() THEN RETURN TRUE END
		lResponse = XML:CreateXMLFILE(SELF.PathXml) !crea archivo xml
		IF  lResponse <> 0 
			SELF.Errorno = Error:CrearXml
			SELF.errorMsg()
			RETURN Level:Notify
		END
		Fecha = clip(self.SetFecha(self.tRetenciones.FechaExp,self.tRetenciones.HoraExp))&'-06:00'
		XML:CreateParent('retenciones:Retenciones')
		XML:CreateAttribute('xsi:schemaLocation','http://www.sat.gob.mx/esquemas/retencionpago/1 http://www.sat.gob.mx/esquemas/retencionpago/1/retencionpagov1.xsd')
		XML:CreateAttribute('xmlns:retenciones','http://www.sat.gob.mx/esquemas/retencionpago/1')
		XML:CreateAttribute('FolioInt',SELF.tRetenciones.FolioInt)
		XML:CreateAttribute('FechaExp',CLIP(Fecha))
		XML:CreateAttribute('Version','1.0')
		XML:CreateAttribute('CveRetenc',self.tRetenciones.CveRetenc)
		XML:CreateAttribute('DescRetenc',self.tRetenciones.DescRetenc)
        
		XML:CreateAttribute('xmlns:xsi','http://www.w3.org/2001/XMLSchema-instance')
        !=======================================================================
        !obtenemos el no de certificado
		r# = SELF.gSAT_GetCertNumber(AStr,clip(SELF.PathCertificado))!pasamos el certificado guardo en datos del sistema
		IF r# <= 0
			r# = SELF.gSAT_LastError(AStr)
			self.Errorno=Error:Certificado
			SELF.errorMsg()
			RETURN Level:Notify
		ELSE
			XML:CreateAttribute('NumCert',AStr) !NO DE CERTIFICADO
			SELF.G_NoCertificado = CLIP(AStr)
            !Certificado en estring 
			r# = SELF.gSAT_GetCertAsString(AStr,clip(SELF.PathCertificado))
			IF r# <= 0
				r# = SELF.gSAT_LastError(AStr)
				SELF.Errorno= Error:CertString
				SELF.errorMsg()
				RETURN Level:Notify
			ELSE
				XML:CreateAttribute('Cert',clip(AStr))
			END
		END
        
        !XML:CreateAttribute('Cert','')
		XML:CreateAttribute('Sello','')
		XML:AddParent()
        
        !---------------------------- EMISOR --------------------------------
		XML:CreateParent('retenciones:Emisor')
		XML:CreateAttribute('NomDenRazSocE',SELF.Convierte_Utf8(SELF.tRetenciones.NomDenRazSocE))
		XML:CreateAttribute('RFCEmisor',SELF.Convierte_Utf8(SELF.tRetenciones.RFCEmisor))
		XML:AddParent(TRUE)
       !expedido end
        !-----------------------------Fin Emisor------------------------------
        
        !---------------------------- RECEPTOR -------------------------------
		XML:CreateParent('retenciones:Receptor')
		XML:CreateAttribute('Nacionalidad',CLIP(SELF.tRetenciones.nacionalidad))
		XML:AddParent()
		XML:CreateParent('retenciones:Extranjero');
		XML:CreateAttribute('NumRegIdTrib',CLIP(SELF.tRetenciones.NumRegIdTrib))
		XML:CreateAttribute('NomDenRazSocR',CLIP(SELF.tRetenciones.NomDenRazSocR))
		XML:AddParent(TRUE)
        !XML:CloseParent('retenciones:Extranjero')
		XML:CloseParent('retenciones:Receptor')
        !--------------------Fin Receptor
		XML:CreateParent('retenciones:Periodo ')
		XML:CreateAttribute('Ejerc',SELF.tRetenciones.Ejerc)
		XML:CreateAttribute('MesFin',SELF.tRetenciones.MesFin)
		XML:CreateAttribute('MesIni',SELF.tRetenciones.MesIni)
		XML:AddParent(TRUE)

		XML:CreateParent('retenciones:Totales')
		XML:CreateAttribute('montoTotOperacion',SELF.QuitaEspacios(FORMAT(SELF.tRetenciones.MontoOperacion,@N_12.2)))
		XML:CreateAttribute('montoTotGrav',SELF.QuitaEspacios(FORMAT(SELF.tRetenciones.MontoGravado,@N_12.2)))
		XML:CreateAttribute('montoTotExent',SELF.QuitaEspacios(FORMAT(SELF.tRetenciones.MontoExcento,@N_12.2)))
		XML:CreateAttribute('montoTotRet',SELF.QuitaEspacios(FORMAT(SELF.tRetenciones.MontoRetencion,@N_12.2)))
		XML:AddParent()
		XML:CreateParent('retenciones:ImpRetenidos');
		XML:CreateAttribute(' BaseRet',SELF.QuitaEspacios(FORMAT(SELF.tRetenciones.Base,@N_12.2)))
		XML:CreateAttribute('Impuesto',CLIP(SELF.tRetenciones.Impuesto))
		XML:CreateAttribute(' montoRet',SELF.QuitaEspacios(FORMAT(SELF.tRetenciones.MontoRetencion,@N_12.2)))
		XML:CreateAttribute('TipoPagoRet',CLIP(SELF.tRetenciones.TipodePago))
		XML:AddParent(TRUE)
        !XML:CloseParent('retenciones:Extranjero')
		XML:CloseParent('retenciones:Totales')
        
        
		XML:CloseParent('retenciones:Retenciones')
		XML:CloseXMLFile()
        
		RETURN lResponse
        
!---------------------------------------------------------
!    No se va a usar 
!---------------------------------------------------------
TimbrarClass.SetCertificado PROCEDURE(STRING pNocer)
	CODE
        


!---------------------------------------------------------
!    pValors a Letras
!---------------------------------------------------------
TimbrarClass.NumeroaLetra   PROCEDURE(STRING pValor,STRING pMoneda)!,STRING      
Centavos                        REAL
Unidades                        REAL
Decenas                         REAL
Centenas                        REAL
Valor                           REAL
SinCientos                      REAL
Respuesta                       STRING(240)
Modulo                          STRING(34)
Parcial                         STRING(34)
D_Unidades_G                    STRING(210)
D_Unidades                      STRING(10),DIM(21),OVER(D_Unidades_G)
D_Decenas_G                     STRING(72)
D_Decenas                       STRING(9),DIM(8),OVER(D_Decenas_G)
D_Centenas_G                    STRING(117)
D_Centenas                      STRING(13),DIM(9),OVER(D_Centenas_G)

	CODE
		D_Unidades_G = 'CERO      ' |        !Carga de Unidades
			& 'UN        ' |
			& 'DOS       ' |
			& 'TRES      ' |
			& 'CUATRO    ' |
			& 'CINCO     ' |
			& 'SEIS      ' |
			& 'SIETE     ' |
			& 'OCHO      ' |
			& 'NUEVE     ' |
			& 'DIEZ      ' |
			& 'ONCE      ' |        ! Y de primera decena
			& 'DOCE      ' |
			& 'TRECE     ' |
			& 'CATORCE   ' |
			& 'QUINCE    ' |
			& 'DIECISEIS ' |
			& 'DIECISIETE' |
			& 'DIECIOCHO ' |
			& 'DIECINUEVE' |
			& 'VEINTE    '
		D_Decenas_G =  'VEINTI   ' |         !Carga de Decenas
			& 'TREINTA  ' |
			& 'CUARENTA ' |
			& 'CINCUENTA' |
			& 'SESENTA  ' |
			& 'SETENTA  ' |
			& 'OCHENTA  ' |
			& 'NOVENTA  '
		D_Centenas_G = 'CIENTO       ' |     !Carga de Centenas
			& 'DOSCIENTOS   ' |
			& 'TRESCIENTOS  ' |
			& 'CUATROCIENTOS' |
			& 'QUINIENTOS   ' |
			& 'SEISCIENTOS  ' |
			& 'SETECIENTOS  ' |
			& 'OCHOCIENTOS  ' |
			& 'NOVECIENTOS  '
!--------------------------------------------------------------------------
		Clear(Respuesta)
		Valor = INT(pValor)
		Centavos = (pValor - Valor) * 100
		if pMoneda='D' then
			Respuesta = 'DOLARES M.A.'
		else
			Respuesta = 'PESOS M.N.'
		end
 ! IF Glo:UsaDecimales
		Valor = INT(Centavos + 0.01)
		Case Valor
		OF 0
			if pMoneda='D' then
				Respuesta = 'DOLARES M.A.'
			else
				Respuesta = 'PESOS M.N.'
			end
		OF 1
			if pMoneda='D' then
				Respuesta = 'DOLARES  UN CENTAVO M.A.'
			else
				Respuesta = 'PESOS UN CENTAVO M.N'
			end
		Else
			Do Calc_Parcial
			if pMoneda='D' then
				Respuesta = 'DOLARES CON '&Clip(Parcial)&'  M.A.'
			else
				Respuesta = 'PESOS  '&Clip(Parcial)&' M.N.'
			end
		End !Case
     !IF ~GLO:LtrDecimales
		if pMoneda='D' then
			Respuesta = 'DOLARES '&FORMAT(Valor,@n02)&'/100 M.A.'
		else
			Respuesta = 'PESOS '&FORMAT(Valor,@n02)&'/100 M.N.'
		end
    ! End !IF
 ! End !IF

		Valor = INT (pValor - (INT(pValor / 1000) *1000))        !Unidades
		If Valor <> 0 Then
			Do Calc_Modulo
			Respuesta = Clip (Modulo) &' '& Clip (Respuesta)
		.

		Valor = INT (pValor / 1000) - (INT(pValor / 1000000) * 1000)  !Miles
		If Valor = 1 Then
			Respuesta = 'UN MIL ' & Clip (Respuesta)
		ElsIf Valor <> 0 Then
			Do Calc_Modulo
			Respuesta = Clip (Modulo) & ' MIL ' & Clip (Respuesta)
		.

		Valor = INT (pValor / 1000000)   !Millones
		If Valor = 1 Then
			Respuesta = 'UN MILLON '& Clip (Respuesta)
		ElsIf Valor <> 0 Then
			Do Calc_Modulo
			Respuesta = Clip (Modulo) & ' MILLONES ' & Clip (Respuesta)
		.

		Return(Respuesta)

!---------------------------------------------------------------------------
Calc_Modulo         ROUTINE
	If valor = 100 Then
		Modulo = 'CIEN'
	Else
		Do Calc_Parcial
		IF Valor > 100 Then
			Valor = int(Valor / 100)
			If SinCientos = 0 Then
				Modulo = Clip(D_Centenas[Valor])
			Else
				Modulo = Clip(D_Centenas[Valor]) &' '& Clip(Parcial)
			.
		Else
			Modulo = Clip(Parcial)
			. .

!---------------------------------------------------------------------------
Calc_Parcial        ROUTINE

	Centenas = Int(Valor/100)
	Decenas  = INT(Valor/10) - Centenas*10
	Unidades = Valor - (Decenas*10) - (Centenas*100) +1
	SinCientos = (Decenas*10)+ Unidades - 1

	Case SinCientos

	Of 0 to 20
		Parcial = Clip(D_Unidades[SinCientos+1])

	Of 21 to 99
		If Unidades = 1 Then
			Parcial = Clip(D_Decenas[Decenas-1])
		ElsIF Decenas > 2 Then
			Parcial = Clip(D_Decenas[Decenas-1]) & ' Y '& Clip(D_Unidades[Unidades])
		Else
			Parcial = Clip(D_Decenas[Decenas-1]) & Clip(D_Unidades[Unidades])
		.

	.




        

        



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
		NChars = SAT_SignXml(Buf, AXML, ALlave, APass, ACert,0)
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
TimbrarClass.gSAT_GetXmlAttribute   PROCEDURE(*STRING PAR:Buffer,STRING PAR:XML,STRING PAR:Atrib,STRING PAR:Elem)!,LONG        
NChars                                  LONG
Buf                                     cSTRING(20000)
AXML                                    CSTRING(1025)
Atrib                                   CSTRING(1025)
Elem                                    CSTRING(1025)
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
!    Crea imagen del Codigo QRCODE
!---------------------------------------------------------        
TimbrarClass.CreaQrCode     PROCEDURE(STRING pContenido,STRING pPath)     
Contenido                       CSTRING(1000)
PathQ                           CSTRING(250)
	CODE
		Contenido = clip(pContenido)
		PathQ     = pPath 
		FastQRCodeA(Contenido,PathQ)
        
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
					SELF.LastError =CHOOSE(clip(SELF.BaseGroup.tipoComprobante)='N','No Emp='&self.Nomina.NumEmp,'')& ' Al  Sellar el Xml errorno '&lResult&' '&AStr
					SELF.errorMsg()
					lOk = FALSE
				ELSE
              ! --- Verificar firma --- !
					lResult = SELF.gSAT_VerifySignature(SELF.PathXmlSellado)
					IF lResult <> 0
						lResult = SELF.gSAT_LastError(AStr)
						SELF.Errorno = Error:SellarXml
						SELF.LastError =CHOOSE(clip(SELF.BaseGroup.tipoComprobante)='N','No Emp='&self.Nomina.NumEmp,'')& ' Al  Verificar la Firma errorno '&lResult
						SELF.errorMsg()
						lOk = FALSE
					END
				END
			ELSE
				MESSAGE('ubo errro ='&lResult)
				lResult = SELF.gSAT_LastError(AStr)
				SELF.LastError = clip(AStr)
				SELF.LastError = CHOOSE(clip(SELF.BaseGroup.tipoComprobante)='N','No Emp='&self.Nomina.NumEmp,'')&' Al  Sellar el Xml errorno '&lResult&' '&AStr
				SELF.Errorno = Error:SellarXml
				SELF.errorMsg()
				lOk = FALSE
			END
		ELSE
			MESSAGE('No se encontraron los archivos necesarios, verifique por favor')
			lOk = FALSE
		END   
        
		RETURN lOk

        

        
!---------------------------------------------------------
!    Timbrar el Xml 
!---------------------------------------------------------
!TimbrarClass.TimbrarXml     PROCEDURE(STRING pUsuario,STRING pPassword,STRING pTipoDoc)!,BYTE     
!!curl                            TCurlMailClass
!!res                             CURLcode,Auto
!respBuffer                      CSTRING(32768) 
!Respuesta1                      STRING(32768)  
!Respuesta                       STRING(32768)  
!envia                           STRING(32768)
!Loc:Msg                         string(1000)
!Recs                            Long
!lOk                             BYTE
!Wproceso                        WINDOW,AT(,,415,44),FONT('Microsoft Sans Serif',8,,FONT:regular),CENTER,GRAY
!									BOX,AT(0,0,415,44),USE(?BOX1),FILL(COLOR:White),LINEWIDTH(1)
!									IMAGE('conexion.jpg'),AT(0,0,97,44),USE(?IMAGE1)
!									STRING('Estabelciendo Conexion con el Pac, Por Favor Espere '),AT(111,19,287,12), |
!										USE(?STRING1),FONT('Segoe UI',13,00CC6600h,FONT:bold)
!								END
!
!	CODE
!		OPEN(Wproceso)
!		DISPLAY
!		lOk = FALSE
!		Loc:Msg='0'!Timbrar(clip(SELF.PathXmlSellado),clip(self.PathXmlTimbrado),clip(pUsuario),clip(pPassword),CLIP(pTipoDoc))
!		if sub(Loc:Msg,1,1)='1'
!			self.BuscaAtributos()   
!			lOk = TRUE
!		ELSE
!			SELF.LastError =CLIP('ERROR No...'&clip(sub(Loc:Msg,2,LEN(Loc:Msg))))
!			MESSAGE(CLIP('ERROR No...'&clip(sub(Loc:Msg,2,LEN(Loc:Msg)))))
!		END
!		CLOSE(Wproceso)
!		CLEAR(Loc:Msg)
!		RETURN lok

TimbrarClass.TimbrarXml2    PROCEDURE(STRING pUsuario,STRING pPassword,STRING pTipoDoc)!,BYTE     
curl                            TCurlClass! TCurlMailClass
res                             CURLcode,Auto
Respuesta1                      STRING(271238)  
Respuesta                       SystemStringClass!STRING(100000)  
oStr                            SystemStringClass
EnviaStr                        SystemStringClass
Resultado                       STRING(100000)
Quitar                          STRING(20000)
Loc:Msg                         string(1000)
Recs                            Long
lOk                             BYTE
Wproceso                        WINDOW,AT(,,415,44),FONT('Microsoft Sans Serif',8,,FONT:regular),CENTER,GRAY
									BOX,AT(0,0,415,44),USE(?BOX1),FILL(COLOR:White),LINEWIDTH(1)
									IMAGE('conexion.jpg'),AT(0,0,97,44),USE(?IMAGE1)
									STRING('Estabelciendo Conexion con el Pac, Por Favor Espere '),AT(111,19,287,12), |
										USE(?STRING1),FONT('Segoe UI',13,00CC6600h,FONT:bold)
								END

	CODE
		OPEN(Wproceso)
		DISPLAY
		lOk = FALSE
		IF ~oStr.FromFile(SELF.PathXmlSellado)
            curl.Init()
			EnviaStr.SetString('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:pax="https://www.paxfacturacion.com.mx:453">'&|
				'<soapenv:Header/>'&|
				'<soapenv:Body>'&|
				'<pax:fnEnviarXML>'&|
				'<pax:psComprobante><![CDATA['&CLIP(oStr.Str())&']]></pax:psComprobante>'&|
				'<pax:psTipoDocumento>'&clip(pTipoDoc)&'</pax:psTipoDocumento>'&|
				'<pax:pnId_Estructura>0</pax:pnId_Estructura>'&|
				'<pax:sNombre>'&clip(pUsuario)&'</pax:sNombre>'&|
				self.Convierte_Utf8('<pax:sContraseña>'&clip(pPassword)&'</pax:sContraseña>')&|
				'<pax:sVersion>3.3</pax:sVersion>'&|
				'</pax:fnEnviarXML>'&|					
				'</soapenv:Body>'&|
                '</soapenv:Envelope>')
            
			curl.AddHttpHeader('Content-Type: text/xml;charset=UTF-8')
			curl.AddHttpHeader('SOAPAction: "https://www.paxfacturacion.com.mx:453/fnEnviarXML"')
			curl.AddHttpHeader('Content-Length:'&EnviaStr.Length())
			curl.AddHttpHeader('Host: www.paxfacturacion.com.mx:453')
			curl.SetHttpHeaders()  
			curl.SetSSLVerifyHost(false)  
			curl.SetSSLVerifyPeer(false)  
            res = curl.SendRequestStr('https://www.paxfacturacion.com.mx:453/webservices/wcfRecepcionasmx.asmx', clip(EnviaStr.Str()), Respuesta1)
            !MESSAGE('res='&res)
            !MESSAGE(clip(respuesta1))
            Respuesta.SetString(respuesta1)
			ERROR#=0
			IF res = CURLE_OK
				IF INSTRING('EL ATRIBUTO',clip(UPPER(Respuesta1)),1,1)
					ERROR#=1
                END
				IF INSTRING('El valor ',clip(Respuesta1),1,1)
					ERROR#=1
				END
				IF INSTRING('XML mal',clip(Respuesta1),1,1)
					ERROR#=1
				END
				IF INSTRING('Error ',clip(Respuesta1),1,1)
					ERROR#=1						
				END
				IF INSTRING('Usuario o contrase',clip(Respuesta1),1,1)
					ERROR#=1						
				END
				IF INSTRING('El campo ',clip(Respuesta1),1,1)
					ERROR#=1						
                END
                IF INSTRING('Incumplimiento',clip(Respuesta1),1,1)
					ERROR#=1						
				END
				IF INSTRING('404 - 404',clip(Respuesta1),1,1)
					ERROR#=1						
				END
				Quitar=''
                Quitar = '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">'
                Respuesta.ReplaceInContent(clip(Quitar),'')
                Quitar = '<soap:Body><fnEnviarXMLResponse xmlns="https://www.paxfacturacion.com.mx:453"><fnEnviarXMLResult>'
                Respuesta.ReplaceInContent(clip(Quitar),'')
                Quitar = '</fnEnviarXMLResult></fnEnviarXMLResponse></soap:Body></soap:Envelope>'
                Respuesta.ReplaceInContent(clip(Quitar),'')
                Quitar = '&gt;'
                Respuesta.ReplaceInContent(clip(Quitar),'>')
                Quitar = '&lt;'
                Respuesta.ReplaceInContent(clip(Quitar),'<')
                Quitar = '&amp;quot;'
                Respuesta.ReplaceInContent(clip(Quitar),'&quot;')
                Quitar = '&amp;amp;'
                Respuesta.ReplaceInContent(clip(Quitar),'&amp;')
                oStr.SetString(clip(Respuesta.Str()))
				IF ERROR#=0
                    IF ~oStr.ToFile(self.PathXmlTimbrado)
						self.BuscaAtributos() 
						lOk=TRUE
						GUarda#=1
					else
						MESSAGE('Error al guardar el xml')
					END
				else
					MESSAGE(choose(clip(self.BaseGroup.tipoComprobante)='N','No Reloj='&self.Nomina.NumEmp,'')&clip(Resultado))
				END
			ELSIF res = -1 
				MESSAGE('EL servicio regreso error')
				ERROR#=1
			ELSE
				message('No hay respuesta del webservice')
				ERROR#=1
			END
			curl.Cleanup()
		ELSE
			MESSAGE('Ocurrio un error al cargar el xml')
			lOk=FALSE
		END
		CLOSE(Wproceso)
		CLEAR(Loc:Msg)
		RETURN lok		
		
		
TimbrarClass.TimbrarXmlR    PROCEDURE(STRING pUsuario,STRING pPassword,STRING pTipoDoc,STRING pTipoRet)!,BYTE     
!curl                            TCurlMailClass
!res                             CURLcode,Auto
respBuffer                      CSTRING(32768) 
Respuesta1                      STRING(32768)  
Respuesta                       STRING(32768)  
envia                           STRING(32768)
Loc:Msg                         string(1000)
Recs                            Long
lOk                             BYTE
Wproceso                        WINDOW,AT(,,415,44),FONT('Microsoft Sans Serif',8,,FONT:regular),CENTER,GRAY
									BOX,AT(0,0,415,44),USE(?BOX1),FILL(COLOR:White),LINEWIDTH(1)
									IMAGE('conexion.jpg'),AT(0,0,97,44),USE(?IMAGE1)
									STRING('Estabelciendo Conexion con el Pac, Por Favor Espere '),AT(111,19,287,12), |
										USE(?STRING1),FONT('Segoe UI',13,00CC6600h,FONT:bold)
								END

	CODE
		OPEN(Wproceso)
		DISPLAY
		lOk = FALSE
		MESSAGE('No implementado aun ')
		RETURN lOk
		Loc:Msg=''!TimbrarR(clip(SELF.PathXmlSellado),clip(self.PathXmlTimbrado),clip(pUsuario),clip(pPassword),CLIP(pTipoDoc),clip(pTipoRet))
		if sub(Loc:Msg,1,1)='1'
			self.BuscaAtributos()   
			lOk = TRUE
		ELSE
			SELF.LastError =CLIP('ERROR No...'&clip(sub(Loc:Msg,2,LEN(Loc:Msg))))
			MESSAGE(CLIP('ERROR No...'&clip(sub(Loc:Msg,2,LEN(Loc:Msg)))))
		END
		CLOSE(Wproceso)
		CLEAR(Loc:Msg)
		RETURN lok
!---------------------------------------------------------
!    Cancala el Xml 
!---------------------------------------------------------
!TimbrarClass.CancelarXml    PROCEDURE(STRING pUUID,STRING pUsuario,STRING pPassword,STRING pRFC,STRING pRfcCli,STRING pTotal)!,BYTE
!!curl                            TCurlMailClass
!!res                             CURLcode,Auto
!respBuffer                      CSTRING(32768) 
!Respuesta1                      STRING(32768)  
!Respuesta                       STRING(32768)  
!envia                           STRING(32768)
!Recs                            Long
!lOk                             BYTE
!Wproceso                        WINDOW,AT(,,415,44),FONT('Microsoft Sans Serif',8,,FONT:regular),CENTER,GRAY
!									BOX,AT(0,0,415,44),USE(?BOX1),FILL(COLOR:White),LINEWIDTH(1)
!									IMAGE('conexion.jpg'),AT(0,0,97,44),USE(?IMAGE1)
!									STRING('Estabelciendo Conexion con el Pac, Por Favor Espere '),AT(111,19,287,12), |
!										USE(?STRING1),FONT('Segoe UI',13,00CC6600h,FONT:bold)
!								END
!
!	CODE
!        !MESSAGE('Usuario='&pUsuario&' pass='&clip(pPassword)&' rfc='&clip(pRFC))
!		lOk = FALSE
!		IF CLIP(SELF.PathXmlCancelado )=''
!			MESSAGE('No se ha indicado el path para guardar la respuesta del sat')
!			RETURN lOk
!		END
!		open(Wproceso)
!		DISPLAY
!		Respuesta='0'!Cancelar(clip(pUUID),clip(pRFC),clip(pUsuario),clip(pPassword),clip(pRfcCli),clip(pTotal))
!		Respuesta=self.Convierte_Utf8(sub(Respuesta,2,len(clip(Respuesta))))
!		IF LEN(CLIP(Respuesta))>50
!			IF SELF.SaveXmlCancelado(Respuesta)
!				lOk = TRUE
!			END
!		ELSE
!			MESSAGE(clip(respuesta),'Clase TimbrarClass.clw')
!			!self.MensajeCancela=left(respuesta,100)
!		END
!        
!		close(Wproceso)
!		RETURN lok

		
TimbrarClass.CancelarXml2   PROCEDURE(STRING pUUID,STRING pUsuario,STRING pPassword,STRING pRFC,STRING pRfcCli,STRING pTotal)!,BYTE
curl                            TCurlClass! TCurlMailClass
res                             CURLcode,Auto
respBuffer                      CSTRING(32768) 
Respuesta1                      STRING(32768)  
Respuesta                       STRING(32768)  
envia                           STRING(32768)
L:Xml                           STRING(32768)
oStr                            SystemStringClass
Resultado                       STRING(32768)
Quitar                          STRING(20000)
Recs                            Long
lOk                             BYTE
Wproceso                        WINDOW,AT(,,415,44),FONT('Microsoft Sans Serif',8,,FONT:regular),CENTER,GRAY
									BOX,AT(0,0,415,44),USE(?BOX1),FILL(COLOR:White),LINEWIDTH(1)
									IMAGE('conexion.jpg'),AT(0,0,97,44),USE(?IMAGE1)
									STRING('Estabelciendo Conexion con el Pac, Por Favor Espere '),AT(111,19,287,12), |
										USE(?STRING1),FONT('Segoe UI',13,00CC6600h,FONT:bold)
								END

	CODE
		lOk = FALSE
		IF CLIP(SELF.PathXmlCancelado )=''
			MESSAGE('No se ha indicado el path para guardar la respuesta del sat')
			RETURN lOk
		END
		
		envia=''
		curl.Init()
		envia ='<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:pax="https://www.paxfacturacion.com.mx:476">'&|
			'<soapenv:Header/>'&|
			'<soapenv:Body>'&|
			'<pax:fnCancelarXML>'&|
			'<pax:sListaUUID>'&|
			'<pax:string>'&clip(pUUID)&'</pax:string>'&|
			'</pax:sListaUUID>'&|
			'<pax:psRFCEmisor>'&clip(pRFC)&'</pax:psRFCEmisor>'&|
			'<pax:psRFCReceptor>'&|
			'<pax:string>'&clip(pRfcCli)&'</pax:string>'&|
			'</pax:psRFCReceptor>'&|
			'<pax:sListaTotales>'&|
			'<pax:string>'&clip(pTotal)&'</pax:string>'&|
			'</pax:sListaTotales>'&|
			'<pax:sNombre>'&clip(pUsuario)&'</pax:sNombre>'&|
			self.Convierte_Utf8('<pax:sContrasena>'&clip(pPassword)&'</pax:sContrasena>')&|
			'</pax:fnCancelarXML>'&|
			'</soapenv:Body>'&|
			'</soapenv:Envelope>'
		Respuesta= ''
		respuesta1=''
		curl.AddHttpHeader('Content-Type: text/xml;charset=UTF-8')
		curl.AddHttpHeader('SOAPAction: "https://www.paxfacturacion.com.mx:476/fnCancelarXML"')
		curl.AddHttpHeader('Content-Length:'&len(clip(envia)))
		curl.AddHttpHeader('Host: www.paxfacturacion.com.mx:476')
		curl.SetHttpHeaders()  
		curl.SetSSLVerifyHost(false)  
		curl.SetSSLVerifyPeer(false)  
		res = curl.SendRequestStr('https://www.paxfacturacion.com.mx:476/webservices/wcfCancelaASMX.asmx', clip(envia), respuesta1)
		Respuesta=respuesta1
		ERROR#=0
		IF res = CURLE_OK
				 !IF LEN(CLIP(Respuesta))>2000
			Quitar=''
			Resultado=''
			Quitar = '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">'
			Resultado =self.Remplazar(clip(Respuesta),clip(Quitar),'')
			Quitar = '<soap:Body><fnCancelarXMLResponse xmlns="https://www.paxfacturacion.com.mx:476"><fnCancelarXMLResult><![CDATA['
			Resultado = self.Remplazar(clip(Resultado),clip(Quitar),'')
			Quitar = ']]></fnCancelarXMLResult></fnCancelarXMLResponse></soap:Body></soap:Envelope>'
			Resultado = self.Remplazar(clip(Resultado),clip(Quitar),'')
			Quitar = '&gt;'
			Resultado = self.Remplazar(clip(Resultado),clip(Quitar),'>')
			Quitar = '&lt;'
			Resultado = self.Remplazar(clip(Resultado),clip(Quitar),'<')
			IF SELF.SaveXmlCancelado(Respuesta)
				lOk = TRUE
			END
		ELSIF res = -1 
			MESSAGE('EL servicio regreso error')
			ERROR#=1
		ELSE
			message('No hay respuesta del webservice')
			ERROR#=1
		END
		curl.Cleanup()
		close(Wproceso)
		RETURN lok		
        
        
!---------------------------------------------------------
!   Carga a un queue el Valor Obtenido de la cancelacion
!---------------------------------------------------------        
TimbrarClass.LoadXmlQueue   PROCEDURE(STRING pRespuesta)!,BYTE
Recs                            LONG
Respuesta                       CSTRING(32670)
QRetVal                         QUEUE,pre(Qr)
UUID                                CSTRING(30)
UUIDEstatus                         CSTRING(4)
UUIDdescripcion                     CSTRING(50)
UUIDfecha                           CSTRING(20)
								END
lResult                         BYTE


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
			IF ~ERRORCODE() AND (QRetVal.UUIDEstatus='201' or QRetVal.UUIDEstatus='202')
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
		Quitar = '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">'        
		Resultado = SELF.Remplazar(clip(pRespuesta),clip(Quitar),'')
		Quitar = '<soap:Body><fnEnviarXMLResponse xmlns="https://www.paxfacturacion.com.mx:453"><fnEnviarXMLResult>'
		Resultado =  SELF.Remplazar(clip(Resultado),clip(Quitar),'')
		Quitar = '</fnEnviarXMLResult></fnEnviarXMLResponse></soap:Body></soap:Envelope>'
		Resultado =  SELF.Remplazar(clip(Resultado),clip(Quitar),'')
		Quitar = '&gt;'
		Resultado = SELF.Remplazar(clip(Resultado),clip(Quitar),'>')
		Quitar = '&lt;'
        Resultado = SELF.Remplazar(clip(Resultado),clip(Quitar),'<')
        
    		oStr.Str(Resultado)
		    IF ~oStr.ToFile(SELF.PathXmlCancelado)
    			lOk = TRUE
			    self.BuscaAtributos()            
            END
        
		RETURN lOk
!---------------------------------------------------------
!    Guarda el archivo xml Cancelado que regresa el pac
!---------------------------------------------------------        
TimbrarClass.SaveXmlCancelado       PROCEDURE(STRING pRespuesta)!,BYTE
lOk                                     BYTE
Resultado                               STRING(32768)
Quitar                                  STRING(20000)
oStr                                    SystemStringClass
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
        IF ~INSTRING('Revisar los parametros',Resultado,1,1)
        !IF SELF.LoadXmlQueue(clip(Resultado))
            oStr.Str(clip(Resultado))
            IF ~oStr.ToFile(SELF.PathXmlCancelado)
                lOk = TRUE
            END
        ELSE
            MESSAGE('Error al cancelar, revise los parametros')
        END
        
        !END
		RETURN lOk
        
!---------------------------------------------------------
!    Formatea la fecha como la requiere el XMl 
!---------------------------------------------------------        
TimbrarClass.SetFecha       PROCEDURE(DATE pFecha,TIME pHora)!,STRING
FechaStr                        STRING(20)        
HoraStr                         STRING(10)
	CODE
		FechaStr = FORMAT(pFecha,@D10-)
		HoraStr  = FORMAT(pHora,@T04)
		RETURN CLIP(FechaStr)&'T'&CLIP(HoraStr)

        
!---------------------------------------------------------
!    Remplaza un substring en un string
!---------------------------------------------------------        
TimbrarClass.Remplazar      PROCEDURE(STRING pTekst,STRING pToken,STRING pReplace)!,STRING       
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

        
TimbrarClass.BuscaAtributos PROCEDURE()
	CODE
		SELF.G_SelloSat  = SELF.GetAtributoXml('TimbreFiscalDigital','SelloSAT')
		SELF.G_NoCertSat = SELF.GetAtributoXml('TimbreFiscalDigital','NoCertificadoSAT')
		SELF.G_FecCertif = SELF.GetAtributoXml('TimbreFiscalDigital','FechaTimbrado')
		SELF.G_UUID      = SELF.GetAtributoXml('TimbreFiscalDigital','UUID')
		SELF.G_SelloDig  = SELF.GetAtributoXml('TimbreFiscalDigital','SelloCFD')
		SELF.G_RFCPAC   =SELF.GetAtributoXml('TimbreFiscalDigital','RfcProvCertif')
		SELF.G_Cadena = ' ||1.1|'&CLIP(SELF.G_UUID)&'|'&CLIP(SELF.G_FecCertif)&'|'&CLIP(SELF.G_RFCPAC)&'|'&CLIP(SELF.G_SelloDig)&'|'&CLIP(SELF.G_NoCertificado)&'||' 
        
        

TimbrarClass.ArmaQrCode     PROCEDURE(STRING pPath)
QRCodeIMG                       STRING(1000)
Pathimg                         STRING(250)
	CODE
        ! QRCodeIMG='?re=' & Clip(self.Emisor.Rfc) & '&rr=' & Clip(self.Convierte_Utf8(self.Receptor.Rfc)) & |
        !        '&tt=' & Format(self.BaseGroup.totalFactura*1000000, @P##########.######P) & '&id=' & Clip(self.G_UUID)
        
		QRCodeIMG='https://verificacfdi.facturaelectronica.sat.gob.mx/default.aspx'&'?id=' & Clip(self.G_UUID) &'&re=' & Clip(self.Emisor.Rfc) & '&rr=' & Clip(self.Receptor.Rfc) &  '&tt=' & SELF.QuitaEspacios(Format(self.BaseGroup.totalFactura, @n_12.4)) &'&fe='&RIGHT(clip( SELF.G_SelloDig),8) 
		if clip(self.BaseGroup.tipoComprobante) = 'P'
			Pathimg = clip(pPath)&'\PAGO'&Format(self.BaseGroup.Folio, @P########P) & '.bmp'
		ELSIF  clip(self.BaseGroup.tipoComprobante) = 'N'		!    12341212345
			Pathimg = clip(pPath)&'\'&Format(self.BaseGroup.Folio, @P###########P) & '.bmp'
		ELSE            
			Pathimg = clip(pPath)&'\'&Format(self.BaseGroup.Folio, @P########P) & '.bmp'
		END
        
		self.CreaQrCode(QRCodeIMG,Pathimg)
    
         
        
        
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
        !IF CLIP(SELF.BaseGroup.lugExpedicion) = ''
        !    MESSAGE('Debe indicar el lugar de expedicion')
        !    lReult = FALSE
        !END
		IF CLIP(SELF.BaseGroup.formaPago) = ''
			MESSAGE('Debe indicar  Forma de Pago')
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
		IF INSTRING('-',CLIP(SELF.Receptor.Rfc),1,1)
			MESSAGE('El Rfc del receptor no puede contener guiones')
			lReult = FALSE
		END
		IF CLIP(SELF.Receptor.usoCfdi)=''
			MESSAGE('Debe indicar el uso del cfdi en el cliente')
			lReult = FALSE
		END
        !IF CLIP(SELF.Emisor.Calle) = ''
        !    MESSAGE('Debe indicar calle del emisor')
        !    lReult = FALSE
        !END
        !IF CLIP(SELF.Emisor.NumExt) = ''
        !    MESSAGE('Debe indicar pValor exterior del emisor')
        !    lReult = FALSE
        !END
        !IF CLIP(SELF.Emisor.Colonia) = ''
        !    MESSAGE('Debe indicar colonia del emisor')
        !    lReult = FALSE
        !END
		IF CLIP(SELF.Emisor.CodPos) = ''
			MESSAGE('Debe indicar codigo postal del emisor')
			lReult = FALSE
		END
        !IF CLIP(SELF.Emisor.Ciudad) = ''
        !    MESSAGE('Debe indicar ciudad del emisor')
        !    lReult = FALSE
        !END
        !IF CLIP(SELF.Emisor.Municipio) = ''
        !    MESSAGE('Debe indicar municipio del emisor')
        !    lReult = FALSE
        !END
        !IF CLIP(SELF.Emisor.Estado) = ''
        !    MESSAGE('Debe indicar estado del emisor')
        !    lReult = FALSE
        !END
        !IF CLIP(SELF.Emisor.Pais) = ''
        !    MESSAGE('Debe indicar pais del emisor')
        !    lReult = FALSE
        !END
		IF CLIP(SELF.Emisor.CveRegimen) = ''
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
        !IF CLIP(SELF.Receptor.Ciudad) = ''
        !    MESSAGE('Debe indicar la ciudad del Receptor')
        !    lReult = FALSE
        !END
        !IF CLIP(SELF.Receptor.Municipio) = ''
        !    MESSAGE('Debe indicar el municipio del Receptor')
        !    lReult = FALSE
        !END
        !IF CLIP(SELF.Receptor.Estado) = ''
        !    MESSAGE('Debe indicar estado del Receptor')
        !    lReult = FALSE
        !END
        !IF CLIP(SELF.Receptor.Pais) = ''
        !    MESSAGE('Debe indicar el pais del Receptor')
        !    lReult = FALSE
        !END
		RETURN lReult
        