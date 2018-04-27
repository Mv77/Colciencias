clear all

global datapath "C:/Users/Mateo/Google Drive/Colciencias/Data"

* Load data
use "$datapath/Panel CEDE/PANEL_CARACTERISTICAS_GENERALES"

local vargen "codmpio depto municipio ano ao_crea pobl* indrural areaoficialkm2 altura discapital dismdo gpc pobreza gini nbi"
keep `vargen' 


merge 1:1 codmpio ano using "$datapath/Panel CEDE/PANEL_AGRICULTURA_Y_TIERRA"
local varag "agua aptitud"
keep `vargen' `varag'

preserve

use "$datapath/Panel CEDE/PANEL_DE_EDUCACION",clear
keep codmpio ano per_alfa* anos_est_mun
gen per_alfa = .
forvalues y = 1918/2005 {
	
	cap replace per_alfa = per_alfa`y' if ano == `y'
	
}

keep codmpio ano per_alfa anos_est_mun

tempfile aux
save "`aux'"

restore

merge 1:1 codmpio ano using `aux'
drop _merge

merge 1:1 codmpio ano using "$datapath/Panel CEDE/PANEL_BUEN_GOBIERNOnuevo13"
local vargob "y_total y_corr_tribut_predial g_cap_FBKF"

keep `vargen' `varag' `vargob' per_alfa anos_est_mun

keep if ano == 1993 | ano == 2005

* Deflactar series nominales por IPC

preserve

import delimited "$datapath/IPC base 2008.csv", clear
keep if fecha == "1993-01" | fecha == "2005-01"
gen ano = 1993
replace ano = 2005 if fecha == "2005-01"
drop fecha

tempfile ipc
save "`ipc'"

restore

merge m:1 ano using `ipc'
drop _merge

sort codmpio ano

local nominales "gpc `vargob'"
foreach x in `nominales' {

	replace `x' = `x'/ipc * 100

}





saveold "$datapath/data.dta", version(12) replace
