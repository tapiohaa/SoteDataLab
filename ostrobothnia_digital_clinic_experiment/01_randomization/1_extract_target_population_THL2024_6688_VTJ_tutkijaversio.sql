/* Population from
'231'	Kaskinen	'17'	Pohjanmaan hyvinvointialue
'280'	Korsnäs	'17'	Pohjanmaan hyvinvointialue
'287'	Kristiinankaupunki	'17'	Pohjanmaan hyvinvointialue
'288'	Kruunupyy	'17'	Pohjanmaan hyvinvointialue
'399'	Laihia	'17'	Pohjanmaan hyvinvointialue
'440'	Luoto	'17'	Pohjanmaan hyvinvointialue
'475'	Maalahti	'17'	Pohjanmaan hyvinvointialue
'499'	Mustasaari	'17'	Pohjanmaan hyvinvointialue
'545'	Närpiö	'17'	Pohjanmaan hyvinvointialue
'599'	Pedersören kunta	'17'	Pohjanmaan hyvinvointialue
'598'	Pietarsaari	'17'	Pohjanmaan hyvinvointialue
'893'	Uusikaarlepyy	'17'	Pohjanmaan hyvinvointialue
'905'	Vaasa	'17'	Pohjanmaan hyvinvointialue
'946'	Vöyri	'17'	Pohjanmaan hyvinvointialue

This version can be made publicly available.
2025, AO.
*/

create table THL2024_6688_vtj as 
select thl_henkilo_id, henkilotunnus as hetu, 
       etunimi, sukunimi,
       -- Finnish address, but if missing, then swedish address
       coalesce(katunimi_suomi, katunimi_ruotsi) as katunimi, katunumero, huoneistokirjain, 
       case when huoneistonumero = '000' then '' else huoneistonumero end as huoneistonumero, 
       postinumero, 
       -- If finnish address, then finnish name for the city / town, otherwise swedish name
       case when katunimi_suomi is null then postitoimipaikka_ruotsi else postitoimipaikka_suomi end as postitoimipaikka,
       asuinpaikantunnus, 
       kotikunta, floor((MONTHS_BETWEEN (TO_DATE('14.03.2025','DD.MM.YYYY'), syntymapaiva ))/12) as ika, sukupuoli, kielikoodi
from (select thl_henkilo_id, syntymapaiva, sukupuoli, kotikunta from henkilo where kuolinpaiva is null)
left join (select thl_henkilo_id, henkilotunnus from henkilotunnus where uusin_versio='K') using(thl_henkilo_id)
left join (select thl_henkilo_id, kielikoodi from aidinkieli where uusin_versio='K') using(thl_henkilo_id)
left join (select thl_henkilo_id, etunimi, sukunimi from henkilon_nimi where uusin_versio='K') using(thl_henkilo_id)
left join (select thl_henkilo_id, katunimi_suomi, katunimi_ruotsi, katunumero, huoneistokirjain, huoneistonumero, 
                  postinumero, postitoimipaikka_suomi, postitoimipaikka_ruotsi
            from kotimainen_osoite 
            where uusin_versio='K' and osoitetyyppi = 'OSOITE' and osoitelaji in ('VAKINAINEN')) using(thl_henkilo_id)
left join (select thl_henkilo_id, asuinpaikantunnus
            from kotimainen_osoite 
            where uusin_versio='K' and osoitetyyppi = 'ASUINPAIKKATUNNUS' and osoitelaji = 'VAKINAINEN') using(thl_henkilo_id)
where kotikunta in ('231', '280', '287', '288', '399', '440', '475', '499', '545', '599', '598', '893', '905', '946')
;

select count(*), count(distinct thl_henkilo_id)
from THL2024_6688_vtj;
-- 178926	178926  14.3.


