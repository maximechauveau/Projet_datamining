#Librairie ElementTree

from xml.etree import ElementTree as ET
tweet = ET.parse("C:/Users/Maxime/Documents/Master_MEDAS/Fouille_de_donnees/Projet_datamining/projet_datamining/source/cmr-presidentielle2017-tei-v1.xml")

root = tweet.getroot()
root.tag
root.attrib


#Récupération des candidats.

ns = {'tei': 'http://www.tei-c.org/ns/1.0'}

for candidat in root.findall(".//tei:person/tei:persName/tei:name", ns):
    print(candidat.text)
    
    
#Récupération des id des candidats

for id in root.findall(".//tei:listPerson/tei:person", ns):
    print(id.attrib)


#Récupération des tweets de Jean-Luc Mélenchon
    
for twText in body.findall('.//*[@who="#cmr-présidentielle-p80820758"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)


#Récupération des tweets de N. Dupont-Aignan

for twText in body.findall('.//*[@who="#cmr-présidentielle-p38170599"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)

#Récupération des tweets d'Emmanuel Macron

for twText in body.findall('.//*[@who="#cmr-présidentielle-p1976143068"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)


#Récupération des tweets de François Fillon
    
for twText in body.findall('.//*[@who="#cmr-présidentielle-p551669623"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)


#Récupération des tweets de Jacques Cheminade

for twText in body.findall('.//*[@who="#cmr-présidentielle-p150201042"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)


#Récupération des tweets de Jean Lassalle

for twText in body.findall('.//*[@who="#cmr-présidentielle-p102722347"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)


#Récupération des tweets de Marine Le Pen

for twText in body.findall('.//*[@who="#cmr-présidentielle-p217749896"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)


#Récupération des tweets de Nathalie Arthaud

for twText in body.findall('.//*[@who="#cmr-présidentielle-p1003575248"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)


#Récupération des tweets de Philippe Poutou

for twText in body.findall('.//*[@who="#cmr-présidentielle-p374392774"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)


#Récupération des tweets de François Asselineau

for twText in body.findall('.//*[@who="#cmr-présidentielle-p200659061"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)


#Récupération des tweets de Benoît Hamon

for twText in body.findall('.//*[@who="#cmr-présidentielle-p14389177"]/tei:p', ns):
    text = "".join(twText.itertext())
    print(text)


















    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
