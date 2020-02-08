import requests
import csv
url = requests.get('https://www.imf.org/external/pubs/ft/weo/2016/01/weodata/weorept.aspx?pr.x=65&pr.y=2&sy=2006&ey=2020&scsm=1&ssd=1&sort=country&ds=.&br=1&c=512%2C672%2C914%2C946%2C612%2C137%2C614%2C546%2C311%2C962%2C213%2C674%2C911%2C676%2C193%2C548%2C122%2C556%2C912%2C678%2C313%2C181%2C419%2C867%2C513%2C682%2C316%2C684%2C913%2C273%2C124%2C868%2C339%2C921%2C638%2C948%2C514%2C943%2C218%2C686%2C963%2C688%2C616%2C518%2C223%2C728%2C516%2C558%2C918%2C138%2C748%2C196%2C618%2C278%2C624%2C692%2C522%2C694%2C622%2C142%2C156%2C449%2C626%2C564%2C628%2C565%2C228%2C283%2C924%2C853%2C233%2C288%2C632%2C293%2C636%2C566%2C634%2C964%2C238%2C182%2C662%2C359%2C960%2C453%2C423%2C968%2C935%2C922%2C128%2C714%2C611%2C862%2C321%2C135%2C243%2C716%2C248%2C456%2C469%2C722%2C253%2C942%2C642%2C718%2C643%2C724%2C939%2C576%2C644%2C936%2C819%2C961%2C172%2C813%2C132%2C199%2C646%2C733%2C648%2C184%2C915%2C524%2C134%2C361%2C652%2C362%2C174%2C364%2C328%2C732%2C258%2C366%2C656%2C734%2C654%2C144%2C336%2C146%2C263%2C463%2C268%2C528%2C532%2C923%2C944%2C738%2C176%2C578%2C534%2C537%2C536%2C742%2C429%2C866%2C433%2C369%2C178%2C744%2C436%2C186%2C136%2C925%2C343%2C869%2C158%2C746%2C439%2C926%2C916%2C466%2C664%2C112%2C826%2C111%2C542%2C298%2C967%2C927%2C443%2C846%2C917%2C299%2C544%2C582%2C941%2C474%2C446%2C754%2C666%2C698%2C668&s=GGXWDN_NGDP&grp=0&a=')
from bs4 import BeautifulSoup

soup = BeautifulSoup(url.text, "lxml")

#f = open('htmlFormat.txt', 'w')
#f.write(soup.prettify())

table = soup.find_all('table', {'class':'fancy'})

dfhead=[]

dfcountry=[]
dfsubject=[]
dfunits=[]
dfscale=[]
dfnotes=[]
df2006=[]
df2007=[]
df2008=[]
df2009=[]
df2010=[]
df2011=[]
df2012=[]
df2013=[]
df2014=[]
df2015=[]
df2016=[]
df2017=[]
df2018=[]
df2019=[]
df2020=[]

headings = soup.find_all('th')
for row in headings:
	dfhead.append(row.extract().text)


dfvalues=[]
tds = soup.find_all('td')
for val in tds:
	dfvalues.append(val.extract().text)

#print(len(dfvalues))
#Length of dfvalues is 3860

newList = dfvalues[17:3837]


#print(len(newList))
#Length of new list is 3820

for x in range(0,3820,20):
	dfcountry.append(newList[x])
	dfsubject.append(newList[x+1])
	dfunits.append(newList[x+2])
	dfscale.append(newList[x+3])
	dfnotes.append(newList[x+4])
	df2006.append(newList[x+5])
	df2007.append(newList[x+6])
	df2008.append(newList[x+7])
	df2009.append(newList[x+8])
	df2010.append(newList[x+9])
	df2011.append(newList[x+10])
	df2012.append(newList[x+11])
	df2013.append(newList[x+12])
	df2014.append(newList[x+13])
	df2015.append(newList[x+14])
	df2016.append(newList[x+15])
	df2017.append(newList[x+16])
	df2018.append(newList[x+17])
	df2019.append(newList[x+18])
	df2020.append(newList[x+19])


rowsCSV = zip(dfcountry,dfsubject,dfunits,dfscale,dfnotes,df2006,df2007,df2008,df2009,df2010,df2011,df2012,df2013,df2014,df2015,df2016,df2017,df2018,df2019,df2020)

fileName = 'netDebtToGDP.csv';
with open(fileName,'w') as file:
	writer=csv.writer(file)
	writer.writerow(dfhead)
	writer.writerows(rowsCSV)

