# Fitting-distributions
Modellierung Univariater Verteilungen am Beispiel des Bitcoins

  Die Renditen der Kryptowährung Bitcoin (BTC) sollen modelliert werden. Der Ausgangspunkt dabei ist, dass zu erwarten ist, 
  dass Renditen von Kryptowährungen sich wie herkömmliche Finanzmarktdaten verhalten (zumeist mehr Wahrscheinlichkeitsmasse im Zentrum und
  in den Randbereichen als die Normalverteilung). Beispielhaft sollen die Gaußsche Mischverteilung, t-Verteilung und Generalisierte Fehlerverteilung
  betrachtet werden. Anhand der Maximum-Likelihood-Methode, werden die Parameter der Verteilung für die Bitcoindaten geschätzt. 
  Es können Kolmogorov-Smirnov-Tests zur Überprüfung der Modellgüte durchgeführt werden.
  Da der Kolmogorov-Smirnov-Test keine genauen Aussagen über die Modellgüte macht, sondern nur
  darüber, ob sie ausreichend gut ist, kann zusätzlich das Akaike Informationskriterium verwendet werden. 
  
  (Data: https://de.finance.yahoo.com/quote/BTC-USD?p=BTC-USD&.tsrc=fin-srch)
