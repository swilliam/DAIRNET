import pracmln
from timeit import timeit
from pracmln import MLN, Database, MLNQuery
#mln = MLN.load(files='C:/Users/Krazdul/AppData/Local/danielnyga/pracmln/examples/smokers/Smokers.pracmln')

mln = MLN(grammar='StandardGrammar', logic='FirstOrderLogic')
mln << 'area={Windfarm,Shipping_lane,Home_waters}'
mln << 'target={Ship1,Ship2,Turbine1,Turbine2,Home_base}'
mln << 'Surveill(area)'
mln << 'Attack(target)'
mln << 'TargetIn(target,area!)'
mln << '0.0 TargetIn(target,area) => (Surveill(area) => Attack(target))'
#mln << '0.0 !TargetIn(target,area) => (Surveill(area) => !Attack(target))'
#mln << '0.0 Surveill(area) ^ !Target(target,area) => !Attack(t)'
mln.write()
db = Database(mln)
db <<'Surveill(Windfarm)'
#db <<'Surveill(Home_waters)'
db <<'Surveill(Shipping_lane)'
db <<'TargetIn(Ship1,Shipping_lane)'
db <<'!TargetIn(Ship1,Home_waters)'
db <<'!TargetIn(Ship1,Windfarm)'
db <<'TargetIn(Turbine1,Windfarm)'
db <<'!TargetIn(Turbine1,Shipping_lane)'
db <<'!TargetIn(Turbine1,Home_waters)'
db <<'TargetIn(Turbine2,Windfarm)'
db <<'!TargetIn(Turbine2,Shipping_lane)'
db <<'!TargetIn(Turbine2,Home_waters)'
db <<'TargetIn(Ship2,Shipping_lane)'
db <<'!TargetIn(Ship2,Home_waters)'
db <<'!TargetIn(Ship2,Windfarm)'
db <<'TargetIn(Home_base, Home_waters)'
db <<'!TargetIn(Home_base, Shipping_lane)'
db <<'!TargetIn(Home_base, Windfarm)'
db <<'Attack(Turbine1)'
db <<'Attack(Ship1)'
db <<'Attack(Turbine2)'
db <<'Attack(Ship2)'
#db <<'Attack(Home_base)'
db.write()
train = mln.learn([db],debug=True)
train.write()
qdb = Database(mln)
qdb << 'Surveill(Home_waters)'
qdb.write()
result = MLNQuery(mln=mln,db=qdb).run()
result.write()
mln.tofile('dairnet.pracmln:dairnet.mln')
#db.tofile('dairnet.pracmln:train.db')
#qdb.tofile('dairnet.pracmln:query.db')
