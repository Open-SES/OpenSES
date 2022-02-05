import json
json_file = open("E:\Dropbox\Work\Current\OpenSES\source\jsontesting\example.json","r")
my_dataset = json.load(json_file) 
#print(my_dataset["ses_simulation"])
print('\n==============')

output=my_dataset['output']

fourfifty= [mypit for mypit in output if mypit['pit']== 450.0] 
#print(fourfifty)


for pit in fourfifty:
      print(pit['pit'],'seconds')
      print('==============')
      for ss in pit['subsegment_data']:
            print(ss['subsegment_id'],ss['temperature'])
      print('==============\n')
