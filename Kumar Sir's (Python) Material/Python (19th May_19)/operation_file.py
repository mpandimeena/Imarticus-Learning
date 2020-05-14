import os
newfile=open("Imarticus_Py.txt","w+")

for i in range(1,10):
    newfile.write("\n Hello, welcome to Python:")


for i in range(1,10):
    print(newfile.read())

newfile.seek(0)
print(newfile.tell())
os.rename("Imarticus.txt","Python.txt")
os.remove("Imarticus.txt")

newfile.close()

