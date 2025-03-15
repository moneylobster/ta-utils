import csv
import sys

def read_csv(filename):
    '''
    Read a csv file and return it as a list of dicts (each element corresponds to a row)
    '''
    with open(filename, "r", newline='') as f:
        # reader=csv.DictReader(remove_bom_from_first(f))
        reader=csv.DictReader(f)
        return [row for row in reader]

def get_field(field,data):
    return [row[field] for row in data]
    
def check_unique(field, data):
    '''
    false if duplicates exist
    '''
    lst=get_field(field,data)
    return len(set(lst))==len(lst)
    
def main():
    input_files=sys.argv[1:]
    data=[]
    for infile in input_files:
        indata=read_csv(infile)
        data+=indata
        print(f"Read {len(indata)} rows from {infile}.")
    print(f"Total rows: {len(data)}")

    if check_unique("Student name", data) and check_unique("ID number", data):
        print("Names and IDs unique!")
    else:
        raise Exception("Duplicates found in csvs. Aborting.")
    
    outname=input("Save to (.csv): ")
    with open(outname+".csv", "w", newline="") as csvfile:
        fieldnames=["Student name","ID number", "Quiz Grade"]
        writer=csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(data)
    print(f"Wrote {len(data)} rows to {outname}.csv.")

if __name__=="__main__":
    main()
