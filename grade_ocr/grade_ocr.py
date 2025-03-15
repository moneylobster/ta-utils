import difflib
import ollama
import csv
import imageio.v2 as iio
import codecs

# from https://stackoverflow.com/questions/20899939/removing-bom-from-gziped-csv-in-python
def remove_bom(line):
    return line[3:] if line.startswith(codecs.BOM_UTF8.decode('utf-8-sig')) else line

def remove_bom_from_first(iterable):
    f = iter(iterable)
    firstline = next(f, None)
    if firstline is not None:
        yield remove_bom(firstline)
        for line in f:
            yield line
            
def read_csv(filename):
    '''
    Read a csv file and return it as a list of dicts (each element corresponds to a row)
    '''
    with open(filename, "r", newline='') as f:
        reader=csv.DictReader(remove_bom_from_first(f))
        # reader=csv.DictReader(f)
        return [row for row in reader]

def filter_nonstudents(participant_list):
    '''
    We need to filter out anyone who isn't a student (assistants/instructors) so remove anyone without an ID number.
    '''
    return [participant for participant in participant_list if participant["ID number"]]
    
def get_student_data(filename):
    '''
    Read the csv file and extract the student names from it.
    '''
    participant_list=read_csv(filename)
    student_list=filter_nonstudents(participant_list)
    student_names=[student["First name"]+" "+student["Last name"] for student in student_list]
    student_data={student["First name"]+" "+student["Last name"]: student["ID number"] for student in student_list}
    return student_names, student_data

def name_filter(misspelled_name, student_names):
    '''
    find most likely student name that was misspelled in misspelled_name.
    '''
    return difflib.get_close_matches(misspelled_name, student_names, n=1, cutoff=0.0)[0]

name_q="This is an image of a quiz. What's the name of the student that took the quiz? Reply only with the name."

# unused, didn't work too reliably
grade_q="What's the grade the student got from this quiz? Reply only with the grade out of 10."

def infer(image_path, question):
    stream=ollama.chat(
        model="minicpm-v",
        messages=[{"role":"user",
                   "content":question,
                   "images":[image_path]}],
        stream=False,
    )

    # for chunk in stream:
        # print(chunk["message"]["content"], end="", flush=True)
    # print(stream.message.content)
    return stream.message.content

def transcribe_quiz(image_path, student_names):
    guessed_name=infer(image_path, name_q)
    # guessed_grade=infer(image_path, grade_q)
    
    name=name_filter(guessed_name, student_names)
    return name
        
def read_pic(ip):
    '''
    Read image from phone camera and save it. Return filepath.
    '''
    img=iio.imread(f"http://{ip}:4747/cam/1/frame.jpg")
    img=img[20:,20:]
    iio.imsave("tmp.png", img)
    return "tmp.png"

def process_quiz(student_names, record, ip):
    res=transcribe_quiz(read_pic(ip), student_names)
    return verify(res, student_names, record, ip)

def verify(res, student_names, record, ip):
    resp=input(f"Enter grade for {res.upper()}? [grade/n/m/q] ")
    
    if resp=="n":
        process_quiz(student_names, record, ip)
    elif resp=="m":
        return verify(parse_manual(input("Enter correct name: "), student_names),
               student_names, record, ip)
    elif resp=="q":
        return False, record
    else:
        if input(f"Confirm {res.upper()}: {resp}? [enter/n] ")=="n":
            process_quiz(student_names, record, ip)
        else:
            record_grade([res, resp], record)

    return True, record
        
def record_grade(res, record):
    record.append(res)
    return record

def parse_manual(txt, student_names):
    return name_filter(txt, student_names)

def main():
    ip="192.168.1.102"
    student_names,student_data=get_student_data("participants.csv")
    record=[]
    while True:
        keepgoing,record=process_quiz(student_names, record, ip)
        if not keepgoing:
            break
    print(record)
    
    gradename=input("Save to (.csv): ")
    data=[{"Student name": student[0],
           "ID number"  : student_data[student[0]],
           "Quiz Grade"  : student[1]}
          for student in record]
    with open(gradename+".csv", "w", newline="") as csvfile:
        fieldnames=["Student name","ID number", "Quiz Grade"]
        writer=csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(data)
    print(f"Wrote {len(data)} rows to {gradename}.csv.")

if __name__=="__main__":
    main()
