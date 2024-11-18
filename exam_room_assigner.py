'''
A script that splits students into different exam rooms.

To extract the student list from Moodle, do this:
Participants -> (bottom) select all users -> with selected users, download as csv.
Verify that the list is sorted by last names alphabetically.

Current limitations:
- Doesnt check if there are repeating surnames. eg. if the rooms are assigned like
room 1: [..., James Smith]
room 2: [Jane Smith, ...]
both surname ranges will have Smith, which is undesired.

- The room range has to be exact. If you have more empty spots than students, the script won't work.

Example usage:
room_info={
    "A3":25,
    "A5":62,
    "B6":35,
    "B9":10}
results=assign_students("participants.csv", room_info)
print(format_resp(results))
'''
import csv
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
        return [row for row in reader]

def filter_nonstudents(participant_list):
    '''
    We need to filter out anyone who isn't a student (assistants/instructors) so remove anyone without an ID number.
    '''
    return [participant for participant in participant_list if participant["ID number"]]
    
def get_student_names(filename):
    '''
    Read the csv file and extract the student names from it.
    '''
    participant_list=read_csv(filename)
    student_list=filter_nonstudents(participant_list)
    return [[student["First name"], student["Last name"]] for student in student_list]

def assign_students(student_list_filename, room_info):
    '''
    Assign students sequentially to empty rooms.
    student_list_filename is the name of the csv file extracted from Moodle.
    room_info is a dict where the key:value is room_name:room_capacity.
    '''
    student_names=get_student_names(student_list_filename)
    student_count=len(student_names)
    assigned=0
    prev_assigned=0
    acc={}
    for room_name, capacity in room_info.items():
        assigned+=capacity
        acc[room_name]=student_names[assigned-capacity:assigned]
        print(f"Assigned {capacity} students to {room_name}, total assigned {assigned}")
    print(f"Students:\t{student_count}\nAssigned:\t{assigned}")
    if student_count>assigned:
        raise Exception(f"Not enough empty spots! Need to allocate {student_count-assigned} more.")
    return acc

def format_resp(resp):
    '''
    Format the assignments to return the surname ranges for each room.
    '''
    acc=""
    for k,v in resp.items():
        acc+=f"{k}:\t{v[0][1]} - {v[-1][1]}\n"
    return acc
