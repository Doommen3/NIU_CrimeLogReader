import pandas as pd
import re
from scratch import *

class ProcessCrashes:
    def __init__(self):
        self.complete_frame = pd.DataFrame()
        self.last_full_case = {}
        self.current_offense = None  # To store the current offense until the disposition is found

    def process_crashes(self, text_lines):
        data = {
            'Case Number': [], 'Offense': [], 'Date Reported': [], 'Time Reported': [],
            'Date Occurred': [], 'Time Occurred': [], 'Location': [],
            'Disposition': [], 'Disposition Date': [], 'Location Address': []
        }
        disposition_pattern = r"(Suspended|Open|Closed by|Unfounded)\s+(\d{1,2}/\d{1,2}/\d{4})"
        for line_group in text_lines:
            for line in line_group:
                print(f'this is line {line}')
                if re.match(r"^\d{12,}", line):  # New case entry
                    self.current_offense = None  # Reset current offense for new case
                    self.parse_line(line, data)
                elif re.match(r"^\w+$", line):  # Simple word that might be an offense
                    self.current_offense = line
                elif self.current_offense and re.search(disposition_pattern, line):  # If we have an offense waiting for a disposition
                    disposition, date = re.findall(disposition_pattern, line)[0]
                    data['Case Number'].append(self.last_full_case.get('Case Number', 'Unknown'))
                    data['Offense'].append(self.current_offense)
                    data['Date Reported'].append(self.last_full_case.get('Date Reported', 'Unknown'))
                    data['Time Reported'].append(self.last_full_case.get('Time Reported', 'Unknown'))
                    data['Date Occurred'].append(self.last_full_case.get('Date Occurred', 'Unknown'))
                    data['Time Occurred'].append(self.last_full_case.get('Time Occurred', 'Unknown'))
                    data['Location'].append(self.last_full_case.get('Location', 'Unknown'))
                    data['Disposition'].append(disposition)
                    data['Disposition Date'].append(date)
                    data['Location Address'].append('Unknown')  # Assuming no address is provided for additional offenses
                    self.current_offense = None  # Reset for the next potential offense

        self.complete_frame = pd.DataFrame(data)
        self.complete_frame.to_csv('df_output.csv')

    def parse_line(self, line, data):
        """
        Parses a single line of text to extract and structure the crash data.
        Expected format:
        [Case Number] [Offense] [Date Reported] [Time Reported] [Date Occurred]
        [Time Occurred] [Location] [Disposition] [Disposition Date] [Location Address]
        """
        parts = re.split(r'\s+', line)
        if len(parts) < 10:
            print(f"this is where we're missing things {parts}")  # Not enough data to parse
            return

        # Regex patterns to identify parts of the data
        date_pattern = re.compile(r'\d{1,2}/\d{1,2}/\d{4}')
        time_pattern = re.compile(r'(\d{1,2}:\d{2} (?:AM|PM)?)')

        # Using finditer to capture both the matches and their positions
        dates = [(m.group(), m.start()) for m in date_pattern.finditer(line)]
        times = [(m.group(), m.start()) for m in time_pattern.finditer(line)]

        # Extracting and assigning values based on positions
        case_number = parts[0]
        offense = line.split(case_number)[-1].split(dates[0][0])[0].strip() if dates else "Unknown"

        # Handling dates and times
        date_reported = dates[0][0] if dates else "0"
        date_occurred = dates[1][0] if len(dates) > 1 else "0"
        disposition_date = dates[-1][0] if len(dates) > 2 else "0"

        time_reported = times[0][0] if times else "0"
        time_occurred = times[1][0] if len(times) > 1 else "0:00 PM"

        # Extracting disposition and location based on positions
        if len(times) > 1:
            disposition_start = times[-1][1] + len(times[-1][0])
            disposition_end = dates[-1][1]
            disposition = line[disposition_start:disposition_end].strip()
        else:
            disposition = "Unknown"

        location_start = times[1][1] + len(times[1][0]) if len(times) > 1 else times[0][1] + len(times[0][0])
        location_end = disposition_start if 'disposition_start' in locals() else len(line)
        location = line[location_start:location_end].strip()

        # Storing the extracted data
        data['Case Number'].append(case_number)
        data['Offense'].append(offense)
        data['Date Reported'].append(date_reported)
        data['Time Reported'].append(time_reported)
        data['Date Occurred'].append(date_occurred)
        data['Time Occurred'].append(time_occurred)
        data['Location'].append(location)
        data['Disposition'].append(disposition)
        data['Disposition Date'].append(disposition_date)
        data['Location Address'].append("0")  # Assuming no specific location address provided

        # Update last_full_case to reflect the newly parsed line
        self.last_full_case = {
            'Case Number': case_number,
            'Date Reported': date_reported,
            'Time Reported': time_reported,
            'Date Occurred': date_occurred,
            'Time Occurred': time_occurred,
            'Location': location,
            'Disposition': disposition,
            'Disposition Date': disposition_date,
        }


    def parse_additional_offenses(self, line, data):
        """Parses lines that contain additional offenses for an existing case."""
        matches = re.findall(r"'([^']+)',\s*'(Open|Closed by|Suspended|Unfounded|Referred to)'\s+(\d{1,2}/\d{1,2}/\d{4})", line)
        for offense, disposition, date in matches:
            data['Case Number'].append(self.last_full_case['Case Number'])
            data['Offense'].append(offense)
            data['Date Reported'].append(self.last_full_case['Date Reported'])
            data['Time Reported'].append(self.last_full_case['Time Reported'])
            data['Date Occurred'].append(self.last_full_case['Date Occurred'])
            data['Time Occurred'].append(self.last_full_case['Time Occurred'])
            data['Location'].append(self.last_full_case['Location'])
            data['Disposition'].append(disposition)
            data['Disposition Date'].append(date)
            data['Location Address'].append('0')  # Assuming no address is provided for additional offenses

    def get_frame(self):
        """Returns the complete DataFrame processed."""
        return self.complete_frame



    # Initialize the DataFrame





   #    enum_index = enumerate(index)
   #    for item, ele in enumerate(index):
   #        next_ele = next(enumerate(index))
   #        print(f"this is ele {ele}")
   #
   #        print(f"this is split ele {split_list[ele: next(enum_index)]}")
   #


                #print(item.index(item[0]))










