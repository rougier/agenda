#!/bin/bash

# Configuration: Define your agenda source file here
AGENDA_FILE="/Users/rougier/.nano.d/agenda.txt"

# Clear the terminal screen for a clean workspace
clear

# Get current year and ISO week number for filtering
YEAR=$(date +%Y)
WEEK=$(date +w%V)
TODAY=$(date +%Y-%m-%d)

# Regex Breakdown
re="^([0-9]{4}-[0-9]{2}-[0-9]{2}( [wW][0-9]+)?( [[:alpha:]]+)?)"
re+="[[:space:]]+([0-9]{2}:[0-9]{2}(-[0-9]{2}:[0-9]{2})?)?"
re+="([[:space:]]+-+)*[[:space:]]+"
re+="([^@+\[]+[^@+\[[:space:]])"
re+="[[:space:]]*((@[^[:space:]+\[]+[[:space:]]*)*)"
re+="[[:space:]]*(([+][^[:space:]\[]+[[:space:]]*)*)"
re+="[[:space:]]*(\[([[:alnum:]]+)\])?$"

# UI Styling Constants
BOLD="\033[1m"
STRIKE="\033[9m"              # ANSI Strikethrough
BRIGHT_RED="\033[91m"         # For @ACTIONS
LIGHT_GRAY="\033[38;5;250m"   # For +TAGS
RESET="\033[0m"

current_date_header=""

if [[ -f "$AGENDA_FILE" ]]; then
    # Filter for week, then SORT alphabetically (chronological by date/time)
    grep "^${YEAR}.*${WEEK}" "$AGENDA_FILE" | sort | while read -r line; do
        
        if [[ $line =~ $re ]]; then
            iso_date="${BASH_REMATCH[1]:0:10}"
            time_str="${BASH_REMATCH[4]:-           }"
            title="${BASH_REMATCH[7]}"
            
            actions="${BASH_REMATCH[8]}"
            actions="${actions#"${actions%%[![:space:]]*}"}" 
            actions="${actions%"${actions##*[![:space:]]}"}" 
            
            tags="${BASH_REMATCH[10]}"
            tags="${tags#"${tags%%[![:space:]]*}"}"
            tags="${tags%"${tags##*[![:space:]]}"}"

            # --- STRIKETHROUGH LOGIC ---
            # Check if +CANCEL exists in the tags string
            line_style=""
            if [[ "$tags" == *"+CANCEL"* ]]; then
                line_style="$STRIKE"
            fi

            # Date Header Logic
            if [[ "$iso_date" != "$current_date_header" ]]; then
                [[ -n "$current_date_header" ]] && printf "\n"
                
                pretty_date=$(date -j -f "%Y-%m-%d" "$iso_date" +"%A %d %B %Y" 2>/dev/null || \
                              date -d "$iso_date" +"%A %d %B %Y" 2>/dev/null)
                
                if [[ "$iso_date" == "$TODAY" ]]; then
                    printf "${BOLD}%s${RESET} (today)\n\n" "${pretty_date:-$iso_date}"
                else
                    printf "${BOLD}%s${RESET}\n\n" "${pretty_date:-$iso_date}"
                fi
                current_date_header="$iso_date"
            fi

            # Print the line using the line_style (Empty or Strikethrough)
            printf "    ${line_style}%s  %s${RESET}" "$time_str" "$title"

            if [[ -n "$actions" ]]; then
                printf " ${BRIGHT_RED}%s${RESET}" "$actions"
            fi

            if [[ -n "$tags" ]]; then
                printf " ${LIGHT_GRAY}%s${RESET}" "$tags"
            fi

            printf "\n"
        fi
    done
else
    echo -e "${BRIGHT_RED}Error: File '$AGENDA_FILE' not found.${RESET}"
fi

printf "\n"

