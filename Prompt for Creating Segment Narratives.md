# Prompt for Creating Segment Narratives and Tables from CSV Data

You are given a CSV file containing proportional responses to survey questions by different population segments.  
The file includes columns for `domain`, `short_name`, `value`, and `segment_1` through `segment_4`,  
where each segment column shows proportional responses.  

Your task is to reproduce the analytical output for **Burkina Faso 2021**, generating narratives and tables for each segment as follows:

---

## 1. Import and Process the CSV
- Load the CSV and ensure all segment columns are numeric.  
- Compute the absolute differences across segments to identify gaps.  
- For each of the five domains:
  - `Health.and.mental.models`
  - `Household.economics.and.living.conditions`
  - `Household.relationships`
  - `Woman.and.her.past.experiences`
  - `Natural.and.human.systems`
- Select at least one indicator with the largest inter-segment gap, for a total of six indicators.

---

## 2. Determine the Farthest Segment
- For each indicator and for each segment, find the **farthest segment** (the one with the largest proportional difference).  
- Record both the farthest segment name and value.  
- Convert all proportional values to **percentages**, rounded to one decimal place.

---

## 3. Generate Output for Each Segment (1–4)
- Write **one concise paragraph (≈150 words)** per segment, using clear, report-style language.  
- The narrative must reference all five domains, describing social, economic, and informational patterns.  
- Each paragraph should summarize key differences, focusing on:
  - Education  
  - Infrastructure  
  - Wealth  
  - Empowerment  
  - Health access  

---

## 4. Format Each Segment’s Table
- Create a **Markdown table** immediately after each narrative with the following columns:  
  `Domain | Indicator | Response | <Current Segment> (%) | <Farthest Segment> (%) | Gap (pp)`
- Ensure values are **sorted by Domain**.  
- Use this format consistently across all segments.

---

## 5. Style and Consistency
- Titles should follow this convention:  
  **Segment X – [Concise Summary Title] in [Urban or Rural] Areas**
- Each narrative must be **one paragraph**, followed by its table.  
- All tables must use identical formatting and alignment for easy insertion into PowerPoint or Word.

---

### ✅ Deliverable
Four Markdown-formatted narratives (**Segments 1–4**), each ≈150 words, followed by a **domain-sorted table** comparing that segment’s real percentage values against its farthest segment and the associated gap in percentage points (pp).
