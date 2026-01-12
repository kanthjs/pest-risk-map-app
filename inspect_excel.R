library(readxl)
excel_path <- "data/Pest_Risk_Template_77_Provinces_ByPest_update2029.xlsx"
sheets <- excel_sheets(excel_path)
print("Sheet Names:")
print(sheets)
print("Structure of the first sheet:")
print(head(read_excel(excel_path, sheet = sheets[1])))
