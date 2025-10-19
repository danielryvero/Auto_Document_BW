> ⚙️ **Create the following ABAP programs in SE38 before using this repository**
>
> ### 📌 Included Programs
> **`ZAUTODOC_BW`** — Main program  
> Generates `.csv` documentation for SAP BW objects after interactive selection through a POPUP window.
>
> **`ZAUTODOC_BW_LAUNCH`** — Launcher program  
> Executes `ZAUTODOC_BW` in background mode (no popup), running all documentation end-to-end in a batch job.
>
> ---
>
> ### 🎯 Usage (Interactive — `ZAUTODOC_BW`)
> 1) Adjust `nopopup` flags on lines 343–344 to enable POPUP mode  
> 2) Set `LV_ROOT` output directory  
> 3) Set `LANGUAGE` and `SECOND_LANG`  
> 4) Run with `F8`  
> 5) Download `.csv` files via `/OAL11`
>
> ---
>
> ### 🤖 Usage (Background — `ZAUTODOC_BW_LAUNCH`)
> 1) Ensure `ZAUTODOC_BW` has `nopopup = 'X'` active  
> 2) Run `ZAUTODOC_BW_LAUNCH` (`F8`)  
> 3) Retrieve `.csv` from `/OAL11` when the job finishes
>
> ---
>
> ### 📂 Example CSV outputs
> ```
> InfoSets_with_IOs.csv
> InfoSource_with_IOBs.csv
> List_Of_Active_ADSO_With_IOBs.csv
> List_Of_Active_ADSOs.csv
> List_Of_Active_Cubes.csv
> List_Of_Cubes_With_IOBs.csv
> List_Of_Active_DTPs.csv
> List_Of_Active_DataSources.csv
> List_Of_Active_InfoObjects.csv
> List_Of_Active_InfoSources.csv
> List_Of_Active_Transformations.csv
> List_Of_Transformation_Code.csv
> Queries_1_List_Of_All_Queries.csv
> Queries_2_First_Lvl_Elem.csv
> Queries_3_All_Elem_By_Query.csv
> Queries_4_Detail_Of_Elem_By_ID.csv
> Queries_5_Restricted_Key_Fig.csv
> Queries_6_Calculated_Key_Fig.csv
> ```
>
> ---
>
> 📝 Notes  
> - Intended for BW on-premise environments with SE38 + AL11  
> - Useful for audits, documentation, reverse engineering and BW migration projects  
> - Source contains inline instructions for exact setup
