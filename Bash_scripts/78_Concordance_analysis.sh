#!/bin/bash

MASTER_ROUTE=$1
analysis=$2


Rscripts_path=$(echo "/home/manuel.tardaguila/Scripts/R/")
module load R/4.1.0


bashrc_file=$(echo "/home/manuel.tardaguila/.bashrc")

source $bashrc_file
eval "$(conda shell.bash hook)"


output_dir=$(echo "$MASTER_ROUTE""$analysis""/")

#rm -rf $output_dir
#mkdir -p $output_dir

Log_files=$(echo "$output_dir""/""Log_files/")

rm -rf $Log_files
mkdir -p $Log_files

  
#### Classification_DE_directionality ############################# 


type=$(echo "Classification_DE_directionality""_""$analysis")
outfile_Classification_DE_directionality=$(echo "$Log_files""outfile_1_""$type"".log")
touch $outfile_Classification_DE_directionality
echo -n "" > $outfile_Classification_DE_directionality
name_Classification_DE_directionality=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Classification_DE_directionality=$(echo "$Rscripts_path""259_Concordance_analysys_1_classification_DE.R")

Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S6_Provisional.rds")
Table_S7=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S7_Provisional.rds")


myjobid_Classification_DE_directionality=$(sbatch --job-name=$name_Classification_DE_directionality --output=$outfile_Classification_DE_directionality --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Classification_DE_directionality --Table_S7 $Table_S7 --Table_S6 $Table_S6 --type $type --out $output_dir")
myjobid_seff_Classification_DE_directionality=$(sbatch --dependency=afterany:$myjobid_Classification_DE_directionality --open-mode=append --output=$outfile_Classification_DE_directionality --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Classification_DE_directionality >> $outfile_Classification_DE_directionality")


#### Classification_MPRA_directionality ############################# 


type=$(echo "Classification_MPRA_directionality""_""$analysis")
outfile_Classification_MPRA_directionality=$(echo "$Log_files""outfile_2_""$type"".log")
touch $outfile_Classification_MPRA_directionality
echo -n "" > $outfile_Classification_MPRA_directionality
name_Classification_MPRA_directionality=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Classification_MPRA_directionality=$(echo "$Rscripts_path""260_Concordance_analysys_2_classification_MPRA.R")

Table_S5=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S5_Provisional.rds")


myjobid_Classification_MPRA_directionality=$(sbatch --job-name=$name_Classification_MPRA_directionality --output=$outfile_Classification_MPRA_directionality --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Classification_MPRA_directionality --Table_S5 $Table_S5 --type $type --out $output_dir")
myjobid_seff_Classification_MPRA_directionality=$(sbatch --dependency=afterany:$myjobid_Classification_MPRA_directionality --open-mode=append --output=$outfile_Classification_MPRA_directionality --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Classification_MPRA_directionality >> $outfile_Classification_MPRA_directionality")

#### Merge_and_results ############################# 


type=$(echo "Merge_and_results""_""$analysis")
outfile_Merge_and_results=$(echo "$Log_files""outfile_3_""$type"".log")
touch $outfile_Merge_and_results
echo -n "" > $outfile_Merge_and_results
name_Merge_and_results=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Merge_and_results=$(echo "$Rscripts_path""261_Concordance_analysys_3_Merge_results.R")

Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S6_Provisional.rds")
MPRA_DIRECTIONALITY=$(echo "$output_dir""Table_S5_depleted_ASE_collapsed_DIRECTIONALITY.rds")
DE_DIRECTIONALITY=$(echo "$output_dir""Table_S7_DE_SIG_collapsed_DIRECTIONALITY.rds")

myjobid_Merge_and_results=$(sbatch --dependency=afterany:$myjobid_Classification_DE_directionality:$myjobid_Classification_MPRA_directionality --job-name=$name_Merge_and_results --output=$outfile_Merge_and_results --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Merge_and_results --MPRA_DIRECTIONALITY $MPRA_DIRECTIONALITY --DE_DIRECTIONALITY $DE_DIRECTIONALITY --Table_S6 $Table_S6 --type $type --out $output_dir")
myjobid_seff_Merge_and_results=$(sbatch --dependency=afterany:$myjobid_Merge_and_results --open-mode=append --output=$outfile_Merge_and_results --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Merge_and_results >> $outfile_Merge_and_results")

