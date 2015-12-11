<!--

make_output.php

David Adelberg
david.adelberg@yale.edu

Creates a .csv file with country names, 2-digit country codes, and predicted growth

-->

<?php
    $prediction_handle = fopen("R/gdp_predictions.csv", "r"); // open file for calculated predictions
    $prediction_head = fgetcsv($prediction_handle); // Call this once to skip column names
    $predictions = [];
    while(($data = fgetcsv($prediction_handle)) !== false) {
        
        // data is in the format country name, prediction
        $predictions[$data[0]] = ["country" => $data[0], "code" => '', "prediction" => $data[1]];
    }
    fclose($prediction_handle);
    
    ini_set('auto_detect_line_endings', true); // different OS have different line breaks
    $code_handle = fopen("iso_2.csv", "r"); // a file downloaded and then modified, so that country names are the same in both files
    $code_head = fgetcsv($code_handle); // call this once to skip column names
    
    while(($code = fgetcsv($code_handle)) !== false) {
        // element 1 is the country name, element 10 is the 2-digit code
        if(isset($predictions[$code[1]])) {
            $predictions[$code[1]]["code"] = $code[10];
        }
    }
    
    fclose($code_handle);
    
    // These two countries aren't in iso_2.csv
    $predictions["Kosovo"]["code"] = "RS";
    $predictions["South Sudan"]["code"] = "SS";
    
    $outfile = fopen("predictions.csv", "w");
    
    foreach($predictions as $prediction) {
        fputcsv($outfile, $prediction, $delimiter="\t");
    }
    
    fclose($outfile);
?>